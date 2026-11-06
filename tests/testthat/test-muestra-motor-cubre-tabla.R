# La muestra del motor cuando lo pedido cubre la tabla.
#
# Las formas de cantidad fija -`RESERVOIR (n ROWS)` de DuckDB y el orden
# pseudoaleatorio con limite- no cuentan la tabla antes de muestrear. La
# comprobacion de "muestra corta" comparaba contra lo pedido, no contra lo
# efectivo: pedir mil filas de una tabla de cincuenta se juzgaba corta. En
# DuckDB eso disparaba un reintento `TABLESAMPLE BERNOULLI (100)` que DuckDB
# rechaza (un numero sin unidad es una cantidad de filas y `BERNOULLI` no las
# acepta, en 1.5 y en 2.0) y dejaba TODAS las metricas `no_disponible`; en los
# motores de orden pseudoaleatorio publicaba una degradacion falsa.
#
# Lo que se publica cuando la muestra es la tabla entera sigue el precedente de
# los motores que cuentan antes (PostgreSQL, SQL Server): `estimado`, con
# `fraccion = 1`.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.cubre_avisos <- function(expr) {
  clases <- character()
  valor <- withCallingHandlers(
    suppressMessages(expr),
    warning = function(w) {
      clases <<- c(clases, class(w)[[1L]])
      invokeRestart("muffleWarning")
    }
  )
  list(valor = valor, clases = clases)
}

.cubre_fila <- function(perfil, metrica, columna = "v") {
  sql <- perfil$resumen_tabla$sql
  sql[sql$columna == columna & sql$metrica == metrica, , drop = FALSE]
}

test_that("DuckDB con la muestra pedida por encima de la tabla mide la tabla", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE cubre_duck AS SELECT CAST(range AS DOUBLE) * 1.5 + 0.25 ",
    "AS v FROM range(50)"
  ))
  for (pedido in c(100L, 1000L)) {
    corrida <- .cubre_avisos(perfilar_dbi(
      con, "cubre_duck", universo = "muestra_motor", muestra_motor = pedido,
      bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
    ))
    perfil <- corrida$valor
    expect_false("lupa_aviso_muestra_motor" %in% corrida$clases)
    media <- .cubre_fila(perfil, "media")
    expect_identical(media$estado, "estimado")
    expect_equal(
      as.numeric(perfil$resumen_tabla$columnas$media),
      mean((0:49) * 1.5 + 0.25)
    )
    expect_identical(.cubre_fila(perfil, "desvio")$estado, "estimado")
    muestreo <- perfil$resumen_tabla$meta$muestreo
    expect_identical(muestreo$metodo, "tablesample_reservoir")
    expect_identical(as.integer(muestreo$reintentos), 0L)
    expect_equal(muestreo$filas_obtenidas, 50)
    expect_equal(muestreo$fraccion, 1)
    cobertura <- perfil$resumen_tabla$cobertura
    expect_false(any(cobertura$estado == "degradado"))
  }
})

test_that("DuckDB escribe BERNOULLI con su unidad y el motor la ejecuta", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  DBI::dbExecute(con,
    "CREATE TABLE cubre_bern AS SELECT range AS v FROM range(50)")
  dialecto <- lupa:::.dialectos_dbi()$limit
  candidatos <- lupa:::.candidatos_muestreo_dbi(con, dialecto)
  expect_true("tablesample_bernoulli" %in% names(candidatos))
  bernoulli <- candidatos[["tablesample_bernoulli"]]
  # La tabla entera (2000 pedidas de 50) y una fraccion (10 de 50): las dos
  # tienen que ser sentencias que DuckDB acepta.
  for (caso in list(c(2000, 50), c(10, 50))) {
    fuente <- lupa:::.fuente_muestreada_dbi(
      "cubre_bern", "v", caso[[1L]], caso[[2L]], dialecto,
      list(candidato = bernoulli)
    )
    filas <- DBI::dbGetQuery(con, paste0(
      "SELECT COUNT(*) AS n FROM (", fuente$sql, ") AS s"
    ))$n
    expect_true(is.finite(as.numeric(filas)))
    if (caso[[1L]] >= caso[[2L]]) expect_equal(as.numeric(filas), 50)
  }
  # La consulta de prueba de la forma tampoco falla.
  prueba_forma <- lupa:::.forma_muestreo_dbi(
    bernoulli, "cubre_bern", "1 AS uno", porcentaje = "1", muestra = 1L,
    dialecto = dialecto
  )
  expect_no_error(DBI::dbGetQuery(con, prueba_forma$sql))
})

test_that("PostgreSQL sigue escribiendo BERNOULLI sin unidad", {
  skip_if_not_installed("RPostgres")
  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = 55432,
      user = "postgres", password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
      dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
    ),
    error = function(e) NULL
  )
  if (is.null(con)) skip("PostgreSQL real no disponible en 55432.")
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  candidatos <- lupa:::.candidatos_muestreo_dbi(con, lupa:::.dialectos_dbi()$limit)
  expect_identical(
    candidatos[["tablesample_bernoulli"]]$constructor("t", "20"),
    "t TABLESAMPLE BERNOULLI (20)"
  )
})

test_that("el orden pseudoaleatorio con la muestra por encima de la tabla no se declara degradado", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "cubre_sqlite", data.frame(v = c(1.25, 2.5, 4)))
  corrida <- .cubre_avisos(perfilar_dbi(
    con, "cubre_sqlite", universo = "muestra_motor", muestra_motor = 10L,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
  ))
  perfil <- corrida$valor
  expect_false("lupa_aviso_muestra_motor" %in% corrida$clases)
  expect_identical(.cubre_fila(perfil, "media")$estado, "estimado")
  expect_equal(as.numeric(perfil$resumen_tabla$columnas$media), 7.75 / 3)
  muestreo <- perfil$resumen_tabla$meta$muestreo
  expect_identical(muestreo$metodo, "random_limit")
  expect_equal(muestreo$filas_obtenidas, 3)
  expect_equal(muestreo$fraccion, 1)
  expect_false(any(perfil$resumen_tabla$cobertura$estado == "degradado"))
})

# El control: una muestra que de verdad queda por debajo de la mitad de lo
# efectivo (10 pedidas de una tabla de 50, y el motor entrega 2) sigue
# declarandose degradada y avisando. La conexion recorta el limite de la
# consulta de la muestra.
skip_if_not_installed("RSQLite")
library(DBI)
if (!methods::isClass("ConexionMuestraRecortadaLupa")) {
  setClass("ConexionMuestraRecortadaLupa", contains = "SQLiteConnection")
}
.recortar_muestra_sql <- function(statement) {
  sub("ORDER BY RANDOM() LIMIT 10", "ORDER BY RANDOM() LIMIT 2", statement,
      fixed = TRUE, useBytes = TRUE)
}
setMethod(
  "dbGetQuery", c("ConexionMuestraRecortadaLupa", "character"),
  function(conn, statement, ...) {
    callNextMethod(conn, .recortar_muestra_sql(statement), ...)
  }
)
setMethod(
  "dbSendQuery", c("ConexionMuestraRecortadaLupa", "character"),
  function(conn, statement, ...) {
    callNextMethod(conn, .recortar_muestra_sql(statement), ...)
  }
)

test_that("una muestra corta de verdad sigue degradada y avisa", {
  skip_if_not_installed("RSQLite")
  cruda <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(cruda, "cubre_corta", data.frame(v = seq_len(50L) + 0.5))
  con <- methods::new("ConexionMuestraRecortadaLupa")
  for (ranura in methods::slotNames(cruda)) {
    methods::slot(con, ranura) <- methods::slot(cruda, ranura)
  }
  on.exit(DBI::dbDisconnect(cruda), add = TRUE)
  corrida <- .cubre_avisos(perfilar_dbi(
    con, "cubre_corta", universo = "muestra_motor", muestra_motor = 10L,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
  ))
  perfil <- corrida$valor
  expect_true("lupa_aviso_muestra_motor" %in% corrida$clases)
  expect_equal(perfil$resumen_tabla$meta$muestreo$filas_obtenidas, 2)
  expect_true(any(perfil$resumen_tabla$cobertura$estado == "degradado"))
})
