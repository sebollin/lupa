# En PostgreSQL anterior a la 12 el doble se lee exacto, por una subconsulta que
# lo reconstruye de sus bytes, y esa subconsulta se funde con los agregados: la
# reconstruccion se calculaba en cada mencion de la columna. `OFFSET 0` la deja
# sin fundir, y vale con o sin politica de ausencia. Desde la 12 la lectura no
# convierte y la consulta es la de siempre.
testthat::skip_on_cran()

# Las sentencias del lote de agregados de una columna sobre una subconsulta de
# lectura, con las senas de PostgreSQL y la version que se pide. No hay servidor:
# la consulta se captura y no se manda.
.blp_sentencias <- function(version, tabla_sql = NULL, con_politica = FALSE) {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  capturado <- new.env(parent = emptyenv())
  capturado$sql <- character()
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "PqConnection rpostgres",
    .version_servidor_postgresql_dbi = function(conexion) version,
    .consultar_dbi = function(conexion, sql, ...) {
      capturado$sql <- c(capturado$sql, sql)
      list(ok = FALSE, datos = NULL, motivo = "no se manda", consulta_id = 1L)
    },
    .package = "lupa"
  )
  if (is.null(tabla_sql)) {
    tabla_sql <- paste0(
      "(SELECT float8send(\"v\") AS \"v\" FROM \"t\") AS \"lupa_lectura\""
    )
  }
  argumentos <- list(
    con, tabla_sql, "v", c(v = "\"v\""), TRUE, "basicos", FALSE, NULL, 1L,
    function(x) paste0("\"", x, "\""), "ventana"
  )
  if (con_politica) {
    argumentos$columnas_medibles <- list(v = "\"v\" = -999")
  }
  suppressWarnings(do.call(
    lupa:::.agregados_lote_con_biseccion_dbi, argumentos
  ))
  # Las del lote (`lupa_l1_c1_*`); la repeticion de la consulta con otros alias,
  # cuando el lote falla, no es la que decide el costo.
  sentencias <- unique(capturado$sql)
  sentencias[grepl("lupa_l1_c1_", sentencias, fixed = TRUE, useBytes = TRUE)]
}

testthat::test_that("PostgreSQL anterior a la 12 no funde la lectura exacta, con o sin politica", {
  for (version in c(90324L, 110022L, NA_integer_)) {
    sin_politica <- .blp_sentencias(version)
    testthat::expect_true(length(sin_politica) > 0L)
    testthat::expect_true(all(grepl(
      "OFFSET 0", sin_politica, fixed = TRUE, useBytes = TRUE
    )))
    con_politica <- .blp_sentencias(version, con_politica = TRUE)
    testthat::expect_true(all(grepl(
      "OFFSET 0", con_politica, fixed = TRUE, useBytes = TRUE
    )))
  }
})

testthat::test_that("desde la 12 sin politica la consulta es la de siempre", {
  sin_politica <- .blp_sentencias(160015L)
  testthat::expect_true(length(sin_politica) > 0L)
  testthat::expect_false(any(grepl(
    "OFFSET 0", sin_politica, fixed = TRUE, useBytes = TRUE
  )))
  # Con politica la barrera vale en toda version: la condicion nombra la
  # columna dos veces.
  con_politica <- .blp_sentencias(160015L, con_politica = TRUE)
  testthat::expect_true(all(grepl(
    "OFFSET 0", con_politica, fixed = TRUE, useBytes = TRUE
  )))
})

testthat::test_that("sin subconsulta de lectura no hay nada que dejar sin fundir", {
  sentencias <- .blp_sentencias(90324L, tabla_sql = "\"t\"")
  testthat::expect_true(length(sentencias) > 0L)
  testthat::expect_false(any(grepl(
    "OFFSET 0", sentencias, fixed = TRUE, useBytes = TRUE
  )))
})

testthat::test_that("PostgreSQL 9.3: sin politica la lectura exacta se calcula una vez por fila", {
  con <- .nf_pg(55493L)
  testthat::skip_if(is.null(con), "PostgreSQL 9.3 no disponible.")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabla <- "lupa_pg93c_barrera"
  .nf_drop(con, tabla)
  on.exit(.nf_drop(con, tabla), add = TRUE, after = FALSE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (id INTEGER PRIMARY KEY, v DOUBLE PRECISION)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES (1, -999), (2, 1.5), (3, 2.5), (4, 4), ",
    "(5, -999)"
  ))
  r <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, bloque_muestra = "solo_agregados"
  )))
  sql <- r$resumen_tabla$sql
  consulta <- unique(sql$sql[sql$columna == "v" & sql$metrica == "media"])
  testthat::expect_identical(length(consulta), 1L)
  # La premisa: el doble se lee por la reconstruccion de sus bytes.
  testthat::expect_true(grepl("float8send", consulta, fixed = TRUE))
  testthat::expect_true(grepl("OFFSET 0", consulta, fixed = TRUE))
  # Las mismas cifras, calculadas a mano.
  fila <- r$resumen_tabla$columnas[r$resumen_tabla$columnas$columna == "v", ]
  testthat::expect_equal(fila$media, (-999 * 2 + 1.5 + 2.5 + 4) / 5)
  # El costo que el planificador le estima: sin la barrera la reconstruccion
  # se calcularia en cada mencion y la consulta costaria mas.
  costo_de <- function(consulta) {
    plan <- DBI::dbGetQuery(con, paste("EXPLAIN", consulta))[[1L]]
    as.numeric(sub("^.*cost=[0-9.]+\\.\\.([0-9.]+) .*$", "\\1", plan[[1L]]))
  }
  sin_barrera <- gsub(" OFFSET 0", "", consulta, fixed = TRUE)
  costos <- c(costo_de(consulta), costo_de(sin_barrera))
  testthat::expect_true(all(is.finite(costos)))
  testthat::expect_lt(costos[[1L]], costos[[2L]])
})
