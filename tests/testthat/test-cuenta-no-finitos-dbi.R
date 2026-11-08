# Los estadisticos de magnitud se calculan solo con valores finitos, y la cuenta
# que decide si hay valores tiene que ser de esos mismos. `COUNT(x)` cuenta un
# `NaN` o un infinito: con `(NaN, NULL)` en PostgreSQL publicaba el desvio
# `no_aplica` ("requiere al menos dos valores") mientras el minimo, el maximo y
# la media decian `sin_valores`; y con un `numeric` solo `NaN`, "La columna no
# contiene valores no nulos", que es falso. Una columna que mostro valores no
# finitos la decide la via de los finitos: sin ninguno, `sin_valores` y "no
# contiene valores finitos"; bajo la muestra del motor, retenida con su motivo.
testthat::skip_on_cran()

.dnf_finitos <- "La columna no contiene valores finitos"
.dnf_uno <- "El desv\u00edo muestral requiere al menos dos valores no nulos."

.dnf_pg <- function(puerto) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = puerto, user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en ", puerto, "."))
  con
}

.dnf_tabla <- function(con, tabla, tipo, valores) {
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (id integer, x ", tipo,
                             ")"))
  filas <- sprintf("(%d, %s)", seq_along(valores), valores)
  DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES ",
                             paste(filas, collapse = ", ")))
  invisible(tabla)
}

.dnf_perfil <- function(con, tabla, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, proteger_datos_personales = FALSE,
    fecha = as.POSIXct("2026-11-08 12:00:00", tz = "UTC"), ...
  )))
}

.dnf_fila <- function(perfil, metrica) {
  sql <- perfil$resumen_tabla$sql
  sql[sql$columna == "x" & sql$metrica == metrica, , drop = FALSE]
}

# Por la tabla completa y con `solo_agregados`, con `c("basicos", "desvio")` y
# con `"desvio"`: cada metrica pedida con el estado y el comienzo del motivo.
.dnf_esperar <- function(con, tabla, estado, motivo) {
  for (via in list(list(), list(bloque_muestra = "solo_agregados"))) {
    for (metricas in list(c("basicos", "desvio"), "desvio")) {
      perfil <- do.call(.dnf_perfil, c(list(con, tabla, metricas = metricas),
                                       via))
      pedidas <- c(
        if ("basicos" %in% metricas) c("minimo", "maximo", "media"), "desvio"
      )
      for (metrica in pedidas) {
        info <- paste(tabla, paste(metricas, collapse = "+"),
                      length(via), metrica)
        fila <- .dnf_fila(perfil, metrica)
        expect_identical(fila$estado, estado, info = info)
        expect_true(isTRUE(startsWith(fila$motivo, motivo)), info = info)
      }
    }
  }
}

# Sin ningun valor finito, por la tabla completa.
.dnf_sin_finitos <- function(con, tabla, tipo, valores) {
  .dnf_tabla(con, tabla, tipo, valores)
  .dnf_esperar(con, tabla, "sin_valores", .dnf_finitos)
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
}

# Bajo la muestra del motor, retenidas con el motivo del no finito: ni
# `no_aplica` ni `sin_valores_validos`, que dirian que se contaron valores.
.dnf_muestra <- function(con, tabla, tipo, valores) {
  .dnf_tabla(con, tabla, tipo, valores)
  for (metricas in list(c("basicos", "desvio"), "desvio")) {
    perfil <- .dnf_perfil(con, tabla, metricas = metricas,
                          universo = "muestra_motor", muestra_motor = 100L,
                          bloque_muestra = "solo_agregados")
    fila <- .dnf_fila(perfil, "desvio")
    info <- paste(tabla, paste(metricas, collapse = "+"))
    expect_identical(fila$estado, "no_disponible", info = info)
    expect_true(isTRUE(startsWith(fila$motivo, "El motor respond")),
                info = paste(info, fila$motivo))
  }
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
}

# Un solo valor finito y un `NaN`: el desvio de los finitos no tiene dos
# valores, y lo que se publica de los demas es ese valor.
.dnf_uno_finito <- function(con, tabla, tipo) {
  .dnf_tabla(con, tabla, tipo, c("1.5", "'NaN'", "NULL"))
  perfil <- .dnf_perfil(con, tabla, metricas = c("basicos", "desvio"))
  fila <- .dnf_fila(perfil, "desvio")
  expect_identical(fila$estado, "no_aplica", info = tabla)
  expect_identical(fila$motivo, .dnf_uno, info = tabla)
  media <- .dnf_fila(perfil, "media")
  expect_identical(media$estado, "calculado", info = tabla)
  columnas <- perfil$resumen_tabla$columnas
  expect_identical(as.numeric(columnas$media[columnas$columna == "x"]), 1.5,
                   info = tabla)
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
}

test_that("PostgreSQL: la cuenta que decide es la de los valores finitos", {
  for (puerto in c(55432L, 55493L)) {
    con <- .dnf_pg(puerto)
    prefijo <- paste0("lupa_dnf_", puerto)
    for (tipo in c("double precision", "numeric")) {
      corto <- if (identical(tipo, "numeric")) "n" else "d"
      .dnf_sin_finitos(con, paste0(prefijo, corto, "1"), tipo,
                       c("'NaN'", "NULL"))
      .dnf_sin_finitos(con, paste0(prefijo, corto, "2"), tipo,
                       c("'NaN'", "'NaN'", "NULL"))
      .dnf_muestra(con, paste0(prefijo, corto, "m"), tipo, c("'NaN'", "NULL"))
      .dnf_uno_finito(con, paste0(prefijo, corto, "u"), tipo)
    }
    .dnf_sin_finitos(con, paste0(prefijo, "i1"), "double precision",
                     c("'Infinity'", "NULL"))
    .dnf_sin_finitos(con, paste0(prefijo, "i2"), "double precision",
                     c("'Infinity'", "'-Infinity'", "NULL"))
    .dnf_muestra(con, paste0(prefijo, "im"), "double precision",
                 c("'Infinity'", "NULL"))
    DBI::dbDisconnect(con)
  }
})

test_that("DuckDB: la cuenta que decide es la de los valores finitos", {
  con <- .dlot_duck()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  .dnf_sin_finitos(con, "dnf_i1", "DOUBLE", c("'Infinity'::DOUBLE", "NULL"))
  .dnf_muestra(con, "dnf_im", "DOUBLE", c("'Infinity'::DOUBLE", "NULL"))
})

# Si la sonda de magnitud falla, la magnitud no quedo establecida y la columna
# ya esta declarada con ese motivo: una cuenta de 1 no la vuelve `no_aplica`.
# Con un rechazo fabricado, para que no dependa de un tipo ni de una version.
test_that("una sonda de magnitud fallida: la cuenta no decide", {
  con <- .dlot_duck()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  .dnf_tabla(con, "dnf_sf", "DOUBLE", c("1.5", "NULL"))
  real <- lupa:::.consultar_dbi
  testthat::local_mocked_bindings(
    .consultar_dbi = function(conexion, sql, presupuesto = NULL, filas = -1L,
                              etapa = "consulta") {
      if (identical(etapa, "sonda_magnitud")) {
        return(list(ok = FALSE, datos = NULL, motivo = "rechazo de prueba"))
      }
      real(conexion, sql, presupuesto, filas, etapa)
    },
    .package = "lupa"
  )
  fila <- .dnf_fila(.dnf_perfil(con, "dnf_sf", metricas = "desvio"), "desvio")
  expect_identical(fila$estado, "no_disponible")
  expect_identical(fila$motivo, "rechazo de prueba")
})

# El caso real: en PostgreSQL 9.3 un `real` con `Infinity` hace fallar la sonda.
test_that("PostgreSQL 9.3: un real con Infinity no sale no_aplica por la cuenta", {
  con <- .dnf_pg(55493L)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dnf_tabla(con, "lupa_dnf_93r", "real", c("'Infinity'", "NULL"))
  fila <- .dnf_fila(.dnf_perfil(con, "lupa_dnf_93r", metricas = "desvio"),
                    "desvio")
  expect_identical(fila$estado, "no_disponible")
  try(DBI::dbExecute(con, "DROP TABLE lupa_dnf_93r"), silent = TRUE)
})

test_that("SQLite: la cuenta que decide es la de los valores finitos", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dnf_sin_finitos(con, "dnf_i1", "REAL", c("9e999", "NULL"))
  .dnf_muestra(con, "dnf_im", "REAL", c("9e999", "NULL"))
})

# Con la reconstruccion exacta escrita decenas de veces en cada nivel, fundir las
# subconsultas multiplicaba la expresion: el planificador tardaba minutos sobre
# tres filas, y `statement_timeout` no lo cortaba. Normal, son milisegundos.
test_that("PostgreSQL: un real con NaN no multiplica la expresion en el plan", {
  con <- .dnf_pg(55432L)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dnf_tabla(con, "lupa_dnf_16r", "real", c("1.5", "'NaN'", "2.5", "NULL"))
  inicio <- Sys.time()
  perfil <- .dnf_perfil(con, "lupa_dnf_16r", metricas = c("basicos", "desvio"))
  expect_lt(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 60)
  columnas <- perfil$resumen_tabla$columnas
  x <- columnas[columnas$columna == "x", , drop = FALSE]
  expect_equal(as.numeric(x$media), 2)
  expect_equal(as.numeric(x$desvio), sd(c(1.5, 2.5)))
  expect_identical(.dnf_fila(perfil, "media")$estado, "calculado")
  try(DBI::dbExecute(con, "DROP TABLE lupa_dnf_16r"), silent = TRUE)
})
