# Una tabla temporal tapa a la de la base con el mismo nombre, y la tabla de
# cada lectura es la que el motor lee: en SQLite un nombre sin esquema busca
# primero en `temp`; en DuckDB tambien un nombre con esquema y sin catalogo
# (`main.ventas` lee la temporal).

.tmp_fecha <- as.POSIXct("2026-06-30", tz = "UTC")

.tmp_medir <- function(con, tablas) {
  nucleo <- metricas_nucleo()
  instancias <- lapply(names(tablas), function(e) {
    instanciar(especializar(nucleo$NoNulo, paste0("N", e)), e, "x")
  })
  medir_dbi(do.call(modelo, instancias), con, tablas = tablas,
            id_medicion = "j", fecha = .tmp_fecha)
}

.tmp_tabla <- function(m, entidad) {
  l <- attr(m, "lecturas", exact = TRUE)
  unique(l$tabla[l$entidad == entidad])
}

test_that("en SQLite un nombre sin esquema lee la temporal y su lectura es la temporal", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE ventas (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO main.ventas VALUES (1), (2), (3), (4), (5)")
  DBI::dbExecute(con, "CREATE TEMP TABLE ventas (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO temp.ventas VALUES (1), (2), (3)")
  m <- .tmp_medir(con, list(
    SinEsquema = "ventas",
    Temporal = DBI::Id(schema = "temp", table = "ventas"),
    Principal = DBI::Id(schema = "main", table = "ventas")
  ))
  expect_identical(m$n_filas_leidas, c(3, 3, 5))
  expect_identical(m$estado_cuentas, rep("cierran", 3L))
  expect_false(anyNA(attr(m, "lecturas", exact = TRUE)$tabla))
  expect_identical(.tmp_tabla(m, "SinEsquema"), .tmp_tabla(m, "Temporal"))
  expect_false(identical(.tmp_tabla(m, "SinEsquema"), .tmp_tabla(m, "Principal")))
})

test_that("en DuckDB un nombre con esquema y sin catalogo lee la temporal", {
  skip_on_cran()
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE ventas (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO memory.main.ventas VALUES (1), (2), (3), (4), (5)")
  DBI::dbExecute(con, "CREATE TEMP TABLE ventas (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO temp.main.ventas VALUES (1), (2), (3)")
  m <- .tmp_medir(con, list(
    ConEsquema = DBI::Id(schema = "main", table = "ventas"),
    Temporal = DBI::Id(catalog = "temp", schema = "main", table = "ventas"),
    Principal = DBI::Id(catalog = "memory", schema = "main", table = "ventas")
  ))
  expect_identical(m$n_filas_leidas, c(3, 3, 5))
  expect_identical(m$estado_cuentas, rep("cierran", 3L))
  expect_identical(.tmp_tabla(m, "ConEsquema"), .tmp_tabla(m, "Temporal"))
  expect_false(identical(.tmp_tabla(m, "ConEsquema"), .tmp_tabla(m, "Principal")))
})

test_that("el control: sin temporal, el nombre sin esquema es la tabla de la base", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE ventas (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO main.ventas VALUES (1), (2), (3), (4), (5)")
  m <- .tmp_medir(con, list(
    SinEsquema = "ventas", Principal = DBI::Id(schema = "main", table = "ventas")
  ))
  expect_identical(m$n_filas_leidas, c(5, 5))
  expect_identical(.tmp_tabla(m, "SinEsquema"), .tmp_tabla(m, "Principal"))
})
