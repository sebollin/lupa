# Si la base es un archivo, lo que identifica a la tabla de una lectura es el
# archivo y la tabla, no el nombre con que la conexion adjunto la base: el mismo
# archivo con dos alias es una sola base. Con el alias en la identidad, las dos
# lecturas de una tabla sumaban `una_lectura` cada una y un cambio entre las
# dos no se veia.

.alias_fecha <- as.POSIXct("2026-06-30", tz = "UTC")
.alias_cortes <- list(Completitud = c(0.5, 0.9))

.alias_medir <- function(con, tablas) {
  nucleo <- metricas_nucleo()
  instancias <- lapply(names(tablas), function(e) {
    instanciar(especializar(nucleo$NoNulo, paste0("N", e)), e, "x")
  })
  medir_dbi(do.call(modelo, instancias), con, tablas = tablas,
            id_medicion = "j", fecha = .alias_fecha)
}

.alias_adjuntar <- function(con, archivo, alias) {
  DBI::dbExecute(con, paste0(
    "ATTACH ", as.character(DBI::dbQuoteString(con, archivo)), " AS ", alias
  ))
}

.alias_tablas <- function(m) unique(attr(m, "lecturas", exact = TRUE)$tabla)

test_that("en SQLite el mismo archivo con dos alias es una tabla, y su cambio se ve", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  archivo <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit({
    DBI::dbDisconnect(con)
    unlink(archivo)
  }, add = TRUE)
  .alias_adjuntar(con, archivo, "uno")
  .alias_adjuntar(con, archivo, "dos")
  DBI::dbExecute(con, "CREATE TABLE uno.ventas (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO uno.ventas VALUES (1), (2), (3)")
  por_uno <- list(A = DBI::Id(schema = "uno", table = "ventas"))
  por_dos <- list(B = DBI::Id(schema = "dos", table = "ventas"))
  # En una llamada: dos lecturas de una tabla no son `una_lectura`.
  m <- .alias_medir(con, c(por_uno, por_dos))
  expect_identical(length(.alias_tablas(m)), 1L)
  s <- semaforo_calidad(m, cortes = .alias_cortes)
  expect_identical(s$control_lecturas, c("limitado", "limitado"))
  # En dos llamadas con un INSERT entre ellas: la tabla cambio.
  a <- .alias_medir(con, por_uno)
  DBI::dbExecute(con, "INSERT INTO dos.ventas VALUES (4)")
  b <- .alias_medir(con, por_dos)
  u <- suppressWarnings(rbind(a, b))
  expect_identical(u$n_filas_leidas, c(3, 4))
  expect_identical(length(.alias_tablas(u)), 1L)
  expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"))
  expect_identical(u$motivo_cuentas, rep("tabla_cambio_entre_lecturas", 2L))
})

test_that("en SQLite el archivo de la base adjunta se encuentra con el alias en otra caja", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  archivo <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit({
    DBI::dbDisconnect(con)
    unlink(archivo)
  }, add = TRUE)
  .alias_adjuntar(con, archivo, "Otra")
  DBI::dbExecute(con, "CREATE TABLE Otra.ventas (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO Otra.ventas VALUES (1), (2), (3)")
  a <- .alias_medir(con, list(Exacta = DBI::Id(schema = "Otra", table = "ventas")))
  DBI::dbExecute(con, "INSERT INTO Otra.ventas VALUES (4)")
  b <- .alias_medir(con, list(OtraCaja = DBI::Id(schema = "otra", table = "ventas")))
  u <- suppressWarnings(rbind(a, b))
  ruta <- normalizePath(archivo, winslash = "/", mustWork = FALSE)
  expect_identical(length(.alias_tablas(u)), 1L)
  expect_true(all(grepl(ruta, .alias_tablas(u), fixed = TRUE, useBytes = TRUE)))
  expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"))
})

test_that("en DuckDB el mismo archivo adjunto otra vez con otro alias es la misma tabla", {
  skip_on_cran()
  skip_if_not_installed("duckdb")
  carpeta <- tempfile("alias-")
  dir.create(carpeta)
  archivo <- file.path(carpeta, "base.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit({
    DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(carpeta, recursive = TRUE)
  }, add = TRUE)
  .alias_adjuntar(con, archivo, "uno")
  DBI::dbExecute(con, "CREATE TABLE uno.ventas (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO uno.ventas VALUES (1), (2), (3)")
  a <- .alias_medir(con, list(
    A = DBI::Id(catalog = "uno", schema = "main", table = "ventas")
  ))
  DBI::dbExecute(con, "DETACH uno")
  .alias_adjuntar(con, archivo, "dos")
  DBI::dbExecute(con, "INSERT INTO dos.ventas VALUES (4)")
  b <- .alias_medir(con, list(
    B = DBI::Id(catalog = "dos", schema = "main", table = "ventas")
  ))
  u <- suppressWarnings(rbind(a, b))
  expect_identical(u$n_filas_leidas, c(3, 4))
  expect_identical(length(.alias_tablas(u)), 1L)
  expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"))
})

test_that("un rotulo con dos archivos no esconde que uno de los dos cambio", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  archivos <- c(tempfile(fileext = ".sqlite"), tempfile(fileext = ".sqlite"))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit({
    DBI::dbDisconnect(con)
    unlink(archivos)
  }, add = TRUE)
  adjunta <- list(Adjunta = DBI::Id(schema = "otra", table = "ventas"))
  medir_como <- function(nombre) {
    medir_dbi(
      modelo(instanciar(especializar(metricas_nucleo()$NoNulo, nombre),
                        "Adjunta", "x")),
      con, tablas = adjunta, id_medicion = "j", fecha = .alias_fecha
    )
  }
  .alias_adjuntar(con, archivos[[1L]], "otra")
  DBI::dbExecute(con, "CREATE TABLE otra.ventas (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO otra.ventas VALUES (1), (2), (3), (4)")
  primera <- medir_como("Primera")
  DBI::dbExecute(con, "DETACH otra")
  .alias_adjuntar(con, archivos[[2L]], "otra")
  DBI::dbExecute(con, "CREATE TABLE otra.ventas (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO otra.ventas VALUES (1), (2), (3), (4), (5), (6), (7)")
  segunda <- medir_como("Segunda")
  # Dos archivos: dos tablas, no una que cambio.
  expect_identical(rbind(primera, segunda)$estado_cuentas, c("cierran", "cierran"))
  DBI::dbExecute(con, "INSERT INTO otra.ventas VALUES (8)")
  tercera <- medir_como("Tercera")
  # El segundo archivo cambio entre dos lecturas. El rotulo da dos tablas y
  # ninguna fila dice cual leyo: no se pinta ninguna.
  u <- suppressWarnings(rbind(primera, segunda, tercera))
  expect_identical(u$n_filas_leidas, c(4, 7, 8))
  expect_identical(u$estado_cuentas, rep("no_cierran", 3L))
  expect_identical(u$motivo_cuentas, rep("tabla_cambio_entre_lecturas", 3L))
})
