# La tabla de cada lectura es la que resuelve el motor: para un nombre sin
# esquema, en su orden de busqueda (SQLite: `temp`, `main` y las adjuntas;
# DuckDB: `temp` antes que la base de la sesion), y con el catalogo, el esquema
# y el nombre que guarda su catalogo, sin importar las mayusculas con que se
# escribieron. Cada parte va citada. Una lectura cuya tabla no se resolvio no
# prueba nada: el control de sus cifras es `desconocido`.

.idt_fecha <- as.POSIXct("2026-06-30", tz = "UTC")

.idt_medir <- function(con, tablas) {
  nucleo <- metricas_nucleo()
  instancias <- lapply(names(tablas), function(e) {
    instanciar(especializar(nucleo$NoNulo, paste0("N", e)), e, "x")
  })
  suppressWarnings(medir_dbi(do.call(modelo, instancias), con, tablas = tablas,
                             id_medicion = "j", fecha = .idt_fecha))
}

.idt_tabla <- function(m, entidad) {
  l <- attr(m, "lecturas", exact = TRUE)
  unique(l$tabla[l$entidad == entidad])
}

.idt_duckdb <- function() {
  skip_if_not_installed("duckdb")
  DBI::dbConnect(duckdb::duckdb())
}

# Dos llamadas sobre la misma tabla con otro rotulo, con un INSERT entre ellas.
.idt_cambio_entre_llamadas <- function(con, primera, segunda, insertar) {
  a <- .idt_medir(con, primera)
  DBI::dbExecute(con, insertar)
  b <- .idt_medir(con, segunda)
  suppressWarnings(rbind(a, b))
}

test_that("en DuckDB un nombre sin esquema lee la temporal y su lectura es la temporal", {
  skip_on_cran()
  con <- .idt_duckdb()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE ventas (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO memory.main.ventas VALUES (1), (2), (3), (4), (5)")
  DBI::dbExecute(con, "CREATE TEMP TABLE ventas (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO temp.main.ventas VALUES (1), (2), (3)")
  m <- .idt_medir(con, list(
    SinEsquema = "ventas",
    Temporal = DBI::Id(catalog = "temp", schema = "main", table = "ventas"),
    Principal = DBI::Id(catalog = "memory", schema = "main", table = "ventas")
  ))
  expect_identical(m$estado_cuentas, rep("cierran", 3L))
  expect_identical(.idt_tabla(m, "SinEsquema"), .idt_tabla(m, "Temporal"))
  expect_false(identical(.idt_tabla(m, "SinEsquema"), .idt_tabla(m, "Principal")))
  expect_identical(m$n_filas_leidas, c(3, 3, 5))
})

test_that("en SQLite un nombre sin esquema encuentra la tabla de una base adjunta", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "ATTACH ':memory:' AS otra")
  DBI::dbExecute(con, "CREATE TABLE otra.ventas (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO otra.ventas VALUES (1), (2), (3), (4)")
  u <- .idt_cambio_entre_llamadas(
    con, list(SinEsquema = "ventas"),
    list(Adjunta = DBI::Id(schema = "otra", table = "ventas")),
    "INSERT INTO otra.ventas VALUES (5)"
  )
  expect_false(anyNA(attr(u, "lecturas", exact = TRUE)$tabla))
  expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"))
  expect_identical(u$motivo_cuentas, rep("tabla_cambio_entre_lecturas", 2L))
})

test_that("un punto dentro de un nombre citado no une dos tablas", {
  skip_on_cran()
  con <- .idt_duckdb()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE main.\"a.b\" (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO main.\"a.b\" VALUES (1), (2)")
  DBI::dbExecute(con, "CREATE SCHEMA \"main.a\"")
  DBI::dbExecute(con, "CREATE TABLE \"main.a\".b (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO \"main.a\".b VALUES (1), (2), (3), (4), (5)")
  m <- .idt_medir(con, list(
    Punto = DBI::Id(schema = "main", table = "a.b"),
    EsquemaPunto = DBI::Id(schema = "main.a", table = "b")
  ))
  expect_identical(m$estado_cuentas, c("cierran", "cierran"))
  expect_false(identical(.idt_tabla(m, "Punto"), .idt_tabla(m, "EsquemaPunto")))
})

test_that("en SQLite el esquema MAIN es main", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "ventas", data.frame(x = 1:4))
  u <- .idt_cambio_entre_llamadas(
    con, list(Minuscula = "ventas"),
    list(Mayuscula = DBI::Id(schema = "MAIN", table = "ventas")),
    "INSERT INTO ventas VALUES (5)"
  )
  expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"))
})

test_that("en DuckDB el esquema MAIN de la base memory es main", {
  skip_on_cran()
  con <- .idt_duckdb()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "ventas", data.frame(x = 1:4))
  u <- .idt_cambio_entre_llamadas(
    con, list(Minuscula = "ventas"),
    list(Mayuscula = DBI::Id(catalog = "memory", schema = "MAIN", table = "ventas")),
    "INSERT INTO ventas VALUES (5)"
  )
  expect_false(anyNA(attr(u, "lecturas", exact = TRUE)$tabla))
  expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"))
})

test_that("en SQLite la base adjunta Otra y otra son la misma", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "ATTACH ':memory:' AS Otra")
  DBI::dbExecute(con, "CREATE TABLE Otra.ventas (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO Otra.ventas VALUES (1), (2), (3), (4)")
  u <- .idt_cambio_entre_llamadas(
    con, list(Mayuscula = DBI::Id(schema = "Otra", table = "ventas")),
    list(Minuscula = DBI::Id(schema = "otra", table = "ventas")),
    "INSERT INTO Otra.ventas VALUES (5)"
  )
  expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"))
})

test_that("una lectura del motor sin tabla resuelta deja el control desconocido", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "ventas", data.frame(x = c(1, NA, 3)))
  local_mocked_bindings(.identidad_tabla_dbi = function(...) NA_character_)
  m <- .idt_medir(con, list(ventas = "ventas"))
  expect_true(is.na(.idt_tabla(m, "ventas")))
  s <- semaforo_calidad(m, cortes = list(Completitud = c(0.5, 0.9)))
  expect_identical(s$control_lecturas, "desconocido")
})

test_that("el control: con la tabla resuelta, una lectura es una_lectura", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "ventas", data.frame(x = c(1, NA, 3)))
  m <- .idt_medir(con, list(ventas = "ventas"))
  s <- semaforo_calidad(m, cortes = list(Completitud = c(0.5, 0.9)))
  expect_identical(s$control_lecturas, "una_lectura")
})
