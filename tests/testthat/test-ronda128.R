# En una base relacional la clave primaria esta declarada en el catalogo. Sobre
# un data.frame hay que preguntarla; aca hay que leerla.

test_that("la clave primaria se lee del catalogo, simple y compuesta", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE padron (id_persona INTEGER PRIMARY KEY,",
    "nombre TEXT, edad INTEGER)"
  ))
  DBI::dbExecute(con, paste(
    "CREATE TABLE evento (anio INTEGER, cod INTEGER, monto REAL,",
    "PRIMARY KEY (anio, cod))"
  ))
  simple <- .clave_primaria_dbi(con, "padron")
  expect_equal(simple$columnas, "id_persona")
  expect_true(is.na(simple$motivo))
  compuesta <- .clave_primaria_dbi(con, "evento")
  # El orden importa: es el de la declaracion, no el alfabetico.
  expect_equal(compuesta$columnas, c("anio", "cod"))
})

test_that("una tabla sin clave declarada devuelve eso, y no un fallo", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE suelta (a INTEGER, b TEXT)")
  resultado <- .clave_primaria_dbi(con, "suelta")
  # Cero columnas y sin motivo: el catalogo contesto que no hay clave. Eso es
  # una respuesta. Un motivo significaria que no se pudo preguntar, que es otra
  # cosa y no hay que confundirlas.
  expect_equal(resultado$columnas, character())
  expect_true(is.na(resultado$motivo))
  expect_equal(resultado$fuente, "pragma")
})

test_that("cuando no se puede leer se dice el motivo y no se inventa una clave", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  resultado <- .clave_primaria_dbi(con, "no_existe_esta_tabla")
  expect_equal(resultado$columnas, character())
  expect_false(is.na(resultado$motivo))
  expect_match(resultado$motivo, "pragma")
})

test_that("la via se elige por el controlador y no probando una tras otra", {
  # Probar hasta acertar gastaria un numero de consultas que depende del motor,
  # y `plan_perfilado_dbi()` promete exactamente cuantas se emiten. Es la misma
  # razon por la que la sonda del desvio gasta siempre dos.
  expect_equal(
    .via_clave_primaria(structure(list(), class = "SQLiteConnection")), "pragma"
  )
  # `ROracle` llama a su clase `OraConnection`, sin la palabra completa.
  expect_equal(
    .via_clave_primaria(structure(list(), class = "OraConnection")),
    "all_constraints"
  )
  expect_equal(
    .via_clave_primaria(structure(list(), class = "PqConnection")),
    "information_schema"
  )
  # Un controlador desconocido cae al estandar, que es la apuesta con mas chance.
  expect_equal(
    .via_clave_primaria(structure(list(), class = "MotorInventado")),
    "information_schema"
  )
})

test_that("el nombre de tabla con comilla no rompe la consulta", {
  # Los nombres llegan validados, pero el literal se arma a mano y una comilla
  # sin escapar cerraria la cadena.
  sql <- .consultas_clave_primaria()[[1L]]$sql(NA_character_, "tab'la")
  expect_match(sql, "'tab''la'", fixed = TRUE)
})
