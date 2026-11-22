# La compilacion de un plan: lo que no compila, lo que no se sabe y lo que es la
# respuesta de un sondeo de capacidad, y la conexion que queda despues. Sin
# servidor: SQLite y DuckDB en memoria. Los motores con servidor tienen sus
# propios archivos.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.cse_sqlite <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "cse_datos", data.frame(
    id = 1:30, x = c(seq(0.5, 14, by = 0.5), NA, NA),
    t = rep(c("a", "b", NA), 10), stringsAsFactors = FALSE
  ))
  con
}

# Las dos formas candidatas del desvio se prueban siempre; cual conoce el motor
# depende de su version (el SQLite de RSQLite trae `STDEV` y no `STDDEV_SAMP`;
# DuckDB, al reves). El oraculo es el motor mismo: la forma que rechaza al
# ejecutarla es la que tiene que quedar como candidata rechazada.
.cse_esperado_candidatas <- function(con, candidatas) {
  vapply(candidatas$sql, function(sql) {
    acepta <- tryCatch({
      DBI::dbGetQuery(con, sql)
      TRUE
    }, error = function(e) FALSE)
    if (acepta) "compilada" else "candidata_rechazada"
  }, character(1L), USE.NAMES = FALSE)
}

.cse_probar_candidatas <- function(con) {
  plan <- plan_perfilado_dbi(con, "cse_datos", compilar = TRUE)
  compilacion <- attr(plan, "compilacion", exact = TRUE)
  candidatas <- compilacion[grepl("^SELECT (STDDEV_SAMP|STDEV)\\(", compilacion$sql), ]
  expect_identical(nrow(candidatas), 2L)
  esperado <- .cse_esperado_candidatas(con, candidatas)
  # Al menos una de las dos no existe en cada motor: si no, la prueba no
  # ejerceria nada.
  expect_true(any(esperado == "candidata_rechazada"))
  expect_identical(candidatas$estado, esperado)
  expect_true(all(nzchar(candidatas$motivo[esperado == "candidata_rechazada"])))
  # El plan sano no tiene ninguna consulta que no compile.
  expect_identical(sum(compilacion$estado == "no_compilada"), 0L)
  expect_true(any(compilacion$estado == "compilada"))
  expect_true(all(compilacion$estado %in% c(
    "compilada", "no_compilada", "candidata_rechazada", "no_verificada"
  )))
}

test_that("en un plan sano de SQLite la forma candidata del desvio que el motor no conoce no cuenta como consulta que no compila", {
  skip_if_not_installed("RSQLite")
  con <- .cse_sqlite()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .cse_probar_candidatas(con)
})

test_that("en un plan sano de DuckDB la forma candidata del desvio que el motor no conoce no cuenta como consulta que no compila", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "cse_datos", data.frame(id = 1:30, x = seq(0.5, 15, by = 0.5)))
  .cse_probar_candidatas(con)
})

test_that("una sentencia rechazada fuera de un sondeo de capacidad sigue siendo una consulta que no compila", {
  skip_if_not_installed("RSQLite")
  con <- .cse_sqlite()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  compilado <- .with_compilador_dbi(con, "sqlite", {
    .compilar_sql_dbi(con, "SELECT columna_que_no_existe FROM cse_datos", "x", "sqlite")
  })
  expect_identical(compilado$compilacion$estado, "no_compilada")
  expect_true(nzchar(compilado$compilacion$motivo))
})

# SQL Server deja `SET NOEXEC ON` en la sesion si no se apaga. La verificacion
# se ejerce sin servidor imitando las ordenes de sesion: bajo NOEXEC un
# `SELECT 1` devuelve cero filas y cero columnas, sin error, que es lo que
# devuelve aca la imitacion de un `SET NOEXEC OFF` que no llego.
.cse_noexec_que_no_se_apaga <- function(conexion, orden, traer = FALSE) {
  if (orden %in% c("SET NOEXEC ON", "SET NOEXEC OFF")) return(0L)
  if (identical(orden, "SELECT 1 AS lupa_noexec")) return(data.frame())
  if (isTRUE(traer)) return(DBI::dbGetQuery(conexion, orden))
  DBI::dbExecute(conexion, orden)
}

test_that("si NOEXEC no se puede apagar, la compilacion falla diciendo que la conexion quedo inutilizable", {
  skip_if_not_installed("RSQLite")
  con <- .cse_sqlite()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  local_mocked_bindings(.orden_transaccion_dbi = .cse_noexec_que_no_se_apaga)
  expect_error(
    .with_compilador_dbi(con, "sqlserver", {
      .compilar_sql_dbi(con, "SELECT COUNT(*) AS n FROM cse_datos", "x", "sqlserver")
    }),
    class = "lupa_error_conexion_dbi", regexp = "inutilizable"
  )
  # Y el plan no lo calla aunque su constructor atrape los errores de cada paso.
  local_mocked_bindings(.motor_medicion_dbi = function(...) "sqlserver")
  expect_error(
    plan_perfilado_dbi(con, "cse_datos", compilar = TRUE),
    class = "lupa_error_conexion_dbi", regexp = "inutilizable"
  )
})

# Con odbc y FreeTDS, apagar NOEXEC con el resultado abierto lo cancela y el
# `SET` falla: el resultado se cierra antes, y NOEXEC se comprueba despues.
test_that("el resultado de la sentencia compilada se cierra antes de apagar NOEXEC", {
  skip_if_not_installed("RSQLite")
  con <- .cse_sqlite()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ordenes <- character()
  limpiar <- .limpiar_consulta_dbi
  local_mocked_bindings(
    .orden_transaccion_dbi = function(conexion, orden, traer = FALSE) {
      ordenes <<- c(ordenes, orden)
      if (orden %in% c("SET NOEXEC ON", "SET NOEXEC OFF")) return(0L)
      if (isTRUE(traer)) return(DBI::dbGetQuery(conexion, orden))
      DBI::dbExecute(conexion, orden)
    },
    .limpiar_consulta_dbi = function(resultado, conexion) {
      ordenes <<- c(ordenes, "cerrar resultado")
      limpiar(resultado, conexion)
    }
  )
  for (sql in c("SELECT COUNT(*) AS n FROM cse_datos", "SELECT nada FROM cse_datos")) {
    ordenes <- character()
    compilado <- .with_compilador_dbi(con, "sqlserver", {
      .compilar_sql_dbi(con, sql, "x", "sqlserver")
    })
    vistas <- ordenes[ordenes %in% c(
      "SET NOEXEC ON", "cerrar resultado", "SET NOEXEC OFF", "SELECT 1 AS lupa_noexec"
    )]
    if (identical(compilado$compilacion$estado, "compilada")) {
      expect_identical(vistas, c(
        "SET NOEXEC ON", "cerrar resultado", "SET NOEXEC OFF", "SELECT 1 AS lupa_noexec"
      ))
    } else {
      # Rechazada al enviar: no hay resultado que cerrar, y NOEXEC se apaga igual.
      expect_identical(compilado$compilacion$estado, "no_compilada")
      expect_identical(vistas, c("SET NOEXEC ON", "SET NOEXEC OFF", "SELECT 1 AS lupa_noexec"))
    }
  }
})
