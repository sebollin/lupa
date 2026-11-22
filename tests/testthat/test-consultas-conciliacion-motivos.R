testthat::skip_on_cran()

test_that("una tabla vacia explica las consultas de mediana que no emite", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "v0", data.frame(
    id = integer(), x = double(), g = character(),
    stringsAsFactors = FALSE
  ))

  perfil <- suppressWarnings(suppressMessages(
    perfilar_dbi(conexion, "v0")
  ))
  conciliacion <- perfil$resumen_tabla$meta$consultas$conciliacion

  expect_identical(conciliacion$estado, "desfasado")
  expect_equal(as.numeric(conciliacion$anunciadas), 20)
  expect_equal(as.numeric(conciliacion$emitidas), 18)
  expect_true(any(grepl(
    "mediana (orden total + limite/salto)",
    conciliacion$detalle$clase_consulta, fixed = TRUE, useBytes = TRUE
  )))
  expect_true(any(grepl(
    "tabla esta vacia", conciliacion$detalle$motivo,
    fixed = TRUE, useBytes = TRUE
  )))
  expect_false(any(grepl(
    "sondas_fallidas", conciliacion$detalle$motivo,
    fixed = TRUE, useBytes = TRUE
  )))
})

test_that("las vias de spool y bloques publican la conciliacion", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", data.frame(
    id = 1:200, x = seq_len(200) / 10, g = rep(letters[1:5], 40),
    stringsAsFactors = FALSE
  ))

  spool <- suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, "t", universo = "muestra_motor", muestra_motor = 100L
  )))
  bloques <- suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, "t", bloque_filas = 20L
  )))

  expect_true(is.list(spool$resumen_tabla$meta$consultas$conciliacion))
  expect_true(is.list(bloques$resumen_tabla$meta$consultas$conciliacion))
})

test_that("el presupuesto agotado se atribuye a clases reales", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", data.frame(
    id = 1:300, x = rnorm(300), g = sample(letters[1:6], 300, TRUE),
    stringsAsFactors = FALSE
  ))

  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, "t", max_consultas = 3L
  )))
  conciliacion <- perfil$resumen_tabla$meta$consultas$conciliacion
  clases_plan <- as.character(perfil$resumen_tabla$meta$plan$clase_consulta)

  expect_true(nrow(conciliacion$detalle) > 0L)
  expect_true(all(conciliacion$detalle$clase_consulta %in% clases_plan))
  expect_false(any(grepl(
    "sin clase del plan", conciliacion$detalle$clase_consulta,
    fixed = TRUE, useBytes = TRUE
  )))
})

test_that("las fallas y los respaldos explican consultas adicionales", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  suppressPackageStartupMessages(library(DBI))
  clase <- "LupaMalaConciliacion"
  if (!methods::isClass(clase)) {
    methods::setClass(clase, contains = "SQLiteConnection")
    rechazar <- function(sql) {
      patron <- getOption("lupa.conciliacion.patron")
      if (!is.null(patron) && grepl(patron, sql, fixed = TRUE)) {
        stop("rechazada por prueba: ", patron, call. = FALSE)
      }
    }
    methods::setMethod("dbGetQuery", signature(clase, "character"),
      function(conn, statement, ...) {
        rechazar(statement)
        callNextMethod()
      })
    methods::setMethod("dbSendQuery", signature(clase, "character"),
      function(conn, statement, ...) {
        rechazar(statement)
        callNextMethod()
      })
    methods::setMethod("dbExecute", signature(clase, "character"),
      function(conn, statement, ...) {
        rechazar(statement)
        callNextMethod()
      })
    methods::setMethod("dbSendStatement", signature(clase, "character"),
      function(conn, statement, ...) {
        rechazar(statement)
        callNextMethod()
      })
  }
  conexion_base <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion_base), add = TRUE)
  DBI::dbWriteTable(conexion_base, "t", data.frame(
    id = 1:300, x = rnorm(300), g = sample(letters[1:6], 300, TRUE),
    s = paste0("v", sample(1:40, 300, TRUE)),
    stringsAsFactors = FALSE
  ))
  conexion <- methods::new(clase, conexion_base)

  opciones_previas <- options(lupa.conciliacion.patron = NULL)
  on.exit(options(opciones_previas), add = TRUE)
  for (patron in c("COUNT(DISTINCT", "LIMIT", "MAX(")) {
    options(lupa.conciliacion.patron = patron)
    perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
      conexion, "t"
    )))
    conciliacion <- perfil$resumen_tabla$meta$consultas$conciliacion
    motivos <- as.character(conciliacion$detalle$motivo)
    expect_gt(conciliacion$emitidas, conciliacion$anunciadas)
    expect_true(any(grepl(patron, motivos, fixed = TRUE, useBytes = TRUE)))
    expect_true(any(grepl(
      "reintentos|respaldo", motivos, perl = TRUE, useBytes = TRUE
    )))
  }
})

test_that("la politica por cardinalidad usa el mismo plan y desglose", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  set.seed(3)
  DBI::dbWriteTable(conexion, "t", data.frame(
    id = 1:100, x = rnorm(100), g = sample(letters[1:4], 100, TRUE),
    stringsAsFactors = FALSE
  ))

  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, "t", politica_costo = "por_cardinalidad"
  )))
  conciliacion <- perfil$resumen_tabla$meta$consultas$conciliacion
  plan <- perfil$resumen_tabla$meta$plan

  expect_identical(conciliacion$estado, "conciliado")
  expect_equal(as.numeric(conciliacion$diferencia), 0)
  expect_equal(plan$n_consultas[plan$clase_consulta ==
    "moda (GROUP BY + orden + limite)"], 1)
  expect_equal(plan$n_consultas[plan$clase_consulta ==
    "mediana (orden total + limite/salto)"], 2)
  expect_equal(nrow(conciliacion$detalle), 0L)
})
