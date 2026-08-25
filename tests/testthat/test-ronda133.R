# Perfilar la tabla entera -lo que viene por omision- puede tardar minutos sobre
# una tabla grande, y una corrida callada no se distingue de una colgada. La
# barra avanza contra las consultas que el plan dice que se van a emitir: es un
# total conocido, no una estimacion.
#
# Lo que estas pruebas cuidan es que la barra sea *sola* una barra: que no se
# abra donde molesta y que no cambie ni un valor de lo que se mide.

test_that("la barra decide bien cuando abrirse", {
  casos <- list(
    list(previstas = 25, opcion = TRUE, espera = TRUE),
    list(previstas = 25, opcion = FALSE, espera = FALSE),
    # Debajo de una docena de consultas la corrida termina antes de que sirva.
    list(previstas = 5, opcion = TRUE, espera = FALSE),
    # Sin total conocido no hay porcentaje que mostrar.
    list(previstas = NA_real_, opcion = TRUE, espera = FALSE),
    list(previstas = Inf, opcion = TRUE, espera = FALSE)
  )
  for (caso in casos) {
    withr_opcion <- options(lupa.progreso = caso$opcion)
    presupuesto <- lupa:::.presupuesto_dbi(Inf)
    invisible(lupa:::.abrir_progreso_dbi(
      presupuesto, caso$previstas, environment()
    ))
    abierta <- !is.null(presupuesto$barra)
    if (abierta) try(cli::cli_progress_done(id = presupuesto$barra), silent = TRUE)
    options(withr_opcion)
    expect_equal(
      abierta, caso$espera,
      info = paste("previstas", caso$previstas, "opcion", caso$opcion)
    )
  }
})

test_that("la barra avanza y no rompe si se pasan las consultas previstas", {
  withr_opcion <- options(lupa.progreso = TRUE)
  on.exit(options(withr_opcion), add = TRUE)
  presupuesto <- lupa:::.presupuesto_dbi(Inf)
  invisible(lupa:::.abrir_progreso_dbi(presupuesto, 20, environment()))
  expect_false(is.null(presupuesto$barra))
  # Emitir mas consultas que las previstas no puede reventar: el plan promete un
  # numero, pero un lote rechazado agrega consultas de respaldo.
  expect_silent(for (i in seq_len(25L)) lupa:::.gastar_dbi(presupuesto))
  expect_equal(presupuesto$usadas, 25)
  expect_gt(presupuesto$usadas, presupuesto$previstas)
  try(cli::cli_progress_done(id = presupuesto$barra), silent = TRUE)
})

test_that("el progreso no cambia ni un valor de lo que se mide", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  set.seed(4)
  datos <- data.frame(id = 1:2000)
  for (k in seq_len(12L)) {
    datos[[paste0("c", k)]] <- sample(letters, 2000L, TRUE)
  }
  DBI::dbWriteTable(con, "t", datos)

  sin_barra <- perfilar_dbi(con, "t")
  withr_opcion <- options(lupa.progreso = TRUE)
  con_barra <- perfilar_dbi(con, "t")
  options(withr_opcion)

  expect_equal(
    sin_barra$resumen_tabla$columnas, con_barra$resumen_tabla$columnas
  )
  expect_equal(
    sin_barra$resumen_tabla$meta$consultas$emitidas,
    con_barra$resumen_tabla$meta$consultas$emitidas
  )
})

test_that("fuera de una sesion interactiva no se abre sin pedirlo", {
  # Una barra que aparece en la salida de un guion es ruido que despues hay que
  # filtrar. En pruebas -no interactivas- no tiene que aparecer nunca sola.
  presupuesto <- lupa:::.presupuesto_dbi(Inf)
  invisible(lupa:::.abrir_progreso_dbi(presupuesto, 40, environment()))
  expect_null(presupuesto$barra)
})

test_that("la coleccion tambien avanza, y tampoco cambia lo que mide", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  set.seed(3)
  for (k in seq_len(5L)) {
    DBI::dbWriteTable(con, paste0("t", k), data.frame(
      id = 1:200, v = sample(letters, 200L, TRUE), stringsAsFactors = FALSE
    ))
  }
  col <- coleccion(con, paste0("t", seq_len(5L)))

  # `momento` es una marca de tiempo: difiere entre dos corridas cualesquiera,
  # con barra o sin ella. Compararlo haria fallar la prueba por la razon
  # equivocada -y de hecho la hizo fallar la primera vez-.
  sin_momento <- function(x) {
    x$resumen_coleccion$momento <- NULL
    x$meta <- NULL
    x
  }
  sin_barra <- sin_momento(perfilar_coleccion(col))
  withr_opcion <- options(lupa.progreso = TRUE)
  con_barra <- sin_momento(perfilar_coleccion(col))
  options(withr_opcion)

  expect_equal(sin_barra$resumen_coleccion, con_barra$resumen_coleccion)
  expect_equal(sin_barra$cobertura_coleccion, con_barra$cobertura_coleccion)
  expect_equal(sin_barra$cobertura_metricas, con_barra$cobertura_metricas)
})

test_that("la puerta del progreso es una sola y decide igual en los dos caminos", {
  # La logica de cuando mostrar la barra vive en `.progreso_activo()` y la usan
  # tanto el perfilado DBI como la coleccion: si se duplicara, una de las dos
  # se desincronizaria al primer ajuste.
  withr_opcion <- options(lupa.progreso = TRUE)
  on.exit(options(withr_opcion), add = TRUE)
  expect_true(lupa:::.progreso_activo(20, 12))
  expect_false(lupa:::.progreso_activo(5, 12))
  expect_true(lupa:::.progreso_activo(5, 3))
  expect_false(lupa:::.progreso_activo(Inf, 3))
  expect_false(lupa:::.progreso_activo(NA_real_, 3))
  options(lupa.progreso = FALSE)
  expect_false(lupa:::.progreso_activo(20, 12))
})
