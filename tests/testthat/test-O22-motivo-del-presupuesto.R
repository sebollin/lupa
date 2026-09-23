# El motivo publicado debe decir si falto presupuesto o capacidad del motor.

skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

.tabla_O22 <- function(conexion) {
  DBI::dbWriteTable(
    conexion, "t",
    data.frame(
      id = seq_len(3000L),
      valor = seq_len(3000L) / 10,
      grupo = rep(c("a", "b", "c"), length.out = 3000L),
      stringsAsFactors = FALSE
    )
  )
}

.cobertura_muestra_O22 <- function(resultado) {
  cobertura <- resultado$resumen_tabla$cobertura
  cobertura[
    cobertura$bloque == "perfil_muestra" & cobertura$elemento == "t",
    , drop = FALSE
  ]
}

test_that("el motivo de una sonda bloqueada nombra el presupuesto", {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  .tabla_O22(conexion)

  resultado <- suppressWarnings(perfilar_dbi(
    conexion, "t", universo = "muestra_motor", muestra_motor = 300L,
    muestra = 300L, max_consultas = 3L, instrumentar = TRUE,
    proteger_datos_personales = FALSE
  ))
  fila <- .cobertura_muestra_O22(resultado)

  expect_equal(nrow(fila), 1L)
  expect_identical(fila$estado[[1L]], "no_disponible")
  expect_match(fila$motivo[[1L]], "max_consultas.*3 consultas")
  expect_false(grepl("capacidad_no_aceptada", fila$motivo[[1L]], fixed = TRUE))
})

test_that("un presupuesto holgado conserva el perfil de muestra", {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  .tabla_O22(conexion)

  resultado <- suppressWarnings(perfilar_dbi(
    conexion, "t", universo = "muestra_motor", muestra_motor = 300L,
    muestra = 300L, max_consultas = Inf,
    proteger_datos_personales = FALSE
  ))

  expect_false(is.null(resultado$perfil_muestra))
})

test_that("un rechazo real del motor conserva el motivo de capacidad", {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  .tabla_O22(conexion)

  resultado <- suppressWarnings(perfilar_dbi(
    conexion, "t", universo = "muestra_motor", muestra_motor = 300L,
    muestra = 300L, dialecto = "top", max_consultas = Inf,
    proteger_datos_personales = FALSE
  ))
  fila <- .cobertura_muestra_O22(resultado)

  expect_equal(nrow(fila), 1L)
  expect_identical(fila$estado[[1L]], "no_disponible")
  expect_identical(fila$motivo[[1L]],
                   "capacidad_no_aceptada:sonda_muestreo")
})
