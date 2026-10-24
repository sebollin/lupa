# Una medicion vacia que SI viene de `medir()` no es una entrada invalida: es una
# corrida donde ninguna metrica fue aplicable, y `medir()` ya declaro por que en
# su `cobertura_metricas`. Las tres puertas la rechazaban con "debe ser ...
# producido por medir()", que afirma algo falso teniendo el motivo a mano.

.medicion_vacia_de_medir <- function() {
  nucleo <- metricas_nucleo()
  instancia <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "NoNuloEdad"),
    "personas", "edad"
  )
  medir(
    modelo(instancia), data.frame(edad = character(0)),
    id_medicion = "vacia"
  )
}

test_that("una medicion vacia de medir() se rechaza diciendo la verdad", {
  m <- .medicion_vacia_de_medir()
  # El punto de partida: `medir()` la produjo y declaro el motivo.
  expect_s3_class(m, "data.frame")
  expect_equal(nrow(m), 0L)
  cobertura <- attr(m, "cobertura_metricas", exact = TRUE)
  expect_true(is.data.frame(cobertura) && nrow(cobertura) >= 1L)

  perfil <- perfil_evaluacion(
    "P", regla_evaluacion("R", function(x) x > 0.5)
  )
  puertas <- list(
    function() evaluar(m, perfil),
    function() tablero_calidad(m),
    function() indice_calidad(m)
  )
  for (puerta in puertas) {
    mensaje <- tryCatch({
      puerta()
      NA_character_
    }, error = function(e) conditionMessage(e))

    expect_false(is.na(mensaje))
    # No afirma que la entrada no venga de `medir()`, porque viene.
    expect_false(grepl("debe ser un data frame producido", mensaje, fixed = TRUE))
    expect_true(grepl("no tiene ninguna medida", mensaje, fixed = TRUE))
    # Y propaga el motivo que `medir()` ya habia declarado.
    expect_true(grepl("Motivo declarado por", mensaje, fixed = TRUE))
    expect_true(grepl("cobertura_metricas", mensaje, fixed = TRUE))
  }
})

test_that("lo que no viene de medir() se sigue rechazando por lo que es", {
  perfil <- perfil_evaluacion(
    "P", regla_evaluacion("R", function(x) x > 0.5)
  )
  mensaje <- tryCatch({
    evaluar(data.frame(a = 1), perfil)
    NA_character_
  }, error = function(e) conditionMessage(e))

  expect_false(is.na(mensaje))
  expect_true(grepl("producido por medir", mensaje, fixed = TRUE))
  expect_false(grepl("no tiene ninguna medida", mensaje, fixed = TRUE))
})
