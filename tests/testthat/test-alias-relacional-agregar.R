# El paquete habla el idioma del marco —`instanciaAtributo`, `atributo`,
# `entidad`— y acepta el nombre relacional como sinónimo de entrada: `celda`,
# `columna`, `tupla`, `tabla`. `metrica()` ya lo hacía; `agregar()` lo rechazaba,
# aunque su propio mensaje de error los enumeraba. Era una inconsistencia.
#
# El alias es sólo de entrada: el objeto guarda siempre el nombre canónico.

.medicion_de_prueba <- function() {
  nucleo <- metricas_nucleo()
  instancias <- list(
    instanciar(especializar(nucleo$NoNulo), "padron", "codigo")
  )
  medir(modelo(instancias), data.frame(codigo = c("A", "B", NA, "D")))
}

test_that("agregar() acepta el nombre relacional y devuelve lo mismo", {
  medicion <- .medicion_de_prueba()

  canonico <- agregar(medicion, "atributo", "ratio")
  relacional <- agregar(medicion, "columna", "ratio")

  expect_identical(canonico, relacional)
  # El objeto habla el idioma del marco, no el de entrada.
  expect_equal(unique(as.character(relacional$granularidad)), "atributo")
})

test_that("el mensaje de error enumera los dos vocabularios", {
  medicion <- .medicion_de_prueba()
  expect_error(
    agregar(medicion, "no_existe", "ratio"),
    "Ontolog"
  )
  expect_error(
    agregar(medicion, "no_existe", "ratio"),
    "Relacional"
  )
})
