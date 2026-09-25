# Dos silencios de la capa de evaluacion.
#
# 1. Una regla DECLARA las metricas que evalua. Si alguna no tiene medidas, el
#    veredicto cubre menos de lo que la regla dice: medido, una regla sobre dos
#    metricas -una nunca instanciada- publicaba `n_medidas = 3, resultado = 1`,
#    identico a la regla que declara solo la que existe. La maquinaria del
#    silencio declarado existia para el borde -si NINGUNA coincide, `evaluar()`
#    se niega nombrando solicitadas y disponibles- y la coincidencia parcial
#    pasaba sin nombrar nada.
# 2. `desenlace = "suprimir"` declara que la medida que no cumple NO DEBE
#    PUBLICARSE. La impresion y el informe lo sostienen; `historico_calidad(ev,
#    detalle = "completo")` -que la documentacion presenta como exportable con
#    `write.csv()`- publicaba su valor.

medicion_o57 <- function(valores = c(20, 35, 40)) {
  instancia <- instanciar(especializar(metricas_nucleo()$NoNulo),
                          "personas", "edad")
  medir(modelo(instancia), data.frame(edad = valores), id_medicion = "O57",
        fecha = as.POSIXct("2026-01-01 10:00:00", tz = "UTC"))
}

test_that("el veredicto declara la metrica que la regla nombra y no se midio", {
  medicion <- medicion_o57()
  regla <- regla_evaluacion(
    "Dos metricas", function(x) x > 0.9,
    metricas = c("NoNulo@personas.edad", "NoNulo@personas.dni")
  )

  expect_warning(
    evaluacion <- evaluar(medicion, perfil_evaluacion("P", regla)),
    "NoNulo@personas.dni"
  )
  cobertura <- attr(evaluacion, "cobertura_reglas", exact = TRUE)
  expect_s3_class(cobertura, "data.frame")
  expect_equal(nrow(cobertura), 1L)
  expect_identical(cobertura$metrica_instanciada, "NoNulo@personas.dni")
  expect_identical(as.character(cobertura$estado), "sin_medidas")
  expect_true(grepl("cubre 1 de 2", cobertura$motivo, fixed = TRUE))

  salida <- NULL
  invisible(capture.output(salida <- cli::cli_fmt(print(evaluacion))))
  expect_true(any(grepl("no tiene medidas", salida)))
})

test_that("la regla cuyas metricas se midieron no declara nada", {
  # El control: si el atributo apareciera siempre, no distinguiria nada.
  medicion <- medicion_o57()
  regla <- regla_evaluacion("Una metrica", function(x) x > 0.9,
                            metricas = "NoNulo@personas.edad")

  evaluacion <- evaluar(medicion, perfil_evaluacion("P", regla))
  expect_null(attr(evaluacion, "cobertura_reglas", exact = TRUE))

  # Y la regla sin `metricas` -que evalua todas- tampoco.
  todas <- evaluar(medicion, perfil_evaluacion(
    "P", regla_evaluacion("Todas", function(x) x > 0.9)
  ))
  expect_null(attr(todas, "cobertura_reglas", exact = TRUE))
})

test_that("una regla sin ninguna metrica medida sigue siendo un error", {
  # La conducta del borde no cambia: se niega y nombra las dos listas.
  medicion <- medicion_o57()
  expect_error(
    evaluar(medicion, perfil_evaluacion("P", regla_evaluacion(
      "Ninguna", function(x) x > 0.9, metricas = "NoNulo@personas.dni"
    ))),
    "no coincide con ninguna"
  )
})

test_that("el historico no publica el valor de una medida suprimida", {
  medicion <- medicion_o57(c(20, NA, 35))
  evaluacion <- evaluar(medicion, perfil_evaluacion("P", regla_evaluacion(
    "Medida publicable", function(x) x > 0.9, desenlace = "suprimir"
  )))
  desenlaces <- as.data.frame(evaluacion$desenlaces)
  expect_equal(nrow(desenlaces), 1L)
  suprimida <- desenlaces$id_medida[[1L]]

  historico <- as.data.frame(historico_calidad(evaluacion,
                                               detalle = "completo"))
  fila <- historico[
    !is.na(historico$id_medida) & historico$id_medida == suprimida, ,
    drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_true(is.na(fila$resultado[[1L]]))
  expect_true(grepl("[valor suprimido]", fila$objeto_medible[[1L]],
                    fixed = TRUE))

  # Las otras dos medidas conservan su valor: el control de que no se enmascara
  # de mas.
  otras <- historico[
    as.character(historico$nivel) == "evaluacion_medida" &
      (is.na(historico$id_medida) | historico$id_medida != suprimida), ,
    drop = FALSE
  ]
  expect_equal(nrow(otras), 2L)
  expect_false(any(is.na(otras$resultado)))
})

test_that("el historico sin supresion no cambia", {
  medicion <- medicion_o57(c(20, NA, 35))
  evaluacion <- evaluar(medicion, perfil_evaluacion("P", regla_evaluacion(
    "Sin desenlace", function(x) x > 0.9
  )))
  historico <- as.data.frame(historico_calidad(evaluacion,
                                               detalle = "completo"))
  medidas <- historico[
    as.character(historico$nivel) == "evaluacion_medida", , drop = FALSE
  ]

  expect_equal(nrow(medidas), 3L)
  expect_false(any(is.na(medidas$resultado)))
})
