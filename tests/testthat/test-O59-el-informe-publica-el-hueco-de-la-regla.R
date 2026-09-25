# Una declaracion vale en todas las salidas, no solo en la primera. La vuelta
# anterior hizo que `evaluar()` avisara -y su impresion declarara- cuando una
# regla nombra una metrica que la medicion no trae. El informe HTML, que es el
# que se manda a otra persona, seguia publicando `resultado = 0.9` y nada mas:
# el veredicto se leia como completo. El documento no contenia ni el nombre de la
# metrica sin medidas ni la palabra `sin_medidas`.

evaluacion_o59 <- function() {
  instancia <- instanciar(especializar(metricas_nucleo()$NoNulo),
                          "personas", "edad")
  medicion <- medir(modelo(instancia), data.frame(edad = c(20, 35, 40)),
                    id_medicion = "o59")
  suppressWarnings(evaluar(medicion, perfil_evaluacion(
    "Evaluador",
    regla_evaluacion("DobleCobertura", function(x) x > 0.5,
                     metricas = c("NoNulo@personas.edad",
                                  "NoNulo@personas.dni"))
  )))
}

test_that("el informe publica la cobertura de las reglas", {
  evaluacion <- evaluacion_o59()
  expect_equal(
    nrow(attr(evaluacion, "cobertura_reglas", exact = TRUE)), 1L
  )

  archivo <- file.path(tempdir(), "o59-evaluacion.html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(evaluacion, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")

  expect_true(grepl("Cobertura de las reglas", html, fixed = TRUE))
  expect_true(grepl("sin_medidas", html, fixed = TRUE))
  # El nombre viaja escapado -el `@` sale como entidad-, asi que se busca la
  # parte que el escape no toca.
  expect_true(grepl("personas.dni", html, fixed = TRUE))
  expect_true(grepl("cubre 1 de 2", html, fixed = TRUE))
})

test_that("una evaluacion sin huecos no publica esa seccion", {
  # El control: si la seccion apareciera siempre, no distinguiria nada.
  instancia <- instanciar(especializar(metricas_nucleo()$NoNulo),
                          "personas", "edad")
  medicion <- medir(modelo(instancia), data.frame(edad = c(20, 35, 40)),
                    id_medicion = "o59b")
  evaluacion <- evaluar(medicion, perfil_evaluacion(
    "Evaluador",
    regla_evaluacion("UnaSola", function(x) x > 0.5,
                     metricas = "NoNulo@personas.edad")
  ))
  expect_null(attr(evaluacion, "cobertura_reglas", exact = TRUE))

  archivo <- file.path(tempdir(), "o59-sin-huecos.html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(evaluacion, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")

  expect_false(grepl("Cobertura de las reglas", html, fixed = TRUE))
  expect_true(grepl("Evaluaciones de reglas", html, fixed = TRUE))
})
