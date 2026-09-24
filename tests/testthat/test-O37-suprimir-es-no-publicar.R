# O37: `desenlace = "suprimir"` declara que las medidas que no cumplen la
# condicion **no deben publicarse**, y el informe enmascaraba solo su
# `valor_medido`: dos secciones mas abajo, la tabla de medidas publicaba el
# resultado de la misma medida. Suprimir a medias no suprime.

.evaluacion_con_supresion <- function() {
  nucleo <- metricas_nucleo()
  instancia <- instanciar(especializar(nucleo$NoNulo), "personas", "edad")
  medicion <- medir(modelo(instancia), data.frame(edad = c(20, NA, 35)),
                    id_medicion = "enero")
  regla <- regla_evaluacion("Presente", function(x) x > 0,
                            desenlace = "suprimir")
  evaluar(medicion, perfil_evaluacion("Basico", regla))
}

test_that("el informe no publica el resultado de una medida suprimida", {
  evaluacion <- .evaluacion_con_supresion()
  suprimida <- as.character(
    evaluacion$desenlaces$id_medida[evaluacion$desenlaces$desenlace == "suprimir"]
  )
  expect_equal(length(suprimida), 1L)

  destino <- tempfile(fileext = ".html")
  on.exit(unlink(destino), add = TRUE)
  suppressMessages(reportar(evaluacion, archivo = destino))
  html <- paste(readLines(destino, warn = FALSE), collapse = " ")
  filas <- unlist(strsplit(html, "<tr", fixed = TRUE))
  filas <- filas[grepl(suprimida, filas, fixed = TRUE)]

  expect_true(length(filas) >= 1L)
  # Ninguna fila del informe que nombre la medida puede traer su desenlace.
  for (fila in filas) {
    celdas <- gsub("<[^>]+>", "|", fila)
    expect_false(grepl("|no|", celdas, fixed = TRUE))
    expect_false(grepl("|si|", celdas, fixed = TRUE))
  }
  expect_true(any(grepl("valor suprimido", filas, fixed = TRUE)))
})

test_that("el objeto conserva el resultado y la fila no desaparece", {
  # La supresion es de la publicacion, no de la medicion: el objeto sigue
  # teniendo el resultado y el conteo de medidas no cambia.
  evaluacion <- .evaluacion_con_supresion()

  expect_equal(nrow(evaluacion$medidas), 3L)
  expect_true(is.logical(evaluacion$medidas$resultado))
  expect_false(all(evaluacion$medidas$resultado))
})

test_that("sin desenlace declarado el informe publica todo como antes", {
  # Control: lo que enmascara es la supresion declarada, no la evaluacion.
  nucleo <- metricas_nucleo()
  instancia <- instanciar(especializar(nucleo$NoNulo), "personas", "edad")
  medicion <- medir(modelo(instancia), data.frame(edad = c(20, NA, 35)),
                    id_medicion = "enero")
  evaluacion <- evaluar(
    medicion,
    perfil_evaluacion("Basico", regla_evaluacion("Presente", function(x) x > 0))
  )

  destino <- tempfile(fileext = ".html")
  on.exit(unlink(destino), add = TRUE)
  suppressMessages(reportar(evaluacion, archivo = destino))
  html <- paste(readLines(destino, warn = FALSE), collapse = " ")

  expect_false(grepl("valor suprimido", html, fixed = TRUE))
})
