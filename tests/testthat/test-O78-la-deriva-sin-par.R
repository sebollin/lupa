# Una deriva vacia porque no hay con que comparar y una vacia porque nada cambio
# eran la misma pantalla.
#
# `detectar_deriva_calidad()` sobre un historico de UNA sola medicion devolvia cero
# filas y ningun atributo -ni uno-, y el informe imprimia la seccion con "No hay
# registros para mostrar." sin decir en ninguna parte que no habia par que comparar.
# Salio de leer los descartes de una refutacion, que lo habia anotado como
# "observacion menor, no acusada" con el motivo de que el objeto no traia ningun
# motivo publicable: eso describia el defecto, no lo excusaba.

deriva_o78 <- function(n_mediciones, nivel = "perfil") {
  instancia <- instanciar(
    especializar(metricas_nucleo()$NoNulo, nombre_especifico = "PresenciaCodigo"),
    "personas", "codigo"
  )
  perfil <- perfil_evaluacion(
    "Basico", regla_evaluacion("Dato presente", function(x) x > 0)
  )
  medir_una <- function(id, fecha) {
    evaluar(
      medir(
        modelo(instancia),
        data.frame(codigo = c("A", NA, "C"), stringsAsFactors = FALSE),
        id_medicion = id, fecha = as.POSIXct(fecha, tz = "UTC")
      ),
      perfil
    )
  }
  evaluaciones <- list(medir_una("enero", "2026-01-31"))
  if (n_mediciones > 1L) {
    evaluaciones <- c(evaluaciones, list(medir_una("febrero", "2026-02-28")))
  }
  detectar_deriva_calidad(
    do.call(historico_calidad, evaluaciones), nivel = nivel
  )
}

test_that("una serie de una sola medicion declara que no hay par que comparar", {
  deriva <- deriva_o78(1L)
  cobertura <- attr(deriva, "cobertura_diagnosticos", exact = TRUE)

  expect_identical(nrow(deriva), 0L)
  expect_true(inherits(cobertura, "data.frame"))
  expect_identical(nrow(cobertura), 1L)
  expect_identical(as.character(cobertura$diagnostico), "detectar_deriva_calidad")
  expect_match(as.character(cobertura$motivo), "una sola medicion")
  expect_match(as.character(cobertura$como_resolverlo), "dos mediciones")
  # El objeto nombrado es legible, no una clave de bytes, y nombra la tabla:
  # la serie es perfil Y tabla, y dos tablas con el mismo perfil daban dos
  # diagnosticos identicos.
  expect_identical(as.character(cobertura$columna), "Basico [personas]")
})

test_that("con dos mediciones no se declara nada de mas", {
  # Mitad de control: la declaracion tiene que aparecer SOLO cuando falta el par. Si
  # apareciera siempre, no diria nada.
  #
  # De las dos aserciones, la primera vale en los dos estados del codigo -es la que
  # hace de control- y la segunda solo se puede evaluar despues del arreglo, porque
  # antes el atributo no existia. El control que mide las dos versiones sin tocar
  # nada interno es el del informe, mas abajo: sin par trae la declaracion y con par
  # no, y eso pasaba igual antes.
  deriva <- deriva_o78(2L)
  cobertura <- attr(deriva, "cobertura_diagnosticos", exact = TRUE)

  expect_gt(nrow(deriva), 0L)
  expect_identical(nrow(cobertura), 0L)
})

test_that("en el nivel de regla el objeto nombra el perfil y la regla", {
  deriva <- deriva_o78(1L, nivel = "regla")
  cobertura <- attr(deriva, "cobertura_diagnosticos", exact = TRUE)

  expect_identical(nrow(cobertura), 1L)
  expect_match(as.character(cobertura$columna), "Basico", fixed = TRUE)
  expect_match(as.character(cobertura$columna), "Dato presente", fixed = TRUE)
})

test_that("el informe publica lo que la deriva no pudo comparar", {
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(deriva_o78(1L), archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")

  expect_true(grepl("no se pudo comparar", html, fixed = TRUE))
  expect_true(grepl("no es conformidad", html, fixed = TRUE))
  expect_true(grepl("una sola medicion", html, fixed = TRUE))
})

test_that("el informe de una deriva con par no trae esa declaracion", {
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(deriva_o78(2L), archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")

  expect_false(grepl("no se pudo comparar", html, fixed = TRUE))
})
