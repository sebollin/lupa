# Una metrica por celda mide SOLO las celdas con valor: sobre cuatro celdas con
# una ausente publica tres medidas, el agregado se calcula sobre esas tres y nada
# decia cuantas quedaron afuera. El extremo estaba cubierto -sin ningun valor, la
# metrica va a `cobertura_metricas` con `sin_valores`- y el caso parcial pasaba en
# silencio: el tablero publicaba `valor = 0.667` con `universo = "celdas"`, que no
# se distingue de un 0.667 sobre las cuatro.

medicion_o60 <- function(valores = c("a", NA, "c", "zz")) {
  instancia <- instanciar(
    especializar(metricas_nucleo()$Formato, expresion_regular = "^[a-c]$"),
    "t", "v"
  )
  medir(modelo(instancia), data.frame(v = valores, stringsAsFactors = FALSE),
        id_medicion = "o60")
}

test_that("la medicion declara cuantas celdas midio de cuantas", {
  medicion <- medicion_o60()
  alcance <- attr(medicion, "alcance_medidas", exact = TRUE)

  expect_equal(nrow(medicion), 3L)
  expect_s3_class(alcance, "data.frame")
  expect_equal(nrow(alcance), 1L)
  expect_identical(alcance$metrica_instanciada, "Formato@t.v")
  expect_equal(alcance$en_el_universo, 4)
  expect_equal(alcance$medidas, 3)
  expect_identical(alcance$unidad, "celda")
  expect_true(grepl("3 de 4", alcance$motivo, fixed = TRUE))
})

test_that("una columna sin ausentes no declara alcance parcial", {
  # El control: si el atributo apareciera siempre, no distinguiria nada.
  medicion <- medicion_o60(c("a", "b", "c", "zz"))
  expect_equal(nrow(medicion), 4L)
  expect_null(attr(medicion, "alcance_medidas", exact = TRUE))
})

test_that("sin ningun valor sigue siendo cobertura_metricas, no alcance", {
  # Los dos estados son afirmaciones distintas: "no se pudo medir" y "se midio
  # una parte". La del extremo ya existia y no cambia.
  medicion <- medicion_o60(rep(NA_character_, 3))
  expect_equal(nrow(medicion), 0L)
  cobertura <- attr(medicion, "cobertura_metricas", exact = TRUE)
  expect_s3_class(cobertura, "data.frame")
  expect_identical(as.character(cobertura$estado), "sin_valores")
  expect_null(attr(medicion, "alcance_medidas", exact = TRUE))
})

test_that("el alcance parcial viaja al tablero y a sus dos salidas", {
  medicion <- medicion_o60()
  tablero <- tablero_calidad(medicion)
  alcance <- attr(tablero, "alcance_medidas", exact = TRUE)

  expect_s3_class(alcance, "data.frame")
  expect_equal(alcance$medidas, 3)

  salida <- NULL
  invisible(capture.output(salida <- cli::cli_fmt(print(tablero))))
  expect_true(any(grepl("Alcance de las medidas", salida)))

  # `reportar()` recibe la medicion -no el tablero suelto-, y ahi tiene que
  # publicarlo: es el documento que se manda a otra persona.
  archivo <- file.path(tempdir(), "o60-medicion.html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(medicion, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")
  expect_true(grepl("Alcance de las medidas", html, fixed = TRUE))
  expect_true(grepl("midi", html))
})

test_that("la impresion de la medicion lo anuncia", {
  medicion <- medicion_o60()
  salida <- NULL
  invisible(capture.output(salida <- cli::cli_fmt(print(medicion))))
  texto <- paste(salida, collapse = " ")

  expect_true(grepl("universo aplicable", texto, fixed = TRUE))
  expect_true(grepl("3 de 4", texto, fixed = TRUE))
})
