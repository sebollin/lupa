# N55: los identificadores declarados se comparan por sus bytes.

suppressPackageStartupMessages(library(lupa))

.texto_n55 <- function(hex) {
  pares <- substring(hex, seq(1L, nchar(hex), by = 2L),
                     seq(2L, nchar(hex), by = 2L))
  rawToChar(as.raw(strtoi(pares, 16L)))
}

.precisio_n55 <- function() .texto_n55("50726563697369c3b36e")

.precisio_descompuesto_n55 <- function() {
  paste0(.texto_n55("50726563697369"), intToUtf8(769L), "n")
}

.localidades_N55 <- function() c("es_UY.UTF-8", "C")

test_that("N55.1: modelo acepta declaraciones unknown bajo cualquier locale", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  nucleo <- lupa::metricas_nucleo()
  instancia <- lupa::instanciar(
    lupa::especializar(nucleo$ErrorEstandar),
    entidad = "personas", atributos = "edad"
  )
  for (locale in .localidades_N55()) {
    puesto <- suppressWarnings(
      # `Sys.setlocale()` AVISA cuando el locale no existe, y `try()` no
      # silencia avisos: en toda maquina sin ese locale -CI, el contenedor y
      # CRAN- estas pruebas ensuciaban la salida con `WARN 6` en las cinco
      # plataformas. El aviso no aporta nada: la decision se toma mirando el
      # valor devuelto.
      try(Sys.setlocale("LC_CTYPE", locale), silent = TRUE)
    )
    if (!identical(puesto, locale)) next
    factor <- .precisio_n55()
    marco <- lupa::marco_calidad(
      "Marco de prueba",
      data.frame(
        dimension = "Exactitud", factor = factor,
        stringsAsFactors = FALSE
      )
    )
    expect_no_error(lupa::modelo(instancia, marco = marco))
    expect_identical(
      charToRaw(marco$factores$factor[[1L]]), charToRaw(factor)
    )
    expect_identical(
      charToRaw(marco$factores$factor[[1L]]),
      charToRaw(.precisio_n55())
    )
  }
})

test_that("N55.2: declaraciones con bytes distintos siguen separadas", {
  compuesto <- .precisio_n55()
  descompuesto <- .precisio_descompuesto_n55()
  expect_false(identical(charToRaw(compuesto), charToRaw(descompuesto)))
  marco <- lupa::marco_calidad(
    "Marco de prueba",
    data.frame(
      dimension = c("Exactitud", "Exactitud"),
      factor = c(compuesto, descompuesto),
      stringsAsFactors = FALSE
    )
  )
  expect_length(marco$factores$factor, 2L)
  expect_identical(
    lapply(marco$factores$factor, charToRaw),
    list(charToRaw(compuesto), charToRaw(descompuesto))
  )
})
