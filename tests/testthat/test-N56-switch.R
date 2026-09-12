# N56: las comparaciones de texto no pueden cambiar por la marca de encoding.

suppressPackageStartupMessages(library(lupa))

.texto_n56 <- function(hex) {
  pares <- substring(hex, seq(1L, nchar(hex), by = 2L),
                     seq(2L, nchar(hex), by = 2L))
  rawToChar(as.raw(strtoi(pares, 16L)))
}

.locale_utf8_n56 <- function() {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  candidatos <- c(
    "C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8",
    "es_UY.UTF-8", "es_UY.utf8", "POSIX.UTF-8", "UTF-8"
  )
  for (candidato in candidatos) {
    puesto <- suppressWarnings(tryCatch(
      Sys.setlocale("LC_CTYPE", candidato),
      error = function(e) NA_character_
    ))
    if (length(puesto) == 1L && !is.na(puesto) &&
        grepl("utf-?8", puesto, ignore.case = TRUE)) {
      return(puesto)
    }
  }
  NA_character_
}

test_that("N56: switch sobre factor da la misma respuesta en ambos locales", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  utf8 <- .locale_utf8_n56()
  if (is.na(utf8)) skip("No hay un locale UTF-8 disponible.")

  puesto_c <- suppressWarnings(tryCatch(
    Sys.setlocale("LC_CTYPE", "C"), error = function(e) NA_character_
  ))
  if (!identical(puesto_c, "C")) skip("No se pudo fijar LC_CTYPE=C.")

  factor <- .texto_n56(
    "436f7272656374697475642073656dc3a16e74696361"
  )
  marcado <- factor
  Encoding(marcado) <- "UTF-8"
  locales <- c("C", utf8)
  resultados <- lapply(locales, function(locale) {
    puesto <- suppressWarnings(tryCatch(
      Sys.setlocale("LC_CTYPE", locale),
      error = function(e) NA_character_
    ))
    if (is.na(puesto)) return(NULL)
    marcos <- list(
      agesic = marco_agesic(), cepal = marco_cepal(), iso25012 = marco_iso25012()
    )
    list(
      resolver_sin_marca = lupa:::.resolver_factor("Exactitud", factor),
      resolver_marcado = lupa:::.resolver_factor("Exactitud", marcado),
      genericos = vapply(
        marcos,
        function(marco) sum(grepl(
          "Requiere un backend", marco$factores$como_resolverlo, fixed = TRUE
        )),
        integer(1L)
      )
    )
  })
  if (any(vapply(resultados, is.null, logical(1L)))) {
    skip("No se pudo fijar alguno de los locales seleccionados.")
  }
  expect_identical(resultados[[1L]], resultados[[2L]])
  expect_identical(
    resultados[[1L]]$resolver_sin_marca,
    resultados[[1L]]$resolver_marcado
  )
  expect_identical(
    resultados[[1L]]$resolver_sin_marca,
    "Crear referencial() e instanciar metricas_referencial()."
  )
})

test_that("N56: factor y split por valores de usuario conservan la identidad", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  utf8 <- .locale_utf8_n56()
  if (is.na(utf8)) skip("No hay un locale UTF-8 disponible.")

  factor <- .texto_n56(
    "436f7272656374697475642073656dc3a16e74696361"
  )
  marcado <- factor
  Encoding(marcado) <- "UTF-8"
  resultados <- lapply(c("C", utf8), function(locale) {
    puesto <- suppressWarnings(tryCatch(
      Sys.setlocale("LC_CTYPE", locale),
      error = function(e) NA_character_
    ))
    if (is.na(puesto)) return(NULL)
    datos <- data.frame(valor = c(factor, marcado), stringsAsFactors = FALSE)
    por <- perfilar_por(
      data.frame(grupo = c(factor, marcado), valor = c("x", "x"),
                 stringsAsFactors = FALSE),
      "grupo", min_filas = 1L, analizar_dependencias = FALSE
    )
    list(
      codigos = lupa:::.codigos_filas(datos),
      eta2 = lupa:::.eta2(c(factor, marcado), c(1, 3)),
      n_grupos = attr(por, "n_grupos"),
      conversion = lupa:::.evaluar_conversion(
        c("uno", "dos"), c(factor, marcado), "convertir_tipo",
        list(tipo = "character")
      )
    )
  })
  if (any(vapply(resultados, is.null, logical(1L)))) {
    skip("No se pudo fijar alguno de los locales seleccionados.")
  }
  expect_identical(resultados[[1L]], resultados[[2L]])
  expect_identical(resultados[[1L]]$codigos, c(1L, 1L))
  expect_identical(resultados[[1L]]$conversion$n_colisionados, 2L)
})
