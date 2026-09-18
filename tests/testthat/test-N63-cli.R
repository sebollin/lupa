# N63: `cli` recibe textos desconocidos como bytes y los escapa bajo LC_CTYPE=C.

.sin_marca_n63 <- function(x) rawToChar(charToRaw(x))

.fijar_locale_n63 <- function(locale) {
  categorias <- c("LC_CTYPE", "LC_COLLATE")
  for (categoria in categorias) {
    suppressWarnings(Sys.setlocale(categoria, locale))
  }
  identical(vapply(categorias, Sys.getlocale, character(1L)),
            setNames(rep(locale, length(categorias)), categorias))
}

.normalizar_escape_cli_n63 <- function(salida) {
  lapply(salida, function(linea) {
    reemplazar <- function(linea, patron, extraer) {
      posiciones <- gregexpr(patron, linea, perl = TRUE)
      encontrados <- regmatches(linea, posiciones)[[1L]]
      if (!length(encontrados) || encontrados[[1L]] == "") return(linea)
      reemplazos <- vapply(encontrados, function(escapado) {
        codigo <- extraer(escapado)
        intToUtf8(strtoi(codigo, base = 16L))
      }, character(1L))
      regmatches(linea, posiciones) <- list(reemplazos)
      linea
    }
    linea <- reemplazar(linea, "<U\\+[0-9A-F]+>", function(escapado) {
      sub("<U\\+", "", sub(">$", "", escapado))
    })
    linea <- reemplazar(linea, "\\\\u[0-9a-fA-F]{4,6}", function(escapado) {
      sub("^\\\\u", "", escapado)
    })
    gsub(intToUtf8(0x2500), "-", linea, fixed = TRUE)
  })
}

test_that("los textos de usuario que publica cli no vuelven a bytes crudos", {
  categorias <- c("LC_CTYPE", "LC_COLLATE")
  anteriores <- setNames(vapply(categorias, Sys.getlocale, character(1L)), categorias)
  on.exit(for (categoria in categorias) {
    suppressWarnings(Sys.setlocale(categoria, anteriores[[categoria]]))
  }, add = TRUE)

  locales_utf8 <- c(
    "es_UY.UTF-8", "es_UY.utf8", "es_ES.UTF-8", "es_ES.utf8",
    "en_US.UTF-8", "en_US.utf8", "C.UTF-8", "C.utf8"
  )
  locale_utf8 <- locales_utf8[
    vapply(locales_utf8, .fijar_locale_n63, logical(1L))
  ]
  if (!length(locale_utf8)) {
    skip("no hay un locale UTF-8 disponible en esta maquina")
  }
  locale_utf8 <- locale_utf8[[1L]]
  expect_true(.fijar_locale_n63(locale_utf8))

  senal <- senal_redundante(
    c("col_a", "col_b"), nombre = .sin_marca_n63("se\u00f1al_medici\u00f3n")
  )
  organizacion <- organizacion(
    .sin_marca_n63("organizaci\u00f3n"), .sin_marca_n63("tabla_a\u00f1o")
  )
  marco <- marco_calidad(
    .sin_marca_n63("marco_a\u00f1o"),
    data.frame(
      dimension = .sin_marca_n63("dimensi\u00f3n"),
      factor = .sin_marca_n63("factor_\u00f1"),
      stringsAsFactors = FALSE
    )
  )
  marco$origen <- .sin_marca_n63("origen_medici\u00f3n")
  objetos <- list(senal = senal, organizacion = organizacion, marco = marco)

  salidas <- lapply(c(locale_utf8, "C"), function(locale) {
    expect_true(.fijar_locale_n63(locale))
    lapply(objetos, function(objeto) {
      tryCatch(
        capture.output(print(objeto), type = "message"),
        error = function(e) e
      )
    })
  })
  expect_false(any(vapply(unlist(salidas, recursive = FALSE), inherits,
                          logical(1L), what = "error")))

  for (salida in salidas) {
    for (lineas in salida) {
      texto <- paste(lineas, collapse = "\n")
      expect_false(grepl("<c3>|<b1>|<b3>", texto, perl = TRUE))
    }
  }
  expect_identical(
    lapply(salidas[[1L]], .normalizar_escape_cli_n63),
    lapply(salidas[[2L]], .normalizar_escape_cli_n63)
  )
})

test_that("el punto de marcado solo declara UTF-8 valido para cli", {
  valido <- .sin_marca_n63("a\u00f1o")
  invalido <- rawToChar(as.raw(c(0x61, 0xff, 0x6f)))
  resultado <- .marcar_objeto_para_exhibir(list(valido = valido, invalido = invalido))

  expect_identical(charToRaw(resultado$valido), charToRaw(valido))
  expect_identical(Encoding(resultado$valido), "UTF-8")
  expect_identical(charToRaw(resultado$invalido), charToRaw(invalido))
  expect_identical(Encoding(resultado$invalido), Encoding(invalido))
})
