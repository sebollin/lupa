# La identidad de un registro no puede depender de COMO R marco su texto.
#
# `.clave_bytes()` decide si dos registros son el mismo. Una cadena marcada
# `latin1` y la misma marcada `UTF-8` son el mismo texto con bytes distintos;
# antes de esta guarda daban claves distintas, bajo los dos locales:
#
#   "B\u00e1sico" marcada UTF-8 o sin marca -> =B%C3%A1sico
#   "B\u00e1sico" marcada latin1            -> =B%5C341sico
#
# porque los bytes latin1 no son UTF-8 valido y caian en el escape por bytes.
# Importa fuera del laboratorio porque Windows marca `latin1` donde Linux no, y
# entonces `acumular_historico()` habria rechazado -o duplicado- la misma
# corrida segun la maquina que la guardo.

.n85_marcas <- function(texto) {
  utf8 <- texto
  Encoding(utf8) <- "UTF-8"
  sin_marca <- rawToChar(charToRaw(texto))
  latin1 <- iconv(texto, "UTF-8", "latin1")
  if (is.na(latin1)) return(NULL)
  Encoding(latin1) <- "latin1"
  list(utf8 = utf8, sin_marca = sin_marca, latin1 = latin1)
}

.n85_locales <- function() {
  candidatos <- c(
    "es_UY.UTF-8", "es_UY.utf8", "es_ES.UTF-8", "en_US.UTF-8", "C.UTF-8"
  )
  previo <- Sys.getlocale("LC_CTYPE")
  disponibles <- character()
  for (locale in candidatos) {
    suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
    if (identical(Sys.getlocale("LC_CTYPE"), locale)) {
      disponibles <- locale
      break
    }
  }
  suppressWarnings(Sys.setlocale("LC_CTYPE", previo))
  # `C` va siempre: es el locale donde el defecto se ve, y esta en toda maquina.
  c(disponibles, "C")
}

test_that("la clave de un texto no cambia con la marca de codificacion", {
  previo <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", previo)), add = TRUE)

  # Dos textos: uno que latin1 puede representar -donde el defecto vivia- y uno
  # que no, para que la guarda no se limite al caso comodo.
  textos <- c("B\u00e1sico", "a\u00f1o_medici\u00f3n", "Zebra_2")
  for (texto in textos) {
    marcas <- .n85_marcas(texto)
    if (is.null(marcas)) next
    for (locale in .n85_locales()) {
      suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
      efectivo <- Sys.getlocale("LC_CTYPE")
      claves <- vapply(marcas, .escapar_clave, character(1L))
      expect_equal(
        length(unique(claves)), 1L,
        info = paste0(
          "texto=", texto, " locale=", efectivo, " claves=",
          paste(names(claves), unname(claves), sep = ":", collapse = " ")
        )
      )
    }
  }
})

test_that("marcar `bytes` no aborta la clave", {
  # `enc2utf8()` aborta sobre la marca `bytes`, y debe hacerlo. La clave no
  # puede heredar ese aborto: recibe lo que le den.
  x <- "B\u00e1sico"
  Encoding(x) <- "bytes"
  expect_no_error(.escapar_clave(x))
})

test_that("un historico guardado con marca latin1 es el mismo historico", {
  previo <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", previo)), add = TRUE)

  construir <- function(marcar) {
    nucleo <- metricas_nucleo()
    instancia <- instanciar(
      especializar(nucleo$NoNulo, nombre_especifico = "NoNuloN85"),
      "personas", "dato"
    )
    perfil <- perfil_evaluacion(
      marcar("B\u00e1sico"),
      regla_evaluacion("Presente", function(x) x == 1)
    )
    medidas <- medir(
      modelo(instancia), data.frame(dato = c(1, 1, NA, NA)),
      id_medicion = "Zebra_2",
      fecha = as.POSIXct("2026-09-17 12:00:00", tz = "UTC")
    )
    historico_calidad(suppressWarnings(evaluar(medidas, perfil)))
  }

  como_utf8 <- function(x) {
    Encoding(x) <- "UTF-8"
    x
  }
  como_latin1 <- function(x) {
    y <- iconv(x, "UTF-8", "latin1")
    Encoding(y) <- "latin1"
    y
  }

  for (locale in .n85_locales()) {
    suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
    efectivo <- Sys.getlocale("LC_CTYPE")
    a <- construir(como_utf8)
    b <- construir(como_latin1)
    expect_identical(
      a$id_registro, b$id_registro,
      info = paste("locale =", efectivo)
    )
    # Y la promesa publica: acumular la version latin1 sobre la UTF-8 no
    # inventa registros nuevos ni rechaza los propios.
    expect_no_error(juntos <- acumular_historico(a, b))
    expect_identical(nrow(juntos), nrow(a), info = paste("locale =", efectivo))
  }
})
