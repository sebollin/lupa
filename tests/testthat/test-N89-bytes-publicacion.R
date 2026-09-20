# La vuelta n89: la marca `bytes` en identidad, publicacion y alcance.
#
# Los valores se construyen con `as.raw()` y se comparan por BYTES o por
# `Encoding()`, nunca por como se ven. Las tres veces que hoy me equivoque
# midiendo esto fue por confiar en una impresion: el shell interpreto escapes,
# R parseo octales, y `cat()` colapso barras. Cuando lo que se mide es el manejo
# de la codificacion, el instrumento no puede usar nada que dependa de ella.

.n89_bytes <- function(codigos) {
  x <- rawToChar(as.raw(codigos))
  Encoding(x) <- "bytes"
  x
}

.n89_utf8 <- function(codigos) {
  x <- rawToChar(as.raw(codigos))
  Encoding(x) <- "UTF-8"
  x
}

# "ano" con enie: los cuatro bytes reales.
.N89_SECUENCIA <- c(0x61, 0xc3, 0xb1, 0x6f)
# El texto de diez ASCII que DESCRIBE esa secuencia: a \ 3 0 3 \ 2 6 1 o
.N89_DESCRIPCION <- c(0x61, 0x5c, 0x33, 0x30, 0x33, 0x5c, 0x32, 0x36, 0x31, 0x6f)

.n89_contiene_enie <- function(x) {
  if (is.na(x)) return(FALSE)
  aguja <- as.integer(charToRaw(.n89_utf8(c(0xc3, 0xb1))))
  cuerpo <- as.integer(charToRaw(x))
  n <- length(aguja)
  length(cuerpo) >= n && any(vapply(
    seq_len(length(cuerpo) - n + 1L),
    function(k) identical(cuerpo[k:(k + n - 1L)], aguja), logical(1L)
  ))
}

test_that("la clave es inyectiva: la secuencia y el texto que la describe difieren", {
  secuencia <- .n89_bytes(.N89_SECUENCIA)
  descripcion <- rawToChar(as.raw(.N89_DESCRIPCION))

  # R los distingue; la clave tiene que decir lo mismo que R.
  expect_false(secuencia == descripcion)
  expect_false(identical(.clave_bytes(secuencia), .clave_bytes(descripcion)))

  valores <- c(secuencia, descripcion, .n89_utf8(.N89_SECUENCIA), "z")
  expect_identical(
    length(unique(.clave_bytes(valores))), length(unique(valores))
  )
})

test_that("la clave de un valor no depende de que lo acompanie en el vector", {
  # Con la via rapida de ASCII por delante, la misma cadena salia sin escapar
  # cuando el vector era todo ASCII y escapada cuando no: una clave decidida por
  # la tanda y no por el valor.
  descripcion <- rawToChar(as.raw(.N89_DESCRIPCION))
  acompanante <- .n89_utf8(.N89_SECUENCIA)
  expect_identical(
    .clave_bytes(descripcion),
    .clave_bytes(c(descripcion, acompanante))[[1L]]
  )
  # Y al reves: un vector todo ASCII no cambia por juntarse con otro.
  expect_identical(
    .clave_bytes("hola"), .clave_bytes(c("hola", acompanante))[[1L]]
  )
})

test_that("la clave no aborta ni cambia con bytes invalidos", {
  # El escape de la barra lo hace `gsub(fixed = TRUE)`, que sin `useBytes`
  # ABORTA sobre bytes invalidos -"input string 1 is invalid in this locale"- y
  # ademas elige su estrategia segun QUE MAS haya en el vector, asi que la misma
  # cadena podia salir de dos maneras segun sus vecinas.
  invalido <- rawToChar(as.raw(c(0x41, 0xff, 0x42)))
  expect_no_error(.clave_bytes(invalido))
  acompanante <- .n89_utf8(.N89_SECUENCIA)
  expect_identical(
    .clave_bytes(invalido), .clave_bytes(c(invalido, acompanante))[[1L]]
  )
  # Y por la promesa publica: el perfil describe esa columna en vez de morir.
  perfil <- suppressWarnings(perfilar(
    data.frame(v = c(invalido, invalido, "ok"), stringsAsFactors = FALSE),
    fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  expect_identical(perfil$columnas$n[[1L]], 3L)
  expect_identical(perfil$columnas$n_codificacion_invalida[[1L]], 2L)
})

test_that("la cardinalidad publicada coincide con unique() de R", {
  valores <- c(
    .n89_bytes(.N89_SECUENCIA), rawToChar(as.raw(.N89_DESCRIPCION)),
    .n89_utf8(.N89_SECUENCIA), "z"
  )
  perfil <- suppressWarnings(perfilar(
    data.frame(v = valores, stringsAsFactors = FALSE),
    fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  expect_identical(perfil$columnas$n_distintos[[1L]], length(unique(valores)))
})

test_that("reportar() no aborta con un valor declarado `bytes`", {
  # `nchar(type = "chars")` ABORTA sobre esa marca, y el reporte entero no se
  # generaba. Basta con que el valor sea la moda de una columna.
  crudo <- .n89_bytes(.N89_SECUENCIA)
  roto <- .n89_bytes(c(0x41, 0xff, 0x62))
  for (valores in list(c(crudo, crudo, "z", "z"), c(roto, roto, "z"))) {
    perfil <- suppressWarnings(perfilar(
      data.frame(v = valores, stringsAsFactors = FALSE),
      fecha = as.POSIXct("2026-09-17", tz = "UTC"),
      analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    ))
    archivo <- tempfile(fileext = ".md")
    expect_no_error(suppressWarnings(reportar(perfil, archivo = archivo)))
    unlink(archivo)
  }
})

test_that("el resultado de comparar_equivalencia() se puede imprimir", {
  # La comparacion corria bien y `print()` abortaba con "width is not
  # computable in bytes encoding". Bastaba con que UN lado llevara la marca,
  # aunque los bytes fueran identicos.
  anterior <- data.frame(
    columna = "m", media = 10, minimo = 1,
    moda = I(list(.n89_utf8(.N89_SECUENCIA))), stringsAsFactors = FALSE
  )
  actual <- data.frame(
    columna = "m", media = 10, minimo = 1,
    moda = I(list(.n89_bytes(.N89_SECUENCIA))), stringsAsFactors = FALSE
  )
  resultado <- suppressWarnings(
    comparar_equivalencia(anterior, actual, tolerancia = 1e-9)
  )
  expect_no_error(invisible(utils::capture.output(print(resultado))))
})

test_that("los patrones publicados no interpretan lo declarado `bytes`", {
  crudo <- .n89_bytes(.N89_SECUENCIA)
  perfil <- suppressWarnings(perfilar(
    data.frame(v = c(crudo, crudo, "zz", "zz", "qq"), stringsAsFactors = FALSE),
    fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  tabla <- perfil$patrones$v
  skip_if(is.null(tabla) || !nrow(tabla), "no se publicaron patrones")
  for (celda in c(tabla$patron, tabla$ejemplos)) {
    expect_false(
      .n89_contiene_enie(celda),
      info = paste("patron o ejemplo que interpreta lo declarado:", celda)
    )
  }
  # Y el caso corriente no cambia: sin la marca, el caracter SI se publica.
  normal <- suppressWarnings(perfilar(
    data.frame(
      v = c(.n89_utf8(.N89_SECUENCIA), .n89_utf8(.N89_SECUENCIA), "zz", "zz",
            "qq"),
      stringsAsFactors = FALSE
    ),
    fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  expect_true(any(vapply(
    c(normal$patrones$v$patron, normal$patrones$v$ejemplos),
    .n89_contiene_enie, logical(1L)
  )))
})

test_that("una distribucion que descarto valores no se declara completa", {
  roto <- .n89_bytes(c(0x41, 0xff, 0x62))
  # Tres de cuatro filas no se pueden comparar: la tabla publicaba `z` con
  # proporcion 1.00 -sobre los analizados- y el estado decia `calculada`.
  parcial <- suppressWarnings(distribucion_valores(
    data.frame(r = c(roto, roto, "z", roto), stringsAsFactors = FALSE)
  ))
  alcance <- as.data.frame(parcial$alcance)
  expect_identical(alcance$estado[[1L]], "calculada_parcial")
  expect_true(alcance$n_analizados[[1L]] < alcance$n_total[[1L]])

  # Una columna corriente sigue declarandose completa.
  completa <- suppressWarnings(distribucion_valores(
    data.frame(r = c("a", "a", "z", "b"), stringsAsFactors = FALSE)
  ))
  expect_identical(as.data.frame(completa$alcance)$estado[[1L]], "calculada")

  # Y un `NA` NO es un descarte: es una ausencia declarada, que el perfil ya
  # informa por otro lado. Confundirlos seria declarar parcial lo que esta
  # completo, que es el error simetrico.
  con_na <- suppressWarnings(distribucion_valores(
    data.frame(r = c("a", NA, "z", "a"), stringsAsFactors = FALSE)
  ))
  expect_identical(as.data.frame(con_na$alcance)$estado[[1L]], "calculada")
})
