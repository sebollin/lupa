# Vuelta n91: tres arreglos, y los tres salen de que dos informes midieron
# mejor que yo. Valores por `as.raw()`, comparacion por bytes o `Encoding()`.

.n91_bytes <- function(codigos) {
  x <- rawToChar(as.raw(codigos))
  Encoding(x) <- "bytes"
  x
}

.n91_utf8 <- function(codigos) {
  x <- rawToChar(as.raw(codigos))
  Encoding(x) <- "UTF-8"
  x
}

.N91_SEC <- c(0x61, 0xc3, 0xb1, 0x6f)   # "ano" con enie
.N91_O <- c(0xc3, 0xb3)                 # "o" con tilde

test_that("lo publicado en frecuencias no depende de lo que acompania al valor", {
  # La tabla publicaba la CLAVE y no el valor. Como la clave escapa la barra
  # invertida -para ser inyectiva- y la forma publicable ya trae barras, el
  # valor salia con la barra duplicada; y la salida cambiaba segun que mas
  # hubiera en la columna. Contar y mostrar son dos preguntas distintas.
  declarado <- .n91_bytes(.N91_SEC)
  sola <- c(declarado, declarado, "z")
  acompanada <- c(declarado, declarado, "z", .n91_utf8(.N91_O))
  valor_de <- function(v) {
    r <- suppressWarnings(distribucion_valores(
      data.frame(x = v, stringsAsFactors = FALSE)
    ))
    r$frecuencias$valor[[1L]]
  }
  expect_identical(charToRaw(valor_de(sola)), charToRaw(valor_de(acompanada)))
  # Y es la forma que muestra la consola, sin barras duplicadas.
  expect_identical(
    charToRaw(valor_de(sola)), charToRaw(format(declarado, justify = "none"))
  )
})

test_that("las frecuencias publican el valor, no su clave normalizada", {
  r <- suppressWarnings(distribucion_valores(
    data.frame(x = c("Estado", "Estado", "z"), stringsAsFactors = FALSE)
  ))
  expect_true("Estado" %in% r$frecuencias$valor)
})

test_that("con `normalizar = FALSE` no se declara que algo se igualo al normalizar", {
  # La funcion publicaba pares `exacto_normalizado` con
  # `igualo_normalizar = TRUE` incluso con la normalizacion apagada, donde no
  # hay ningun mecanismo declarado para igualar textos. El Rd dice que esa
  # etiqueta significa "coinciden despues de la normalizacion declarada".
  skip_if_not_installed("stringdist")
  declarado <- .n91_bytes(.N91_SEC)
  texto <- .n91_utf8(.N91_SEC)
  valores <- c(declarado, texto, "z", texto)

  sin_normalizar <- suppressWarnings(detectar_duplicados_aproximados(
    data.frame(v = valores, stringsAsFactors = FALSE), "v", normalizar = FALSE
  ))
  expect_false(any(sin_normalizar$pares$igualo_normalizar))
  # Y coincide con R: `sum(duplicated())` sobre la misma columna.
  expect_identical(nrow(sin_normalizar$pares), sum(duplicated(valores)))

  # El control: con texto corriente la etiqueta SI aparece al normalizar, y no
  # aparece sin normalizar. Sin este control, la comprobacion de arriba pasaria
  # tambien si la etiqueta no apareciera nunca.
  corriente <- c("Estado", "estado", "z", "q")
  apagado <- suppressWarnings(detectar_duplicados_aproximados(
    data.frame(v = corriente, stringsAsFactors = FALSE), "v",
    normalizar = FALSE
  ))
  encendido <- suppressWarnings(detectar_duplicados_aproximados(
    data.frame(v = corriente, stringsAsFactors = FALSE), "v", normalizar = TRUE
  ))
  expect_identical(nrow(apagado$pares), 0L)
  expect_gt(nrow(encendido$pares), 0L)
})
