# N96. El render de evidencia no aborta, y las dos capas publican la misma
# cadena.
#
# Dos defectos que salieron de refutacion externa sobre N95:
#
#   * `.escapar_texto_visible()` ABORTABA sobre cualquier texto que no
#     decodificara como UTF-8. `utf8ToInt()` devuelve `NA` -no vacio, no
#     error- y ese `NA` llegaba a un `if`. No era un rincon exotico: una
#     columna `latin1` de un CSV viejo con valores repetidos tumbaba
#     `detectar_claves()` entero, y `latin1` es la codificacion mas comun en
#     datos publicos de la region.
#
#   * El arreglo de N95 cubrio UN llamador del render y dejo el otro. En el
#     mismo perfil, la tabla de patrones publicaba `ca\xc3\xb1o` y el hallazgo
#     `espacios_sobrantes` publicaba `cano` con los acentos armados: quien iba
#     a verificar no encontraba el valor que le mostraron.
#
# Los fixtures se construyen con `rawToChar(as.raw(...))`: la fuente del
# paquete es ASCII y ademas hay que poder elegir la marca, que es la propiedad
# que se mide.

.n96_marcar <- function(bytes, codificacion) {
  s <- rawToChar(as.raw(bytes))
  Encoding(s) <- codificacion
  s
}

test_that("el render no aborta sobre ninguna forma de declarar el texto", {
  casos <- list(
    ascii                = "hola",
    utf8_sin_marca       = .n96_marcar(c(0x6e, 0x69, 0xc3, 0xb1, 0x6f), "unknown"),
    utf8_marcado         = .n96_marcar(c(0x6e, 0x69, 0xc3, 0xb1, 0x6f), "UTF-8"),
    bytes_utf8_valido    = .n96_marcar(c(0x6e, 0x69, 0xc3, 0xb1, 0x6f), "bytes"),
    bytes_invalido       = .n96_marcar(c(0x41, 0xff, 0x42), "bytes"),
    latin1               = .n96_marcar(c(0x6e, 0x69, 0xf1, 0x6f), "latin1"),
    sin_marca_invalido   = .n96_marcar(c(0x63, 0x61, 0x66, 0xe9), "unknown")
  )
  for (nombre in names(casos)) {
    salida <- lupa:::.escapar_texto_visible(casos[[nombre]])
    expect_true(is.character(salida), info = nombre)
    expect_length(salida, 1L)
    expect_false(is.na(salida), info = nombre)
    # Devolver "" seria peor que abortar: afirmaria que el valor es la cadena
    # vacia. Ninguna de estas entradas lo es.
    expect_true(nzchar(salida), info = nombre)
  }
  # `latin1` se convierte sin perdida, no se escapa: R conoce la codificacion.
  expect_false(grepl("\\x", lupa:::.escapar_texto_visible(casos$latin1), fixed = TRUE))
  # Lo que no decodifica se publica como lo imprime R.
  expect_true(grepl("\\x", lupa:::.escapar_texto_visible(casos$sin_marca_invalido),
                    fixed = TRUE))
})

test_that("detectar_claves no aborta con una columna latin1 con colisiones", {
  # La puerta publica por la que el aborto llegaba al usuario.
  columna <- function(valor) {
    data.frame(
      clave = c(paste0("id", sprintf("%02d", seq_len(90L))), rep(valor, 10L)),
      stringsAsFactors = FALSE
    )
  }
  expect_no_error(detectar_claves(columna(.n96_marcar(c(0x6e, 0x69, 0xf1, 0x6f), "latin1"))))
  expect_no_error(detectar_claves(columna(.n96_marcar(c(0x63, 0x61, 0x66, 0xe9), "unknown"))))
  expect_no_error(detectar_claves(columna(.n96_marcar(c(0x6e, 0x69, 0xc3, 0xb1, 0x6f), "bytes"))))
})

test_that("la evidencia de espacios publica la misma cadena que la tabla de patrones", {
  con_espacio <- .n96_marcar(c(0x63, 0x61, 0xc3, 0xb1, 0x6f, 0x20), "bytes")
  sin_espacio <- .n96_marcar(c(0x63, 0x61, 0xc3, 0xb1, 0x6f), "bytes")
  datos <- data.frame(
    id = seq_len(6L),
    col = c(rep(con_espacio, 3L), rep(sin_espacio, 3L)),
    stringsAsFactors = FALSE
  )
  Encoding(datos$col) <- rep("bytes", 6L)
  perfil <- perfilar(datos)
  hallazgo <- hallazgos(perfil)
  evidencia <- as.character(
    hallazgo$evidencia[as.character(hallazgo$tipo) == "espacios_sobrantes"]
  )
  expect_true(length(evidencia) >= 1L)

  ejemplos <- as.character(perfil$patrones$col$ejemplos)
  expect_true(length(ejemplos) >= 1L)
  # La promesa: quien lee el hallazgo puede casar el valor contra la tabla.
  casan <- vapply(
    ejemplos, function(e) grepl(e, evidencia[[1L]], fixed = TRUE), logical(1L)
  )
  expect_true(any(casan))

  # Y no puede traer los octetos crudos: son una declaracion de no interpretar.
  crudos <- rawToChar(as.raw(c(0xc3, 0xb1)))
  expect_false(grepl(crudos, evidencia[[1L]], fixed = TRUE, useBytes = TRUE))
})

test_that("una columna latin1 publica el caracter en la evidencia de espacios", {
  con_espacio <- .n96_marcar(c(0x63, 0x61, 0xf1, 0x6f, 0x20), "latin1")
  sin_espacio <- .n96_marcar(c(0x63, 0x61, 0xf1, 0x6f), "latin1")
  hallazgo <- hallazgos(perfilar(data.frame(
    id = seq_len(6L),
    col = c(rep(con_espacio, 3L), rep(sin_espacio, 3L)),
    stringsAsFactors = FALSE
  )))
  evidencia <- as.character(
    hallazgo$evidencia[as.character(hallazgo$tipo) == "espacios_sobrantes"]
  )
  expect_true(length(evidencia) >= 1L)
  expect_false(grepl("\\x", evidencia[[1L]], fixed = TRUE))
})

test_that("citar un valor no vuelve a escapar lo ya rendido", {
  declarado <- .n96_marcar(c(0x63, 0x61, 0xc3, 0xb1, 0x6f), "bytes")
  citado <- lupa:::.citar_publicable(declarado)
  # `encodeString()` escapa la barra del propio `\xNN` y producia `\\xc3`,
  # que no casa con lo que publica la tabla de patrones.
  expect_false(grepl("\\\\x", citado, fixed = TRUE))
  expect_true(grepl("\\xc3", citado, fixed = TRUE))
  # Sobre texto corriente se sigue usando `encodeString`, que escapa comillas.
  expect_identical(lupa:::.citar_publicable("di \"hola\""), "\"di \\\"hola\\\"\"")
})
