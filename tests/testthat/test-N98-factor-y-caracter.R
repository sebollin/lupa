# N98. El veredicto no puede depender de la forma fisica de la tabla.
#
# `.texto_publicable()` preguntaba `is.character(x)`, que da FALSE para un
# factor porque el texto vive en los NIVELES. La columna salia sin rendir, y
# los mismos datos guardados como factor y como character daban perfiles
# distintos: la tabla de patrones perdia las filas del valor declarado -cinco
# de veinte- y publicaba `proporcion = 1.000` para el unico patron que
# quedaba. Informar 100 % sobre el 75 % es informar como completo lo que es
# parcial, que es justo lo que el paquete promete no hacer.
#
# El factor se construye ANTES de marcar los niveles: `factor()` ordena sus
# niveles con `order()`, que aborta sobre una cadena marcada `bytes`. Es una
# mina de R, no del paquete, pero decide como se arma el fixture.

.n98_datos <- function() {
  roto <- rawToChar(as.raw(c(0x63, 0x61, 0xf1, 0x6f, 0x6e)))
  como_factor <- factor(rep(c("CASA", roto), times = c(15L, 5L)))
  Encoding(levels(como_factor)) <- "bytes"
  Encoding(roto) <- "bytes"
  texto <- c(rep("CASA", 15L), rep(roto, 5L))
  Encoding(texto) <- c(rep("unknown", 15L), rep("bytes", 5L))
  list(
    factor = data.frame(t = como_factor, id = seq_len(20L)),
    caracter = data.frame(t = texto, id = seq_len(20L), stringsAsFactors = FALSE)
  )
}

test_that("un factor y un character con los mismos datos dan la misma tabla de patrones", {
  datos <- .n98_datos()
  uno <- perfilar(datos$factor, proteger_datos_personales = FALSE)$patrones$t
  otro <- perfilar(datos$caracter, proteger_datos_personales = FALSE)$patrones$t

  expect_identical(nrow(uno), nrow(otro))
  expect_identical(as.character(uno$patron), as.character(otro$patron))
  expect_identical(uno$n, otro$n)
  expect_identical(uno$proporcion, otro$proporcion)
  expect_identical(as.character(uno$ejemplos), as.character(otro$ejemplos))
})

test_that("la proporcion de un patron no se infla cuando faltan valores", {
  datos <- .n98_datos()
  for (forma in names(datos)) {
    tabla <- perfilar(datos[[forma]], proteger_datos_personales = FALSE)$patrones$t
    # Con cinco de veinte valores declarados, ningun patron cubre el total.
    expect_true(nrow(tabla) >= 2L, info = forma)
    expect_false(any(tabla$proporcion == 1), info = forma)
    expect_equal(sum(tabla$n), 20L, info = forma)
  }
})

test_that("rendir un factor devuelve un factor y conserva sus valores", {
  roto <- rawToChar(as.raw(c(0x63, 0x61, 0xf1, 0x6f)))
  f <- factor(c("a", roto, "a"))
  Encoding(levels(f)) <- c("unknown", "bytes")[match(levels(f), c("a", roto))]
  rendido <- lupa:::.texto_publicable(f)
  expect_s3_class(rendido, "factor")
  expect_length(rendido, 3L)
  # El nivel declarado sale escapado; el corriente queda igual.
  expect_true(any(grepl("\\x", levels(rendido), fixed = TRUE)))
  expect_true("a" %in% levels(rendido))
  # Un factor sin niveles no rompe.
  expect_s3_class(lupa:::.texto_publicable(factor(character())), "factor")
})
