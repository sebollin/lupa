# La forma de digito repetido decidia sola cuando el valor era extremo, y
# marcaba `222` como centinela sin mirar que `221` y `223` estaban al lado.

test_that("un tramo de codigos altos no se confunde con un centinela", {
  # Codigos concentrados abajo y un tramo alto: los tres valores del tramo son
  # igual de frecuentes e igual de lejanos. La unica diferencia entre `222` y
  # sus vecinos es como se escribe, y eso no puede decidir.
  set.seed(6)
  codigos <- c(
    sample.int(50L, 900L, replace = TRUE),
    rep(221L, 8L), rep(222L, 8L), rep(223L, 8L)
  )
  perfil <- perfilar(
    data.frame(cod = codigos), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  expect_true(is.na(perfil$columnas$centinela_valor))
  tipos <- as.character(perfil$hallazgos$tipo_hallazgo)
  expect_false("posible_centinela_numerico" %in% tipos)
})

test_that("el centinela real sigue reconociendose, que es la otra direccion", {
  # Un centinela esta solo: no hay `9998` ni `10000` alrededor. La guarda no
  # puede costar ningun centinela verdadero.
  documentos <- c(seq(10000001, 10000100), rep(9999L, 8L))
  perfil <- perfilar(
    data.frame(doc = documentos), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  expect_equal(perfil$columnas$centinela_valor, 9999)
})

test_that("un vecino suelto no alcanza para descartar el centinela", {
  # El borde de la guarda: un unico `9998` contra ocho `9999` da razon 0,125,
  # muy por debajo del medio que se exige. Si un dato aislado bastara para
  # callar el centinela, la guarda seria peor que el problema que arregla.
  documentos <- c(seq(10000001, 10000100), rep(9999L, 8L), 9998L)
  perfil <- perfilar(
    data.frame(doc = documentos), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  expect_equal(perfil$columnas$centinela_valor, 9999)
})
