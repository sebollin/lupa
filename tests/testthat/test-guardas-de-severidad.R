# Guardas escritas a partir de un banco de mutación: se rompe a propósito un
# comportamiento sobre una copia del paquete y se comprueba que la suite se
# entera. Una prueba que sigue pasando con el comportamiento roto no prueba nada.
#
# Estas nacieron de mutantes que **sobrevivieron la suite entera** —13.379
# aserciones— y que por lo tanto no estaban guardados por nada.

test_that("la severidad de faltantes escala a error por encima del umbral", {
  # Mutante que sobrevivía: forzar a FALSE la comparación con
  # `umbral_faltantes_error` deja la severidad siempre en `sospechoso`, y hasta
  # ahora nada lo detectaba. La escala `ok < sospechoso < error` es la que
  # decide qué se automatiza, así que degradarla en silencio es de las cosas más
  # caras que pueden pasar.
  faltantes_de <- function(datos) {
    hallazgos <- perfilar(datos)$hallazgos
    hallazgos[as.character(hallazgos$tipo_hallazgo) == "faltantes", , drop = FALSE]
  }

  # Por encima de `umbral_faltantes_error` (0,4 por omisión): error.
  muchos <- faltantes_de(data.frame(casi_vacia = c(1, rep(NA_real_, 19L))))
  expect_equal(nrow(muchos), 1L)
  expect_equal(as.character(muchos$severidad), "error")

  # Entre los dos umbrales: sospechoso, no error.
  intermedios <- faltantes_de(
    data.frame(media = c(rep(1, 14L), rep(NA_real_, 6L)))
  )
  expect_equal(nrow(intermedios), 1L)
  expect_equal(as.character(intermedios$severidad), "sospechoso")

  # Por debajo del umbral inferior: sin hallazgo de faltantes.
  pocos <- faltantes_de(data.frame(casi_llena = c(rep(1, 99L), NA_real_)))
  expect_equal(nrow(pocos), 0L)

  # Y los umbrales se respetan cuando el usuario los cambia: con un umbral de
  # error alto, la misma columna deja de ser `error`.
  hallazgos <- perfilar(
    data.frame(casi_vacia = c(1, rep(NA_real_, 19L))),
    umbral_faltantes_error = 0.99
  )$hallazgos
  degradado <- hallazgos[
    as.character(hallazgos$tipo_hallazgo) == "faltantes", , drop = FALSE
  ]
  expect_equal(as.character(degradado$severidad), "sospechoso")
})

test_that("la igualdad exacta en el umbral no escala la severidad", {
  # El contrato documentado: `umbral_faltantes_error` escala **al superarlo**, y
  # la igualdad conserva la severidad sospechosa. Sin esta prueba, cambiar `>`
  # por `>=` pasaría inadvertido.
  datos <- data.frame(mitad = c(rep(1, 12L), rep(NA_real_, 8L)))
  hallazgos <- perfilar(datos, umbral_faltantes_error = 0.4)$hallazgos
  fila <- hallazgos[
    as.character(hallazgos$tipo_hallazgo) == "faltantes", , drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  # 8 de 20 es exactamente 0,4: la igualdad no escala.
  expect_equal(as.character(fila$severidad), "sospechoso")
})
