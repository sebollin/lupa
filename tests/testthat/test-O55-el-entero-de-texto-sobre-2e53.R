# Un entero escrito como TEXTO por encima de 2^53 no sobrevive al doble, y el
# resumen publicaba el redondeo como si fuera el dato: sobre tres impares
# consecutivos -...93, ...94, ...95- se publicaba `minimo = ...92` y
# `maximo = ...96`, dos valores que ninguna fila contiene, con
# `estado_resumen_cuantitativo = "calculados"`. La misma columna guardada como
# `integer64` recibia la respuesta correcta desde el primer dia: medidas en `NA`,
# extremos exactos y `omitidos_precision`. Dos caminos, un dato, dos respuestas.

valores_o55 <- function() {
  c("9007199254740993", "9007199254740994", "9007199254740995")
}

perfil_o55 <- function(x) {
  perfilar(data.frame(codigo = x, stringsAsFactors = FALSE),
           analizar_dependencias = FALSE)$columnas
}

test_that("el texto sobre 2^53 no publica un extremo que no existe", {
  fila <- perfil_o55(valores_o55())

  expect_identical(as.character(fila$tipo_inferido), "entero")
  expect_true(is.na(fila$minimo))
  expect_true(is.na(fila$maximo))
  expect_identical(fila$minimo_exacto, "9007199254740993")
  expect_identical(fila$maximo_exacto, "9007199254740995")
  expect_identical(
    as.character(fila$estado_resumen_cuantitativo), "omitidos_precision"
  )
})

test_that("el limite se prueba donde cambia la respuesta", {
  # 2^53 - 1 es exacto en doble: ahi las medidas se calculan. Un valor mas y ya
  # no. La guarda se prueba en el borde, no con un numero comodo.
  seguro <- perfil_o55(c("9007199254740989", "9007199254740990",
                         "9007199254740991"))
  expect_false(is.na(seguro$minimo))
  expect_equal(seguro$maximo, 9007199254740991)
  expect_identical(
    as.character(seguro$estado_resumen_cuantitativo), "calculados"
  )

  apenas_afuera <- perfil_o55(c("9007199254740991", "9007199254740992",
                               "9007199254740993"))
  expect_true(is.na(apenas_afuera$maximo))
  expect_identical(apenas_afuera$maximo_exacto, "9007199254740993")
  expect_identical(
    as.character(apenas_afuera$estado_resumen_cuantitativo),
    "omitidos_precision"
  )
})

test_that("los negativos se ordenan como numeros y no como texto", {
  fila <- perfil_o55(c("-9007199254740993", "-9007199254740995", "10"))

  expect_identical(fila$minimo_exacto, "-9007199254740995")
  expect_identical(fila$maximo_exacto, "10")
  expect_equal(fila$n_negativos, 2)
})

test_that("una columna que entra en el doble no cambia de conducta", {
  # El control: si la guarda se disparara siempre, este resumen perderia sus
  # medidas y la prueba lo diria.
  fila <- perfil_o55(c("10", "20", "30"))

  expect_equal(fila$minimo, 10)
  expect_equal(fila$maximo, 30)
  expect_true(is.na(fila$minimo_exacto))
  expect_identical(as.character(fila$estado_resumen_cuantitativo), "calculados")
})

test_that("el texto y el integer64 dicen lo mismo del mismo dato", {
  skip_if_not_installed("bit64")
  texto <- perfil_o55(valores_o55())
  entero <- perfilar(
    data.frame(codigo = bit64::as.integer64(valores_o55())),
    analizar_dependencias = FALSE
  )$columnas

  expect_identical(texto$minimo_exacto, entero$minimo_exacto)
  expect_identical(texto$maximo_exacto, entero$maximo_exacto)
  expect_identical(
    as.character(texto$estado_resumen_cuantitativo),
    as.character(entero$estado_resumen_cuantitativo)
  )
})
