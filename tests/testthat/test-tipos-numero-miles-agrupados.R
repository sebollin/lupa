# Un importe con los miles agrupados -`1.234,56`- es la escritura corriente de
# un monto en Uruguay. La inferencia lo dejaba `texto` y el resumen en
# `no_aplica`, al lado de `numero_texto_seguro = TRUE` y de la convencion que la
# misma fila publicaba; la misma columna sin los puntos de miles salia `doble`
# con su resumen.

.fecha_miles <- as.POSIXct("2026-01-01", tz = "UTC")

.fila_miles <- function(x) {
  p <- perfilar(data.frame(v = x, stringsAsFactors = FALSE),
                fecha = .fecha_miles)
  p$columnas[p$columnas$columna == "v", , drop = FALSE]
}

test_that("los miles agrupados con coma decimal se resumen como la columna sin miles", {
  con_miles <- c("1.234,56", "2.345,67", "3.456,78", "10.000,00", "987,10")
  sin_miles <- c("1234,56", "2345,67", "3456,78", "10000,00", "987,10")
  expect_identical(inferir_tipo(con_miles)$tipo, "doble")
  expect_identical(inferir_tipo(con_miles)$compatibles, 5L)
  f <- .fila_miles(con_miles)
  expect_identical(f$tipo_inferido, "doble")
  expect_identical(f$numero_texto_convencion, "decimal_coma")
  expect_identical(f$estado_resumen_cuantitativo, "calculados")
  # A mano, con la coma decimal que la fila publica.
  valores <- c(1234.56, 2345.67, 3456.78, 10000, 987.1)
  expect_equal(f$minimo, min(valores))
  expect_equal(f$maximo, max(valores))
  expect_equal(f$media, mean(valores))
  expect_identical(f$n_valores_excluidos_resumen, 0L)
  # Y el mismo resumen que la columna gemela sin los puntos de miles.
  g <- .fila_miles(sin_miles)
  campos <- c("tipo_inferido", "minimo", "maximo", "media", "mediana",
              "desvio", "estado_resumen_cuantitativo",
              "numero_texto_convencion", "numero_texto_seguro")
  expect_identical(f[, campos], g[, campos], ignore_attr = TRUE)
})

test_that("los miles agrupados con punto decimal se leen igual, por simetria", {
  x <- c("1,234.56", "2,345.67", "3,456.78")
  expect_identical(inferir_tipo(x)$tipo, "doble")
  f <- .fila_miles(x)
  expect_identical(f$tipo_inferido, "doble")
  expect_identical(f$numero_texto_convencion, "decimal_punto")
  expect_equal(c(f$minimo, f$maximo, f$media), c(1234.56, 3456.78, 2345.67))
})

test_that("un grupo de miles sin decimales se lee con la evidencia de la columna", {
  # `3.456,5` decide la coma decimal; con ella `1.234.567` es un entero.
  x <- c("1.234.567", "2.345.678", "3.456,5")
  f <- .fila_miles(x)
  expect_identical(f$tipo_inferido, "doble")
  expect_identical(f$numero_texto_convencion, "decimal_coma")
  expect_equal(c(f$minimo, f$maximo), c(3456.5, 2345678))
})

test_that("sin evidencia de una sola convencion, el separador de tres digitos sigue sin leerse", {
  # `1.500` puede ser mil quinientos o uno y medio: la ambiguedad documentada
  # no cambia.
  f <- .fila_miles(c("1.500", "2.300", "4.100"))
  expect_identical(f$tipo_inferido, "texto")
  expect_identical(f$numero_texto_convencion, "ambigua")
  expect_identical(f$estado_resumen_cuantitativo, "no_aplica")
  expect_true(is.na(f$media))
  # Un valor que no agrupa de a tres no tiene lectura con la coma decimal y no
  # cuenta como numero.
  expect_identical(inferir_tipo(c("1.23,5", "2.34,5", "3.45,5"))$tipo, "texto")
  # Con evidencia de las dos convenciones el plan bloquea la conversion por
  # falta de evidencia, y la columna sigue `texto`: pasarla a `doble` publicaba
  # que convertirla no pierde nada.
  mixta <- .fila_miles(c("1.234,56", "2.345,67", "1,234.56", "2,345.67"))
  expect_identical(mixta$numero_texto_convencion, "mixta")
  expect_identical(mixta$tipo_inferido, "texto")
})
