# El resumen de una columna de numeros escritos como texto lee la misma forma que
# la inferencia cuenta: un telefono con sus miles (`099.123.456`) junto a un
# importe se publicaba como maximo 99123456, y un valor fuera de la muestra de la
# inferencia entraba a la media sin declararse.

.fecha_forma <- as.POSIXct("2026-01-01", tz = "UTC")

.fila_forma <- function(x, ...) {
  p <- perfilar(data.frame(v = x, stringsAsFactors = FALSE),
                fecha = .fecha_forma, ...)
  p$columnas
}

test_that("un grupo de miles que empieza con cero no es un importe en ninguna convencion", {
  # Con un importe solo, el telefono no cuenta y la columna no llega a numero.
  f <- .fila_forma(c("1.234,56", "099.123.456"))
  expect_identical(f$tipo_inferido, "texto")
  expect_true(is.na(f$maximo))
  expect_identical(inferir_tipo(c("1.234,56", "099.123.456"))$tipo, "texto")
  expect_identical(inferir_tipo(c("1,234.56", "099,123,456"))$tipo, "texto")
  # Con nueve importes la columna es numero, y el telefono queda fuera del
  # resumen y se declara.
  f <- .fila_forma(c(rep("1.234,56", 9L), "099.123.456"))
  expect_identical(f$tipo_inferido, "doble")
  expect_identical(f$estado_resumen_cuantitativo, "calculados_sobre_valores")
  expect_identical(f$n_valores_excluidos_resumen, 1L)
  # A mano: el unico importe es 1234,56.
  expect_equal(c(f$minimo, f$maximo, f$media), rep(1234.56, 3L))
  # Con coma decimal el punto agrupa miles, y `0.500` es un cero con grupo.
  f <- .fila_forma(c("1.234,56", "2.345,67", "0.500", "3.456,78", "4.567,89"))
  expect_identical(f$n_valores_excluidos_resumen, 1L)
  expect_equal(f$minimo, 1234.56)
  # Con punto decimal, `0.500` es medio.
  f <- .fila_forma(c("1,234.56", "2,345.67", "0.500", "3,456.78", "4,567.89"))
  expect_identical(f$n_valores_excluidos_resumen, 0L)
  expect_equal(f$minimo, 0.5)
})

test_that("un entero con los miles de la convencion de la columna es un importe", {
  # `1.234.567` junto a `1.234,56` tiene la forma de `12.500.000` pesos: con la
  # coma decimal de la columna, solo se lee de una manera.
  f <- .fila_forma(c("1.234,56", "2.345,67", "12.500.000"))
  expect_identical(f$tipo_inferido, "doble")
  expect_equal(f$maximo, 12500000)
  expect_identical(f$n_valores_excluidos_resumen, 0L)
  # Sin evidencia de convencion no hay lectura: la columna de `1.234.567`
  # sigue como texto.
  f <- .fila_forma(c("1.234.567", "2.345.678", "3.456.789"))
  expect_identical(f$numero_texto_convencion, "ambigua")
  expect_identical(f$tipo_inferido, "texto")
})

test_that("lo que la muestra no vio y no tiene la forma de un numero no entra al resumen", {
  # `.muestrear_vector()` reparte la muestra: con `muestra = 2` sobre tres
  # filas mira la primera y la tercera, y la del medio queda afuera.
  casos <- list(
    telefono = c("1.234,56", "099.123.456", "2.345,67"),
    hexadecimal = c("1234,56", "0x1A", "2345,67"),
    punto_final = c("1234,56", "5.", "2345,67")
  )
  for (nombre in names(casos)) {
    f <- .fila_forma(casos[[nombre]], muestra = 2)
    expect_identical(f$tipo_inferido, "doble", info = nombre)
    expect_identical(f$estado_resumen_cuantitativo, "calculados_sobre_valores",
                     info = nombre)
    expect_identical(f$n_valores_excluidos_resumen, 1L, info = nombre)
    expect_equal(c(f$minimo, f$maximo), c(1234.56, 2345.67), info = nombre)
  }
  # `as.numeric()` lee los tres: 99123456, 26 y 5.
  expect_identical(suppressWarnings(as.numeric(c("0x1A", "5."))), c(26, 5))
})

test_that("NaN e Inf escritos siguen contandose en sus campos", {
  f <- .fila_forma(c("1,5", "2,5", "3,5", "Inf", "4,5"))
  expect_identical(f$n_infinito_positivo, 1L)
  expect_equal(f$maximo, 4.5)
})
