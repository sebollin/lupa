# Un campo que uno de los dos lados no mide no se puede comparar, y hasta aca no
# aparecia en ninguna parte.
#
# `comparar_equivalencia()` compara por interseccion. Un campo REGISTRADO presente
# en un solo lado se caia de las dos listas: no entraba en `campos` -la
# interseccion- ni en `campos_no_comparables` -que solo recogia los campos sin eje
# registrado-, asi que no habia fila, ni detalle, ni diagnostico, ni proteccion. El
# `resumen` contaba los campos que si pudo comparar como si fueran el universo
# entero. Es el invariante central del paquete: un resumen sobre parte declara lo
# que quedo afuera.

test_that("un campo registrado presente en un solo lado queda declarado", {
  anterior <- data.frame(
    columna = "monto", media = 10, minimo = 1, stringsAsFactors = FALSE
  )
  actual <- data.frame(columna = "monto", minimo = 2, stringsAsFactors = FALSE)

  equivalencia <- comparar_equivalencia(anterior, actual, tolerancia = 1e-9)

  expect_true("media" %in% attr(equivalencia, "campos_no_comparables"))
  detalle <- attr(equivalencia, "detalle_campos_no_comparables")
  fila <- detalle[as.character(detalle$campo) == "media", , drop = FALSE]
  expect_identical(nrow(fila), 1L)
  expect_identical(as.character(fila$motivo), "campo_solo_en_anterior")
  # No se le atribuye a una columna: falta en el resumen entero.
  expect_true(is.na(fila$columna))
  cobertura <- attr(equivalencia, "cobertura_diagnosticos")
  expect_true(any(grepl(
    "campo_solo_en_anterior: media", cobertura$motivo, fixed = TRUE
  )))
})

test_that("el campo que falta del otro lado se declara con su propio motivo", {
  anterior <- data.frame(columna = "monto", minimo = 1, stringsAsFactors = FALSE)
  actual <- data.frame(
    columna = "monto", minimo = 2, mediana = 5, stringsAsFactors = FALSE
  )

  equivalencia <- comparar_equivalencia(anterior, actual, tolerancia = 1e-9)

  detalle <- attr(equivalencia, "detalle_campos_no_comparables")
  fila <- detalle[as.character(detalle$campo) == "mediana", , drop = FALSE]
  expect_identical(as.character(fila$motivo), "campo_solo_en_actual")
  expect_true("mediana" %in% attr(equivalencia, "campos_no_comparables"))
})

test_that("un campo medido en los dos lados sigue comparandose, no declarandose", {
  # La mitad de control: la declaracion nueva no debe tragarse un campo comparable.
  anterior <- data.frame(
    columna = "monto", media = 10, minimo = 1, stringsAsFactors = FALSE
  )
  actual <- data.frame(
    columna = "monto", media = 11, minimo = 1, stringsAsFactors = FALSE
  )

  equivalencia <- comparar_equivalencia(anterior, actual, tolerancia = 1e-9)

  expect_true("media" %in% as.character(equivalencia$campo))
  expect_false("media" %in% attr(equivalencia, "campos_no_comparables"))
  detalle <- attr(equivalencia, "detalle_campos_no_comparables")
  expect_false("media" %in% as.character(detalle$campo))
})

test_that("un campo SIN eje registrado sigue declarandose como antes", {
  # El otro control: los campos no registrados ya se declaraban, y la vuelta nueva
  # no tiene que duplicarlos ni cambiarles el motivo.
  anterior <- data.frame(
    columna = "monto", minimo = 1, inventado = 3, stringsAsFactors = FALSE
  )
  actual <- data.frame(columna = "monto", minimo = 2, stringsAsFactors = FALSE)

  equivalencia <- comparar_equivalencia(anterior, actual, tolerancia = 1e-9)

  expect_true("inventado" %in% attr(equivalencia, "campos_no_comparables"))
  detalle <- attr(equivalencia, "detalle_campos_no_comparables")
  expect_false("inventado" %in% as.character(detalle$campo))
})
