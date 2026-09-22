crear_modelo_o16 <- function(tipo = "real", marco = NULL) {
  metodo <- function(tablas, instancia) data.frame(
    resultado = 1, entidad = "tabla", atributo = "x", fila = NA_integer_,
    objeto = "x"
  )
  m <- metrica(
    "MideA", "a", "atributo", tipo, dimension = "DimA",
    factor = "FactorA", metodo = metodo
  )
  instancia <- instanciar(especializar(m), "tabla", "x")
  modelo(instancia, marco = marco)
}

medir_o16 <- function(modelo_calidad, id, fecha) medir(
  modelo_calidad, data.frame(x = 1), id_medicion = id,
  fecha = as.POSIXct(fecha, tz = "UTC")
)

evaluar_o16 <- function(medida) evaluar(
  medida,
  perfil_evaluacion("Operativo", regla_evaluacion("Pasa", function(x) x >= 1))
)

test_that("el tablero usa el marco declarado en la medicion", {
  marco <- marco_calidad("Marco declarado", list(
    DimA = "FactorA", DimB = "FactorB"
  ))
  medidas <- medir_o16(
    crear_modelo_o16(marco = marco), "marco", "2026-09-22 12:00:00"
  )

  expect_equal(attr(medidas, "marco_calidad", exact = TRUE), marco)
  configuracion <- attr(medidas, "configuracion_modelo", exact = TRUE)
  expect_equal(configuracion$marco$nombre, "Marco declarado")
  expect_equal(
    configuracion$marco$factores,
    as.data.frame(marco)[, c("dimension", "factor"), drop = FALSE]
  )
  expect_identical(unname(configuracion$tipos_resultado), "real")

  automatico <- tablero_calidad(medidas)
  explicito <- tablero_calidad(medidas, marco = marco)
  expect_equal(
    attr(automatico, "alcance", exact = TRUE),
    attr(explicito, "alcance", exact = TRUE)
  )
  expect_equal(attr(automatico, "alcance", exact = TRUE)$factores_marco, 2)
  expect_equal(attr(automatico, "alcance", exact = TRUE)$factores_medidos, 1)
  expect_equal(
    attr(automatico, "alcance", exact = TRUE)$sin_metrica_declarada, 1
  )
})

test_that("cambiar el marco deja la deriva como no comparable", {
  marco_uno <- marco_calidad("Marco uno", list(DimA = "FactorA"))
  marco_dos <- marco_calidad("Marco dos", list(
    DimA = "FactorA", DimB = "FactorB"
  ))
  anterior <- evaluar_o16(medir_o16(
    crear_modelo_o16(marco = marco_uno), "marco-uno", "2026-01-01"
  ))
  actual <- evaluar_o16(medir_o16(
    crear_modelo_o16(marco = marco_dos), "marco-dos", "2026-02-01"
  ))

  deriva <- detectar_deriva_calidad(historico_calidad(anterior, actual))
  fila <- deriva[deriva$aspecto == "configuracion_modelo", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$cambio), "no_comparable")
  expect_identical(as.character(fila$severidad), "error")
  expect_match(fila$descripcion, "marco", ignore.case = TRUE)
  expect_false(any(grepl("se mantuvo", deriva$descripcion, fixed = TRUE)))
})

test_that("cambiar el tipo de resultado deja la deriva como no comparable", {
  anterior <- evaluar_o16(medir_o16(
    crear_modelo_o16(tipo = "real"), "tipo-real", "2026-01-01"
  ))
  actual <- evaluar_o16(medir_o16(
    crear_modelo_o16(tipo = "entero"), "tipo-entero", "2026-02-01"
  ))

  deriva <- detectar_deriva_calidad(historico_calidad(anterior, actual))
  fila <- deriva[deriva$aspecto == "configuracion_modelo", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$cambio), "no_comparable")
  expect_identical(as.character(fila$severidad), "error")
  expect_match(fila$descripcion, "tipo_resultado", fixed = TRUE)
  expect_false(any(grepl("se mantuvo", deriva$descripcion, fixed = TRUE)))
})

test_that("mismo marco y tipos siguen comparandose", {
  marco <- marco_calidad("Marco estable", list(DimA = "FactorA"))
  anterior <- evaluar_o16(medir_o16(
    crear_modelo_o16(marco = marco), "estable-uno", "2026-01-01"
  ))
  actual <- evaluar_o16(medir_o16(
    crear_modelo_o16(marco = marco), "estable-dos", "2026-02-01"
  ))

  deriva <- detectar_deriva_calidad(historico_calidad(anterior, actual))
  expect_false(any(deriva$cambio == "no_comparable", na.rm = TRUE))
  expect_true(any(deriva$aspecto == "resultado"))
})

test_that("una medicion sin marco declarado conserva su camino", {
  medidas <- medir_o16(
    crear_modelo_o16(), "sin-marco", "2026-09-22 12:00:00"
  )

  expect_null(attr(medidas, "marco_calidad", exact = TRUE))
  tablero <- tablero_calidad(medidas)
  alcance <- attr(tablero, "alcance", exact = TRUE)
  expect_equal(alcance$factores_marco, 1)
  expect_equal(alcance$factores_medidos, 1)
  expect_equal(alcance$sin_metrica_declarada, 0)
})
