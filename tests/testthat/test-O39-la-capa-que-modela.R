# O39: la salida debe declarar lo que el modelo puede medir y agregar.

test_that("un campo sin valores medibles no es identico", {
  anterior <- data.frame(
    columna = "importe",
    media = NA_real_,
    minimo = NA_real_,
    stringsAsFactors = FALSE
  )
  actual <- data.frame(
    columna = "importe",
    media = NA_real_,
    minimo = 2,
    stringsAsFactors = FALSE
  )

  resultado <- comparar_equivalencia(anterior, actual, tolerancia = 0)
  expect_equal(as.character(resultado$veredicto), c(
    "no_comparable", "no_comparable"
  ))
  expect_equal(resultado$motivo, c(
    "sin_valor_medible_en_ambos", "valor_medible_en_un_solo_lado"
  ))
  expect_equal(attr(resultado, "resumen")[["no_comparable"]], 2L)

  sano <- comparar_equivalencia(
    data.frame(columna = "importe", media = 3, minimo = 1),
    data.frame(columna = "importe", media = 3, minimo = 1),
    tolerancia = 0
  )
  expect_true(all(as.character(sano$veredicto) == "identico"))
})

test_that("madurez reconoce tipo, unidad y rango no acotados", {
  estimaciones <- data.frame(
    celda = c("Montevideo", "Interior"), cv = c(0.08, 0.34),
    n = c(1200L, 90L)
  )
  medidas <- medicion_desde_estimaciones(
    estimaciones, entidad = "ech2024", atributo = "celda", fuente = "survey"
  )
  expect_error(
    evaluar(medidas, perfiles_madurez()$Intermedio),
    "El perfil de madurez usa un umbral en [0, 1] y no puede juzgar una metrica no acotada",
    fixed = TRUE
  )

  por_unidad <- medidas[medidas$metrica == "CoeficienteVariacion", , drop = FALSE]
  por_unidad$unidad <- "casos"
  expect_error(
    evaluar(por_unidad, perfiles_madurez()$Intermedio),
    "no puede juzgar una metrica no acotada", fixed = TRUE
  )

  por_valor <- medidas[medidas$metrica == "TamanoMuestra", , drop = FALSE]
  por_valor$tipo_resultado <- "numero_real"
  por_valor$unidad <- "proporcion"
  expect_error(
    evaluar(por_valor, perfiles_madurez()$Intermedio),
    "no puede juzgar una metrica no acotada", fixed = TRUE
  )

  nucleo <- metricas_nucleo()
  medida_acotada <- medir(
    modelo(instanciar(especializar(nucleo$NoNulo), "t", "dato")),
    data.frame(dato = c(1, 2, NA, 4))
  )
  evaluada <- evaluar(medida_acotada, perfiles_madurez()$Intermedio)
  expect_equal(evaluada$reglas$resultado, 0.75)
})

test_that("conjuntoEntidades declara el promedio sin pesos", {
  nucleo <- metricas_nucleo()
  no_nulo <- especializar(nucleo$NoNulo)
  modelo_dos <- modelo(
    instanciar(no_nulo, "personas", "edad"),
    instanciar(no_nulo, "empresas", "ruc")
  )
  medidas <- medir(modelo_dos, list(
    personas = data.frame(edad = c(rep(NA, 900), 1, 2, 3)),
    empresas = data.frame(ruc = c("1", "2"))
  ))
  por_entidad <- agregar(agregar(medidas, "atributo", "ratio"),
                         "entidad", "promedio")
  sin_pesos <- agregar(por_entidad, "conjuntoEntidades", "promedio")
  expect_equal(sin_pesos$resultado, (3 / 903 + 1) / 2)
  expect_identical(
    sin_pesos$advertencia_agregacion,
    "promedio_sin_pesos: el alcance de cada parte no es conocido por agregar()."
  )

  balanceado <- medir(modelo_dos, list(
    personas = data.frame(edad = c(1, NA)),
    empresas = data.frame(ruc = c("1", "2"))
  ))
  balanceado <- agregar(agregar(balanceado, "atributo", "ratio"),
                        "entidad", "promedio")
  balanceado <- agregar(balanceado, "conjuntoEntidades", "promedio")
  expect_equal(balanceado$resultado, 0.75)
})

test_that("implementada distingue medicion de agregacion", {
  niveles <- granularidades()
  conjunto <- niveles[niveles$granularidad == "conjuntoAtributos", , drop = FALSE]
  expect_true(conjunto$implementada)
  expect_false(conjunto$agregable)
  expect_match(conjunto$motivo_agregacion, "No hay transiciones")
  transiciones <- transiciones_granularidad()
  expect_false(any(
    transiciones$origen == "conjuntoAtributos" |
      transiciones$destino == "conjuntoAtributos"
  ))

  metodo <- function(tablas, instancia) {
    x <- tablas[[instancia$entidad]][[instancia$atributos[[1L]]]]
    filas <- seq_along(x)
    data.frame(
      resultado = as.numeric(!is.na(x)) / 2,
      entidad = instancia$entidad,
      atributo = instancia$atributos[[1L]],
      fila = filas,
      objeto = paste0(instancia$entidad, "$", instancia$atributos[[1L]],
                      "[", filas, "]")
    )
  }
  metrica_prueba <- metrica(
    "PruebaConjuntoAtributos", "prueba", "conjuntoAtributos", "real",
    orientacion = "conformidad", metodo = metodo
  )
  medida <- medir(
    modelo(instanciar(especializar(metrica_prueba), "personas", "edad")),
    data.frame(edad = c(20, NA, 35))
  )
  expect_equal(medida$resultado, c(0.5, 0, 0.5))
  expect_error(agregar(medida, "atributo", "ratio_umbral", umbral = 0.5),
               "No existe una", fixed = TRUE)
})
