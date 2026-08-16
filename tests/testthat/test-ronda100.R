test_that("una regla agregada declara umbral, veredicto y universo", {
  nucleo <- metricas_nucleo()
  presente <- especializar(
    nucleo$NoNulo, nombre_especifico = "PresenteR100"
  )
  medida_a <- instanciar(presente, "tabla", "a")
  medida_b <- instanciar(presente, "tabla", "b")
  datos <- data.frame(
    a = c(rep(NA, 4L), 5:15),
    b = c(rep(NA, 4L), 5:15)
  )
  medicion <- medir(
    modelo(medida_a, medida_b), datos, id_medicion = "ronda-100"
  )
  regla <- regla_evaluacion(
    "70pc", function(m) m == 1,
    metricas = unique(medicion$metrica_instanciada),
    proporcion_minima = 0.7
  )
  evaluacion <- evaluar(
    medicion, perfil_evaluacion("Publicable", regla)
  )

  expect_identical(regla$nivel, "agregado")
  expect_identical(regla$proporcion_minima, 0.7)
  expect_equal(nrow(evaluacion$medidas), 30L)
  expect_equal(sum(evaluacion$medidas$resultado), 22L)
  expect_equal(evaluacion$reglas$n_medidas, 30L)
  expect_equal(evaluacion$reglas$n_cumplen, 22L)
  expect_equal(evaluacion$reglas$resultado, 22 / 30)
  expect_identical(evaluacion$reglas$proporcion_minima, 0.7)
  expect_true(evaluacion$reglas$cumple)
  expect_match(evaluacion$reglas$universo, "30 medidas seleccionadas")
  expect_true(all(vapply(
    unique(medicion$metrica_instanciada), grepl, logical(1L),
    x = evaluacion$reglas$universo, fixed = TRUE
  )))

  regla_exigente <- regla_evaluacion(
    "75pc", function(m) m == 1,
    metricas = unique(medicion$metrica_instanciada),
    proporcion_minima = 0.75
  )
  evaluacion_exigente <- evaluar(
    medicion, perfil_evaluacion("No publicable", regla_exigente)
  )
  expect_false(evaluacion_exigente$reglas$cumple)

  regla_en_limite <- regla_evaluacion(
    "En el límite", function(m) m == 1,
    metricas = unique(medicion$metrica_instanciada),
    proporcion_minima = 22 / 30
  )
  evaluacion_en_limite <- evaluar(
    medicion, perfil_evaluacion("Límite inclusivo", regla_en_limite)
  )
  expect_true(evaluacion_en_limite$reglas$cumple)
})

test_that("las reglas por medida conservan su contrato", {
  nucleo <- metricas_nucleo()
  instancia <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "LegadoR100"),
    "tabla", "dato"
  )
  medicion <- medir(
    modelo(instancia), data.frame(dato = c(1, NA, 3)),
    id_medicion = "legado-r100"
  )
  regla <- regla_evaluacion("Presente", function(m) m == 1)
  evaluacion <- evaluar(medicion, perfil_evaluacion("Legado", regla))

  expect_identical(names(regla), c("nombre", "condicion", "metricas"))
  expect_identical(
    names(evaluacion$reglas),
    c("id_medicion", "fecha", "perfil", "regla", "n_medidas", "resultado")
  )
  expect_equal(evaluacion$reglas$resultado, 2 / 3)

  regla_escalar <- regla_evaluacion(
    "Forma anterior inválida", function(m) mean(m, na.rm = TRUE) >= 0.7
  )
  expect_error(
    evaluar(medicion, perfil_evaluacion("Inválido", regla_escalar)),
    "debe devolver lógicos sin NA, uno por medida", fixed = TRUE
  )
})

test_that("una regla agregada conserva el error de metricas disponibles", {
  nucleo <- metricas_nucleo()
  instancia <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "DisponibleR100"),
    "tabla", "dato"
  )
  medicion <- medir(
    modelo(instancia), data.frame(dato = 1:3), id_medicion = "enganche-r100"
  )
  regla <- regla_evaluacion(
    "Agregada inexistente", function(m) m == 1,
    metricas = "NoExisteR100", proporcion_minima = 0.7
  )

  error <- expect_error(
    evaluar(medicion, perfil_evaluacion("Enganche", regla))
  )
  mensaje <- conditionMessage(error)
  expect_match(mensaje, "Solicitadas: NoExisteR100", fixed = TRUE)
  expect_match(
    mensaje, unique(medicion$metrica_instanciada), fixed = TRUE
  )

  expect_error(
    regla_evaluacion("Mal umbral", identity, proporcion_minima = 1.01),
    "proporcion_minima"
  )
  expect_error(
    regla_evaluacion("Umbral NA", identity, proporcion_minima = NA_real_),
    "proporcion_minima"
  )
})
