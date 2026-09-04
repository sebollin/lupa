test_that("la proximidad protege candidatos personales por omision", {
  skip_if_not_installed("stringdist")
  padron <- referencial(
    data.frame(
      dni = c("55555555", "66666666"),
      nombre = c("ALFA BETA", "GAMMA DELTA"),
      stringsAsFactors = FALSE
    ),
    clave = "dni", valor = "nombre"
  )
  metrica <- instanciar(
    especializar(metricas_referencial()$CorrectitudSemDebil),
    "ventas", c("dni", "nombre"), referencial = padron
  )
  medicion <- medir(
    modelo(metrica),
    data.frame(dni = "55555555", nombre = "ALFA BETX",
               stringsAsFactors = FALSE),
    id_medicion = "proteccion"
  )

  objeto <- medicion$objeto_medible[[1L]]
  expect_match(objeto, "candidato_referencial=\\[valor protegido\\]")
  expect_match(objeto, "distancia=")
  expect_false(grepl("55555555|ALFA BETA", objeto))
  historico <- historico_calidad(medicion)
  expect_identical(historico$objeto_medible[[1L]], objeto)

  visible <- medir(
    modelo(metrica),
    data.frame(dni = "55555555", nombre = "ALFA BETX",
               stringsAsFactors = FALSE),
    id_medicion = "sin-proteccion", proteger_datos_personales = FALSE
  )
  expect_match(visible$objeto_medible[[1L]], "55555555", fixed = TRUE)
  expect_match(visible$objeto_medible[[1L]], "ALFA BETA", fixed = TRUE)
})

test_that("la evidencia referencial no personal conserva su candidato", {
  skip_if_not_installed("stringdist")
  referencia <- referencial(
    data.frame(departamento = c("Montevideo", "Canelones")),
    clave = "departamento"
  )
  metrica <- instanciar(
    especializar(metricas_referencial()$CorrectitudSemFuerte),
    "ventas", "departamento", referencial = referencia
  )
  medicion <- medir(
    modelo(metrica), data.frame(departamento = "Montevido"),
    id_medicion = "evidencia-util"
  )
  expect_match(medicion$objeto_medible[[1L]], "Montevideo", fixed = TRUE)
  expect_match(medicion$objeto_medible[[1L]], "distancia=")
})

test_that("una metrica sin valores queda en cobertura y no produce un uno", {
  datos <- data.frame(
    dni = c("1", "2", "3", "4"),
    email = rep(NA_character_, 4L),
    stringsAsFactors = FALSE
  )
  nucleo <- metricas_nucleo()
  m1 <- instanciar(especializar(nucleo$NoNulo), "clientes", "dni")
  m2 <- instanciar(
    especializar(nucleo$Formato, expresion_regular = "^[^@]+@[^@]+$"),
    "clientes", "email"
  )
  medicion <- medir(
    modelo(m1, m2), datos, id_medicion = "cobertura",
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )

  expect_equal(nrow(medicion), 4L)
  cobertura <- attr(medicion, "cobertura_metricas", exact = TRUE)
  expect_true(is.data.frame(cobertura))
  expect_equal(cobertura$metrica_instanciada, m2$nombre)
  expect_equal(cobertura$estado, "sin_valores")
  expect_match(cobertura$motivo, "no se pudo medir", ignore.case = TRUE)
  expect_match(cobertura$motivo, "sin valores no nulos", ignore.case = TRUE)
  historico_medicion <- historico_calidad(medicion)
  expect_equal(
    sum(historico_medicion$nivel == "metrica_no_evaluada"), 1L
  )

  perfil <- perfil_evaluacion(
    "Operativo", regla_evaluacion("Todo medido pasa", function(x) x >= 1)
  )
  evaluacion <- evaluar(medicion, perfil)
  expect_true(is.na(evaluacion$reglas$resultado[[1L]]))
  expect_true(is.na(evaluacion$reglas$n_medidas[[1L]]))
  expect_true(is.na(evaluacion$perfiles$resultado[[1L]]))
  expect_true(is.na(evaluacion$perfiles$n_reglas[[1L]]))
  expect_identical(
    attr(evaluacion, "cobertura_metricas", exact = TRUE), cobertura
  )
  historico <- historico_calidad(evaluacion)
  no_evaluada <- historico[historico$nivel == "metrica_no_evaluada", ,
                            drop = FALSE]
  expect_equal(nrow(no_evaluada), 1L)
  expect_match(no_evaluada$objeto_medible, "no se pudo medir", ignore.case = TRUE)
})
