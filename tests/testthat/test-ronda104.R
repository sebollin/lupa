modelo_r104 <- function() {
  metrica <- metrica(
    "EstimacionR104", "Estimación ya calculada que se evalúa para publicar.",
    "instanciaAtributo", "numero_real",
    metodo = function(tablas, instancia) {
      datos <- tablas[[instancia$entidad]]
      data.frame(
        resultado = datos[[instancia$atributos]],
        entidad = instancia$entidad,
        atributo = instancia$atributos,
        fila = seq_len(nrow(datos)),
        objeto = paste0(instancia$entidad, "$", instancia$atributos, "[",
                        seq_len(nrow(datos)), "]"),
        stringsAsFactors = FALSE
      )
    }
  )
  instancia <- instanciar(
    especializar(metrica), "tabulado", "estimacion"
  )
  modelo(instancia)
}

medicion_r104 <- function() {
  medir(
    modelo_r104(),
    data.frame(estimacion = c(0.81234567, 0.123456789, 0.93456789)),
    id_medicion = "ronda-104",
    fecha = as.POSIXct("2026-08-16 12:00:00", tz = "UTC")
  )
}

test_that("un desenlace declarado produce un plan trazable", {
  medicion <- medicion_r104()
  regla <- regla_evaluacion(
    "Estimación confiable", function(x) x >= 0.8,
    metricas = unique(medicion$metrica_instanciada),
    desenlace = "suprimir"
  )
  evaluacion <- evaluar(
    medicion, perfil_evaluacion("Publicación", regla)
  )

  expect_identical(regla$desenlace, "suprimir")
  expect_s3_class(evaluacion$desenlaces, "plan_desenlaces")
  expect_equal(nrow(evaluacion$desenlaces), 1L)
  expect_identical(
    evaluacion$desenlaces$id_medida, medicion$id_medida[[2L]]
  )
  expect_identical(evaluacion$desenlaces$regla, "Estimación confiable")
  expect_identical(evaluacion$desenlaces$desenlace, "suprimir")
  expect_match(evaluacion$desenlaces$motivo, "no cumple la condición")
  expect_identical(
    evaluacion$desenlaces$objeto_medible, "tabulado$estimacion[2]"
  )
  expect_identical(evaluacion$desenlaces$valor_medido, 0.123456789)
  expect_identical(medicion$resultado, c(0.81234567, 0.123456789, 0.93456789))

  agregada <- regla_evaluacion(
    "Dos tercios confiables", function(x) x >= 0.8,
    proporcion_minima = 2 / 3, desenlace = "suprimir"
  )
  evaluacion_agregada <- evaluar(
    medicion, perfil_evaluacion("Publicación agregada", agregada)
  )
  expect_true(evaluacion_agregada$reglas$cumple)
  expect_equal(nrow(evaluacion_agregada$desenlaces), 1L)
  expect_identical(
    evaluacion_agregada$desenlaces$id_medida, medicion$id_medida[[2L]]
  )
})

test_that("sin declaracion no hay supresion ni cambios de contrato", {
  medicion <- medicion_r104()
  regla <- regla_evaluacion(
    "Estimación confiable", function(x) x >= 0.8,
    metricas = unique(medicion$metrica_instanciada)
  )
  evaluacion <- evaluar(
    medicion, perfil_evaluacion("Publicación", regla)
  )

  expect_identical(names(regla), c("nombre", "condicion", "metricas"))
  expect_identical(
    names(evaluacion), c("medidas", "reglas", "perfiles")
  )
  expect_null(evaluacion$desenlaces)
  expect_false(evaluacion$medidas$resultado[[2L]])

  archivo <- tempfile("control-reporte-r104-", fileext = ".html")
  reportar(
    medicion, evaluacion, archivo = archivo,
    fecha = as.POSIXct("2026-08-16 13:00:00", tz = "UTC")
  )
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")
  expect_match(html, "0.12345679", fixed = TRUE)
  expect_false(grepl("[valor suprimido]", html, fixed = TRUE))

  regla_sin_alcanzadas <- regla_evaluacion(
    "Todas confiables", function(x) x >= 0,
    desenlace = "suprimir"
  )
  sin_alcanzadas <- evaluar(
    medicion, perfil_evaluacion("Sin supresiones", regla_sin_alcanzadas)
  )
  expect_s3_class(sin_alcanzadas$desenlaces, "plan_desenlaces")
  expect_equal(nrow(sin_alcanzadas$desenlaces), 0L)
})

test_that("reportar oculta valores suprimidos sin modificar objetos", {
  medicion <- medicion_r104()
  evaluacion <- evaluar(
    medicion,
    perfil_evaluacion(
      "Publicación",
      regla_evaluacion(
        "Estimación confiable", function(x) x >= 0.8,
        desenlace = "suprimir"
      )
    )
  )
  medicion_antes <- medicion
  evaluacion_antes <- evaluacion
  archivo <- tempfile("reporte-r104-", fileext = ".html")
  reportar(
    medicion, evaluacion, archivo = archivo,
    fecha = as.POSIXct("2026-08-16 13:00:00", tz = "UTC")
  )
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")

  expect_match(html, "Plan de desenlaces", fixed = TRUE)
  expect_match(html, "[valor suprimido]", fixed = TRUE)
  expect_false(grepl("0.12345679", html, fixed = TRUE))
  expect_match(html, "0.81234567", fixed = TRUE)
  expect_match(html, "0.93456789", fixed = TRUE)
  expect_identical(medicion, medicion_antes)
  expect_identical(evaluacion, evaluacion_antes)
})

test_that("un analisis respeta los desenlaces de su evaluacion", {
  datos <- data.frame(estimacion = c(0.81234567, 0.123456789, 0.93456789))
  perfil <- perfil_evaluacion(
    "Publicación",
    regla_evaluacion(
      "Estimación confiable", function(x) x >= 0.8,
      desenlace = "suprimir"
    )
  )
  analisis <- analizar(
    datos, nombre = "Análisis R104", modelo_confirmado = modelo_r104(),
    perfil_evaluacion = perfil, id_medicion = "ronda-104",
    fecha = as.POSIXct("2026-08-16 12:00:00", tz = "UTC"),
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  )
  analisis_antes <- analisis
  archivo <- tempfile("reporte-analisis-r104-", fileext = ".html")
  reportar(
    analisis, archivo = archivo, proteger_datos_personales = FALSE,
    fecha = as.POSIXct("2026-08-16 13:00:00", tz = "UTC")
  )
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")

  expect_match(html, "[valor suprimido]", fixed = TRUE)
  expect_identical(analisis, analisis_antes)
})

test_that("la supresion se conserva en todas las salidas derivadas", {
  datos <- data.frame(codigo = c("A", "B", "A"), stringsAsFactors = FALSE)
  nucleo <- metricas_nucleo()
  modelo_confirmado <- modelo(list(
    instanciar(especializar(nucleo$EntidadDuplicada), "datos")
  ))
  analisis <- analizar(
    datos, modelo_confirmado = modelo_confirmado,
    perfil_evaluacion = perfil_evaluacion(
      "Operativo",
      regla_evaluacion(
        "Medida publicable", function(x) x > 0.9, desenlace = "suprimir"
      )
    ), analizar_dependencias = FALSE,
    fecha = as.POSIXct("2026-01-15", tz = "UTC"), id_medicion = "m1"
  )
  tablero <- tablero_calidad(analisis)
  indice <- indice_calidad(analisis, pesos = c(Unicidad = 1))
  historico <- historico_calidad(
    analisis$medicion, analisis$evaluacion, detalle = "completo"
  )

  expect_equal(tablero$valor, "[valor suprimido]")
  expect_true(is.na(indice$valor))
  expect_match(indice$motivo, "suprimir")
  medida <- historico[historico$nivel == "medida", , drop = FALSE]
  expect_true(is.na(medida$resultado))
  expect_match(medida$objeto_medible, "valor suprimido", ignore.case = TRUE)
  expect_true(is.na(analisis$medicion$resultado))
  expect_match(analisis$medicion$objeto_medible, "valor suprimido",
               ignore.case = TRUE)
  expect_equal(
    analisis$evaluacion$desenlaces$valor_medido, "[valor suprimido]"
  )

  impresos <- c(
    capture.output(print(analisis)),
    capture.output(print(analisis$medicion)),
    capture.output(print(analisis$evaluacion)),
    capture.output(print(tablero)),
    capture.output(print(indice)),
    capture.output(print(historico))
  )
  texto_impreso <- paste(impresos, collapse = "\n")
  expect_match(texto_impreso, "valor suprimido", ignore.case = TRUE)
  expect_false(grepl("0.6666667", texto_impreso, fixed = TRUE))

  archivo_html <- tempfile(fileext = ".html")
  reportar(analisis, archivo = archivo_html, sobrescribir = TRUE)
  html <- paste(readLines(archivo_html, warn = FALSE), collapse = "\n")
  expect_match(html, "valor suprimido", ignore.case = TRUE)
  expect_false(grepl("0.6666667", html, fixed = TRUE))

  archivo_rds <- tempfile(fileext = ".rds")
  guardar_analisis(analisis, archivo_rds, sobrescribir = TRUE)
  guardado <- readRDS(archivo_rds)
  expect_true(is.na(guardado$medicion$resultado))
  expect_equal(guardado$tablero$valor, "[valor suprimido]")
  expect_equal(
    guardado$evaluacion$desenlaces$valor_medido, "[valor suprimido]"
  )
  unlink(c(archivo_html, archivo_rds))
})

test_that("los errores y las reglas anteriores conservan su comportamiento", {
  medicion <- medicion_r104()
  regla <- regla_evaluacion(
    "Métrica ausente", function(x) x >= 0.8,
    metricas = "NoExisteR104", desenlace = "suprimir"
  )
  error <- expect_error(
    evaluar(medicion, perfil_evaluacion("Publicación", regla))
  )
  mensaje <- conditionMessage(error)
  expect_match(mensaje, "Solicitadas: NoExisteR104", fixed = TRUE)
  expect_match(
    mensaje, unique(medicion$metrica_instanciada), fixed = TRUE
  )
  expect_error(
    regla_evaluacion("Inválida", identity, desenlace = "revisar"),
    "NULL o 'suprimir'", fixed = TRUE
  )
})
