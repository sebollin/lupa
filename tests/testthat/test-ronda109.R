.agregados_orientacion_r109 <- function() {
  codigos <- c(sprintf("AA%02d", seq_len(18L)), "AA19", "AA19")
  datos <- data.frame(codigo = codigos, stringsAsFactors = FALSE)
  nucleo <- metricas_nucleo()
  instancias <- list(
    instanciar(especializar(nucleo$NoNulo), "padron", "codigo"),
    instanciar(
      especializar(nucleo$Formato, expresion_regular = "^AA[0-9]{2}$"),
      "padron", "codigo"
    ),
    instanciar(especializar(nucleo$EntidadDuplicada), "padron")
  )
  medidas <- medir(
    modelo(instancias), datos, id_medicion = "r109",
    fecha = as.POSIXct("2026-08-16", tz = "UTC")
  )
  partes <- lapply(
    c("NoNulo", "Formato", "EntidadDuplicada"),
    function(nombre) {
      seleccion <- medidas$metrica == nombre
      destino <- if (nombre == "EntidadDuplicada") "entidad" else "atributo"
      agregar(medidas[seleccion, , drop = FALSE], destino, "ratio")
    }
  )
  resultado <- do.call(rbind, partes)
  rownames(resultado) <- NULL
  resultado$id_medida <- paste0("r109-agg-", resultado$metrica)
  class(resultado) <- c("medicion", "data.frame")
  resultado
}

test_that("cada metrica del nucleo pertenece al marco AGESIC", {
  declaraciones <- lapply(metricas_nucleo(), lupa:::.declaracion_metrica)
  pares_metricas <- unique(vapply(declaraciones, function(x) {
    paste(x$dimension, x$factor, sep = "|")
  }, character(1L)))
  marco <- marco_agesic()
  pares_marco <- unique(paste(
    marco$factores$dimension, marco$factores$factor, sep = "|"
  ))
  fuera_del_marco <- setdiff(pares_metricas, pares_marco)

  expect_equal(
    fuera_del_marco, character(),
    info = paste("Pares fuera del marco:", paste(fuera_del_marco, collapse = ", "))
  )
})

test_that("el flujo confirmado reconoce Correctitud sintactica como medida", {
  datos <- data.frame(
    codigo = sprintf("AA-%03d", seq_len(10L)),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE, expandir = TRUE)
  propuesta <- proponer_modelo(perfil, datos)
  propuesta$incluir[] <- FALSE
  propuesta$incluir[propuesta$metrica == "Formato"] <- TRUE

  analisis <- analizar(
    datos, propuesta_confirmada = propuesta,
    analizar_dependencias = FALSE, expandir = TRUE,
    id_medicion = "r109-formato",
    fecha = as.POSIXct("2026-08-16", tz = "UTC")
  )
  fila <- analisis$cobertura[
    analisis$cobertura$dimension == "Exactitud" &
      analisis$cobertura$factor == "Correctitud sint\u00e1ctica", ,
    drop = FALSE
  ]

  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$estado), "medida")
  expect_true(all(
    analisis$medicion$factor == "Correctitud sint\u00e1ctica"
  ))
  # Ronda 111: analizar() ya conserva esta medicion en granularidad atributo;
  # volver a agregar atributo -> atributo seria una transicion inexistente.
  expect_identical(analisis$medicion$granularidad, "atributo")
  expect_identical(analisis$medicion$agregacion, "ratio")
  expect_identical(analisis$medicion$factor, "Correctitud sint\u00e1ctica")
  expect_identical(analisis$medicion$orientacion, "conformidad")
})

test_that("la orientacion viaja de la declaracion a la agregacion", {
  declaraciones <- lapply(metricas_nucleo(), lupa:::.declaracion_metrica)
  orientaciones <- vapply(declaraciones, `[[`, character(1L), "orientacion")
  expect_setequal(
    unique(orientaciones), c("conformidad", "defecto", "no_aplica")
  )
  expect_identical(orientaciones[["NoNulo"]], "conformidad")
  expect_identical(orientaciones[["Formato"]], "conformidad")
  expect_identical(orientaciones[["EntidadDuplicada"]], "defecto")
  expect_identical(orientaciones[["ErrorEstandar"]], "no_aplica")
  expect_identical(orientaciones[["Escala"]], "no_aplica")

  tabla <- .agregados_orientacion_r109()
  vista <- tabla[c("dimension", "factor", "metrica", "resultado", "orientacion")]
  expect_equal(
    vista$orientacion, c("conformidad", "conformidad", "defecto")
  )
  expect_equal(vista$resultado, c(1, 1, 0.1))
})

test_that("reporte y evaluacion distinguen conformidad de defecto", {
  medidas <- .agregados_orientacion_r109()
  regla <- regla_evaluacion(
    "Lectura segun orientacion",
    function(x, orientacion) {
      ifelse(orientacion == "conformidad", x >= 0.9, x <= 0.1)
    }
  )
  evaluacion <- evaluar(
    medidas, perfil_evaluacion("Lectura orientada", regla)
  )

  expect_true(all(evaluacion$medidas$resultado))
  expect_equal(
    evaluacion$medidas$orientacion,
    c("conformidad", "conformidad", "defecto")
  )

  archivo <- tempfile(fileext = ".html")
  reportar(medidas, evaluacion, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")
  expect_match(html, "orientacion", fixed = TRUE)
  expect_match(html, "conformidad", fixed = TRUE)
  expect_match(html, "defecto", fixed = TRUE)
})

test_that("los historicos de esquema 1 siguen siendo legibles", {
  historico_previo <- historico_calidad(.agregados_orientacion_r109())
  expect_identical(attr(historico_previo, "version_esquema"), 1L)
  expect_false("orientacion" %in% names(historico_previo))
  archivo <- tempfile(fileext = ".rds")
  saveRDS(historico_previo, archivo, version = 3L)

  recuperado <- leer_historico(archivo)
  expect_s3_class(recuperado, "historico_calidad")
  expect_equal(recuperado, historico_previo)
})

test_that("el vocabulario de orientacion es cerrado y coherente con el tipo", {
  expect_error(
    metrica(
      "M", "Semantica.", "atributo", "real", orientacion = "mayor_es_mejor"
    ),
    "orientacion"
  )
  expect_error(
    metrica(
      "M", "Semantica.", "instanciaAtributo", "booleano",
      orientacion = "no_aplica"
    ),
    "booleana"
  )
  expect_error(
    metrica(
      "M", "Semantica.", "atributo", "numero_real",
      orientacion = "defecto"
    ),
    "no acotada"
  )
})
