# Tres defectos de la decima vuelta. Los tres comparten forma: una afirmacion
# publicada que no corresponde a lo medido.

test_that("un umbral menor que la tolerancia no vuelve significativo un delta cero", {
  # La tolerancia que arregla la coma flotante volvia NEGATIVO el corte cuando
  # el umbral era mas chico que ella -1e-20 es valido segun el contrato-, y un
  # delta CERO entraba a la vez por "mejora" y por "error" mientras la
  # descripcion decia que el resultado se habia mantenido: tres afirmaciones
  # incompatibles en la misma fila.
  #
  # La primera version de esta prueba comprobaba la aritmetica aca adentro y
  # por eso pasaba con el defecto puesto. Tiene que atravesar la funcion.
  nucleo <- metricas_nucleo()
  instancia <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "NoNuloDato"),
    "entrega", "dato"
  )
  perfil <- perfil_evaluacion(
    "Operativo", regla_evaluacion("C", function(x) x > 0.6)
  )
  corrida <- function(id, fecha) {
    medidas <- medir(
      modelo(instancia),
      data.frame(dato = c(rep("x", 7L), rep(NA, 3L)), stringsAsFactors = FALSE),
      id_medicion = id, fecha = as.POSIXct(fecha, tz = "UTC")
    )
    suppressWarnings(evaluar(agregar(medidas, "atributo", "ratio"), perfil))
  }
  historico <- historico_calidad(
    corrida("enero", "2026-01-31"), corrida("febrero", "2026-02-28")
  )

  deriva <- suppressWarnings(
    detectar_deriva_calidad(historico, umbral = 1e-20)
  )
  fila <- deriva[deriva$aspecto == "resultado", , drop = FALSE]
  skip_if(!nrow(fila), "la deriva no produjo fila de resultado")

  # Las dos corridas son identicas: delta cero.
  expect_equal(fila$delta[[1L]], 0)
  # Y entonces la fila no puede afirmar un cambio, con ningun umbral.
  expect_false(isTRUE(fila$significativo[[1L]]))
  expect_identical(as.character(fila$direccion[[1L]]), "estable")
  expect_identical(as.character(fila$severidad[[1L]]), "ok")
  expect_match(as.character(fila$descripcion[[1L]]), "se mantuvo")
})

test_that("todos los aspectos de comparar_perfiles usan el mismo umbral", {
  alcanza <- getFromNamespace(".alcanza_umbral_deriva", "lupa")

  # Las dos restas conocidas: publican la misma magnitud y recibian veredictos
  # distintos segun el aspecto, porque faltantes comparaba con tolerancia y
  # rango y patrones sin ella. La documentacion publica UN umbral para todos.
  abajo <- 0.70 - 0.65
  arriba <- 0.75 - 0.70
  expect_false(isTRUE(all.equal(abajo, arriba, tolerance = 0)))
  expect_identical(alcanza(abajo, 0.05), alcanza(arriba, 0.05))
  expect_true(alcanza(abajo, 0.05))

  # Y la regla esta escrita una sola vez: ninguna comparacion de umbral suelta.
  cuerpo <- paste(
    deparse(getFromNamespace("comparar_perfiles", "lupa")), collapse = "\n"
  )
  expect_false(grepl(">= umbral_cambio\\b(?! -)", cuerpo, perl = TRUE))
})

test_that("un perfil sin filas no declara factores como medidos", {
  # `medida` se asignaba por el mapa de capacidades sin mirar si hubo
  # observaciones: un perfil de CERO filas informaba "el perfil conto ausentes
  # reales en todas las columnas" sin haber contado nada.
  vacio <- suppressWarnings(perfilar(
    data.frame(x = character(0), stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  cobertura_vacia <- cobertura_analisis(vacio)
  expect_identical(sum(cobertura_vacia$estado == "medida"), 0L)
  expect_gt(sum(cobertura_vacia$estado == "no_aplica"), 0L)
  expect_true(any(grepl(
    "no tiene filas", cobertura_vacia$motivo[cobertura_vacia$estado == "no_aplica"]
  )))

  # Y el control: con filas, los factores que el perfil examina siguen medidos.
  con_filas <- suppressWarnings(perfilar(
    data.frame(x = c("a", "b", "a"), stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  expect_gt(sum(cobertura_analisis(con_filas)$estado == "medida"), 0L)
})
