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

test_that("la tolerancia no borra un cambio legitimamente diminuto", {
  alcanza <- getFromNamespace(".alcanza_umbral_deriva", "lupa")

  # La primera version de la guarda usaba un piso ABSOLUTO -"por debajo del
  # ruido no es un cambio"- y con eso convertia en cero toda magnitud pequena,
  # incluso cuando el usuario pidio `umbral = 0` y el cambio era real. La
  # tolerancia estabiliza la comparacion contra el corte; no redefine la
  # magnitud.
  expect_true(alcanza(1e-10, 0))
  expect_true(alcanza(1e-10, 1e-20))
  # Y una diferencia exactamente nula sigue sin ser un cambio, con cualquier
  # umbral: es el defecto que motivo la guarda.
  expect_false(alcanza(0, 0))
  expect_false(alcanza(0, 1e-20))
  # El caso de la coma flotante, que es lo que la tolerancia vino a absorber.
  expect_identical(alcanza(0.70 - 0.65, 0.05), alcanza(0.75 - 0.70, 0.05))
  expect_true(alcanza(0.70 - 0.65, 0.05))
  expect_false(alcanza(0.04, 0.05))
})

test_that("un perfil sin filas no desmiente una medicion real", {
  nucleo <- metricas_nucleo()
  instancia <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "NN"), "t", "x"
  )
  vacio <- suppressWarnings(perfilar(
    data.frame(x = character(0), stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  medicion <- medir(
    modelo(instancia),
    data.frame(x = c("A", NA), stringsAsFactors = FALSE), id_medicion = "m1"
  )
  expect_gt(nrow(medicion), 0L)

  densidad <- function(cobertura) {
    indice <- which(
      cobertura$dimension == "Completitud" & grepl("Densidad", cobertura$factor)
    )
    skip_if(!length(indice), "el marco no trae Completitud/Densidad")
    as.character(cobertura$estado[indice[[1L]]])
  }

  # Sin medicion, el perfil vacio no puede decir que midio.
  expect_identical(densidad(cobertura_analisis(vacio)), "no_aplica")
  # Pero con una medicion real encima, esa guarda no puede pisarla: puesta
  # despues del bloque de la medicion, publicaba "no hubo nada que examinar"
  # sobre dos filas medidas.
  expect_identical(densidad(cobertura_analisis(vacio, medicion)), "medida")
})

test_that("las cuatro columnas del veredicto de deriva comparten una regla", {
  alcanza <- getFromNamespace(".alcanza_umbral_deriva", "lupa")

  # `significativo` se arreglo con la tolerancia relativa y `direccion`,
  # `severidad` y `descripcion` quedaron con los cortes viejos: una misma fila
  # publicaba significativo TRUE, direccion "mejora", severidad "error" y "el
  # resultado se mantuvo" sobre un DETERIORO diminuto. La direccion sale del
  # signo, no de comparar el delta firmado contra un corte que puede ser
  # negativo.
  expect_true(alcanza(1e-20, 1e-20))
  # Un deterioro de UN umbral no alcanza los dos: sospechoso, no error.
  expect_false(alcanza(1e-20, 2e-20))
  # Y la tolerancia escala con las magnitudes: con un piso fijo, el corte de
  # dos umbrales tambien se cumplia y todo deterioro diminuto salia "error".
  expect_true(alcanza(0.05, 0.05))
  expect_false(alcanza(0.05, 0.10))
})

test_that("una columna que el perfil no pudo resumir no se declara medida", {
  densidad <- function(perfil) {
    cobertura <- cobertura_analisis(perfil)
    indice <- which(
      cobertura$dimension == "Completitud" & grepl("Densidad", cobertura$factor)
    )
    skip_if(!length(indice), "el marco no trae Completitud/Densidad")
    as.character(cobertura$estado[indice[[1L]]])
  }
  perfilar_callado <- function(datos) {
    suppressWarnings(perfilar(
      datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    ))
  }

  # Una columna matriz queda `desconocido` y sus resumenes en NA: hay filas,
  # pero el perfil no obtuvo evidencia. El estado salia `medida` igual, porque
  # se asignaba desde el mapa de capacidades sin consultar si el resumen existe.
  sin_resumen <- perfilar_callado(data.frame(x = I(matrix(1:6, nrow = 3))))
  expect_true(all(is.na(sin_resumen$columnas$n_faltantes)))
  expect_identical(densidad(sin_resumen), "no_aplica")

  # Los dos controles: sin filas tambien, y con datos normales sigue medida.
  expect_identical(
    densidad(perfilar_callado(
      data.frame(x = character(0), stringsAsFactors = FALSE)
    )),
    "no_aplica"
  )
  expect_identical(
    densidad(perfilar_callado(
      data.frame(x = c("a", NA, "b"), stringsAsFactors = FALSE)
    )),
    "medida"
  )
})

test_that("una cobertura parcial no se publica como completa", {
  # `all(is.na(faltantes))` dejaba pasar el caso parcial: UN exito desactivaba
  # la guarda entera. Una tabla con una columna resumible y dos columnas matriz
  # publicaba "El perfil conto ausentes reales y disfrazados EN TODAS LAS
  # COLUMNAS". Con dos de tres sin contar, esa frase es falsa: es la forma suave
  # del defecto que el paquete existe para no cometer, informar como completo lo
  # que es parcial.
  armar <- function(n_matriz) {
    d <- data.frame(dato = c("a", "b", NA, "c"), stringsAsFactors = FALSE)
    for (i in seq_len(n_matriz)) d[[paste0("m", i)]] <- I(matrix(1:8, nrow = 4L))
    d
  }
  densidad <- function(n_matriz) {
    perfil <- suppressWarnings(perfilar(
      armar(n_matriz), analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    ))
    cobertura <- cobertura_analisis(perfil)
    cobertura[cobertura$factor == "Densidad", , drop = FALSE]
  }

  parcial <- densidad(2L)
  skip_if(!nrow(parcial), "el marco no trae el factor Densidad")
  # El factor SI se midio -en la columna que se pudo resumir-, asi que el estado
  # se mantiene; lo que no puede es afirmar sobre las que no examino.
  expect_identical(as.character(parcial$estado[[1L]]), "medida")
  expect_false(grepl("en todas las columnas", parcial$motivo[[1L]]))
  expect_match(as.character(parcial$motivo[[1L]]), "1 de 3 columnas")

  # Control de los dos extremos, que son los que distinguen la guarda de una
  # que siempre diga lo mismo.
  completa <- densidad(0L)
  expect_identical(as.character(completa$estado[[1L]]), "medida")
  expect_match(as.character(completa$motivo[[1L]]), "en todas las columnas")
})

test_that("el alcance parcial sobrevive al cruce del tablero", {
  # La cobertura declaraba "Parcial: ... en 1 de 100 columnas" y la primera capa
  # que la cruza lo tiraba: `tablero_calidad()` reescribia el motivo con un texto
  # fijo, asi que publicaba "El tablero contiene al menos una metrica" -o "El
  # perfil examino el factor"- con noventa y nueve columnas sin resumen. Declarar
  # algo y que el consumidor siguiente lo tire es el defecto que este paquete
  # persigue; el `n_evaluados` de los hallazgos no lo sufre porque viaja en su
  # propia columna.
  datos <- data.frame(num = c(1, 2, NA, 4))
  for (i in seq_len(9L)) {
    datos[[paste0("m", i)]] <- I(matrix(letters[1:8], nrow = 4L))
  }
  perfil <- suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  cobertura <- cobertura_analisis(perfil)
  densidad <- cobertura[cobertura$factor == "Densidad", , drop = FALSE]
  skip_if(!nrow(densidad), "el marco no trae el factor Densidad")
  expect_match(as.character(densidad$motivo[[1L]]), "^Parcial: ")

  nucleo <- metricas_nucleo()
  medidas <- medir(
    modelo(instanciar(
      especializar(nucleo$NoNulo, nombre_especifico = "NNDensidad"), "t", "num"
    )),
    data.frame(num = c(1, 2, NA, 4))
  )
  cruzada <- as.data.frame(attr(
    tablero_calidad(medidas, cobertura = cobertura), "cobertura", exact = TRUE
  ))
  fila <- cruzada[cruzada$factor == "Densidad", , drop = FALSE]
  skip_if(!nrow(fila), "el tablero no conservo el factor Densidad")
  # El motivo del tablero es el que manda, y el alcance de la capa anterior sigue
  # ahi: la fila dice las dos cosas.
  expect_match(as.character(fila$motivo[[1L]]), "1 de 10 columnas")

  # Control: un perfil que resumio TODAS sus columnas no recibe una frase de
  # alcance que no corresponde.
  completo <- suppressWarnings(perfilar(
    data.frame(num = c(1, 2, NA, 4), otro = c("a", "b", "c", "d")),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  cruzada_completa <- as.data.frame(attr(
    tablero_calidad(medidas, cobertura = cobertura_analisis(completo)),
    "cobertura", exact = TRUE
  ))
  fila_completa <- cruzada_completa[
    cruzada_completa$factor == "Densidad", , drop = FALSE
  ]
  if (nrow(fila_completa)) {
    expect_false(grepl("obtuvo resumen en", fila_completa$motivo[[1L]]))
  }
})
