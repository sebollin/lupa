test_that("se detectan dependencias exactas y aproximadas sin ruido trivial", {
  datos <- data.frame(
    codigo = rep(1:4, each = 10),
    descripcion = rep(LETTERS[1:4], each = 10),
    casi_constante = c(rep("x", 39), "y"),
    id = seq_len(40), stringsAsFactors = FALSE
  )
  datos$descripcion[[40L]] <- "error"
  resultado <- detectar_dependencias(
    datos, umbral = 0.97, min_observaciones = 10
  )

  fila <- resultado[
    resultado$determinante == "codigo" &
      resultado$dependiente == "descripcion", , drop = FALSE
  ]
  expect_equal(fila$cumplimiento, 39 / 40)
  expect_equal(fila$n_violaciones, 1L)
  expect_match(fila$evidencia, "D|error")
  expect_false(any(resultado$determinante == "casi_constante"))
  expect_false(any(resultado$determinante == "id"))
})

test_that("el recorte de dependencias queda declarado", {
  datos <- data.frame(a = rep(1:2, 10), b = rep(c("x", "y"), 10), c = 1:20)
  resultado <- detectar_dependencias(
    datos, muestra = 10, max_columnas = 2, min_observaciones = 5,
    incluir_claves = TRUE
  )

  expect_true(attr(resultado, "muestreado"))
  expect_true(attr(resultado, "truncado"))
  expect_equal(attr(resultado, "filas_analizadas"), 10L)
  expect_equal(attr(resultado, "columnas_omitidas"), "c")
  expect_error(detectar_dependencias(datos, umbral = 2), "umbrales")
  expect_error(detectar_dependencias(datos, max_columnas = 0), "enteros positivos")
  expect_error(detectar_dependencias(1:3), "data.frame")
  expect_error(detectar_dependencias(datos, incluir_claves = NA), "lógico")
  expect_error(detectar_dependencias(datos, muestra = 0), "positivo")
  vacio <- detectar_dependencias(data.frame(a = integer(), b = integer()))
  expect_equal(nrow(vacio), 0L)
  expect_equal(lupa:::.resumen_dependencia(c(NA, NA), c(1, 2))$n, 0L)
  expect_equal(nrow(lupa:::.mapa_dependencia(
    data.frame(a = c(1, 1), b = c("x", "y")), "a", "b"
  )), 0L)

  casi_claves <- data.frame(
    a = sprintf("a%03d", c(seq_len(86), rep(1:14, each = 1))),
    b = sprintf("b%03d", c(seq_len(86), rep(1:14, each = 1)))
  )
  descartadas <- detectar_dependencias(casi_claves)
  expect_equal(nrow(descartadas), 0L)
  expect_equal(
    attr(descartadas, "columnas_descartadas")$motivo,
    c("casi_clave", "casi_clave")
  )
})

test_that("perfilar conserva dependencias y la propuesta explica su origen", {
  datos <- data.frame(
    codigo = rep(1:3, each = 5),
    descripcion = rep(c("A", "B", "C"), each = 5),
    valor = c(seq_len(14), NA)
  )
  perfil <- perfilar(datos, nombre = "padron", muestra = Inf)
  propuesta <- proponer_modelo(perfil, datos)

  expect_s3_class(perfil$dependencias, "dependencias_funcionales")
  expect_s3_class(propuesta, "propuesta_modelo")
  expect_true(all(c("origen", "justificacion", "incluir") %in% names(propuesta)))
  expect_true(any(propuesta$metrica == "NoNulo"))
  expect_true(any(grepl("dependencia_funcional", propuesta$origen)))
  expect_false(all(propuesta$incluir[propuesta$metrica == "Formato"]))

  modelo_propuesto <- modelo_desde_propuesta(propuesta)
  expect_s3_class(modelo_propuesto, "modelo_calidad")
  expect_s3_class(medir(modelo_propuesto, datos), "medicion")
  expect_error(modelo_desde_propuesta(propuesta[0, ]), "no contiene")
  recortada <- proponer_modelo(perfil, datos, max_sugerencias = 1)
  expect_true(attr(recortada, "truncado"))
  expect_equal(nrow(recortada), 1L)
  expect_s3_class(recortada[, , drop = FALSE], "propuesta_modelo")
  expect_false(inherits(recortada[, "metrica", drop = FALSE], "propuesta_modelo"))
})

test_that("NoNulo se propone inactiva ante ausentes disfrazados", {
  disfrazados <- data.frame(a = c(rep("S/D", 8), "x", "y"))
  propuesta <- proponer_modelo(
    perfilar(disfrazados, analizar_dependencias = FALSE), disfrazados
  )
  no_nulo <- propuesta[propuesta$metrica == "NoNulo", , drop = FALSE]

  expect_equal(nrow(no_nulo), 1L)
  expect_false(no_nulo$incluir)
  expect_equal(no_nulo$origen, "hallazgo:faltantes")
  expect_match(no_nulo$justificacion, "normalizarlos")

  reales <- data.frame(a = c(rep(NA, 8), "x", "y"))
  propuesta_reales <- proponer_modelo(
    perfilar(reales, analizar_dependencias = FALSE), reales
  )
  expect_true(propuesta_reales$incluir[propuesta_reales$metrica == "NoNulo"])
})

test_that("las relaciones pueden originar una sugerencia inter-entidad", {
  personas <- data.frame(id = 1:4)
  tramites <- data.frame(persona_id = c(1, 2, 2, 4))
  relaciones <- detectar_relaciones(personas, tramites, muestra = Inf)
  perfil <- perfilar(tramites, nombre = "tramites", analizar_dependencias = FALSE)
  propuesta <- proponer_modelo(
    perfil, tramites, relaciones, c("personas", "tramites")
  )
  inter <- propuesta[propuesta$metrica == "ReglaIntegridadInterEntidad", ]

  expect_true(nrow(inter) >= 1L)
  expect_match(inter$origen[[1L]], "relacion_detectada")
  expect_error(proponer_modelo(perfil, relaciones = relaciones),
               "dos nombres")
})

test_that("la propuesta valida entradas y estados editables", {
  perfil_vacio <- perfilar(data.frame(), analizar_dependencias = FALSE)
  vacia <- proponer_modelo(perfil_vacio)
  expect_s3_class(vacia, "propuesta_modelo")
  expect_equal(nrow(vacia), 0L)
  expect_error(proponer_modelo(data.frame()), "clase perfil")
  expect_error(proponer_modelo(perfil_vacio, datos = 1:3), "data.frame")
  expect_error(proponer_modelo(perfil_vacio, max_sugerencias = 0), "enteros")

  perfil <- perfilar(data.frame(x = c(1, NA)), analizar_dependencias = FALSE)
  propuesta <- proponer_modelo(perfil, data.frame(x = c(1, NA)))
  contrato <- propuesta
  contrato$incluir[[1L]] <- NA
  expect_error(modelo_desde_propuesta(contrato), "contrato")
  bloqueada <- propuesta
  bloqueada$estado[[1L]] <- "requiere_datos"
  expect_error(modelo_desde_propuesta(bloqueada), "estado 'lista'")
  desconocida <- propuesta
  desconocida$metrica[[1L]] <- "NoExiste"
  expect_error(modelo_desde_propuesta(desconocida), "No existe")
  expect_error(modelo_desde_propuesta(data.frame()), "contrato")
  expect_equal(lupa:::.patron_a_regex("A+.a+@a+.a+"),
               "^[[:upper:]]+\\.[[:lower:]]+@[[:lower:]]+\\.[[:lower:]]+$")
  expect_equal(lupa:::.patron_a_regex("99/aa"),
               "^[0-9][0-9]/[[:lower:]][[:lower:]]$")
})

test_that("la imputación por dependencia es deducible y auditada", {
  datos <- data.frame(
    codigo = rep(1:3, each = 5),
    descripcion = rep(c("A", "B", "C"), each = 5),
    stringsAsFactors = FALSE
  )
  datos$descripcion[c(2, 7)] <- NA
  perfil <- perfilar(datos, muestra = Inf)
  plan <- planificar_limpieza(perfil, datos, soporte_minimo_dependencia = 2)
  indice <- grep("^imputar_dependencia_funcional__codigo$", plan$estrategia)

  expect_length(indice, 1L)
  expect_false(plan$recomendada[[indice]])
  expect_false(plan$aplicar[[indice]])
  expect_equal(as.character(plan$decision_grupo[[indice]]), "pendiente")
  plan$aplicar[[indice]] <- TRUE
  plan$decision_grupo[[indice]] <- "elegida"
  resultado <- aplicar(plan, datos)
  expect_equal(resultado$datos$descripcion, rep(c("A", "B", "C"), each = 5))
  expect_equal(resultado$registro$n_cambiadas[
    grepl("^imputar_dependencia", resultado$registro$estrategia)
  ], 2)

  alterados <- datos
  alterados$descripcion[[1L]] <- "contradicción"
  fallo <- aplicar(plan, alterados)
  expect_true(any(grepl("contradicen", fallo$registro$error)))
})
