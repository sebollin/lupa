activar_estrategia <- function(plan, estrategia) {
  indice <- which(plan$estrategia == estrategia)
  stopifnot(length(indice) == 1L)
  grupo <- plan$grupo[[indice]]
  if (!is.na(grupo)) plan$aplicar[plan$grupo == grupo] <- FALSE
  plan$aplicar[[indice]] <- TRUE
  plan
}

test_that("el catálogo crea alternativas excluyentes y decisiones explícitas", {
  capital <- planificar_limpieza(perfilar(data.frame(
    zona = c("Norte", "NORTE", "sur", "SUR")
  )))
  acciones_capital <- capital[
    capital$hallazgo == "mayusculas_inconsistentes", , drop = FALSE
  ]
  expect_setequal(acciones_capital$estrategia, c(
    "convertir_minusculas", "convertir_titulo", "convertir_mayusculas",
    "convertir_segun_diccionario"
  ))
  expect_length(unique(acciones_capital$grupo), 1L)
  expect_true(all(as.character(acciones_capital$decision_grupo) == "pendiente"))
  expect_false(any(acciones_capital$recomendada))
  expect_false(any(acciones_capital$aplicar))

  duplicadas <- planificar_limpieza(perfilar(data.frame(x = c(1, 1, 2))))
  acciones_filas <- duplicadas[
    duplicadas$hallazgo == "filas_duplicadas", , drop = FALSE
  ]
  expect_setequal(acciones_filas$estrategia, c(
    "marcar_filas_duplicadas", "conservar_primera_duplicada",
    "conservar_mas_completa"
  ))
  expect_true(acciones_filas$aplicar[
    acciones_filas$estrategia == "marcar_filas_duplicadas"
  ])
  expect_true(all(acciones_filas$destructiva[
    grepl("^conservar", acciones_filas$estrategia)
  ]))
  expect_true(all(!acciones_filas$reversible[acciones_filas$destructiva]))

  columnas <- planificar_limpieza(perfilar(data.frame(a = 1:3, b = 1:3)))
  acciones_columnas <- columnas[
    columnas$hallazgo == "columnas_duplicadas", , drop = FALSE
  ]
  expect_setequal(acciones_columnas$estrategia, c(
    "marcar_columnas_duplicadas", "eliminar_columna_duplicada"
  ))
  expect_true(acciones_columnas$recomendada[
    acciones_columnas$estrategia == "marcar_columnas_duplicadas"
  ])
  expect_true(acciones_columnas$destructiva[
    acciones_columnas$estrategia == "eliminar_columna_duplicada"
  ])

  numericos <- planificar_limpieza(perfilar(data.frame(x = c(999, 1:9))))
  accion_numerica <- numericos[
    numericos$estrategia == "convertir_sentinelas_numericos", , drop = FALSE
  ]
  expect_equal(as.character(accion_numerica$decision_grupo), "pendiente")
  expect_true(is.na(accion_numerica$recomendacion_grupo))

  extremos <- planificar_limpieza(perfilar(data.frame(
    x = c(rep(1, 20), 100)
  )))
  acciones_extremos <- extremos[extremos$hallazgo == "outliers", , drop = FALSE]
  expect_setequal(acciones_extremos$estrategia, c(
    "marcar_outliers", "winsorizar_outliers"
  ))
  expect_true(acciones_extremos$recomendada[
    acciones_extremos$estrategia == "marcar_outliers"
  ])
  expect_false(any(acciones_extremos$aplicar))

  nombres <- data.frame(x = 1:2, check.names = FALSE)
  names(nombres) <- " Nombre malo "
  acciones_nombres <- planificar_limpieza(perfilar(nombres))
  acciones_nombres <- acciones_nombres[
    acciones_nombres$hallazgo == "nombres_columnas_problematicos", , drop = FALSE
  ]
  expect_setequal(acciones_nombres$estrategia, c(
    "normalizar_nombres", "normalizar_nombres_snake_case"
  ))
  expect_true(acciones_nombres$aplicar[
    acciones_nombres$estrategia == "normalizar_nombres"
  ])

  constante <- planificar_limpieza(perfilar(data.frame(x = rep("UY", 3))))
  accion_constante <- constante[
    constante$estrategia == "eliminar_columna_constante", , drop = FALSE
  ]
  expect_equal(accion_constante$recomendacion_grupo, "no_hacer_nada")
  expect_equal(as.character(accion_constante$decision_grupo), "recomendada")
  expect_false(accion_constante$aplicar)
  expect_true(accion_constante$destructiva)

  datos_tipo <- data.frame(x = c("1", "2", "3"))
  tipo <- planificar_limpieza(perfilar(datos_tipo), datos_tipo)
  accion_tipo <- tipo[tipo$estrategia == "convertir_tipo", , drop = FALSE]
  expect_true(accion_tipo$recomendada)
  expect_true(accion_tipo$aplicar)
  expect_equal(accion_tipo$recomendacion_grupo, "convertir_tipo")
})

test_that("aplicar rechaza dos alternativas activas y nombra el conflicto", {
  datos <- data.frame(zona = c("Norte", "NORTE", "sur", "SUR"))
  plan <- planificar_limpieza(perfilar(datos))
  indices <- which(plan$hallazgo == "mayusculas_inconsistentes")[1:2]
  plan$aplicar[indices] <- TRUE
  grupo <- plan$grupo[[indices[[1L]]]]

  expect_error(
    aplicar(plan, datos),
    paste0(grupo, ".*convertir_minusculas, convertir_titulo")
  )
})

test_that("la marca de duplicados identifica repetidos y grupos completos", {
  datos <- data.frame(
    id = c(rep(1, 5), rep(2, 5)),
    valor = c(rep("A", 5), rep("B", 5))
  )
  perfil <- perfilar(datos)
  expect_equal(perfil$general$filas_duplicadas, 8L)
  expect_equal(perfil$general$filas_en_grupos_duplicados, 10L)

  resultado <- aplicar(planificar_limpieza(perfil), datos)$datos
  expect_equal(sum(resultado$.fila_duplicada), 8L)
  expect_equal(sum(!is.na(resultado$.grupo_duplicado)), 10L)
  expect_equal(length(unique(resultado$.grupo_duplicado)), 2L)
})

test_that("el segundo consentimiento protege y conserva filas eliminadas", {
  datos <- data.frame(id = c(1, 1, 2), valor = c("A", "A", "B"))
  plan <- planificar_limpieza(perfilar(datos))
  plan <- activar_estrategia(plan, "conservar_primera_duplicada")

  expect_error(
    aplicar(plan, datos),
    "permitir_eliminacion = TRUE.*conservar_primera_duplicada"
  )
  resultado <- aplicar(plan, datos, permitir_eliminacion = TRUE)
  expect_equal(nrow(resultado$datos), 2L)
  expect_equal(resultado$registro$n_filas_eliminadas, 1)
  expect_equal(resultado$registro$n_columnas_eliminadas, 0)
  expect_true(resultado$registro$destructiva)
  expect_equal(nrow(resultado$eliminados$filas[[1L]]), 1L)
  expect_identical(datos, data.frame(
    id = c(1, 1, 2), valor = c("A", "A", "B")
  ))
  expect_equal(
    as.character(resultado$plan$decision_grupo[
      resultado$plan$estrategia == "conservar_primera_duplicada"
    ]),
    "elegida"
  )

  liviano <- aplicar(
    plan, datos, permitir_eliminacion = TRUE, conservar_eliminados = FALSE
  )
  expect_length(liviano$eliminados$filas, 0L)
  expect_equal(liviano$registro$n_filas_eliminadas, 1)
})

test_that("se pueden eliminar ausentes y columnas con recuperación", {
  con_na <- data.frame(id = 1:4, valor = c(10, NA, 30, 40))
  plan_na <- planificar_limpieza(perfilar(con_na))
  plan_na <- activar_estrategia(plan_na, "eliminar_filas_ausentes")
  resultado_na <- aplicar(plan_na, con_na, permitir_eliminacion = TRUE)
  expect_equal(resultado_na$datos$id, c(1L, 3L, 4L))
  expect_equal(resultado_na$registro$n_filas_eliminadas, 1)
  expect_equal(resultado_na$eliminados$filas[[1L]]$id, 2L)

  duplicadas <- data.frame(a = 1:3, b = 1:3, c = letters[1:3])
  plan_columnas <- planificar_limpieza(perfilar(duplicadas))
  plan_columnas <- activar_estrategia(
    plan_columnas, "eliminar_columna_duplicada"
  )
  resultado_columnas <- aplicar(
    plan_columnas, duplicadas, permitir_eliminacion = TRUE
  )
  expect_false("b" %in% names(resultado_columnas$datos))
  expect_equal(resultado_columnas$registro$n_columnas_eliminadas, 1)
  expect_equal(resultado_columnas$eliminados$columnas[[1L]]$nombre, "b")
  expect_equal(resultado_columnas$eliminados$columnas[[1L]]$posicion, 2L)
  expect_equal(resultado_columnas$eliminados$columnas[[1L]]$valores, 1:3)

  constante <- data.frame(x = rep("UY", 3), id = 1:3)
  plan_constante <- activar_estrategia(
    planificar_limpieza(perfilar(constante)), "eliminar_columna_constante"
  )
  resultado_constante <- aplicar(
    plan_constante, constante, permitir_eliminacion = TRUE
  )
  expect_false("x" %in% names(resultado_constante$datos))
  expect_equal(resultado_constante$registro$n_columnas_eliminadas, 1)
})

test_that("conservar la más completa exige y respeta una clave", {
  datos <- data.frame(
    id = c(1, 1, 2, 2, 3),
    nombre = c(NA, "Ana", "Beto", "Beto", "Caro"),
    edad = c(20, 20, 30, 30, 40), stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(perfilar(datos))
  indice <- which(plan$estrategia == "conservar_mas_completa")
  grupo <- plan$grupo[[indice]]
  plan$aplicar[plan$grupo == grupo] <- FALSE
  plan$aplicar[[indice]] <- TRUE
  plan$estado[[indice]] <- "lista"

  fallo <- aplicar(plan, datos, permitir_eliminacion = TRUE)
  expect_match(fallo$registro$error, "requiere configurar")
  plan$parametros[[indice]] <- list(clave = "id")
  resultado <- aplicar(plan, datos, permitir_eliminacion = TRUE)
  expect_equal(resultado$datos$id, 1:3)
  expect_equal(resultado$datos$nombre, c("Ana", "Beto", "Caro"))
  expect_equal(resultado$registro$n_filas_eliminadas, 2)
})

test_that("el modo guiado no bloquea fuera de una sesión interactiva", {
  datos <- data.frame(zona = c("Norte", "NORTE", "sur", "SUR"))
  plan <- planificar_limpieza(perfilar(datos))

  expect_false(interactive())
  expect_identical(guiar_limpieza(plan, datos), plan)
})

test_that("el modo guiado muestra evidencia y elige una sola alternativa", {
  datos <- data.frame(zona = c("Norte", "NORTE", "sur", "SUR"))
  plan <- planificar_limpieza(perfilar(datos))
  observado <- new.env(parent = emptyenv())
  selector <- function(decision) {
    observado$ejemplos <- decision$ejemplos
    observado$justificaciones <- decision$acciones$justificacion
    "convertir_minusculas"
  }
  guiado <- suppressMessages(guiar_limpieza(plan, datos, selector = selector))
  acciones <- guiado[guiado$hallazgo == "mayusculas_inconsistentes", ]

  expect_setequal(observado$ejemplos, c('"Norte"', '"NORTE"', '"sur"', '"SUR"'))
  expect_true(all(nzchar(observado$justificaciones)))
  expect_equal(sum(acciones$aplicar), 1L)
  expect_true(acciones$aplicar[acciones$estrategia == "convertir_minusculas"])
  expect_true(all(as.character(acciones$decision_grupo) == "elegida"))
  resultado <- aplicar(guiado, datos)$datos
  expect_equal(resultado$zona, c("norte", "norte", "sur", "sur"))
})

test_that("el modo guiado registra no actuar y admite diccionarios", {
  datos <- data.frame(zona = c("Norte", "NORTE", "sur", "SUR"))
  plan <- planificar_limpieza(perfilar(datos))
  omitido <- suppressMessages(guiar_limpieza(
    plan, datos, selector = function(decision) 0L
  ))
  grupo <- unique(omitido$grupo[omitido$hallazgo == "mayusculas_inconsistentes"])
  en_grupo <- !is.na(omitido$grupo) & omitido$grupo == grupo
  expect_gt(sum(en_grupo), 0L)
  expect_true(all(as.character(omitido$decision_grupo[en_grupo]) ==
                    "omitida"))
  expect_false(any(omitido$aplicar[en_grupo]))

  diccionario <- c(Norte = "Norte", NORTE = "Norte", sur = "Sur", SUR = "Sur")
  elegido <- suppressMessages(guiar_limpieza(
    plan, datos,
    selector = function(decision) "convertir_segun_diccionario",
    diccionarios = list(zona = diccionario)
  ))
  resultado <- aplicar(elegido, datos)$datos
  expect_equal(resultado$zona, c("Norte", "Norte", "Sur", "Sur"))
})

test_that("los grupos resueltos sin destrucción no se vuelven a preguntar", {
  datos <- data.frame(" Nombre malo " = 1:3, check.names = FALSE)
  plan <- planificar_limpieza(perfilar(datos))
  llamadas <- 0L
  guiado <- suppressMessages(guiar_limpieza(
    plan, datos,
    selector = function(decision) {
      llamadas <<- llamadas + 1L
      0L
    }
  ))
  expect_equal(llamadas, 0L)
  expect_identical(guiado, plan)
})

test_that("las estrategias no destructivas alternativas se ejecutan", {
  extremos <- data.frame(valor = c(rep(1, 20), 100))
  plan_extremos <- activar_estrategia(
    planificar_limpieza(perfilar(extremos)), "winsorizar_outliers"
  )
  resultado_extremos <- aplicar(plan_extremos, extremos)$datos
  expect_lt(max(resultado_extremos$valor), 100)
  expect_false(any(grepl("outlier", names(resultado_extremos))))

  nombres <- data.frame("Nombre raro +" = 1:2, check.names = FALSE)
  plan_nombres <- activar_estrategia(
    planificar_limpieza(perfilar(nombres)), "normalizar_nombres_snake_case"
  )
  expect_equal(names(aplicar(plan_nombres, nombres)$datos), "nombre_raro")

  capital <- data.frame(zona = c("norte", "NORTE"))
  plan_capital <- planificar_limpieza(perfilar(capital))
  mayusculas <- activar_estrategia(plan_capital, "convertir_mayusculas")
  expect_equal(aplicar(mayusculas, capital)$datos$zona, c("NORTE", "NORTE"))
  titulo <- activar_estrategia(plan_capital, "convertir_titulo")
  expect_equal(aplicar(titulo, capital)$datos$zona, c("Norte", "Norte"))
})

test_that("se distingue una recomendación desactivada de una decisión pendiente", {
  datos_tipo <- data.frame(x = c("1", "2", "3"))
  tipo <- planificar_limpieza(perfilar(datos_tipo), datos_tipo)
  tipo$aplicar[tipo$estrategia == "convertir_tipo"] <- FALSE
  resultado <- aplicar(tipo, data.frame(x = c("1", "2", "3")))
  expect_equal(
    as.character(resultado$plan$decision_grupo[
      resultado$plan$estrategia == "convertir_tipo"
    ]),
    "desactivada"
  )

  numerico <- planificar_limpieza(perfilar(data.frame(x = c(999, 1:9))))
  expect_equal(
    as.character(numerico$decision_grupo[
      numerico$estrategia == "convertir_sentinelas_numericos"
    ]),
    "pendiente"
  )
})

test_that("se validan permisos y entradas del modo guiado", {
  datos <- data.frame(zona = c("Norte", "NORTE"))
  plan <- planificar_limpieza(perfilar(datos))
  expect_error(aplicar(plan, datos, permitir_eliminacion = NA), "lógicos escalares")
  expect_error(aplicar(plan, datos, conservar_eliminados = 1), "lógicos escalares")
  expect_error(
    guiar_limpieza(plan, datos, selector = 1), "selector.*función"
  )
  expect_error(
    guiar_limpieza(plan, datos, selector = function(x) 0, diccionarios = 1),
    "diccionarios"
  )
  expect_error(
    guiar_limpieza(plan, datos, selector = function(x) 0, max_ejemplos = 0),
    "número positivo"
  )
  expect_error(
    suppressMessages(guiar_limpieza(
      plan, datos, selector = function(x) "inexistente"
    )),
    "no identifica"
  )
  expect_error(
    guiar_limpieza(plan, 1:2, selector = function(x) 0L),
    "data.frame"
  )
})

test_that("el contrato valida las propiedades compartidas de cada grupo", {
  datos <- data.frame(x = c(1, 1, 2))
  base <- planificar_limpieza(perfilar(datos))
  destructiva <- which(base$estrategia == "conservar_primera_duplicada")
  grupo <- base$grupo[[destructiva]]
  indices <- which(base$grupo == grupo)

  invalido <- base
  invalido$grupo <- factor(invalido$grupo)
  expect_error(aplicar(invalido, datos), "grupo.*texto")

  invalido <- base
  invalido$recomendada[[1L]] <- NA
  expect_error(aplicar(invalido, datos), "recomendada.*lógica sin NA")

  invalido <- base
  invalido$estado <- as.character(invalido$estado)
  invalido$estado[[1L]] <- "desconocida"
  expect_error(aplicar(invalido, datos), "estado.*no reconocido")

  invalido <- base
  invalido$recomendada[[destructiva]] <- TRUE
  expect_error(aplicar(invalido, datos), "nunca puede ser recomendada")

  invalido <- base
  invalido$reversible[[destructiva]] <- TRUE
  expect_error(aplicar(invalido, datos), "reversible = FALSE")

  invalido <- base
  invalido$estrategia[indices[[2L]]] <- invalido$estrategia[indices[[1L]]]
  expect_error(aplicar(invalido, datos), "repite una estrategia")

  invalido <- base
  invalido$decision_grupo[indices[[2L]]] <- "omitida"
  expect_error(aplicar(invalido, datos), "compartir una sola decisión")

  invalido <- base
  invalido$recomendacion_grupo[indices[[2L]]] <- "otra_recomendacion"
  expect_error(aplicar(invalido, datos), "recomendaciones incompatibles")
})

test_that("la marca de ausentes es una alternativa conservadora", {
  datos <- data.frame(id = 1:4, valor = c(10, NA, 30, NA))
  plan <- activar_estrategia(
    planificar_limpieza(perfilar(datos)), "marcar_filas_ausentes"
  )
  resultado <- aplicar(plan, datos)

  expect_equal(resultado$datos$.ausente_valor, c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(resultado$registro$n_cambiadas, 2)
  expect_length(resultado$eliminados$filas, 0L)
})

test_that("las columnas duplicadas se anotan y detectan deriva", {
  datos <- data.frame(a = 1:3, b = 1:3, c = 1:3)
  plan <- planificar_limpieza(perfilar(datos))
  resultado <- aplicar(plan, datos)
  marcas <- attr(resultado$datos, "columnas_duplicadas_marcadas")

  expect_equal(nrow(marcas), 3L)
  expect_setequal(paste(marcas$columna_1, marcas$columna_2), c(
    "a b", "a c", "b c"
  ))

  datos_derivados <- datos
  datos_derivados$b[[1L]] <- 99
  fallo <- aplicar(plan, datos_derivados)
  expect_true(any(grepl("dejaron de tener contenido", fallo$registro$error)))

  perfil_incompleto <- perfilar(datos)
  perfil_incompleto$general$columnas_duplicadas <- NULL
  hallazgo <- perfil_incompleto$hallazgos[
    perfil_incompleto$hallazgos$tipo_hallazgo == "columnas_duplicadas", ,
    drop = FALSE
  ][1L, , drop = FALSE]
  expect_null(lupa:::.par_columnas_duplicadas(perfil_incompleto, hallazgo))
})

test_that("la eliminación comprueba que una columna siga siendo constante", {
  original <- data.frame(x = rep("UY", 3), id = 1:3,
                         stringsAsFactors = FALSE)
  plan <- activar_estrategia(
    planificar_limpieza(perfilar(original)), "eliminar_columna_constante"
  )
  derivados <- original
  derivados$x[[3L]] <- "AR"
  fallo <- aplicar(plan, derivados, permitir_eliminacion = TRUE)
  expect_match(fallo$registro$error, "dejó de ser constante")
})

test_that("las transformaciones de capitalización devuelven texto y validan", {
  datos <- data.frame(zona = factor(c("Norte", "NORTE", "sur", "SUR")))
  plan <- activar_estrategia(
    planificar_limpieza(perfilar(datos)), "convertir_minusculas"
  )
  resultado <- aplicar(plan, datos)$datos
  expect_type(resultado$zona, "character")
  expect_identical(resultado$zona, c("norte", "norte", "sur", "sur"))

  expect_error(
    lupa:::.transformar_capitalizacion(1:2, "convertir_minusculas", list()),
    "columna de texto"
  )
  expect_error(
    lupa:::.transformar_capitalizacion(
      c("Norte", "Sur"), "convertir_segun_diccionario", list(diccionario = 1:2)
    ),
    "vector atómico con nombres"
  )
})

test_that("los auxiliares destructivos cubren tablas especiales", {
  sin_repetidos <- lupa:::.grupos_filas_duplicadas(data.frame(x = 1:3))
  expect_true(all(is.na(sin_repetidos$grupos)))

  sin_columnas <- data.frame(row.names = c("a", "b"))
  grupos_vacios <- lupa:::.grupos_filas_duplicadas(sin_columnas)
  expect_equal(grupos_vacios$grupos, c(1L, 1L))

  con_lista <- data.frame(x = I(list(1, 1)))
  expect_error(
    lupa:::.grupos_filas_duplicadas(con_lista), "columnas de lista"
  )
  expect_error(
    lupa:::.conservar_mas_completa(con_lista, "x"),
    "clave no puede contener columnas de lista"
  )
  expect_false(lupa:::.contenido_igual(1:2, 1:3))
  expect_false(lupa:::.contenido_igual(c(1, NA), c(1, 2)))
})

test_that("data.table preserva el original también al eliminar", {
  skip_if_not_installed("data.table")
  datos <- data.table::data.table(id = c(1, 1, 2), valor = c("A", "A", "B"))
  original <- data.table::copy(datos)
  plan <- activar_estrategia(
    planificar_limpieza(perfilar(datos)), "conservar_primera_duplicada"
  )
  resultado <- aplicar(plan, datos, permitir_eliminacion = TRUE)

  expect_identical(datos, original)
  expect_s3_class(resultado$datos, "data.table")
  expect_equal(nrow(resultado$datos), 2L)
  expect_s3_class(resultado$eliminados$filas[[1L]], "data.table")
})

test_that("los ejemplos guiados se calculan sobre los datos recibidos", {
  casos <- list(
    duplicados = data.frame(x = c(1, 1, 2)),
    columnas = data.frame(a = 1:3, b = 1:3),
    nombres = structure(data.frame(x = 1:2), names = " nombre +"),
    numericos = data.frame(x = c(999, 1:9)),
    extremos = data.frame(x = c(rep(1, 20), 100)),
    ausentes = data.frame(x = c(1, NA, 2)),
    constante = data.frame(x = rep("UY", 3))
  )
  tipos <- c(
    duplicados = "filas_duplicadas",
    columnas = "columnas_duplicadas",
    nombres = "nombres_columnas_problematicos",
    numericos = "faltantes_disfrazados",
    extremos = "outliers",
    ausentes = "faltantes",
    constante = "constante"
  )
  for (nombre in names(casos)) {
    datos <- casos[[nombre]]
    plan <- planificar_limpieza(perfilar(datos))
    acciones <- plan[plan$hallazgo == tipos[[nombre]], , drop = FALSE]
    ejemplos <- lupa:::.ejemplos_grupo(acciones, datos, 3L)
    expect_true(length(ejemplos) > 0L, info = nombre)
    expect_true(all(nzchar(ejemplos)), info = nombre)
  }
  expect_equal(lupa:::.texto_ejemplo(NA), "<NA>")
})

test_that("el selector acepta posiciones e identificadores", {
  datos <- data.frame(zona = c("Norte", "NORTE", "sur", "SUR"))
  plan <- planificar_limpieza(perfilar(datos))
  por_posicion <- suppressMessages(guiar_limpieza(
    plan, datos, selector = function(decision) 1L
  ))
  expect_true(por_posicion$aplicar[
    por_posicion$estrategia == "convertir_minusculas"
  ])

  por_omision <- suppressMessages(guiar_limpieza(
    plan, datos,
    selector = function(decision) length(decision$elegibles) + 1L
  ))
  expect_gt(nrow(por_omision), 0L)
  expect_false(any(por_omision$aplicar))

  id <- plan$id_accion[plan$estrategia == "convertir_mayusculas"]
  por_id <- suppressMessages(guiar_limpieza(
    plan, datos, selector = function(decision) id
  ))
  expect_true(por_id$aplicar[por_id$id_accion == id])
})

test_that("el modo guiado identifica la recomendación explícita de conservar", {
  datos <- data.frame(x = rep("UY", 3), id = 1:3)
  plan <- planificar_limpieza(perfilar(datos))
  observado <- NULL
  guiado <- suppressMessages(guiar_limpieza(
    plan, datos,
    selector = function(decision) {
      observado <<- decision$opciones
      length(decision$opciones)
    }
  ))

  expect_match(tail(observado, 1L), "No hacer nada \\(Recomendado\\)")
  expect_true(all(as.character(guiado$decision_grupo[!is.na(guiado$grupo)]) ==
                    "omitida"))
  expect_gt(nrow(guiado), 0L)
  expect_false(any(guiado$aplicar))
})

test_that("la impresión hace visibles las acciones y resultados destructivos", {
  datos <- data.frame(x = c(1, 1, 2))
  plan <- activar_estrategia(
    planificar_limpieza(perfilar(datos)), "conservar_primera_duplicada"
  )
  salida_plan <- suppressMessages(capture.output(print(plan)))
  expect_match(paste(salida_plan, collapse = "\n"), "destructiva")

  resultado <- aplicar(plan, datos, permitir_eliminacion = TRUE)
  mensajes <- capture.output(print(resultado), type = "message")
  expect_match(paste(mensajes, collapse = "\n"), "filas y.*columnas eliminadas")
})
