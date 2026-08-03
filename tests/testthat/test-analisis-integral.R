test_that("las distribuciones acotan valores y calculan cuantiles", {
  datos <- data.frame(
    categoria = c("b", "a", "b", "c", "d"),
    numero = c(1, 2, 3, 4, Inf),
    stringsAsFactors = FALSE
  )
  resultado <- distribucion_valores(
    datos, max_valores = 2, probabilidades = c(0, 0.5, 1), muestra = Inf,
    proteger_datos_personales = FALSE
  )

  expect_s3_class(resultado, "distribuciones_perfil")
  categoria <- resultado$frecuencias[
    resultado$frecuencias$columna == "categoria", , drop = FALSE
  ]
  expect_equal(categoria$valor, c("b", "a"))
  expect_equal(categoria$frecuencia, c(2L, 1L))
  expect_true(resultado$alcance$truncado[
    resultado$alcance$columna == "categoria"
  ])
  expect_equal(
    resultado$cuantiles$valor[resultado$cuantiles$columna == "numero"],
    c(1, 2.5, 4)
  )
  expect_true(all(resultado$frecuencias$proporcion >= 0 &
                    resultado$frecuencias$proporcion <= 1))
})

test_that("las distribuciones declaran muestreo, tipos no comparables y proteccion", {
  datos <- data.frame(
    nombre = c("Maria Perez", "Juan Gomez", "Ana Silva", "Luis Diaz"),
    x = 1:4, stringsAsFactors = FALSE
  )
  datos$lista <- I(list(1, 2, 3, 4))
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  resultado <- distribucion_valores(datos, perfil, max_valores = 1, muestra = 2)

  expect_true(all(resultado$alcance$muestreado[resultado$alcance$columna != "lista"]))
  expect_equal(
    resultado$alcance$estado[resultado$alcance$columna == "lista"],
    "tipo_no_comparable"
  )
  expect_true(resultado$alcance$protegida[resultado$alcance$columna == "nombre"])
  expect_true(all(resultado$frecuencias$valor[
    resultado$frecuencias$columna == "nombre"
  ] == "[valor protegido]"))
  expect_equal(nrow(distribucion_valores(data.frame())$alcance), 0L)
})

test_that("distribucion_valores valida su contrato", {
  datos <- data.frame(x = 1:3)
  expect_error(distribucion_valores(1:3), "data.frame")
  expect_error(distribucion_valores(datos, data.frame()), "perfil")
  expect_error(distribucion_valores(datos, max_valores = 0), "entero positivo")
  expect_error(distribucion_valores(datos, probabilidades = c(-1, 1)), "\u005b0, 1\u005d")
  expect_error(distribucion_valores(datos, muestra = 0), "positivo")
  expect_error(
    distribucion_valores(datos, proteger_datos_personales = NA), "TRUE o FALSE"
  )
  sin_valores <- distribucion_valores(data.frame(x = c(NA_real_, NA_real_)))
  expect_equal(sin_valores$alcance$estado, "sin_valores")
})

test_that("las asociaciones usan la medida acorde al par de columnas", {
  datos <- data.frame(
    x = 1:40,
    y = 2 * (1:40),
    categoria = rep(c("A", "B"), each = 20),
    categoria_2 = rep(c("sur", "norte"), each = 20),
    stringsAsFactors = FALSE
  )
  resultado <- detectar_asociaciones(datos, umbral = 0, muestra = Inf)

  expect_s3_class(resultado, "asociaciones_columnas")
  metodos <- setNames(
    resultado$metodo,
    paste(resultado$columna_1, resultado$columna_2, sep = "|")
  )
  expect_equal(metodos[["x|y"]], "pearson_absoluto")
  expect_equal(metodos[["categoria|categoria_2"]], "cramer_v")
  expect_true(any(resultado$metodo == "eta2"))
  expect_equal(resultado$asociacion[metodos == "pearson_absoluto"], 1)
  expect_true(all(resultado$asociacion >= 0 & resultado$asociacion <= 1))
  expect_match(resultado$supuesto[resultado$metodo == "pearson_absoluto"],
               "no queda confirmada")
})

test_that("las asociaciones no duplican dependencias y declaran recortes", {
  datos <- data.frame(
    a = rep(letters[1:4], 10), b = rep(LETTERS[1:4], 10),
    x = 1:40, y = 40:1, z = rep(c(0, 1), 20), stringsAsFactors = FALSE
  )
  dependencias <- data.frame(
    determinante = "a", dependiente = "b", exacta = TRUE,
    stringsAsFactors = FALSE
  )
  resultado <- detectar_asociaciones(
    datos, dependencias, umbral = 0, muestra = 20,
    max_columnas = 4, max_pares = 1
  )

  expect_true(attr(resultado, "muestreado"))
  expect_equal(attr(resultado, "filas_analizadas"), 20L)
  expect_length(attr(resultado, "columnas_omitidas"), 1L)
  expect_length(attr(resultado, "columnas_omitidas_limite"), 1L)
  expect_true(attr(resultado, "truncado_columnas"))
  expect_true(attr(resultado, "truncado"))
  expect_equal(nrow(resultado), 1L)
  expect_gt(attr(resultado, "pares_omitidos_dependencia"), 0L)
  expect_false(any(
    (resultado$columna_1 == "a" & resultado$columna_2 == "b") |
      (resultado$columna_1 == "b" & resultado$columna_2 == "a")
  ))
})

test_that("detectar_asociaciones valida entradas y casos degenerados", {
  datos <- data.frame(a = 1:3, b = 1:3)
  expect_error(detectar_asociaciones(1:3), "data.frame")
  expect_error(detectar_asociaciones(datos, dependencias = 1), "data frame")
  expect_error(detectar_asociaciones(datos, umbral = 2), "\u005b0, 1\u005d")
  expect_error(detectar_asociaciones(datos, max_columnas = 0), "entero positivo")
  expect_error(detectar_asociaciones(datos, max_niveles = 0), "entero positivo")
  expect_error(detectar_asociaciones(datos, max_pares = 0), "entero positivo")
  expect_equal(nrow(detectar_asociaciones(data.frame(a = 1:3))), 0L)
  expect_true(is.na(lupa:::.cramer_v(c("a", "a"), c("b", "b"))))
  expect_true(is.na(lupa:::.eta2(c("a", "b"), c(1, 1))))
  poco_solape <- detectar_asociaciones(data.frame(
    a = c(1, 2, NA), b = c(NA, 2, 3)
  ), umbral = 0)
  expect_equal(nrow(poco_solape), 0L)
})

test_that("el analisis temporal informa dias, huecos y regularidad", {
  fechas <- as.Date("2026-01-01") + c(0:9, 26:45)
  resultado <- analizar_tiempo(
    data.frame(fecha = fechas), frecuencia_dias = 1, max_huecos = 5
  )

  expect_s3_class(resultado, "analisis_temporal")
  expect_equal(resultado$resumen$n_fechas_esperadas_ausentes, 16L)
  expect_equal(resultado$resumen$n_grupos_huecos, 1L)
  expect_equal(resultado$huecos$duracion_dias, 16)
  expect_equal(sum(resultado$dias_semana$frecuencia), length(fechas))
  expect_equal(sum(resultado$dias_semana$proporcion), 1)
  expect_equal(resultado$resumen$monotonicidad, 1)
  expect_false(resultado$propuestas$confirmada)
})

test_that("el calendario laboral no inventa huecos de fin de semana", {
  fechas <- seq.Date(as.Date("2026-01-05"), as.Date("2026-01-16"), by = "day")
  fechas <- fechas[as.integer(format(fechas, "%u")) %in% 1:5]
  resultado <- analizar_tiempo(
    data.frame(fecha = c(fechas, fechas[[1L]])), calendario = 1:5,
    frecuencia_dias = 1
  )

  expect_equal(resultado$resumen$n_fechas_esperadas_ausentes, 0L)
  expect_equal(resultado$resumen$n_fechas_fuera_calendario, 0L)
  expect_equal(resultado$resumen$n_duplicados_temporales, 1L)
  expect_equal(resultado$resumen$cobertura_periodo, 1)
  expect_equal(resultado$propuestas$confianza, 1)
  expect_true(all(!resultado$dias_semana$esperado[
    resultado$dias_semana$dia %in% c("sabado", "domingo")
  ]))

  con_domingo <- analizar_tiempo(
    data.frame(fecha = c(fechas, as.Date("2026-01-11"))),
    calendario = 1:5, frecuencia_dias = 1
  )
  expect_equal(con_domingo$resumen$n_fechas_fuera_calendario, 1L)
})

test_that("la confianza temporal no excede la cobertura observada", {
  fechas <- as.Date("2024-01-01") + c(0, 3, 17, 40, 95, 200)
  resultado <- analizar_tiempo(data.frame(fecha = fechas))

  expect_equal(resultado$propuestas$contiguidad, 1)
  expect_equal(
    resultado$propuestas$cobertura_periodo,
    resultado$resumen$cobertura_periodo
  )
  expect_equal(
    resultado$propuestas$confianza,
    min(
      resultado$propuestas$contiguidad,
      resultado$propuestas$cobertura_periodo
    )
  )
  expect_lt(resultado$propuestas$confianza, 0.04)
  expect_false(resultado$propuestas$confirmada)
})

test_that("el analisis temporal maneja texto, ambiguedad y recortes", {
  datos <- data.frame(
    iso = c("2026-01-01", "2026-01-03"),
    ambigua = c("03-04-2023", "05-06-2023"),
    otra = as.Date("2026-01-01") + 0:1,
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  resultado <- analizar_tiempo(datos, perfil, max_columnas = 1, max_huecos = 1)

  expect_equal(resultado$resumen$columna, "iso")
  expect_true(attr(resultado, "truncado"))
  expect_equal(attr(resultado, "columnas_omitidas"), "otra")
  expect_false("ambigua" %in% attr(resultado, "columnas_analizadas"))

  muchos <- analizar_tiempo(
    data.frame(fecha = as.Date("2026-01-01") + c(0, 2, 4, 6)),
    frecuencia_dias = 1, max_huecos = 1
  )
  expect_true(muchos$resumen$huecos_truncados)
  expect_equal(nrow(muchos$huecos), 1L)
})

test_that("analizar_tiempo valida su contrato", {
  datos <- data.frame(f = as.Date("2026-01-01"))
  expect_error(analizar_tiempo(1:3), "data.frame")
  expect_error(analizar_tiempo(datos, data.frame()), "perfil")
  expect_error(analizar_tiempo(datos, columnas = "x"), "inexistentes")
  expect_error(analizar_tiempo(datos, calendario = 0), "dias ISO")
  expect_error(analizar_tiempo(datos, frecuencia_dias = 0), "positivo")
  expect_error(analizar_tiempo(datos, frecuencia_dias = 0.5), "entero positivo")
  expect_error(analizar_tiempo(datos, max_huecos = 0), "entero positivo")
  expect_equal(nrow(analizar_tiempo(data.frame(x = 1:3))$resumen), 0L)
  expect_equal(length(lupa:::.grupos_huecos(integer())), 0L)
  posix <- lupa:::.fecha_columna_avanzada(
    as.POSIXct("2026-01-01 10:00:00", tz = "UTC")
  )
  expect_equal(posix, as.Date("2026-01-01"))
  expect_null(lupa:::.fecha_columna_avanzada(1:3))
  expect_null(lupa:::.fecha_columna_avanzada(c("03-04-2023", "05-06-2023")))
  manual <- analizar_tiempo(datos, columnas = c("f"))
  expect_equal(nrow(manual$resumen), 1L)
  no_temporal <- analizar_tiempo(data.frame(x = 1:3), columnas = "x")
  expect_equal(nrow(no_temporal$resumen), 0L)
})

test_that("clasificar_variables conserva escalas y niveles declarados", {
  datos <- data.frame(
    prioridad = ordered(
      c("baja", "alta"), levels = c("baja", "media", "alta")
    ),
    zona = factor(c("sur", "sur"), levels = c("sur", "norte")),
    activo = c(TRUE, FALSE), cantidad = c(1L, 2L)
  )
  resultado <- clasificar_variables(datos)

  expect_s3_class(resultado, "clasificacion_variables")
  expect_equal(resultado$escala_propuesta[resultado$columna == "prioridad"],
               "ordinal")
  expect_true(resultado$confirmada[resultado$columna == "prioridad"])
  expect_equal(resultado$niveles_ausentes[[1L]], "media")
  expect_equal(resultado$niveles_ausentes[[2L]], "norte")
  expect_equal(resultado$escala_propuesta[resultado$columna == "activo"],
               "binaria")
  expect_false(resultado$confirmada[resultado$columna == "cantidad"])
})

test_that("los metadatos confirman sin inferir y protegen niveles sensibles", {
  datos <- data.frame(
    codigo = c(1, 2, 3),
    nombre = c("Maria Perez", "Juan Gomez", "Ana Silva"),
    stringsAsFactors = FALSE
  )
  attr(datos$codigo, "units") <- "personas"
  metadatos <- data.frame(
    columna = "codigo", escala = "ordinal", rol = "nivel",
    confianza = 0.95, confirmada = TRUE, unidad = "grado",
    stringsAsFactors = FALSE
  )
  metadatos$niveles <- I(list(c("1", "2", "3", "4")))
  resultado <- clasificar_variables(datos, metadatos = metadatos)

  codigo <- resultado[resultado$columna == "codigo", , drop = FALSE]
  expect_equal(codigo$escala_propuesta, "ordinal")
  expect_equal(codigo$rol, "nivel")
  expect_equal(codigo$unidad, "grado")
  expect_true(codigo$confirmada)
  expect_equal(codigo$niveles_ausentes[[1L]], "4")
  nombre <- resultado[resultado$columna == "nombre", , drop = FALSE]
  expect_true(all(nombre$niveles_observados[[1L]] == "[valor protegido]"))
})

test_that("clasificar_variables valida metadatos y declara muestreo", {
  datos <- data.frame(x = letters[1:10])
  resultado <- clasificar_variables(
    datos, muestra = 3, max_niveles = 2, proteger_datos_personales = FALSE
  )
  expect_true(resultado$niveles_muestreados)
  expect_true(resultado$niveles_truncados)
  expect_error(clasificar_variables(1:3), "data.frame")
  expect_error(clasificar_variables(datos, data.frame()), "perfil")
  expect_error(clasificar_variables(datos, max_niveles = 0), "entero positivo")
  expect_error(
    clasificar_variables(datos, proteger_datos_personales = NA), "TRUE o FALSE"
  )
  expect_error(
    clasificar_variables(datos, metadatos = data.frame(columna = "y")),
    "columna existente"
  )
  expect_error(clasificar_variables(
    datos, metadatos = data.frame(columna = "x", extra = 1)
  ), "no reconocidos")
  expect_error(clasificar_variables(
    datos, metadatos = data.frame(columna = "x", escala = "intervalo")
  ), "no reconocida")
  expect_error(clasificar_variables(
    datos, metadatos = data.frame(
      columna = c("x", "x"), escala = c("nominal", "nominal")
    )
  ), "fila unica")
  expect_error(clasificar_variables(
    datos, metadatos = data.frame(
      columna = "x", escala = "nominal", confianza = 2
    )
  ), "\u005b0, 1\u005d")
  expect_error(clasificar_variables(
    datos, metadatos = data.frame(columna = "x", confirmada = NA)
  ), "logica")
  expect_equal(nrow(clasificar_variables(data.frame())), 0L)
})

test_that("las señales explícitas y los tipos restantes se distinguen", {
  medida <- c(1, 2, 3)
  attr(medida, "measure") <- "scale"
  etiquetada <- c(1, 2, 1)
  attr(etiquetada, "labels") <- c(uno = 1, dos = 2)
  continua <- c(1.2, 2.5, 4.1)
  attr(continua, "units") <- "kg"
  lista <- I(list(1, 2, 3))
  datos <- data.frame(
    medida = medida, fecha = as.Date("2026-01-01") + 0:2,
    etiquetada = etiquetada, continua = continua
  )
  datos$lista <- lista
  resultado <- clasificar_variables(
    datos, proteger_datos_personales = FALSE
  )

  expect_equal(resultado$escala_propuesta[resultado$columna == "medida"],
               "continua")
  expect_true(resultado$confirmada[resultado$columna == "medida"])
  expect_equal(resultado$escala_propuesta[resultado$columna == "fecha"],
               "temporal")
  expect_equal(resultado$escala_propuesta[resultado$columna == "etiquetada"],
               "nominal")
  expect_false(resultado$confirmada[resultado$columna == "etiquetada"])
  expect_equal(resultado$escala_propuesta[resultado$columna == "continua"],
               "continua")
  expect_equal(resultado$unidad[resultado$columna == "continua"], "kg")
  expect_true(resultado$confirmada[resultado$columna == "continua"])
  expect_equal(resultado$escala_propuesta[resultado$columna == "lista"],
               "desconocida")
  expect_equal(lupa:::.metricas_por_escala("nominal", "identificador"),
               "Unicidad; integridad referencial")
  expect_equal(lupa:::.metricas_por_escala("desconocida", "otro"),
               "Requiere declaracion")

  confirmacion <- clasificar_variables(
    data.frame(x = c(1, 2, 3)),
    metadatos = data.frame(
      columna = "x", confirmada = TRUE, confianza = 0.9,
      stringsAsFactors = FALSE
    )
  )
  expect_true(confirmacion$confirmada)
  expect_equal(confirmacion$confianza, 0.9)
})

test_that("analizar es descriptivo por omision e incluye cobertura", {
  fecha <- as.POSIXct("2026-02-01", tz = "UTC")
  resultado <- analizar(
    datos_administrativos, fecha = fecha, analizar_dependencias = FALSE,
    max_valores = 3
  )

  expect_s3_class(resultado, "analisis")
  expect_s3_class(resultado$perfil, "perfil")
  expect_s3_class(resultado$propuesta_modelo, "propuesta_modelo")
  expect_s3_class(resultado$plan_limpieza, "plan_limpieza")
  expect_true(all(c("estado", "motivo", "como_resolverlo") %in%
                    names(resultado$cobertura)))
  expect_null(resultado$medicion)
  expect_null(resultado$evaluacion)
  expect_null(resultado$datos)
  expect_false(resultado$meta$modelo_medido)
  expect_equal(resultado$meta$fecha, fecha)
  mensajes <- utils::capture.output(
    salida <- utils::capture.output(impreso <- print(resultado)),
    type = "message"
  )
  expect_match(paste(mensajes, collapse = "\n"), "Analisis de datos")
  expect_identical(impreso, resultado)
  expect_true(length(salida) > 0L)
})

test_that("analizar acepta tablas comunes sin mutarlas", {
  testthat::skip_if_not_installed("data.table")
  datos <- data.table::data.table(
    codigo = rep(1:3, each = 2), valor = c(1, 2, 3, 4, 5, NA)
  )
  antes <- data.table::copy(datos)
  resultado <- analizar(datos, analizar_dependencias = FALSE)

  expect_s3_class(resultado, "analisis")
  expect_identical(datos, antes)

  testthat::skip_if_not_installed("tibble")
  tabla <- tibble::tibble(codigo = 1:3, valor = c("a", "b", "c"))
  expect_s3_class(
    analizar(tabla, analizar_dependencias = FALSE), "analisis"
  )
})

test_that("analizar mide solo decisiones confirmadas", {
  datos <- data.frame(x = c(1, NA, 3))
  especifica <- especializar(metricas_nucleo()$NoNulo, nombre_especifico = "Presencia")
  confirmado <- modelo(instanciar(especifica, "tabla", "x"))
  regla <- regla_evaluacion("Cumple", function(x) x > 0.5)
  perfil_eval <- perfil_evaluacion("Basico", regla)
  resultado <- analizar(
    datos, modelo_confirmado = confirmado, perfil_evaluacion = perfil_eval,
    analizar_dependencias = FALSE
  )

  expect_s3_class(resultado$medicion, "medicion")
  expect_s3_class(resultado$evaluacion, "evaluacion_calidad")
  expect_true(resultado$meta$modelo_medido)
  expect_true(resultado$meta$evaluado)

  propuesta <- resultado$propuesta_modelo
  propuesta$incluir[] <- FALSE
  propuesta$incluir[propuesta$metrica == "NoNulo"] <- TRUE
  desde_propuesta <- analizar(
    datos, propuesta_confirmada = propuesta, analizar_dependencias = FALSE
  )
  expect_s3_class(desde_propuesta$medicion, "medicion")
})

test_that("analizar valida decisiones y argumentos coordinados", {
  datos <- data.frame(x = 1:3)
  modelo_x <- modelo(instanciar(
    especializar(metricas_nucleo()$NoNulo), "tabla", "x"
  ))
  propuesta <- proponer_modelo(perfilar(datos, analizar_dependencias = FALSE), datos)
  expect_error(analizar(1:3), "data.frame")
  expect_error(analizar(datos, argumentos_perfil = list(1)), "nombres")
  expect_error(analizar(datos, argumentos_perfil = list(nombre = "x")),
               "no puede reemplazar")
  expect_error(analizar(datos, argumentos_perfil = list(a = 1), a = 2),
               "repetirse")
  expect_error(analizar(datos, conservar_datos = NA), "logicos")
  expect_error(analizar(datos, modelo_confirmado = data.frame()), "modelo")
  expect_error(analizar(datos, propuesta_confirmada = data.frame()), "propuesta")
  expect_error(analizar(
    datos, modelo_confirmado = modelo_x, propuesta_confirmada = propuesta
  ), "no ambos")
  expect_error(analizar(datos, perfil_evaluacion = list()), "requiere un modelo")
  sin_proteccion <- analizar(
    datos, proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  )
  expect_s3_class(sin_proteccion, "analisis")
})

test_that("las advertencias enumeran cada recorte sin ocultarlo", {
  distribuciones <- list(alcance = data.frame(
    muestreado = TRUE, truncado = TRUE
  ))
  asociaciones <- data.frame()
  attr(asociaciones, "muestreado") <- TRUE
  attr(asociaciones, "columnas_omitidas") <- "c"
  attr(asociaciones, "columnas_omitidas_limite") <- "c"
  attr(asociaciones, "truncado") <- TRUE
  temporal <- list(
    propuestas = data.frame(confirmada = FALSE),
    resumen = data.frame(huecos_truncados = TRUE)
  )
  attr(temporal, "truncado") <- TRUE
  variables <- data.frame(confirmada = FALSE, n_niveles_ausentes = 1L)
  propuesta <- data.frame()
  attr(propuesta, "truncado") <- TRUE
  resultado <- lupa:::.advertencias_analisis(
    distribuciones, asociaciones, temporal, variables, propuesta
  )

  expect_equal(nrow(resultado), 11L)
  expect_setequal(
    resultado$componente,
    c("distribuciones", "asociaciones", "tiempo", "variables", "modelo")
  )
  expect_true(all(as.character(resultado$severidad) == "sospechoso"))
})

test_that("la persistencia excluye datos y funciones por omision", {
  datos <- data.frame(
    codigo = rep(1:3, each = 4),
    descripcion = rep(c("A", "B", "C"), each = 4),
    stringsAsFactors = FALSE
  )
  resultado <- analizar(datos, muestra = Inf)
  archivo <- tempfile(fileext = ".rds")
  ruta <- withVisible(guardar_analisis(resultado, archivo))
  recuperado <- leer_analisis(archivo)

  expect_false(ruta$visible)
  expect_equal(ruta$value, normalizePath(archivo, winslash = "/"))
  expect_null(recuperado$datos)
  expect_equal(recuperado$meta$persistencia$version_esquema, 1L)
  expect_false(recuperado$meta$persistencia$datos_incluidos)
  expect_gt(recuperado$meta$persistencia$funciones_sustituidas, 0L)
  filas_fd <- grepl("dependencia_funcional", recuperado$propuesta_modelo$origen)
  expect_true(all(recuperado$propuesta_modelo$estado[filas_fd] == "requiere_datos"))
  expect_lt(file.info(archivo)$size, 1e6)
})

test_that("las dependencias se reconstruyen solo con datos incluidos", {
  datos <- data.frame(
    codigo = rep(1:3, each = 4),
    descripcion = rep(c("A", "B", "C"), each = 4),
    stringsAsFactors = FALSE
  )
  resultado <- analizar(datos, conservar_datos = TRUE, muestra = Inf)
  archivo <- tempfile(fileext = ".rds")
  guardar_analisis(
    resultado, archivo, incluir_datos = TRUE,
    proteger_datos_personales = FALSE
  )
  recuperado <- leer_analisis(archivo)
  filas_fd <- grepl("dependencia_funcional", recuperado$propuesta_modelo$origen)

  expect_s3_class(recuperado$datos, "data.frame")
  expect_true(all(recuperado$propuesta_modelo$estado[filas_fd] == "lista"))
  expect_true(all(vapply(
    recuperado$propuesta_modelo$configuracion[filas_fd],
    function(x) is.function(x$regla), logical(1L)
  )))
})

test_that("una funcion arbitraria persiste como requisito explícito", {
  datos <- data.frame(x = c("A", "B", "A"), stringsAsFactors = FALSE)
  propuesta <- proponer_modelo(
    perfilar(datos, analizar_dependencias = FALSE), datos
  )
  propuesta$configuracion[[1L]]$validador <- function(x) !is.na(x)
  propuesta$incluir[[1L]] <- TRUE
  deshidratada <- lupa:::.deshidratar_propuesta(propuesta)
  recuperada <- lupa:::.hidratar_propuesta(deshidratada$propuesta, NULL)

  expect_gt(deshidratada$n_funciones, 0L)
  expect_equal(recuperada$estado[[1L]], "requiere_funcion")
  expect_false(recuperada$incluir[[1L]])
  expect_equal(lupa:::.deshidratar_propuesta(data.frame())$n_funciones, 0L)
  expect_s3_class(lupa:::.hidratar_propuesta(data.frame(), NULL), "data.frame")
})

test_that("la propuesta persistida protege dominios personales", {
  datos <- data.frame(
    nombre = c("Maria Perez", "Juan Gomez", "Maria Perez"),
    stringsAsFactors = FALSE
  )
  propuesta <- proponer_modelo(
    perfilar(datos, analizar_dependencias = FALSE,
             proteger_datos_personales = FALSE), datos
  )
  indice <- which(vapply(
    propuesta$configuracion,
    function(x) any(c("valores", "diccionario") %in% names(x)), logical(1L)
  ))
  if (!length(indice)) {
    indice <- which(vapply(
      propuesta$atributos_ligados,
      function(x) "nombre" %in% x, logical(1L)
    ))[[1L]]
    propuesta$configuracion[[indice]]$valores <- c("Maria Perez", "Juan Gomez")
  }
  protegida <- lupa:::.proteger_propuesta_analisis(propuesta, "nombre")

  expect_equal(
    unname(unlist(protegida$configuracion[[indice[[1L]]]][
      intersect(names(protegida$configuracion[[indice[[1L]]]]),
                c("valores", "diccionario"))
    ])), "[valor protegido]"
  )
  expect_false(protegida$incluir[[indice[[1L]]]])
  expect_equal(protegida$estado[[indice[[1L]]]], "requiere_valores_protegidos")
  expect_identical(lupa:::.proteger_propuesta_analisis(data.frame(), "x"),
                   data.frame())
})

test_that("guardar y leer analisis aplican salvaguardas", {
  datos <- data.frame(
    nombre = c("Maria Perez", "Juan Gomez"), stringsAsFactors = FALSE
  )
  resultado <- analizar(
    datos, conservar_datos = TRUE, analizar_dependencias = FALSE
  )
  archivo <- tempfile(fileext = ".rds")
  expect_error(guardar_analisis(data.frame(), archivo), "analizar")
  expect_error(guardar_analisis(resultado, ""), "ruta")
  expect_error(guardar_analisis(resultado, archivo, incluir_datos = NA), "logicos")
  expect_error(guardar_analisis(resultado, archivo, comprimir = "zip"), "admitido")
  expect_error(guardar_analisis(
    resultado, file.path(tempfile(), "x.rds")
  ), "directorio")
  expect_error(guardar_analisis(resultado, archivo, incluir_datos = TRUE),
               "datos personales")
  sin_datos <- resultado
  sin_datos$datos <- NULL
  sin_datos$perfil$datos_personales <- sin_datos$perfil$datos_personales[0, ]
  expect_error(guardar_analisis(
    sin_datos, tempfile(fileext = ".rds"), incluir_datos = TRUE
  ), "no conservo datos")
  guardar_analisis(resultado, archivo)
  expect_error(guardar_analisis(resultado, archivo), "ya existe")
  expect_silent(guardar_analisis(resultado, archivo, sobrescribir = TRUE))
  expect_error(leer_analisis(""), "RDS existente")

  invalido <- tempfile(fileext = ".rds")
  saveRDS(list(), invalido)
  expect_error(leer_analisis(invalido), "versionado")
  incompatible <- resultado
  incompatible$meta$version_esquema <- 99L
  saveRDS(incompatible, invalido)
  expect_error(leer_analisis(invalido), "no es compatible")

  sin_proteger <- analizar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  protegido <- tempfile(fileext = ".rds")
  guardar_analisis(sin_proteger, protegido)
  recuperado <- leer_analisis(protegido)
  textos <- c(
    recuperado$perfil$columnas$moda,
    recuperado$distribuciones$frecuencias$valor,
    recuperado$plan_limpieza$evidencia
  )
  expect_false(any(grepl("Maria Perez|Juan Gomez", textos)))
  expect_true(any(grepl("proteg", textos, ignore.case = TRUE)))
})

test_that("reportar acepta el analisis entero sin exponer valores personales", {
  datos <- data.frame(
    nombre = c("Maria Fernandez", "Juan Perez", "Ana Silva"),
    valor = c(1, 2, 3), stringsAsFactors = FALSE
  )
  resultado <- analizar(datos, analizar_dependencias = FALSE)
  archivo <- tempfile(fileext = ".html")
  reportar(resultado, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")

  expect_match(html, "Analisis integral", fixed = TRUE)
  expect_match(html, "Distribuciones y cuantiles", fixed = TRUE)
  expect_match(html, "Escalas y roles propuestos", fixed = TRUE)
  expect_match(html, "Cobertura del análisis", fixed = TRUE)
  expect_false(grepl("Maria Fernandez|Juan Perez|Ana Silva", html))
  expect_match(html, "[valor protegido]", fixed = TRUE)
  expect_error(
    reportar(resultado, archivo = tempfile(fileext = ".html"),
             proteger_datos_personales = NA),
    "TRUE o FALSE"
  )
})

test_that("el reporte tolera perfiles heredados y declara dependencias recortadas", {
  perfil <- perfilar(data.frame(x = 1:4), analizar_dependencias = FALSE)
  perfil$dependencias <- NULL
  expect_match(lupa:::.seccion_perfil(perfil, Inf, Inf),
               "Dependencias funcionales", fixed = TRUE)

  dependencias <- data.frame(
    determinante = "x", dependiente = "x", stringsAsFactors = FALSE
  )
  attr(dependencias, "truncado") <- TRUE
  attr(dependencias, "columnas_analizadas") <- "x"
  attr(dependencias, "columnas_omitidas") <- "y"
  perfil$dependencias <- dependencias
  expect_match(lupa:::.seccion_perfil(perfil, Inf, Inf),
               "quedaron fuera 1", fixed = TRUE)
})
