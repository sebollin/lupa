test_that("la granularidad es un grafo y declara los diez niveles", {
  niveles <- granularidades()
  transiciones <- transiciones_granularidad()

  expect_equal(nrow(niveles), 10L)
  expect_equal(niveles$granularidad[[1L]], "instanciaAtributo")
  # Las diez se miden. Las cuatro de arriba exigen que el usuario declare la
  # frontera -que tablas, que bases, que colecciones, que organizaciones-,
  # porque ninguna de las cuatro esta en los datos. `organizacion()` cerro las
  # dos ultimas: el alcance institucional se declara, no se adivina.
  expect_true(all(niveles$implementada))
  expect_false(is.ordered(niveles$granularidad))
  expect_true(any(
    transiciones$origen == "instanciaAtributo" &
      transiciones$destino == "atributo"
  ))
  expect_true(any(
    transiciones$origen == "instanciaAtributo" &
      transiciones$destino == "instanciaEntidad" &
      transiciones$fuente == "extension_documentada"
  ))
  expect_false(any(
    transiciones$origen == "atributo" &
      transiciones$destino == "instanciaEntidad"
  ))
})

test_that("EntidadContradictoria compara el vocabulario y declara su alcance", {
  skip_if_not_installed("stringdist")
  metrica <- especializar(metricas_nucleo()$EntidadContradictoria)
  instancia <- instanciar(metrica, "personas", "nombre")
  datos <- data.frame(
    nombre = c(
      "Organismo Nacional de Estadistica",
      "Organismo Nacional de Estadstica",
      "MONTEVIDEO", "Montevideo ", "Otra entidad", NA_character_
    ),
    stringsAsFactors = FALSE
  )
  medicion <- medir(modelo(instancia), datos)
  expect_equal(medicion$resultado, c(1, 1, 0, 0, 0))
  alcance <- attr(medicion, "alcance_metricas")[[1L]]
  expect_equal(alcance$unidad_conteo, "valor_distinto")
  expect_equal(alcance$n_valores_distintos_total, 4L)
  expect_equal(alcance$n_valores_distintos_comparados, 4L)
  expect_equal(alcance$n_pares_comparados, 6)
  expect_equal(alcance$n_pares_bajo_umbral, 1L)
  expect_equal(alcance$n_afectados, 2L)
  expect_false(alcance$truncado)
  expect_equal(agregar(medicion, "atributo", "ratio")$resultado, 2 / 5)

  recortada <- especializar(
    metricas_nucleo()$EntidadContradictoria, max_valores = 2L
  )
  medicion_recortada <- medir(
    modelo(instanciar(recortada, "personas", "nombre")), datos
  )
  alcance_recortado <- attr(medicion_recortada, "alcance_metricas")[[1L]]
  expect_true(alcance_recortado$truncado)
  expect_equal(alcance_recortado$n_valores_distintos_total, 4L)
  expect_equal(alcance_recortado$n_valores_distintos_comparados, 2L)
  expect_equal(alcance_recortado$alcance, "prefijo_vocabulario")
  expect_equal(nrow(medicion_recortada), 2L)
})

test_that("las métricas recorren los tres niveles mediante closures", {
  generica <- metrica(
    "MayorQue", "Indica si un valor supera el umbral.",
    "instanciaAtributo", "booleano", propiedades = "umbral"
  )
  especifica <- especializar(generica, umbral = 10)
  metodo <- function(tablas, instancia) {
    x <- tablas[[instancia$entidad]][[instancia$atributos]]
    filas <- seq_along(x)
    data.frame(
      resultado = x > instancia$configuracion$umbral,
      entidad = instancia$entidad,
      atributo = instancia$atributos,
      fila = filas,
      objeto = paste0("tabla$x[", filas, "]")
    )
  }
  instancia <- instanciar(
    especifica, "tabla", "x", metodo = metodo,
    referencial = list(origen = "contrato")
  )

  expect_true(is.function(generica))
  expect_s3_class(generica, "metrica_generica")
  expect_true("umbral" %in% names(formals(generica)))
  expect_true(is.function(especifica))
  expect_s3_class(especifica, "metrica_especifica")
  expect_equal(
    names(formals(especifica)),
    c("entidad", "atributos", "nombre_instancia", "metodo", "referencial")
  )
  expect_s3_class(instancia, "metrica_instanciada")
  expect_equal(instancia$configuracion$umbral, 10)
  expect_equal(instancia$referencial$origen, "contrato")

  resultado <- medir(
    modelo(instancia), data.frame(x = c(5, 15)),
    id_medicion = "prueba", fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
  expect_equal(resultado$resultado, c(0, 1))
  expect_true(all(resultado$id_medicion == "prueba"))
  expect_equal(length(unique(resultado$id_medida)), 2L)

  expect_error(especializar(generica), "Faltan propiedades")
  expect_error(especializar(generica, otra = 1), "no declaradas")
  expect_error(instanciar(generica, "tabla", "x"), "especializar")
  expect_error(modelo(instancia, instancia), "deben ser únicos")
})

test_that("el núcleo declara sus veintidós métricas", {
  nucleo <- metricas_nucleo()
  expect_named(nucleo, c(
    "NoNulo", "Formato", "ValoresPosiblesPorExtension",
    "ReglaIntegridadIntraEntidad", "ReglaIntegridadInterEntidad",
    "ErrorEstandar", "Escala", "ValoresPosiblesPorComprension", "AtributoDuplicado",
    "ConjuntoAtributosDuplicado", "EntidadDuplicada", "EntidadContradictoria",
    "DesactualizacionPorFormato", "DesactualizacionPorFecha",
    "DesactualizacionPorCambios", "OportunidadAtributoPorFecha",
    "OportunidadAtributoPorIntervalo", "GradoOportunidadAtributoPorFecha",
    "GradoOportunidadAtributoPorIntervalo", "OportunidadEntPorFecha",
    "OportunidadEntPorIntervalo", "DensidadPonderada"
  ))
  declaraciones <- lapply(nucleo, lupa:::.declaracion_metrica)
  expect_true(all(
    c("expresion_regular", "diccionario", "validador") %in%
      names(formals(nucleo$Formato))
  ))
  expect_equal(
    unname(vapply(declaraciones, `[[`, character(1L), "granularidad")),
    c(
      "instanciaAtributo", "instanciaAtributo", "instanciaAtributo",
      "instanciaEntidad", "entidad", "atributo", "instanciaAtributo",
      "instanciaAtributo", "instanciaAtributo", "instanciaEntidad",
      "instanciaEntidad", "instanciaAtributo", "instanciaAtributo",
      "instanciaAtributo", "instanciaAtributo", "instanciaAtributo",
      "instanciaAtributo", "instanciaAtributo", "instanciaAtributo",
      "instanciaEntidad", "instanciaEntidad", "instanciaEntidad"
    )
  )
  expect_equal(
    unname(vapply(declaraciones, `[[`, character(1L), "tipo_resultado")),
    c(
      rep("booleano", 4L), "real", "numero_real", "real",
      rep("booleano", 6L), "duracion", "entero", "booleano", "booleano",
      "real", "real", "booleano", "booleano", "real"
    )
  )

  no_nulo <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "NoNuloCodigo"),
    "personas", "codigo"
  )
  formato <- instanciar(
    especializar(
      nucleo$Formato, nombre_especifico = "FormatoCodigo",
      expresion_regular = "^AA[0-9]$"
    ),
    "personas", "codigo"
  )
  dominio <- instanciar(
    especializar(
      nucleo$ValoresPosiblesPorExtension,
      nombre_especifico = "DominioTipo", valores = c("A", "B")
    ),
    "personas", "tipo"
  )
  intra <- instanciar(
    especializar(
      nucleo$ReglaIntegridadIntraEntidad,
      nombre_especifico = "EdadAdulta",
      regla = function(x) x$edad >= 18
    ),
    "personas", "edad"
  )
  inter <- instanciar(
    especializar(
      nucleo$ReglaIntegridadInterEntidad,
      nombre_especifico = "TramiteTienePersona", muestra = Inf
    ),
    c("personas", "tramites"), c("id", "persona_id")
  )
  error_especifico <- especializar(
    nucleo$ErrorEstandar, nombre_especifico = "ErrorMonto"
  )
  error <- instanciar(error_especifico, "personas", "monto")
  error_2 <- instanciar(error_especifico, "personas", "monto_2")
  personas <- data.frame(
    id = 1:4, edad = c(20, 17, 40, 30), tipo = c("A", "B", "X", "A"),
    codigo = c("AA1", "mal", "AA3", NA), monto = c(10, 12, 9, 11),
    monto_2 = c(100, 110, 105, 115)
  )
  tramites <- data.frame(persona_id = c(1, 2, 9))
  medicion <- medir(
    modelo(no_nulo, formato, dominio, intra, inter, error, error_2),
    list(personas = personas, tramites = tramites), id_medicion = "nucleo"
  )

  expect_s3_class(medicion, "medicion")
  expect_equal(
    medicion$resultado[medicion$metrica_especifica == "FormatoCodigo"],
    c(1, 0, 1)
  )
  expect_equal(
    medicion$resultado[medicion$metrica_especifica == "DominioTipo"],
    c(1, 1, 0, 1)
  )
  expect_equal(
    medicion$resultado[medicion$metrica_especifica == "EdadAdulta"],
    c(1, 0, 1, 1)
  )
  expect_equal(
    medicion$resultado[medicion$metrica_especifica == "TramiteTienePersona"],
    2 / 3
  )
  error_medido <- medicion$resultado[
    medicion$metrica_especifica == "ErrorMonto"
  ]
  expect_equal(
    error_medido,
    c(stats::sd(personas$monto), stats::sd(personas$monto_2))
  )
  expect_equal(length(error_medido), 2L)

  errores <- medicion[
    medicion$metrica_especifica == "ErrorMonto", , drop = FALSE
  ]
  expect_error(agregar(errores, "entidad", "ratio_umbral", umbral = 0),
               "\u005b0, 1\u005d")
  expect_error(agregar(errores, "entidad", "promedio"), "\u005b0, 1\u005d")

  medidas_intra <- medicion[
    medicion$metrica_especifica == "EdadAdulta", , drop = FALSE
  ]
  expect_equal(agregar(medidas_intra, "entidad", "ratio")$resultado, 0.75)
})

test_that("NoNulo admite un diccionario explícito de ausencias", {
  no_nulo <- especializar(
    metricas_nucleo()$NoNulo, valores_nulos = c("S/D", "-")
  )
  instancia <- instanciar(no_nulo, "personas", "dato")
  resultado <- medir(
    modelo(instancia), data.frame(dato = c("A", "S/D", "-", NA))
  )

  expect_equal(resultado$resultado, c(1, 0, 0, 0))
  expect_error(especializar(metricas_nucleo()$NoNulo, otra = 1),
               "sólo acepta")
})

test_that("Formato admite un validador arbitrario y una específica se reutiliza", {
  nucleo <- metricas_nucleo()
  ocho_digitos <- especializar(
    nucleo$Formato, nombre_especifico = "OchoDigitos",
    validador = function(x) !is.na(x) & grepl("^[0-9]{8}$", x)
  )
  a <- instanciar(ocho_digitos, "personas", "documento")
  b <- instanciar(ocho_digitos, "personas", "tramite")
  medicion <- medir(
    modelo(a, b),
    data.frame(documento = c("12345678", "123"),
               tramite = c("87654321", "x"))
  )

  expect_equal(medicion$resultado, c(1, 0, 1, 0))
  expect_equal(length(unique(medicion$metrica_especifica)), 1L)

  diccionario <- instanciar(
    especializar(nucleo$Formato, diccionario = c("A", "B")),
    "personas", "categoria"
  )
  por_diccionario <- medir(
    modelo(diccionario), data.frame(categoria = c("A", "X", NA))
  )
  expect_equal(por_diccionario$resultado, c(1, 0))
  expect_error(
    especializar(
      nucleo$Formato, expresion_regular = "x", diccionario = "x"
    ),
    "exactamente una"
  )
})

test_that("Formato y dominio no miden los valores ausentes", {
  nucleo <- metricas_nucleo()
  no_nulo <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "NoNuloCodigo"),
    "personas", "codigo"
  )
  formato <- instanciar(
    especializar(
      nucleo$Formato, nombre_especifico = "FormatoCodigo",
      expresion_regular = "^[A-Z]$"
    ),
    "personas", "codigo"
  )
  dominio <- instanciar(
    especializar(
      nucleo$ValoresPosiblesPorExtension,
      nombre_especifico = "DominioCodigo", valores = "A"
    ),
    "personas", "codigo"
  )
  medicion <- medir(
    modelo(no_nulo, formato, dominio),
    data.frame(codigo = c(NA_character_, "A"))
  )

  medidas_formato <- medicion[
    medicion$metrica_especifica == "FormatoCodigo", , drop = FALSE
  ]
  medidas_dominio <- medicion[
    medicion$metrica_especifica == "DominioCodigo", , drop = FALSE
  ]
  medidas_no_nulo <- medicion[
    medicion$metrica_especifica == "NoNuloCodigo", , drop = FALSE
  ]
  expect_equal(medidas_formato$fila, 2L)
  expect_equal(medidas_dominio$fila, 2L)
  expect_equal(agregar(medidas_formato, "atributo", "ratio")$resultado, 1)
  expect_equal(agregar(medidas_dominio, "atributo", "ratio")$resultado, 1)
  expect_equal(agregar(medidas_no_nulo, "atributo", "ratio")$resultado, 0.5)

  solo_ausentes <- medir(
    modelo(formato), data.frame(codigo = c(NA_character_, NA_character_))
  )
  expect_equal(nrow(solo_ausentes), 0L)
})

test_that("las celdas se agregan por atributo o por instancia de entidad", {
  nucleo <- metricas_nucleo()
  no_nulo <- especializar(nucleo$NoNulo, nombre_especifico = "NoNuloCampos")
  a <- instanciar(no_nulo, "personas", "a")
  b <- instanciar(no_nulo, "personas", "b")
  medidas <- medir(
    modelo(a, b),
    data.frame(a = c(1, 2, 3), b = c(1, NA, NA)),
    id_medicion = "agregaciones"
  )

  atributos <- agregar(medidas, "atributo", "ratio")
  filas <- agregar(medidas, "instanciaEntidad", "ratio")

  expect_equal(atributos$resultado, c(1, 1 / 3))
  expect_equal(filas$resultado, c(1, 0.5, 0.5))
  expect_equal(atributos$granularidad, rep("atributo", 2))
  expect_equal(filas$granularidad, rep("instanciaEntidad", 3))

  entidad_desde_atributos <- agregar(atributos, "entidad", "promedio")
  entidad_desde_filas <- agregar(
    filas, "entidad", "ratio_umbral", umbral = 0.75
  )
  ponderado <- agregar(
    atributos, "entidad", "promedio_ponderado", pesos = c(0.25, 0.75)
  )
  expect_equal(entidad_desde_atributos$resultado, 2 / 3)
  expect_equal(entidad_desde_filas$resultado, 1 / 3)
  expect_equal(ponderado$resultado, 0.5)

  expect_error(agregar(atributos, "entidad", "ratio"), "booleano")
  expect_error(agregar(medidas, "atributo", "ratio_umbral", umbral = 0.5),
               "resultado real")
  expect_error(agregar(atributos, "instanciaEntidad", "promedio"),
               "No existe una transici\u00f3n")
  expect_error(
    agregar(atributos, "entidad", "promedio_ponderado", pesos = c(0.2, 0.2)),
    "sumar 1"
  )
  # Agregar a coleccion exige primero la frontera declarada: sin saber sobre
  # que tablas se agrega, la funcion elegida es una discusion posterior. La
  # restriccion a `promedio_ponderado` se prueba en test-coleccion.R, donde si
  # hay una coleccion declarada.
  expect_error(
    agregar(entidad_desde_atributos, "coleccion", "promedio"),
    "declarar la frontera"
  )
  expect_error(
    agregar(entidad_desde_atributos, "coleccion", "promedio_ponderado",
            pesos = c(0.5, 0.5)),
    "declarar la frontera"
  )
  coleccion <- transform(entidad_desde_atributos, granularidad = "coleccion")
  organizacion <- transform(coleccion, granularidad = "organizacion")
  expect_error(
    agregar(coleccion, "conjuntoColecciones", "promedio"),
    "`colecciones` debe ser"
  )
  expect_error(
    agregar(coleccion, "organizacion", "promedio"),
    "requiere una organizaci\u00f3n declarada"
  )
  expect_error(
    agregar(organizacion, "conjuntoOrganizaciones", "promedio"),
    "requiere un conjunto de organizaciones declarado"
  )
})

test_that("la evaluación recorre medidas, reglas y perfil", {
  nucleo <- metricas_nucleo()
  no_nulo <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "NoNuloEdad"),
    "personas", "edad"
  )
  modelo_calidad <- modelo(no_nulo)
  anterior <- medir(
    modelo_calidad, data.frame(edad = c(1, NA, NA, 4)),
    id_medicion = "anterior"
  )
  actual <- medir(
    modelo_calidad, data.frame(edad = c(1, 2, 3, NA)),
    id_medicion = "actual"
  )
  regla_1 <- regla_evaluacion(
    "No nulo", function(x) x == 1, metricas = no_nulo$nombre
  )
  regla_2 <- regla_evaluacion(
    "Resultado válido", function(x) x >= 0, metricas = no_nulo$nombre
  )
  perfil <- perfil_evaluacion("Operativo", regla_1, regla_2)
  evaluacion_anterior <- evaluar(anterior, perfil)
  evaluacion_actual <- evaluar(actual, perfil)

  expect_equal(nrow(evaluacion_anterior$medidas), 8L)
  expect_equal(
    evaluacion_anterior$reglas$resultado[
      evaluacion_anterior$reglas$regla == "No nulo"
    ],
    0.5
  )
  expect_equal(evaluacion_anterior$perfiles$resultado, 0.75)
  expect_equal(evaluacion_actual$perfiles$resultado, 0.875)

  comparacion <- comparar_evaluaciones(evaluacion_anterior, evaluacion_actual)
  expect_equal(comparacion$delta, 0.125)
  expect_equal(comparacion$id_medicion_anterior, "anterior")
  expect_equal(comparacion$id_medicion_actual, "actual")
})

test_that("los perfiles de madurez usan los tres umbrales del marco", {
  nucleo <- metricas_nucleo()
  instancia <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "NoNuloDato"),
    "tabla", "dato"
  )
  medidas <- medir(
    modelo(instancia), data.frame(dato = c(1, 2, 3, NA)),
    id_medicion = "madurez"
  )
  medida_atributo <- agregar(medidas, "atributo", "ratio")
  perfiles <- perfiles_madurez(medida_atributo$metrica_instanciada)
  resultados <- vapply(
    perfiles,
    function(perfil) evaluar(medida_atributo, perfil)$perfiles$resultado,
    numeric(1L)
  )

  expect_equal(names(perfiles), c("Basico", "Intermedio", "Avanzado"))
  expect_equal(resultados, c(Basico = 1, Intermedio = 1, Avanzado = 0))
  expect_error(regla_evaluacion("mala", 1), "función")
  expect_error(perfil_evaluacion("vacío"), "una o más reglas")
  expect_error(
    evaluar(
      medida_atributo,
      perfil_evaluacion(
        "inválido", regla_evaluacion("mal", function(x) NA, "inexistente")
      )
    ),
    "no coincide"
  )
})
