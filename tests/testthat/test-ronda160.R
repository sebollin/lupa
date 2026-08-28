.ronda160_convertir <- function(x, tipo) {
  if (identical(tipo, "data.frame")) return(as.data.frame(x))
  if (identical(tipo, "data.table")) return(data.table::as.data.table(x))
  tibble::as_tibble(x)
}

.ronda160_normalizar <- function(x) {
  variables_reloj <- c(
    "memoria_bytes", "entrada_convertida", "fecha_hora",
    "tiempo_benchmark", "tiempo_estimado_segundos", "tiempo_estimado_etapa",
    "tiempo_determinista", "velocidad_comparacion", "pares_benchmark"
  )
  limpiar_atributos <- function(y) {
    atributos <- attributes(y)
    if (is.null(atributos)) return(y)
    atributos$class <- NULL
    atributos$.internal.selfref <- NULL
    for (nombre in setdiff(names(atributos), c("names", "row.names"))) {
      atributos[[nombre]] <- .ronda160_normalizar(atributos[[nombre]])
    }
    attributes(y) <- atributos
    y
  }
  if (is.data.frame(x)) {
    salida <- as.data.frame(x, stringsAsFactors = FALSE)
    salida[] <- lapply(salida, .ronda160_normalizar)
    return(limpiar_atributos(salida))
  }
  if (is.list(x)) {
    nombres <- names(x)
    if (!is.null(nombres)) x <- x[setdiff(nombres, variables_reloj)]
    x <- lapply(x, .ronda160_normalizar)
    return(limpiar_atributos(x))
  }
  x
}

.ronda160_datos <- function() {
  data.frame(
    id = seq_len(12L),
    grupo = rep(c("A", "B", "C"), 4L),
    valor = c(" a", "a", "b", "b", "c", "c", "d", "d", "e", "e", "f", NA),
    fecha = as.Date("2026-01-01") + seq_len(12L),
    stringsAsFactors = FALSE
  )
}

.ronda160_modelo <- function() {
  metrica <- especializar(
    metricas_nucleo()$NoNulo, nombre_especifico = "NoNuloValor"
  )
  modelo(instanciar(metrica, "tabla_prueba", "valor"))
}

.ronda160_perfil <- function(datos) {
  perfilar(
    datos, nombre = "tabla_prueba", analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
}

.ronda160_ejecutar_api <- function(datos) {
  perfil <- .ronda160_perfil(datos)
  modelo_prueba <- .ronda160_modelo()
  metadatos <- .ronda160_convertir(
    data.frame(
      columna = "valor", escala = "nominal", stringsAsFactors = FALSE
    ), class(datos)[[1L]]
  )
  fecha_1 <- as.POSIXct("2026-01-01", tz = "UTC")
  fecha_2 <- as.POSIXct("2026-02-01", tz = "UTC")
  medidas <- medir(
    modelo_prueba, datos, id_medicion = "m1", fecha = fecha_1
  )
  medidas_entrada <- .ronda160_convertir(medidas, class(datos)[[1L]])
  class(medidas_entrada) <- unique(c("medicion", class(medidas_entrada)))
  regla <- regla_evaluacion("regla_prueba", function(x) x >= 0)
  perfil_evaluacion_prueba <- perfil_evaluacion("perfil_prueba", regla)
  evaluacion <- evaluar(medidas_entrada, perfil_evaluacion_prueba)
  medidas_2 <- medir(
    modelo_prueba, datos, id_medicion = "m2", fecha = fecha_2
  )
  evaluacion_2 <- evaluar(
    .ronda160_convertir(medidas_2, class(datos)[[1L]]),
    perfil_evaluacion_prueba
  )
  cobertura_prueba <- cobertura_analisis(perfil, medidas_entrada)
  cobertura_entrada <- .ronda160_convertir(
    cobertura_prueba, class(datos)[[1L]]
  )
  relaciones_prueba <- detectar_relaciones(
    datos, datos, columnas_candidatas = list(tabla1 = "id", tabla2 = "id")
  )
  relaciones_entrada <- .ronda160_convertir(
    relaciones_prueba, class(datos)[[1L]]
  )
  agregaciones <- .ronda160_convertir(
    data.frame(
      metrica_instanciada = medidas_entrada$metrica_instanciada[[1L]],
      agregacion = "ratio", stringsAsFactors = FALSE
    ), class(datos)[[1L]]
  )
  historico <- historico_calidad(evaluacion, evaluacion_2)
  historico_entrada <- .ronda160_convertir(historico, class(datos)[[1L]])
  ruta <- tempfile(fileext = ".rds")
  guardar_historico(historico_entrada, ruta)
  guardado <- leer_historico(ruta)
  unlink(ruta)
  plan <- planificar_limpieza(perfil, datos)
  plan_entrada <- .ronda160_convertir(plan, class(datos)[[1L]])
  class(plan_entrada) <- unique(c("plan_limpieza", class(plan_entrada)))
  salida <- list(
    perfilar = perfil,
    perfilar_por = perfilar_por(
      datos, "grupo", min_filas = 2L,
      analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE
    ),
    analizar = analizar(
      datos, fecha = fecha_1, analizar_dependencias = FALSE,
      casi_duplicados_vocabulario = FALSE, max_columnas_dependencias = 2L,
      medir_propuesta = FALSE, proteger_datos_personales = FALSE,
      metadatos_variables = metadatos
    ),
    analizar_tiempo = analizar_tiempo(datos, columnas = "fecha"),
    clasificar_variables = clasificar_variables(
      datos, metadatos = metadatos, proteger_datos_personales = FALSE
    ),
    distribucion_valores = distribucion_valores(
      datos, proteger_datos_personales = FALSE
    ),
    detectar_asociaciones = detectar_asociaciones(
      datos, umbral = 0, muestra = 12L, max_columnas = 4L
    ),
    detectar_claves = detectar_claves(datos, max_combinacion = 1L),
    detectar_dependencias = detectar_dependencias(
      datos, min_observaciones = 2L, max_columnas = 4L
    ),
    detectar_discordancias = detectar_discordancias(
      datos, senal_redundante(c("id", "grupo"))
    ),
    detectar_relaciones = detectar_relaciones(
      datos, datos,
      columnas_candidatas = list(tabla1 = "id", tabla2 = "id")
    ),
    sugerir_clave = sugerir_clave(datos),
    elegir_clave = suppressMessages(elegir_clave(datos)),
    referencial = referencial(datos, "id"),
    proponer_modelo = proponer_modelo(perfil, datos),
    proponer_modelo_relaciones = proponer_modelo(
      perfil, datos, relaciones = relaciones_entrada,
      entidades_relacion = c("tabla_prueba", "tabla_ref")
    ),
    planificar_limpieza = plan,
    aplicar = aplicar(plan_entrada, datos),
    guiar_limpieza = suppressMessages(guiar_limpieza(
      plan_entrada, datos, selector = function(decision) "ninguna"
    )),
    medir = medidas_entrada,
    cobertura_analisis = cobertura_prueba,
    evaluar = evaluacion,
    acumular_historico = acumular_historico(historico_entrada),
    guardar_y_leer_historico = guardado,
    detectar_deriva_calidad = detectar_deriva_calidad(historico_entrada),
    medicion_desde_estimaciones = medicion_desde_estimaciones(
      .ronda160_convertir(
        data.frame(
          celda = c("A", "B"), promedio = c(0.4, 0.5),
          cv = c(0.1, 0.2), n = c(10L, 20L), stringsAsFactors = FALSE
        ), class(datos)[[1L]]
      ), "entidad_prueba", "fuente_prueba", atributo = "celda",
      fecha = fecha_1
    ),
    agregar = agregar(medidas_entrada, "instanciaEntidad", "ratio"),
    tablero_calidad = tablero_calidad(
      medidas_entrada, agregaciones = agregaciones,
      cobertura = cobertura_entrada
    ),
    indice_calidad = indice_calidad(
      medidas_entrada, pesos = c(Completitud = 1)
    )
  )
  if (requireNamespace("stringdist", quietly = TRUE)) {
    salida$detectar_duplicados_aproximados <- detectar_duplicados_aproximados(
      datos, columnas = "valor", max_pares = 100L, max_resultados = 3L,
      nucleos = 1L
    )
    salida$estimar_costo <- estimar_costo(
      datos, columnas = "valor", max_pares = 100L, nucleos = 1L
    )
  }
  salida
}

test_that("el contador de duplicados fija los bordes de la semantica de base", {
  casos <- list(
    cero_filas = data.frame(a = numeric(), b = character()),
    una_fila = data.frame(a = 1, b = "x"),
    lista = data.frame(
      id = 1:3,
      valores = I(list(c(1, 2), c(1, 2), c(2, 3)))
    ),
    matriz = data.frame(
      id = c("a", "a", "b"),
      valores = I(rbind(c(1, 2), c(1, 2), c(3, 4)))
    ),
    tipos = data.frame(
      factor = factor(c("a", "a", "b", "b")),
      fecha = as.Date("2026-01-01") + c(0, 0, 1, 1),
      momento = as.POSIXct("2026-01-01", tz = "UTC") + c(0, 0, 1, 1),
      infinito = c(Inf, Inf, -Inf, -Inf)
    )
  )
  for (nombre in names(casos)) {
    datos <- casos[[nombre]]
    esperado <- c(
      filas_duplicadas = sum(base::duplicated.data.frame(datos)),
      filas_en_grupos_duplicados = sum(
        base::duplicated.data.frame(datos) |
          base::duplicated.data.frame(datos, fromLast = TRUE)
      )
    )
    tipos <- if (identical(nombre, "matriz")) {
      c("data.frame", "tibble")
    } else {
      c("data.frame", "data.table", "tibble")
    }
    for (tipo in tipos) {
      tabla <- .ronda160_convertir(datos, tipo)
      resultado <- lupa:::.conteos_filas_duplicadas(tabla)
      expect_identical(
        unname(unlist(resultado)), unname(esperado),
        info = paste(nombre, tipo)
      )
    }
  }
  con_nan <- data.frame(
    valor = rep(c(NA_real_, NaN, NA_real_, NaN, 1), 6L)
  )
  esperado_nan <- c(
    filas_duplicadas = sum(base::duplicated.data.frame(con_nan)),
    filas_en_grupos_duplicados = sum(
      base::duplicated.data.frame(con_nan) |
        base::duplicated.data.frame(con_nan, fromLast = TRUE)
    )
  )
  for (tipo in c("data.frame", "data.table", "tibble")) {
    tabla <- .ronda160_convertir(con_nan, tipo)
    expect_true(lupa:::.tiene_nan_en_dobles(tabla))
    expect_identical(
      unname(unlist(lupa:::.conteos_filas_duplicadas(tabla))),
      unname(esperado_nan), info = paste("NaN", tipo)
    )
  }
})

test_that("las selecciones de columnas quedan auditadas", {
  # Necesita `R/`, que bajo `R CMD check` no existe: alli se corre contra el
  # paquete instalado. Lo que fija el comportamiento es la prueba de conducta,
  # no este recuento; esto documenta el tamano de la superficie.
  raiz <- testthat::test_path("..", "..", "R")
  skip_if_not(
    dir.exists(raiz),
    "Sin `R/` a la vista: bajo R CMD check lo comprueba el check nativo."
  )
  archivos <- list.files(raiz, pattern = "[.]R$", full.names = TRUE)
  lineas <- unlist(lapply(archivos, readLines, warn = FALSE), use.names = FALSE)
  sitios <- lineas[
    grepl("\\[,.*drop[[:space:]]*=[[:space:]]*FALSE", lineas) &
      !grepl("^[[:space:]]*#", lineas)
  ]
  expect_gte(length(sitios), 18L)
})

test_that("NAMESPACE no importa data.table porque cedta cambia la sintaxis de [", {
  # `data.table` decide la semantica de `[` segun si el paquete que llama lo
  # tiene entre los imports de su espacio de nombres. Con el import puesto,
  # `tabla[, columnas, drop = FALSE]` deja de seleccionar columnas en toda
  # funcion que recibe una tabla de quien usa el paquete. `lupa` no importa
  # nada: llama a `cli` y a `data.table` con `::`.
  ruta <- system.file("NAMESPACE", package = "lupa")
  if (!nzchar(ruta)) ruta <- testthat::test_path("..", "..", "NAMESPACE")
  skip_if_not(file.exists(ruta), "no se encontro el NAMESPACE")
  namespace <- readLines(ruta)

  # Control positivo: si esto falla, se leyo un archivo que no es el NAMESPACE
  # y la comprobacion de abajo estaria pasando en vacio.
  expect_gt(sum(grepl("^(export|S3method)", namespace)), 50L)

  importaciones <- namespace[grepl("^[[:space:]]*import", namespace)]
  expect_false(any(grepl("data[.]table", importaciones)))
  expect_length(importaciones, 0L)
})

test_that("dentro del paquete, [ , columnas, drop = FALSE] selecciona columnas", {
  # La prueba de conducta que corresponde a la estructural de arriba: lo que
  # importa no es el texto del NAMESPACE sino que la sintaxis siga significando
  # lo mismo cuando la tabla es un data.table.
  skip_if_not_installed("data.table")
  seleccionar <- function(tabla) tabla[, c("a"), drop = FALSE]
  environment(seleccionar) <- asNamespace("lupa")

  resultado <- seleccionar(data.table::data.table(a = 1:3, b = 4:6))

  expect_s3_class(resultado, "data.frame")
  expect_identical(ncol(resultado), 1L)
  expect_identical(names(resultado), "a")
})

test_that("toda la API tabular coincide en data.frame, data.table y tibble", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("tibble")
  datos <- .ronda160_datos()
  tablas <- list(
    data.frame = .ronda160_convertir(datos, "data.frame"),
    data.table = .ronda160_convertir(datos, "data.table"),
    tibble = .ronda160_convertir(datos, "tibble")
  )
  resultados <- lapply(tablas, .ronda160_ejecutar_api)
  esperado <- .ronda160_normalizar(resultados[[1L]])
  for (nombre in names(resultados)[-1L]) {
    expect_equal(
      .ronda160_normalizar(resultados[[nombre]]), esperado, info = nombre
    )
  }
})

test_that("las metricas que seleccionan atributos son iguales en tres clases", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("tibble")
  datos <- .ronda160_datos()
  nucleo <- metricas_nucleo()
  intra <- instanciar(
    especializar(
      nucleo$ReglaIntegridadIntraEntidad,
      nombre_especifico = "ReglaValor", regla = function(x) !is.na(x$valor)
    ),
    "tabla_prueba", "valor"
  )
  conjunto <- instanciar(
    especializar(nucleo$ConjuntoAtributosDuplicado),
    "tabla_prueba", c("grupo", "valor")
  )
  densidad <- instanciar(
    especializar(
      nucleo$DensidadPonderada,
      nombre_especifico = "DensidadGrupoValor", coeficientes = c(0.5, 0.5)
    ),
    "tabla_prueba", c("id", "grupo")
  )
  modelo_prueba <- modelo(intra, conjunto, densidad)
  resultados <- lapply(c("data.frame", "data.table", "tibble"), function(tipo) {
    medir(
      modelo_prueba, .ronda160_convertir(datos, tipo), id_medicion = "m1",
      fecha = as.POSIXct("2026-01-01", tz = "UTC")
    )
  })
  esperado <- .ronda160_normalizar(resultados[[1L]])
  for (resultado in resultados[-1L]) {
    expect_equal(.ronda160_normalizar(resultado), esperado)
  }
})

test_that("la salida tabular de una metrica tambien queda aislada de cedta", {
  skip_if_not_installed("data.table")
  fabrica <- metrica(
    "MetricaTablaPrueba", "Metrica de prueba", "instanciaAtributo", "booleano",
    dimension = "Consistencia", factor = "Regla de prueba",
    metodo = function(tablas, instancia) {
      tabla <- tablas[[instancia$entidad[[1L]]]]
      data.table::data.table(
        resultado = rep(TRUE, nrow(tabla)),
        entidad = instancia$entidad[[1L]],
        atributo = instancia$atributos[[1L]], fila = seq_len(nrow(tabla)),
        objeto = paste0(instancia$entidad[[1L]], "$", instancia$atributos[[1L]])
      )
    }
  )
  instancia <- instanciar(especializar(fabrica), "tabla_prueba", "valor")
  resultado <- medir(
    modelo(instancia), data.table::data.table(valor = c("a", "b")),
    id_medicion = "m1", fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
  expect_equal(resultado$resultado, c(1, 1))
  expect_s3_class(resultado, "medicion")
})

test_that("la API de coleccion coincide cuando su tabla declarada cambia de clase", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "tabla_a", data.frame(id = 1:3))
  DBI::dbWriteTable(con, "tabla_b", data.frame(id = 1:3))
  base <- data.frame(
    esquema = NA_character_, tabla = c("tabla_a", "tabla_b"),
    stringsAsFactors = FALSE
  )
  pares_base <- data.frame(
    tabla_1 = "tabla_a", tabla_2 = "tabla_b", stringsAsFactors = FALSE
  )
  salidas <- lapply(c("data.frame", "data.table", "tibble"), function(tipo) {
    tablas <- .ronda160_convertir(base, tipo)
    pares <- .ronda160_convertir(pares_base, tipo)
    declarada <- coleccion(con, tablas, nombre = "coleccion_prueba")
    list(
      coleccion = as.data.frame(declarada$tablas),
      costo = estimar_costo_coleccion(declarada, pares),
      relaciones = relaciones_coleccion(
        declarada, pares, muestra = 3L, tope_cache_mb = 10, tope_memoria_mb = 10
      )
    )
  })
  esperado <- .ronda160_normalizar(salidas[[1L]])
  for (salida in salidas[-1L]) {
    expect_equal(.ronda160_normalizar(salida), esperado)
  }
})
