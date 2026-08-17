.version_esquema_analisis <- 1L

.advertencias_analisis <- function(distribuciones, asociaciones, temporal,
                                   variables, propuesta, perfil = NULL) {
  filas <- list()
  agregar <- function(componente, tipo, descripcion, severidad = "sospechoso") {
    filas[[length(filas) + 1L]] <<- data.frame(
      componente = componente, tipo = tipo, severidad = severidad,
      descripcion = descripcion, stringsAsFactors = FALSE
    )
  }
  if (any(distribuciones$alcance$muestreado)) agregar(
    "distribuciones", "muestreo",
    "Al menos una distribucion o sus cuantiles se estimaron sobre una muestra."
  )
  if (any(distribuciones$alcance$truncado)) agregar(
    "distribuciones", "truncamiento",
    "Las tablas de frecuencia muestran solo los valores mas frecuentes."
  )
  if (isTRUE(attr(asociaciones, "muestreado", exact = TRUE))) agregar(
    "asociaciones", "muestreo",
    "Las asociaciones se calcularon sobre una muestra comun de filas."
  )
  if (length(attr(asociaciones, "columnas_omitidas_limite", exact = TRUE))) agregar(
    "asociaciones", "columnas_omitidas",
    "Algunas columnas analizables excedieron el limite configurado."
  )
  if (isTRUE(attr(asociaciones, "truncado", exact = TRUE))) agregar(
    "asociaciones", "truncamiento",
    "La salida de asociaciones se recorto despues de ordenarla."
  )
  if (nrow(temporal$propuestas) && any(!temporal$propuestas$confirmada)) agregar(
    "tiempo", "frecuencia_no_confirmada",
    "Las frecuencias temporales son propuestas observadas, no requisitos confirmados."
  )
  if (any(temporal$resumen$huecos_truncados)) agregar(
    "tiempo", "huecos_truncados",
    "Al menos una serie tiene mas grupos de huecos que los mostrados."
  )
  if (isTRUE(attr(temporal, "truncado", exact = TRUE))) agregar(
    "tiempo", "columnas_omitidas",
    "El analisis temporal alcanzo el maximo de columnas configurado."
  )
  if (nrow(variables) && any(!variables$confirmada)) agregar(
    "variables", "escalas_no_confirmadas",
    "Algunas escalas se propusieron desde los valores y requieren confirmacion."
  )
  if (nrow(variables) && any(variables$n_niveles_ausentes > 0L)) agregar(
    "variables", "niveles_ausentes",
    "Hay niveles declarados que no aparecen en la entrega."
  )
  if (isTRUE(attr(propuesta, "truncado", exact = TRUE))) agregar(
    "modelo", "propuesta_truncada",
    "La propuesta de modelo contiene menos filas que el total detectado."
  )
  if (!is.null(perfil) && inherits(perfil, "perfil") &&
      nrow(perfil$hallazgos)) {
    casi_claves <- perfil$hallazgos[
      perfil$hallazgos$tipo_hallazgo == "casi_clave", , drop = FALSE
    ]
    if (nrow(casi_claves)) {
      for (i in seq_len(nrow(casi_claves))) agregar(
        "perfil", "casi_clave",
        paste0(
          "La columna '", casi_claves$columna[[i]],
          "' tiene colisiones concentradas y no es una clave valida. ",
          casi_claves$evidencia[[i]]
        )
      )
    }
  }
  resultado <- if (length(filas)) do.call(rbind, filas) else data.frame(
    componente = character(), tipo = character(), severidad = character(),
    descripcion = character(), stringsAsFactors = FALSE
  )
  resultado$severidad <- factor(
    resultado$severidad, levels = c("ok", "sospechoso", "error"), ordered = TRUE
  )
  rownames(resultado) <- NULL
  resultado
}

.decision_medicion_propuesta <- function(propuesta, seleccion, confirmada,
                                         medicion_automatica) {
  if (!nrow(propuesta)) {
    return(data.frame(
      metrica = character(), objeto = character(), estado = character(),
      medida = logical(), motivo = character(), justificacion = character(),
      confirmada = logical(), stringsAsFactors = FALSE
    ))
  }
  medida <- seq_len(nrow(propuesta)) %in% seleccion
  motivo <- ifelse(
    medida,
    if (medicion_automatica) {
      "lupa la eligi\u00f3 porque la propuesta qued\u00f3 en estado 'lista'."
    } else {
      "La propuesta confirmada incluy\u00f3 esta m\u00e9trica."
    },
    if (!medicion_automatica && !confirmada) {
      "La medici\u00f3n autom\u00e1tica fue desactivada."
    } else ifelse(
      propuesta$estado != "lista",
      paste0("Qued\u00f3 afuera por su estado: ", propuesta$estado, "."),
      "Qued\u00f3 afuera de la selecci\u00f3n confirmada."
    )
  )
  data.frame(
    metrica = propuesta$metrica,
    objeto = ifelse(
      nzchar(propuesta$atributos), propuesta$atributos, "(tabla)"
    ),
    estado = as.character(propuesta$estado),
    medida = medida,
    motivo = motivo,
    justificacion = propuesta$justificacion,
    confirmada = rep(confirmada, nrow(propuesta)),
    stringsAsFactors = FALSE
  )
}

.decision_medicion_modelo <- function(modelo_elegido) {
  if (is.null(modelo_elegido)) return(NULL)
  filas <- lapply(modelo_elegido$metricas, function(x) {
    data.frame(
      metrica = x$declaracion$nombre,
      objeto = if (length(x$atributos)) {
        paste(x$atributos, collapse = " + ")
      } else {
        "(tabla)"
      },
      estado = "confirmada", medida = TRUE,
      motivo = "La m\u00e9trica pertenece al modelo confirmado.",
      justificacion = "Modelo declarado por quien realiza el an\u00e1lisis.",
      confirmada = TRUE, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}

.marco_modelo_analisis <- function(modelo_elegido) {
  if (is.null(modelo_elegido)) return(marco_agesic())
  if (inherits(modelo_elegido$marco, "marco_calidad")) {
    return(modelo_elegido$marco)
  }
  declaraciones <- lapply(modelo_elegido$metricas, `[[`, "declaracion")
  factores <- unique(data.frame(
    dimension = vapply(declaraciones, `[[`, character(1L), "dimension"),
    factor = vapply(declaraciones, `[[`, character(1L), "factor"),
    stringsAsFactors = FALSE
  ))
  if (anyNA(factores) || any(!nzchar(factores$dimension)) ||
      any(!nzchar(factores$factor))) {
    return(marco_agesic())
  }
  agesic <- marco_agesic()
  claves <- paste(factores$dimension, factores$factor, sep = "\r")
  claves_agesic <- paste(
    agesic$factores$dimension, agesic$factores$factor, sep = "\r"
  )
  if (all(claves %in% claves_agesic)) agesic else {
    marco_calidad("Marco del modelo confirmado", factores)
  }
}

#' Ejecutar el análisis descriptivo completo
#'
#' Es la puerta de entrada al recorrido de `lupa`. Construye el perfil, las
#' distribuciones, asociaciones, diagnóstico temporal, clasificación confirmable
#' de variables, propuesta de modelo, medición agregada, tablero, cobertura
#' conceptual y plan de limpieza. No modifica los datos.
#'
#' Por omisión mide todas las sugerencias de la propuesta cuyo estado es
#' `"lista"`, aunque no estuvieran activas, y declara que esa selección fue
#' realizada por `lupa` sin confirmación. Agrega inmediatamente el detalle y
#' conserva [tablero_calidad()]; las medidas fila a fila sólo quedan en el
#' resultado si `conservar_detalle_medicion = TRUE`. La evaluación, cuando se
#' solicita, usa la medición agregada.
#' Los hallazgos `casi_clave` del perfil se reiteran en `advertencias`, con su
#' columna, valores en colisión, frecuencias y criterio observado, para que no
#' queden ocultos dentro del recorrido integral.
#'
#' @param datos Tabla que se desea analizar.
#' @param nombre Nombre de la entrega.
#' @param fecha Fecha y hora reproducible de la corrida.
#' @param argumentos_perfil Lista de argumentos adicionales para [perfilar()].
#' @param metadatos_variables Declaraciones para [clasificar_variables()].
#' @param modelo_confirmado Modelo creado por [modelo()] o `NULL`.
#' @param propuesta_confirmada Propuesta editada por el usuario o `NULL`.
#' @param marco Taxonomía opcional creada por [marco_calidad()]. Si se omite,
#'   usa la asociada a `modelo_confirmado` y, en último término,
#'   [marco_agesic()].
#' @param perfil_evaluacion Perfil explícito para [evaluar()] o `NULL`.
#' @param id_medicion Identificador opcional enviado a [medir()].
#' @param medir_propuesta Si se mide automáticamente la propuesta en estado
#'   `"lista"` cuando no se recibe un modelo o una propuesta confirmada. Use
#'   `FALSE` para conservar el comportamiento descriptivo anterior.
#' @param conservar_detalle_medicion Si se retienen las medidas fila a fila.
#'   Es `FALSE` por omisión: el tablero y la medición agregada permanecen.
#' @param muestra Límite de filas para perfil, distribuciones y enumeración de
#'   niveles observados.
#' @param muestra_asociacion Límite común de filas para asociaciones.
#' @param max_valores Máximo de valores por tabla de frecuencias.
#' @param probabilidades Cuantiles solicitados.
#' @param umbral_asociacion Asociación mínima informada.
#' @param max_columnas_asociacion Máximo de columnas para asociaciones.
#' @param max_niveles_asociacion Máximo de niveles categóricos.
#' @param max_pares_asociacion Máximo de pares devueltos.
#' @param calendario Días ISO usados por el análisis temporal.
#' @param frecuencia_dias Frecuencia temporal conocida o `NULL` para proponerla.
#' @param max_huecos Máximo de grupos de huecos por columna.
#' @param max_columnas_temporales Máximo de columnas temporales analizadas.
#' @param conservar_datos Si el objeto retiene una copia de la entrada. Es
#'   `FALSE` por omisión para limitar tamaño y exposición. Con protección activa,
#'   las columnas personales de esa copia también se enmascaran; para conservar
#'   sus valores debe usarse `proteger_datos_personales = FALSE`.
#' @param proteger_datos_personales Si perfiles y resúmenes ocultan valores de
#'   columnas cuya clasificación activa protección automática, incluidos
#'   estadísticos de orden, cuantiles y rangos temporales.
#' @param ... Argumentos con nombre enviados a [perfilar()]. Es una alternativa
#'   concisa a `argumentos_perfil`.
#'
#' @return Objeto S3 `analisis` con todos los componentes y su cobertura.
#' @export
#' @seealso [guardar_analisis()], [reportar()], [cobertura_analisis()]
#'
#' @examples
#' resultado <- analizar(
#'   datos_administrativos, analizar_dependencias = FALSE,
#'   fecha = as.POSIXct("2026-01-15", tz = "UTC")
#' )
#' resultado
analizar <- function(datos, nombre = deparse(substitute(datos)), fecha = Sys.time(),
                     argumentos_perfil = list(), metadatos_variables = NULL,
                     modelo_confirmado = NULL, propuesta_confirmada = NULL,
                     marco = NULL,
                     perfil_evaluacion = NULL, id_medicion = NULL,
                     medir_propuesta = TRUE,
                     conservar_detalle_medicion = FALSE,
                     muestra = 1e5, muestra_asociacion = 1e4,
                     max_valores = 20L,
                     probabilidades = c(0, 0.25, 0.5, 0.75, 1),
                     umbral_asociacion = 0.3,
                     max_columnas_asociacion = 50L,
                     max_niveles_asociacion = 50L,
                     max_pares_asociacion = 500L,
                     calendario = 1:7, frecuencia_dias = NULL,
                     max_huecos = 20L, max_columnas_temporales = 50L,
                     conservar_datos = FALSE,
                     proteger_datos_personales = TRUE, ...) {
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe heredar de data.frame.", call. = FALSE)
  }
  extras <- list(...)
  if (!is.list(argumentos_perfil) || is.null(names(argumentos_perfil)) &&
      length(argumentos_perfil)) {
    stop("`argumentos_perfil` debe ser una lista con nombres.", call. = FALSE)
  }
  if (length(argumentos_perfil) && any(!nzchar(names(argumentos_perfil))) ||
      length(extras) && (is.null(names(extras)) || any(!nzchar(names(extras))))) {
    stop("Todos los argumentos de perfilar() deben tener nombre.", call. = FALSE)
  }
  argumentos_perfil <- c(argumentos_perfil, extras)
  if (anyDuplicated(names(argumentos_perfil))) {
    stop("Los argumentos de perfilar() no pueden repetirse.", call. = FALSE)
  }
  reservados <- c("datos", "nombre", "fecha", "muestra",
                  "proteger_datos_personales")
  if (any(names(argumentos_perfil) %in% reservados)) {
    stop("`argumentos_perfil` no puede reemplazar argumentos coordinados por analizar().",
         call. = FALSE)
  }
  logicos <- c(
    conservar_datos, proteger_datos_personales, medir_propuesta,
    conservar_detalle_medicion
  )
  if (!is.logical(conservar_datos) || length(conservar_datos) != 1L ||
      !is.logical(proteger_datos_personales) ||
      length(proteger_datos_personales) != 1L ||
      !is.logical(medir_propuesta) || length(medir_propuesta) != 1L ||
      !is.logical(conservar_detalle_medicion) ||
      length(conservar_detalle_medicion) != 1L || anyNA(logicos)) {
    stop("Los controles de medicion, conservacion y proteccion deben ser logicos.",
         call. = FALSE)
  }
  if (!is.null(modelo_confirmado) && !is.null(propuesta_confirmada)) {
    stop("Use `modelo_confirmado` o `propuesta_confirmada`, no ambos.",
         call. = FALSE)
  }
  if (!is.null(modelo_confirmado) && !inherits(modelo_confirmado, "modelo_calidad")) {
    stop("`modelo_confirmado` debe provenir de modelo().", call. = FALSE)
  }
  if (!is.null(propuesta_confirmada) &&
      !inherits(propuesta_confirmada, "propuesta_modelo")) {
    stop("`propuesta_confirmada` debe provenir de proponer_modelo().",
         call. = FALSE)
  }
  if (!is.null(marco) && !inherits(marco, "marco_calidad")) {
    stop("`marco` debe provenir de marco_calidad().", call. = FALSE)
  }
  argumentos <- c(list(
    datos = datos, nombre = nombre, fecha = fecha, muestra = muestra,
    proteger_datos_personales = proteger_datos_personales
  ), argumentos_perfil)
  perfil <- do.call(perfilar, argumentos)
  distribuciones <- distribucion_valores(
    datos, perfil, max_valores, probabilidades, muestra,
    proteger_datos_personales
  )
  variables <- clasificar_variables(
    datos, perfil, metadatos_variables,
    muestra = muestra,
    proteger_datos_personales = proteger_datos_personales
  )
  asociaciones <- detectar_asociaciones(
    datos, perfil$dependencias, umbral_asociacion, muestra_asociacion,
    max_columnas_asociacion, max_niveles_asociacion, max_pares_asociacion
  )
  temporal <- analizar_tiempo(
    datos, perfil, calendario = calendario, frecuencia_dias = frecuencia_dias,
    max_huecos = max_huecos, max_columnas = max_columnas_temporales
  )
  propuesta <- proponer_modelo(perfil, datos)
  plan <- planificar_limpieza(perfil, datos)
  seleccion_automatica <- if (is.null(modelo_confirmado) &&
                              is.null(propuesta_confirmada) &&
                              medir_propuesta) {
    which(as.character(propuesta$estado) == "lista")
  } else integer()
  propuesta_medida <- if (length(seleccion_automatica)) {
    propuesta_automatica <- propuesta
    propuesta_automatica$incluir[] <- FALSE
    propuesta_automatica$incluir[seleccion_automatica] <- TRUE
    propuesta_automatica
  } else NULL
  modelo_elegido <- if (!is.null(propuesta_confirmada)) {
    modelo_desde_propuesta(propuesta_confirmada)
  } else if (!is.null(modelo_confirmado)) {
    modelo_confirmado
  } else if (!is.null(propuesta_medida)) {
    modelo_desde_propuesta(propuesta_medida)
  } else NULL
  marco_elegido <- if (!is.null(marco)) {
    marco
  } else {
    .marco_modelo_analisis(modelo_elegido)
  }
  if (!is.null(modelo_elegido)) {
    dimensiones_modelo <- vapply(modelo_elegido$metricas, function(x) {
      x$declaracion$dimension
    }, character(1L))
    factores_modelo <- vapply(modelo_elegido$metricas, function(x) {
      x$declaracion$factor
    }, character(1L))
    claves_modelo <- vapply(modelo_elegido$metricas, function(x) {
      paste(x$declaracion$dimension, x$declaracion$factor, sep = "\r")
    }, character(1L))
    claves_marco <- paste(
      marco_elegido$factores$dimension, marco_elegido$factores$factor,
      sep = "\r"
    )
    if (!anyNA(dimensiones_modelo) && !anyNA(factores_modelo) &&
        all(nzchar(dimensiones_modelo)) && all(nzchar(factores_modelo)) &&
        all(claves_modelo %in% claves_marco)) {
      modelo_elegido <- modelo(modelo_elegido$metricas, marco = marco_elegido)
    }
  }
  detalle_medicion <- if (!is.null(modelo_elegido)) {
    medir(modelo_elegido, datos, id_medicion = id_medicion, fecha = fecha)
  } else NULL
  if (!is.null(perfil_evaluacion) && is.null(detalle_medicion)) {
    stop("`perfil_evaluacion` requiere una medici\u00f3n activa.",
         call. = FALSE)
  }
  cobertura <- cobertura_analisis(
    perfil, detalle_medicion, modelo = marco_elegido
  )
  preparado <- if (!is.null(detalle_medicion)) {
    .preparar_tablero(
      detalle_medicion, marco = marco_elegido, cobertura = cobertura
    )
  } else {
    list(
      tablero = .tablero_vacio(cobertura = cobertura, marco = marco_elegido),
      medicion = NULL
    )
  }
  medicion <- preparado$medicion
  tablero <- preparado$tablero
  evaluacion <- if (!is.null(perfil_evaluacion)) {
    evaluar(medicion, perfil_evaluacion)
  } else NULL
  seleccion_confirmada <- if (!is.null(propuesta_confirmada)) {
    which(propuesta_confirmada$incluir &
            as.character(propuesta_confirmada$estado) == "lista")
  } else integer()
  decision_medicion <- if (!is.null(modelo_confirmado)) {
    .decision_medicion_modelo(modelo_elegido)
  } else if (!is.null(propuesta_confirmada)) {
    .decision_medicion_propuesta(
      propuesta_confirmada, seleccion_confirmada,
      confirmada = TRUE, medicion_automatica = FALSE
    )
  } else {
    .decision_medicion_propuesta(
      propuesta, seleccion_automatica,
      confirmada = FALSE, medicion_automatica = medir_propuesta
    )
  }
  advertencias <- .advertencias_analisis(
    distribuciones, asociaciones, temporal, variables, propuesta, perfil
  )
  fecha_utc <- .fecha_utc(fecha)
  estructura <- list(
    perfil = perfil, distribuciones = distribuciones,
    asociaciones = asociaciones, temporal = temporal, variables = variables,
    propuesta_modelo = propuesta, decision_medicion = decision_medicion,
    tablero = tablero, medicion = medicion,
    detalle_medicion = if (conservar_detalle_medicion) detalle_medicion else NULL,
    evaluacion = evaluacion,
    plan_limpieza = plan, cobertura = cobertura, advertencias = advertencias,
    datos = if (conservar_datos) as.data.frame(datos) else NULL,
    meta = list(
      nombre = nombre, fecha = fecha_utc,
      version_esquema = .version_esquema_analisis,
      version_paquete = .version_paquete(), datos_conservados = conservar_datos,
      modelo_medido = !is.null(medicion), evaluado = !is.null(evaluacion),
      propuesta_medida_automaticamente = length(seleccion_automatica) > 0L,
      propuesta_confirmada = !is.null(propuesta_confirmada) ||
        !is.null(modelo_confirmado),
      detalle_medicion_conservado = conservar_detalle_medicion &&
        !is.null(detalle_medicion),
      marco_calidad = marco_elegido$nombre
    )
  )
  class(estructura) <- "analisis"
  if (proteger_datos_personales) .proteger_analisis(estructura) else estructura
}

#' @export
print.analisis <- function(x, ...) {
  cli::cli_h1(paste("Analisis de datos:", x$meta$nombre))
  cli::cli_dl(c(
    "Filas" = x$perfil$general$filas,
    "Columnas" = x$perfil$general$columnas,
    "Hallazgos del perfil" = nrow(x$perfil$hallazgos),
    "Advertencias de alcance" = nrow(x$advertencias),
    "Asociaciones informadas" = nrow(x$asociaciones),
    "Series temporales" = nrow(x$temporal$resumen),
    "Modelo medido" = if (isTRUE(x$meta$modelo_medido)) "si" else "no",
    "Detalle fila a fila" = if (isTRUE(x$meta$detalle_medicion_conservado)) {
      "conservado"
    } else {
      "no conservado"
    }
  ))
  cli::cli_h2("Tablero de calidad")
  print(x$tablero)
  cli::cli_h2("Cobertura conceptual")
  conteos <- table(x$cobertura$estado)
  vista <- data.frame(
    estado = names(conteos), factores = as.integer(conteos),
    stringsAsFactors = FALSE
  )
  print(vista, row.names = FALSE)
  if (nrow(x$advertencias)) {
    cli::cli_h2("Advertencias de alcance")
    print(x$advertencias[c("componente", "tipo", "descripcion")],
          row.names = FALSE)
  }
  invisible(x)
}

.descriptor_funcion <- function(propuesta, i, nombre) {
  atributos <- propuesta$atributos_ligados[[i]]
  if (identical(nombre, "regla") &&
      startsWith(propuesta$origen[[i]], "dependencia_funcional:") &&
      length(atributos) == 2L) {
    return(list(
      tipo = "dependencia_funcional", determinante = atributos[[1L]],
      dependiente = atributos[[2L]], incluir_original = propuesta$incluir[[i]],
      estado_original = propuesta$estado[[i]]
    ))
  }
  list(
    tipo = "funcion_no_reconstruible", propiedad = nombre,
    incluir_original = propuesta$incluir[[i]],
    estado_original = propuesta$estado[[i]]
  )
}

.deshidratar_propuesta <- function(propuesta) {
  if (!inherits(propuesta, "propuesta_modelo")) {
    return(list(propuesta = propuesta, n_funciones = 0L))
  }
  n_funciones <- 0L
  for (i in seq_len(nrow(propuesta))) {
    configuracion <- propuesta$configuracion[[i]]
    funciones <- names(configuracion)[vapply(configuracion, is.function, logical(1L))]
    if (!length(funciones)) next
    for (nombre in funciones) {
      configuracion[[nombre]] <- structure(
        .descriptor_funcion(propuesta, i, nombre),
        class = "descriptor_funcion_analisis"
      )
      n_funciones <- n_funciones + 1L
    }
    propuesta$configuracion[[i]] <- configuracion
    propuesta$incluir[[i]] <- FALSE
    propuesta$estado[[i]] <- "requiere_reconstruccion"
  }
  list(propuesta = propuesta, n_funciones = n_funciones)
}

.hidratar_propuesta <- function(propuesta, datos) {
  if (!inherits(propuesta, "propuesta_modelo")) return(propuesta)
  for (i in seq_len(nrow(propuesta))) {
    configuracion <- propuesta$configuracion[[i]]
    descriptores <- names(configuracion)[vapply(
      configuracion, inherits, logical(1L), "descriptor_funcion_analisis"
    )]
    if (!length(descriptores)) next
    reconstruida <- TRUE
    requiere_funcion <- FALSE
    for (nombre in descriptores) {
      descriptor <- configuracion[[nombre]]
      if (identical(descriptor$tipo, "dependencia_funcional") &&
          inherits(datos, "data.frame") &&
          all(c(descriptor$determinante, descriptor$dependiente) %in% names(datos))) {
        configuracion[[nombre]] <- .regla_desde_dependencia(
          datos, descriptor$determinante, descriptor$dependiente
        )
      } else {
        reconstruida <- FALSE
        requiere_funcion <- requiere_funcion ||
          identical(descriptor$tipo, "funcion_no_reconstruible")
      }
    }
    propuesta$configuracion[[i]] <- configuracion
    if (reconstruida) {
      propuesta$incluir[[i]] <- isTRUE(descriptor$incluir_original)
      propuesta$estado[[i]] <- descriptor$estado_original
    } else {
      propuesta$incluir[[i]] <- FALSE
      propuesta$estado[[i]] <- if (requiere_funcion) {
        "requiere_funcion"
      } else "requiere_datos"
    }
  }
  propuesta
}

.proteger_propuesta_analisis <- function(propuesta, sensibles) {
  if (!inherits(propuesta, "propuesta_modelo") || !length(sensibles)) return(propuesta)
  for (i in seq_len(nrow(propuesta))) {
    if (!any(propuesta$atributos_ligados[[i]] %in% sensibles)) next
    configuracion <- propuesta$configuracion[[i]]
    campos <- intersect(names(configuracion), c("valores", "diccionario"))
    for (campo in campos) configuracion[[campo]] <- "[valor protegido]"
    if (length(campos)) {
      propuesta$configuracion[[i]] <- configuracion
      propuesta$incluir[[i]] <- FALSE
      propuesta$estado[[i]] <- "requiere_valores_protegidos"
    }
  }
  propuesta
}

.proteger_datos_conservados <- function(datos, sensibles) {
  if (!inherits(datos, "data.frame") || !length(sensibles)) return(datos)
  reemplazo <- "[valor protegido]"
  for (nombre in intersect(names(datos), sensibles)) {
    x <- datos[[nombre]]
    if (is.character(x) || is.factor(x)) {
      datos[[nombre]] <- rep(reemplazo, NROW(x))
    } else if (is.atomic(x)) {
      x[] <- NA
      datos[[nombre]] <- x
    } else {
      datos[[nombre]] <- rep(list(NULL), NROW(x))
    }
  }
  datos
}

.proteger_analisis <- function(x) {
  sensibles <- .columnas_personales_protegidas(x$perfil)
  x$perfil <- .proteger_perfil(x$perfil)
  if (inherits(x$perfil, "perfil") &&
      !is.null(x$perfil$duplicados_aproximados)) {
    x$perfil$duplicados_aproximados <- .proteger_duplicados_aproximados(
      x$perfil$duplicados_aproximados, sensibles
    )
  }
  if (length(sensibles)) {
    indices <- x$distribuciones$frecuencias$columna %in% sensibles
    x$distribuciones$frecuencias$valor[indices] <- "[valor protegido]"
    if (!"estado" %in% names(x$distribuciones$cuantiles)) {
      x$distribuciones$cuantiles$estado <- rep(
        "calculado", nrow(x$distribuciones$cuantiles)
      )
    }
    indices_cuantiles <- x$distribuciones$cuantiles$columna %in% sensibles
    x$distribuciones$cuantiles$valor[indices_cuantiles] <- NA_real_
    x$distribuciones$cuantiles$estado[indices_cuantiles] <- "valor_protegido"
    indices_variables <- x$variables$columna %in% sensibles
    for (campo in c("niveles_declarados", "niveles_observados", "niveles_ausentes")) {
      x$variables[[campo]][indices_variables] <- lapply(
        x$variables[[campo]][indices_variables],
        function(y) if (length(y)) "[valor protegido]" else character()
      )
    }
    indices_tiempo <- x$temporal$resumen$columna %in% sensibles
    x$temporal$resumen$fecha_minima[indices_tiempo] <- as.Date(NA)
    x$temporal$resumen$fecha_maxima[indices_tiempo] <- as.Date(NA)
    if (!"proteccion_temporal" %in% names(x$temporal$resumen)) {
      x$temporal$resumen$proteccion_temporal <- rep(
        NA_character_, nrow(x$temporal$resumen)
      )
    }
    x$temporal$resumen$proteccion_temporal[indices_tiempo] <-
      "[rangos y huecos protegidos]"
    x$temporal$huecos <- x$temporal$huecos[
      !x$temporal$huecos$columna %in% sensibles, , drop = FALSE
    ]
    if (inherits(x$plan_limpieza, "data.frame") &&
        all(c("columna", "evidencia") %in% names(x$plan_limpieza))) {
      indices_plan <- !is.na(x$plan_limpieza$columna) &
        x$plan_limpieza$columna %in% sensibles
      x$plan_limpieza$evidencia[indices_plan] <- "[evidencia protegida]"
    }
    x$propuesta_modelo <- .proteger_propuesta_analisis(
      x$propuesta_modelo, sensibles
    )
    x$datos <- .proteger_datos_conservados(x$datos, sensibles)
    x$meta$columnas_datos_protegidas <- intersect(
      sensibles,
      if (inherits(x$datos, "data.frame")) names(x$datos) else character()
    )
  }
  x
}

#' Guardar y recuperar un análisis
#'
#' Persiste un objeto [analizar()] en RDS con número de esquema. Los datos de
#' entrada no se guardan por omisión. Las funciones de reglas se sustituyen por
#' declaraciones pequeñas: una dependencia funcional se reconstruye al leer
#' sólo si los datos fueron incluidos; una función arbitraria queda desactivada.
#' Así el archivo no serializa entornos de ejecución completos.
#'
#' La protección de datos personales con evidencia suficiente se vuelve a
#' aplicar antes de escribir. Incluir datos que contienen columnas protegidas
#' exige desactivar expresamente esa protección; una clasificación débil se
#' conserva como información pero no activa esa restricción.
#'
#' @param x Objeto creado por [analizar()].
#' @param archivo Ruta del archivo RDS.
#' @param incluir_datos Si se persiste la copia de los datos conservada por
#'   `analizar(conservar_datos = TRUE)`.
#' @param proteger_datos_personales Si se protege toda evidencia derivada.
#' @param sobrescribir Si se permite reemplazar un archivo existente.
#' @param comprimir Compresión admitida por [saveRDS()]: un lógico, `"gzip"`,
#'   `"bzip2"` o `"xz"`.
#'
#' @return `guardar_analisis()` devuelve la ruta de forma invisible;
#'   `leer_analisis()` devuelve un objeto `analisis`.
#' @seealso [analizar()], [reportar()], [guardar_historico()]
#'
#' @examples
#' a <- analizar(datos_administrativos, argumentos_perfil = list(
#'   analizar_dependencias = FALSE
#' ))
#' ruta <- tempfile(fileext = ".rds")
#' guardar_analisis(a, ruta)
#' b <- leer_analisis(ruta)
#' unlink(ruta)
#' @name persistir_analisis
NULL

#' @rdname persistir_analisis
#' @export
guardar_analisis <- function(x, archivo, incluir_datos = FALSE,
                             proteger_datos_personales = TRUE,
                             sobrescribir = FALSE,
                             comprimir = "xz") {
  if (!inherits(x, "analisis")) {
    stop("`x` debe provenir de analizar().", call. = FALSE)
  }
  if (!.es_texto_escalar(archivo)) stop("`archivo` debe ser una ruta.", call. = FALSE)
  logicos <- c(incluir_datos, proteger_datos_personales, sobrescribir)
  if (!is.logical(incluir_datos) || length(incluir_datos) != 1L ||
      !is.logical(proteger_datos_personales) ||
      length(proteger_datos_personales) != 1L ||
      !is.logical(sobrescribir) || length(sobrescribir) != 1L ||
      anyNA(logicos)) {
    stop("Los controles de persistencia deben ser logicos.", call. = FALSE)
  }
  compresiones <- c("FALSE", "TRUE", "gzip", "bzip2", "xz")
  if (length(comprimir) != 1L || is.na(comprimir) ||
      !as.character(comprimir) %in% compresiones) {
    stop("`comprimir` no es un metodo admitido por saveRDS().", call. = FALSE)
  }
  if (file.exists(archivo) && !sobrescribir) {
    stop("El archivo ya existe; use `sobrescribir = TRUE`.", call. = FALSE)
  }
  if (!dir.exists(dirname(archivo))) {
    stop("No existe el directorio de destino.", call. = FALSE)
  }
  if (incluir_datos && is.null(x$datos)) {
    stop("El analisis no conservo datos; use `conservar_datos = TRUE`.",
         call. = FALSE)
  }
  sensibles <- .columnas_personales_protegidas(x$perfil)
  if (incluir_datos && proteger_datos_personales && length(sensibles)) {
    stop(
      "Incluir datos personales exige `proteger_datos_personales = FALSE` de forma explicita.",
      call. = FALSE
    )
  }
  copia <- if (proteger_datos_personales) .proteger_analisis(x) else x
  if (!incluir_datos) copia$datos <- NULL
  propuesta <- .deshidratar_propuesta(copia$propuesta_modelo)
  copia$propuesta_modelo <- propuesta$propuesta
  copia$meta$persistencia <- list(
    version_esquema = .version_esquema_analisis,
    datos_incluidos = incluir_datos,
    evidencia_protegida = proteger_datos_personales,
    funciones_sustituidas = propuesta$n_funciones
  )
  temporal <- tempfile(".lupa-analisis-", tmpdir = dirname(archivo), fileext = ".rds")
  on.exit(unlink(temporal), add = TRUE)
  saveRDS(copia, temporal, version = 3L, compress = comprimir)
  if (!file.copy(temporal, archivo, overwrite = sobrescribir)) {
    stop("No se pudo escribir el analisis.", call. = FALSE)
  }
  invisible(normalizePath(archivo, winslash = "/", mustWork = TRUE))
}

#' @rdname persistir_analisis
#' @export
leer_analisis <- function(archivo) {
  if (!.es_texto_escalar(archivo) || !file.exists(archivo)) {
    stop("`archivo` debe ser un RDS existente.", call. = FALSE)
  }
  x <- readRDS(archivo)
  if (!inherits(x, "analisis") || is.null(x$meta$version_esquema)) {
    stop("El archivo no contiene un analisis versionado.", call. = FALSE)
  }
  if (!identical(as.integer(x$meta$version_esquema), .version_esquema_analisis)) {
    stop("La version del esquema de analisis no es compatible.", call. = FALSE)
  }
  x$propuesta_modelo <- .hidratar_propuesta(x$propuesta_modelo, x$datos)
  x
}
