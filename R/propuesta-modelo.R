.propuesta_vacia <- function() {
  x <- data.frame(
    id_sugerencia = character(), prioridad = character(), incluir = logical(),
    metrica = character(), entidad = character(), atributos = character(),
    origen = character(), justificacion = character(), estado = character(),
    stringsAsFactors = FALSE
  )
  x$configuracion <- I(list())
  x$entidades_ligadas <- I(list())
  x$atributos_ligados <- I(list())
  x$referencial <- I(list())
  x
}

.nueva_sugerencia <- function(prioridad, incluir, metrica, entidad, atributos,
                              origen, justificacion, configuracion = list(),
                              entidades_ligadas = entidad,
                              atributos_ligados = atributos,
                              referencial = NULL, estado = "lista") {
  x <- data.frame(
    id_sugerencia = "", prioridad = prioridad, incluir = incluir,
    metrica = metrica, entidad = paste(entidad, collapse = " + "),
    atributos = paste(atributos, collapse = " + "), origen = origen,
    justificacion = justificacion, estado = estado,
    stringsAsFactors = FALSE
  )
  x$configuracion <- I(list(configuracion))
  x$entidades_ligadas <- I(list(entidades_ligadas))
  x$atributos_ligados <- I(list(atributos_ligados))
  x$referencial <- I(list(referencial))
  x
}

.escapar_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", x, perl = TRUE)
}

.patron_a_regex <- function(patron) {
  caracteres <- strsplit(patron, "", fixed = TRUE)[[1L]]
  salida <- character()
  i <- 1L
  while (i <= length(caracteres)) {
    token <- caracteres[[i]]
    if (token %in% c("9", "a", "A")) {
      pieza <- switch(token, `9` = "[0-9]", a = "[[:lower:]]", A = "[[:upper:]]")
      if (i < length(caracteres) && caracteres[[i + 1L]] == "+") {
        pieza <- paste0(pieza, "+")
        i <- i + 1L
      }
      salida <- c(salida, pieza)
    } else {
      salida <- c(salida, .escapar_regex(token))
    }
    i <- i + 1L
  }
  paste0("^", paste0(salida, collapse = ""), "$")
}

.regla_desde_dependencia <- function(datos, determinante, dependiente) {
  mapa <- .mapa_dependencia(datos, determinante, dependiente, soporte_minimo = 1L)
  claves <- .valores_relacion(mapa$determinante)
  valores <- .valores_relacion(mapa$dependiente)
  function(df) {
    x <- .valores_relacion(df[[determinante]])
    y <- .valores_relacion(df[[dependiente]])
    indice <- match(x, claves)
    ausente <- is.na(df[[determinante]]) | is.na(df[[dependiente]])
    resultado <- !is.na(indice) & y == valores[indice]
    resultado[ausente] <- TRUE
    resultado[is.na(resultado)] <- FALSE
    resultado
  }
}

#' Proponer un modelo de calidad desde el profiling
#'
#' Construye el puente entre el examen de datos y la medición. El resultado no
#' ejecuta métricas: es una tabla editable donde cada fila conserva el hallazgo
#' o diagnóstico que la originó, la justificación, la configuración y una marca
#' `incluir`. [modelo_desde_propuesta()] materializa únicamente las filas que el
#' usuario deja activas.
#'
#' Las reglas observadas que pueden sobreajustarse a una entrega —dominios y
#' patrones dominantes— se proponen inactivas. Los controles estructurales
#' directos, como `NoNulo`, duplicación exacta y dependencias funcionales
#' exactas, se activan. `max_sugerencias` recorta después de ordenar por
#' prioridad y el objeto declara el total en sus atributos.
#' Si una columna contiene faltantes disfrazados, `NoNulo` se propone pero queda
#' inactiva hasta normalizarlos o configurar la métrica para reconocerlos. De
#' otro modo mediría sólo los `NA` reales y sobrestimaría la completitud.
#'
#' @param perfil Objeto creado por [perfilar()].
#' @param datos Datos originales opcionales. Son necesarios para proponer
#'   dominios observados y materializar reglas de dependencia funcional.
#' @param relaciones Resultado opcional de [detectar_relaciones()].
#' @param entidades_relacion Dos nombres de entidad correspondientes a las
#'   tablas usadas en `relaciones`.
#' @param max_valores_dominio Máximo de valores para proponer un dominio por
#'   extensión.
#' @param max_sugerencias Máximo de filas devueltas.
#'
#' @return Data frame S3 de clase `propuesta_modelo`; las columnas de listas
#'   contienen la configuración y los vínculos sin convertirlos en texto.
#' @export
#'
#' @seealso [perfilar()], [detectar_dependencias()], [modelo_desde_propuesta()],
#'   [modelo()]
#'
#' @examples
#' datos <- data.frame(
#'   codigo = rep(1:3, each = 4),
#'   categoria = rep(c("A", "B", "C"), each = 4),
#'   valor = c(1:11, NA)
#' )
#' propuesta <- proponer_modelo(perfilar(datos), datos)
#' propuesta[, c("metrica", "origen", "incluir")]
proponer_modelo <- function(perfil, datos = NULL, relaciones = NULL,
                            entidades_relacion = character(),
                            max_valores_dominio = 20L,
                            max_sugerencias = 100L) {
  if (!inherits(perfil, "perfil")) {
    stop("`perfil` debe ser un objeto de clase perfil.", call. = FALSE)
  }
  if (!is.null(datos) && !inherits(datos, "data.frame")) {
    stop("`datos` debe ser NULL o heredar de data.frame.", call. = FALSE)
  }
  limites <- c(max_valores_dominio, max_sugerencias)
  if (anyNA(limites) || any(!is.finite(limites)) || any(limites < 1) ||
      any(limites != floor(limites))) {
    stop("Los m\u00e1ximos deben ser enteros positivos.", call. = FALSE)
  }
  entidad <- perfil$meta$nombre
  if (!.es_texto_escalar(entidad)) entidad <- "datos"
  if (!is.null(datos) && !identical(names(datos), perfil$columnas$columna)) {
    stop("Los nombres de `datos` no coinciden con los usados por el perfil.",
         call. = FALSE)
  }
  sugerencias <- list()
  agregar_sugerencia <- function(x) {
    sugerencias[[length(sugerencias) + 1L]] <<- x
  }

  for (i in seq_len(nrow(perfil$columnas))) {
    fila <- perfil$columnas[i, , drop = FALSE]
    columna <- fila$columna[[1L]]
    if (fila$n_faltantes_totales[[1L]] > 0L) {
      hallazgo <- perfil$hallazgos[
        perfil$hallazgos$columna == columna &
          perfil$hallazgos$tipo_hallazgo %in%
            c("faltantes", "faltantes_disfrazados"), , drop = FALSE
      ]
      tipo_origen <- if (any(hallazgo$tipo_hallazgo == "faltantes")) {
        "faltantes"
      } else if (nrow(hallazgo)) {
        "faltantes_disfrazados"
      } else {
        ""
      }
      origen <- if (nzchar(tipo_origen)) {
        paste0("hallazgo:", tipo_origen)
      } else {
        "perfil:n_faltantes_totales"
      }
      n_disfrazados <- fila$n_faltantes_disfrazados[[1L]]
      agregar_sugerencia(.nueva_sugerencia(
        "alta", n_disfrazados == 0L, "NoNulo", entidad, columna, origen,
        if (n_disfrazados > 0L) {
          paste0(
            "La columna contiene ", fila$n_faltantes_totales[[1L]],
            " ausentes totales, incluidos ", n_disfrazados,
            " disfrazados. La sugerencia queda inactiva hasta normalizarlos ",
            "o declararlos en `valores_nulos`."
          )
        } else {
          paste0(
            "La columna contiene ", fila$n_faltantes[[1L]],
            " valores ausentes; la completitud debe medirse por separado."
          )
        }
      ))
    }
    patrones <- perfil$patrones[[i]]
    if (!is.null(patrones) && nrow(patrones) &&
        is.finite(patrones$proporcion[[1L]]) &&
        patrones$proporcion[[1L]] >= 0.8) {
      patron <- patrones$patron[[1L]]
      agregar_sugerencia(.nueva_sugerencia(
        "media", FALSE, "Formato", entidad, columna,
        paste0("perfil:patron_dominante:", patron),
        paste0(
          "El patr\u00f3n ", patron, " cubre ",
          sprintf("%.3f", patrones$proporcion[[1L]]),
          " de los valores analizados; debe confirmarse como requisito, no s\u00f3lo como costumbre."
        ),
        configuracion = list(expresion_regular = .patron_a_regex(patron))
      ))
    }
    if (!is.null(datos) &&
        fila$tipo_declarado[[1L]] %in% c("texto", "factor", "factor-ordenado") &&
        fila$n_distintos[[1L]] >= 2L &&
        fila$n_distintos[[1L]] <= max_valores_dominio &&
        is.finite(fila$tasa_distintos[[1L]]) && fila$tasa_distintos[[1L]] <= 0.5) {
      valores <- unique(datos[[columna]][!is.na(datos[[columna]])])
      agregar_sugerencia(.nueva_sugerencia(
        "baja", FALSE, "ValoresPosiblesPorExtension", entidad, columna,
        "perfil:dominio_observado",
        paste0(
          "Se observaron ", length(valores),
          " categor\u00edas; una entrega no demuestra que el dominio est\u00e9 completo."
        ), configuracion = list(valores = valores)
      ))
    }
  }

  duplicados <- perfil$hallazgos$tipo_hallazgo == "filas_duplicadas"
  if (any(duplicados)) {
    agregar_sugerencia(.nueva_sugerencia(
      "alta", TRUE, "EntidadDuplicada", entidad, character(),
      "hallazgo:filas_duplicadas",
      "El perfil encontr\u00f3 filas completas id\u00e9nticas; la m\u00e9trica permite seguir su evoluci\u00f3n."
    ))
  }

  dependencias <- perfil$dependencias
  if (!is.null(dependencias) && nrow(dependencias)) {
    for (i in seq_len(nrow(dependencias))) {
      dependencia <- dependencias[i, , drop = FALSE]
      atributos <- c(dependencia$determinante[[1L]], dependencia$dependiente[[1L]])
      puede_materializar <- !is.null(datos)
      configuracion <- if (puede_materializar) {
        list(regla = .regla_desde_dependencia(datos, atributos[[1L]], atributos[[2L]]))
      } else {
        list()
      }
      agregar_sugerencia(.nueva_sugerencia(
        if (dependencia$exacta[[1L]]) "alta" else "media",
        dependencia$exacta[[1L]] && puede_materializar,
        "ReglaIntegridadIntraEntidad", entidad, atributos,
        paste0("dependencia_funcional:", atributos[[1L]], "->", atributos[[2L]]),
        paste0(
          "La dependencia cumple en ",
          sprintf("%.4f", dependencia$cumplimiento[[1L]]), " de ",
          dependencia$n_evaluados[[1L]], " pares presentes."
        ), configuracion = configuracion,
        estado = if (puede_materializar) "lista" else "requiere_datos"
      ))
    }
  }

  if (!is.null(relaciones)) {
    if (!inherits(relaciones, "data.frame") || length(entidades_relacion) != 2L ||
        anyNA(entidades_relacion) || any(!nzchar(entidades_relacion))) {
      stop(
        "`relaciones` requiere dos nombres en `entidades_relacion`.",
        call. = FALSE
      )
    }
    for (i in seq_len(nrow(relaciones))) {
      relacion <- relaciones[i, , drop = FALSE]
      direcciones <- list(
        list(cobertura = relacion$cobertura_tabla2_en_tabla1[[1L]],
             entidades = entidades_relacion,
             atributos = c(relacion$columna_tabla1[[1L]],
                           relacion$columna_tabla2[[1L]])),
        list(cobertura = relacion$cobertura_tabla1_en_tabla2[[1L]],
             entidades = rev(entidades_relacion),
             atributos = c(relacion$columna_tabla2[[1L]],
                           relacion$columna_tabla1[[1L]]))
      )
      for (direccion in direcciones) {
        if (!is.finite(direccion$cobertura) || direccion$cobertura < 0.8) next
        agregar_sugerencia(.nueva_sugerencia(
          if (direccion$cobertura == 1) "alta" else "media",
          direccion$cobertura == 1, "ReglaIntegridadInterEntidad",
          direccion$entidades, direccion$atributos,
          paste0("relacion_detectada:cobertura=",
                 sprintf("%.4f", direccion$cobertura)),
          "La cobertura observada sugiere una relaci\u00f3n PK/FK que debe confirmarse.",
          configuracion = list(muestra = Inf),
          entidades_ligadas = direccion$entidades,
          atributos_ligados = direccion$atributos
        ))
      }
    }
  }

  resultado <- if (length(sugerencias)) do.call(rbind, sugerencias) else .propuesta_vacia()
  if (nrow(resultado)) {
    orden_prioridad <- match(resultado$prioridad, c("alta", "media", "baja"))
    resultado <- resultado[order(orden_prioridad, resultado$metrica,
                                 resultado$entidad, resultado$atributos), , drop = FALSE]
    rownames(resultado) <- NULL
  }
  total <- nrow(resultado)
  resultado <- utils::head(resultado, max_sugerencias)
  resultado$id_sugerencia <- sprintf("sugerencia-%04d", seq_len(nrow(resultado)))
  resultado$prioridad <- factor(
    resultado$prioridad, levels = c("baja", "media", "alta"), ordered = TRUE
  )
  class(resultado) <- c("propuesta_modelo", "data.frame")
  attr(resultado, "total_sugerencias") <- total
  attr(resultado, "truncado") <- total > nrow(resultado)
  resultado
}

#' Materializar una propuesta de modelo de calidad
#'
#' Convierte las filas con `incluir == TRUE` en métricas instanciadas y las
#' reúne mediante [modelo()]. No vuelve a examinar los datos ni ejecuta una
#' medición.
#'
#' @param propuesta Objeto creado por [proponer_modelo()].
#'
#' @return Objeto `modelo_calidad` listo para [medir()].
#' @export
#'
#' @seealso [proponer_modelo()], [modelo()], [medir()]
#'
#' @examples
#' datos <- data.frame(x = c(1, NA, 3))
#' propuesta <- proponer_modelo(perfilar(datos), datos)
#' modelo_propuesto <- modelo_desde_propuesta(propuesta)
#' medir(modelo_propuesto, datos)
modelo_desde_propuesta <- function(propuesta) {
  requeridas <- c(
    "id_sugerencia", "incluir", "metrica", "configuracion",
    "entidades_ligadas", "atributos_ligados", "referencial", "estado"
  )
  if (!inherits(propuesta, "data.frame") || !all(requeridas %in% names(propuesta)) ||
      !is.logical(propuesta$incluir) || anyNA(propuesta$incluir)) {
    stop("`propuesta` no cumple el contrato de una propuesta de modelo.",
         call. = FALSE)
  }
  seleccion <- which(propuesta$incluir)
  if (!length(seleccion)) {
    stop("La propuesta no contiene sugerencias activas.", call. = FALSE)
  }
  if (any(propuesta$estado[seleccion] != "lista")) {
    stop("S\u00f3lo se pueden materializar sugerencias con estado 'lista'.",
         call. = FALSE)
  }
  nucleo <- metricas_nucleo()
  instancias <- lapply(seleccion, function(i) {
    nombre <- propuesta$metrica[[i]]
    if (!nombre %in% names(nucleo)) {
      stop("No existe una m\u00e9trica materializable llamada '", nombre, "'.",
           call. = FALSE)
    }
    configuracion <- propuesta$configuracion[[i]]
    especifica <- do.call(
      especializar,
      c(list(metrica = nucleo[[nombre]],
             nombre_especifico = paste0(nombre, "Propuesto")), configuracion)
    )
    instanciar(
      especifica,
      entidad = propuesta$entidades_ligadas[[i]],
      atributos = propuesta$atributos_ligados[[i]],
      nombre_instancia = propuesta$id_sugerencia[[i]],
      referencial = propuesta$referencial[[i]]
    )
  })
  modelo(instancias)
}

#' @export
`[.propuesta_modelo` <- function(x, ...) {
  resultado <- NextMethod("[")
  if (inherits(resultado, "data.frame") &&
      all(c("id_sugerencia", "incluir", "configuracion") %in% names(resultado))) {
    class(resultado) <- unique(c("propuesta_modelo", class(resultado)))
  } else if (inherits(resultado, "data.frame")) {
    class(resultado) <- setdiff(class(resultado), "propuesta_modelo")
  }
  resultado
}
