.deriva_perfil_vacia <- function() {
  resultado <- data.frame(
    columna = character(), aspecto = character(), cambio = character(),
    severidad = character(), valor_anterior = character(),
    valor_actual = character(), delta = numeric(),
    cambio_relativo = numeric(), significativo = logical(),
    fecha_anterior = as.POSIXct(character(), tz = "UTC"),
    fecha_actual = as.POSIXct(character(), tz = "UTC"),
    descripcion = character(), evidencia = character(),
    stringsAsFactors = FALSE
  )
  resultado$severidad <- factor(
    resultado$severidad, levels = c("ok", "sospechoso", "error"),
    ordered = TRUE
  )
  class(resultado) <- c("deriva_perfil", "data.frame")
  resultado
}

.validar_perfil_deriva <- function(x, nombre) {
  if (!inherits(x, "perfil") || !inherits(x$columnas, "data.frame") ||
      !is.list(x$patrones) || !inherits(x$hallazgos, "data.frame") ||
      is.null(x$meta$fecha_hora)) {
    stop("`", nombre, "` debe ser un objeto producido por perfilar().",
         call. = FALSE)
  }
  x
}

.mapa_columnas_perfil <- function(perfil) {
  nombres <- as.character(perfil$columnas$columna)
  nombres[is.na(nombres) | !nzchar(nombres)] <- "<sin_nombre>"
  data.frame(
    clave = make.unique(nombres), original = nombres,
    indice = seq_along(nombres), stringsAsFactors = FALSE
  )
}

.distinto_deriva <- function(a, b) {
  if (length(a) != 1L || length(b) != 1L) return(TRUE)
  if (is.na(a) && is.na(b)) return(FALSE)
  if (is.na(a) || is.na(b)) return(TRUE)
  !isTRUE(all.equal(a, b, check.attributes = FALSE))
}

.texto_deriva <- function(x) {
  if (!length(x) || is.na(x)) return(NA_character_)
  if (is.numeric(x)) return(format(x, digits = 8L, trim = TRUE))
  as.character(x)
}

.configuracion_patrones_perfil <- function(perfil) {
  campos <- c(
    "muestra", "max_patrones", "distinguir_mayusculas", "expandir",
    "umbral_patron_raro"
  )
  if (!all(campos %in% names(perfil$meta))) return(NULL)
  perfil$meta[campos]
}

.configuracion_sentinelas_perfil <- function(perfil) {
  if (!"sentinelas_numericos" %in% names(perfil$meta)) return(NULL)
  perfil$meta$sentinelas_numericos
}

.rango_perfil <- function(fila) {
  if (is.finite(fila$minimo) && is.finite(fila$maximo)) {
    return(list(
      tipo = "numerico", minimo = fila$minimo, maximo = fila$maximo,
      texto = paste0("[", .texto_deriva(fila$minimo), ", ",
                     .texto_deriva(fila$maximo), "]")
    ))
  }
  if (!is.na(fila$minimo_fecha) && !is.na(fila$maximo_fecha)) {
    limites <- tryCatch(
      suppressWarnings(as.POSIXct(
        c(fila$minimo_fecha, fila$maximo_fecha), tz = "UTC"
      )),
      error = function(e) as.POSIXct(c(NA, NA), tz = "UTC")
    )
    if (length(limites) != 2L || anyNA(limites)) return(NULL)
    a <- as.numeric(limites[[1L]])
    b <- as.numeric(limites[[2L]])
    return(list(
      tipo = "fecha", minimo = a, maximo = b,
      texto = paste0("[", fila$minimo_fecha, ", ", fila$maximo_fecha, "]")
    ))
  }
  NULL
}

.magnitud_rango <- function(anterior, actual) {
  if (is.null(anterior) || is.null(actual) ||
      anterior$tipo != actual$tipo) return(Inf)
  ancho <- abs(anterior$maximo - anterior$minimo)
  escala <- if (is.finite(ancho) && ancho > 0) ancho else {
    max(abs(c(anterior$minimo, anterior$maximo)), 1)
  }
  max(
    abs(actual$minimo - anterior$minimo),
    abs(actual$maximo - anterior$maximo)
  ) / escala
}

.patrones_perfil <- function(perfil, indice) {
  x <- perfil$patrones[[indice]]
  if (!inherits(x, "data.frame") || !all(c("patron", "proporcion") %in% names(x))) {
    return(data.frame(
      patron = character(), proporcion = numeric(), stringsAsFactors = FALSE
    ))
  }
  x[c("patron", "proporcion")]
}

.clave_patron <- function(x) {
  ifelse(is.na(x), "<NA>", as.character(x))
}

# Los diagnosticos que el perfil declino, como `columna tipo`, para poder
# distinguir un hallazgo resuelto de uno que no se volvio a evaluar.
.diagnosticos_declinados_deriva <- function(perfil) {
  cobertura <- perfil$cobertura_diagnosticos
  if (!inherits(cobertura, "data.frame") || !nrow(cobertura) ||
        !all(c("columna", "diagnostico") %in% names(cobertura))) {
    return(character())
  }
  paste(as.character(cobertura$columna), as.character(cobertura$diagnostico))
}

.resumir_hallazgos_deriva <- function(perfil) {
  x <- perfil$hallazgos
  if (!nrow(x)) {
    return(data.frame(
      clave = character(), columna = character(), tipo_hallazgo = character(),
      severidad = character(), evidencia = character(), stringsAsFactors = FALSE
    ))
  }
  columna <- ifelse(is.na(x$columna), "<tabla>", as.character(x$columna))
  clave <- paste(columna, x$tipo_hallazgo, sep = "\034")
  grupos <- split(seq_len(nrow(x)), clave, drop = TRUE)
  partes <- lapply(grupos, function(indices) {
    nivel <- match(as.character(x$severidad[indices]),
                   c("ok", "sospechoso", "error"))
    elegido <- indices[which.max(nivel)]
    data.frame(
      clave = clave[[elegido]], columna = columna[[elegido]],
      tipo_hallazgo = x$tipo_hallazgo[[elegido]],
      severidad = as.character(x$severidad[[elegido]]),
      evidencia = x$evidencia[[elegido]], stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, partes)
  rownames(resultado) <- NULL
  resultado
}

#' Comparar dos perfiles y detectar deriva estructural
#'
#' Compara entregas sin exigir que tengan las mismas columnas. Devuelve cambios
#' de esquema, tipos, faltantes, cardinalidad, rango, patrones y hallazgos como
#' un objeto de datos filtrable.
#'
#' @param anterior,actual Objetos producidos por [perfilar()].
#' @param umbral_cambio Diferencia mínima para considerar significativo un
#'   cambio de proporción, cardinalidad o rango relativo. Cinco puntos
#'   porcentuales evita elevar variaciones pequeñas a hallazgo.
#' @param umbral_error Diferencia a partir de la cual un aumento de faltantes o
#'   un patrón nuevo se clasifica como `error`.
#'
#' @return Data frame `deriva_perfil`. `severidad` usa el factor ordenado
#'   `ok < sospechoso < error`; los cambios menores permanecen como filas `ok`
#'   para que la serie sea exportable sin ocultar diferencias.
#'
#' @details
#' Los patrones se comparan sobre el resumen acotado que conserva cada perfil,
#' no sobre los valores originales ni una distribución completa. Si las dos
#' corridas usaron configuraciones de patrones diferentes, se informa un error
#' de comparabilidad y esa parte de la comparación se omite.
#'
#' Las columnas que aparecen o desaparecen generan cambios estructurales de
#' severidad `error`, pero no impiden comparar las columnas compartidas. Un
#' hallazgo de una columna retirada no se presenta como resuelto.
#'
#' Un hallazgo que ya no aparece se informa como `resuelto` sólo si el
#' diagnóstico volvió a evaluarse. Si el perfil nuevo lo declinó —y lo dice en
#' su `cobertura_diagnosticos`—, el cambio se informa como `no_evaluado` con
#' severidad `sospechoso`, porque no se sabe si el hallazgo sigue: dejar de
#' mirar no es lo mismo que arreglar.
#'
#' @export
#' @seealso [perfilar()], [detectar_deriva_calidad()], [reportar()],
#'   [comparar_equivalencia()]
#'
#' @examples
#' anterior <- perfilar(data.frame(codigo = c("AA1", "AA2")),
#'                      fecha = as.POSIXct("2026-01-01", tz = "UTC"))
#' actual <- perfilar(data.frame(codigo = c("AA1", "B-2"), nueva = 1:2),
#'                    fecha = as.POSIXct("2026-02-01", tz = "UTC"))
#' comparar_perfiles(anterior, actual)
comparar_perfiles <- function(anterior, actual, umbral_cambio = 0.05,
                              umbral_error = 0.20) {
  anterior <- .validar_perfil_deriva(anterior, "anterior")
  actual <- .validar_perfil_deriva(actual, "actual")
  umbrales <- c(umbral_cambio, umbral_error)
  if (!is.numeric(umbrales) || anyNA(umbrales) ||
      any(!is.finite(umbrales)) || any(umbrales < 0 | umbrales > 1) ||
      umbral_error < umbral_cambio) {
    stop(
      "Los umbrales deben estar en [0, 1] y `umbral_error` no puede ser menor.",
      call. = FALSE
    )
  }
  fecha_anterior <- .fecha_utc(anterior$meta$fecha_hora)
  fecha_actual <- .fecha_utc(actual$meta$fecha_hora)
  mapa_a <- .mapa_columnas_perfil(anterior)
  mapa_b <- .mapa_columnas_perfil(actual)
  cambios <- list()
  k <- 0L
  agregar <- function(columna, aspecto, cambio, severidad, valor_anterior,
                      valor_actual, delta = NA_real_, cambio_relativo = NA_real_,
                      significativo = TRUE, descripcion, evidencia = "") {
    k <<- k + 1L
    cambios[[k]] <<- data.frame(
      columna = columna, aspecto = aspecto, cambio = cambio,
      severidad = severidad, valor_anterior = .texto_deriva(valor_anterior),
      valor_actual = .texto_deriva(valor_actual), delta = delta,
      cambio_relativo = cambio_relativo, significativo = significativo,
      fecha_anterior = fecha_anterior, fecha_actual = fecha_actual,
      descripcion = descripcion, evidencia = evidencia,
      stringsAsFactors = FALSE
    )
  }

  desaparecidas <- setdiff(mapa_a$clave, mapa_b$clave)
  aparecidas <- setdiff(mapa_b$clave, mapa_a$clave)
  for (columna in desaparecidas) {
    agregar(
      columna, "columna", "desaparecida", "error", columna, NA_character_,
      descripcion = "La columna desapareci\u00f3 de la entrega actual.",
      evidencia = "No se comparan sus m\u00e9tricas, patrones ni rango."
    )
  }
  for (columna in aparecidas) {
    agregar(
      columna, "columna", "aparecida", "error", NA_character_, columna,
      descripcion = "Apareci\u00f3 una columna que no estaba en la entrega anterior.",
      evidencia = "No existe una base anterior para sus m\u00e9tricas."
    )
  }

  comunes <- intersect(mapa_a$clave, mapa_b$clave)
  for (columna in comunes) {
    indice_a <- mapa_a$indice[match(columna, mapa_a$clave)]
    indice_b <- mapa_b$indice[match(columna, mapa_b$clave)]
    a <- anterior$columnas[indice_a, , drop = FALSE]
    b <- actual$columnas[indice_b, , drop = FALSE]
    for (campo in c("tipo_declarado", "tipo_inferido")) {
      if (.distinto_deriva(a[[campo]], b[[campo]])) {
        agregar(
          columna, campo, "modificado", "error", a[[campo]], b[[campo]],
          descripcion = paste0(
            "Cambi\u00f3 el ", gsub("_", " ", campo, fixed = TRUE),
            " de la columna."
          )
        )
      }
    }
    for (especificacion in list(
      c(campo = "prop_faltantes_totales", aspecto = "faltantes"),
      c(campo = "tasa_distintos", aspecto = "cardinalidad")
    )) {
      campo <- especificacion[["campo"]]
      aspecto <- especificacion[["aspecto"]]
      if (.distinto_deriva(a[[campo]], b[[campo]])) {
        delta <- b[[campo]] - a[[campo]]
        significativo <- is.finite(delta) && abs(delta) >= umbral_cambio
        severidad <- if (!significativo || delta < 0) {
          "ok"
        } else if (aspecto == "faltantes" && delta >= umbral_error) {
          "error"
        } else {
          "sospechoso"
        }
        evidencia <- if (aspecto == "cardinalidad") {
          paste0(
            "Valores distintos: ", a$n_distintos, " -> ", b$n_distintos,
            "; tasas: ", sprintf("%.4f", a[[campo]]), " -> ",
            sprintf("%.4f", b[[campo]])
          )
        } else {
          paste0("Cambio de ", sprintf("%+.4f", delta), " en escala [0, 1].")
        }
        descripcion <- if (aspecto == "faltantes") {
          "Cambi\u00f3 la proporci\u00f3n de faltantes de la columna."
        } else {
          "Cambi\u00f3 la tasa de valores distintos de la columna."
        }
        agregar(
          columna, aspecto, "modificado", severidad, a[[campo]], b[[campo]],
          delta = delta, significativo = significativo,
          descripcion = descripcion,
          evidencia = evidencia
        )
      }
    }
    rango_a <- .rango_perfil(a)
    rango_b <- .rango_perfil(b)
    if ((!is.null(rango_a) || !is.null(rango_b)) &&
        (is.null(rango_a) || is.null(rango_b) ||
         rango_a$texto != rango_b$texto || rango_a$tipo != rango_b$tipo)) {
      magnitud <- .magnitud_rango(rango_a, rango_b)
      significativo <- is.infinite(magnitud) || magnitud >= umbral_cambio
      agregar(
        columna, "rango", "modificado",
        if (significativo) "sospechoso" else "ok",
        if (is.null(rango_a)) NA_character_ else rango_a$texto,
        if (is.null(rango_b)) NA_character_ else rango_b$texto,
        cambio_relativo = magnitud, significativo = significativo,
        descripcion = "Cambi\u00f3 el rango observado de la columna."
      )
    }
  }

  config_a <- .configuracion_patrones_perfil(anterior)
  config_b <- .configuracion_patrones_perfil(actual)
  patrones_comparables <- !is.null(config_a) && !is.null(config_b) &&
    isTRUE(all.equal(config_a, config_b, check.attributes = FALSE))
  if (!patrones_comparables) {
    agregar(
      NA_character_, "configuracion_patrones", "no_comparable", "error",
      paste(unlist(config_a), collapse = "; "),
      paste(unlist(config_b), collapse = "; "),
      descripcion = paste0(
        "Las corridas no declaran la misma configuraci\u00f3n de patrones; ",
        "sus patrones no se comparan."
      )
    )
  } else {
    for (columna in comunes) {
      indice_a <- mapa_a$indice[match(columna, mapa_a$clave)]
      indice_b <- mapa_b$indice[match(columna, mapa_b$clave)]
      pa <- .patrones_perfil(anterior, indice_a)
      pb <- .patrones_perfil(actual, indice_b)
      claves_a <- .clave_patron(pa$patron)
      claves_b <- .clave_patron(pb$patron)
      for (patron in setdiff(claves_b, claves_a)) {
        proporcion <- pb$proporcion[match(patron, claves_b)]
        agregar(
          columna, "patron", "aparecido",
          if (proporcion >= umbral_error) "error" else "sospechoso",
          NA_character_, patron, delta = proporcion, significativo = TRUE,
          descripcion = "Apareci\u00f3 un patr\u00f3n de formato nuevo.",
          evidencia = paste0("Proporci\u00f3n actual: ", sprintf("%.4f", proporcion))
        )
      }
      for (patron in setdiff(claves_a, claves_b)) {
        proporcion <- pa$proporcion[match(patron, claves_a)]
        agregar(
          columna, "patron", "desaparecido",
          if (proporcion >= umbral_error) "error" else "sospechoso",
          patron, NA_character_, delta = -proporcion, significativo = TRUE,
          descripcion = "Desapareci\u00f3 un patr\u00f3n de formato anterior.",
          evidencia = paste0("Proporci\u00f3n anterior: ", sprintf("%.4f", proporcion))
        )
      }
    }
  }

  sentinelas_a <- .configuracion_sentinelas_perfil(anterior)
  sentinelas_b <- .configuracion_sentinelas_perfil(actual)
  sentinelas_comparables <- !is.null(sentinelas_a) &&
    !is.null(sentinelas_b) &&
    isTRUE(all.equal(sentinelas_a, sentinelas_b, check.attributes = FALSE))
  if (!sentinelas_comparables) {
    texto_configuracion <- function(x) {
      if (is.null(x)) return(NA_character_)
      paste(unlist(x), collapse = "; ")
    }
    agregar(
      NA_character_, "configuracion_sentinelas_numericos", "modificado",
      "error", texto_configuracion(sentinelas_a),
      texto_configuracion(sentinelas_b),
      descripcion = paste(
        "Cambi\u00f3 la pol\u00edtica de centinelas num\u00e9ricos; se mantienen las",
        "comparaciones para que las corridas siguientes sigan detectando",
        "deriva con la pol\u00edtica vigente."
      ),
      evidencia = paste(
        "Las diferencias de m\u00e9tricas o hallazgos pueden atribuirse a esta",
        "pol\u00edtica en la transici\u00f3n; revisar la configuraci\u00f3n publicada."
      )
    )
  }

  hallazgos_a <- .resumir_hallazgos_deriva(anterior)
  hallazgos_b <- .resumir_hallazgos_deriva(actual)
  nuevos <- setdiff(hallazgos_b$clave, hallazgos_a$clave)
  resueltos <- setdiff(hallazgos_a$clave, hallazgos_b$clave)
  nombres_actuales <- unique(mapa_b$original)
  for (clave in nuevos) {
    x <- hallazgos_b[match(clave, hallazgos_b$clave), , drop = FALSE]
    agregar(
      if (x$columna == "<tabla>") NA_character_ else x$columna,
      "hallazgo", "aparecido", x$severidad, NA_character_, x$tipo_hallazgo,
      descripcion = "Apareci\u00f3 un hallazgo que no estaba en el perfil anterior.",
      evidencia = x$evidencia
    )
  }
  # Un hallazgo que ya no esta puede haberse resuelto o puede no haberse
  # evaluado, y son cosas distintas: la primera es una mejora y la segunda es
  # una medicion que falta. Se distinguen mirando la cobertura del perfil nuevo,
  # que es donde el paquete declara lo que declino. Sin esta consulta se
  # informaba "resuelto" con severidad `ok` sobre un diagnostico que decia, en
  # esa misma tabla, "no se evaluaron los limites de Tukey".
  declinados_ahora <- .diagnosticos_declinados_deriva(actual)
  for (clave in resueltos) {
    x <- hallazgos_a[match(clave, hallazgos_a$clave), , drop = FALSE]
    if (x$columna != "<tabla>" && !x$columna %in% nombres_actuales) next
    declinado <- paste(x$columna, x$tipo_hallazgo) %in% declinados_ahora
    agregar(
      if (x$columna == "<tabla>") NA_character_ else x$columna,
      "hallazgo",
      if (declinado) "no_evaluado" else "resuelto",
      if (declinado) "sospechoso" else "ok",
      x$tipo_hallazgo, NA_character_,
      descripcion = if (declinado) {
        paste(
          "El diagn\u00f3stico no se evalu\u00f3 en el perfil nuevo, as\u00ed que no se sabe",
          "si el hallazgo sigue: el motivo est\u00e1 en `cobertura_diagnosticos`."
        )
      } else {
        "Un hallazgo del perfil anterior ya no est\u00e1 presente."
      },
      evidencia = x$evidencia
    )
  }
  compartidos <- intersect(hallazgos_a$clave, hallazgos_b$clave)
  for (clave in compartidos) {
    indice_a <- match(clave, hallazgos_a$clave)
    indice_b <- match(clave, hallazgos_b$clave)
    sa <- hallazgos_a$severidad[[indice_a]]
    sb <- hallazgos_b$severidad[[indice_b]]
    if (sa != sb) {
      columna <- hallazgos_b$columna[[indice_b]]
      agregar(
        if (columna == "<tabla>") NA_character_ else columna,
        "severidad_hallazgo",
        if (match(sb, c("ok", "sospechoso", "error")) >
            match(sa, c("ok", "sospechoso", "error"))) "agravado" else "atenuado",
        sb, sa, sb,
        descripcion = "Cambi\u00f3 la severidad de un hallazgo persistente.",
        evidencia = hallazgos_b$tipo_hallazgo[[indice_b]]
      )
    }
  }

  if (!length(cambios)) return(.deriva_perfil_vacia())
  resultado <- do.call(rbind, cambios)
  resultado$severidad <- factor(
    resultado$severidad, levels = c("ok", "sospechoso", "error"),
    ordered = TRUE
  )
  rownames(resultado) <- NULL
  class(resultado) <- c("deriva_perfil", "data.frame")
  resultado
}

# Registro fijo de los campos que el comparador puede interpretar. La clase
# almacenada de una columna no decide el eje: memoria y DBI publican algunos
# conteos con clases distintas, y el mismo campo debe conservar la misma regla.
# Si aparece una métrica nueva, se agrega explícitamente acá antes de compararla.
.registro_campos_equivalencia <- function() {
  list(
    flotante = c("media", "mediana", "desvio", "longitud_media"),
    exacto = c(
      "proporcion_tipo_inferido", "n_filas_analizadas_tipo", "n",
      "n_validos",
      "n_aplicables", "n_no_aplica", "n_aplicabilidad_indeterminada",
      "n_presentes_fuera_de_aplicabilidad", "n_faltantes",
      "prop_faltantes", "n_faltantes_disfrazados",
      "n_faltantes_disfrazados_textuales", "n_faltantes_disfrazados_numericos",
      "prop_faltantes_disfrazados", "n_faltantes_totales",
      "prop_faltantes_totales", "n_distintos", "tasa_distintos",
      "secuencia_entera_densa", "densidad_secuencia_entera",
      "n_posiciones_secuencia_entera", "n_huecos_secuencia_entera",
      "hueco_maximo_secuencia_entera", "salto_de_escala_secuencia_entera",
      "umbral_densidad_secuencia_entera", "min_distintos_secuencia_entera",
      "frecuencia_moda", "longitud_minima", "longitud_maxima",
      "minimo", "maximo", "minimo_exacto", "maximo_exacto",
      "n_fechas_resumidas", "n_fechas_excluidas_granularidad",
      "n_valores_excluidos_resumen", "n_ceros",
      "n_negativos", "n_outliers", "centinela_repeticiones",
      "densidad_sin_centinela", "n_nan", "n_infinito_positivo",
      "n_infinito_negativo", "n_filas_fecha_civil_distinta_utc", "n_blancos",
      "n_espacios_borde", "n_variantes_mayusculas", "n_variantes_unicode",
      "n_codificacion_rota", "n_codificacion_reparable",
      "n_codificacion_reparable_parcialmente", "n_codificacion_irreparable",
      "n_codificacion_no_se_pudo", "n_codificacion_invalida",
      "n_controles_invisibles", "n_invisibles_eliminables",
      "n_espacios_invisibles", "n_invisibles_significativos",
      "n_entidades_html", "n_separadores_en_campo", "n_numeros_texto",
      "proporcion_numeros_texto"
    ),
    fecha = c(
      "minimo_fecha", "maximo_fecha", "media_fecha", "mediana_fecha"
    ),
    valor = c("moda", "centinela_valor")
  )
}

.campos_magnitud_equivalencia <- function(registro) {
  # Estos son los campos cuyo numero representa una magnitud de la columna.
  # Los conteos y proporciones quedan fuera: que la columna haya pasado de
  # fecha a numero no cambia la unidad de contar filas o valores distintos.
  unique(intersect(
    c(registro$flotante, "minimo", "maximo", "centinela_valor"),
    unlist(registro, use.names = FALSE)
  ))
}

.tipo_columna_equivalencia <- function(columnas, indice) {
  nombres <- c(
    "tipo_inferido", "clase_temporal", "tipo_temporal", "clase",
    "tipo_declarado"
  )
  nombres <- intersect(nombres, names(columnas))
  for (nombre in nombres) {
    valor <- as.character(columnas[[nombre]][[indice]])
    if (length(valor) == 1L && !is.na(valor) && nzchar(trimws(valor))) {
      return(tolower(trimws(valor)))
    }
  }
  NA_character_
}

.es_temporal_equivalencia <- function(columnas, indice) {
  tipo <- .tipo_columna_equivalencia(columnas, indice)
  if (is.na(tipo)) return(FALSE)
  tipo %in% c(
    "fecha", "fecha-hora", "fecha_hora", "date", "datetime", "timestamp",
    "posixct", "posixlt", "temporal"
  ) || grepl("^(fecha|date|datetime|timestamp|posix)", tipo)
}

.campos_protegidos_equivalencia_vacios <- function() {
  data.frame(
    columna = character(), campo = character(), lado = character(),
    stringsAsFactors = FALSE
  )
}

.detalle_proteccion_campo_equivalencia <- function(detalle, campo) {
  if (length(detalle) != 1L || is.na(detalle) || !nzchar(detalle)) {
    return(FALSE)
  }
  if (identical(campo, "moda") && grepl("moda", detalle, fixed = TRUE)) {
    return(TRUE)
  }
  if (campo %in% c("media", "media_fecha") &&
      grepl("momentos", detalle, fixed = TRUE)) {
    return(TRUE)
  }
  campos_orden <- c(
    "minimo", "maximo", "mediana", "minimo_exacto", "maximo_exacto",
    "minimo_fecha", "maximo_fecha", "mediana_fecha", "centinela_valor",
    "n_posiciones_secuencia_entera", "n_huecos_secuencia_entera",
    "hueco_maximo_secuencia_entera", "densidad_secuencia_entera",
    "densidad_sin_centinela"
  )
  campo %in% campos_orden && grepl("orden", detalle, fixed = TRUE)
}

.campo_protegido_equivalencia <- function(columnas, campo, indice) {
  protegido <- if ("dato_personal_protegido" %in% names(columnas)) {
    isTRUE(columnas$dato_personal_protegido[[indice]])
  } else {
    FALSE
  }
  valor <- .valor_equivalencia(columnas, campo, indice)
  if (protegido || (is.character(valor) && length(valor) == 1L &&
                    identical(valor, "[valor protegido]"))) {
    return(TRUE)
  }
  if (!.faltante_equivalencia(valor) ||
      !"detalle_proteccion_personal" %in% names(columnas)) {
    return(FALSE)
  }
  .detalle_proteccion_campo_equivalencia(
    columnas$detalle_proteccion_personal[[indice]], campo
  )
}

.columnas_equivalencia <- function(x, nombre) {
  if (inherits(x, "perfil")) {
    columnas <- x$columnas
  } else if (inherits(x, "perfil_dbi")) {
    columnas <- x$resumen_tabla$columnas
  } else if (is.data.frame(x)) {
    columnas <- x
  } else {
    stop(
      "`", nombre, "` debe ser un `perfil`, un `perfil_dbi` o un frame `columnas`.",
      call. = FALSE
    )
  }
  if (!is.data.frame(columnas) || !"columna" %in% names(columnas)) {
    stop(
      "`", nombre, "` no contiene un frame `columnas` con el campo `columna`.",
      call. = FALSE
    )
  }
  nombres <- as.character(columnas$columna)
  if (anyNA(nombres) || anyDuplicated(nombres)) {
    stop(
      "El frame `columnas` de `", nombre,
      "` debe tener nombres de columna presentes y sin duplicados.", call. = FALSE
    )
  }
  columnas
}

.valor_equivalencia <- function(columnas, campo, indice) {
  columnas[[campo]][[indice]]
}

.igualdad_equivalencia <- function(a, b) {
  resultado <- tryCatch(a == b, error = function(e) FALSE)
  isTRUE(resultado)
}

.faltante_equivalencia <- function(x) {
  resultado <- tryCatch(is.na(x), error = function(e) FALSE)
  isTRUE(resultado)
}

.infinito_equivalencia <- function(x) {
  if (length(x) != 1L || !is.numeric(x)) return(FALSE)
  isTRUE(is.infinite(x))
}

.comparar_valor_equivalencia <- function(a, b, tipo_eje, tolerancia) {
  if (length(a) != 1L || length(b) != 1L) {
    stop(
      "Cada valor de un campo comparable debe ser escalar.", call. = FALSE
    )
  }
  faltante_a <- .faltante_equivalencia(a)
  faltante_b <- .faltante_equivalencia(b)
  if (faltante_a && faltante_b) {
    return(list(
      veredicto = if (identical(a, b)) "identico" else "equivalente",
      motivo = if (identical(a, b)) "igualdad_exacta" else
        "faltante_misma_clase",
      diferencia_relativa = NA_real_
    ))
  }
  if (xor(faltante_a, faltante_b)) {
    return(list(
      veredicto = "materialmente_distinto", motivo = "faltante_un_lado",
      diferencia_relativa = NA_real_
    ))
  }
  if (.infinito_equivalencia(a) || .infinito_equivalencia(b)) {
    igual <- .igualdad_equivalencia(a, b)
    return(list(
      veredicto = if (igual) "identico" else "materialmente_distinto",
      motivo = if (igual) "igualdad_exacta" else "infinito",
      diferencia_relativa = NA_real_
    ))
  }

  igual <- .igualdad_equivalencia(a, b)
  if (tipo_eje != "flotante") {
    return(list(
      veredicto = if (igual) "identico" else "materialmente_distinto",
      motivo = if (igual) "igualdad_exacta" else paste0("eje_", tipo_eje),
      diferencia_relativa = NA_real_
    ))
  }
  if (!is.numeric(a) || !is.numeric(b) ||
      !isTRUE(is.finite(a)) || !isTRUE(is.finite(b))) {
    stop(
      "Los campos del eje `flotante` deben contener numeros finitos o faltantes.",
      call. = FALSE
    )
  }
  diferencia_relativa <- suppressWarnings(
    abs(a - b) / pmax(1, abs(a), abs(b))
  )
  if (igual) {
    return(list(
      veredicto = "identico", motivo = "igualdad_exacta",
      diferencia_relativa = as.numeric(diferencia_relativa)
    ))
  }
  dentro <- .dentro_tolerancia_aritmetica(a, b, tolerancia)
  list(
    veredicto = if (isTRUE(dentro)) "equivalente" else
      "materialmente_distinto",
    motivo = if (isTRUE(dentro)) "dentro_de_tolerancia" else
      "fuera_de_tolerancia",
    diferencia_relativa = as.numeric(diferencia_relativa)
  )
}

#' Comparar la equivalencia de dos resúmenes de perfiles
#'
#' Compara por intersección los campos registrados de dos perfiles y devuelve
#' una fila por cada par de columna y campo. Los campos que no tienen un eje
#' registrado no se comparan y quedan declarados en `campos_no_comparables`.
#' Los campos bajo protección tampoco se comparan: se declaran por columna,
#' campo y lado en `campos_protegidos`.
#'
#' @param anterior,actual Un objeto `perfil` de [perfilar()], un objeto
#'   `perfil_dbi` de [perfilar_dbi()] o directamente un frame `columnas`.
#' @param tolerancia Número escalar no negativo y finito, declarado por quien
#'   llama. No tiene valor por omisión y se publica en cada fila.
#'
#' @return Un frame de clase `equivalencia_perfiles` con `columna`, `campo`,
#'   `valor_anterior`, `valor_actual`, `diferencia_relativa`, `veredicto`,
#'   `motivo`, `tipo_eje` y `tolerancia`. `veredicto` es un factor ordenado con
#'   niveles `identico < equivalente < materialmente_distinto`. Los atributos
#'   `campos_no_comparables`, `detalle_campos_no_comparables`,
#'   `campos_protegidos` y `resumen` declaran, respectivamente, los campos
#'   omitidos, los motivos estructurales de esos campos, los campos omitidos
#'   por protección y el conteo de cada veredicto. `campos_protegidos` es un
#'   data frame con las columnas `columna`, `campo` y `lado`; este último toma
#'   los valores `anterior` y `actual`.
#'
#' @details
#' El registro fijo asigna tolerancia sólo a `media`, `mediana`, `desvio` y
#' `longitud_media`. Los conteos, proporciones de conteos y extremos por
#' selección se comparan en el eje `exacto`; las fechas canónicas en `fecha` y
#' `moda` y `centinela_valor` en `valor`. Los ejes `exacto`, `fecha` y `valor`
#' son binarios por construcción.
#'
#' El comparador devuelve datos, no decisiones: no alimenta hallazgos,
#' severidades ni puntajes. La tolerancia es del llamador y jamás entra en una
#' regla del paquete; sólo se aplica al eje flotante finito y queda publicada
#' para que el llamador decida cómo usarla.
#'
#' Si una columna es temporal en exactamente uno de los perfiles, los campos
#' de magnitud numérica se omiten por el cambio de esquema y su motivo queda en
#' `detalle_campos_no_comparables` como
#' `tipo_cambiado:temporal_vs_no_temporal`. Así no se comparan duraciones en
#' segundos contra magnitudes numéricas sin unidad común. Cuando ambos lados
#' son temporales, `desvio` sí se compara en flotante porque ambas puertas lo
#' expresan en segundos.
#'
#' @name comparar_equivalencia
#' @usage comparar_equivalencia(anterior, actual, tolerancia)
#' @export
#' @seealso [comparar_perfiles()], [perfilar()], [perfilar_dbi()]
#'
#' @examples
#' anterior <- data.frame(
#'   columna = "monto", media = 10, minimo = 1, moda = "a",
#'   stringsAsFactors = FALSE
#' )
#' actual <- data.frame(
#'   columna = "monto", media = 10.00000000001, minimo = 2, moda = "b",
#'   stringsAsFactors = FALSE
#' )
#' comparar_equivalencia(anterior, actual, tolerancia = 1e-9)
comparar_equivalencia <- function(anterior, actual, tolerancia) {
  if (!is.numeric(tolerancia) || length(tolerancia) != 1L ||
      is.na(tolerancia) || !is.finite(tolerancia) || tolerancia < 0) {
    stop(
      "`tolerancia` debe ser un escalar numerico finito, sin NA y mayor o igual a 0.",
      call. = FALSE
    )
  }
  tolerancia <- as.numeric(tolerancia)
  anterior <- .columnas_equivalencia(anterior, "anterior")
  actual <- .columnas_equivalencia(actual, "actual")
  registro <- .registro_campos_equivalencia()
  campos_registrados <- unlist(registro, use.names = FALSE)
  campos_presentes <- unique(c(names(anterior), names(actual)))
  campos_no_comparables <- setdiff(
    setdiff(campos_presentes, "columna"), campos_registrados
  )
  campos <- intersect(
    setdiff(names(anterior), "columna"), setdiff(names(actual), "columna")
  )
  campos <- campos[campos %in% campos_registrados]
  columnas <- intersect(as.character(anterior$columna), as.character(actual$columna))
  campos_magnitud <- .campos_magnitud_equivalencia(registro)
  detalle_campos_no_comparables <- data.frame(
    columna = character(), campo = character(), motivo = character(),
    stringsAsFactors = FALSE
  )
  campos_protegidos <- .campos_protegidos_equivalencia_vacios()
  niveles <- c("identico", "equivalente", "materialmente_distinto")
  salida <- list()
  k <- 0L
  for (columna in columnas) {
    indice_a <- match(columna, as.character(anterior$columna))
    indice_b <- match(columna, as.character(actual$columna))
    temporal_a <- .es_temporal_equivalencia(anterior, indice_a)
    temporal_b <- .es_temporal_equivalencia(actual, indice_b)
    for (campo in campos) {
      protegido_a <- .campo_protegido_equivalencia(anterior, campo, indice_a)
      protegido_b <- .campo_protegido_equivalencia(actual, campo, indice_b)
      if (protegido_a || protegido_b) {
        registros <- list()
        if (protegido_a) {
          registros[[length(registros) + 1L]] <- data.frame(
            columna = columna, campo = campo, lado = "anterior",
            stringsAsFactors = FALSE
          )
        }
        if (protegido_b) {
          registros[[length(registros) + 1L]] <- data.frame(
            columna = columna, campo = campo, lado = "actual",
            stringsAsFactors = FALSE
          )
        }
        campos_protegidos <- rbind(campos_protegidos, do.call(rbind, registros))
        next
      }
      if (xor(temporal_a, temporal_b) && campo %in% campos_magnitud) {
        campos_no_comparables <- unique(c(campos_no_comparables, campo))
        detalle_campos_no_comparables <- rbind(
          detalle_campos_no_comparables,
          data.frame(
            columna = columna, campo = campo,
            motivo = "tipo_cambiado:temporal_vs_no_temporal",
            stringsAsFactors = FALSE
          )
        )
        next
      }
      tipo_eje <- names(registro)[vapply(
        registro, function(campos_eje) campo %in% campos_eje, logical(1L)
      )]
      a <- .valor_equivalencia(anterior, campo, indice_a)
      b <- .valor_equivalencia(actual, campo, indice_b)
      comparacion <- .comparar_valor_equivalencia(
        a, b, tipo_eje[[1L]], tolerancia
      )
      k <- k + 1L
      salida[[k]] <- list(
        columna = columna, campo = campo,
        valor_anterior = a, valor_actual = b,
        diferencia_relativa = comparacion$diferencia_relativa,
        veredicto = comparacion$veredicto, motivo = comparacion$motivo,
        tipo_eje = tipo_eje[[1L]], tolerancia = tolerancia
      )
    }
  }
  if (length(salida)) {
    resultado <- data.frame(
      columna = vapply(salida, `[[`, character(1L), "columna"),
      campo = vapply(salida, `[[`, character(1L), "campo"),
      valor_anterior = I(lapply(salida, `[[`, "valor_anterior")),
      valor_actual = I(lapply(salida, `[[`, "valor_actual")),
      diferencia_relativa = vapply(
        salida, `[[`, numeric(1L), "diferencia_relativa"
      ),
      veredicto = vapply(salida, `[[`, character(1L), "veredicto"),
      motivo = vapply(salida, `[[`, character(1L), "motivo"),
      tipo_eje = vapply(salida, `[[`, character(1L), "tipo_eje"),
      tolerancia = vapply(salida, `[[`, numeric(1L), "tolerancia"),
      stringsAsFactors = FALSE
    )
  } else {
    resultado <- data.frame(
      columna = character(), campo = character(),
      valor_anterior = I(list()), valor_actual = I(list()),
      diferencia_relativa = numeric(), veredicto = character(),
      motivo = character(), tipo_eje = character(), tolerancia = numeric(),
      stringsAsFactors = FALSE
    )
  }
  resultado$veredicto <- factor(
    resultado$veredicto, levels = niveles, ordered = TRUE
  )
  resumen <- stats::setNames(
    as.integer(table(factor(resultado$veredicto, levels = niveles))), niveles
  )
  attr(resultado, "campos_no_comparables") <- campos_no_comparables
  attr(resultado, "detalle_campos_no_comparables") <-
    detalle_campos_no_comparables
  attr(resultado, "campos_protegidos") <- campos_protegidos
  attr(resultado, "resumen") <- resumen
  rownames(resultado) <- NULL
  class(resultado) <- c("equivalencia_perfiles", "data.frame")
  resultado
}
