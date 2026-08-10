.trazabilidad_vacia <- function(estado = "no_disponible",
                                indices = integer(), total = NA_real_,
                                alcance = "no_evaluado", limite = 1000L) {
  indices <- as.integer(indices[!is.na(indices)])
  total <- if (length(total) && is.finite(total)) as.numeric(total) else NA_real_
  truncado <- estado %in% c("disponible", "truncada") &&
    is.finite(total) && length(indices) < total
  if (truncado) estado <- "truncada"
  list(
    estado = estado,
    indices_fila = indices,
    total = total,
    mostrados = length(indices),
    truncado = truncado,
    limite = if (is.infinite(limite)) Inf else as.integer(limite),
    alcance = alcance
  )
}

.trazabilidad_indices <- function(indices, alcance, limite = 1000L) {
  indices <- as.integer(indices[!is.na(indices)])
  total <- length(indices)
  .trazabilidad_vacia(
    estado = if (total) "disponible" else "disponible",
    indices = utils::head(indices, limite),
    total = total, alcance = alcance, limite = limite
  )
}

.nuevo_hallazgo <- function(columna, tipo, severidad, descripcion,
                            evidencia, sugerencia, n_evaluados = NA_real_,
                            n_afectados = NA_real_, unidad_conteo = NA_character_,
                            estado_reparacion = NA_character_) {
  data.frame(
    columna = columna,
    tipo_hallazgo = tipo,
    severidad = severidad,
    descripcion = descripcion,
    evidencia = evidencia,
    sugerencia = sugerencia,
    n_evaluados = as.numeric(n_evaluados),
    n_afectados = as.numeric(n_afectados),
    unidad_conteo = as.character(unidad_conteo),
    estado_reparacion = as.character(estado_reparacion),
    trazabilidad = I(list(.trazabilidad_vacia())),
    stringsAsFactors = FALSE
  )
}

# Las relaciones de orden son una extension del diagnostico, no una metrica:
# encuentran pares de columnas que parecen compartir una relacion aritmetica
# estable para que el usuario pueda declararla despues en el marco de calidad.
.tipo_orden_columna <- function(x, fila) {
  if (inherits(x, "Date")) return("fecha")
  if (inherits(x, "POSIXt")) return("fecha-hora")
  if (inherits(x, "integer64")) return(NA_character_)
  if (is.numeric(x)) return("numero")
  tipo <- as.character(fila$tipo_inferido[[1L]])
  if (tipo %in% c("fecha", "fecha-hora", "entero", "doble")) {
    if (tipo %in% c("entero", "doble")) "numero" else tipo
  } else NA_character_
}

.valores_orden_columna <- function(x, fila, formatos) {
  cuantitativos <- tryCatch(
    .valores_cuantitativos(
      x, list(tipo = as.character(fila$tipo_inferido[[1L]])), formatos
    ),
    error = function(e) NULL
  )
  if (is.null(cuantitativos) ||
      !cuantitativos$clase %in% c("numero", "fecha", "fecha-hora")) {
    return(NULL)
  }
  suppressWarnings(as.numeric(cuantitativos$valores))
}

# Solapamiento relativo de los rangos intercuartilicos. Las columnas ya llegan
# convertidas a una escala numerica comun (incluidas Date y POSIXct). Un rango
# de anchura cero se trata explicitamente: dos constantes solo son comparables
# si tienen el mismo valor, y una constante se solapa si cae dentro del rango
# intercuartil de la otra columna.
.solapamiento_iqr_orden <- function(a, b) {
  a <- a[is.finite(a)]
  b <- b[is.finite(b)]
  if (!length(a) || !length(b)) return(NA_real_)
  qa <- stats::quantile(a, c(0.25, 0.75), na.rm = TRUE,
                        names = FALSE, type = 7)
  qb <- stats::quantile(b, c(0.25, 0.75), na.rm = TRUE,
                        names = FALSE, type = 7)
  if (any(!is.finite(c(qa, qb)))) return(NA_real_)
  anchura_a <- qa[[2L]] - qa[[1L]]
  anchura_b <- qb[[2L]] - qb[[1L]]
  if (anchura_a == 0 && anchura_b == 0) {
    return(as.numeric(qa[[1L]] == qb[[1L]]))
  }
  if (anchura_a == 0) {
    return(as.numeric(qa[[1L]] >= qb[[1L]] && qa[[1L]] <= qb[[2L]]))
  }
  if (anchura_b == 0) {
    return(as.numeric(qb[[1L]] >= qa[[1L]] && qb[[1L]] <= qa[[2L]]))
  }
  ancho_comun <- max(0, min(qa[[2L]], qb[[2L]]) -
    max(qa[[1L]], qb[[1L]]))
  as.numeric(ancho_comun / max(anchura_a, anchura_b))
}

.alcance_orden_columnas <- function(nombres, seleccion, max_columnas,
                                    tipos, n_filas, umbral,
                                    umbral_solapamiento = 0) {
  grupos <- split(seleccion, tipos[seleccion])
  pares <- if (length(grupos)) {
    sum(vapply(grupos, function(x) choose(length(x), 2L), numeric(1L)))
  } else 0
  grupos_analizados <- split(nombres[seleccion], tipos[seleccion])
  pares_analizados <- if (length(grupos_analizados)) {
    sum(vapply(grupos_analizados, function(x) choose(length(x), 2L), numeric(1L)))
  } else 0
  list(
    filas_evaluadas = as.numeric(n_filas),
    columnas_comparables = nombres[which(!is.na(tipos))],
    columnas_analizadas = nombres[seleccion],
    columnas_omitidas = nombres[setdiff(which(!is.na(tipos)), seleccion)],
    pares_comparables = as.numeric(pares),
    pares_analizados = as.numeric(pares_analizados),
    truncado = length(which(!is.na(tipos))) > max_columnas,
    max_columnas = as.integer(max_columnas),
    umbral_cumplimiento = as.numeric(umbral),
    umbral_solapamiento_iqr = as.numeric(umbral_solapamiento),
    pares_descartados_magnitud = 0,
    pares_evaluados_orden = 0,
    minimo_filas = 3L
  )
}

.detectar_orden_columnas <- function(datos, columnas, resultados,
                                     formatos_fecha, umbral = 0.95,
                                     max_columnas = 20L,
                                     umbral_solapamiento = 0) {
  n_columnas <- ncol(datos)
  if (!n_columnas || !nrow(datos)) {
    return(list(
      hallazgos = list(),
      alcance = .alcance_orden_columnas(
        character(), integer(), max_columnas, character(), nrow(datos), umbral,
        umbral_solapamiento
      )
    ))
  }
  tipos <- vapply(seq_len(n_columnas), function(i) {
    .tipo_orden_columna(datos[[i]], columnas[i, , drop = FALSE])
  }, character(1L))
  comparables <- which(!is.na(tipos))
  seleccion <- utils::head(comparables, max_columnas)
  alcance <- .alcance_orden_columnas(
    names(datos), seleccion, max_columnas, tipos, nrow(datos), umbral,
    umbral_solapamiento
  )
  if (length(seleccion) < 2L) return(list(hallazgos = list(), alcance = alcance))

  grupos <- split(seleccion, tipos[seleccion])
  pares <- lapply(grupos, function(indices) {
    if (length(indices) < 2L) return(NULL)
    utils::combn(indices, 2L, simplify = FALSE)
  })
  pares <- unlist(pares, recursive = FALSE)
  if (!length(pares)) return(list(hallazgos = list(), alcance = alcance))

  # La conversion semantica de una columna se hace una sola vez. Sin esta
  # cache, una tabla ancha volveria a parsear cada columna por cada par.
  valores <- lapply(seleccion, function(indice) {
    .valores_orden_columna(
      datos[[indice]], columnas[indice, , drop = FALSE],
      formatos_fecha[[indice]]
    )
  })
  names(valores) <- as.character(seleccion)

  hallazgos <- list()
  for (par in pares) {
    izquierda <- valores[[as.character(par[[1L]])]]
    derecha <- valores[[as.character(par[[2L]])]]
    if (is.null(izquierda) || is.null(derecha) ||
        length(izquierda) != length(derecha)) next
    comparables_fila <- is.finite(izquierda) & is.finite(derecha)
    n_evaluados <- sum(comparables_fila)
    if (n_evaluados < 3L) next
    izquierda <- izquierda[comparables_fila]
    derecha <- derecha[comparables_fila]
    filas <- which(comparables_fila)
    solapamiento <- .solapamiento_iqr_orden(izquierda, derecha)
    if (!is.finite(solapamiento) || solapamiento < umbral_solapamiento) {
      alcance$pares_descartados_magnitud <-
        alcance$pares_descartados_magnitud + 1
      next
    }
    alcance$pares_evaluados_orden <- alcance$pares_evaluados_orden + 1
    cumple_izquierda <- izquierda <= derecha
    cumple_derecha <- derecha <= izquierda
    proporcion_izquierda <- mean(cumple_izquierda)
    proporcion_derecha <- mean(cumple_derecha)
    if (proporcion_izquierda == proporcion_derecha) next
    direccion_izquierda <- proporcion_izquierda > proporcion_derecha
    proporcion <- if (direccion_izquierda) {
      proporcion_izquierda
    } else proporcion_derecha
    # Con pocas filas se permite una sola inversion para no perder el caso
    # minimo (2 de 3); desde 20 comparables se exige el umbral medido de 95 %.
    umbral_efectivo <- if (n_evaluados < 20L) {
      (n_evaluados - 1) / n_evaluados
    } else umbral
    if (proporcion < umbral_efectivo || proporcion >= 1) next
    if (direccion_izquierda) {
      primero <- par[[1L]]
      segundo <- par[[2L]]
      incumple <- !cumple_izquierda
    } else {
      primero <- par[[2L]]
      segundo <- par[[1L]]
      incumple <- !cumple_derecha
    }
    indices_incumplen <- filas[incumple]
    ejemplos <- utils::head(indices_incumplen, 5L)
    evidencia_ejemplos <- if (length(ejemplos)) paste(vapply(
      ejemplos,
      function(fila) paste0(
        "fila ", fila, ": ", names(datos)[[primero]], "=",
        .texto_valor(datos[[primero]][fila]), "; ", names(datos)[[segundo]],
        "=", .texto_valor(datos[[segundo]][fila])
      ), character(1L)
    ), collapse = " | ") else "sin ejemplos"
    nombre_primero <- names(datos)[[primero]]
    nombre_segundo <- names(datos)[[segundo]]
    hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
      paste(nombre_primero, nombre_segundo, sep = ","),
      "relacion_orden_columnas", "sospechoso",
      paste0(
        "La relaci\u00f3n ", nombre_primero, " <= ", nombre_segundo,
        " se rompe en una minor\u00eda de las filas comparables."
      ),
      paste0(
        sprintf("%.3f de cumplimiento; %d de %d filas fuera de orden. ",
                proporcion, length(indices_incumplen), n_evaluados),
        sprintf("Solapamiento intercuartil: %.3f. ", solapamiento),
        evidencia_ejemplos
      ),
      paste0(
        "Formalizar la relaci\u00f3n con ReglaIntegridadIntraEntidad(",
        nombre_primero, ",", nombre_segundo,
        ") y revisar las filas se\u00f1aladas antes de corregirlas."
      ),
      n_evaluados, length(indices_incumplen), "fila"
    )
  }
  list(hallazgos = hallazgos, alcance = alcance)
}

.conteo_hallazgo_columna <- function(tipo, fila, resultado, n_validos) {
  n <- if (length(fila$n) && is.finite(fila$n[[1L]])) {
    as.numeric(fila$n[[1L]])
  } else NA_real_
  afectados <- switch(
    tipo,
    tipo_compuesto_no_analizado = if (!is.null(resultado$estructura_no_analizada)) {
      as.numeric(resultado$estructura_no_analizada$filas)
    } else NA_real_,
    constante = as.numeric(fila$frecuencia_moda),
    faltantes = as.numeric(fila$n_faltantes + fila$n_faltantes_disfrazados),
    faltantes_disfrazados = as.numeric(fila$n_faltantes_disfrazados),
    espacios_sobrantes = as.numeric(fila$n_espacios_borde),
    controles_invisibles = as.numeric(fila$n_controles_invisibles),
    entidades_html = as.numeric(fila$n_entidades_html),
    saltos_linea = as.numeric(fila$n_saltos_linea),
    mayusculas_inconsistentes = as.numeric(fila$n_variantes_mayusculas),
    normalizacion_unicode = as.numeric(fila$n_variantes_unicode),
    codificacion_invalida = as.numeric(fila$n_codificacion_invalida),
    codificacion_rota = as.numeric(fila$n_codificacion_rota),
    valores_no_finitos = as.numeric(
      fila$n_nan + fila$n_infinito_positivo + fila$n_infinito_negativo
    ),
    ceros_no_permitidos = as.numeric(fila$n_ceros),
    negativos_no_permitidos = as.numeric(fila$n_negativos),
    outliers = as.numeric(fila$n_outliers),
    numero_como_texto = as.numeric(fila$n_numeros_texto),
    patron_raro = {
      resumen <- attr(resultado$patrones, "resumen_patrones")
      distintos <- attr(resultado$patrones, "n_patrones_distintos")
      completo <- !is.null(resumen) && (
        is.null(distintos) || !is.finite(distintos) ||
          distintos <= nrow(resumen)
      )
      if (completo && !is.null(resumen) && nrow(resumen) > 1L) {
        sum(resumen$n[-1L], na.rm = TRUE)
      } else NA_real_
    },
    NA_real_
  )
  unidad <- if (tipo %in% c(
    "formato_fecha_ambiguo", "anio_de_dos_digitos", "formatos_fecha_mixtos"
  )) {
    "formato"
  } else if (tipo %in% c(
    "posible_identificador", "alta_cardinalidad", "tipo_declarado_distinto",
    "integer64_fuera_precision_double", "integer64_sin_soporte"
  )) {
    "columna"
  } else "fila"
  if (unidad == "formato") {
    n <- if (!is.null(resultado$formatos)) nrow(resultado$formatos) else NA_real_
    afectados <- if (tipo == "formato_fecha_ambiguo") {
      sum(resultado$formatos$estado == "candidato")
    } else if (tipo == "anio_de_dos_digitos") {
      sum(resultado$formatos$anio_dos_digitos)
    } else {
      nrow(resultado$formatos)
    }
  } else if (unidad == "columna") {
    n <- 1
    afectados <- 1
  } else if (tipo == "patron_raro") {
    analizados <- attr(resultado$patrones, "analizados", exact = TRUE)
    if (!is.null(analizados) && is.finite(analizados)) {
      n <- as.numeric(analizados)
    }
  }
  list(n_evaluados = n, n_afectados = afectados, unidad_conteo = unidad)
}

.indices_patron_raro <- function(x, resultado, expandir = FALSE,
                                 distinguir_mayusculas = TRUE) {
  resumen <- attr(resultado$patrones, "resumen_patrones")
  if (is.null(resumen) || nrow(resumen) < 2L) return(NULL)
  distintos <- attr(resultado$patrones, "n_patrones_distintos")
  if (!is.null(distintos) && is.finite(distintos) &&
      distintos > nrow(resumen)) return(NULL)
  total <- length(x)
  analizados <- attr(resultado$patrones, "analizados")
  muestreado <- isTRUE(attr(resultado$patrones, "muestreado"))
  base <- if (muestreado && is.finite(analizados)) {
    unique(as.integer(round(seq.int(1, total, length.out = analizados))))
  } else {
    seq_len(total)
  }
  valores <- tryCatch(.texto_analizable(x[base])$valores,
                      error = function(e) NULL)
  if (is.null(valores)) return(NULL)
  patrones <- as.character(valores)
  validos <- !is.na(patrones)
  patrones[validos] <- gsub("[[:digit:]]", "9", patrones[validos], perl = TRUE)
  if (isTRUE(distinguir_mayusculas)) {
    patrones[validos] <- gsub("[[:lower:]]", "a", patrones[validos], perl = TRUE)
    patrones[validos] <- gsub("[[:upper:]]", "A", patrones[validos], perl = TRUE)
  } else {
    patrones[validos] <- gsub("[[:alpha:]]", "a", patrones[validos], perl = TRUE)
  }
  if (!isTRUE(expandir)) {
    patrones[validos] <- gsub("9{2,}", "9+", patrones[validos], perl = TRUE)
    patrones[validos] <- gsub("a{2,}", "a+", patrones[validos], perl = TRUE)
    patrones[validos] <- gsub("A{2,}", "A+", patrones[validos], perl = TRUE)
  }
  raros <- unique(as.character(resumen$patron[-1L]))
  base[which(!is.na(patrones) & patrones %in% raros)]
}

.indices_hallazgo_columna <- function(tipo, x, fila, resultado,
                                      expandir = FALSE,
                                      distinguir_mayusculas = TRUE) {
  if (is.matrix(x) || is.list(x)) return(NULL)
  n <- length(x)
  if (!n) return(integer())
  texto <- tryCatch(.texto_analizable(x)$valores, error = function(e) NULL)
  idx <- switch(
    tipo,
    tipo_compuesto_no_analizado = seq_len(n),
    constante = tryCatch(
      which(!is.na(x) & as.character(x) == as.character(fila$moda[[1L]])),
      error = function(e) NULL
    ),
    faltantes = {
      mascara <- tryCatch(resultado$faltantes_disfrazados$mascara,
                          error = function(e) NULL)
      if (is.null(mascara) || length(mascara) != n) NULL else
        which(is.na(x) | mascara)
    },
    faltantes_disfrazados = {
      mascara <- tryCatch(resultado$faltantes_disfrazados$mascara,
                          error = function(e) NULL)
      if (is.null(mascara) || length(mascara) != n) NULL else which(mascara)
    },
    espacios_sobrantes = if (is.null(texto)) NULL else
      which(!is.na(texto) & texto != trimws(texto)),
    controles_invisibles = if (is.null(texto)) NULL else
      which(.tiene_control_invisible(texto)),
    entidades_html = if (is.null(texto)) NULL else
      which(.entidades_html_en_texto(texto)),
    saltos_linea = if (is.null(texto)) NULL else
      which(.tiene_salto_linea(texto)),
    mayusculas_inconsistentes = if (is.null(texto)) NULL else {
      presentes <- !is.na(texto)
      canon <- tolower(texto)
      grupos <- split(seq_len(n)[presentes], canon[presentes])
      unlist(lapply(grupos, function(g) {
        if (length(unique(texto[g])) > 1L) g else integer()
      }), use.names = FALSE)
    },
    normalizacion_unicode = if (is.null(texto) ||
      !requireNamespace("stringi", quietly = TRUE)) NULL else {
        presentes <- !is.na(texto)
        normal <- stringi::stri_trans_nfc(texto[presentes])
        grupos <- split(which(presentes), normal)
        unlist(lapply(grupos, function(g) {
          if (length(unique(texto[g])) > 1L) g else integer()
        }), use.names = FALSE)
      },
    codificacion_invalida = tryCatch(
      .texto_analizable(x)$posiciones, error = function(e) NULL
    ),
    codificacion_rota = if (is.null(texto)) NULL else {
      cod <- tryCatch(.analizar_codificacion_vocabulario(texto), error = function(e) NULL)
      if (is.null(cod)) NULL else {
        candidatos <- grepl("[\u00c3\u00c2\u00e2\u00f0\ufffd]", texto,
                             perl = TRUE)
        which(candidatos & (!is.na(cod$reparados) |
          grepl("\ufffd", texto, fixed = TRUE)))
      }
    },
    numero_como_texto = if (is.null(texto)) NULL else {
      partes <- tryCatch(.componentes_numero_texto_optimizado(texto),
                         error = function(e) NULL)
      if (is.null(partes)) NULL else which(
        !is.na(texto) & nzchar(texto) & partes$compatible & partes$especial
      )
    },
    valores_no_finitos = if (is.numeric(x)) which(is.nan(x) | !is.finite(x)) else NULL,
    ceros_no_permitidos = if (is.numeric(x)) which(!is.na(x) & x == 0) else NULL,
    negativos_no_permitidos = if (is.numeric(x)) which(!is.na(x) & x < 0) else NULL,
    outliers = if (is.numeric(x)) tryCatch({
      valores <- x[is.finite(x)]
      if (length(valores) < 4L) integer() else {
        q <- stats::quantile(valores, c(0.25, 0.75), names = FALSE)
        iqr <- q[[2L]] - q[[1L]]
        which(x < q[[1L]] - 1.5 * iqr | x > q[[2L]] + 1.5 * iqr)
      }
    }, error = function(e) NULL) else NULL,
    patron_raro = .indices_patron_raro(
      x, resultado, expandir, distinguir_mayusculas
    ),
    NULL
  )
  if (is.null(idx)) return(NULL)
  as.integer(idx)
}

.trazabilidad_hallazgo <- function(tipo, hallazgo, datos, nombres,
                                   resultados, expandir, aproximados,
                                   indice_hallazgo, tipos_hallazgos, limite,
                                   distinguir_mayusculas = TRUE) {
  unidad <- as.character(hallazgo$unidad_conteo[[1L]])
  if (unidad %in% c("columna", "formato")) {
    return(.trazabilidad_vacia("no_aplica", alcance = "no_aplica", limite = limite))
  }
  if (tipo %in% c("duplicados_aproximados", "duplicados_exactos_columnas")) {
    if (is.null(aproximados) || !nrow(aproximados$pares)) {
      return(.trazabilidad_vacia(limite = limite))
    }
    anteriores <- seq_len(indice_hallazgo - 1L)
    ocurrencia <- 1L + sum(
      tipos_hallazgos[anteriores] == tipo
    )
    tipo_par <- if (tipo == "duplicados_exactos_columnas") "exacto" else "aproximado"
    indices_pares <- which(aproximados$pares$tipo_par == tipo_par)
    if (ocurrencia > length(indices_pares)) {
      return(.trazabilidad_vacia(limite = limite))
    }
    pares <- aproximados$pares[indices_pares[[ocurrencia]], , drop = FALSE]
    parcial <- aproximados$alcance$n_pares_comparados[[1L]] <
      aproximados$alcance$n_pares_posibles[[1L]]
    return(.trazabilidad_indices(
      c(pares$fila_1[[1L]], pares$fila_2[[1L]]),
      if (parcial) "comparacion_parcial" else "comparacion_completa", limite
    ))
  }
  if (tipo == "filas_duplicadas") {
    indices <- which(duplicated(datos) | duplicated(datos, fromLast = TRUE))
    return(.trazabilidad_indices(indices, "completo", limite))
  }
  if (tipo == "relacion_orden_columnas") {
    nombres_par <- strsplit(
      as.character(hallazgo$columna[[1L]]), ",", fixed = TRUE
    )[[1L]]
    if (length(nombres_par) != 2L) {
      return(.trazabilidad_vacia(limite = limite))
    }
    indices_columnas <- match(trimws(nombres_par), nombres)
    if (anyNA(indices_columnas)) {
      return(.trazabilidad_vacia(limite = limite))
    }
    izquierda <- tryCatch(
      .valores_orden_columna(
        datos[[indices_columnas[[1L]]]],
        resultados[[indices_columnas[[1L]]]]$fila,
        resultados[[indices_columnas[[1L]]]]$formatos
      ),
      error = function(e) NULL
    )
    derecha <- tryCatch(
      .valores_orden_columna(
        datos[[indices_columnas[[2L]]]],
        resultados[[indices_columnas[[2L]]]]$fila,
        resultados[[indices_columnas[[2L]]]]$formatos
      ),
      error = function(e) NULL
    )
    if (is.null(izquierda) || is.null(derecha) ||
        length(izquierda) != length(derecha)) {
      return(.trazabilidad_vacia(limite = limite))
    }
    comparables <- is.finite(izquierda) & is.finite(derecha)
    indices <- which(comparables & izquierda > derecha)
    return(.trazabilidad_indices(indices, "completo", limite))
  }
  if (tipo == "bloqueo_por_con_perdida") {
    return(.trazabilidad_vacia(
      estado = "no_disponible",
      total = hallazgo$n_afectados[[1L]],
      alcance = "bloqueo_fuera_de_alcance", limite = limite
    ))
  }
  nombre <- as.character(hallazgo$columna[[1L]])
  indice <- match(nombre, nombres)
  if (is.na(indice)) return(.trazabilidad_vacia(limite = limite))
  indices <- .indices_hallazgo_columna(
    tipo, datos[[indice]], resultados[[indice]]$fila,
    resultados[[indice]], expandir, distinguir_mayusculas
  )
  if (is.null(indices)) {
    return(.trazabilidad_vacia(limite = limite))
  }
  alcance <- if (tipo == "patron_raro" &&
                 isTRUE(attr(resultados[[indice]]$patrones,
                             "muestreado", exact = TRUE))) {
    "muestra_patrones"
  } else {
    "completo"
  }
  .trazabilidad_indices(indices, alcance, limite)
}

.agregar_trazabilidad_hallazgos <- function(
    hallazgos, datos, nombres, resultados, expandir = FALSE,
    aproximados = NULL, limite = 1000L, distinguir_mayusculas = TRUE) {
  if (!nrow(hallazgos)) {
    hallazgos$trazabilidad <- I(list())
    return(hallazgos)
  }
  tipos_hallazgos <- as.character(hallazgos$tipo_hallazgo)
  trazabilidad <- lapply(seq_len(nrow(hallazgos)), function(i) {
    .trazabilidad_hallazgo(
      as.character(hallazgos$tipo_hallazgo[[i]]), hallazgos[i, , drop = FALSE],
      datos, nombres, resultados, expandir, aproximados, i,
      tipos_hallazgos, limite, distinguir_mayusculas
    )
  })
  hallazgos$trazabilidad <- I(trazabilidad)
  hallazgos
}

.tipos_equivalentes <- function(declarado, inferido) {
  if (identical(declarado, inferido)) {
    return(TRUE)
  }
  declarado %in% c("factor", "factor-ordenado", "texto") &&
    inferido %in% c("texto", "identificador", "desconocido")
}

.hallazgos_columnas <- function(resultados, columnas,
                                umbral_alta_cardinalidad,
                                umbral_faltantes_sospechoso,
                                umbral_faltantes_error,
                                umbral_patron_raro,
                                umbral_patron_dominante,
                                columnas_sin_ceros,
                                columnas_no_negativas) {
  hallazgos <- list()
  k <- 0L
  agregar <- function(x) {
    conteo <- .conteo_hallazgo_columna(
      as.character(x$tipo_hallazgo[[1L]]), fila, resultado, n_validos
    )
    x$n_evaluados <- conteo$n_evaluados
    x$n_afectados <- conteo$n_afectados
    x$unidad_conteo <- conteo$unidad_conteo
    k <<- k + 1L
    hallazgos[[k]] <<- x
  }

  for (i in seq_along(resultados)) {
    resultado <- resultados[[i]]
    fila <- resultado$fila
    nombre <- columnas[[i]]
    n_invalidos <- if ("n_codificacion_invalida" %in% names(fila) &&
        is.finite(fila$n_codificacion_invalida)) {
      fila$n_codificacion_invalida
    } else {
      0L
    }
    n_validos <- fila$n - fila$n_faltantes - n_invalidos

    if (identical(fila$estado_resumen_cuantitativo, "tipo_compuesto_no_analizado")) {
      estructura <- resultado$estructura_no_analizada
      agregar(.nuevo_hallazgo(
        nombre, "tipo_compuesto_no_analizado", "sospechoso",
        paste0(
          "La columna contiene una matriz por fila; no se aplan\u00f3 porque mezclar ",
          "sus componentes inventar\u00eda una sem\u00e1ntica de celda."
        ),
        paste0(
          estructura$filas, " filas y ", estructura$componentes,
          " componentes por fila; m\u00e9tricas por valor en NA."
        ),
        "Separar los componentes en columnas con significado expl\u00edcito antes de perfilarlos."
      ))
    }

    if (!is.na(fila$n_distintos) && fila$n_distintos == 1L &&
        isTRUE(n_validos > 1L)) {
      agregar(.nuevo_hallazgo(
        nombre, "constante", "sospechoso",
        "La columna contiene un \u00fanico valor no ausente.",
        paste0("Valor: ", fila$moda, "; frecuencia: ", fila$frecuencia_moda),
        "Confirmar si la columna aporta informaci\u00f3n o si corresponde retirarla."
      ))
    }

    if (isTRUE(n_validos > 1L) && fila$tipo_inferido == "identificador" &&
        is.finite(fila$tasa_distintos) && fila$tasa_distintos >= 0.9) {
      agregar(.nuevo_hallazgo(
        nombre, "posible_identificador", "ok",
        "La forma y la alta unicidad son compatibles con un identificador.",
        paste0(
          fila$n_distintos, " valores distintos de ", n_validos,
          " (", sprintf("%.3f", fila$tasa_distintos), ")"
        ),
        "Validar con el diccionario de datos si corresponde declarar una clave."
      ))
    } else if (
      fila$tipo_declarado %in% c("texto", "factor", "factor-ordenado") &&
        is.finite(fila$tasa_distintos) &&
        fila$tasa_distintos > umbral_alta_cardinalidad &&
        fila$n_distintos > 1L
    ) {
      agregar(.nuevo_hallazgo(
        nombre, "alta_cardinalidad", "sospechoso",
        "La columna categ\u00f3rica tiene alta cardinalidad.",
        sprintf("Tasa de valores distintos: %.3f", fila$tasa_distintos),
        "Revisar si es texto libre, un identificador o una categor\u00eda mal normalizada."
      ))
    }

    if (is.finite(fila$prop_faltantes_totales) &&
        fila$prop_faltantes_totales > umbral_faltantes_sospechoso) {
      severidad <- if (fila$prop_faltantes_totales > umbral_faltantes_error) {
        "error"
      } else {
        "sospechoso"
      }
      if (severidad == "error" && fila$n_faltantes == 0L &&
          resultado$faltantes_disfrazados$n_textuales == 0L) {
        severidad <- "sospechoso"
      }
      agregar(.nuevo_hallazgo(
        nombre, "faltantes", severidad,
        "La proporci\u00f3n total de faltantes supera el umbral configurado.",
        sprintf(
          "%d ausentes reales y %d disfrazados (%.3f del total)",
          fila$n_faltantes, fila$n_faltantes_disfrazados,
          fila$prop_faltantes_totales
        ),
        "Revisar la obligatoriedad del campo y el proceso que origina los faltantes."
      ))
    }

    if (isTRUE(fila$n_faltantes_disfrazados > 0L)) {
      solo_numericos <- resultado$faltantes_disfrazados$n_textuales == 0L
      agregar(.nuevo_hallazgo(
        nombre, "faltantes_disfrazados",
        if (solo_numericos) "sospechoso" else "error",
        "Hay valores que representan ausencia sin estar codificados como NA.",
        resultado$faltantes_disfrazados$evidencia,
        if (solo_numericos) {
          "Confirmar que los sentinelas num\u00e9ricos representan ausencia antes de normalizarlos."
        } else {
          "Normalizar estas representaciones a NA conservando su significado si fuera necesario."
        }
      ))
    }

    candidatos_fecha <- resultado$formatos[
      resultado$formatos$estado == "candidato" &
        (!resultado$formatos$anio_dos_digitos |
           resultado$formatos$n_inequivocos == 0L), , drop = FALSE
    ]
    if (nrow(candidatos_fecha) &&
        fila$tipo_inferido %in% c("fecha", "fecha-hora")) {
      agregar(.nuevo_hallazgo(
        nombre, "formato_fecha_ambiguo", "sospechoso",
        paste0(
          "La columna es compatible con fecha, pero no permite distinguir ",
          "d\u00eda/mes de mes/d\u00eda; el rango temporal no se calcula."
        ),
        paste(candidatos_fecha$formato, collapse = " o "),
        "Desambiguar el formato con el origen de los datos antes de convertir la columna."
      ))
    }

    formatos_anio_corto <- resultado$formatos[
      resultado$formatos$anio_dos_digitos, , drop = FALSE
    ]
    if (nrow(formatos_anio_corto) &&
        fila$tipo_inferido %in% c("fecha", "fecha-hora")) {
      agregar(.nuevo_hallazgo(
        nombre, "anio_de_dos_digitos", "sospechoso",
        paste0(
          "La fecha expresa el a\u00f1o con dos d\u00edgitos y no permite decidir ",
          "el siglo sin conocimiento del dominio; el rango temporal no se calcula."
        ),
        paste(unique(formatos_anio_corto$formato), collapse = " o "),
        "Confirmar el siglo con el origen antes de convertir a una fecha completa."
      ))
    }

    if (isTRUE(attr(resultado$formatos, "formatos_mixtos")) &&
        fila$tipo_inferido %in% c("fecha", "fecha-hora")) {
      evidencia <- paste0(
        resultado$formatos$formato, " (", resultado$formatos$n, ")",
        collapse = "; "
      )
      agregar(.nuevo_hallazgo(
        nombre, "formatos_fecha_mixtos", "error",
        "Conviven dos o m\u00e1s formatos de fecha o fecha-hora.",
        evidencia,
        "Estandarizar la columna a un \u00fanico formato antes de convertirla a fecha."
      ))
    }

    if (!.tipos_equivalentes(fila$tipo_declarado, fila$tipo_inferido) &&
        fila$tipo_inferido != "desconocido") {
      agregar(.nuevo_hallazgo(
        nombre, "tipo_declarado_distinto", "sospechoso",
        "El tipo declarado no coincide con el tipo impl\u00edcito dominante.",
        sprintf(
          "Declarado: %s; inferido: %s (%.3f compatible)",
          fila$tipo_declarado, fila$tipo_inferido,
          fila$proporcion_tipo_inferido
        ),
        "Confirmar el tipo esperado y convertir la columna de forma expl\u00edcita."
      ))
    }

    patrones <- attr(resultado$patrones, "resumen_patrones")
    if (!is.null(patrones) && nrow(patrones) > 1L &&
        patrones$proporcion[[1L]] >= umbral_patron_dominante) {
      raros <- patrones[-1L, , drop = FALSE]
      raros <- raros[raros$proporcion < umbral_patron_raro, , drop = FALSE]
      if (nrow(raros)) {
        evidencia <- paste0(
          raros$patron, " [", raros$ejemplos, "]",
          collapse = "; "
        )
        agregar(.nuevo_hallazgo(
          nombre, "patron_raro", "sospechoso",
          "Hay valores infrecuentes que no siguen el patr\u00f3n dominante.",
          paste0(
            "Dominante: ", patrones$patron[[1L]], ". Desv\u00edos: ",
            paste(utils::head(strsplit(evidencia, "; ", fixed = TRUE)[[1L]], 6L),
                  collapse = "; ")
          ),
          "Revisar los valores concretos y validar el formato esperado."
        ))
      }
    }

    if (isTRUE(fila$n_espacios_borde > 0L)) {
      agregar(.nuevo_hallazgo(
        nombre, "espacios_sobrantes", "sospechoso",
        "Hay texto con espacios sobrantes al inicio o al final.",
        paste0(
          fila$n_espacios_borde, " valores; ejemplos: ",
          resultado$diagnostico_texto$evidencia_espacios
        ),
        "Aplicar trimws() despu\u00e9s de confirmar que los espacios no son significativos."
      ))
    }
    if (isTRUE(fila$n_controles_invisibles > 0L)) {
      agregar(.nuevo_hallazgo(
        nombre, "controles_invisibles", "error",
        "Hay caracteres de control o invisibles Unicode dentro de los valores.",
        resultado$diagnostico_texto$evidencia_controles_invisibles,
        "Eliminar los controles invisibles despu\u00e9s de confirmar que no forman parte de un protocolo de transporte."
      ))
    }
    if (isTRUE(fila$n_entidades_html > 0L)) {
      agregar(.nuevo_hallazgo(
        nombre, "entidades_html", "sospechoso",
        "Hay entidades HTML sin decodificar dentro de los valores.",
        resultado$diagnostico_texto$evidencia_entidades_html,
        "Decodificar las entidades HTML s\u00f3lo despu\u00e9s de confirmar que la columna proviene de una fuente escapada."
      ))
    }
    if (isTRUE(fila$n_saltos_linea > 0L)) {
      agregar(.nuevo_hallazgo(
        nombre, "saltos_linea", "sospechoso",
        "Hay saltos de l\u00ednea dentro de los valores de texto.",
        resultado$diagnostico_texto$evidencia_saltos_linea,
        "Reemplazar los saltos por espacios s\u00f3lo despu\u00e9s de confirmar que no son parte del contenido leg\u00edtimo."
      ))
    }
    if (isTRUE(fila$n_variantes_mayusculas > 0L)) {
      agregar(.nuevo_hallazgo(
        nombre, "mayusculas_inconsistentes", "sospechoso",
        "Conviven valores que s\u00f3lo se diferencian por may\u00fasculas y min\u00fasculas.",
        resultado$diagnostico_texto$evidencia_mayusculas,
        "Definir y aplicar una convenci\u00f3n de capitalizaci\u00f3n para la columna."
      ))
    }
    if (!is.na(fila$n_variantes_unicode) && fila$n_variantes_unicode > 0L) {
      agregar(.nuevo_hallazgo(
        nombre, "normalizacion_unicode", "sospechoso",
        paste0(
          "Conviven textos visualmente equivalentes con representaciones ",
          "Unicode NFC y NFD distintas."
        ),
        resultado$diagnostico_texto$evidencia_unicode,
        paste0(
          "Confirmar el dominio y normalizar a NFC sin alterar el contenido ",
          "visible del texto."
        )
      ))
    }
    if (isTRUE(fila$n_codificacion_invalida > 0L)) {
      agregar(.nuevo_hallazgo(
        nombre, "codificacion_invalida", "error",
        paste0(
          "Hay texto con secuencias de bytes que no forman UTF-8 v\u00e1lido; ",
          "esos valores se excluyeron de los an\u00e1lisis textuales."
        ),
        resultado$diagnostico_texto$evidencia_codificacion_invalida,
        paste0(
          "Volver a leer la fuente declarando su codificaci\u00f3n original; no ",
          "reinterpretar ni reemplazar los bytes sin ese dato."
        )
      ))
    }
    if (isTRUE(fila$n_codificacion_rota > 0L)) {
      reparables_total <- fila$n_codificacion_reparable[[1L]] +
        if ("n_codificacion_reparable_parcialmente" %in% names(fila)) {
          fila$n_codificacion_reparable_parcialmente[[1L]]
        } else 0
      agregar(.nuevo_hallazgo(
        nombre, "codificacion_rota", "error",
        "Hay texto con se\u00f1ales de una conversi\u00f3n de codificaci\u00f3n incorrecta.",
        resultado$diagnostico_texto$evidencia_codificacion,
        if (isTRUE(reparables_total > 0L)) {
          paste0(
            "Reparar los valores cuyo viaje por las codificaciones conocidas ",
            "cierra en texto UTF-8; revisar manualmente los caracteres perdidos."
          )
        } else {
          "Recuperar el dato desde la fuente: el car\u00e1cter original ya no est\u00e1 disponible."
        },
        estado_reparacion = fila$estado_codificacion_reparacion[[1L]]
      ))
    }
    if (isTRUE(fila$n_numeros_texto > 0L) &&
        is.finite(fila$proporcion_numeros_texto) &&
        fila$proporcion_numeros_texto >= 0.8) {
      agregar(.nuevo_hallazgo(
        nombre, "numero_como_texto", "sospechoso",
        if (isTRUE(fila$numero_texto_ambiguo)) {
          paste0(
            "La columna parece num\u00e9rica, pero un separador seguido por tres ",
            "d\u00edgitos puede representar decimales o miles sin evidencia adicional."
          )
        } else {
          paste0(
            "La columna contiene n\u00fameros escritos con una convenci\u00f3n decimal, ",
            "unidad o moneda identificable."
          )
        },
        resultado$numeros_texto$evidencia,
        if (isTRUE(fila$numero_texto_seguro)) {
          "Convertir de forma expl\u00edcita conservando en la bit\u00e1cora la convenci\u00f3n y la unidad."
        } else {
          "Confirmar la convenci\u00f3n decimal y la unidad antes de convertir."
        }
      ))
    }

    if (identical(fila$estado_resumen_cuantitativo, "omitidos_precision")) {
      agregar(.nuevo_hallazgo(
        nombre, "integer64_fuera_precision_double", "sospechoso",
        paste0(
          "La columna integer64 excede el rango de enteros exactamente ",
          "representables por double; se omiten estad\u00edsticos aproximados."
        ),
        paste0(
          "M\u00ednimo exacto: ", fila$minimo_exacto,
          "; m\u00e1ximo exacto: ", fila$maximo_exacto
        ),
        "Conservar la clase integer64 y usar los extremos exactos informados."
      ))
    } else if (identical(fila$estado_resumen_cuantitativo, "requiere_bit64")) {
      agregar(.nuevo_hallazgo(
        nombre, "integer64_sin_soporte", "sospechoso",
        "La clase integer64 no se resumi\u00f3 porque falta su soporte opcional.",
        "Los estad\u00edsticos cuantitativos se dejaron en NA.",
        "Instalar el paquete 'bit64' para calcular extremos exactos."
      ))
    }

    n_inf <- fila$n_infinito_positivo + fila$n_infinito_negativo
    if (isTRUE(n_inf > 0L) || isTRUE(fila$n_nan > 0L)) {
      agregar(.nuevo_hallazgo(
        nombre, "valores_no_finitos", "error",
        paste0(
          "La columna contiene NaN o infinitos; los estad\u00edsticos de rango ",
          "describen s\u00f3lo los valores finitos."
        ),
        paste0(
          "NaN: ", fila$n_nan, "; +Inf: ", fila$n_infinito_positivo,
          "; -Inf: ", fila$n_infinito_negativo
        ),
        "Revisar operaciones indefinidas o divisiones por cero en el origen."
      ))
    }

    if (nombre %in% columnas_sin_ceros &&
        !is.na(fila$n_ceros) && fila$n_ceros > 0L) {
      agregar(.nuevo_hallazgo(
        nombre, "ceros_no_permitidos", "sospechoso",
        "La columna contiene ceros aunque se configur\u00f3 como no admisible.",
        paste(fila$n_ceros, "ceros"),
        "Verificar si los ceros son v\u00e1lidos o representan otro estado."
      ))
    }
    if (nombre %in% columnas_no_negativas &&
        !is.na(fila$n_negativos) && fila$n_negativos > 0L) {
      agregar(.nuevo_hallazgo(
        nombre, "negativos_no_permitidos", "sospechoso",
        "La columna contiene valores negativos aunque se configur\u00f3 como no negativa.",
        paste(fila$n_negativos, "valores negativos"),
        "Corregir los valores o ajustar la restricci\u00f3n del dominio."
      ))
    }
    if (!is.na(fila$n_outliers) && fila$n_outliers > 0L) {
      agregar(.nuevo_hallazgo(
        nombre, "outliers", "sospechoso",
        "Se detectaron valores fuera de los l\u00edmites de Tukey (1,5 x IQR).",
        paste(fila$n_outliers, "valores"),
        "Examinar los valores extremos antes de decidir si son errores."
      ))
    }
  }
  hallazgos
}

.nombres_columnas_problematicos <- function(nombres) {
  if (!length(nombres)) {
    return(data.frame(
      original = character(), propuesto = character(), stringsAsFactors = FALSE
    ))
  }
  originales <- as.character(nombres)
  originales[is.na(originales)] <- ""
  propuestos <- make.names(originales, unique = TRUE)
  problema <- originales != propuestos | originales != trimws(originales) |
    duplicated(originales) | duplicated(originales, fromLast = TRUE)
  data.frame(
    original = originales[problema],
    propuesto = propuestos[problema],
    stringsAsFactors = FALSE
  )
}

.normalizar_nombre_fecha <- function(x) {
  y <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  y[is.na(y)] <- x[is.na(y)]
  tolower(gsub("[^[:alnum:]]+", "_", y, perl = TRUE))
}

.detectar_fecha_partida <- function(datos, nombres) {
  if (ncol(datos) < 3L || !nrow(datos)) return(character())
  normalizados <- .normalizar_nombre_fecha(nombres)
  roles <- list(
    anio = "(?:anio|ano|year)", mes = "(?:mes|month)", dia = "(?:dia|day)"
  )
  indices <- lapply(roles, function(patron) {
    which(grepl(paste0("(^|_)", patron, "($|_)"), normalizados, perl = TRUE))
  })
  if (any(lengths(indices) == 0L)) return(character())
  candidatos <- expand.grid(indices, KEEP.OUT.ATTRS = FALSE,
                            stringsAsFactors = FALSE)
  names(candidatos) <- names(roles)
  candidatos <- candidatos[
    apply(candidatos, 1L, function(x) length(unique(x)) == 3L), , drop = FALSE
  ]
  hallados <- character()
  for (i in seq_len(nrow(candidatos))) {
    posicion <- unlist(candidatos[i, ], use.names = FALSE)
    valores <- lapply(posicion, function(j) {
      suppressWarnings(as.integer(trimws(as.character(datos[[j]]))))
    })
    names(valores) <- c("anio", "mes", "dia")
    presentes <- Reduce(`&`, lapply(valores, Negate(is.na)))
    if (sum(presentes) < 3L) next
    en_rango <- valores$anio >= 1800L & valores$anio <= 2100L &
      valores$mes >= 1L & valores$mes <= 12L &
      valores$dia >= 1L & valores$dia <= 31L
    fechas <- suppressWarnings(as.Date(sprintf(
      "%04d-%02d-%02d", valores$anio, valores$mes, valores$dia
    )))
    proporcion <- mean(en_rango[presentes] & !is.na(fechas[presentes]))
    if (is.finite(proporcion) && proporcion >= 0.8) {
      hallados <- c(hallados, paste0(
        nombres[[posicion[[3L]]]], " + ", nombres[[posicion[[2L]]]], " + ",
        nombres[[posicion[[1L]]]], " (", sprintf("%.3f", proporcion), " v\u00e1lidas)"
      ))
    }
  }
  unique(hallados)
}

.columnas_duplicadas <- function(datos, nombres) {
  if (ncol(datos) < 2L) {
    return(data.frame(
      columna_1 = character(), columna_2 = character(), stringsAsFactors = FALSE
    ))
  }
  pares <- utils::combn(seq_len(ncol(datos)), 2L)
  iguales <- apply(pares, 2L, function(indice) {
    x <- datos[[indice[[1L]]]]
    y <- datos[[indice[[2L]]]]
    .columnas_identicas(x, y)
  })
  pares <- pares[, iguales, drop = FALSE]
  if (!ncol(pares)) {
    return(data.frame(
      columna_1 = character(), columna_2 = character(), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    columna_1 = nombres[pares[1L, ]],
    columna_2 = nombres[pares[2L, ]],
    stringsAsFactors = FALSE
  )
}

.construir_hallazgos <- function(datos, resultados, columnas, duplicadas,
                                 umbral_alta_cardinalidad,
                                 umbral_faltantes_sospechoso,
                                 umbral_faltantes_error,
                                 umbral_patron_raro,
                                 umbral_patron_dominante,
                                 columnas_sin_ceros,
                                 columnas_no_negativas,
                                 n_filas_duplicadas,
                                 relaciones_orden = list()) {
  hallazgos <- .hallazgos_columnas(
    resultados, columnas, umbral_alta_cardinalidad,
    umbral_faltantes_sospechoso, umbral_faltantes_error,
    umbral_patron_raro, umbral_patron_dominante,
    columnas_sin_ceros, columnas_no_negativas
  )
  if (n_filas_duplicadas > 0L) {
    hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
      NA_character_, "filas_duplicadas", "error",
      "La tabla contiene filas duplicadas exactas.",
      paste(n_filas_duplicadas, "filas duplicadas"),
      "Definir una clave y revisar la causa antes de eliminar duplicados.",
      nrow(datos), n_filas_duplicadas, "fila"
    )
  }
  if (nrow(duplicadas)) {
    for (i in seq_len(nrow(duplicadas))) {
      hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
        duplicadas$columna_1[[i]], "columnas_duplicadas", "sospechoso",
        "Dos columnas tienen el mismo contenido.",
        paste(duplicadas$columna_1[[i]], "=", duplicadas$columna_2[[i]]),
        "Confirmar si ambas columnas son necesarias o si existe redundancia.",
        ncol(datos), 2, "columna"
      )
    }
  }
  nombres_problematicos <- .nombres_columnas_problematicos(columnas)
  if (nrow(nombres_problematicos)) {
    evidencia <- paste0(
      encodeString(nombres_problematicos$original, quote = '"'),
      " -> ",
      encodeString(nombres_problematicos$propuesto, quote = '"'),
      collapse = "; "
    )
    hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
      NA_character_, "nombres_columnas_problematicos", "sospechoso",
      "La tabla contiene nombres de columna no sint\u00e1cticos o duplicados.",
      evidencia,
      "Renombrar las columnas con nombres sint\u00e1cticos, \u00fanicos y sin espacios al borde.",
      ncol(datos), nrow(nombres_problematicos), "columna"
    )
  }
  fechas_partidas <- .detectar_fecha_partida(datos, columnas)
  if (length(fechas_partidas)) {
    columnas_partidas <- unique(trimws(unlist(strsplit(
      sub("\\s*\\(.*$", "", fechas_partidas), "\\+"
    ))))
    hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
      NA_character_, "fecha_partida_columnas", "sospechoso",
      "La tabla parece representar una fecha mediante columnas separadas de a\u00f1o, mes y d\u00eda.",
      paste(fechas_partidas, collapse = "; "),
      "Confirmar la sem\u00e1ntica y construir una fecha expl\u00edcita sin descartar las columnas de origen.",
      ncol(datos), length(columnas_partidas), "columna"
    )
  }
  if (length(relaciones_orden)) {
    hallazgos <- c(hallazgos, relaciones_orden)
  }

  if (length(hallazgos)) {
    resultado <- do.call(rbind, hallazgos)
  } else {
    resultado <- data.frame(
      columna = character(), tipo_hallazgo = character(),
      severidad = character(), descripcion = character(),
      evidencia = character(), sugerencia = character(),
      n_evaluados = numeric(), n_afectados = numeric(),
      unidad_conteo = character(),
      estado_reparacion = character(),
      trazabilidad = I(list()),
      stringsAsFactors = FALSE
    )
  }
  resultado$severidad <- factor(
    resultado$severidad,
    levels = c("ok", "sospechoso", "error"), ordered = TRUE
  )
  rownames(resultado) <- NULL
  resultado
}
