.moda_columna <- function(x) {
  validos <- !is.na(x)
  if (!any(validos) || is.list(x)) {
    return(list(valor = NA_character_, frecuencia = 0L))
  }
  valores <- x[validos]
  unicos <- unique(valores)
  indices <- match(valores, unicos)
  frecuencias <- tabulate(indices, nbins = length(unicos))
  posicion <- which.max(frecuencias)
  list(
    valor = .texto_valor(unicos[posicion]),
    frecuencia = as.integer(frecuencias[[posicion]])
  )
}

.resumen_longitud <- function(x) {
  if (!is.character(x) && !is.factor(x)) {
    return(c(minimo = NA_real_, maximo = NA_real_, media = NA_real_))
  }
  longitudes <- nchar(as.character(x), type = "chars", allowNA = TRUE)
  longitudes <- longitudes[!is.na(longitudes)]
  if (!length(longitudes)) {
    return(c(minimo = NA_real_, maximo = NA_real_, media = NA_real_))
  }
  c(
    minimo = min(longitudes),
    maximo = max(longitudes),
    media = mean(longitudes)
  )
}

.valores_cuantitativos <- function(x, inferencia, formatos) {
  if (inherits(x, "POSIXt")) {
    return(list(valores = as.numeric(x), clase = "fecha-hora"))
  }
  if (inherits(x, "Date")) {
    return(list(valores = as.numeric(x) * 86400, clase = "fecha"))
  }
  if (inherits(x, "integer64")) {
    return(list(valores = x, clase = "integer64"))
  }
  if (is.numeric(x)) {
    return(list(valores = as.numeric(x), clase = "numero"))
  }
  if (is.character(x) || is.factor(x)) {
    if (inferencia$tipo %in% c("entero", "doble")) {
      valores <- suppressWarnings(as.numeric(sub(",", ".", as.character(x), fixed = TRUE)))
      return(list(valores = valores, clase = "numero"))
    }
    if (inferencia$tipo %in% c("fecha", "fecha-hora")) {
      fechas <- .parsear_fechas(x, formatos)
      return(list(valores = as.numeric(fechas), clase = inferencia$tipo))
    }
  }
  list(valores = numeric(), clase = "ninguna")
}

.resumen_vacio_cuantitativo <- function(estado = "no_aplica") {
  list(
    minimo = NA_real_, maximo = NA_real_, media = NA_real_,
    mediana = NA_real_, desvio = NA_real_, minimo_exacto = NA_character_,
    maximo_exacto = NA_character_, minimo_fecha = NA_character_,
    maximo_fecha = NA_character_, media_fecha = NA_character_,
    mediana_fecha = NA_character_, n_ceros = NA_integer_,
    n_negativos = NA_integer_, n_outliers = NA_integer_, n_nan = 0L,
    n_infinito_positivo = 0L, n_infinito_negativo = 0L,
    estado_resumen_cuantitativo = estado
  )
}

.resumen_integer64 <- function(x) {
  vacio <- .resumen_vacio_cuantitativo("sin_valores")
  validos <- !is.na(x)
  if (!any(validos)) return(vacio)
  if (!requireNamespace("bit64", quietly = TRUE)) {
    vacio$estado_resumen_cuantitativo <- "requiere_bit64"
    return(vacio)
  }
  valores <- x[validos]
  minimo_exacto <- as.character(min(valores))
  maximo_exacto <- as.character(max(valores))
  limite <- bit64::as.integer64("9007199254740991")
  seguros <- valores >= -limite & valores <= limite
  if (!all(seguros)) {
    vacio$minimo_exacto <- minimo_exacto
    vacio$maximo_exacto <- maximo_exacto
    vacio$n_ceros <- as.integer(sum(valores == bit64::as.integer64(0)))
    vacio$n_negativos <- as.integer(sum(valores < bit64::as.integer64(0)))
    vacio$estado_resumen_cuantitativo <- "omitidos_precision"
    return(vacio)
  }
  resultado <- .resumen_cuantitativo(
    as.numeric(valores),
    list(tipo = "doble"),
    data.frame(stringsAsFactors = FALSE)
  )
  resultado$minimo_exacto <- minimo_exacto
  resultado$maximo_exacto <- maximo_exacto
  resultado
}

.fecha_resumida <- function(valor, clase) {
  if (!is.finite(valor)) {
    return(NA_character_)
  }
  if (identical(clase, "fecha")) {
    return(format(
      as.POSIXct(valor, origin = "1970-01-01", tz = "UTC"),
      "%Y-%m-%d", tz = "UTC"
    ))
  }
  format(
    as.POSIXct(valor, origin = "1970-01-01", tz = "UTC"),
    "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"
  )
}

.resumen_cuantitativo <- function(x, inferencia, formatos) {
  cuantitativos <- .valores_cuantitativos(x, inferencia, formatos)
  if (identical(cuantitativos$clase, "integer64")) {
    return(.resumen_integer64(cuantitativos$valores))
  }
  valores <- cuantitativos$valores
  n_nan <- if (is.numeric(valores)) sum(is.nan(valores)) else 0L
  n_infinito_positivo <- if (is.numeric(valores)) {
    sum(is.infinite(valores) & valores > 0, na.rm = TRUE)
  } else 0L
  n_infinito_negativo <- if (is.numeric(valores)) {
    sum(is.infinite(valores) & valores < 0, na.rm = TRUE)
  } else 0L
  valores <- valores[is.finite(valores)]
  vacio <- .resumen_vacio_cuantitativo(if (
    identical(cuantitativos$clase, "ninguna")) "no_aplica" else "sin_valores"
  )
  vacio$n_nan <- as.integer(n_nan)
  vacio$n_infinito_positivo <- as.integer(n_infinito_positivo)
  vacio$n_infinito_negativo <- as.integer(n_infinito_negativo)
  if (!length(valores)) {
    return(vacio)
  }

  minimo <- min(valores)
  maximo <- max(valores)
  media <- mean(valores)
  mediana <- stats::median(valores)
  desvio <- if (length(valores) > 1L) stats::sd(valores) else NA_real_
  iqr <- stats::IQR(valores, na.rm = TRUE, type = 7)
  if (is.finite(iqr)) {
    q <- stats::quantile(valores, probs = c(0.25, 0.75), names = FALSE, type = 7)
    n_outliers <- sum(valores < q[[1L]] - 1.5 * iqr |
      valores > q[[2L]] + 1.5 * iqr)
  } else {
    n_outliers <- 0L
  }

  if (identical(cuantitativos$clase, "numero")) {
    return(list(
      minimo = minimo, maximo = maximo, media = media, mediana = mediana,
      desvio = desvio, minimo_exacto = NA_character_, maximo_exacto = NA_character_,
      minimo_fecha = NA_character_, maximo_fecha = NA_character_,
      media_fecha = NA_character_, mediana_fecha = NA_character_,
      n_ceros = sum(valores == 0), n_negativos = sum(valores < 0),
      n_outliers = n_outliers, n_nan = as.integer(n_nan),
      n_infinito_positivo = as.integer(n_infinito_positivo),
      n_infinito_negativo = as.integer(n_infinito_negativo),
      estado_resumen_cuantitativo = "calculados"
    ))
  }

  list(
    minimo = NA_real_, maximo = NA_real_, media = NA_real_, mediana = NA_real_,
    desvio = desvio, minimo_exacto = NA_character_, maximo_exacto = NA_character_,
    minimo_fecha = .fecha_resumida(minimo, cuantitativos$clase),
    maximo_fecha = .fecha_resumida(maximo, cuantitativos$clase),
    media_fecha = .fecha_resumida(media, cuantitativos$clase),
    mediana_fecha = .fecha_resumida(mediana, cuantitativos$clase),
    n_ceros = 0L, n_negativos = 0L, n_outliers = n_outliers,
    n_nan = as.integer(n_nan),
    n_infinito_positivo = as.integer(n_infinito_positivo),
    n_infinito_negativo = as.integer(n_infinito_negativo),
    estado_resumen_cuantitativo = "calculados"
  )
}

.reparar_mojibake_uno <- function(x, max_iteraciones = 4L) {
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  actual <- enc2utf8(as.character(x))
  cambio <- FALSE
  for (i in seq_len(max_iteraciones)) {
    crudo <- tryCatch(
      iconv(actual, from = "UTF-8", to = "latin1", toRaw = TRUE)[[1L]],
      error = function(e) NULL
    )
    if (is.null(crudo)) break
    candidato <- rawToChar(crudo)
    Encoding(candidato) <- "UTF-8"
    if (!validUTF8(candidato) || identical(candidato, actual)) break
    actual <- candidato
    cambio <- TRUE
  }
  if (cambio) actual else NA_character_
}

.umbral_vocabulario_barato <- 0.5
.umbral_vocabulario_codificacion <- 0.8

# La deduplicacion se activa segun la cardinalidad. En 240.000 filas, cuatro
# valores fueron claramente favorables y 132.610 valores ya hicieron mas caro
# el recorrido para predicados baratos; la reparacion de codificacion siguio
# siendo favorable hasta una relacion de 0,8.
.vocabulario_texto <- function(textos, umbral, valores = NULL) {
  n <- length(textos)
  presentes <- !is.na(textos)
  if (is.null(valores)) valores <- unique(textos[presentes])
  distintos <- length(valores)
  usar <- n > 1L && distintos > 0L && distintos / n <= umbral
  if (usar) {
    indices <- match(textos, valores)
  } else {
    valores <- textos
    indices <- seq_len(n)
  }
  list(
    valores = valores,
    indices = indices,
    usar = usar,
    n_distintos = distintos
  )
}

.mapear_vocabulario <- function(textos, fn, umbral = .umbral_vocabulario_barato,
                               valores = NULL) {
  vocabulario <- .vocabulario_texto(textos, umbral, valores = valores)
  evaluados <- fn(vocabulario$valores)
  if (isTRUE(vocabulario$usar)) {
    evaluados[vocabulario$indices]
  } else {
    evaluados
  }
}

.componentes_numero_texto_optimizado <- function(textos, valores = NULL) {
  vocabulario <- .vocabulario_texto(
    textos, .umbral_vocabulario_barato, valores = valores
  )
  partes <- .componentes_numero_texto(vocabulario$valores)
  if (isTRUE(vocabulario$usar)) {
    partes <- lapply(partes, function(valores) valores[vocabulario$indices])
  }
  partes
}

.analizar_codificacion_vocabulario <- function(textos, valores = NULL) {
  if (!is.null(valores) && length(textos) > 1L &&
      length(valores) / length(textos) > .umbral_vocabulario_codificacion) {
    return(.analizar_codificacion(textos))
  }
  vocabulario <- .vocabulario_texto(
    textos, .umbral_vocabulario_codificacion, valores = valores
  )
  if (!isTRUE(vocabulario$usar)) {
    return(.analizar_codificacion(textos))
  }
  unico <- .analizar_codificacion(vocabulario$valores)
  reparados <- rep(NA_character_, length(textos))
  estados <- rep(NA_character_, length(textos))
  pasos <- vector("list", length(textos))
  presentes <- !is.na(vocabulario$indices)
  reparados[presentes] <- unico$reparados[vocabulario$indices[presentes]]
  estados[presentes] <- unico$estados[vocabulario$indices[presentes]]
  pasos[presentes] <- unico$pasos[vocabulario$indices[presentes]]
  reparables <- !is.na(reparados) & !is.na(textos) &
    reparados != textos & estados == "reparado"
  parciales <- !is.na(reparados) & !is.na(textos) &
    reparados != textos & estados == "reparado_parcialmente"
  irreparables <- !is.na(textos) & grepl("\uFFFD", textos, fixed = TRUE)
  afectados <- reparables | parciales | irreparables
  ejemplos <- utils::head(which(afectados), 5L)
  evidencia <- if (!length(ejemplos)) "" else paste(vapply(ejemplos, function(i) {
    origen <- encodeString(textos[[i]], quote = '"')
    if (reparables[[i]] || parciales[[i]]) {
      paste0(origen, " -> ", encodeString(reparados[[i]], quote = '"'),
             " [", estados[[i]], "]")
    } else {
      paste0(origen, " (contiene un caracter de reemplazo irrecuperable)")
    }
  }, character(1L)), collapse = "; ")
  list(
    n = sum(afectados), n_reparables = sum(reparables),
    n_reparables_parcialmente = sum(parciales),
    n_irreparables = sum(irreparables),
    n_no_se_pudo = sum(estados == "no_se_pudo", na.rm = TRUE),
    evidencia = evidencia, reparados = reparados, estados = estados,
    pasos = pasos, estado = .ftfy_estado_agregado(estados)
  )
}

.componentes_numero_texto <- function(x) {
  texto <- trimws(as.character(x))
  prefijo <- "(?:[[:upper:]]{3}[[:space:]]+|\\p{Sc}[[:space:]]*)?"
  sufijo <- "[[:space:]]*(?:%|[[:alpha:]]+)?$"
  numero_coma <- paste0(
    "(?:[0-9]{1,3}(?:\\.[0-9]{3})+|[0-9]+)(?:,[0-9]+)?"
  )
  numero_punto <- paste0(
    "(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]+)(?:\\.[0-9]+)?"
  )
  patron_coma <- paste0("^", prefijo, "[+-]?", numero_coma, sufijo)
  patron_punto <- paste0("^", prefijo, "[+-]?", numero_punto, sufijo)
  compatible_coma <- !is.na(texto) & grepl(patron_coma, texto, perl = TRUE)
  compatible_punto <- !is.na(texto) & grepl(patron_punto, texto, perl = TRUE)
  compatible <- compatible_coma | compatible_punto
  patron_prefijo <- paste0(
    "^[[:space:]]*(?:([[:upper:]]{3})[[:space:]]+|(\\p{Sc})[[:space:]]*)"
  )
  tiene_moneda <- compatible & grepl(patron_prefijo, texto, perl = TRUE)
  moneda <- ifelse(
    tiene_moneda,
    sub(paste0(patron_prefijo, ".*$"), "\\1\\2", texto, perl = TRUE),
    ""
  )
  cuerpo <- texto
  cuerpo <- sub(patron_prefijo, "", cuerpo, perl = TRUE)
  tiene_unidad <- grepl("(?:%|[[:alpha:]]+)$", cuerpo, perl = TRUE)
  unidad <- ifelse(
    compatible & tiene_unidad,
    sub("^.*?[[:space:]]*(%|[[:alpha:]]+)$", "\\1", cuerpo, perl = TRUE),
    ""
  )
  sin_unidad <- sub("[[:space:]]*(?:%|[[:alpha:]]+)$", "", cuerpo, perl = TRUE)
  tiene_coma <- grepl(",", sin_unidad, fixed = TRUE)
  tiene_punto <- grepl(".", sin_unidad, fixed = TRUE)
  punto_tres <- grepl(
    "^[+-]?[0-9]{1,3}(?:\\.[0-9]{3})+$", sin_unidad, perl = TRUE
  )
  coma_tres <- grepl(
    "^[+-]?[0-9]{1,3}(?:,[0-9]{3})+$", sin_unidad, perl = TRUE
  )
  evidencia_coma <- compatible_coma & (
    grepl("\\.[0-9]{3}(?:\\.[0-9]{3})*,[0-9]+$", sin_unidad, perl = TRUE) |
      grepl(",[0-9]{1,2}$|,[0-9]{4,}$", sin_unidad, perl = TRUE) |
      grepl("^[+-]?[0-9]{4,},[0-9]{3}$", sin_unidad, perl = TRUE)
  )
  evidencia_punto <- compatible_punto & (
    grepl(",[0-9]{3}(?:,[0-9]{3})*\\.[0-9]+$", sin_unidad, perl = TRUE) |
      grepl("\\.[0-9]{1,2}$|\\.[0-9]{4,}$", sin_unidad, perl = TRUE) |
      grepl("^[+-]?[0-9]{4,}\\.[0-9]{3}$", sin_unidad, perl = TRUE)
  )
  especial <- tiene_coma | tiene_punto | tiene_moneda | nzchar(unidad)
  list(
    texto = texto, compatible = compatible, especial = especial,
    cuerpo = sin_unidad, unidad = unidad, tiene_coma = tiene_coma,
    tiene_punto = tiene_punto, punto_tres = punto_tres, coma_tres = coma_tres,
    compatible_coma = compatible_coma, compatible_punto = compatible_punto,
    evidencia_coma = evidencia_coma, evidencia_punto = evidencia_punto,
    moneda = moneda
  )
}

.analizar_numeros_texto_directo <- function(x, umbral_compatibilidad = 0.8) {
  vacio <- list(
    n = 0L, proporcion = NA_real_, ambiguo = FALSE, seguro = FALSE,
    evidencia = "", unidad = "", moneda = "", convencion = "",
    n_presentes = 0L
  )
  if (!is.character(x) && !is.factor(x)) return(vacio)
  textos <- as.character(x)
  presentes <- !is.na(textos) & nzchar(textos)
  n_presentes <- sum(presentes)
  vacio$n_presentes <- n_presentes
  if (!n_presentes || !any(grepl("[0-9]", textos[presentes], perl = TRUE))) {
    return(vacio)
  }
  inicio_numerico <- grepl(
    paste0(
      "^[[:space:]]*(?:[[:upper:]]{3}[[:space:]]+|",
      "\\p{Sc}[[:space:]]*)?[+-]?[0-9]"
    ), textos[presentes], perl = TRUE
  )
  if (mean(inicio_numerico) < umbral_compatibilidad) return(vacio)
  partes <- .componentes_numero_texto(textos)
  especiales <- presentes & partes$compatible & partes$especial
  if (!any(especiales)) return(vacio)
  hay_evidencia_coma <- any(partes$evidencia_coma[presentes])
  hay_evidencia_punto <- any(partes$evidencia_punto[presentes])
  convencion <- if (hay_evidencia_coma && hay_evidencia_punto) {
    "mixta"
  } else if (hay_evidencia_coma) {
    "decimal_coma"
  } else if (hay_evidencia_punto) {
    "decimal_punto"
  } else if (any((partes$punto_tres | partes$coma_tres) & especiales)) {
    "ambigua"
  } else {
    "sin_separadores"
  }
  compatibles_convencion <- switch(
    convencion,
    decimal_coma = partes$compatible_coma,
    decimal_punto = partes$compatible_punto,
    sin_separadores = partes$compatible,
    partes$compatible
  )
  ambiguos <- convencion %in% c("ambigua", "mixta") |
    any(presentes & !compatibles_convencion)
  unidades <- unique(partes$unidad[presentes & partes$compatible])
  unidades_no_vacias <- unidades[nzchar(unidades)]
  unidad_consistente <- length(unidades_no_vacias) <= 1L &&
    !(length(unidades_no_vacias) && any(!nzchar(unidades)))
  monedas <- unique(partes$moneda[presentes & partes$compatible])
  monedas_no_vacias <- monedas[nzchar(monedas)]
  moneda_consistente <- length(monedas_no_vacias) <= 1L &&
    !(length(monedas_no_vacias) && any(!nzchar(monedas)))
  compatibles <- sum(presentes & partes$compatible)
  list(
    n = sum(especiales),
    proporcion = if (n_presentes) compatibles / n_presentes else NA_real_,
    ambiguo = isTRUE(ambiguos),
    seguro = compatibles == n_presentes && !isTRUE(ambiguos) &&
      unidad_consistente && moneda_consistente,
    evidencia = paste(
      encodeString(utils::head(unique(partes$texto[especiales]), 6L), quote = '"'),
      collapse = "; "
    ),
    unidad = if (length(unidades_no_vacias) == 1L) unidades_no_vacias else "",
    moneda = if (length(monedas_no_vacias) == 1L) monedas_no_vacias else "",
    convencion = convencion,
    n_presentes = n_presentes
  )
}

.analizar_numeros_texto <- function(x, umbral_compatibilidad = 0.8,
                                    vocabulario = NULL, n_distintos = NULL,
                                    directo = FALSE) {
  if (isTRUE(directo)) {
    return(.analizar_numeros_texto_directo(x, umbral_compatibilidad))
  }
  vacio <- list(
    n = 0L, proporcion = NA_real_, ambiguo = FALSE, seguro = FALSE,
    evidencia = "", unidad = "", moneda = "", convencion = "",
    n_presentes = 0L
  )
  if (!is.character(x) && !is.factor(x)) return(vacio)
  textos <- as.character(x)
  presentes <- !is.na(textos) & nzchar(textos)
  n_presentes <- sum(presentes)
  vacio$n_presentes <- n_presentes
  if (is.null(vocabulario) && !is.null(n_distintos) &&
      length(textos) > 1L && is.finite(n_distintos) &&
      n_distintos / length(textos) > .umbral_vocabulario_barato) {
    vocabulario_numeros <- list(
      valores = textos, indices = seq_len(length(textos)), usar = FALSE
    )
  } else {
    vocabulario_numeros <- .vocabulario_texto(
      textos, .umbral_vocabulario_barato, valores = vocabulario
    )
  }
  if (isTRUE(vocabulario_numeros$usar)) {
    valores_vocabulario <- vocabulario_numeros$valores
    indices_vocabulario <- vocabulario_numeros$indices
    tiene_digitos_vocabulario <- !is.na(valores_vocabulario) &
      grepl("[0-9]", valores_vocabulario, perl = TRUE)
    tiene_digitos <- tiene_digitos_vocabulario[indices_vocabulario]
  } else {
    valores_vocabulario <- textos
    indices_vocabulario <- seq_len(length(textos))
    tiene_digitos <- !is.na(textos) &
      grepl("[0-9]", textos, perl = TRUE)
  }
  if (!n_presentes || !any(tiene_digitos[presentes])) {
    return(vacio)
  }
  patron_inicio <- paste0(
    "^[[:space:]]*(?:[[:upper:]]{3}[[:space:]]+|",
    "\\p{Sc}[[:space:]]*)?[+-]?[0-9]"
  )
  if (isTRUE(vocabulario_numeros$usar)) {
    inicio_vocabulario <- grepl(
      patron_inicio, valores_vocabulario, perl = TRUE
    )
    inicio_numerico <- inicio_vocabulario[indices_vocabulario]
    partes <- .componentes_numero_texto_optimizado(
      textos, valores = valores_vocabulario
    )
  } else {
    inicio_numerico <- rep(FALSE, length(textos))
    inicio_numerico[presentes] <- grepl(
      patron_inicio,
      textos[presentes], perl = TRUE
    )
    partes <- .componentes_numero_texto(textos)
  }
  if (mean(inicio_numerico[presentes]) < umbral_compatibilidad) {
    return(vacio)
  }
  especiales <- presentes & partes$compatible & partes$especial
  if (!any(especiales)) {
    return(vacio)
  }
  hay_evidencia_coma <- any(partes$evidencia_coma[presentes])
  hay_evidencia_punto <- any(partes$evidencia_punto[presentes])
  convencion <- if (hay_evidencia_coma && hay_evidencia_punto) {
    "mixta"
  } else if (hay_evidencia_coma) {
    "decimal_coma"
  } else if (hay_evidencia_punto) {
    "decimal_punto"
  } else if (any((partes$punto_tres | partes$coma_tres) & especiales)) {
    "ambigua"
  } else {
    "sin_separadores"
  }
  compatibles_convencion <- switch(
    convencion,
    decimal_coma = partes$compatible_coma,
    decimal_punto = partes$compatible_punto,
    sin_separadores = partes$compatible,
    partes$compatible
  )
  ambiguos <- convencion %in% c("ambigua", "mixta") |
    any(presentes & !compatibles_convencion)
  unidades <- unique(partes$unidad[presentes & partes$compatible])
  unidades_no_vacias <- unidades[nzchar(unidades)]
  unidad_consistente <- length(unidades_no_vacias) <= 1L &&
    !(length(unidades_no_vacias) && any(!nzchar(unidades)))
  monedas <- unique(partes$moneda[presentes & partes$compatible])
  monedas_no_vacias <- monedas[nzchar(monedas)]
  moneda_consistente <- length(monedas_no_vacias) <= 1L &&
    !(length(monedas_no_vacias) && any(!nzchar(monedas)))
  compatibles <- sum(presentes & partes$compatible)
  list(
    n = sum(especiales),
    proporcion = if (n_presentes) compatibles / n_presentes else NA_real_,
    ambiguo = isTRUE(ambiguos),
    seguro = compatibles == n_presentes && !isTRUE(ambiguos) &&
      unidad_consistente && moneda_consistente,
    evidencia = paste(
      encodeString(utils::head(unique(partes$texto[especiales]), 6L), quote = '"'),
      collapse = "; "
    ),
    unidad = if (length(unidades_no_vacias) == 1L) unidades_no_vacias else "",
    moneda = if (length(monedas_no_vacias) == 1L) monedas_no_vacias else "",
    convencion = convencion,
    n_presentes = n_presentes
  )
}

.diagnosticar_texto <- function(x, vocabulario = NULL) {
  vacio <- list(
    n_espacios_borde = 0L,
    evidencia_espacios = "",
    n_variantes_mayusculas = 0L,
    evidencia_mayusculas = "",
    n_variantes_unicode = NA_integer_,
    evidencia_unicode = "",
    unicode_evaluado = FALSE,
    n_codificacion_rota = 0L,
    n_codificacion_reparable = 0L,
    n_codificacion_reparable_parcialmente = 0L,
    n_codificacion_irreparable = 0L,
    n_codificacion_no_se_pudo = 0L,
    estado_codificacion_reparacion = "no_parece_roto",
    evidencia_codificacion = "",
    n_codificacion_invalida = 0L,
    evidencia_codificacion_invalida = ""
  )
  if (!is.character(x) && !is.factor(x)) {
    return(vacio)
  }
  preparacion <- .texto_analizable(x)
  textos <- preparacion$valores
  vacio$n_codificacion_invalida <- length(preparacion$posiciones)
  if (length(preparacion$posiciones)) {
    mostradas <- utils::head(preparacion$posiciones, 8L)
    vacio$evidencia_codificacion_invalida <- paste0(
      length(preparacion$posiciones), " valores; filas: ",
      paste(mostradas, collapse = ", "),
      if (length(preparacion$posiciones) > length(mostradas)) ", ..." else ""
    )
  }
  validos <- !is.na(textos)
  if (!any(validos)) {
    return(vacio)
  }

  unicos <- if (is.null(vocabulario)) {
    unique(textos[validos])
  } else {
    vocabulario
  }
  if (length(textos) > 1L &&
      length(unicos) / length(textos) <= .umbral_vocabulario_barato) {
    vocabulario_barato <- .vocabulario_texto(
      textos, .umbral_vocabulario_barato, valores = unicos
    )
    espacios_unicos <- vocabulario_barato$valores !=
      trimws(vocabulario_barato$valores)
    espacios <- validos &
      espacios_unicos[vocabulario_barato$indices]
  } else {
    espacios <- validos & textos != trimws(textos)
  }
  ejemplos_espacios <- utils::head(unique(textos[espacios]), 6L)
  minusculas <- tolower(unicos)
  colision <- duplicated(minusculas) | duplicated(minusculas, fromLast = TRUE)
  variantes <- unicos[colision]
  solo_ascii <- !any(grepl("[^\\x01-\\x7F]", unicos, perl = TRUE))
  if (solo_ascii) {
    evidencia_unicode <- ""
    n_variantes_unicode <- 0L
    unicode_evaluado <- TRUE
  } else if (requireNamespace("stringi", quietly = TRUE)) {
    normalizados_unicode <- stringi::stri_trans_nfc(unicos)
    colision_unicode <- duplicated(normalizados_unicode) |
      duplicated(normalizados_unicode, fromLast = TRUE)
    variantes_unicode <- unicos[colision_unicode]
    evidencia_unicode <- paste(
      stringi::stri_escape_unicode(utils::head(variantes_unicode, 6L)),
      collapse = "; "
    )
    n_variantes_unicode <- length(variantes_unicode)
    unicode_evaluado <- TRUE
  } else {
    evidencia_unicode <- "Se necesita el paquete opcional 'stringi'."
    n_variantes_unicode <- NA_integer_
    unicode_evaluado <- FALSE
  }
  codificacion <- .analizar_codificacion_vocabulario(textos, valores = unicos)

  list(
    n_espacios_borde = sum(espacios),
    evidencia_espacios = paste(
      encodeString(ejemplos_espacios, quote = '"'), collapse = "; "
    ),
    n_variantes_mayusculas = length(variantes),
    evidencia_mayusculas = paste(
      encodeString(utils::head(variantes, 6L), quote = '"'), collapse = "; "
    ),
    n_variantes_unicode = n_variantes_unicode,
    evidencia_unicode = evidencia_unicode,
    unicode_evaluado = unicode_evaluado,
    n_codificacion_rota = codificacion$n,
    n_codificacion_reparable = codificacion$n_reparables,
    n_codificacion_reparable_parcialmente = codificacion$n_reparables_parcialmente,
    n_codificacion_irreparable = codificacion$n_irreparables,
    n_codificacion_no_se_pudo = codificacion$n_no_se_pudo,
    estado_codificacion_reparacion = codificacion$estado,
    evidencia_codificacion = codificacion$evidencia,
    n_codificacion_invalida = length(preparacion$posiciones),
    evidencia_codificacion_invalida = vacio$evidencia_codificacion_invalida
  )
}

.n_distintos_columna <- function(x) {
  validos <- tryCatch(!is.na(x), error = function(e) NULL)
  if (is.null(validos) || length(validos) != length(x)) return(NA_integer_)
  if (!any(validos)) return(0L)
  tryCatch(
    as.integer(length(unique(x[validos]))),
    error = function(e) NA_integer_
  )
}

.perfilar_columna <- function(x, nombre, muestra, max_patrones,
                              distinguir_mayusculas, expandir,
                              umbral_patron_raro,
                              sentinelas_numericos) {
  if (is.matrix(x)) {
    return(.perfilar_columna_matriz(
      x, nombre, muestra, max_patrones, distinguir_mayusculas, expandir,
      umbral_patron_raro, sentinelas_numericos
    ))
  }
  preparacion_texto <- .texto_analizable(x)
  x_analisis <- preparacion_texto$valores
  inferencia <- inferir_tipo(x_analisis, muestra = muestra)
  formatos <- inferencia$formatos_fecha
  if (is.null(formatos)) {
    formatos <- detectar_formatos_fecha(x_analisis, muestra = muestra)
  }
  patrones <- if (is.character(x_analisis) || is.factor(x_analisis)) {
    descubrir_patrones(
      x_analisis,
      distinguir_mayusculas = distinguir_mayusculas,
      expandir = expandir,
      max_patrones = max_patrones,
      muestra = muestra,
      umbral_raro = umbral_patron_raro
    )
  } else {
    estructura <- data.frame(
      patron = character(), n = integer(), proporcion = numeric(),
      ejemplos = character(), stringsAsFactors = FALSE
    )
    class(estructura) <- c("patrones", "data.frame")
    estructura
  }
  faltantes_disfrazados <- .detectar_faltantes_disfrazados(
    x_analisis, sentinelas_numericos = sentinelas_numericos
  )
  n <- length(x)
  n_faltantes <- sum(is.na(x))
  n_codificacion_invalida <- length(preparacion_texto$posiciones)
  n_validos <- n - n_faltantes - n_codificacion_invalida
  n_distintos <- .n_distintos_columna(x_analisis)
  vocabulario_texto <- if (
    (is.character(x_analisis) || is.factor(x_analisis)) &&
      n > 1L && is.finite(n_distintos) &&
      n_distintos / n <= .umbral_vocabulario_codificacion
  ) {
    unique(x_analisis[!is.na(x_analisis)])
  } else {
    NULL
  }
  moda <- .moda_columna(x_analisis)
  longitudes <- .resumen_longitud(x_analisis)
  cuantitativo <- .resumen_cuantitativo(x_analisis, inferencia, formatos)
  diagnostico_texto <- .diagnosticar_texto(x, vocabulario = vocabulario_texto)
  vocabulario_numeros <- if (
    is.null(vocabulario_texto) &&
      (is.character(x_analisis) || is.factor(x_analisis))
  ) x_analisis else vocabulario_texto
  numeros_texto <- .analizar_numeros_texto(
    x_analisis, vocabulario = vocabulario_numeros, n_distintos = n_distintos,
    directo = is.null(vocabulario_texto) &&
      (is.character(x_analisis) || is.factor(x_analisis))
  )
  n_blancos <- if (is.character(x_analisis) || is.factor(x_analisis)) {
    sum(!is.na(x_analisis) & !nzchar(trimws(as.character(x_analisis))))
  } else {
    0L
  }

  fila <- data.frame(
    columna = nombre,
    tipo_declarado = .tipo_declarado(x),
    tipo_inferido = inferencia$tipo,
    proporcion_tipo_inferido = inferencia$proporcion,
    n = n,
    n_faltantes = n_faltantes,
    prop_faltantes = if (n) n_faltantes / n else NA_real_,
    n_faltantes_disfrazados = faltantes_disfrazados$n,
    n_faltantes_disfrazados_textuales = faltantes_disfrazados$n_textuales,
    n_faltantes_disfrazados_numericos = faltantes_disfrazados$n_numericos,
    prop_faltantes_disfrazados = if (n) faltantes_disfrazados$n / n else NA_real_,
    n_faltantes_totales = n_faltantes + faltantes_disfrazados$n,
    prop_faltantes_totales = if (n) {
      (n_faltantes + faltantes_disfrazados$n) / n
    } else {
      NA_real_
    },
    n_distintos = n_distintos,
    tasa_distintos = if (n_validos && !is.na(n_distintos)) {
      n_distintos / n_validos
    } else NA_real_,
    moda = moda$valor,
    frecuencia_moda = moda$frecuencia,
    longitud_minima = unname(longitudes[["minimo"]]),
    longitud_maxima = unname(longitudes[["maximo"]]),
    longitud_media = unname(longitudes[["media"]]),
    minimo = cuantitativo$minimo,
    maximo = cuantitativo$maximo,
    media = cuantitativo$media,
    mediana = cuantitativo$mediana,
    desvio = cuantitativo$desvio,
    minimo_exacto = cuantitativo$minimo_exacto,
    maximo_exacto = cuantitativo$maximo_exacto,
    minimo_fecha = cuantitativo$minimo_fecha,
    maximo_fecha = cuantitativo$maximo_fecha,
    media_fecha = cuantitativo$media_fecha,
    mediana_fecha = cuantitativo$mediana_fecha,
    n_ceros = cuantitativo$n_ceros,
    n_negativos = cuantitativo$n_negativos,
    n_outliers = cuantitativo$n_outliers,
    n_nan = cuantitativo$n_nan,
    n_infinito_positivo = cuantitativo$n_infinito_positivo,
    n_infinito_negativo = cuantitativo$n_infinito_negativo,
    estado_resumen_cuantitativo = cuantitativo$estado_resumen_cuantitativo,
    detalle_proteccion_personal = NA_character_,
    n_blancos = n_blancos,
    n_espacios_borde = diagnostico_texto$n_espacios_borde,
    n_variantes_mayusculas = diagnostico_texto$n_variantes_mayusculas,
    n_variantes_unicode = diagnostico_texto$n_variantes_unicode,
    n_codificacion_rota = diagnostico_texto$n_codificacion_rota,
    n_codificacion_reparable = diagnostico_texto$n_codificacion_reparable,
    n_codificacion_reparable_parcialmente = diagnostico_texto$n_codificacion_reparable_parcialmente,
    n_codificacion_irreparable = diagnostico_texto$n_codificacion_irreparable,
    n_codificacion_no_se_pudo = diagnostico_texto$n_codificacion_no_se_pudo,
    estado_codificacion_reparacion = diagnostico_texto$estado_codificacion_reparacion,
    n_codificacion_invalida = diagnostico_texto$n_codificacion_invalida,
    n_numeros_texto = numeros_texto$n,
    proporcion_numeros_texto = numeros_texto$proporcion,
    numero_texto_ambiguo = numeros_texto$ambiguo,
    numero_texto_seguro = numeros_texto$seguro,
    numero_texto_unidad = numeros_texto$unidad,
    numero_texto_moneda = numeros_texto$moneda,
    numero_texto_convencion = numeros_texto$convencion,
    stringsAsFactors = FALSE
  )

  list(
    fila = fila,
    inferencia = inferencia,
    formatos = formatos,
    patrones = patrones,
    faltantes_disfrazados = faltantes_disfrazados,
    diagnostico_texto = diagnostico_texto,
    numeros_texto = numeros_texto
  )
}

.perfilar_columna_matriz <- function(x, nombre, muestra, max_patrones,
                                     distinguir_mayusculas, expandir,
                                     umbral_patron_raro,
                                     sentinelas_numericos) {
  resultado <- .perfilar_columna(
    rep(NA_character_, NROW(x)), nombre, muestra, max_patrones,
    distinguir_mayusculas, expandir, umbral_patron_raro,
    sentinelas_numericos
  )
  fila <- resultado$fila
  fila$tipo_declarado <- "matriz"
  fila$tipo_inferido <- "desconocido"
  fila$proporcion_tipo_inferido <- NA_real_
  fila$n <- NROW(x)
  enteros_na <- c(
    "n_faltantes", "n_faltantes_disfrazados",
    "n_faltantes_disfrazados_textuales",
    "n_faltantes_disfrazados_numericos", "n_faltantes_totales",
    "n_distintos", "frecuencia_moda", "n_ceros", "n_negativos",
    "n_outliers", "n_nan", "n_infinito_positivo", "n_infinito_negativo",
    "n_blancos", "n_espacios_borde", "n_variantes_mayusculas",
    "n_variantes_unicode", "n_codificacion_rota",
    "n_codificacion_reparable", "n_codificacion_reparable_parcialmente",
    "n_codificacion_irreparable", "n_codificacion_no_se_pudo",
    "n_codificacion_invalida", "n_numeros_texto"
  )
  fila[enteros_na] <- NA_integer_
  reales_na <- c(
    "prop_faltantes", "prop_faltantes_disfrazados",
    "prop_faltantes_totales", "tasa_distintos", "longitud_minima",
    "longitud_maxima", "longitud_media", "minimo", "maximo", "media",
    "mediana", "desvio", "proporcion_numeros_texto"
  )
  fila[reales_na] <- NA_real_
  fila$moda <- NA_character_
  fila$minimo_exacto <- NA_character_
  fila$maximo_exacto <- NA_character_
  fila$minimo_fecha <- NA_character_
  fila$maximo_fecha <- NA_character_
  fila$media_fecha <- NA_character_
  fila$mediana_fecha <- NA_character_
  fila$estado_resumen_cuantitativo <- "tipo_compuesto_no_analizado"
  fila$numero_texto_ambiguo <- FALSE
  fila$numero_texto_seguro <- FALSE
  fila$numero_texto_unidad <- ""
  fila$numero_texto_moneda <- ""
  fila$numero_texto_convencion <- ""
  resultado$fila <- fila
  resultado$inferencia$tipo <- "desconocido"
  resultado$inferencia$proporcion <- NA_real_
  resultado$inferencia$compatibles <- 0L
  resultado$inferencia$n_analizados <- 0L
  resultado$estructura_no_analizada <- list(
    tipo = "matriz", filas = NROW(x), componentes = NCOL(x),
    dimensiones = dim(x)
  )
  resultado
}
