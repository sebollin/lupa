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
    return(list(valores = as.numeric(x), clase = "fecha"))
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

.fecha_resumida <- function(valor, clase) {
  if (!is.finite(valor)) {
    return(NA_character_)
  }
  if (identical(clase, "fecha")) {
    return(format(as.Date(valor, origin = "1970-01-01"), "%Y-%m-%d"))
  }
  format(
    as.POSIXct(valor, origin = "1970-01-01", tz = "UTC"),
    "%Y-%m-%d %H:%M:%S", tz = "UTC"
  )
}

.resumen_cuantitativo <- function(x, inferencia, formatos) {
  cuantitativos <- .valores_cuantitativos(x, inferencia, formatos)
  valores <- cuantitativos$valores
  valores <- valores[is.finite(valores)]
  vacio <- list(
    minimo = NA_real_, maximo = NA_real_, media = NA_real_,
    mediana = NA_real_, desvio = NA_real_, minimo_fecha = NA_character_,
    maximo_fecha = NA_character_, media_fecha = NA_character_,
    mediana_fecha = NA_character_, n_ceros = 0L, n_negativos = 0L,
    n_outliers = 0L
  )
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
      desvio = desvio, minimo_fecha = NA_character_, maximo_fecha = NA_character_,
      media_fecha = NA_character_, mediana_fecha = NA_character_,
      n_ceros = sum(valores == 0), n_negativos = sum(valores < 0),
      n_outliers = n_outliers
    ))
  }

  list(
    minimo = NA_real_, maximo = NA_real_, media = NA_real_, mediana = NA_real_,
    desvio = desvio,
    minimo_fecha = .fecha_resumida(minimo, cuantitativos$clase),
    maximo_fecha = .fecha_resumida(maximo, cuantitativos$clase),
    media_fecha = .fecha_resumida(media, cuantitativos$clase),
    mediana_fecha = .fecha_resumida(mediana, cuantitativos$clase),
    n_ceros = 0L, n_negativos = 0L, n_outliers = n_outliers
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

.analizar_codificacion <- function(textos) {
  reparados <- rep(NA_character_, length(textos))
  candidatos <- !is.na(textos) &
    grepl("[\u00c3\u00c2\u00e2\u00f0\ufffd]", textos, perl = TRUE)
  if (any(candidatos)) {
    reparados[candidatos] <- vapply(
      textos[candidatos], .reparar_mojibake_uno, character(1L)
    )
  }
  reparables <- !is.na(reparados) & !is.na(textos) & reparados != textos
  irreparables <- !is.na(textos) & grepl("\ufffd", textos, fixed = TRUE)
  ejemplos <- utils::head(which(reparables | irreparables), 5L)
  evidencia <- paste(vapply(ejemplos, function(i) {
    origen <- encodeString(textos[[i]], quote = '"')
    if (reparables[[i]]) {
      paste0(origen, " -> ", encodeString(reparados[[i]], quote = '"'))
    } else {
      paste0(origen, " (contiene un car\u00e1cter de reemplazo irrecuperable)")
    }
  }, character(1L)), collapse = "; ")
  list(
    n = sum(reparables | irreparables),
    n_reparables = sum(reparables),
    n_irreparables = sum(irreparables),
    evidencia = evidencia,
    reparados = reparados
  )
}

.componentes_numero_texto <- function(x) {
  texto <- trimws(as.character(x))
  patron <- paste0(
    "^(?:[$]|UYU)?[[:space:]]*[+-]?",
    "(?:[0-9]{1,3}(?:\\.[0-9]{3})+|[0-9]+)",
    "(?:,[0-9]+)?[[:space:]]*(?:%|[[:alpha:]]+)?$"
  )
  compatible <- !is.na(texto) & grepl(patron, texto, perl = TRUE)
  cuerpo <- texto
  cuerpo <- sub("^(?:[$]|UYU)?[[:space:]]*", "", cuerpo, perl = TRUE)
  tiene_unidad <- grepl("(?:%|[[:alpha:]]+)$", cuerpo, perl = TRUE)
  unidad <- ifelse(
    compatible & tiene_unidad,
    sub("^.*?[[:space:]]*(%|[[:alpha:]]+)$", "\\1", cuerpo, perl = TRUE),
    ""
  )
  sin_unidad <- sub("[[:space:]]*(?:%|[[:alpha:]]+)$", "", cuerpo, perl = TRUE)
  tiene_coma <- grepl(",", sin_unidad, fixed = TRUE)
  punto_tres <- grepl("\\.[0-9]{3}(?:$|\\.)", sin_unidad, perl = TRUE)
  especial <- tiene_coma | punto_tres |
    grepl("^(?:[$]|UYU)", texto, perl = TRUE) | nzchar(unidad)
  list(
    texto = texto, compatible = compatible, especial = especial,
    cuerpo = sin_unidad, unidad = unidad, tiene_coma = tiene_coma,
    punto_tres = punto_tres
  )
}

.analizar_numeros_texto <- function(x) {
  vacio <- list(
    n = 0L, proporcion = NA_real_, ambiguo = FALSE, seguro = FALSE,
    evidencia = "", unidad = "", n_presentes = 0L
  )
  if (!is.character(x) && !is.factor(x)) return(vacio)
  partes <- .componentes_numero_texto(x)
  presentes <- !is.na(partes$texto) & nzchar(partes$texto)
  n_presentes <- sum(presentes)
  especiales <- presentes & partes$compatible & partes$especial
  if (!any(especiales)) {
    vacio$n_presentes <- n_presentes
    return(vacio)
  }
  hay_coma <- any(partes$tiene_coma[presentes & partes$compatible])
  ambiguos <- especiales & partes$punto_tres & !hay_coma
  unidades <- unique(partes$unidad[presentes & partes$compatible])
  unidades_no_vacias <- unidades[nzchar(unidades)]
  unidad_consistente <- length(unidades_no_vacias) <= 1L &&
    !(length(unidades_no_vacias) && any(!nzchar(unidades)))
  compatibles <- sum(presentes & partes$compatible)
  list(
    n = sum(especiales),
    proporcion = if (n_presentes) compatibles / n_presentes else NA_real_,
    ambiguo = any(ambiguos),
    seguro = compatibles == n_presentes && !any(ambiguos) && unidad_consistente,
    evidencia = paste(
      encodeString(utils::head(unique(partes$texto[especiales]), 6L), quote = '"'),
      collapse = "; "
    ),
    unidad = if (length(unidades_no_vacias) == 1L) unidades_no_vacias else "",
    n_presentes = n_presentes
  )
}

.diagnosticar_texto <- function(x) {
  vacio <- list(
    n_espacios_borde = 0L,
    evidencia_espacios = "",
    n_variantes_mayusculas = 0L,
    evidencia_mayusculas = "",
    n_codificacion_rota = 0L,
    n_codificacion_reparable = 0L,
    n_codificacion_irreparable = 0L,
    evidencia_codificacion = ""
  )
  if (!is.character(x) && !is.factor(x)) {
    return(vacio)
  }
  textos <- as.character(x)
  validos <- !is.na(textos)
  if (!any(validos)) {
    return(vacio)
  }

  espacios <- validos & textos != trimws(textos)
  ejemplos_espacios <- utils::head(unique(textos[espacios]), 6L)
  unicos <- unique(textos[validos])
  minusculas <- tolower(unicos)
  colision <- duplicated(minusculas) | duplicated(minusculas, fromLast = TRUE)
  variantes <- unicos[colision]
  codificacion <- .analizar_codificacion(textos)

  list(
    n_espacios_borde = sum(espacios),
    evidencia_espacios = paste(
      encodeString(ejemplos_espacios, quote = '"'), collapse = "; "
    ),
    n_variantes_mayusculas = length(variantes),
    evidencia_mayusculas = paste(
      encodeString(utils::head(variantes, 6L), quote = '"'), collapse = "; "
    ),
    n_codificacion_rota = codificacion$n,
    n_codificacion_reparable = codificacion$n_reparables,
    n_codificacion_irreparable = codificacion$n_irreparables,
    evidencia_codificacion = codificacion$evidencia
  )
}

.perfilar_columna <- function(x, nombre, muestra, max_patrones,
                              distinguir_mayusculas, expandir,
                              umbral_patron_raro,
                              sentinelas_numericos) {
  inferencia <- inferir_tipo(x, muestra = muestra)
  formatos <- inferencia$formatos_fecha
  if (is.null(formatos)) {
    formatos <- detectar_formatos_fecha(x, muestra = muestra)
  }
  patrones <- if (is.character(x) || is.factor(x)) {
    descubrir_patrones(
      x,
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
    x, sentinelas_numericos = sentinelas_numericos
  )
  n <- length(x)
  n_faltantes <- sum(is.na(x))
  n_validos <- n - n_faltantes
  n_distintos <- if (n_validos && !is.list(x)) length(unique(x[!is.na(x)])) else 0L
  moda <- .moda_columna(x)
  longitudes <- .resumen_longitud(x)
  cuantitativo <- .resumen_cuantitativo(x, inferencia, formatos)
  diagnostico_texto <- .diagnosticar_texto(x)
  numeros_texto <- .analizar_numeros_texto(x)
  n_blancos <- if (is.character(x) || is.factor(x)) {
    sum(!is.na(x) & !nzchar(trimws(as.character(x))))
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
    tasa_distintos = if (n_validos) n_distintos / n_validos else NA_real_,
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
    minimo_fecha = cuantitativo$minimo_fecha,
    maximo_fecha = cuantitativo$maximo_fecha,
    media_fecha = cuantitativo$media_fecha,
    mediana_fecha = cuantitativo$mediana_fecha,
    n_ceros = cuantitativo$n_ceros,
    n_negativos = cuantitativo$n_negativos,
    n_outliers = cuantitativo$n_outliers,
    n_blancos = n_blancos,
    n_espacios_borde = diagnostico_texto$n_espacios_borde,
    n_variantes_mayusculas = diagnostico_texto$n_variantes_mayusculas,
    n_codificacion_rota = diagnostico_texto$n_codificacion_rota,
    n_codificacion_reparable = diagnostico_texto$n_codificacion_reparable,
    n_codificacion_irreparable = diagnostico_texto$n_codificacion_irreparable,
    n_numeros_texto = numeros_texto$n,
    proporcion_numeros_texto = numeros_texto$proporcion,
    numero_texto_ambiguo = numeros_texto$ambiguo,
    numero_texto_seguro = numeros_texto$seguro,
    numero_texto_unidad = numeros_texto$unidad,
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
