.validar_config_comprension <- function(configuracion) {
  permitidas <- c("predicado", "minimo", "maximo", "inclusivo")
  desconocidas <- setdiff(names(configuracion), permitidas)
  if (length(desconocidas)) {
    .detener(
      "ValoresPosiblesPorComprension no acepta: ",
      paste(desconocidas, collapse = ", "), ".", call. = FALSE
    )
  }
  tiene_predicado <- !is.null(configuracion$predicado)
  tiene_rango <- !is.null(configuracion$minimo) ||
    !is.null(configuracion$maximo) || !is.null(configuracion$inclusivo)
  if (tiene_predicado == tiene_rango) {
    .detener(
      "ValoresPosiblesPorComprension exige un `predicado` o un rango, no ambos.",
      call. = FALSE
    )
  }
  if (tiene_predicado) {
    if (!is.function(configuracion$predicado)) {
      .detener("`predicado` debe ser una funci\u00f3n.", call. = FALSE)
    }
    return(list(predicado = configuracion$predicado))
  }
  if (is.null(configuracion$minimo) || is.null(configuracion$maximo) ||
      length(configuracion$minimo) != 1L ||
      length(configuracion$maximo) != 1L ||
      anyNA(c(configuracion$minimo, configuracion$maximo))) {
    .detener("El rango exige `minimo` y `maximo` escalares no ausentes.",
         call. = FALSE)
  }
  orden_valido <- tryCatch(
    isTRUE(configuracion$minimo <= configuracion$maximo),
    error = function(e) FALSE
  )
  if (!orden_valido) {
    .detener("`minimo` no puede ser mayor que `maximo`.", call. = FALSE)
  }
  inclusivo <- configuracion$inclusivo
  if (is.null(inclusivo)) inclusivo <- c(TRUE, TRUE)
  if (!is.logical(inclusivo) || anyNA(inclusivo) ||
      !length(inclusivo) %in% c(1L, 2L)) {
    .detener("`inclusivo` debe contener uno o dos valores l\u00f3gicos.",
         call. = FALSE)
  }
  if (length(inclusivo) == 1L) inclusivo <- rep(inclusivo, 2L)
  list(
    minimo = configuracion$minimo,
    maximo = configuracion$maximo,
    inclusivo = inclusivo
  )
}

.metodo_valores_comprension <- function(tablas, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  entidad <- instancia$entidad[[1L]]
  atributo <- instancia$atributos[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  x <- .obtener_columna_modelo(tabla, atributo, entidad)
  x <- .texto_analizable(x)$valores
  filas <- which(!is.na(x))
  valores <- x[filas]
  config <- instancia$configuracion
  if (!is.null(config$predicado)) {
    resultado <- .resultado_validador(
      config$predicado(valores), length(valores),
      "ValoresPosiblesPorComprension"
    )
  } else {
    # Un rango de otra clase que la columna no corre el numero: lo vacia. R avisa
    # a su manera -«Metodos incompatibles ("Ops.Date", "Ops.POSIXt")»-, que no
    # dice que hacer, y el resultado es `0 0 0` contra el `1 1 0` del mismo rango
    # declarado como fechas de calendario.
    .declarar_mezcla_de_husos(
      instancia$declaracion$nombre, atributo, valores,
      list(config$minimo, config$maximo),
      consecuencia = paste0(
        "as\u00ed que el rango no se puede comparar con la columna y la m\u00e9trica ",
        "publica que ning\u00fan valor lo cumple"
      )
    )
    izquierda <- if (config$inclusivo[[1L]]) {
      valores >= config$minimo
    } else {
      valores > config$minimo
    }
    derecha <- if (config$inclusivo[[2L]]) {
      valores <= config$maximo
    } else {
      valores < config$maximo
    }
    resultado <- izquierda & derecha
    if (!is.logical(resultado) || anyNA(resultado)) {
      .detener("El rango no se puede comparar con el atributo ligado.",
           call. = FALSE)
    }
  }
  .salida_metodo(
    resultado, entidad, atributo, filas,
    .pegar_texto0(entidad, "$", atributo, "[", filas, "]")
  )
}

.duplicados_completos <- function(x) {
  # `integer64` por su texto exacto: el metodo de `bit64` para `duplicated()` no
  # tiene `fromLast` y lo traga en `...`, asi que solo marcaba la segunda
  # aparicion -0 0 1 donde son 0 1 1- y la proporcion de duplicados salia la
  # mitad, justo en la columna donde mas se busca uno: un identificador leido de
  # una base. Medido. Es la misma regla que sigue la remediacion
  # (`.codigos_filas_exactos()`).
  if (inherits(x, "integer64")) {
    x <- as.character(x)
    attributes(x) <- NULL
  }
  duplicated(x) | duplicated(x, fromLast = TRUE)
}

.metodo_atributo_duplicado <- function(tablas, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  entidad <- instancia$entidad[[1L]]
  atributo <- instancia$atributos[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  x <- .obtener_columna_modelo(tabla, atributo, entidad)
  x <- .texto_analizable(x)$valores
  filas <- which(!is.na(x))
  .salida_metodo(
    .duplicados_completos(x[filas]), entidad, atributo, filas,
    .pegar_texto0(entidad, "$", atributo, "[", filas, "]")
  )
}

.validar_atributos_tabla <- function(tabla, instancia, minimo = 1L) {
  if (length(instancia$entidad) != 1L ||
      length(instancia$atributos) < minimo ||
      anyDuplicated(.nombres_para_operar(instancia$atributos))) {
    .detener(
      "La m\u00e9trica ", instancia$declaracion$nombre,
      " requiere una entidad y al menos ", minimo,
      if (minimo == 1L) " atributo distinto." else " atributos distintos.",
      call. = FALSE
    )
  }
  faltantes <- instancia$atributos[
    is.na(.indice_nombre(instancia$atributos, names(tabla)))
  ]
  if (length(faltantes)) {
    .detener(
      "No se encontraron atributos ligados: ",
      paste(faltantes, collapse = ", "), ".", call. = FALSE
    )
  }
}

.metodo_conjunto_duplicado <- function(tablas, instancia) {
  if (length(instancia$entidad) != 1L) {
    .detener(
      "ConjuntoAtributosDuplicado requiere una entidad y al menos dos atributos.",
      call. = FALSE
    )
  }
  entidad <- instancia$entidad[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  .validar_atributos_tabla(tabla, instancia, minimo = 2L)
  valores <- .seleccionar_columnas(tabla, instancia$atributos)
  filas <- seq_len(nrow(tabla))
  .salida_metodo(
    .duplicados_completos(valores), entidad, NA_character_, filas,
    paste0(entidad, "[", filas, ",",
           paste(instancia$atributos, collapse = "+"), "]")
  )
}

.metodo_entidad_duplicada <- function(tablas, instancia) {
  if (length(instancia$entidad) != 1L) {
    .detener("EntidadDuplicada requiere una entidad.", call. = FALSE)
  }
  entidad <- instancia$entidad[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  filas <- seq_len(nrow(tabla))
  if (!length(instancia$atributos)) {
    resultado <- .duplicados_completos(tabla)
  } else {
    .validar_atributos_tabla(tabla, instancia, minimo = 1L)
    grupos <- split(
      filas, .codigos_filas(.seleccionar_columnas(tabla, instancia$atributos))
    )
    resultado <- rep(FALSE, nrow(tabla))
    indices_atributos <- .indice_nombre(instancia$atributos, names(tabla))
    otros <- names(tabla)[-unique(indices_atributos)]
    compatibles <- function(b, a) {
      if (!length(otros)) return(TRUE)
      all(vapply(otros, function(nombre) {
        x <- tabla[[nombre]][[a]]
        y <- tabla[[nombre]][[b]]
        is.na(x) || is.na(y) || identical(as.character(x), as.character(y))
      }, logical(1L)))
    }
    for (indices in grupos[lengths(grupos) > 1L]) {
      for (i in seq_along(indices)) {
        restantes <- indices[-i]
        resultado[indices[[i]]] <- any(vapply(
          restantes, compatibles, logical(1L), a = indices[[i]]
        ))
      }
    }
  }
  .salida_metodo(
    resultado, entidad, NA_character_, filas,
    paste0(entidad, "[", filas, ",]")
  )
}

.validar_config_entidad_contradictoria <- function(configuracion) {
  permitidas <- c("metodo", "p", "umbral", "normalizar", "max_valores", "nucleos")
  desconocidas <- setdiff(names(configuracion), permitidas)
  if (length(desconocidas)) {
    .detener(
      "EntidadContradictoria no acepta: ",
      paste(desconocidas, collapse = ", "), ".", call. = FALSE
    )
  }
  metodo <- configuracion$metodo
  if (is.null(metodo)) metodo <- "jw"
  if (!is.character(metodo) || length(metodo) != 1L || is.na(metodo) ||
      !metodo %in% c(
        "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine",
        "jaccard", "jw", "soundex"
      )) {
    .detener(
      "`metodo` debe ser una medida admitida: osa, lv, dl, hamming, lcs, ",
      "qgram, cosine, jaccard, jw o soundex.", call. = FALSE
    )
  }
  p <- configuracion$p
  if (is.null(p)) p <- 0.1
  if (!is.numeric(p) || length(p) != 1L || is.na(p) || !is.finite(p) ||
      p < 0 || p > 0.25) {
    .detener("`p` debe ser un n\u00famero finito entre 0 y 0.25.", call. = FALSE)
  }
  umbral <- configuracion$umbral
  if (is.null(umbral)) umbral <- 0.10
  if (!is.numeric(umbral) || length(umbral) != 1L || is.na(umbral) ||
      !is.finite(umbral) || umbral < 0) {
    .detener("`umbral` debe ser un n\u00famero finito no negativo.", call. = FALSE)
  }
  normalizar <- configuracion$normalizar
  if (is.null(normalizar)) normalizar <- TRUE
  # Se resuelve solo para rechazar aca un perfil invalido: lo que se guarda
  # es `normalizar` tal como llego.
  tryCatch(
    .resolver_normalizacion(normalizar),
    error = function(e) {
    .detener("normalizar no describe un perfil v\u00e1lido: ",
           conditionMessage(e), call. = FALSE)
    }
  )
  max_valores <- configuracion$max_valores
  if (is.null(max_valores)) max_valores <- 10000L
  if (!is.numeric(max_valores) || length(max_valores) != 1L ||
      is.na(max_valores) || max_valores < 1 ||
      (!is.infinite(max_valores) && max_valores != floor(max_valores))) {
    .detener("`max_valores` debe ser un entero positivo o Inf.", call. = FALSE)
  }
  nucleos <- .resolver_nucleos_lupa(configuracion$nucleos)
  list(
    metodo = metodo, p = p, umbral = umbral, normalizar = normalizar,
    max_valores = if (is.infinite(max_valores)) Inf else as.integer(max_valores),
    nucleos = nucleos
  )
}

.metodo_entidad_contradictoria <- function(tablas, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  entidad <- instancia$entidad[[1L]]
  atributo <- instancia$atributos[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  # Se pide la columna para que una que falta se rechace aca; el valor no
  # hace falta, el texto sale de `.texto_fila_aproximada()`.
  .obtener_columna_modelo(tabla, atributo, entidad)
  config <- instancia$configuracion
  textos <- .texto_fila_aproximada(
    .seleccionar_columnas(tabla, atributo), atributo, config$normalizar
  )
  valores <- textos$valores
  presentes <- textos$presentes
  vocabulario <- unique(valores[presentes])
  total_valores <- length(vocabulario)
  vocabulario_evaluado <- if (is.infinite(config$max_valores)) {
    vocabulario
  } else {
    utils::head(vocabulario, config$max_valores)
  }
  if (length(vocabulario_evaluado) > 1L && !.stringdist_disponible()) {
    .detener(
      "EntidadContradictoria requiere el paquete opcional 'stringdist'.",
      call. = FALSE
    )
  }
  truncado <- length(vocabulario_evaluado) < total_valores
  n_posibles <- as.numeric(total_valores) * (total_valores - 1) / 2
  n_comparados <- as.numeric(length(vocabulario_evaluado)) *
    (length(vocabulario_evaluado) - 1) / 2
  pares <- if (length(vocabulario_evaluado) > 1L) {
    .comparar_bloques_duplicados(
      vocabulario_evaluado, seq_along(vocabulario_evaluado),
      config$metodo, config$umbral,
      bloque = min(1000L, length(vocabulario_evaluado)),
      max_resultados = Inf, nucleos = config$nucleos, p = config$p
    )
  } else {
    list(pares = data.frame(
      fila_1 = integer(), fila_2 = integer(), distancia = numeric(),
      stringsAsFactors = FALSE
    ), n_hallados = 0L)
  }
  indices_contradictorios <- if (nrow(pares$pares)) {
    unique(c(pares$pares$fila_1, pares$pares$fila_2))
  } else integer()
  valores_contradictorios <- vocabulario_evaluado[indices_contradictorios]
  filas <- which(presentes & valores %in% vocabulario_evaluado)
  resultado <- valores[filas] %in% valores_contradictorios
  salida <- .salida_metodo(
    resultado, entidad, atributo, filas,
    paste0(entidad, "$", atributo, "[", filas, "]")
  )
  attr(salida, "alcance") <- list(
    unidad_conteo = "valor_distinto",
    n_evaluados = length(vocabulario_evaluado),
    n_afectados = length(valores_contradictorios),
    n_valores_distintos_total = total_valores,
    n_valores_distintos_comparados = length(vocabulario_evaluado),
    n_filas_evaluadas = length(filas),
    n_pares_posibles = n_posibles,
    n_pares_comparados = n_comparados,
    n_pares_bajo_umbral = nrow(pares$pares),
    umbral = config$umbral,
    metodo = config$metodo,
    p = config$p,
    normalizar = config$normalizar,
    normalizacion = .normalizacion_resumen(.resolver_normalizacion(
      config$normalizar)),
    max_valores = config$max_valores,
    truncado = truncado,
    pares_sin_comparar = n_posibles - n_comparados,
    alcance = if (truncado) "prefijo_vocabulario" else "universo_completo",
    disponible = TRUE
  )
  salida
}

.validar_config_desactualizacion <- function(configuracion) {
  permitidas <- c("expresion_regular", "validador")
  desconocidas <- setdiff(names(configuracion), permitidas)
  activas <- intersect(names(configuracion), permitidas)
  activas <- activas[!vapply(configuracion[activas], is.null, logical(1L))]
  if (length(desconocidas) || length(activas) != 1L) {
    .detener(
      "DesactualizacionPorFormato exige exactamente `expresion_regular` o `validador`.",
      call. = FALSE
    )
  }
  if (activas == "expresion_regular" &&
      !.es_texto_escalar(configuracion$expresion_regular)) {
    .detener("`expresion_regular` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  if (activas == "validador" && !is.function(configuracion$validador)) {
    .detener("`validador` debe ser una funci\u00f3n.", call. = FALSE)
  }
  configuracion
}

.metodo_desactualizacion_formato <- function(tablas, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  entidad <- instancia$entidad[[1L]]
  atributo <- instancia$atributos[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  x <- .obtener_columna_modelo(tabla, atributo, entidad)
  filas <- which(!is.na(x))
  valores <- x[filas]
  config <- instancia$configuracion
  vigente <- if (!is.null(config$expresion_regular)) {
    grepl(config$expresion_regular, as.character(valores), perl = TRUE)
  } else {
    .resultado_validador(
      config$validador(valores), length(valores),
      "DesactualizacionPorFormato"
    )
  }
  .salida_metodo(
    !vigente, entidad, atributo, filas,
    paste0(entidad, "$", atributo, "[", filas, "]")
  )
}

.es_fecha_modelo <- function(x) {
  inherits(x, "Date") || inherits(x, "POSIXt")
}

.fecha_numerica <- function(x) {
  if (inherits(x, "Date")) as.numeric(x) * 86400 else as.numeric(x)
}

.validar_intervalo_fechas <- function(inicio, fin, nombre_inicio, nombre_fin) {
  if (!.es_fecha_modelo(inicio) || !.es_fecha_modelo(fin) ||
      !length(inicio) || !length(fin) || anyNA(inicio) || anyNA(fin)) {
    .detener(
      "`", nombre_inicio, "` y `", nombre_fin,
      "` deben contener fechas no ausentes.", call. = FALSE
    )
  }
  if (length(inicio) != 1L && length(fin) != 1L &&
      length(inicio) != length(fin)) {
    .detener("Los extremos del intervalo deben tener longitudes compatibles.",
         call. = FALSE)
  }
  n <- max(length(inicio), length(fin))
  diferencia <- rep(.fecha_numerica(fin), length.out = n) -
    rep(.fecha_numerica(inicio), length.out = n)
  if (any(!is.finite(diferencia))) {
    .detener("El intervalo contiene una duraci\u00f3n no finita.", call. = FALSE)
  }
  if (any(diferencia == 0)) {
    .detener(
      "El intervalo tiene duraci\u00f3n cero y no define oportunidad.",
      call. = FALSE
    )
  }
  if (any(diferencia < 0)) {
    .detener(
      "`", nombre_fin, "` debe ser posterior a `", nombre_inicio,
      "`; el intervalo est\u00e1 invertido.", call. = FALSE
    )
  }
  invisible(TRUE)
}

.validar_config_oportunidad_fecha <- function(configuracion) {
  configuracion <- .validar_propiedades_base(configuracion, "fecha_limite")
  limite <- configuracion$fecha_limite
  if (!.es_fecha_modelo(limite) || !length(limite) || anyNA(limite)) {
    .detener("`fecha_limite` debe contener fechas no ausentes.", call. = FALSE)
  }
  configuracion
}

.validar_config_oportunidad_intervalo <- function(configuracion) {
  configuracion <- .validar_propiedades_base(
    configuracion, c("inicio_vigencia", "fin_vigencia")
  )
  inicio <- configuracion$inicio_vigencia
  fin <- configuracion$fin_vigencia
  if (!.es_fecha_modelo(inicio) || !.es_fecha_modelo(fin) ||
      !length(inicio) || !length(fin) || anyNA(inicio) || anyNA(fin)) {
    .detener(
      "`inicio_vigencia` y `fin_vigencia` deben contener fechas no ausentes.",
      call. = FALSE
    )
  }
  if (length(inicio) != 1L && length(fin) != 1L &&
      length(inicio) != length(fin)) {
    .detener("Los extremos del intervalo deben tener longitudes compatibles.",
         call. = FALSE)
  }
  n <- max(length(inicio), length(fin))
  diferencia <- rep(.fecha_numerica(fin), length.out = n) -
    rep(.fecha_numerica(inicio), length.out = n)
  if (any(!is.finite(diferencia))) {
    .detener("El intervalo contiene una duraci\u00f3n no finita.", call. = FALSE)
  }
  if (any(diferencia < 0)) {
    .detener(
      "`fin_vigencia` no puede ser anterior a `inicio_vigencia`; ",
      "el intervalo est\u00e1 invertido.", call. = FALSE
    )
  }
  configuracion
}

.validar_config_grado_oportunidad_fecha <- function(configuracion) {
  configuracion <- .validar_propiedades_base(
    configuracion, c("fecha_solicitud", "fecha_fin_utilidad")
  )
  .validar_intervalo_fechas(
    configuracion$fecha_solicitud, configuracion$fecha_fin_utilidad,
    "fecha_solicitud", "fecha_fin_utilidad"
  )
  configuracion
}

.validar_config_grado_oportunidad_intervalo <- function(configuracion) {
  configuracion <- .validar_propiedades_base(
    configuracion, c("inicio_vigencia", "fin_vigencia")
  )
  .validar_intervalo_fechas(
    configuracion$inicio_vigencia, configuracion$fin_vigencia,
    "inicio_vigencia", "fin_vigencia"
  )
  configuracion
}

.fecha_para_filas <- function(x, filas, n_total, nombre) {
  if (length(x) == 1L) return(rep(.fecha_numerica(x), length(filas)))
  if (length(x) != n_total) {
    .detener(
      "`", nombre, "` debe tener longitud 1 o tantas fechas como filas.",
      call. = FALSE
    )
  }
  .fecha_numerica(x[filas])
}

.datos_oportunidad <- function(tablas, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  entidad <- instancia$entidad[[1L]]
  atributo <- instancia$atributos[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  x <- .obtener_columna_modelo(tabla, atributo, entidad)
  if (!.es_fecha_modelo(x)) {
    .detener("La oportunidad requiere un atributo Date o POSIXt.", call. = FALSE)
  }
  filas <- which(!is.na(x))
  list(
    entidad = entidad,
    atributo = atributo,
    x = x,
    filas = filas,
    entrega = .fecha_numerica(x[filas])
  )
}

.salida_oportunidad <- function(resultado, datos) {
  .salida_metodo(
    resultado, datos$entidad, datos$atributo, datos$filas,
    paste0(datos$entidad, "$", datos$atributo, "[", datos$filas, "]")
  )
}

.metodo_oportunidad_fecha <- function(tablas, instancia) {
  datos <- .datos_oportunidad(tablas, instancia)
  # La misma guarda que las metricas por entidad. Medido con una columna `Date` y
  # un `fecha_limite` `POSIXct` leido sin huso, la fila que cae EXACTAMENTE en el
  # limite daba `1 1 0` con `TZ=UTC` y `1 0 0` con `TZ=Asia/Tokyo`, sin que nada
  # lo dijera: cuatro metricas avisaban y sus cuatro gemelas por atributo no.
  .declarar_mezcla_de_husos(
    instancia$declaracion$nombre, datos$atributo, datos$x,
    list(instancia$configuracion$fecha_limite)
  )
  limite <- .fecha_para_filas(
    instancia$configuracion$fecha_limite, datos$filas, length(datos$x),
    "fecha_limite"
  )
  .salida_oportunidad(datos$entrega <= limite, datos)
}

.metodo_oportunidad_intervalo <- function(tablas, instancia) {
  datos <- .datos_oportunidad(tablas, instancia)
  config <- instancia$configuracion
  .declarar_mezcla_de_husos(
    instancia$declaracion$nombre, datos$atributo, datos$x,
    list(config$inicio_vigencia, config$fin_vigencia)
  )
  inicio <- .fecha_para_filas(
    config$inicio_vigencia, datos$filas, length(datos$x), "inicio_vigencia"
  )
  fin <- .fecha_para_filas(
    config$fin_vigencia, datos$filas, length(datos$x), "fin_vigencia"
  )
  .salida_oportunidad(
    datos$entrega >= inicio & datos$entrega <= fin,
    datos
  )
}

.metodo_grado_oportunidad <- function(tablas, instancia,
                                      inicio_nombre, fin_nombre) {
  datos <- .datos_oportunidad(tablas, instancia)
  config <- instancia$configuracion
  # Aca la mezcla pesa dos veces: la entrega se compara con el inicio Y la
  # duracion `fin - inicio` escala el resultado.
  .declarar_mezcla_de_husos(
    instancia$declaracion$nombre, datos$atributo, datos$x,
    list(config[[inicio_nombre]], config[[fin_nombre]])
  )
  inicio <- .fecha_para_filas(
    config[[inicio_nombre]], datos$filas, length(datos$x), inicio_nombre
  )
  fin <- .fecha_para_filas(
    config[[fin_nombre]], datos$filas, length(datos$x), fin_nombre
  )
  duracion <- fin - inicio
  if (any(duracion <= 0)) {
    .detener("El intervalo de oportunidad debe tener duraci\u00f3n positiva.",
         call. = FALSE)
  }
  resultado <- pmin(1, pmax(0, 1 - (datos$entrega - inicio) / duracion))
  .salida_oportunidad(resultado, datos)
}

.metodo_grado_oportunidad_fecha <- function(tablas, instancia) {
  .metodo_grado_oportunidad(
    tablas, instancia, "fecha_solicitud", "fecha_fin_utilidad"
  )
}

.metodo_grado_oportunidad_intervalo <- function(tablas, instancia) {
  .metodo_grado_oportunidad(
    tablas, instancia, "inicio_vigencia", "fin_vigencia"
  )
}

.validar_config_densidad <- function(configuracion) {
  configuracion <- .validar_propiedades_base(configuracion, "coeficientes")
  coeficientes <- configuracion$coeficientes
  if (!is.numeric(coeficientes) || !length(coeficientes) ||
      anyNA(coeficientes) || any(!is.finite(coeficientes)) ||
      any(coeficientes < 0 | coeficientes > 1) ||
      abs(sum(coeficientes) - 1) > sqrt(.Machine$double.eps)) {
    .detener(
      "`coeficientes` debe contener pesos en [0, 1] que sumen 1.",
      call. = FALSE
    )
  }
  if (!is.null(names(coeficientes)) &&
      (any(!nzchar(names(coeficientes))) ||
       anyDuplicated(.nombres_para_operar(names(coeficientes))))) {
    .detener("Los nombres de `coeficientes` deben ser \u00fanicos y no vac\u00edos.",
         call. = FALSE)
  }
  configuracion
}

.metodo_densidad_ponderada <- function(tablas, instancia) {
  if (length(instancia$entidad) != 1L) {
    .detener("DensidadPonderada requiere una entidad.", call. = FALSE)
  }
  entidad <- instancia$entidad[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  .validar_atributos_tabla(tabla, instancia, minimo = 1L)
  pesos <- instancia$configuracion$coeficientes
  if (!is.null(names(pesos))) {
    if (!setequal(.nombres_para_operar(names(pesos)),
                  .nombres_para_operar(instancia$atributos))) {
      .detener(
        "Los nombres de `coeficientes` deben coincidir con los atributos ligados.",
        call. = FALSE
      )
    }
    pesos <- pesos[.indice_identificador(instancia$atributos, names(pesos))]
  } else if (length(pesos) != length(instancia$atributos)) {
    .detener("Debe haber un coeficiente por atributo ligado.", call. = FALSE)
  }
  presencia <- !is.na(.seleccionar_columnas(tabla, instancia$atributos))
  resultado <- as.numeric(as.matrix(presencia) %*% pesos)
  filas <- seq_len(nrow(tabla))
  .salida_metodo(
    resultado, entidad, NA_character_, filas,
    paste0(entidad, "[", filas, ",]")
  )
}

.metricas_adicionales <- function() {
  list(
    Escala = metrica(
      "Escala",
      "Estima la precisi\u00f3n de un valor con el error experto de su escala.",
      "instanciaAtributo", "real", propiedades = "escala",
      dimension = "Exactitud", factor = "Precisi\u00f3n",
      metodo = .metodo_escala, validar_propiedades = .validar_config_escala,
      orientacion = "no_aplica"
    ),
    ValoresPosiblesPorComprension = metrica(
      "ValoresPosiblesPorComprension",
      "Indica si un valor satisface un predicado o pertenece a un rango.",
      "instanciaAtributo", "booleano",
      propiedades = c("predicado", "minimo", "maximo", "inclusivo"),
      dimension = "Consistencia", factor = "Integridad de dominio",
      metodo = .metodo_valores_comprension,
      validar_propiedades = .validar_config_comprension,
      orientacion = "conformidad"
    ),
    AtributoDuplicado = metrica(
      "AtributoDuplicado",
      "Indica si un valor participa en un grupo duplicado del atributo.",
      "instanciaAtributo", "booleano",
      dimension = "Unicidad", factor = "No-duplicaci\u00f3n",
      metodo = .metodo_atributo_duplicado, orientacion = "defecto"
    ),
    ConjuntoAtributosDuplicado = metrica(
      "ConjuntoAtributosDuplicado",
      "Indica si una combinaci\u00f3n de atributos se repite en otra fila.",
      "instanciaEntidad", "booleano",
      dimension = "Unicidad", factor = "No-duplicaci\u00f3n",
      metodo = .metodo_conjunto_duplicado, orientacion = "defecto"
    ),
    EntidadDuplicada = metrica(
      "EntidadDuplicada",
      paste0(
        "Indica si otra fila con la misma clave representa la misma entidad, ",
        "con los dem\u00e1s datos iguales o ausentes."
      ),
      "instanciaEntidad", "booleano",
      dimension = "Unicidad", factor = "No-duplicaci\u00f3n",
      metodo = .metodo_entidad_duplicada, orientacion = "defecto"
    ),
    EntidadContradictoria = metrica(
      "EntidadContradictoria",
      paste0(
        "Indica si un valor del vocabulario tiene otro casi igual dentro del ",
        "mismo atributo; no modifica los datos."
      ),
      "instanciaAtributo", "booleano",
      propiedades = c("metodo", "p", "umbral", "normalizar", "max_valores", "nucleos"),
      dimension = "Unicidad", factor = "No-contradicci\u00f3n",
      metodo = .metodo_entidad_contradictoria,
      validar_propiedades = .validar_config_entidad_contradictoria,
      orientacion = "defecto"
    ),
    DesactualizacionPorFormato = metrica(
      "DesactualizacionPorFormato",
      "Indica si el formato de un valor delata que est\u00e1 desactualizado.",
      "instanciaAtributo", "booleano",
      propiedades = c("expresion_regular", "validador"),
      dimension = "Frescura", factor = "Actualidad",
      metodo = .metodo_desactualizacion_formato,
      validar_propiedades = .validar_config_desactualizacion,
      orientacion = "defecto"
    ),
    DesactualizacionPorFecha = metrica(
      "DesactualizacionPorFecha",
      "Mide en d\u00edas el atraso respecto del \u00faltimo cambio o frecuencia esperada.",
      "instanciaAtributo", "duracion", propiedades = "vigencia",
      dimension = "Frescura", factor = "Actualidad",
      metodo = .metodo_desactualizacion_fecha,
      validar_propiedades = .validar_config_vigencia,
      orientacion = "no_aplica"
    ),
    DesactualizacionPorCambios = metrica(
      "DesactualizacionPorCambios",
      "Estima cambios esperados desde la \u00faltima actualizaci\u00f3n.",
      "instanciaAtributo", "entero", propiedades = "vigencia",
      dimension = "Frescura", factor = "Actualidad",
      metodo = .metodo_desactualizacion_cambios,
      validar_propiedades = .validar_config_vigencia,
      orientacion = "no_aplica"
    ),
    OportunidadAtributoPorFecha = metrica(
      "OportunidadAtributoPorFecha",
      "Indica si el valor estuvo disponible hasta la fecha l\u00edmite inclusive.",
      "instanciaAtributo", "booleano",
      propiedades = "fecha_limite",
      dimension = "Frescura", factor = "Oportunidad",
      metodo = .metodo_oportunidad_fecha,
      validar_propiedades = .validar_config_oportunidad_fecha,
      orientacion = "conformidad"
    ),
    OportunidadAtributoPorIntervalo = metrica(
      "OportunidadAtributoPorIntervalo",
      "Indica si el valor estuvo disponible dentro del intervalo vigente.",
      "instanciaAtributo", "booleano",
      propiedades = c("inicio_vigencia", "fin_vigencia"),
      dimension = "Frescura", factor = "Oportunidad",
      metodo = .metodo_oportunidad_intervalo,
      validar_propiedades = .validar_config_oportunidad_intervalo,
      orientacion = "conformidad"
    ),
    GradoOportunidadAtributoPorFecha = metrica(
      "GradoOportunidadAtributoPorFecha",
      paste0(
        "Cuantifica el margen de utilidad entre solicitud, entrega y fin ",
        "de utilidad; extensi\u00f3n propia basada en el curso CPAP."
      ),
      "instanciaAtributo", "real",
      propiedades = c("fecha_solicitud", "fecha_fin_utilidad"),
      dimension = "Frescura", factor = "Oportunidad",
      metodo = .metodo_grado_oportunidad_fecha,
      validar_propiedades = .validar_config_grado_oportunidad_fecha,
      orientacion = "conformidad"
    ),
    GradoOportunidadAtributoPorIntervalo = metrica(
      "GradoOportunidadAtributoPorIntervalo",
      paste0(
        "Cuantifica el margen de utilidad dentro de un intervalo; ",
        "extensi\u00f3n propia basada en el curso CPAP."
      ),
      "instanciaAtributo", "real",
      propiedades = c("inicio_vigencia", "fin_vigencia"),
      dimension = "Frescura", factor = "Oportunidad",
      metodo = .metodo_grado_oportunidad_intervalo,
      validar_propiedades = .validar_config_grado_oportunidad_intervalo,
      orientacion = "conformidad"
    ),
    OportunidadEntPorFecha = metrica(
      "OportunidadEntPorFecha",
      # La descripcion decia "antes de" y el calculo es `<=`: la fila exactamente
      # en el limite sale oportuna. La gemela por atributo ya declara "hasta la
      # fecha limite inclusive" y el catalogo de la tabla 16.29 dice "entrega
      # hasta Tf, inclusive", asi que la conducta es la correcta -entregar EL dia
      # del plazo es a tiempo- y la que mentia era la descripcion publicada.
      "Indica si una entidad fue actualizada hasta su fecha l\u00edmite inclusive.",
      "instanciaEntidad", "booleano", propiedades = "vigencia",
      dimension = "Frescura", factor = "Oportunidad",
      metodo = .metodo_oportunidad_entidad_fecha,
      validar_propiedades = .validar_config_vigencia,
      orientacion = "conformidad"
    ),
    OportunidadEntPorIntervalo = metrica(
      "OportunidadEntPorIntervalo",
      "Indica si una entidad fue actualizada dentro de su intervalo vigente.",
      "instanciaEntidad", "booleano", propiedades = "vigencia",
      dimension = "Frescura", factor = "Oportunidad",
      metodo = .metodo_oportunidad_entidad_intervalo,
      validar_propiedades = .validar_config_vigencia,
      orientacion = "conformidad"
    ),
    DensidadPonderada = metrica(
      "DensidadPonderada",
      "Mide la presencia ponderada de atributos en cada fila.",
      "instanciaEntidad", "real", propiedades = "coeficientes",
      dimension = "Completitud", factor = "Densidad",
      metodo = .metodo_densidad_ponderada,
      validar_propiedades = .validar_config_densidad,
      orientacion = "conformidad"
    )
  )
}

.meta_especificaciones_metricas_catalogo <- function() {
  datos_referencial <- function() {
    data.frame(
      id = 1:3, nombre = c("A", "B", "C"),
      stringsAsFactors = FALSE
    )
  }
  datos_fecha <- function() {
    data.frame(
      actualizado = as.Date(c("2026-06-01", "2026-06-29"))
    )
  }
  contrato_referencial <- function(clave, valor = NULL, completo = FALSE) {
    argumentos <- list(datos = datos_referencial(), clave = clave)
    if (!is.null(valor)) argumentos$valor <- valor
    if (isTRUE(completo)) {
      argumentos$completo <- TRUE
      argumentos$alcance <- "padron de prueba"
    }
    list(nombre = "referencial", argumentos = argumentos)
  }
  contrato_vigencia <- function(...) {
    list(
      nombre = "vigencia",
      argumentos = c(
        list(
          columna_actualizacion = "actualizado",
          fecha_acceso = as.Date("2026-06-30")
        ),
        list(...)
      )
    )
  }

  list(
    CorrectitudSemDebil = list(
      contrato = contrato_referencial("id", valor = "nombre"),
      vinculo = list(entidad = "personas", atributos = c("id", "nombre")),
      datos = function() data.frame(
        id = c(1, 4), nombre = c("A", "X"), stringsAsFactors = FALSE
      )
    ),
    CorrectitudSemFuerte = list(
      contrato = contrato_referencial("id"),
      vinculo = list(entidad = "personas", atributos = "id"),
      datos = function() data.frame(id = c(1, 4))
    ),
    Formato = list(
      configuraciones = list(
        list(expresion_regular = "^[A-Z][0-9]$"),
        list(diccionario = c("A1", "B2")),
        list(validador = function(x) grepl("^[A-Z][0-9]$", x))
      ),
      vinculo = list(entidad = "personas", atributos = "codigo"),
      datos = function() data.frame(codigo = c("A1", "x"))
    ),
    Escala = list(
      contrato = list(
        nombre = "escala",
        argumentos = list(error = 1, tipo = "absoluto")
      ),
      vinculo = list(entidad = "personas", atributos = "edad"),
      datos = function() data.frame(edad = c(20, 30))
    ),
    ErrorEstandar = list(
      vinculo = list(entidad = "personas", atributos = "edad"),
      datos = function() data.frame(edad = c(20, 30))
    ),
    ReglaIntegridadInterEntidad = list(
      configuraciones = list(list()),
      propiedades_opcionales = "muestra",
      omisiones_opcionales = list(muestra = "Inf"),
      vinculo = list(
        entidad = c("padres", "hijos"), atributos = c("id", "id_padre")
      ),
      datos = function() list(
        padres = data.frame(id = 1:3),
        hijos = data.frame(id_padre = c(1, 2, 4))
      )
    ),
    ReglaIntegridadIntraEntidad = list(
      configuraciones = list(list(
        regla = function(d) !is.na(d$edad) & d$edad >= 18
      )),
      vinculo = list(entidad = "personas", atributos = "edad"),
      datos = function() data.frame(edad = c(17, 18))
    ),
    ValoresPosiblesPorExtension = list(
      configuraciones = list(list(valores = c("A", "B"))),
      vinculo = list(entidad = "personas", atributos = "sexo"),
      datos = function() data.frame(sexo = c("A", "C"))
    ),
    ValoresPosiblesPorComprension = list(
      configuraciones = list(
        list(predicado = function(x) !is.na(x) & x > 0),
        list(minimo = 0, maximo = 100, inclusivo = TRUE)
      ),
      vinculo = list(entidad = "personas", atributos = "edad"),
      datos = function() data.frame(edad = c(20, 30))
    ),
    RatioCobertura = list(
      contrato = contrato_referencial("id", completo = TRUE),
      vinculo = list(entidad = "personas", atributos = "id"),
      datos = function() data.frame(id = c(1, 4))
    ),
    NoNulo = list(
      configuraciones = list(list()),
      propiedades_opcionales = c("valores_nulos", "aplicable", "opcional"),
      vinculo = list(entidad = "personas", atributos = "edad"),
      datos = function() data.frame(edad = c(20, NA_real_))
    ),
    DensidadPonderada = list(
      configuraciones = list(list(coeficientes = c(edad = 1))),
      vinculo = list(entidad = "personas", atributos = "edad"),
      datos = function() data.frame(edad = c(20, NA_real_))
    ),
    AtributoDuplicado = list(
      configuraciones = list(list()),
      vinculo = list(entidad = "personas", atributos = "codigo"),
      datos = function() data.frame(codigo = c("A", "A", "B"))
    ),
    ConjuntoAtributosDuplicado = list(
      configuraciones = list(list()),
      vinculo = list(
        entidad = "personas", atributos = c("codigo", "tipo")
      ),
      datos = function() data.frame(
        codigo = c("A", "A"), tipo = c("X", "X")
      )
    ),
    EntidadDuplicada = list(
      configuraciones = list(list()),
      vinculo = list(
        entidad = "personas", atributos = c("codigo", "tipo")
      ),
      datos = function() data.frame(
        codigo = c("A", "A"), tipo = c("X", "X")
      )
    ),
    EntidadContradictoria = list(
      configuraciones = list(list()),
      paquete = "stringdist",
      ratio = list(granularidad = "atributo", agregacion = "ratio"),
      propiedades_opcionales = c(
        "metodo", "p", "umbral", "normalizar", "max_valores", "nucleos"
      ),
      incluir_propiedades_opcionales = TRUE,
      vinculo = list(entidad = "personas", atributos = "nombre"),
      datos = function() data.frame(
        nombre = c("Org Nac", "Org Nacl", "Otro"),
        stringsAsFactors = FALSE
      )
    ),
    DesactualizacionPorFecha = list(
      contrato = list(
        nombre = "vigencia",
        alternativas = list(
          contrato_vigencia(
            fecha_ultimo_cambio = as.Date("2026-06-01")
          ),
          contrato_vigencia(frecuencia_cambio = 30)
        )
      ),
      vinculo = list(entidad = "personas", atributos = "actualizado"),
      datos = datos_fecha
    ),
    DesactualizacionPorCambios = list(
      contrato = contrato_vigencia(frecuencia_cambio = 30),
      vinculo = list(entidad = "personas", atributos = "actualizado"),
      datos = datos_fecha
    ),
    DesactualizacionPorFormato = list(
      configuraciones = list(
        list(expresion_regular = "^2026-[0-9]{2}-[0-9]{2}$"),
        list(validador = function(x) !is.na(x))
      ),
      vinculo = list(entidad = "personas", atributos = "actualizado"),
      datos = function() data.frame(
        actualizado = c("2026-06-01", "viejo"), stringsAsFactors = FALSE
      )
    ),
    OportunidadAtributoPorFecha = list(
      configuraciones = list(list(fecha_limite = as.Date("2026-06-30"))),
      vinculo = list(entidad = "personas", atributos = "actualizado"),
      datos = datos_fecha
    ),
    OportunidadAtributoPorIntervalo = list(
      configuraciones = list(list(
        inicio_vigencia = as.Date("2026-01-01"),
        fin_vigencia = as.Date("2026-12-31")
      )),
      vinculo = list(entidad = "personas", atributos = "actualizado"),
      datos = datos_fecha
    ),
    OportunidadEntPorFecha = list(
      contrato = contrato_vigencia(fecha_limite = as.Date("2026-06-30")),
      vinculo = list(entidad = "personas", atributos = "actualizado"),
      datos = datos_fecha
    ),
    OportunidadEntPorIntervalo = list(
      contrato = contrato_vigencia(
        inicio_intervalo = as.Date("2026-01-01"),
        fin_intervalo = as.Date("2026-12-31")
      ),
      vinculo = list(entidad = "personas", atributos = "actualizado"),
      datos = datos_fecha
    )
  )
}

.configuraciones_especificacion <- function(meta) {
  contrato <- meta$contrato
  if (!is.null(contrato) && identical(contrato$nombre, "escala")) {
    return(list(list(escala = do.call(escala, contrato$argumentos))))
  }
  if (!is.null(contrato) && identical(contrato$nombre, "vigencia")) {
    contratos <- if (is.null(contrato$alternativas)) {
      list(contrato)
    } else {
      contrato$alternativas
    }
    return(lapply(contratos, function(x) {
      list(vigencia = do.call(vigencia, x$argumentos))
    }))
  }
  configuraciones <- meta$configuraciones
  if (is.null(configuraciones)) list(list()) else configuraciones
}

.validar_especificacion_metrica <- function(meta, metrica, configuraciones) {
  declaracion <- attr(metrica, "declaracion", exact = TRUE)
  validador <- attr(metrica, "validador_propiedades", exact = TRUE)
  for (configuracion in configuraciones) {
    if (!is.list(configuracion)) {
      .detener("Cada configuracion de receta debe ser una lista.", call. = FALSE)
    }
    if (!is.null(validador)) {
      validada <- validador(configuracion)
      if (!is.list(validada)) {
        .detener("El validador de la metrica no devolvio una lista.", call. = FALSE)
      }
    } else {
      .validar_propiedades_base(configuracion, declaracion$propiedades)
    }
  }
  if (is.null(meta$vinculo) ||
      !all(c("entidad", "atributos") %in% names(meta$vinculo))) {
    .detener("La especificacion debe declarar el vinculo de la metrica.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.contrato_ruta_especificacion <- function(meta, configuracion, indice) {
  contrato <- meta$contrato
  if (is.null(contrato)) return(NULL)
  definicion <- if (is.null(contrato$alternativas)) {
    contrato
  } else {
    contrato$alternativas[[indice]]
  }
  objeto <- if (identical(contrato$nombre, "referencial")) {
    do.call(referencial, definicion$argumentos)
  } else if (!is.null(configuracion[[contrato$nombre]])) {
    configuracion[[contrato$nombre]]
  } else {
    do.call(get(contrato$nombre, mode = "function"), definicion$argumentos)
  }
  destino <- if (contrato$nombre %in% c("vigencia", "escala")) {
    list(funcion = "especializar", argumento = contrato$nombre)
  } else {
    list(funcion = "instanciar", argumento = contrato$nombre)
  }
  list(
    funcion = contrato$nombre,
    argumentos = definicion$argumentos,
    objeto = objeto,
    destino = destino,
    salida = "contrato"
  )
}

.pasos_ruta_especificacion <- function(meta, metrica, configuracion, indice) {
  contrato <- .contrato_ruta_especificacion(meta, configuracion, indice)
  especializar_args <- c(list(metrica = metrica), configuracion)
  propiedades <- setdiff(names(configuracion), "vigencia")
  if (!is.null(contrato) && contrato$destino$funcion == "especializar") {
    especializar_args[[contrato$destino$argumento]] <- contrato$objeto
    propiedades <- setdiff(propiedades, contrato$destino$argumento)
  }
  opcionales <- if (is.null(meta$propiedades_opcionales)) {
    character()
  } else {
    meta$propiedades_opcionales
  }
  paso_especializar <- list(
    funcion = "especializar",
    argumentos = especializar_args,
    propiedades = list(
      obligatorias = propiedades,
      opcionales = opcionales
    ),
    contrato = if (is.null(contrato) ||
        contrato$destino$funcion != "especializar") NULL else
      list(funcion = contrato$funcion,
           argumento = contrato$destino$argumento),
    salida = "metrica_especifica"
  )
  argumentos_instanciar <- list(
    entidad = meta$vinculo$entidad,
    atributos = meta$vinculo$atributos
  )
  if (!is.null(contrato) && contrato$destino$funcion == "instanciar") {
    argumentos_instanciar[[contrato$destino$argumento]] <- NULL
  }
  paso_instanciar <- list(
    funcion = "instanciar",
    argumentos = argumentos_instanciar,
    entrada = "metrica_especifica",
    contrato = if (is.null(contrato) ||
        contrato$destino$funcion != "instanciar") NULL else
      list(funcion = contrato$funcion,
           argumento = contrato$destino$argumento),
    granularidad = attr(metrica, "declaracion", exact = TRUE)$granularidad,
    atributos_referencial = if (!is.null(contrato) &&
        identical(contrato$funcion, "referencial") &&
        attr(metrica, "declaracion", exact = TRUE)$granularidad %in%
          c("instanciaAtributo", "atributo")) {
      argumentos <- contrato$argumentos
      roles <- list(clave = length(argumentos$clave))
      if (length(argumentos$valor)) roles$valor <- length(argumentos$valor)
      roles
    } else NULL,
    salida = "instancia"
  )
  pasos <- list()
  if (!is.null(contrato)) pasos[[length(pasos) + 1L]] <- contrato
  pasos[[length(pasos) + 1L]] <- paso_especializar
  pasos[[length(pasos) + 1L]] <- paso_instanciar
  pasos[[length(pasos) + 1L]] <- list(
    funcion = "medir",
    entrada = "instancia",
    datos = meta$datos,
    salida = "medicion"
  )
  if (!is.null(meta$ratio)) {
    pasos[[length(pasos) + 1L]] <- list(
      funcion = "agregar",
      entrada = "medicion",
      argumentos = list(
        destino = meta$ratio$granularidad,
        funcion = meta$ratio$agregacion
      ),
      salida = "medicion"
    )
  }
  pasos
}

.pasos_especificacion <- function(meta, metrica, configuraciones) {
  lapply(seq_along(configuraciones), function(indice) {
    .pasos_ruta_especificacion(
      meta, metrica, configuraciones[[indice]], indice
    )
  })
}

.ejecutar_pasos_especificacion <- function(pasos) {
  objetos <- new.env(parent = emptyenv())
  for (paso in pasos) {
    if (identical(paso$funcion, "referencial") ||
        identical(paso$funcion, "vigencia") ||
        identical(paso$funcion, "escala")) {
      valor <- do.call(
        get(paso$funcion, mode = "function"), paso$argumentos
      )
    } else if (identical(paso$funcion, "especializar")) {
      argumentos <- paso$argumentos
      if (!is.null(paso$contrato)) {
        argumentos[[paso$contrato$argumento]] <- get(
          "contrato", envir = objetos, inherits = FALSE
        )
      }
      valor <- do.call(especializar, argumentos)
    } else if (identical(paso$funcion, "instanciar")) {
      argumentos <- paso$argumentos
      argumentos$metrica_especifica <- get(
        paso$entrada, envir = objetos, inherits = FALSE
      )
      if (!is.null(paso$contrato)) {
        argumentos[[paso$contrato$argumento]] <- get(
          "contrato", envir = objetos, inherits = FALSE
        )
      }
      valor <- do.call(instanciar, argumentos)
    } else if (identical(paso$funcion, "medir")) {
      instrumento <- get(paso$entrada, envir = objetos, inherits = FALSE)
      valor <- medir(modelo(instrumento), paso$datos())
    } else if (identical(paso$funcion, "agregar")) {
      valor <- do.call(
        agregar,
        c(
          list(medidas = get(paso$entrada, envir = objetos, inherits = FALSE)),
          paso$argumentos
        )
      )
    } else {
      .detener(
        "Funcion no contemplada en los pasos de la receta: ",
        paso$funcion, ".", call. = FALSE
      )
    }
    assign(paso$salida, valor, envir = objetos)
  }
  get("medicion", envir = objetos, inherits = FALSE)
}

.cache_especificaciones_metricas_catalogo <- new.env(parent = emptyenv())

.especificaciones_metricas_catalogo <- function(catalogo = NULL) {
  if (is.null(catalogo)) catalogo <- catalogo_agesic()
  if (exists("catalogo", envir = .cache_especificaciones_metricas_catalogo,
             inherits = FALSE) &&
      identical(
        catalogo,
        get("catalogo", envir = .cache_especificaciones_metricas_catalogo)
      )) {
    return(get(
      "especificaciones", envir = .cache_especificaciones_metricas_catalogo
    ))
  }
  nucleo <- metricas_nucleo()
  referenciales <- metricas_referencial()
  meta <- .meta_especificaciones_metricas_catalogo()
  nombres <- .identificadores_unicos(catalogo$metrica_lupa[
    !is.na(catalogo$metrica_lupa)
  ])
  faltantes <- setdiff(nombres, names(meta))
  if (length(faltantes)) {
    .detener(
      "Faltan especificaciones para las metricas del catalogo: ",
      paste(faltantes, collapse = ", "), ".", call. = FALSE
    )
  }
  salida <- lapply(nombres, function(nombre) {
    es_referencial <- nombre %in% names(referenciales)
    metricas <- if (es_referencial) referenciales else nucleo
    metrica <- metricas[[nombre]]
    if (is.null(metrica)) {
      .detener("La metrica del catalogo no existe: ", nombre, ".", call. = FALSE)
    }
    declaracion <- attr(metrica, "declaracion", exact = TRUE)
    configuraciones <- .configuraciones_especificacion(meta[[nombre]])
    .validar_especificacion_metrica(meta[[nombre]], metrica, configuraciones)
    indices <- which(catalogo$metrica_lupa == nombre)
    indice <- indices[[1L]]
    propiedades <- propiedades_metrica(metrica)$propiedad
    contrato <- meta[[nombre]]$contrato
    if (!is.null(contrato)) {
      funcion <- get(contrato$nombre, mode = "function")
      formales <- names(formals(funcion))
      argumentos <- if (!is.null(contrato$alternativas)) {
        unlist(lapply(contrato$alternativas, function(x) names(x$argumentos)))
      } else {
        names(contrato$argumentos)
      }
      faltan_formales <- setdiff(unique(argumentos), formales)
      if (length(faltan_formales)) {
        .detener(
          "El contrato de ", nombre, " usa argumentos no declarados: ",
          paste(faltan_formales, collapse = ", "), ".", call. = FALSE
        )
      }
      contrato$formales <- formales
    }
    estructura <- meta[[nombre]]
    estructura$nombre <- nombre
    estructura$catalogo <- if (es_referencial) {
      "metricas_referencial"
    } else {
      "metricas_nucleo"
    }
    estructura$metrica <- metrica
    estructura$declaracion <- declaracion
    estructura$dimension <- as.character(catalogo$dimension[[indice]])
    estructura$factor <- as.character(catalogo$factor[[indice]])
    estructura$propiedades <- propiedades
    estructura$validador_propiedades <- attr(
      metrica, "validador_propiedades", exact = TRUE
    )
    estructura$configuraciones <- configuraciones
    estructura$contrato <- contrato
    estructura$pasos <- .pasos_especificacion(
      meta[[nombre]], metrica, configuraciones
    )
    estructura$operaciones <- c("especializar", "instanciar", "medir")
    estructura$propiedad_grupos <- lapply(configuraciones, names)
    estructura$propiedad_grupos <- estructura$propiedad_grupos[
      lengths(estructura$propiedad_grupos) > 0L
    ]
    if (!is.null(contrato) && identical(contrato$nombre, "vigencia")) {
      estructura$propiedad_grupos <- lapply(
        estructura$propiedad_grupos,
        function(grupo) setdiff(grupo, "vigencia")
      )
      estructura$propiedad_grupos <- estructura$propiedad_grupos[
        lengths(estructura$propiedad_grupos) > 0L
      ]
    }
    if (isTRUE(estructura$incluir_propiedades_opcionales)) {
      estructura$propiedad_grupos <- list(propiedades)
    }
    opcionales_texto <- unique(unlist(lapply(
      estructura$pasos,
      function(pasos) .pasos_de_funcion(pasos, "especializar")[[1L]]$propiedades$opcionales
    ), use.names = FALSE))
    estructura$tokens_obligatorios <- unique(c(
      estructura$operaciones,
      if (is.null(contrato)) character() else {
        argumentos <- if (!is.null(contrato$alternativas)) {
          unlist(lapply(contrato$alternativas, function(x) names(x$argumentos)))
        } else {
          names(contrato$argumentos)
        }
        setdiff(unique(argumentos), c("datos", "columna_actualizacion"))
      },
      unlist(estructura$propiedad_grupos, use.names = FALSE),
      opcionales_texto
    ))
    estructura
  })
  salida <- stats::setNames(salida, nombres)
  assign(
    "catalogo", catalogo,
    envir = .cache_especificaciones_metricas_catalogo
  )
  assign(
    "especificaciones", salida,
    envir = .cache_especificaciones_metricas_catalogo
  )
  salida
}

.codigo_metrica_especificacion <- function(especificacion) {
  paste0(
    "`", especificacion$catalogo, "()$", especificacion$nombre, "`"
  )
}

.codigo_lista_especificacion <- function(nombres) {
  ultimo <- nombres[[length(nombres)]]
  # "e" solo delante de lo que empieza con el sonido i ("i", "hi" sin
  # diptongo: "inclusivo"); delante de todo lo demas, "y".
  conjuncion <- if (grepl("^(i|hi)([^aeiou]|$)", ultimo, useBytes = TRUE)) {
    " e "
  } else {
    " y "
  }
  nombres <- paste0("`", nombres, "`")
  if (length(nombres) == 1L) return(nombres)
  paste0(
    paste(nombres[-length(nombres)], collapse = ", "),
    conjuncion, nombres[[length(nombres)]]
  )
}

.texto_propiedades_especificacion <- function(especificacion) {
  rutas <- especificacion$pasos
  grupos <- lapply(rutas, function(pasos) {
    paso <- pasos[[which(vapply(
      pasos, function(x) identical(x$funcion, "especializar"), logical(1L)
    ))]]
    paso$propiedades$obligatorias
  })
  grupos <- grupos[lengths(grupos) > 0L]
  if (!length(grupos)) return(character())
  grupos <- unique(vapply(
    grupos, .codigo_lista_especificacion, character(1L)
  ))
  if (length(grupos) == 1L) return(grupos)
  paste(grupos, collapse = ", o bien ")
}

.texto_vinculo_pasos <- function(pasos) {
  paso <- pasos[[which(vapply(
    pasos, function(x) identical(x$funcion, "instanciar"), logical(1L)
  ))]]
  granularidad <- paso$granularidad
  n_entidades <- length(paso$argumentos$entidad)
  n_atributos <- length(paso$argumentos$atributos)
  if (n_entidades > 1L) return("dos entidades y sus claves")
  if (length(paso$atributos_referencial)) {
    roles <- names(paso$atributos_referencial)
    nombres <- paste0("`", roles, "`")
    if (length(roles) == 1L) {
      return(paste0("un atributo por cada columna de ", nombres))
    }
    return(paste0(
      "un atributo por cada columna de ",
      paste(nombres, collapse = " y de "), ", en ese orden"
    ))
  }
  if (granularidad %in% c("instanciaAtributo", "atributo")) {
    if (n_atributos > 1L) {
      return("varios atributos")
    }
    return("un atributo")
  }
  if (identical(granularidad, "entidad")) return("una entidad y sus atributos")
  "una entidad y sus atributos"
}

.argumentos_contrato_paso <- function(paso) {
  argumentos <- paso$argumentos
  nombres <- setdiff(names(argumentos), c("datos", "columna_actualizacion"))
  if (!length(nombres)) return(character())
  texto <- .codigo_lista_especificacion(nombres)
  if ("completo" %in% nombres && isTRUE(argumentos$completo)) {
    texto <- sub(
      "`completo`", "`completo` = TRUE", texto, fixed = TRUE
    )
  }
  texto
}

.pasos_de_funcion <- function(pasos, funcion) {
  pasos[vapply(pasos, function(x) identical(x$funcion, funcion), logical(1L))]
}

.texto_contrato_pasos <- function(especificacion) {
  contratos <- lapply(especificacion$pasos, function(pasos) {
    candidatos <- Filter(function(x) x$funcion %in% c(
      "referencial", "vigencia", "escala"
    ), pasos)
    if (length(candidatos)) candidatos[[1L]] else NULL
  })
  contratos <- Filter(Negate(is.null), contratos)
  if (!length(contratos)) return(character())
  nombre <- contratos[[1L]]$funcion
  argumentos <- unique(vapply(
    contratos, .argumentos_contrato_paso, character(1L)
  ))
  detalle <- paste(argumentos, collapse = " o bien ")
  if (identical(nombre, "referencial")) {
    return(paste0(
      "Crear `referencial()` con ",
      detalle, "."
    ))
  }
  if (identical(nombre, "vigencia")) {
    return(paste0("Crear `vigencia()` con ", detalle, "."))
  }
  if (identical(nombre, "escala")) {
    return(paste0(
      "Declarar `escala()` con `error`; `tipo` es opcional."
    ))
  }
  character()
}

.texto_opcionales_pasos <- function(especificacion) {
  opcionales <- unique(unlist(lapply(especificacion$pasos, function(pasos) {
    especializar <- .pasos_de_funcion(pasos, "especializar")[[1L]]
    especializar$propiedades$opcionales
  }), use.names = FALSE))
  if (!length(opcionales)) return(character())
  texto <- .codigo_lista_especificacion(opcionales)
  omisiones <- especificacion$omisiones_opcionales
  if (!is.null(omisiones) && "muestra" %in% opcionales) {
    return("`muestra` es opcional (por omisi\u00f3n Inf)")
  }
  paste0("las propiedades ", texto, " son opcionales")
}

.texto_destino_contrato_pasos <- function(pasos, funcion) {
  especializar <- .pasos_de_funcion(pasos, "especializar")[[1L]]
  instanciar <- .pasos_de_funcion(pasos, "instanciar")[[1L]]
  if (!is.null(especializar$contrato) &&
      identical(especializar$contrato$funcion, funcion)) {
    return(paste0(" con `", funcion, "`"))
  }
  if (!is.null(instanciar$contrato) &&
      identical(instanciar$contrato$funcion, funcion)) {
    return(paste0(" con `", funcion, "`"))
  }
  character()
}

.texto_especificacion_metrica <- function(especificacion) {
  pasos <- especificacion$pasos[[1L]]
  propiedad <- .texto_propiedades_especificacion(especificacion)
  metrica <- .codigo_metrica_especificacion(especificacion)
  contrato <- .texto_contrato_pasos(especificacion)
  inicio <- contrato
  if (length(inicio)) inicio <- paste0(inicio, " ")
  especializar <- paste0("Aplicar `especializar()` a ", metrica)
  if (length(propiedad)) {
    especializar <- paste0(especializar, " con ", propiedad)
  }
  destino <- unique(c(
    .texto_destino_contrato_pasos(pasos, "vigencia"),
    .texto_destino_contrato_pasos(pasos, "escala")
  ))
  if (length(destino)) especializar <- paste0(especializar, destino[[1L]])
  opcionales <- .texto_opcionales_pasos(especificacion)
  if (length(opcionales)) especializar <- paste0(especializar, "; ", opcionales)
  pasar <- .texto_destino_contrato_pasos(pasos, "referencial")
  vinculo <- .texto_vinculo_pasos(pasos)
  separador <- if (length(pasar) && grepl(
      "en ese orden", vinculo, fixed = TRUE, useBytes = TRUE
  )) "," else ""
  texto <- paste0(
    inicio, especializar, "; luego `instanciar()` sobre ",
    vinculo, separador, pasar,
    "; despu\u00e9s `medir()`."
  )
  agregar <- .pasos_de_funcion(pasos, "agregar")
  if (length(agregar)) {
    argumentos <- agregar[[1L]]$argumentos
    texto <- paste0(
      texto, " Si se comparan varios valores distintos, se necesita stringdist;",
      " despu\u00e9s, `agregar()` con destino \"", argumentos$destino,
      "\" y agregaci\u00f3n \"", argumentos$funcion,
      "\" resume la medida."
    )
  }
  texto
}

.texto_factor_especificaciones <- function(especificaciones) {
  if (!length(especificaciones)) return(character())
  paste(
    vapply(especificaciones, .texto_especificacion_metrica, character(1L)),
    collapse = " "
  )
}

.identificadores_metricas_texto <- function(texto) {
  encontrados <- regmatches(
    texto,
    gregexpr(
      "`?[A-Za-z][A-Za-z0-9_]*\\(\\)\\$[A-Za-z][A-Za-z0-9_]*`?",
      texto, perl = TRUE, useBytes = TRUE
    )
  )[[1L]]
  if (identical(encontrados, -1L)) return(character())
  sub("`$", "", sub("^`", "", encontrados))
}

.errores_identificadores_metricas <- function(texto, especificaciones) {
  publicados <- .identificadores_metricas_texto(texto)
  conocidos <- unique(vapply(
    especificaciones,
    function(x) paste0(x$catalogo, "()$", x$nombre),
    character(1L)
  ))
  faltantes <- setdiff(publicados, conocidos)
  if (!length(faltantes)) return(character())
  paste0("La receta publica metricas inexistentes: ",
         paste(faltantes, collapse = ", "), ".")
}

.errores_destinos_contrato_texto <- function(texto) {
  errores <- character()
  usos_invalidos <- list(
    c("vigencia", "instanciar"),
    c("escala", "instanciar"),
    c("referencial", "especializar")
  )
  for (uso in usos_invalidos) {
    patron <- paste0(
      "`?", uso[[1L]], "`?[[:space:]]+a[[:space:]]+`?",
      uso[[2L]], "\\(\\)`?"
    )
    if (grepl(patron, texto, perl = TRUE, useBytes = TRUE)) {
      errores <- c(
        errores,
        paste0("El texto pasa `", uso[[1L]], "` a `", uso[[2L]], "()`."))
    }
  }
  errores
}

.errores_texto_especificacion <- function(texto, especificacion) {
  errores <- character()
  exigir <- function(condicion, mensaje) {
    if (!isTRUE(condicion)) errores <<- c(errores, mensaje)
  }
  for (indice in seq_along(especificacion$pasos)) {
    pasos <- especificacion$pasos[[indice]]
    exigir(
      grepl(.codigo_metrica_especificacion(especificacion), texto,
            fixed = TRUE, useBytes = TRUE),
      paste0(especificacion$nombre, " no publica la metrica.")
    )
    for (paso in pasos) {
      funcion <- paso$funcion
      exigir(
        grepl(paste0("`", funcion, "()"), texto,
              fixed = TRUE, useBytes = TRUE),
        paste0(especificacion$nombre, " no publica ", funcion, "().")
      )
    }
    especializar <- .pasos_de_funcion(pasos, "especializar")[[1L]]
    propiedades <- especializar$propiedades
    for (nombre in propiedades$obligatorias) {
      exigir(
        grepl(paste0("`", nombre, "`"), texto,
              fixed = TRUE, useBytes = TRUE),
        paste0(especificacion$nombre, " omite la propiedad ", nombre, ".")
      )
    }
    if (length(propiedades$opcionales)) {
      for (nombre in propiedades$opcionales) {
        exigir(
          grepl(paste0("`", nombre, "`"), texto,
                fixed = TRUE, useBytes = TRUE),
          paste0(especificacion$nombre, " omite la propiedad opcional ",
                 nombre, ".")
        )
      }
    }
    for (paso in pasos) {
      if (identical(paso$funcion, "referencial") ||
          identical(paso$funcion, "vigencia") ||
          identical(paso$funcion, "escala")) {
        for (nombre in setdiff(
          names(paso$argumentos), c("datos", "columna_actualizacion")
        )) {
          exigir(
            grepl(paste0("`", nombre, "`"), texto,
                  fixed = TRUE, useBytes = TRUE),
            paste0(especificacion$nombre, " omite el argumento ", nombre,
                   " del contrato.")
          )
        }
      }
    }
    vinculo <- .texto_vinculo_pasos(pasos)
    exigir(
      grepl(vinculo, texto, fixed = TRUE, useBytes = TRUE),
      paste0(especificacion$nombre, " no publica el vinculo ", vinculo, ".")
    )
    contrato <- .pasos_de_funcion(pasos, "especializar")[[1L]]$contrato
    inicio_especializar <- regexpr(
      "`especializar()`", texto, fixed = TRUE, useBytes = TRUE
    )[[1L]]
    inicio_instanciar <- regexpr(
      "`instanciar()`", texto, fixed = TRUE, useBytes = TRUE
    )[[1L]]
    if (!is.null(contrato)) {
      nombre <- contrato$funcion
      posicion <- regexpr(
        paste0("`", nombre, "`"), texto, fixed = TRUE, useBytes = TRUE
      )[[1L]]
      apariciones <- gregexpr(
        paste0("`", nombre, "`"), texto, fixed = TRUE, useBytes = TRUE
      )[[1L]]
      exigir(
        length(apariciones) == 1L && posicion > inicio_especializar &&
          posicion < inicio_instanciar,
        paste0(especificacion$nombre, " no pasa ", nombre,
               " a especializar().")
      )
    }
    instanciar <- .pasos_de_funcion(pasos, "instanciar")[[1L]]
    if (!is.null(instanciar$contrato)) {
      nombre <- instanciar$contrato$funcion
      posicion <- regexpr(
        paste0("`", nombre, "`"), texto, fixed = TRUE, useBytes = TRUE
      )[[1L]]
      apariciones <- gregexpr(
        paste0("`", nombre, "`"), texto, fixed = TRUE, useBytes = TRUE
      )[[1L]]
      exigir(
        length(apariciones) == 1L && posicion > inicio_instanciar &&
          grepl(
            paste0("con `", nombre, "`"), texto,
            fixed = TRUE, useBytes = TRUE
          ),
        paste0(especificacion$nombre, " no pasa ", nombre,
               " a instanciar().")
      )
    }
    agregar <- .pasos_de_funcion(pasos, "agregar")
    if (length(agregar)) {
      argumentos <- agregar[[1L]]$argumentos
      exigir(
        grepl(paste0("\"", argumentos$destino, "\""), texto,
              fixed = TRUE, useBytes = TRUE) &&
          grepl(paste0("\"", argumentos$funcion, "\""), texto,
                fixed = TRUE, useBytes = TRUE),
        paste0(especificacion$nombre,
               " no publica el destino y la agregacion.")
      )
    }
  }
  unique(errores)
}

.errores_textos_catalogo <- function(
    recetas = NULL, marco = NULL, especificaciones = NULL
) {
  if (is.null(especificaciones)) {
    especificaciones <- .especificaciones_metricas_catalogo()
  }
  if (is.null(recetas)) {
    recetas <- .recetas_metricas_catalogo(especificaciones)
  }
  if (is.null(marco)) marco <- marco_agesic()
  errores <- character()
  for (nombre in names(especificaciones)) {
    errores <- c(
      errores,
      .errores_texto_especificacion(recetas[[nombre]], especificaciones[[nombre]])
    )
  }
  catalogo <- catalogo_agesic()
  claves <- .clave_par_identificador(catalogo$dimension, catalogo$factor)
  claves_marco <- .clave_par_identificador(
    marco$factores$dimension, marco$factores$factor
  )
  for (i in seq_len(nrow(marco$factores))) {
    nombres <- .identificadores_unicos(catalogo$metrica_lupa[
      claves == claves_marco[[i]] &
        as.character(catalogo$estado) %in% c("implementada", "via_agregacion")
    ])
    errores <- c(
      errores,
      .errores_identificadores_metricas(
        marco$factores$como_resolverlo[[i]], especificaciones
      )
    )
    errores <- c(
      errores,
      .errores_destinos_contrato_texto(
        marco$factores$como_resolverlo[[i]]
      )
    )
    for (nombre in nombres) {
      receta <- recetas[[nombre]]
      if (!grepl(receta, marco$factores$como_resolverlo[[i]],
                 fixed = TRUE, useBytes = TRUE)) {
        errores <- c(
          errores,
          paste0(
            nombre, " no publica su receta individual completa en el factor."
          )
        )
      }
      errores <- c(
        errores,
        .errores_texto_especificacion(receta, especificaciones[[nombre]])
      )
    }
  }
  unique(errores)
}

.recetas_metricas_catalogo <- function(especificaciones = NULL) {
  if (is.null(especificaciones)) especificaciones <-
    .especificaciones_metricas_catalogo()
  textos <- vapply(
    especificaciones, .texto_especificacion_metrica, character(1L)
  )
  stats::setNames(textos, names(especificaciones))
}

.medir_especificacion_catalogo <- function(especificacion, indice = 1L) {
  .ejecutar_pasos_especificacion(especificacion$pasos[[indice]])
}

#' Correspondencia con el catálogo de métricas de AGESIC
#'
#' Devuelve las 49 entradas del catálogo del marco y explicita qué parte está
#' disponible en `lupa`. Los ratios no se duplican como métricas: aparecen con
#' estado `"via_agregacion"` y con la llamada que los materializa.
#'
#' @return Data frame con una fila por entrada y las columnas `numero`,
#'   `dimension`, `factor`, `metrica_agesic`, `clase_catalogo`, `estado`,
#'   `motivo`, `metrica_lupa`, `implementacion` y `observacion`. `estado` y
#'   `motivo` son factores.
#'
#' @details
#' `estado` responde qué disponibilidad tiene cada entrada:
#'
#' - `implementada`: existe un motor ejecutable, completo o parcial;
#' - `via_agregacion`: se obtiene agregando una métrica base;
#' - `pendiente`: es automatizable, pero el motor necesario aún no existe;
#' - `fuera_de_alcance`: se decidió no implementarla en el alcance tabular de
#'   esta versión, por ejemplo métricas geográficas o de imágenes.
#'
#' `motivo` separa la causa o el matiz: `semantica_completa`,
#' `semantica_parcial`, `agregacion`, `requiere_referencial`,
#' `requiere_configuracion`, `motor_pendiente` o `decision_alcance`. Así una
#' métrica ejecutable que necesita un padrón externo no se confunde con una
#' métrica cuyo motor falta. `observacion` explica la situación concreta de
#' cada una de las 49 entradas y nunca queda vacía.
#'
#' `Escala` se clasifica como implementada con configuración experta mediante
#' [escala()], no como referencial. `DesactualizacionPorFecha`,
#' `DesactualizacionPorCambios` y las oportunidades de entidad requieren un
#' contrato [vigencia()]. `ErrorEstandar` sigue la semántica literal de la tabla
#' 16.5 y devuelve desviación estándar, aunque su nombre pueda sugerir el error
#' estándar de la media.
#'
#' La implementación de `ReglaIntegridadInterEntidad` se declara parcial:
#' calcula cobertura PK/FK, pero todavía no materializa la regla booleana de
#' inclusión o expresión condicional con granularidad `conjuntoEntidades` que
#' define el marco. `OportunidadAtributo*`, en cambio, sigue las tablas 16.29 y
#' 16.30 con resultado booleano. Las variantes continuas del curso CPAP se
#' conservan como `GradoOportunidadAtributo*` y no figuran como entradas del
#' catálogo. `RatioDensidadPonderada` usa `ratio_umbral`, porque su medida base
#' es real y `ratio` sólo es válido para medidas booleanas.
#'
#' `implementacion` publica una receta, no una expresión que se evalúe sola: en
#' las entradas `via_agregacion`, `m` es la medición ya tomada de la métrica
#' base que nombra `metrica_lupa` y `u` el umbral que elige quien agrega. Son
#' los dos únicos nombres libres que aparecen, y ninguno se inventa aquí: `m`
#' sale de [medir()] y `u` es una decisión de política —cuánta densidad se
#' considera suficiente— que el paquete no toma por nadie.
#'
#' @references [AGESIC (2020)](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/).
#'   *Marco de trabajo para la Gestión de la Calidad
#'   de Datos en Gobierno Digital*, versión 1.6, capítulo 16, Presidencia de la
#'   República, Uruguay.
#'
#' @export
#' @seealso [metricas_nucleo()], [metricas_referencial()], [agregar()]
#'
#' @examples
#' catalogo <- catalogo_agesic()
#' subset(catalogo, estado == "via_agregacion")
#' table(catalogo$estado, catalogo$motivo)
catalogo_agesic <- function() {
  metricas <- c(
    "CorrectitudSemDebil",
    "CorrectitudSemFuerte",
    "RatioCorrectitudSemFuerte",
    "RatioCorrectitudSemD\u00e9bil",
    "Formato",
    "Formato(Pais, ISOAlpha3)",
    "Formato(Enfermedad, CIE10) [DA:\u00abSalud\u00bb]",
    "Formato(NumeroDocumento, DNIC)",
    "Escala",
    "ErrorEstandar",
    "\u00cdndiceErroresPosicionalesPorUmbral",
    "ValorMedioIncertidumbrePosicional",
    "ErrorHorizontalRelativo",
    "IndiceFidelidadConReferencia",
    "IndiceFidelidadSinReferencia",
    "IndiceFidelidadReferenciaReducida",
    "ReglaIntegridadInterEntidad",
    "ReglaEspacial [TD:\u00abGeo\u00bb]",
    "ReglaIntegridadInterEntidad(Sexo, Enfermedad) [DA:\u00abSalud\u00bb]",
    "ReglaIntegridadIntraEntidad",
    "RatioIntegridadIntraEntidad",
    "ReglaIntegridadIntraEntidad(Sexo,Enfermedad) [DA:\u00abSalud\u00bb]",
    "ValoresPosiblesPorExtensi\u00f3n",
    "ValoresPosiblesPorComprensi\u00f3n",
    "ValoresPosiblesPE(Sexo, AGESIC)",
    "\u00cdndiceFallosConexi\u00f3nNodosEnlace",
    "RatioCobertura",
    "NoNulo",
    "DensidadPonderada",
    "RatioNoNulos",
    "RatioDensidadPonderada",
    "\u00cdtemExcedente",
    "\u00cdndiceItemsExcedentes",
    "AtributoDuplicado",
    "ConjuntoAtributosDuplicado",
    "EntidadDuplicada",
    "RatioAtributoDuplicado",
    "RatioConjuntoAtributosDuplicado",
    "RatioEntidadesDuplicadas",
    "EntidadContradictoria",
    "RatioEntidadContradictoria",
    "Desactualizaci\u00f3nPorFecha",
    "Desactualizaci\u00f3nPorCambios",
    "Desactualizaci\u00f3nPorFormato",
    "Desactualizaci\u00f3nPorFormato(TelefonoFijo, FormatoPNN1)",
    "OportunidadAtributoPorFecha",
    "OportunidadAtributoPorIntervalo",
    "OportunidadEntPorFecha",
    "OportunidadEntPorIntervalo"
  )
  dimension <- c(
    rep("Exactitud", 16L), rep("Consistencia", 10L),
    rep("Completitud", 7L), rep("Unicidad", 8L), rep("Frescura", 8L)
  )
  factor <- c(
    rep("Correctitud sem\u00e1ntica", 4L),
    rep("Correctitud sint\u00e1ctica", 4L),
    rep("Precisi\u00f3n", 2L), rep("Exactitud posicional absoluta", 2L),
    "Exactitud posicional relativa", rep("Fidelidad", 3L),
    rep("Integridad inter-entidad", 3L),
    rep("Integridad intra-entidad", 3L),
    rep("Integridad de dominio", 3L), "Consistencia topol\u00f3gica",
    "Cobertura", rep("Densidad", 4L), rep("Comisi\u00f3n", 2L),
    rep("No-duplicaci\u00f3n", 6L), rep("No-contradicci\u00f3n", 2L),
    rep("Actualidad", 4L), rep("Oportunidad", 4L)
  )
  clase <- rep("generica", 49L)
  clase[c(6:8, 19L, 22L, 25L, 45L)] <- "especifica"
  clase[c(3:4, 21L, 30:31, 37:39, 41L)] <- "agregada"

  implementadas <- c(
    1:2, 5:10, 17L, 20L, 22:25, 27:29, 34:36, 40L, 42:49
  )
  agregadas <- c(3:4, 21L, 30:31, 37:39, 41L)
  pendientes <- 19L
  fuera <- setdiff(seq_len(49L), c(implementadas, agregadas, pendientes))
  estado <- rep(NA_character_, 49L)
  estado[implementadas] <- "implementada"
  estado[agregadas] <- "via_agregacion"
  estado[pendientes] <- "pendiente"
  estado[fuera] <- "fuera_de_alcance"

  motivo <- rep(NA_character_, 49L)
  motivo[c(5L, 10L, 28L, 34:36)] <- "semantica_completa"
  motivo[17L] <- "semantica_parcial"
  motivo[agregadas] <- "agregacion"
  motivo[c(1:2, 27L)] <- "requiere_referencial"
  motivo[c(6:9, 20L, 22:25, 29L, 40L, 42:49)] <- "requiere_configuracion"
  motivo[pendientes] <- "motor_pendiente"
  motivo[fuera] <- "decision_alcance"

  metrica_lupa <- rep(NA_character_, 49L)
  metrica_lupa[1L] <- "CorrectitudSemDebil"
  metrica_lupa[2L] <- "CorrectitudSemFuerte"
  metrica_lupa[c(5:8)] <- "Formato"
  metrica_lupa[9L] <- "Escala"
  metrica_lupa[10L] <- "ErrorEstandar"
  metrica_lupa[17L] <- "ReglaIntegridadInterEntidad"
  metrica_lupa[c(20L, 22L)] <- "ReglaIntegridadIntraEntidad"
  metrica_lupa[c(23L, 25L)] <- "ValoresPosiblesPorExtension"
  metrica_lupa[24L] <- "ValoresPosiblesPorComprension"
  metrica_lupa[27L] <- "RatioCobertura"
  metrica_lupa[28L] <- "NoNulo"
  metrica_lupa[29L] <- "DensidadPonderada"
  metrica_lupa[34L] <- "AtributoDuplicado"
  metrica_lupa[35L] <- "ConjuntoAtributosDuplicado"
  metrica_lupa[36L] <- "EntidadDuplicada"
  metrica_lupa[40L] <- "EntidadContradictoria"
  metrica_lupa[42L] <- "DesactualizacionPorFecha"
  metrica_lupa[43L] <- "DesactualizacionPorCambios"
  metrica_lupa[c(44L, 45L)] <- "DesactualizacionPorFormato"
  metrica_lupa[46L] <- "OportunidadAtributoPorFecha"
  metrica_lupa[47L] <- "OportunidadAtributoPorIntervalo"
  metrica_lupa[48L] <- "OportunidadEntPorFecha"
  metrica_lupa[49L] <- "OportunidadEntPorIntervalo"
  metrica_lupa[agregadas] <- c(
    "CorrectitudSemFuerte", "CorrectitudSemDebil",
    "ReglaIntegridadIntraEntidad", "NoNulo", "DensidadPonderada",
    "AtributoDuplicado", "ConjuntoAtributosDuplicado", "EntidadDuplicada",
    "EntidadContradictoria"
  )

  implementacion <- rep(NA_character_, 49L)
  implementacion[implementadas] <- paste0(
    "metricas_nucleo()$", metrica_lupa[implementadas]
  )
  implementacion[1:2] <- paste0(
    "metricas_referencial()$", metrica_lupa[1:2]
  )
  implementacion[27L] <- "metricas_referencial()$RatioCobertura"
  implementacion[3:4] <- "agregar(m, \"atributo\", \"ratio\")"
  implementacion[21L] <- "agregar(m, \"entidad\", \"ratio\")"
  implementacion[30L] <- "agregar(m, \"atributo\", \"ratio\")"
  implementacion[31L] <- paste0(
    "agregar(m, \"entidad\", \"ratio_umbral\", umbral = u)"
  )
  implementacion[37L] <- "agregar(m, \"atributo\", \"ratio\")"
  implementacion[38:39] <- "agregar(m, \"entidad\", \"ratio\")"
  implementacion[41L] <- "agregar(m, \"atributo\", \"ratio\")"

  observacion <- c(
    "Compara cada par clave-valor con referencial(); omite ausentes.",
    "Verifica que cada clave exista en referencial(); omite ausentes.",
    "Se obtiene con Ratio sobre CorrectitudSemFuerte.",
    "Se obtiene con Ratio sobre CorrectitudSemDebil.",
    "Admite expresi\u00f3n regular, diccionario o funci\u00f3n validadora.",
    "Especializa Formato con el diccionario ISO Alpha-3 provisto al medir.",
    "Especializa Formato con el diccionario CIE-10 provisto al medir.",
    "Especializa Formato con validar_ci_uy(); comprueba estructura y d\u00edgito de control, no existencia registral.",
    "Requiere configuraci\u00f3n experta mediante escala(); no usa referencial.",
    "Sigue la tabla 16.5: devuelve desviaci\u00f3n est\u00e1ndar muestral.",
    "M\u00e9trica posicional geogr\u00e1fica; no se implementa en el n\u00facleo tabular.",
    "M\u00e9trica posicional geogr\u00e1fica; no se implementa en el n\u00facleo tabular.",
    "M\u00e9trica posicional geogr\u00e1fica; no se implementa en el n\u00facleo tabular.",
    "M\u00e9trica [TD:\u00abImg\u00bb]; exige comparar im\u00e1genes contra una referencia.",
    "M\u00e9trica [TD:\u00abImg\u00bb]; exige evaluar fidelidad de imagen sin referencia.",
    "M\u00e9trica [TD:\u00abImg\u00bb]; exige una referencia reducida de imagen.",
    paste0(
      "Implementaci\u00f3n parcial: calcula cobertura PK/FK como real sobre dos ",
      "entidades; falta la regla booleana de inclusi\u00f3n o expresi\u00f3n ",
      "condicional con granularidad conjuntoEntidades."
    ),
    "M\u00e9trica [TD:\u00abGeo\u00bb]; requiere un motor de reglas espaciales.",
    paste0(
      "El marco pide una expresi\u00f3n condicional Sexo-Enfermedad, no un ",
      "referencial; el motor inter-entidad todav\u00eda no admite esa regla."
    ),
    "Ejecuta una regla booleana provista por el usuario sobre cada fila.",
    "Se obtiene con Ratio sobre ReglaIntegridadIntraEntidad.",
    "Especializa la regla intra-entidad con una condici\u00f3n Sexo-Enfermedad provista.",
    "Requiere enumerar el diccionario de valores admitidos.",
    "Requiere un predicado o un rango que defina el dominio.",
    "Especializa ValoresPosiblesPorExtension con el dominio de sexo provisto.",
    "M\u00e9trica de consistencia topol\u00f3gica; queda fuera del n\u00facleo tabular.",
    "Exige referencial() con completo = TRUE y alcance expl\u00edcito.",
    "Indica presencia de valores y admite declarar sentinelas como nulos.",
    "Requiere coeficientes por atributo que sumen uno.",
    "Se obtiene con Ratio sobre NoNulo.",
    "Usa RatioUmbral porque DensidadPonderada es real, no booleana.",
    "El factor Comisi\u00f3n est\u00e1 marcado [TD:\u00abGeo\u00bb] en el marco.",
    "El ratio de Comisi\u00f3n depende de la m\u00e9trica geogr\u00e1fica \u00cdtemExcedente.",
    "Marca todas las apariciones que participan en un grupo repetido.",
    "Marca cada fila cuya combinaci\u00f3n de atributos se repite.",
    "Compara la fila completa o una clave ligada con la sem\u00e1ntica del marco.",
    "Se obtiene con Ratio sobre AtributoDuplicado.",
    "Se obtiene con Ratio sobre ConjuntoAtributosDuplicado.",
    "Se obtiene con Ratio sobre EntidadDuplicada.",
    paste0(
      "Compara el vocabulario distinto de un atributo con la misma medida y ",
      "normalizaci\u00f3n que duplicados; declara valores, pares y umbral."
    ),
    "Se obtiene con Ratio sobre EntidadContradictoria.",
    "Requiere vigencia(); devuelve atraso no negativo desde la referencia temporal.",
    "Requiere vigencia(); estima cambios esperados con la frecuencia declarada.",
    "Requiere declarar el formato vigente o una funci\u00f3n validadora.",
    "Especializa DesactualizacionPorFormato con el formato PNN1 de ocho d\u00edgitos.",
    "Resultado booleano de la tabla 16.29: entrega hasta Tf, inclusive.",
    "Resultado booleano de la tabla 16.30: entrega dentro de [Ti, Tf].",
    "Resultado booleano por fila; exige fecha l\u00edmite en vigencia().",
    "Resultado booleano por fila; exige intervalo en vigencia()."
  )

  .data_frame_simple(
    numero = seq_along(metricas),
    dimension = dimension,
    factor = factor,
    metrica_agesic = metricas,
    clase_catalogo = base::factor(
      clase, levels = c("generica", "especifica", "agregada")
    ),
    estado = base::factor(
      estado,
      levels = c("implementada", "via_agregacion", "pendiente", "fuera_de_alcance")
    ),
    motivo = base::factor(
      motivo,
      levels = c(
        "semantica_completa", "semantica_parcial", "agregacion",
        "requiere_referencial", "requiere_configuracion",
        "motor_pendiente", "decision_alcance"
      )
    ),
    metrica_lupa = metrica_lupa,
    implementacion = implementacion,
    observacion = observacion,
    stringsAsFactors = FALSE
  )
}
