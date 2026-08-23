.umbrales_benford <- function() {
  list(
    minimo_ordenes_magnitud = 3,
    minima_proporcion_positivos = 1,
    minimo_observaciones_utilizables = 100L,
    nivel_significacion = 0.01
  )
}

.columna_candidata_benford <- function(x) {
  is.numeric(x) &&
    !inherits(x, c("Date", "POSIXt", "difftime", "integer64")) &&
    sum(is.finite(x)) >= 50L
}

.primer_digito_significativo <- function(x) {
  exponente <- floor(log10(x))
  digito <- floor(10 ^ (log10(x) - exponente) + 1e-12)
  as.integer(pmin(9, pmax(1, digito)))
}

.texto_distribucion_benford <- function(distribucion) {
  paste0(
    distribucion$digito, ":",
    sprintf("%.3f", distribucion$proporcion_observada), "/",
    sprintf("%.3f", distribucion$proporcion_esperada),
    collapse = "; "
  )
}

.precondiciones_benford <- function(variacion, es_identificador,
                                    n_positivos, proporcion_positivos,
                                    ordenes, umbrales) {
  fallas <- character()
  if (!variacion) {
    fallas <- c(fallas, "sin_variacion")
  }
  if (es_identificador) {
    fallas <- c(fallas, "parece_identificador")
  }
  if (n_positivos < umbrales$minimo_observaciones_utilizables) {
    fallas <- c(fallas, "observaciones_utilizables_insuficientes")
  }
  if (!is.finite(proporcion_positivos) ||
      proporcion_positivos < umbrales$minima_proporcion_positivos) {
    fallas <- c(fallas, "proporcion_positivos_insuficiente")
  }
  if (!is.finite(ordenes) ||
      ordenes < umbrales$minimo_ordenes_magnitud) {
    fallas <- c(fallas, "ordenes_magnitud_insuficientes")
  }
  fallas
}

# Un identificador no es una magnitud, y Benford describe magnitudes que salen
# de procesos multiplicativos. Que `MotId` se desvie de la distribucion es
# cierto y no significa nada.
#
# Esta guarda exigia una corrida consecutiva SIN HUECOS, y un identificador real
# tiene huecos: los que se dieron de baja. Un `MotId` que va de 1 a 4557 sobre
# 3.159 filas no la pasaba, y Benford se corria igual.
#
# Lo que lo describe es la DENSIDAD: una numeracion ocupa un tramo compacto de
# los enteros -0,69 en ese caso- y una magnitud se reparte por varios ordenes de
# magnitud -0,00005 para montos entre 9 y 9.999.999-. La unicidad no sirve: un
# monto tambien es casi unico.

# El minimo de valores distintos es el mismo que usa el detector de secuencias
# enteras: con menos de veinte, una densidad alta no dice nada.
.MIN_DISTINTOS_NUMERACION_BENFORD <- 20L

.parece_correlativo_benford <- function(x) {
  if (!is.numeric(x)) return(FALSE)
  valores <- as.numeric(x[is.finite(x)])
  if (length(valores) < 2L) return(FALSE)
  if (any(valores != floor(valores))) return(FALSE)
  distintos <- sort(unique(valores))
  if (length(distintos) < .MIN_DISTINTOS_NUMERACION_BENFORD) return(FALSE)
  rango <- distintos[[length(distintos)]] - distintos[[1L]] + 1
  if (!is.finite(rango) || rango <= 0) return(FALSE)
  length(distintos) / rango >= .MIN_DENSIDAD_NUMERACION
}

.resultado_benford_columna <- function(x, nombre, tipo_inferido,
                                       posible_identificador,
                                       umbrales = .umbrales_benford()) {
  finitos <- if (is.numeric(x)) {
    as.numeric(x[is.finite(x)])
  } else {
    numeric()
  }
  positivos <- finitos[finitos > 0]
  n_finitos <- length(finitos)
  n_positivos <- length(positivos)
  proporcion_positivos <- if (n_finitos) n_positivos / n_finitos else NA_real_
  variacion <- length(unique(finitos)) > 1L
  ordenes <- if (n_positivos && variacion) {
    log10(max(positivos)) - log10(min(positivos))
  } else {
    NA_real_
  }
  es_identificador <- identical(as.character(tipo_inferido), "identificador") ||
    isTRUE(posible_identificador) || .parece_correlativo_benford(x)

  fallas <- .precondiciones_benford(
    variacion, es_identificador, n_positivos, proporcion_positivos,
    ordenes, umbrales
  )

  base <- list(
    columna = nombre,
    aplica = !length(fallas),
    precondiciones_fallidas = fallas,
    n_finitos = as.integer(n_finitos),
    n_positivos = as.integer(n_positivos),
    proporcion_positivos = proporcion_positivos,
    ordenes_magnitud = ordenes,
    parece_identificador = es_identificador
  )
  if (length(fallas)) return(base)

  digitos <- .primer_digito_significativo(positivos)
  observados <- tabulate(digitos, nbins = 9L)
  proporcion_esperada <- log10(1 + 1 / seq_len(9L))
  esperados <- n_positivos * proporcion_esperada
  estadistico <- sum((observados - esperados) ^ 2 / esperados)
  p_valor <- stats::pchisq(estadistico, df = 8L, lower.tail = FALSE)
  distribucion <- data.frame(
    digito = seq_len(9L),
    n_observado = as.integer(observados),
    proporcion_observada = as.numeric(observados / n_positivos),
    proporcion_esperada = as.numeric(proporcion_esperada),
    n_esperado = as.numeric(esperados),
    stringsAsFactors = FALSE
  )
  c(base, list(
    metodo = "chi-cuadrado de Pearson; 8 grados de libertad",
    estadistico = as.numeric(estadistico),
    grados_libertad = 8L,
    p_valor = as.numeric(p_valor),
    distribucion = distribucion,
    desviacion = isTRUE(p_valor < umbrales$nivel_significacion)
  ))
}

.motivo_no_aplica_benford <- function(resultado, umbrales) {
  etiquetas <- c(
    sin_variacion = "sin variacion",
    parece_identificador = paste0(
      "parece un identificador (tipo_inferido, posible_identificador o secuencia correlativa)"
    ),
    observaciones_utilizables_insuficientes = paste0(
      "observaciones positivas utilizables ", resultado$n_positivos,
      " < ", umbrales$minimo_observaciones_utilizables
    ),
    proporcion_positivos_insuficiente = paste0(
      "proporcion de positivos ", sprintf("%.3f", resultado$proporcion_positivos),
      " < ", sprintf("%.3f", umbrales$minima_proporcion_positivos)
    ),
    ordenes_magnitud_insuficientes = paste0(
      "ordenes de magnitud log10(max/min) ",
      if (is.finite(resultado$ordenes_magnitud)) {
        sprintf("%.3f", resultado$ordenes_magnitud)
      } else {
        "no calculables"
      },
      " < ", umbrales$minimo_ordenes_magnitud
    )
  )
  paste0(
    "No aplica la ley de Benford. Precondiciones fallidas: ",
    paste(unname(etiquetas[resultado$precondiciones_fallidas]), collapse = "; "),
    "."
  )
}

.diagnosticar_benford <- function(datos, columnas, hallazgos) {
  candidatas <- which(vapply(datos, .columna_candidata_benford, logical(1L)))
  if (!length(candidatas)) {
    return(list(hallazgos = list(), cobertura = .cobertura_diagnosticos_vacia(),
                meta = NULL))
  }

  umbrales <- .umbrales_benford()
  identificadores <- as.character(hallazgos$columna[
    hallazgos$tipo_hallazgo == "posible_identificador"
  ])
  resultados <- lapply(candidatas, function(i) {
    .resultado_benford_columna(
      datos[[i]], names(datos)[[i]], columnas$tipo_inferido[[i]],
      names(datos)[[i]] %in% identificadores, umbrales
    )
  })
  names(resultados) <- make.unique(names(datos)[candidatas])

  cobertura <- lapply(resultados, function(resultado) {
    if (isTRUE(resultado$aplica)) return(NULL)
    .nuevo_diagnostico_no_evaluado(
      "ley_benford", resultado$columna,
      .motivo_no_aplica_benford(resultado, umbrales),
      paste0(
        "No interpretar una distribucion de primeros digitos. Benford requiere ",
        "montos positivos no asignados, al menos ",
        umbrales$minimo_observaciones_utilizables,
        " observaciones y al menos ", umbrales$minimo_ordenes_magnitud,
        " ordenes de magnitud."
      )
    )
  })
  cobertura <- cobertura[!vapply(cobertura, is.null, logical(1L))]
  cobertura <- if (length(cobertura)) {
    do.call(rbind, cobertura)
  } else {
    .cobertura_diagnosticos_vacia()
  }

  nuevos <- lapply(resultados, function(resultado) {
    if (!isTRUE(resultado$aplica) || !isTRUE(resultado$desviacion)) return(NULL)
    .nuevo_hallazgo(
      resultado$columna, "desviacion_benford", "sospechoso",
      paste0(
        "La distribucion de primeros digitos se aparta de la esperada por la ",
        "ley de Benford. Es una senal descriptiva para revisar, no evidencia ",
        "de fraude ni de manipulacion."
      ),
      paste0(
        resultado$metodo, ": X2=", sprintf("%.3f", resultado$estadistico),
        ", p=", format.pval(resultado$p_valor, digits = 4L),
        "; observado/esperado por digito: ",
        .texto_distribucion_benford(resultado$distribucion), "."
      ),
      paste0(
        "Revisar el proceso y explicaciones inocentes como topes administrativos, ",
        "redondeos, precios psicologicos o subsidios de monto fijo."
      ),
      resultado$n_positivos, NA_real_, "valor_positivo"
    )
  })
  nuevos <- nuevos[!vapply(nuevos, is.null, logical(1L))]

  list(
    hallazgos = nuevos,
    cobertura = cobertura,
    meta = list(
      metodo = "chi-cuadrado de Pearson; 8 grados de libertad",
      umbrales = umbrales,
      resultados = resultados
    )
  )
}
