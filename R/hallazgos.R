.nuevo_hallazgo <- function(columna, tipo, severidad, descripcion,
                            evidencia, sugerencia) {
  data.frame(
    columna = columna,
    tipo_hallazgo = tipo,
    severidad = severidad,
    descripcion = descripcion,
    evidencia = evidencia,
    sugerencia = sugerencia,
    stringsAsFactors = FALSE
  )
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
    k <<- k + 1L
    hallazgos[[k]] <<- x
  }

  for (i in seq_along(resultados)) {
    resultado <- resultados[[i]]
    fila <- resultado$fila
    nombre <- columnas[[i]]
    n_validos <- fila$n - fila$n_faltantes

    if (fila$n_distintos == 1L && n_validos > 1L) {
      agregar(.nuevo_hallazgo(
        nombre, "constante", "sospechoso",
        "La columna contiene un \u00fanico valor no ausente.",
        paste0("Valor: ", fila$moda, "; frecuencia: ", fila$frecuencia_moda),
        "Confirmar si la columna aporta informaci\u00f3n o si corresponde retirarla."
      ))
    }

    if (n_validos > 1L && isTRUE(all.equal(fila$tasa_distintos, 1))) {
      agregar(.nuevo_hallazgo(
        nombre, "posible_identificador", "ok",
        "Todos los valores no ausentes son distintos; puede ser un identificador.",
        paste0(fila$n_distintos, " valores distintos de ", n_validos),
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

    if (is.finite(fila$pct_faltantes_totales) &&
        fila$pct_faltantes_totales >= umbral_faltantes_sospechoso) {
      severidad <- if (fila$pct_faltantes_totales >= umbral_faltantes_error) {
        "error"
      } else {
        "sospechoso"
      }
      agregar(.nuevo_hallazgo(
        nombre, "faltantes", severidad,
        "La proporci\u00f3n total de faltantes supera el umbral configurado.",
        sprintf(
          "%d ausentes reales y %d disfrazados (%.3f del total)",
          fila$n_faltantes, fila$n_faltantes_disfrazados,
          fila$pct_faltantes_totales
        ),
        "Revisar la obligatoriedad del campo y el proceso que origina los faltantes."
      ))
    }

    if (fila$n_faltantes_disfrazados > 0L) {
      agregar(.nuevo_hallazgo(
        nombre, "faltantes_disfrazados", "error",
        "Hay valores que representan ausencia sin estar codificados como NA.",
        resultado$faltantes_disfrazados$evidencia,
        "Normalizar estas representaciones a NA conservando su significado si fuera necesario."
      ))
    }

    if (isTRUE(attr(resultado$formatos, "formatos_mixtos"))) {
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

    patrones <- attr(resultado$patrones, "patrones_completos")
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

    if (nombre %in% columnas_sin_ceros && fila$n_ceros > 0L) {
      agregar(.nuevo_hallazgo(
        nombre, "ceros_no_permitidos", "sospechoso",
        "La columna contiene ceros aunque se configur\u00f3 como no admisible.",
        paste(fila$n_ceros, "ceros"),
        "Verificar si los ceros son v\u00e1lidos o representan otro estado."
      ))
    }
    if (nombre %in% columnas_no_negativas && fila$n_negativos > 0L) {
      agregar(.nuevo_hallazgo(
        nombre, "negativos_no_permitidos", "sospechoso",
        "La columna contiene valores negativos aunque se configur\u00f3 como no negativa.",
        paste(fila$n_negativos, "valores negativos"),
        "Corregir los valores o ajustar la restricci\u00f3n del dominio."
      ))
    }
    if (fila$n_outliers > 0L) {
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
    if (length(x) != length(y)) {
      return(FALSE)
    }
    na_x <- is.na(x)
    na_y <- is.na(y)
    if (!identical(na_x, na_y)) {
      return(FALSE)
    }
    identical(as.character(x[!na_x]), as.character(y[!na_y]))
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
                                 n_filas_duplicadas) {
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
      "Definir una clave y revisar la causa antes de eliminar duplicados."
    )
  }
  if (nrow(duplicadas)) {
    for (i in seq_len(nrow(duplicadas))) {
      hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
        duplicadas$columna_1[[i]], "columnas_duplicadas", "sospechoso",
        "Dos columnas tienen el mismo contenido.",
        paste(duplicadas$columna_1[[i]], "=", duplicadas$columna_2[[i]]),
        "Confirmar si ambas columnas son necesarias o si existe redundancia."
      )
    }
  }

  if (length(hallazgos)) {
    resultado <- do.call(rbind, hallazgos)
  } else {
    resultado <- data.frame(
      columna = character(), tipo_hallazgo = character(),
      severidad = character(), descripcion = character(),
      evidencia = character(), sugerencia = character(),
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
