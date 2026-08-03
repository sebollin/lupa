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
    n_invalidos <- if ("n_codificacion_invalida" %in% names(fila) &&
        is.finite(fila$n_codificacion_invalida)) {
      fila$n_codificacion_invalida
    } else {
      0L
    }
    n_validos <- fila$n - fila$n_faltantes - n_invalidos

    if (identical(fila$estado_estadisticos, "tipo_compuesto_no_analizado")) {
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
      agregar(.nuevo_hallazgo(
        nombre, "codificacion_rota", "error",
        "Hay texto con se\u00f1ales de una conversi\u00f3n de codificaci\u00f3n incorrecta.",
        resultado$diagnostico_texto$evidencia_codificacion,
        if (isTRUE(fila$n_codificacion_reparable > 0L)) {
          paste0(
            "Reparar s\u00f3lo los valores cuyo viaje UTF-8 a latin1 cierra en ",
            "texto UTF-8 v\u00e1lido; revisar manualmente los caracteres perdidos."
          )
        } else {
          "Recuperar el dato desde la fuente: el car\u00e1cter original ya no est\u00e1 disponible."
        }
      ))
    }
    if (isTRUE(fila$n_numeros_texto > 0L) &&
        is.finite(fila$proporcion_numeros_texto) &&
        fila$proporcion_numeros_texto >= 0.8) {
      agregar(.nuevo_hallazgo(
        nombre, "numero_como_texto", "sospechoso",
        if (isTRUE(fila$numero_texto_ambiguo)) {
          paste0(
            "La columna parece num\u00e9rica, pero el punto puede representar ",
            "decimales o miles porque no hay una coma que desambig\u00fce."
          )
        } else {
          "La columna contiene n\u00fameros escritos con formato regional, unidad o s\u00edmbolo."
        },
        resultado$numeros_texto$evidencia,
        if (isTRUE(fila$numero_texto_seguro)) {
          "Convertir de forma expl\u00edcita conservando en la bit\u00e1cora la convenci\u00f3n y la unidad."
        } else {
          "Confirmar la convenci\u00f3n decimal y la unidad antes de convertir."
        }
      ))
    }

    if (identical(fila$estado_estadisticos, "omitidos_precision")) {
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
    } else if (identical(fila$estado_estadisticos, "requiere_bit64")) {
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
      "Renombrar las columnas con nombres sint\u00e1cticos, \u00fanicos y sin espacios al borde."
    )
  }
  fechas_partidas <- .detectar_fecha_partida(datos, columnas)
  if (length(fechas_partidas)) {
    hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
      NA_character_, "fecha_partida_columnas", "sospechoso",
      "La tabla parece representar una fecha mediante columnas separadas de a\u00f1o, mes y d\u00eda.",
      paste(fechas_partidas, collapse = "; "),
      "Confirmar la sem\u00e1ntica y construir una fecha expl\u00edcita sin descartar las columnas de origen."
    )
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
