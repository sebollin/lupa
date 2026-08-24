.html_escapar <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  # Tambien se neutralizan secuencias que podrian parecer atributos o recursos.
  x <- gsub("=", "&#61;", x, fixed = TRUE)
  x <- gsub("@", "&#64;", x, fixed = TRUE)
  gsub(":", "&#58;", x, fixed = TRUE)
}

.html_texto <- function(x) {
  x <- .html_escapar(x)
  gsub("\n", "<br>", gsub("\r\n?", "\n", x, perl = TRUE), fixed = TRUE)
}

.resumir_valor_reporte <- function(x, max_caracteres = 240L) {
  if (is.null(x) || !length(x)) return("")
  if (is.function(x)) return("<funci\u00f3n>")
  if (inherits(x, "data.frame")) {
    return(paste0("<tabla: ", nrow(x), " filas \u00d7 ", ncol(x), " columnas>"))
  }
  if (is.list(x) && !is.null(x$estado) &&
      "indices_fila" %in% names(x) && "mostrados" %in% names(x)) {
    total <- if (length(x$total) && is.na(x$total)) "NA" else as.character(x$total)
    return(paste0(
      "estado=", x$estado,
      "; filas mostradas=", x$mostrados,
      " de ", total,
      "; alcance=", x$alcance
    ))
  }
  if (inherits(x, "POSIXt")) {
    texto <- format(x, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
  } else if (inherits(x, "Date")) {
    texto <- format(x, "%Y-%m-%d")
  } else if (is.list(x)) {
    limite <- min(length(x), 8L)
    partes <- vapply(seq_len(limite), function(i) {
      valor <- .resumir_valor_reporte(x[[i]], max_caracteres = 80L)
      nombre <- names(x)[i]
      if (!is.null(nombre) && !is.na(nombre) && nzchar(nombre)) {
        paste0(nombre, "=", valor)
      } else {
        valor
      }
    }, character(1L))
    texto <- paste(partes, collapse = "; ")
    if (length(x) > limite) texto <- paste0(texto, "; \u2026")
  } else if (is.logical(x)) {
    texto <- ifelse(is.na(x), NA_character_, ifelse(x, "s\u00ed", "no"))
  } else if (is.numeric(x)) {
    texto <- ifelse(
      is.na(x), NA_character_,
      format(x, digits = 8L, trim = TRUE, scientific = FALSE)
    )
  } else {
    texto <- as.character(x)
  }
  texto <- paste(texto, collapse = ", ")
  if (nchar(texto, type = "chars") > max_caracteres) {
    texto <- paste0(substr(texto, 1L, max_caracteres - 1L), "\u2026")
  }
  texto
}

.valor_celda_reporte <- function(x, i) {
  valor <- if (is.list(x) && !inherits(x, c("POSIXt", "Date"))) x[[i]] else x[i]
  if (!length(valor) || (length(valor) == 1L && is.atomic(valor) && is.na(valor))) {
    return("<span class=\"sin-dato\">\u2014</span>")
  }
  .html_texto(.resumir_valor_reporte(valor))
}

.validar_limite_reporte <- function(x, nombre) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1) {
    stop("`", nombre, "` debe ser un n\u00famero positivo o Inf.", call. = FALSE)
  }
  if (is.infinite(x)) Inf else floor(x)
}

.nota_truncamiento <- function(mostradas, total, unidad = "filas") {
  if (mostradas >= total) return("")
  paste0(
    "<p class=\"nota\">Se muestran ", .html_texto(mostradas), " de ",
    .html_texto(total), " ",
    .html_texto(unidad), ".</p>"
  )
}

.html_tabla <- function(x, max_filas, columnas = names(x)) {
  if (!inherits(x, "data.frame")) x <- as.data.frame(x, stringsAsFactors = FALSE)
  columnas <- intersect(columnas, names(x))
  x <- x[, columnas, drop = FALSE]
  total <- nrow(x)
  limite <- if (is.infinite(max_filas)) total else min(total, max_filas)
  if (!total || !length(columnas)) {
    return(paste0(
      "<p class=\"sin-registros\">No hay registros para mostrar.</p>",
      .nota_truncamiento(limite, total)
    ))
  }
  vista <- x[seq_len(limite), , drop = FALSE]
  encabezado <- paste0(
    "<thead><tr>",
    paste0("<th>", .html_texto(names(vista)), "</th>", collapse = ""),
    "</tr></thead>"
  )
  filas <- vapply(seq_len(nrow(vista)), function(i) {
    celdas <- vapply(seq_along(vista), function(j) {
      clase <- ""
      if (names(vista)[[j]] == "severidad") {
        nivel <- as.character(vista[[j]][i])
        if (!is.na(nivel) && nivel %in% c("ok", "sospechoso", "error")) {
          clase <- paste0(" class=\"nivel-", nivel, "\"")
        }
      }
      paste0("<td", clase, ">", .valor_celda_reporte(vista[[j]], i), "</td>")
    }, character(1L))
    paste0("<tr>", paste0(celdas, collapse = ""), "</tr>")
  }, character(1L))
  paste0(
    "<div class=\"tabla-contenedor\"><table>", encabezado,
    "<tbody>", paste0(filas, collapse = ""), "</tbody></table></div>",
    .nota_truncamiento(limite, total)
  )
}

.tarjeta_reporte <- function(etiqueta, valor, clase = "") {
  paste0(
    "<div class=\"tarjeta ", clase, "\"><span>",
    .html_texto(etiqueta), "</span><strong>", .html_texto(valor),
    "</strong></div>"
  )
}

.resumen_severidades <- function(x, no_evaluados = 0L) {
  severidades <- as.character(x)
  diagnosticos <- if (length(no_evaluados)) no_evaluados[[1L]] else 0L
  paste0(
    "<div class=\"tarjetas\">",
    .tarjeta_reporte("Errores", sum(severidades == "error", na.rm = TRUE), "error"),
    .tarjeta_reporte(
      "Sospechosos", sum(severidades == "sospechoso", na.rm = TRUE),
      "sospechoso"
    ),
    .tarjeta_reporte("Correctos", sum(severidades == "ok", na.rm = TRUE), "ok"),
    if (isTRUE(diagnosticos > 0L)) {
      .tarjeta_reporte("No evaluados", diagnosticos, "informativo")
    } else "",
    "</div>"
  )
}

.seccion_perfil <- function(x, max_filas, max_patrones,
                            proteger_datos_personales = TRUE,
                            cobertura = NULL) {
  if (proteger_datos_personales) x <- .proteger_perfil(x)
  n_protegidas <- length(.columnas_personales_protegidas(x))
  general <- data.frame(
    indicador = c(
      "Filas", "Columnas", "Celdas", "Filas completas",
      "Filas duplicadas", "Memoria de los datos"
    ),
    valor = c(
      x$general$filas, x$general$columnas, x$general$celdas,
      x$general$filas_completas, x$general$filas_duplicadas,
      format(
        structure(x$general$memoria_bytes, class = "object_size"),
        units = "auto"
      )
    ),
    stringsAsFactors = FALSE
  )
  hallazgos <- x$hallazgos
  severidades <- if ("severidad" %in% names(hallazgos)) hallazgos$severidad else {
    character()
  }
  cobertura_diagnosticos <- x$cobertura_diagnosticos
  if (!inherits(cobertura_diagnosticos, "data.frame")) {
    cobertura_diagnosticos <- .cobertura_diagnosticos_vacia()
  }
  con_patrones <- which(vapply(
    x$patrones,
    function(tabla) inherits(tabla, "data.frame") && nrow(tabla) > 0L,
    logical(1L)
  ))
  n_columnas_patrones <- length(con_patrones)
  indices_patrones <- if (is.infinite(max_filas)) con_patrones else {
    utils::head(con_patrones, max_filas)
  }
  bloques_patrones <- vapply(indices_patrones, function(i) {
    patrones <- x$patrones[[i]]
    columna <- names(x$patrones)[[i]]
    total <- attr(patrones, "n_patrones_distintos", exact = TRUE)
    if (is.null(total) || !length(total) || is.na(total)) total <- nrow(patrones)
    limite <- if (is.infinite(max_patrones)) nrow(patrones) else {
      min(nrow(patrones), max_patrones)
    }
    muestreo <- if (isTRUE(attr(patrones, "muestreado", exact = TRUE))) {
      paste0(
        "<p class=\"nota\">Patrones estimados sobre ",
        .html_texto(attr(patrones, "analizados", exact = TRUE)), " de ",
        .html_texto(attr(patrones, "total", exact = TRUE)), " valores.</p>"
      )
    } else {
      ""
    }
    paste0(
      "<h4>", .html_texto(columna), "</h4>",
      .html_tabla(patrones, max_patrones),
      if (limite < total) {
        paste0(
          "<p class=\"nota\">Se muestran ", .html_texto(limite), " de ",
          .html_texto(total),
          " patrones distintos.</p>"
        )
      } else {
        ""
      },
      muestreo
    )
  }, character(1L))
  nota_columnas_patron <- .nota_truncamiento(
    length(indices_patrones), n_columnas_patrones, "columnas con patrones"
  )
  formatos <- lapply(seq_along(x$formatos_fecha), function(i) {
    tabla <- x$formatos_fecha[[i]]
    if (!inherits(tabla, "data.frame") || !nrow(tabla)) return(NULL)
    tabla$columna <- names(x$formatos_fecha)[[i]]
    tabla[c("columna", setdiff(names(tabla), "columna"))]
  })
  formatos <- formatos[!vapply(formatos, is.null, logical(1L))]
  tabla_formatos <- if (length(formatos)) do.call(rbind, formatos) else {
    data.frame(stringsAsFactors = FALSE)
  }
  dependencias <- x$dependencias
  if (is.null(dependencias)) dependencias <- data.frame(stringsAsFactors = FALSE)
  nota_dependencias <- if (isTRUE(attr(dependencias, "truncado", exact = TRUE))) {
    paste0(
      "<p class=\"nota\">La b\u00fasqueda de dependencias se limit\u00f3 a ",
      .html_texto(length(attr(dependencias, "columnas_analizadas", exact = TRUE))),
      " columnas; quedaron fuera ",
      .html_texto(length(attr(dependencias, "columnas_omitidas", exact = TRUE))),
      ".</p>"
    )
  } else {
    ""
  }
  paste0(
    "<section><h2>Perfil de datos: ", .html_texto(x$meta$nombre), "</h2>",
    "<p class=\"meta\">Corrida: ",
    .html_texto(.resumir_valor_reporte(x$meta$fecha_hora)), "</p>",
    if (proteger_datos_personales && n_protegidas) {
      paste0(
        "<p class=\"nota\">Se protegieron modas, ejemplos, evidencia y ",
        "estadisticos de orden de ",
        .html_texto(n_protegidas),
        " columna(s) con evidencia suficiente de datos personales.</p>"
      )
    } else "",
    .resumen_severidades(severidades, nrow(cobertura_diagnosticos)),
    "<h3>Resumen general</h3>", .html_tabla(general, Inf),
    "<h3>Hallazgos por severidad</h3>", .html_tabla(hallazgos, max_filas),
    "<h3>Resumen por columna</h3>", .html_tabla(x$columnas, max_filas),
    "<h3>Clasificaci\u00f3n de posibles datos personales</h3>",
    "<p class=\"nota\">La clasificaci\u00f3n informa todos los casos posibles; s\u00f3lo la evidencia discriminante activa la protecci\u00f3n y no se juzga si esos datos deben existir en la entrega.</p>",
    .html_tabla(x$datos_personales, max_filas),
    "<h3>Cobertura del an\u00e1lisis</h3>",
    "<p class=\"nota\">La ausencia de hallazgos no implica que todos los factores se hayan evaluado.</p>",
    .html_tabla(if (is.null(cobertura)) cobertura_analisis(x) else cobertura, Inf),
    "<h3>Cobertura de diagn\u00f3sticos</h3>",
    "<p class=\"nota\">Estos diagn\u00f3sticos no se evaluaron; un perfil sin hallazgos no es un perfil limpio si esta tabla tiene filas.</p>",
    .html_tabla(cobertura_diagnosticos, Inf),
    "<h3>Patrones de formato</h3>",
    if (length(bloques_patrones)) paste0(bloques_patrones, collapse = "") else {
      "<p class=\"sin-registros\">No hay patrones para mostrar.</p>"
    },
    nota_columnas_patron,
    "<h3>Formatos de fecha</h3>", .html_tabla(tabla_formatos, max_filas),
    "<h3>Dependencias funcionales</h3>",
    .html_tabla(dependencias, max_filas), nota_dependencias,
    if (!is.null(x$duplicados_aproximados)) {
      .seccion_duplicados_aproximados(
        x$duplicados_aproximados, max_filas, proteger_datos_personales
      )
    } else "",
    "</section>"
  )
}

.seccion_medicion <- function(x, max_filas) {
  paste0(
    "<section><h2>Medidas de calidad</h2>",
    "<p class=\"meta\">", .html_texto(nrow(x)), " medidas en ",
    .html_texto(length(unique(x$id_medicion))), " corrida(s).</p>",
    .html_tabla(x, max_filas), "</section>"
  )
}

.seccion_tablero <- function(x, max_filas) {
  alcance <- attr(x, "alcance", exact = TRUE)
  cobertura <- attr(x, "cobertura", exact = TRUE)
  paste0(
    "<section><h2>Tablero de calidad</h2>",
    "<p class=\"nota\">Cada fila declara la agregaci\u00f3n usada; el alcance ",
    "impide leer estas medidas como si cubrieran todo el marco.</p>",
    .html_tabla(x, max_filas),
    "<h3>Alcance del marco</h3>", .html_tabla(alcance, Inf),
    "<h3>Detalle de cobertura</h3>", .html_tabla(cobertura, Inf),
    "</section>"
  )
}

.clave_medida_reporte <- function(id_medicion, id_medida,
                                  metrica_instanciada) {
  paste(id_medicion, id_medida, metrica_instanciada, sep = "\r")
}

.desenlaces_reporte <- function(objetos) {
  partes <- lapply(objetos, function(x) {
    evaluacion <- if (inherits(x, "evaluacion_calidad")) {
      x
    } else if (inherits(x, "analisis")) {
      x$evaluacion
    } else {
      NULL
    }
    if (is.null(evaluacion) ||
        !inherits(evaluacion$desenlaces, "data.frame") ||
        !nrow(evaluacion$desenlaces)) {
      return(NULL)
    }
    evaluacion$desenlaces[
      evaluacion$desenlaces$desenlace == "suprimir", , drop = FALSE
    ]
  })
  partes <- partes[!vapply(partes, is.null, logical(1L))]
  if (!length(partes)) return(NULL)
  resultado <- do.call(rbind, partes)
  rownames(resultado) <- NULL
  resultado[
    !duplicated(resultado[c("id_medicion", "id_medida", "regla")]),
    , drop = FALSE
  ]
}

.proteger_medicion_desenlaces <- function(x, desenlaces) {
  if (!inherits(x, "data.frame") || !nrow(x) || is.null(desenlaces) ||
      !all(c(
        "id_medida", "id_medicion", "metrica_instanciada", "resultado"
      ) %in% names(x))) {
    return(x)
  }
  claves <- .clave_medida_reporte(
    x$id_medicion, x$id_medida, x$metrica_instanciada
  )
  claves_suprimidas <- .clave_medida_reporte(
    desenlaces$id_medicion, desenlaces$id_medida,
    desenlaces$metrica_instanciada
  )
  suprimidas <- claves %in% claves_suprimidas
  if (any(suprimidas)) {
    x$resultado <- as.character(x$resultado)
    x$resultado[suprimidas] <- "[valor suprimido]"
  }
  x
}

.proteger_evaluacion_desenlaces <- function(x) {
  if (!inherits(x$desenlaces, "data.frame") || !nrow(x$desenlaces) ||
      !"valor_medido" %in% names(x$desenlaces)) {
    return(x)
  }
  suprimidas <- x$desenlaces$desenlace == "suprimir"
  if (any(suprimidas)) {
    x$desenlaces$valor_medido <- as.character(x$desenlaces$valor_medido)
    x$desenlaces$valor_medido[suprimidas] <- "[valor suprimido]"
  }
  x
}

.proteger_objeto_desenlaces <- function(x, desenlaces) {
  if (inherits(x, "medicion")) {
    return(.proteger_medicion_desenlaces(x, desenlaces))
  }
  if (inherits(x, "evaluacion_calidad")) {
    return(.proteger_evaluacion_desenlaces(x))
  }
  if (inherits(x, "analisis")) {
    if (!is.null(x$medicion)) {
      x$medicion <- .proteger_medicion_desenlaces(x$medicion, desenlaces)
    }
    if (!is.null(x$detalle_medicion)) {
      x$detalle_medicion <- .proteger_medicion_desenlaces(
        x$detalle_medicion, desenlaces
      )
    }
    if (!is.null(x$evaluacion)) {
      x$evaluacion <- .proteger_evaluacion_desenlaces(x$evaluacion)
    }
  }
  x
}

.seccion_duplicados_aproximados <- function(
    x, max_filas, proteger_datos_personales = TRUE) {
  alcance <- x$alcance
  pares <- x$pares
  estimacion <- if (is.list(x$estimacion) && length(x$estimacion)) {
    data.frame(
      indicador = names(x$estimacion),
      valor = vapply(seq_along(x$estimacion), function(i) {
        valor <- x$estimacion[[i]]
        if (length(valor) != 1L || is.na(valor)) return("NA")
        if (names(x$estimacion)[[i]] %in% c(
          "candidatos_previstos", "muestra_estimacion", "pares_benchmark",
          "vocabulario"
        )) {
          return(format(round(as.numeric(valor)), big.mark = ".",
                         decimal.mark = ",", scientific = FALSE, trim = TRUE))
        }
        as.character(valor)
      }, character(1L)),
      stringsAsFactors = FALSE
    )
  } else NULL
  if (proteger_datos_personales &&
      isFALSE(x$proteccion_aplicada) && nrow(pares)) {
    pares$evidencia_1 <- "[valor protegido]"
    pares$evidencia_2 <- "[valor protegido]"
    pares$proteccion_evidencia <- "[valores personales protegidos]"
  }
  nota <- if (!isTRUE(x$disponible)) {
    paste0(
      "<p class=\"nota\">No se ejecut\u00f3 la comparaci\u00f3n aproximada: ",
      .html_texto(x$razon), "</p>"
    )
  } else {
    paste0(
      "<p class=\"nota\">La similitud no demuestra identidad. Se us\u00f3 ",
      .html_texto(x$metodo), " con umbral ", .html_texto(x$umbral),
      "; el alcance y los pares omitidos se muestran abajo.</p>"
    )
  }
  paste0(
    "<section><h2>Duplicados aproximados</h2>", nota,
    "<p class=\"nota\">`muestra` es el limite solicitado; `muestra_efectiva` y ",
    "`n_filas_muestra` indican cuantas filas entraron realmente en la comparacion. ",
    "La comparacion por bloques es exhaustiva para esas filas; el tamano del bloque ",
    "y los pares que quedaron fuera se declaran en el alcance.</p>",
    "<p class=\"nota\"><code>tipo_par</code> distingue texto guardado ",
    "igual (<code>exacto</code>), coincidencia creada por la normalizacion ",
    "(<code>exacto_normalizado</code>) y similitud (<code>aproximado</code>). ",
    "<code>igualo_normalizar</code> marca solo el segundo caso.</p>",
    "<h3>Alcance de la comparaci\u00f3n</h3>", .html_tabla(alcance, Inf),
    if (!is.null(estimacion)) paste0(
      "<h3>Referencia temporal</h3><p class=\"nota\">",
      "La referencia se midio en esta corrida, no es determinista y solo ",
      "estima la medida aislada; no incluye firmas, cubetas ni troceo.</p>",
      .html_tabla(estimacion, Inf)
    ) else "",
    "<h3>Pares detectados</h3>", .html_tabla(pares, max_filas),
    if (isTRUE(x$alcance$truncado[[1L]])) {
      "<p class=\"nota\">La tabla de pares mostrados est\u00e1 truncada; el total hallado permanece en el alcance.</p>"
    } else "",
    "</section>"
  )
}

.seccion_evaluacion <- function(x, max_filas) {
  paste0(
    "<section><h2>Evaluaci\u00f3n de calidad</h2>",
    if (!is.null(x$desenlaces)) {
      paste0(
        "<h3>Plan de desenlaces</h3>",
        "<p class=\"nota\">Los desenlaces provienen exclusivamente de reglas ",
        "declaradas; este reporte no modifica la medici\u00f3n ni los datos.</p>",
        .html_tabla(x$desenlaces, max_filas)
      )
    } else "",
    "<h3>Evaluaciones de medidas</h3>", .html_tabla(x$medidas, max_filas),
    "<h3>Evaluaciones de reglas</h3>", .html_tabla(x$reglas, max_filas),
    "<h3>Perfiles de madurez</h3>", .html_tabla(x$perfiles, max_filas),
    "</section>"
  )
}

.evolucion_historico <- function(x) {
  perfiles <- x[x$nivel == "evaluacion_perfil", c(
    "id_medicion", "fecha", "perfil", "resultado"
  ), drop = FALSE]
  if (!nrow(perfiles)) return(perfiles)
  perfiles <- perfiles[order(perfiles$perfil, perfiles$fecha, perfiles$id_medicion), ]
  perfiles$delta <- NA_real_
  grupos <- split(seq_len(nrow(perfiles)), perfiles$perfil, drop = TRUE)
  for (indices in grupos) {
    if (length(indices) > 1L) {
      perfiles$delta[indices[-1L]] <- diff(perfiles$resultado[indices])
    }
  }
  rownames(perfiles) <- NULL
  perfiles
}

.seccion_historico <- function(x, max_filas) {
  x <- x[order(x$fecha, x$id_medicion, x$nivel), , drop = FALSE]
  paste0(
    "<section><h2>Hist\u00f3rico de calidad</h2>",
    "<p class=\"meta\">", .html_texto(length(unique(x$id_medicion))),
    " corrida(s); esquema ", .html_texto(attr(x, "version_esquema")), ".</p>",
    "<h3>Evoluci\u00f3n de perfiles de madurez</h3>",
    .html_tabla(.evolucion_historico(x), max_filas),
    "<h3>Registros hist\u00f3ricos</h3>", .html_tabla(x, max_filas),
    "</section>"
  )
}

.seccion_deriva <- function(x, max_filas, titulo) {
  severidades <- if ("severidad" %in% names(x)) x$severidad else character()
  paste0(
    "<section><h2>", titulo, "</h2>",
    .resumen_severidades(severidades),
    .html_tabla(x, max_filas), "</section>"
  )
}

.seccion_plan <- function(x, max_filas) {
  resumen <- data.frame(
    indicador = c(
      "Acciones propuestas", "Acciones activas", "Acciones recomendadas",
      "Acciones destructivas"
    ),
    valor = c(
      nrow(x), sum(x$aplicar, na.rm = TRUE), sum(x$recomendada, na.rm = TRUE),
      sum(x$destructiva, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  paste0(
    "<section><h2>Plan de limpieza</h2>",
    "<p>El plan es una propuesta editable; este informe no aplica cambios.</p>",
    .html_tabla(resumen, Inf),
    "<h3>Acciones y justificaci\u00f3n</h3>", .html_tabla(x, max_filas),
    "</section>"
  )
}

.seccion_analisis <- function(x, max_filas, max_patrones,
                              proteger_datos_personales) {
  if (proteger_datos_personales) x <- .proteger_analisis(x)
  alcance_asociaciones <- data.frame(
    indicador = c(
      "Filas analizadas", "Muestreado", "Columnas analizadas",
      "Columnas no analizables", "Columnas omitidas por limite",
      "Pares examinables", "Pares omitidos por dependencia",
      "Asociaciones antes del recorte", "Salida truncada", "Umbral"
    ),
    valor = c(
      attr(x$asociaciones, "filas_analizadas", exact = TRUE),
      attr(x$asociaciones, "muestreado", exact = TRUE),
      length(attr(x$asociaciones, "columnas_analizadas", exact = TRUE)),
      length(attr(x$asociaciones, "columnas_no_analizables", exact = TRUE)),
      length(attr(x$asociaciones, "columnas_omitidas_limite", exact = TRUE)),
      attr(x$asociaciones, "pares_posibles", exact = TRUE),
      attr(x$asociaciones, "pares_omitidos_dependencia", exact = TRUE),
      attr(x$asociaciones, "total_informadas", exact = TRUE),
      attr(x$asociaciones, "truncado", exact = TRUE),
      attr(x$asociaciones, "umbral", exact = TRUE)
    ), stringsAsFactors = FALSE
  )
  alcance_temporal <- data.frame(
    indicador = c("Columnas analizadas", "Columnas omitidas", "Salida truncada"),
    valor = c(
      length(attr(x$temporal, "columnas_analizadas", exact = TRUE)),
      length(attr(x$temporal, "columnas_omitidas", exact = TRUE)),
      attr(x$temporal, "truncado", exact = TRUE)
    ), stringsAsFactors = FALSE
  )
  distribuciones <- paste0(
    "<section><h2>Distribuciones y cuantiles</h2>",
    if (proteger_datos_personales) {
      paste0(
        "<p class=\"nota\">Los cuantiles personales conservan su ",
        "probabilidad, pero muestran valor ausente y estado ",
        "valor_protegido.</p>"
      )
    } else "",
    "<h3>Alcance por columna</h3>",
    .html_tabla(x$distribuciones$alcance, max_filas),
    "<h3>Frecuencias principales</h3>",
    .html_tabla(x$distribuciones$frecuencias, max_filas),
    "<h3>Cuantiles</h3>", .html_tabla(x$distribuciones$cuantiles, max_filas),
    "</section>"
  )
  asociaciones <- paste0(
    "<section><h2>Asociaciones entre columnas</h2>",
    "<p class=\"nota\">Metodo y soporte se declaran por par; el resultado no implica causalidad.</p>",
    "<h3>Alcance y recortes</h3>", .html_tabla(alcance_asociaciones, Inf),
    "<h3>Asociaciones informadas</h3>",
    .html_tabla(x$asociaciones, max_filas), "</section>"
  )
  temporal <- paste0(
    "<section><h2>Analisis temporal</h2>",
    paste0(
      "<p class=\"nota\">Las frecuencias inferidas son propuestas no ",
      "confirmadas; los rangos y huecos personales se marcan como ",
      "protegidos.</p>"
    ),
    "<h3>Alcance y recortes</h3>", .html_tabla(alcance_temporal, Inf),
    "<h3>Resumen</h3>", .html_tabla(x$temporal$resumen, max_filas),
    "<h3>Propuestas de frecuencia</h3>",
    .html_tabla(x$temporal$propuestas, max_filas),
    "<h3>Distribucion por dia de semana</h3>",
    .html_tabla(x$temporal$dias_semana, max_filas),
    "<h3>Huecos</h3>", .html_tabla(x$temporal$huecos, max_filas),
    "</section>"
  )
  variables <- paste0(
    "<section><h2>Escalas y roles propuestos</h2>",
    "<p class=\"nota\">Una escala basada solo en valores requiere confirmacion.</p>",
    .html_tabla(x$variables, max_filas), "</section>"
  )
  propuesta <- paste0(
    "<section><h2>Propuesta de modelo</h2>",
    "<p class=\"nota\">",
    if (isTRUE(x$meta$propuesta_confirmada)) {
      "La selecci\u00f3n de medici\u00f3n fue confirmada por quien realiz\u00f3 el an\u00e1lisis. "
    } else {
      "La propuesta es de lupa y nadie la confirm\u00f3. "
    },
    "La tabla siguiente declara qu\u00e9 m\u00e9tricas se midieron, cu\u00e1les ",
    "quedaron afuera y por qu\u00e9.</p>",
    "<h3>Decisi\u00f3n de medici\u00f3n</h3>",
    .html_tabla(x$decision_medicion, max_filas),
    "<h3>Propuesta completa</h3>",
    .html_tabla(x$propuesta_modelo, max_filas), "</section>"
  )
  advertencias <- paste0(
    "<section><h2>Advertencias de alcance</h2>",
    .resumen_severidades(x$advertencias$severidad),
    .html_tabla(x$advertencias, max_filas), "</section>"
  )
  paste0(
    "<section><h2>Analisis integral</h2><p class=\"meta\">Esquema ",
    .html_texto(x$meta$version_esquema), "; datos conservados: ",
    .html_texto(x$meta$datos_conservados), ".</p></section>",
    advertencias,
    .seccion_perfil(
      x$perfil, max_filas, max_patrones, proteger_datos_personales,
      cobertura = x$cobertura
    ),
    distribuciones, asociaciones, temporal, variables, propuesta,
    .seccion_tablero(x$tablero, max_filas),
    if (!is.null(x$medicion)) {
      .seccion_medicion(x$medicion, max_filas)
    } else "",
    if (!is.null(x$detalle_medicion)) {
      paste0(
        "<section><h2>Detalle de medici\u00f3n conservado</h2>",
        "<p class=\"nota\">Este detalle fila a fila se conserv\u00f3 por pedido ",
        "expl\u00edcito.</p>", .html_tabla(x$detalle_medicion, max_filas),
        "</section>"
      )
    } else "",
    if (!is.null(x$evaluacion)) .seccion_evaluacion(x$evaluacion, max_filas) else "",
    .seccion_plan(x$plan_limpieza, max_filas)
  )
}

.clase_objeto_reporte <- function(x) {
  clases <- c(
    "analisis", "perfil", "medicion", "evaluacion_calidad", "historico_calidad",
    "deriva_perfil", "deriva_calidad", "plan_limpieza",
    "duplicados_aproximados"
  )
  coincidencias <- clases[vapply(clases, inherits, logical(1L), x = x)]
  if (length(coincidencias)) coincidencias[[1L]] else NA_character_
}

.aplanar_objetos_reporte <- function(objetos) {
  salida <- list()
  recorrer <- function(x) {
    clase <- .clase_objeto_reporte(x)
    if (!is.na(clase)) {
      salida[[length(salida) + 1L]] <<- x
    } else if (is.list(x) && !inherits(x, "data.frame")) {
      for (elemento in x) recorrer(elemento)
    } else {
      stop(
        "Cada objeto debe ser un analisis, perfil, medicion, evaluacion_calidad, ",
        "historico_calidad, deriva_perfil, deriva_calidad, plan_limpieza o ",
        "duplicados_aproximados.",
        call. = FALSE
      )
    }
  }
  for (objeto in objetos) recorrer(objeto)
  if (!length(salida)) stop("Se necesita al menos un objeto para el reporte.", call. = FALSE)
  salida
}

.renderizar_objeto_reporte <- function(x, max_filas, max_patrones,
                                       proteger_datos_personales = TRUE,
                                       desenlaces = NULL) {
  x <- .proteger_objeto_desenlaces(x, desenlaces)
  switch(
    .clase_objeto_reporte(x),
    analisis = .seccion_analisis(
      x, max_filas, max_patrones, proteger_datos_personales
    ),
    perfil = .seccion_perfil(
      x, max_filas, max_patrones, proteger_datos_personales
    ),
    medicion = .seccion_medicion(x, max_filas),
    evaluacion_calidad = .seccion_evaluacion(x, max_filas),
    historico_calidad = .seccion_historico(x, max_filas),
    deriva_perfil = .seccion_deriva(x, max_filas, "Deriva del perfil de datos"),
    deriva_calidad = .seccion_deriva(x, max_filas, "Deriva del modelo de calidad"),
    plan_limpieza = .seccion_plan(x, max_filas),
    duplicados_aproximados = .seccion_duplicados_aproximados(
      x, max_filas, proteger_datos_personales
    )
  )
}

.css_reporte <- function() {
  paste0(
    "*{box-sizing:border-box}body{margin:0;background:#f4f6f8;color:#1e293b;",
    "font-family:system-ui,-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif;",
    "line-height:1.45}main{max-width:1180px;margin:0 auto;padding:2rem}",
    "header,section{background:#fff;border:1px solid #dbe2ea;border-radius:10px;",
    "padding:1.4rem;margin-bottom:1rem}header{border-top:6px solid #335c81}",
    "h1{margin:.1rem 0 .4rem;color:#17324d}h2{color:#234e70;margin-top:0}",
    "h3{margin-top:1.6rem}h4{margin-bottom:.45rem}.meta,.nota{color:#526373}",
    ".nota{font-size:.9rem}.sin-registros,.sin-dato{color:#6b7280;font-style:italic}",
    ".tabla-contenedor{overflow-x:auto;border:1px solid #dbe2ea;border-radius:6px}",
    "table{border-collapse:collapse;width:100%;font-size:.88rem;background:#fff}",
    "th,td{padding:.5rem .65rem;border-bottom:1px solid #e5e9ee;text-align:left;",
    "vertical-align:top;white-space:normal}th{background:#edf3f7;position:sticky;top:0}",
    "tbody tr:nth-child(even){background:#fafbfc}.tarjetas{display:flex;gap:.7rem;",
    "flex-wrap:wrap;margin:1rem 0}.tarjeta{min-width:150px;padding:.75rem 1rem;",
    "border-left:5px solid #64748b;background:#f8fafc}.tarjeta span{display:block;",
    "font-size:.82rem}.tarjeta strong{font-size:1.5rem}.tarjeta.error{border-color:#b42318}",
    ".tarjeta.sospechoso{border-color:#b7791f}.tarjeta.ok{border-color:#27864a}",
    ".nivel-error{color:#8a1c13;font-weight:700}.nivel-sospechoso{color:#8a5800;",
    "font-weight:700}.nivel-ok{color:#176b38;font-weight:700}footer{text-align:center;",
    "color:#66788a;padding:1rem}@media(max-width:700px){main{padding:.6rem}",
    "header,section{padding:1rem}}@media print{body{background:#fff;color:#000}",
    "main{max-width:none;padding:0}header,section{break-inside:avoid;border-color:#aaa;",
    "box-shadow:none}.tabla-contenedor{overflow:visible}th{position:static}",
    "footer{display:none}}"
  )
}

.copiar_archivo_reporte <- function(origen, destino, sobrescribir) {
  file.copy(origen, destino, overwrite = sobrescribir)
}

.escribir_reporte <- function(contenido, archivo, sobrescribir) {
  if (!.es_texto_escalar(archivo)) {
    stop("`archivo` debe ser una ruta no vac\u00eda.", call. = FALSE)
  }
  if (!is.logical(sobrescribir) || length(sobrescribir) != 1L ||
      is.na(sobrescribir)) {
    stop("`sobrescribir` debe ser TRUE o FALSE.", call. = FALSE)
  }
  directorio <- .validar_destino_archivo(archivo, sobrescribir)
  temporal <- tempfile(".lupa-reporte-", tmpdir = directorio, fileext = ".html")
  on.exit(unlink(temporal), add = TRUE)
  conexion <- file(temporal, open = "wt", encoding = "UTF-8")
  tryCatch(
    writeLines(enc2utf8(contenido), conexion, useBytes = TRUE),
    finally = close(conexion)
  )
  if (!.copiar_archivo_reporte(temporal, archivo, sobrescribir)) {
    stop("No se pudo escribir el reporte en la ruta solicitada.", call. = FALSE)
  }
  unlink(temporal)
  invisible(normalizePath(archivo, winslash = "/", mustWork = TRUE))
}

#' Crear un reporte HTML autocontenido
#'
#' Genera un unico archivo HTML en espanol usando solo funciones de R base. El
#' estilo se incluye dentro del documento: no requiere red, navegador especial,
#' conversor externo ni archivos auxiliares. Cada valor dinamico se escapa antes
#' de incorporarlo al documento.
#'
#' Se pueden combinar objetos producidos por el profiling, la medicion, la
#' evaluacion, el historico, las comparaciones de deriva y la planificacion de
#' limpieza. Cada tipo anade su seccion; el reporte no modifica datos ni aplica
#' planes. Si una evaluacion contiene desenlaces de supresion declarados por
#' reglas, el reporte enmascara su `valor_medido` y el `resultado` de las mismas
#' medidas incluidas en el documento. El enmascarado se hace sobre copias y no
#' modifica los objetos recibidos.
#'
#' @param x Un objeto compatible o una lista de objetos compatibles.
#' @param ... Objetos adicionales de clase `analisis`, `perfil`, `medicion`,
#'   `evaluacion_calidad`, `historico_calidad`, `deriva_perfil`,
#'   `deriva_calidad`, `plan_limpieza` o `duplicados_aproximados`.
#' @param archivo Ruta de salida. De forma predeterminada crea un archivo en
#'   `tempdir()`.
#' @param sobrescribir Si se permite reemplazar un archivo existente.
#' @param titulo Titulo visible del reporte.
#' @param fecha Fecha y hora de generacion, inyectable para obtener resultados
#'   reproducibles. Se normaliza a UTC.
#' @param max_filas Maximo de filas por tabla y de columnas del perfil cuyos
#'   patrones se detallan. Las omisiones se informan dentro del reporte.
#' @param max_patrones Maximo de patrones mostrados por columna. Las omisiones
#'   se informan dentro del reporte.
#' @param proteger_datos_personales Si se enmascaran modas, ejemplos, evidencia,
#'   estadisticos de orden, cuantiles y rangos temporales de columnas
#'   cuya clasificacion activa proteccion automatica. Es `TRUE` por defecto.
#'   Las coincidencias debiles se informan sin suprimir. Para ver valores
#'   concretos deben haberse conservado tambien con
#'   `perfilar(..., proteger_datos_personales = FALSE)`.
#'
#' @return La ruta normalizada del archivo, de forma invisible.
#' @export
#' @seealso [perfilar()], [medir()], [evaluar()], [historico_calidad()],
#'   [comparar_perfiles()], [planificar_limpieza()]
#'
#' @examples
#' perfil <- perfilar(datos_administrativos)
#' archivo <- reportar(perfil)
#' unlink(archivo)
reportar <- function(x, ...,
                     archivo = tempfile("reporte-lupa-", fileext = ".html"),
                     sobrescribir = FALSE,
                     titulo = "Reporte de calidad de datos",
                     fecha = Sys.time(), max_filas = 100L,
                     max_patrones = 20L,
                     proteger_datos_personales = TRUE) {
  objetos <- .aplanar_objetos_reporte(c(list(x), list(...)))
  if (!.es_texto_escalar(titulo)) {
    stop("`titulo` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  max_filas <- .validar_limite_reporte(max_filas, "max_filas")
  max_patrones <- .validar_limite_reporte(max_patrones, "max_patrones")
  if (!is.logical(proteger_datos_personales) ||
      length(proteger_datos_personales) != 1L ||
      is.na(proteger_datos_personales)) {
    stop("`proteger_datos_personales` debe ser TRUE o FALSE.", call. = FALSE)
  }
  fecha <- tryCatch(.fecha_utc(fecha), error = function(e) NA)
  if (length(fecha) != 1L || is.na(fecha) || !is.finite(as.numeric(fecha))) {
    stop("`fecha` debe contener una fecha y hora v\u00e1lida.", call. = FALSE)
  }
  secciones <- vapply(objetos, .renderizar_objeto_reporte, character(1L),
                      max_filas = max_filas, max_patrones = max_patrones,
                      proteger_datos_personales = proteger_datos_personales,
                      desenlaces = .desenlaces_reporte(objetos))
  documento <- paste0(
    "<!doctype html><html lang=\"es\"><head><meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    "<title>", .html_texto(titulo), "</title><style>", .css_reporte(),
    "</style></head><body><main><header><h1>", .html_texto(titulo), "</h1>",
    "<p class=\"meta\">Generado: ",
    .html_texto(.resumir_valor_reporte(fecha)), " \u00b7 Archivo: ",
    .html_texto(basename(archivo)), "</p>",
    "<p class=\"nota\">Los textos de m\u00e1s de 240 caracteres se abrevian con ",
    "un punto suspensivo. Cada truncamiento de filas o patrones se indica en ",
    "su secci\u00f3n.</p></header>",
    paste0(secciones, collapse = ""),
    "<footer>Reporte autocontenido generado con lupa ",
    .html_texto(.version_paquete()), ".</footer></main></body></html>"
  )
  .escribir_reporte(documento, archivo, sobrescribir)
}
