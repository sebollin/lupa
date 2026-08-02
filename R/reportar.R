.html_escapar <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  # También se neutralizan secuencias que podrían parecer atributos o recursos.
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

.resumen_severidades <- function(x) {
  severidades <- as.character(x)
  paste0(
    "<div class=\"tarjetas\">",
    .tarjeta_reporte("Errores", sum(severidades == "error", na.rm = TRUE), "error"),
    .tarjeta_reporte(
      "Sospechosos", sum(severidades == "sospechoso", na.rm = TRUE),
      "sospechoso"
    ),
    .tarjeta_reporte("Correctos", sum(severidades == "ok", na.rm = TRUE), "ok"),
    "</div>"
  )
}

.seccion_perfil <- function(x, max_filas, max_patrones) {
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
    .resumen_severidades(severidades),
    "<h3>Resumen general</h3>", .html_tabla(general, Inf),
    "<h3>Hallazgos por severidad</h3>", .html_tabla(hallazgos, max_filas),
    "<h3>Resumen por columna</h3>", .html_tabla(x$columnas, max_filas),
    "<h3>Patrones de formato</h3>",
    if (length(bloques_patrones)) paste0(bloques_patrones, collapse = "") else {
      "<p class=\"sin-registros\">No hay patrones para mostrar.</p>"
    },
    nota_columnas_patron,
    "<h3>Formatos de fecha</h3>", .html_tabla(tabla_formatos, max_filas),
    "<h3>Dependencias funcionales</h3>",
    .html_tabla(dependencias, max_filas), nota_dependencias,
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

.seccion_evaluacion <- function(x, max_filas) {
  paste0(
    "<section><h2>Evaluaci\u00f3n de calidad</h2>",
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

.clase_objeto_reporte <- function(x) {
  clases <- c(
    "perfil", "medicion", "evaluacion_calidad", "historico_calidad",
    "deriva_perfil", "deriva_calidad", "plan_limpieza"
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
        "Cada objeto debe ser un perfil, medicion, evaluacion_calidad, ",
        "historico_calidad, deriva_perfil, deriva_calidad o plan_limpieza.",
        call. = FALSE
      )
    }
  }
  for (objeto in objetos) recorrer(objeto)
  if (!length(salida)) stop("Se necesita al menos un objeto para el reporte.", call. = FALSE)
  salida
}

.renderizar_objeto_reporte <- function(x, max_filas, max_patrones) {
  switch(
    .clase_objeto_reporte(x),
    perfil = .seccion_perfil(x, max_filas, max_patrones),
    medicion = .seccion_medicion(x, max_filas),
    evaluacion_calidad = .seccion_evaluacion(x, max_filas),
    historico_calidad = .seccion_historico(x, max_filas),
    deriva_perfil = .seccion_deriva(x, max_filas, "Deriva del perfil de datos"),
    deriva_calidad = .seccion_deriva(x, max_filas, "Deriva del modelo de calidad"),
    plan_limpieza = .seccion_plan(x, max_filas)
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
  directorio <- dirname(archivo)
  if (!dir.exists(directorio)) {
    stop("No existe el directorio de destino: ", directorio, ".", call. = FALSE)
  }
  if (file.exists(archivo) && !sobrescribir) {
    stop("El archivo ya existe; use `sobrescribir = TRUE` para reemplazarlo.",
         call. = FALSE)
  }
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
#' Genera un único archivo HTML en español usando sólo funciones de R base. El
#' estilo se incluye dentro del documento: no requiere red, navegador especial,
#' conversor externo ni archivos auxiliares. Cada valor dinámico se escapa antes
#' de incorporarlo al documento.
#'
#' Se pueden combinar objetos producidos por el profiling, la medición, la
#' evaluación, el histórico, las comparaciones de deriva y la planificación de
#' limpieza. Cada tipo añade su sección; el reporte no modifica datos ni aplica
#' planes.
#'
#' @param x Un objeto compatible o una lista de objetos compatibles.
#' @param ... Objetos adicionales de clase `perfil`, `medicion`,
#'   `evaluacion_calidad`, `historico_calidad`, `deriva_perfil`,
#'   `deriva_calidad` o `plan_limpieza`.
#' @param archivo Ruta de salida. De forma predeterminada crea un archivo en
#'   `tempdir()`.
#' @param sobrescribir Si se permite reemplazar un archivo existente.
#' @param titulo Título visible del reporte.
#' @param fecha Fecha y hora de generación, inyectable para obtener resultados
#'   reproducibles. Se normaliza a UTC.
#' @param max_filas Máximo de filas por tabla y de columnas del perfil cuyos
#'   patrones se detallan. Las omisiones se informan dentro del reporte.
#' @param max_patrones Máximo de patrones mostrados por columna. Las omisiones
#'   se informan dentro del reporte.
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
                     max_patrones = 20L) {
  objetos <- .aplanar_objetos_reporte(c(list(x), list(...)))
  if (!.es_texto_escalar(titulo)) {
    stop("`titulo` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  max_filas <- .validar_limite_reporte(max_filas, "max_filas")
  max_patrones <- .validar_limite_reporte(max_patrones, "max_patrones")
  fecha <- tryCatch(.fecha_utc(fecha), error = function(e) NA)
  if (length(fecha) != 1L || is.na(fecha) || !is.finite(as.numeric(fecha))) {
    stop("`fecha` debe contener una fecha y hora v\u00e1lida.", call. = FALSE)
  }
  secciones <- vapply(objetos, .renderizar_objeto_reporte, character(1L),
                      max_filas = max_filas, max_patrones = max_patrones)
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
