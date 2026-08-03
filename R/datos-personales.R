.proporcion_compatible <- function(x, patron) {
  valores <- trimws(.texto_analizable(x)$valores)
  presentes <- !is.na(valores) & nzchar(valores)
  if (!any(presentes)) return(NA_real_)
  mean(grepl(patron, valores[presentes], perl = TRUE))
}

.clasificar_dato_personal <- function(x, nombre, inferencia) {
  if (is.matrix(x)) {
    return(list(tipo = NA_character_, proporcion = NA_real_, fundamento = ""))
  }
  normalizado <- .normalizar_nombre_fecha(nombre)
  reglas_nombre <- c(
    documento_identidad = paste0(
      "(^|_)(cedula|ci|dni|rut|curp|documento|documento_identidad|",
      "numero_documento|nro_documento|identificacion|id_nacional|",
      "pasaporte|passport)($|_)"
    ),
    correo = "(^|_)(correo|email|mail)($|_)",
    telefono = "(^|_)(telefono|celular|movil)($|_)",
    fecha_nacimiento = "(^|_)(fecha_nacimiento|f_nacimiento|nacimiento)($|_)",
    nombre = "(^|_)(nombre|nombres|apellido|apellidos|nombre_completo)($|_)",
    domicilio = "(^|_)(direccion|domicilio|calle|address)($|_)"
  )
  por_nombre <- names(reglas_nombre)[vapply(
    reglas_nombre, grepl, logical(1L), x = normalizado, perl = TRUE
  )]
  textos <- trimws(.texto_analizable(x)$valores)
  presentes <- !is.na(textos) & nzchar(textos)
  proporcion_correo <- if ("correo" %in% por_nombre ||
    any(grepl("@", textos[presentes], fixed = TRUE))) {
    .proporcion_compatible(
      x, "^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$"
    )
  } else NA_real_
  longitudes <- nchar(textos[presentes], type = "chars")
  forma_documento_posible <- length(longitudes) &&
    mean(longitudes >= 7L & longitudes <= 12L) >= 0.8
  proporcion_documento <- if (
      "documento_identidad" %in% por_nombre || forma_documento_posible) {
    .proporcion_compatible(
      x,
      "^(?:[0-9]{1,2}\\.?[0-9]{3}\\.?[0-9]{3}-?[0-9Kk]|[0-9]{7,9})$"
    )
  } else NA_real_

  tipo <- NA_character_
  proporcion <- NA_real_
  fundamento <- ""
  if (is.finite(proporcion_correo) && proporcion_correo >= 0.8) {
    tipo <- "correo"
    proporcion <- proporcion_correo
    fundamento <- "forma de correo dominante"
  } else if ("correo" %in% por_nombre) {
    tipo <- "correo"
    proporcion <- proporcion_correo
    fundamento <- "nombre de columna"
  } else if ("documento_identidad" %in% por_nombre ||
             (is.finite(proporcion_documento) &&
              proporcion_documento >= 0.8)) {
    tipo <- "documento_identidad"
    proporcion <- proporcion_documento
    fundamento <- if ("documento_identidad" %in% por_nombre) {
      "nombre de columna y forma compatible"
    } else {
      "forma de documento dominante"
    }
  } else if (length(por_nombre)) {
    tipo <- por_nombre[[1L]]
    proporcion <- if (tipo == "fecha_nacimiento" &&
      inferencia$tipo %in% c("fecha", "fecha-hora")) 1 else NA_real_
    fundamento <- "nombre de columna"
  }
  list(tipo = tipo, proporcion = proporcion, fundamento = fundamento)
}

.detectar_datos_personales <- function(datos, nombres, resultados) {
  filas <- lapply(seq_along(datos), function(i) {
    clasificacion <- .clasificar_dato_personal(
      datos[[i]], nombres[[i]], resultados[[i]]$inferencia
    )
    if (is.na(clasificacion$tipo)) return(NULL)
    data.frame(
      columna = nombres[[i]],
      tipo = clasificacion$tipo,
      proporcion_compatible = clasificacion$proporcion,
      fundamento = clasificacion$fundamento,
      stringsAsFactors = FALSE
    )
  })
  filas <- filas[!vapply(filas, is.null, logical(1L))]
  resultado <- if (length(filas)) do.call(rbind, filas) else data.frame(
    columna = character(), tipo = character(),
    proporcion_compatible = numeric(), fundamento = character(),
    stringsAsFactors = FALSE
  )
  rownames(resultado) <- NULL
  class(resultado) <- c("clasificacion_datos_personales", "data.frame")
  resultado
}

.hallazgos_datos_personales <- function(clasificacion, permitidos) {
  if (!nrow(clasificacion)) return(list())
  lapply(seq_len(nrow(clasificacion)), function(i) {
    fila <- clasificacion[i, , drop = FALSE]
    .nuevo_hallazgo(
      fila$columna[[1L]], "dato_personal_posible",
      if (permitidos) "ok" else "error",
      if (permitidos) {
        paste0(
          "La columna parece contener datos personales; esta clasificaci\u00f3n ",
          "no implica un problema de calidad ni de cumplimiento."
        )
      } else {
        paste0(
          "La columna parece contener datos personales y la entrega fue ",
          "declarada como libre de ellos."
        )
      },
      paste0(
        "Tipo posible: ", fila$tipo[[1L]], "; fundamento: ",
        fila$fundamento[[1L]],
        if (is.finite(fila$proporcion_compatible[[1L]])) {
          paste0("; proporci\u00f3n compatible: ",
                 sprintf("%.3f", fila$proporcion_compatible[[1L]]))
        } else ""
      ),
      if (permitidos) {
        paste0(
          "Mantener protegidos la moda, los ejemplos, la evidencia y los ",
          "estadisticos de orden al compartir salidas."
        )
      } else {
        "Confirmar el contrato de la entrega antes de retirar o transformar datos."
      }
    )
  })
}

.fecha_resumida_personal <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(as.Date(NA))
  tryCatch(
    suppressWarnings(as.Date(substr(as.character(x), 1L, 10L))),
    error = function(e) as.Date(NA)
  )
}

.hallazgos_rango_nacimiento <- function(columnas, clasificacion,
                                        fecha_referencia) {
  nacimientos <- clasificacion[
    clasificacion$tipo == "fecha_nacimiento", , drop = FALSE
  ]
  if (!nrow(nacimientos)) return(list())
  limite_inferior <- as.Date("1900-01-01")
  limite_superior <- as.Date(fecha_referencia, tz = "UTC")
  hallazgos <- list()
  for (i in seq_len(nrow(nacimientos))) {
    nombre <- nacimientos$columna[[i]]
    indice <- match(nombre, columnas$columna)
    if (is.na(indice)) next
    minimo <- .fecha_resumida_personal(columnas$minimo_fecha[[indice]])
    maximo <- .fecha_resumida_personal(columnas$maximo_fecha[[indice]])
    anterior <- !is.na(minimo) && minimo < limite_inferior
    futura <- !is.na(maximo) && maximo > limite_superior
    if (!anterior && !futura) next
    situaciones <- c(
      if (anterior) "al menos una fecha anterior a 1900",
      if (futura) "al menos una fecha posterior a la fecha del perfil"
    )
    hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
      nombre, "fecha_nacimiento_fuera_rango",
      if (futura) "error" else "sospechoso",
      paste0(
        "La columna clasificada como fecha de nacimiento contiene ",
        paste(situaciones, collapse = " y "), "."
      ),
      paste0(
        "Se aplicaron limites de plausibilidad sin publicar las fechas ",
        "observadas."
      ),
      "Revisar los registros se\u00f1alados contra la fuente antes de corregirlos."
    )
  }
  hallazgos
}

.proteger_componentes_perfil <- function(columnas, patrones, dependencias,
                                          hallazgos, clasificacion) {
  sensibles <- unique(clasificacion$columna)
  if (!length(sensibles)) {
    return(list(
      columnas = columnas, patrones = patrones, dependencias = dependencias,
      hallazgos = hallazgos
    ))
  }
  reemplazo <- "[valor protegido]"
  indices_columnas <- columnas$columna %in% sensibles
  columnas$moda[indices_columnas & !is.na(columnas$moda)] <- reemplazo
  campos_numericos <- intersect(
    c("minimo", "maximo", "mediana"), names(columnas)
  )
  campos_texto <- intersect(
    c(
      "minimo_exacto", "maximo_exacto", "minimo_fecha", "maximo_fecha",
      "mediana_fecha"
    ),
    names(columnas)
  )
  tenia_orden <- rep(FALSE, nrow(columnas))
  for (campo in campos_numericos) {
    ocultar <- indices_columnas & !is.na(columnas[[campo]])
    tenia_orden <- tenia_orden | ocultar
    columnas[[campo]][ocultar] <- NA_real_
  }
  for (campo in campos_texto) {
    ocultar <- indices_columnas & !is.na(columnas[[campo]]) &
      nzchar(columnas[[campo]])
    tenia_orden <- tenia_orden | ocultar
    columnas[[campo]][ocultar] <- reemplazo
  }
  if (!"proteccion_estadisticos" %in% names(columnas)) {
    columnas$proteccion_estadisticos <- NA_character_
  }
  columnas$proteccion_estadisticos[tenia_orden] <-
    "[estadisticos de orden protegidos]"
  for (i in seq_along(patrones)) {
    if (names(patrones)[[i]] %in% sensibles && "ejemplos" %in% names(patrones[[i]])) {
      patrones[[i]]$ejemplos[nzchar(patrones[[i]]$ejemplos)] <- reemplazo
      resumen <- attr(patrones[[i]], "resumen_patrones", exact = TRUE)
      if (!is.null(resumen) && "ejemplos" %in% names(resumen)) {
        resumen$ejemplos[nzchar(resumen$ejemplos)] <- reemplazo
        attr(patrones[[i]], "resumen_patrones") <- resumen
      }
    }
  }
  indices_hallazgos <- !is.na(hallazgos$columna) &
    hallazgos$columna %in% sensibles &
    hallazgos$tipo_hallazgo != "dato_personal_posible"
  hallazgos$evidencia[indices_hallazgos] <- "[evidencia protegida]"
  if (nrow(dependencias)) {
    indices_dependencias <- dependencias$determinante %in% sensibles |
      dependencias$dependiente %in% sensibles
    dependencias$evidencia[indices_dependencias & nzchar(dependencias$evidencia)] <-
      "[evidencia protegida]"
  }
  list(
    columnas = columnas, patrones = patrones, dependencias = dependencias,
    hallazgos = hallazgos
  )
}

.proteger_perfil <- function(perfil) {
  protegidos <- .proteger_componentes_perfil(
    perfil$columnas, perfil$patrones, perfil$dependencias,
    perfil$hallazgos, perfil$datos_personales
  )
  perfil$columnas <- protegidos$columnas
  perfil$patrones <- protegidos$patrones
  perfil$dependencias <- protegidos$dependencias
  perfil$hallazgos <- protegidos$hallazgos
  perfil
}
