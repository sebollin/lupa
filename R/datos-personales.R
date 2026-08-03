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
    cedula = "(^|_)(cedula|ci|documento_identidad|numero_documento|nro_documento)($|_)",
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
  proporcion_cedula <- if ("cedula" %in% por_nombre || forma_documento_posible) {
    .proporcion_compatible(
      x, "^(?:[0-9]{1,2}\\.?[0-9]{3}\\.?[0-9]{3}-?[0-9]|[0-9]{7,8})$"
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
  } else if ("cedula" %in% por_nombre ||
             (is.finite(proporcion_cedula) && proporcion_cedula >= 0.8)) {
    tipo <- "cedula"
    proporcion <- proporcion_cedula
    fundamento <- if ("cedula" %in% por_nombre) {
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
        "Mantener protegidos la moda, los ejemplos y la evidencia al compartir salidas."
      } else {
        "Confirmar el contrato de la entrega antes de retirar o transformar datos."
      }
    )
  })
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
