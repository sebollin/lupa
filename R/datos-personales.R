.proporcion_compatible <- function(x, patron) {
  valores <- trimws(.texto_analizable(x)$valores)
  presentes <- !is.na(valores) & nzchar(valores)
  if (!any(presentes)) return(NA_real_)
  mean(grepl(patron, valores[presentes], perl = TRUE))
}

.normalizar_validadores_personales <- function(validadores = NULL) {
  if (is.null(validadores)) validadores <- validadores_uruguay()
  if (identical(validadores, FALSE) ||
      (is.numeric(validadores) && !length(validadores))) {
    return(list())
  }
  if (!is.list(validadores) || is.null(names(validadores)) ||
      !length(validadores) || anyNA(names(validadores)) ||
      any(!nzchar(names(validadores))) || anyDuplicated(names(validadores)) ||
      !all(vapply(validadores, is.function, logical(1L)))) {
    stop("`validadores_personales` debe ser un pack o una lista con nombres de funciones.",
         call. = FALSE)
  }
  validadores
}

.proporcion_validadores <- function(textos, validadores, umbral,
                                    muestra = 1000L) {
  if (!length(validadores) || !length(textos)) return(numeric())
  indices <- if (length(textos) <= muestra) seq_along(textos) else {
    unique(round(seq(1, length(textos), length.out = muestra)))
  }
  proporciones <- vapply(validadores, function(validador) {
    parcial <- validador(textos[indices])
    if (!is.logical(parcial) || length(parcial) != length(indices)) {
      stop("Cada validador personal debe devolver un vector l\u00f3gico de igual longitud.",
           call. = FALSE)
    }
    parcial <- mean(parcial %in% TRUE)
    if (!is.finite(parcial) || parcial < umbral || length(indices) == length(textos)) {
      return(parcial)
    }
    completo <- validador(textos)
    if (!is.logical(completo) || length(completo) != length(textos)) {
      stop("Cada validador personal debe devolver un vector l\u00f3gico de igual longitud.",
           call. = FALSE)
    }
    mean(completo %in% TRUE)
  }, numeric(1L))
  proporciones
}

.clasificar_dato_personal <- function(x, nombre, inferencia,
                                      validadores = list(),
                                      umbral_verificado = 0.9,
                                      muestra_validadores = 1000L) {
  if (is.matrix(x)) {
    return(list(
      tipo = NA_character_, proporcion = NA_real_, fundamento = "",
      poder_discriminante = NA_character_, proteger = FALSE
    ))
  }
  normalizado <- .normalizar_nombre_fecha(nombre)
  reglas_nombre <- c(
    documento_identidad = paste0(
      "(^|_)(cedula|ci|dni|rut|curp|documento|documento_identidad|",
      "numero_documento|nro_documento|identificacion|id_nacional|",
      "pasaporte|passport)($|_)"
    ),
    correo = "(^|_)(correo|email|mail|casilla)($|_)",
    telefono = "(^|_)(telefono|celular|movil|contacto)($|_)",
    fecha_nacimiento = "(^|_)(fecha_nacimiento|f_nacimiento|nacimiento)($|_)",
    # El lexico de nombres no puede ser completo: una columna se puede llamar
    # de cualquier forma. Cubre las mas frecuentes en registros administrativos
    # y `columnas_personales` esta para el resto, que lo declara quien conoce
    # el dato.
    nombre = paste0(
      "(^|_)(nombre|nombres|apellido|apellidos|nombre_completo|persona|",
      "cliente|paciente|socio|beneficiario|titular|funcionario|usuario|",
      "solicitante|responsable|contribuyente)($|_)"
    ),
    domicilio = paste0(
      "(^|_)(direccion|domicilio|calle|address|residencia|",
      "lugar_residencia|barrio)($|_)"
    )
  )
  por_nombre <- names(reglas_nombre)[vapply(
    reglas_nombre, grepl, logical(1L), x = normalizado, perl = TRUE
  )]
  textos <- trimws(.texto_analizable(x)$valores)
  presentes <- !is.na(textos) & nzchar(textos)
  proporcion_correo <- if ("correo" %in% por_nombre ||
    any(grepl("@", textos[presentes], fixed = TRUE))) {
    if (any(presentes)) mean(validar_correo(textos[presentes])) else NA_real_
  } else NA_real_
  textos_presentes <- textos[presentes]
  longitudes_crudas <- nchar(textos_presentes, type = "chars")
  composicion_documental <- if (length(textos_presentes)) {
    mean(grepl("^[0-9 .-]+$", textos_presentes, perl = TRUE)) >= 0.8
  } else FALSE
  cerca_de_forma_documento <- length(longitudes_crudas) &&
    mean(longitudes_crudas >= 7L & longitudes_crudas <= 20L) >= 0.8 &&
    composicion_documental
  longitudes <- if (cerca_de_forma_documento) {
    nchar(gsub("[^0-9]", "", textos_presentes, perl = TRUE), type = "chars")
  } else integer()
  parece_fecha <- inferencia$tipo %in% c("fecha", "fecha-hora")
  forma_documento_posible <- length(longitudes) &&
    mean(longitudes >= 7L & longitudes <= 12L) >= 0.8 &&
    (!parece_fecha || "documento_identidad" %in% por_nombre)
  proporcion_documento <- if (
      "documento_identidad" %in% por_nombre || forma_documento_posible) {
    .proporcion_compatible(
      x,
      paste0(
        "^(?:[0-9]{7,12}|",
        "[0-9]{1,2}\\.?[0-9]{3}\\.?[0-9]{3}-?[0-9Kk]|",
        "[0-9]{2}(?:[ .][0-9]{3}){2}[ .][0-9]{4})$"
      )
    )
  } else NA_real_
  # Un entero de ocho digitos y una cedula escrita con puntos y guion caen los
  # dos en la forma de documento, y no son la misma evidencia. Un importe, un
  # numero de factura o un identificador de transaccion son enteros de ese
  # largo; nadie escribe un importe como `5.836.595-5`. La separacion importa
  # porque decide si, ante un validador que no verifica, corresponde proteger.
  proporcion_documento_formateado <- if (is.finite(proporcion_documento)) {
    .proporcion_compatible(
      x,
      paste0(
        "^(?:[0-9]{1,2}\\.[0-9]{3}\\.[0-9]{3}-[0-9Kk]|",
        "[0-9]{2}(?:[ .][0-9]{3}){2}[ .][0-9]{4})$"
      )
    )
  } else NA_real_
  documentos_distintos <- if (is.finite(proporcion_documento)) {
    length(unique(gsub("[^0-9]", "", textos_presentes, perl = TRUE)))
  } else 0L
  proporciones_validadores <- if (is.finite(proporcion_documento) &&
      documentos_distintos >= 3L &&
      (!length(por_nombre) || "documento_identidad" %in% por_nombre)) {
    .proporcion_validadores(
      textos_presentes, validadores, umbral_verificado,
      muestra = muestra_validadores
    )
  } else numeric()
  indice_validador <- if (length(proporciones_validadores)) {
    which.max(proporciones_validadores)
  } else integer()
  proporcion_verificada <- if (length(indice_validador)) {
    proporciones_validadores[[indice_validador]]
  } else NA_real_
  documento_verificado <- documentos_distintos >= 3L &&
    is.finite(proporcion_verificada) &&
    proporcion_verificada >= umbral_verificado

  tipo <- NA_character_
  proporcion <- NA_real_
  fundamento <- ""
  poder <- NA_character_
  proteger <- FALSE
  if (is.finite(proporcion_correo) && proporcion_correo >= 0.8) {
    tipo <- "correo"
    proporcion <- proporcion_correo
    fundamento <- "forma de correo dominante"
    poder <- "alto"
    proteger <- TRUE
  } else if ("correo" %in% por_nombre) {
    tipo <- "correo"
    proporcion <- proporcion_correo
    fundamento <- "nombre de columna"
    poder <- "medio"
    proteger <- TRUE
  } else if (length(por_nombre)) {
    tipo <- por_nombre[[1L]]
    proporcion <- switch(
      tipo,
      documento_identidad = proporcion_documento,
      fecha_nacimiento = if (inferencia$tipo %in% c("fecha", "fecha-hora")) 1 else NA_real_,
      NA_real_
    )
    fundamento <- if (tipo == "documento_identidad" &&
                      is.finite(proporcion_documento)) {
      "nombre de columna y forma compatible"
    } else "nombre de columna"
    poder <- if (tipo == "documento_identidad") "alto" else "medio"
    proteger <- TRUE
    if (tipo == "documento_identidad" && documento_verificado) {
      fundamento <- paste0(
        "nombre de columna y forma verificada por ",
        names(proporciones_validadores)[[indice_validador]]
      )
      poder <- "verificado"
    }
  } else if (is.finite(proporcion_documento) &&
             proporcion_documento >= 0.8) {
    tipo <- "documento_identidad"
    proporcion <- proporcion_documento
    if (documento_verificado) {
      fundamento <- paste0(
        "forma verificada por ", names(proporciones_validadores)[[indice_validador]]
      )
      poder <- "verificado"
      proteger <- TRUE
    } else if (isTRUE(proporcion_documento_formateado >= 0.8)) {
      # Escritos como documento y sin verificar. Es el caso de una base sucia
      # —documentos mal cargados, con digito verificador equivocado—, que es la
      # poblacion para la que existe el paquete. Ante la duda se protege, y sin
      # exigir una cantidad minima de valores distintos: una columna con un solo
      # documento repetido no identifica a nadie DENTRO de la tabla, pero si
      # identifica a una persona fuera de ella, y el hallazgo que la nombra
      # —que la columna es constante— no necesita mostrar cual es el valor.
      # La evidencia sigue declarandose debil, que es lo que es.
      fundamento <- "forma de documento con separadores, sin verificar"
      poder <- "debil"
      proteger <- TRUE
    } else {
      # Digitos pelados. Aca la forma no distingue un documento de un importe,
      # un numero de factura o un identificador de transaccion, y proteger por
      # esa sola coincidencia le sacaria los estadisticos a media tabla. La
      # sospecha se declara y no suprime nada.
      fundamento <- "forma de documento dominante"
      poder <- "debil"
      proteger <- FALSE
    }
  }
  list(
    tipo = tipo, proporcion = proporcion, fundamento = fundamento,
    poder_discriminante = poder, proteger = proteger
  )
}

.detectar_datos_personales <- function(datos, nombres, resultados,
                                       validadores = list(),
                                       umbral_verificado = 0.9,
                                       muestra_validadores = 1000L) {
  filas <- lapply(seq_along(datos), function(i) {
    clasificacion <- .clasificar_dato_personal(
      datos[[i]], nombres[[i]], resultados[[i]]$inferencia,
      validadores = validadores, umbral_verificado = umbral_verificado,
      muestra_validadores = muestra_validadores
    )
    if (is.na(clasificacion$tipo)) return(NULL)
    data.frame(
      columna = nombres[[i]],
      tipo = clasificacion$tipo,
      proporcion_compatible = clasificacion$proporcion,
      fundamento = clasificacion$fundamento,
      poder_discriminante = clasificacion$poder_discriminante,
      proteger = clasificacion$proteger,
      stringsAsFactors = FALSE
    )
  })
  filas <- filas[!vapply(filas, is.null, logical(1L))]
  resultado <- if (length(filas)) do.call(rbind, filas) else data.frame(
    columna = character(), tipo = character(),
    proporcion_compatible = numeric(), fundamento = character(),
    poder_discriminante = character(), proteger = logical(),
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
        "; poder discriminante: ", fila$poder_discriminante[[1L]],
        "; proteccion automatica: ", if (fila$proteger[[1L]]) "si" else "no",
        if (identical(fila$poder_discriminante[[1L]], "debil") &&
            isTRUE(fila$proteger[[1L]])) {
          " (por precaucion: la forma es compatible y no se pudo verificar)"
        } else "",
        if (is.finite(fila$proporcion_compatible[[1L]])) {
          paste0("; proporci\u00f3n compatible: ",
                 sprintf("%.3f", fila$proporcion_compatible[[1L]]))
        } else ""
      ),
      if (permitidos) {
        if (fila$proteger[[1L]]) {
          paste0(
            "Mantener protegidos la moda, los ejemplos, la evidencia y los ",
            "estadisticos de orden al compartir salidas."
          )
        } else {
          paste0(
        "Confirmar la semantica de la columna si se necesita decidir si ",
            "sus valores deben protegerse."
          )
        }
      } else {
        "Confirmar el contrato de la entrega antes de retirar o transformar datos."
      },
      1, 1, "columna"
    )
  })
}

# Toda columna que el clasificador reconocio como personal, aunque la evidencia
# sea debil y no corresponda suprimir sus estadisticos. Es el alcance que usa la
# evidencia de los hallazgos.
.columnas_personales_clasificadas <- function(clasificacion) {
  if (inherits(clasificacion, "perfil")) {
    clasificacion <- clasificacion$datos_personales
  }
  if (!inherits(clasificacion, "data.frame") || !nrow(clasificacion)) {
    return(character())
  }
  unique(clasificacion$columna)
}

.columnas_personales_protegidas <- function(clasificacion) {
  if (inherits(clasificacion, "perfil")) {
    clasificacion <- clasificacion$datos_personales
  }
  if (!inherits(clasificacion, "data.frame") || !nrow(clasificacion)) {
    return(character())
  }
  if (!"proteger" %in% names(clasificacion)) {
    return(unique(clasificacion$columna))
  }
  unique(clasificacion$columna[!is.na(clasificacion$proteger) &
    clasificacion$proteger])
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
      "Revisar los registros se\u00f1alados contra la fuente antes de corregirlos.",
      columnas$n[[indice]], NA_real_, "fila"
    )
  }
  hallazgos
}

.proteger_componentes_perfil <- function(columnas, patrones, dependencias,
                                          hallazgos, clasificacion) {
  sensibles <- .columnas_personales_protegidas(clasificacion)
  # Dos alcances, y la diferencia es deliberada. `sensibles` gobierna los
  # estadisticos de la tabla de columnas —minimo, maximo, moda—, donde suprimir
  # de mas tiene un costo real: una columna de importes con forma de documento
  # perderia su resumen cuantitativo. `clasificadas` gobierna la evidencia de
  # los hallazgos, donde el valor casi nunca hace falta: el hallazgo `constante`
  # dice que la columna tiene un unico valor, y para actuar sobre eso no se
  # necesita saber cual. Ahi conviene ocultar aunque la clasificacion sea debil.
  clasificadas <- .columnas_personales_clasificadas(clasificacion)
  if (!length(sensibles) && !length(clasificadas)) {
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
  if (!"detalle_proteccion_personal" %in% names(columnas)) {
    columnas$detalle_proteccion_personal <- NA_character_
  }
  columnas$detalle_proteccion_personal[tenia_orden] <-
    "[estadisticos de orden protegidos]"
  for (i in seq_along(patrones)) {
    if (names(patrones)[[i]] %in% sensibles && "ejemplos" %in% names(patrones[[i]])) {
      patrones[[i]]$ejemplos[nzchar(patrones[[i]]$ejemplos)] <- reemplazo
      resumen <- attr(patrones[[i]], "resumen_patrones", exact = TRUE)
      if (!is.null(resumen) && "ejemplos" %in% names(resumen)) {
        resumen$ejemplos[nzchar(resumen$ejemplos)] <- reemplazo
        attr(patrones[[i]], "resumen_patrones") <- resumen
      }
      desvios <- attr(patrones[[i]], "desvios_patron_raro", exact = TRUE)
      if (!is.null(desvios) && "ejemplos" %in% names(desvios)) {
        desvios$ejemplos[nzchar(desvios$ejemplos)] <- reemplazo
        attr(patrones[[i]], "desvios_patron_raro") <- desvios
      }
    }
  }
  # `columna` puede ser una columna simple o una lista de columnas
  # separadas por comas. La decisión se toma por el contenido, no por el tipo
  # de hallazgo, para que los nuevos hallazgos compuestos queden protegidos
  # automáticamente.
  columna_completa <- as.character(hallazgos$columna)
  coincide <- columna_completa %in% clasificadas |
    vapply(strsplit(columna_completa, ",", fixed = TRUE),
           function(columnas) any(trimws(columnas) %in% clasificadas),
           logical(1L))
  indices_hallazgos <- !is.na(hallazgos$columna) &
    hallazgos$tipo_hallazgo != "dato_personal_posible" & coincide
  hallazgos$evidencia[indices_hallazgos] <- "[evidencia protegida]"
  # La clave declarada es, por definicion, lo que identifica una fila: es la
  # que permite ir a verificar el caso y tambien la que identifica a una
  # persona. Si alguna de sus columnas quedo clasificada como personal, sus
  # valores salen enmascarados igual que la evidencia, en TODOS los hallazgos y
  # no solo en los de las columnas sensibles: la clave no pertenece a la
  # columna del hallazgo, viaja con la fila.
  hallazgos <- .proteger_claves_trazabilidad(hallazgos, sensibles)
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

.proteger_claves_trazabilidad <- function(hallazgos, sensibles) {
  if (!nrow(hallazgos) || !length(sensibles)) return(hallazgos)
  if (!"trazabilidad" %in% names(hallazgos)) return(hallazgos)
  hallazgos$trazabilidad <- I(lapply(hallazgos$trazabilidad, function(traza) {
    claves <- traza$claves
    if (is.null(claves) || !is.data.frame(claves) || !nrow(claves)) {
      return(traza)
    }
    protegidas <- intersect(names(claves), sensibles)
    if (!length(protegidas)) return(traza)
    for (columna in protegidas) {
      claves[[columna]] <- rep("[clave protegida]", nrow(claves))
    }
    traza$claves <- claves
    traza$claves_protegidas <- protegidas
    traza
  }))
  hallazgos
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

.proteger_duplicados_aproximados <- function(resultado, sensibles) {
  if (!inherits(resultado, "duplicados_aproximados") || !length(sensibles) ||
      !inherits(resultado$pares, "data.frame") || !nrow(resultado$pares)) {
    return(resultado)
  }
  resultado$pares$evidencia_1 <- "[valor protegido]"
  resultado$pares$evidencia_2 <- "[valor protegido]"
  resultado$pares$proteccion_evidencia <- "[valores personales protegidos]"
  resultado$proteccion_aplicada <- TRUE
  resultado
}
