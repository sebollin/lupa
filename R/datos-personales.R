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

# Cuantos valores puede recorrer la confirmacion completa. Cuando la muestra
# supera el umbral, el validador se vuelve a correr para confirmar sobre la
# columna entera, y sobre una tabla de millones de filas eso es una pasada que
# nadie pidio. El tope la acota, y lo que se evaluo viaja con el resultado.
.max_validacion_completa <- 200000L

# Un alcance parcial se dice; uno completo no necesita decirse.
.alcance_validacion <- function(proporciones, indice) {
  evaluados <- attr(proporciones, "n_evaluados", exact = TRUE)
  total <- attr(proporciones, "n_total", exact = TRUE)
  if (is.null(evaluados) || is.null(total) || is.na(evaluados[[indice]]) ||
      evaluados[[indice]] >= total) {
    return("")
  }
  paste0(" sobre ", evaluados[[indice]], " de ", total, " valores")
}

.proporcion_validadores <- function(textos, validadores, umbral,
                                    muestra = 1000L,
                                    max_completo = .max_validacion_completa) {
  if (!length(validadores) || !length(textos)) return(numeric())
  indices <- if (length(textos) <= muestra) seq_along(textos) else {
    unique(round(seq(1, length(textos), length.out = muestra)))
  }
  evaluados <- rep(NA_integer_, length(validadores))
  proporciones <- vapply(seq_along(validadores), function(k) {
    validador <- validadores[[k]]
    parcial <- validador(textos[indices])
    if (!is.logical(parcial) || length(parcial) != length(indices)) {
      stop("Cada validador personal debe devolver un vector l\u00f3gico de igual longitud.",
           call. = FALSE)
    }
    parcial <- mean(parcial %in% TRUE)
    evaluados[[k]] <<- length(indices)
    if (!is.finite(parcial) || parcial < umbral || length(indices) == length(textos)) {
      return(parcial)
    }
    confirmacion <- if (length(textos) <= max_completo) {
      seq_along(textos)
    } else {
      unique(round(seq(1, length(textos), length.out = max_completo)))
    }
    completo <- validador(textos[confirmacion])
    if (!is.logical(completo) || length(completo) != length(confirmacion)) {
      stop("Cada validador personal debe devolver un vector l\u00f3gico de igual longitud.",
           call. = FALSE)
    }
    evaluados[[k]] <<- length(confirmacion)
    mean(completo %in% TRUE)
  }, numeric(1L))
  names(proporciones) <- names(validadores)
  # El conteo viaja con la proporcion para que el fundamento pueda decir sobre
  # cuantos valores se confirmo: una proporcion medida sobre una parte no es
  # una proporcion de la columna.
  attr(proporciones, "n_evaluados") <- evaluados
  attr(proporciones, "n_total") <- length(textos)
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
    fecha_fallecimiento = paste0(
      "(^|_)(fallecimiento|defuncion|deceso|obito|fecha_muerte|",
      "f_fallecimiento|fecha_defuncion)($|_)"
    ),
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
  # Un correo escrito `usuario at dominio punto com` no es un correo valido y
  # `validar_correo()` tiene razon en decir que no lo es: eso es una medida de
  # formato. Pero la columna sigue trayendo direcciones de personas, y esa es
  # una pregunta distinta. Las dos respuestas conviven: el validador mide la
  # forma, el clasificador decide si hay dato personal.
  proporcion_correo_ofuscado <- if (any(presentes)) {
    mean(grepl(
      paste0(
        "^[A-Za-z0-9._%+-]+[[:space:]]*(?:\\(|\\[)?(?:at|arroba)(?:\\)|\\])?",
        "[[:space:]]*[A-Za-z0-9-]+",
        # El dominio tiene que traer su separador: sin el, `lunes at casa` es
        # una frase y no una direccion, y protegerla seria inventar el dato.
        "(?:[[:space:]]*(?:\\(|\\[)?(?:dot|punto)(?:\\)|\\])?[[:space:]]*|\\.)",
        "[A-Za-z]{2,}$"
      ),
      textos[presentes], perl = TRUE, ignore.case = TRUE
    ))
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
  } else if (is.finite(proporcion_correo_ofuscado) &&
             proporcion_correo_ofuscado >= 0.8) {
    tipo <- "correo"
    proporcion <- proporcion_correo_ofuscado
    fundamento <- "forma de correo ofuscada dominante"
    poder <- "debil"
    proteger <- TRUE
  } else if (length(por_nombre)) {
    tipo <- por_nombre[[1L]]
    proporcion <- switch(
      tipo,
      documento_identidad = proporcion_documento,
      fecha_nacimiento = if (inferencia$tipo %in% c("fecha", "fecha-hora")) 1 else NA_real_,
      fecha_fallecimiento = if (inferencia$tipo %in% c("fecha", "fecha-hora")) 1 else NA_real_,
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
        names(proporciones_validadores)[[indice_validador]],
        .alcance_validacion(proporciones_validadores, indice_validador)
      )
      poder <- "verificado"
    }
  } else if (is.finite(proporcion_documento) &&
             proporcion_documento >= 0.8) {
    tipo <- "documento_identidad"
    proporcion <- proporcion_documento
    if (documento_verificado) {
      fundamento <- paste0(
        "forma verificada por ",
        names(proporciones_validadores)[[indice_validador]],
        .alcance_validacion(proporciones_validadores, indice_validador)
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

# Los tipos que el paquete sabe nombrar. Declarar uno de estos hace que la
# columna se trate igual que si el lexico la hubiera reconocido; declarar otro
# nombre tambien vale, y viaja tal cual, porque el usuario puede conocer una
# categoria que el paquete no tiene.
# No hay lista cerrada a proposito: hubo una y no la miraba nadie, que es peor
# que no tenerla porque se lee como una lista blanca que no existe.

# El lexico de nombres de columna no puede ser completo: una columna con
# documentos se puede llamar `cod_benef` y ninguna lista de nombres la va a
# reconocer. `columnas_personales` es la salida correcta a ese limite, y es el
# mismo patron que `columnas_opcionales`: lo declara quien conoce el dato, y lo
# declarado gana sobre lo inferido.
.normalizar_columnas_personales <- function(columnas_personales, nombres) {
  if (is.null(columnas_personales)) return(character())
  if (!is.character(columnas_personales) || anyNA(columnas_personales)) {
    stop(
      "`columnas_personales` debe ser un vector de texto: nombres de columna, ",
      "o un vector con nombre donde el nombre es la columna y el valor es el ",
      "tipo de dato personal.", call. = FALSE
    )
  }
  if (!length(columnas_personales)) return(character())
  etiquetas <- names(columnas_personales)
  declaradas <- if (is.null(etiquetas) || !all(nzchar(etiquetas))) {
    if (!is.null(etiquetas) && any(nzchar(etiquetas))) {
      stop(
        "`columnas_personales` mezcla elementos con nombre y sin nombre. ",
        "Corresponde una sola forma: o todos nombres de columna, o todos ",
        "`columna = \"tipo\"`.", call. = FALSE
      )
    }
    stats::setNames(rep("declarado", length(columnas_personales)),
                    columnas_personales)
  } else {
    stats::setNames(as.character(columnas_personales), etiquetas)
  }
  if (any(!nzchar(names(declaradas)))) {
    stop("`columnas_personales` nombra una columna vac\u00eda.", call. = FALSE)
  }
  if (anyDuplicated(names(declaradas))) {
    stop("`columnas_personales` repite una columna.", call. = FALSE)
  }
  desconocidas <- setdiff(names(declaradas), nombres)
  if (length(desconocidas)) {
    stop("`columnas_personales` nombra columnas inexistentes: ",
         paste(desconocidas, collapse = ", "), ".", call. = FALSE)
  }
  vacios <- names(declaradas)[!nzchar(declaradas)]
  if (length(vacios)) {
    stop("`columnas_personales` declara un tipo vac\u00edo en: ",
         paste(vacios, collapse = ", "), ".", call. = FALSE)
  }
  declaradas
}

.detectar_datos_personales <- function(datos, nombres, resultados,
                                       validadores = list(),
                                       umbral_verificado = 0.9,
                                       muestra_validadores = 1000L,
                                       declaradas = character()) {
  filas <- lapply(seq_along(datos), function(i) {
    if (nombres[[i]] %in% names(declaradas)) {
      # Lo declarado no se vuelve a inferir. El paquete no tiene con que
      # contradecir a quien conoce el dato, y una columna declarada personal
      # que el lexico no reconoce es justamente el caso que esto resuelve.
      return(data.frame(
        columna = nombres[[i]],
        tipo = unname(declaradas[[nombres[[i]]]]),
        proporcion_compatible = NA_real_,
        fundamento = "declarado con `columnas_personales`",
        poder_discriminante = "declarado",
        proteger = TRUE,
        stringsAsFactors = FALSE
      ))
    }
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

.hallazgos_rango_fecha_personal <- function(columnas, clasificacion,
                                            fecha_referencia, tipo) {
  configuraciones <- list(
    fecha_nacimiento = list(
      hallazgo = "fecha_nacimiento_fuera_rango",
      descripcion = "fecha de nacimiento"
    ),
    fecha_fallecimiento = list(
      hallazgo = "fecha_fallecimiento_fuera_rango",
      descripcion = "fecha de fallecimiento"
    )
  )
  configuracion <- configuraciones[[tipo]]
  if (is.null(configuracion)) return(list())
  fechas <- clasificacion[clasificacion$tipo == tipo, , drop = FALSE]
  if (!nrow(fechas)) return(list())
  limite_inferior <- as.Date("1900-01-01")
  limite_superior <- as.Date(fecha_referencia, tz = "UTC")
  hallazgos <- list()
  for (i in seq_len(nrow(fechas))) {
    nombre <- fechas$columna[[i]]
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
      nombre, configuracion$hallazgo,
      if (futura) "error" else "sospechoso",
      paste0(
        "La columna clasificada como ", configuracion$descripcion,
        " contiene ",
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

.hallazgos_rango_nacimiento <- function(columnas, clasificacion,
                                        fecha_referencia) {
  .hallazgos_rango_fecha_personal(
    columnas, clasificacion, fecha_referencia, "fecha_nacimiento"
  )
}

# La proteccion no puede depender de que cada diagnostico recuerde llamar a un
# enmascarador. Las reglas se escriben en archivos distintos y varias de ellas
# arman prosa con valores de filas; si una queda fuera, el dato llega igual a
# `hallazgos`, al HTML o al plan. Este es el inventario de representaciones que
# puede publicar una salida sin conservar la columna original.
.valores_publicables_protegidos <- function(datos, sensibles) {
  if (!inherits(datos, "data.frame") || !length(sensibles)) {
    return(character())
  }
  valores <- unlist(lapply(intersect(names(datos), sensibles), function(nombre) {
    x <- datos[[nombre]]
    if (is.data.frame(x) || is.matrix(x) || is.list(x)) return(character())
    crudos <- tryCatch(as.character(x), error = function(e) character())
    formateados <- tryCatch(c(
      format(x, digits = 15L, trim = TRUE, scientific = FALSE),
      format(x, digits = 8L, trim = TRUE, scientific = FALSE),
      vapply(seq_along(x), function(i) .texto_valor(x[i]), character(1L))
    ), error = function(e) character())
    c(crudos, formateados)
  }), use.names = FALSE)
  valores <- unique(valores[!is.na(valores) & nzchar(valores)])
  # Los valores largos van primero para que `12` no deje restos dentro de un
  # identificador `312`. La sustitucion es fija: estos textos ya son valores,
  # no expresiones regulares.
  valores[order(nchar(valores, type = "bytes"), decreasing = TRUE,
                method = "radix")]
}

.valores_perfil_protegidos <- function(columnas, patrones, clasificacion,
                                       meta = NULL) {
  sensibles <- .columnas_personales_protegidas(clasificacion)
  if (!inherits(columnas, "data.frame") || !length(sensibles)) {
    return(character())
  }
  indices <- which(columnas$columna %in% sensibles)
  campos <- intersect(
    c(
      "moda", "minimo", "maximo", "mediana", "centinela_valor",
      "minimo_exacto", "maximo_exacto", "minimo_fecha", "maximo_fecha",
      "media_fecha", "mediana_fecha"
    ),
    names(columnas)
  )
  valores <- unlist(lapply(campos, function(campo) {
    tryCatch(as.character(columnas[[campo]][indices]),
             error = function(e) character())
  }), use.names = FALSE)
  if (length(patrones) && length(indices)) {
    nombres <- names(patrones)
    for (i in which(nombres %in% sensibles)) {
      tabla <- patrones[[i]]
      if (inherits(tabla, "data.frame") && "ejemplos" %in% names(tabla)) {
        valores <- c(valores, as.character(tabla$ejemplos))
      }
      for (atributo in c("resumen_patrones", "desvios_patron_raro")) {
        resumen <- attr(tabla, atributo, exact = TRUE)
        if (inherits(resumen, "data.frame") && "ejemplos" %in% names(resumen)) {
          valores <- c(valores, as.character(resumen$ejemplos))
        }
      }
    }
  }
  # Si el perfil abierto conserva una declaracion de centinelas, se trata como
  # una posible representacion del dato. Cuando no hay `datos` para decidir a
  # que columna corresponde, ocultarlos todos es la opcion segura.
  if (is.list(meta) && length(meta$sentinelas_numericos)) {
    valores <- c(valores, as.character(meta$sentinelas_numericos))
  }
  valores <- unique(valores[!is.na(valores) & nzchar(valores)])
  valores[order(nchar(valores, type = "bytes"), decreasing = TRUE,
                method = "radix")]
}

.reemplazar_valores_protegidos <- function(x, valores) {
  if (!is.character(x) || !length(valores)) return(x)
  for (valor in valores) {
    largo <- nchar(valor, type = "bytes")
    if (is.na(largo) || !largo) next
    if (largo < 3L) {
      escapado <- gsub(
        "([][{}()+*^$|\\\\?.])", "\\\\\\1", valor,
        fixed = FALSE, useBytes = TRUE
      )
      patron <- paste0(
        "(?<![[:alnum:]_])", escapado, "(?![[:alnum:]_])"
      )
      x <- tryCatch(
        gsub(patron, "[valor protegido]", x, perl = TRUE, useBytes = TRUE),
        error = function(e) gsub(
          valor, "[valor protegido]", x, fixed = TRUE, useBytes = TRUE
        )
      )
    } else {
      x <- gsub(valor, "[valor protegido]", x, fixed = TRUE, useBytes = TRUE)
    }
  }
  x
}

# Recorre la parte textual de una salida sin convertir estadisticos numericos
# que no sean valores de celda. Los parametros del plan tienen un recorrido
# adicional para quitar tambien valores numericos que todavia no se volvieron
# texto.
.proteger_textos_salida <- function(x, valores) {
  if (!length(valores)) return(x)
  atributos <- attributes(x)
  estructurales <- c("names", "class", "row.names", "dim", "dimnames")
  adicionales <- setdiff(names(atributos), estructurales)
  for (atributo in adicionales) {
    attr(x, atributo) <- .proteger_textos_salida(
      attr(x, atributo, exact = TRUE), valores
    )
  }
  if (inherits(x, "data.frame")) {
    for (j in seq_along(x)) {
      columna <- x[[j]]
      if (is.character(columna)) {
        x[[j]] <- .reemplazar_valores_protegidos(columna, valores)
      } else if (is.factor(columna)) {
        levels(columna) <- .reemplazar_valores_protegidos(
          levels(columna), valores
        )
        x[[j]] <- columna
      } else if (is.list(columna)) {
        x[[j]] <- lapply(columna, .proteger_textos_salida, valores = valores)
      }
    }
    return(x)
  }
  if (is.list(x)) {
    x[] <- lapply(x, .proteger_textos_salida, valores = valores)
    return(x)
  }
  .reemplazar_valores_protegidos(x, valores)
}

.proteger_numeros_parametros <- function(x, valores) {
  if (!length(valores)) return(x)
  if (inherits(x, "data.frame")) {
    for (j in seq_along(x)) {
      if (is.list(x[[j]])) {
        x[[j]] <- lapply(x[[j]], .proteger_numeros_parametros,
                         valores = valores)
      } else {
        x[[j]] <- .proteger_numeros_parametros(x[[j]], valores)
      }
    }
    return(x)
  }
  if (is.list(x)) {
    x[] <- lapply(x, .proteger_numeros_parametros, valores = valores)
    return(x)
  }
  if (!is.numeric(x) || inherits(x, c("Date", "POSIXt"))) return(x)
  textos <- tryCatch(
    as.character(x),
    error = function(e) rep(NA_character_, length(x))
  )
  protegidos <- !is.na(textos) & textos %in% valores
  if (any(protegidos)) x[protegidos] <- NA
  x
}

# La accion sigue siendo visible, pero sus argumentos no pueden llevarse un
# valor personal de vuelta a la tabla. Un plan con parametros enmascarados es
# deliberadamente informativo: quien necesite ejecutarlo debe confirmar esos
# valores en la fuente protegida. La imputacion es una excepcion operativa: el
# mapa privado no viaja, pero sus columnas y su soporte si; `aplicar()` vuelve a
# resolver la relacion sobre los datos que recibe.
.proteger_plan_limpieza <- function(plan, perfil, datos = NULL) {
  if (!inherits(plan, "data.frame")) return(plan)
  sensibles <- .columnas_personales_protegidas(perfil)
  indices_dependencias <- which(startsWith(
    as.character(plan$estrategia), "imputar_dependencia_funcional__"
  ))
  if (length(indices_dependencias) && is.list(plan$parametros)) {
    for (indice in indices_dependencias) {
      parametros <- plan$parametros[[indice]]
      if (!is.list(parametros)) next
      involucra_protegida <- any(c(
        parametros$determinante, parametros$dependiente
      ) %in% sensibles)
      if (isTRUE(involucra_protegida)) {
        parametros$mapa_enmascarado <- TRUE
        plan$parametros[[indice]] <- parametros
      }
    }
  }
  valores <- .valores_perfil_protegidos(
    perfil$columnas, perfil$patrones, perfil$datos_personales, perfil$meta
  )
  valores <- unique(c(
    valores, .valores_publicables_protegidos(datos, sensibles)
  ))
  valores <- valores[!is.na(valores) & nzchar(valores)]
  valores <- valores[order(nchar(valores, type = "bytes"), decreasing = TRUE,
                           method = "radix")]
  if (!length(valores)) {
    attr(plan, "columnas_datos_personales_protegidas") <- sensibles
    return(plan)
  }
  plan <- .proteger_textos_salida(plan, valores)
  if ("parametros" %in% names(plan) && is.list(plan$parametros)) {
    plan$parametros <- I(lapply(
      plan$parametros,
      function(parametros) .proteger_numeros_parametros(parametros, valores)
    ))
  }
  cobertura <- attr(plan, "cobertura_diagnosticos", exact = TRUE)
  if (inherits(cobertura, "data.frame")) {
    attr(plan, "cobertura_diagnosticos") <- .proteger_textos_salida(
      cobertura, valores
    )
  }
  # `guiar_limpieza()` vuelve a consultar los datos de origen para construir
  # ejemplos. Llevar sólo los nombres de columnas permite enmascararlos en esa
  # salida sin serializar ni publicar los valores protegidos.
  attr(plan, "columnas_datos_personales_protegidas") <- sensibles
  plan
}

.proteger_ausencia_estructural <- function(hallazgos, sensibles) {
  if (!nrow(hallazgos) || !length(sensibles)) return(hallazgos)
  indices <- which(hallazgos$tipo_hallazgo ==
                     "posible_ausencia_estructural")
  if (!length(indices)) return(hallazgos)
  for (i in indices) {
    evidencia <- as.character(hallazgos$evidencia[[i]])
    encontrado <- regexec("^`([^`]*)` predice", evidencia, perl = TRUE)
    partes <- regmatches(evidencia, encontrado)[[1L]]
    determinante <- if (length(partes) >= 2L) partes[[2L]] else NA_character_
    if (is.na(determinante) || !determinante %in% sensibles) next
    inicio <- sub("\\. La columna corresponde.*$", "", evidencia)
    if (identical(inicio, evidencia)) next
    tipo_criterio <- if (grepl("por un umbral", evidencia, fixed = TRUE)) {
      "un umbral"
    } else {
      "niveles"
    }
    hallazgos$evidencia[[i]] <- paste0(
      inicio,
      ". La columna corresponde segun ", tipo_criterio,
      " de `", determinante,
      "` que no se publica porque esa columna esta protegida."
    )
    columna <- as.character(hallazgos$columna[[i]])
    hallazgos$sugerencia[[i]] <- paste0(
      "Si es asi, declararlo y volver a perfilar con `aplicabilidad` usando en `",
      determinante,
      "` el criterio confirmado en la fuente. El criterio no se reproduce aqui",
      " porque esa columna esta protegida. Con la regla declarada, la ausencia",
      " fuera de ese universo deja de contarse como defecto y el alcance queda",
      " escrito en `cobertura_diagnosticos` para `", columna, "`."
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
  ocultar_moda <- indices_columnas & !is.na(columnas$moda) &
    columnas$moda != reemplazo
  columnas$moda[ocultar_moda] <- reemplazo
  # `media` entra por lo mismo que la via DBI ya la tapaba, y con el mismo
  # argumento que esta escrito alla: la media de las cedulas de una tabla chica
  # reconstruye demasiado. Que una puerta la ocultara y la otra la publicara
  # significaba que la proteccion dependia de por donde entraras: la misma
  # columna de documentos salia con `media = 5108024` por `perfilar()` y con
  # `media = NA` por `perfilar_dbi()`.
  # `centinela_valor` publica un valor de celda de la columna, igual que el
  # minimo o la moda, asi que tiene que taparse por el mismo motivo. Quedaba
  # afuera y sobre una columna de documentos protegida el perfil mostraba
  # `moda = "[valor protegido]"`, `minimo = NA`... y `centinela_valor = 9999`.
  # Que el valor sea casi seguro un centinela y no un documento no cambia la
  # regla: la proteccion no adivina cuales valores son inocentes.
  # Los campos de la secuencia entera codifican el rango aunque no lo muestren:
  # `n_posiciones` es `maximo - minimo + 1`, `n_huecos` es eso menos los
  # distintos, y la densidad es los distintos sobre eso. Con cualquiera de los
  # tres y un segundo dato del mismo perfil se despeja el par.
  #
  # Medido sobre una columna de cedulas protegida: `n_posiciones` = 599.891 junto
  # con los ordenes de magnitud que publica Benford -log10(maximo/minimo)- son
  # dos ecuaciones con dos incognitas y devuelven **27 y 599.917 exactos**,
  # mientras `minimo` y `maximo` salian en NA como corresponde. Proteger el
  # minimo y el maximo y dejar publicado su rango no protege nada.
  campos_numericos <- intersect(
    c(
      "minimo", "maximo", "mediana", "media", "centinela_valor",
      "n_posiciones_secuencia_entera", "n_huecos_secuencia_entera",
      "hueco_maximo_secuencia_entera", "densidad_secuencia_entera",
      "densidad_sin_centinela"
    ),
    names(columnas)
  )
  campos_momento <- c("media", "media_fecha")
  campos_texto <- intersect(
    c(
      "minimo_exacto", "maximo_exacto", "minimo_fecha", "maximo_fecha",
      "media_fecha", "mediana_fecha"
    ),
    names(columnas)
  )
  # La moda es un valor observado, igual que los extremos. Si se reemplazo,
  # el detalle tiene que declararlo aunque la columna no tenga otros campos de
  # orden para tapar.
  tenia_orden <- ocultar_moda
  tenia_momento <- rep(FALSE, nrow(columnas))
  for (campo in campos_numericos) {
    ocultar <- indices_columnas & !is.na(columnas[[campo]])
    if (campo %in% campos_momento) {
      tenia_momento <- tenia_momento | ocultar
    } else {
      tenia_orden <- tenia_orden | ocultar
    }
    columnas[[campo]][ocultar] <- NA_real_
  }
  for (campo in campos_texto) {
    ocultar <- indices_columnas & !is.na(columnas[[campo]]) &
      nzchar(columnas[[campo]])
    if (campo %in% campos_momento) {
      tenia_momento <- tenia_momento | ocultar
    } else {
      tenia_orden <- tenia_orden | ocultar
    }
    columnas[[campo]][ocultar] <- reemplazo
  }
  # La caja envolvente es una estadistica espacial distinta de los extremos
  # numericos, pero sobre domicilios publica coordenadas de personas. Se
  # conserva el bloque geometrico y su alcance, ocultando sólo los cuatro
  # valores de coordenadas.
  campos_bbox <- intersect(
    c("bbox_xmin", "bbox_xmax", "bbox_ymin", "bbox_ymax"),
    names(columnas)
  )
  geometria <- rep(FALSE, nrow(columnas))
  if ("representacion_geometrica" %in% names(columnas)) {
    geometria <- geometria | !is.na(columnas$representacion_geometrica)
  }
  if ("tipo_geometria" %in% names(columnas)) {
    geometria <- geometria | !is.na(columnas$tipo_geometria)
  }
  if ("bbox_alcance" %in% names(columnas)) {
    geometria <- geometria | !is.na(columnas$bbox_alcance)
  }
  geometria <- indices_columnas & geometria
  if (length(campos_bbox)) {
    for (campo in campos_bbox) {
      ocultar <- geometria & !is.na(columnas[[campo]])
      columnas[[campo]][ocultar] <- NA_real_
    }
  }
  if ("bbox_alcance" %in% names(columnas)) {
    columnas$bbox_alcance[geometria] <-
      "no_publicado_por_geometria_protegida"
  }
  if (!"detalle_proteccion_personal" %in% names(columnas)) {
    columnas$detalle_proteccion_personal <- NA_character_
  }
  # El texto dice lo que de verdad se tapo. Decir "y momentos" cuando la media
  # ya era NA seria declarar una proteccion que no se aplico.
  columnas$detalle_proteccion_personal[tenia_orden & !tenia_momento] <-
    "[estadisticos de orden protegidos]"
  columnas$detalle_proteccion_personal[tenia_momento & !tenia_orden] <-
    "[momentos protegidos]"
  columnas$detalle_proteccion_personal[tenia_orden & tenia_momento] <-
    "[estadisticos de orden y momentos protegidos]"
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
  # La evidencia se tapaba y la descripcion no, y hay hallazgos que nombran un
  # valor de celda ahi adentro: `posible_centinela_numerico` decia "El valor
  # 9999 aparece 5 veces". Sobre una columna de documentos protegida eso es una
  # fuga por la puerta de al lado, y llegaba hasta el informe HTML.
  #
  # No se tapa la descripcion entera, que explica el diagnostico y hay que poder
  # leerla: se saca el valor y se dice que se saco.
  con_valor <- indices_hallazgos &
    grepl("^El valor ", as.character(hallazgos$descripcion))
  if (any(con_valor)) {
    hallazgos$descripcion[con_valor] <- sub(
      "^El valor [^ ]+ aparece", "Un valor aparece",
      as.character(hallazgos$descripcion[con_valor])
    )
    hallazgos$descripcion[con_valor] <- paste(
      hallazgos$descripcion[con_valor],
      "El valor no se nombra porque la columna esta protegida."
    )
  }
  # La clave declarada es, por definicion, lo que identifica una fila: es la
  # que permite ir a verificar el caso y tambien la que identifica a una
  # persona. Si alguna de sus columnas quedo clasificada como personal, sus
  # valores salen enmascarados igual que la evidencia, en TODOS los hallazgos y
  # no solo en los de las columnas sensibles: la clave no pertenece a la
  # columna del hallazgo, viaja con la fila.
  hallazgos <- .proteger_claves_trazabilidad(hallazgos, sensibles)
  # La ausencia estructural es una señal válida aunque el determinante sea
  # personal. Se conserva la predicción y su precisión, pero no el corte ni los
  # niveles que permitirían reconstruir valores de la columna protegida.
  hallazgos <- .proteger_ausencia_estructural(hallazgos, sensibles)
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

# `meta` y `cobertura_diagnosticos` quedaban fuera de la proteccion, y las dos
# publican cantidades derivadas de los extremos: Benford guarda
# `ordenes_magnitud`, que es log10(maximo/minimo), y el motivo de la cobertura lo
# imprime en el texto. Ninguno muestra un valor de celda, y por eso se pasaron
# por alto: lo que se filtra no es el dato sino la relacion entre dos datos, que
# con otra del mismo perfil se despeja.
.proteger_meta_y_cobertura <- function(perfil, sensibles,
                                       valores = character()) {
  if (!length(sensibles)) return(perfil)
  if (length(perfil$meta$sentinelas_numericos)) {
    if (length(valores)) {
      perfil$meta$sentinelas_numericos <- .proteger_numeros_parametros(
        perfil$meta$sentinelas_numericos, valores
      )
    } else {
      # Un perfil abierto puede llegar a `reportar()` sin conservar `datos`.
      # En ese caso no se puede saber que centinela pertenece a que columna;
      # conservar la lista completa publicaria uno si coincide con una columna
      # protegida, asi que se oculta de forma conservadora.
      perfil$meta$sentinelas_numericos <- rep(
        NA_real_, length(perfil$meta$sentinelas_numericos)
      )
    }
  }
  resultados <- perfil$meta$benford$resultados
  if (length(resultados)) {
    for (nombre in intersect(names(resultados), sensibles)) {
      perfil$meta$benford$resultados[[nombre]]$ordenes_magnitud <- NA_real_
    }
  }
  cobertura <- perfil$cobertura_diagnosticos
  if (inherits(cobertura, "data.frame") && nrow(cobertura) &&
        "columna" %in% names(cobertura) && "motivo" %in% names(cobertura)) {
    afectadas <- as.character(cobertura$columna) %in% sensibles
    if (any(afectadas)) {
      cobertura$motivo[afectadas] <- gsub(
        "log10\\(max/min\\)[[:space:]]*[-+0-9.eE]+",
        "log10(max/min) [valor protegido]",
        as.character(cobertura$motivo[afectadas])
      )
      perfil$cobertura_diagnosticos <- cobertura
    }
  }
  perfil
}

.proteger_perfil <- function(perfil, datos = NULL) {
  valores_perfil <- .valores_perfil_protegidos(
    perfil$columnas, perfil$patrones, perfil$datos_personales, perfil$meta
  )
  protegidos <- .proteger_componentes_perfil(
    perfil$columnas, perfil$patrones, perfil$dependencias,
    perfil$hallazgos, perfil$datos_personales
  )
  perfil$columnas <- protegidos$columnas
  perfil$patrones <- protegidos$patrones
  perfil$dependencias <- protegidos$dependencias
  perfil$hallazgos <- protegidos$hallazgos
  sensibles <- as.character(protegidos$columnas$columna[
    !is.na(protegidos$columnas$tipo_dato_personal)
  ])
  valores <- .valores_publicables_protegidos(
    datos, .columnas_personales_protegidas(perfil)
  )
  valores <- unique(c(valores_perfil, valores))
  valores <- valores[!is.na(valores) & nzchar(valores)]
  valores <- valores[order(nchar(valores, type = "bytes"), decreasing = TRUE,
                           method = "radix")]
  perfil <- .proteger_meta_y_cobertura(perfil, sensibles, valores)
  .proteger_textos_salida(perfil, valores)
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
