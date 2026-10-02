# La representacion numerica que alimenta un patron no puede quedar a merced de
# `options(scipen)` ni de `options(digits)`: una misma columna no puede cambiar
# de categoria porque la sesion eligio notacion cientifica. `OutDec` tambien se
# fija aqui porque esta conversion es para decidir una forma, no para publicar
# un numero; `.formatear_numero_publicado()` sigue siendo la unica conversion
# de numeros del paquete.
.textos_para_clasificacion_personal <- function(x) {
  textos <- .texto_analizable(x)$valores
  if (is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) {
    opciones <- options(OutDec = ".")
    on.exit(options(opciones), add = TRUE)
    textos <- if (is.double(x) && !inherits(x, "integer64") || is.integer(x)) {
      .formatear_numeros_uno_a_uno(x)
    } else {
      vapply(
        seq_along(x), function(i) .formatear_numero_publicado(x[[i]]),
        character(1L), USE.NAMES = FALSE
      )
    }
  }
  trimws(textos)
}

# Lo mismo que `.formatear_numero_publicado()` valor por valor -15 cifras, sin
# notacion cientifica, cada numero con sus propias cifras- pero en una pasada.
# Se llamaba una vez POR VALOR: en una tabla de 200.000 filas, el 20 % del tiempo
# de `perfilar()` se iba en formatear numeros para clasificarlos como dato
# personal. `as.character()` con `scipen` alto da el mismo texto -medido sobre
# 1,3 millones de valores de todas las magnitudes y signos: cero diferencias-,
# salvo en los valores no nulos por debajo de 1e-4, donde `format()` deja ceros
# finales; esos, pocos, siguen por el camino de uno en uno. Un `integer64` no
# entra aca: `as.double()` lo redondearia.
.formatear_numeros_uno_a_uno <- function(x) {
  salida <- rep(NA_character_, length(x))
  utiles <- !is.na(x)
  if (!any(utiles)) return(salida)
  valores <- as.double(x[utiles]) + 0
  # `OutDec` NO se toca aca: quien llama decide la marca -la clasificacion la fija
  # en "." y la cosecha de valores protegidos usa la de la sesion, que es con la
  # que salen publicados-. `as.character()` y `format()` la respetan las dos.
  opciones <- options(scipen = 999)
  on.exit(options(opciones), add = TRUE)
  texto <- as.character(valores)
  chicos <- is.finite(valores) & valores != 0 & abs(valores) < 1e-4
  if (any(chicos)) {
    texto[chicos] <- vapply(
      valores[chicos], .formatear_numero_publicado, character(1L)
    )
  }
  salida[utiles] <- texto
  salida
}

# La proporcion se calcula sobre los valores que se pueden juzgar -un blanco no
# tiene forma que comparar contra un patron- y por eso los excluye. Pero ese
# denominador se publicaba sin decirse: una columna con ocho documentos y treinta
# y dos blancos publicaba `proporcion_compatible = 1`, o sea "100 % compatible",
# calculado sobre ocho de cuarenta valores.
#
# No se cambia el denominador -incluir los blancos convertiria toda columna
# medio vacia en incompatible, que es un falso positivo peor-. Se declara sobre
# cuantos se calculo, que es lo que faltaba para poder leer el numero.
.proporcion_compatible <- function(x, patron) {
  valores <- .textos_para_clasificacion_personal(x)
  presentes <- !is.na(valores) & nzchar(valores)
  if (!any(presentes)) {
    return(structure(NA_real_, evaluados = 0L, totales = length(valores)))
  }
  structure(
    mean(grepl(patron, valores[presentes], perl = TRUE)),
    evaluados = as.integer(sum(presentes)),
    totales = length(valores)
  )
}

.normalizar_validadores_personales <- function(validadores = NULL) {
  if (is.null(validadores)) validadores <- validadores_uruguay()
  if (identical(validadores, FALSE) ||
      (is.numeric(validadores) && !length(validadores))) {
    return(list())
  }
  if (!is.list(validadores) || is.null(names(validadores)) ||
      !length(validadores) || anyNA(names(validadores)) ||
      any(!nzchar(names(validadores))) ||
      anyDuplicated(.nombres_para_operar(names(validadores))) ||
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
  if (.es_columna_compuesta(x)) {
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
    # El patron abreviaba la PRIMERA palabra (`f_nacimiento`) y no la segunda,
    # asi que `fecha_nac` -de las formas mas frecuentes en registros
    # administrativos de la region- no se clasificaba y la fecha de nacimiento
    # quedaba sin proteger. Se enumeraron las cuatro formas de abreviar en vez
    # de agregar la que aparecio: entera, primera palabra, segunda palabra, las
    # dos, y pegada. Con dos casos parecia una convencion; con cinco era un
    # hueco.
    fecha_nacimiento = paste0(
      "(^|_)(fecha_nacimiento|f_nacimiento|fec_nacimiento|nacimiento|",
      "fecha_nac|f_nac|fec_nac|fechanac)($|_)"
    ),
    # La hermana tenia el mismo hueco por el mismo lado. No se agrega
    # `fecha_fall`: `fall` es tambien "falla" en datos de mantenimiento, y una
    # clasificacion de mas enmascara una columna que no es personal.
    fecha_fallecimiento = paste0(
      "(^|_)(fallecimiento|defuncion|deceso|obito|fecha_muerte|",
      "f_fallecimiento|fecha_defuncion|f_defuncion|fec_defuncion|",
      "fecha_deceso)($|_)"
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
  textos <- .textos_para_clasificacion_personal(x)
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
  # Un correo no necesita ser mayoria para ser un correo. La rama de arriba pide
  # que lo sean el 80 % de los valores -una columna de correos-, y una columna de
  # nombre neutro con 70 correos y 30 textos se perfilaba sin proteger: los
  # ejemplos de sus patrones publicaban las direcciones enteras. El paquete ya
  # dice por que un correo se protege por su forma y un numero de ocho digitos no:
  # "su forma no deja dudas". Eso vale para cada valor, no para la mayoria. Es un
  # piso que falla cerrado: se aplica solo si nada de lo anterior protegio.
  if (!proteger && is.finite(proporcion_correo) && proporcion_correo > 0) {
    tipo <- "correo"
    proporcion <- proporcion_correo
    fundamento <- "valores con forma de correo, aunque no dominen la columna"
    poder <- "medio"
    proteger <- TRUE
  }
  list(
    tipo = tipo, proporcion = proporcion,
    # Sobre cuantos valores se calculo esa proporcion. Un blanco no tiene forma
    # que comparar contra un patron y por eso no entra al denominador, pero eso
    # se dice en vez de suponerse: ocho documentos y treinta y dos blancos
    # publicaban "100 % compatible" sin decir que era sobre ocho de cuarenta.
    valores_evaluados = as.integer(
      attr(proporcion, "evaluados", exact = TRUE) %||% NA_integer_
    ),
    valores_totales = as.integer(
      attr(proporcion, "totales", exact = TRUE) %||% NA_integer_
    ),
    fundamento = fundamento,
    poder_discriminante = poder, proteger = proteger,
    # La proporcion que decide `verificado` viajaba solo en la decision: una
    # columna con el 90 % de los documentos validos -el umbral- publicaba
    # exactamente lo mismo que una con el 100 %. El paquete ya se escribio la
    # regla en otra guarda: "el motivo tiene que nombrar el numero que decidio,
    # y decir de donde sale".
    proporcion_verificada = if (is.finite(proporcion_verificada)) {
      as.numeric(proporcion_verificada)
    } else {
      NA_real_
    }
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
  if (anyDuplicated(.nombres_para_operar(names(declaradas)))) {
    stop("`columnas_personales` repite una columna.", call. = FALSE)
  }
  # `nombres = NULL` significa "no se puede saber que columnas hay". Lo usa
  # medir(), donde la entrada puede ser una coleccion o una conexion y no hay
  # una lista de columnas que mirar. Con NULL se valida la forma y no la
  # existencia: es preferible a rechazar una declaracion valida.
  indices <- if (is.null(nombres)) integer() else {
    .indice_nombre(names(declaradas), nombres)
  }
  desconocidas <- if (is.null(nombres)) character() else {
    names(declaradas)[is.na(indices)]
  }
  if (length(desconocidas)) {
    stop("`columnas_personales` nombra columnas inexistentes: ",
         paste(desconocidas, collapse = ", "), ".", call. = FALSE)
  }
  vacios <- names(declaradas)[!nzchar(declaradas)]
  if (length(vacios)) {
    stop("`columnas_personales` declara un tipo vac\u00edo en: ",
         paste(vacios, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.null(nombres)) {
    names(declaradas) <- nombres[indices]
  }
  declaradas
}

.detectar_datos_personales <- function(datos, nombres, resultados,
                                       validadores = list(),
                                       umbral_verificado = 0.9,
                                       muestra_validadores = 1000L,
                                       declaradas = character()) {
  filas <- lapply(seq_along(datos), function(i) {
    indice_declarada <- .indice_nombre(nombres[[i]], names(declaradas))
    if (!is.na(indice_declarada)) {
      # Lo declarado no se vuelve a inferir. El paquete no tiene con que
      # contradecir a quien conoce el dato, y una columna declarada personal
      # que el lexico no reconoce es justamente el caso que esto resuelve.
      return(data.frame(
        columna = nombres[[i]],
        tipo = unname(declaradas[[indice_declarada]]),
        proporcion_compatible = NA_real_,
        valores_evaluados = NA_integer_,
        valores_totales = NA_integer_,
        fundamento = "declarado con `columnas_personales`",
        poder_discriminante = "declarado",
        proporcion_verificada = NA_real_,
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
      proporcion_compatible = as.numeric(clasificacion$proporcion),
      proporcion_verificada = as.numeric(
        clasificacion$proporcion_verificada %||% NA_real_
      ),
      valores_evaluados = clasificacion$valores_evaluados %||% NA_integer_,
      valores_totales = clasificacion$valores_totales %||% NA_integer_,
      fundamento = clasificacion$fundamento,
      poder_discriminante = clasificacion$poder_discriminante,
      proteger = clasificacion$proteger,
      stringsAsFactors = FALSE
    )
  })
  filas <- filas[!vapply(filas, is.null, logical(1L))]
  resultado <- if (length(filas)) do.call(rbind, filas) else data.frame(
    columna = character(), tipo = character(),
    proporcion_compatible = numeric(), proporcion_verificada = numeric(),
    valores_evaluados = integer(), valores_totales = integer(),
    fundamento = character(),
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
                 .formatear_decimal_publicado(
                   fila$proporcion_compatible[[1L]]
                 ))
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
    indice <- .indice_nombre(nombre, columnas$columna)
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
# `.texto_valor()` sobre cada elemento, en una pasada por clase. Se llamaba una
# vez POR CELDA de cada columna protegida: sobre 200.000 filas, 13 de los 44
# segundos de la proteccion. Las clases que no se reconocen siguen por el camino
# de uno en uno.
.texto_valor_vector <- function(x) {
  if (!length(x)) return(character())
  if (inherits(x, "POSIXt")) {
    salida <- format(x, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
    salida[is.na(x)] <- NA_character_
    return(salida)
  }
  if (inherits(x, "Date")) {
    salida <- format(x, "%Y-%m-%d")
    salida[is.na(x)] <- NA_character_
    return(salida)
  }
  if ((is.double(x) && !inherits(x, "integer64") && is.null(attr(x, "class"))) ||
      (is.integer(x) && is.null(attr(x, "class")))) {
    return(.formatear_numeros_uno_a_uno(x))
  }
  if (is.character(x) || is.factor(x) || is.logical(x)) {
    salida <- as.character(x)
    salida[is.na(x)] <- NA_character_
    return(salida)
  }
  vapply(seq_along(x), function(i) .texto_valor(x[i]), character(1L))
}

.columna_de_fechas_compactas <- function(x) {
  if (!(is.numeric(x) || is.character(x) || is.factor(x)) ||
      inherits(x, "integer64")) {
    return(FALSE)
  }
  # En bytes: `trimws()` aborta sobre texto que no es UTF-8 valido, y este paquete
  # trabaja con esos. Ante cualquier duda, no es una columna de fechas: la
  # columna sigue aportando sus valores al piso.
  tryCatch({
    texto <- gsub("^[[:space:]]+|[[:space:]]+$", "", as.character(x),
                  useBytes = TRUE)
    texto <- texto[!is.na(texto) & nzchar(texto)]
    length(texto) > 0L && all(grepl(
      "^(1[89]|20)[0-9]{2}(0[1-9]|1[0-2])(0[1-9]|[12][0-9]|3[01])$", texto,
      perl = TRUE, useBytes = TRUE
    ))
  }, error = function(e) FALSE)
}

.valores_publicables_protegidos <- function(datos, sensibles) {
  if (!inherits(datos, "data.frame") || !length(sensibles)) {
    return(character())
  }
  indices_sensibles <- .indice_nombre(sensibles, names(datos))
  valores <- unlist(lapply(unique(indices_sensibles[!is.na(indices_sensibles)]), function(indice) {
    x <- datos[[indice]]
    if (is.data.frame(x) || .es_columna_compuesta(x) || is.list(x)) {
      return(character())
    }
    # Una columna de fechas escritas como AAAAMMDD -entero o texto- es una
    # columna de fechas, y una fecha sola no entra en el piso (ver
    # `.valores_identificantes()`). Se decide por la COLUMNA y no por el valor:
    # una cedula de ocho digitos puede parecer una fecha, una columna entera de
    # cedulas no. Medido en una refutacion: tapaba los estadisticos de fecha de
    # las demas columnas.
    if (.columna_de_fechas_compactas(x)) return(character())
    crudos <- tryCatch(as.character(x), error = function(e) character())
    formateados <- tryCatch(c(
      format(x, digits = 15L, trim = TRUE, scientific = FALSE),
      format(x, digits = 8L, trim = TRUE, scientific = FALSE),
      .texto_valor_vector(x)
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
                                       meta = NULL, datos = NULL) {
  sensibles <- .columnas_personales_protegidas(clasificacion)
  # Con los datos a mano, una columna de fechas compactas -AAAAMMDD- no aporta
  # agujas tampoco por sus estadisticos: su maximo `20000111` tapaba el maximo
  # `2000-01-11` de otra columna de fechas. Ver `.valores_publicables_protegidos()`.
  if (inherits(datos, "data.frame") && length(sensibles)) {
    indices_datos <- .indice_nombre(sensibles, names(datos))
    compactas <- vapply(seq_along(sensibles), function(i) {
      !is.na(indices_datos[[i]]) &&
        .columna_de_fechas_compactas(datos[[indices_datos[[i]]]])
    }, logical(1L))
    sensibles <- sensibles[!compactas]
  }
  if (!inherits(columnas, "data.frame") || !length(sensibles)) {
    return(character())
  }
  indices <- which(.nombres_para_operar(columnas$columna) %in%
                    .nombres_para_operar(sensibles))
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
    indices_patrones <- intersect(indices, seq_along(patrones))
    for (i in indices_patrones) {
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
  # El catalogo de centinelas DEL PAQUETE -`.numeros_na_locales`- es vocabulario y
  # no entra: como aguja se tapaba a si mismo en `meta`, y el plan quedaba con la
  # accion que convierte `-999` en `NA` sin efecto en una columna de montos,
  # recomendada y activa. Medido en una refutacion.
  if (is.list(meta) && length(meta$sentinelas_numericos)) {
    propios <- meta$sentinelas_numericos[
      !meta$sentinelas_numericos %in% .numeros_na_locales
    ]
    valores <- c(valores, as.character(propios))
  }
  # Las celdas de `ejemplos` no traen UN valor: traen hasta tres UNIDOS con
  # `.SEPARADOR_EJEMPLOS`. Una aguja que es la cadena unida no existe en ningun
  # otro lado de la salida, asi que el reemplazo no encontraba nada y el piso se
  # quedaba sin agujas. Medido: con documentos de TEXTO -la moda y los extremos
  # en `NA`, de modo que la celda unida es el UNICO portador del valor- el
  # informe publicaba tres de cuatro cedulas CON su proteccion puesta. Con
  # documentos numericos el mismo camino funciona, porque las agujas llegan por
  # `minimo` y `maximo`; por eso las dos pruebas de la suite que reportan un
  # perfil abierto pasaban sin ver nada.
  #
  # Se agregan las partes ADEMAS de la cadena entera, que tambien aparece tal
  # cual en la celda que la origino. `useBytes = TRUE` porque este paquete
  # trabaja con codificaciones rotas y sin eso `strsplit()` avisa "input string
  # is invalid in this locale" por cada valor sin marca. El piso de seis
  # caracteres lo aplica `.valores_identificantes()` mas adelante, asi que una
  # parte corta no enmascara media tabla.
  partes <- unlist(
    strsplit(valores, .SEPARADOR_EJEMPLOS, fixed = TRUE, useBytes = TRUE),
    use.names = FALSE
  )
  valores <- c(valores, partes)
  valores <- unique(valores[!is.na(valores) & nzchar(valores)])
  valores[order(nchar(valores, type = "bytes"), decreasing = TRUE,
                method = "radix")]
}

# Si la aguja -una forma ya plegada por `.plegar_para_comparar()`, sin nada que no
# sea letra o digito- aparece en el texto plegado sin una LETRA pegada antes ni
# despues, con cualquier separador entre sus caracteres. El limite es de letras y
# no de alfanumericos a proposito: lo que se quiere dejar de tapar es un tramo de
# letras que cruza palabras de la prosa, y un digito pegado no es eso. Con limite
# alfanumerico se escapaba "maria.nunez123@correo.uy" frente al nombre protegido
# "Maria Nunez", que antes se tapaba. La aguja se parte por CARACTERES y las
# clases son de Unicode: partida por bytes, una letra griega o cirilica quedaba
# cortada al medio y el separador opcional se metia entre sus bytes. Ante un texto
# que no se puede examinar, TRUE: es una funcion de privacidad y el valor por
# omision tiene que ser cerrado.
.variante_en_limites <- function(aguja, textos) {
  caracteres <- intToUtf8(utf8ToInt(aguja), multiple = TRUE)
  patron <- paste0(
    "(?<!\\p{L})",
    paste(caracteres, collapse = "[^\\p{L}\\p{N}]*"),
    "(?!\\p{L})"
  )
  tryCatch(
    grepl(patron, textos, perl = TRUE),
    error = function(e) rep(TRUE, length(textos))
  )
}

.reemplazar_variantes_separadas <- function(x, valores,
                                           exigir_limites = FALSE) {
  # El reemplazo de arriba busca la cadena EXACTA, asi que el mismo documento
  # escrito con separadores se le escapa: `"771.771-01"` no contiene
  # `"77177101"`. Lo encontro una refutacion externa.
  #
  # Aca se compara la forma SIN separadores. Si al sacarle todo lo que no es
  # alfanumerico una cadena contiene un valor protegido igualmente normalizado,
  # la celda entera se enmascara: la separacion es cosmetica y el valor es el
  # mismo. Se enmascara entera y no por tramos porque el separador puede estar
  # en cualquier lado y reconstruir el resto no aportaria nada.
  #
  # Solo entran los valores identificantes que ya trae el piso -seis caracteres
  # o mas-, y ademas se exige que su forma normalizada conserve ese largo: sin
  # eso, un valor corto tras normalizar enmascararia media tabla.
  if (!is.character(x) || !length(valores)) return(x)
  apartado <- .apartar_marcas_paquete(x)
  x <- apartado$x
  # La caja, la tilde y la codificacion tambien son cosmeticas. El mismo nombre
  # en minusculas dentro de un texto libre se publicaba mientras la forma
  # canonica quedaba enmascarada en todo el informe; "juan.perez" se publicaba
  # frente al protegido "Juan Perez" con tilde; y una refutacion lo midio despues
  # fuera del latin occidental -vietnamita sin marcas, griego y cirilico en
  # mayusculas, ancho completo- y con el protegido en latin1 sin marca frente a la
  # celda en UTF-8. Agujas y celdas pasan por `.plegar_para_comparar()`, que deja
  # todo en UTF-8 valido y no depende del locale, y despues se les saca lo que no
  # es letra ni digito con clases de Unicode. Antes eso se hacia por bytes y una
  # letra no latina perdia la mayoria de los suyos: la comparacion era por azar.
  plegar <- function(v, desescapar = FALSE, invalidos = "por_byte") {
    tryCatch(
      .plegar_para_comparar(v, desescapar = desescapar, invalidos = invalidos),
      error = function(e) .textos_para_plegar(as.character(v))
    )
  }
  sin_separadores <- function(v) {
    tryCatch(
      gsub("[^\\p{L}\\p{N}]", "", v, perl = TRUE),
      error = function(e) gsub("[^[:alnum:]]", "", v, useBytes = TRUE)
    )
  }
  normalizar <- function(v, ...) sin_separadores(plegar(v, ...))
  # Un texto sin marca que no es UTF-8 valido se lee de las dos maneras -byte por
  # byte y entero como CP1252-: en latin1, una E acentuada seguida de un espacio
  # duro es por azar una letra UTF-8 valida, y la lectura por byte daba otra
  # letra. Medido en una refutacion.
  ambiguos <- function(v) {
    !is.na(v) & Encoding(v) %in% c("unknown", "bytes", "UTF-8") & !validUTF8(v)
  }
  agujas <- unique(c(
    normalizar(valores),
    normalizar(valores[ambiguos(valores)], invalidos = "entero")
  ))
  # El piso se cuenta en CARACTERES, como en `.valores_identificantes()`: en bytes,
  # un nombre cirilico de cuatro letras pasaba el piso de seis.
  largos <- nchar(agujas, type = "chars", allowNA = TRUE)
  agujas <- agujas[
    !is.na(agujas) & (is.na(largos) | largos >= .MIN_LARGO_VALOR_IDENTIFICANTE)
  ]
  if (!length(agujas)) return(.reponer_marcas_paquete(x, apartado))
  candidatas <- !is.na(x) & x != "[valor protegido]"
  if (!any(candidatas)) return(.reponer_marcas_paquete(x, apartado))
  palabras_paquete <- .LEXICO_PAQUETE$palabras
  # El texto publicado se compara en hasta tres formas: como esta, con los
  # escapes del paquete deshechos -un espacio duro sale `<U+00A0>`- y, si no es
  # UTF-8 valido, leido entero como CP1252. Basta que una contenga el valor. El
  # valor protegido no se desescapa: un valor con una barra literal y su cita, que
  # dobla la barra, solo coinciden asi. Medido en una refutacion.
  elegidos <- x[candidatas]
  crudos <- list(plegar(elegidos), plegar(elegidos, desescapar = TRUE))
  rotos <- ambiguos(elegidos)
  if (any(rotos)) {
    entero <- crudos[[1L]]
    entero[rotos] <- plegar(elegidos[rotos], invalidos = "entero")
    crudos[[3L]] <- entero
  }
  pajares <- lapply(crudos, sin_separadores)
  pajar <- pajares[[1L]]
  # La regla de digitos de mas abajo compara en la direccion contraria -la corrida
  # de la celda DENTRO de la aguja, que es el documento sin su verificador-, y
  # ahi el comienzo de la aguja no tiene por que aparecer: usa todas.
  agujas_todas <- agujas
  agujas <- .valores_que_pueden_aparecer(
    agujas, unlist(pajares, use.names = FALSE), .MIN_LARGO_VALOR_IDENTIFICANTE
  )
  # Las agujas de PUROS DIGITOS no se buscan aca: sin separadores, dos numeros
  # distintos se pegan -"N/A (49710); - (49576)" contenia una cedula de ocho
  # digitos que no estaba en la celda-. Las busca la regla de digitos de abajo,
  # que respeta donde empieza y termina cada numero.
  agujas <- agujas[!grepl("^[0-9]+$", agujas, useBytes = TRUE)]
  limites <- rep_len(as.logical(exigir_limites), length(x))[candidatas]
  limites[is.na(limites)] <- FALSE
  # Lo que la prosa cita entre comillas dobles no es prosa: es un valor de celda
  # que el paquete incrusta, como los niveles de un determinante en la sugerencia
  # de `posible_ausencia_estructural` -`~ proveedor == "juanperezsrl"`-. Ahi vale
  # la regla fuerte. Medido en una refutacion: con el titular "Juan Perez"
  # protegido, ese nivel salia entero porque la aguja esta pegada a "srl". Se tapa
  # solo lo citado, no el texto entero, para que la sugerencia se siga leyendo.
  # Las citas se buscan en el texto ORIGINAL y no en el transliterado: la
  # transliteracion puede convertir comillas tipograficas en comillas rectas, y
  # entonces las citas de uno y otro no se corresponden.
  patron_cita <- "\"(?:[^\"\\\\]|\\\\.)*\""
  citas <- vector("list", length(pajar))
  citado <- rep("", length(pajar))
  if (any(limites)) {
    citas[limites] <- regmatches(
      x[candidatas][limites],
      gregexpr(patron_cita, x[candidatas][limites], perl = TRUE, useBytes = TRUE)
    )
    citado[limites] <- vapply(citas[limites], function(partes) {
      if (!length(partes)) return("")
      paste(c(normalizar(partes), normalizar(partes, desescapar = TRUE)),
            collapse = " ")
    }, character(1L))
  }
  agujas_citadas <- vector("list", length(pajar))
  golpea <- rep(FALSE, length(pajar))
  # La forma sin separadores CONTIENE a la aguja. En la PROSA del paquete -los
  # campos de `.CAMPOS_DE_PROSA`, y solo esos- se exige ademas que la aguja caiga
  # sin letras pegadas en el texto: no puede empezar ni terminar a mitad de una
  # palabra. Sin eso, la prosa del paquete se tapaba entera
  # por azar: con los nombres de millones de personas como agujas, alguno de seis
  # letras aparece cruzando palabras -"...COLUMNACONTIENE..."-, y en una base real
  # quedaron tapadas 38 de 64 descripciones y 37 de 64 sugerencias, tambien de
  # columnas que no eran personales. Lo que la regla existe para atrapar sigue
  # cayendo en limites: "771.771-01" contra el documento 77177101, "Maria Nunez"
  # contra el nombre escrito junto. Fuera de la prosa no se exige: un valor del
  # usuario -un ejemplo, una moda, una evidencia- con el nombre pegado a otras
  # letras es justo lo que hay que tapar. Medido en una refutacion:
  # "juanperezsrl@correo.uy" frente al titular protegido se publicaba en los
  # ejemplos de los patrones cuando la regla de limites valia en todas partes.
  for (aguja in agujas) {
    pendientes <- which(!golpea)
    if (!length(pendientes)) break
    contiene <- pendientes[Reduce(`|`, lapply(pajares, function(p) {
      grepl(aguja, p[pendientes], fixed = TRUE, useBytes = TRUE)
    }))]
    if (length(contiene)) {
      con_limites <- contiene[limites[contiene]]
      golpea[setdiff(contiene, con_limites)] <- TRUE
      if (length(con_limites)) {
        # Una palabra del paquete en su propia prosa no es un valor: solo cuenta
        # si aparece citada, que se mira abajo.
        golpea[con_limites] <- if (aguja %in% palabras_paquete) {
          FALSE
        } else {
          Reduce(`|`, lapply(crudos, function(forma) {
            .variante_en_limites(aguja, forma[con_limites])
          }))
        }
        en_cita <- con_limites[!golpea[con_limites]]
        en_cita <- en_cita[
          grepl(aguja, citado[en_cita], fixed = TRUE, useBytes = TRUE)
        ]
        agujas_citadas[en_cita] <- lapply(agujas_citadas[en_cita], c, aguja)
      }
    }
  }
  # Y los DIGITOS aparte, que es donde la variante no es cosmetica sino PARCIAL:
  # la cedula sin su verificador -"4.123.456" frente a "4.123.456-7"- normaliza a
  # "4123456", que NO contiene a "41234567", asi que la regla de arriba no la ve
  # por ninguna de sus dos puntas. Medido: los tres fragmentos se publicaban con
  # la proteccion doble puesta. Se comparan las corridas de digitos de la celda
  # contra las de la aguja, en las dos direcciones, y solo de seis digitos para
  # arriba, que es el mismo piso que usa el resto de esta capa.
  #
  # Limitado a DIGITOS a proposito. La misma regla sobre texto -tapar una celda
  # que comparte seis caracteres con un valor protegido- taparia una palabra
  # corriente por compartir un tramo con un apellido, y eso silencia contenido
  # real del informe en vez de proteger un dato. Ver la mitad de control de la
  # prueba, que exige que una celda ajena siga publicandose.
  # Extraer las corridas con `gregexpr()` sobre todas las agujas costaba 6,6 s
  # con 600.000 valores protegidos -medido en `perfilar_por()`, que compara tres
  # etiquetas-. Casi todas son solo digitos -un documento, un telefono- o no
  # tienen ninguno -un nombre-: esas se resuelven sin expresiones, y solo las
  # mezcladas pasan por `gregexpr()`.
  corridas_largas <- function(v) {
    piezas <- vector("list", length(v))
    solo_digitos <- !is.na(v) & grepl("^[0-9]+$", v, useBytes = TRUE)
    piezas[solo_digitos] <- as.list(v[solo_digitos])
    mezcladas <- which(!solo_digitos & !is.na(v) & grepl("[0-9]", v, useBytes = TRUE))
    if (length(mezcladas)) {
      piezas[mezcladas] <- strsplit(
        gsub("[^0-9]+", " ", v[mezcladas], useBytes = TRUE), " ", fixed = TRUE
      )
    }
    lapply(piezas, function(p) {
      p[!is.na(p) & nchar(p, type = "bytes") >= .MIN_LARGO_VALOR_IDENTIFICANTE]
    })
  }
  # La regla de digitos, por NUMEROS: un numero es una corrida de grupos de
  # digitos unidos por un solo separador de los que se usan adentro de una cifra
  # -punto, guion, barra, coma, espacio, espacio duro o fino, apostrofo, guion
  # bajo-. De cada numero se prueban los tramos alineados a esos separadores, y
  # se tapa la celda si alguno es un documento protegido, el documento sin su
  # verificador o sin su primer digito. Asi se reconoce "6.111.222-9" -verificador
  # mal tipeado-, "3.777.888/2024", "4,123,456" o "*.555.666-2", y no se pegan dos
  # conteos vecinos: "(199985); - (100114)" son dos numeros. Antes se pegaban, y
  # cualquier tramo de seis cifras dentro de un documento tapaba la celda: con un
  # millon de cedulas casi todo conteo lo era. Medido en dos refutaciones. Un
  # documento pegado sin separador a otras cifras no se reconoce: ahi no se
  # distingue de un numero mas largo.
  if (!all(golpea)) {
    separador <- "[-./ ,'_\u00a0\u2009\u202f]"
    patron_numero <- paste0("[0-9]+(?:", separador, "[0-9]+)*")
    # Un tramo que une grupos tiene que tener la forma de un numero escrito con
    # separadores de miles: el primero de hasta tres cifras, los del medio de
    # tres, y el ultimo de una a tres -un verificador de una o dos cifras-. Sin
    # eso, un decimal como el estadistico de Benford `367277.602` daba
    # `367277602`, cuyo prefijo era una cedula de ocho digitos. Un numero de un
    # solo grupo se prueba entero y sin su ultima cifra -el verificador mal
    # tipeado sin separadores-.
    tramos <- function(numero) {
      grupos <- strsplit(numero, separador, perl = TRUE)[[1L]]
      k <- length(grupos)
      largos <- nchar(grupos)
      if (k == 1L) {
        return(c(grupos, substr(grupos, 1L, largos - 1L)))
      }
      salida <- grupos
      for (i in seq_len(k - 1L)) {
        if (largos[[i]] > 3L) next
        for (j in (i + 1L):k) {
          medio <- if (j > i + 1L) largos[(i + 1L):(j - 1L)] else integer()
          if (any(medio != 3L) || !largos[[j]] %in% 1:3) break
          salida <- c(salida, paste0(grupos[i:j], collapse = ""))
        }
      }
      salida
    }
    # Una fecha de calendario no es un documento: partida en tramos,
    # `2003-05-17` da `20030517`, que con un millon de cedulas es la de alguien.
    # Medido: tapaba ejemplos y estadisticos de columnas de fechas. Se borran de
    # la celda antes de buscar los numeros.
    patron_fecha <- paste0(
      "(?<![0-9])(?:[0-9]{4}[-/.](?:0?[1-9]|1[0-2])[-/.](?:0?[1-9]|[12][0-9]|3[01])|",
      "(?:0?[1-9]|[12][0-9]|3[01])[-/.](?:0?[1-9]|1[0-2])[-/.][0-9]{4})(?![0-9])"
    )
    # En la PROSA del paquete, fuera de lo citado, los numeros son conteos que
    # escribe el paquete -"12.497.500 pares posibles"-, y por su forma no se
    # distinguen de un documento: con un millon de cedulas alguno coincidia y la
    # frase se tapaba entera. Es el mismo principio que las palabras del
    # paquete: en la prosa, solo lo citado es un valor. Ahi la regla mira solo
    # las citas.
    citas_prosa <- vapply(citas, function(partes) {
      if (!length(partes)) "" else paste(partes, collapse = " ")
    }, character(1L))
    formas_digitos <- list(
      plegar(citas_prosa), plegar(citas_prosa, desescapar = TRUE)
    )
    candidatos_celda <- vector("list", length(golpea))
    for (indice_forma in seq_along(crudos)) {
      forma <- crudos[[indice_forma]]
      if (any(limites)) {
        forma[limites] <- formas_digitos[[min(indice_forma, 2L)]][limites]
      }
      forma <- gsub(patron_fecha, " ", forma, perl = TRUE)
      pendientes <- which(!golpea & grepl("[0-9]", forma, useBytes = TRUE))
      if (!length(pendientes)) next
      numeros <- regmatches(
        forma[pendientes], gregexpr(patron_numero, forma[pendientes], perl = TRUE)
      )
      for (k in seq_along(pendientes)) {
        lista <- numeros[[k]]
        lista <- lista[nchar(gsub("[^0-9]", "", lista)) >=
                         .MIN_LARGO_VALOR_IDENTIFICANTE]
        if (!length(lista)) next
        candidatos <- unlist(lapply(lista, tramos), use.names = FALSE)
        # Un numero redondo -`1.000.000`, `100.000`- es un conteo: que coincida
        # con el documento de alguien sin su verificador no lo vuelve un dato.
        candidatos <- candidatos[!grepl("^[1-9]0+$", candidatos, perl = TRUE)]
        celda <- pendientes[[k]]
        candidatos_celda[[celda]] <- c(candidatos_celda[[celda]], candidatos)
      }
    }
    # Los documentos -corridas de las agujas, que pueden ser millones- se arman
    # SOLO si alguna celda tiene un numero que probar: sin esa pereza,
    # `perfilar_por()` los rearmaba en cada grupo y costaba 8 s de 65.
    con_candidatos <- which(lengths(candidatos_celda) > 0L)
    if (length(con_candidatos)) {
      digitos_aguja <- unique(unlist(corridas_largas(agujas_todas), use.names = FALSE))
      # El documento parcial -sin verificador o sin primer digito- tiene que
      # conservar SIETE cifras: con cedulas viejas de siete digitos el parcial tiene
      # seis, y con cuatrocientas mil protegidas casi todo numero de seis cifras
      # coincidia con alguno -medido: dieciseis celdas tapadas de mas, motivos de
      # cobertura y conteos incluidos-. El documento entero sigue contando desde el
      # piso.
      largo <- nchar(digitos_aguja)
      parcial <- largo - 1L >= .MIN_LARGO_VALOR_IDENTIFICANTE + 1L
      # El parcial sin primer digito que empieza con cero no se usa: es la parte
      # decimal de un numero -`p=0.0001184` daba `0001184`, la cedula
      # `5.000.118-4` sin su 5- y no la forma en que se escribe un documento.
      sin_primero <- substr(digitos_aguja, 2L, largo)[parcial]
      documentos <- unique(c(
        digitos_aguja,
        substr(digitos_aguja, 1L, largo - 1L)[parcial],
        sin_primero[!startsWith(sin_primero, "0")]
      ))
      for (celda in con_candidatos) {
        if (any(candidatos_celda[[celda]] %in% documentos)) golpea[[celda]] <- TRUE
      }
    }
  }
  con_cita <- which(lengths(agujas_citadas) > 0L & !golpea)
  if (length(con_cita)) {
    indices_x <- which(candidatas)[con_cita]
    for (j in seq_along(con_cita)) {
      texto <- x[[indices_x[[j]]]]
      marca <- Encoding(texto)
      coincidencias <- gregexpr(patron_cita, texto, perl = TRUE, useBytes = TRUE)
      partes <- regmatches(texto, coincidencias)[[1L]]
      # Las dos formas, como al detectar: la cita de un nivel que no es UTF-8 sale
      # como `<lupa-byte:...>`, y solo desescapada contiene el valor.
      normalizadas <- paste(normalizar(partes), normalizar(partes, desescapar = TRUE))
      tapar <- vapply(normalizadas, function(parte) {
        any(vapply(agujas_citadas[[con_cita[[j]]]], function(aguja) {
          grepl(aguja, parte, fixed = TRUE, useBytes = TRUE)
        }, logical(1L)))
      }, logical(1L), USE.NAMES = FALSE)
      partes[tapar] <- "\"[valor protegido]\""
      regmatches(texto, coincidencias) <- list(partes)
      Encoding(texto) <- marca
      x[[indices_x[[j]]]] <- texto
    }
  }
  if (any(golpea)) x[candidatas][golpea] <- "[valor protegido]"
  .reponer_marcas_paquete(x, apartado)
}

# Prefiltro para las dos reglas de reemplazo. Su costo era el producto de la
# cantidad de valores protegidos por la cantidad de textos de la salida -cada
# valor recorria todos los textos-, y en una base real los dos crecen con las
# filas: sobre 200.000 filas, la proteccion era la mitad de `perfilar()`. Si un
# valor aparece dentro de un texto, sus primeros `k` bytes aparecen como un tramo
# de `k` bytes de ese texto: se arma una vez el conjunto de esos tramos y solo se
# buscan los valores cuyo comienzo esta en el. Nunca descarta una coincidencia
# verdadera. Se trabaja en BYTES, igual que los `gsub(useBytes = TRUE)` que
# reemplazan, para no depender de la marca de codificacion; el byte nulo separa
# los textos y no puede formar parte de un valor.
.tramos_bytes <- function(textos, k) {
  textos <- textos[!is.na(textos)]
  if (!length(textos)) return(numeric())
  bytes <- as.integer(unlist(
    lapply(textos, function(texto) c(charToRaw(texto), as.raw(0L))),
    use.names = FALSE
  ))
  n <- length(bytes) - k + 1L
  if (n < 1L) return(numeric())
  clave <- numeric(n)
  for (j in seq_len(k)) clave <- clave * 256 + bytes[j:(j + n - 1L)]
  unique(clave)
}

# Recibe valores de `k` bytes o mas. Los primeros `k` bytes de todos se toman con
# una sola expresion en modo bytes -`(?s)` para que el punto cruce un salto de
# linea- y se pegan: una llamada por valor costaba 2 s sobre 600.000 valores. Si
# algun valor no diera `k` bytes, el pegado se desalinearia; ahi se vuelve al
# camino de a uno.
.comienzo_bytes <- function(valores, k) {
  potencias <- 256^((k - 1L):0L)
  if (!length(valores)) return(numeric())
  prefijos <- sub(
    paste0("(?s)^(.{", k, "}).*$"), "\\1", valores,
    perl = TRUE, useBytes = TRUE
  )
  Encoding(prefijos) <- "bytes"
  bytes <- as.integer(charToRaw(paste(prefijos, collapse = "")))
  if (length(bytes) != k * length(valores)) {
    return(vapply(valores, function(valor) {
      sum(as.integer(charToRaw(valor)[seq_len(k)]) * potencias)
    }, numeric(1L), USE.NAMES = FALSE))
  }
  colSums(matrix(bytes, nrow = k) * potencias)
}

.valores_que_pueden_aparecer <- function(valores, textos, k) {
  largos <- nchar(valores, type = "bytes", allowNA = TRUE)
  largos_ok <- !is.na(largos) & largos >= k
  if (!any(largos_ok)) return(valores)
  presentes <- .tramos_bytes(textos, k)
  posibles <- !largos_ok
  posibles[largos_ok] <- .comienzo_bytes(valores[largos_ok], k) %in% presentes
  valores[posibles]
}

# Las marcas del paquete -`<blanco>`, `grupo_maximo`, ver `.LEXICO_PAQUETE`- se
# apartan antes de comparar y se reponen despues: son estructura, y un apellido
# `Blanco` protegido tapaba toda evidencia que dijera `<blanco>`. Se reemplazan
# por un caracter de control que la normalizacion borra; un texto que ya lo
# trae no se toca.
.apartar_marcas_paquete <- function(x) {
  sin_cambios <- list(x = x, indices = integer(), guardadas = list(),
                      codificaciones = character())
  marcas <- .LEXICO_PAQUETE$marcas
  if (!is.character(x) || !length(x) || !length(marcas)) return(sin_cambios)
  patron <- "<[a-z_]+>|[a-z0-9]+(_[a-z0-9]+)+"
  indices <- which(
    !is.na(x) & grepl("<[a-z_]+>|[a-z0-9]_[a-z0-9]", x, perl = TRUE,
                      useBytes = TRUE) &
      !grepl("\001", x, fixed = TRUE, useBytes = TRUE)
  )
  if (!length(indices)) return(sin_cambios)
  elegidos <- x[indices]
  codificaciones <- Encoding(elegidos)
  coincidencias <- gregexpr(patron, elegidos, perl = TRUE, useBytes = TRUE)
  tokens <- regmatches(elegidos, coincidencias)
  guardadas <- lapply(tokens, function(t) t[t %in% marcas])
  con_marca <- lengths(guardadas) > 0L
  if (!any(con_marca)) return(sin_cambios)
  regmatches(elegidos, coincidencias) <- lapply(tokens, function(t) {
    ifelse(t %in% marcas, "\001", t)
  })
  x[indices[con_marca]] <- elegidos[con_marca]
  list(x = x, indices = indices[con_marca], guardadas = guardadas[con_marca],
       codificaciones = codificaciones[con_marca])
}

.reponer_marcas_paquete <- function(x, apartado) {
  if (!length(apartado$indices)) return(x)
  for (k in seq_along(apartado$indices)) {
    i <- apartado$indices[[k]]
    texto <- x[[i]]
    if (is.na(texto) || !grepl("\001", texto, fixed = TRUE, useBytes = TRUE)) next
    partes <- strsplit(texto, "\001", fixed = TRUE, useBytes = TRUE)[[1L]]
    if (endsWith(texto, "\001")) partes <- c(partes, "")
    marcas <- apartado$guardadas[[k]]
    huecos <- length(partes) - 1L
    marcas <- c(marcas, rep("", max(0L, huecos - length(marcas))))[seq_len(huecos)]
    texto <- paste0(c(rbind(partes[-length(partes)], marcas), partes[length(partes)]),
                    collapse = "")
    Encoding(texto) <- apartado$codificaciones[[k]]
    x[[i]] <- texto
  }
  x
}

# Los valores protegidos de una sola palabra que el paquete tambien escribe en
# su prosa -`Maximo`, `Patron`, `Constante`-, segun su forma plegada.
.valores_que_son_palabras_del_paquete <- function(valores) {
  palabras <- .LEXICO_PAQUETE$palabras
  if (!length(valores) || !length(palabras)) return(character())
  candidatos <- valores[
    !is.na(valores) &
      !grepl("[[:space:][:digit:][:punct:]]", valores, useBytes = TRUE) &
      nchar(valores, type = "bytes") <= 80L
  ]
  if (!length(candidatos)) return(character())
  plegados <- tryCatch(
    gsub("[^\\p{L}\\p{N}]", "", .plegar_para_comparar(candidatos), perl = TRUE),
    error = function(e) rep(NA_character_, length(candidatos))
  )
  candidatos[!is.na(plegados) & plegados %in% palabras]
}

.reemplazar_valores_protegidos <- function(x, valores, exigir_limites = FALSE) {
  if (!is.character(x) || !length(valores)) return(x)
  # DE MAS LARGO A MAS CORTO, y una sola vez cada uno. Las dos cosas arreglan una
  # fuga medida con el piso del propio paquete -seis caracteres identifican-:
  #
  # Sustituyendo valor por valor EN EL ORDEN DE LA LISTA, un valor protegido que
  # es prefijo de otro deja publicada la cola del largo. Medido con
  # `c("Maria Nunez", "Maria Nunez de Castro")` sobre
  # `"beneficiaria: Maria Nunez de Castro"`:
  #
  #   antes:  beneficiaria: [valor protegido] de Castro   <- 9 caracteres afuera
  #   ahora:  beneficiaria: [valor protegido]
  #
  # Y no era un caso de laboratorio: los valores salen de las celdas de las
  # columnas protegidas EN EL ORDEN DE LAS FILAS, asi que cual va primero depende
  # de como este ordenada la tabla.
  #
  # El `unique()` cierra otra: repetir un valor lo sustituye dos veces, y si ese
  # valor aparece dentro del marcador -`protegido`, por ejemplo- la segunda pasada
  # corrompe el marcador y anida `[valor [valor protegido]]`.
  #
  # El orden NO se toca en `.reemplazar_variantes_separadas()`: alli las agujas
  # solo se buscan para enmascarar el elemento entero, y cual case primero no
  # cambia el resultado.
  valores <- unique(valores)
  valores <- valores[order(-nchar(valores, type = "bytes"))]
  # La regla de variantes recibe TODOS los valores: el prefiltro de aca mira la
  # escritura exacta, y una variante no la comparte.
  apartado <- .apartar_marcas_paquete(x)
  x <- apartado$x
  # Las marcas tambien se apartan de las AGUJAS: un valor protegido que trae una
  # -`Av. Italia 2345<br>Apto 101`- se publicaba exacto, porque en la celda la
  # marca ya no estaba y en la aguja si. Medido en una refutacion.
  sin_marcas <- .apartar_marcas_paquete(valores)$x
  valores <- unique(c(valores, sin_marcas))
  valores <- valores[order(-nchar(valores, type = "bytes"))]
  todos <- valores
  exacta <- function(textos, valores) {
    valores <- .valores_que_pueden_aparecer(valores, textos, 3L)
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
        textos <- tryCatch(
          gsub(patron, "[valor protegido]", textos, perl = TRUE, useBytes = TRUE),
          error = function(e) gsub(
            valor, "[valor protegido]", textos, fixed = TRUE, useBytes = TRUE
          )
        )
      } else {
        textos <- gsub(valor, "[valor protegido]", textos, fixed = TRUE,
                       useBytes = TRUE)
      }
    }
    textos
  }
  # En la prosa del paquete, un valor de una sola palabra que el paquete tambien
  # escribe no se busca tal cual: "Patron dominante" es su texto. Lo citado
  # entre comillas lo sigue tapando la regla de variantes, con la regla fuerte.
  limites <- rep_len(as.logical(exigir_limites), length(x))
  limites[is.na(limites)] <- FALSE
  del_paquete <- if (any(limites)) {
    .valores_que_son_palabras_del_paquete(valores)
  } else character()
  if (length(del_paquete)) {
    x[limites] <- exacta(x[limites], setdiff(valores, del_paquete))
    x[!limites] <- exacta(x[!limites], valores)
  } else {
    x <- exacta(x, valores)
  }
  x <- .reemplazar_variantes_separadas(x, todos, exigir_limites)
  .reponer_marcas_paquete(x, apartado)
}

# Recorre la parte textual de una salida sin convertir estadisticos numericos
# que no sean valores de celda. Los parametros del plan tienen un recorrido
# adicional para quitar tambien valores numericos que todavia no se volvieron
# texto.
# EL RECORRIDO, separado de lo que se hace con cada hoja. Lo usan las dos pasadas
# de `.proteger_textos_salida()`, asi que las dos visitan las mismas hojas en el
# mismo orden por construccion -y los atributos se siguen protegiendo igual-.
# ¿Esta columna de una tabla de salida guarda NOMBRES DE COLUMNA de la entrada?
#
# Se decide por el CONTENIDO y no por el nombre del campo: lo es si todos sus
# valores no vacios son nombres de la entrada, solos o unidos por los separadores
# que el paquete usa para las referencias a varias columnas -`, `, `+`, ` | `-.
# Una lista de campos escrita a mano -`columna`, `determinante`, `dependiente`...-
# se habria quedado corta con el primero que se agregara, que es la forma en que
# este paquete ya perdio varias guardas.
#
# Y no puede ser "proteger los nombres de columna en todas partes": medido sobre
# el caso que lo destapo, el VALOR `"documento"` es igual al NOMBRE de columna
# `documento`, asi que proteger el nombre en todos lados dejaba de enmascarar
# `columnas$moda = "documento"` y filtraba el valor. La proteccion va por campo.
.es_columna_de_nombres <- function(x, nombres) {
  if (!length(nombres) || !length(x)) return(FALSE)
  valores <- as.character(x)
  valores <- valores[!is.na(valores) & nzchar(valores)]
  if (!length(valores)) return(FALSE)
  claves <- .nombres_para_operar(nombres)
  exactos <- .nombres_para_operar(valores) %in% claves
  if (all(exactos)) return(TRUE)
  # Los que no son un nombre exacto se prueban como referencia a varios: se
  # parten por los separadores del paquete y cada pieza tiene que ser un nombre.
  # Primero el valor entero, porque un nombre de columna puede llevar una coma.
  restantes <- valores[!exactos]
  compuestos <- vapply(restantes, function(v) {
    piezas <- strsplit(v, "\\s*(,|\\+|\\|)\\s*", perl = TRUE)[[1L]]
    piezas <- piezas[nzchar(piezas)]
    length(piezas) > 1L && all(.nombres_para_operar(piezas) %in% claves)
  }, logical(1L), USE.NAMES = FALSE)
  all(compuestos)
}

# El VOCABULARIO del paquete es estructura, igual que los nombres de columna: un
# tipo de hallazgo, una severidad, una estrategia, un estado o el nombre de un
# diagnostico o de una metrica no son datos de nadie, los escribio el paquete. El
# barrido los trataba como cualquier texto, y como enmascara por CONTENIDO -una
# celda que, sin separadores y en mayusculas, contiene un valor protegido de seis
# caracteres o mas se tapa entera-, bastaba un nombre de persona contenido en
# `faltantes_disfrazados` para que el tipo saliera `[valor protegido]`. Medido en
# la tercera evaluacion real, sobre tres columnas de nombres de persona: seis
# hallazgos sin tipo; y reproducido aca, el plan quedaba ademas SIN NINGUNA
# accion para ellos, porque ninguna estrategia casa con un tipo que no se lee.
#
# La decision sigue siendo por campo y por contenido, la misma de los nombres
# (`.es_columna_de_nombres()`): se recolecta lo que el objeto trae en sus campos
# de vocabulario y un campo cuyos valores son TODOS de ahi queda afuera del
# barrido. Un campo que mezcla vocabulario y valores, no. La lista de nombres de
# campo solo decide DE DONDE se recolecta: un campo de vocabulario que falte aca
# se sigue barriendo, que es el lado seguro para la privacidad.
.CAMPOS_DE_VOCABULARIO <- c(
  "tipo_hallazgo", "hallazgo", "estrategia", "severidad", "severidad_origen",
  "estado", "estado_reparacion", "estado_tipo_inferido", "unidad_conteo",
  "decision_grupo", "diagnostico", "metrica", "metrica_especifica",
  "dimension", "factor", "orientacion", "granularidad", "tipo_resultado",
  "agregacion", "componente", "tipo", "cambio", "aspecto", "direccion", "nivel",
  # La unidad de una columna -`segundos` en las fechas, la de un objeto `units`-
  # no es el dato de nadie, y `Segundo` protegido tapaba la de toda columna de
  # fechas. Medido en una refutacion.
  "unidad"
)

# La PROSA del paquete: los campos que explican un diagnostico con frases. En
# ellos la regla de variantes exige limites de palabra -ver
# `.reemplazar_variantes_separadas()`-; en cualquier otro campo, no. Un campo de
# prosa que falte aca se barre con la regla fuerte, que es el lado seguro.
.CAMPOS_DE_PROSA <- c(
  "descripcion", "sugerencia", "motivo", "como_resolverlo", "justificacion",
  "recomendacion_grupo"
)

.vocabulario_de_objeto <- function(x, profundidad = 0L) {
  if (profundidad > 6L || is.null(x)) return(character())
  vocabulario <- character()
  adicionales <- setdiff(
    names(attributes(x)), c("names", "class", "row.names", "dim", "dimnames")
  )
  for (atributo in adicionales) {
    vocabulario <- c(vocabulario, .vocabulario_de_objeto(
      attr(x, atributo, exact = TRUE), profundidad + 1L
    ))
  }
  if (inherits(x, "data.frame")) {
    for (campo in intersect(names(x), .CAMPOS_DE_VOCABULARIO)) {
      columna <- x[[campo]]
      if (is.factor(columna)) {
        vocabulario <- c(vocabulario, levels(columna))
      } else if (is.character(columna)) {
        vocabulario <- c(vocabulario, columna)
      }
    }
    for (columna in x) {
      if (is.list(columna) && !is.data.frame(columna)) {
        for (elemento in columna) {
          if (is.list(elemento)) {
            vocabulario <- c(vocabulario,
                             .vocabulario_de_objeto(elemento, profundidad + 1L))
          }
        }
      }
    }
  } else if (is.list(x)) {
    for (elemento in x) {
      vocabulario <- c(vocabulario, .vocabulario_de_objeto(elemento, profundidad + 1L))
    }
  }
  vocabulario <- vocabulario[!is.na(vocabulario) & nzchar(vocabulario)]
  unique(vocabulario)
}

# `aplicar` recibe la hoja y el nombre del campo de donde viene -la columna, el
# elemento de lista o el atributo, heredado hacia adentro-, para que el barrido
# pueda tratar distinto la prosa del paquete y los valores.
.recorrer_textos_salida <- function(x, aplicar, intocables = character(),
                                    campo = NULL) {
  atributos <- attributes(x)
  estructurales <- c("names", "class", "row.names", "dim", "dimnames")
  adicionales <- setdiff(names(atributos), estructurales)
  for (atributo in adicionales) {
    attr(x, atributo) <- .recorrer_textos_salida(
      attr(x, atributo, exact = TRUE), aplicar, intocables, campo = atributo
    )
  }
  if (inherits(x, "data.frame")) {
    for (j in seq_along(x)) {
      columna <- x[[j]]
      # Los campos que guardan nombres de columna son ESTRUCTURA: el paquete los
      # necesita para cruzar el perfil con los datos, y un nombre no filtra el
      # valor que casualmente contiene -existia antes y aparte del dato-.
      # Enmascararlos rompia las dos guardas que comparan nombres: medido en una
      # base real de millones de filas, `fecha_nacimiento` salia publicada como
      # `fecha_[valor protegido]` y `planificar_limpieza()` y `analizar()`
      # abortaban. Se saltean en las DOS pasadas, asi que las hojas siguen
      # alineadas: la decision depende solo del contenido original.
      if ((is.character(columna) || is.factor(columna)) &&
          .es_columna_de_nombres(columna, intocables)) {
        next
      }
      nombre_columna <- names(x)[[j]]
      if (is.character(columna)) {
        x[[j]] <- aplicar(columna, nombre_columna)
      } else if (is.factor(columna)) {
        levels(columna) <- aplicar(levels(columna), nombre_columna)
        x[[j]] <- columna
      } else if (is.list(columna)) {
        x[[j]] <- lapply(
          columna, .recorrer_textos_salida,
          aplicar = aplicar, intocables = intocables, campo = nombre_columna
        )
      }
    }
    return(x)
  }
  if (is.list(x)) {
    nombres <- names(x)
    for (i in seq_along(x)) {
      propio <- if (!is.null(nombres) && !is.na(nombres[[i]]) &&
                    nzchar(nombres[[i]])) nombres[[i]] else campo
      elemento <- .recorrer_textos_salida(
        x[[i]], aplicar = aplicar, intocables = intocables, campo = propio
      )
      if (is.null(elemento)) {
        x[i] <- list(NULL)
      } else {
        x[[i]] <- elemento
      }
    }
    return(x)
  }
  if (is.character(x)) return(aplicar(x, campo))
  x
}

# DOS PASADAS: cosechar las hojas de texto, reemplazar UNA vez sobre el vector
# entero, repartir.
#
# Por que. El reemplazo recorre cada valor protegido con un `gsub`, asi que cuesta
# lo mismo sobre una cadena que sobre diez mil; pagarlo por hoja es lo que hacia
# que publicar 4.754 hallazgos costara 42.794 llamadas -el 99% de las del objeto-
# con una mediana de UN texto por llamada. La culpable medida es
# `hallazgos$trazabilidad`, una columna-lista con una entrada por hallazgo y nueve
# hojas en cada entrada.
#
# Localizarlo costo tres intentos, y los tres errores fueron del instrumento: las
# tres primeras pilas apuntaban al `perfilar()` interno -que cuesta 1 s de 176-,
# contar en un entorno con asignacion compleja dio cero, y recien una muestra
# uniforme al 2 % dijo que el 99 % venia de aca.
#
# Lo que NO se hace: tocar el motor de coincidencia. Cambiar el bucle por una
# alternancia de expresion regular es mas rapido y, medido, DEJA DE ENMASCARAR el
# texto marcado `latin1` -armar el patron traduce a UTF-8-. Aca no se cambia ni
# `fixed = TRUE` ni la codificacion: solo se agrupa.
.proteger_textos_salida <- function(x, valores, intocables = character()) {
  if (!length(valores)) return(x)
  intocables <- c(intocables, .vocabulario_de_objeto(x))
  cofre <- new.env(parent = emptyenv())
  cofre$hojas <- list()
  cofre$prosa <- list()
  invisible(.recorrer_textos_salida(x, function(hoja, campo) {
    cofre$hojas[[length(cofre$hojas) + 1L]] <- hoja
    cofre$prosa[[length(cofre$prosa) + 1L]] <- rep(
      !is.null(campo) && campo %in% .CAMPOS_DE_PROSA, length(hoja)
    )
    hoja
  }, intocables))
  if (!length(cofre$hojas)) return(x)
  protegidas <- .reemplazar_valores_protegidos(
    unlist(cofre$hojas, use.names = FALSE), valores,
    exigir_limites = unlist(cofre$prosa, use.names = FALSE)
  )
  cofre$desde <- 0L
  .recorrer_textos_salida(x, function(hoja, campo) {
    tramo <- protegidas[cofre$desde + seq_along(hoja)]
    cofre$desde <- cofre$desde + length(hoja)
    # Los atributos de la hoja -nombres, sobre todo- los pierde el corte del
    # vector y hay que devolverlos.
    attributes(tramo) <- attributes(hoja)
    tramo
  }, intocables)
}

# Los campos numericos que son ESTRUCTURA -cuentan, ubican o configuran- y no un
# dato: un conteo, un indice de fila, un tamano, un tiempo, una proporcion, un
# umbral. El piso numerico tapaba cualquier numero cuya representacion fuera un
# valor protegido, y con eso rompia la estructura por coincidencia. Medido: los
# `indices_fila` de una traza que coincidian con un documento protegido quedaban
# en NA y lupa se acusaba a si misma -"total de traza no coincide con sus
# indices"-, en una base real de millones de filas; y en `perfilar_dbi()` una
# columna `id = 1..n` hacia que el conteo `n` saliera NA en todas las columnas.
# Es la misma propiedad que ya rompio los nombres de columna: la proteccion
# reemplaza por coincidencia de valor cosas que no son celdas.
#
# La lista es de ESTRUCTURA, no de valores, y eso decide para que lado falla: un
# campo nuevo que no este aca se sigue tapando. Salio de recorrer los 271 campos
# numericos que publican `perfilar()`, `perfilar_dbi()` y `analizar()`; lo que
# puede llevar un valor de la tabla -`minimo`, `maximo`, `media`, `mediana`,
# `valor`, `resultado`, `rango`, `desvio`, `centinela_valor`, las coordenadas- no
# esta, y tampoco lo que se deriva de valores, como `ordenes_magnitud`.
.PATRON_NUMERICO_DE_ESTRUCTURA <- paste0(
  "^(n|n_.+|indices_.+|fila|filas|filas_.+|total|.+_totales|valores_evaluados|",
  "pares_.+|celdas|celdas_.+|columnas|columnas_.+|lote|lotes_.+|tamano_.+|",
  ".+_bytes|bytes_.+|memoria_.+|.+_ms|duracion_estimada_.+|umbral|umbral_.+|",
  "max_.+|min_.+|minimo_filas|minimo_observaciones_utilizables|",
  "minima_proporcion_positivos|prop_.+|proporcion|proporcion_.+|fraccion|",
  "tasa_.+|version_.+|consulta_id|id_consulta|consultas_.+|emitidas|",
  "llamadas_.+|bloques_.+|muestra|muestra_.+|presupuesto|mostrados|frecuencia|",
  "frecuencia_.+|absoluta|relativa|longitud_.+|centinela_repeticiones)$"
)

# Solo una HOJA numerica se saltea, nunca un contenedor: el perfil tiene un
# componente que se llama `columnas`, y saltearlo por el nombre dejaba sin tapar
# la tabla entera -el `minimo` de una columna copia volvia a publicar documentos-.
# Lo atrapo la prueba que ya cuidaba esa filtracion.
.es_numero_de_estructura <- function(nombre, valor) {
  !is.null(nombre) && length(nombre) == 1L && !is.na(nombre) &&
    is.numeric(valor) && !is.list(valor) &&
    grepl(.PATRON_NUMERICO_DE_ESTRUCTURA, nombre)
}

.proteger_numeros_parametros <- function(x, valores) {
  if (!length(valores)) return(x)
  if (inherits(x, "data.frame")) {
    for (j in seq_along(x)) {
      if (.es_numero_de_estructura(names(x)[[j]], x[[j]])) next
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
    nombres <- names(x)
    for (i in seq_along(x)) {
      if (!is.null(nombres) && .es_numero_de_estructura(nombres[[i]], x[[i]])) {
        next
      }
      x[i] <- list(.proteger_numeros_parametros(x[[i]], valores))
    }
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
  # Con la proteccion apagada en el perfil, el plan tampoco se protege: el
  # usuario declaro que quiere los valores, y el plan tapaba igual los
  # parametros de sus acciones. Medido en una refutacion.
  if (is.list(perfil$meta) && isFALSE(perfil$meta$proteger_datos_personales)) {
    attr(plan, "columnas_datos_personales_protegidas") <- character()
    return(plan)
  }
  sensibles <- .columnas_personales_protegidas(perfil)
  indices_dependencias <- which(startsWith(
    as.character(plan$estrategia), "imputar_dependencia_funcional__"
  ))
  if (length(indices_dependencias) && is.list(plan$parametros)) {
    for (indice in indices_dependencias) {
      parametros <- plan$parametros[[indice]]
      if (!is.list(parametros)) next
      involucra_protegida <- any(.nombres_para_operar(c(
        parametros$determinante, parametros$dependiente
      )) %in% .nombres_para_operar(sensibles))
      if (isTRUE(involucra_protegida)) {
        parametros$mapa_enmascarado <- TRUE
        plan$parametros[[indice]] <- parametros
      }
    }
  }
  valores <- .valores_perfil_protegidos(
    perfil$columnas, perfil$patrones, perfil$datos_personales, perfil$meta,
    datos = datos
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
  nombres_entrada <- if (!is.null(perfil$columnas$columna)) {
    as.character(perfil$columnas$columna)
  } else character()
  # El barrido de todo el plan pasa por el PISO, como el del perfil: sin el, un
  # valor corto o un centinela del catalogo -`-999`- se tapaba en los parametros
  # de cualquier columna, y la accion que lo convierte quedaba sin efecto. Los
  # parametros de las acciones sobre una columna protegida se barren ademas con
  # todos sus valores: es la proteccion por columna.
  identificantes <- .valores_identificantes(valores)
  plan <- .proteger_textos_salida(
    plan, identificantes, intocables = nombres_entrada
  )
  if ("parametros" %in% names(plan) && is.list(plan$parametros)) {
    propias <- !is.na(plan$columna) &
      .nombres_para_operar(as.character(plan$columna)) %in%
        .nombres_para_operar(sensibles)
    plan$parametros <- I(lapply(seq_along(plan$parametros), function(i) {
      parametros <- plan$parametros[[i]]
      if (isTRUE(propias[[i]])) {
        parametros <- .proteger_textos_salida(parametros, valores)
        .proteger_numeros_parametros(parametros, valores)
      } else {
        .proteger_numeros_parametros(parametros, identificantes)
      }
    }))
  }
  cobertura <- attr(plan, "cobertura_diagnosticos", exact = TRUE)
  if (inherits(cobertura, "data.frame")) {
    attr(plan, "cobertura_diagnosticos") <- .proteger_textos_salida(
      cobertura, identificantes
    )
  }
  # Este atributo copia la sugerencia del hallazgo, que es texto del paquete
  # pero puede nombrar un valor de la columna. Enmascararlo aqui es lo que
  # impide que un atributo nuevo publique lo que las columnas del plan ya no
  # publican.
  sin_accion <- attr(plan, "hallazgos_sin_accion", exact = TRUE)
  if (inherits(sin_accion, "data.frame")) {
    attr(plan, "hallazgos_sin_accion") <- .proteger_textos_salida(
      sin_accion, identificantes
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
  # El determinante se reconoce comparando el comienzo de la evidencia con cada
  # nombre protegido, y no extrayendolo con una expresion: un nombre con un
  # acento grave dentro cortaba la extraccion y la sugerencia salia entera, con
  # los niveles o el corte de la columna protegida.
  textos_sensibles <- unique(c(
    .marcar_utf8_textos(as.character(sensibles)),
    as.character(sensibles)
  ))
  for (i in indices) {
    evidencia <- as.character(hallazgos$evidencia[[i]])
    if (is.na(evidencia)) next
    prefijos <- paste0("`", textos_sensibles, "` predice ")
    coinciden <- which(startsWith(evidencia, prefijos))
    if (!length(coinciden)) next
    candidatos <- textos_sensibles[coinciden]
    determinante <- candidatos[[which.max(nchar(candidatos, type = "bytes"))]]
    inicio <- sub("\\. La columna corresponde.*$", "", evidencia)
    if (identical(inicio, evidencia)) inicio <- NA_character_
    tipo_criterio <- if (grepl("por un umbral", evidencia, fixed = TRUE)) {
      "un umbral"
    } else {
      "niveles"
    }
    hallazgos$evidencia[[i]] <- if (is.na(inicio)) {
      "[evidencia protegida]"
    } else paste0(
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
  indices_columnas <- .nombres_para_operar(columnas$columna) %in%
    .nombres_para_operar(sensibles)
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
  # `desvio` NO esta en esta lista, y es una decision tomada -la prueba que la
  # fija se llama "F-6 conserva el desvio numerico y protege la media"-: una
  # dispersion no identifica a nadie y la media, que junto con ella
  # reconstruiria los valores, si se enmascara. Lo que estaba mal era la
  # ETIQUETA, que decia "momentos protegidos" mientras publicaba uno.
  #
  # Una lista de campos a ocultar falla ABIERTA: el campo que se agregue manana
  # se publica salvo que alguien se acuerde de anotarlo aca. Por eso viaja una
  # prueba que no lee esta lista: perfila una columna protegida de magnitudes
  # conocidas y exige que ningun campo publicado traiga una magnitud de la
  # columna, con `desvio` como unica excepcion declarada.
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
    "[estadisticos de orden y la media protegidos]"
  for (i in intersect(which(indices_columnas), seq_along(patrones))) {
    if (.nombres_para_operar(columnas$columna[[i]]) %in%
        .nombres_para_operar(sensibles) &&
        "ejemplos" %in% names(patrones[[i]])) {
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
  coincide <- .nombres_para_operar(columna_completa) %in%
      .nombres_para_operar(clasificadas) |
    vapply(strsplit(columna_completa, ",", fixed = TRUE),
           function(columnas) any(.nombres_para_operar(trimws(columnas)) %in%
                                  .nombres_para_operar(clasificadas)),
           logical(1L))
  indices_hallazgos <- !is.na(hallazgos$columna) &
    hallazgos$tipo_hallazgo != "dato_personal_posible" & coincide
  # La ausencia estructural es una señal válida aunque el determinante sea
  # personal. Se conserva la predicción y su precisión, pero no el corte ni los
  # niveles que permitirían reconstruir valores de la columna protegida.
  # Corre antes de tapar la evidencia, porque reconoce el determinante en ella:
  # si la dependiente tambien era personal, la evidencia ya estaba tapada y la
  # sugerencia salia con los niveles o el corte del determinante.
  hallazgos <- .proteger_ausencia_estructural(hallazgos, sensibles)
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
  if (nrow(dependencias)) {
    indices_dependencias <-
      .nombres_para_operar(dependencias$determinante) %in%
        .nombres_para_operar(sensibles) |
      .nombres_para_operar(dependencias$dependiente) %in%
        .nombres_para_operar(sensibles)
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
    indices_protegidas <- .indice_nombre(sensibles, names(claves))
    protegidas <- names(claves)[
      unique(indices_protegidas[!is.na(indices_protegidas)])
    ]
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
    sentinelas <- perfil$meta$sentinelas_numericos
    del_catalogo <- sentinelas %in% .numeros_na_locales
    if (length(valores)) {
      # Con el piso, como el resto de la salida: un centinela corto no identifica.
      perfil$meta$sentinelas_numericos <- .proteger_numeros_parametros(
        sentinelas, .valores_identificantes(valores)
      )
    } else {
      # Un perfil abierto puede llegar a `reportar()` sin conservar `datos`.
      # En ese caso no se puede saber que centinela pertenece a que columna;
      # conservar la lista completa publicaria uno si coincide con una columna
      # protegida, asi que se oculta de forma conservadora. El catalogo del
      # paquete no es de nadie y queda.
      sentinelas[!del_catalogo] <- NA_real_
      perfil$meta$sentinelas_numericos <- sentinelas
    }
  }
  resultados <- perfil$meta$benford$resultados
  if (length(resultados)) {
    nombres_resultados <- vapply(resultados, `[[`, character(1L), "columna")
    indices_resultados <- which(.nombres_para_operar(nombres_resultados) %in%
                                .nombres_para_operar(sensibles))
    for (indice in indices_resultados) {
      perfil$meta$benford$resultados[[indice]]$ordenes_magnitud <- NA_real_
    }
  }
  cobertura <- perfil$cobertura_diagnosticos
  if (inherits(cobertura, "data.frame") && nrow(cobertura) &&
        "columna" %in% names(cobertura) && "motivo" %in% names(cobertura)) {
    afectadas <- .nombres_para_operar(cobertura$columna) %in%
      .nombres_para_operar(sensibles)
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

.MIN_LARGO_VALOR_IDENTIFICANTE <- 6L

.valores_identificantes <- function(valores) {
  # El piso de la proteccion, en un solo lugar. Un valor identifica si tiene
  # seis caracteres o mas: es el mismo corte con el que la bateria de fugas
  # decide que cuenta como filtracion, y tener dos definiciones distintas en la
  # regla y en su prueba es por donde se cuela un defecto.
  #
  # `allowNA = TRUE`, y el NA cuenta como identificante: `nchar()` con
  # `type = "chars"` ABORTA sobre una cadena que no es UTF-8 valido, y este
  # paquete trabaja con codificaciones rotas. Ante un valor cuyo largo no se
  # puede medir, protegerlo es el lado seguro.
  if (!length(valores)) return(character())
  # El marcador nunca es una aguja. Si el perfil del que se cosechan los valores
  # ya paso por proteccion, `"[valor protegido]"` entra en la lista, y su forma
  # sin separadores -`valorprotegido`- coincide con la del estado
  # `valor_protegido`: el marcador terminaba enmascarandose a si mismo y un
  # campo que declara la proteccion pasaba a declarar un valor.
  valores <- valores[
    !is.na(valores) & nzchar(valores) & valores != "[valor protegido]"
  ]
  if (!length(valores)) return(character())
  largos <- nchar(valores, type = "chars", allowNA = TRUE)
  valores <- valores[is.na(largos) | largos >= .MIN_LARGO_VALOR_IDENTIFICANTE]
  # Una fecha de calendario sola no identifica, aunque tenga diez caracteres: en
  # una tabla de miles de personas casi todo dia es el cumpleanos de alguien. Con
  # la fecha de nacimiento protegida, el piso tapaba la media, la mediana, el
  # minimo y la moda de TODAS las demas columnas de fechas -doce estadisticos con
  # 50.000 personas, medido en una refutacion- y el umbral de fecha de una
  # sugerencia, que no son la fecha de nadie. La columna de fechas se sigue
  # protegiendo entera, por columna; lo que sale del piso es la busqueda de sus
  # valores en el resto de la salida. Una fecha con hora si queda: con segundos
  # vuelve a ser casi unica.
  dia <- "(0?[1-9]|[12][0-9]|3[01])"
  mes <- "(0?[1-9]|1[0-2])"
  # Tambien la fecha escrita como fecha-hora a medianoche -ISO con `T`, como la
  # escriben muchos sistemas una fecha sin hora-: es la misma fecha.
  medianoche <- "([T ]00:00(:00(\\.0+)?)?(Z| ?UTC)?)?"
  fecha <- paste0(
    "^([0-9]{4}[-/.]", mes, "[-/.]", dia, "|", dia, "[-/.]", mes, "[-/.][0-9]{4}|",
    mes, "[-/.]", dia, "[-/.][0-9]{4})", medianoche, "$"
  )
  valores <- valores[!grepl(fecha, valores, perl = TRUE, useBytes = TRUE)]
  # Y un marcador de ausencia del catalogo del paquete -`sin dato`, `N/A`- no es
  # el dato de nadie aunque aparezca en una columna de nombres: es vocabulario
  # compartido, como dice `?perfilar`. Como aguja tapaba el marcador en las
  # demas columnas, y en el plan dejaba sin efecto la accion que lo convierte en
  # `NA` en una columna vecina, recomendada y activa. Medido en una refutacion.
  # La columna protegida lo sigue tapando, por columna.
  catalogo <- .normalizacion_minusculas_vector(trimws(c(
    .cadenas_na_naniar_1_1_0, .cadenas_na_locales
  )))
  normalizados <- tryCatch(
    .normalizacion_minusculas_vector(trimws(valores)),
    error = function(e) rep(NA_character_, length(valores))
  )
  valores[is.na(normalizados) | !normalizados %in% catalogo]
}


.proteger_perfil <- function(perfil, datos = NULL) {
  valores_perfil <- .valores_perfil_protegidos(
    perfil$columnas, perfil$patrones, perfil$datos_personales, perfil$meta,
    datos = datos
  )
  protegidos <- .proteger_componentes_perfil(
    perfil$columnas, perfil$patrones, perfil$dependencias,
    perfil$hallazgos, perfil$datos_personales
  )
  perfil$columnas <- protegidos$columnas
  # `dato_personal_protegido` dice si el valor **quedo** protegido -asi esta
  # escrito donde se fija al perfilar-, y esta funcion es la que lo protege.
  # Cuando corre sobre un perfil hecho SIN proteccion -por ejemplo al escribir
  # con `guardar_analisis()`, que vuelve a aplicarla antes de guardar- los
  # valores salian enmascarados y la marca seguia en `FALSE`: el objeto decia
  # "no hay columnas protegidas" al lado de una moda `[valor protegido]`, y
  # quien filtra por la marca no encontraba nada que el archivo si ocultaba.
  if ("dato_personal_protegido" %in% names(perfil$columnas) &&
      inherits(perfil$datos_personales, "data.frame")) {
    protegidas <- .nombres_para_operar(perfil$columnas$columna) %in%
      .nombres_para_operar(.columnas_personales_protegidas(perfil))
    perfil$columnas$dato_personal_protegido <-
      as.logical(perfil$columnas$dato_personal_protegido) | protegidas
  }
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
  # `meta` describe la CORRIDA, no los datos: la version del paquete, los
  # umbrales, la configuracion. Ya tiene su propio paso dirigido y por columna
  # -centinelas, Benford, cobertura- justo arriba, y el barrido general lo
  # volvia a pisar reemplazando cualquier coincidencia de texto. Medido: con
  # una columna protegida que contiene el valor "1", `meta$version` salia como
  # "0.[valor protegido].0" y la fuente de una medicion decia
  # "[valor protegido].000 columnas".
  meta_declarada <- perfil$meta
  # El campo `patron` es un VOCABULARIO DE FORMA, no un valor: `9+-9+-9+`
  # significa "digitos, guion, digitos, guion, digitos", y ese `9` es el simbolo
  # de "digito". Enmascararlo destruye la unica informacion que el campo existe
  # para dar -quedaba `[valor protegido]+[valor protegido]+[valor protegido]+`-
  # y no protege nada, porque el patron esta construido sobre un alfabeto fijo y
  # nunca lleva un valor de la tabla. Vale para toda columna, protegida o no.
  #
  # `ejemplos` es lo contrario: son valores de la tabla y siguen enmascarados.
  patrones_forma <- NULL
  if (is.list(perfil$patrones) && length(perfil$patrones)) {
    patrones_forma <- lapply(perfil$patrones, function(x) {
      if (is.list(x) && !is.null(x$patron)) x$patron else NULL
    })
  }
  # Enmascarado POR COLUMNA. `.proteger_componentes_perfil()` ya tapo, columna
  # por columna, lo que describe a una columna protegida: su `moda`, sus
  # estadisticos, sus `ejemplos` y la evidencia de sus hallazgos. El barrido
  # general que viene despues busca los valores en TODA la salida, y ahi tapaba
  # de mas: `"S/D"` es un centinela de `cedula` y a la vez el "sin dato" de
  # `sexo`, asi que `sexo` publicaba `[valor protegido]` sin tener un solo dato
  # personal, y el lector no puede distinguir eso de una columna que si los
  # tiene. Publicar `"S/D"` en `sexo` no dice nada de la cedula.
  #
  # Se devuelve el estado por columna SOLO en los componentes donde ese paso ya
  # corrio. `formatos_fecha` y `cobertura_diagnosticos` no lo tienen, asi que
  # ahi el barrido general se queda: es la unica proteccion que tienen.
  # `general`, `datos_personales` y los atributos tampoco tienen columna
  # atribuible.
  # Enmascarado POR COLUMNA, CON UN PISO.
  #
  # `.proteger_componentes_perfil()` ya tapo, columna por columna, lo que
  # describe a una columna protegida: su `moda`, sus estadisticos, sus
  # `ejemplos` y la evidencia de sus hallazgos. Ahi entra TODO valor, largo o
  # corto. El barrido general que viene ahora busca los valores en toda la
  # salida, y lleva SOLO los que identifican.
  #
  # Por que el piso. Sin el, una columna que el clasificador no marco publicaba
  # los valores de la protegida con solo repetirlos: medido, una copia llamada
  # `codigo_operacion` y hasta un texto libre con el documento adentro
  # publicaban tres de cuatro documentos de ocho digitos. Con el piso, un valor
  # que identifica no sale por ninguna puerta, tenga o no columna atribuible.
  #
  # Por que el corte en seis. No es un numero elegido aca: es el que la bateria
  # de fugas ya usa para decidir que cuenta como filtracion, y usar dos
  # definiciones distintas de "identificante" en la regla y en su prueba es
  # exactamente como se cuela un defecto entre las dos.
  #
  # Y por que alcanza con filtrar. El vocabulario corto -`"S/D"`, que es
  # centinela de una cedula y a la vez el "sin dato" de `sexo`- ya quedo tapado
  # donde describe a la columna protegida y no lo toca nadie mas, asi que `sexo`
  # lo publica. No hace falta restituir nada despues del barrido: lo que el
  # barrido no lleva, no lo pisa.
  # `allowNA = TRUE`, y el NA cuenta como identificante. Sin eso, `nchar()` con
  # `type = "chars"` ABORTA sobre una cadena que no es UTF-8 valido -"invalid
  # multibyte string"-, y este paquete trabaja justamente con codificaciones
  # rotas: la suite lo rompio en `detectar_duplicados_aproximados()`. Ante un
  # valor cuyo largo no se puede medir, protegerlo es el lado seguro: es una
  # funcion de privacidad y el valor por omision tiene que ser cerrado.
  identificantes <- .valores_identificantes(valores)
  # Los nombres se toman ANTES de enmascarar: son los de la entrada.
  perfil <- .proteger_textos_salida(
    perfil, identificantes,
    intocables = as.character(perfil$columnas$columna)
  )
  # Y lo mismo sobre los campos NUMERICOS, que el barrido de texto no toca.
  # Sin esto el piso quedaba a medias: medido, una columna copia clasificada
  # `documento_identidad` con poder discriminante debil -asi que no se protege-
  # publicaba en `minimo`, `maximo`, `media` y `mediana` los documentos de la
  # columna que si esta protegida, en el mismo objeto donde esa columna sale
  # enmascarada. Sobre doce filas, `minimo` y `maximo` son dos documentos
  # exactos. Se reusa el ayudante que ya tapaba los parametros de una accion del
  # plan, con el mismo criterio: un numero cuya representacion es un valor
  # identificante protegido no se publica.
  perfil <- .proteger_numeros_parametros(perfil, identificantes)
  perfil$meta <- meta_declarada
  if (!is.null(patrones_forma)) {
    for (i in seq_along(patrones_forma)) {
      if (!is.null(patrones_forma[[i]]) && !is.null(perfil$patrones[[i]])) {
        perfil$patrones[[i]]$patron <- patrones_forma[[i]]
      }
    }
  }
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
