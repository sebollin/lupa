# La misma validacion estaba escrita tres veces —aqui, en `reportar.R` y en
# `analisis.R`— y ya habia divergido en tres cosas a la vez: el orden de las
# comprobaciones, si el mensaje nombra el directorio que falta, y la redaccion
# del aviso de sobrescritura. La de `analisis.R` decia "No existe el directorio
# de destino." sin decir cual, que es justo el dato que necesita quien lo lee.
#
# Devuelve el directorio porque las tres lo usan despues para el archivo
# temporal: escribir primero al lado del destino y copiar es lo que evita dejar
# un archivo a medias si algo falla.
.validar_destino_archivo <- function(archivo, sobrescribir) {
  directorio <- dirname(archivo)
  if (!dir.exists(directorio)) {
    stop("No existe el directorio de destino: ", directorio, ".", call. = FALSE)
  }
  # Un directorio no es un archivo que se pueda reemplazar: `file.copy()` copiaba
  # el temporal ADENTRO, con su nombre de temporal, y la funcion devolvia la ruta
  # del directorio como si ahi estuviera lo guardado. Y sin `sobrescribir`, el
  # mensaje mandaba justamente a ese camino.
  if (dir.exists(archivo)) {
    stop("`archivo` es un directorio: ", archivo, ". Indicar la ruta de un ",
         "archivo dentro de \u00e9l.", call. = FALSE)
  }
  if (file.exists(archivo) && !sobrescribir) {
    stop("El archivo ya existe; use `sobrescribir = TRUE` para reemplazarlo.",
         call. = FALSE)
  }
  directorio
}

.version_esquema_historico <- 1L

.columnas_historico <- c(
  "version_esquema", "nivel", "id_registro", "id_medida", "id_medicion",
  "fecha", "perfil", "regla", "metrica", "metrica_especifica",
  "metrica_instanciada", "dimension", "factor", "granularidad",
  "tipo_resultado", "entidad", "atributo", "fila", "objeto_medible",
  "n_elementos", "resultado", "agregacion"
)

# La configuracion de cada corrida viaja TAMBIEN en las filas `evaluacion_perfil`,
# con los mismos nombres que en el atributo `configuracion_evaluacion`. El
# atributo no lo escribe `write.csv()`, y la deriva de un historico releido de un
# CSV agrupaba todo bajo `<sin_configuracion>`: restaba la corrida de una tabla a
# la de otra y publicaba `error` sobre un cambio de marco que el objeto declaraba
# no comparable. Medido en la ronda 25. Son opcionales a la entrada -un RDS o un
# CSV anterior no las trae y se lee igual, con NA-, asi que el esquema sigue en 1:
# es un agregado, como `parte_no_medida`, y un lector anterior las descarta.
.columnas_configuracion_filas_historico <- c(
  "identidad_tabla", "configuracion_modelo", "configuracion_marco",
  "configuracion_tipos_resultado", "configuracion_aplicabilidad",
  "configuracion_perfil"
)

.columnas_salida_historico <- c(
  .columnas_historico, .columnas_configuracion_filas_historico
)

#' @export
`[.historico_calidad` <- function(x, ...) {
  resultado <- NextMethod("[")
  .conservar_atributos_objeto(resultado, x, por_corrida = "configuracion_evaluacion")
}

#' @export
rbind.historico_calidad <- function(..., deparse.level = 1) {
  # Unir dos historicos es acumularlos: `rbind.data.frame()` se quedaba con la
  # configuracion de corridas del PRIMERO, y la deriva de `rbind(hA, hB)` decia
  # "una sola medicion en la serie" sobre una serie de dos; y repetir un registro
  # dejaba un historico que su propia validacion rechaza. Medido en la ronda 22.
  partes <- list(...)
  if (!all(vapply(partes, inherits, logical(1L), "historico_calidad"))) {
    return(.unir_con_marca_sin_proteger(..., deparse.level = deparse.level))
  }
  Reduce(.combinar_historico, partes)
}

.historico_vacio <- function() {
  resultado <- data.frame(
    version_esquema = integer(), nivel = character(),
    id_registro = character(), id_medida = character(),
    id_medicion = character(), fecha = as.POSIXct(character(), tz = "UTC"),
    perfil = character(), regla = character(), metrica = character(),
    metrica_especifica = character(), metrica_instanciada = character(),
    dimension = character(), factor = character(), granularidad = character(),
    tipo_resultado = character(), entidad = character(), atributo = character(),
    fila = integer(), objeto_medible = character(), n_elementos = integer(),
    resultado = numeric(), agregacion = character(),
    identidad_tabla = character(), configuracion_modelo = character(),
    configuracion_marco = character(),
    configuracion_tipos_resultado = character(),
    configuracion_aplicabilidad = character(),
    configuracion_perfil = character(),
    stringsAsFactors = FALSE
  )
  class(resultado) <- c("historico_calidad", "data.frame")
  attr(resultado, "version_esquema") <- .version_esquema_historico
  attr(resultado, "configuracion_evaluacion") <-
    .configuraciones_historico_vacias()
  resultado
}

.fecha_utc <- function(x) {
  if (inherits(x, "Date")) {
    resultado <- as.POSIXct(x, tz = "UTC")
    attr(resultado, "tzone") <- "UTC"
    return(resultado)
  }
  # Un TEXTO se lee en UTC y elemento por elemento. `as.POSIXct()` lo leia en el
  # huso de la sesion -la misma llamada daba 03:00 o 00:00 UTC segun la maquina- y
  # elegia UN formato para todo el vector: un historico exportado con
  # `write.csv()`, que escribe la medianoche sin la hora, volvia sin las horas de
  # las demas fechas, y la deriva ordenaba al reves dos corridas del mismo dia.
  # Medido en la ronda 22. El historico guarda sus fechas en UTC, y asi las
  # escribe `write.csv()`.
  #
  # Y un texto se lee ENTERO o no se lee. `as.POSIXct(format = )` ignora lo que
  # sigue al formato: "10:00:00-03:00" -la salida de una columna `timestamptz`,
  # y la ayuda nombra la base de datos como destino- se leia como 10:00 UTC, que
  # son las 13:00, y "10:00:00 basura" pasaba igual. En el cambio de hora eso
  # invertia el orden de dos corridas y el veredicto de la deriva. Medido en la
  # ronda 25. El desplazamiento se lee -`Z`, `UTC`, `GMT`, `+HH`, `+HHMM`,
  # `+HH:MM`- y lo que no casa con la forma completa se rechaza.
  if (is.character(x) || is.factor(x)) {
    texto <- trimws(as.character(x))
    resultado <- as.POSIXct(rep(NA_real_, length(texto)), origin = "1970-01-01",
                            tz = "UTC")
    presentes <- !is.na(texto) & nzchar(texto)
    partes <- regmatches(texto, regexec(paste0(
      "^([0-9]{4}-[0-9]{1,2}-[0-9]{1,2})",
      "(?:[ T]([0-9]{1,2}:[0-9]{2}(?::[0-9]{2}(?:[.][0-9]+)?)?)",
      "[ ]*(Z|UTC|GMT|[+-][0-9]{2}(?::?[0-9]{2})?)?)?$"
    ), texto, perl = TRUE))
    leidas <- presentes & lengths(partes) == 4L
    if (any(leidas)) {
      campo <- function(i) vapply(partes[leidas], `[[`, character(1L), i)
      dia <- campo(2L)
      hora <- campo(3L)
      zona <- campo(4L)
      formato <- ifelse(
        !nzchar(hora), "%Y-%m-%d",
        ifelse(nchar(hora) <= 5L, "%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%OS")
      )
      local <- rep(NA_real_, length(dia))
      for (f in unique(formato)) {
        en <- formato == f
        local[en] <- as.numeric(as.POSIXct(
          trimws(paste(dia[en], hora[en])), tz = "UTC", format = f
        ))
      }
      signo <- ifelse(substr(zona, 1L, 1L) == "-", -1, 1)
      digitos <- gsub("[^0-9]", "", zona)
      horas <- suppressWarnings(as.numeric(substr(digitos, 1L, 2L)))
      minutos <- suppressWarnings(as.numeric(substr(digitos, 3L, 4L)))
      horas[!nzchar(digitos)] <- 0
      minutos[is.na(minutos)] <- 0
      fuera <- horas > 14 | minutos >= 60
      local[fuera] <- NA_real_
      # Con `origin`: `as.POSIXct()` de un numero sin el falla antes de R 4.3.
      resultado[leidas] <- as.POSIXct(
        local - signo * (horas * 3600 + minutos * 60), origin = "1970-01-01",
        tz = "UTC"
      )
    }
    ilegibles <- is.na(resultado) & presentes
    if (any(ilegibles)) {
      stop(
        "No se pudo leer como fecha: \"", texto[ilegibles][[1L]], "\"",
        if (sum(ilegibles) > 1L) paste0(" (y ", sum(ilegibles) - 1L, " m\u00e1s)"),
        ". Se espera AAAA-MM-DD, con la hora y el desplazamiento opcionales ",
        "(2026-01-31 10:00:00-03:00).", call. = FALSE
      )
    }
    attr(resultado, "tzone") <- "UTC"
    return(resultado)
  }
  x <- as.POSIXct(x)
  resultado <- as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC")
  attr(resultado, "tzone") <- "UTC"
  resultado
}

.columnas_configuracion_historico <- c(
  "id_medicion", "fecha", "perfil", "identidad_tabla",
  "configuracion_modelo", "configuracion_marco",
  "configuracion_tipos_resultado", "configuracion_aplicabilidad",
  "configuracion_perfil"
)

.configuraciones_historico_vacias <- function() {
  data.frame(
    id_medicion = character(), fecha = as.POSIXct(character(), tz = "UTC"),
    perfil = character(), identidad_tabla = character(),
    configuracion_modelo = character(),
    configuracion_marco = character(),
    configuracion_tipos_resultado = character(),
    configuracion_aplicabilidad = character(),
    configuracion_perfil = character(), stringsAsFactors = FALSE
  )
}

.fila_configuracion_historico <- function(ids, fechas, perfiles,
                                          configuracion_modelo = NULL,
                                          configuracion_aplicabilidad = NULL,
                                          configuracion_perfil = NULL) {
  ids <- .clave_bytes(as.character(ids))
  if (!length(ids) || (
    is.null(configuracion_modelo) &&
      is.null(configuracion_aplicabilidad) &&
      is.null(configuracion_perfil)
  )) return(.configuraciones_historico_vacias())
  if (length(fechas) == 1L) fechas <- rep(fechas, length(ids))
  if (length(perfiles) == 1L) perfiles <- rep(perfiles, length(ids))
  perfiles <- .clave_bytes(as.character(perfiles))
  entidades <- if (is.list(configuracion_modelo) &&
                   length(configuracion_modelo$entidades)) {
    .clave_bytes(paste(
      .identificadores_ordenados(configuracion_modelo$entidades),
      collapse = "+"
    ))
  } else NA_character_
  marco <- if (is.list(configuracion_modelo)) {
    configuracion_modelo$marco
  } else {
    NULL
  }
  tipos_resultado <- if (is.list(configuracion_modelo)) {
    configuracion_modelo$tipos_resultado
  } else {
    NULL
  }
  data.frame(
    id_medicion = ids, fecha = .fecha_utc(fechas), perfil = as.character(perfiles),
    identidad_tabla = rep(entidades, length.out = length(ids)),
    configuracion_modelo = rep(
      if (is.null(configuracion_modelo)) NA_character_ else
        .clave_bytes(.texto_configuracion_calidad(configuracion_modelo)),
      length.out = length(ids)
    ),
    configuracion_marco = rep(
      if (is.null(marco)) NA_character_ else
        .clave_bytes(.texto_configuracion_calidad(marco)),
      length.out = length(ids)
    ),
    configuracion_tipos_resultado = rep(
      if (is.null(tipos_resultado)) NA_character_ else
        .clave_bytes(.texto_configuracion_calidad(tipos_resultado)),
      length.out = length(ids)
    ),
    configuracion_aplicabilidad = rep(
      if (is.null(configuracion_aplicabilidad)) NA_character_ else
        .clave_bytes(as.character(configuracion_aplicabilidad)),
      length.out = length(ids)
    ),
    configuracion_perfil = rep(
      if (is.null(configuracion_perfil)) NA_character_ else
        .clave_bytes(.texto_configuracion_calidad(configuracion_perfil)),
      length.out = length(ids)
    ), stringsAsFactors = FALSE
  )
}

.configuracion_historico_medicion <- function(x) {
  configuracion <- attr(x, "configuracion_modelo", exact = TRUE)
  aplicabilidad <- attr(x, "configuracion_aplicabilidad", exact = TRUE)
  cobertura <- attr(x, "cobertura_metricas", exact = TRUE)
  ids <- .identificadores_unicos(c(
    as.character(x$id_medicion),
    if (inherits(cobertura, "data.frame")) as.character(cobertura$id_medicion)
  ))
  fechas <- if (length(ids) && nrow(x)) {
    x$fecha[.indice_identificador(ids, x$id_medicion)]
  } else if (length(ids) && inherits(cobertura, "data.frame")) {
    cobertura$fecha[.indice_identificador(ids, cobertura$id_medicion)]
  } else {
    as.POSIXct(character())
  }
  .fila_configuracion_historico(
    ids, fechas, NA_character_, configuracion, aplicabilidad
  )
}

.configuracion_historico_evaluacion <- function(x) {
  configuracion <- attr(x, "configuracion_modelo", exact = TRUE)
  aplicabilidad <- attr(x, "configuracion_aplicabilidad", exact = TRUE)
  perfil <- attr(x, "configuracion_perfil", exact = TRUE)
  fuente <- x$perfiles
  if (!inherits(fuente, "data.frame") || !nrow(fuente)) fuente <- x$reglas
  if (!inherits(fuente, "data.frame") || !nrow(fuente)) {
    cobertura <- x$cobertura_metricas
    if (!inherits(cobertura, "data.frame")) {
      return(.configuraciones_historico_vacias())
    }
    ids <- .identificadores_unicos(as.character(cobertura$id_medicion))
    fechas <- cobertura$fecha[.indice_identificador(ids, cobertura$id_medicion)]
    perfiles <- NA_character_
  } else {
    ids <- .identificadores_unicos(as.character(fuente$id_medicion))
    fechas <- fuente$fecha[.indice_identificador(ids, fuente$id_medicion)]
    nombre_perfil <- if (inherits(x$perfiles, "data.frame") &&
                         nrow(x$perfiles)) {
      x$perfiles$perfil[[1L]]
    } else {
      x$reglas$perfil[[1L]]
    }
    perfiles <- rep(nombre_perfil, length(ids))
  }
  .fila_configuracion_historico(
    ids, fechas, perfiles, configuracion, aplicabilidad, perfil
  )
}

.validar_configuraciones_historico <- function(x) {
  if (is.null(x)) return(.configuraciones_historico_vacias())
  if (!inherits(x, "data.frame")) {
    stop("La configuraci\u00f3n del hist\u00f3rico no cumple su esquema tabular.",
         call. = FALSE)
  }
  ausentes <- setdiff(.columnas_configuracion_historico, names(x))
  for (nombre in ausentes) x[[nombre]] <- NA_character_
  x <- x[.columnas_configuracion_historico]
  if (nrow(x) && (
    anyNA(x$id_medicion) || any(!nzchar(x$id_medicion)) || anyNA(x$fecha) ||
      anyDuplicated(paste(
        .nombres_para_operar(x$id_medicion),
        .nombres_para_operar(x$perfil), sep = "\034"
      ))
  )) {
    stop("La configuraci\u00f3n del hist\u00f3rico contiene registros inv\u00e1lidos o duplicados.",
         call. = FALSE)
  }
  x$fecha <- .fecha_utc(x$fecha)
  x
}

.combinar_configuraciones_historico <- function(anterior, nuevo) {
  anterior <- .validar_configuraciones_historico(anterior)
  nuevo <- .validar_configuraciones_historico(nuevo)
  if (!nrow(anterior)) return(nuevo)
  if (!nrow(nuevo)) return(anterior)
  clave_anterior <- paste(
    .nombres_para_operar(anterior$id_medicion),
    .nombres_para_operar(anterior$perfil), sep = "\034"
  )
  clave_nuevo <- paste(
    .nombres_para_operar(nuevo$id_medicion),
    .nombres_para_operar(nuevo$perfil), sep = "\034"
  )
  compartidas <- intersect(clave_nuevo, clave_anterior)
  for (clave in compartidas) {
    i <- match(clave, clave_nuevo)
    j <- match(clave, clave_anterior)
    lado_nuevo <- nuevo[i, , drop = FALSE]
    lado_anterior <- anterior[j, , drop = FALSE]
    # La hermana de esta comparacion -la de filas de datos, en
    # `.combinar_historico()`- normaliza por bytes sus columnas de texto, y esta
    # se habia quedado con `all.equal()` plano. Dos filas con los mismos bytes y
    # distinta marca de codificacion -que es lo que pasa cuando una viene de un
    # RDS y la otra se acaba de calcular- se comparan distintas bajo un locale
    # no UTF-8. Arreglar un lado de una comparacion y dejar el otro es como se
    # cuelan estos defectos.
    for (nombre in intersect(names(lado_anterior), names(lado_nuevo))) {
      if (is.character(lado_anterior[[nombre]]) &&
          is.character(lado_nuevo[[nombre]])) {
        lado_anterior[[nombre]] <- .clave_bytes(lado_anterior[[nombre]])
        lado_nuevo[[nombre]] <- .clave_bytes(lado_nuevo[[nombre]])
      }
    }
    if (!isTRUE(all.equal(lado_nuevo, lado_anterior,
                         check.attributes = FALSE))) {
      # El mensaje decia "configuracion diferente" sin nombrar que difiere, y
      # el caso mas comun es que no difiera ninguna configuracion: reusar el
      # mismo `id_medicion` en dos entregas de meses distintos choca por la
      # FECHA, que es parte de la identidad de la corrida. Acusar a la
      # configuracion manda a revisar el modelo, el marco y la aplicabilidad
      # cuando lo unico distinto es el dia.
      distintos <- character()
      for (nombre in intersect(names(lado_anterior), names(lado_nuevo))) {
        if (!isTRUE(all.equal(lado_nuevo[[nombre]], lado_anterior[[nombre]],
                              check.attributes = FALSE))) {
          distintos <- c(distintos, nombre)
        }
      }
      detalle <- if (length(distintos)) {
        paste0(
          " Difiere en: ", paste(distintos, collapse = ", "), ".",
          if ("fecha" %in% distintos) {
            paste0(
              " La fecha es parte de la identidad de la corrida: para una",
              " entrega nueva, usar otro `id_medicion`."
            )
          } else {
            ""
          }
        )
      } else {
        ""
      }
      stop(
        "Una corrida ya existente no coincide con la que se quiere acumular: ",
        nuevo$id_medicion[[i]], ".", detalle, call. = FALSE
      )
    }
  }
  resultado <- rbind(
    anterior,
    nuevo[!.identificadores_en(clave_nuevo, clave_anterior), , drop = FALSE]
  )
  rownames(resultado) <- NULL
  resultado
}

.valor_columna <- function(x, nombre, tipo = c("texto", "entero")) {
  tipo <- match.arg(tipo)
  indice <- .indice_nombre(nombre, names(x))
  if (is.na(indice)) {
    return(if (tipo == "entero") rep(NA_integer_, nrow(x)) else {
      rep(NA_character_, nrow(x))
    })
  }
  if (tipo == "entero") {
    as.integer(x[[indice]])
  } else {
    as.character(.texto_analizable(x[[indice]])$valores)
  }
}

.parte_historico <- function(x, nivel, id_registro, perfil = NA_character_,
                             regla = NA_character_, n_elementos = 1L) {
  n <- nrow(x)
  if (length(perfil) == 1L) perfil <- rep(perfil, n)
  if (length(regla) == 1L) regla <- rep(regla, n)
  if (length(n_elementos) == 1L) n_elementos <- rep(n_elementos, n)
  data.frame(
    version_esquema = rep(.version_esquema_historico, n),
    nivel = rep(nivel, n), id_registro = id_registro,
    id_medida = .valor_columna(x, "id_medida"),
    id_medicion = .valor_columna(x, "id_medicion"),
    fecha = .fecha_utc(x$fecha), perfil = as.character(perfil),
    regla = as.character(regla), metrica = .valor_columna(x, "metrica"),
    metrica_especifica = .valor_columna(x, "metrica_especifica"),
    metrica_instanciada = .valor_columna(x, "metrica_instanciada"),
    dimension = .valor_columna(x, "dimension"),
    factor = .valor_columna(x, "factor"),
    granularidad = .valor_columna(x, "granularidad"),
    tipo_resultado = .valor_columna(x, "tipo_resultado"),
    entidad = .valor_columna(x, "entidad"),
    atributo = .valor_columna(x, "atributo"),
    fila = .valor_columna(x, "fila", "entero"),
    objeto_medible = .valor_columna(x, "objeto_medible"),
    n_elementos = as.integer(n_elementos), resultado = as.numeric(x$resultado),
    agregacion = .valor_columna(x, "agregacion"),
    stringsAsFactors = FALSE
  )
}

.escapar_clave <- function(x) {
  x <- .clave_bytes(as.character(x))
  cod <- vapply(
    x,
    function(z) if (is.na(z)) NA_character_ else utils::URLencode(z, TRUE),
    character(1L), USE.NAMES = FALSE
  )
  ifelse(is.na(x), "~", paste0("=", cod))
}

.clave_historico <- function(nivel, id_medicion, perfil = NA_character_,
                             regla = NA_character_, id_medida = NA_character_) {
  paste(
    .escapar_clave(nivel), .escapar_clave(id_medicion),
    .escapar_clave(perfil), .escapar_clave(regla),
    .escapar_clave(id_medida), sep = "|"
  )
}

.normalizar_medicion_historico <- function(x) {
  if (!inherits(x, "medicion")) {
    stop("El objeto de medidas debe provenir de medir().", call. = FALSE)
  }
  .validar_medicion_evaluacion(x, permitir_suprimidas = TRUE)
  clave <- .clave_historico("medida", x$id_medicion, id_medida = x$id_medida)
  resultado <- .parte_historico(x, "medida", clave)
  cobertura <- attr(x, "cobertura_metricas", exact = TRUE)
  if (inherits(cobertura, "data.frame") && nrow(cobertura)) {
    resultado <- rbind(
      resultado, .parte_historico_metricas_no_evaluadas(cobertura)
    )
  }
  if (nrow(x)) {
    no_medidas <- .parte_historico_partes_no_medidas(x, x$id_medicion, x$fecha)
    if (!is.null(no_medidas)) resultado <- rbind(resultado, no_medidas)
  }
  attr(resultado, "configuracion_evaluacion") <-
    .configuracion_historico_medicion(x)
  attr(resultado, "datos_personales_sin_proteger") <-
    attr(x, "datos_personales_sin_proteger", exact = TRUE)
  resultado
}

.validar_tabla_evaluacion <- function(x, requeridas, nombre,
                                     permitir_na = FALSE) {
  if (!inherits(x, "data.frame") || !nrow(x) ||
      !all(requeridas %in% names(x)) || anyNA(x$id_medicion) ||
      anyNA(x$fecha) ||
      (!is.numeric(x$resultado) && !is.logical(x$resultado)) ||
      (!isTRUE(permitir_na) && anyNA(x$resultado)) ||
      any(!is.na(x$resultado) & !is.finite(x$resultado)) ||
      any(!is.na(x$resultado) &
          (x$resultado < 0 | x$resultado > 1))) {
    stop("La tabla `", nombre, "` de la evaluaci\u00f3n no cumple su contrato.",
         call. = FALSE)
  }
  x
}

.normalizar_evaluacion_historico <- function(x, detalle) {
  if (!inherits(x, "evaluacion_calidad")) {
    stop("El objeto de evaluaci\u00f3n debe provenir de evaluar().", call. = FALSE)
  }
  cobertura <- x$cobertura_metricas
  tiene_cobertura <- inherits(cobertura, "data.frame") && nrow(cobertura)
  reglas <- .validar_tabla_evaluacion(
    x$reglas, c("id_medicion", "fecha", "perfil", "regla", "n_medidas",
                "resultado"), "reglas", permitir_na = tiene_cobertura
  )
  perfiles <- .validar_tabla_evaluacion(
    x$perfiles, c("id_medicion", "fecha", "perfil", "n_reglas", "resultado"),
    "perfiles", permitir_na = tiene_cobertura
  )
  partes <- list(
    .parte_historico(
      reglas, "evaluacion_regla",
      .clave_historico(
        "evaluacion_regla", reglas$id_medicion, reglas$perfil, reglas$regla
      ),
      perfil = reglas$perfil, regla = reglas$regla,
      n_elementos = reglas$n_medidas
    ),
    .parte_historico(
      perfiles, "evaluacion_perfil",
      .clave_historico(
        "evaluacion_perfil", perfiles$id_medicion, perfiles$perfil
      ),
      perfil = perfiles$perfil, n_elementos = perfiles$n_reglas
    )
  )
  # Con el detalle resumido, las medidas SUPRIMIDAS entran igual, enmascaradas:
  # son lo que permite que una medicion acumulada DESPUES las tape. Sin ellas,
  # `acumular_historico(historico_calidad(evaluacion), medicion)` publicaba con su
  # valor las medidas que la evaluacion mando suprimir, y el resultado dependia
  # del orden en que llegaban los objetos. Medido en la ronda 22. Van en la tabla
  # y no en un atributo porque la tabla se exporta con `write.csv()`.
  if (detalle != "completo") {
    suprimidas <- .ids_suprimidos_de(x)
    if (length(suprimidas) && inherits(x$medidas, "data.frame") &&
        "id_medida" %in% names(x$medidas)) {
      medidas <- x$medidas[
        .identificadores_en(x$medidas$id_medida, suprimidas), , drop = FALSE
      ]
      if (nrow(medidas)) {
        medidas <- .validar_tabla_evaluacion(
          medidas,
          c("id_medida", "id_medicion", "fecha", "perfil", "regla",
            "metrica_instanciada", "resultado"), "medidas"
        )
        partes <- c(list(.parte_historico(
          medidas, "evaluacion_medida",
          .clave_historico(
            "evaluacion_medida", medidas$id_medicion, medidas$perfil,
            medidas$regla, medidas$id_medida
          ),
          perfil = medidas$perfil, regla = medidas$regla
        )), partes)
      }
    }
  }
  if (detalle == "completo") {
    medidas <- .validar_tabla_evaluacion(
      x$medidas,
      c("id_medida", "id_medicion", "fecha", "perfil", "regla",
        "metrica_instanciada", "resultado"), "medidas"
    )
    partes <- c(list(.parte_historico(
      medidas, "evaluacion_medida",
      .clave_historico(
        "evaluacion_medida", medidas$id_medicion, medidas$perfil,
        medidas$regla, medidas$id_medida
      ),
      perfil = medidas$perfil, regla = medidas$regla
    )), partes)
  }
  if (tiene_cobertura) {
    partes <- c(partes, list(.parte_historico_metricas_no_evaluadas(cobertura)))
  }
  no_medidas <- .parte_historico_partes_no_medidas(x, reglas$id_medicion, reglas$fecha)
  if (!is.null(no_medidas)) partes <- c(partes, list(no_medidas))
  resultado <- do.call(rbind, partes)
  resultado <- .enmascarar_suprimidas_historico(resultado, x)
  attr(resultado, "configuracion_evaluacion") <-
    .configuracion_historico_evaluacion(x)
  resultado
}

# `desenlace = "suprimir"` declara que las medidas que no cumplen la condicion NO
# DEBEN PUBLICARSE, y la impresion y el informe lo sostienen enmascarando el
# valor. El historico no: es un data frame que la documentacion presenta como
# exportable directamente con `write.csv()`, y publicaba la fila de la medida
# suprimida con su numero -medido: `evaluacion_medida MS1-000002 resultado = 0`-.
# Dos salidas del mismo objeto con politicas opuestas sobre la misma medida, y la
# que se exporta era la que no la respetaba.
#
# La convencion ya existe en esta tabla para el nivel `medida`: `resultado` en
# `NA` y `[valor suprimido]` en `objeto_medible`, que es lo que la validacion
# reconoce. Se aplica igual al nivel `evaluacion_medida`, sin cambiar el esquema
# ni su version.
.ids_suprimidos_de <- function(evaluacion) {
  desenlaces <- .desenlaces_de_objeto(evaluacion)
  if (!inherits(desenlaces, "data.frame") || !nrow(desenlaces) ||
      !all(c("id_medida", "desenlace") %in% names(desenlaces))) {
    return(character())
  }
  unique(as.character(desenlaces$id_medida[
    as.character(desenlaces$desenlace) == "suprimir"
  ]))
}

.enmascarar_suprimidas_historico <- function(historico, evaluacion) {
  suprimidas <- .ids_suprimidos_de(evaluacion)
  if (!length(suprimidas)) return(historico)
  objetivo <- !is.na(historico$id_medida) &
    .identificadores_en(historico$id_medida, suprimidas) &
    as.character(historico$nivel) %in% c("medida", "evaluacion_medida")
  .marcar_suprimidas_historico(historico, objetivo)
}

# Las medidas que el historico YA tiene suprimidas -una fila `medida` o
# `evaluacion_medida` enmascarada- tapan las filas de la misma corrida y la misma
# medida en LOS DOS niveles, venga en el mismo objeto o en otro que se acumula, y
# sea cual sea el perfil o la regla que la evalua. Se tapaba solo el nivel
# `medida`: la misma medida evaluada por OTRO perfil publicaba su numero en
# `evaluacion_medida` -con una regla `x > 0` sobre una metrica booleana, el valor
# suprimido mismo-, y con las dos reglas en un solo perfil se tapaba. Partir un
# perfil en dos no cambia que se puede publicar. Medido en la ronda 25.
.claves_suprimidas_historico <- function(historico) {
  if (!nrow(historico)) return(character())
  marcadas <- .filas_suprimidas_historico(historico)
  unique(paste(.clave_bytes(as.character(historico$id_medicion[marcadas])),
               .clave_bytes(as.character(historico$id_medida[marcadas])),
               sep = "\r"))
}

# Que filas del historico declaran una medida suprimida: la regla la leen la
# propagacion entre historicos y `reportar()`, que reparte esas supresiones a
# los demas objetos del informe.
.filas_suprimidas_historico <- function(historico) {
  as.character(historico$nivel) %in% c("medida", "evaluacion_medida") &
    is.na(historico$resultado) & !is.na(historico$objeto_medible) &
    grepl("[valor suprimido]", as.character(historico$objeto_medible), fixed = TRUE)
}

.propagar_suprimidas_historico <- function(historico, claves) {
  if (!length(claves) || !nrow(historico)) return(historico)
  objetivo <- as.character(historico$nivel) %in% c("medida", "evaluacion_medida") &
    paste(.clave_bytes(as.character(historico$id_medicion)),
          .clave_bytes(as.character(historico$id_medida)), sep = "\r") %in% claves
  .marcar_suprimidas_historico(historico, objetivo)
}

.marcar_suprimidas_historico <- function(historico, objetivo) {
  objetivo[is.na(objetivo)] <- FALSE
  if (!any(objetivo)) return(historico)
  historico$resultado[objetivo] <- NA_real_
  historico$objeto_medible <- as.character(historico$objeto_medible)
  ya_marcadas <- !is.na(historico$objeto_medible) &
    grepl("[valor suprimido]", historico$objeto_medible, fixed = TRUE)
  nuevas <- objetivo & !ya_marcadas
  historico$objeto_medible[nuevas] <- ifelse(
    is.na(historico$objeto_medible[nuevas]),
    "[valor suprimido]",
    paste0(historico$objeto_medible[nuevas], " [valor suprimido]")
  )
  historico
}

.parte_historico_metricas_no_evaluadas <- function(cobertura) {
  n <- nrow(cobertura)
  data.frame(
    version_esquema = rep(.version_esquema_historico, n),
    nivel = rep("metrica_no_evaluada", n),
    id_registro = .clave_historico(
      "metrica_no_evaluada", cobertura$id_medicion,
      cobertura$metrica_instanciada
    ),
    id_medida = rep(NA_character_, n),
    id_medicion = as.character(cobertura$id_medicion),
    fecha = .fecha_utc(cobertura$fecha),
    perfil = rep(NA_character_, n), regla = rep(NA_character_, n),
    metrica = as.character(cobertura$metrica),
    metrica_especifica = as.character(cobertura$metrica_especifica),
    metrica_instanciada = as.character(cobertura$metrica_instanciada),
    dimension = rep(NA_character_, n), factor = rep(NA_character_, n),
    granularidad = rep(NA_character_, n), tipo_resultado = rep(NA_character_, n),
    entidad = as.character(cobertura$entidad),
    atributo = as.character(cobertura$atributo), fila = rep(NA_integer_, n),
    objeto_medible = paste0("M\u00e9trica no evaluada: ", cobertura$motivo),
    n_elementos = rep(NA_integer_, n), resultado = rep(NA_real_, n),
    agregacion = as.character(cobertura$estado), stringsAsFactors = FALSE
  )
}

# La cobertura de la FRONTERA en el historico: una tabla -o una coleccion, o una
# organizacion- declarada que no entro al numero. La medicion la llevaba y el
# historico no tenia donde guardarla, asi que la serie de una coleccion
# incompleta se leia como la de una completa. Un registro por parte sin medir,
# con su motivo, como `metrica_no_evaluada`. Medido en la ronda 24.
#
# Con VARIAS corridas en el objeto -`rbind()` de dos mediciones- devolvia NULL:
# la coleccion incompleta de cada corrida quedaba registrada como completa, que
# es la falla que el nivel vino a cerrar. `rbind()` marca ahora cada cobertura
# con su `id_medicion`, y cada parte va a su corrida. Una cobertura sin marca en
# un objeto de varias corridas no se puede atribuir: se rechaza, no se calla.
# Medido en la ronda 25.
.parte_historico_partes_no_medidas <- function(x, ids, fechas) {
  corridas <- .identificadores_unicos(ids)
  if (!length(corridas)) return(NULL)
  fecha_de <- function(id) fechas[[.indice_identificador(id, ids)]]
  coberturas <- list()
  for (atributo in .ATRIBUTOS_COBERTURA_FRONTERA) {
    valor <- attr(x, atributo, exact = TRUE)
    if (!is.null(valor)) coberturas <- c(coberturas, stats::setNames(list(valor), atributo))
  }
  partes_previas <- attr(x, "cobertura_de_partes", exact = TRUE)
  if (length(partes_previas)) coberturas <- c(coberturas, partes_previas)
  coberturas <- .coberturas_sin_repetir(coberturas)
  if (!length(coberturas)) return(NULL)
  filas <- lapply(seq_along(coberturas), function(i) {
    cc <- coberturas[[i]]
    atributo <- names(coberturas)[[i]]
    partes <- if (!is.null(cc$tablas_sin_medir)) cc$tablas_sin_medir else cc$sin_medir
    partes <- as.character(partes)
    if (!length(partes)) return(NULL)
    ids <- if (!is.null(cc$id_medicion)) {
      # Una cobertura de otra corrida -un objeto de varias, recortado con `[`-
      # no es de esta.
      if (!.identificadores_en(cc$id_medicion, corridas)) return(NULL)
      .identificadores_unicos(cc$id_medicion)
    } else if (length(corridas) == 1L) {
      corridas
    } else {
      stop(
        "El objeto re\u00fane ", length(corridas), " corridas y su cobertura de ",
        "la ", .etiqueta_cobertura_parte(atributo, cc), " no dice a cu\u00e1l ",
        "pertenece, as\u00ed que el hist\u00f3rico no puede registrar la parte ",
        "que no se midi\u00f3. Acumular cada corrida por separado: ",
        "`historico_calidad(corrida_1, corrida_2)`.", call. = FALSE
      )
    }
    motivos <- if (length(cc$motivo_sin_medir) == length(partes)) {
      as.character(cc$motivo_sin_medir)
    } else rep("", length(partes))
    frontera <- .etiqueta_cobertura_parte(atributo, cc)
    granularidad <- switch(
      atributo,
      cobertura_coleccion = "entidad",
      cobertura_conjunto_colecciones = "coleccion",
      cobertura_organizacion = "coleccion",
      cobertura_conjunto_organizaciones = "organizacion",
      NA_character_
    )
    n <- length(partes)
    data.frame(
      version_esquema = rep(.version_esquema_historico, n),
      nivel = rep("parte_no_medida", n),
      id_registro = .clave_historico(
        "parte_no_medida", rep(ids, n), perfil = rep(frontera, n),
        regla = partes
      ),
      id_medida = rep(NA_character_, n),
      id_medicion = rep(ids, n), fecha = rep(.fecha_utc(fecha_de(ids)), n),
      perfil = rep(NA_character_, n), regla = rep(NA_character_, n),
      metrica = rep(NA_character_, n), metrica_especifica = rep(NA_character_, n),
      metrica_instanciada = rep(NA_character_, n),
      dimension = rep(NA_character_, n), factor = rep(NA_character_, n),
      granularidad = rep(granularidad, n),
      tipo_resultado = rep(NA_character_, n), entidad = partes,
      atributo = rep(NA_character_, n), fila = rep(NA_integer_, n),
      objeto_medible = paste0(
        "Parte no medida de la ", frontera, ": ", motivos
      ),
      n_elementos = rep(NA_integer_, n), resultado = rep(NA_real_, n),
      agregacion = rep(NA_character_, n), stringsAsFactors = FALSE
    )
  })
  filas <- Filter(Negate(is.null), filas)
  if (!length(filas)) return(NULL)
  salida <- do.call(rbind, filas)
  salida[!duplicated(.nombres_para_operar(salida$id_registro)), , drop = FALSE]
}

# El tipo de cada columna del esquema, restituido. `read.csv()` lee como
# `logical` toda columna que viene entera en NA -`agregacion` casi siempre, y
# trece columnas en un historico solo de evaluaciones-, y la comparacion de un
# registro repetido decia "Difieren: id_medida (NA contra NA)": re-acumular sobre
# el CSV la corrida que ya tenia -el caso de uso de la idempotencia- se rechazaba.
# Medido en la ronda 25. El tipo lo fija el esquema, no el lector.
.columnas_historico_texto <- c(
  "nivel", "id_registro", "id_medida", "id_medicion", "perfil", "regla",
  "metrica", "metrica_especifica", "metrica_instanciada", "dimension", "factor",
  "granularidad", "tipo_resultado", "entidad", "atributo", "objeto_medible",
  "agregacion", .columnas_configuracion_filas_historico
)

.restituir_tipos_historico <- function(x) {
  for (nombre in .columnas_configuracion_filas_historico) {
    if (!nombre %in% names(x)) x[[nombre]] <- rep(NA_character_, nrow(x))
  }
  for (nombre in .columnas_historico_texto) {
    if (!is.character(x[[nombre]])) x[[nombre]] <- as.character(x[[nombre]])
  }
  for (nombre in c("version_esquema", "fila", "n_elementos")) {
    valores <- x[[nombre]]
    if (is.logical(valores) ||
        (is.numeric(valores) && !is.integer(valores) &&
         all(is.na(valores) | valores == round(valores)))) {
      x[[nombre]] <- as.integer(valores)
    }
  }
  if (is.logical(x$resultado)) x$resultado <- as.numeric(x$resultado)
  x
}

.clave_configuracion_historico <- function(ids, perfiles) {
  paste(
    .nombres_para_operar(as.character(ids)),
    .nombres_para_operar(as.character(perfiles)), sep = "\034"
  )
}

# Sin el atributo -un historico leido de un CSV-, la configuracion de cada
# corrida se reconstruye de sus filas `evaluacion_perfil`. Con el atributo, manda
# el atributo: lo que falta en el se completa de las filas.
.configuracion_desde_filas_historico <- function(x, configuracion) {
  if (!nrow(x)) return(configuracion)
  columnas <- .columnas_configuracion_filas_historico
  filas <- as.character(x$nivel) %in% "evaluacion_perfil"
  if (any(filas)) {
    filas[filas] <- Reduce(`|`, lapply(columnas, function(nombre) {
      !is.na(x[[nombre]][filas])
    }))
  }
  if (!any(filas)) return(configuracion)
  desde <- data.frame(
    id_medicion = .clave_bytes(as.character(x$id_medicion[filas])),
    fecha = x$fecha[filas],
    perfil = .clave_bytes(as.character(x$perfil[filas])),
    stringsAsFactors = FALSE
  )
  for (nombre in columnas) {
    desde[[nombre]] <- .clave_bytes(as.character(x[[nombre]][filas]))
  }
  desde <- desde[.columnas_configuracion_historico]
  clave <- .clave_configuracion_historico(desde$id_medicion, desde$perfil)
  nuevas <- !clave %in% .clave_configuracion_historico(
    configuracion$id_medicion, configuracion$perfil
  ) & !duplicated(clave)
  if (!any(nuevas)) return(configuracion)
  resultado <- rbind(configuracion, desde[nuevas, , drop = FALSE])
  rownames(resultado) <- NULL
  .validar_configuraciones_historico(resultado)
}

# Y al reves: las columnas de configuracion de las filas se escriben desde el
# atributo, que es una sola fuente. Fuera de `evaluacion_perfil` quedan en NA.
.configuracion_a_filas_historico <- function(x, configuracion) {
  columnas <- .columnas_configuracion_filas_historico
  for (nombre in columnas) x[[nombre]] <- rep(NA_character_, nrow(x))
  filas <- which(as.character(x$nivel) %in% "evaluacion_perfil")
  if (!length(filas) || !nrow(configuracion)) return(x)
  i <- match(
    .clave_configuracion_historico(x$id_medicion[filas], x$perfil[filas]),
    .clave_configuracion_historico(configuracion$id_medicion, configuracion$perfil)
  )
  con <- !is.na(i)
  for (nombre in columnas) {
    x[[nombre]][filas[con]] <- as.character(configuracion[[nombre]][i[con]])
  }
  x
}

.validar_historico <- function(x) {
  if (!inherits(x, "data.frame") ||
      !all(.columnas_historico %in% names(x))) {
    stop("`historico` no cumple el esquema tabular esperado.", call. = FALSE)
  }
  configuracion <- attr(x, "configuracion_evaluacion", exact = TRUE)
  # La marca de que el historico viene de mediciones SIN proteccion: `reportar()`
  # la lee para no publicarlo en claro, y `.tabla_base()` la borraba.
  sin_proteger <- attr(x, "datos_personales_sin_proteger", exact = TRUE)
  x <- .tabla_base(x)
  x <- .restituir_tipos_historico(x)
  # La fecha se lee ANTES de las guardas: la de "una unica fecha por corrida"
  # hacia `as.numeric()` sobre el texto de un historico leido de un CSV, todo
  # daba NA, la guarda no corria y se filtraba el aviso de coercion de R.
  # Medido en la ronda 22.
  if (nrow(x)) x$fecha <- .fecha_utc(x$fecha)
  configuracion <- .validar_configuraciones_historico(configuracion)
  configuracion <- .configuracion_desde_filas_historico(x, configuracion)
  version <- unique(x$version_esquema)
  if (!length(version)) version <- attr(x, "version_esquema", exact = TRUE)
  if (length(version) != 1L || is.na(version) ||
      version != .version_esquema_historico) {
    stop(
      "Versi\u00f3n de esquema hist\u00f3rico no compatible: ",
      paste(version, collapse = ", "), ".", call. = FALSE
    )
  }
  ids_incompletos <- if (nrow(x)) {
    .identificadores_unicos(x$id_medicion[x$nivel == "metrica_no_evaluada"])
  } else character()
  valores_suprimidos <- if (nrow(x) && "objeto_medible" %in% names(x)) {
    !is.na(x$objeto_medible) &
      grepl("[valor suprimido]", x$objeto_medible, fixed = TRUE)
  } else {
    rep(FALSE, nrow(x))
  }
  niveles_evaluacion <- x$nivel %in% c(
    "evaluacion_medida", "evaluacion_regla", "evaluacion_perfil"
  )
  if (nrow(x) &&
      (anyNA(x$id_registro) || any(!nzchar(x$id_registro)) ||
       anyNA(x$id_medicion) || any(!nzchar(x$id_medicion)) ||
       anyNA(x$fecha) ||
       any(is.na(x$resultado) & !(
         x$nivel %in% c("metrica_no_evaluada", "parte_no_medida") |
           niveles_evaluacion & .identificadores_en(
             x$id_medicion, ids_incompletos
           ) |
           # La supresion declarada vale en los dos niveles donde una medida
           # aparece: la medida cruda y su evaluacion. Antes solo se admitia en
           # `medida`, asi que enmascarar la evaluacion -que es lo que el
           # historico completo exporta- chocaba con esta misma validacion.
           x$nivel %in% c("medida", "evaluacion_medida") & valores_suprimidos
       )))) {
    stop("El hist\u00f3rico contiene identificadores, fechas o resultados inv\u00e1lidos.",
         call. = FALSE)
  }
  if (nrow(x)) {
    medidas <- x$nivel == "medida"
    medidas_validas <- medidas & !valores_suprimidos
    if (any(medidas) && !.resultados_validos_tipo(
      x$resultado[medidas_validas], x$tipo_resultado[medidas_validas]
    )) {
      stop("Los resultados de las medidas hist\u00f3ricas no respetan su tipo.",
           call. = FALSE)
    }
    evaluaciones <- x$nivel %in% c(
      "evaluacion_medida", "evaluacion_regla", "evaluacion_perfil"
    )
    if (any(evaluaciones) && any(
      !is.na(x$resultado[evaluaciones]) &
        (x$resultado[evaluaciones] < 0 | x$resultado[evaluaciones] > 1)
    )) {
      stop("Los resultados de evaluaciones hist\u00f3ricas deben estar en [0, 1].",
           call. = FALSE)
    }
  }
  niveles <- c(
    "medida", "evaluacion_medida", "evaluacion_regla", "evaluacion_perfil",
    "metrica_no_evaluada", "parte_no_medida"
  )
  if (nrow(x) && (any(!x$nivel %in% niveles) ||
                  anyDuplicated(.nombres_para_operar(x$id_registro)))) {
    stop("El hist\u00f3rico contiene niveles o identificadores de registro duplicados.",
         call. = FALSE)
  }
  if (nrow(x)) {
    fechas <- split(
      as.numeric(x$fecha), .nombres_para_operar(x$id_medicion), drop = TRUE
    )
    if (any(vapply(fechas, function(z) length(unique(z)) != 1L, logical(1L)))) {
      stop("Cada `id_medicion` debe corresponder a una \u00fanica fecha.",
           call. = FALSE)
    }
  }
  x <- .seleccionar_columnas(x, .columnas_salida_historico)
  x$fecha <- .fecha_utc(x$fecha)
  x <- .configuracion_a_filas_historico(x, configuracion)
  class(x) <- c("historico_calidad", "data.frame")
  attr(x, "version_esquema") <- .version_esquema_historico
  attr(x, "configuracion_evaluacion") <- configuracion
  attr(x, "datos_personales_sin_proteger") <- sin_proteger
  x
}

.combinar_historico <- function(anterior, nuevo) {
  anterior <- .validar_historico(anterior)
  nuevo <- .validar_historico(nuevo)
  # La configuracion de una corrida que ya no tiene filas sobra: quedaba despues
  # de quitarla con `dplyr::filter()`, que no pasa por `[`, y volver a acumular
  # esa corrida corregida chocaba con la configuracion vieja. Medido en la ronda
  # 22.
  vigente <- function(x) {
    tabla <- attr(x, "configuracion_evaluacion", exact = TRUE)
    if (!inherits(tabla, "data.frame") || !"id_medicion" %in% names(tabla) ||
        !nrow(x)) {
      return(tabla)
    }
    tabla[as.character(tabla$id_medicion) %in% as.character(x$id_medicion), ,
          drop = FALSE]
  }
  configuracion <- .combinar_configuraciones_historico(
    vigente(anterior), vigente(nuevo)
  )
  suprimidas <- unique(c(
    .claves_suprimidas_historico(anterior), .claves_suprimidas_historico(nuevo)
  ))
  anterior <- .propagar_suprimidas_historico(anterior, suprimidas)
  nuevo <- .propagar_suprimidas_historico(nuevo, suprimidas)
  coincidencias <- .indice_identificador(
    nuevo$id_registro, anterior$id_registro
  )
  coincidencias[is.na(coincidencias)] <- 0L
  repetidos <- which(coincidencias > 0L)
  if (length(repetidos)) {
    iguales <- vapply(repetidos, function(i) {
      j <- coincidencias[[i]]
      lado_anterior <- .seleccionar_columnas(
        anterior, .columnas_historico, filas = j
      )
      lado_nuevo <- .seleccionar_columnas(
        nuevo, .columnas_historico, filas = i
      )
      for (nombre in intersect(names(lado_anterior), names(lado_nuevo))) {
        if (is.character(lado_anterior[[nombre]]) &&
            is.character(lado_nuevo[[nombre]])) {
          lado_anterior[[nombre]] <- .clave_bytes(lado_anterior[[nombre]])
          lado_nuevo[[nombre]] <- .clave_bytes(lado_nuevo[[nombre]])
        }
      }
      isTRUE(all.equal(lado_anterior, lado_nuevo, check.attributes = FALSE))
    }, logical(1L))
    if (any(!iguales)) {
      # El mensaje nombraba el registro y no QUE difiere, asi que quien lo
      # recibe no sabe si le cambio un resultado, una fecha o una etiqueta. Y
      # cuando el choque aparece solo en otra plataforma -medido en Windows, con
      # una clave con tilde- la diferencia es lo unico que permite entenderlo sin
      # gastar una corrida por hipotesis. Se publica el campo y los dos valores,
      # escapados, para que el mensaje no dependa a su vez de la codificacion de
      # quien lo lea.
      primero <- repetidos[which(!iguales)[[1L]]]
      lado_anterior <- .seleccionar_columnas(
        anterior, .columnas_historico, filas = coincidencias[[primero]]
      )
      lado_nuevo <- .seleccionar_columnas(
        nuevo, .columnas_historico, filas = primero
      )
      comunes <- intersect(names(lado_anterior), names(lado_nuevo))
      difieren <- comunes[!vapply(comunes, function(nombre) {
        isTRUE(all.equal(
          lado_anterior[[nombre]], lado_nuevo[[nombre]],
          check.attributes = FALSE
        ))
      }, logical(1L))]
      detalle <- if (length(difieren)) {
        paste0(
          " Difieren: ",
          paste(vapply(utils::head(difieren, 3L), function(nombre) {
            paste0(
              nombre, " (", encodeString(
                as.character(lado_anterior[[nombre]])[[1L]], quote = "\""
              ), " contra ", encodeString(
                as.character(lado_nuevo[[nombre]])[[1L]], quote = "\""
              ), ")"
            )
          }, character(1L)), collapse = "; "), "."
        )
      } else ""
      stop(
        "Un registro ya existente tiene contenido diferente: ",
        nuevo$id_registro[primero], ".", detalle,
        call. = FALSE
      )
    }
  }
  agregar <- nuevo[coincidencias == 0L, , drop = FALSE]
  resultado <- if (nrow(agregar)) {
    rbind.data.frame(anterior, agregar)
  } else anterior
  rownames(resultado) <- NULL
  attr(resultado, "configuracion_evaluacion") <- configuracion
  # `rbind()` conserva solo la marca del primero: se unen las dos.
  sin_proteger <- unique(c(
    attr(anterior, "datos_personales_sin_proteger", exact = TRUE),
    attr(nuevo, "datos_personales_sin_proteger", exact = TRUE)
  ))
  attr(resultado, "datos_personales_sin_proteger") <-
    if (length(sin_proteger)) sin_proteger
  .validar_historico(resultado)
}

.normalizar_objeto_historico <- function(x, detalle) {
  if (inherits(x, "historico_calidad")) return(.validar_historico(x))
  if (inherits(x, "medicion")) return(.normalizar_medicion_historico(x))
  if (inherits(x, "evaluacion_calidad")) {
    return(.normalizar_evaluacion_historico(x, detalle))
  }
  stop(
    "Cada objeto debe ser una medicion, evaluacion_calidad o historico_calidad.",
    call. = FALSE
  )
}

.proteger_historico_desenlaces <- function(x, desenlaces) {
  if (!inherits(x, "data.frame") || !nrow(x) || is.null(desenlaces) ||
      !all(c("nivel", "resultado", "objeto_medible") %in% names(x))) {
    return(x)
  }
  medidas <- x$nivel == "medida"
  if (!any(medidas)) return(x)
  suprimidas <- rep(FALSE, nrow(x))
  suprimidas[medidas] <- .filas_desenlaces(
    x[medidas, , drop = FALSE], desenlaces
  )
  if (any(suprimidas)) {
    x$resultado[suprimidas] <- NA_real_
    x$objeto_medible <- as.character(x$objeto_medible)
    ya_marcadas <- !is.na(x$objeto_medible) & grepl(
      "[valor suprimido]", x$objeto_medible, fixed = TRUE
    )
    nuevas <- suprimidas & !ya_marcadas
    x$objeto_medible[nuevas] <- paste0(
      x$objeto_medible[nuevas], " [valor suprimido]"
    )
  }
  x
}

.desenlaces_historico <- function(objetos) {
  partes <- lapply(objetos, .desenlaces_de_objeto)
  partes <- partes[vapply(partes, function(x) {
    inherits(x, "data.frame") && nrow(x)
  }, logical(1L))]
  if (!length(partes)) return(NULL)
  resultado <- do.call(rbind, partes)
  clave <- paste(
    .nombres_para_operar(resultado$id_medicion),
    .nombres_para_operar(resultado$id_medida),
    .nombres_para_operar(resultado$regla), sep = "\r"
  )
  resultado[!duplicated(clave), , drop = FALSE]
}

#' Construir y ampliar un histórico de calidad
#'
#' Crea un data frame plano y versionado con corridas producidas por [medir()] o
#' [evaluar()]. `acumular_historico()` agrega objetos al mismo esquema y es
#' idempotente cuando recibe otra vez registros idénticos, incluso si el RDS se
#' guardó o se vuelve a leer bajo otro locale.
#'
#' @param ... Objetos `medicion`, `evaluacion_calidad` o `historico_calidad`.
#'   También puede darse una única lista que los contenga.
#' @param detalle Para evaluaciones, `"resumen"` conserva los niveles de regla y
#'   perfil; `"completo"` conserva además cada evaluación de medida. Una
#'   `medicion` pasada explícitamente siempre se conserva completa.
#' @param historico Objeto creado por `historico_calidad()`.
#'
#' @return Data frame S3 `historico_calidad`. La columna `version_esquema` y el
#'   atributo del mismo nombre permiten migraciones futuras. `nivel` corresponde
#'   a `medida`, `evaluacion_medida`, `evaluacion_regla` o
#'   `evaluacion_perfil`; una métrica sin valores se conserva como
#'   `metrica_no_evaluada` con su motivo, siempre que la medición tenga al
#'   menos una medida; y una parte declarada en una frontera —una tabla de la
#'   colección, una colección de la organización— que no entró al número, como
#'   `parte_no_medida`, con la frontera en `id_registro`, la parte en `entidad`
#'   y el motivo en `objeto_medible` —también cuando un objeto reúne varias
#'   corridas con `rbind()`: cada parte va a la corrida a la que le faltó—. Una medida que una regla declaró `desenlace = "suprimir"`
#'   no publica su valor **tampoco aquí**: su fila deja `resultado` en `NA` y
#'   marca `objeto_medible` con `[valor suprimido]`, en los dos niveles donde esa
#'   medida aparece —`medida` y `evaluacion_medida`, en este último bajo
#'   cualquier perfil o regla que la evalúe, no solo el que la suprimió—,
#'   porque esta tabla está pensada para exportarse. Con el detalle resumido, las medidas suprimidas
#'   entran igual, enmascaradas en el nivel `evaluacion_medida`: así una
#'   medición acumulada **después** —también sobre un histórico guardado o
#'   leído de un CSV— queda tapada en las mismas medidas, sea cual sea el orden
#'   en que llegan los objetos. Una medición **enteramente** vacía —ninguna métrica
#'   pudo aplicarse— no se acumula: se rechaza citando el motivo que `medir()`
#'   declaró en `cobertura_metricas`, porque no hay corrida que registrar. El atributo
#'   `configuracion_evaluacion` conserva, en una tabla plana separada, el
#'   modelo, su marco y sus tipos, la aplicabilidad, el perfil, la identidad
#'   de tabla y **la fecha** de cada corrida. Acumular una corrida con un
#'   `id_medicion` ya presente exige que todo eso coincida, la fecha incluida:
#'   dos entregas distintas son dos corridas, y el error nombra en qué difieren.
#'   Esa misma configuración viaja **en las filas** `evaluacion_perfil`, en las
#'   columnas `identidad_tabla`, `configuracion_modelo`, `configuracion_marco`,
#'   `configuracion_tipos_resultado`, `configuracion_aplicabilidad` y
#'   `configuracion_perfil` —en `NA` en los demás niveles—, porque el atributo no
#'   sobrevive a `write.csv()`: un histórico releído de un CSV la recupera de
#'   ahí. Son columnas agregadas al esquema 1: un histórico anterior que no las
#'   trae se lee igual, con ellas en `NA`.
#'
#' @details
#' El detalle predeterminado evita repetir una fila por celda y regla cuando el
#' objetivo es monitorear la serie de evaluaciones. El objeto no guarda modelos,
#' closures, datos originales ni perfiles de profiling. Esto mantiene la tabla
#' exportable directamente con `write.csv()` o una herramienta de base de datos.
#' Al volver a leerla, cada columna recupera el tipo del esquema —`read.csv()`
#' lee como lógica una columna de texto que viene entera en `NA`— y las fechas
#' de texto se leen en UTC, con su hora y con su desplazamiento cuando lo traen
#' —`2026-01-31 10:00:00-03:00` son las 13:00 UTC; también `Z`, `+0100` o
#' `-03`, como los escribe una columna `timestamptz` de una base—. Un texto que
#' no se puede leer entero se rechaza: no se lee a medias.
#'
#' `[`, `subset()` y `dplyr::filter()` conservan la configuración de las
#' corridas que quedan; `rbind()` de dos históricos los acumula, como
#' [acumular_historico()].
#'
#' El esquema largo mapea las cuatro tablas de la sección 9.5 del marco mediante
#' `nivel`. Las columnas que no corresponden a un nivel quedan como `NA`.
#'
#' @references [AGESIC (2020)](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/).
#'   *Marco de trabajo para la Gestión de la Calidad
#'   de Datos en Gobierno Digital*, versión 1.6, sección 9.5, Presidencia de la
#'   República, Uruguay.
#'
#' @export
#' @seealso [medir()], [evaluar()], [detectar_deriva_calidad()], [reportar()]
#'
#' @examples
#' nucleo <- metricas_nucleo()
#' instancia <- instanciar(especializar(nucleo$NoNulo), "personas", "edad")
#' medidas <- medir(
#'   modelo(instancia), data.frame(edad = c(20, NA)),
#'   id_medicion = "enero", fecha = as.POSIXct("2026-01-31", tz = "UTC")
#' )
#' evaluacion <- evaluar(
#'   medidas,
#'   perfil_evaluacion("Basico", regla_evaluacion("Presente", function(x) x > 0))
#' )
#' historico_calidad(medidas, evaluacion)
historico_calidad <- function(..., detalle = c("resumen", "completo")) {
  detalle <- match.arg(detalle)
  objetos <- list(...)
  # Un unico argumento que sea una lista se despliega como lista de objetos. Un
  # `data.frame` TAMBIEN es una lista, y por eso `historico_calidad(data.frame())`
  # se desplegaba a cero objetos y devolvia un historico vacio en silencio,
  # mientras `historico_calidad(NULL)` y `character(0)` -igual de vacios- daban
  # error. Cuatro entradas equivalentes, dos conductas opuestas; y para las
  # aceptadas el mensaje de las rechazadas era falso: no habia ningun objeto
  # malo, no habia objetos.
  #
  # Un `data.frame` no es un contenedor de mediciones: no se despliega y cae en
  # la validacion, que lo nombra.
  if (length(objetos) == 1L && is.list(objetos[[1L]]) &&
      !inherits(objetos[[1L]], "data.frame") &&
      !inherits(objetos[[1L]], c(
        "medicion", "evaluacion_calidad", "historico_calidad"
      ))) {
    objetos <- objetos[[1L]]
  }
  resultado <- .historico_vacio()
  for (objeto in objetos) {
    resultado <- .combinar_historico(
      resultado, .normalizar_objeto_historico(objeto, detalle)
    )
  }
  resultado <- .proteger_historico_desenlaces(
    resultado, .desenlaces_historico(objetos)
  )
  .validar_historico(resultado)
}

#' @rdname historico_calidad
#' @export
#' @seealso [historico_calidad()], [leer_historico()]
acumular_historico <- function(historico, ...,
                               detalle = c("resumen", "completo")) {
  detalle <- match.arg(detalle)
  resultado <- .validar_historico(historico)
  objetos <- list(...)
  # Un unico argumento que sea una lista se despliega como lista de objetos. Un
  # `data.frame` TAMBIEN es una lista, y por eso `historico_calidad(data.frame())`
  # se desplegaba a cero objetos y devolvia un historico vacio en silencio,
  # mientras `historico_calidad(NULL)` y `character(0)` -igual de vacios- daban
  # error. Cuatro entradas equivalentes, dos conductas opuestas; y para las
  # aceptadas el mensaje de las rechazadas era falso: no habia ningun objeto
  # malo, no habia objetos.
  #
  # Un `data.frame` no es un contenedor de mediciones: no se despliega y cae en
  # la validacion, que lo nombra.
  if (length(objetos) == 1L && is.list(objetos[[1L]]) &&
      !inherits(objetos[[1L]], "data.frame") &&
      !inherits(objetos[[1L]], c(
        "medicion", "evaluacion_calidad", "historico_calidad"
      ))) {
    objetos <- objetos[[1L]]
  }
  for (objeto in objetos) {
    resultado <- .combinar_historico(
      resultado, .normalizar_objeto_historico(objeto, detalle)
    )
  }
  resultado <- .proteger_historico_desenlaces(
    resultado, .desenlaces_historico(objetos)
  )
  .validar_historico(resultado)
}

#' Guardar y recuperar un histórico de calidad
#'
#' Persiste el data frame versionado mediante RDS de base R. La escritura no
#' reemplaza un archivo existente salvo consentimiento explícito.
#'
#' @param historico Objeto creado por [historico_calidad()].
#' @param archivo Ruta del archivo RDS. Una ruta que ya es un directorio se
#'   rechaza: el archivo va dentro, con su nombre.
#' @param sobrescribir Si se permite reemplazar un archivo existente.
#'
#' @return `guardar_historico()` devuelve invisiblemente la ruta normalizada;
#'   `leer_historico()` devuelve un `historico_calidad` validado.
#' @export
#' @seealso [guardar_historico()], [detectar_deriva_calidad()]
#'
#' @examples
#' archivo <- tempfile(fileext = ".rds")
#' guardar_historico(historico_calidad(), archivo)
#' leer_historico(archivo)
#' unlink(archivo)
guardar_historico <- function(historico, archivo, sobrescribir = FALSE) {
  historico <- .validar_historico(historico)
  if (!.es_texto_escalar(archivo)) {
    stop("`archivo` debe ser una ruta no vac\u00eda.", call. = FALSE)
  }
  if (!is.logical(sobrescribir) || length(sobrescribir) != 1L ||
      is.na(sobrescribir)) {
    stop("`sobrescribir` debe ser TRUE o FALSE.", call. = FALSE)
  }
  directorio <- .validar_destino_archivo(archivo, sobrescribir)
  temporal <- tempfile(".lupa-historico-", tmpdir = directorio)
  on.exit(unlink(temporal), add = TRUE)
  # Version 2 y no 3, y no es una preferencia: es lo unico que conserva el texto.
  #
  # El formato 3 anota en la cabecera la codificacion NATIVA de quien escribe y
  # al leer TRADUCE desde ella las cadenas sin marca. Escribiendo bajo
  # `LC_CTYPE=C` la cabecera dice `ANSI_X3.4-1968`; al leer bajo UTF-8, glibc
  # falla al traducir y R deja los bytes intactos -el dato se salva por el
  # camino del error-, pero `win_iconv` NO falla: apaga el bit alto y devuelve
  # algo. Medido en R-hub Windows, `B\u00e1sico` se leia como `BC!sico`, y
  # `acumular_historico()` rechazaba su propia corrida guardada.
  #
  # Medido sobre los mismos bytes sin marca, escribiendo bajo `C` y leyendo bajo
  # UTF-8:
  #
  #   version 3 -> cabecera con ANSI_X3.4-1968, 1 aviso de traduccion
  #   version 2 -> sin campo de codificacion nativa, 0 avisos, bytes y marca
  #                identicos
  #
  # La otra salida -declarar UTF-8 el texto al sellar el objeto- se probo y se
  # DESCARTO: rompe la promesa de `test-N52`, que exige que un nombre publicado
  # conserve bytes Y MARCA para poder indexar la tabla del usuario. Bajo `C`,
  # una cadena marcada y la misma sin marcar no son iguales para `==` ni para
  # `[[`. Conservar los bytes en el archivo cumple las dos; marcar cumple una y
  # rompe la otra.
  saveRDS(historico, temporal, version = 2L)
  if (!file.copy(temporal, archivo, overwrite = sobrescribir)) {
    stop("No se pudo guardar el hist\u00f3rico en el destino indicado.", call. = FALSE)
  }
  invisible(normalizePath(archivo, mustWork = TRUE))
}

#' @rdname guardar_historico
#' @export
#' @seealso [historico_calidad()], [comparar_evaluaciones()]
leer_historico <- function(archivo) {
  if (!.es_texto_escalar(archivo) || !file.exists(archivo)) {
    stop("`archivo` debe identificar un RDS existente.", call. = FALSE)
  }
  objeto <- readRDS(archivo)
  .validar_historico(objeto)
}

#' Detectar deriva en una serie de evaluaciones
#'
#' Compara corridas consecutivas, ordenadas por fecha dentro de cada perfil o
#' regla, y marca cambios significativos en la escala `[0, 1]`. Los empates de
#' fecha se resuelven por los bytes de `id_medicion`, de modo que la misma
#' secuencia persiste igual bajo cualquier locale.
#'
#' @param historico Objeto creado por [historico_calidad()].
#' @param nivel `"perfil"` o `"regla"`.
#' @param umbral Cambio absoluto mínimo considerado significativo. El valor
#'   predeterminado de `0.05` representa cinco puntos porcentuales: evita tratar
#'   como deriva diferencias de redondeo, pero sigue siendo sensible a cambios
#'   operativamente visibles.
#'
#' @return Data frame `deriva_calidad` con una fila por par de corridas
#'   consecutivas. Una mejora significativa conserva severidad `ok`; un
#'   deterioro de al menos un umbral es `sospechoso` y uno de al menos dos
#'   umbrales es `error`. `identidad_tabla` separa series de tablas distintas y
#'   `aspecto` marca el resultado o un cambio de configuracion; `cambio` usa
#'   `no_comparable` cuando una configuracion del modelo impide comparar las
#'   corridas —cambió el marco o los tipos de resultado, y entonces la fila de
#'   resultado no se publica—. Cualquier otro cambio de modelo se declara en su
#'   fila `configuracion_modelo` sin esa etiqueta: la comparación se mantiene, y
#'   la fila de resultado del mismo par conserva su veredicto.
#'
#'   **Un par que no se puede comparar no recibe veredicto.** Si alguna de las
#'   dos corridas no evaluó su resultado, `delta`, `cambio_absoluto`,
#'   `significativo`, `direccion` y `severidad` quedan en `NA` —no en `estable`
#'   ni en `ok`— y `descripcion` nombra de qué lado falta el resultado. Filtrar
#'   por `severidad != "ok"` deja fuera esas filas a propósito: no son filas
#'   sanas, son filas sin medición, y se encuentran con `is.na(significativo)`.
#'   La comparación es entre corridas **consecutivas**: una corrida sin evaluar
#'   en medio de la serie deja sin veredicto sus dos pares y la deriva no
#'   compara por encima de ella. Para comparar las corridas de los dos lados,
#'   quitar la del medio del histórico —`h[h$id_medicion != "B", ]`—.
#'   Lo mismo cuando a una de las dos corridas le falta su configuración —una
#'   tabla armada a mano, o un CSV exportado sin las columnas `identidad_tabla`
#'   y `configuracion_*`—: sin ella no se sabe si las dos miden la misma tabla
#'   con el mismo marco, así que `cambio` es `no_comparable`, el veredicto queda
#'   en `NA` y el atributo `cobertura_diagnosticos` lo declara.
#'
#'   **Un cambio de frontera tampoco se compara.** Si las dos corridas no dejan
#'   afuera las mismas partes declaradas —el nivel `parte_no_medida` del
#'   histórico: una tabla de la colección que entró o salió del número—, sus
#'   números no cubren lo mismo: la fila de resultado no se publica y una fila
#'   `aspecto = "cobertura_frontera"`, `cambio = "no_comparable"`, lo declara,
#'   con las partes sin medir de cada lado en `evidencia`. Dos corridas a las que
#'   les falta la misma parte sí se comparan.
#' @export
#'
#' @examples
#' # El ejemplo de historico_calidad() muestra cómo construir las corridas.
detectar_deriva_calidad <- function(historico, nivel = c("perfil", "regla"),
                                    umbral = 0.05) {
  historico <- .validar_historico(historico)
  nivel <- match.arg(nivel)
  if (!is.numeric(umbral) || length(umbral) != 1L || is.na(umbral) ||
      !is.finite(umbral) || umbral <= 0 || umbral > 1) {
    stop("`umbral` debe ser un n\u00famero en (0, 1].", call. = FALSE)
  }
  nombre_nivel <- paste0("evaluacion_", nivel)
  datos <- historico[historico$nivel == nombre_nivel, , drop = FALSE]
  columnas <- c(
    "nivel", "perfil", "regla", "identidad_tabla",
    "id_medicion_anterior", "fecha_anterior", "resultado_anterior",
    "id_medicion_actual", "fecha_actual", "resultado_actual", "delta",
    "cambio_absoluto", "significativo", "direccion", "severidad", "cambio",
    "aspecto",
    "descripcion", "evidencia"
  )
  vacio <- data.frame(
    nivel = character(), perfil = character(), regla = character(),
    identidad_tabla = character(),
    id_medicion_anterior = character(),
    fecha_anterior = as.POSIXct(character(), tz = "UTC"),
    resultado_anterior = numeric(), id_medicion_actual = character(),
    fecha_actual = as.POSIXct(character(), tz = "UTC"),
    resultado_actual = numeric(), delta = numeric(), cambio_absoluto = numeric(),
    significativo = logical(), direccion = character(), severidad = character(),
    cambio = character(), aspecto = character(), descripcion = character(),
    evidencia = character(),
    stringsAsFactors = FALSE
  )
  if (!nrow(datos)) {
    stop("El hist\u00f3rico no contiene evaluaciones en el nivel solicitado.",
         call. = FALSE)
  }
  configuraciones <- attr(historico, "configuracion_evaluacion", exact = TRUE)
  # La FRONTERA de cada corrida: las partes declaradas -una tabla de la
  # coleccion, una coleccion de la organizacion- que no entraron al numero. La
  # deriva no las miraba: una coleccion que medio sus tres tablas y despues dos
  # publicaba la diferencia como mejora -o como deterioro `error` al volver la
  # tercera-, con los datos de cada tabla identicos. El historico las guardaba
  # desde la ronda 24 justamente para que la serie de una coleccion incompleta
  # no se leyera como la de una completa; quien lee la serie es esta funcion.
  # Medido en la ronda 25. La clave de una parte es su frontera y su nombre, sin
  # la corrida -los campos 3 y 4 de `id_registro`, escapados, asi que `|` no
  # aparece dentro de ninguno-.
  partes_fuera <- historico[
    as.character(historico$nivel) %in% "parte_no_medida", , drop = FALSE
  ]
  campos_parte <- strsplit(as.character(partes_fuera$id_registro), "|", fixed = TRUE)
  partes_fuera$clave_parte <- vapply(campos_parte, function(campos) {
    paste(campos[3:4], collapse = "|")
  }, character(1L))
  # Sin partes, nada que pegar: `paste0()` con vectores vacios devuelve un
  # elemento, no cero.
  partes_fuera$etiqueta_parte <- if (!nrow(partes_fuera)) character() else paste0(
    vapply(campos_parte, function(campos) {
      # La clave se escapo desde bytes UTF-8 (`.clave_bytes()`): se marca asi
      # al volver, para que la etiqueta no dependa del locale de quien la lee.
      etiqueta <- utils::URLdecode(sub("^=", "", campos[[3L]]))
      if (validUTF8(etiqueta)) Encoding(etiqueta) <- "UTF-8"
      etiqueta
    }, character(1L)),
    ": ", as.character(partes_fuera$entidad)
  )
  corridas_partes <- .clave_bytes(as.character(partes_fuera$id_medicion))
  partes_de <- function(id) {
    en <- corridas_partes %in% .clave_bytes(as.character(id))
    orden <- order(partes_fuera$clave_parte[en], method = "radix")
    list(
      claves = unique(partes_fuera$clave_parte[en][orden]),
      etiquetas = unique(partes_fuera$etiqueta_parte[en][orden])
    )
  }
  clave_configuracion <- function(ids, perfiles) {
    perfiles <- as.character(perfiles)
    perfiles[is.na(perfiles)] <- "~"
    .clave_bytes(paste(
      .clave_bytes(as.character(ids)), .clave_bytes(perfiles), sep = "\034"
    ))
  }
  claves_datos <- clave_configuracion(datos$id_medicion, datos$perfil)
  claves_configuraciones <- if (nrow(configuraciones)) {
    clave_configuracion(configuraciones$id_medicion, configuraciones$perfil)
  } else character()
  indices_configuracion <- match(claves_datos, claves_configuraciones)
  identidad <- rep(NA_character_, nrow(datos))
  if (length(indices_configuracion)) {
    identidad <- configuraciones$identidad_tabla[indices_configuracion]
  }
  identidad[is.na(identidad) | !nzchar(identidad)] <- "<sin_configuracion>"
  clave <- if (nivel == "perfil") {
    .clave_bytes(as.character(datos$perfil))
  } else {
    .clave_bytes(paste(
      .clave_bytes(as.character(datos$perfil)),
      .clave_bytes(as.character(datos$regla)), sep = "\034"
    ))
  }
  clave <- .clave_bytes(paste(
    clave, .clave_bytes(as.character(identidad)), sep = "\034"
  ))
  grupos <- split(seq_len(nrow(datos)), clave, drop = TRUE)
  sin_par <- list()
  partes <- lapply(grupos, function(indices) {
    fechas <- as.numeric(datos$fecha[indices])
    orden_fecha <- .orden_seguro(fechas)
    indices <- indices[orden_fecha]
    fechas <- fechas[orden_fecha]
    grupos_fecha <- split(
      seq_along(indices), cumsum(c(TRUE, diff(fechas) != 0)), drop = TRUE
    )
    indices <- unlist(lapply(grupos_fecha, function(posiciones) {
      candidatos <- indices[posiciones]
      candidatos[.orden_seguro(
        .clave_bytes(as.character(datos$id_medicion[candidatos]))
      )]
    }), use.names = FALSE)
    if (length(indices) < 2L) {
      # Un grupo con UNA sola medicion no tiene par que comparar, y hasta aca
      # devolvia el marco vacio sin decirlo: la salida quedaba en cero filas y sin
      # ningun atributo -ni uno-, asi que para quien lee una deriva vacia porque no
      # hay con que comparar y una deriva vacia porque nada cambio son la misma
      # pantalla. Es lo que el paquete escribe en todos los otros lugares: su
      # ausencia no es conformidad.
      #
      # Se declara con el mecanismo que la capa vecina ya usa para esto:
      # `comparar_equivalencia()` publica `cobertura_diagnosticos` cuando no puede
      # comparar. Salio de leer los descartes de una refutacion, que lo habia
      # anotado como "observacion menor" con el motivo de que el objeto no traia
      # ningun motivo publicable. Eso describia el defecto, no lo excusaba.
      sin_par[[length(sin_par) + 1L]] <<- .nuevo_diagnostico_no_evaluado(
        "detectar_deriva_calidad",
        # Legible, no una clave: este campo lo lee una persona. Las claves de
        # bytes son para agrupar, no para publicar.
        #
        # Y con la tabla: la serie se agrupa por perfil Y tabla, y dos tablas
        # medidas con el mismo perfil publicaban dos diagnosticos identicos, sin
        # decir a cual le faltaba el par.
        paste0(
          if (nivel == "regla") {
            paste0(
              as.character(datos$perfil[[indices[[1L]]]]), " / ",
              as.character(datos$regla[[indices[[1L]]]])
            )
          } else {
            as.character(datos$perfil[[indices[[1L]]]])
          },
          if (!identical(identidad[[indices[[1L]]]], "<sin_configuracion>")) {
            paste0(" [", identidad[[indices[[1L]]]], "]")
          } else ""
        ),
        "no_comparable: una sola medicion en la serie",
        paste0(
          "Acumular al menos dos mediciones del mismo perfil -y de la misma tabla- ",
          "para que haya un par que comparar."
        )
      )
      return(vacio)
    }
    a <- indices[-length(indices)]
    b <- indices[-1L]
    delta <- datos$resultado[b] - datos$resultado[a]
    # El umbral se compara CON TOLERANCIA, como ya lo hace `tablero-calidad.R`
    # con los pesos. Sin ella, la resta en coma flotante decide el veredicto:
    # 0,70 - 0,65 da 0,049999999999999933 y 0,75 - 0,70 da 0,050000000000000044,
    # asi que dos pares que publican el mismo `delta = 0.05` recibian "estable" y
    # "mejora". Que la misma operacion se compare con tolerancia en un archivo y
    # sin ella en otro no es una decision: es una inconsistencia.
    # UNA regla para las cuatro columnas que publican el veredicto. Estaban
    # escritas por separado y dejaron de compartir semantica: `significativo`
    # se arreglo con la tolerancia relativa y `direccion`, `severidad` y
    # `descripcion` quedaron con los cortes viejos, asi que una misma fila
    # publicaba significativo = TRUE, direccion "mejora", severidad "error" y
    # "el resultado se mantuvo" sobre un DETERIORO diminuto.
    #
    # Dos decisiones, y las dos costaron una vuelta cada una:
    #   - la direccion sale del SIGNO, no de comparar el delta firmado contra
    #     `umbral - tolerancia`: con un umbral chico ese corte es negativo y un
    #     delta negativo entraba por "mejora";
    #   - la tolerancia escala con las MAGNITUDES en juego, no con un piso de 1.
    #     El error de `b - a` es del orden de `eps * max(|a|, |b|)`; un piso fijo
    #     se traga cambios reales cuando el umbral es diminuto.
    alcanza <- function(corte) {
      # `pmax`, no `max`: `delta` es un VECTOR -una fila por par- y `max()`
      # colapsa el grupo entero. La tolerancia de cada fila salia del delta
      # mas grande del grupo, y un solo delta `NA` -una medicion sin
      # evaluar- dejaba el veredicto de TODAS las filas del grupo en `NA`:
      # un deterioro de 0,9 a 0,7, cuatro veces el umbral, se publicaba con
      # las cuatro columnas vacias. Una fila se decide con su propio delta,
      # como ya lo hacia `.dentro_tolerancia_aritmetica()` con `pmax`.
      tolerancia <- sqrt(.Machine$double.eps) *
        pmax(abs(delta), abs(corte), na.rm = TRUE)
      is.finite(delta) & delta != 0 & abs(delta) >= corte - tolerancia
    }
    # Un par donde uno de los dos resultados no se evaluo NO es un par estable:
    # es un par que no se puede comparar. Publicaba `significativo = FALSE`,
    # `direccion = "estable"` y `severidad = "ok"` -tres afirmaciones de salud-
    # al lado de su propia `descripcion`, que decia "No se puede comparar: el
    # resultado anterior no se evaluo". El usuario que filtra `severidad != "ok"`
    # para encontrar problemas no veia esas filas: lo no medido quedaba contado
    # entre lo sano. La convencion del paquete para lo desconocido ya estaba
    # escrita -"los conteos desconocidos son NA, nunca cero"- y es la que se
    # aplica: sin comparacion no hay veredicto, y `NA` lo dice.
    comparable <- is.finite(delta)
    significativo <- ifelse(comparable, alcanza(umbral), NA)
    # Las dos columnas que siguen heredan el `NA` solas: `ifelse()` lo propaga.
    direccion <- ifelse(
      !significativo, "estable", ifelse(delta > 0, "mejora", "deterioro")
    )
    severidad <- ifelse(
      !significativo | delta > 0, "ok",
      ifelse(alcanza(2 * umbral), "error", "sospechoso")
    )
    regular <- data.frame(
      nivel = rep(nivel, length(a)), perfil = datos$perfil[a],
      regla = if (nivel == "regla") datos$regla[a] else NA_character_,
      identidad_tabla = identidad[a],
      id_medicion_anterior = datos$id_medicion[a], fecha_anterior = datos$fecha[a],
      resultado_anterior = datos$resultado[a],
      id_medicion_actual = datos$id_medicion[b], fecha_actual = datos$fecha[b],
      resultado_actual = datos$resultado[b], delta = delta,
      cambio_absoluto = abs(delta), significativo = significativo,
      direccion = direccion, severidad = severidad, cambio = NA_character_,
      aspecto = "resultado",
      # La fila existe por cada par consecutivo, cambie o no, asi que el texto
      # no puede afirmar un cambio: con dos corridas identicas decia "Cambio el
      # resultado" al lado de `delta = 0`, y una descripcion que contradice a su
      # propio dato es peor que no tenerla.
      #
      # Dos arreglos de la misma linea. `isTRUE(delta == 0)` trataba como escalar
      # un vector con un elemento POR PAR: con mas de un par devuelve `FALSE`
      # siempre -incluso con todos los deltas en cero-, asi que la guarda que este
      # comentario describe no corria nunca. Se vectoriza.
      #
      # Y `delta = NA` -el resultado anterior no se evaluo por cobertura- no es
      # ni "cambio" ni "se mantuvo": el cero dice "no cambio" y el NA dice "no
      # se". Afirmar un cambio ahi es exactamente lo que el parrafo de arriba
      # prohibe, un caso mas adentro.
      # Y decir "no se" no alcanza: hay que decir de QUE LADO. La version
      # anterior culpaba siempre al resultado anterior, asi que cuando el NA
      # venia de la corrida ACTUAL la fila se contradecia a si misma -publicaba
      # `resultado_anterior = 0,8` al lado de "el resultado anterior no se
      # evaluo"-. Paso los controles porque la deriva ordena por fecha y el
      # espejo del par si salia bien.
      descripcion = ifelse(
        is.na(datos$resultado[a]) & is.na(datos$resultado[b]),
        "No se puede comparar: ninguna de las dos corridas se evalu\u00f3.",
        ifelse(
          is.na(datos$resultado[a]),
          "No se puede comparar: el resultado anterior no se evalu\u00f3.",
          ifelse(
            is.na(datos$resultado[b]),
            "No se puede comparar: el resultado actual no se evalu\u00f3.",
            ifelse(
              # La misma regla que decide el veredicto, no un piso absoluto
              # aparte: con un piso propio la descripcion decia "se mantuvo"
              # sobre una fila que publicaba un cambio significativo.
              significativo,
              "Cambi\u00f3 el resultado de la evaluaci\u00f3n.",
              "El resultado de la evaluaci\u00f3n se mantuvo."
            )
          )
        )
      ),
      evidencia = NA_character_,
      stringsAsFactors = FALSE
    )
    # Un par al que le falta la configuracion de una corrida no se puede
    # comparar: sin ella no se sabe si las dos corridas miden la misma tabla, ni
    # si cambio el marco. La deriva lo SABIA -la columna decia
    # `<sin_configuracion>`- y publicaba igual: sobre un historico releido de un
    # CSV restaba la corrida de una tabla a la de otra y publicaba `error`, y un
    # cambio de marco que el objeto declaraba no comparable salia como deterioro.
    # Medido en la ronda 25. La fila queda, sin veredicto, y la cobertura lo dice.
    sin_configuracion <- is.na(indices_configuracion[a]) |
      is.na(indices_configuracion[b])
    if (any(sin_configuracion)) {
      for (columna in c("delta", "cambio_absoluto")) {
        regular[[columna]][sin_configuracion] <- NA_real_
      }
      regular$significativo[sin_configuracion] <- NA
      regular$direccion[sin_configuracion] <- NA_character_
      regular$severidad[sin_configuracion] <- NA_character_
      regular$cambio[sin_configuracion] <- "no_comparable"
      regular$descripcion[sin_configuracion] <- paste(
        "No se puede comparar: el hist\u00f3rico no declara la configuraci\u00f3n",
        "de", ifelse(
          is.na(indices_configuracion[a][sin_configuracion]) &
            is.na(indices_configuracion[b][sin_configuracion]),
          "ninguna de las dos corridas",
          ifelse(
            is.na(indices_configuracion[a][sin_configuracion]),
            "la corrida anterior", "la corrida actual"
          )
        ),
        "(tabla, marco y tipos de resultado), as\u00ed que no se sabe si miden lo mismo."
      )
      sin_par[[length(sin_par) + 1L]] <<- .nuevo_diagnostico_no_evaluado(
        "detectar_deriva_calidad",
        if (nivel == "regla") {
          paste0(
            as.character(datos$perfil[[indices[[1L]]]]), " / ",
            as.character(datos$regla[[indices[[1L]]]])
          )
        } else {
          as.character(datos$perfil[[indices[[1L]]]])
        },
        paste0(
          "no_comparable: ", sum(sin_configuracion), " par(es) sin la ",
          "configuraci\u00f3n de sus corridas"
        ),
        paste0(
          "Acumular las corridas desde los objetos de evaluar() o medir(), o ",
          "leer un hist\u00f3rico que conserve las columnas identidad_tabla y ",
          "configuracion_* de sus filas evaluacion_perfil."
        )
      )
    }
    # Un par cuya frontera cambio -una parte declarada entro o salio del
    # numero- no compara lo mismo: se declara como el cambio de marco, sin la
    # fila de resultado.
    partes_a <- lapply(datos$id_medicion[a], partes_de)
    partes_b <- lapply(datos$id_medicion[b], partes_de)
    frontera_cambiada <- !vapply(seq_along(a), function(i) {
      identical(partes_a[[i]]$claves, partes_b[[i]]$claves)
    }, logical(1L))
    declaracion_frontera <- NULL
    if (any(frontera_cambiada)) {
      i <- which(frontera_cambiada)
      describir <- function(partes) {
        vapply(partes, function(p) {
          if (length(p$etiquetas)) {
            paste0("sin medir ", paste(p$etiquetas, collapse = "; "))
          } else {
            "todas las partes medidas"
          }
        }, character(1L))
      }
      declaracion_frontera <- data.frame(
        nivel = rep(nivel, length(i)), perfil = datos$perfil[a[i]],
        regla = if (nivel == "regla") datos$regla[a[i]] else NA_character_,
        identidad_tabla = identidad[a[i]],
        id_medicion_anterior = datos$id_medicion[a[i]],
        fecha_anterior = datos$fecha[a[i]], resultado_anterior = NA_real_,
        id_medicion_actual = datos$id_medicion[b[i]],
        fecha_actual = datos$fecha[b[i]], resultado_actual = NA_real_,
        delta = NA_real_, cambio_absoluto = NA_real_, significativo = TRUE,
        direccion = "cobertura", severidad = "error", cambio = "no_comparable",
        aspecto = "cobertura_frontera",
        descripcion = paste(
          "Cambi\u00f3 la parte medida de la frontera: el n\u00famero de una",
          "corrida no cubre lo mismo que el de la otra; no se publica la",
          "comparaci\u00f3n del resultado."
        ),
        evidencia = paste0(
          "Anterior: ", describir(partes_a[i]), "; actual: ",
          describir(partes_b[i]), "."
        ),
        stringsAsFactors = FALSE
      )
    }
    if (!nrow(configuraciones)) {
      return(rbind(
        regular[!frontera_cambiada, , drop = FALSE], declaracion_frontera
      ))
    }
    anterior_configuracion <- indices_configuracion[a]
    actual_configuracion <- indices_configuracion[b]
    anterior_configuracion[sin_configuracion] <- NA_integer_
    actual_configuracion[sin_configuracion] <- NA_integer_
    diferencias_componente <- function(campo) {
      anterior <- rep(NA_character_, length(a))
      actual <- rep(NA_character_, length(b))
      validos_a <- !is.na(anterior_configuracion)
      validos_b <- !is.na(actual_configuracion)
      anterior[validos_a] <- configuraciones[[campo]][
        anterior_configuracion[validos_a]
      ]
      actual[validos_b] <- configuraciones[[campo]][
        actual_configuracion[validos_b]
      ]
      distintos <- (is.na(anterior) & !is.na(actual)) |
        (!is.na(anterior) & is.na(actual)) |
        (!is.na(anterior) & !is.na(actual) & anterior != actual)
      distintos[is.na(distintos)] <- FALSE
      distintos
    }
    marco_cambiado <- diferencias_componente("configuracion_marco")
    tipos_cambiados <- diferencias_componente("configuracion_tipos_resultado")
    modelo_no_comparable <- marco_cambiado | tipos_cambiados
    if (any(modelo_no_comparable | frontera_cambiada)) {
      regular <- regular[!(modelo_no_comparable | frontera_cambiada), , drop = FALSE]
    }
    regular <- rbind(regular, declaracion_frontera)
    campos <- c(
      modelo = "configuracion_modelo",
      aplicabilidad = "configuracion_aplicabilidad",
      perfil = "configuracion_perfil"
    )
    cambios_configuracion <- lapply(names(campos), function(nombre) {
      campo <- unname(campos[[nombre]])
      anterior <- rep(NA_character_, length(a))
      actual <- rep(NA_character_, length(b))
      validos_a <- !is.na(anterior_configuracion)
      validos_b <- !is.na(actual_configuracion)
      anterior[validos_a] <- configuraciones[[campo]][anterior_configuracion[validos_a]]
      actual[validos_b] <- configuraciones[[campo]][actual_configuracion[validos_b]]
      distintos <- (is.na(anterior) & !is.na(actual)) |
        (!is.na(anterior) & is.na(actual)) |
        (!is.na(anterior) & !is.na(actual) & anterior != actual)
      if (!any(distintos)) return(NULL)
      i <- which(distintos)
      data.frame(
        nivel = rep(nivel, length(i)), perfil = datos$perfil[a[i]],
        regla = if (nivel == "regla") datos$regla[a[i]] else NA_character_,
        identidad_tabla = identidad[a[i]],
        id_medicion_anterior = datos$id_medicion[a[i]],
        fecha_anterior = datos$fecha[a[i]], resultado_anterior = NA_real_,
        id_medicion_actual = datos$id_medicion[b[i]],
        fecha_actual = datos$fecha[b[i]], resultado_actual = NA_real_,
        delta = NA_real_, cambio_absoluto = NA_real_, significativo = TRUE,
        direccion = "configuracion", severidad = "error",
        # `no_comparable` solo cuando la fila de resultado se quita -cambio el
        # marco o los tipos-. Un cambio de modelo que mantiene la comparacion
        # salia `no_comparable` al lado de su propia fila de resultado con
        # veredicto y de una descripcion que dice "se mantienen las
        # comparaciones"; y `reportar()`, que confia en la etiqueta, borraba el
        # delta que la deriva publicaba. Medido en la ronda 25.
        cambio = if (nombre == "modelo") {
          ifelse(marco_cambiado[i] | tipos_cambiados[i], "no_comparable",
                 NA_character_)
        } else {
          NA_character_
        },
        aspecto = paste0("configuracion_", nombre),
        descripcion = paste(
          switch(
            nombre,
            # Los tipos se guardan como un solo vector, sin el nombre de la
            # metrica: cambian igual si una metrica cambio de tipo que si el
            # modelo gano o perdio una. Decia "cambio el tipo_resultado" sobre
            # un modelo de dos metricas booleanas contra uno de una.
            # `i` puede traer varios pares: todo lo que decide el texto va
            # vectorizado. Con `if` sobre el vector, dos pares a la vez
            # abortaban la deriva entera.
            modelo = ifelse(
              marco_cambiado[i] & tipos_cambiados[i],
              paste(
                "Cambio el marco y el tipo_resultado de una metrica o el",
                "conjunto de metricas del modelo;"
              ),
              ifelse(
                marco_cambiado[i], "Cambio el marco de calidad de la corrida;",
                ifelse(
                  tipos_cambiados[i],
                  paste(
                    "Cambio el tipo_resultado de una metrica o el conjunto de",
                    "metricas del modelo;"
                  ),
                  "Cambio el modelo de calidad de la corrida;"
                )
              )
            ),
            aplicabilidad = "Cambio la aplicabilidad de la corrida;",
            perfil = "Cambio el perfil de evaluacion de la corrida;"
          ),
          ifelse(
            marco_cambiado[i] | tipos_cambiados[i],
            if (nombre == "modelo") {
              "no se publica la comparacion del resultado."
            } else {
              "la comparacion no se publica porque tambien cambio el modelo."
            },
            paste(
              "se mantienen las comparaciones para que la deriva de datos no",
              "quede oculta."
            )
          )
        ),
        evidencia = paste0(
          "Anterior: ", ifelse(is.na(anterior[i]), "no declarada", anterior[i]),
          "; actual: ", ifelse(is.na(actual[i]), "no declarada", actual[i]), "."
        ), stringsAsFactors = FALSE
      )
    })
    cambios_configuracion <- cambios_configuracion[
      !vapply(cambios_configuracion, is.null, logical(1L))
    ]
    if (length(cambios_configuracion)) {
      return(rbind(regular, do.call(rbind, cambios_configuracion)))
    }
    regular
  })
  resultado <- do.call(rbind, unname(partes))
  resultado <- resultado[, columnas, drop = FALSE]
  resultado$severidad <- factor(
    resultado$severidad, levels = c("ok", "sospechoso", "error"),
    ordered = TRUE
  )
  rownames(resultado) <- NULL
  attr(resultado, "cobertura_diagnosticos") <- if (length(sin_par)) {
    do.call(rbind, sin_par)
  } else {
    .cobertura_diagnosticos_vacia()
  }
  class(resultado) <- c("deriva_calidad", "data.frame")
  resultado
}

# Lo que la deriva dice de UN par de corridas, leido igual por todos los que la
# consumen -la evolucion de `reportar()` y `comparar_evaluaciones()`-: el delta,
# si la deriva lo publica, y el texto de todas sus filas, la de resultado
# primero. Un par con alguna fila `no_comparable` no tiene delta. Leerlo en dos
# lugares con dos reglas es como el informe termino borrando un delta que la
# deriva publicaba.
#
# Y cuando la deriva NO arma el par -las dos corridas miden tablas distintas, o la
# serie no las junta-, la lectura dice por que si quien llama nombra las dos
# corridas (`ids`, la anterior primero) y su perfil. `comparar_evaluaciones()` lo
# decia y la evolucion del informe dejaba la celda vacia: la serie de una tabla y
# la de otra se leian, bajo el mismo perfil, como una mejora. Ronda 26.
.lectura_par_deriva <- function(deriva, filas, configuracion = NULL,
                                perfil = NULL, ids = NULL) {
  if (!length(filas)) {
    return(list(
      delta = NA_real_,
      comparacion = if (length(ids) == 2L) {
        .motivo_sin_par_deriva(configuracion, perfil, ids[[1L]], ids[[2L]])
      } else NA_character_
    ))
  }
  resultado <- filas[as.character(deriva$aspecto[filas]) == "resultado"]
  no_comparable <- any(deriva$cambio[filas] %in% "no_comparable")
  orden <- filas[order(as.character(deriva$aspecto[filas]) != "resultado")]
  list(
    delta = if (!no_comparable && length(resultado)) {
      deriva$delta[[resultado[[1L]]]]
    } else {
      NA_real_
    },
    comparacion = paste(
      unique(as.character(deriva$descripcion[orden])), collapse = " "
    )
  )
}

# Por que la deriva no compara dos corridas de un perfil: la tabla de cada una
# sale de la configuracion del historico, que es con lo que la deriva separa las
# series.
.motivo_sin_par_deriva <- function(configuracion, perfil, id_anterior, id_actual) {
  clave <- function(x) .clave_bytes(as.character(x))
  identidad_de <- function(id) {
    if (!inherits(configuracion, "data.frame") || !nrow(configuracion) ||
        !all(c("id_medicion", "perfil", "identidad_tabla") %in%
               names(configuracion))) {
      return(NA_character_)
    }
    i <- which(clave(configuracion$id_medicion) == clave(id) &
                 clave(configuracion$perfil) == clave(perfil))
    if (length(i)) as.character(configuracion$identidad_tabla[[i[[1L]]]]) else NA_character_
  }
  tabla_anterior <- identidad_de(id_anterior)
  tabla_actual <- identidad_de(id_actual)
  if (!identical(tabla_anterior, tabla_actual)) {
    paste0(
      "No se puede comparar: las dos corridas no miden la misma tabla (",
      tabla_anterior, " contra ", tabla_actual, ")."
    )
  } else {
    "No se puede comparar: la deriva no arma un par con estas dos corridas."
  }
}
