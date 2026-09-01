.UMBRAL_FILAS_DATA_TABLE_DUPLICADOS <- 25L
.UMBRAL_CELDAS_AVISO_TABLA_ANCHA <- 100000L
.VELOCIDAD_REFERENCIA_TABLA_ANCHA <- 10000
.MAX_CELDAS_MUESTRA <- 1000000L
.MAX_BYTES_MUESTRA <- 512 * 1024^2

.proyectar_costo_tabla_ancha <- function(
    n_filas, n_columnas, umbral_celdas = .UMBRAL_CELDAS_AVISO_TABLA_ANCHA) {
  celdas <- as.numeric(n_filas) * as.numeric(n_columnas)
  list(
    celdas = celdas,
    duracion_estimada_segundos = celdas / .VELOCIDAD_REFERENCIA_TABLA_ANCHA,
    umbral_celdas = umbral_celdas,
    fuente = paste0(
      "mediciones de lupa con 500 filas y 50, 300 y 1.000 columnas; ",
      "referencia conservadora de ", .VELOCIDAD_REFERENCIA_TABLA_ANCHA,
      " celdas por segundo"
    )
  )
}

.avisar_costo_tabla_ancha <- function(
    proyeccion, habilitado = TRUE, interactiva = interactive()) {
  if (!isTRUE(habilitado) || !isTRUE(interactiva) ||
      is.null(proyeccion) || !is.finite(proyeccion$celdas) ||
      proyeccion$celdas < proyeccion$umbral_celdas) {
    return(invisible(NULL))
  }
  segundos <- formatC(
    proyeccion$duracion_estimada_segundos, format = "f", digits = 1,
    decimal.mark = ","
  )
  celdas <- format(
    round(proyeccion$celdas), big.mark = ".", decimal.mark = ",",
    scientific = FALSE, trim = TRUE
  )
  cli::cli_alert_warning(paste0(
    "Costo estimado del perfilado de tabla ancha: ~", segundos,
    " s para ", celdas, " celdas. Fuente: ", proyeccion$fuente,
    ". Es una estimacion, no una medicion."
  ))
  invisible(NULL)
}

.muestra_tabla_datos <- function(datos, filas) {
  indices <- if (!length(filas) || !is.finite(filas) || filas >= nrow(datos)) {
    seq_len(nrow(datos))
  } else if (filas == 0) {
    integer()
  } else {
    .muestrear_vector(seq_len(nrow(datos)), filas)$valores
  }
  .seleccionar_columnas(datos, seq_len(ncol(datos)), filas = indices)
}

.resolver_muestra_perfilado <- function(datos, muestra,
                                        max_celdas_muestra,
                                        max_bytes_muestra) {
  muestra <- .validar_muestra(muestra)
  max_celdas_muestra <- .validar_limite_duplicados(
    max_celdas_muestra, "max_celdas_muestra"
  )
  max_bytes_muestra <- .validar_limite_duplicados(
    max_bytes_muestra, "max_bytes_muestra"
  )

  filas_totales <- as.numeric(nrow(datos))
  columnas_totales <- as.numeric(ncol(datos))
  filas_solicitadas <- min(filas_totales, as.numeric(muestra))
  celdas_solicitadas <- filas_solicitadas * columnas_totales
  filas_por_celdas <- if (!columnas_totales) {
    Inf
  } else {
    floor(as.numeric(max_celdas_muestra) / columnas_totales)
  }
  if (filas_totales > 0 && filas_por_celdas < 1) {
    stop(
      "`max_celdas_muestra` debe permitir al menos una fila para todas las columnas.",
      call. = FALSE
    )
  }
  filas_candidatas <- min(filas_solicitadas, filas_por_celdas)

  bytes_sonda <- NA_real_
  bytes_vacios <- NA_real_
  filas_por_bytes <- Inf
  if (is.finite(max_bytes_muestra) && filas_solicitadas > 0) {
    datos_vacios <- .muestra_tabla_datos(datos, 0L)
    bytes_vacios <- as.numeric(utils::object.size(datos_vacios))
    if (bytes_vacios > max_bytes_muestra) {
      stop(
        paste0(
          "`max_bytes_muestra` es menor que el tamano minimo de la muestra ",
          "vacia (", bytes_vacios, " bytes)."
        ),
        call. = FALSE
      )
    }
    filas_sonda <- min(filas_solicitadas, 100)
    datos_sonda <- .muestra_tabla_datos(datos, filas_sonda)
    bytes_sonda <- as.numeric(utils::object.size(datos_sonda))
    bytes_por_fila <- max(
      (bytes_sonda - bytes_vacios) / filas_sonda,
      bytes_sonda / filas_sonda,
      1
    )
    filas_por_bytes <- floor(
      max(0, as.numeric(max_bytes_muestra) - bytes_vacios) / bytes_por_fila
    )
    filas_candidatas <- min(filas_candidatas, filas_por_bytes)
  }

  if (filas_totales > 0 && filas_candidatas < 1) {
    stop(
      "`max_bytes_muestra` no permite materializar una fila de la muestra.",
      call. = FALSE
    )
  }

  # La estimación de bytes sólo elige el rango; esta búsqueda comprueba el
  # objeto materializado y evita que el redondeo de la sonda viole el tope.
  bytes_muestra <- if (!filas_totales) {
    as.numeric(utils::object.size(.muestra_tabla_datos(datos, 0L)))
  } else if (is.infinite(max_bytes_muestra)) {
    NA_real_
  } else {
    lo <- 0
    hi <- as.integer(floor(filas_candidatas))
    valido <- 0L
    while (lo <= hi) {
      medio <- floor((lo + hi) / 2)
      bytes_medio <- as.numeric(utils::object.size(
        .muestra_tabla_datos(datos, medio)
      ))
      if (is.infinite(max_bytes_muestra) || bytes_medio <= max_bytes_muestra) {
        valido <- medio
        lo <- medio + 1L
      } else {
        hi <- medio - 1L
      }
    }
    filas_candidatas <- valido
    as.numeric(utils::object.size(.muestra_tabla_datos(datos, filas_candidatas)))
  }
  if (filas_totales > 0 && filas_candidatas < 1) {
    stop(
      "`max_bytes_muestra` no permite materializar una fila de la muestra.",
      call. = FALSE
    )
  }

  filas_efectivas <- as.numeric(filas_candidatas)
  celdas_efectivas <- filas_efectivas * columnas_totales
  recortada <- filas_efectivas < filas_solicitadas
  list(
    filas_solicitadas = filas_solicitadas,
    filas_efectivas = filas_efectivas,
    celdas_solicitadas = celdas_solicitadas,
    celdas_efectivas = celdas_efectivas,
    bytes_muestra = bytes_muestra,
    bytes_sonda = bytes_sonda,
    filas_por_celdas = filas_por_celdas,
    filas_por_bytes = filas_por_bytes,
    max_celdas_muestra = max_celdas_muestra,
    max_bytes_muestra = max_bytes_muestra,
    recortada = recortada,
    motivos = .motivos_muestra_perfilado(list(
      filas_solicitadas = filas_solicitadas,
      filas_efectivas = filas_efectivas,
      celdas_solicitadas = celdas_solicitadas,
      bytes_muestra = bytes_muestra,
      bytes_sonda = bytes_sonda,
      filas_por_celdas = filas_por_celdas,
      filas_por_bytes = filas_por_bytes,
      max_celdas_muestra = max_celdas_muestra,
      max_bytes_muestra = max_bytes_muestra
    ))
  )
}

.topes_muestra_perfilado <- function(alcance) {
  topes_filas <- c(
    celdas = if (is.finite(alcance$max_celdas_muestra)) {
      alcance$filas_por_celdas
    } else Inf,
    bytes = if (is.finite(alcance$max_bytes_muestra)) {
      alcance$filas_por_bytes
    } else Inf
  )
  topes_filas[is.finite(topes_filas)]
}

.topes_mandaron_muestra_perfilado <- function(alcance) {
  topes_filas <- .topes_muestra_perfilado(alcance)
  minimo_tope <- if (length(topes_filas)) {
    min(topes_filas, alcance$filas_solicitadas)
  } else {
    alcance$filas_solicitadas
  }
  topes_mandaron <- names(topes_filas)[
    topes_filas == minimo_tope & minimo_tope < alcance$filas_solicitadas
  ]
}

.tope_que_mando <- function(alcance) {
  mandaron <- .topes_mandaron_muestra_perfilado(alcance)
  if (length(mandaron)) mandaron[[1L]] else "muestra"
}

.motivos_muestra_perfilado <- function(alcance) {
  topes_mandaron <- .topes_mandaron_muestra_perfilado(alcance)
  motivos <- character()
  if (is.finite(alcance$max_celdas_muestra) &&
      alcance$filas_efectivas < alcance$filas_solicitadas) {
    motivos <- c(motivos, paste0(
      "celdas solicitadas = ", alcance$celdas_solicitadas,
      "; umbral = ", alcance$max_celdas_muestra,
      if ("celdas" %in% topes_mandaron) "; manda el tope de celdas" else ""
    ))
  }
  if (is.finite(alcance$max_bytes_muestra) &&
      alcance$filas_efectivas < alcance$filas_solicitadas) {
    motivos <- c(motivos, paste0(
      "bytes observados = ", alcance$bytes_muestra,
      "; umbral = ", alcance$max_bytes_muestra,
      if (is.finite(alcance$bytes_sonda)) {
        paste0("; bytes de sonda = ", alcance$bytes_sonda)
      } else "",
      if ("bytes" %in% topes_mandaron) "; manda el tope de bytes" else ""
    ))
  }
  motivos
}

.cobertura_muestra_perfilado <- function(alcance) {
  if (!isTRUE(alcance$recortada)) {
    return(.cobertura_diagnosticos_vacia())
  }
  detalles <- paste0(
    "La muestra se redujo de ", alcance$filas_solicitadas, " a ",
    alcance$filas_efectivas, " filas"
  )
  if (is.finite(alcance$max_celdas_muestra)) {
    detalles <- paste0(
      detalles, "; celdas observadas: ", alcance$celdas_solicitadas,
      " (umbral ", alcance$max_celdas_muestra, ")"
    )
  }
  if (is.finite(alcance$max_bytes_muestra)) {
    detalles <- paste0(
      detalles, "; bytes observados en la muestra efectiva: ",
      alcance$bytes_muestra, " (umbral ", alcance$max_bytes_muestra, ")"
    )
  }
  motivo <- paste0(
    detalles, ". Motivos: ",
    if (length(alcance$motivos)) paste(alcance$motivos, collapse = "; ") else
      "el tope efectivo de la muestra"
  )
  data.frame(
    diagnostico = "muestra_perfilado", columna = "", motivo = motivo,
    como_resolverlo = paste0(
      "Aumente `max_celdas_muestra` o `max_bytes_muestra`, o use `Inf` ",
      "si acepta analizar la muestra solicitada completa."
    ),
    dependencia = NA_character_, stringsAsFactors = FALSE
  )
}

.filas_duplicadas_base <- function(datos) {
  datos_base <- if (inherits(datos, "data.table")) {
    as.data.frame(datos, stringsAsFactors = FALSE)
  } else {
    datos
  }
  columnas_matriciales <- vapply(
    datos_base,
    function(columna) is.matrix(columna) ||
      (is.array(columna) && length(dim(columna)) > 1L),
    logical(1L)
  )
  if (any(columnas_matriciales)) {
    columnas <- list()
    for (i in seq_along(datos_base)) {
      columna <- datos_base[[i]]
      if (columnas_matriciales[[i]]) {
        componentes <- as.data.frame(
          unclass(columna), stringsAsFactors = FALSE
        )
        for (j in seq_along(componentes)) {
          columnas[[length(columnas) + 1L]] <- componentes[[j]]
        }
      } else {
        columnas[[length(columnas) + 1L]] <- columna
      }
    }
    datos_base <- as.data.frame(
      columnas, check.names = FALSE, stringsAsFactors = FALSE
    )
  }

  duplicadas_adelante <- tryCatch(
    base::duplicated.data.frame(datos_base),
    error = function(e) NULL
  )
  # `fromLast` se calcula dando vuelta las filas y no pidiendoselo al metodo,
  # porque hay metodos de `duplicated()` que lo ignoran: `bit64` devuelve para
  # `integer64` el mismo vector con `fromLast = TRUE` que sin el. Con eso, una
  # tabla de 30 filas y 3 valores distintos daba 27 filas en grupos repetidos en
  # vez de 30, y la respuesta cambiaba segun el umbral de filas, porque la via
  # rapida si daba 30. Dar vuelta las filas no depende de que el metodo respete
  # el argumento.
  duplicadas_atras <- if (is.null(duplicadas_adelante)) {
    NULL
  } else {
    tryCatch(
      rev(base::duplicated.data.frame(
        datos_base[rev(seq_len(nrow(datos_base))), , drop = FALSE]
      )),
      error = function(e) NULL
    )
  }
  if (is.null(duplicadas_adelante) || is.null(duplicadas_atras) ||
      length(duplicadas_adelante) != nrow(datos_base) ||
      length(duplicadas_atras) != nrow(datos_base)) {
    return(NULL)
  }
  list(adelante = duplicadas_adelante, atras = duplicadas_atras)
}

.resumir_filas_duplicadas <- function(duplicadas, n_filas) {
  pudo_contar_adelante <- !is.null(duplicadas) &&
    length(duplicadas$adelante) == n_filas
  pudo_contar_atras <- pudo_contar_adelante &&
    length(duplicadas$atras) == n_filas
  n_filas_duplicadas <- if (pudo_contar_adelante) {
    tryCatch(sum(duplicadas$adelante), error = function(e) NA_integer_)
  } else {
    NA_integer_
  }
  n_filas_en_grupos_duplicados <- if (pudo_contar_atras) {
    tryCatch(
      sum(duplicadas$adelante | duplicadas$atras),
      error = function(e) NA_integer_
    )
  } else {
    NA_integer_
  }
  list(
    filas_duplicadas = n_filas_duplicadas,
    filas_en_grupos_duplicados = n_filas_en_grupos_duplicados
  )
}

.tiene_metodo_duplicated_personalizado <- function(datos) {
  clases <- setdiff(class(datos), c("data.frame", "data.table", "tbl_df", "tbl"))
  if (!length(clases)) return(FALSE)
  any(vapply(clases, function(clase) {
    !is.null(utils::getS3method("duplicated", clase, optional = TRUE))
  }, logical(1L)))
}

.conteos_filas_duplicadas_imprevistos <- function(datos) {
  duplicadas_adelante <- tryCatch(
    duplicated(datos), error = function(e) NULL
  )
  duplicadas_atras <- if (is.null(duplicadas_adelante)) {
    NULL
  } else {
    tryCatch(
      duplicated(datos, fromLast = TRUE), error = function(e) NULL
    )
  }
  .resumir_filas_duplicadas(
    list(adelante = duplicadas_adelante, atras = duplicadas_atras),
    nrow(datos)
  )
}

.conteos_filas_duplicadas <- function(datos) {
  columnas_no_compatibles <- vapply(
    datos,
    function(columna) is.list(columna) || is.matrix(columna) ||
      (is.array(columna) && length(dim(columna)) > 1L),
    logical(1L)
  )
  if (.tiene_metodo_duplicated_personalizado(datos)) {
    return(.conteos_filas_duplicadas_imprevistos(datos))
  }
  usa_via_rapida <- nrow(datos) >= .UMBRAL_FILAS_DATA_TABLE_DUPLICADOS &&
    !any(columnas_no_compatibles) && !.tiene_nan_en_dobles(datos) &&
    .redondeo_numerico_es_exacto(datos)
  # Un fallo imprevisto de la via rapida repliega a base en vez de abortar:
  # el resultado exacto lo fija `duplicated()`, no el atajo.
  duplicadas <- if (usa_via_rapida) {
    tryCatch(.filas_duplicadas_frank(datos), error = function(e) NULL)
  } else {
    NULL
  }
  if (is.null(duplicadas)) duplicadas <- .filas_duplicadas_base(datos)
  .resumir_filas_duplicadas(duplicadas, nrow(datos))
}

# `duplicated()` de un `data.frame` arma una estructura intermedia que combina
# todas las columnas -desde R 4.0 con `Map()`, antes pegando cadenas-, de modo
# que su costo crece con el ancho de la tabla. `frank()` con rangos densos da el
# mismo agrupamiento ordenando las columnas directamente.
#
# Se llama a `data.table` con `::` y NO se lo importa al NAMESPACE a proposito.
# `data.table` decide la semantica de `[` segun si el paquete que llama lo tiene
# entre sus imports (lo que su documentacion llama `cedta`): con el import
# puesto, `tabla[, columnas, drop = FALSE]` deja de seleccionar columnas en toda
# funcion de `lupa` que recibe una tabla del usuario. `frank()` y `uniqueN()` no
# consultan `cedta`, asi que dan su velocidad sin ese efecto; `duplicated()`
# sobre un `data.table` si la consulta, y por eso no se usa aca.
.filas_duplicadas_frank <- function(datos) {
  ids <- tryCatch(
    data.table::frank(datos, ties.method = "dense", na.last = TRUE),
    error = function(e) NULL
  )
  if (is.null(ids) || length(ids) != nrow(datos) || anyNA(ids)) return(NULL)
  list(
    adelante = duplicated(ids),
    atras = duplicated(ids, fromLast = TRUE)
  )
}

# `data.table` tiene un ajuste GLOBAL DE LA SESION, `setNumericRounding()`, que
# cambia cuantos bits se comparan de un doble al ordenar. Con 1 o 2, `frank()`
# agrupa valores que `duplicated()` distingue, y el conteo cambia: medido sobre
# dobles que difieren en un `eps`, sobre `POSIXct` con diferencias de
# microsegundos y sobre magnitudes grandes, los tres divergen.
#
# Cualquier otro paquete de la sesion puede haberlo cambiado, asi que el ajuste
# se consulta, no se supone -y no se modifica: cambiarlo desde aca alteraria el
# comportamiento de codigo ajeno-. Solo importa si hay columnas dobles;
# `POSIXct` cuenta como doble.
.redondeo_numerico_es_exacto <- function(datos) {
  if (!any(vapply(datos, is.double, logical(1L)))) return(TRUE)
  redondeo <- tryCatch(
    data.table::getNumericRounding(), error = function(e) NA_integer_
  )
  isTRUE(redondeo == 0L)
}

# `frank()` ordena y por eso no distingue `NaN` de `NA`, mientras que la
# semantica de `duplicated()` sobre un `data.frame` si los distingue. Donde
# aparece un `NaN` se mide por la via de base, que es la que fija el resultado.
.tiene_nan_en_dobles <- function(datos) {
  any(vapply(
    datos,
    function(columna) {
      is.double(columna) && anyNA(columna) && any(is.nan(columna))
    },
    logical(1L)
  ))
}

# Una clave declarada responde dos preguntas distintas. `duplicated()` resuelve
# la unicidad con la semantica de R, donde dos `NA` de una misma posicion
# colisionan; `is.na()` resuelve si todos los componentes estan presentes. No
# mezclar las dos respuestas evita atribuir una colision de la traza a SQL.
.filas_clave_con_ausentes <- function(valores) {
  ausentes <- tryCatch(is.na(valores), error = function(e) NULL)
  if (is.null(ausentes) || length(dim(ausentes)) != 2L) return(NULL)
  if (nrow(valores)) rowSums(ausentes) > 0L else logical()
}

.evaluar_clave_declarada <- function(datos, clave) {
  if (is.null(clave) || !length(clave)) return(NULL)

  valores <- .seleccionar_columnas(datos, clave)
  n <- nrow(datos)
  # La clave se evalua con la semantica de un data.frame de R, aunque otro
  # objeto haya registrado un metodo `duplicated()` durante la sesion.
  duplicadas <- .filas_duplicadas_base(valores)
  ausentes <- tryCatch(is.na(valores), error = function(e) NULL)
  pudo_contar_ausentes <- !is.null(ausentes) &&
    length(dim(ausentes)) == 2L
  n_ausentes <- if (pudo_contar_ausentes) {
    sum(ausentes)
  } else {
    NA_integer_
  }
  filas_con_ausentes <- if (pudo_contar_ausentes && n) {
    sum(rowSums(ausentes) > 0L)
  } else if (pudo_contar_ausentes) {
    0L
  } else {
    NA_integer_
  }
  pudo_comprobar_unicidad <- !is.null(duplicadas)
  colisionan <- if (pudo_comprobar_unicidad) {
    duplicadas$adelante | duplicadas$atras
  } else {
    logical()
  }
  filas_con_ausentes_logica <- .filas_clave_con_ausentes(valores)
  n_repeticiones <- if (pudo_comprobar_unicidad) {
    sum(duplicadas$adelante)
  } else {
    NA_integer_
  }
  n_filas_colision <- if (!is.null(colisionan)) sum(colisionan) else NA_integer_
  n_filas_colision_con_ausentes <- if (length(colisionan) &&
      length(filas_con_ausentes_logica) == length(colisionan)) {
    sum(colisionan & filas_con_ausentes_logica)
  } else {
    NA_integer_
  }
  # La unicidad se evalua SOLO entre las filas con la clave completa. Una
  # repeticion entre filas incompletas no viola la unicidad -en SQL dos NULL no
  # son iguales-, y contarla aca hacia que `lupa` llamara "no es unica" a una
  # clave que si lo es entre sus casos evaluables. Esa colision no se pierde: se
  # informa en `trazabilidad`, que es donde importa, porque deja el localizador
  # ambiguo.
  completas_logica <- if (pudo_contar_ausentes && n) {
    !filas_con_ausentes_logica
  } else if (pudo_contar_ausentes) {
    logical()
  } else {
    NULL
  }
  n_completas <- if (is.null(completas_logica)) {
    NA_integer_
  } else {
    sum(completas_logica)
  }
  duplicadas_completas <- if (pudo_comprobar_unicidad &&
      !is.null(completas_logica)) {
    .filas_duplicadas_base(valores[completas_logica, , drop = FALSE])
  } else {
    NULL
  }
  n_repeticiones_completas <- if (is.null(duplicadas_completas)) {
    NA_integer_
  } else {
    sum(duplicadas_completas$adelante)
  }
  n_colision_completas <- if (is.null(duplicadas_completas)) {
    NA_integer_
  } else {
    tryCatch(
      sum(duplicadas_completas$adelante | duplicadas_completas$atras),
      error = function(e) NA_integer_
    )
  }
  # Cuando ninguna fila tiene la clave completa, la unicidad entre claves
  # completas es cierta sobre un conjunto vacio. Decir "verificada" seria cierto
  # y engañoso a la vez, asi que tiene estado propio.
  unicidad <- if (is.null(duplicadas_completas) || is.na(n_completas)) {
    "no_verificada"
  } else if (n_completas == 0L) {
    "sin_casos_evaluables"
  } else if (isTRUE(n_repeticiones_completas > 0L)) {
    "refutada"
  } else {
    "verificada"
  }
  ausencia_nulos <- if (!pudo_contar_ausentes) {
    "no_verificada"
  } else if (isTRUE(n_ausentes > 0L)) {
    "refutada"
  } else {
    "verificada"
  }
  list(
    columnas = as.character(clave),
    unicidad = list(
      estado = unicidad,
      semantica = "claves_completas",
      filas_evaluadas = as.integer(n_completas),
      filas_totales = as.integer(n),
      filas_repetidas = as.numeric(n_repeticiones_completas),
      filas_en_colision = as.numeric(n_colision_completas)
    ),
    ausencia_nulos = list(
      estado = ausencia_nulos,
      valores_ausentes = as.numeric(n_ausentes),
      filas_con_ausentes = as.numeric(filas_con_ausentes),
      columnas_evaluadas = as.integer(length(clave))
    ),
    trazabilidad = list(
      localizador = "clave_declarada",
      semantica = "R",
      colisiona_con_ausentes = isTRUE(n_filas_colision_con_ausentes > 0L),
      filas_colision_con_ausentes = as.numeric(
        n_filas_colision_con_ausentes
      ),
      nota = paste(
        "La trazabilidad agrupa valores de la clave con la semantica de R;",
        "dos NULL de SQL no son iguales, pero una ausencia sigue violando",
        "la exigencia de NOT NULL."
      )
    ),
    requiere_declaracion = !identical(unicidad, "verificada") ||
      !identical(ausencia_nulos, "verificada")
  )
}

.advertir_clave_declarada <- function(evaluacion) {
  if (is.null(evaluacion) || !isTRUE(evaluacion$requiere_declaracion)) {
    return(invisible(NULL))
  }
  unicidad <- evaluacion$unicidad
  ausencia <- evaluacion$ausencia_nulos
  mensajes <- character()
  if (identical(unicidad$estado, "refutada")) {
    mensajes <- c(mensajes, paste0(
      "La clave declarada no es unica: entre las ", unicidad$filas_evaluadas,
      " filas con la clave completa, ", unicidad$filas_repetidas,
      " repiten su valor (", unicidad$filas_en_colision,
      " filas participan en colisiones)."
    ))
  } else if (identical(unicidad$estado, "verificada")) {
    mensajes <- c(mensajes, paste0(
      "La comprobacion de unicidad fue verificada entre las ",
      unicidad$filas_evaluadas, " filas con la clave completa: no se ",
      "encontraron colisiones."
    ))
  } else if (identical(unicidad$estado, "sin_casos_evaluables")) {
    # No es "verificada" -seria cierto sobre un conjunto vacio y enganoso- ni
    # "no verificada" -si se recorrio la tabla entera y se comprobo que no habia
    # ningun caso-. Es un tercer estado y se dice por que.
    mensajes <- c(mensajes, paste0(
      "No se pudo evaluar la unicidad: ninguna de las ",
      unicidad$filas_totales,
      " filas tiene la clave completa, asi que no hay casos que comparar."
    ))
  } else {
    mensajes <- c(mensajes,
      "La comprobacion de unicidad no fue verificada: no se pudo comparar la clave."
    )
  }
  if (identical(ausencia$estado, "refutada")) {
    mensajes <- c(mensajes, paste0(
      "La comprobacion de ausencia de nulos fue refutada: se encontraron ",
      ausencia$valores_ausentes, " valores ausentes en la clave (",
      ausencia$filas_con_ausentes, " filas afectadas)."
    ))
  } else if (identical(ausencia$estado, "verificada")) {
    mensajes <- c(mensajes,
      "La comprobacion de ausencia de nulos fue verificada: no se encontraron valores ausentes."
    )
  } else {
    mensajes <- c(mensajes,
      "La comprobacion de ausencia de nulos no fue verificada: no habia filas o no se pudo leer la clave."
    )
  }
  if (isTRUE(evaluacion$trazabilidad$colisiona_con_ausentes)) {
    mensajes <- c(mensajes, paste(
      "La trazabilidad conserva la semantica de R para localizar esas filas;",
      "la colision entre ausentes no demuestra por si sola una violacion de",
      "unicidad SQL, pero los valores ausentes impiden la garantia NOT NULL."
    ))
  }
  cli::cli_warn(paste(mensajes, collapse = " "))
  invisible(NULL)
}

#' Perfilar un conjunto de datos
#'
#' Examina un `data.frame`, `tibble` o `data.table` y devuelve estadísticas
#' generales, métricas por columna, patrones, formatos de fecha y hallazgos
#' accionables. Todas las proporciones se expresan en `[0, 1]`.
#'
#' Los umbrales de faltantes se aplican a la suma de ausentes reales y
#' faltantes disfrazados y son estrictos: la proporción debe superar el umbral
#' para generar el nivel correspondiente. La lista de cadenas está congelada con referencia a
#' [naniar](https://github.com/njtierney/naniar)::common_na_strings 1.1.0 y suma extensiones habituales en datos
#' administrativos uruguayos. Las entradas que [naniar](https://github.com/njtierney/naniar) expresa como patrones
#' escapados se adaptan a los signos literales de interrogación, asterisco y
#' punto porque aquí se comparan por igualdad. La lista no depende de la
#' versión instalada.
#' Los sentinelas numéricos predeterminados son `-9`, `-99`, `-999`, `-9999` y
#' `999`. La lista es deliberadamente más corta que
#' [naniar](https://github.com/njtierney/naniar)::common_na_numbers 1.1.0:
#' `66`, `77`, `88` y `9999` también pueden
#' ser edades, códigos o años legítimos. `sentinelas_numericos` representa la
#' política completa, no una lista que se agrega silenciosamente: use
#' `numeric()` para desactivar todos los sentinelas numéricos, o
#' `sentinelas_naniar` para solicitar explícitamente la lista de naniar.
#'
#' `muestra` limita el descubrimiento de patrones, la inferencia de tipos, la
#' detección de formatos de fecha y la búsqueda de dependencias funcionales.
#' Las demás métricas y hallazgos se calculan sobre todas las filas. Por eso
#' `meta$filas_analizadas` describe el máximo usado por los análisis muestreados,
#' no el alcance del perfil completo.
#' En cada fila de `columnas`, `n_filas_analizadas_tipo` y
#' `muestreado_tipo_inferido` declaran el alcance concreto de
#' `proporcion_tipo_inferido`; no debe interpretarse esa proporción como si
#' hubiera usado necesariamente toda la columna.
#'
#' Cuando el tipo inferido es `"fecha"` o `"fecha-hora"`,
#' `estado_tipo_inferido` declara además cómo quedó establecida esa lectura:
#' `"confirmado"` si todo formato con casamientos es inequívoco, y
#' `"candidato"` si el veredicto se apoya en casamientos ambiguos — el caso de
#' una columna donde `proporcion_tipo_inferido` llega a 1 sin un solo valor
#' inequívoco. En los demás tipos la columna queda `NA`. El resumen impreso
#' anota `(candidato)` junto al tipo cuando corresponde; la conversión, el
#' rango temporal y la remediación ya exigían formatos confirmados y no
#' cambian.
#'
#' En una columna temporal, `minimo`, `maximo`, `media` y `mediana` quedan en
#' `NA` y su valor viaja en `minimo_fecha`, `maximo_fecha`, `media_fecha` y
#' `mediana_fecha`, que son texto legible. `desvio` es la excepción y conviene
#' saberlo: no es un momento sino una duración, así que se informa como número,
#' y ese número está **en segundos** tanto para `"fecha"` como para
#' `"fecha-hora"`, porque las dos clases se unifican en esa unidad antes de
#' resumirlas. Un desvío de `136610.4` sobre una columna de fechas son 1,6 días.
#'
#' Ese detalle importa al comparar las dos puertas. [perfilar_dbi()] no clasifica
#' tipos: informa el que declara el motor, y un motor que no preserva `DATE` ni
#' `BOOLEAN` —SQLite guarda ambos como número— hace que esas columnas se midan
#' como números. La misma columna de fechas da entonces `desvio` en días por la
#' puerta DBI y en segundos por ésta, y su `moda` sale como el entero crudo del
#' motor en vez de la fecha formateada. No es una discrepancia de cálculo: cada
#' puerta describe lo que tiene delante, y lo que tiene delante es distinto.
#'
#' Una columna cuyo año se expresa con dos dígitos se informa con su
#' `tipo_inferido` —`"fecha"` o `"fecha-hora"`— pero deja `minimo_fecha`,
#' `maximo_fecha`, `media_fecha` y `mediana_fecha` en `NA`. No es una omisión:
#' `23` puede ser 1923 o 2023, y elegir el siglo para calcular un rango sería
#' inventarlo. El hallazgo `anio_de_dos_digitos` señala esas columnas, y el
#' rango aparece una vez que el usuario resuelve la ambigüedad.
#' Una columna de períodos expresados sólo como mes y año informa la
#' `granularidad` `"mes"` en `formatos_fecha` y deja los resúmenes de fecha en
#' `NA` con `estado_resumen_cuantitativo = "granularidad_incompleta"`: asignar
#' el día 1 para obtener un mínimo o una media también sería inventar un dato.
#' Si esos períodos son minoritarios dentro de una columna que también contiene
#' fechas con día, los resúmenes se calculan sólo sobre las fechas completas y
#' declaran `estado_resumen_cuantitativo = "calculados_sobre_dias"`, junto con
#' `n_fechas_resumidas` y `n_fechas_excluidas_granularidad`. El mínimo y el máximo
#' son entonces condicionales al subconjunto con día; no representan un rango
#' de toda la columna.
#'
#' Para números ordinarios, los estadísticos cuantitativos se calculan sólo con
#' valores finitos; `n_nan`, `n_infinito_positivo` y `n_infinito_negativo`
#' declaran lo excluido. En columnas `integer64` que exceden el entero máximo
#' representable exactamente por `double`, `minimo` y `maximo` quedan en `NA` y
#' los extremos exactos se conservan en `minimo_exacto` y `maximo_exacto`.
#' Una columna de listas intenta contar sus valores distintos; si la clase no
#' admite comparación, informa `NA` en lugar de afirmar cero.
#' Las columnas matriciales se conservan como una unidad por fila: `n` informa
#' las filas de la tabla, pero los estadísticos por valor quedan en `NA` y un
#' hallazgo explica que deben separarse en columnas con semántica explícita.
#'
#' Una columna numérica emite `valor_concentrado` como señal `sospechoso` cuando
#' tiene al menos 20 valores válidos y 10 valores distintos, y su moda tiene
#' una frecuencia al menos cinco veces mayor que la del segundo valor más
#' frecuente y representa al menos 0,15 de los valores válidos. La elegibilidad
#' es parte de la señal: una columna con menos categorías no recibe una fila de
#' `cobertura_diagnosticos`, porque allí la moda es la distribución. La
#' evidencia publica el valor modal, ambas frecuencias, el cociente y la
#' fracción. M2 fue la regla seleccionada en
#' `.trabajo-agente/medicion-concentracion.md`: produjo cero falsos positivos en
#' 114 columnas limpias, con un margen medido de 2,2 veces. Es una sospecha, no
#' una acusación: un valor legítimo puede dominar. Sus puntos ciegos medidos
#' son las concentraciones por debajo de 15 % y los empates naturales en
#' columnas enteras pequeñas, donde el cociente puede quedar por debajo de
#' cinco.
#'
#' La ley de Benford se evalúa sólo en columnas numéricas con al menos 50
#' valores finitos, para no agregar cobertura a columnas que ni siquiera son
#' candidatas. Antes de comparar exige variación, que la columna no parezca un
#' identificador ni una secuencia correlativa, al menos 100 observaciones
#' positivas utilizables, una proporción de positivos igual a 1 y tres órdenes
#' de magnitud según `log10(max/min)`. Si falla alguna precondición no emite un
#' hallazgo: la enumera en `cobertura_diagnosticos`. Si aplica,
#' `meta$benford$resultados` conserva la distribución observada y esperada por
#' primer dígito, el chi-cuadrado de Pearson, ocho grados de libertad y el valor
#' p; `meta$benford$umbrales` publica todos los cortes. Un valor p menor que
#' `0.01` genera `desviacion_benford` como señal descriptiva para revisar, no
#' como evidencia de fraude o manipulación. Topes administrativos, redondeos,
#' precios psicológicos y subsidios de monto fijo son explicaciones posibles.
#'
#' Las relaciones aritméticas se buscan sólo entre columnas numéricas
#' declaradas y con variación: `Date`, `POSIXt`, `difftime`, `integer64`, texto
#' numérico y columnas constantes no participan. Cada relación requiere al
#' menos tres filas con valores finitos en todas las columnas involucradas;
#' los `NA`, `NaN` e infinitos quedan fuera del universo que publica la
#' evidencia. Para cada terna se prueban las tres orientaciones de una identidad
#' aditiva; esto cubre sumas y sus restas equivalentes sin informar tres veces
#' la misma igualdad. En pares proporcionales, `k` es la mediana de los cocientes
#' finitos cuya base no es cero, pero el cumplimiento se evalúa después también
#' en las filas con base cero. Si una identidad aditiva ya relaciona una terna,
#' se omiten las proporcionalidades redundantes entre su total y sus sumandos;
#' se conserva la proporcionalidad entre los dos sumandos. Una regularidad
#' completa se informa con severidad `"ok"`; si alcanza el umbral pero tiene
#' discrepancias, sigue el criterio de las relaciones de orden y es
#' `"sospechoso"`. Todo esto describe evidencia observada: no declara una regla
#' del dominio ni autoriza una corrección.
#' Los valores de texto que no forman UTF-8 válido tampoco se convierten: se
#' cuentan, se excluyen de los análisis textuales y generan un hallazgo con sus
#' posiciones. Los diagnósticos de invisibles incluyen controles C0/C1,
#' espacios Unicode, marcas direccionales, BOM y otros caracteres de transporte.
#' La evidencia los muestra como puntos de código; los espacios Unicode se
#' detectan aunque sólo se normalizan mediante una acción explícita, y ZWJ/ZWNJ
#' se informan pero se conservan porque pueden ser semánticos. La comparación
#' de duplicados con `normalizar = TRUE` aplica estas mismas clases sin borrar
#' ZWJ/ZWNJ.
#'
#' Los resúmenes de fecha-hora se expresan siempre en UTC y llevan el sufijo
#' `UTC` en el texto para hacer visible la zona aplicada. El instante se
#' conserva aunque la columna de entrada use otra zona horaria. Las columnas
#' `POSIXt` declaran `zona_horaria_origen` y
#' `n_filas_fecha_civil_distinta_utc`, que cuenta filas cuya fecha civil cambia
#' al mostrar el instante en UTC. Cuando la fecha civil cambia se emite un
#' hallazgo `zona_horaria_fecha_hora`; si la zona de origen no está declarada,
#' el conteo queda en `NA` y `cobertura_diagnosticos` declara que no se evaluó.
#' La zona se declara en la columna original, por ejemplo:
#' `attr(x, "tzone") <- "America/Montevideo"`.
#'
#' El diagnóstico de formas Unicode compara sin modificar el texto y puede usar
#' el paquete opcional `stringi` para enriquecer la evidencia cuando existen
#' caracteres no ASCII. `columnas$unicode_evaluado` declara si esa comprobación
#' pudo ejecutarse; en texto no ASCII sin `stringi` queda `FALSE`,
#' `n_variantes_unicode` queda en `NA` y `cobertura_diagnosticos` informa la
#' dependencia ausente. Las columnas ASCII se evalúan siempre y conservan cero.
#' El perfil de comparación es completamente R base.
#'
#' Las columnas `sfc` declaran su CRS, los tipos concretos y la dimensión
#' (`XY`, `XYZ`, `XYM` o `XYZM`), además de geometrías vacías, validez, dominio y
#' caja envolvente. `POINT`/`MULTIPOINT`, `LINESTRING`/`MULTILINESTRING` y
#' `POLYGON`/`MULTIPOLYGON` son una misma familia: el hallazgo de tipos mixtos
#' aparece sólo al combinar familias o ante `GEOMETRYCOLLECTION`.
#' La validez se calcula siempre con GEOS en el plano, sin CRS, para que el
#' resultado no dependa de que `s2` esté instalado; `validez_criterio` publica
#' `"planar"` y `n_validez_evaluados` publica su universo, que incluye las
#' geometrías vacías porque GEOS sí devuelve su validez. Si hay una dimensión
#' M, se aplica `sf::st_zm()` y se valida la topología XY;
#' `validez_preprocesamiento = "st_zm(x)"` declara esa transformación. Si el
#' CRS es geográfico, un fallo es `sospechoso` y no un `error`, porque no afirma
#' invalidez esférica. Las dimensiones Z y M no se evalúan como medidas:
#' `dimensiones_no_evaluadas` las enumera y `cobertura_diagnosticos` deja
#' constancia explícita incluso cuando la validez XY sí pudo calcularse.
#' En un CRS geográfico el dominio exige longitudes en `[-180, 180]` y latitudes
#' en `[-90, 90]`. Además, las coordenadas se comparan en longitud/latitud con
#' la `BBOX` del área de uso incluida en el WKT del CRS. Este control puede
#' detectar valores en unidades incompatibles —por ejemplo, grados declarados
#' como metros—, pero no una zona UTM equivocada cuando esa interpretación cae
#' dentro del área de la zona declarada. Una `BBOX` mundial es un no-op válido;
#' si el WKT no permite extraer la caja, el dominio queda en `NA` y
#' `cobertura_diagnosticos` lo declara en lugar de suponer el mundo entero.
#' Las geometrías vacías se cuentan aparte y no integran el universo del dominio
#' ni de la bbox. `n_dominio_evaluados` y `n_bbox_evaluados` publican ambos
#' universos; la bbox se calcula sobre coordenadas crudas de geometrías no
#' vacías, incluidas las que el dominio marque fuera, y `bbox_alcance` lo
#' declara. Si todas son vacías, sus conteos evaluados y fuera de dominio son
#' cero. Sin CRS,
#' `n_fuera_de_dominio` queda en `NA` y se emite `crs_no_declarado`: nunca se
#' supone EPSG:4326. Si falta el paquete opcional `sf`, todos esos campos quedan
#' en `NA`, no se emite un hallazgo geométrico y `cobertura_diagnosticos`
#' registra la dependencia ausente.
#' El argumento `normalizar` declara el perfil de comparación que se conserva
#' en `meta$normalizacion`; cambia sólo la representación usada para comparar,
#' no el texto guardado. `TRUE` usa el perfil predeterminado, `FALSE` desactiva
#' sus pasos configurables, `"amplio"` activa los tres pliegues optativos y
#' [normalizacion()] permite declararlos. También admite una lista nombrada por
#' columna. `meta$normalizacion_fusiones` informa, para cada paso activo, la
#' diferencia entre los valores distintos con el perfil completo y los valores
#' distintos con ese paso apagado y todo lo demás igual. Esa comparación
#' responde cuánto aporta cada paso por separado; sus números no son aditivos
#' porque dos pasos pueden fundir el mismo par. `n_distintos` y
#' `n_distintos_normalizados` declaran el total antes y después del perfil
#' completo. La descomposición canónica es siempre activa y forma parte de la
#' línea base, no una fila configurable.
#' Con `normalizar = FALSE` no hay pasos configurables que medir y el informe
#' de fusiones queda en `NULL`; en un perfil por columna sólo se incluyen las
#' columnas con algún paso activo. Si `detectar_duplicados_aproximados()` recibe
#' un perfil, puede reutilizar este informe ya calculado.
#' El informe usa el vocabulario completo: `n_distintos` y `n_usados` son el
#' número de valores distintos realmente comparados y el estado es `exacto`.
#' Las fusiones son una propiedad de pares, por lo que muestrear valores
#' aislados podría dejar fuera los dos miembros de cada par y convertir una
#' fusión real en un cero falso. La normalización se vectoriza para que este
#' alcance completo no dependa de la cardinalidad de la columna.
#' Las columnas sin ausentes que tienen al menos 100 filas y 90 % de valores
#' distintos producen un hallazgo `casi_clave` cuando el valor dominante
#' concentra al menos la mitad de los duplicados excedentes. La concentración
#' se calcula sobre las repeticiones posteriores a la primera de cada valor, no
#' sólo sobre la tasa de distintos. La evidencia declara el mínimo de filas,
#' ambos umbrales, los valores que colisionan y sus frecuencias. Las variables
#' con rol propuesto `fecha`, incluidas fecha-hora, se excluyen. Así una colisión
#' concentrada queda separada del texto libre de alta cardinalidad con
#' repeticiones dispersas.
#' Un vector `double` sólo participa si todos sus valores finitos son enteros;
#' así se conservan identificadores importados con ese almacenamiento y se
#' excluyen medidas con alguna parte fraccionaria, como importes o coordenadas.
#' Los vectores `integer64` se tratan como enteros semánticos. La evidencia del
#' hallazgo declara este criterio y el recuento observado.
#' Además, si `casi_duplicados_vocabulario = TRUE`, el perfil busca variantes
#' casi duplicadas en columnas de texto. Agrupa el vocabulario crudo mediante
#' fusiones exactas de la normalización y estrellas de distancia centradas en
#' un valor de frecuencia estrictamente mayor y único; los empates no se fuerzan.
#' No cierra cadenas transitivamente ni elige una forma canónica. La unidad es
#' el valor distinto, no la fila, y cada variante
#' conserva su frecuencia. Cada grupo declara sus distancias mínima y máxima.
#' El límite `max_proporcion_grupo_vocabulario` evita presentar un grupo que
#' abarque casi toda la columna como un diagnóstico útil: en ese caso el alcance
#' dice que el diagnóstico no aplica. Ese límite se activa desde 20 valores
#' distintos o cuando el grupo mayor ya tiene 10 variantes; sólo suprime el
#' grupo si además ocupa una fracción mayor que el umbral. Así un grupo de tres
#' en cuatro valores se entrega, pero quince variantes que ocupan toda una
#' columna no se presentan como una sola familia. Un grupo grande dentro de un
#' vocabulario mucho mayor puede seguir pasando si su proporción es pequeña. El
#' alcance expone ambos cortes. El argumento permite apagar el detector cuando
#' no corresponde a la tabla. Si hay pares cercanos pero todas las frecuencias
#' empatan, el alcance declara que no hubo asimetría para formar una estrella y
#' sugiere [detectar_duplicados_aproximados()] para comparar filas. El alcance
#' clasifica cada grupo como `normalizacion_exacta`, `dentro_de_palabra`,
#' `token_completo`, `token_unico`, `mixta` o `indeterminada`. La primera indica
#' una coincidencia tras normalizar; `dentro_de_palabra`, una diferencia dentro
#' de un token; `token_completo`, diferencias en tokens completos;
#' `token_unico`, que ambos valores son un único token y la clase estructural no
#' aplica; `mixta`, aristas de más de una clase; e `indeterminada`, que no hubo
#' aristas clasificables. Son evidencia descriptiva, no una decisión sobre
#' identidad. El agrupamiento no cambia por esa etiqueta. El alcance
#' también descarta aristas de distancia cuyos números no coinciden. Se comparan
#' las secuencias numéricas, quitando ceros de relleno y separadores de miles;
#' una diferencia numérica se trata como otra entidad. Esto puede dejar sin
#' agrupar una errata dentro de un número, porque no hay evidencia para
#' distinguirla de dos entidades reales. El alcance declara los valores y pares
#' comparados, los recortes por cardinalidad y si `stringdist` no estuvo
#' disponible; también informa cuántas aristas se descartaron por esa regla y
#' el tamaño compatible después del filtro. El tamaño máximo usado para el
#' límite conserva el componente potencial antes del filtro numérico, para que
#' una familia de entidades numeradas no vuelva a presentarse como una sola
#' variante; el tamaño compatible se muestra aparte. Para mantener acotado el
#' perfil, por omisión se evalúan hasta
#' 5.000 valores distintos y 2.000.000 de pares de unidades normalizadas; si
#' se alcanza un límite, la evidencia lo declara y no presenta el resultado
#' como universo completo. Las fusiones exactas se informan aun sin ese paquete
#' opcional.
#' Antes de formar esos grupos se retiran los valores que el mismo perfil ya
#' informó como `faltantes_disfrazados`. El diagnóstico fuerte de ausencia tiene
#' precedencia: un centinela no se presenta también como posible variante de un
#' valor válido. El alcance declara cuántas observaciones retiró este filtro.
#'
#' La clasificación de posibles datos personales es más amplia que la
#' protección. Cada clasificación declara `poder_discriminante` y `proteger`:
#'
#' - `debil`: una forma genérica, como siete a doce dígitos, coincide también
#'   con importes, facturas y códigos; se informa pero no se ocultan valores;
#' - `medio`: el nombre de la columna expresa una categoría personal (por
#'   ejemplo `telefono` o `fecha_nacimiento`); se protege aunque sus valores no
#'   se puedan validar. El nombre tiene prioridad sobre una forma numérica
#'   genérica y también determina la etiqueta de tipo;
#' - `alto`: una forma muy específica, como un correo, o nombre y forma se
#'   apoyan mutuamente; se protege;
#' - `verificado`: al menos tres valores distintos y al menos el 90% cumple uno
#'   de los validadores personales configurados; se protege incluso sin un
#'   nombre orientador. El pack uruguayo es el predeterminado, pero puede
#'   reemplazarse por un `pack_validadores()` de otro país o desactivarse con
#'   `FALSE`. La tolerancia del 10% permite tipeos aislados sin convertir una
#'   columna real en una salida pública; el umbral es configurable.
#'
#' La forma genérica de siete a doce dígitos tiene poder discriminante débil:
#' también describe importes, teléfonos, facturas e identificadores. Las formas
#' con separadores sólo se aceptan cuando tienen una estructura de documento
#' reconocible (por ejemplo, una cédula con grupos y guion o un RUT con grupos
#' de tres y cuatro dígitos); una fecha ISO, una fecha con puntos o guiones y
#' separadores arbitrarios no se consideran documentos. Un validador de dígito
#' que supera el umbral aporta evidencia verificable y eleva la clasificación;
#' una forma sola nunca se trata como prueba de identidad.
#'
#' Este criterio mide capacidad de discriminación, no juzga si la presencia del
#' dato es correcta. La protección sustituye modas, ejemplos, evidencia y
#' extremos o medianas que corresponden a observaciones reales. Los desvíos se conservan; las medias se protegen en sus dos formas
#' (`media` y
#' `media_fecha`), porque la media de una columna personal puede reconstruir
#' demasiado. Los desvíos siguen siendo síntesis no ligadas a una fila;
#' `detalle_proteccion_personal` hace visible la supresión. En fechas de
#' nacimiento, un hallazgo separado conserva el diagnóstico de valores
#' anteriores a 1900 o posteriores a la corrida sin publicar las fechas.
#' Los números escritos como texto reconocen tanto coma como punto decimal y
#' sus separadores de miles simétricos. Los prefijos de tres letras separados
#' del número, incluso como sufijo, `U$S` y los símbolos monetarios se
#' conservan como evidencia; `monedas_mixtas` informa sus frecuencias sin
#' convertir ni suponer tasas de cambio. Una única moneda o un símbolo `$`
#' aislado no produce ese hallazgo.
#' Un sufijo de unidad se reconoce sólo si es `%` o una abreviatura alfabética
#' en minúsculas; por eso `12 kg` y `13500 g` son unidades, mientras que
#' `12A` y `13B` se tratan como códigos. Si hay más de una unidad observada,
#' `unidades_mixtas` informa sus frecuencias y no convierte ni compara sus
#' magnitudes. Una única unidad no genera ese hallazgo.
#' `celdas_multivaluadas` es deliberadamente conservador: usa los patrones de
#' [descubrir_patrones()] y exige partes numéricas, alfanuméricas o
#' identificadoras puntuadas homogéneas, compatibles con el patrón del resto de
#' la columna. No interpreta comas en nombres o direcciones como listas; el
#' delimitador, la cantidad de celdas y la distribución de valores por celda
#' quedan en la evidencia del hallazgo.
#'
#' @param datos Objeto que hereda de `data.frame` —`tibble` y `data.table`
#'   entran por ahí— o una matriz de dos dimensiones, que se convierte con
#'   [as.data.frame()]. La conversión queda declarada en `meta$entrada_convertida`
#'   para que el perfil no aparente haber recibido lo que no recibió.
#' @param nombre Nombre descriptivo del objeto.
#' @param fecha Fecha y hora de la corrida. Se puede fijar para construir series
#'   reproducibles; se normaliza a UTC.
#' @param muestra Máximo de filas usadas para patrones, inferencia de tipos,
#'   formatos de fecha y dependencias funcionales. Use `Inf` para analizar todas
#'   las filas en esos análisis.
#' @param max_patrones Máximo de patrones mostrados por columna.
#' @param distinguir_mayusculas Si se distinguen mayúsculas y minúsculas.
#' @param expandir Si se emite un token por carácter en los patrones.
#' @param clave Nombres de las columnas que identifican una fila. Cuando se
#'   declaran, la trazabilidad de cada hallazgo trae además el valor de esas
#'   columnas para las filas señaladas, de modo que el caso se pueda verificar
#'   en el sistema de origen sin abrir la tabla. El índice de fila se conserva
#'   siempre. La clave declarada se trata como sensible: si la protección de
#'   datos personales está activa y alguna de esas columnas se clasifica como
#'   personal, sus valores salen enmascarados igual que la evidencia.
#'   La comprobación de una clave tiene dos ejes independientes:
#'   `meta$clave$unicidad` comprueba si sus combinaciones son únicas con la
#'   semántica de R,
#'   y `meta$clave$ausencia_nulos` comprueba que sus componentes no tengan
#'   valores ausentes. Cada eje declara `verificada`, `refutada` o
#'   `no_verificada`; una clave con ambos ejes verificados conserva exactamente
#'   el objeto histórico y no agrega metadatos. Cuando un eje falla o no se
#'   puede comprobar, `meta$clave$trazabilidad` explica que la localización
#'   agrupa con la semántica de R, incluso si el motor SQL trata dos `NULL` como
#'   distintos. `hallazgos` separa esos ejes: `clave_con_ausentes` enumera las
#'   filas que impiden la garantía `NOT NULL`, mientras `clave_no_unica` sólo
#'   informa repeticiones entre filas con la clave completa, el mismo universo
#'   que `meta$clave$unicidad`.
#' @param umbral_alta_cardinalidad Umbral sobre la tasa de valores distintos
#'   de una columna categórica. No alcanza por sí solo: el hallazgo exige
#'   además al menos diez valores distintos, porque con pocos la tasa está
#'   dominada por el tamaño de la tabla —dos valores en tres filas dan 0,67 y
#'   superan cualquier umbral razonable— y una columna de dos valores no
#'   puede tener cardinalidad alta.
#' @param umbral_faltantes_sospechoso Umbral inferior de faltantes. El
#'   hallazgo se activa al superarlo en sentido estricto.
#' @param umbral_faltantes_error Umbral por encima del cual los faltantes son
#'   un error; la igualdad conserva la severidad sospechosa.
#' @param umbral_patron_raro Máxima frecuencia de un patrón raro.
#' @param umbral_patron_dominante Frecuencia mínima del patrón dominante.
#' @param columnas_opcionales Nombres de columnas donde la ausencia no es un
#'   defecto. Su universo de completitud son las celdas presentes, y
#'   `cobertura_diagnosticos` declara el recorte. Sirve para el vacío por
#'   diseño: un historial con vigencia abierta, una columna que sólo
#'   corresponde a algunas filas.
#' @param aplicabilidad Lista con nombre por columna, donde cada elemento es
#'   una fórmula de un solo lado evaluada sobre `datos` — por ejemplo
#'   `list(marca_auto = ~ tiene_auto == "Si")`. Las filas donde el predicado no
#'   se cumple salen del universo de esa columna: no cuentan como ausencia. Las
#'   filas donde el predicado no se puede determinar se declaran aparte, sin
#'   contarse ni como aplicables ni como no aplicables. Un valor presente fuera
#'   del universo produce el hallazgo `valor_fuera_de_aplicabilidad`, que es el
#'   error simétrico y hoy no tiene otra forma de aparecer.
#'
#'   `lupa` no infiere el universo: si nadie lo declara, toda la columna aplica
#'   y el resultado es el de siempre. Declararlo es lo que distingue el vacío
#'   por diseño del vacío por error, y sin esa distinción una tabla sana puede
#'   informar completitud baja siendo completa.
#'
#'   **Hasta dónde llega el universo.** Gobierna todo el perfilado: los conteos,
#'   las proporciones, los hallazgos y las acciones que propone
#'   [planificar_limpieza()]. Medido sobre una columna condicionada de mil filas
#'   con universo de trescientas, treinta de ellas vacías: declarándolo, la
#'   proporción de faltantes es `0,100`, no hay hallazgo y el plan no propone
#'   nada; sin declararlo son `0,730`, sale `faltantes` y el plan propone dos
#'   acciones.
#'
#'   La medición contra un marco recibe la misma declaración: [medir()] acepta
#'   `aplicabilidad` y recorta las filas antes de medir, así que el histórico y
#'   la deriva —que consumen mediciones, no perfiles— heredan el número
#'   correcto. El `aplicabilidad` de [marco_calidad()] es otra cosa: dice si un
#'   factor aplica a datos temporales o geométricos, no qué filas entran al
#'   universo.
#' @param ausencia_estructural Si se busca evidencia de que la ausencia de una
#'   columna es por diseño. `lupa` no infiere el universo ni lo cambia por su
#'   cuenta, pero declarar `aplicabilidad` exige saber que existe: quien perfila
#'   una tabla con columnas condicionadas sin declarar nada recibe el mismo
#'   informe engañoso que si el vacío fuera un defecto. Con `TRUE`, cuando el
#'   valor de otra columna decide qué filas tienen ésta, o cuando dos columnas
#'   se reparten las filas sin pisarse, se emite `posible_ausencia_estructural`
#'   con severidad `ok`, la evidencia medida y la línea exacta que habría que
#'   escribir para declararlo. Sugiere; no decide. Las columnas ya declaradas
#'   quedan fuera del examen.
#'
#'   Con el mismo argumento viaja `regla_silencia_ausencia`, también `ok`: avisa
#'   cuando una columna declarada opcional o con universo propio sigue casi
#'   vacía dentro de ese universo. La declaración funcionó y por eso el perfil
#'   salió limpio; el aviso existe para que eso sea una decisión y no un efecto.
#' @param columnas_personales Columnas que traen datos personales, declaradas
#'   por quien conoce el dato. `c("cod_benef", "apodo")` las nombra sin decir de
#'   qué tipo son; `c(cod_benef = "documento_identidad")` además lo dice. Lo
#'   declarado gana sobre lo inferido y no se vuelve a examinar.
#'
#'   Existe porque el léxico de nombres de columna **no puede ser completo**:
#'   una columna con documentos se puede llamar de cualquier manera, y ninguna
#'   lista de nombres frecuentes la va a reconocer. Es el mismo patrón que
#'   `columnas_opcionales`, aplicado a la otra decisión que el paquete no puede
#'   tomar solo.
#' @param variantes_equifrecuentes_vocabulario Si se informa el diagnóstico
#'   `variantes_equifrecuentes_vocabulario`, que señala dos formas cercanas que
#'   se reparten la columna sin que ninguna sea dominante —el error sistemático
#'   de dos operadores, una plantilla rota o una migración parcial—.
#'
#'   **Está apagado por omisión y la razón está medida**: sobre la batería de
#'   31 tablas limpias produce un grupo sospechoso donde no hay defecto. Lo que
#'   lo dispara no es el tamaño de la tabla sino el reparto: dos formas
#'   parecidas con frecuencias del mismo orden lo activan igual con cuatro filas
#'   que con quinientas, y en una tabla chica ese reparto sale por casualidad
#'   más seguido. Decía «tablas de menos de veinte filas», y se midió de cuatro
#'   a quinientas: dispara en todas. Es aditivo: encenderlo no cambia ni pierde ninguna
#'   detección de `casi_duplicados_vocabulario`. El límite que deja de cubrir se
#'   declara igual, encendido o apagado, en `n_grupos_sin_variante_rara`.
#' @param max_asimetria_equifrecuente_vocabulario Razón máxima entre la
#'   frecuencia mayor y la menor para considerar que dos formas se reparten la
#'   columna. Con `2`, `40` contra `5` no entra y `5` contra `5` sí.
#' @param max_comparaciones_dependencias Tope de pares determinante-dependiente
#'   que se comparan. El costo de las dependencias es del orden de
#'   `columnas^2 x filas` y empeora con determinantes casi únicos; cuando el
#'   presupuesto se agota, lo comparado se informa y lo que quedó sin comparar
#'   se declara en `cobertura_diagnosticos`, nunca como cero.
#' @param max_trabajo_dependencias Tope predeterminado de unidades fila-par para
#'   las dependencias. Se combina con `max_comparaciones_dependencias`; el
#'   límite efectivo baja cuando hay muchas filas. `Inf` desactiva este tope,
#'   pero no el de pares.
#' @param max_largo_valor_vocabulario Maximo de caracteres permitido en cada
#'   valor que entra en la comparacion de casi duplicados del vocabulario. Por
#'   defecto es `10000`, elegido porque la distancia normalizada deja de
#'   distinguir de forma estable una diferencia local de muchas diferencias en
#'   textos largos. Una columna que supera el tope se declara completa fuera
#'   de alcance en `cobertura_diagnosticos`; no se recortan valores en silencio.
#'   `Inf` recupera explicitamente el comportamiento anterior sin tope.
#' @param max_celdas_muestra Maximo de celdas que puede contener la muestra
#'   comun de los diagnosticos que muestrean filas. Por defecto es `1000000`;
#'   se calcula como filas efectivas por columnas de la tabla. Si reduce la
#'   muestra, `cobertura_diagnosticos` informa las filas y celdas solicitadas,
#'   el umbral y el nuevo alcance. `Inf` desactiva este tope.
#' @param max_bytes_muestra Maximo de bytes de la muestra materializada que
#'   alimenta los diagnosticos que muestrean filas. Por defecto es `512 MiB`.
#'   El tamaño se estima sobre una sonda de hasta cien filas y se comprueba
#'   sobre la muestra efectiva; si la cota reduce el alcance,
#'   `cobertura_diagnosticos` informa los bytes observados y el umbral. `Inf`
#'   desactiva este tope.
#' @param avisar_costo_tabla_ancha Si es `TRUE`, avisa en sesiones interactivas
#'   cuando las celdas proyectadas superan `umbral_celdas_aviso_tabla_ancha`.
#'   El aviso es una estimacion y queda en silencio en scripts no interactivos.
#' @param umbral_celdas_aviso_tabla_ancha Cantidad de celdas a partir de la cual
#'   se emite el aviso de tabla ancha. Por defecto, `100000`; `Inf` lo desactiva
#'   explicitamente.
#' @param columnas_sin_ceros Nombres de columnas donde cero no es admisible.
#' @param columnas_no_negativas Nombres de columnas que deben ser no negativas.
#' @param sentinelas_numericos Vector completo de valores numéricos que se
#'   interpretan como ausencia. `numeric()` los desactiva; las cadenas de
#'   ausencia se siguen evaluando por separado.
#' @param analizar_dependencias Si se buscan dependencias funcionales entre
#'   pares de columnas. Se aplica una sola muestra común a toda la tabla.
#' @param umbral_dependencia Cumplimiento mínimo para informar una dependencia.
#' @param umbral_casi_clave_dependencia Tasa de valores distintos a partir de
#'   la cual un determinante se descarta como casi-clave antes de agrupar.
#' @param max_columnas_dependencias Máximo de columnas que intervienen en la
#'   búsqueda, cuyo costo crece cuadráticamente.
#' @param datos_personales_permitidos Si la entrega admite datos personales.
#'   El valor predeterminado no juzga su presencia: la clasificación se informa
#'   con severidad `"ok"`. Use `FALSE` sólo cuando el contrato de la entrega
#'   declare que no deben existir.
#' @param proteger_datos_personales Si se reemplazan modas, ejemplos, evidencia
#'   y estadísticos de orden concretos cuando `poder_discriminante` es medio,
#'   alto o verificado. Las clasificaciones débiles se conservan como aviso pero
#'   no suprimen estadísticos. Para conservar todo en el objeto debe desactivarse
#'   explícitamente; [reportar()] aplica además su propia protección
#'   predeterminada.
#' @param validadores_personales Pack o lista nombrada de funciones que reciben
#'   un vector de texto y devuelven un lógico de igual longitud. `NULL` usa
#'   `validadores_uruguay()` por compatibilidad; `FALSE` o `numeric()` desactiva
#'   la verificación de documentos. El nombre del mejor validador queda en el
#'   fundamento de la clasificación.
#' @param umbral_documento_verificado Proporción mínima de valores que debe
#'   aceptar un validador para clasificar una forma de documento como
#'   `verificado`. Por defecto es `0.9`.
#' @param muestra_validadores Máximo de valores usados en el filtro preliminar
#'   de cada validador. Si la proporción preliminar ya queda bajo el umbral no
#'   se valida la columna completa; use `Inf` para revisar todos desde el inicio.
#' @param duplicados_aproximados `FALSE` por omisión. Use `TRUE` o una lista de
#'   argumentos para ejecutar [detectar_duplicados_aproximados()] y añadir sus
#'   pares y hallazgos al perfil. Es un análisis acotado y opcional porque no
#'   afirma identidad ni debe encarecer todas las corridas.
#' @param normalizar Perfil de comparación que se conserva en `meta$normalizacion`
#'   y que heredan los análisis de duplicados y claves cuando no reciben uno
#'   explícito. Cambia sólo la representación usada para comparar.
#' @param casi_duplicados_vocabulario Lógico que activa el diagnóstico de
#'   variantes casi duplicadas dentro del vocabulario de cada columna de texto.
#'   Por defecto es `TRUE`; `FALSE` lo omite sin afectar los demás hallazgos.
#'   La distancia es una señal heurística, no una prueba de identidad: Jaro--Winkler
#'   puede agrupar nombres de calles o códigos que sólo comparten un prefijo o un
#'   sufijo. En vocabularios heterogéneos revise la evidencia como sospechosa,
#'   declare una regla de dominio o use `FALSE` para desactivar este diagnóstico.
#' @param max_proporcion_grupo_vocabulario Proporción máxima del vocabulario
#'   que puede abarcar el grupo mayor para entregar grupos de variantes. Por
#'   defecto es `0.5`; si se supera, el alcance declara que el diagnóstico no
#'   aplica en vez de entregar un bloque que abarque casi toda la columna.
#' @param max_trabajo_vocabulario Tope de comparaciones de carácter para las
#'   comparaciones de distancia del vocabulario. Comparar dos valores cuesta del
#'   orden del producto de sus largos, así que el trabajo es la suma de ese
#'   producto sobre todos los pares. Contar pares por longitud media subestima
#'   las cadenas largas: medido, el mismo presupuesto compraba 5,3 millones de
#'   unidades por segundo con valores de 900 caracteres y 44 millones con
#'   valores de 40. Se combina con el límite interno de pares del detector y
#'   manda el más restrictivo; ese límite interno no es un argumento de
#'   `perfilar()`. El recorte declara valores, pares y trabajo sin comparar.
#'   `Inf` lo desactiva.
#' @param umbral_variante_rara_vocabulario Proporcion maxima de la columna que
#'   puede ocupar una variante breve para abrir la comparacion por una edicion.
#' @param min_asimetria_vocabulario Razón mínima entre la frecuencia de la
#'   forma dominante y la de la variante para abrir un grupo por la vía general
#'   de distancia. Por omisión `2`. Una asimetría de `1,5` significa que las dos
#'   formas son casi igual de comunes, que es evidencia muy floja de una errata:
#'   medido sobre tablas limpias y sobre erratas sembradas, los falsos positivos
#'   están entre `1,0` y `1,5` y las erratas reales desde `9,0`.
#' @param min_asimetria_vocabulario_corto Razon minima entre la frecuencia de
#'   una forma dominante y una variante breve para abrir la comparacion por una
#'   edicion.
#' @param min_participacion_dominante_vocabulario_corto Proporcion minima de la
#'   columna que debe ocupar la forma dominante en la comparacion por una
#'   edicion.
#' @param max_filas_hallazgo Tope de índices de fila que conserva cada
#'   trazabilidad disponible. Por defecto es `1000`; cuando se supera, el
#'   estado queda como `truncada` y el total se conserva. Use `Inf` sólo si
#'   necesita desactivar explícitamente el tope.
#' @param umbral_orden_columnas Cumplimiento mínimo de una relación de orden
#'   entre columnas comparables. Se usa `0.95` por omisión; con menos de 20
#'   filas comparables se permite una sola inversión para no descartar tablas
#'   pequeñas. El alcance efectivo queda en `meta$orden_columnas`.
#' @param max_columnas_orden Máximo de columnas numéricas o temporales que se
#'   comparan entre sí para detectar relaciones de orden. Las columnas que
#'   exceden el límite se conservan en `meta$orden_columnas$columnas_omitidas`.
#' @param umbral_solapamiento_orden Solapamiento mínimo de los rangos
#'   intercuartiles para considerar que dos columnas representan magnitudes
#'   comparables. Por defecto es `0.1`: al menos una décima parte del rango
#'   intercuartílico más ancho debe ser común a ambos. Esto evita interpretar
#'   como restricción fila a fila un orden explicado sólo por escalas
#'   separadas. Si no hay ese solapamiento, una brecha con IQR exactamente cero
#'   conserva el par porque la mitad central sostiene el mismo desplazamiento
#'   fila a fila. No se aplica una tolerancia oculta. Use `0` para desactivar el
#'   filtro de magnitud. Ambos criterios se publican en la evidencia. Los pares
#'   descartados se cuentan en
#'   `meta$orden_columnas$pares_descartados_magnitud` y los recuperados en
#'   `meta$orden_columnas$pares_rescatados_brecha_estable`.
#' @param umbral_aritmetica Proporción mínima de filas comparables que deben
#'   satisfacer una identidad dentro de `tolerancia_aritmetica` para reconocer
#'   una regularidad aritmética entre columnas numéricas. El valor por omisión
#'   es `0.9`. Una vez reconocida la relación se informan todas sus
#'   discrepancias, sin un segundo filtro por su cantidad absoluta. La
#'   proporción y el criterio efectivos se publican en cada evidencia.
#' @param min_filas_aritmetica Mínimo de filas comparables necesario para
#'   evaluar una candidata aritmética. Por omisión es `3`.
#' @param tolerancia_aritmetica Tolerancia numérica relativa escalada usada al
#'   comparar un valor observado y uno esperado. Por omisión es `1e-8`; el
#'   criterio completo y el valor efectivo se declaran en cada evidencia.
#' @param max_columnas_aritmetica Máximo de columnas numéricas que intervienen
#'   en la búsqueda aritmética, cuyo costo crece cúbicamente. Por omisión es
#'   `20`. Si se omiten columnas, `cobertura_diagnosticos` declara el recorte y
#'   `meta$aritmetica_columnas` conserva los conteos de combinaciones.
#'
#' @return Objeto S3 de clase `perfil`. Cada fila de hallazgos incluye
#'   n_evaluados, n_afectados y unidad_conteo: son conteos de las unidades
#'   declaradas (por ejemplo fila, columna, formato o par). En
#'   `mayusculas_inconsistentes`, `normalizacion_unicode` y
#'   `casi_duplicados_vocabulario`, la unidad es
#'   `valor_distinto`: `n_evaluados` cuenta los valores distintos evaluados y
#'   `n_afectados` los valores distintos que participan en la colisión. Su
#'   `trazabilidad` sigue siendo por fila y enumera todas las filas que
#'   contienen esos valores, no sólo las filas defectuosas; por eso su total
#'   puede ser mayor que `n_afectados`. En `casi_duplicados_vocabulario`, la
#'   traza incluye todas las filas cuyos
#'   valores pertenecen al grupo elegido, incluida la forma dominante. La
#'   distancia es una senal heuristica, no una prueba de que cada fila deba
#'   corregirse; la evidencia declara cuantas filas mostradas pertenecen a las
#'   formas variantes y cuantas a las formas dominantes.
#'   En `filas_duplicadas`, el conteo y la traza incluyen todas las filas
#'   participantes de los grupos; el numero de excedentes queda en la
#'   evidencia. Cuando el camino
#'   no puede conocer un conteo, informa NA, nunca cero. La columna de lista
#'   `trazabilidad` distingue `disponible`, `truncada`, `no_aplica` y
#'   `no_disponible`; cuando corresponde conserva índices de fila acotados por
#'   `max_filas_hallazgo`, el total conocido y el alcance. En
#'   `clave_con_ausentes`, cuenta las filas con al menos un componente ausente
#'   y su traza enumera esas filas. En `clave_no_unica`, cuenta las filas que
#'   participan en colisiones entre claves completas y su traza excluye las
#'   filas incompletas; ambas unidades son `fila`. Así, una colisión entre
#'   ausentes no aparece como una repetición que refute la unicidad.
#'   `casi_duplicados_vocabulario`, donde la traza mezcla filas de formas
#'   variantes con filas de la forma dominante, conserva además
#'   `n_filas_formas_variantes` y `n_filas_formas_dominantes` con el reparto
#'   completo, y `mostrados_formas_variantes` y `mostrados_formas_dominantes`
#'   con el reparto de lo que sobrevivió al truncado. Las variantes se entregan
#'   primero, de modo que el truncado no se lleve lo accionable. Para
#'   `patron_raro`,
#'   el alcance puede ser `completo`, `muestra_patrones`,
#'   `patrones_parciales` o `muestra_patrones+patrones_parciales`. El resumen y
#'   el texto de evidencia de `patron_raro` muestran como maximo seis patrones,
#'   pero la trazabilidad conserva los nombres de todos los patrones raros hasta
#'   un limite de 5.000; `patrones_parciales` indica que se alcanzo ese limite,
#'   no que se haya alcanzado el tope de presentacion.
#'   Cuando se emite un hallazgo `patron_raro`, su evidencia incluye la
#'   proporcion del patron dominante y cuantas filas pertenecen a patrones no
#'   dominantes que superan `umbral_patron_raro` y por eso quedan excluidos.
#'   Si el patron dominante no alcanza `umbral_patron_dominante`, no se emite
#'   el hallazgo: `cobertura_diagnosticos` declara la no medicion, su proporcion
#'   observada y el argumento que se puede ajustar.
#'   Si el conteo y la traza no coinciden, conserva el hallazgo y emite una
#'   advertencia de clase `lupa_trazabilidad_incoherente`. La guarda compara el
#'   total previo al truncado y respeta la unidad declarada.
#'   Una matriz no analizada conserva en la traza todas sus filas. Si una
#'   columna de listas se reconoce como constante pero no se puede contar su
#'   frecuencia, el conteo afectado queda en NA y `cobertura_diagnosticos`
#'   explica la no evaluación.
#'   Los índices no contienen valores. Usarlos para extraer filas de los datos
#'   originales puede volver a exponer datos personales; el paquete no realiza
#'   esa extracción y la protección de salidas no sustituye el control de acceso
#'   a los datos de entrada.
#'   En la evidencia de `casi_duplicados_vocabulario`, `clase_diferencia` puede
#'   ser `normalizacion_exacta` (la coincidencia aparece después de normalizar),
#'   `dentro_de_palabra` (la diferencia está dentro de un token),
#'   `token_completo` (cambian tokens completos), `token_unico` (ambos valores
#'   son un solo token y esa distinción estructural no aplica), `mixta` (el grupo
#'   reúne aristas de más de una clase) o `indeterminada` (no hubo aristas
#'   clasificables). Son categorías de evidencia, no veredictos de identidad.
#'   `cobertura_diagnosticos` es una tabla hermana de `hallazgos`, con una fila
#'   por diagnóstico que no pudo evaluarse o cuya enumeración quedó parcial y
#'   las columnas `diagnostico`,
#'   `columna`, `motivo`, `como_resolverlo` y `dependencia`. Incluye la falta de
#'   `stringdist`, `stringi`, `bit64` o `sf`, y las zonas horarias POSIXt sin
#'   declarar. Los patrones de frecuencia intermedia no se consideran desvios
#'   del patron dominante: `patron_raro` es completo respecto de su criterio de
#'   rareza cuando no hay recorte de trazabilidad. Si el conjunto de nombres
#'   raros supera 5.000, `cobertura_diagnosticos` declara el recorte y su limite.
#'   Quien decida automáticamente sobre un perfil debe revisar
#'   `nrow(perfil$cobertura_diagnosticos)` además de las severidades: un perfil
#'   sin hallazgos y con diagnósticos no evaluados no es un perfil limpio.
#'   Cuando una clave declarada no queda plenamente verificada, `meta$clave`
#'   conserva los estados de unicidad y ausencia de nulos, sus conteos y la
#'   semántica usada por la trazabilidad. Los tres responden preguntas distintas
#'   y no comparten universo:
#'
#'   - `unicidad` se evalúa **sólo entre las filas con la clave completa**
#'     (`semantica = "claves_completas"`), porque una repetición entre filas
#'     incompletas no viola la unicidad: en SQL dos `NULL` no son iguales.
#'     `filas_evaluadas` cuenta esas filas y `filas_totales` la tabla entera. Su
#'     estado es `"verificada"`, `"refutada"`, `"no_verificada"` cuando no se
#'     pudo comparar, o `"sin_casos_evaluables"` cuando **ninguna** fila tiene la
#'     clave completa: ahí la unicidad sería cierta sobre un conjunto vacío, que
#'     es cierto y engañoso a la vez, y por eso tiene estado propio.
#'   - `ausencia_nulos` responde si todos los componentes están presentes.
#'     Su hallazgo asociado, `clave_con_ausentes`, cuenta las filas con al menos
#'     un componente ausente y conserva sus índices.
#'   - `clave_no_unica` sólo se emite cuando hay valores repetidos entre las
#'     filas completas y usa `filas_evaluadas` como `n_evaluados`; no convierte
#'     una colisión entre ausentes en una violación de unicidad.
#'   - `trazabilidad` conserva la semántica de R, que es la que localiza las
#'     filas, e informa en `colisiona_con_ausentes` si el localizador queda
#'     ambiguo porque dos filas con ausentes comparten representación.
#' @export
#' @seealso [descubrir_patrones()], [detectar_dependencias()],
#'   [proponer_modelo()], [planificar_limpieza()]
#'
#' @examples
#' perfil <- perfilar(datos_administrativos)
#' perfil
#' summary(perfil)
perfilar <- function(datos,
                     nombre = .nombre_de_los_datos(substitute(datos)),
                     fecha = Sys.time(),
                     muestra = 1e5,
                     max_patrones = 20,
                     distinguir_mayusculas = TRUE,
                     expandir = FALSE,
                     umbral_alta_cardinalidad = 0.5,
                     umbral_faltantes_sospechoso = 0.1,
                     clave = NULL,
                     umbral_faltantes_error = 0.4,
                     umbral_patron_raro = 0.05,
                     umbral_patron_dominante = 0.5,
                     columnas_sin_ceros = character(),
                     columnas_no_negativas = character(),
                     columnas_opcionales = character(),
                     aplicabilidad = NULL,
                     ausencia_estructural = TRUE,
                     sentinelas_numericos = c(-9, -99, -999, -9999, 999),
                     analizar_dependencias = TRUE,
                     umbral_dependencia = 0.995,
                     umbral_casi_clave_dependencia = 0.8,
                     max_columnas_dependencias = 100L,
                     datos_personales_permitidos = TRUE,
                     proteger_datos_personales = TRUE,
                     columnas_personales = character(),
                     validadores_personales = NULL,
                     umbral_documento_verificado = 0.9,
                     muestra_validadores = 1000L,
                     duplicados_aproximados = FALSE,
                     normalizar = TRUE,
                     max_filas_hallazgo = 1000L,
                     umbral_orden_columnas = 0.95,
                     max_columnas_orden = 20L,
                     umbral_solapamiento_orden = 0.1,
                     umbral_aritmetica = 0.9,
                     min_filas_aritmetica = 3L,
                     tolerancia_aritmetica = 1e-8,
                     max_columnas_aritmetica = 20L,
                     casi_duplicados_vocabulario = TRUE,
                     max_proporcion_grupo_vocabulario = 0.5,
                     umbral_variante_rara_vocabulario = 0.05,
                     min_asimetria_vocabulario_corto = 10,
                     min_asimetria_vocabulario = 2,
                     min_participacion_dominante_vocabulario_corto = 0.5,
                     variantes_equifrecuentes_vocabulario = FALSE,
                     max_asimetria_equifrecuente_vocabulario = 2,
                      max_comparaciones_dependencias = 200000L,
                     max_trabajo_vocabulario = 2e10,
                     max_trabajo_dependencias = 100000000,
                     max_largo_valor_vocabulario =
                       .MAX_LARGO_VALOR_CASI_DUPLICADOS,
                     max_celdas_muestra = .MAX_CELDAS_MUESTRA,
                     max_bytes_muestra = .MAX_BYTES_MUESTRA,
                     avisar_costo_tabla_ancha = TRUE,
                     umbral_celdas_aviso_tabla_ancha =
                       .UMBRAL_CELDAS_AVISO_TABLA_ANCHA) {
  # Una matriz de dos dimensiones es una tabla, y rechazarla obligaba a escribir
  trazador_tiempos <- attr(
    datos, "lupa_trazador_tiempos_dbi", exact = TRUE
  )

# `deparse()` de una expresion larga devuelve VARIAS lineas, y con
# `do.call(perfilar, list(data.frame(...)))` la expresion es la tabla entera: el
# nombre salia con ocho elementos y `reportar()` reventaba con "values must be
# length 1". `do.call()` es una forma corriente de llamar a esto -pasar los
# argumentos en una lista es lo natural cuando se perfila en un bucle-, asi que
# el nombre tiene que sobrevivirla.
#
# Si la expresion no cabe en una linea no sirve como nombre: no lo escribio
# nadie, es una tabla deparseada. En ese caso se usa una etiqueta generica.
.nombre_de_los_datos <- function(expresion) {
  texto <- tryCatch(deparse(expresion), error = function(e) character())
  if (length(texto) != 1L || !nzchar(trimws(texto)) || nchar(texto) > 120L) {
    return("datos")
  }
  texto
}

  # la conversion afuera. Se acepta y se convierte, y la conversion queda
  # declarada en `meta` para que el perfil no aparente haber recibido lo que no
  # recibio. Una matriz sin nombres de columna los recibe de R.
  entrada_convertida <- NA_character_
  if (!inherits(datos, "data.frame") && is.matrix(datos)) {
    entrada_convertida <- paste0(
      "matriz de ", nrow(datos), " por ", ncol(datos),
      " convertida con as.data.frame()"
    )
    datos <- as.data.frame(datos, stringsAsFactors = FALSE)
  }
  if (!inherits(datos, "data.frame")) {
    stop(
      "`datos` debe ser un data.frame, tibble, data.table o una matriz.",
      call. = FALSE
    )
  }
  if (inherits(datos, "data.table")) {
    entrada_convertida <- paste0(
      "data.table de ", nrow(datos), " por ", ncol(datos),
      " convertida con as.data.frame()"
    )
    datos <- as.data.frame(datos, stringsAsFactors = FALSE)
  }
  muestra <- .validar_muestra(muestra)
  alcance_muestra <- .resolver_muestra_perfilado(
    datos, muestra, max_celdas_muestra, max_bytes_muestra
  )
  muestra_diagnosticos <- if (nrow(datos)) {
    alcance_muestra$filas_efectivas
  } else {
    1
  }
  normalizacion_resuelta <- .resolver_normalizacion(normalizar)
  fecha_hora <- tryCatch(.fecha_utc(fecha), error = function(e) NA)
  if (length(fecha_hora) != 1L || is.na(fecha_hora) ||
      !is.finite(as.numeric(fecha_hora))) {
    stop("`fecha` debe contener una fecha y hora v\u00e1lida.", call. = FALSE)
  }
  umbrales <- c(
    umbral_alta_cardinalidad, umbral_faltantes_sospechoso,
    umbral_faltantes_error, umbral_patron_raro, umbral_patron_dominante
  )
  if (anyNA(umbrales) || any(umbrales < 0 | umbrales > 1)) {
    stop("Todos los umbrales deben estar entre 0 y 1.", call. = FALSE)
  }
  if (umbral_faltantes_error < umbral_faltantes_sospechoso) {
    stop("El umbral de error no puede ser menor que el sospechoso.", call. = FALSE)
  }
  evaluacion_clave <- NULL
  if (!is.null(clave)) {
    if (!is.character(clave) || !length(clave) || anyNA(clave) ||
        !all(nzchar(clave))) {
      stop(
        "`clave` debe ser un vector de nombres de columna sin NA.",
        call. = FALSE
      )
    }
    faltantes <- setdiff(clave, names(datos))
    if (length(faltantes)) {
      stop(
        "`clave` nombra columnas que no estan en los datos: ",
        paste(faltantes, collapse = ", "),
        ". Disponibles: ", paste(names(datos), collapse = ", "), ".",
        call. = FALSE
      )
    }
    if (anyDuplicated(clave)) {
      stop("`clave` repite una columna.", call. = FALSE)
    }
    evaluacion_clave <- .evaluar_clave_declarada(datos, clave)
    # Una clave que no identifica una fila o que tiene ausentes sirve igual
    # para localizar, pero no queda garantizada: cada eje se informa por
    # separado y se sigue sin romper el perfil.
    .advertir_clave_declarada(evaluacion_clave)
  }
  if (!is.numeric(sentinelas_numericos) || anyNA(sentinelas_numericos) ||
      any(!is.finite(sentinelas_numericos))) {
    stop("`sentinelas_numericos` debe ser un vector num\u00e9rico finito.", call. = FALSE)
  }
  if (!is.logical(analizar_dependencias) || length(analizar_dependencias) != 1L ||
      is.na(analizar_dependencias)) {
    stop("`analizar_dependencias` debe ser un l\u00f3gico escalar sin NA.",
         call. = FALSE)
  }
  for (argumento in c("datos_personales_permitidos", "proteger_datos_personales")) {
    valor <- get(argumento)
    if (!is.logical(valor) || length(valor) != 1L || is.na(valor)) {
      stop("`", argumento, "` debe ser TRUE o FALSE.", call. = FALSE)
    }
  }
  if (!is.numeric(umbral_documento_verificado) ||
      length(umbral_documento_verificado) != 1L ||
      is.na(umbral_documento_verificado) ||
      umbral_documento_verificado < 0 || umbral_documento_verificado > 1) {
    stop("`umbral_documento_verificado` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.numeric(muestra_validadores) || length(muestra_validadores) != 1L ||
      is.na(muestra_validadores) || muestra_validadores < 1 ||
      (!is.infinite(muestra_validadores) &&
       muestra_validadores != floor(muestra_validadores))) {
    stop("`muestra_validadores` debe ser un entero positivo o Inf.", call. = FALSE)
  }
  muestra_validadores <- if (is.infinite(muestra_validadores)) Inf else {
    as.integer(muestra_validadores)
  }
  if (!is.numeric(max_filas_hallazgo) || length(max_filas_hallazgo) != 1L ||
      is.na(max_filas_hallazgo) || max_filas_hallazgo < 1 ||
      (!is.infinite(max_filas_hallazgo) &&
       max_filas_hallazgo != floor(max_filas_hallazgo))) {
    stop("`max_filas_hallazgo` debe ser un entero positivo o Inf.", call. = FALSE)
  }
  max_filas_hallazgo <- if (is.infinite(max_filas_hallazgo)) {
    Inf
  } else as.integer(max_filas_hallazgo)
  if (!is.numeric(umbral_orden_columnas) ||
      length(umbral_orden_columnas) != 1L ||
      is.na(umbral_orden_columnas) || umbral_orden_columnas <= 0 ||
      umbral_orden_columnas > 1) {
    stop("`umbral_orden_columnas` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.numeric(max_columnas_orden) || length(max_columnas_orden) != 1L ||
      is.na(max_columnas_orden) || max_columnas_orden < 2 ||
      max_columnas_orden != floor(max_columnas_orden)) {
    stop("`max_columnas_orden` debe ser un entero de al menos 2.", call. = FALSE)
  }
  max_columnas_orden <- as.integer(max_columnas_orden)
  if (!is.numeric(umbral_solapamiento_orden) ||
      length(umbral_solapamiento_orden) != 1L ||
      is.na(umbral_solapamiento_orden) ||
      umbral_solapamiento_orden < 0 || umbral_solapamiento_orden > 1) {
    stop("`umbral_solapamiento_orden` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.numeric(umbral_aritmetica) || length(umbral_aritmetica) != 1L ||
      is.na(umbral_aritmetica) || umbral_aritmetica <= 0 ||
      umbral_aritmetica > 1) {
    stop("`umbral_aritmetica` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.numeric(min_filas_aritmetica) ||
      length(min_filas_aritmetica) != 1L ||
      is.na(min_filas_aritmetica) || !is.finite(min_filas_aritmetica) ||
      min_filas_aritmetica < 3 ||
      min_filas_aritmetica != floor(min_filas_aritmetica)) {
    stop("`min_filas_aritmetica` debe ser un entero de al menos 3.",
         call. = FALSE)
  }
  min_filas_aritmetica <- as.integer(min_filas_aritmetica)
  if (!is.numeric(tolerancia_aritmetica) ||
      length(tolerancia_aritmetica) != 1L ||
      is.na(tolerancia_aritmetica) || !is.finite(tolerancia_aritmetica) ||
      tolerancia_aritmetica < 0) {
    stop("`tolerancia_aritmetica` debe ser un n\u00famero finito no negativo.",
         call. = FALSE)
  }
  if (!is.numeric(max_columnas_aritmetica) ||
      length(max_columnas_aritmetica) != 1L ||
      is.na(max_columnas_aritmetica) || max_columnas_aritmetica < 2 ||
      max_columnas_aritmetica != floor(max_columnas_aritmetica)) {
    stop("`max_columnas_aritmetica` debe ser un entero de al menos 2.",
         call. = FALSE)
  }
  max_columnas_aritmetica <- as.integer(max_columnas_aritmetica)
  if (!is.logical(casi_duplicados_vocabulario) ||
      length(casi_duplicados_vocabulario) != 1L ||
      is.na(casi_duplicados_vocabulario)) {
    stop("`casi_duplicados_vocabulario` debe ser TRUE o FALSE.", call. = FALSE)
  }
  if (!is.logical(ausencia_estructural) ||
      length(ausencia_estructural) != 1L || is.na(ausencia_estructural)) {
    stop("`ausencia_estructural` debe ser TRUE o FALSE.", call. = FALSE)
  }
  if (!is.logical(variantes_equifrecuentes_vocabulario) ||
      length(variantes_equifrecuentes_vocabulario) != 1L ||
      is.na(variantes_equifrecuentes_vocabulario)) {
    stop("`variantes_equifrecuentes_vocabulario` debe ser TRUE o FALSE.",
         call. = FALSE)
  }
  if (!is.numeric(max_asimetria_equifrecuente_vocabulario) ||
      length(max_asimetria_equifrecuente_vocabulario) != 1L ||
      is.na(max_asimetria_equifrecuente_vocabulario) ||
      max_asimetria_equifrecuente_vocabulario < 1) {
    stop(
      "`max_asimetria_equifrecuente_vocabulario` debe ser un numero mayor o igual a 1.",
      call. = FALSE
    )
  }
  if (!is.numeric(max_comparaciones_dependencias) ||
      length(max_comparaciones_dependencias) != 1L ||
      is.na(max_comparaciones_dependencias) ||
      max_comparaciones_dependencias < 1 ||
      (!is.infinite(max_comparaciones_dependencias) &&
       max_comparaciones_dependencias !=
         floor(max_comparaciones_dependencias))) {
    stop(
      "`max_comparaciones_dependencias` debe ser un entero positivo o Inf.",
      call. = FALSE
    )
  }
  for (argumento in c("max_trabajo_vocabulario", "max_trabajo_dependencias")) {
    valor <- get(argumento)
    if (!is.numeric(valor) || length(valor) != 1L || is.na(valor) ||
        valor <= 0 || (!is.infinite(valor) && valor != floor(valor))) {
      stop(
        "`", argumento, "` debe ser un entero positivo o Inf.",
        call. = FALSE
      )
    }
  }
  max_largo_valor_vocabulario <- .validar_largo_valor_duplicados(
    max_largo_valor_vocabulario, "max_largo_valor_vocabulario"
  )
  if (!is.logical(avisar_costo_tabla_ancha) ||
      length(avisar_costo_tabla_ancha) != 1L ||
      is.na(avisar_costo_tabla_ancha)) {
    stop("`avisar_costo_tabla_ancha` debe ser TRUE o FALSE.", call. = FALSE)
  }
  umbral_celdas_aviso_tabla_ancha <- .validar_limite_duplicados(
    umbral_celdas_aviso_tabla_ancha, "umbral_celdas_aviso_tabla_ancha"
  )
  costo_tabla_ancha <- .proyectar_costo_tabla_ancha(
    nrow(datos), ncol(datos), umbral_celdas_aviso_tabla_ancha
  )
  .avisar_costo_tabla_ancha(costo_tabla_ancha, avisar_costo_tabla_ancha)
  columnas_personales <- .normalizar_columnas_personales(
    columnas_personales, names(datos)
  )
  if (!is.numeric(max_proporcion_grupo_vocabulario) ||
      length(max_proporcion_grupo_vocabulario) != 1L ||
      is.na(max_proporcion_grupo_vocabulario) ||
      max_proporcion_grupo_vocabulario < 0 ||
      max_proporcion_grupo_vocabulario > 1) {
    stop("`max_proporcion_grupo_vocabulario` debe estar entre 0 y 1.",
         call. = FALSE)
  }
  if (!is.numeric(umbral_variante_rara_vocabulario) ||
      length(umbral_variante_rara_vocabulario) != 1L ||
      is.na(umbral_variante_rara_vocabulario) ||
      umbral_variante_rara_vocabulario < 0 ||
      umbral_variante_rara_vocabulario > 1) {
    stop("`umbral_variante_rara_vocabulario` debe estar entre 0 y 1.",
         call. = FALSE)
  }
  if (!is.numeric(min_asimetria_vocabulario_corto) ||
      length(min_asimetria_vocabulario_corto) != 1L ||
      is.na(min_asimetria_vocabulario_corto) ||
      !is.finite(min_asimetria_vocabulario_corto) ||
      min_asimetria_vocabulario_corto < 1) {
    stop("`min_asimetria_vocabulario_corto` debe ser al menos 1.",
         call. = FALSE)
  }
  if (!is.numeric(min_participacion_dominante_vocabulario_corto) ||
      length(min_participacion_dominante_vocabulario_corto) != 1L ||
      is.na(min_participacion_dominante_vocabulario_corto) ||
      min_participacion_dominante_vocabulario_corto < 0 ||
      min_participacion_dominante_vocabulario_corto > 1) {
    stop(paste0(
      "`min_participacion_dominante_vocabulario_corto` debe estar entre 0 y 1."
    ), call. = FALSE)
  }
  if (!is.logical(duplicados_aproximados) &&
      !is.list(duplicados_aproximados)) {
    stop("`duplicados_aproximados` debe ser FALSE, TRUE o una lista de argumentos.",
         call. = FALSE)
  }
  if (is.logical(duplicados_aproximados) &&
      (length(duplicados_aproximados) != 1L ||
       is.na(duplicados_aproximados))) {
    stop("`duplicados_aproximados` debe ser FALSE o TRUE.", call. = FALSE)
  }
  if (is.list(duplicados_aproximados) &&
      any(names(duplicados_aproximados) %in% c(
        "datos", "clasificacion", "perfil", "proteger_datos_personales",
        "normalizar"
      ))) {
    stop("`duplicados_aproximados` no puede reemplazar argumentos coordinados por perfilar().",
         call. = FALSE)
  }
  validadores_personales <- .normalizar_validadores_personales(
    validadores_personales
  )

  nombres <- names(datos)
  if (is.null(nombres)) {
    nombres <- paste0("V", seq_len(ncol(datos)))
  }
  nombres_lista <- make.unique(nombres)
  aplicabilidad_resuelta <- .resolver_aplicabilidad(
    datos, nombres, columnas_opcionales, aplicabilidad
  )
  resultados <- .medir_etapa_dbi(
    trazador_tiempos, "perfilado_columnas",
    lapply(seq_len(ncol(datos)), function(i) {
      .perfilar_columna(
        datos[[i]], nombres[[i]], muestra_diagnosticos, max_patrones,
        distinguir_mayusculas, expandir, umbral_patron_raro,
        sentinelas_numericos,
        aplicable = aplicabilidad_resuelta$mascaras[[i]]
      )
    })
  )

  columnas <- if (length(resultados)) {
    do.call(rbind, lapply(resultados, `[[`, "fila"))
  } else {
      .perfilar_columna(
      character(), "", muestra_diagnosticos, max_patrones,
      distinguir_mayusculas, expandir, umbral_patron_raro,
      sentinelas_numericos
    )$fila[0, , drop = FALSE]
  }
  rownames(columnas) <- NULL
  patrones <- lapply(resultados, `[[`, "patrones")
  formatos_fecha <- lapply(resultados, `[[`, "formatos")
  names(patrones) <- nombres_lista
  names(formatos_fecha) <- nombres_lista
  dependencias <- .medir_etapa_dbi(
    trazador_tiempos, "dependencias",
    if (analizar_dependencias) {
      detectar_dependencias(
        datos, umbral = umbral_dependencia, muestra = muestra_diagnosticos,
        max_columnas = max_columnas_dependencias,
        umbral_casi_clave = umbral_casi_clave_dependencia,
        max_comparaciones = max_comparaciones_dependencias,
        max_trabajo = max_trabajo_dependencias
      )
    } else {
      # `datos[0, 0]` sobre un objeto `sf` conserva la geometria: la columna es
      # pegajosa por diseno de ese paquete. Con la clase puesta, el objeto vacio
      # llegaba con una columna y el diagnostico declaraba un recorte que nadie
      # pidio -- las dependencias estaban apagadas, no truncadas.
      detectar_dependencias(
        as.data.frame(datos)[0, 0, drop = FALSE], umbral = umbral_dependencia,
        max_columnas = 1L,
        umbral_casi_clave = umbral_casi_clave_dependencia
      )
    },
    activa = analizar_dependencias
  )

  conteos_duplicados <- .conteos_filas_duplicadas(datos)
  n_filas_duplicadas <- conteos_duplicados$filas_duplicadas
  n_filas_en_grupos_duplicados <-
    conteos_duplicados$filas_en_grupos_duplicados
  filas_completas <- tryCatch(
    sum(stats::complete.cases(datos)),
    error = function(e) NA_integer_
  )
  duplicadas <- .columnas_duplicadas(datos, nombres)
  relaciones_orden <- .detectar_orden_columnas(
    datos, columnas, formatos_fecha,
    umbral = umbral_orden_columnas, max_columnas = max_columnas_orden,
    umbral_solapamiento = umbral_solapamiento_orden
  )
  relaciones_aritmeticas <- .detectar_aritmetica_columnas(
    datos, umbral = umbral_aritmetica,
    min_filas = min_filas_aritmetica,
    tolerancia = tolerancia_aritmetica,
    max_columnas = max_columnas_aritmetica
  )
  normalizacion_fusiones <- .normalizacion_fusiones_tabla(
    datos, normalizacion_resuelta
  )
  tipos <- table(vapply(seq_along(datos), function(i) {
    .tipo_declarado(datos[[i]])
  }, character(1L)))
  tipos_columnas <- data.frame(
    tipo = names(tipos), n = as.integer(tipos), stringsAsFactors = FALSE
  )
  general <- list(
    filas = nrow(datos),
    columnas = ncol(datos),
    celdas = as.numeric(nrow(datos)) * as.numeric(ncol(datos)),
    memoria_bytes = as.numeric(utils::object.size(datos)),
    filas_completas = filas_completas,
    filas_duplicadas = n_filas_duplicadas,
    filas_en_grupos_duplicados = n_filas_en_grupos_duplicados,
    tipos_columnas = tipos_columnas,
    columnas_duplicadas = duplicadas
  )
  hallazgos <- .construir_hallazgos(
    datos, resultados, nombres, duplicadas,
    umbral_alta_cardinalidad, umbral_faltantes_sospechoso,
    umbral_faltantes_error, umbral_patron_raro,
    umbral_patron_dominante, columnas_sin_ceros,
    columnas_no_negativas,
    if (is.na(n_filas_duplicadas)) 0L else n_filas_duplicadas,
    if (is.na(n_filas_en_grupos_duplicados)) 0L else
      n_filas_en_grupos_duplicados,
    clave_declarada = clave,
    evaluacion_clave = evaluacion_clave,
    relaciones_orden = relaciones_orden$hallazgos,
    relaciones_aritmeticas = relaciones_aritmeticas$hallazgos,
    normalizacion = normalizacion_resuelta,
    detectar_casi_duplicados = casi_duplicados_vocabulario,
    max_proporcion_grupo = max_proporcion_grupo_vocabulario,
    umbral_variante_rara = umbral_variante_rara_vocabulario,
    min_asimetria_variante = min_asimetria_vocabulario_corto,
    min_asimetria_general = min_asimetria_vocabulario,
    min_participacion_dominante =
      min_participacion_dominante_vocabulario_corto,
    detectar_variantes_equifrecuentes = variantes_equifrecuentes_vocabulario,
    max_asimetria_equifrecuente = max_asimetria_equifrecuente_vocabulario,
    max_trabajo = max_trabajo_vocabulario,
    max_largo_valor = max_largo_valor_vocabulario,
    trazador_tiempos = trazador_tiempos
  )
  cobertura_diagnosticos <- attr(
    hallazgos, "cobertura_diagnosticos", exact = TRUE
  )
  if (is.null(cobertura_diagnosticos)) {
    cobertura_diagnosticos <- .cobertura_diagnosticos_vacia()
  }
  cobertura_muestra <- .cobertura_muestra_perfilado(alcance_muestra)
  if (nrow(cobertura_muestra)) {
    cobertura_diagnosticos <- rbind(
      cobertura_diagnosticos, cobertura_muestra
    )
  }
  if (nrow(relaciones_aritmeticas$cobertura)) {
    cobertura_diagnosticos <- rbind(
      cobertura_diagnosticos, relaciones_aritmeticas$cobertura
    )
  }
  cobertura_dependencias <- .cobertura_dependencias(dependencias)
  if (!is.null(cobertura_dependencias)) {
    cobertura_diagnosticos <- rbind(
      cobertura_diagnosticos, cobertura_dependencias
    )
  }
  cobertura_aplicabilidad <- .cobertura_aplicabilidad(aplicabilidad_resuelta$reglas)
  if (!is.null(cobertura_aplicabilidad)) {
    cobertura_diagnosticos <- rbind(
      cobertura_diagnosticos, cobertura_aplicabilidad
    )
  }
  cobertura_indescifrable <- .cobertura_texto_indescifrable(datos, nombres)
  if (!is.null(cobertura_indescifrable)) {
    cobertura_diagnosticos <- rbind(
      cobertura_diagnosticos, cobertura_indescifrable
    )
  }
  attr(hallazgos, "cobertura_diagnosticos") <- NULL
  if (isTRUE(ausencia_estructural)) {
    estructural <- .medir_etapa_dbi(
      trazador_tiempos, "ausencia_estructural",
      .diagnosticar_ausencia_estructural(
        datos, nombres, resultados, aplicabilidad_resuelta,
        umbral_faltantes_error
      )
    )
    if (nrow(estructural$cobertura)) {
      cobertura_diagnosticos <- rbind(
        cobertura_diagnosticos, estructural$cobertura
      )
    }
    if (length(estructural$hallazgos)) {
      hallazgos <- .cruzar_faltantes_con_estructural(
        hallazgos, estructural$hallazgos
      )
      hallazgos <- do.call(rbind, c(list(hallazgos), estructural$hallazgos))
      hallazgos$severidad <- factor(
        as.character(hallazgos$severidad),
        levels = c("ok", "sospechoso", "error"), ordered = TRUE
      )
      rownames(hallazgos) <- NULL
    }
  } else {
    .registrar_etapa_dbi(
      trazador_tiempos, "ausencia_estructural", estado = "no_solicitado"
    )
  }
  benford <- .diagnosticar_benford(
    datos, columnas, hallazgos, clave_declarada = clave
  )
  if (nrow(benford$cobertura)) {
    cobertura_diagnosticos <- rbind(
      cobertura_diagnosticos, benford$cobertura
    )
  }
  if (length(benford$hallazgos)) {
    hallazgos <- do.call(rbind, c(list(hallazgos), benford$hallazgos))
    hallazgos$severidad <- factor(
      as.character(hallazgos$severidad),
      levels = c("ok", "sospechoso", "error"), ordered = TRUE
    )
    rownames(hallazgos) <- NULL
  }
  datos_personales <- .detectar_datos_personales(
    datos, nombres, resultados,
    validadores = validadores_personales,
    umbral_verificado = umbral_documento_verificado,
    muestra_validadores = muestra_validadores,
    declaradas = columnas_personales
  )
  indice_personal <- match(columnas$columna, datos_personales$columna)
  columnas$dato_personal_posible <- !is.na(indice_personal)
  columnas$tipo_dato_personal <- datos_personales$tipo[indice_personal]
  columnas$proporcion_dato_personal <-
    datos_personales$proporcion_compatible[indice_personal]
  columnas$poder_discriminante_dato_personal <-
    datos_personales$poder_discriminante[indice_personal]
  # El campo dice si el valor **quedo** protegido, no si la clasificacion
  # pensaba protegerlo: con `proteger_datos_personales = FALSE` la moda se ve, y
  # decir `TRUE` al lado de un valor visible es informar como hecho algo que no
  # paso. La intencion de la clasificacion sigue disponible en
  # `datos_personales$proteger`.
  columnas$dato_personal_protegido <- isTRUE(proteger_datos_personales) & ifelse(
    is.na(indice_personal), FALSE, datos_personales$proteger[indice_personal]
  )
  hallazgos_personales <- .hallazgos_datos_personales(
    datos_personales, datos_personales_permitidos
  )
  hallazgos_personales <- c(
    hallazgos_personales,
    .hallazgos_rango_nacimiento(columnas, datos_personales, fecha_hora)
  )
  if (length(hallazgos_personales)) {
    hallazgos <- do.call(rbind, c(list(hallazgos), hallazgos_personales))
    hallazgos$severidad <- factor(
      as.character(hallazgos$severidad),
      levels = c("ok", "sospechoso", "error"), ordered = TRUE
    )
    rownames(hallazgos) <- NULL
  }
  aproximados <- if (is.logical(duplicados_aproximados) &&
      !duplicados_aproximados) {
    .registrar_etapa_dbi(
      trazador_tiempos, "duplicados_aproximados", estado = "no_solicitado"
    )
    NULL
  } else {
    configuracion <- if (isTRUE(duplicados_aproximados)) {
      list()
    } else duplicados_aproximados
    .medir_etapa_dbi(
      trazador_tiempos, "duplicados_aproximados",
      do.call(
        .detectar_duplicados_aproximados,
        c(
          list(
            datos = datos, clasificacion = datos_personales,
            normalizar = normalizacion_resuelta,
            proteger_datos_personales = proteger_datos_personales,
            fusiones_precomputadas = normalizacion_fusiones
          ),
          configuracion
        )
      )
    )
  }
  if (!is.null(aproximados) && nrow(aproximados$hallazgos)) {
    hallazgos <- rbind(hallazgos, aproximados$hallazgos)
    rownames(hallazgos) <- NULL
  }
  hallazgos <- .agregar_trazabilidad_hallazgos(
    hallazgos, datos, nombres, resultados, expandir = expandir,
    aproximados = aproximados, limite = max_filas_hallazgo,
    distinguir_mayusculas = distinguir_mayusculas, clave = clave
  )
  meta <- list(
    nombre = nombre,
    fecha_hora = fecha_hora,
    version = .version_paquete(),
    entrada_convertida = entrada_convertida,
    columnas_personales_declaradas = names(columnas_personales),
    ausencia_estructural = ausencia_estructural,
    filas_totales = nrow(datos),
    filas_analizadas = alcance_muestra$filas_efectivas,
    muestreo = nrow(datos) > alcance_muestra$filas_efectivas,
    muestra = muestra,
    muestra_efectiva = alcance_muestra$filas_efectivas,
    celdas_solicitadas = alcance_muestra$celdas_solicitadas,
    celdas_efectivas = alcance_muestra$celdas_efectivas,
    max_celdas_muestra = max_celdas_muestra,
    max_bytes_muestra = max_bytes_muestra,
    bytes_sonda = alcance_muestra$bytes_sonda,
    bytes_muestra = alcance_muestra$bytes_muestra,
    max_patrones = max_patrones,
    distinguir_mayusculas = distinguir_mayusculas,
    expandir = expandir,
    umbral_patron_raro = umbral_patron_raro,
    analizar_dependencias = analizar_dependencias,
    umbral_dependencia = umbral_dependencia,
    umbral_casi_clave_dependencia = umbral_casi_clave_dependencia,
    max_columnas_dependencias = max_columnas_dependencias,
    max_comparaciones_dependencias = max_comparaciones_dependencias,
    max_trabajo_dependencias = max_trabajo_dependencias,
    trabajo_dependencias = list(
      estimado = attr(dependencias, "trabajo_estimado", exact = TRUE),
      comparado = attr(dependencias, "trabajo_comparado", exact = TRUE),
      sin_comparar = attr(dependencias, "trabajo_sin_comparar", exact = TRUE),
      unidad = attr(dependencias, "unidad_trabajo", exact = TRUE),
      maximo = attr(dependencias, "max_trabajo", exact = TRUE)
    ),
    max_trabajo_vocabulario = max_trabajo_vocabulario,
    max_largo_valor_vocabulario = max_largo_valor_vocabulario,
    avisar_costo_tabla_ancha = avisar_costo_tabla_ancha,
    umbral_celdas_aviso_tabla_ancha = umbral_celdas_aviso_tabla_ancha,
    costo_tabla_ancha = costo_tabla_ancha,
    sentinelas_numericos = .numeros_na(sentinelas_numericos),
    datos_personales_permitidos = datos_personales_permitidos,
    proteger_datos_personales = proteger_datos_personales,
    validadores_personales = names(validadores_personales),
    umbral_documento_verificado = umbral_documento_verificado,
    muestra_validadores = muestra_validadores,
    max_filas_hallazgo = max_filas_hallazgo,
    umbral_orden_columnas = umbral_orden_columnas,
    max_columnas_orden = max_columnas_orden,
    umbral_solapamiento_orden = umbral_solapamiento_orden,
    casi_duplicados_vocabulario = casi_duplicados_vocabulario,
    max_proporcion_grupo_vocabulario = max_proporcion_grupo_vocabulario,
    umbral_variante_rara_vocabulario =
      umbral_variante_rara_vocabulario,
    min_asimetria_vocabulario_corto = min_asimetria_vocabulario_corto,
    min_participacion_dominante_vocabulario_corto =
      min_participacion_dominante_vocabulario_corto,
    orden_columnas = relaciones_orden$alcance,
    normalizacion = normalizacion_resuelta,
    normalizacion_resumen = .normalizacion_resumen(normalizacion_resuelta),
    normalizacion_fusiones = normalizacion_fusiones
  )
  # Una clave plenamente verificada no cambia el objeto histórico: su presencia
  # ya queda en `trazabilidad$localizador`. Cuando hay algo que impide llamarla
  # garantía, la explicación queda junto a esa trazabilidad y no depende de que
  # el consumidor haya visto el warning.
  if (!is.null(evaluacion_clave) &&
      isTRUE(evaluacion_clave$requiere_declaracion)) {
    meta$clave <- evaluacion_clave
  }
  if (!is.null(benford$meta)) {
    meta$benford <- benford$meta
  }
  if (length(relaciones_aritmeticas$hallazgos) ||
      isTRUE(relaciones_aritmeticas$alcance$truncado)) {
    meta$aritmetica_columnas <- relaciones_aritmeticas$alcance
  }
  estructura <- list(
    general = general,
    columnas = columnas,
    patrones = patrones,
    formatos_fecha = formatos_fecha,
    dependencias = dependencias,
    hallazgos = hallazgos,
    cobertura_diagnosticos = cobertura_diagnosticos,
    datos_personales = datos_personales,
    meta = meta
  )
  if (!is.null(aproximados)) {
    estructura$duplicados_aproximados <- aproximados
    estructura <- estructura[c(
      "general", "columnas", "patrones", "formatos_fecha", "dependencias",
      "duplicados_aproximados", "hallazgos", "cobertura_diagnosticos",
      "datos_personales", "meta"
    )]
  }
  class(estructura) <- "perfil"
  if (proteger_datos_personales) estructura <- .proteger_perfil(estructura)
  estructura
}
