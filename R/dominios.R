# Reglas de dominio: una declaracion puede afirmar un limite, mientras que el
# lexico solo puede proponerlo con una severidad mas baja.

.TIPOS_HALLAZGO_DOMINIO <- c(
  "valor_fuera_dominio", "fecha_futura", "fecha_centinela"
)

.CENTINELAS_FECHA_DOMINIO <- c(
  "1900-01-01", "9999-12-31", "1970-01-01", "1899-12-30"
)

# Una regla inferida por el nombre se aplica fila por fila (hallazgo
# `sospechoso` con las filas afuera) mientras los valores de afuera sean la
# minoria de hasta este tope. Pasado el tope la columna no sostiene la regla:
# el hallazgo es de la columna (severidad `ok`, sin acusar filas). El corte se
# decide con enteros (`fuera * 10 <= n`): 11 de 100 y 3 de 20 pasan el tope,
# 10 de 100 y 2 de 20 no.
.UMBRAL_DOMINIO_INFERIDO_FUERA <- 0.10
# Con el 90% o mas de la columna afuera y, en mediana, a mas de un ancho del
# dominio de su borde, la columna es un reescalado consistente (microgrados en
# vez de grados): lo imposible no se acusa fila por fila, el aviso es el de la
# columna, que nombra la escala probable.
# Una columna es un reescalado si al dividirla por un factor de una lista corta
# el 95% o mas de sus valores cae en el dominio y antes de dividir al menos la
# mitad estaba afuera, lejos del dominio (mas de un ancho en mediana). Los factores son los de un error de unidad habitual:
# decimales (prefijos: 10, 100, 1000, 10^4, 10^5, 10^6) y, para una edad en anios,
# solo los del tiempo (12 meses, 52 semanas, 365 dias, 8760 horas): una edad
# nunca esta "multiplicada por 1000". Un factor de tiempo solo cuenta si la
# columna dividida no queda reducida a casi nada (q95 >= 2% del maximo): sin eso
# 8760 explicaria cualquier columna que 365 ya explica. Se dejan afuera 24, 60 y
# 3600: no son una edad en anios expresada en otra unidad.
.UMBRAL_REESCALADO_DENTRO <- 0.95
# La parte de una longitud que pasa de 180 tiene que ser sustancial (>= 5 valores
# y >= 10%) para leerse como la convencion de 0 a 360; con menos son errores.
.MIN_VALORES_LONGITUD_360 <- 5L
.UMBRAL_LONGITUD_360 <- 0.10
.UMBRAL_REESCALADO_FUERA <- 0.50
.UMBRAL_REESCALADO_DISTANCIA <- 0.5
.FACTORES_REESCALADO_DECIMALES <- c(10, 100, 1000, 1e4, 1e5, 1e6)
.FACTORES_REESCALADO_TIEMPO <- c(`meses` = 12, `semanas` = 52, `dias` = 365,
                                 `horas` = 8760)
.UMBRAL_DOMINIO_OTRA_UNIDAD_ANCHOS <- 1
.UMBRAL_HUECO_CENTINELA_FECHA <- 0.05
.UMBRAL_HUECO_GRANDE_CENTINELA_FECHA <- 0.15
.UMBRAL_PROPORCION_PLAUSIBLE_CENTINELA <- 0.05
.MIN_REPETICIONES_CENTINELA_FECHA <- 2L
.MIN_REPETICIONES_CENTINELA_1970 <- 3L
# Un centinela inferido que es la mayoria de la columna no es un valor suelto:
# la columna lo usa como convencion (inicio o fin de vigencia abierto).
.UMBRAL_CENTINELA_MAYORIA <- 0.5
.MIN_VALORES_GUARDA_DOMINIO <- 5L

.PALABRAS_EDAD_OBJETO <- c(
  "edificio", "edificios", "building", "buildings", "arbol", "arboles",
  "tree", "trees", "planta", "plantas", "obra", "obras", "inmueble",
  "inmuebles", "antiguedad", "antiguedades", "geologico", "geologica"
)

.PALABRAS_PERSONA_EDAD <- c(
  "madre", "padre", "hijo", "hija", "alumno", "alumna", "empleado", "empleada",
  "person", "student", "employee", "mother", "father", "child",
  "persona", "personas", "humano", "humanos", "paciente", "pacientes",
  "cliente", "clientes", "socio", "socios", "usuario", "usuarios",
  "estudiante", "estudiantes", "nino", "ninos", "adulto", "adultos",
  "persona", "people", "human", "patient", "client", "customer", "user"
)

.PALABRAS_PORCENTAJE_ABIERTO <- c(
  "variacion", "variaciones", "cambio", "cambios", "crecimiento",
  "crecimientos", "retorno", "retornos", "rendimiento", "rendimientos",
  "inflacion", "inflacionaria", "inflacionario", "variability",
  "change", "changes", "growth", "delta", "deltas", "error", "errores",
  "descuento", "descuentos", "discount", "aumento", "aumentos",
  "incremento", "incrementos", "increase", "diferencia", "diferencias",
  "yoy", "roi", "cumplimiento", "utilizacion", "utilization",
  "var", "diff", "crec", "margin", "margen", "profit", "utilidad"
)

# Unidades de tiempo en el nombre: una `edad_*_dias` no es una edad en anios.
.PALABRAS_UNIDAD_EDAD <- c(
  "dia", "dias", "day", "days", "mes", "meses", "month", "months",
  "semana", "semanas", "week", "weeks", "hora", "horas", "hour", "hours",
  "minuto", "minutos", "segundo", "segundos", "gestacional", "tenure",
  "cuenta", "account"
)

# Palabras que hacen del porcentaje uno relativo a otra cosa (`pct_of_sales`,
# `porcentaje_sobre_costo`): puede pasar de 100 sin ser un error.
.PALABRAS_PORCENTAJE_RELATIVO <- c(
  "of", "over", "sobre", "vs", "respecto", "per"
)
# `de`, `del` y `por` son ambiguas: `porcentaje_de_avance` esta acotado y
# `porcentaje_del_costo` puede pasar de 100. Infieren el dominio solo para el
# aviso de la columna, nunca para acusar filas.
.PALABRAS_PORCENTAJE_AMBIGUO <- c("de", "del", "por")

# Valores que se usan como ausencia: si lo repetido de afuera es uno de ellos, es
# un codigo y no una escala.
.SENTINELAS_DOMINIO <- c(-1, -9, -99, -999, -9999, 999, 9999, 99999)

# Palabras que acompanan a `longitud` sin cambiar que es una coordenada.
.PALABRAS_COORDENADA <- c(
  "coord", "coordenada", "coordenadas", "grados", "deg", "decimal", "gps",
  "inicio", "fin", "origen", "destino", "start", "end", "from", "to",
  "min", "max", "x", "y"
)

.PALABRAS_NACIMIENTO <- c(
  "nacimiento", "nac", "fnac", "fecnac", "birth", "birthdate", "dob", "born",
  "nacido"
)

.TOKENS_LATITUD_COMPLETA <- c("latitud", "latitude")
.TOKENS_LONGITUD_COMPLETA <- c("longitud", "longitude")
.TOKENS_LONGITUD_ABREVIADA <- c("lon", "lng")
.PALABRAS_COORDENADA_RELATIVA <- c(
  "rel", "relativa", "relativo", "relative", "delta", "dif", "diff", "offset",
  "desv", "error", "err"
)
# Un 9999-12-31 en estos nombres es la convencion de "sin fin": vigencia abierta.
.PALABRAS_FIN_VIGENCIA <- c(
  "hasta", "fin", "vigencia", "vencimiento", "until", "end", "expiry"
)

.PALABRAS_FECHA_FUTURA <- c(
  "vencimiento", "vencimientos", "vence", "expiracion", "expiraciones",
  "caducidad", "caducidades", "cita", "citas", "turno", "turnos",
  "reserva", "reservas", "evento", "eventos", "inicio", "inicios",
  "fin", "fines", "entrega", "entregas", "appointment", "appointments",
  "due", "expiry", "expiration", "start", "end", "delivery",
  "prevista", "previsto", "previstas", "previstos", "programada",
  "programado", "programadas", "programados", "planificada", "planificado",
  "planned", "scheduled", "estimada", "estimado", "estimadas", "estimados",
  "estimated", "limite", "limites", "deadline", "proxima", "proximo",
  "proximas", "proximos", "next", "renovacion", "renovaciones", "renewal",
  "valid", "until", "hasta", "vigencia", "plazo", "plazos", "forecast",
  "pronostico", "pronosticos", "target", "objetivo", "esperada", "esperado",
  "expected", "pago", "pagos", "baja", "bajas", "cierre", "cierres",
  "publicacion", "publicaciones", "envio", "envios", "retiro", "retiros"
)

.dominio_nombre_normalizado <- function(nombre) {
  .normalizar_nombre_fecha(.separar_camel(
    .nombre_para_clasificar(nombre)
  ))
}

.dominio_tokens <- function(nombre) {
  normalizado <- tryCatch(
    .dominio_nombre_normalizado(nombre),
    error = function(e) ""
  )
  partes <- strsplit(normalizado, "_", fixed = TRUE)[[1L]]
  partes[nzchar(partes)]
}

.dominio_texto_centinelas <- function(x) {
  if (inherits(x, "Date")) return(as.character(x))
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }
  as.character(x)
}

.normalizar_regla_dominio <- function(regla, columna) {
  if (!is.list(regla) || is.null(names(regla)) ||
      any(!nzchar(names(regla)))) {
    .detener(
      "Cada dominio declarado debe ser una lista con campos nombrados en `",
      columna, "`.", call. = FALSE
    )
  }
  permitidos <- c("minimo", "maximo", "no_futuras", "centinelas")
  desconocidos <- setdiff(names(regla), permitidos)
  if (length(desconocidos)) {
    .detener(
      "`dominios` tiene campos desconocidos en `", columna, "`: ",
      paste(desconocidos, collapse = ", "), ".", call. = FALSE
    )
  }
  if (!length(regla)) {
    .detener("El dominio de `", columna, "` no puede estar vacio.", call. = FALSE)
  }
  salida <- list()
  for (campo in c("minimo", "maximo")) {
    if (!campo %in% names(regla)) next
    valor <- regla[[campo]]
    if (!is.numeric(valor) || length(valor) != 1L || is.na(valor) ||
        !is.finite(valor)) {
      .detener(
        "`", campo, "` de `dominios` debe ser un numero finito escalar en `",
        columna, "`.", call. = FALSE
      )
    }
    salida[[campo]] <- as.numeric(valor)
  }
  if (all(c("minimo", "maximo") %in% names(salida)) &&
      salida$minimo > salida$maximo) {
    .detener(
      "`minimo` no puede ser mayor que `maximo` en el dominio de `",
      columna, "`.", call. = FALSE
    )
  }
  if ("no_futuras" %in% names(regla)) {
    valor <- regla$no_futuras
    if (!is.logical(valor) || length(valor) != 1L || is.na(valor)) {
      .detener(
        "`no_futuras` debe ser TRUE o FALSE en el dominio de `", columna,
        "`.", call. = FALSE
      )
    }
    salida$no_futuras <- isTRUE(valor)
  }
  if ("centinelas" %in% names(regla)) {
    valor <- regla$centinelas
    if (!is.character(valor) && !inherits(valor, c("Date", "POSIXt"))) {
      .detener(
        "`centinelas` debe contener fechas como texto, Date o POSIXt en `",
        columna, "`.", call. = FALSE
      )
    }
    valor <- .dominio_texto_centinelas(valor)
    if (anyNA(valor) || !length(valor) || any(!nzchar(trimws(valor)))) {
      .detener(
        "`centinelas` no puede contener ausentes ni textos vacios en `",
        columna, "`.", call. = FALSE
      )
    }
    salida$centinelas <- unique(valor)
    invisible(.dominio_fecha_numero(salida$centinelas, columna))
  }
  if (!length(salida)) {
    .detener("El dominio de `", columna, "` no tiene una regla.", call. = FALSE)
  }
  salida
}

.normalizar_dominios <- function(dominios, nombres) {
  inferir <- TRUE
  declarados <- list()
  if (is.logical(dominios)) {
    if (length(dominios) != 1L || is.na(dominios) || isTRUE(dominios)) {
      .detener("`dominios` debe ser NULL, FALSE o una lista nombrada.", call. = FALSE)
    }
    inferir <- FALSE
  } else if (!is.null(dominios)) {
    if (!is.list(dominios) || is.null(names(dominios)) ||
        anyNA(names(dominios)) || any(!nzchar(names(dominios)))) {
      .detener(
        "`dominios` debe ser una lista nombrada por columnas, o FALSE.",
        call. = FALSE
      )
    }
    if (anyDuplicated(.nombres_para_operar(names(dominios)))) {
      .detener("`dominios` no puede repetir columnas.", call. = FALSE)
    }
    indices <- .indice_nombre(names(dominios), nombres)
    if (anyNA(indices)) {
      .detener(
        "`dominios` nombra columnas inexistentes: ",
        paste(names(dominios)[is.na(indices)], collapse = ", "), ".",
        call. = FALSE
      )
    }
    nombres_reales <- nombres[indices]
    for (i in seq_along(dominios)) {
      valor <- dominios[[i]]
      if (is.logical(valor) && length(valor) == 1L && !is.na(valor) &&
          !isTRUE(valor)) {
        posicion <- length(declarados) + 1L
        declarados[[posicion]] <- FALSE
      } else {
        posicion <- length(declarados) + 1L
        declarados[[posicion]] <- .normalizar_regla_dominio(
          valor, nombres_reales[[i]]
        )
      }
      names(declarados)[[posicion]] <- nombres_reales[[i]]
    }
  } else if (!is.null(dominios) && !is.logical(dominios)) {
    .detener("`dominios` debe ser NULL, FALSE o una lista nombrada.", call. = FALSE)
  }
  if (length(declarados)) {
    orden <- .orden_por_bytes(.clave_bytes(names(declarados)), method = "radix")
    declarados <- declarados[orden]
  }
  list(inferir = inferir, declarados = declarados)
}

.dominio_regla_inferida <- function(nombre, tokens, nombres, indice, resultados) {
  tipo <- as.character(resultados[[indice]]$inferencia$tipo)
  tokens_sin_objeto <- !any(tokens %in% .PALABRAS_EDAD_OBJETO) &&
    !any(tokens %in% .PALABRAS_UNIDAD_EDAD)
  edad <- any(tokens %in% c("edad", "age")) && tokens_sin_objeto &&
    (length(tokens) == 1L || any(tokens %in% .PALABRAS_PERSONA_EDAD) ||
     any(tokens %in% c("anios", "anos", "years", "year", "yrs")))
  if (edad) {
    return(list(
      minimo = 0, maximo = 130, tipo_dominio = "edad",
      fuente = "nombre_y_contenido"
    ))
  }
  porcentaje <- any(tokens %in% c(
    "porcentaje", "pct", "percent", "proporcion", "participacion"
  )) && !any(tokens %in% .PALABRAS_PORCENTAJE_ABIERTO)
  if (porcentaje) {
    # Un porcentaje relativo a otra cosa (`pct_of_sales`) puede pasar de 100:
    # el nombre no basta para inferir un dominio, ni siquiera por fila.
    if (any(tokens %in% .PALABRAS_PORCENTAJE_RELATIVO)) return(NULL)
    regla <- list(
      minimo = 0, maximo = 100, tipo_dominio = "porcentaje",
      fuente = "nombre_y_contenido"
    )
    if (any(tokens %in% .PALABRAS_PORCENTAJE_AMBIGUO)) regla$solo_aviso <- TRUE
    return(regla)
  }
  # Coordenadas. Los nombres completos (`latitud`, `longitude`) bastan; las
  # abreviaturas (`lat`, `lon`, `lng`) necesitan la columna pareja en la tabla,
  # completa o abreviada, y admiten calificativos de coordenada (`coord_lat`,
  # `lat_inicio`). `lat` o `lon` solas no infieren: son ambiguas (`latencia`,
  # `lona`, `long_cable` no tienen nada que ver y no pasan por aca).
  tiene_latitud <- any(tokens %in% .TOKENS_LATITUD_COMPLETA)
  tiene_longitud <- any(tokens %in% .TOKENS_LONGITUD_COMPLETA)
  relativa <- any(tokens %in% .PALABRAS_COORDENADA_RELATIVA)
  sin_calificar <- function(base) {
    !length(setdiff(tokens, c(base, .PALABRAS_COORDENADA)))
  }
  otras <- setdiff(seq_along(nombres), indice)
  tokens_otras <- lapply(nombres[otras], .dominio_tokens)
  # La columna pareja tiene que ser una coordenada sin calificativo de otra cosa
  # (`longitud_cable`, `longitud_texto` y `latitud_rel` no habilitan nada): se usa
  # la misma lista de palabras que ya excluye a la propia columna.
  es_coordenada <- function(t, base) {
    any(t %in% base) && !length(setdiff(t, c(base, .PALABRAS_COORDENADA)))
  }
  pareja_lon <- any(vapply(tokens_otras, es_coordenada, logical(1L),
                           base = c(.TOKENS_LONGITUD_COMPLETA,
                                    .TOKENS_LONGITUD_ABREVIADA)))
  pareja_lat <- any(vapply(tokens_otras, es_coordenada, logical(1L),
                           base = c(.TOKENS_LATITUD_COMPLETA, "lat")))
  hay_latitud_en_tabla <- pareja_lat
  # Si la tabla ya tiene su latitud (o longitud) con nombre completo, una
  # abreviatura sobrante no es de ese par: no se infiere.
  latitud_completa_aparte <- any(vapply(tokens_otras, function(t) {
    any(t %in% .TOKENS_LATITUD_COMPLETA)
  }, logical(1L)))
  longitud_completa_aparte <- any(vapply(tokens_otras, function(t) {
    any(t %in% .TOKENS_LONGITUD_COMPLETA)
  }, logical(1L)))
  if (!relativa && (tiene_latitud ||
      ("lat" %in% tokens && sin_calificar("lat") && pareja_lon &&
       !latitud_completa_aparte))) {
    return(list(
      minimo = -90, maximo = 90, tipo_dominio = "latitud",
      fuente = "nombre_y_contenido"
    ))
  }
  if (!relativa && (tiene_longitud ||
      (any(tokens %in% .TOKENS_LONGITUD_ABREVIADA) &&
       sin_calificar(.TOKENS_LONGITUD_ABREVIADA) && pareja_lat &&
       !longitud_completa_aparte))) {
    ingles <- any(tokens %in% "longitude")
    calificada <- length(setdiff(tokens, c(
      .TOKENS_LONGITUD_COMPLETA, .PALABRAS_COORDENADA
    ))) > 0L
    # `longitud` es tambien el largo de algo: sin una latitud en la tabla, o con
    # palabras que no son de coordenada, el nombre no basta para inferir.
    if (tiene_longitud && !ingles && (calificada || !hay_latitud_en_tabla)) {
      return(NULL)
    }
    return(list(
      minimo = -180, maximo = 180, tipo_dominio = "longitud",
      fuente = "nombre_y_contenido"
    ))
  }
  if (tipo %in% c("fecha", "fecha-hora")) {
    futuro_legitimo <- any(tokens %in% .PALABRAS_FECHA_FUTURA)
    regla <- list(
      no_futuras = !futuro_legitimo,
      centinelas = .CENTINELAS_FECHA_DOMINIO,
      tipo_dominio = "fecha",
      fuente = "contenido_y_nombre"
    )
    # Nadie nace en el futuro: esta fecha futura es imposible, no una cuestion
    # de unidad o de escala.
    if (!futuro_legitimo && any(tokens %in% .PALABRAS_NACIMIENTO)) {
      regla$imposible_futuras <- TRUE
    }
    if (any(tokens %in% .PALABRAS_FIN_VIGENCIA)) regla$fin_vigencia <- TRUE
    return(regla)
  }
  NULL
}

.validar_tipo_regla_dominio <- function(regla, resultado, columna) {
  tipo <- as.character(resultado$inferencia$tipo)
  es_fecha <- tipo %in% c("fecha", "fecha-hora")
  es_numerica <- tipo %in% c("entero", "doble", "integer64")
  tiene_rango <- !is.null(regla$minimo) || !is.null(regla$maximo)
  if (tiene_rango && es_fecha) {
    .detener(
      "El dominio numerico declarado en `", columna,
      "` no puede aplicarse a una fecha.", call. = FALSE
    )
  }
  if (tiene_rango && !es_numerica) {
    .detener(
      "El dominio numerico declarado en `", columna,
      "` requiere una columna numerica; el tipo inferido es `", tipo, "`.",
      call. = FALSE
    )
  }
  if (isTRUE(regla$no_futuras) && !es_fecha) {
    .detener(
      "`no_futuras` declarado en `", columna,
      "` requiere una columna de fecha.", call. = FALSE
    )
  }
  if (!is.null(regla$centinelas) && !es_fecha) {
    .detener(
      "`centinelas` declarado en `", columna,
      "` requiere una columna de fecha.", call. = FALSE
    )
  }
}

.dominio_fecha_numero <- function(x, columna) {
  salida <- suppressWarnings(.strptime_utc(x, "%Y-%m-%d"))
  sin_lectura <- is.na(salida)
  if (any(sin_lectura)) {
    salida[sin_lectura] <- suppressWarnings(.strptime_utc(
      x[sin_lectura], "%Y-%m-%d %H:%M:%S"
    ))
  }
  if (anyNA(salida)) {
    .detener(
      "`centinelas` contiene una fecha ilegible en `", columna, "`.",
      call. = FALSE
    )
  }
  as.numeric(salida)
}

.dominio_valores <- function(x, resultado, aplicable) {
  texto <- tryCatch(.texto_analizable(x)$valores, error = function(e) NULL)
  if (is.null(texto) || length(texto) != length(aplicable)) return(NULL)
  cuantitativo <- tryCatch(
    .valores_cuantitativos(
      texto, resultado$inferencia, resultado$formatos,
      sentinelas_numericos = numeric()
    ),
    error = function(e) NULL
  )
  if (is.null(cuantitativo) ||
      !(cuantitativo$clase %in% c("numero", "integer64", "fecha", "fecha-hora"))) {
    return(NULL)
  }
  valores <- tryCatch(suppressWarnings(as.numeric(cuantitativo$valores)),
                      error = function(e) NULL)
  if (is.null(valores) || length(valores) != length(aplicable)) return(NULL)
  usable <- as.logical(aplicable) & !is.na(valores) & is.finite(valores)
  list(
    valores = valores, usable = usable,
    fecha = cuantitativo$clase %in% c("fecha", "fecha-hora")
  )
}

.dominio_evidencia_numero <- function(regla) {
  partes <- c(
    if (!is.null(regla$minimo)) paste0("minimo=", regla$minimo),
    if (!is.null(regla$maximo)) paste0("maximo=", regla$maximo)
  )
  paste(partes, collapse = ", ")
}

.dominio_centinela_aislado <- function(valores, candidato) {
  valores <- valores[!is.na(valores) & is.finite(valores)]
  if (!length(valores)) return(FALSE)
  otros <- valores[abs(valores - candidato) != 0]
  if (!length(otros)) return(TRUE)
  amplitud <- diff(range(valores))
  if (!is.finite(amplitud) || amplitud <= 0) return(FALSE)
  hueco <- min(abs(otros - candidato)) / amplitud
  proporcion <- mean(abs(valores - candidato) == 0)
  hueco > .UMBRAL_HUECO_CENTINELA_FECHA &&
    (proporcion < .UMBRAL_PROPORCION_PLAUSIBLE_CENTINELA ||
     hueco > .UMBRAL_HUECO_GRANDE_CENTINELA_FECHA)
}

.dominio_centinelas_repetidos <- function(valores, indices, centinelas,
                                          declarada, columna,
                                          fin_abierto = FALSE) {
  vacio <- list(indices = integer(), observados = character())
  if (!length(centinelas)) return(vacio)
  candidatos <- .dominio_fecha_numero(centinelas, columna)
  validos <- vapply(seq_along(candidatos), function(j) {
    valor <- candidatos[[j]]
    encontrados <- indices[abs(valores[indices] - valor) == 0]
    # 1970-01-01 es tambien una fecha real (el origen de muchos sistemas): dos
    # apariciones no la vuelven centinela.
    minimo <- if (identical(centinelas[[j]], "1970-01-01")) {
      .MIN_REPETICIONES_CENTINELA_1970
    } else if (fin_abierto && identical(centinelas[[j]], "9999-12-31")) {
      1L
    } else .MIN_REPETICIONES_CENTINELA_FECHA
    # El aislamiento se mide sin los otros centinelas: un 9999 no puede
    # esconder un 1900.
    base <- valores[indices]
    otros <- candidatos[-j]
    base <- base[!(base %in% otros)]
    declarada || (length(encontrados) >= minimo &&
                    .dominio_centinela_aislado(base, valor))
  }, logical(1L))
  if (!any(validos)) return(vacio)
  candidatos_validos <- candidatos[validos]
  coincide <- rowSums(abs(outer(
    valores[indices], candidatos_validos, "-"
  )) == 0) > 0
  list(indices = indices[coincide], observados = centinelas[validos])
}

.dominio_no_futuro <- function(valores, referencia, fecha_es_date) {
  if (isTRUE(fecha_es_date)) {
    return(floor(valores / 86400) <= floor(referencia / 86400))
  }
  valores <= referencia
}

# Los limites que ningun valor legitimo cruza, sea cual sea la unidad: una
# edad negativa, una coordenada fuera de su esfera. Un limite que depende de la
# escala (edad > 130, porcentaje > 100) no es de estos.
.dominio_limites_imposibles <- function(regla) {
  tipo <- as.character(regla$tipo_dominio %||% "")
  if (tipo %in% c("edad", "porcentaje")) {
    return(list(minimo = 0, maximo = NULL))
  }
  if (tipo %in% c("latitud", "longitud")) {
    return(list(minimo = regla$minimo, maximo = regla$maximo))
  }
  list(minimo = NULL, maximo = NULL)
}

.dominio_fuera_de <- function(v, minimo, maximo) {
  (if (is.null(minimo)) FALSE else v < minimo) |
    (if (is.null(maximo)) FALSE else v > maximo)
}

# Decide como se lee una regla INFERIDA sobre los valores de la columna. Cada
# componente (`rango`, `futuras`, `centinelas`) toma uno de dos modos:
# - "fila": la columna sostiene la regla; lo de afuera es un valor suelto y se
#   publica como hallazgo `sospechoso` con sus filas;
# - "columna": lo de afuera pasa del tope de una minoria, asi que o la columna
#   no esta en la unidad o el dominio que el nombre sugiere, o tiene un error de
#   carga sistematico. Los datos no deciden entre las dos lecturas: se publica
#   un aviso de la columna con las dos, sin acusar filas.
# Nunca se calla: lo que cae afuera de una regla inferida siempre produce algo.
# Con menos de cinco valores evaluables no hay con que medir el sosten y se lee
# por fila. Lo que se decide aca no depende de cuantas columnas ni grupos haya.
.dominio_decidir_inferida <- function(valores, regla, fecha_hora,
                                      fecha_es_date) {
  salida <- list(rango = "fila", futuras = "fila", centinelas = "fila",
                 aplica = TRUE, hay = FALSE)
  indices <- which(valores$usable)
  if (!length(indices)) {
    salida$aplica <- FALSE
    return(salida)
  }
  n <- length(indices)
  suficiente <- n >= .MIN_VALORES_GUARDA_DOMINIO
  tope <- .UMBRAL_DOMINIO_INFERIDO_FUERA
  if (!is.null(regla$minimo) || !is.null(regla$maximo)) {
    v <- valores$valores[indices]
    n_fuera <- sum(.dominio_fuera_de(v, regla$minimo, regla$maximo))
    if (n_fuera > 0L) salida$hay <- TRUE
    salida$cuentas <- list(rango = c(n_fuera, n))
    if (suficiente && n_fuera > 0L && n_fuera / n > tope) {
      salida$rango <- "columna"
    }
    # Un porcentaje ambiguo (`porcentaje_de_avance`) solo avisa de la columna,
    # en cualquier proporcion: nunca acusa filas.
    if (isTRUE(regla$solo_aviso) && n_fuera > 0L) salida$rango <- "columna"
    # Longitud en la convencion de 0 a 360 (o mezclada con la de -180 a 180):
    # se decide con los valores que caben en alguna convencion, tolerando una
    # minoria fuera de todas.
    if (identical(regla$tipo_dominio, "longitud") && suficiente) {
      convencion <- .dominio_convencion_longitud(v)
      if (!is.null(convencion)) {
        salida$rango <- "columna"
        salida$longitud360 <- convencion
      }
    }
  }
  if (isTRUE(regla$no_futuras) && valores$fecha) {
    referencia <- suppressWarnings(as.numeric(fecha_hora))
    if (length(referencia) == 1L && is.finite(referencia)) {
      futuras <- !.dominio_no_futuro(
        valores$valores[indices], referencia, fecha_es_date
      )
      if (sum(futuras) > 0L) salida$hay <- TRUE
      salida$cuentas$futuras <- c(sum(futuras), n)
      if (suficiente && sum(futuras) > 0L && sum(futuras) / n > tope &&
          !isTRUE(regla$imposible_futuras)) {
        salida$futuras <- "columna"
      }
    }
  }
  if (length(regla$centinelas) && valores$fecha) {
    candidatos <- .dominio_fecha_numero(regla$centinelas, "la columna")
    coincide <- Reduce(`|`, lapply(candidatos, function(valor) {
      abs(valores$valores[indices] - valor) == 0
    }))
    if (any(coincide)) salida$hay <- TRUE
    # La mayoria se mide con lo mismo que despues se publica como centinela
    # (repetido y aislado), no con todo candidato que coincida.
    publicados <- .dominio_centinelas_repetidos(
      valores$valores, indices, regla$centinelas, FALSE, "la columna",
      fin_abierto = isTRUE(regla$fin_vigencia)
    )$indices
    salida$cuentas$centinelas <- c(length(publicados), n)
    if (suficiente && length(publicados) &&
        length(publicados) / n >= .UMBRAL_CENTINELA_MAYORIA) {
      salida$centinelas <- "columna"
    }
    # Una fecha sin regla de futuro (nombre de fecha planificada) solo aplica
    # si aparece alguno de los centinelas candidatos.
    if (!isTRUE(regla$no_futuras) && is.null(regla$minimo) &&
        is.null(regla$maximo)) {
      salida$aplica <- any(coincide)
    }
  }
  if (identical(salida$rango, "columna")) {
    salida$reescala <- .dominio_factor_reescalado(valores$valores[indices], regla)
  }
  salida
}

# Longitud con valores mayores que 180: la convencion de 0 a 360, o una mezcla
# con la de -180 a 180 (negativos). Hace falta una parte sustancial arriba de 180
# (>= 5 valores y >= 10% de los que caben en alguna convencion). Lo que no cabe en
# ninguna (< -180 o > 360) no cuenta para decidir y se acusa por fila siempre, en
# cualquier proporcion. Devuelve NULL, o el modo
# ("0_360" o "mezcla") y cuantos negativos hay.
.dominio_convencion_longitud <- function(v) {
  sin_convencion <- v < -180 | v > 360
  resto <- v[!sin_convencion]
  if (!length(resto)) return(NULL)
  arriba <- sum(resto > 180)
  if (arriba < .MIN_VALORES_LONGITUD_360 ||
      arriba / length(resto) < .UMBRAL_LONGITUD_360) {
    return(NULL)
  }
  negativos <- sum(resto < 0)
  list(modo = if (negativos > 0L) "mezcla" else "0_360", negativos = negativos)
}

# Distancia mediana de los valores de afuera al borde del dominio, en anchos del
# dominio. Mas de un ancho: la columna esta en otra escala, no apenas afuera.
.dominio_distancia_en_anchos <- function(afuera, regla) {
  if (is.null(regla$minimo) || is.null(regla$maximo)) return(NA_real_)
  ancho <- regla$maximo - regla$minimo
  if (!is.finite(ancho) || ancho <= 0 || !length(afuera)) return(NA_real_)
  distancia <- ifelse(afuera < regla$minimo, regla$minimo - afuera,
                      afuera - regla$maximo)
  stats::median(distancia) / ancho
}

# Los valores repetidos que concentran lo de afuera: codigos (999, -1, 500), no
# una escala. Cada uno repetido al menos 3 veces y con el 10% o mas de lo de
# afuera; juntos, mas de la mitad. Devuelve esos valores (vacio si no hay).
.dominio_codigos_repetidos <- function(afuera) {
  if (!length(afuera)) return(numeric())
  tabla <- table(afuera)
  candidatos <- tabla[tabla >= 3L & tabla / length(afuera) >= 0.1]
  if (sum(candidatos) / length(afuera) > 0.5) {
    return(as.numeric(names(candidatos)))
  }
  numeric()
}

# Los factores que llevan casi toda la columna al dominio. Con `estricto` (el
# reescalado que cambia como se acusa lo imposible) la mitad o mas tiene que
# estar afuera y lejos del dominio (mas de medio ancho en mediana); sin el, es la
# lista de posibilidades que se nombra en el aviso de columna. Devuelve la lista
# de factores que explican (puede haber mas de uno: 365 y 52 llevan 0:3650 dias al
# dominio de una edad y los datos no distinguen dias de semanas), o NULL.
# - Un negativo contra un dominio que empieza en 0 no lo arregla ningun factor.
# - Una edad solo se explica con unidades de tiempo.
# - En los demas dominios la columna dividida no puede quedar apinada (los
#   porcentajes 101:140 no son un 10:14 multiplicado por 10: existen los indices
#   de base 100), y entre las potencias de diez se conservan las que explican
#   por igual.
# - Los valores repetidos que concentran lo de afuera son codigos y no una escala,
#   si alguno es un centinela conocido (999, -1...) o si alguno no cae en el
#   dominio al dividir; una escala redondeada a pocos niveles (400, 450, 500...)
#   sigue siendo una escala.
.dominio_factor_reescalado <- function(v, regla, estricto = TRUE) {
  if (is.null(regla$minimo) || is.null(regla$maximo)) return(NULL)
  v <- v[is.finite(v)]
  if (!length(v)) return(NULL)
  fuera <- .dominio_fuera_de(v, regla$minimo, regla$maximo)
  if (!any(fuera)) return(NULL)
  afuera <- v[fuera]
  if (estricto) {
    if (mean(fuera) < .UMBRAL_REESCALADO_FUERA) return(NULL)
    # Pegada al borde del dominio (91:110 en una latitud) no es una escala: es un
    # valor apenas pasado. Un reescalado esta lejos.
    if (!isTRUE(.dominio_distancia_en_anchos(afuera, regla) >
                .UMBRAL_REESCALADO_DISTANCIA)) {
      return(NULL)
    }
  }
  codigos <- .dominio_codigos_repetidos(afuera)
  if (any(codigos %in% .SENTINELAS_DOMINIO)) return(NULL)
  explica <- function(factor, tiempo) {
    dividido <- v / factor
    dentro <- !.dominio_fuera_de(dividido, regla$minimo, regla$maximo)
    if (mean(dentro) < .UMBRAL_REESCALADO_DENTRO) return(NULL)
    if (length(codigos) &&
        any(.dominio_fuera_de(codigos / factor, regla$minimo, regla$maximo))) {
      return(NULL)
    }
    tope <- max(abs(regla$minimo), abs(regla$maximo))
    q95 <- stats::quantile(abs(dividido[dentro]), 0.95, names = FALSE)
    if (tiempo) {
      if (q95 < 0.02 * tope) return(NULL)
    } else if (q95 < 0.25 * tope) {
      return(NULL)
    }
    mean(dentro)
  }
  armar <- function(factores, unidades, tiempo) {
    salida <- list()
    for (i in seq_along(factores)) {
      dentro <- explica(factores[[i]], tiempo)
      if (!is.null(dentro)) {
        salida[[length(salida) + 1L]] <- list(
          factor = factores[[i]], unidad = unidades[[i]], dentro = dentro
        )
      }
    }
    salida
  }
  if (identical(regla$tipo_dominio, "edad")) {
    encontrados <- armar(
      unname(.FACTORES_REESCALADO_TIEMPO), names(.FACTORES_REESCALADO_TIEMPO),
      TRUE
    )
  } else {
    decimales <- armar(
      .FACTORES_REESCALADO_DECIMALES,
      rep(NA_character_, length(.FACTORES_REESCALADO_DECIMALES)), FALSE
    )
    # La menor que explica sin apinar; si otra explica por igual, tambien.
    encontrados <- list()
    if (length(decimales)) {
      mejor <- max(vapply(decimales, `[[`, numeric(1L), "dentro"))
      encontrados <- Filter(function(f) f$dentro >= mejor - 1e-9, decimales)
    }
  }
  if (!length(encontrados)) return(NULL)
  list(factores = encontrados, dentro = min(vapply(
    encontrados, `[[`, numeric(1L), "dentro"
  )))
}

# Lo que se le dice a quien lee el aviso de columna: los factores de reescalado
# si los hay (como posibilidad: los datos no prueban la causa), los codigos
# repetidos que concentran lo de afuera y los otros valores de afuera, o lo
# observado. `parte` es la fraccion de la columna que quedo afuera: si no es casi
# toda, el texto habla de esa parte y no de la columna entera.
.dominio_diagnostico_escala <- function(afuera, regla, reescala = NULL,
                                        parte = 1, codigos = numeric(),
                                        posibles = NULL) {
  formato <- function(x) format(signif(x, 4L), scientific = FALSE, trim = TRUE)
  texto_codigos <- if (length(codigos)) {
    otros <- afuera[!(afuera %in% codigos)]
    paste0(
      " Los valores ", paste(formato(codigos), collapse = " y "),
      " se repiten y concentran lo de afuera: pueden ser c\u00f3digos de ",
      "ausencia (centinelas).",
      if (length(otros)) {
        paste0(" Quedan afuera ", length(otros), " valores m\u00e1s, de ",
               formato(min(otros)), " a ", formato(max(otros)), ".")
      } else ""
    )
  } else ""
  factores_texto <- function(r, sujeto) {
    factores <- vapply(r$factores, `[[`, numeric(1L), "factor")
    unidades <- vapply(r$factores, function(f) {
      if (is.na(f$unidad)) NA_character_ else f$unidad
    }, character(1L))
    lectura <- if (anyNA(unidades)) {
      paste0("multiplicada por ", paste(formato(factores), collapse = " o "),
             " (u otra escala parecida)")
    } else {
      nombres_u <- ifelse(unidades == "dias", "d\u00edas", unidades)
      lista <- if (length(nombres_u) > 1L) {
        paste0(paste(utils::head(nombres_u, -1L), collapse = ", "), " o ",
               utils::tail(nombres_u, 1L))
      } else nombres_u
      paste0("expresada en ", lista, " y no en a\u00f1os",
             if (length(nombres_u) > 1L) {
               " (los datos no distinguen cu\u00e1l)"
             } else "")
    }
    paste0(
      " Al dividir por ", paste(formato(factores), collapse = " o "), ", el ",
      round(100 * r$dentro), "% de los valores cae en el dominio: ", sujeto,
      " puede estar ", lectura, "."
    )
  }
  if (!is.null(reescala)) {
    sujeto <- if (parte >= 0.95) "la columna" else paste0(
      "el ", round(100 * parte), "% de la columna"
    )
    return(paste0(factores_texto(reescala, sujeto), texto_codigos))
  }
  if (length(codigos)) return(texto_codigos)
  anchos <- .dominio_distancia_en_anchos(afuera, regla)
  paste0(
    " Los valores de afuera van de ", formato(min(afuera)), " a ",
    formato(max(afuera)), "; su distancia mediana al dominio es ",
    if (is.finite(anchos)) formato(anchos) else "NA",
    " veces el ancho del dominio",
    if (isTRUE(anchos > .UMBRAL_DOMINIO_OTRA_UNIDAD_ANCHOS)) {
      "; est\u00e1n lejos y ning\u00fan factor habitual (decimal, meses, d\u00edas) los explica."
    } else ".",
    if (!is.null(posibles)) {
      factores_texto(posibles, if (parte >= 0.95) "la columna" else paste0(
        "el ", round(100 * parte), "% de la columna"
      ))
    } else ""
  )
}

# Cuando `perfilar_por()` perfila un grupo, el sosten de la regla se decide con
# la tabla entera y no con el grupo: con la proporcion del grupo, un mismo valor
# suelto se acusaba en un grupo de 4 filas y no en uno de 5, y un grupo chico
# perdia el aviso que la tabla entera daba. `consulta` devuelve el sostenimiento
# de la columna medido sobre la tabla entera (con su pareja `lat`/`lng` a la
# vista), o NULL si no pudo medirlo.
.dominio_sostenimiento_con_tabla <- function(sostenimiento, nombre) {
  consulta <- .CONTEXTO_POR$dominio_columna
  if (!is.function(consulta) || !isTRUE(sostenimiento$hay)) {
    return(sostenimiento)
  }
  de_tabla <- consulta(nombre)
  if (is.null(de_tabla)) return(sostenimiento)
  for (campo in c("rango", "futuras", "centinelas")) {
    if (!is.null(de_tabla[[campo]])) sostenimiento[[campo]] <- de_tabla[[campo]]
  }
  # Que la columna sea un reescalado tambien lo decide la tabla: un grupo no
  # puede llamar reescalado a lo que la tabla acusa por fila.
  sostenimiento["reescala"] <- list(de_tabla$reescala)
  sostenimiento["longitud360"] <- list(de_tabla$longitud360)
  sostenimiento["cuentas_tabla"] <- list(de_tabla$cuentas)
  sostenimiento
}

.nueva_traza_dominio <- function(indices, posicion) {
  traza <- .trazabilidad_indices(indices, "completo", limite = Inf)
  traza$posicion_columna <- posicion
  traza
}

.hallazgos_dominio_columna <- function(x, nombre, indice, resultado, regla,
                                      aplicable, fecha_hora,
                                      fecha_es_date = FALSE, valores = NULL,
                                      sostenimiento = NULL) {
  if (isFALSE(regla)) return(list())
  if (is.null(aplicable)) aplicable <- rep(TRUE, length(x))
  indices_muestra <- resultado$indices_muestra
  if (is.null(indices_muestra)) {
    indices_muestra <- seq_along(x)
  } else {
    indices_muestra <- as.integer(indices_muestra)
    indices_muestra <- indices_muestra[
      !is.na(indices_muestra) & indices_muestra >= 1L &
        indices_muestra <= length(x)
    ]
    indices_muestra <- unique(indices_muestra)
  }
  universo <- rep(FALSE, length(x))
  universo[indices_muestra] <- as.logical(aplicable[indices_muestra])
  if (is.null(valores)) valores <- .dominio_valores(x, resultado, universo)
  if (is.null(valores)) return(list())
  indices_evaluados <- which(valores$usable)
  if (!length(indices_evaluados)) return(list())
  declarada <- identical(regla$fuente, "declarado")
  inferida <- !declarada
  if (is.null(sostenimiento)) {
    sostenimiento <- if (declarada) {
      list(rango = "fila", futuras = "fila", centinelas = "fila",
           aplica = TRUE)
    } else {
      .dominio_decidir_inferida(valores, regla, fecha_hora, fecha_es_date)
    }
  }
  severidad_fila <- if (declarada) "error" else "sospechoso"
  salida <- list()
  # `lectura` es el modo con que se publica el componente: "fila" acusa las
  # filas; "columna" avisa de la columna (severidad `ok`) con las dos lecturas.
  agregar <- function(tipo_hallazgo, indices, evidencia, descripcion,
                      sugerencia, lectura = "fila", extra = "",
                      abierto = FALSE) {
    if (!length(indices)) return()
    por_columna <- identical(lectura, "columna")
    if (por_columna) {
      proporcion <- length(indices) / length(indices_evaluados)
      clave_cuenta <- switch(
        tipo_hallazgo, valor_fuera_dominio = "rango", fecha_futura = "futuras",
        "centinelas"
      )
      cuenta_tabla <- sostenimiento$cuentas_tabla[[clave_cuenta]]
      evidencia <- paste0(
        evidencia,
        if (!is.null(cuenta_tabla)) {
          # El modo viene de la tabla entera: se dice, en vez de afirmar sobre
          # el grupo algo que solo vale para la tabla.
          paste0(
            " Lectura de columna decidida con la tabla entera: ", cuenta_tabla[[1L]],
            " de ", cuenta_tabla[[2L]], " valores ",
            if (identical(tipo_hallazgo, "fecha_centinela")) "son ese valor"
            else "quedan afuera",
            "; en este grupo, ", length(indices), " de ",
            length(indices_evaluados), " (",
            .formatear_decimal_publicado(proporcion), ")",
            if (identical(tipo_hallazgo, "fecha_centinela")) "." else {
              paste0(". En la tabla es m\u00e1s de lo que una minor\u00eda de ",
                     "valores sueltos explica.", extra)
            }
          )
        } else paste0(" Lectura de columna: ", length(indices), " de ",
        length(indices_evaluados), " valores (",
        .formatear_decimal_publicado(proporcion),
        if (identical(tipo_hallazgo, "fecha_centinela")) {
          ") son ese valor: la columna lo usa como convenci\u00f3n."
        } else {
          if (length(indices) == 1L) {
            paste0(") queda afuera.", extra)
          } else {
            paste0(") quedan afuera, m\u00e1s de lo que una minor\u00eda de ",
                   "valores sueltos explica.", extra)
          }
        })
      )
      descripcion <- if (isTRUE(regla$solo_aviso) &&
                         identical(tipo_hallazgo, "valor_fuera_dominio")) {
        paste0(
          "Si es un porcentaje acotado, estos valores quedan fuera de [0, 100]. El ",
          "nombre puede ser relativo a otra cosa (`porcentaje_del_costo`) y ",
          "ah\u00ed no es un error: por eso no se acusan filas."
        )
      } else if (abierto) {
        paste0(
          "La columna usa 9999-12-31 como \u00absin fin\u00bb: es la ",
          "convenci\u00f3n de vigencia abierta, no una fecha. No se acusan filas."
        )
      } else if (identical(tipo_hallazgo, "fecha_centinela")) {
        paste0(
          "La mayor\u00eda de la columna es una misma fecha centinela: se usa ",
          if (isTRUE(regla$imposible_futuras)) {
            "como ausencia disfrazada de la fecha de nacimiento. "
          } else {
            paste0("como intervalo abierto (inicio o fin de vigencia sin fecha) ",
                   "o como ausencia disfrazada. ")
          },
          "No se acusan filas."
        )
      } else paste0(
        "La columna no sostiene la regla que sugiere su nombre. ",
        "O no est\u00e1 en la unidad o el dominio inferido (declararlo con ",
        "`dominios` lo apaga o lo exige), o tiene un error de carga ",
        "sistem\u00e1tico. No se acusan filas."
      )
      sugerencia <- if (abierto) {
        paste0(
          "Tratar 9999-12-31 como ausencia (vigencia abierta) al calcular ",
          "con esta columna; declararlo en `dominios` (`centinelas`) lo ",
          "hace expl\u00edcito."
        )
      } else if (identical(tipo_hallazgo, "fecha_centinela")) {
        paste0(
          "Tratarlo como ausencia al calcular con esta columna",
          if (isTRUE(regla$imposible_futuras)) "" else {
            " (si es un intervalo abierto, al calcular vigencias)"
          },
          "; declararlo en `dominios` (`centinelas`) lo hace expl\u00edcito."
        )
      } else paste0(
        "Declarar el dominio de la columna con `dominios` si la unidad es ",
        "otra; si no, revisar la carga contra la fuente."
      )
    }
    hallazgo <- .nuevo_hallazgo(
      nombre,
      tipo = if (tipo_hallazgo == "valor_fuera_dominio") {
        "valor_fuera_dominio"
      } else if (tipo_hallazgo == "fecha_futura") {
        "fecha_futura"
      } else {
        "fecha_centinela"
      },
      severidad = if (por_columna) "ok" else severidad_fila,
      descripcion = descripcion, evidencia = evidencia,
      sugerencia = sugerencia, n_evaluados = length(indices_evaluados),
      n_afectados = length(indices), unidad_conteo = "fila"
    )
    hallazgo$trazabilidad <- I(list(
      .nueva_traza_dominio(indices, indice)
    ))
    salida[[length(salida) + 1L]] <<- hallazgo
  }
  if (!is.null(regla$minimo) || !is.null(regla$maximo)) {
    fuera <- indices_evaluados[
      (if (is.null(regla$minimo)) FALSE else
        valores$valores[indices_evaluados] < regla$minimo) |
        (if (is.null(regla$maximo)) FALSE else
          valores$valores[indices_evaluados] > regla$maximo)
    ]
    # Lo imposible en cualquier unidad (edad o porcentaje negativos, latitud
    # mayor que 90) se acusa por fila aunque la columna no sostenga la regla:
    # no admite la lectura de otra unidad. Lo que depende de la escala queda en
    # el aviso de la columna, que nombra el factor si la columna es un
    # reescalado. Un reescalado no arregla un valor imposible despues de dividir
    # (un negativo sigue negativo): ese se acusa por fila, salvo que sea un
    # valor repetido que concentra lo imposible (un centinela), que se nombra.
    fila_idx <- fuera
    columna_idx <- integer()
    reescala <- NULL
    codigos <- numeric()
    convencion <- if (inferida) sostenimiento$longitud360 else NULL
    posibles <- NULL
    if (!is.null(convencion)) {
      # Lo que no cabe en ninguna convencion (400, -200) se acusa por fila; el
      # resto avisa de la convencion o de la mezcla.
      v_fuera <- valores$valores[fuera]
      sin_convencion <- v_fuera < -180 | v_fuera > 360
      fila_idx <- fuera[sin_convencion]
      columna_idx <- fuera[!sin_convencion]
    } else if (inferida && isTRUE(regla$solo_aviso)) {
      # Un porcentaje ambiguo solo avisa de la columna.
      fila_idx <- integer()
      columna_idx <- fuera
    } else if (inferida && identical(sostenimiento$rango, "columna")) {
      reescala <- sostenimiento$reescala
      limites <- .dominio_limites_imposibles(regla)
      v_fuera <- valores$valores[fuera]
      es_imposible <- .dominio_fuera_de(v_fuera, limites$minimo, limites$maximo)
      if (!is.null(reescala)) {
        # Sigue imposible despues de dividir por cualquiera de los factores.
        es_imposible <- es_imposible & Reduce(`&`, lapply(
          reescala$factores, function(f) {
            .dominio_fuera_de(v_fuera / f$factor, regla$minimo, regla$maximo)
          }
        ))
        # Solo el valor repetido que concentra lo imposible es un centinela y
        # se perdona (se nombra); los demas imposibles se acusan por fila.
        if (any(es_imposible)) {
          codigos <- .dominio_codigos_repetidos(v_fuera[es_imposible])
          es_imposible <- es_imposible & !(v_fuera %in% codigos)
        }
      }
      if (any(es_imposible)) {
        fila_idx <- fuera[es_imposible]
        columna_idx <- fuera[!es_imposible]
      } else {
        fila_idx <- integer()
        columna_idx <- fuera
      }
      if (is.null(reescala) && length(columna_idx)) {
        codigos <- .dominio_codigos_repetidos(valores$valores[columna_idx])
        # Sin reescalado estricto, los factores que igual llevan casi toda la
        # columna al dominio se nombran como posibilidad.
        if (!length(codigos)) {
          posibles <- .dominio_factor_reescalado(
            valores$valores[indices_evaluados], regla, estricto = FALSE
          )
        }
      }
    }
    textos_rango <- list(
      paste0(
        "Dominio inclusivo [", .dominio_evidencia_numero(regla), "]; fuente: ",
        regla$fuente, "; filas afectadas: "
      ),
      "La columna contiene valores fuera del dominio declarado o inferido.",
      "Revisar los valores se\u00f1alados contra la fuente; no reemplazarlos sin confirmar el dominio."
    )
    if (length(fila_idx)) {
      agregar(
        tipo_hallazgo = "valor_fuera_dominio", indices = fila_idx,
        lectura = "fila",
        paste0(textos_rango[[1L]], length(fila_idx), "."),
        textos_rango[[2L]], textos_rango[[3L]]
      )
    }
    if (length(columna_idx)) {
      agregar(
        tipo_hallazgo = "valor_fuera_dominio", indices = columna_idx,
        lectura = "columna",
        paste0(textos_rango[[1L]], length(columna_idx), "."),
        textos_rango[[2L]], textos_rango[[3L]],
        extra = if (!is.null(convencion)) {
          if (identical(convencion$modo, "mezcla")) {
            paste0(
              " La columna mezcla las dos convenciones de longitud: ",
              convencion$negativos, " valores negativos (de -180 a 180) y ",
              length(columna_idx), " mayores que 180 (de 0 a 360). Es un ",
              "problema de consistencia: llevarla a una sola convenci\u00f3n ",
              "(restar 360 a los que pasan de 180)."
            )
          } else {
            paste0(
              " Los valores caben en [0, 360] y una parte sustancial pasa de ",
              "180: puede ser la convenci\u00f3n de longitud de 0 a 360 (este ",
              "positivo) y no un error de carga. Declararla con `dominios` o ",
              "restar 360 a los que pasan de 180 la lleva a [-180, 180]."
            )
          }
        } else if (isTRUE(regla$solo_aviso)) {
          paste0(
            " ", sum(valores$valores[columna_idx] < 0),
            " valores quedan por debajo de 0 y ",
            sum(valores$valores[columna_idx] > 100),
            " por encima de 100: si es un porcentaje acotado, es un error; ",
            "si es relativo, no."
          )
        } else {
          .dominio_diagnostico_escala(
            valores$valores[columna_idx], regla, reescala,
            parte = length(columna_idx) / length(indices_evaluados),
            codigos = codigos, posibles = posibles
          )
        }
      )
    }
  }
  if (isTRUE(regla$no_futuras) && valores$fecha) {
    referencia <- suppressWarnings(as.numeric(fecha_hora))
    if (length(referencia) == 1L && is.finite(referencia)) {
      no_futuras <- .dominio_no_futuro(
        valores$valores[indices_evaluados], referencia, fecha_es_date
      )
      futuras <- indices_evaluados[!no_futuras]
      if (length(futuras)) {
        agregar(
          tipo_hallazgo = "fecha_futura", indices = futuras,
          lectura = if (inferida) sostenimiento$futuras else "fila",
          paste0(
            "Fecha posterior a la fecha del perfil; fuente: ", regla$fuente,
            "; fecha de referencia UTC: ",
            format(as.POSIXct(referencia, origin = "1970-01-01", tz = "UTC"),
                   "%Y-%m-%d %H:%M:%S"), "; filas afectadas: ", length(futuras), "."
          ),
          "La columna contiene fechas posteriores a la fecha usada para el perfil.",
          "Confirmar si la fecha futura es leg\u00edtima para el proceso antes de corregirla."
        )
      }
    }
  }
  if (length(regla$centinelas) && valores$fecha) {
    centinelas <- .dominio_centinelas_repetidos(
      valores$valores, indices_evaluados, regla$centinelas, declarada, nombre,
      fin_abierto = inferida && isTRUE(regla$fin_vigencia)
    )
    indices_c <- centinelas$indices
    # `9999-12-31` en un nombre de fin de vigencia es la convencion de "sin fin":
    # aviso de la columna en cualquier proporcion, no una acusacion por fila.
    abierto <- integer()
    if (inferida && isTRUE(regla$fin_vigencia) && length(indices_c)) {
      fecha_9999 <- .dominio_fecha_numero("9999-12-31", nombre)
      es_9999 <- abs(valores$valores[indices_c] - fecha_9999) == 0
      abierto <- indices_c[es_9999]
      indices_c <- indices_c[!es_9999]
    }
    textos_centinela <- list(
      "La columna contiene fechas que suelen representar valores centinela.",
      "Confirmar el significado en la fuente y declarar la fecha como ausencia si corresponde."
    )
    if (length(abierto)) {
      agregar(
        tipo_hallazgo = "fecha_centinela", indices = abierto,
        lectura = "columna", abierto = TRUE,
        paste0(
          "Centinelas observados: 9999-12-31; fuente: ", regla$fuente,
          "; filas afectadas: ", length(abierto), "."
        ),
        textos_centinela[[1L]], textos_centinela[[2L]]
      )
    }
    if (length(indices_c)) {
      agregar(
        tipo_hallazgo = "fecha_centinela", indices = indices_c,
        lectura = if (inferida) sostenimiento$centinelas else "fila",
        paste0(
          "Centinelas observados: ", paste(centinelas$observados, collapse = ", "),
          "; fuente: ", regla$fuente, "; filas afectadas: ",
          length(indices_c), "."
        ),
        textos_centinela[[1L]], textos_centinela[[2L]]
      )
    }
  }
  salida
}

.evaluar_dominios <- function(datos, nombres, resultados, dominios,
                              aplicables, fecha_hora,
                              fecha_es_date = FALSE) {
  reglas <- vector("list", length(nombres))
  names(reglas) <- nombres
  inferidos <- list()
  hallazgos <- list()
  for (i in seq_along(nombres)) {
    nombre <- nombres[[i]]
    clave <- .nombres_para_operar(nombre)
    indices_declarados <- if (length(dominios$declarados)) {
      match(clave, .nombres_para_operar(names(dominios$declarados)))
    } else NA_integer_
    declarada <- if (length(indices_declarados) &&
                     !is.na(indices_declarados[[1L]])) {
      dominios$declarados[[indices_declarados[[1L]]]]
    } else NULL
    if (!is.null(declarada) && !isFALSE(declarada)) {
      .validar_tipo_regla_dominio(declarada, resultados[[i]], nombre)
    }
    inferida <- FALSE
    if (!is.null(declarada)) {
      regla <- declarada
      if (isFALSE(regla)) {
        reglas[[i]] <- FALSE
        next
      }
      regla$fuente <- "declarado"
    } else if (isTRUE(dominios$inferir)) {
      inferida <- TRUE
      regla <- .dominio_regla_inferida(
        nombre, .dominio_tokens(nombre), nombres, i, resultados
      )
      # Una rebanada que perdio la columna pareja (`lng` toda ausente en el
      # grupo) no infiere la latitud por el nombre: la regla es la que infirio la
      # tabla entera.
      if (is.null(regla) && is.function(.CONTEXTO_POR$dominio_columna) &&
          any(.dominio_tokens(nombre) %in% c("lat", "lon", "lng"))) {
        regla <- .CONTEXTO_POR$dominio_columna(nombre)$regla
      }
    } else {
      regla <- NULL
    }
    reglas[[i]] <- regla
    if (!is.null(regla)) {
      universo <- rep(FALSE, length(datos[[i]]))
      indices_muestra <- resultados[[i]]$indices_muestra
      if (is.null(indices_muestra)) indices_muestra <- seq_along(universo)
      indices_muestra <- as.integer(indices_muestra)
      indices_muestra <- indices_muestra[
        !is.na(indices_muestra) & indices_muestra >= 1L &
          indices_muestra <= length(universo)
      ]
      aplicable <- aplicables[[i]]
      if (is.null(aplicable)) aplicable <- rep(TRUE, length(universo))
      universo[unique(indices_muestra)] <- as.logical(
        aplicable[unique(indices_muestra)]
      )
      valores <- .dominio_valores(datos[[i]], resultados[[i]], universo)
      sostenimiento <- if (inferida) {
        if (is.null(valores)) {
          list(rango = "fila", futuras = "fila", centinelas = "fila",
               aplica = FALSE)
        } else {
          decidido <- .dominio_decidir_inferida(
            valores, regla, fecha_hora, fecha_es_date
          )
          # Lo que decide la tabla entera se captura para `perfilar_por()`.
          if (is.environment(.CONTEXTO_POR$captura_dominio)) {
            assign(.nombres_para_operar(nombre),
                   c(decidido, list(regla = regla)),
                   envir = .CONTEXTO_POR$captura_dominio)
          }
          .dominio_sostenimiento_con_tabla(decidido, nombre)
        }
      } else list(rango = "fila", futuras = "fila", centinelas = "fila",
                  aplica = TRUE)
      aplicar_regla <- !inferida || isTRUE(sostenimiento$aplica)
      if (inferida && aplicar_regla) inferidos[[nombre]] <- regla
      if (!aplicar_regla) {
        resultados[[i]]$dominio <- NULL
        next
      }
      hallazgos_columna <- .hallazgos_dominio_columna(
        datos[[i]], nombre, i, resultados[[i]], regla,
        aplicable, fecha_hora, fecha_es_date, valores, sostenimiento
      )
      if (length(hallazgos_columna)) {
        hallazgos <- c(hallazgos, hallazgos_columna)
      }
      resultados[[i]]$dominio <- list(
        regla = regla,
        indices = lapply(hallazgos_columna, function(x) {
          x$trazabilidad[[1L]]$indices_fila
        })
      )
    } else {
      resultados[[i]]$dominio <- NULL
    }
  }
  declaracion <- dominios$declarados
  tiene_informacion <- length(declaracion) > 0L || length(inferidos) > 0L ||
    !isTRUE(dominios$inferir)
  meta <- if (tiene_informacion) {
    list(
      inferir = isTRUE(dominios$inferir),
      declarados = declaracion,
      inferidos = inferidos
    )
  } else NULL
  list(
    resultados = resultados,
    reglas = reglas,
    hallazgos = hallazgos,
    meta = meta
  )
}
