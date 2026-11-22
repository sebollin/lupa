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
# Una abreviatura de coordenada sin pareja (`lat`) solo se toma por coordenada
# si hay con que juzgar (10 valores) y la mayoria (80%) esta escrita con los
# decimales de una coordenada en grados (de 3 a 8): un entero o un valor de 15
# decimales (un calculo) no lo es.
.MIN_VALORES_COORDENADA_SOLA <- 10L
.UMBRAL_DECIMALES_COORDENADA <- 0.8
# Hasta la mitad de la columna puede estar fuera de rango y seguir pareciendo una
# coordenada con errores (los decimales se miden en lo que cabe): sin acantilado
# en una proporcion intermedia.
.UMBRAL_COORDENADA_FUERA <- 0.5
# Una columna de fechas es un calendario (o un pronostico, un periodo) si sus
# fechas distintas forman una grilla sin huecos; con menos de 8 distintas no hay
# con que distinguir una grilla de unas pocas fechas sueltas.
.MIN_FECHAS_SERIE_REGULAR <- 8L

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
  "persona", "people", "human", "patient", "client", "customer", "user",
  # Parientes y roles de una persona: la edad de un conyuge, de un tutor o de
  # un trabajador es una edad en anios como la de una madre.
  "conyuge", "conyuges", "esposo", "esposa", "esposos", "esposas", "pareja",
  "hermano", "hermana", "hermanos", "hermanas", "abuelo", "abuela", "abuelos",
  "abuelas", "nieto", "nieta", "nietos", "nietas", "tio", "tia", "tios", "tias",
  "sobrino", "sobrina", "sobrinos", "sobrinas", "primo", "prima", "suegro",
  "suegra", "yerno", "nuera", "hijos", "hijas", "padres", "madres", "bebe",
  "bebes", "nina", "ninas", "menor", "menores", "joven", "jovenes",
  "adolescente", "adolescentes", "anciano", "anciana", "tutor", "tutora",
  "tutores", "beneficiario", "beneficiaria", "beneficiarios", "beneficiarias",
  "titular", "titulares", "afiliado", "afiliada", "afiliados", "afiliadas",
  "jubilado", "jubilada", "jubilados", "jubiladas", "residente", "residentes",
  "votante", "votantes", "postulante", "postulantes", "candidato", "candidata",
  "candidatos", "candidatas", "victima", "victimas", "testigo", "testigos",
  "imputado", "imputada", "imputados", "denunciante", "denunciantes",
  "inquilino", "inquilina", "vecino", "vecina", "dueno", "duena", "propietario",
  "propietaria", "pasajero", "pasajera", "pasajeros", "conductor", "conductora",
  "chofer", "choferes", "trabajador", "trabajadora", "trabajadores",
  "trabajadoras", "maestro", "maestra", "profesor",
  "profesora", "profesores", "medico", "medica", "enfermero", "enfermera",
  "funcionario", "funcionaria", "funcionarios", "obrero", "obrera", "jefe",
  "jefa",
  "wife", "husband", "spouse", "sibling", "brother", "sister",
  "grandmother", "grandfather", "grandchild", "uncle", "aunt", "nephew", "niece",
  "cousin", "baby", "infant", "minor", "teenager", "elder", "guardian",
  "beneficiary", "resident", "voter", "applicant",
  "candidate", "victim", "witness", "defendant", "tenant", "neighbor",
  "owner", "passenger", "driver", "worker", "teacher", "doctor", "nurse",
  "owners", "passengers", "drivers", "workers", "teachers", "doctors", "nurses",
  "tenants", "victims", "witnesses", "residents", "voters", "babies",
  "children", "kids", "parent", "parents", "daughter", "son"
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
  "cuenta", "account", "seconds", "second", "minutes", "minute", "sec", "secs",
  "ms", "millis", "milliseconds"
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
  "min", "max", "x", "y",
  # Sistemas de referencia y calificativos geograficos habituales.
  "wgs84", "wgs", "geo", "geografica", "geografico", "epsg", "epsg4326",
  "4326", "srid"
)

.PALABRAS_NACIMIENTO <- c(
  "nacimiento", "nac", "fnac", "fecnac", "birth", "birthdate", "dob", "born",
  "nacido"
)

.TOKENS_LATITUD_COMPLETA <- c("latitud", "latitude")
.TOKENS_LONGITUD_COMPLETA <- c("longitud", "longitude")
# `long` es la abreviatura inglesa habitual, pero tambien el largo de algo: solo
# forma pareja con una latitud; sola no infiere nada.
.TOKENS_LONGITUD_ABREVIADA <- c("lon", "lng", "long")
.TOKENS_LONGITUD_ABREVIADA_SOLA <- c("lon", "lng")
.PALABRAS_COORDENADA_RELATIVA <- c(
  "rel", "relativa", "relativo", "relative", "delta", "dif", "diff", "offset",
  "desv", "error", "err"
)
# Un 9999-12-31 en estos nombres es la convencion de "sin fin": vigencia abierta.
.PALABRAS_FIN_VIGENCIA <- c(
  "hasta", "fin", "vigencia", "vencimiento", "until", "end", "expiry"
)

# Fechas de algo que ya ocurrio al registrarse: una serie de fechas seguidas
# hacia adelante en una `fecha_alta` no es un calendario, es un registro con
# fechas que todavia no pasaron.
.PALABRAS_FECHA_REGISTRO <- c(
  "alta", "altas", "registro", "registros", "ingreso", "ingresos", "creacion",
  "carga", "emision", "firma", "inscripcion", "matricula", "compra", "compras",
  "venta", "ventas", "pedido", "pedidos", "factura", "facturas", "operacion",
  "transaccion", "egreso", "defuncion", "fallecimiento", "created",
  "registered", "registration", "signup", "purchase", "sale", "order",
  "invoice", "transaction", "entry", "admission", "death",
  # Marcas de cuando se toco un registro: tampoco pueden ser futuras.
  "modificacion", "actualizacion", "captura", "revision", "validacion",
  "modified", "updated", "captured", "review", "validation"
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

.dominio_regla_inferida <- function(nombre, tokens, nombres, indice, resultados,
                                    contenido = function(j) TRUE) {
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
  # `lat_inicio`). `lat`, `lon` o `lng` solas infieren solo si el contenido
  # parece una coordenada, y solo avisan (`latencia`, `lona` y `long_cable` no
  # son una abreviatura sin calificar: no pasan por aca).
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
  # `long` es tambien el largo de algo: como pareja exige que su contenido sea
  # el de una coordenada (`contenido()` mira la columna `j` de la tabla).
  pareja_lon <- any(vapply(seq_along(tokens_otras), function(k) {
    t <- tokens_otras[[k]]
    es_coordenada(t, c(.TOKENS_LONGITUD_COMPLETA, .TOKENS_LONGITUD_ABREVIADA)) &&
      (!("long" %in% t) || isTRUE(contenido(otras[[k]])))
  }, logical(1L)))
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
  # La abreviatura sin pareja (`lat`, `lon`, `lng` a solas) puede ser una
  # coordenada o no (`lat` de latencia): solo se infiere si el contenido parece
  # una coordenada (se decide con los valores en `.dominio_decidir_inferida()`)
  # y entonces solo avisa de la columna, sin acusar filas.
  if (!relativa && "lat" %in% tokens && sin_calificar("lat") &&
      !latitud_completa_aparte) {
    return(list(
      minimo = -90, maximo = 90, tipo_dominio = "latitud",
      fuente = "nombre_y_contenido", solo_aviso = TRUE,
      contenido_coordenada = TRUE
    ))
  }
  if (!relativa && any(tokens %in% .TOKENS_LONGITUD_ABREVIADA_SOLA) &&
      sin_calificar(.TOKENS_LONGITUD_ABREVIADA_SOLA) &&
      !(any(tokens %in% .TOKENS_LONGITUD_ABREVIADA) && pareja_lat) &&
      !longitud_completa_aparte) {
    return(list(
      minimo = -180, maximo = 180, tipo_dominio = "longitud",
      fuente = "nombre_y_contenido", solo_aviso = TRUE,
      contenido_coordenada = TRUE
    ))
  }
  if (!relativa && (tiene_longitud ||
      (any(tokens %in% .TOKENS_LONGITUD_ABREVIADA) &&
       sin_calificar(.TOKENS_LONGITUD_ABREVIADA) && pareja_lat &&
       !longitud_completa_aparte))) {
    ingles <- any(tokens %in% "longitude")
    # Una `long` con pareja se toma por longitud solo si su contenido lo parece,
    # pero entonces acusa filas como cualquier pareja.
    contenido_largo <- !tiene_longitud && "long" %in% tokens
    calificada <- length(setdiff(tokens, c(
      .TOKENS_LONGITUD_COMPLETA, .PALABRAS_COORDENADA
    ))) > 0L
    # `longitud` es tambien el largo de algo: sin una latitud en la tabla, o con
    # palabras que no son de coordenada, el nombre no basta para inferir.
    if (tiene_longitud && !ingles && (calificada || !hay_latitud_en_tabla)) {
      return(NULL)
    }
    regla <- list(
      minimo = -180, maximo = 180, tipo_dominio = "longitud",
      fuente = "nombre_y_contenido"
    )
    if (contenido_largo) regla$contenido_coordenada <- TRUE
    return(regla)
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
    if (!futuro_legitimo && any(tokens %in% .PALABRAS_FECHA_REGISTRO)) {
      regla$fecha_de_registro <- TRUE
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
      # Los meses en texto ya los detecto el perfil de la columna: se reusan
      # en vez de volver a recorrer los valores.
      meses_texto = resultado$meses_texto,
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

# La reserva de una fecha centinela, en un solo lugar. Memoria la calcula sobre el
# vector y el motor sobre agregados que trae con comparaciones contra el literal
# (`.resumen_fechas_dbi()`): las dos vias llaman a esta funcion con los mismos
# seis numeros -cuantos valores hay en la base, cuantos son la candidata, el
# minimo y el maximo de la base, el mayor valor por debajo y el menor por encima
# de la candidata-, asi que no pueden decidir distinto. La proporcion es
# `cuenta / n`: `mean()` de un logico puede diferir de esa division en el ultimo
# bit, pero nunca a los dos lados del umbral (`cuenta / n` y 0.05 distan al menos
# `1 / (20 n)`, muchisimo mas que un bit para cualquier `n` de una tabla).
.dominio_centinela_aislado_extremos <- function(n, cuenta, minimo, maximo,
                                                debajo, encima, candidato) {
  if (!isTRUE(n > 0)) return(FALSE)
  if (cuenta >= n) return(TRUE)
  amplitud <- maximo - minimo
  if (!is.finite(amplitud) || amplitud <= 0) return(FALSE)
  hueco <- min(c(candidato - debajo, encima - candidato), na.rm = TRUE) / amplitud
  proporcion <- cuenta / n
  hueco > .UMBRAL_HUECO_CENTINELA_FECHA &&
    (proporcion < .UMBRAL_PROPORCION_PLAUSIBLE_CENTINELA ||
     hueco > .UMBRAL_HUECO_GRANDE_CENTINELA_FECHA)
}

.dominio_centinela_aislado <- function(valores, candidato) {
  valores <- valores[!is.na(valores) & is.finite(valores)]
  if (!length(valores)) return(FALSE)
  es <- abs(valores - candidato) == 0
  debajo <- valores[!es & valores < candidato]
  encima <- valores[!es & valores > candidato]
  .dominio_centinela_aislado_extremos(
    n = length(valores), cuenta = sum(es),
    minimo = min(valores), maximo = max(valores),
    debajo = if (length(debajo)) max(debajo) else NA_real_,
    encima = if (length(encima)) min(encima) else NA_real_,
    candidato = candidato
  )
}

# Cuantas apariciones hacen falta para que una fecha incorporada sea centinela.
# 1970-01-01 es tambien una fecha real (el origen de muchos sistemas): dos
# apariciones no la vuelven centinela. En un fin de vigencia, 9999-12-31 basta
# una vez: es la convencion de "sin fin".
.dominio_minimo_centinela <- function(centinela, fin_abierto = FALSE) {
  if (identical(centinela, "1970-01-01")) {
    .MIN_REPETICIONES_CENTINELA_1970
  } else if (isTRUE(fin_abierto) && identical(centinela, "9999-12-31")) {
    1L
  } else .MIN_REPETICIONES_CENTINELA_FECHA
}

# `declarada` va por candidata (un valor de largo 1 vale para toda la lista,
# como hasta ahora): las fechas que declara `centinelas_fecha` se suman a las
# incorporadas sin quitarles su reserva. El aislamiento de cada candidata se
# mide sin TODAS las otras, declaradas incluidas: un 0001-01-01 declarado no
# puede esconder un 1900, como un 9999 tampoco.
.dominio_centinelas_repetidos <- function(valores, indices, centinelas,
                                          declarada, columna,
                                          fin_abierto = FALSE) {
  vacio <- list(indices = integer(), observados = character(),
                indices_declarados = integer(),
                observados_declarados = character())
  if (!length(centinelas)) return(vacio)
  candidatos <- .dominio_fecha_numero(centinelas, columna)
  declarada <- rep_len(as.logical(declarada), length(candidatos))
  declarada[is.na(declarada)] <- FALSE
  validos <- vapply(seq_along(candidatos), function(j) {
    if (declarada[[j]]) return(TRUE)
    valor <- candidatos[[j]]
    encontrados <- sum(abs(valores[indices] - valor) == 0)
    if (encontrados < .dominio_minimo_centinela(centinelas[[j]], fin_abierto)) {
      return(FALSE)
    }
    base <- valores[indices]
    base <- base[!(base %in% candidatos[-j])]
    .dominio_centinela_aislado(base, valor)
  }, logical(1L))
  if (!any(validos)) return(vacio)
  coincide_con <- function(cuales) {
    if (!length(cuales)) return(rep(FALSE, length(indices)))
    rowSums(abs(outer(valores[indices], cuales, "-")) == 0) > 0
  }
  coincide <- coincide_con(candidatos[validos])
  coincide_declarada <- coincide_con(candidatos[validos & declarada])
  # Un centinela declarado vale aunque no aparezca, pero la evidencia nombra
  # solo los que se observaron: decir que un valor ausente esta en la columna
  # es falso.
  presentes <- vapply(candidatos, function(valor) {
    any(abs(valores[indices] - valor) == 0)
  }, logical(1L))
  list(indices = indices[coincide],
       observados = centinelas[validos & presentes],
       indices_declarados = indices[coincide_declarada],
       observados_declarados = centinelas[validos & presentes & declarada])
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

# TRUE para los valores escritos con entre 3 y 8 decimales: lo que tiene una
# coordenada en grados decimales (un GPS da de 4 a 7). Un valor con 15 decimales
# es un calculo, y uno con 2 o menos (un entero, 95.5) es otra cosa.
.dominio_decimales_coordenada <- function(v) {
  tiene <- function(k) abs(v * 10^k - round(v * 10^k)) < 1e-5
  tiene(8) & !tiene(2)
}

# El contenido de una columna se parece al de una coordenada: como mucho la
# mitad queda fuera del rango y la mayoria de lo que cabe tiene los decimales de
# una coordenada.
.dominio_parece_coordenada <- function(v, regla) {
  n <- length(v)
  if (n < .MIN_VALORES_COORDENADA_SOLA) return(FALSE)
  fuera <- .dominio_fuera_de(v, regla$minimo, regla$maximo)
  if (sum(fuera) / n > .UMBRAL_COORDENADA_FUERA) return(FALSE)
  mean(.dominio_decimales_coordenada(v[!fuera])) >=
    .UMBRAL_DECIMALES_COORDENADA
}

# Si las fechas distintas de la columna (en segundos) forman una grilla regular
# sin huecos, devuelve el paso en dias (el mayor, si es de dias habiles; 31 por
# cada mes); si no, NA. Las grillas son: todos los dias, los dias habiles, cada
# 7 o 14 dias, o un dia fijo del mes (o el fin de mes) cada 1, 3, 6 o 12 meses.
# Es lo que tiene un calendario, un pronostico o una lista de periodos: un valor
# suelto que se sale de la grilla la rompe, asi que un error de carga no se
# confunde con uno.
.dominio_serie_regular <- function(valores) {
  dias <- unique(floor(valores[is.finite(valores)] / 86400))
  n <- length(dias)
  if (n < .MIN_FECHAS_SERIE_REGULAR) return(NA_real_)
  minimo <- min(dias)
  maximo <- max(dias)
  for (paso in c(1, 7, 14)) {
    if (all((dias - minimo) %% paso == 0) &&
        (maximo - minimo) / paso + 1 == n) {
      return(paso)
    }
  }
  # El 1970-01-01 fue jueves: 0 es domingo.
  semana <- (dias + 4) %% 7
  if (all(semana %in% 1:5)) {
    todos <- seq(minimo, maximo)
    if (sum(((todos + 4) %% 7) %in% 1:5) == n) return(3)
  }
  fechas <- as.POSIXlt(as.Date(dias, origin = "1970-01-01"), tz = "UTC")
  mes <- (fechas$year + 1900) * 12 + fechas$mon
  dia_fijo <- length(unique(fechas$mday)) == 1L
  fin_de_mes <- all(as.POSIXlt(as.Date(dias + 1, origin = "1970-01-01"),
                               tz = "UTC")$mday == 1L)
  if (!dia_fijo && !fin_de_mes) return(NA_real_)
  for (paso in c(1, 3, 6, 12)) {
    if (all((mes - min(mes)) %% paso == 0) &&
        (max(mes) - min(mes)) / paso + 1 == n) {
      return(31 * paso)
    }
  }
  NA_real_
}

# Como se leen las futuras y las centinelas de una regla INFERIDA de fecha, a
# partir de cuentas: la usan `.dominio_decidir_inferida()` sobre los valores y
# el resumen del motor sobre los agregados (`.resumen_fechas_dbi()`), para que
# las dos vias no puedan leer distinto. `serie` se llama solo cuando la grilla
# importa (hay futuras, alcanzan los valores, no es nacimiento ni registro):
# en el motor cuesta una consulta.
# - futuras: "calendario" si las fechas forman una grilla sin huecos que
#   arranca en el presente (a lo sumo un paso adelante); "columna" si pasan del
#   tope de una minoria; si no, "fila". Una tanda de fechas seguidas que empieza
#   lejos en el futuro (`fecha_registro` de 2030) no es un calendario.
# - centinelas: "columna" si la centinela es la mayoria de la columna.
.dominio_lecturas_fecha <- function(n, regla, n_futuras = 0, minimo = NA_real_,
                                    referencia = NA_real_,
                                    serie = function() NA_real_,
                                    n_centinelas = 0) {
  suficiente <- n >= .MIN_VALORES_GUARDA_DOMINIO
  futuras <- "fila"
  if (isTRUE(n_futuras > 0)) {
    paso <- if (suficiente && !isTRUE(regla$imposible_futuras) &&
                !isTRUE(regla$fecha_de_registro)) serie() else NA_real_
    if (!is.na(paso) && floor(minimo / 86400) <= floor(referencia / 86400) + paso) {
      futuras <- "calendario"
    } else if (suficiente && n_futuras / n > .UMBRAL_DOMINIO_INFERIDO_FUERA &&
               !isTRUE(regla$imposible_futuras)) {
      futuras <- "columna"
    }
  }
  centinelas <- if (suficiente && isTRUE(n_centinelas > 0) &&
                    n_centinelas / n >= .UMBRAL_CENTINELA_MAYORIA) {
    "columna"
  } else "fila"
  list(futuras = futuras, centinelas = centinelas)
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
  # Una abreviatura sin pareja solo es una coordenada si el contenido lo parece.
  if (isTRUE(regla$contenido_coordenada) &&
      !.dominio_parece_coordenada(valores$valores[indices], regla)) {
    salida$aplica <- FALSE
    return(salida)
  }
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
      salida$futuras <- .dominio_lecturas_fecha(
        n, regla, n_futuras = sum(futuras),
        minimo = min(valores$valores[indices]), referencia = referencia,
        serie = function() .dominio_serie_regular(valores$valores[indices])
      )$futuras
    }
  }
  if (length(regla$centinelas) && valores$fecha) {
    candidatos <- .dominio_fecha_numero(regla$centinelas, "la columna")
    coincide <- Reduce(`|`, lapply(candidatos, function(valor) {
      abs(valores$valores[indices] - valor) == 0
    }))
    if (any(coincide)) salida$hay <- TRUE
    # La mayoria se mide con lo mismo que despues se publica como centinela
    # (repetido y aislado), no con todo candidato que coincida. Y solo con las
    # incorporadas: una fecha declarada en `centinelas_fecha` se publica aparte
    # y no vuelve "de la columna" a una incorporada que es una minoria.
    repetidos <- .dominio_centinelas_repetidos(
      valores$valores, indices, regla$centinelas,
      regla$centinelas_declaradas %||% FALSE, "la columna",
      fin_abierto = isTRUE(regla$fin_vigencia)
    )
    publicados <- setdiff(repetidos$indices, repetidos$indices_declarados)
    salida$cuentas$centinelas <- c(length(publicados), n)
    salida$centinelas <- .dominio_lecturas_fecha(
      n, regla, n_centinelas = length(publicados)
    )$centinelas
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
                      abierto = FALSE, severidad = severidad_fila) {
    if (!length(indices)) return()
    por_columna <- identical(lectura, "columna")
    evidencia_base <- evidencia
    if (por_columna) {
      proporcion <- length(indices) / length(indices_evaluados)
      # Una abreviatura de coordenada solo avisa aunque lo de afuera sea una
      # minoria: no se dice que sea mas de lo que una minoria explica.
      aviso_coordenada <- isTRUE(regla$solo_aviso) &&
        identical(tipo_hallazgo, "valor_fuera_dominio") &&
        regla$tipo_dominio %in% c("latitud", "longitud")
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
            if (identical(tipo_hallazgo, "fecha_centinela")) "." else if (
              aviso_coordenada
            ) paste0(".", extra) else {
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
          } else if (aviso_coordenada) {
            paste0(") quedan afuera.", extra)
          } else {
            paste0(") quedan afuera, m\u00e1s de lo que una minor\u00eda de ",
                   "valores sueltos explica.", extra)
          }
        })
      )
      descripcion <- if (aviso_coordenada) {
        paste0(
          "Si es una coordenada, estos valores quedan fuera de [",
          .dominio_evidencia_numero(regla), "]. El nombre abreviado puede ser ",
          "otra cosa (`lat` de latencia) y ah\u00ed no es un error: por eso no ",
          "se acusan filas."
        )
      } else if (isTRUE(regla$solo_aviso) &&
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
    # Las fechas futuras de una grilla regular sin huecos: se dice lo observado
    # y la posibilidad, sin acusar filas.
    if (por_columna && identical(tipo_hallazgo, "fecha_futura") && inferida &&
        identical(sostenimiento$futuras, "calendario")) {
      evidencia <- paste0(
        evidencia_base, " Lectura de columna: ", length(indices), " de ",
        length(indices_evaluados), " valores (",
        .formatear_decimal_publicado(proporcion), ") son posteriores a la ",
        "fecha del perfil, y las fechas de la columna forman una grilla regular ",
        "sin huecos."
      )
      descripcion <- paste0(
        "Las fechas futuras forman una grilla regular sin huecos (diaria, ",
        "mensual...): puede ser un calendario o un pron\u00f3stico. No se ",
        "acusan filas."
      )
      sugerencia <- paste0(
        "Si la columna no es un calendario ni un pron\u00f3stico, revisar la ",
        "carga contra la fuente; declarar el dominio con `dominios` lo hace ",
        "expl\u00edcito."
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
      severidad = if (por_columna) "ok" else severidad,
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
        } else if (isTRUE(regla$solo_aviso) &&
                   regla$tipo_dominio %in% c("latitud", "longitud")) {
          paste0(
            " Lo que cabe en el rango tiene los decimales de una coordenada, ",
            "pero el nombre abreviado no alcanza para afirmar que lo sea."
          )
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
          lectura = if (!inferida) "fila" else if (
            identical(sostenimiento$futuras, "calendario")
          ) "columna" else sostenimiento$futuras,
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
  resumen_centinelas <- NULL
  if (length(regla$centinelas) && valores$fecha) {
    centinelas <- .dominio_centinelas_repetidos(
      valores$valores, indices_evaluados, regla$centinelas,
      if (declarada) TRUE else regla$centinelas_declaradas %||% FALSE, nombre,
      fin_abierto = inferida && isTRUE(regla$fin_vigencia)
    )
    indices_c <- centinelas$indices
    # Lo que declara `centinelas_fecha` sobre una regla inferida se publica
    # aparte, como una declaracion (`error`, por fila): no hereda la lectura de
    # las incorporadas ni cuenta para su mayoria.
    declaradas <- if (inferida) centinelas$indices_declarados else integer()
    indices_c <- setdiff(indices_c, declaradas)
    observados <- if (inferida) {
      setdiff(centinelas$observados, centinelas$observados_declarados)
    } else centinelas$observados
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
          "Centinelas observados: ", paste(observados, collapse = ", "),
          "; fuente: ", regla$fuente, "; filas afectadas: ",
          length(indices_c), "."
        ),
        textos_centinela[[1L]], textos_centinela[[2L]]
      )
    }
    if (length(declaradas)) {
      agregar(
        tipo_hallazgo = "fecha_centinela", indices = declaradas,
        lectura = "fila", severidad = "error",
        paste0(
          "Centinelas observados: ",
          paste(centinelas$observados_declarados, collapse = ", "),
          "; fuente: declarado (`centinelas_fecha`); filas afectadas: ",
          length(declaradas), "."
        ),
        textos_centinela[[1L]], textos_centinela[[2L]]
      )
    }
    resumen_centinelas <- list(
      n = length(abierto) + length(indices_c) + length(declaradas),
      n_declaradas = if (declarada) length(indices_c) else length(declaradas),
      n_fin_abierto = length(abierto),
      lectura = if (inferida && length(indices_c)) sostenimiento$centinelas else NA_character_,
      indices_declarados = if (declarada) indices_c else declaradas,
      incorporadas = if (declarada) numeric() else
        .dominio_fecha_numero(observados, nombre)
    )
  }
  attr(salida, "resumen_centinelas") <- resumen_centinelas
  salida
}

# Las filas de la columna `i` que cuentan para su regla: las de la muestra del
# perfil que son aplicables.
.dominio_universo <- function(datos, resultados, aplicables, i) {
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
  universo
}

.evaluar_dominios <- function(datos, nombres, resultados, dominios,
                              aplicables, fecha_hora,
                              fecha_es_date = FALSE,
                              centinelas_fecha = NULL,
                              resumen_fechas = FALSE) {
  # El contenido de la columna `j` parece una coordenada (de longitud si su
  # nombre lo es, de latitud si no): lo pide `long` como pareja.
  contenido <- function(j) {
    valores <- .dominio_valores(
      datos[[j]], resultados[[j]], .dominio_universo(datos, resultados, aplicables, j)
    )
    if (is.null(valores)) return(FALSE)
    t <- .dominio_tokens(nombres[[j]])
    lon <- any(t %in% c(.TOKENS_LONGITUD_COMPLETA, .TOKENS_LONGITUD_ABREVIADA))
    regla <- if (lon) list(minimo = -180, maximo = 180) else
      list(minimo = -90, maximo = 90)
    .dominio_parece_coordenada(valores$valores[valores$usable], regla)
  }
  reglas <- vector("list", length(nombres))
  names(reglas) <- nombres
  inferidos <- list()
  hallazgos <- list()
  # Lo que el resumen de fechas necesita de cada columna: la regla que se
  # aplico, como se leyo y las cuentas de centinelas que publicaron sus
  # hallazgos (ver `.resumen_fechas_dominios()`).
  aplicadas <- vector("list", length(nombres))
  for (i in seq_along(nombres)) {
    nombre <- nombres[[i]]
    elegida <- .dominio_regla_columna(
      nombre, i, nombres, resultados, dominios, centinelas_fecha, contenido
    )
    if (isTRUE(elegida$apagada)) {
      reglas[[i]] <- FALSE
      next
    }
    regla <- elegida$regla
    inferida <- elegida$inferida
    regla_publicada <- elegida$publicada
    reglas[[i]] <- regla
    if (!is.null(regla)) {
      universo <- .dominio_universo(datos, resultados, aplicables, i)
      aplicable <- aplicables[[i]]
      if (is.null(aplicable)) aplicable <- rep(TRUE, length(datos[[i]]))
      valores <- .dominio_valores(datos[[i]], resultados[[i]], universo)
      sostenimiento <- if (inferida) {
        if (is.null(valores)) {
          list(rango = "fila", futuras = "fila", centinelas = "fila",
               aplica = FALSE)
        } else {
          decidido <- .dominio_decidir_inferida(
            valores, regla, fecha_hora, fecha_es_date
          )
          # Que el contenido parezca una coordenada lo decide la tabla: un
          # grupo chico no la toma por coordenada ni deja de tomarla por su
          # cuenta.
          if (isTRUE(regla$contenido_coordenada) &&
              is.function(.CONTEXTO_POR$dominio_columna)) {
            de_tabla <- .CONTEXTO_POR$dominio_columna(nombre)
            if (!is.null(de_tabla$aplica)) decidido$aplica <- de_tabla$aplica
          }
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
      # La marca de fecha de registro es de uso interno: no se publica en las
      # reglas inferidas (el resto de la regla es la de siempre).
      if (inferida && aplicar_regla) {
        inferidos[[nombre]] <- regla_publicada[
          setdiff(names(regla_publicada), "fecha_de_registro")
        ]
      }
      aplicadas[[i]] <- list(
        regla = regla, inferida = inferida, sostenimiento = sostenimiento,
        aplicada = aplicar_regla, leida = !is.null(valores)
      )
      if (!aplicar_regla) {
        resultados[[i]]$dominio <- NULL
        next
      }
      hallazgos_columna <- .hallazgos_dominio_columna(
        datos[[i]], nombre, i, resultados[[i]], regla,
        aplicable, fecha_hora, fecha_es_date, valores, sostenimiento
      )
      aplicadas[[i]]$centinelas <- attr(
        hallazgos_columna, "resumen_centinelas", exact = TRUE
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
  salida <- list(
    resultados = resultados,
    reglas = reglas,
    hallazgos = hallazgos,
    meta = meta
  )
  if (isTRUE(resumen_fechas)) {
    salida$fechas <- .resumen_fechas_dominios(
      datos, nombres, resultados, aplicables, aplicadas, fecha_hora,
      fecha_es_date
    )
  }
  salida
}

# Las fechas que `centinelas_fecha` declara para toda la tabla, sumadas a la
# regla de una columna de fecha. Una regla inferida conserva sus cuatro
# incorporadas con su reserva y gana las declaradas, marcadas por candidata
# (`centinelas_declaradas`); si una declarada es una incorporada -el mismo
# instante, escrita como sea-, gana la declaracion. Una regla declarada en
# `dominios` con sus propias `centinelas` manda en su columna, como siempre; sin
# ellas, recibe las de la tabla. Sin regla (`dominios = FALSE`), la columna
# recibe una regla con solo las declaradas.
.dominio_regla_centinelas_fecha <- function(regla, centinelas_fecha, resultado,
                                            columna) {
  if (!length(centinelas_fecha)) return(regla)
  tipo <- as.character(resultado$inferencia$tipo)
  if (!length(tipo) || !tipo[[1L]] %in% c("fecha", "fecha-hora")) return(regla)
  if (is.null(regla)) {
    return(list(
      centinelas = centinelas_fecha, tipo_dominio = "fecha",
      fuente = "declarado"
    ))
  }
  if (identical(regla$fuente, "declarado")) {
    if (is.null(regla$centinelas)) regla$centinelas <- centinelas_fecha
    return(regla)
  }
  todas <- c(regla$centinelas, centinelas_fecha)
  marcas <- c(
    if (length(regla$centinelas_declaradas)) regla$centinelas_declaradas else
      rep(FALSE, length(regla$centinelas)),
    rep(TRUE, length(centinelas_fecha))
  )
  numeros <- .dominio_fecha_numero(todas, columna)
  primero <- !duplicated(numeros)
  regla$centinelas <- todas[primero]
  regla$centinelas_declaradas <- vapply(numeros[primero], function(v) {
    any(marcas[numeros == v])
  }, logical(1L))
  regla
}

# El resumen de fechas de cada columna (`resumen_fechas = TRUE`): lo que
# publicaron sus hallazgos, contado, mas lo que cuenta la misma definicion de
# futura (`.dominio_no_futuro()`) en toda columna de fecha -tambien en las que
# admiten futuras, donde no es un hallazgo sino un dato- y el ultimo dato para
# la actualidad. El ultimo dato excluye las futuras y las centinelas
# declaradas; una incorporada no sale de la estadistica (lo declarado sale, lo
# supuesto no). Devuelve una fila por columna, con `NA` en las que no son fecha.
.resumen_fechas_dominios <- function(datos, nombres, resultados, aplicables,
                                     aplicadas, fecha_hora, fecha_es_date) {
  referencia <- suppressWarnings(as.numeric(fecha_hora))
  filas <- lapply(seq_along(nombres), function(i) {
    vacia <- .fila_resumen_fechas(nombres[[i]])
    tipo <- as.character(resultados[[i]]$inferencia$tipo)
    if (!length(tipo) || !tipo[[1L]] %in% c("fecha", "fecha-hora")) return(vacia)
    valores <- .dominio_valores(
      datos[[i]], resultados[[i]], .dominio_universo(datos, resultados, aplicables, i)
    )
    if (is.null(valores) || !isTRUE(valores$fecha)) return(vacia)
    clase <- if (identical(tipo[[1L]], "fecha")) "fecha" else "fecha-hora"
    posiciones <- which(valores$usable)
    v <- valores$valores[posiciones]
    futura <- if (length(referencia) == 1L && is.finite(referencia)) {
      !.dominio_no_futuro(v, referencia, fecha_es_date)
    } else rep(NA, length(v))
    aplicada <- aplicadas[[i]]
    regla <- aplicada$regla
    centinelas <- aplicada$centinelas
    declaradas <- posiciones %in% (centinelas$indices_declarados %||% integer())
    .fila_resumen_fechas(
      nombres[[i]], clase = clase, n_valores = length(v),
      regla = regla, aplicada = aplicada, centinelas = centinelas,
      futura = futura, declaradas = declaradas, valores = v
    )
  })
  .apilar_filas(filas)
}

# Una fila del resumen de fechas. La usan memoria (sobre los valores) y el
# resumen del motor (sobre agregados, `.resumen_fechas_dbi()`): los mismos
# campos con los mismos nombres, para que `comparar_equivalencia()` los
# compare. Sin argumentos devuelve la fila vacia de una columna que no es fecha.
.fila_resumen_fechas <- function(columna, clase = NA_character_, n_valores = NA_real_,
                                 regla = NULL, aplicada = NULL, centinelas = NULL,
                                 futura = NULL, declaradas = NULL, valores = NULL,
                                 cuentas = NULL) {
  fila <- data.frame(
    columna = columna, clase_fecha = clase,
    n_fechas_centinela = NA_real_, n_fechas_centinela_declaradas = NA_real_,
    n_fechas_fin_abierto = NA_real_, lectura_fechas_centinela = NA_character_,
    n_fechas_futuras = NA_real_, lectura_fechas_futuras = NA_character_,
    ultimo_dato_fecha = NA_character_,
    n_evaluadas_actualidad = NA_real_,
    n_centinelas_declaradas_no_futuras = NA_real_,
    admite_futuras = NA, ultimo_dato_seg = NA_real_,
    ultimo_es_incorporada = NA, stringsAsFactors = FALSE
  )
  if (is.na(clase)) return(fila)
  # `cuentas` llega del motor ya contado; memoria cuenta sobre los valores.
  if (is.null(cuentas)) {
    cuentas <- list(
      n_futuras = if (anyNA(futura)) NA_real_ else sum(futura),
      ultimo = if (!anyNA(futura) && any(!futura & !declaradas)) {
        max(valores[!futura & !declaradas])
      } else NA_real_,
      n_evaluadas = if (anyNA(futura)) NA_real_ else sum(!futura & !declaradas),
      n_declaradas_no_futuras = if (anyNA(futura)) NA_real_ else
        sum(declaradas & !futura)
    )
  }
  tiene_centinelas <- !is.null(regla) && length(regla$centinelas)
  if (!tiene_centinelas) {
    fila$lectura_fechas_centinela <- "no_aplica"
  } else if (!isTRUE(aplicada$aplicada) ||
             (is.null(centinelas) && isTRUE(n_valores == 0))) {
    # La regla se evaluo y no aplico: ninguna candidata aparece en la columna
    # (o la columna no tiene valores).
    fila$n_fechas_centinela <- 0
    fila$n_fechas_centinela_declaradas <- 0
    fila$n_fechas_fin_abierto <- 0
  } else if (!is.null(centinelas)) {
    # Cuentas como dobles en las dos vias: el motor las trae asi.
    fila$n_fechas_centinela <- as.numeric(centinelas$n)
    fila$n_fechas_centinela_declaradas <- as.numeric(centinelas$n_declaradas)
    fila$n_fechas_fin_abierto <- as.numeric(centinelas$n_fin_abierto)
    fila$lectura_fechas_centinela <- centinelas$lectura
  }
  fila$n_fechas_futuras <- as.numeric(cuentas$n_futuras)
  admite <- is.null(regla) || !isTRUE(regla$no_futuras)
  fila$admite_futuras <- admite
  if (isTRUE(cuentas$n_futuras > 0)) {
    fila$lectura_fechas_futuras <- if (admite) {
      "admitidas"
    } else if (!isTRUE(aplicada$inferida)) {
      "fila"
    } else {
      aplicada$sostenimiento$futuras %||% "fila"
    }
  }
  fila$ultimo_dato_fecha <- .fecha_resumida(cuentas$ultimo, clase)
  fila$ultimo_dato_seg <- cuentas$ultimo
  # El ultimo dato puede ser una incorporada que la reserva acepto: no se
  # mueve -no se declaro-, pero se avisa.
  fila$ultimo_es_incorporada <- is.finite(cuentas$ultimo) &&
    any(abs((centinelas$incorporadas %||% numeric()) - cuentas$ultimo) == 0)
  fila$n_evaluadas_actualidad <- as.numeric(cuentas$n_evaluadas)
  fila$n_centinelas_declaradas_no_futuras <- as.numeric(cuentas$n_declaradas_no_futuras)
  fila
}

# La regla de dominio de una columna: la declarada en `dominios` (que manda), la
# inferida por el nombre y el tipo, o ninguna; con las fechas de
# `centinelas_fecha` sumadas. Una sola eleccion para `perfilar()` y para el
# resumen de fechas del motor (`.resumen_fechas_dbi()`), que no pueden aplicar
# reglas distintas a la misma columna. `contenido(j)` dice si la columna `j`
# parece una coordenada; el motor no lo necesita (las fechas no lo usan).
# Devuelve la regla, si es inferida, la que se publica como inferida (sin lo
# declarado) y `apagada` cuando la columna se declaro `FALSE` y no queda nada.
.dominio_regla_columna <- function(nombre, i, nombres, resultados, dominios,
                                   centinelas_fecha = character(),
                                   contenido = function(j) TRUE) {
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
      # Apagar el dominio de la columna no apaga lo que se declaro para toda la
      # tabla: `centinelas_fecha` sigue valiendo, sola.
      regla <- .dominio_regla_centinelas_fecha(
        NULL, centinelas_fecha, resultados[[i]], nombre
      )
      if (is.null(regla)) {
        return(list(regla = NULL, inferida = FALSE, publicada = NULL,
                    apagada = TRUE))
      }
    }
    regla$fuente <- "declarado"
  } else if (isTRUE(dominios$inferir)) {
    inferida <- TRUE
    regla <- .dominio_regla_inferida(
      nombre, .dominio_tokens(nombre), nombres, i, resultados, contenido
    )
    # En un grupo, la tabla decide si la abreviatura es una pareja o una
    # sola: un grupo que perdio la columna pareja no baja la severidad.
    if (isTRUE(regla$contenido_coordenada) &&
        is.function(.CONTEXTO_POR$dominio_columna)) {
      de_tabla <- .CONTEXTO_POR$dominio_columna(nombre)$regla
      if (!is.null(de_tabla)) regla <- de_tabla
    }
    # Una rebanada que perdio la columna pareja (`lng` toda ausente en el
    # grupo) no infiere la latitud por el nombre: la regla es la que infirio la
    # tabla entera.
    if (is.null(regla) && is.function(.CONTEXTO_POR$dominio_columna) &&
        any(.dominio_tokens(nombre) %in% c("lat", "lon", "lng", "long"))) {
      regla <- .CONTEXTO_POR$dominio_columna(nombre)$regla
    }
  } else {
    regla <- NULL
  }
  # La regla que se publica como inferida no lleva lo declarado.
  publicada <- regla
  regla <- .dominio_regla_centinelas_fecha(
    regla, centinelas_fecha, resultados[[i]], nombre
  )
  # Una regla que solo trae lo declarado para toda la tabla es una declaracion.
  if (inferida && is.null(publicada) && !is.null(regla)) inferida <- FALSE
  list(regla = regla, inferida = inferida, publicada = publicada,
       apagada = FALSE)
}
