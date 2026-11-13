# Spool externo de sesion cliente para `universo = "muestra_motor"`.
#
# El archivo no es un RDS unico: es una secuencia de registros serializados.
# Eso permite escribir y leer por chunks, cerrar siempre con un trailer y
# verificarlo sin volver a pedir filas al motor. El motor solo ve la consulta
# de seleccion; el resto de las pasadas lee este archivo.

.VERSION_SPOOL_MUESTRA_DBI <- "1"
.MAGIC_SPOOL_MUESTRA_DBI <- "lupa-spool-muestra-dbi"
.MAGIC_FIN_SPOOL_MUESTRA_DBI <- "lupa-spool-muestra-dbi-fin"
.BACKEND_SPOOL_MUESTRA_DBI <- "spool_sesion_cliente"
.MODULO_CHECKSUM_SPOOL_DBI <- 1000003
.RESERVA_TRAILER_SPOOL_DBI <- 4096

# Lo que dice el resumen cuando no hay un perfil de la materializacion pedida.
# Tiene dos puertas -el spool no se pudo leer y queda el perfil aislado, o se
# leyo y el perfil de todas sus filas fallo- y un solo texto.
.MOTIVO_RESUMEN_SIN_MATERIALIZACION_DBI <- paste(
  "La materializaci\u00f3n completa fall\u00f3; el perfil aislado es s\u00f3lo",
  "diagn\u00f3stico y no representa la materializaci\u00f3n solicitada.",
  "Las m\u00e9tricas del resumen quedan no_disponible."
)

.validar_presupuesto_bytes_dbi <- function(x, nombre) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 ||
      (!is.infinite(x) && x != floor(x))) {
    .detener_dbi(
      "lupa_error_argumento_dbi",
      paste0("`", nombre, "` debe ser un entero positivo o Inf.")
    )
  }
  if (is.infinite(x)) Inf else as.numeric(x)
}

.checksum_spool_vacio_dbi <- function() {
  list(bytes = 0, suma = 0, ponderada = 0, posicion = 0)
}

# Se usan dos sumas modulares sobre los bytes serializados. La segunda lleva la
# posicion relativa para detectar tambien permutaciones; las operaciones quedan
# por debajo de 2^53 dentro de cada chunk y no dependen de paquetes opcionales.
.checksum_spool_actualizar_dbi <- function(estado, raw) {
  if (!length(raw)) return(estado)
  valores <- as.double(as.integer(raw))
  posiciones <- (seq_along(valores) + estado$posicion) %%
    .MODULO_CHECKSUM_SPOOL_DBI
  estado$suma <- (estado$suma + sum(valores)) %%
    .MODULO_CHECKSUM_SPOOL_DBI
  estado$ponderada <- (estado$ponderada + sum(posiciones * valores)) %%
    .MODULO_CHECKSUM_SPOOL_DBI
  estado$bytes <- estado$bytes + length(raw)
  estado$posicion <- (estado$posicion + length(raw)) %%
    .MODULO_CHECKSUM_SPOOL_DBI
  estado
}

.checksum_spool_texto_dbi <- function(estado) {
  paste0(
    "lupa16:", estado$bytes, ":", format(estado$suma, scientific = FALSE,
                                           trim = TRUE), ":",
    format(estado$ponderada, scientific = FALSE, trim = TRUE)
  )
}

.eventos_spool_vacios_dbi <- function() {
  data.frame(
    tipo_evento = character(), motivo = character(), bloque_id = integer(),
    bytes_retenidos = numeric(), bytes_resultado = numeric(),
    stringsAsFactors = FALSE
  )
}

.evento_spool_dbi <- function(tipo, motivo, bloque_id = NA_integer_,
                              bytes_retenidos = NA_real_,
                              bytes_resultado = NA_real_) {
  data.frame(
    tipo_evento = as.character(tipo), motivo = as.character(motivo),
    bloque_id = as.integer(bloque_id),
    bytes_retenidos = as.numeric(bytes_retenidos),
    bytes_resultado = as.numeric(bytes_resultado),
    stringsAsFactors = FALSE
  )
}

.serializar_spool_dbi <- function(objeto) {
  serialize(objeto, NULL, version = 2)
}

.abrir_spool_muestra_dbi <- function(path, campos, muestra_id, snapshot_id,
                                     orden_id, max_bytes, backend,
                                     sql = NA_character_) {
  con <- file(path, open = "wb")
  header <- list(
    tipo = "header", magic = .MAGIC_SPOOL_MUESTRA_DBI,
    version = .VERSION_SPOOL_MUESTRA_DBI,
    campos = as.character(campos), muestra_id = as.character(muestra_id),
    snapshot_id = as.character(snapshot_id), orden_id = as.character(orden_id),
    presupuesto = as.numeric(max_bytes), backend = as.character(backend),
    sql = as.character(sql)
  )
  raw <- .serializar_spool_dbi(header)
  ok <- tryCatch({
    writeBin(raw, con)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    try(close(con), silent = TRUE)
    return(list(ok = FALSE, conexion = NULL,
                estado = .checksum_spool_vacio_dbi(), n_filas = 0,
                bloque_id = 0L, motivo = "spool_incompleto:escritura_fallida",
                eventos = .evento_spool_dbi(
                  "spool_escritura_fallida", "spool_incompleto:escritura_fallida"
                )))
  }
  list(
    ok = TRUE, conexion = con,
    estado = .checksum_spool_actualizar_dbi(
      .checksum_spool_vacio_dbi(), raw
    ), n_filas = 0, bloque_id = 0L, n_chunks = 0L,
    bytes_bloques = numeric(),
    campos = as.character(campos), muestra_id = as.character(muestra_id),
    snapshot_id = as.character(snapshot_id), orden_id = as.character(orden_id),
    max_bytes = max_bytes, eventos = .eventos_spool_vacios_dbi(),
    trailer_reserva = .RESERVA_TRAILER_SPOOL_DBI, truncada = FALSE,
    motivo = NULL, filas_bloques = integer()
  )
}

.escribir_chunk_spool_muestra_dbi <- function(escritor, datos) {
  if (!is.data.frame(datos)) {
    escritor$motivo <- "muestra_inestable:chunk_no_data_frame"
    return(escritor)
  }
  n <- nrow(datos)
  if (!n) return(escritor)
  ordinal_inicio <- escritor$n_filas + 1
  registro <- list(
    tipo = "chunk", ordinal_inicio = ordinal_inicio,
    ordinal_fin = ordinal_inicio + n - 1,
    n_filas = n, payload = datos
  )
  raw <- .serializar_spool_dbi(registro)
  bytes_antes <- escritor$estado$bytes
  if (is.finite(escritor$max_bytes) &&
      bytes_antes + length(raw) + escritor$trailer_reserva >
        escritor$max_bytes) {
    motivo <- "muestra_inestable:presupuesto_materializacion"
    capacidad <- escritor$max_bytes - bytes_antes - escritor$trailer_reserva
    aceptadas <- 0L
    if (capacidad > 0) {
      # Los bloques que llegan desde el motor ya estan acotados por la cota de
      # fila. Si la serializacion aun excede el saldo, se conserva un prefijo
      # recorriendo ese bloque chico; nunca se vuelve a serializar un bloque de
      # gigabytes en una busqueda binaria.
      for (i in seq_len(n)) {
        parcial <- datos[seq_len(i), , drop = FALSE]
        bruto <- .serializar_spool_dbi(list(
          tipo = "chunk", ordinal_inicio = ordinal_inicio,
          ordinal_fin = ordinal_inicio + i - 1L, n_filas = i,
          payload = parcial
        ))
        if (length(bruto) > capacidad) break
        aceptadas <- i
        raw <- bruto
      }
    }
    if (aceptadas > 0L) {
      datos <- datos[seq_len(aceptadas), , drop = FALSE]
      n <- aceptadas
      registro <- list(
        tipo = "chunk", ordinal_inicio = ordinal_inicio,
        ordinal_fin = ordinal_inicio + n - 1L, n_filas = n, payload = datos
      )
      raw <- .serializar_spool_dbi(registro)
      escritor$truncada <- TRUE
    } else {
      # Un chunk posterior puede no entrar aunque el prefijo ya sea un spool
      # valido. En ese caso se cierra como materializacion recortada y se
      # conserva lo que entro; solo la primera fila que no cabe deja la
      # materializacion sin resultado util y conserva el fallo historico.
      if (escritor$n_filas > 0L) {
        escritor$truncada <- TRUE
      } else {
        escritor$motivo <- motivo
      }
    }
    escritor$eventos <- rbind(
      escritor$eventos,
      .evento_spool_dbi(
        "spool_presupuesto_excedido", motivo,
        bloque_id = escritor$bloque_id + 1L,
        bytes_retenidos = bytes_antes, bytes_resultado = length(raw)
      )
    )
    if (!is.null(escritor$motivo) ||
        (isTRUE(escritor$truncada) && aceptadas == 0L)) {
      return(escritor)
    }
  }
  escrito <- tryCatch({
    writeBin(raw, escritor$conexion)
    TRUE
  }, error = function(e) FALSE)
  if (!escrito) {
    escritor$motivo <- "spool_incompleto:escritura_fallida"
    escritor$eventos <- rbind(
      escritor$eventos,
      .evento_spool_dbi(
        "spool_escritura_fallida", escritor$motivo,
        bloque_id = escritor$bloque_id + 1L,
        bytes_retenidos = bytes_antes, bytes_resultado = length(raw)
      )
    )
    return(escritor)
  }
  escritor$estado <- .checksum_spool_actualizar_dbi(escritor$estado, raw)
  escritor$n_filas <- escritor$n_filas + n
  escritor$bloque_id <- escritor$bloque_id + 1L
  escritor$n_chunks <- escritor$n_chunks + 1L
  escritor$bytes_bloques <- c(escritor$bytes_bloques, as.numeric(length(raw)))
  escritor$filas_bloques <- c(escritor$filas_bloques, as.integer(n))
  escritor
}

.cerrar_spool_muestra_dbi <- function(escritor) {
  if (!is.null(escritor$conexion)) {
    try(close(escritor$conexion), silent = TRUE)
  }
  if (!is.null(escritor$motivo)) {
    return(list(
      ok = FALSE, motivo = escritor$motivo, n_filas = escritor$n_filas,
      bytes = NA_real_, checksum = NA_character_,
      n_chunks = escritor$n_chunks, bytes_bloques = escritor$bytes_bloques,
      eventos = escritor$eventos, truncada = isTRUE(escritor$truncada)
    ))
  }
  trailer <- list(
    tipo = "trailer", magic = .MAGIC_SPOOL_MUESTRA_DBI,
    magic_fin = .MAGIC_FIN_SPOOL_MUESTRA_DBI,
    version = .VERSION_SPOOL_MUESTRA_DBI,
    muestra_id = escritor$muestra_id, snapshot_id = escritor$snapshot_id,
    orden_id = escritor$orden_id, n_filas = escritor$n_filas,
    bytes = escritor$estado$bytes,
    checksum = .checksum_spool_texto_dbi(escritor$estado),
    n_chunks = escritor$n_chunks,
    bytes_payload = escritor$estado$bytes,
    checksum_payload = .checksum_spool_texto_dbi(escritor$estado)
  )
  raw <- .serializar_spool_dbi(trailer)
  con <- tryCatch(file(escritor$path, open = "ab"), error = function(e) NULL)
  if (is.null(con)) {
    return(list(
      ok = FALSE, motivo = "spool_incompleto:escritura_fallida",
      n_filas = escritor$n_filas, bytes = NA_real_, checksum = NA_character_,
      n_chunks = escritor$n_chunks, bytes_bloques = escritor$bytes_bloques,
      eventos = rbind(escritor$eventos, .evento_spool_dbi(
        "spool_escritura_fallida", "spool_incompleto:escritura_fallida"
      )), truncada = isTRUE(escritor$truncada)
    ))
  }
  escrito <- tryCatch({
    writeBin(raw, con)
    TRUE
  }, error = function(e) FALSE)
  try(close(con), silent = TRUE)
  if (!escrito) {
    return(list(
      ok = FALSE, motivo = "spool_incompleto:escritura_fallida",
      n_filas = escritor$n_filas, bytes = NA_real_, checksum = NA_character_,
      n_chunks = escritor$n_chunks, bytes_bloques = escritor$bytes_bloques,
      eventos = rbind(escritor$eventos, .evento_spool_dbi(
        "spool_escritura_fallida", "spool_incompleto:escritura_fallida"
      )), truncada = isTRUE(escritor$truncada)
    ))
  }
  info <- file.info(escritor$path)
  list(
    ok = TRUE, motivo = NA_character_, n_filas = escritor$n_filas,
    bytes = as.numeric(info$size[[1L]]),
    checksum = trailer$checksum, n_chunks = escritor$n_chunks,
    bytes_bloques = escritor$bytes_bloques, eventos = escritor$eventos,
    truncada = isTRUE(escritor$truncada),
    motivo = if (isTRUE(escritor$truncada)) {
      "muestra_inestable:presupuesto_materializacion"
    } else NA_character_
  )
}

# Helper de contrato: escribe una lista de data frames por chunks, sin conocer
# el motor. Los jueces lo usan para probar trailer, checksum y presupuesto.
.escribir_spool_muestra_dbi <- function(chunks, path = tempfile(
    "lupa-spool-muestra-", fileext = ".bin"), campos = character(),
    muestra_id = "muestra-1", snapshot_id = "snapshot-1",
    orden_id = "orden-1", max_bytes = Inf, backend = "prueba",
    sql = NA_character_) {
  max_bytes <- .validar_presupuesto_bytes_dbi(max_bytes, "max_bytes")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  escritor <- .abrir_spool_muestra_dbi(
    path, campos, muestra_id, snapshot_id, orden_id, max_bytes, backend, sql
  )
  if (!isTRUE(escritor$ok)) return(c(escritor, list(path = path)))
  escritor$path <- path
  for (datos in chunks) {
    escritor <- .escribir_chunk_spool_muestra_dbi(escritor, datos)
    if (!is.null(escritor$motivo)) break
  }
  salida <- .cerrar_spool_muestra_dbi(escritor)
  salida$path <- path
  if (isTRUE(salida$ok)) {
    verificada <- .leer_spool_muestra_dbi(path, devolver_chunks = FALSE)
    if (!isTRUE(verificada$ok)) {
      salida$ok <- FALSE
      salida$motivo <- verificada$motivo
    }
  }
  salida
}

.leer_spool_muestra_dbi <- function(path, devolver_chunks = TRUE) {
  if (!file.exists(path)) {
    return(list(ok = FALSE, motivo = "spool_incompleto:trailer_ausente"))
  }
  con <- tryCatch(file(path, open = "rb"), error = function(e) NULL)
  if (is.null(con)) {
    return(list(ok = FALSE, motivo = "spool_incompleto:trailer_ausente"))
  }
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  estado <- .checksum_spool_vacio_dbi()
  leer <- function() tryCatch(unserialize(con), error = function(e) e)
  header <- leer()
  if (inherits(header, "condition") || !identical(header$tipo, "header") ||
      !identical(header$magic, .MAGIC_SPOOL_MUESTRA_DBI) ||
      !identical(as.character(header$version), .VERSION_SPOOL_MUESTRA_DBI)) {
    return(list(ok = FALSE, motivo = "spool_checksum_invalido"))
  }
  estado <- .checksum_spool_actualizar_dbi(
    estado, .serializar_spool_dbi(header)
  )
  chunks <- list()
  filas <- 0
  ordinal <- 1
  trailer <- NULL
  n_chunks <- 0L
  repeat {
    registro <- leer()
    if (inherits(registro, "condition")) {
      return(list(ok = FALSE, motivo = "spool_incompleto:trailer_ausente"))
    }
    if (identical(registro$tipo, "chunk")) {
      if (!is.data.frame(registro$payload) ||
          !identical(as.numeric(registro$n_filas), as.numeric(nrow(registro$payload))) ||
          !identical(as.numeric(registro$ordinal_inicio), as.numeric(ordinal))) {
        return(list(ok = FALSE, motivo = "spool_checksum_invalido"))
      }
      raw <- .serializar_spool_dbi(registro)
      estado <- .checksum_spool_actualizar_dbi(estado, raw)
      chunks[[length(chunks) + 1L]] <- registro$payload
      n_chunks <- n_chunks + 1L
      n <- nrow(registro$payload)
      filas <- filas + n
      ordinal <- ordinal + n
      next
    }
    if (identical(registro$tipo, "trailer")) {
      trailer <- registro
      break
    }
    return(list(ok = FALSE, motivo = "spool_checksum_invalido"))
  }
  checksum <- .checksum_spool_texto_dbi(estado)
  if (!identical(trailer$magic, .MAGIC_SPOOL_MUESTRA_DBI) ||
      !identical(trailer$magic_fin, .MAGIC_FIN_SPOOL_MUESTRA_DBI) ||
      !identical(as.character(trailer$version), .VERSION_SPOOL_MUESTRA_DBI) ||
      !identical(as.character(trailer$muestra_id),
                 as.character(header$muestra_id)) ||
      !identical(as.character(trailer$snapshot_id),
                 as.character(header$snapshot_id)) ||
      !identical(as.character(trailer$orden_id), as.character(header$orden_id)) ||
      !identical(as.numeric(trailer$n_filas), as.numeric(filas)) ||
      !identical(as.numeric(trailer$n_chunks), as.numeric(n_chunks)) ||
      !identical(as.numeric(trailer$bytes), as.numeric(estado$bytes)) ||
      !identical(as.numeric(trailer$bytes_payload), as.numeric(estado$bytes)) ||
      !identical(as.character(trailer$checksum), checksum) ||
      !identical(as.character(trailer$checksum_payload), checksum)) {
    return(list(ok = FALSE, motivo = "spool_checksum_invalido"))
  }
  datos <- if (isTRUE(devolver_chunks)) {
    if (length(chunks)) do.call(rbind, chunks) else {
      campos <- as.character(header$campos)
      as.data.frame(stats::setNames(replicate(length(campos), logical(), simplify = FALSE),
                             campos), stringsAsFactors = FALSE)
    }
  } else NULL
  list(
    ok = TRUE, motivo = NA_character_, datos = datos, chunks = chunks,
    header = header, trailer = trailer, n_filas = filas,
    n_chunks = n_chunks,
    bytes = as.numeric(file.info(path)$size[[1L]]), checksum = checksum
  )
}

.verificar_spool_muestra_dbi <- function(path) {
  .leer_spool_muestra_dbi(path, devolver_chunks = FALSE)
}

.materializacion_spool_vacia_dbi <- function(backend, max_bytes,
                                             muestra_id, snapshot_id, orden_id,
                                             estado = "no_disponible",
                                             motivo = NA_character_) {
  list(
    backend = as.character(backend), version = .VERSION_SPOOL_MUESTRA_DBI,
    muestra_id = as.character(muestra_id), snapshot_id = as.character(snapshot_id),
    orden_id = as.character(orden_id), checksum = NA_character_, bytes = NA_real_,
    n_filas = NA_real_, presupuesto = as.numeric(max_bytes),
    estado = estado, motivo = if (is.null(motivo)) NA_character_ else
      as.character(motivo),
    externo = TRUE, objetos_temporales_motor = FALSE,
    validado_relectura = FALSE
  )
}

# La consulta de catalogo que estima los bytes por fila y las filas de la
# tabla, o `NULL` si el motor no tiene una o el nombre no se puede escribir como
# literal. Vive aparte para que el plan sepa, con la misma pregunta, si la
# corrida la va a mandar (ver `.consultas_catalogo_aviso_dbi()`).
.sql_catalogo_bytes_fila_dbi <- function(conexion, tabla, campos = NULL,
                                         tipos_motor = NULL) {
  senas <- .senas_conexion_dbi(conexion)
  nombre <- .texto_tabla_dbi(tabla)
  literal <- tryCatch(
    as.character(DBI::dbQuoteString(conexion, nombre)),
    error = function(e) NA_character_
  )
  if (length(literal) != 1L || is.na(literal) || !nzchar(literal)) {
    return(NULL)
  }
  partes <- strsplit(gsub('"', "", nombre, fixed = TRUE), ".", fixed = TRUE)[[1L]]
  tabla_nombre <- partes[[length(partes)]]
  esquema_nombre <- if (length(partes) > 1L) {
    partes[[length(partes) - 1L]]
  } else NA_character_
  quote_texto <- function(x) {
    tryCatch(as.character(DBI::dbQuoteString(conexion, x)),
             error = function(e) NA_character_)
  }
  if (grepl("postgres|pqconnection", senas, perl = TRUE)) {
    esquema <- if (is.na(esquema_nombre)) "current_schema()" else
      quote_texto(esquema_nombre)
    tabla_citada <- quote_texto(tabla_nombre)
    if (is.na(tabla_citada) || is.na(esquema)) return(NULL)
    nombres_proyeccion <- character()
    binarios <- logical()
    if (length(campos) && length(tipos_motor) == length(campos)) {
      nombres_proyeccion <- vapply(as.character(campos), quote_texto, character(1L))
      binarios <- vapply(seq_along(campos), function(i) {
        .es_tipo_binario_muestra_dbi(
          .atributo_columna_dbi(tipos_motor, campos[[i]], i) %||%
            NA_character_, senas
        )
      }, logical(1L))
      if (any(is.na(nombres_proyeccion))) {
        nombres_proyeccion <- character()
        binarios <- logical()
      }
    }
    expresion_proyeccion <- if (length(nombres_proyeccion)) {
      paste0(
        "(SELECT CASE WHEN COUNT(CASE WHEN a.attname NOT IN (",
        paste(nombres_proyeccion[binarios], collapse = ","),
        ") THEN s.attname END) = COUNT(CASE WHEN a.attname NOT IN (",
        paste(nombres_proyeccion[binarios], collapse = ","),
        ") THEN 1 END) THEN ",
        "SUM(CASE WHEN a.attname IN (",
        paste(nombres_proyeccion[binarios], collapse = ","),
        ") THEN 64 ELSE COALESCE(s.avg_width, 0) END) ELSE NULL END ",
        "FROM pg_attribute a LEFT JOIN pg_stats s ON s.schemaname = n.nspname ",
        "AND s.tablename = c.relname AND s.attname = a.attname ",
        "WHERE a.attrelid = c.oid AND a.attnum > 0 AND NOT a.attisdropped)"
      )
    } else "NULL"
    if (length(nombres_proyeccion) && !any(binarios)) {
      expresion_proyeccion <- paste0(
        "(SELECT CASE WHEN COUNT(s.attname) = COUNT(a.attname) THEN ",
        "SUM(COALESCE(s.avg_width, 0)) ELSE NULL END ",
        "FROM pg_attribute a LEFT JOIN pg_stats s ON s.schemaname = n.nspname ",
        "AND s.tablename = c.relname AND s.attname = a.attname ",
        "WHERE a.attrelid = c.oid AND a.attnum > 0 AND NOT a.attisdropped)"
      )
    }
    estimacion <- paste0(
      "COALESCE(", expresion_proyeccion,
      ", pg_table_size(c.oid) / NULLIF(c.reltuples, 0))"
    )
    sql <- paste0(
      "SELECT CASE WHEN c.reltuples > 0 THEN ", estimacion,
      " ELSE NULL END AS lupa_bytes_fila, c.reltuples AS lupa_filas_catalogo ",
      "FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace ",
      "WHERE n.nspname = ", esquema, " AND c.relname = ", tabla_citada
    )
    return(list(
      motor = "postgres", sql = sql,
      proyeccion = length(nombres_proyeccion) > 0L
    ))
  }
  if (grepl("sql server|microsoft sql|sqlserver|mssql", senas, perl = TRUE)) {
    sql <- paste0(
      "WITH lupa_filas AS (SELECT SUM(p.rows) AS row_count FROM ",
      "sys.partitions AS p WHERE p.object_id = OBJECT_ID(", literal,
      ") AND p.index_id IN (0, 1)), lupa_paginas AS (SELECT ",
      "SUM(au.total_pages) AS total_pages FROM sys.partitions AS p ",
      "JOIN sys.allocation_units AS au ON (au.type IN (1, 3) AND ",
      "au.container_id = p.hobt_id) OR (au.type = 2 AND ",
      "au.container_id = p.partition_id) WHERE p.object_id = OBJECT_ID(",
      literal, ") AND p.index_id IN (0, 1)) SELECT ",
      "lupa_paginas.total_pages * 8192.0 / NULLIF(lupa_filas.row_count, 0) ",
      "AS lupa_bytes_fila, lupa_filas.row_count AS lupa_filas_catalogo, ",
      "CONVERT(int, SERVERPROPERTY('ProductMajorVersion')) AS ",
      "lupa_version_mayor FROM lupa_filas CROSS JOIN lupa_paginas"
    )
    return(list(motor = "sqlserver", sql = sql, proyeccion = FALSE))
  }
  NULL
}

# `contar_en` es un presupuesto donde se cuenta la consulta sin pasar por el:
# la del aviso de costo no puede quedar fuera por `max_consultas` -se manda
# siempre, antes de los agregados- pero viaja al motor y el plan la anticipa.
.estimar_bytes_fila_muestra_dbi <- function(
    conexion, tabla, presupuesto, campos = NULL, tipos_motor = NULL,
    contar_en = NULL) {
  vacia <- list(
    disponible = FALSE, bytes_por_fila = NA_real_, filas_catalogo = NA_real_,
    fuente = NA_character_, motivo = "sin_estimacion_catalogo",
    version_sqlserver = NA_real_,
    consulta = .medicion_consulta_vacia_dbi("catalogo_bytes_muestra")
  )
  consulta <- .sql_catalogo_bytes_fila_dbi(conexion, tabla, campos, tipos_motor)
  if (is.null(consulta)) return(vacia)
  .contar_dbi(contar_en)
  resultado <- .consultar_dbi(
    conexion, consulta$sql, presupuesto, etapa = "catalogo_bytes_muestra"
  )
  if (!isTRUE(resultado$ok) || !is.data.frame(resultado$datos) ||
      !nrow(resultado$datos)) return(vacia)
  bytes <- suppressWarnings(as.numeric(resultado$datos[[1L]][[1L]]))
  filas <- if (ncol(resultado$datos) > 1L) {
    suppressWarnings(as.numeric(resultado$datos[[2L]][[1L]]))
  } else NA_real_
  if (identical(consulta$motor, "postgres")) {
    if (!is.finite(bytes) || bytes <= 0) return(vacia)
    return(list(
      disponible = TRUE, bytes_por_fila = bytes, filas_catalogo = filas,
      fuente = if (isTRUE(consulta$proyeccion)) {
        "pg_stats.avg_width sobre la proyecci\u00f3n; pg_table_size / reltuples como respaldo"
      } else "pg_table_size / pg_class.reltuples",
      motivo = NA_character_, version_sqlserver = NA_real_,
      consulta = resultado
    ))
  }
  version <- if (ncol(resultado$datos) > 2L) {
    suppressWarnings(as.numeric(resultado$datos[[3L]][[1L]]))
  } else NA_real_
  if (!is.finite(bytes) || bytes <= 0) return(vacia)
  list(
    disponible = TRUE, bytes_por_fila = bytes, filas_catalogo = filas,
    fuente = paste(
      "sys.partitions + sys.allocation_units total_pages * 8192 / row_count"
    ),
    motivo = NA_character_, version_sqlserver = version,
    consulta = resultado
  )
}

.alternativa_muestreo_reintento_dbi <- function(
    candidatos, metodo_inicial, senas, n_total, objetivo,
    medianas_pendientes = 1L) {
  if (!length(candidatos) || !length(metodo_inicial) ||
      is.na(metodo_inicial)) {
    return(list(candidato = NULL, guardia = NULL, motivo = NA_character_))
  }
  inicial <- candidatos[[as.character(metodo_inicial)]]
  por_bloques <- if (!is.null(inicial) && identical(inicial$tipo, "tablesample")) {
    inicial
  } else {
    bloques <- candidatos[vapply(
      candidatos, function(x) identical(x$tipo, "tablesample"), logical(1L)
    )]
    if (length(bloques)) bloques[[1L]] else NULL
  }
  alternativa <- NULL
  motivo <- NA_character_
  if (grepl("postgres|pqconnection", senas, perl = TRUE) &&
      "tablesample_bernoulli" %in% names(candidatos)) {
    alternativa <- candidatos[["tablesample_bernoulli"]]
    motivo <- "Se prefiri\u00f3 BERNOULLI para recuperar filas sin ordenar la tabla."
  } else if (!is.null(por_bloques)) {
    alternativa <- por_bloques
    motivo <- paste(
      "Se mantuvo el m\u00e9todo por bloques y se aument\u00f3 la fracci\u00f3n para evitar",
      "un ordenamiento de la tabla completa."
    )
  } else if ("random_limit" %in% names(candidatos)) {
    alternativa <- candidatos[["random_limit"]]
    funcion <- if (length(alternativa$funciones)) {
      alternativa$funciones[[1L]]$nombre
    } else "random"
    guardia <- .evaluar_guardia_aleatorio_dbi(
      n_total, objetivo, funcion, medianas_pendientes
    )
    if (!isTRUE(guardia$aceptado)) {
      return(list(
        candidato = NULL, guardia = guardia,
        motivo = paste(
          "El fallback aleatorio no se ejecut\u00f3 porque la guarda de costo lo",
          "rechaz\u00f3 y no hay un m\u00e9todo por bloques utilizable."
        )
      ))
    }
    motivo <- paste0(
      "Se us\u00f3 `", funcion,
      "` porque el dialecto no ofreci\u00f3 un m\u00e9todo por bloques y la guarda de",
      " costo lo acept\u00f3."
    )
    return(list(candidato = alternativa, guardia = guardia, motivo = motivo))
  }
  list(candidato = alternativa, guardia = NULL, motivo = motivo)
}

.mejor_spool_muestra_dbi <- function(primero, segundo) {
  if (!isTRUE(primero$ok)) return(segundo)
  if (!isTRUE(segundo$ok)) return(primero)
  n_primero <- suppressWarnings(as.numeric(primero$n_filas))
  n_segundo <- suppressWarnings(as.numeric(segundo$n_filas))
  if (is.finite(n_segundo) && (!is.finite(n_primero) || n_segundo > n_primero)) {
    segundo
  } else {
    primero
  }
}

.materializar_muestra_spool_dbi <- function(conexion, sql, campos, muestra_id,
                                           snapshot_id, orden_id, presupuesto,
                                           bloque_filas = 10000,
                                           max_bytes = Inf, backend = NULL,
                                           max_filas = Inf,
                                           bytes_por_fila_max = NA_real_,
                                           medir_bytes = FALSE) {
  max_bytes <- .validar_presupuesto_bytes_dbi(
    max_bytes, "max_bytes_materializacion"
  )
  bloque_filas <- .validar_bloque_filas_dbi(bloque_filas)
  if (!is.numeric(max_filas) || length(max_filas) != 1L ||
      is.na(max_filas) || max_filas < 0 ||
      (is.finite(max_filas) && max_filas != floor(max_filas))) {
    .detener_dbi(
      "lupa_error_argumento_dbi",
      "`max_filas` debe ser un entero no negativo o Inf."
    )
  }
  max_filas <- as.numeric(max_filas)
  bloque_maximo <- bloque_filas
  backend <- backend %||% .BACKEND_SPOOL_MUESTRA_DBI
  path <- tempfile("lupa-spool-muestra-", fileext = ".bin")
  resultado <- NULL
  escritor <- NULL
  materializacion_exitosa <- FALSE
  limpiar_salida_error <- function() {
    if (!is.null(resultado)) {
      try(.limpiar_consulta_dbi(resultado, conexion), silent = TRUE)
    }
    if (!is.null(escritor) && !is.null(escritor$conexion)) {
      try(close(escritor$conexion), silent = TRUE)
    }
    if (!isTRUE(materializacion_exitosa)) {
      try(unlink(path, force = TRUE), silent = TRUE)
    }
  }
  on.exit(limpiar_salida_error(), add = TRUE)
  escritor <- .abrir_spool_muestra_dbi(
    path, campos, muestra_id, snapshot_id, orden_id, max_bytes, backend, sql
  )
  if (!isTRUE(escritor$ok)) {
    return(list(
      ok = FALSE, path = path, consulta = NULL, n_filas = 0,
      bytes = NA_real_, checksum = NA_character_,
      fetches = 0L, bytes_bloques = numeric(),
      motivo = escritor$motivo, eventos = escritor$eventos, truncada = FALSE
    ))
  }
  escritor$path <- path
  medicion <- NULL
  if (!.gastar_dbi(presupuesto)) {
    escritor$motivo <- .motivo_presupuesto_dbi(presupuesto)
    cerrado <- .cerrar_spool_muestra_dbi(escritor)
    return(list(
      ok = FALSE, path = path, consulta = NULL, n_filas = NA_real_,
      bytes = NA_real_, checksum = NA_character_, fetches = 0L,
      bytes_bloques = cerrado$bytes_bloques, motivo = escritor$motivo,
      eventos = cerrado$eventos, truncada = isTRUE(cerrado$truncada)
    ))
  }
  medicion <- .iniciar_consulta_dbi(presupuesto, "materializacion_muestra")
  error_lectura <- NULL
  fetches <- 0L
  bytes_traidos_r <- 0
  bytes_por_fila_medido <- NA_real_
  resultado <- tryCatch(
    .enviar_consulta_dbi(conexion, sql),
    error = function(e) {
      .repropagar_transaccion_dbi(e)
      error_lectura <<- .mensaje_motor(e)
      NULL
    }
  )
  if (is.null(resultado)) {
    # `dbSendQuery()` no pudo abrir el result set: el archivo queda sin trailer
    # y la salida nunca puede mezclarlo con una lectura alternativa.
    if (is.null(error_lectura)) {
      error_lectura <- "muestra_inestable:error_lectura_spool"
    }
  }
  n_filas <- 0
  repeat {
    if (is.null(resultado)) break
    restantes <- if (is.finite(max_filas)) max_filas - n_filas else Inf
    if (restantes <= 0) break
    n_pedir <- if (is.finite(restantes)) min(bloque_filas, restantes) else {
      bloque_filas
    }
    if (is.finite(max_bytes)) {
      saldo <- max_bytes - escritor$estado$bytes - escritor$trailer_reserva
      # La cota de tipos solo decide que se puede pedir al menos una fila en la
      # via adaptativa. Una vez que llega, la capacidad se mide con la fila
      # real; pedir un bloque calculado con la cota conservadora era lo que
      # convertia una tabla de filas chicas en una muestra vacia. Una fila por
      # fetch mantiene tambien el tope de memoria si la fila siguiente es mas
      # grande que la primera.
      if (saldo < 1) {
        escritor$truncada <- TRUE
        escritor$eventos <- rbind(
          escritor$eventos,
          .evento_spool_dbi(
            "spool_presupuesto_excedido",
            "muestra_inestable:presupuesto_materializacion",
            bloque_id = escritor$bloque_id + 1L,
            bytes_retenidos = escritor$estado$bytes,
            bytes_resultado = NA_real_
          )
        )
        break
      }
      if (isTRUE(medir_bytes)) {
        # La primera fila se pide sola y se mide; las siguientes, en bloques
        # con el doble de margen sobre la fila mas grande medida hasta ahi. Fila
        # por fila costaba unos 12 ms cada una: 3.196 filas de 3 KB bajo un
        # tope de 10 MB tardaban 39 s. El escritor sigue sin pasar el tope
        # aunque una fila posterior sea mas grande que las medidas.
        n_pedir <- if (is.finite(bytes_por_fila_medido) &&
                       bytes_por_fila_medido > 0) {
          max(1L, min(n_pedir, as.integer(floor(
            saldo / (2 * bytes_por_fila_medido)
          ))))
        } else 1L
      } else {
        capacidad <- if (is.finite(bytes_por_fila_max) &&
                         bytes_por_fila_max > 0) {
          floor(saldo / bytes_por_fila_max)
        } else NA_real_
        if (is.finite(capacidad) && capacidad < 1) {
          escritor$truncada <- TRUE
          escritor$eventos <- rbind(
            escritor$eventos,
            .evento_spool_dbi(
              "spool_presupuesto_excedido",
              "muestra_inestable:presupuesto_materializacion",
              bloque_id = escritor$bloque_id + 1L,
              bytes_retenidos = escritor$estado$bytes,
              bytes_resultado = NA_real_
            )
          )
          break
        }
        n_pedir <- if (is.finite(capacidad)) min(n_pedir, capacidad) else {
          min(n_pedir, 100L)
        }
      }
    } else if (!is.finite(bytes_por_fila_max)) {
      # Sin una cota del motor se sondea un bloque pequeno y la escritura
      # adapta el prefijo que entra; la proyeccion de tipos extensos evita esta
      # via en los motores que pueden resumirlos antes del fetch.
      n_pedir <- min(n_pedir, 100L)
    }
    if (n_pedir < 1) break
    datos <- tryCatch(
      .marcar_utf8_tabla(.traer_filas_dbi(
        resultado, n = as.integer(n_pedir), conexion
      )),
      error = function(e) {
        .repropagar_transaccion_dbi(e)
        error_lectura <<- .mensaje_motor(e)
        NULL
      }
    )
    fetches <- fetches + 1L
    if (!is.null(error_lectura)) break
    if (is.null(datos) || !nrow(datos)) break
    bytes_bloque_r <- as.numeric(utils::object.size(datos))
    bytes_traidos_r <- bytes_traidos_r + bytes_bloque_r
    escritor <- .escribir_chunk_spool_muestra_dbi(escritor, datos)
    n_filas <- escritor$n_filas
    if (!is.null(escritor$motivo)) break
    bytes_chunk <- utils::tail(escritor$bytes_bloques, 1L)
    if (length(bytes_chunk) && nrow(datos) > 0L) {
      bytes_por_fila_medido <- max(
        bytes_por_fila_medido,
        bytes_bloque_r / nrow(datos),
        as.numeric(bytes_chunk) / nrow(datos), na.rm = TRUE
      )
    }
    if (isTRUE(escritor$truncada)) break
    completado <- tryCatch(DBI::dbHasCompleted(resultado), error = function(e) NA)
    if (isTRUE(completado)) break
  }
  if (!is.null(resultado)) {
    .limpiar_consulta_dbi(resultado, conexion)
    resultado <- NULL
  }
  if (!is.null(error_lectura)) {
    escritor$motivo <- paste0(
      "muestra_inestable:error_lectura_spool: ", error_lectura
    )
    escritor$eventos <- rbind(
      escritor$eventos,
      .evento_spool_dbi("spool_lectura_fallida", escritor$motivo,
                        bloque_id = escritor$bloque_id + 1L)
    )
  }
  medicion_final <- .terminar_consulta_dbi(medicion, NULL)
  medicion_final$n_filas_resultado <- as.numeric(n_filas)
  medicion_final$bytes_resultado_r <- bytes_traidos_r
  cerrado <- .cerrar_spool_muestra_dbi(escritor)
  if (!isTRUE(cerrado$ok)) {
    return(list(
      ok = FALSE, path = path, consulta = medicion_final,
      n_filas = NA_real_, bytes = NA_real_, checksum = NA_character_,
      fetches = fetches, bytes_bloques = cerrado$bytes_bloques,
      motivo = if (!is.null(escritor$motivo)) escritor$motivo else cerrado$motivo,
      eventos = cerrado$eventos, max_filas = max_filas,
      truncada = isTRUE(cerrado$truncada)
    ))
  }
  verificada <- .verificar_spool_muestra_dbi(path)
  if (!isTRUE(verificada$ok)) {
    return(list(
      ok = FALSE, path = path, consulta = medicion_final,
      n_filas = NA_real_, bytes = NA_real_, checksum = NA_character_,
      fetches = fetches, bytes_bloques = cerrado$bytes_bloques,
      motivo = verificada$motivo,
      eventos = rbind(cerrado$eventos, .evento_spool_dbi(
        "spool_verificacion_fallida", verificada$motivo
      )), max_filas = max_filas
    ))
  }
  materializacion_exitosa <- TRUE
  list(
    ok = TRUE, path = path, consulta = medicion_final,
    n_filas = verificada$n_filas, bytes = verificada$bytes,
    checksum = verificada$checksum, motivo = cerrado$motivo,
    fetches = fetches, bytes_bloques = cerrado$bytes_bloques,
    eventos = cerrado$eventos, header = verificada$header,
    trailer = verificada$trailer, max_filas = max_filas,
    truncada = isTRUE(cerrado$truncada),
    bytes_por_fila_max = bytes_por_fila_max,
    bytes_por_fila_medido = bytes_por_fila_medido
  )
}

.muestra_id_spool_dbi <- function(consulta_id = NULL) {
  # `consulta_id` identifica una sentencia, no la relacion que esta
  # materializa. Un token nuevo por spool evita que dos selecciones distintas
  # compartan rotulo cuando DBI reutiliza el contador o las corridas coinciden
  # dentro del mismo tick del reloj.
  paste0("muestra_motor-", basename(tempfile("lupa-muestra-")))
}

.plan_materializacion_spool_dbi <- function(conexion, preparacion,
                                            max_bytes_materializacion) {
  candidato <- preparacion$muestreo$candidato
  proyeccion_muestra <- .campos_sql_muestra_dbi(
    conexion, preparacion$campos, preparacion$campos_sql_proyeccion,
    preparacion$prototipo, preparacion$tipos, preparacion$dialecto,
    tipos_motor = preparacion$esquema$tipos_motor
  )
  fuente <- if (!is.null(candidato)) {
    captura <- .capturar_fuente_muestreada_dbi(
      preparacion$tabla_sql, proyeccion_muestra$campos_sql,
      preparacion$muestra_motor, preparacion$n_total,
      preparacion$dialecto, preparacion$muestreo
    )
    captura$fuente
  } else NULL
  motivo_fuente <- if (!is.null(candidato) && is.null(fuente)) {
    if (exists("captura", inherits = FALSE) &&
        length(captura$motivo) == 1L && !is.na(captura$motivo)) {
      captura$motivo
    } else "capacidad_no_aceptada:fuente_muestreada"
  } else NA_character_
  list(
    pagado = FALSE, seleccion_unica = 1L,
    backend = .BACKEND_SPOOL_MUESTRA_DBI,
    version = .VERSION_SPOOL_MUESTRA_DBI,
    muestra_id = NA_character_, snapshot_id = NA_character_,
    orden_id = "spool-ordinal-1", checksum_esperado = NA_character_,
    bytes_esperados = NA_real_, presupuesto = max_bytes_materializacion,
    estado = "planificado", externo = TRUE,
    objetos_temporales_motor = FALSE,
    sql_seleccion = if (is.null(fuente)) NA_character_ else fuente$sql,
    motivo_fuente = motivo_fuente,
    metodo = if (is.null(fuente)) NA_character_ else fuente$metodo,
    costo = list(sql = 1L, resultsets = 1L, fetches = NA_real_,
                 filas = preparacion$muestra_motor, bytes = NA_real_),
    pasadas = list(valor = "spool", indice = "spool", lsh = "spool",
                   materializacion = "una selecci\u00f3n materializada antes de la primera pasada")
  )
}

.cobertura_orden_inestable_spool_dbi <- function(perfil) {
  familias <- c("trazabilidad", "ejemplos", "muestra")
  cobertura <- perfil$cobertura_diagnosticos
  if (!is.data.frame(cobertura)) cobertura <- .cobertura_diagnosticos_vacia()
  nuevas <- lapply(familias, function(familia) {
    .nuevo_diagnostico_no_evaluado(
      familia, NA_character_,
      paste0(
        "El orden de la muestra del motor no fue demostrado como estable; la",
        " familia `", familia, "` no se publica como exhaustiva."
      ),
      "Declarar y verificar un orden \u00fanico, o consumir s\u00f3lo los diagn\u00f3sticos disponibles."
    )
  })
  perfil$cobertura_diagnosticos <- do.call(rbind, c(list(cobertura), nuevas))
  rownames(perfil$cobertura_diagnosticos) <- NULL
  # `perfilar()` puede haber encontrado faltantes y adjuntado a cada hallazgo
  # los indices de sus filas. Esos indices son ciertos para este archivo, pero
  # no son una identidad estable de la seleccion del motor: no se publican
  # como si fueran trazabilidad reproducible. Se conserva la columna para no
  # romper el contrato de forma de `hallazgos`, dejando sus valores ausentes y
  # la explicacion completa en `cobertura_diagnosticos`.
  #
  # Retirar los indices no es retirar la traza. Antes cada traza se reemplazaba
  # por `NULL`, que no es ninguno de los cuatro estados que la ayuda promete
  # -`disponible`, `truncada`, `no_aplica`, `no_disponible`- y que la ayuda de
  # `perfilar_dbi()` descarta con todas las letras ("no reemplaza la ausencia
  # con `NULL` silencioso"): quien leia `traza$estado` recibia `NULL`. Ahora es
  # una traza `no_disponible` con alcance `orden_muestra_no_estable`, sin
  # indices ni claves, que conserva el total y los conteos que no nombran filas.
  if (is.data.frame(perfil$hallazgos) &&
      "trazabilidad" %in% names(perfil$hallazgos)) {
    perfil$hallazgos$trazabilidad <- I(lapply(
      perfil$hallazgos$trazabilidad, .traza_retenida_orden_inestable
    ))
  }
  # Los NOMBRES de los patrones raros se retiran por la misma razon que los
  # indices -con ellos la traza se rearma-, pero su CANTIDAD no: se forzaba a
  # cero, y `descubrir_patrones()` documenta que ese atributo registra cuantos
  # patrones raros habia antes de su limite. Medido: un patron raro conocido
  # se publicaba como cero en el mismo perfil que emitia el hallazgo.
  if (is.list(perfil$patrones)) {
    for (i in seq_along(perfil$patrones)) {
      patrones <- perfil$patrones[[i]]
      if (is.data.frame(patrones) &&
          "patrones_raros_trazabilidad" %in%
            names(attributes(patrones))) {
        attr(patrones, "patrones_raros_trazabilidad") <- character()
        perfil$patrones[[i]] <- patrones
      }
    }
  }
  perfil
}

.ALCANCE_TRAZA_ORDEN_INESTABLE <- "orden_muestra_no_estable"

.traza_retenida_orden_inestable <- function(traza) {
  # Una traza `no_aplica` no nombra filas: no hay nada que retener, y su estado
  # es el que corresponde a la unidad del hallazgo.
  if (is.list(traza) && identical(traza$estado, "no_aplica")) return(traza)
  limite <- if (is.list(traza) && length(traza$limite) == 1L &&
                !is.na(traza$limite)) traza$limite else 1000L
  total <- if (is.list(traza) && length(traza$total) == 1L) {
    suppressWarnings(as.numeric(traza$total))
  } else NA_real_
  retenida <- .trazabilidad_vacia(
    estado = "no_disponible", total = total,
    alcance = .ALCANCE_TRAZA_ORDEN_INESTABLE, limite = limite
  )
  if (is.list(traza)) {
    conteos <- c(
      "n_patrones_raros", "limite_patrones_raros_trazabilidad",
      "n_filas_formas_variantes", "n_filas_formas_dominantes"
    )
    for (campo in intersect(conteos, names(traza))) {
      retenida[[campo]] <- traza[[campo]]
    }
  }
  retenida
}

.resumen_muestra_desde_spool_dbi <- function(datos, campos, n_total,
                                             metricas, materializacion,
                                             muestreo, sql, consulta_id,
                                             motivo_falla = NULL,
                                             consulta_n = NULL) {
  n_total_num <- .conteo_dbi(n_total)
  n_conocido <- length(n_total_num) == 1L && is.finite(n_total_num)
  motivo_n <- if (n_conocido) {
    NA_character_
  } else if (!is.null(consulta_n) && length(consulta_n$motivo) == 1L &&
             !is.na(consulta_n$motivo) && nzchar(as.character(consulta_n$motivo))) {
    as.character(consulta_n$motivo)
  } else if (length(motivo_falla) == 1L && !is.na(motivo_falla) &&
             nzchar(as.character(motivo_falla))) {
    as.character(motivo_falla)
  } else {
    "No se pudo conocer `n` dentro del presupuesto de consultas."
  }
  estado_n <- if (n_conocido) "calculado" else "no_disponible"
  filas <- lapply(campos, function(campo) .fila_resumen_dbi(campo, n_total_num))
  columnas <- as.data.frame(do.call(rbind, lapply(filas, as.data.frame)),
                            stringsAsFactors = FALSE)
  if (is.null(datos)) datos <- data.frame()
  perfil_columnas <- NULL
  if (is.data.frame(attr(datos, "lupa_columnas_perfil", exact = TRUE))) {
    perfil_columnas <- attr(datos, "lupa_columnas_perfil", exact = TRUE)
  }
  if (!is.null(perfil_columnas)) {
    comunes <- intersect(names(columnas), names(perfil_columnas))
    for (nombre in comunes) {
      if (!identical(nombre, "n")) columnas[[nombre]] <- perfil_columnas[[nombre]]
    }
    columnas$n <- .conteo_dbi(n_total_num)
    rownames(columnas) <- NULL
  }
  estado <- if (is.null(motivo_falla)) "observado_muestra" else "no_disponible"
  motivo <- if (is.null(motivo_falla)) {
    paste(
      "Medido sobre la \u00fanica muestra materializada en el spool de sesi\u00f3n",
      "cliente; todas las pasadas de esta salida leen esa relaci\u00f3n."
    )
  } else as.character(motivo_falla)
  metadatos <- .metadatos_sql_dbi(
    alcance = "muestra", universo = n_total_num,
    tamano_muestra = if (is.null(muestreo)) NA_real_ else muestreo$filas_obtenidas,
    fraccion = if (is.null(muestreo)) NA_real_ else muestreo$fraccion,
    metodo = "spool_sesion_cliente", error_esperado = NA_character_,
    id_consulta = consulta_id
  )
  metadatos$muestreo_regla <- if (is.null(muestreo)) {
    list(disponible = FALSE)
  } else muestreo
  metadatos$fraccion_regla <- if (is.null(muestreo)) NA_real_ else
    muestreo$fraccion
  registros <- lapply(seq_along(campos), function(i) {
    campo <- campos[[i]]
    campos_metrica <- unlist(.CAMPOS_METRICA_DBI[metricas], use.names = FALSE)
    if (!length(campos_metrica)) return(NULL)
    motivo_metrica <- if (is.null(motivo_falla)) {
      vapply(campos_metrica, function(nombre) {
        .clasificar_metrica_muestreo_dbi(
          nombre, metadatos$muestreo_regla, metadatos$fraccion_regla
        )$motivo
      }, character(1L))
    } else {
      rep_len(motivo, length(campos_metrica))
    }
    .registro_sql_dbi(
      campo, campos_metrica, estado, motivo_metrica, sql,
      metadatos = metadatos,
      medicion = list(
        consulta_id = consulta_id, etapa = "lectura_spool_muestra",
        duracion_ms = NA_real_, n_filas_resultado = if (is.null(muestreo)) {
          NA_real_
        } else muestreo$filas_obtenidas,
        bytes_resultado_r = materializacion$bytes, cpu_ms = NA_real_
      )
    )
  })
  sql_n <- .registro_sql_dbi(
    campos, rep("n", length(campos)), estado_n, motivo_n,
    if (is.null(muestreo)) NA_character_ else muestreo$sql_muestra,
    metadatos = .metadatos_sql_dbi(
      alcance = "tabla_completa", universo = n_total_num,
      tamano_muestra = if (is.null(muestreo)) NA_real_ else muestreo$filas_obtenidas,
      fraccion = if (is.null(muestreo)) NA_real_ else muestreo$fraccion,
      metodo = "conteo_universo", error_esperado = "no_aplica",
      id_consulta = if (is.null(muestreo)) NA_integer_ else muestreo$id_consulta_n
    ),
    medicion = list(
      consulta_id = if (is.null(muestreo)) NA_integer_ else muestreo$id_consulta_n,
      etapa = "conteo_filas"
    )
  )
  sql <- if (length(Filter(Negate(is.null), registros))) {
    do.call(rbind, c(list(sql_n), Filter(Negate(is.null), registros)))
  } else sql_n
  rownames(sql) <- NULL
  list(columnas = columnas, sql = .marcar_nivel_sql_dbi(sql),
       cobertura = .cobertura_dbi_vacia())
}

.perfil_muestra_spool_dbi <- function(conexion, tabla, preparacion,
                                      incluir_valores, bloque_filas,
                                      max_bytes_procesamiento,
                                      max_bytes_materializacion, argumentos,
                                      trazador = NULL,
                                      max_celdas_muestra = .MAX_CELDAS_MUESTRA,
                                      max_bytes_muestra = .MAX_BYTES_MUESTRA) {
  presupuesto <- preparacion$presupuesto
  campos <- as.character(preparacion$campos)
  max_bytes_lectura <- min(
    if (is.finite(max_bytes_materializacion)) max_bytes_materializacion else Inf,
    if (is.finite(max_bytes_muestra)) max_bytes_muestra else Inf
  )
  estimacion_bytes <- .estimar_bytes_fila_muestra_dbi(
    conexion, tabla, presupuesto, campos = preparacion$campos,
    tipos_motor = preparacion$esquema$tipos_motor
  )
  proyeccion_muestra <- .campos_sql_muestra_dbi(
    conexion, preparacion$campos, preparacion$campos_sql_proyeccion,
    preparacion$prototipo, preparacion$tipos, preparacion$dialecto,
    tipos_motor = preparacion$esquema$tipos_motor,
    version_sqlserver = estimacion_bytes$version_sqlserver,
    max_largo_valor_vocabulario = argumentos[[
      "max_largo_valor_vocabulario"
    ]] %||% .MAX_LARGO_VALOR_CASI_DUPLICADOS
  )
  estimacion_bytes$bytes_por_fila_max <- proyeccion_muestra$bytes_por_fila_max
  estimacion_bytes$columnas_texto_largo <- proyeccion_muestra$columnas_texto_largo
  estimacion_bytes$max_largo_valor_vocabulario <-
    proyeccion_muestra$max_largo_valor_vocabulario
  columnas_castadas <- proyeccion_muestra$columnas_castadas
  info_conexion <- .info_conexion_dbi(conexion)
  backend <- .BACKEND_SPOOL_MUESTRA_DBI
  consulta_n <- if (!is.null(preparacion$conteo)) {
    preparacion$conteo
  } else NULL
  n_total <- .numero_dbi(preparacion$n_total)
  if (length(n_total) != 1L || is.na(n_total) || !is.finite(n_total)) {
    consulta_n <- .escalar_dbi(
      conexion, preparacion$sql_conteo, "lupa_n_total", presupuesto,
      etapa = "conteo_filas"
    )
    n_total <- if (isTRUE(consulta_n$ok)) .conteo_dbi(consulta_n$valor) else NA_real_
  }
  alcance <- .tope_muestra_dbi(
    preparacion$muestra_motor, n_total, length(campos),
    max_celdas_muestra, max_bytes_muestra
  )
  # La cota de tipos no se convierte en una cantidad final de filas: con
  # `text`, `bytea` y otros tipos sin largo declarado puede ser varios ordenes
  # mayor que la fila real. La lectura se inicia con una fila y el materializador
  # mide cada fila que conserva.
  filas_por_catalogo <- Inf
  filas_por_bytes_cota <- Inf
  if (is.finite(max_bytes_materializacion) || is.finite(max_bytes_muestra)) {
    presupuesto_filas_bytes <- min(
      if (is.finite(max_bytes_materializacion)) max_bytes_materializacion else Inf,
      if (is.finite(max_bytes_muestra)) max_bytes_muestra else Inf
    )
    fila_maxima <- estimacion_bytes$bytes_por_fila_max
    filas_por_bytes_cota <- if (is.finite(fila_maxima) && fila_maxima > 0) {
      floor(presupuesto_filas_bytes / fila_maxima)
    } else Inf
  }
  filas_efectivas <- min(alcance$filas_efectivas, filas_por_catalogo)
  alcance$filas_por_bytes <- NA_real_
  alcance$filas_efectivas <- filas_efectivas
  alcance$celdas_efectivas <- filas_efectivas * length(campos)
  medir_bytes <- is.finite(max_bytes_lectura) &&
    max_bytes_lectura < .MAX_BYTES_MUESTRA && (
    !is.finite(filas_por_bytes_cota) || filas_por_bytes_cota <= 100
  )
  filas_pedidas_motor <- filas_efectivas
  id_consulta_n <- if (!is.null(consulta_n)) {
    suppressWarnings(as.integer(consulta_n$consulta_id))
  } else NA_integer_
  muestra_id <- .muestra_id_spool_dbi(id_consulta_n)
  snapshot_id <- paste0("spool-snapshot-", muestra_id)
  orden_id <- paste0("spool-ordinal-", .VERSION_SPOOL_MUESTRA_DBI)
  publico <- NULL
  fuente <- NULL
  motivo <- NULL
  if (is.null(preparacion$muestreo) ||
      !isTRUE(preparacion$muestreo$disponible)) {
    motivo <- if (is.null(preparacion$muestreo)) {
      "capacidad_no_aceptada:sonda_muestreo"
    } else preparacion$muestreo$motivo
    if (length(motivo) != 1L || is.na(motivo) || !nzchar(motivo)) {
      motivo <- "capacidad_no_aceptada:sonda_muestreo"
    }
  } else {
    captura_fuente <- .capturar_fuente_muestreada_dbi(
      preparacion$tabla_sql, proyeccion_muestra$campos_sql,
      filas_pedidas_motor, n_total, preparacion$dialecto,
      preparacion$muestreo
    )
    fuente <- captura_fuente$fuente
    if (is.null(fuente)) {
      motivo <- captura_fuente$motivo
      if (length(motivo) != 1L || is.na(motivo) || !nzchar(motivo)) {
        motivo <- "capacidad_no_aceptada:fuente_muestreada"
      }
    } else {
      publico <- .publicar_muestreo_dbi(
        preparacion$muestreo, fuente, n_total
      )
      publico$id_consulta_n <- id_consulta_n
      publico$muestra_id <- muestra_id
      publico$snapshot_id <- snapshot_id
      publico$orden_id <- orden_id
      publico$reproducible <- FALSE
      publico$motivo_reproducibilidad <- paste(
        "El orden interno del result set no fue demostrado como \u00fanico y",
        "reproducible entre corridas; el ordinal del spool s\u00f3lo identifica esta",
        "materializacion."
      )
    }
  }
  sql_muestra <- if (is.null(fuente)) NA_character_ else fuente$sql
  bloque_filas <- bloque_filas %||% max(1, min(10000, filas_pedidas_motor))
  if (is.finite(max_bytes_lectura)) {
    # Con `medir_bytes`, el tamano de cada lectura lo decide la medicion dentro
    # del bucle del materializador: la primera fila va sola.
    if (!isTRUE(medir_bytes) &&
        is.finite(estimacion_bytes$bytes_por_fila_max) &&
               estimacion_bytes$bytes_por_fila_max > 0) {
      propuesta <- floor(
        max_bytes_lectura / estimacion_bytes$bytes_por_fila_max
      )
      bloque_filas <- min(bloque_filas, max(1L, as.integer(propuesta)))
    } else {
      bloque_filas <- min(bloque_filas, 100L)
    }
  }
  materializacion <- .materializacion_spool_vacia_dbi(
    backend, max_bytes_lectura, muestra_id, snapshot_id, orden_id,
    motivo = motivo
  )
  spool <- NULL
  perfil_aislado <- NULL
  cobertura_aislada <- .cobertura_dbi_vacia()
  if (is.null(motivo)) {
    inicio_materializacion <- if (!is.null(trazador) &&
                                  isTRUE(trazador$activo)) .ahora_dbi() else NULL
    cpu_materializacion <- if (!is.null(trazador) &&
                               isTRUE(trazador$activo)) .ahora_cpu_dbi() else NULL
    spool <- .materializar_muestra_spool_dbi(
      conexion, sql_muestra, campos, muestra_id, snapshot_id, orden_id,
      presupuesto, bloque_filas = bloque_filas,
      max_bytes = max_bytes_lectura, backend = backend,
      max_filas = filas_efectivas,
      bytes_por_fila_max = estimacion_bytes$bytes_por_fila_max,
      medir_bytes = medir_bytes
    )
    .registrar_etapa_dbi(
      trazador, "materializacion_muestra", inicio_materializacion,
      .ahora_dbi(), cpu_inicio = cpu_materializacion,
      cpu_fin = .ahora_cpu_dbi(), estado = if (isTRUE(spool$ok)) NULL else
        "no_disponible"
    )
    if (!isTRUE(spool$ok)) {
      motivo <- spool$motivo
      presupuesto$fallas_consultas[[length(presupuesto$fallas_consultas) + 1L]] <-
        list(
          etapa = "materializacion_muestra",
          consulta_id = if (is.null(spool$consulta)) NA_integer_ else
            as.integer(spool$consulta$consulta_id),
          motivo = as.character(spool$motivo), sqlstate = NA_character_,
          tipo = .clasificar_falla_consulta_dbi(NA_character_, spool$motivo)
        )
    }
    materializacion <- .materializacion_spool_vacia_dbi(
      backend, max_bytes_lectura, muestra_id, snapshot_id, orden_id,
      estado = if (!isTRUE(spool$ok) ||
                   (isTRUE(spool$truncada) &&
                    identical(as.numeric(spool$n_filas), 0))) "no_disponible" else
        "cerrada_verificada",
      motivo = if (isTRUE(spool$truncada)) spool$motivo else motivo
    )
    materializacion$estimacion_bytes <- estimacion_bytes
    materializacion$filas_solicitadas <- preparacion$muestra_motor
    materializacion$filas_efectivas <- filas_efectivas
    materializacion$filas_por_celdas <- alcance$filas_por_celdas
    materializacion$filas_por_bytes <- alcance$filas_por_bytes
    materializacion$max_celdas_muestra <- max_celdas_muestra
    materializacion$max_bytes_muestra <- max_bytes_muestra
    materializacion$checksum <- spool$checksum
    materializacion$bytes <- spool$bytes
    materializacion$n_filas <- spool$n_filas
    materializacion$validado_relectura <- isTRUE(spool$ok) && !(
      isTRUE(spool$truncada) && identical(as.numeric(spool$n_filas), 0)
    )
    materializacion$recortada <- isTRUE(spool$truncada)
    materializacion$bytes_medidos <- spool$bytes
    if (!isTRUE(spool$ok) && grepl(
      "error_lectura_spool", as.character(spool$motivo), fixed = TRUE,
      useBytes = TRUE
    )) {
      aislado <- tryCatch(
        .bloque_muestra_dbi(
          conexion, tabla, preparacion$tabla_sql, preparacion$campos,
          preparacion$campos_sql_proyeccion, preparacion$muestra,
          preparacion$muestra_motor, preparacion$orden_muestra,
          preparacion$orden_sql, preparacion$dialecto, n_total, presupuesto,
          info_conexion, argumentos, muestreo = preparacion$muestreo,
          prototipo = preparacion$prototipo, tipos_declarados = preparacion$tipos,
          tipos_motor = preparacion$esquema$tipos_motor,
          max_celdas_muestra = Inf, max_bytes_muestra = Inf,
          enmascaradas = preparacion$enmascaradas
        ),
        error = function(e) {
          .repropagar_transaccion_dbi(e)
          NULL
        }
      )
      if (!is.null(aislado)) {
        perfil_aislado <- aislado$perfil
        cobertura_aislada <- aislado$cobertura
      }
    }
  }
  reintentos_muestreo <- 0L
  metodo_inicial <- if (is.null(publico)) NA_character_ else publico$metodo
  metodo_reintento <- NA_character_
  motivo_reintento <- NA_character_
  guardia_reintento <- NULL
  if (is.null(motivo) && !is.null(spool) && isTRUE(spool$ok) &&
      !isTRUE(spool$truncada) &&
      is.finite(filas_efectivas) && filas_efectivas > 0 &&
      !is.null(publico) && grepl("^tablesample_", publico$metodo) &&
      isTRUE(spool$n_filas < filas_efectivas / 2)) {
    candidatos <- .candidatos_muestreo_dbi(
      conexion, preparacion$dialecto,
      tipo_objeto = preparacion$tipo_objeto
    )
    objetivo_reintento <- min(
      preparacion$muestra_motor * 2, if (is.finite(n_total)) n_total else
        preparacion$muestra_motor * 2
    )
    pendientes_mediana <- if (
      "mediana" %in% preparacion$metricas_ejecucion &&
      isTRUE(incluir_valores)
    ) sum(preparacion$es_numerico) else 1L
    decision_reintento <- .alternativa_muestreo_reintento_dbi(
      candidatos, metodo_inicial, .senas_conexion_dbi(conexion), n_total,
      objetivo_reintento, pendientes_mediana
    )
    alternativa <- decision_reintento$candidato
    guardia_reintento <- decision_reintento$guardia
    motivo_reintento <- decision_reintento$motivo
    if (!is.null(alternativa)) {
      metodo_reintento <- alternativa$nombre
      resolucion_reintento <- preparacion$muestreo
      resolucion_reintento$candidato <- alternativa
      captura_reintento <- .capturar_fuente_muestreada_dbi(
        preparacion$tabla_sql, proyeccion_muestra$campos_sql,
        objetivo_reintento, n_total, preparacion$dialecto,
        resolucion_reintento
      )
      fuente_reintento <- captura_reintento$fuente
      if (is.null(fuente_reintento) &&
          length(captura_reintento$motivo) == 1L &&
          !is.na(captura_reintento$motivo)) {
        motivo_reintento <- captura_reintento$motivo
      }
      if (!is.null(fuente_reintento)) {
        reintentos_muestreo <- 1L
        publico_reintento <- .publicar_muestreo_dbi(
          resolucion_reintento, fuente_reintento, n_total
        )
        publico_reintento$id_consulta_n <- id_consulta_n
        publico_reintento$muestra_id <- muestra_id
        publico_reintento$snapshot_id <- snapshot_id
        publico_reintento$orden_id <- orden_id
        inicio_materializacion <- if (!is.null(trazador) &&
                                      isTRUE(trazador$activo)) .ahora_dbi() else NULL
        cpu_materializacion <- if (!is.null(trazador) &&
                                   isTRUE(trazador$activo)) .ahora_cpu_dbi() else NULL
        spool_reintento <- .materializar_muestra_spool_dbi(
          conexion, fuente_reintento$sql, campos, muestra_id, snapshot_id,
          orden_id,
          presupuesto, bloque_filas = bloque_filas,
          max_bytes = max_bytes_lectura, backend = backend,
          max_filas = filas_efectivas,
          bytes_por_fila_max = estimacion_bytes$bytes_por_fila_max,
          medir_bytes = medir_bytes
        )
        .registrar_etapa_dbi(
          trazador, "materializacion_muestra", inicio_materializacion,
          .ahora_dbi(), cpu_inicio = cpu_materializacion,
          cpu_fin = .ahora_cpu_dbi(), estado = if (isTRUE(spool_reintento$ok)) NULL else
            "no_disponible"
        )
        mejor <- .mejor_spool_muestra_dbi(spool, spool_reintento)
        uso_reintento <- identical(mejor, spool_reintento)
        if (uso_reintento) {
          try(unlink(spool$path, force = TRUE), silent = TRUE)
          spool <- spool_reintento
          publico <- publico_reintento
          sql_muestra <- fuente_reintento$sql
          materializacion <- .materializacion_spool_vacia_dbi(
            backend, max_bytes_lectura, muestra_id, snapshot_id, orden_id,
            estado = "cerrada_verificada",
            motivo = if (isTRUE(spool$truncada)) spool$motivo else motivo
          )
          materializacion$estimacion_bytes <- estimacion_bytes
          materializacion$filas_solicitadas <- preparacion$muestra_motor
          materializacion$filas_efectivas <- filas_efectivas
          materializacion$filas_por_celdas <- alcance$filas_por_celdas
          materializacion$filas_por_bytes <- alcance$filas_por_bytes
          materializacion$max_celdas_muestra <- max_celdas_muestra
          materializacion$max_bytes_muestra <- max_bytes_muestra
          materializacion$checksum <- spool$checksum
          materializacion$bytes <- spool$bytes
          materializacion$n_filas <- spool$n_filas
          materializacion$validado_relectura <- TRUE
          materializacion$recortada <- isTRUE(spool$truncada)
          materializacion$bytes_medidos <- spool$bytes
        } else {
          try(unlink(spool_reintento$path, force = TRUE), silent = TRUE)
          motivo_nuevo <- paste(
            "La primera muestra se conserv\u00f3 porque el reintent\u00f3 no trajo m\u00e1s filas."
          )
          motivo_reintento <- if (is.na(motivo_reintento)) motivo_nuevo else
            paste(motivo_reintento, motivo_nuevo)
        }
        if (!isTRUE(spool_reintento$ok)) {
          motivo_nuevo <- paste(
            "El reintent\u00f3 fall\u00f3 y la primera muestra v\u00e1lida se conserv\u00f3."
          )
          motivo_reintento <- if (is.na(motivo_reintento)) motivo_nuevo else
            paste(motivo_reintento, motivo_nuevo)
        }
      }
    }
  }
  if (is.null(publico)) {
    publico <- list(
      disponible = FALSE, metodo = NA_character_, metodo_muestreo = NA_character_,
      funcion_muestreo = NA_character_, descripcion = NA_character_,
      fraccion = NA_real_, tamano_muestra = filas_efectivas,
      filas_solicitadas = preparacion$muestra_motor,
      filas_pedidas = filas_efectivas, filas_obtenidas = NA_real_,
      universo = n_total, muestra_id = muestra_id, snapshot_id = snapshot_id,
      orden_id = orden_id, reproducible = FALSE, id_consulta_n = id_consulta_n,
      motivo_filas_obtenidas = NA_character_
    )
  }
  materializacion$estimacion_bytes <- estimacion_bytes
  materializacion$filas_solicitadas <- preparacion$muestra_motor
  materializacion$filas_efectivas <- filas_efectivas
  materializacion$filas_por_celdas <- alcance$filas_por_celdas
  materializacion$filas_por_bytes <- alcance$filas_por_bytes
  materializacion$max_celdas_muestra <- max_celdas_muestra
  materializacion$max_bytes_muestra <- max_bytes_muestra
  materializacion$max_bytes_materializacion <- max_bytes_materializacion
  materializacion$presupuesto_efectivo <- max_bytes_lectura
  publico$filas_solicitadas_sin_topes <- preparacion$muestra_motor
  publico$filas_solicitadas <- preparacion$muestra_motor
  publico$filas_pedidas <- filas_efectivas
  publico$max_celdas_muestra <- max_celdas_muestra
  publico$max_bytes_muestra <- max_bytes_muestra
  publico$filas_por_celdas <- alcance$filas_por_celdas
  publico$filas_por_bytes <- alcance$filas_por_bytes
  publico$bytes_por_fila_estimados <- estimacion_bytes$bytes_por_fila
  publico$estimacion_bytes <- estimacion_bytes
  # Una materializacion truncada y verificada puede contener cero filas. Se
  # conserva ese cero aunque la lectura de chunks se marque inutil para el
  # perfil: `NA` significaria que no se pudo saber cuantas filas llegaron.
  # Solo el cero: si la lectura falla con filas materializadas, al perfil no
  # llego ninguna y publicar su numero seria afirmar una muestra que no hay.
  spool_n_filas_verificado <- if (!is.null(spool) && isTRUE(spool$ok) &&
                                  length(spool$n_filas) == 1L &&
                                  identical(as.numeric(spool$n_filas), 0)) {
    0
  } else NA_real_
  if (!is.null(spool) && isTRUE(spool$ok)) {
    inicio_lectura_spool <- if (!is.null(trazador) &&
                                isTRUE(trazador$activo)) .ahora_dbi() else NULL
    cpu_lectura_spool <- if (!is.null(trazador) &&
                             isTRUE(trazador$activo)) .ahora_cpu_dbi() else NULL
    leido <- .leer_spool_muestra_dbi(spool$path, devolver_chunks = TRUE)
    .registrar_etapa_dbi(
      trazador, "lectura_spool", inicio_lectura_spool, .ahora_dbi(),
      cpu_inicio = cpu_lectura_spool, cpu_fin = .ahora_cpu_dbi(),
      estado = if (isTRUE(leido$ok)) NULL else "no_disponible"
    )
    if (!isTRUE(leido$ok)) {
      motivo <- leido$motivo
      spool$ok <- FALSE
    }
    spool_inutil <- !is.null(spool) && isTRUE(spool$truncada) &&
      identical(as.numeric(spool$n_filas), 0)
    if (spool_inutil) {
      motivo <- spool$motivo
      if (is.null(motivo) || is.na(motivo)) {
        motivo <- "muestra_inestable:presupuesto_materializacion"
      }
      leido$ok <- FALSE
    }
  } else {
    leido <- list(ok = FALSE, motivo = motivo)
  }
  spool_valido <- isTRUE(leido$ok)
  n_filas_spool <- if (spool_valido) as.numeric(leido$n_filas) else
    spool_n_filas_verificado
  n_chunks_spool <- if (spool_valido) as.integer(leido$n_chunks) else 0L
  fetches_spool <- if (!is.null(spool)) as.integer(spool$fetches %||% 0L) else 0L
  filas_por_bytes_medidas <- if (is.finite(max_bytes_lectura) &&
                                 spool_valido) n_filas_spool else NA_real_
  fuente_filas_por_bytes <- if (!is.finite(max_bytes_lectura)) {
    "no_aplica"
  } else if (spool_valido) {
    "medicion_materializacion"
  } else {
    "no_disponible"
  }
  bytes_por_fila_medido <- if (!is.null(spool) && spool_valido &&
                               n_filas_spool > 0) {
    as.numeric(spool$bytes_por_fila_medido %||% NA_real_)
  } else NA_real_
  alcance$filas_por_bytes <- filas_por_bytes_medidas
  publico$reintentos <- reintentos_muestreo
  publico$metodo_inicial <- metodo_inicial
  publico$metodo_reintento <- metodo_reintento
  publico$motivo_reintento <- motivo_reintento
  publico$politica_aleatoria_reintento <- guardia_reintento
  publico$filas_obtenidas <- if (is.finite(n_filas_spool)) {
    n_filas_spool
  } else NA_real_
  publico$motivo_filas_obtenidas <- if (is.finite(n_filas_spool)) {
    NA_character_
  } else if (length(motivo) == 1L && !is.na(motivo) && nzchar(as.character(motivo))) {
    as.character(motivo)
  } else {
    "No se pudo conocer cu\u00e1ntas filas devolvi\u00f3 la muestra."
  }
  if (spool_valido) {
    publico$fraccion_solicitada <- .fraccion_solicitada_muestreo_dbi(
      preparacion$muestra_motor, n_total
    )
    publico$fraccion <- .fraccion_muestreo_dbi(n_filas_spool, n_total)
  }
  if (grepl("^tablesample_", as.character(publico$metodo)) ||
      reintentos_muestreo > 0L) {
    publico$coincide_con_lo_pedido <- spool_valido &&
      identical(as.numeric(n_filas_spool), as.numeric(filas_efectivas))
  }
  filas_base <- min(
    as.numeric(preparacion$muestra_motor),
    if (is.finite(n_total)) n_total else preparacion$muestra_motor
  )
  recorte_celdas <- is.finite(alcance$filas_por_celdas) &&
    alcance$filas_por_celdas < filas_base
  recorte_bytes_prelectura <- FALSE
  recorte_bytes <- isTRUE(spool$truncada)
  recorte_procesamiento <- FALSE
  recorte_topes <- recorte_celdas || recorte_bytes
  materializacion$fraccion_materializada <- if (
    spool_valido && is.finite(n_total)
  ) .fraccion_muestreo_dbi(n_filas_spool, n_total) else NA_real_
  materializacion$tope_que_mando <- if (recorte_bytes) "bytes" else if (
    recorte_celdas
  ) "celdas" else NA_character_
  materializacion$bytes_por_fila_max <- estimacion_bytes$bytes_por_fila_max
  materializacion$filas_por_bytes_cota <- filas_por_bytes_cota
  materializacion$filas_por_bytes_fuente <- fuente_filas_por_bytes
  materializacion$filas_por_bytes <- filas_por_bytes_medidas
  materializacion$bytes_por_fila_medido <- bytes_por_fila_medido
  materializacion$recortada <- recorte_bytes
  if (recorte_bytes && (is.null(materializacion$motivo) ||
                        is.na(materializacion$motivo))) {
    materializacion$motivo <- "muestra_inestable:presupuesto_materializacion"
  }
  muestreo_corto <- spool_valido && filas_efectivas > 0 &&
    n_filas_spool < filas_efectivas / 2
  bloques <- list(
    solicitados = if (spool_valido && n_filas_spool > 0) {
      as.integer(ceiling(n_filas_spool / bloque_filas))
    } else 0L,
    recorridos = n_chunks_spool, filas_vistas = if (spool_valido) {
      n_filas_spool
    } else 0,
    fetches = fetches_spool,
    consultas_sql = if (!is.null(spool) && !is.null(spool$consulta)) 1L else 0L,
    sin_solapamiento = if (spool_valido) TRUE else NA,
    primer_ordinal = if (spool_valido && n_filas_spool > 0) 1 else NA_real_,
    ultimo_ordinal = if (spool_valido && n_filas_spool > 0) n_filas_spool else
      NA_real_
  )
  pasadas <- list(
    primera = "spool", valor = "spool", indice = "spool", lsh = "spool",
    materializacion = "una selecci\u00f3n materializada antes de la primera pasada"
  )
  if (isTRUE(leido$ok)) {
    publico$filas_obtenidas <- leido$n_filas
    publico$motivo_filas_obtenidas <- NA_character_
    publico$tamano_muestra <- min(
      as.numeric(publico$tamano_muestra), as.numeric(leido$n_filas)
    )
    publico$bytes_spool <- leido$bytes
    publico$checksum <- leido$checksum
    publico$backend <- backend
    publico$version <- .VERSION_SPOOL_MUESTRA_DBI
    if (!is.finite(publico$fraccion) || is.na(publico$fraccion)) {
      publico$fraccion <- .fraccion_muestreo_dbi(leido$n_filas, n_total)
    }
  }
  publico$filas_por_bytes <- filas_por_bytes_medidas
  publico$filas_por_bytes_fuente <- fuente_filas_por_bytes
  publico$filas_por_bytes_cota <- filas_por_bytes_cota
  publico$bytes_por_fila_medido <- bytes_por_fila_medido
  datos_materializados <- if (isTRUE(leido$ok)) {
    if (length(leido$chunks)) do.call(rbind, leido$chunks) else {
      as.data.frame(stats::setNames(replicate(length(campos), logical(), simplify = FALSE),
                             campos), stringsAsFactors = FALSE)
    }
  } else NULL
  conversion_muestra <- .convertir_enteros_ancho_muestra_dbi(
    datos_materializados, columnas_castadas, preparacion$tipos
  )
  # La media, la mediana y el desvio de un decimal que llego como texto y
  # quedo en doble se publican del texto, no de los dobles.
  decimales_texto <- .media_mediana_decimales_muestra_dbi(
    datos_materializados, conversion_muestra$datos, columnas_castadas,
    preparacion$tipos
  )
  datos_completos <- .ajustar_sentinelas_real_dbi(
    conversion_muestra$datos, campos, preparacion$tipos,
    argumentos[["sentinelas_numericos"]],
    tipos_motor = preparacion$esquema$tipos_motor
  )
  datos <- datos_completos
  if (!is.null(datos) && is.finite(preparacion$muestra) &&
      nrow(datos) > preparacion$muestra) {
    # El recorte diagnostico es una lectura local del mismo spool; no vuelve a
    # emitir la seleccion ni elige otra muestra.
    n_diagnosticos <- as.integer(preparacion$muestra)
    datos <- if (n_diagnosticos > 0L) datos[seq_len(n_diagnosticos), , drop = FALSE] else
      datos[0L, , drop = FALSE]
  }
  perfil <- perfil_aislado
  # Las metricas del resumen se afirman solo sobre la materializacion que
  # `muestra_motor` pidio. Hay dos puertas por las que no la hay, y las dos
  # cierran igual -`no_disponible`, sin tamano ni fraccion de la muestra-:
  # el spool no se pudo leer y lo que queda es el perfil aislado, otra lectura
  # sin la columna rota (aca); o el spool se leyo y el perfil de todas sus
  # filas fallo (mas abajo). Lo que cambia es que puede mostrar
  # `resumen_tabla$columnas`. El aislado es una muestra entera de las columnas
  # sanas, y una columna rota no les saca sus cuentas: se publican las de las
  # filas que se perfilaron, con `n` igual a esas filas (ver donde se arma
  # `columnas`). En la otra puerta lo unico perfilado es un recorte de un
  # spool mas grande, y `n` queda sin dato.
  resumen_sin_materializacion <- !is.null(perfil_aislado)
  perfil_completo_fallido <- FALSE
  perfil_resumen <- perfil
  motivo_metricas <- if (resumen_sin_materializacion) {
    .MOTIVO_RESUMEN_SIN_MATERIALIZACION_DBI
  } else if (is.null(motivo)) {
    paste(
      "Medido sobre la \u00fanica selecci\u00f3n materializada en el spool de sesi\u00f3n",
      "cliente; las pasadas de esta salida leen el mismo `muestra_id`."
    )
  } else as.character(motivo)
  estado_metricas <- if (resumen_sin_materializacion || is.null(perfil)) {
    "no_disponible"
  } else "observado_muestra"
  tamano_muestra_metricas <- if (resumen_sin_materializacion) {
    NA_real_
  } else publico$filas_obtenidas
  fraccion_muestra_metricas <- if (!is.null(perfil_aislado)) {
    NA_real_
  } else publico$fraccion
  cobertura <- cobertura_aislada
  if (reintentos_muestreo > 0L || muestreo_corto) {
    estado_muestreo <- if (muestreo_corto) "degradado" else "medido"
    motivo_muestreo <- if (muestreo_corto) {
      paste0(
        "La muestra final contiene ", .entero_sql_dbi(n_filas_spool),
        " de ", .entero_sql_dbi(filas_efectivas),
        if (filas_efectivas == 1L) " fila pedida." else " filas pedidas."
      )
    } else {
      paste0(
        "La muestra inicial fue corta y el reintent\u00f3 con `",
        as.character(publico$metodo), "` alcanz\u00f3 el tama\u00f1o pedido."
      )
    }
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "resumen_tabla", .texto_tabla_dbi(tabla), estado_muestreo,
      motivo_muestreo,
      "Revisar `meta$muestreo` para los m\u00e9todos y filas de cada intento.",
      publico$sql
    ))
  }
  if (!is.null(perfil_aislado)) {
    cobertura <- rbind(
      cobertura,
      .registro_cobertura_dbi(
        "resumen_tabla", paste(campos, collapse = ", "), "no_disponible",
        motivo_metricas,
        "Corregir la vista o repetir la materializaci\u00f3n completa antes de usar el resumen."
      )
    )
  }
  # Lo leido entero, tambien las columnas enmascaradas que no se perfilan: su
  # completitud sale de aca (ver abajo).
  datos_todos <- datos_completos
  if (isTRUE(leido$ok) && (nrow(datos) > 0L ||
                          (is.finite(n_total) && n_total == 0))) {
    if (is.finite(max_bytes_procesamiento) &&
        as.numeric(utils::object.size(datos_completos)) > max_bytes_procesamiento) {
      motivo <- "muestra_inestable:presupuesto_procesamiento"
      recorte_procesamiento <- TRUE
      leido$ok <- FALSE
    } else {
      args <- argumentos
      if (is.null(args$nombre)) {
        args$nombre <- paste0("muestra DBI de ", .texto_tabla_dbi(tabla))
      }
      args$muestra <- Inf
      args$max_celdas_muestra <- Inf
      args$max_bytes_muestra <- Inf
      # Las columnas enmascaradas (DDM) no se perfilan en la muestra: sus
      # valores son la mascara. Ver `.bloque_muestra_dbi()`.
      sacar <- .nombres_presentes(names(datos), preparacion$enmascaradas)
      for (columna in names(datos)[sacar]) {
        cobertura <- rbind(cobertura, .registro_cobertura_dbi(
          "perfil_muestra", columna, "alcance_distinto",
          .motivo_columna_enmascarada_dbi(), .remedio_columna_enmascarada_dbi()
        ))
      }
      if (any(sacar)) datos <- .seleccionar_columnas(datos, which(!sacar))
      perfil <- tryCatch(
        .medir_etapa_dbi(
          trazador, "perfilado_muestra",
          do.call(perfilar, c(list(datos = datos), args))
        ),
        error = function(e) e
      )
      if (inherits(perfil, "condition")) {
        motivo <- paste0(
          "muestra_inestable:perfilado_spool: ",
          .motivo_perfilado_muestra_dbi(perfil, datos, args)
        )
        perfil <- NULL
      }
      if (!is.null(perfil)) {
        perfil <- .ajustar_longitudes_binarias_dbi(
          perfil, datos, proyeccion_muestra$columnas_binarias,
          proyeccion_muestra$columnas_binarias_prefijo
        )
      }
      if (!is.null(datos_completos) && nrow(datos_completos) != nrow(datos)) {
        perfil_resumen <- tryCatch(
          do.call(perfilar, c(list(datos = datos_completos), args)),
          error = function(e) NULL
        )
        perfil_completo_fallido <- is.null(perfil_resumen)
        resumen_sin_materializacion <- perfil_completo_fallido
      } else {
        perfil_resumen <- perfil
      }
      if (!is.null(perfil)) {
        perfil <- .ajustar_textos_largos_dbi(
          perfil, datos,
          proyeccion_muestra$columnas_texto_largo,
          proyeccion_muestra$max_largo_valor_vocabulario
        )
      }
      if (!is.null(perfil_resumen) &&
          !identical(perfil_resumen, perfil) &&
          is.data.frame(datos_completos)) {
        perfil_resumen <- .ajustar_textos_largos_dbi(
          perfil_resumen, datos_completos,
          proyeccion_muestra$columnas_texto_largo,
          proyeccion_muestra$max_largo_valor_vocabulario
        )
      }
      # `perfilar()` decidio los rellenos de las columnas protegidas sobre la
      # muestra del motor, y con `universo = "muestra_motor"` no hay conteo de
      # la tabla entera que los confirme: vuelven al piso, como los no
      # confirmados de la via SQL (`.rellenos_confirmados_tabla_dbi()`). Sin
      # esto la cedula valida de un cliente frecuente -600 de 20.600
      # movimientos- era "relleno" en la muestra y salia en la moda, los
      # ejemplos y la evidencia de `obs`. Medido.
      proteger_spool <- .protege_argumentos(args)
      if (!is.null(perfil) && proteger_spool) {
        # Los valores protegidos de la muestra, para el mensaje del motor de las
        # demas tablas de una coleccion: ver `.valores_motor()`.
        tryCatch({
          protegidas <- .columnas_personales_protegidas(perfil$datos_personales)
          .sumar_valores_motor(
            .valores_publicables_protegidos(datos, protegidas),
            .fechas_que_identifican(datos, protegidas)
          )
        }, error = function(e) NULL)
        candidatos <- tryCatch(.rellenos_de_columnas(
          datos, .columnas_personales_protegidas(perfil$datos_personales)
        ), error = function(e) character())
        if (length(candidatos)) {
          perfil <- .proteger_con_valores_dbi(
            perfil, candidatos, as.character(perfil$columnas$columna)
          )
          if (!is.null(perfil_resumen) && !identical(perfil_resumen, perfil)) {
            # El resumen de `muestra_motor` usa la materializacion completa,
            # pero los rellenos se deciden sobre la muestra diagnostica. La
            # proteccion que esa muestra justifico tambien debe alcanzar el
            # perfil completo, o sus ejemplos vuelven a publicar el valor.
            perfil_resumen <- .proteger_con_valores_dbi(
              perfil_resumen, candidatos,
              as.character(perfil_resumen$columnas$columna)
            )
          }
        }
      }
    }
  } else if (isTRUE(leido$ok) && !(is.finite(n_total) && n_total == 0)) {
    metodo_vacio <- if (is.null(publico$metodo) ||
                        is.na(publico$metodo) || !nzchar(publico$metodo)) {
      "muestreo_sin_metodo"
    } else as.character(publico$metodo)
    motivo <- paste0("muestra_vacia:", metodo_vacio, "_sin_filas")
  }
  # Una tabla sin filas se perfila igual -el perfil de la muestra lleva las
  # columnas y sus tipos-, pero la muestra no tiene filas, y el resumen dice lo
  # que dice una muestra vacia de una tabla con filas y la via de solo
  # agregados: `muestra_vacia:<metodo>`, sin ceros ni estados observados. Antes
  # publicaba `n_validos`, `n_faltantes` y `n_distintos` en cero como
  # `observado_muestra`, y lo demas con `muestra_inestable:sin_valores_validos`.
  motivo_muestra_sin_filas <- NULL
  if (isTRUE(leido$ok) && is.null(motivo) && !is.null(perfil) &&
      NROW(datos_completos) == 0L) {
    metodo_vacio <- if (is.null(publico$metodo) ||
                        is.na(publico$metodo) || !nzchar(publico$metodo)) {
      "muestreo_sin_metodo"
    } else as.character(publico$metodo)
    motivo_muestra_sin_filas <- paste0("muestra_vacia:", metodo_vacio, "_sin_filas")
  }
  recorte_topes <- recorte_celdas || recorte_bytes || recorte_procesamiento
  tope_que_mando <- if (recorte_procesamiento) {
    "procesamiento"
  } else if (recorte_bytes) {
    "bytes"
  } else if (recorte_celdas) {
    "celdas"
  } else NA_character_
  materializacion$tope_que_mando <- tope_que_mando
  materializacion$recortada <- recorte_bytes
  if (recorte_procesamiento) {
    materializacion$motivo <- "muestra_inestable:presupuesto_procesamiento"
  }
  if (recorte_bytes) {
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "resumen_tabla", .texto_tabla_dbi(tabla), "alcance_distinto",
      paste(
        "El resumen describe las filas que entraron al spool; la",
        "materializaci\u00f3n qued\u00f3 recortada por el presupuesto de bytes."
      ),
      "Revisar `meta$materializacion` para el total pedido y el tope aplicado."
    ))
  }
  # Un reintento que recupera la muestra no avisa: lo que se pidio llego y la
  # escasez transitoria queda en `meta$muestreo` (`reintentos`,
  # `metodo_inicial`), como en la via de solo agregados. Si hay otro motivo de
  # aviso, el mensaje tambien nombra el reintento.
  if (recorte_topes || muestreo_corto) {
    motivos_aviso <- character()
    if (recorte_bytes) motivos_aviso <- c(
      motivos_aviso,
      paste0(
        "la materializaci\u00f3n se acot\u00f3 a ",
        .entero_sql_dbi(n_filas_spool), if (n_filas_spool == 1L) " fila por el presupuesto de bytes" else " filas por el presupuesto de bytes"
      )
    )
    if (recorte_celdas) motivos_aviso <- c(
      motivos_aviso,
      paste0(
        "la muestra se acot\u00f3 a ", .entero_sql_dbi(filas_efectivas),
        if (filas_efectivas == 1L) " fila por el presupuesto de celdas" else " filas por el presupuesto de celdas"
      )
    )
    if (recorte_procesamiento) motivos_aviso <- c(
      motivos_aviso,
      "el perfil local se descart\u00f3 porque super\u00f3 el presupuesto de procesamiento"
    )
    if (muestreo_corto) motivos_aviso <- c(
      motivos_aviso,
      paste0(
        "el motor devolvi\u00f3 ", .entero_sql_dbi(n_filas_spool),
        " de ", .entero_sql_dbi(filas_efectivas),
        if (filas_efectivas == 1L) " fila pedida" else " filas pedidas"
      )
    )
    if (reintentos_muestreo > 0L && !muestreo_corto) motivos_aviso <- c(
      motivos_aviso,
      paste0(
        "la primera muestra fue corta y se reintent\u00f3 con `",
        as.character(publico$metodo), "`"
      )
    )
    .avisar_muestra_motor_dbi(
      paste0("La muestra_motor tuvo alcance degradado: ",
             paste(motivos_aviso, collapse = "; "), "."),
      datos = list(
        tope_que_mando = tope_que_mando,
        recorte_bytes = recorte_bytes,
        recorte_celdas = recorte_celdas,
        recorte_procesamiento = recorte_procesamiento,
        muestreo_corto = muestreo_corto
      )
    )
  }
  # El perfil se puede construir despues de leer el spool. Recalcular el
  # estado aqui evita publicar `no_disponible` solo porque al entrar todavia no
  # existia el perfil local; si la materializacion completa fallo, en cambio,
  # la marca queda cerrada aunque el perfil aislado haya llegado.
  motivo_metricas <- if (isTRUE(resumen_sin_materializacion)) {
    .MOTIVO_RESUMEN_SIN_MATERIALIZACION_DBI
  } else if (is.null(motivo)) {
    paste(
      "Medido sobre la \u00fanica selecci\u00f3n materializada en el spool de sesi\u00f3n",
      "cliente; las pasadas de esta salida leen el mismo `muestra_id`."
    )
  } else as.character(motivo)
  estado_metricas <- if (isTRUE(resumen_sin_materializacion)) {
    "no_disponible"
  } else if (is.null(perfil)) {
    "no_disponible"
  } else "observado_muestra"
  tamano_muestra_metricas <- if (isTRUE(resumen_sin_materializacion)) {
    NA_real_
  } else publico$filas_obtenidas
  fraccion_muestra_metricas <- if (!is.null(perfil_aislado)) {
    NA_real_
  } else publico$fraccion
  if (!is.null(perfil)) {
    if (nrow(conversion_muestra$cobertura)) {
      perfil$cobertura_diagnosticos <- rbind(
        perfil$cobertura_diagnosticos, conversion_muestra$cobertura
      )
      rownames(perfil$cobertura_diagnosticos) <- NULL
    }
    perfil$meta$filas_analizadas <- as.numeric(nrow(datos))
    perfil$meta$filas_totales_fuente <- .numero_dbi(n_total)
    perfil$meta$alcance <- list(
      universo_id = "muestra_motor",
      fuente_id = paste0("dbi:", .texto_tabla_dbi(tabla)),
      orden = orden_id, snapshot_id = snapshot_id, muestra_id = muestra_id
    )
    perfil$meta$bloques <- bloques
    perfil$meta$pasadas <- pasadas
    perfil$meta$materializacion <- materializacion
    perfil$meta$muestra <- preparacion$muestra_motor
    perfil$meta$muestra_efectiva <- filas_efectivas
    perfil$meta$celdas_solicitadas <- alcance$celdas_solicitadas
    perfil$meta$celdas_efectivas <- alcance$celdas_efectivas
    perfil$meta$max_celdas_muestra <- max_celdas_muestra
    perfil$meta$max_bytes_muestra <- max_bytes_muestra
    perfil$meta$bytes_sonda <- NA_real_
    perfil$meta$bytes_muestra <- if (spool_valido) spool$bytes else NA_real_
    perfil$meta$tope_que_mando <- tope_que_mando
    perfil$meta$tope_que_mando_texto <- if (recorte_topes) {
      if (recorte_procesamiento) {
        "El perfil local se descart\u00f3 al superar el presupuesto de procesamiento."
      } else if (recorte_bytes) {
        paste0(
          "La materializaci\u00f3n se acot\u00f3 a ",
          .entero_sql_dbi(n_filas_spool),
          if (n_filas_spool == 1L) " fila por el presupuesto de bytes." else
            " filas por el presupuesto de bytes."
        )
      } else {
        paste0(
          "La muestra del motor qued\u00f3 acotada a ",
          .entero_sql_dbi(filas_efectivas),
          if (filas_efectivas == 1L) " fila antes de leerla." else
            " filas antes de leerla."
        )
      }
    } else NA_character_
    perfil$meta$origen_dbi <- list(
      tipo = "DBI", conexion = info_conexion,
      tabla = .texto_tabla_dbi(tabla), muestreo = publico,
      materializacion = materializacion, solo_lectura = TRUE,
      objetos_temporales = FALSE
    )
    perfil <- .cobertura_orden_inestable_spool_dbi(perfil)
  }
  columnas <- lapply(campos, function(campo) .fila_resumen_dbi(campo, n_total))
  columnas <- if (length(columnas)) {
    as.data.frame(do.call(rbind, lapply(columnas, as.data.frame)),
                  stringsAsFactors = FALSE)
  } else .columnas_dbi_vacias()
  perfil_columnas <- if (!is.null(perfil_resumen) &&
                         is.data.frame(perfil_resumen$columnas)) {
    perfil_resumen$columnas
  } else if (!perfil_completo_fallido && !is.null(perfil) &&
             is.data.frame(perfil$columnas)) {
    perfil$columnas
  } else NULL
  if (!is.null(perfil_columnas) && is.data.frame(perfil_columnas)) {
    # Por nombre y no por posicion: una columna enmascarada no entra en el
    # perfil, y copiar por posicion reciclaba las de las demas
    # columnas sobre la tabla entera.
    posicion <- .indice_nombre(
      as.character(columnas$columna), as.character(perfil_columnas$columna)
    )
    comunes <- intersect(names(columnas), names(perfil_columnas))
    for (nombre in setdiff(comunes, c("n", "columna"))) {
      columnas[[nombre]] <- perfil_columnas[[nombre]][posicion]
    }
    if (all(c("n", "n_faltantes") %in% names(perfil_columnas))) {
      columnas$n_validos <- as.numeric(perfil_columnas$n)[posicion] -
        as.numeric(perfil_columnas$n_faltantes)[posicion]
    }
    # La completitud de la que no se perfilo, sobre las mismas filas: DDM
    # conserva el nulo, y es lo unico de la columna que describe el dato.
    for (k in which(is.na(posicion))) {
      j <- .indice_nombre(as.character(columnas$columna[[k]]), names(datos_todos))
      if (is.na(j) || !nrow(datos_todos)) next
      faltan <- sum(is.na(datos_todos[[j]]))
      columnas$n_faltantes[[k]] <- faltan
      columnas$n_validos[[k]] <- nrow(datos_todos) - faltan
      columnas$prop_faltantes[[k]] <- faltan / nrow(datos_todos)
    }
    # `n` es el total de la tabla, como en la via de solo agregados y como
    # dice la ayuda: las demas cuentas son de la muestra y se leen contra el.
    # Eso supone que lo perfilado es la muestra que se pidio. Cuando no lo es
    # -el spool quedo recortado por el presupuesto de bytes, o no se pudo leer
    # y lo que hay es el perfil aislado-, el total de la tabla al lado de los
    # validos de unas pocas filas se leia como una columna casi vacia: ahi `n`
    # es la cantidad de filas perfiladas.
    n_perfil <- suppressWarnings(as.numeric(perfil_columnas$n))
    n_perfil <- n_perfil[is.finite(n_perfil)]
    if (!length(n_perfil) && is.data.frame(datos)) {
      n_perfil <- as.numeric(nrow(datos))
    }
    perfilado_parcial <- isTRUE(recorte_bytes) || !is.null(perfil_aislado)
    columnas$n <- if (perfilado_parcial && length(n_perfil)) {
      n_perfil[[1L]]
    } else .conteo_dbi(n_total)
  }
  if (isTRUE(perfil_completo_fallido)) {
    # El perfil corto sigue disponible como diagnostico, pero no midio el
    # tamano de la materializacion completa que el resumen debia describir.
    columnas$n <- NA_real_
  }
  if (!is.null(motivo_muestra_sin_filas)) {
    for (nombre in setdiff(names(columnas), c("columna", "n"))) {
      columnas[[nombre]][] <- NA
    }
  }
  metricas <- preparacion$metricas_ejecucion
  # Si no hubo muestra, `metricas_ejecucion` puede quedar vacio porque la
  # capacidad de muestreo se rechazo durante la preparacion. Igual hay que
  # publicar cada metrica solicitada como no calculada: quitar las filas haria
  # parecer que nunca se pidieron. Cuando hay perfil, se conserva la lista de
  # ejecucion para no publicar estrategias que el plan retiro.
  metricas_registro <- if (is.null(perfil)) {
    preparacion$metricas
  } else metricas
  n_conocido <- length(n_total) == 1L && is.finite(n_total)
  motivo_n <- if (n_conocido) {
    NA_character_
  } else if (!is.null(consulta_n) && length(consulta_n$motivo) == 1L &&
             !is.na(consulta_n$motivo) && nzchar(as.character(consulta_n$motivo))) {
    as.character(consulta_n$motivo)
  } else if (length(motivo) == 1L && !is.na(motivo) && nzchar(as.character(motivo))) {
    as.character(motivo)
  } else {
    "No se pudo conocer `n` dentro del presupuesto de consultas."
  }
  estado_n <- if (n_conocido) "calculado" else "no_disponible"
  metadatos_muestra <- .metadatos_sql_dbi(
    alcance = "muestra", universo = n_total,
    tamano_muestra = tamano_muestra_metricas,
    fraccion = fraccion_muestra_metricas, metodo = "spool_sesion_cliente",
    error_esperado = NA_character_, id_consulta = if (is.null(spool)) {
      NA_integer_
    } else spool$consulta$consulta_id
  )
  metadatos_muestra$muestreo_regla <- if (is.null(spool)) {
    list(disponible = FALSE)
  } else if (is.null(preparacion$muestreo)) {
    list(disponible = FALSE)
  } else preparacion$muestreo
  metadatos_muestra$fraccion_regla <- fraccion_muestra_metricas
  campos_metricas_publicas <- unlist(
    .CAMPOS_METRICA_DBI[metricas_registro], use.names = FALSE
  )
  motivos_metricas_publicas <- if (is.null(perfil_aislado) &&
                                   !is.null(perfil)) {
    vapply(campos_metricas_publicas, function(nombre) {
      .clasificar_metrica_muestreo_dbi(
        nombre, metadatos_muestra$muestreo_regla,
        metadatos_muestra$fraccion_regla
      )$motivo
    }, character(1L))
  } else {
    rep_len(motivo_metricas, length(campos_metricas_publicas))
  }
  if (!is.null(motivo_muestra_sin_filas)) {
    estado_metricas <- "no_disponible"
    motivos_metricas_publicas <- rep_len(
      motivo_muestra_sin_filas, length(campos_metricas_publicas)
    )
  }
  registros <- list(.registro_sql_dbi(
    campos, rep("n", length(campos)), estado_n, motivo_n,
    preparacion$sql_conteo,
    metadatos = .metadatos_sql_dbi(
      alcance = "tabla_completa", universo = n_total,
      tamano_muestra = tamano_muestra_metricas,
      fraccion = fraccion_muestra_metricas, metodo = "conteo_universo",
      error_esperado = "no_aplica", id_consulta = id_consulta_n
    ),
    medicion = if (is.null(consulta_n)) NULL else consulta_n,
    etapa = "conteo_filas"
  ))
  for (i in seq_along(campos)) {
    campos_metrica <- unlist(
      .CAMPOS_METRICA_DBI[metricas_registro], use.names = FALSE
    )
    if (!length(campos_metrica)) next
    registros[[length(registros) + 1L]] <- .registro_sql_dbi(
      campos[[i]], campos_metrica,
      estado_metricas,
      motivos_metricas_publicas, sql_muestra,
      metadatos = metadatos_muestra,
      medicion = if (is.null(spool)) NULL else spool$consulta,
      etapa = "lectura_spool_muestra"
    )
  }
  # Las columnas que el esquema no pudo leer -sin permiso segun el catalogo,
  # rechazadas por el motor, o con un nombre que la sesion no puede escribir- se
  # declaran como en la via SQL y en la de bloques: esta via las dejaba fuera de
  # `columnas`, de `sql` y de la cobertura, y la tabla salia con menos columnas
  # sin decirlo. No hay completitud que publicar:
  # no se leyeron.
  ilegibles <- preparacion$esquema$ilegibles
  if (length(ilegibles)) {
    columnas <- rbind(columnas, do.call(rbind, lapply(ilegibles, function(campo) {
      as.data.frame(.fila_resumen_dbi(campo, n_total), stringsAsFactors = FALSE)
    })))
    rownames(columnas) <- NULL
    campos_metrica <- unlist(.CAMPOS_METRICA_DBI[metricas_registro], use.names = FALSE)
    for (campo in ilegibles) {
      if (!length(campos_metrica)) next
      registros[[length(registros) + 1L]] <- .registro_sql_dbi(
        campo, campos_metrica, "no_disponible",
        preparacion$esquema$motivos[[campo]], NA_character_,
        metadatos = .metadatos_sql_dbi(),
        medicion = list(consulta_id = NA_integer_, etapa = "esquema")
      )
    }
  }
  sql <- if (length(registros)) do.call(rbind, registros) else {
    .registro_sql_dbi(character(), character(), character(), character(), character())
  }
  # La lectura exacta de un DECIMAL ancho puede dejar el perfil local en
  # `omitidos_precision`: hay extremos y conteos, pero no una media, mediana o
  # desvio representables como doble. Si esas filas quedan con el motivo
  # generico de ausencia, se pierde la causa que ya conoce el perfil.
  if (is.data.frame(sql) && nrow(sql) && is.data.frame(perfil$columnas) &&
      "estado_resumen_cuantitativo" %in% names(perfil$columnas)) {
    for (campo in campos) {
      posicion_campo <- .indice_nombre(
        as.character(campo), as.character(perfil$columnas$columna)
      )
      tipo <- .atributo_columna_dbi(
        preparacion$tipos, campo, match(campo, campos)
      ) %||% NA_character_
      if (is.na(posicion_campo) ||
          !identical(as.character(perfil$columnas$estado_resumen_cuantitativo[[
            posicion_campo
          ]]), "omitidos_precision") ||
          !.tipo_declarado_decimal_dbi(tipo)) next
      tocar <- as.character(sql$columna) == campo &
        as.character(sql$metrica) %in% c(
          "minimo", "media", "mediana", "desvio"
        ) & (is.na(sql$motivo) |
               startsWith(as.character(sql$motivo), "La consulta termin"))
      sql$motivo[tocar] <- .MOTIVO_DECIMAL_ANCHO_SPOOL_DBI
    }
  }
  sql <- .marcar_nivel_sql_dbi(sql)
  if (!is.null(motivo)) {
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "perfil_muestra", .texto_tabla_dbi(tabla), "no_disponible", motivo,
      paste(
        "No se publica una muestra parcial: corregir el presupuesto, la",
        "capacidad o el error declarado y repetir la materializaci\u00f3n."
      ), sql_muestra
    ))
  }
  meta <- list(
    universo = "muestra_motor", alcance = list(
      universo_id = "muestra_motor",
      fuente_id = paste0("dbi:", .texto_tabla_dbi(tabla)),
      orden = orden_id, snapshot_id = snapshot_id, muestra_id = muestra_id
    ), alcance_texto = "tabla_muestreada",
    tabla = .texto_tabla_dbi(tabla), filas = n_total,
    filas_obtenidas_muestra = publico$filas_obtenidas,
    motor = info_conexion, sql_conteo_filas = preparacion$sql_conteo,
    muestreo = publico, origen_dbi = list(
      tipo = "DBI", tabla = .texto_tabla_dbi(tabla), muestreo = publico,
      materializacion = materializacion, solo_lectura = TRUE,
      objetos_temporales = FALSE
    ),
    materializacion = materializacion,
    solo_lectura = TRUE, objetos_temporales = FALSE, snapshot = FALSE,
    nota_snapshot = paste(
      "`snapshot_id` identifica la fotograf\u00eda del spool cliente y no demuestra",
      "una instant\u00e1nea transaccional del motor. S\u00f3lo se publica evidencia",
      "positiva de snapshot cuando el adaptador la demuestra."
    ),
    clave = preparacion$catalogo_cardinalidad,
    estrategia_mediana = .publicar_estrategia_mediana_dbi(preparacion),
    metricas = preparacion$metricas, metricas_ejecucion = metricas,
    politica_costo = preparacion$politica_costo,
    estrategia_distintos = .publicar_estrategia_distintos_dbi(
      preparacion$estrategia_distintos
    ),
    estimacion_derrame = preparacion$estimacion_derrame,
    estimacion_derrame_moda = preparacion$estimacion_derrame_moda,
    estimacion_derrame_mediana = preparacion$estimacion_derrame_mediana,
    max_bytes_procesamiento = max_bytes_procesamiento,
    max_bytes_materializacion = max_bytes_materializacion,
    bloques = bloques, pasadas = pasadas,
    consultas = list(
      emitidas = presupuesto$usadas, presupuesto = presupuesto$max,
      agotado = isTRUE(presupuesto$agotado),
      fallas = .publicar_fallas_consultas_dbi(presupuesto),
      sondas_fallidas = .publicar_fallas_sondas_dbi(presupuesto)
    ),
    plan = NULL,
    proteccion_personal = list(
      aplicada = FALSE, base = "se resuelve en el perfil de muestra",
      columnas = character()
    )
  )
  # La medicion de la muestra y todas sus pasadas quedan con un solo alcance.
  # El RSS del proceso no se adivina desde R: queda NA salvo que el juez de
  # banco lo mida desde fuera.
  meta$bytes <- list(
    max_bloque = if (is.null(spool) || !length(spool$bytes_bloques)) {
      if (spool_valido) 0 else NA_real_
    } else max(spool$bytes_bloques),
    entrada = if (is.null(spool)) NA_real_ else spool$bytes,
    texto = NA_real_, retenidos = NA_real_, rss_maximo = NA_real_
  )
  meta$bytes_muestra <- if (is.null(spool)) NA_real_ else spool$bytes
  meta$max_celdas_muestra <- max_celdas_muestra
  meta$max_bytes_muestra <- max_bytes_muestra
  meta$tope_que_mando <- tope_que_mando
  meta$muestra_solicitada <- preparacion$muestra_motor
  meta$muestra_efectiva <- filas_efectivas
  meta$tiempos <- .resumen_tiempos_dbi(trazador)
  meta$vigilante <- list(eventos = if (is.null(spool)) {
    .eventos_spool_vacios_dbi()
  } else spool$eventos)
  if (!is.null(perfil)) {
    perfil$meta$bytes <- meta$bytes
    perfil$meta$vigilante <- meta$vigilante
    perfil$meta$tiempos <- meta$tiempos
  }
  if (!is.null(perfil)) {
    args_proteger <- .protege_argumentos(argumentos)
    if (args_proteger) {
      columnas_personales <- .columnas_personales_protegidas(
        perfil$datos_personales
      )
      if (length(columnas_personales)) {
        columnas[.nombres_para_operar(columnas$columna) %in%
                   .nombres_para_operar(columnas_personales), "moda"] <- NA_character_
      }
      meta$proteccion_personal <- list(
        aplicada = TRUE, base = "perfil_muestra",
        columnas = columnas_personales
      )
    } else {
      meta$proteccion_personal <- list(
        aplicada = FALSE, base = "desactivada por el usuario",
        columnas = character()
      )
    }
  }
  if (length(ilegibles)) {
    cobertura <- rbind(cobertura, .cobertura_ilegibles_dbi(preparacion$esquema))
  }
  fallas_consultas <- .publicar_fallas_consultas_dbi(presupuesto)
  if (is.data.frame(fallas_consultas) && nrow(fallas_consultas)) {
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "resumen_tabla", .texto_tabla_dbi(tabla), "degradado",
      paste0(
        "Fallaron ", nrow(fallas_consultas),
        " consultas solicitadas; revisar",
        " `resumen_tabla$meta$consultas$fallas`."
      ),
      "Revisar el motivo del motor, reducir el alcance o repetir con otra muestra.",
      NA_character_
    ))
  }
  resumen <- list(
    columnas = columnas, sql = sql, cobertura = cobertura,
    literales = character(), meta = meta, tiempos = meta$tiempos
  )
  if (is.data.frame(fallas_consultas) && nrow(fallas_consultas)) {
    .avisar_muestra_motor_dbi(
      paste0(
        "Una o m\u00e1s consultas solicitadas no estuvieron disponibles (",
        nrow(fallas_consultas), "). El detalle queda en",
        " `resumen_tabla$meta$consultas$fallas`."
      ),
      datos = list(fallas = fallas_consultas)
    )
  }
  columnas_decimal <- .columnas_decimal_doble_muestra_dbi(
    datos_completos, campos, preparacion$tipos
  )
  estructura <- list(
    resumen_tabla = resumen,
    perfil_muestra = perfil
  )
  estructura <- .publicar_media_mediana_decimal_muestra_dbi(
    estructura, decimales_texto
  )
  estructura <- .tapar_desvio_decimal_doble_muestra_dbi(
    estructura, datos_completos, campos, preparacion$tipos, conexion,
    desvio_exacto = names(Filter(function(x) {
      isTRUE(is.finite(x$desvio))
    }, decimales_texto))
  )
  estructura <- .aplicar_guardas_salida_muestra_dbi(
    estructura, incluir_valores = incluir_valores,
    columnas_decimal = columnas_decimal,
    motivos_decimal = if (length(columnas_decimal)) {
      stats::setNames(
        rep(.motivo_decimal_doble_dbi(), length(columnas_decimal)),
        columnas_decimal
      )
    } else NULL
  )
  if (!is.null(spool$path)) unlink(spool$path, force = TRUE)
  estructura <- .sellar_perfil_dbi(estructura)
  estructura
}

# La media, la mediana y el desvio de cada decimal que la muestra trajo como
# texto -para no perder sus cifras- y la conversion dejo en doble: las de
# `perfilar()` salen de esos dobles, y la media y la mediana de los dobles no
# son las del decimal (la de 0,0001 y 0,0002 daba 0,00015000000000000001), y su
# desvio se aparta del del decimal cuando la magnitud es mucho mayor que el
# desvio (un `numeric(18,2)` de 5e14 con centavos: una parte en 300). Salen del
# texto (`.media_mediana_decimal_texto_dbi()`). Un decimal entero que la
# conversion llevo a `integer64` o a `integer` ya es exacto y no entra.
.media_mediana_decimales_muestra_dbi <- function(textos, datos, columnas,
                                                tipos) {
  salida <- list()
  if (!is.data.frame(textos) || !is.data.frame(datos) || !length(columnas) ||
      nrow(textos) != nrow(datos)) {
    return(salida)
  }
  for (columna in intersect(columnas, names(datos))) {
    posicion <- match(columna, names(datos))
    tipo <- .atributo_columna_dbi(tipos, columna, posicion) %||% NA_character_
    x <- datos[[columna]]
    texto <- if (columna %in% names(textos)) textos[[columna]] else
      textos[[posicion]]
    if (!.tipo_decimal_o_moneda_dbi(tipo) || !is.double(x) ||
        inherits(x, "integer64") || !is.character(texto)) {
      next
    }
    salida[[columna]] <- .media_mediana_decimal_texto_dbi(
      texto, x
    )
  }
  salida
}

# Pone en el resumen del spool la media, la mediana y el desvio del texto de
# cada decimal (`.media_mediana_decimales_muestra_dbi()`), donde el resumen los
# publico: las guardas que vienen despues -la del decimal desde 2^53, la
# privacidad- siguen mandando, y la del desvio de los dobles ya no corre sobre
# el resumen de esas columnas (ver `.tapar_desvio_decimal_doble_muestra_dbi()`).
# `perfil_muestra` es `perfilar()` sobre lo leido y conserva los suyos.
.publicar_media_mediana_decimal_muestra_dbi <- function(resultado, exactas) {
  columnas <- resultado$resumen_tabla$columnas
  if (!length(exactas) || !is.data.frame(columnas)) return(resultado)
  for (columna in names(exactas)) {
    filas <- .nombres_presentes(columnas$columna, columna)
    metricas <- intersect(c("media", "mediana", "desvio"), names(columnas))
    for (metrica in metricas) {
      valor <- exactas[[columna]][[metrica]]
      publicado <- filas & is.finite(suppressWarnings(
        as.numeric(columnas[[metrica]])
      ))
      if (length(valor) == 1L && is.finite(valor) && any(publicado)) {
        columnas[[metrica]][publicado] <- valor
      }
    }
  }
  resultado$resumen_tabla$columnas <- columnas
  resultado
}
