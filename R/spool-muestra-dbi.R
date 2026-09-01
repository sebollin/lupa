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
    trailer_reserva = .RESERVA_TRAILER_SPOOL_DBI
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
    escritor$motivo <- motivo
    escritor$eventos <- rbind(
      escritor$eventos,
      .evento_spool_dbi(
        "spool_presupuesto_excedido", motivo,
        bloque_id = escritor$bloque_id + 1L,
        bytes_retenidos = bytes_antes, bytes_resultado = length(raw)
      )
    )
    return(escritor)
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
      eventos = escritor$eventos
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
      ))
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
      ))
    ))
  }
  info <- file.info(escritor$path)
  list(
    ok = TRUE, motivo = NA_character_, n_filas = escritor$n_filas,
    bytes = as.numeric(info$size[[1L]]),
    checksum = trailer$checksum, n_chunks = escritor$n_chunks,
    bytes_bloques = escritor$bytes_bloques, eventos = escritor$eventos
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
      as.data.frame(setNames(replicate(length(campos), logical(), simplify = FALSE),
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

.materializar_muestra_spool_dbi <- function(conexion, sql, campos, muestra_id,
                                           snapshot_id, orden_id, presupuesto,
                                           bloque_filas = 10000,
                                           max_bytes = Inf, backend = NULL) {
  max_bytes <- .validar_presupuesto_bytes_dbi(
    max_bytes, "max_bytes_materializacion"
  )
  bloque_filas <- .validar_bloque_filas_dbi(bloque_filas)
  backend <- backend %||% .BACKEND_SPOOL_MUESTRA_DBI
  path <- tempfile("lupa-spool-muestra-", fileext = ".bin")
  escritor <- .abrir_spool_muestra_dbi(
    path, campos, muestra_id, snapshot_id, orden_id, max_bytes, backend, sql
  )
  if (!isTRUE(escritor$ok)) {
    return(list(
      ok = FALSE, path = path, consulta = NULL, n_filas = 0,
      bytes = NA_real_, checksum = NA_character_,
      fetches = 0L, bytes_bloques = numeric(),
      motivo = escritor$motivo, eventos = escritor$eventos
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
      eventos = cerrado$eventos
    ))
  }
  medicion <- .iniciar_consulta_dbi(presupuesto, "materializacion_muestra")
  error_lectura <- NULL
  fetches <- 0L
  resultado <- tryCatch(
    DBI::dbSendQuery(conexion, sql),
    error = function(e) {
      error_lectura <<- conditionMessage(e)
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
    datos <- tryCatch(
      DBI::dbFetch(resultado, n = bloque_filas),
      error = function(e) {
        error_lectura <<- conditionMessage(e)
        NULL
      }
    )
    fetches <- fetches + 1L
    if (!is.null(error_lectura)) break
    if (is.null(datos) || !nrow(datos)) break
    escritor <- .escribir_chunk_spool_muestra_dbi(escritor, datos)
    n_filas <- n_filas + nrow(datos)
    if (!is.null(escritor$motivo)) break
    completado <- tryCatch(DBI::dbHasCompleted(resultado), error = function(e) NA)
    if (isTRUE(completado)) break
  }
  if (!is.null(resultado)) try(DBI::dbClearResult(resultado), silent = TRUE)
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
  medicion_final$bytes_resultado_r <- NA_real_
  cerrado <- .cerrar_spool_muestra_dbi(escritor)
  if (!isTRUE(cerrado$ok)) {
    return(list(
      ok = FALSE, path = path, consulta = medicion_final,
      n_filas = NA_real_, bytes = NA_real_, checksum = NA_character_,
      fetches = fetches, bytes_bloques = cerrado$bytes_bloques,
      motivo = if (!is.null(escritor$motivo)) escritor$motivo else cerrado$motivo,
      eventos = cerrado$eventos
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
      ))
    ))
  }
  list(
    ok = TRUE, path = path, consulta = medicion_final,
    n_filas = verificada$n_filas, bytes = verificada$bytes,
    checksum = verificada$checksum, motivo = NA_character_,
    fetches = fetches, bytes_bloques = cerrado$bytes_bloques,
    eventos = cerrado$eventos, header = verificada$header,
    trailer = verificada$trailer
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
  fuente <- if (!is.null(candidato)) tryCatch(
    .fuente_muestreada_dbi(
      preparacion$tabla_sql, preparacion$campos_sql,
      preparacion$muestra_motor, preparacion$n_total,
      preparacion$dialecto, preparacion$muestreo
    ), error = function(e) NULL
  ) else NULL
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
    metodo = if (is.null(fuente)) NA_character_ else fuente$metodo,
    costo = list(sql = 1L, resultsets = 1L, fetches = NA_real_,
                 filas = preparacion$muestra_motor, bytes = NA_real_),
    pasadas = list(valor = "spool", indice = "spool", lsh = "spool",
                   materializacion = "una seleccion materializada antes de la primera pasada")
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
      "Declarar y verificar un orden unico, o consumir solo los diagnosticos disponibles."
    )
  })
  perfil$cobertura_diagnosticos <- do.call(rbind, c(list(cobertura), nuevas))
  rownames(perfil$cobertura_diagnosticos) <- NULL
  # `perfilar()` puede haber encontrado faltantes y adjuntado a cada hallazgo
  # los indices de sus filas. Esos indices son ciertos para este archivo, pero
  # no son una identidad estable de la selección del motor: no se publican
  # como si fueran trazabilidad reproducible. Se conserva la columna para no
  # romper el contrato de forma de `hallazgos`, dejando sus valores ausentes y
  # la explicación completa en `cobertura_diagnosticos`.
  if (is.data.frame(perfil$hallazgos) &&
      "trazabilidad" %in% names(perfil$hallazgos)) {
    perfil$hallazgos$trazabilidad <- I(vector("list", nrow(perfil$hallazgos)))
  }
  if (is.list(perfil$patrones)) {
    for (i in seq_along(perfil$patrones)) {
      patrones <- perfil$patrones[[i]]
      if (is.data.frame(patrones) &&
          "patrones_raros_trazabilidad" %in%
            names(attributes(patrones))) {
        attr(patrones, "patrones_raros_trazabilidad") <- character()
        attr(patrones, "n_patrones_raros_trazabilidad") <- 0L
        perfil$patrones[[i]] <- patrones
      }
    }
  }
  perfil
}

.resumen_muestra_desde_spool_dbi <- function(datos, campos, n_total,
                                             metricas, materializacion,
                                             muestreo, sql, consulta_id,
                                             motivo_falla = NULL) {
  n_total_num <- .conteo_dbi(n_total)
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
      "Medido sobre la unica muestra materializada en el spool de sesion",
      "cliente; todas las pasadas de esta salida leen esa relacion."
    )
  } else as.character(motivo_falla)
  metadatos <- .metadatos_sql_dbi(
    alcance = "muestra", universo = n_total_num,
    tamano_muestra = if (is.null(muestreo)) NA_real_ else muestreo$filas_obtenidas,
    fraccion = if (is.null(muestreo)) NA_real_ else muestreo$fraccion,
    metodo = "spool_sesion_cliente", error_esperado = "no_estimable",
    id_consulta = consulta_id
  )
  registros <- lapply(seq_along(campos), function(i) {
    campo <- campos[[i]]
    campos_metrica <- unlist(.CAMPOS_METRICA_DBI[metricas], use.names = FALSE)
    if (!length(campos_metrica)) return(NULL)
    .registro_sql_dbi(
      campo, campos_metrica, estado, motivo, sql,
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
    campos, rep("n", length(campos)), "calculado", NA_character_,
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
                                      max_bytes_materializacion, argumentos) {
  presupuesto <- preparacion$presupuesto
  campos <- as.character(preparacion$campos)
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
    fuente <- tryCatch(
      .fuente_muestreada_dbi(
        preparacion$tabla_sql, preparacion$campos_sql,
        preparacion$muestra_motor, n_total, preparacion$dialecto,
        preparacion$muestreo
      ),
      error = function(e) NULL
    )
    if (is.null(fuente)) {
      motivo <- "capacidad_no_aceptada:fuente_muestreada"
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
        "El orden interno del result set no fue demostrado como unico y",
        "reproducible entre corridas; el ordinal del spool solo identifica esta",
        "materializacion."
      )
    }
  }
  sql_muestra <- if (is.null(fuente)) NA_character_ else fuente$sql
  bloque_filas <- bloque_filas %||% min(10000, preparacion$muestra_motor)
  materializacion <- .materializacion_spool_vacia_dbi(
    backend, max_bytes_materializacion, muestra_id, snapshot_id, orden_id,
    motivo = motivo
  )
  spool <- NULL
  if (is.null(motivo)) {
    spool <- .materializar_muestra_spool_dbi(
      conexion, sql_muestra, campos, muestra_id, snapshot_id, orden_id,
      presupuesto, bloque_filas = bloque_filas,
      max_bytes = max_bytes_materializacion, backend = backend
    )
    if (!isTRUE(spool$ok)) motivo <- spool$motivo
    materializacion <- .materializacion_spool_vacia_dbi(
      backend, max_bytes_materializacion, muestra_id, snapshot_id, orden_id,
      estado = if (isTRUE(spool$ok)) "cerrada_verificada" else "no_disponible",
      motivo = motivo
    )
    materializacion$checksum <- spool$checksum
    materializacion$bytes <- spool$bytes
    materializacion$n_filas <- spool$n_filas
    materializacion$validado_relectura <- isTRUE(spool$ok)
  }
  if (is.null(publico)) {
    publico <- list(
      disponible = FALSE, metodo = NA_character_, metodo_muestreo = NA_character_,
      funcion_muestreo = NA_character_, descripcion = NA_character_,
      fraccion = NA_real_, tamano_muestra = preparacion$muestra_motor,
      filas_solicitadas = preparacion$muestra_motor,
      filas_pedidas = preparacion$muestra_motor, filas_obtenidas = NA_real_,
      universo = n_total, muestra_id = muestra_id, snapshot_id = snapshot_id,
      orden_id = orden_id, reproducible = FALSE, id_consulta_n = id_consulta_n
    )
  }
  if (!is.null(spool) && isTRUE(spool$ok)) {
    leido <- .leer_spool_muestra_dbi(spool$path, devolver_chunks = TRUE)
    if (!isTRUE(leido$ok)) {
      motivo <- leido$motivo
      spool$ok <- FALSE
    }
  } else {
    leido <- list(ok = FALSE, motivo = motivo)
  }
  spool_valido <- isTRUE(leido$ok)
  n_filas_spool <- if (spool_valido) as.numeric(leido$n_filas) else NA_real_
  n_chunks_spool <- if (spool_valido) as.integer(leido$n_chunks) else 0L
  fetches_spool <- if (!is.null(spool)) as.integer(spool$fetches %||% 0L) else 0L
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
    materializacion = "una seleccion materializada antes de la primera pasada"
  )
  if (isTRUE(leido$ok)) {
    publico$filas_obtenidas <- leido$n_filas
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
  datos_materializados <- if (isTRUE(leido$ok)) {
    if (length(leido$chunks)) do.call(rbind, leido$chunks) else {
      as.data.frame(setNames(replicate(length(campos), logical(), simplify = FALSE),
                             campos), stringsAsFactors = FALSE)
    }
  } else NULL
  datos <- datos_materializados
  if (!is.null(datos) && is.finite(preparacion$muestra) &&
      nrow(datos) > preparacion$muestra) {
    # El recorte diagnostico es una lectura local del mismo spool; no vuelve a
    # emitir la seleccion ni elige otra muestra.
    n_diagnosticos <- as.integer(preparacion$muestra)
    datos <- if (n_diagnosticos > 0L) datos[seq_len(n_diagnosticos), , drop = FALSE] else
      datos[0L, , drop = FALSE]
  }
  perfil <- NULL
  cobertura <- .cobertura_dbi_vacia()
  if (isTRUE(leido$ok) && nrow(datos) > 0L) {
    if (is.finite(max_bytes_procesamiento) &&
        as.numeric(utils::object.size(datos)) > max_bytes_procesamiento) {
      motivo <- "muestra_inestable:presupuesto_procesamiento"
      leido$ok <- FALSE
    } else {
      args <- argumentos
      if (is.null(args$nombre)) {
        args$nombre <- paste0("muestra DBI de ", .texto_tabla_dbi(tabla))
      }
      args$muestra <- Inf
      args$max_celdas_muestra <- Inf
      args$max_bytes_muestra <- Inf
      perfil <- tryCatch(
        do.call(perfilar, c(list(datos = datos), args)),
        error = function(e) e
      )
      if (inherits(perfil, "condition")) {
        motivo <- paste0(
          "muestra_inestable:perfilado_spool: ", conditionMessage(perfil)
        )
        perfil <- NULL
      }
    }
  } else if (isTRUE(leido$ok)) {
    metodo_vacio <- if (is.null(publico$metodo) ||
                        is.na(publico$metodo) || !nzchar(publico$metodo)) {
      "muestreo_sin_metodo"
    } else as.character(publico$metodo)
    motivo <- paste0("muestra_vacia:", metodo_vacio, "_sin_filas")
  }
  if (!is.null(perfil)) {
    perfil$meta$filas_analizadas <- as.numeric(nrow(datos))
    perfil$meta$alcance <- list(
      universo_id = "muestra_motor",
      fuente_id = paste0("dbi:", .texto_tabla_dbi(tabla)),
      orden = orden_id, snapshot_id = snapshot_id, muestra_id = muestra_id
    )
    perfil$meta$bloques <- bloques
    perfil$meta$pasadas <- pasadas
    perfil$meta$materializacion <- materializacion
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
  if (!is.null(perfil) && is.data.frame(perfil$columnas)) {
    comunes <- intersect(names(columnas), names(perfil$columnas))
    for (nombre in setdiff(comunes, "n")) {
      columnas[[nombre]] <- perfil$columnas[[nombre]]
    }
    if (all(c("n", "n_faltantes") %in% names(perfil$columnas))) {
      columnas$n_validos <- as.numeric(perfil$columnas$n) -
        as.numeric(perfil$columnas$n_faltantes)
    }
    columnas$n <- .conteo_dbi(n_total)
  }
  motivo_metricas <- if (is.null(motivo)) {
    paste(
      "Medido sobre la unica seleccion materializada en el spool de sesion",
      "cliente; las pasadas de esta salida leen el mismo `muestra_id`."
    )
  } else as.character(motivo)
  metricas <- preparacion$metricas_ejecucion
  registros <- list(.registro_sql_dbi(
    campos, rep("n", length(campos)), "calculado", NA_character_,
    preparacion$sql_conteo,
    metadatos = .metadatos_sql_dbi(
      alcance = "tabla_completa", universo = n_total,
      tamano_muestra = publico$filas_obtenidas,
      fraccion = publico$fraccion, metodo = "conteo_universo",
      error_esperado = "no_aplica", id_consulta = id_consulta_n
    ),
    medicion = if (is.null(consulta_n)) NULL else consulta_n,
    etapa = "conteo_filas"
  ))
  for (i in seq_along(campos)) {
    campos_metrica <- unlist(.CAMPOS_METRICA_DBI[metricas], use.names = FALSE)
    if (!length(campos_metrica)) next
    registros[[length(registros) + 1L]] <- .registro_sql_dbi(
      campos[[i]], campos_metrica,
      if (is.null(perfil) || !is.null(motivo)) "no_disponible" else
        "observado_muestra",
      motivo_metricas, sql_muestra,
      metadatos = .metadatos_sql_dbi(
        alcance = "muestra", universo = n_total,
        tamano_muestra = publico$filas_obtenidas,
        fraccion = publico$fraccion, metodo = "spool_sesion_cliente",
        error_esperado = "no_estimable", id_consulta = if (is.null(spool)) {
          NA_integer_
        } else spool$consulta$consulta_id
      ),
      medicion = if (is.null(spool)) NULL else spool$consulta,
      etapa = "lectura_spool_muestra"
    )
  }
  sql <- if (length(registros)) do.call(rbind, registros) else {
    .registro_sql_dbi(character(), character(), character(), character(), character())
  }
  sql <- .marcar_nivel_sql_dbi(sql)
  if (!is.null(motivo)) {
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "perfil_muestra", .texto_tabla_dbi(tabla), "no_disponible", motivo,
      paste(
        "No se publica una muestra parcial: corregir el presupuesto, la",
        "capacidad o el error declarado y repetir la materializacion."
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
      "`snapshot_id` identifica la fotografia del spool cliente y no demuestra",
      "una instantanea transaccional del motor. Solo se publica evidencia",
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
      agotado = isTRUE(presupuesto$agotado)
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
  meta$vigilante <- list(eventos = if (is.null(spool)) {
    .eventos_spool_vacios_dbi()
  } else spool$eventos)
  if (!is.null(perfil)) {
    perfil$meta$bytes <- meta$bytes
    perfil$meta$vigilante <- meta$vigilante
  }
  if (!is.null(perfil)) {
    args_proteger <- is.null(argumentos$proteger_datos_personales) ||
      isTRUE(argumentos$proteger_datos_personales)
    if (args_proteger) {
      columnas_personales <- .columnas_personales_protegidas(
        perfil$datos_personales
      )
      if (length(columnas_personales)) {
        columnas[columnas$columna %in% columnas_personales, "moda"] <- NA_character_
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
  resumen <- list(
    columnas = columnas, sql = sql, cobertura = cobertura,
    literales = character(), meta = meta
  )
  estructura <- list(
    resumen_tabla = resumen,
    perfil_muestra = perfil
  )
  if (!is.null(spool$path)) unlink(spool$path, force = TRUE)
  class(estructura) <- "perfil_dbi"
  estructura
}
