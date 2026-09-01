# LSH externo para el ejecutor por bloques.
#
# La pasada publica de `detectar_duplicados_aproximados()` conserva su camino
# de una pasada. Estas funciones son la familia `lsh` del ejecutor interno: el
# texto de un bloque y todos los indices intermedios viven en RDS separados y
# ordenados. Nunca se conserva el diccionario de q-gramas completo en R.

.VERSION_DERRAME_LSH <- "lsh-runs-1"
.FACTOR_PICO_LSH <- 30
.TAMANO_CHUNK_LSH <- 100000L
.TAMANO_MERGE_LSH <- 8L
.TAMANO_CUBETA_LSH <- 100L

.factor_pico_lsh <- function(q = 3L) {
  list(
    factor_pico = as.numeric(.FACTOR_PICO_LSH),
    q = as.integer(q),
    fuente = "medicion_8MB_q3_expansion_aproximada_29x_redondeada_30x",
    es_dimensionamiento = TRUE
  )
}

.rss_proceso_lsh <- function() {
  # `gc()` no es RSS. En Linux `/proc/self/statm` permite registrar el dato
  # separado; en otros sistemas la ausencia queda visible y no se inventa.
  ruta <- "/proc/self/statm"
  if (!file.exists(ruta)) return(NA_real_)
  paginas <- tryCatch(as.numeric(strsplit(trimws(readLines(ruta, n = 1L)),
                                          "\\s+")[[1L]]),
                      error = function(e) numeric())
  if (length(paginas) < 2L || !is.finite(paginas[[2L]])) return(NA_real_)
  paginas[[2L]] * 4096
}

.lsh_backend_externo_disponible <- function(directorio = NULL) {
  base <- if (is.null(directorio)) tempdir() else directorio
  if (!dir.exists(base) && !dir.create(base, recursive = TRUE,
                                        showWarnings = FALSE)) {
    return(FALSE)
  }
  archivo <- tempfile("lupa-lsh-probe-", tmpdir = base, fileext = ".rds")
  ok <- tryCatch({
    saveRDS(list(version = .VERSION_DERRAME_LSH), archivo, compress = FALSE)
    identical(readRDS(archivo)$version, .VERSION_DERRAME_LSH)
  }, error = function(e) FALSE)
  if (file.exists(archivo)) unlink(archivo)
  isTRUE(ok)
}

.lsh_directorio <- function(directorio = NULL) {
  base <- if (is.null(directorio)) tempdir() else directorio
  if (!dir.exists(base) && !dir.create(base, recursive = TRUE,
                                        showWarnings = FALSE)) {
    stop("No se pudo crear el directorio base de derrame LSH.", call. = FALSE)
  }
  salida <- tempfile("lupa-lsh-", tmpdir = base)
  if (!dir.create(salida, recursive = TRUE, showWarnings = FALSE)) {
    stop("No se pudo crear el directorio de derrame LSH.", call. = FALSE)
  }
  salida
}

.lsh_guardar_chunk <- function(datos, directorio, prefijo) {
  archivo <- tempfile(paste0("lupa-", prefijo, "-"), tmpdir = directorio,
                      fileext = ".rds")
  saveRDS(datos, archivo, compress = FALSE)
  archivo
}

.lsh_bytes_archivos <- function(archivos) {
  archivos <- archivos[file.exists(archivos)]
  if (!length(archivos)) return(0)
  tamanos <- file.info(archivos)$size
  tamanos[!is.finite(tamanos)] <- 0
  sum(as.numeric(tamanos))
}

.lsh_checksum <- function(archivos) {
  archivos <- sort(archivos[file.exists(archivos)])
  if (!length(archivos)) return(NA_character_)
  suma <- tryCatch(tools::md5sum(archivos), error = function(e) NULL)
  if (is.null(suma)) return(NA_character_)
  paste(unname(suma), collapse = ":")
}

.lsh_validar_run <- function(archivo, columnas) {
  datos <- tryCatch(readRDS(archivo), error = function(e) e)
  if (inherits(datos, "condition") || !is.data.frame(datos) ||
      !all(columnas %in% names(datos))) {
    stop("run LSH ilegible o con formato incompatible.", call. = FALSE)
  }
  datos
}

.lsh_partes_run <- function(archivo) {
  if (!file.exists(archivo)) return(character())
  contenido <- tryCatch(readRDS(archivo), error = function(e) NULL)
  if (is.list(contenido) && isTRUE(contenido$lsh_manifest)) {
    return(as.character(contenido$chunks %||% character()))
  }
  archivo
}

.lsh_borrar_runs <- function(archivos) {
  archivos <- unique(archivos[file.exists(archivos)])
  if (!length(archivos)) return(invisible(NULL))
  partes <- unique(unlist(lapply(archivos, .lsh_partes_run), use.names = FALSE))
  unlink(unique(c(archivos, partes)[file.exists(c(archivos, partes))]),
         force = TRUE)
  invisible(NULL)
}

.lsh_cursor_run <- function(archivo, columnas) {
  estado <- new.env(parent = emptyenv())
  estado$partes <- .lsh_partes_run(archivo)
  estado$indice <- 0L
  estado$datos <- NULL
  estado$posicion <- 1L
  cargar_siguiente <- function() {
    repeat {
      estado$indice <- estado$indice + 1L
      if (estado$indice > length(estado$partes)) {
        estado$datos <- NULL
        estado$posicion <- 1L
        return(invisible(NULL))
      }
      datos <- .lsh_validar_run(estado$partes[[estado$indice]], columnas)
      if (nrow(datos)) {
        estado$datos <- datos
        estado$posicion <- 1L
        return(invisible(NULL))
      }
    }
  }
  cargar_siguiente()
  list(
    valor = function(columna) {
      if (is.null(estado$datos)) return(NULL)
      estado$datos[[columna]][[estado$posicion]]
    },
    fila = function() {
      if (is.null(estado$datos)) return(NULL)
      lapply(estado$datos, function(columna) columna[[estado$posicion]])
    },
    resto = function() {
      if (is.null(estado$datos)) return(NULL)
      estado$datos[estado$posicion:nrow(estado$datos), , drop = FALSE]
    },
    maximo = function(columnas) {
      if (is.null(estado$datos)) return(NULL)
      lapply(columnas, function(columna) {
        estado$datos[[columna]][[nrow(estado$datos)]]
      }) |>
        stats::setNames(columnas)
    },
    consumir = function(n) {
      if (is.null(estado$datos) || !n) return(invisible(NULL))
      estado$posicion <- estado$posicion + as.integer(n)
      if (estado$posicion > nrow(estado$datos)) cargar_siguiente()
      invisible(NULL)
    },
    avanzar = function() {
      if (is.null(estado$datos)) return(invisible(NULL))
      estado$posicion <- estado$posicion + 1L
      if (estado$posicion > nrow(estado$datos)) cargar_siguiente()
      invisible(NULL)
    }
  )
}

.lsh_menor_igual_clave <- function(datos, limite, columnas) {
  igual <- rep(TRUE, nrow(datos))
  menor <- rep(FALSE, nrow(datos))
  for (columna in columnas) {
    valor <- datos[[columna]]
    tope <- limite[[columna]]
    if (is.na(tope)) {
      igual <- igual & is.na(valor) & is.na(tope)
      next
    }
    valido <- !is.na(valor)
    menor <- menor | (igual & valido & valor < tope)
    igual <- igual & valido & valor == tope
  }
  menor | igual
}

.lsh_guardar_por_chunks <- function(datos, directorio, prefijo,
                                    tamano = .TAMANO_CHUNK_LSH) {
  if (!nrow(datos)) return(character())
  bordes <- seq.int(1L, nrow(datos), by = tamano)
  vapply(bordes, function(inicio) {
    fin <- min(nrow(datos), inicio + tamano - 1L)
    .lsh_guardar_chunk(datos[inicio:fin, , drop = FALSE], directorio, prefijo)
  }, character(1L))
}

.lsh_ordenar_datos <- function(datos, columnas) {
  if (!nrow(datos)) return(datos)
  # El orden de q-gramas sólo necesita ser total y consistente entre runs; la
  # numeracion del diccionario se decide despues por (ordinal, posicion), no
  # por este orden. `radix` evita que la colacion de millones de strings haga
  # cuadruplicar el RSS. Las claves de cubeta, en cambio, conservan el orden
  # de caracteres de R para mantener la salida publica byte a byte.
  argumentos <- unname(datos[columnas])
  if ("qgrama" %in% columnas) {
    argumentos <- c(argumentos, list(method = "radix"))
  }
  datos[do.call(order, argumentos), , drop = FALSE]
}

.lsh_deduplicar <- function(datos, columnas) {
  if (!nrow(datos) || !length(columnas)) return(datos)
  datos[!duplicated(datos[columnas]), , drop = FALSE]
}

.lsh_normalizar_runs <- function(archivos, columnas_orden, directorio,
                                prefijo, tamano = .TAMANO_CHUNK_LSH) {
  # Un spool de una fila larga puede llegar en chunks que estan ordenados
  # internamente pero no entre si. Primero se ordena cada entrada; recien
  # despues el merge considera cada chunk como un run monotono.
  archivos <- archivos[file.exists(archivos)]
  if (!length(archivos)) return(character())
  salidas <- unlist(lapply(seq_along(archivos), function(i) {
    partes <- .lsh_partes_run(archivos[[i]])
    unlist(lapply(seq_along(partes), function(j) {
      datos <- .lsh_validar_run(partes[[j]], columnas_orden)
      datos <- .lsh_ordenar_datos(datos, columnas_orden)
      .lsh_guardar_por_chunks(
        datos, directorio, paste0(prefijo, "-", i, "-", j), tamano
      )
    }), use.names = FALSE)
  }), use.names = FALSE)
  .lsh_borrar_runs(archivos)
  salidas
}

# Fusiona como maximo ocho runs a la vez. Cada entrada y cada salida es un
# chunk, asi que el merge no convierte un run externo en un objeto de tamano
# texto completo.
.lsh_fusionar_grupo <- function(archivos, columnas_orden, directorio,
                                prefijo, deduplicar = character(),
                                tamano = .TAMANO_CHUNK_LSH) {
  if (!length(archivos)) return(character())
  cursores <- lapply(archivos, .lsh_cursor_run, columnas = columnas_orden)
  archivos_salida <- character()
  anterior <- NULL
  iteracion <- 0L
  repeat {
    iteracion <- iteracion + 1L
    restos <- lapply(cursores, function(cursor) cursor$resto())
    disponibles <- which(vapply(restos, Negate(is.null), logical(1L)))
    if (!length(disponibles)) break
    maximos <- lapply(disponibles, function(i) {
      cursores[[i]]$maximo(columnas_orden)
    })
    valores_tope <- lapply(seq_along(columnas_orden), function(j) {
      unlist(lapply(maximos, `[[`, j), use.names = FALSE)
    })
    orden_tope <- if ("qgrama" %in% columnas_orden) {
      do.call(order, c(valores_tope, list(method = "radix")))
    } else {
      do.call(order, valores_tope)
    }
    limite <- maximos[[orden_tope[[1L]]]]
    partes <- lapply(disponibles, function(i) {
      datos <- restos[[i]]
      datos$.lsh_run <- i
      datos
    })
    unidos <- do.call(rbind, partes)
    unidos <- .lsh_ordenar_datos(unidos, columnas_orden)
    if ("qgrama" %in% columnas_orden) {
      iguales_limite <- Reduce(`&`, lapply(columnas_orden, function(columna) {
        unidos[[columna]] == limite[[columna]]
      }))
      tomar <- seq_len(max(which(iguales_limite)))
    } else {
      tomar <- .lsh_menor_igual_clave(unidos, limite, columnas_orden)
    }
    indices_tomar <- if (is.logical(tomar)) which(tomar) else tomar
    if (!length(indices_tomar)) stop("merge LSH sin avance.", call. = FALSE)
    for (i in disponibles) {
      cursores[[i]]$consumir(sum(unidos$.lsh_run[indices_tomar] == i))
    }
    salida <- as.data.frame(unidos[indices_tomar, , drop = FALSE])
    salida$.lsh_run <- NULL
    if (length(deduplicar)) {
      salida <- salida[!duplicated(salida[deduplicar]),
                       , drop = FALSE]
      while (nrow(salida) && !is.null(anterior)) {
        igual_anterior <- all(vapply(deduplicar, function(columna) {
          isTRUE(all.equal(salida[[columna]][[1L]], anterior[[columna]],
                           check.attributes = FALSE))
        }, logical(1L)))
        if (!isTRUE(igual_anterior)) break
        salida <- salida[-1L, , drop = FALSE]
      }
    }
    if (nrow(salida)) {
      salida <- as.data.frame(salida)
      archivos_salida <- c(
        archivos_salida,
        .lsh_guardar_por_chunks(salida, directorio,
                                paste0(prefijo, "-chunk"), tamano)
      )
      anterior <- as.list(salida[nrow(salida), , drop = FALSE])
    }
    # `restos` y `unidos` son copias temporales de varios buffers. Liberarlos
    # antes de pedir el siguiente lote evita que el RSS acumule iteraciones del
    # merge aunque R todavía tenga espacio libre administrado por el GC.
    rm(restos, disponibles, maximos, valores_tope, orden_tope, limite,
       partes, unidos, tomar, salida)
    if (tamano >= 10000L || iteracion %% 8L == 0L) gc(verbose = FALSE)
  }
  if (!length(archivos_salida)) return(character())
  .lsh_guardar_chunk(
    list(lsh_manifest = TRUE, chunks = archivos_salida), directorio,
    paste0(prefijo, "-manifest")
  )
}

.lsh_ordenar_runs <- function(archivos, columnas_orden, directorio,
                              prefijo, deduplicar = character(),
                              tamano = .TAMANO_CHUNK_LSH) {
  archivos <- archivos[file.exists(archivos)]
  if (!length(archivos)) return(character())
  archivos <- .lsh_normalizar_runs(
    archivos, columnas_orden, directorio, paste0(prefijo, "-entrada"), tamano
  )
  if (!length(archivos)) return(character())
  nivel <- 1L
  while (length(archivos) > .TAMANO_MERGE_LSH) {
    grupos <- split(archivos, ceiling(seq_along(archivos) /
                                      .TAMANO_MERGE_LSH))
    nuevos <- vapply(seq_along(grupos), function(i) {
      .lsh_fusionar_grupo(
        grupos[[i]], columnas_orden, directorio,
        paste0(prefijo, "-m", nivel, "-", i), deduplicar, tamano
      )
    }, character(1L))
    .lsh_borrar_runs(archivos)
    archivos <- nuevos
    nivel <- nivel + 1L
  }
  final <- .lsh_fusionar_grupo(
    archivos, columnas_orden, directorio,
    paste0(prefijo, "-final"), deduplicar, tamano
  )
  .lsh_borrar_runs(archivos)
  partes <- .lsh_partes_run(final)
  if (length(final) && file.exists(final)) unlink(final, force = TRUE)
  partes[file.exists(partes)]
}

.lsh_emitir_qgramas <- function(texto, ordinal, q, directorio, prefijo,
                                tamano = .TAMANO_CHUNK_LSH) {
  if (length(texto) != 1L || is.na(texto) || !nzchar(texto)) {
    return(character())
  }
  texto <- as.character(texto)
  largo <- nchar(texto, type = "chars")
  total <- if (largo < q) 1L else largo - q + 1L
  inicios <- seq.int(1L, total, by = tamano)
  vapply(inicios, function(inicio) {
    fin <- min(total, inicio + tamano - 1L)
    posiciones <- inicio:fin
    gramas <- if (largo < q) texto else {
      substring(texto, posiciones, posiciones + q - 1L)
    }
    datos <- data.frame(
      qgrama = as.character(gramas),
      ordinal = rep(as.numeric(ordinal), length(gramas)),
      posicion = as.numeric(posiciones), stringsAsFactors = FALSE
    )
    datos <- .lsh_ordenar_datos(datos, c("qgrama", "ordinal", "posicion"))
    .lsh_guardar_chunk(datos, directorio, prefijo)
  }, character(1L))
}

.lsh_runs_bloque <- function(valores, ordinales, q, directorio, prefijo,
                             tamano = .TAMANO_CHUNK_LSH) {
  if (!length(valores)) return(character())
  archivos <- unlist(lapply(seq_along(valores), function(i) {
    .lsh_emitir_qgramas(
      valores[[i]], ordinales[[i]], q, directorio,
      paste0(prefijo, "-f", i), tamano
    )
  }), use.names = FALSE)
  .lsh_ordenar_runs(
    archivos, c("qgrama", "ordinal", "posicion"), directorio,
    paste0(prefijo, "-sort"), tamano = tamano
  )
}

.lsh_construir_diccionario <- function(runs_qgramas, directorio,
                                       tamano = .TAMANO_CHUNK_LSH) {
  qgramas <- .lsh_ordenar_runs(
    runs_qgramas, c("qgrama", "ordinal", "posicion"), directorio,
    "diccionario-qgrama", deduplicar = "qgrama", tamano = tamano
  )
  if (!length(qgramas)) {
    return(list(runs = character(), vocabulario = 0L, bytes = 0,
                checksum = NA_character_))
  }
  por_primera_aparicion <- .lsh_ordenar_runs(
    qgramas, c("ordinal", "posicion", "qgrama"), directorio,
    "diccionario-primero", tamano = tamano
  )
  n_vocabulario <- sum(vapply(
    por_primera_aparicion,
    function(archivo) nrow(.lsh_validar_run(
      archivo, c("qgrama", "ordinal", "posicion")
    )), integer(1L)
  ))
  siguiente_id <- 1
  con_ids <- unlist(lapply(seq_along(por_primera_aparicion), function(i) {
    datos <- .lsh_validar_run(
      por_primera_aparicion[[i]], c("qgrama", "ordinal", "posicion")
    )
    inicio <- siguiente_id
    siguiente_id <<- siguiente_id + nrow(datos)
    ids <- data.frame(
      qgrama = as.character(datos$qgrama), id = as.numeric(inicio) +
        seq_len(nrow(datos)) - 1, stringsAsFactors = FALSE
    )
    .lsh_guardar_chunk(ids, directorio, paste0("diccionario-ids-", i))
  }), use.names = FALSE)
  unlink(por_primera_aparicion[file.exists(por_primera_aparicion)], force = TRUE)
  runs_finales <- .lsh_ordenar_runs(
    con_ids, c("qgrama", "id"), directorio, "diccionario-final",
    tamano = tamano
  )
  unlink(qgramas[file.exists(qgramas)], force = TRUE)
  list(
    runs = runs_finales, vocabulario = as.numeric(n_vocabulario),
    bytes = .lsh_bytes_archivos(runs_finales),
    checksum = if (length(runs_finales)) .lsh_checksum(runs_finales) else NA_character_
  )
}

.lsh_join_diccionario <- function(runs_qgramas, runs_diccionario,
                                  directorio, prefijo,
                                  tamano = .TAMANO_CHUNK_LSH,
                                  on_cache = NULL) {
  if (!length(runs_qgramas)) {
    return(list(runs = character(), faltantes = 0L, max_cache = 0))
  }
  diccionario <- runs_diccionario[file.exists(runs_diccionario)]
  if (!length(diccionario)) {
    return(list(runs = character(), faltantes = sum(vapply(
      runs_qgramas, function(a) nrow(.lsh_validar_run(
        a, c("qgrama", "ordinal", "posicion")
      )), integer(1L)
    )), max_cache = 0))
  }
  dfile <- 1L
  ddata <- .lsh_validar_run(diccionario[[dfile]], c("qgrama", "id"))
  max_cache <- as.numeric(utils::object.size(ddata))
  if (is.function(on_cache)) on_cache(max_cache)
  while (dfile <= length(diccionario) && !nrow(ddata)) {
    dfile <- dfile + 1L
    if (dfile <= length(diccionario)) {
      ddata <- .lsh_validar_run(diccionario[[dfile]], c("qgrama", "id"))
      bytes_cache <- as.numeric(utils::object.size(ddata))
      max_cache <- max(max_cache, bytes_cache)
      if (is.function(on_cache)) on_cache(bytes_cache)
    }
  }
  salida_ordinal <- numeric()
  salida_id <- numeric()
  faltantes <- 0L
  salidas <- character()
  flush <- function() {
    if (!length(salida_ordinal)) return(invisible(NULL))
    datos <- data.frame(
      ordinal = salida_ordinal, id = as.numeric(salida_id),
      stringsAsFactors = FALSE
    )
    salidas <<- c(
      salidas, .lsh_guardar_chunk(datos, directorio, paste0(prefijo, "-join"))
    )
    salida_ordinal <<- numeric()
    salida_id <<- numeric()
    invisible(NULL)
  }
  avanzar_diccionario <- function() {
    dfile <<- dfile + 1L
    if (dfile <= length(diccionario)) {
      # El cursor conserva un solo run del diccionario. El anterior queda
      # elegible para GC antes de abrir el siguiente.
      ddata <<- .lsh_validar_run(diccionario[[dfile]], c("qgrama", "id"))
      bytes_cache <- as.numeric(utils::object.size(ddata))
      max_cache <<- max(max_cache, bytes_cache)
      if (is.function(on_cache)) on_cache(bytes_cache)
    }
  }
  for (archivo in runs_qgramas) {
    datos <- .lsh_validar_run(archivo, c("qgrama", "ordinal", "posicion"))
    if (!nrow(datos)) next
    pendientes <- seq_len(nrow(datos))
    while (length(pendientes) && dfile <= length(diccionario)) {
      objetivos <- datos$qgrama[pendientes]
      combinado <- c(ddata$qgrama, objetivos)
      orden <- order(combinado, method = "radix")
      rangos <- integer(length(combinado))
      rangos[orden] <- seq_along(orden)
      tope <- ddata$qgrama[[nrow(ddata)]]
      limite_tope <- max(which(combinado[orden] == tope))
      posiciones_despues <- rangos[-seq_len(nrow(ddata))] > limite_tope
      encontrados <- match(objetivos, ddata$qgrama, nomatch = 0L)
      posiciones_encontradas <- which(encontrados > 0L)
      if (length(posiciones_encontradas)) {
        posiciones <- pendientes[posiciones_encontradas]
        salida_ordinal <- c(salida_ordinal, datos$ordinal[posiciones])
        salida_id <- c(salida_id,
                       ddata$id[encontrados[posiciones_encontradas]])
      }
      pendientes <- pendientes[posiciones_despues]
      if (length(pendientes)) avanzar_diccionario()
    }
    faltantes <- faltantes + length(pendientes)
    if (length(salida_ordinal) >= tamano) flush()
  }
  flush()
  unlink(runs_qgramas[file.exists(runs_qgramas)], force = TRUE)
  list(runs = .lsh_ordenar_runs(
    salidas, c("ordinal", "id"), directorio, paste0(prefijo, "-sort"),
    tamano = tamano
  ), faltantes = faltantes, max_cache = max_cache)
}

.lsh_firmas_desde_ids <- function(runs_ids, ordinales, n_hashes,
                                  vocabulario) {
  firmas <- matrix(Inf, nrow = length(ordinales), ncol = n_hashes)
  if (!length(runs_ids) || !length(ordinales) || !vocabulario) return(firmas)
  .con_rng_interno_lsh(1L, function() {
    # Una permutacion de `vocabulario` puede pesar cientos de MB. Se conserva
    # una sola por vez para que el diccionario no reaparezca completo en RAM;
    # el costo adicional es volver a leer los runs de ids por cada hash.
    for (h in seq_len(n_hashes)) {
      consulta <- c(NA_real_, as.numeric(sample.int(as.integer(vocabulario))))
      for (archivo in runs_ids) {
        datos <- .lsh_validar_run(archivo, c("ordinal", "id"))
        if (!nrow(datos)) next
        posiciones <- match(datos$ordinal, ordinales)
        validas <- !is.na(posiciones)
        if (!any(validas)) next
        posiciones <- posiciones[validas]
        ids <- as.integer(datos$id[validas]) + 1L
        minimos <- tapply(consulta[ids], posiciones, min)
        filas <- as.integer(names(minimos))
        firmas[filas, h] <- pmin(firmas[filas, h],
                                 as.numeric(minimos), na.rm = TRUE)
      }
      rm(consulta)
    }
    firmas
  })
}

.lsh_guardar_firmas <- function(firmas, ordinales, bandas, filas_banda,
                                directorio, prefijo) {
  if (!length(ordinales)) return(character())
  registros <- lapply(seq_len(bandas), function(banda) {
    columnas <- ((banda - 1L) * filas_banda + 1L):(banda * filas_banda)
    claves <- do.call(paste, c(
      lapply(columnas, function(j) firmas[, j]), sep = ":"
    ))
    data.frame(
      banda = rep(as.integer(banda), length(ordinales)),
      clave = as.character(claves), ordinal = as.numeric(ordinales),
      stringsAsFactors = FALSE
    )
  })
  datos <- do.call(rbind, registros)
  datos <- .lsh_ordenar_datos(datos, c("banda", "clave", "ordinal"))
  .lsh_guardar_chunk(datos, directorio, prefijo)
}

.lsh_cubeta_pares <- function(archivos_grupo, tamano_grupo, banda,
                              max_cubeta, directorio, prefijo,
                              tamano = .TAMANO_CHUNK_LSH,
                              secuencia_inicio = 0) {
  if (tamano_grupo < 2L) {
    return(list(runs = character(), generados = 0, grandes = 0L,
                troceadas = 0, teselas = 0L, lotes = 0L,
                secuencia_fin = secuencia_inicio))
  }
  posibles <- as.numeric(tamano_grupo) * (tamano_grupo - 1) / 2
  grande <- tamano_grupo > max_cubeta
  archivos_grupo <- archivos_grupo[file.exists(archivos_grupo)]
  salida <- character()
  f1 <- numeric()
  f2 <- numeric()
  secuencia <- numeric()
  siguiente_secuencia <- as.numeric(secuencia_inicio)
  flush <- function() {
    if (!length(f1)) return(invisible(NULL))
    datos <- data.frame(
      fila_1 = f1, fila_2 = f2, banda = rep(as.integer(banda), length(f1)),
      secuencia = secuencia,
      stringsAsFactors = FALSE
    )
    salida <<- c(salida, .lsh_guardar_chunk(
      datos, directorio, paste0(prefijo, "-pares")
    ))
    f1 <<- numeric()
    f2 <<- numeric()
    secuencia <<- numeric()
    invisible(NULL)
  }
  n_archivos <- length(archivos_grupo)
  for (i in seq_len(n_archivos)) {
    izquierda <- as.numeric(.lsh_validar_run(
      archivos_grupo[[i]], "ordinal"
    )$ordinal)
    if (!length(izquierda)) next
    for (j in i:n_archivos) {
      derecha <- as.numeric(.lsh_validar_run(
        archivos_grupo[[j]], "ordinal"
      )$ordinal)
      if (!length(derecha)) next
      if (i == j) {
        if (length(izquierda) < 2L) next
        pares <- utils::combn(seq_along(izquierda), 2L)
        a <- izquierda[pares[1L, ], drop = TRUE]
        b <- izquierda[pares[2L, ], drop = TRUE]
      } else {
        a <- rep(izquierda, each = length(derecha))
        b <- rep(derecha, times = length(izquierda))
      }
      f1 <- c(f1, a)
      f2 <- c(f2, b)
      secuencia <- c(secuencia, siguiente_secuencia + seq_along(a))
      siguiente_secuencia <- siguiente_secuencia + length(a)
      if (length(f1) >= tamano) flush()
    }
  }
  flush()
  unlink(archivos_grupo[file.exists(archivos_grupo)], force = TRUE)
  list(
    runs = salida, generados = posibles, grandes = as.integer(grande),
    troceadas = if (grande) posibles else 0,
    teselas = if (grande && banda == 1L) {
      nt <- ceiling(tamano_grupo / 2000)
      as.integer(nt * (nt + 1) / 2)
    } else 0L,
    lotes = if (grande && banda > 1L) as.integer(ceiling(
      (tamano_grupo - 1) / 100
    )) else 0L,
    secuencia_fin = siguiente_secuencia
  )
}

.lsh_candidatos_externos <- function(runs_firmas, directorio, bandas,
                                     max_cubeta, tamano = .TAMANO_CHUNK_LSH) {
  runs_firmas <- .lsh_ordenar_runs(
    runs_firmas, c("banda", "clave", "ordinal"), directorio,
    "cubetas", tamano = tamano
  )
  candidatos <- character()
  generados <- 0
  grandes <- 0L
  troceadas <- 0
  teselas <- 0L
  lotes <- 0L
  siguiente_secuencia <- 0
  actual_banda <- NULL
  actual_clave <- NULL
  grupo <- character()
  ordinales_grupo <- numeric()
  guardar_ordinal <- function() {
    if (!length(ordinales_grupo)) return(invisible(NULL))
    grupo <<- c(grupo, .lsh_guardar_chunk(
      data.frame(ordinal = ordinales_grupo), directorio, "cubeta-grupo"
    ))
    ordinales_grupo <<- numeric()
    invisible(NULL)
  }
  cerrar <- function() {
    guardar_ordinal()
    if (!length(grupo)) return(invisible(NULL))
    res <- .lsh_cubeta_pares(
      grupo, sum(vapply(grupo, function(a) nrow(.lsh_validar_run(a, "ordinal")),
                        integer(1L))),
      actual_banda, max_cubeta, directorio, "candidatos", tamano,
      secuencia_inicio = siguiente_secuencia
    )
    siguiente_secuencia <<- res$secuencia_fin
    candidatos <<- c(candidatos, res$runs)
    generados <<- generados + res$generados
    grandes <<- grandes + res$grandes
    troceadas <<- troceadas + res$troceadas
    teselas <<- teselas + res$teselas
    lotes <<- lotes + res$lotes
    grupo <<- character()
    invisible(NULL)
  }
  for (archivo in runs_firmas) {
    datos <- .lsh_validar_run(archivo, c("banda", "clave", "ordinal"))
    if (!nrow(datos)) next
    for (i in seq_len(nrow(datos))) {
      banda <- datos$banda[[i]]
      clave <- datos$clave[[i]]
      cambio <- !identical(actual_banda, banda) ||
        !identical(actual_clave, clave)
      if (cambio && !is.null(actual_banda)) cerrar()
      if (cambio) {
        actual_banda <- banda
        actual_clave <- clave
      }
      ordinales_grupo <- c(ordinales_grupo, datos$ordinal[[i]])
      if (length(ordinales_grupo) >= .TAMANO_CUBETA_LSH) guardar_ordinal()
    }
  }
  cerrar()
  unlink(runs_firmas[file.exists(runs_firmas)], force = TRUE)
  list(
    runs = {
      unicos <- .lsh_ordenar_runs(
        candidatos, c("fila_1", "fila_2", "secuencia"), directorio,
        "candidatos-unicos", deduplicar = c("fila_1", "fila_2"),
        tamano = tamano
      )
      .lsh_ordenar_runs(
        unicos, "secuencia", directorio, "candidatos-ordenados",
        tamano = tamano
      )
    },
    generados = generados, grandes = grandes, troceadas = troceadas,
    teselas = teselas, lotes = lotes
  )
}

.lsh_bloques_lector <- function(estado) {
  cache <- new.env(parent = emptyenv())
  cache$indice <- NA_integer_
  cache$bloque <- NULL
  localizar <- function(ordinal) {
    intervalos <- estado$bloques
    candidato <- findInterval(ordinal, vapply(intervalos, `[[`, numeric(1L),
                              "inicio"))
    candidato <- max(1L, min(length(intervalos), candidato))
    if (ordinal >= intervalos[[candidato]]$inicio &&
        ordinal <= intervalos[[candidato]]$fin) return(candidato)
    encontrado <- which(vapply(intervalos, function(b) ordinal %in% b$ordinales,
                               logical(1L)))
    if (length(encontrado)) encontrado[[1L]] else NA_integer_
  }
  cargar <- function(indice) {
    if (!identical(cache$indice, indice)) {
      cache$bloque <- readRDS(estado$bloques[[indice]]$spool)
      cache$indice <- indice
    }
    cache$bloque
  }
  valores <- function(ordinales) {
    lapply(ordinales, function(ordinal) {
      indice <- localizar(ordinal)
      if (is.na(indice)) return(NA_character_)
      bloque <- cargar(indice)
      posicion <- match(ordinal, bloque$ordinales)
      if (is.na(posicion)) NA_character_ else bloque$valores[[posicion]]
    })
  }
  bloqueos <- function(ordinales) {
    lapply(ordinales, function(ordinal) {
      indice <- localizar(ordinal)
      if (is.na(indice)) return(NA)
      bloque <- cargar(indice)
      posicion <- match(ordinal, bloque$ordinales)
      if (is.na(posicion)) NA else bloque$bloqueos[[posicion]]
    })
  }
  list(valores = valores, bloqueos = bloqueos)
}

.lsh_rango_externo <- function(estado, directorio, tamano = .TAMANO_CHUNK_LSH) {
  runs <- unlist(lapply(seq_along(estado$bloques), function(i) {
    bloque <- readRDS(estado$bloques[[i]]$spool)
    datos <- data.frame(
      valor = as.character(bloque$valores),
      ordinal = as.numeric(bloque$ordinales), stringsAsFactors = FALSE
    )
    datos <- .lsh_ordenar_datos(datos, c("valor", "ordinal"))
    .lsh_guardar_chunk(datos, directorio, paste0("rango-", i))
  }), use.names = FALSE)
  runs <- .lsh_ordenar_runs(
    runs, c("valor", "ordinal"), directorio, "rango-sort", tamano = tamano
  )
  maximo <- max(vapply(estado$bloques, function(b) max(b$ordinales), numeric(1L)))
  rango <- integer(as.integer(maximo))
  nivel <- 0L
  anterior <- NULL
  for (archivo in runs) {
    datos <- .lsh_validar_run(archivo, c("valor", "ordinal"))
    for (i in seq_len(nrow(datos))) {
      valor <- datos$valor[[i]]
      if (is.null(anterior) || !identical(anterior, valor)) {
        nivel <- nivel + 1L
        anterior <- valor
      }
      rango[[as.integer(datos$ordinal[[i]])]] <- nivel
    }
  }
  unlink(runs[file.exists(runs)], force = TRUE)
  rango
}

.lsh_estimar_externo <- function(firmas, estado, lector, bandas, filas_banda,
                                 muestra_estimacion, metodo, nucleos, p) {
  n <- nrow(firmas)
  pares <- .muestra_pares_lsh(n, muestra_estimacion)
  if (!nrow(pares)) return(list(
    candidatos_previstos = 0, probabilidad = NA_real_, muestra_usada = 0L,
    pares_benchmark = 0L, tiempo_benchmark = NA_real_, velocidad = NA_real_,
    tiempo = NA_real_
  ))
  iguales <- firmas[pares$fila_1, , drop = FALSE] ==
    firmas[pares$fila_2, , drop = FALSE]
  colisiones <- do.call(cbind, lapply(seq_len(bandas), function(banda) {
    columnas <- ((banda - 1L) * filas_banda + 1L):(banda * filas_banda)
    rowSums(matrix(iguales[, columnas], nrow = nrow(iguales))) == filas_banda
  }))
  candidatos <- rowSums(colisiones) > 0L
  total <- as.numeric(n) * (as.numeric(n) - 1) / 2
  probabilidad <- mean(candidatos)
  previstos <- probabilidad * total
  n_benchmark <- min(5000L, nrow(pares))
  if (!n_benchmark) return(list(
    candidatos_previstos = previstos, probabilidad = probabilidad,
    muestra_usada = nrow(pares), pares_benchmark = 0L,
    tiempo_benchmark = NA_real_, velocidad = NA_real_, tiempo = NA_real_
  ))
  p1 <- pares$fila_1[seq_len(n_benchmark)]
  p2 <- pares$fila_2[seq_len(n_benchmark)]
  # El benchmark usa los ordinales de la muestra. `firmas` esta indexada por
  # posicion de fila global, como en la ruta publica.
  todos_ordinales <- unlist(lapply(estado$bloques, `[[`, "ordinales"),
                            use.names = FALSE)
  v1 <- unlist(lector$valores(todos_ordinales[p1]), use.names = FALSE)
  v2 <- unlist(lector$valores(todos_ordinales[p2]), use.names = FALSE)
  invisible(.distancias_pares_duplicados(v1[[1L]], v2[[1L]], metodo, nucleos, p))
  inicio <- proc.time()[["elapsed"]]
  transcurrido <- 0
  repeticiones <- 0L
  while (transcurrido < 0.05 && repeticiones < 10000L) {
    invisible(.distancias_pares_duplicados(v1, v2, metodo, nucleos, p))
    repeticiones <- repeticiones + 1L
    transcurrido <- proc.time()[["elapsed"]] - inicio
  }
  pares_cronometrados <- n_benchmark * repeticiones
  velocidad <- if (transcurrido > 0) pares_cronometrados / transcurrido else NA_real_
  list(
    candidatos_previstos = previstos, probabilidad = probabilidad,
    muestra_usada = nrow(pares), pares_benchmark = pares_cronometrados,
    tiempo_benchmark = transcurrido, velocidad = velocidad,
    tiempo = if (is.finite(velocidad) && velocidad > 0) previstos / velocidad
             else NA_real_
  )
}

.lsh_residentes <- function(acumulador, buffers = 0, cache = 0,
                            estado_fila = 0, otros = 0) {
  residentes <- list(
    buffers_runs = max(0, as.numeric(buffers)),
    cache_diccionario = max(0, as.numeric(cache)),
    estado_fila = max(0, as.numeric(estado_fila)),
    otros = max(0, as.numeric(otros))
  )
  total <- sum(unlist(residentes))
  acumulador$estado_familia$residentes_lsh <- residentes
  acumulador$estado_familia$bytes_residentes_lsh <- total
  acumulador$estado_familia$maximo_residentes_lsh <- max(
    acumulador$estado_familia$maximo_residentes_lsh %||% 0, total
  )
  acumulador$estado_familia$maximo_cache_diccionario <- max(
    acumulador$estado_familia$maximo_cache_diccionario %||% 0,
    residentes$cache_diccionario
  )
  rss <- .rss_proceso_lsh()
  if (is.finite(rss)) {
    rss_anterior <- acumulador$estado_familia$rss_maximo
    if (length(rss_anterior) != 1L || !is.finite(rss_anterior)) {
      rss_anterior <- 0
    }
    acumulador$estado_familia$rss_maximo <- max(
      rss_anterior, rss
    )
  }
  invisible(acumulador)
}

.lsh_bytes_retenidos_acumulador <- function(acumulador) {
  estado <- acumulador$estado_familia
  residentes <- sum(unlist(estado$residentes_lsh %||% list()))
  estado_base <- list(
    configuracion = acumulador$configuracion,
    directorio = estado$directorio,
    bloques = lapply(estado$bloques %||% list(), function(bloque) {
      bloque[c("spool", "inicio", "fin")]
    }),
    salida = estado$salida
  )
  base <- as.numeric(utils::object.size(estado_base))
  max(0, residentes + base)
}

.sobre_lsh_acumulador <- function(acumulador) {
  estado_familia <- acumulador$estado_familia
  disponible <- !identical(acumulador$estado, "no_disponible") &&
    !is.null(estado_familia$salida)
  residentes <- estado_familia$residentes_lsh %||% list(
    buffers_runs = 0, cache_diccionario = 0,
    estado_fila = 0, otros = 0
  )
  bytes <- .bytes_retenidos(acumulador)
  if (!disponible) {
    return(list(
      resultado = NULL, estado = "no_disponible", exacto = NA,
      motivo = acumulador$fallo %||% "resultado_no_disponible",
      como_resolverlo = "Habilitar un backend de runs, un snapshot y un orden estable.",
      cota = NULL,
      tope = list(entradas = acumulador$max_entradas,
                  bytes = acumulador$max_bytes, nombre = "factor_pico_lsh"),
      almacenamiento = "derrame", derrame = estado_familia$derrame,
      alcance = list(filas = acumulador$ultimo_ordinal,
                     orden = acumulador$configuracion$orden_id,
                     snapshot = acumulador$configuracion$snapshot_id,
                     muestra = acumulador$configuracion$muestra_id),
      residentes_lsh = residentes, bytes_retenidos = bytes,
      memoria_diccionario = list(
        maximo_cache = estado_familia$maximo_cache_diccionario %||% 0,
        diccionario_completo_en_memoria = FALSE,
        maximo_intervalo = estado_familia$maximo_intervalo %||% 0,
        rss_maximo = estado_familia$rss_maximo %||% NA_real_
      )
    ))
  }
  salida <- estado_familia$salida
  alcance <- salida$alcance
  alcance$lsh_derrame_bytes <- estado_familia$bytes_derrame
  alcance$lsh_derrame_version <- estado_familia$version_derrame
  alcance$lsh_checksum_derrame <- estado_familia$checksum_derrame
  alcance$lsh_memoria_maximo_intervalo <- estado_familia$maximo_intervalo
  alcance$lsh_rss_maximo <- estado_familia$rss_maximo
  list(
    resultado = salida, estado = "calculado", exacto = TRUE,
    motivo = NA_character_, como_resolverlo = NA_character_, cota = NULL,
    tope = list(entradas = acumulador$max_entradas,
                bytes = acumulador$max_bytes, nombre = "factor_pico_lsh"),
    almacenamiento = "derrame", derrame = estado_familia$derrame,
    alcance = alcance, residentes_lsh = residentes,
    bytes_retenidos = bytes,
    memoria_diccionario = list(
      maximo_cache = estado_familia$maximo_cache_diccionario,
      diccionario_completo_en_memoria = FALSE,
      maximo_intervalo = estado_familia$maximo_intervalo,
      rss_maximo = estado_familia$rss_maximo
    )
  )
}

.lsh_fallo <- function(acumulador, motivo) {
  acumulador$estado <- "no_disponible"
  acumulador$fallo <- as.character(motivo)
  acumulador$estado_familia$fase <- "fallida"
  acumulador
}

.lsh_iniciar_estado <- function(acumulador) {
  if (identical(acumulador$estado, "no_disponible")) return(acumulador)
  configuracion <- acumulador$configuracion$configuracion
  directorio <- configuracion$directorio_lsh %||% NULL
  if (!.lsh_backend_externo_disponible(directorio)) {
    acumulador <- .lsh_fallo(
      acumulador, "backend_runs_merge_no_disponible"
    )
    return(acumulador)
  }
  directorio <- tryCatch(.lsh_directorio(directorio), error = function(e) e)
  if (inherits(directorio, "condition")) {
    return(.lsh_fallo(
      acumulador, paste0("derrame_fallido:directorio:", conditionMessage(directorio))
    ))
  }
  acumulador$estado_familia$directorio <- directorio
  acumulador$estado_familia$version_derrame <- .VERSION_DERRAME_LSH
  acumulador$estado_familia$factor_pico <- .FACTOR_PICO_LSH
  acumulador$estado_familia$factor_pico_fuente <-
    .factor_pico_lsh(configuracion$q %||% 3L)$fuente
  acumulador$estado_familia$fase <- "fase_1_diccionario"
  acumulador$estado_familia$bloques <- list()
  acumulador$estado_familia$runs_qgramas <- character()
  acumulador$estado_familia$runs_diccionario <- character()
  acumulador$estado_familia$runs_firmas <- character()
  acumulador$estado_familia$salida <- NULL
  acumulador$estado_familia$derrame <- NULL
  acumulador$estado_familia$maximo_residentes_lsh <- 0
  acumulador$estado_familia$maximo_cache_diccionario <- 0
  acumulador$estado_familia$maximo_intervalo <- 0
  .lsh_residentes(acumulador, estado_fila = as.numeric(utils::object.size(
    acumulador$estado_familia
  )))
  acumulador
}

.absorber_lsh <- function(acumulador, bloque) {
  if (identical(acumulador$estado, "no_disponible")) return(acumulador)
  estado <- acumulador$estado_familia
  configuracion <- acumulador$configuracion$configuracion
  ordinales <- .ordinales_bloque(bloque)
  valores <- bloque$valores
  if (is.data.frame(valores) || !is.atomic(valores) || length(ordinales) != length(valores)) {
    return(.lsh_fallo(acumulador, "lsh_valores_no_textuales"))
  }
  valores <- suppressWarnings(as.character(valores))
  aplicable <- !is.na(bloque$aplicable) & bloque$aplicable
  bloqueos <- bloque$bloqueos
  if (is.null(bloqueos)) bloqueos <- rep(NA, length(valores))
  if (length(bloqueos) != length(valores)) {
    return(.lsh_fallo(acumulador, "lsh_bloqueos_no_reproducibles"))
  }
  spool <- tempfile("lupa-lsh-texto-", tmpdir = estado$directorio, fileext = ".rds")
  datos_spool <- list(
    valores = valores, ordinales = as.numeric(ordinales),
    aplicable = aplicable, bloqueos = bloqueos
  )
  ok <- tryCatch({ saveRDS(datos_spool, spool, compress = FALSE); TRUE },
                 error = function(e) e)
  if (!isTRUE(ok)) {
    return(.lsh_fallo(acumulador, paste0("derrame_fallido:spool:",
                                         conditionMessage(ok))))
  }
  q <- as.integer(configuracion$q %||% 3L)
  runs <- tryCatch(.lsh_runs_bloque(
    valores[aplicable], ordinales[aplicable], q, estado$directorio,
    paste0("fase1-bloque-", length(estado$bloques) + 1L),
    as.integer(configuracion$chunk_qgramas %||% .TAMANO_CHUNK_LSH)
  ), error = function(e) e)
  if (inherits(runs, "condition")) {
    return(.lsh_fallo(acumulador, paste0("derrame_fallido:qgramas:",
                                         conditionMessage(runs))))
  }
  estado$bloques[[length(estado$bloques) + 1L]] <- list(
    spool = spool, ordinales = as.numeric(ordinales),
    inicio = min(ordinales), fin = max(ordinales)
  )
  estado$runs_qgramas <- c(estado$runs_qgramas, runs)
  acumulador$estado_familia <- estado
  .lsh_residentes(acumulador, estado_fila = as.numeric(utils::object.size(
    list(valores = valores, ordinales = ordinales)
  )))
  # El bloque y las listas de q-gramas se liberan antes de la barrera del
  # vigilante. El spool externo no es memoria residente.
  .lsh_residentes(acumulador)
  acumulador
}

.lsh_comparar_candidatos <- function(acumulador, candidatos, lector, rango,
                                     metodo, umbral, nucleos, p,
                                     max_resultados, bloqueos = FALSE) {
  estado <- acumulador$estado_familia
  acumulador_duplicados <- .nuevo_acumulador_duplicados(max_resultados, rango)
  comparados <- 0
  descartados_bloque <- 0
  jaccard <- numeric()
  jaccard_elegibles <- 0
  jaccard_bajo <- 0
  for (archivo in candidatos) {
    datos <- .lsh_validar_run(archivo, c("fila_1", "fila_2", "banda"))
    if (!nrow(datos)) next
    dentro <- rep(TRUE, nrow(datos))
    if (bloqueos) {
      b1 <- unlist(lector$bloqueos(datos$fila_1), use.names = FALSE)
      b2 <- unlist(lector$bloqueos(datos$fila_2), use.names = FALSE)
      dentro <- !is.na(b1) & !is.na(b2) & b1 == b2
      descartados_bloque <- descartados_bloque + sum(!dentro)
    }
    datos <- datos[dentro, , drop = FALSE]
    if (!nrow(datos)) next
    valores_1 <- unlist(lector$valores(datos$fila_1), use.names = FALSE)
    valores_2 <- unlist(lector$valores(datos$fila_2), use.names = FALSE)
    distancias <- .distancias_pares_duplicados(
      valores_1, valores_2, metodo, nucleos, p
    )
    comparados <- comparados + nrow(datos)
    pasan <- is.finite(distancias) & distancias <= umbral
    if (!any(pasan)) next
    jaccard_elegibles <- jaccard_elegibles + sum(pasan)
    indices <- which(pasan)
    if (length(jaccard) < 10000L) {
      tomar <- indices[seq_len(min(length(indices), 10000L - length(jaccard)))]
      for (indice in tomar) {
        valor <- .jaccard_qgramas(
          .qgramas_lsh(
            valores_1[[indice]],
            acumulador$configuracion$configuracion$q %||% 3L
          )[[1L]],
          .qgramas_lsh(
            valores_2[[indice]],
            acumulador$configuracion$configuracion$q %||% 3L
          )[[1L]]
        )
        jaccard <- c(jaccard, valor)
        jaccard_bajo <- jaccard_bajo + (valor < 0.7)
      }
    }
    iguales <- valores_1[indices] == valores_2[indices]
    originales <- if (is.function(
      acumulador$configuracion$configuracion$igualdad_original
    )) {
      acumulador$configuracion$configuracion$igualdad_original(
        datos$fila_1[indices], datos$fila_2[indices]
      )
    } else iguales
    acumulador_duplicados <- .acumular_pares_duplicados(
      acumulador_duplicados, datos$fila_1[indices], datos$fila_2[indices],
      distancias[indices], iguales, originales
    )
  }
  acumulados <- .pares_acumulador_con_igualdad(acumulador_duplicados)
  list(
    acumulador = acumulador_duplicados, pares = acumulados$pares,
    iguales = acumulados$iguales, comparados = comparados,
    descartados_bloque = descartados_bloque, jaccard = jaccard,
    jaccard_elegibles = jaccard_elegibles, jaccard_bajo = jaccard_bajo
  )
}

.finalizar_lsh <- function(acumulador) {
  if (identical(acumulador$estado, "no_disponible")) return(acumulador)
  estado <- acumulador$estado_familia
  conf <- acumulador$configuracion$configuracion
  fase <- tryCatch({
    diccionario <- .lsh_construir_diccionario(
      estado$runs_qgramas, estado$directorio,
      as.integer(conf$chunk_qgramas %||% .TAMANO_CHUNK_LSH)
    )
    estado$runs_diccionario <- diccionario$runs
    estado$vocabulario <- diccionario$vocabulario
    estado$checksum_diccionario <- diccionario$checksum
    cache_diccionario <- if (length(estado$runs_diccionario)) {
      as.numeric(utils::object.size(readRDS(estado$runs_diccionario[[1L]])))
    } else 0
    estado$maximo_cache_diccionario <- max(
      estado$maximo_cache_diccionario %||% 0, cache_diccionario
    )
    .lsh_residentes(acumulador, cache = cache_diccionario)
    estado$fase <- "fase_2_firmas"
    lector <- .lsh_bloques_lector(estado)
    runs_firmas <- character()
    n_hashes <- as.integer(conf$bandas) * as.integer(conf$filas_banda)
    for (i in seq_along(estado$bloques)) {
      bloque <- readRDS(estado$bloques[[i]]$spool)
      runs_q <- .lsh_runs_bloque(
        bloque$valores[bloque$aplicable],
        bloque$ordinales[bloque$aplicable], as.integer(conf$q),
        estado$directorio, paste0("fase2-bloque-", i),
        as.integer(conf$chunk_qgramas %||% .TAMANO_CHUNK_LSH)
      )
      # La unicidad por (qgrama, ordinal) reproduce `unique()` por fila sin
      # conservar todos los q-gramas de una fila en RAM.
      runs_q <- .lsh_ordenar_runs(
        runs_q, c("qgrama", "ordinal", "posicion"), estado$directorio,
        paste0("fase2-unicos-", i), deduplicar = c("qgrama", "ordinal"),
        tamano = as.integer(conf$chunk_qgramas %||% .TAMANO_CHUNK_LSH)
      )
      unidos <- .lsh_join_diccionario(
        runs_q, estado$runs_diccionario, estado$directorio,
                paste0("fase2-join-", i),
        as.integer(conf$chunk_qgramas %||% .TAMANO_CHUNK_LSH),
        on_cache = function(bytes) {
          estado$maximo_cache_diccionario <- max(
            estado$maximo_cache_diccionario %||% 0, bytes
          )
          .lsh_residentes(acumulador, cache = bytes)
        }
      )
      estado$maximo_cache_diccionario <- max(
        estado$maximo_cache_diccionario %||% 0,
        unidos$max_cache %||% 0
      )
      if (unidos$faltantes > 0L) stop(
        "merge-join del diccionario dejo q-gramas sin identificador.",
        call. = FALSE
      )
      firmas <- .lsh_firmas_desde_ids(
        unidos$runs, bloque$ordinales, n_hashes, estado$vocabulario
      )
      runs_firmas <- c(runs_firmas, .lsh_guardar_firmas(
        firmas, bloque$ordinales, conf$bandas, conf$filas_banda,
        estado$directorio, paste0("firmas-", i)
      ))
      unlink(unidos$runs[file.exists(unidos$runs)], force = TRUE)
      .lsh_residentes(acumulador,
                      buffers = .lsh_bytes_archivos(runs_q),
                      cache = max(1, unidos$max_cache %||% 0),
                      estado_fila = as.numeric(utils::object.size(firmas)))
      .lsh_residentes(acumulador)
    }
    estado$runs_firmas <- runs_firmas
    estado$fase <- "fase_2_cubetas_y_candidatos"
    firmas <- do.call(rbind, lapply(runs_firmas, function(a) {
      datos <- .lsh_validar_run(a, c("banda", "clave", "ordinal"))
      datos
    }))
    firmas <- firmas[order(firmas$banda, firmas$ordinal), , drop = FALSE]
    # Reconstruir la matriz sólo para la estimacion. Se descarta antes de las
    # cubetas; el diccionario sigue siendo exclusivamente externo.
    n <- sum(vapply(estado$bloques, function(b) length(b$ordinales), integer(1L)))
    todos_ordinales <- unlist(lapply(estado$bloques, `[[`, "ordinales"),
                              use.names = FALSE)
    matriz_firmas <- matrix(Inf, nrow = n, ncol = n_hashes)
    for (banda in seq_len(conf$bandas)) {
      parte <- firmas[firmas$banda == banda, , drop = FALSE]
      grupos <- split(parte, parte$ordinal)
      for (nombre in names(grupos)) {
        ordinal <- as.integer(nombre)
        claves <- strsplit(grupos[[nombre]]$clave[[1L]], ":", fixed = TRUE)[[1L]]
        # La tabla consultable de estimacion se completa mas abajo desde las
        # claves por banda; este bloque sólo evita conservar `ids`.
        invisible(claves)
        indice <- match(ordinal, todos_ordinales)
        if (is.na(indice)) next
        # Las claves de cada banda son las filas de firma concatenadas. Se
        # recuperan como numeros; `Inf` se conserva para filas sin q-gramas.
        inicio <- (banda - 1L) * conf$filas_banda + 1L
        matriz_firmas[indice, inicio:(inicio + conf$filas_banda - 1L)] <-
          as.numeric(claves)
      }
    }
    lector <- .lsh_bloques_lector(estado)
    estimacion <- .lsh_estimar_externo(
      matriz_firmas, estado, lector, conf$bandas, conf$filas_banda,
      conf$muestra_estimacion, conf$metodo, conf$nucleos, conf$p
    )
    .controlar_presupuesto_pares(
      estimacion$candidatos_previstos, conf$presupuesto_pares
    )
    candidatos <- .lsh_candidatos_externos(
      runs_firmas, estado$directorio, conf$bandas, conf$max_cubeta,
      as.integer(conf$chunk_qgramas %||% .TAMANO_CHUNK_LSH)
    )
    rango <- .lsh_rango_externo(estado, estado$directorio)
    comparacion <- .lsh_comparar_candidatos(
      acumulador, candidatos$runs, lector, rango, conf$metodo, conf$umbral,
      conf$nucleos, conf$p, conf$max_resultados,
      bloqueos = isTRUE(conf$usar_bloqueos)
    )
    acumulador_duplicados <- comparacion$acumulador
    resumen_jaccard <- if (length(comparacion$jaccard)) {
      stats::quantile(comparacion$jaccard,
                      probs = c(0, .25, .5, .75, 1), names = FALSE)
    } else rep(NA_real_, 5L)
    estado$salida <- list(
      pares = comparacion$pares, iguales = comparacion$iguales,
      distancia_minima_descartada = acumulador_duplicados$distancia_minima_descartada,
      n_hallados = acumulador_duplicados$n_hallados,
      n_exactos = acumulador_duplicados$n_exactos,
      n_exactos_normalizados = acumulador_duplicados$n_exactos_normalizados,
      n_aproximados = acumulador_duplicados$n_aproximados,
      n_bloques = length(estado$bloques),
      estimacion = list(
        candidatos_previstos = estimacion$candidatos_previstos,
        probabilidad_candidato_estimada = estimacion$probabilidad,
        muestra_estimacion = estimacion$muestra_usada,
        vocabulario = estado$vocabulario,
        pares_benchmark = estimacion$pares_benchmark,
        tiempo_benchmark = estimacion$tiempo_benchmark,
        velocidad_comparacion = estimacion$velocidad,
        tiempo_estimado_segundos = estimacion$tiempo,
        tiempo_estimado_etapa = "comparacion_stringdist",
        tiempo_estimado_es_piso = TRUE, tiempo_determinista = FALSE
      ),
      alcance = data.frame(
        lsh_bandas = conf$bandas, lsh_filas = conf$filas_banda,
        lsh_tamano_firma = n_hashes, lsh_q = conf$q,
        lsh_vocabulario = estado$vocabulario, lsh_semilla_hash = 1L,
        lsh_hash_familia = "permutacion_aleatoria_determinista_inyectiva",
        lsh_candidatos_dependen_orden_filas = TRUE,
        lsh_orden_vocabulario = "primera_aparicion",
        lsh_max_cubeta = conf$max_cubeta,
        lsh_garantia_jaccard_09 = .garantia_lsh(
          0.9, conf$bandas, conf$filas_banda, 0
        ),
        lsh_garantia_jaccard_08 = .garantia_lsh(
          0.8, conf$bandas, conf$filas_banda, 0
        ),
        lsh_garantia_jaccard_07 = .garantia_lsh(
          0.7, conf$bandas, conf$filas_banda, 0
        ),
        lsh_garantia_aplica_a = "Jaccard de qgramas; no garantiza la medida final",
        lsh_garantia_estado = .estado_garantia_lsh(0),
        lsh_cubetas_grandes = candidatos$grandes,
        lsh_pares_descartados_cubetas = 0,
        lsh_pares_cubetas_troceadas = candidatos$troceadas,
        lsh_teselas_cubetas_grandes = candidatos$teselas,
        lsh_lotes_cubetas_grandes = candidatos$lotes,
        lsh_candidatos_generados = candidatos$generados,
        lsh_candidatos_unicos = sum(vapply(candidatos$runs, function(a) {
          nrow(.lsh_validar_run(a, c("fila_1", "fila_2", "banda")))
        }, integer(1L))),
        lsh_candidatos_descartados_bloque = comparacion$descartados_bloque,
        lsh_candidatos_previstos = estimacion$candidatos_previstos,
        lsh_candidatos_previstos_es_estimacion = TRUE,
        lsh_probabilidad_candidato_estimada = estimacion$probabilidad,
        lsh_muestra_estimacion = estimacion$muestra_usada,
        lsh_muestra_estimacion_configurada = conf$muestra_estimacion,
        nucleos_usados = conf$nucleos,
        lsh_presupuesto_pares = conf$presupuesto_pares,
        lsh_candidatos_descartados_bandas = candidatos$generados -
          sum(vapply(candidatos$runs, function(a) {
            nrow(.lsh_validar_run(a, c("fila_1", "fila_2", "banda")))
          }, integer(1L))),
        lsh_pares_comparados = comparacion$comparados,
        lsh_jaccard_evaluados = length(comparacion$jaccard),
        lsh_jaccard_pares_elegibles = comparacion$jaccard_elegibles,
        lsh_jaccard_alcance = if (comparacion$jaccard_elegibles >
                                   length(comparacion$jaccard)) {
          paste0("primeros_del_recorrido_de_", length(comparacion$jaccard),
                 "_pares_de_", comparacion$jaccard_elegibles)
        } else "todos_los_pares_que_pasaron_el_umbral",
        lsh_jaccard_bajo_07 = comparacion$jaccard_bajo,
        lsh_prop_jaccard_bajo_07 = if (length(comparacion$jaccard)) {
          comparacion$jaccard_bajo / length(comparacion$jaccard)
        } else NA_real_,
        lsh_jaccard_minimo = resumen_jaccard[[1L]],
        lsh_jaccard_q1 = resumen_jaccard[[2L]],
        lsh_jaccard_mediana = resumen_jaccard[[3L]],
        lsh_jaccard_q3 = resumen_jaccard[[4L]],
        lsh_jaccard_maximo = resumen_jaccard[[5L]],
        lsh_jaccard_muestra = length(comparacion$jaccard),
        lsh_externo = TRUE,
        lsh_backend = "rds_runs_merge_join_streaming",
        lsh_fase_1 = "qgramas_runs_diccionario_merge",
        lsh_fase_2 = "merge_join_firmas_cubetas_candidatos",
        lsh_diccionario_completo_en_memoria = FALSE,
        lsh_factor_pico = .FACTOR_PICO_LSH,
        lsh_factor_pico_fuente = estado$factor_pico_fuente,
        lsh_piso_fila = 1L,
        lsh_checksum_diccionario = estado$checksum_diccionario,
        stringsAsFactors = FALSE
      )
    )
    estado$bytes_derrame <- .lsh_bytes_archivos(
      list.files(estado$directorio, full.names = TRUE)
    )
    estado$checksum_derrame <- .lsh_checksum(
      list.files(estado$directorio, full.names = TRUE)
    )
    estado$derrame <- list(
      estado = "completo", bytes = estado$bytes_derrame,
      ubicacion_logica = estado$directorio, version = .VERSION_DERRAME_LSH,
      checksum = estado$checksum_derrame
    )
    estado$maximo_residentes_lsh <- max(
      estado$maximo_residentes_lsh %||% 0,
      acumulador$estado_familia$maximo_residentes_lsh %||% 0
    )
    # `maximo_intervalo` es memoria residente, no el tamaño de un run en
    # disco. El tamaño del run de diccionario queda en `bytes_derrame`.
    estado$maximo_intervalo <- max(
      estado$maximo_intervalo %||% 0,
      estado$maximo_residentes_lsh %||% 0
    )
    rss_local <- estado$rss_maximo
    rss_acc <- acumulador$estado_familia$rss_maximo
    if (length(rss_local) != 1L || !is.finite(rss_local)) rss_local <- 0
    if (length(rss_acc) != 1L || !is.finite(rss_acc)) rss_acc <- 0
    estado$rss_maximo <- max(rss_local, rss_acc)
    acumulador$estado_familia <- estado
    .lsh_residentes(acumulador,
                    cache = estado$maximo_cache_diccionario,
                    otros = as.numeric(utils::object.size(estado$salida)))
    acumulador
  }, error = function(e) {
    .lsh_fallo(acumulador, paste0("derrame_fallido:", conditionMessage(e)))
  })
  if (inherits(fase, "acumulador_bloques")) acumulador <- fase
  else acumulador <- .lsh_fallo(acumulador, paste0("derrame_fallido:",
                                                   conditionMessage(fase)))
  acumulador
}

.ejecutar_lsh_bloques <- function(
    bloques, metodo = "jw", umbral = 0.10, p = 0.1, bandas = 12L,
    filas_banda = 3L, q = 3L, max_cubeta = 1000L, max_resultados = 100L,
    muestra_estimacion = 400000L, presupuesto_pares = Inf,
    directorio_lsh = NULL, chunk_qgramas = .TAMANO_CHUNK_LSH,
    nucleos = 2L, bloquear = FALSE, max_bytes = .MAX_BYTES_ESTADO_BLOQUES,
    fuente_id = "memoria", snapshot_id = "memoria",
    universo_id = "tabla_completa", muestra_id = NULL,
    orden_id = "orden_entrada", vigilante = NULL) {
  if (!is.list(bloques)) stop("`bloques` debe ser una lista.", call. = FALSE)
  conf <- list(
    metodo = as.character(metodo), umbral = as.numeric(umbral), p = as.numeric(p),
    bandas = as.integer(bandas), filas_banda = as.integer(filas_banda),
    q = as.integer(q), max_cubeta = as.integer(max_cubeta),
    max_resultados = max_resultados, muestra_estimacion = muestra_estimacion,
    presupuesto_pares = presupuesto_pares, directorio_lsh = directorio_lsh,
    chunk_qgramas = as.integer(chunk_qgramas), nucleos = as.integer(nucleos),
    usar_bloqueos = isTRUE(bloquear)
  )
  acumulador <- .iniciar_acumulador(
    "lsh", "character", familia = "lsh", configuracion = conf,
    fuente_id = fuente_id, snapshot_id = snapshot_id,
    universo_id = universo_id, muestra_id = muestra_id, orden_id = orden_id,
    max_entradas = Inf, max_bytes = max_bytes, requiere_orden = TRUE
  )
  if (!identical(acumulador$estado, "no_disponible")) {
    for (bloque in bloques) {
      acumulador <- .absorber_acumulador(acumulador, bloque)
      if (!is.null(vigilante)) {
        gc(verbose = FALSE)
        .registrar_barrera_vigilante(
          vigilante, "bloque", "lsh", length(acumulador$intervalos),
          acumulador = acumulador
        )
      }
      if (identical(acumulador$estado, "no_disponible")) break
    }
  }
  resultado <- .finalizar_acumulador(acumulador)
  if (!is.null(vigilante)) {
    gc(verbose = FALSE)
    .registrar_barrera_vigilante(
      vigilante, "finalizar", "lsh", length(acumulador$intervalos),
      acumulador = acumulador, resultado = resultado
    )
  }
  list(acumulador = acumulador, resultado = resultado, vigilante = vigilante)
}

.lsh_bloques <- .ejecutar_lsh_bloques
.comparar_lsh_bloques <- .ejecutar_lsh_bloques
