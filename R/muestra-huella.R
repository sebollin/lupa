# Seleccion determinista de filas por contenido y numero de aparicion.

.METODO_MUESTRA_HUELLA <- "huella_v2"
.D16_HUELLA <- 65536
.D32_HUELLA <- 4294967296
.SEMILLA_HUELLA_FILA_1 <- 0x243f6a88
.SEMILLA_HUELLA_FILA_2 <- 0x85a308d3
.SEMILLA_HUELLA_NA_1 <- 0x13579bdf
.SEMILLA_HUELLA_NA_2 <- 0x2468ace0
.SEMILLA_HUELLA_NAN_1 <- 0xdeadbeef
.SEMILLA_HUELLA_NAN_2 <- 0x10203040
.MODULO_TEXTO_HUELLA_1 <- 67108859
.MODULO_TEXTO_HUELLA_2 <- 67108813

.suma_mod32_huella <- function(x, y) {
  salida <- x + y
  salida - floor(salida / .D32_HUELLA) * .D32_HUELLA
}

.hash_bytes_huella_vectorizado <- function(valores, semilla_1, semilla_2) {
  n <- length(valores)
  if (!n) return(list(h1 = numeric(), h2 = numeric()))
  longitudes <- nchar(valores, type = "bytes")
  h1 <- rep(.mezclar32_lsh(semilla_1), n)
  h2 <- rep(.mezclar32_lsh(semilla_2), n)
  con_bytes <- longitudes > 0L
  if (!any(con_bytes)) return(list(h1 = h1, h2 = h2))

  valores_bytes <- valores[con_bytes]
  longitudes <- longitudes[con_bytes]
  separador <- rawToChar(as.raw(1L))
  flujo <- as.integer(charToRaw(paste(valores_bytes, collapse = separador)))
  separadores <- cumsum(longitudes + 1L)
  separadores <- separadores[separadores < length(flujo)]
  if (length(separadores)) flujo <- flujo[-separadores]
  grupos <- rep.int(seq_along(longitudes), longitudes)
  posiciones <- sequence(longitudes)

  modulo_1 <- .MODULO_TEXTO_HUELLA_1
  modulo_2 <- .MODULO_TEXTO_HUELLA_2
  posiciones <- as.numeric(posiciones) - 1
  flujo <- as.numeric(flujo)
  # El termino de cada byte depende solo de su posicion (desde cero), de su
  # valor y de la semilla: la clave es posicion * 256 + byte. Asi el hash de un
  # valor no cambia con los otros valores de la columna. Al reducir a 32 bits,
  # posiciones que difieren en 2^24 coinciden para el mismo byte.
  posiciones <- posiciones %% 16777216
  claves <- posiciones * 256 + flujo
  largo_tabla <- max(longitudes) * 256
  usar_tabla <- max(longitudes) <= 16777216 && largo_tabla < length(flujo)
  termino <- function(semilla, modulo) {
    if (usar_tabla) {
      claves_tabla <- seq_len(largo_tabla) - 1
      tabla <- .mezclar32_lsh(.suma_mod32_huella(claves_tabla, semilla)) %% modulo
      tabla[claves + 1]
    } else {
      .mezclar32_lsh(.suma_mod32_huella(claves, semilla)) %% modulo
    }
  }
  terminos_1 <- termino(semilla_1, modulo_1)
  sumas_1 <- rowsum(terminos_1, grupos, reorder = FALSE)[, 1L]
  base_1 <- (sumas_1 + (longitudes %% modulo_1) * 65521 +
    (as.numeric(semilla_1) %% modulo_1)) %% modulo_1

  terminos_2 <- termino(semilla_2, modulo_2)
  sumas_2 <- rowsum(terminos_2, grupos, reorder = FALSE)[, 1L]
  base_2 <- (sumas_2 + (longitudes %% modulo_2) * 65519 +
    (as.numeric(semilla_2) %% modulo_2)) %% modulo_2

  posiciones_salida <- which(con_bytes)
  h1[posiciones_salida] <- .mezclar32_lsh(base_1)
  h2[posiciones_salida] <- .mezclar32_lsh(base_2)
  list(h1 = h1, h2 = h2)
}

.canonizar_texto_huella <- function(x) {
  x <- as.character(x)
  if (!length(x)) return(x)
  x <- .texto_para_operar(x)
  # La marca `bytes` solo describe el transporte. Si sus bytes son UTF-8
  # validos, la forma canonica es ese texto; si no, se conserva cada byte.
  invalidos <- !is.na(x) & !validUTF8(x)
  if (any(invalidos)) {
    crudos <- x[invalidos]
    Encoding(crudos) <- "bytes"
    x[invalidos] <- crudos
  }
  x
}

.hash_texto_huella <- function(x) {
  canonicos <- .canonizar_texto_huella(x)
  claves <- .clave_bytes(canonicos)
  ausentes <- is.na(canonicos)
  h1 <- rep(.mezclar32_lsh(.SEMILLA_HUELLA_NA_1), length(canonicos))
  h2 <- rep(.mezclar32_lsh(.SEMILLA_HUELLA_NA_2), length(canonicos))
  presentes <- which(!ausentes)
  if (length(presentes)) {
    unicas <- unique(claves[presentes])
    representantes <- match(unicas, claves)
    valores_unicos <- canonicos[representantes]
    hashes <- .hash_bytes_huella_vectorizado(
      valores_unicos, 0x811c9dc5, 0x9e3779b9
    )
    indices <- match(claves[presentes], unicas)
    h1[presentes] <- hashes$h1[indices]
    h2[presentes] <- hashes$h2[indices]
  }
  list(h1 = h1, h2 = h2)
}

.palabras32_huella <- function(bytes, n) {
  if (!n) return(list(lo = numeric(), hi = numeric()))
  valores <- as.numeric(bytes)
  posiciones <- seq.int(1L, 8L * n, by = 8L)
  lo <- valores[posiciones] +
    valores[posiciones + 1L] * 256 +
    valores[posiciones + 2L] * 65536 +
    valores[posiciones + 3L] * 16777216
  hi <- valores[posiciones + 4L] +
    valores[posiciones + 5L] * 256 +
    valores[posiciones + 6L] * 65536 +
    valores[posiciones + 7L] * 16777216
  list(lo = lo, hi = hi)
}

.hash_numerico_huella <- function(x, entero64 = FALSE) {
  n <- length(x)
  ausentes <- is.na(x)
  nan <- if (entero64) rep(FALSE, n) else is.nan(x)
  valores <- if (entero64) unclass(x) else as.double(x)
  if (!entero64 && any(!ausentes & valores == 0)) {
    valores[!ausentes & valores == 0] <- 0
  }
  bytes <- writeBin(valores, raw(), size = 8L, endian = "little")
  palabras <- .palabras32_huella(bytes, n)
  h1 <- .mezclar32_lsh(.xor32_lsh(palabras$lo, palabras$hi))
  h2 <- .mezclar32_lsh(.xor32_lsh(palabras$hi, palabras$lo))
  h1[ausentes & !nan] <- .mezclar32_lsh(.SEMILLA_HUELLA_NA_1)
  h2[ausentes & !nan] <- .mezclar32_lsh(.SEMILLA_HUELLA_NA_2)
  h1[nan] <- .mezclar32_lsh(.SEMILLA_HUELLA_NAN_1)
  h2[nan] <- .mezclar32_lsh(.SEMILLA_HUELLA_NAN_2)
  list(h1 = h1, h2 = h2)
}

.hash_logico_huella <- function(x) {
  codigos <- ifelse(is.na(x), 0, ifelse(x, 1, 2))
  h1 <- .mezclar32_lsh(.suma_mod32_huella(codigos, 0x51ed270b))
  h2 <- .mezclar32_lsh(.suma_mod32_huella(codigos, 0xa5a5a5a5))
  list(h1 = h1, h2 = h2)
}

.hash_columna_huella <- function(x) {
  if (is.list(x) || !is.null(dim(x)) || !is.atomic(x)) return(NULL)
  if (is.factor(x) || is.character(x)) return(.hash_texto_huella(x))
  if (inherits(x, "integer64")) {
    if (!.bit64_disponible()) return(NULL)
    return(.hash_numerico_huella(x, entero64 = TRUE))
  }
  if (inherits(x, "Date") || inherits(x, "POSIXct")) {
    return(.hash_numerico_huella(as.numeric(x)))
  }
  if (is.integer(x) || (is.double(x) && is.null(oldClass(x)))) {
    return(.hash_numerico_huella(x))
  }
  if (is.logical(x)) return(.hash_logico_huella(x))
  if (is.raw(x)) return(.hash_numerico_huella(as.integer(x)))
  NULL
}

.orden_prioridad_huella <- function(fila_1, fila_2, cantidad) {
  n <- length(fila_1)
  if (!n || n <= cantidad) {
    return(list(indices = seq_len(n), aparicion = seq_len(n)))
  }
  orden_contenido <- .orden_por_bytes(fila_1, fila_2, method = "radix")
  h1_ordenado <- fila_1[orden_contenido]
  h2_ordenado <- fila_2[orden_contenido]
  mismo <- h1_ordenado[-1L] == h1_ordenado[-n] &
    h2_ordenado[-1L] == h2_ordenado[-n]
  inicios <- which(c(TRUE, !mismo))
  tamanos <- diff(c(inicios, n + 1L))
  corrimiento <- rep(inicios - 1L, tamanos)
  aparicion_ordenada <- seq_len(n) - corrimiento
  aparicion <- integer(n)
  aparicion[orden_contenido] <- aparicion_ordenada

  prioridad_1 <- .mezclar32_lsh(.xor32_lsh(fila_1, aparicion))
  prioridad_2 <- .mezclar32_lsh(.xor32_lsh(
    fila_2, .por_mod32_lsh(aparicion, 0x9e37, 0x79b1)
  ))
  orden_prioridad <- .orden_por_bytes(
    prioridad_1, prioridad_2, fila_1, fila_2, aparicion, method = "radix"
  )
  list(
    indices = as.integer(orden_prioridad[seq_len(as.integer(cantidad))]),
    aparicion = aparicion
  )
}

.indices_huella_vector <- function(x, muestra) {
  n <- length(x)
  cantidad <- min(as.numeric(n), as.numeric(.validar_muestra(muestra)))
  if (!n || n <= cantidad) return(seq_len(n))
  valores <- .hash_columna_huella(x)
  fila_1 <- rep(.SEMILLA_HUELLA_FILA_1, n)
  fila_2 <- rep(.SEMILLA_HUELLA_FILA_2, n)
  if (!is.null(valores)) {
    fila_1 <- .suma_mod32_huella(
      fila_1, .mezclar32_lsh(valores$h1)
    )
    fila_2 <- .suma_mod32_huella(
      fila_2, .mezclar32_lsh(valores$h2)
    )
  }
  .orden_prioridad_huella(fila_1, fila_2, cantidad)$indices
}

.ordenar_nombres_huella <- function(x) {
  if (!length(x)) return(character())
  x[.orden_por_bytes(.clave_bytes(x), method = "radix")]
}

.indices_huella_tabla <- function(datos, muestra, columnas = NULL) {
  n <- nrow(datos)
  k <- min(as.numeric(n), as.numeric(.validar_muestra(muestra)))
  if (!n || n <= k) {
    return(list(
      indices = seq_len(n), metodo = NULL, k = k,
      columnas = character(), columnas_omitidas = character(),
      columnas_fuera = character()
    ))
  }
  nombres <- names(datos)
  if (is.null(nombres)) nombres <- paste0("V", seq_len(ncol(datos)))
  posiciones <- if (is.null(columnas)) {
    seq_len(ncol(datos))
  } else {
    match(columnas, nombres)
  }
  posiciones <- posiciones[!is.na(posiciones)]
  usados <- nombres[posiciones]
  omitidas <- character()
  fila_1 <- rep(.SEMILLA_HUELLA_FILA_1, n)
  fila_2 <- rep(.SEMILLA_HUELLA_FILA_2, n)
  for (posicion in posiciones) {
    valores <- .hash_columna_huella(datos[[posicion]])
    if (is.null(valores)) {
      omitidas <- c(omitidas, nombres[[posicion]])
      next
    }
    nombre <- .hash_texto_huella(nombres[[posicion]])
    aporte_1 <- .mezclar32_lsh(.xor32_lsh(nombre$h1[[1L]], valores$h1))
    aporte_2 <- .mezclar32_lsh(.xor32_lsh(nombre$h2[[1L]], valores$h2))
    fila_1 <- .suma_mod32_huella(fila_1, aporte_1)
    fila_2 <- .suma_mod32_huella(fila_2, aporte_2)
  }
  cantidad <- as.integer(k)
  seleccion <- .orden_prioridad_huella(fila_1, fila_2, cantidad)$indices
  columnas_ordenadas <- .ordenar_nombres_huella(usados[!usados %in% omitidas])
  omitidas <- .ordenar_nombres_huella(omitidas)
  list(
    indices = seleccion, metodo = .METODO_MUESTRA_HUELLA, k = k,
    columnas = columnas_ordenadas,
    columnas_omitidas = omitidas,
    columnas_fuera = .ordenar_nombres_huella(setdiff(nombres, usados))
  )
}

.muestra_con_indices <- function(k, indices, n_total = NULL, huella = NULL) {
  salida <- structure(
    as.numeric(k), indices_muestra = as.integer(indices),
    total_muestra = n_total
  )
  if (!is.null(huella)) attr(salida, "huella_muestra") <- huella
  salida
}

.indices_precalculados_muestra <- function(muestra, n) {
  indices <- attr(muestra, "indices_muestra", exact = TRUE)
  if (is.null(indices)) return(NULL)
  if (!is.numeric(indices) || anyNA(indices) || any(indices < 1L) ||
      anyDuplicated(indices)) {
    .detener("Los indices precalculados de la muestra no son validos.",
             call. = FALSE)
  }
  total_muestra <- attr(muestra, "total_muestra", exact = TRUE)
  if (!is.null(total_muestra) && !identical(as.numeric(total_muestra), as.numeric(n))) {
    return(NULL)
  }
  # Algunas etapas pasan a otra etapa la columna que ya recortaron. En ese
  # caso los indices pertenecen a la tabla original y no a ese vector corto;
  # dejar que la etapa interior conserve su entrada evita muestrear dos veces.
  if (any(indices > n)) return(NULL)
  as.integer(indices)
}
