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

# El recorrido de los bytes va por bloques de este tamano. El hash de un valor
# es una suma de terminos que dependen solo de la posicion y del byte, asi que
# partir el recorrido no cambia el resultado y acota la memoria: antes se
# armaban de una vez varios vectores de dobles del largo de TODOS los textos de
# la columna, y doscientas mil filas de dos mil caracteres pedian decenas de GB.
# Con medio MB el recorrido de esa tabla tarda lo mismo que con dos y su pico
# de memoria baja de 500 a 342 MB.
.BYTES_BLOQUE_HUELLA <- 524288
# Posiciones cuyo termino se precalcula en una tabla (256 por posicion y por
# semilla). Mas alla se calcula byte por byte; da lo mismo, cuesta mas.
.POSICIONES_TABLA_HUELLA <- 4096

.hash_bytes_huella_vectorizado <- function(valores, semilla_1, semilla_2,
                                           bloque = .BYTES_BLOQUE_HUELLA) {
  n <- length(valores)
  if (!n) return(list(h1 = numeric(), h2 = numeric()))
  longitudes <- nchar(valores, type = "bytes")
  h1 <- rep(.mezclar32_lsh(semilla_1), n)
  h2 <- rep(.mezclar32_lsh(semilla_2), n)
  con_bytes <- which(longitudes > 0L)
  if (!length(con_bytes)) return(list(h1 = h1, h2 = h2))
  bloque <- min(as.numeric(bloque), 4194304)

  valores_bytes <- valores[con_bytes]
  largos <- longitudes[con_bytes]
  modulo_1 <- .MODULO_TEXTO_HUELLA_1
  modulo_2 <- .MODULO_TEXTO_HUELLA_2
  # El termino de cada byte depende solo de su posicion (desde cero), de su
  # valor y de la semilla: la clave es posicion * 256 + byte. Asi el hash de un
  # valor no cambia con los otros valores de la columna. Al reducir a 32 bits,
  # posiciones que difieren en 2^24 coinciden para el mismo byte.
  posiciones_tabla <- min(max(largos), .POSICIONES_TABLA_HUELLA)
  largo_tabla <- posiciones_tabla * 256
  tabla_1 <- NULL
  tabla_2 <- NULL
  if (largo_tabla < sum(as.numeric(largos))) {
    claves_tabla <- seq_len(largo_tabla) - 1
    tabla_1 <- .mezclar32_lsh(.suma_mod32_huella(claves_tabla, semilla_1)) %% modulo_1
    tabla_2 <- .mezclar32_lsh(.suma_mod32_huella(claves_tabla, semilla_2)) %% modulo_2
  }
  # Cada tramo suma los terminos de sus bytes por valor y los acumula modulo
  # el primo. Un tramo tiene a lo sumo dos bloques de bytes y cada termino es
  # menor que 2^26: las sumas parciales no pasan de 2^49 y son exactas.
  suma_1 <- numeric(length(largos))
  suma_2 <- numeric(length(largos))
  directo <- function(claves, semilla, modulo) {
    .mezclar32_lsh(.suma_mod32_huella(as.numeric(claves), semilla)) %% modulo
  }
  acumular <- function(cuales, claves, fines, en_tabla) {
    if (is.null(tabla_1)) {
      parcial_1 <- directo(claves, semilla_1, modulo_1)
      parcial_2 <- directo(claves, semilla_2, modulo_2)
    } else {
      indice <- claves + 1L
      parcial_1 <- tabla_1[indice]
      parcial_2 <- tabla_2[indice]
      fuera <- if (en_tabla) integer() else which(claves >= largo_tabla)
      if (length(fuera)) {
        parcial_1[fuera] <- directo(claves[fuera], semilla_1, modulo_1)
        parcial_2[fuera] <- directo(claves[fuera], semilla_2, modulo_2)
      }
    }
    parcial_1 <- cumsum(parcial_1)[fines]
    parcial_2 <- cumsum(parcial_2)[fines]
    suma_1[cuales] <<- (suma_1[cuales] + diff(c(0, parcial_1))) %% modulo_1
    suma_2[cuales] <<- (suma_2[cuales] + diff(c(0, parcial_2))) %% modulo_2
  }

  # Los valores que caben en un bloque se recorren juntos, en tandas de un
  # bloque de bytes. Sus posiciones no llegan a 2^22 y la clave entra en un
  # entero.
  cortos <- which(largos <= bloque)
  if (length(cortos)) {
    largos_cortos <- as.numeric(largos[cortos])
    tanda <- floor((cumsum(largos_cortos) - largos_cortos) / bloque)
    fin_tanda <- cumsum(rle(tanda)$lengths)
    inicio_tanda <- c(1L, fin_tanda[-length(fin_tanda)] + 1L)
    for (t in seq_along(fin_tanda)) {
      cuales <- cortos[inicio_tanda[[t]]:fin_tanda[[t]]]
      largos_tanda <- largos[cuales]
      bytes <- charToRaw(paste(valores_bytes[cuales], collapse = ""))
      if (length(bytes) != sum(largos_tanda)) {
        bytes <- unlist(lapply(valores_bytes[cuales], charToRaw), use.names = FALSE)
      }
      claves <- sequence(largos_tanda, from = 0L) * 256L + as.integer(bytes)
      acumular(cuales, claves, cumsum(largos_tanda),
               max(largos_tanda) <= posiciones_tabla)
    }
  }
  # Un valor mas largo que un bloque se recorre solo, de a un bloque.
  for (cual in which(largos > bloque)) {
    bytes <- charToRaw(valores_bytes[[cual]])
    total <- length(bytes)
    for (desde in seq.int(0, total - 1, by = bloque)) {
      posiciones <- seq.int(desde, min(total, desde + bloque) - 1)
      claves <- (posiciones %% 16777216) * 256 + as.integer(bytes[posiciones + 1])
      acumular(cual, claves, length(posiciones), FALSE)
    }
  }

  base_1 <- (suma_1 + (largos %% modulo_1) * 65521 +
    (as.numeric(semilla_1) %% modulo_1)) %% modulo_1
  base_2 <- (suma_2 + (largos %% modulo_2) * 65519 +
    (as.numeric(semilla_2) %% modulo_2)) %% modulo_2
  h1[con_bytes] <- .mezclar32_lsh(base_1)
  h2[con_bytes] <- .mezclar32_lsh(base_2)
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

# El hash de cada valor distinto, y en `indice` a cual corresponde cada
# elemento: `h1[indice]` es el hash elemento por elemento. El ultimo lugar es
# el de los faltantes. Lo que se calcula despues sobre el hash de un valor
# (mezclarlo con el nombre de la columna) se calcula asi una vez por valor
# distinto y no una vez por fila.
.hash_texto_huella_unicos <- function(x) {
  canonicos <- .canonizar_texto_huella(x)
  claves <- .clave_bytes(canonicos)
  presentes <- which(!is.na(canonicos))
  indice <- rep(length(presentes) + 1L, length(canonicos))
  h1 <- numeric()
  h2 <- numeric()
  if (length(presentes)) {
    # Una sola pasada de `match()` da la primera aparicion de cada valor y,
    # con ella, a que valor unico corresponde cada fila.
    claves_presentes <- claves[presentes]
    primera <- match(claves_presentes, claves_presentes)
    es_primera <- primera == seq_along(primera)
    hashes <- .hash_bytes_huella_vectorizado(
      canonicos[presentes[es_primera]], 0x811c9dc5, 0x9e3779b9
    )
    h1 <- hashes$h1
    h2 <- hashes$h2
    indice[presentes] <- cumsum(es_primera)[primera]
    indice[-presentes] <- length(h1) + 1L
  }
  list(
    h1 = c(h1, .mezclar32_lsh(.SEMILLA_HUELLA_NA_1)),
    h2 = c(h2, .mezclar32_lsh(.SEMILLA_HUELLA_NA_2)),
    indice = indice
  )
}

.hash_texto_huella <- function(x) {
  unicos <- .hash_texto_huella_unicos(x)
  list(h1 = unicos$h1[unicos$indice], h2 = unicos$h2[unicos$indice])
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

# Como `.hash_columna_huella()`, por valor distinto. Los numeros se agrupan con
# `match()`, que junta 0 y -0 y los `NaN` entre si, igual que el hash; un
# `integer64` no, porque sus bits leidos como dobles juntarian valores
# distintos, y va elemento por elemento (`indice` en `NULL`).
.hash_columna_huella_unicos <- function(x) {
  if (is.list(x) || !is.null(dim(x)) || !is.atomic(x)) return(NULL)
  if (is.factor(x) || is.character(x)) return(.hash_texto_huella_unicos(x))
  numeros <- if (inherits(x, "integer64")) {
    NULL
  } else if (inherits(x, "Date") || inherits(x, "POSIXct")) {
    as.numeric(x)
  } else if (is.integer(x) || (is.double(x) && is.null(oldClass(x)))) {
    x
  } else if (is.raw(x)) {
    as.integer(x)
  } else {
    NULL
  }
  if (is.null(numeros)) {
    valores <- .hash_columna_huella(x)
    if (is.null(valores)) return(NULL)
    return(list(h1 = valores$h1, h2 = valores$h2, indice = NULL))
  }
  primera <- match(numeros, numeros)
  es_primera <- primera == seq_along(primera)
  valores <- .hash_numerico_huella(numeros[es_primera])
  list(h1 = valores$h1, h2 = valores$h2, indice = cumsum(es_primera)[primera])
}

# `desempate`, si viene, recibe las filas (en orden creciente) que comparten
# su hash con otra y devuelve dos vectores con los que ordenarlas entre si.
# Sin el, las filas del mismo hash se numeran por su posicion.
.orden_prioridad_huella <- function(fila_1, fila_2, cantidad, desempate = NULL) {
  n <- length(fila_1)
  if (!n || n <= cantidad) {
    return(list(indices = seq_len(n), aparicion = seq_len(n)))
  }
  orden_contenido <- .orden_por_bytes(fila_1, fila_2, method = "radix")
  h1_ordenado <- fila_1[orden_contenido]
  h2_ordenado <- fila_2[orden_contenido]
  mismo <- h1_ordenado[-1L] == h1_ordenado[-n] &
    h2_ordenado[-1L] == h2_ordenado[-n]
  if (!is.null(desempate) && any(mismo)) {
    repetidas <- sort(orden_contenido[c(mismo, FALSE) | c(FALSE, mismo)])
    claves <- desempate(repetidas)
    desempate_1 <- numeric(n)
    desempate_2 <- numeric(n)
    desempate_1[repetidas] <- claves[[1L]]
    desempate_2[repetidas] <- claves[[2L]]
    # Los grupos son los mismos; cambia solo el orden dentro de cada uno.
    orden_contenido <- .orden_por_bytes(
      fila_1, fila_2, desempate_1, desempate_2, method = "radix"
    )
  }
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
  valores <- .hash_columna_huella_unicos(x)
  fila_1 <- rep(.SEMILLA_HUELLA_FILA_1, n)
  fila_2 <- rep(.SEMILLA_HUELLA_FILA_2, n)
  if (!is.null(valores)) {
    mezcla_1 <- .mezclar32_lsh(valores$h1)
    mezcla_2 <- .mezclar32_lsh(valores$h2)
    if (!is.null(valores$indice)) {
      mezcla_1 <- mezcla_1[valores$indice]
      mezcla_2 <- mezcla_2[valores$indice]
    }
    fila_1 <- .suma_mod32_huella(fila_1, mezcla_1)
    fila_2 <- .suma_mod32_huella(fila_2, mezcla_2)
  }
  .orden_prioridad_huella(fila_1, fila_2, cantidad)$indices
}

.ordenar_nombres_huella <- function(x) {
  if (!length(x)) return(character())
  x[.orden_por_bytes(.clave_bytes(x), method = "radix")]
}

# El hash de cada fila: la suma, modulo 2^32, del aporte de cada columna (el
# hash de su valor mezclado con el de su nombre), que no depende del orden de
# las columnas. Con `filas`, solo esas filas, con el mismo valor que tendrian
# en la tabla entera. Las columnas que no se reducen a un valor por fila
# quedan en `omitidas`.
.hash_filas_huella <- function(datos, posiciones, nombres, filas = NULL) {
  n <- if (is.null(filas)) nrow(datos) else length(filas)
  omitidas <- character()
  fila_1 <- rep(.SEMILLA_HUELLA_FILA_1, n)
  fila_2 <- rep(.SEMILLA_HUELLA_FILA_2, n)
  for (posicion in posiciones) {
    x <- datos[[posicion]]
    if (!is.null(filas) && is.atomic(x) && is.null(dim(x))) x <- x[filas]
    valores <- .hash_columna_huella_unicos(x)
    if (is.null(valores)) {
      omitidas <- c(omitidas, nombres[[posicion]])
      next
    }
    nombre <- .hash_texto_huella(nombres[[posicion]])
    aporte_1 <- .mezclar32_lsh(.xor32_lsh(nombre$h1[[1L]], valores$h1))
    aporte_2 <- .mezclar32_lsh(.xor32_lsh(nombre$h2[[1L]], valores$h2))
    if (!is.null(valores$indice)) {
      aporte_1 <- aporte_1[valores$indice]
      aporte_2 <- aporte_2[valores$indice]
    }
    fila_1 <- .suma_mod32_huella(fila_1, aporte_1)
    fila_2 <- .suma_mod32_huella(fila_2, aporte_2)
  }
  list(h1 = fila_1, h2 = fila_2, omitidas = omitidas)
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
  filas <- .hash_filas_huella(datos, posiciones, nombres)
  omitidas <- filas$omitidas
  # Con `clave`, las filas que la repiten tienen el mismo hash y quedaban
  # ordenadas por su posicion: invertir las filas cambiaba cuales entraban.
  # Entre ellas decide el hash de la fila entera; las que tambien son iguales
  # en todo lo demas son intercambiables.
  desempate <- if (length(posiciones) < ncol(datos)) {
    function(cuales) {
      completas <- .hash_filas_huella(
        datos, seq_len(ncol(datos)), nombres, filas = cuales
      )
      list(completas$h1, completas$h2)
    }
  }
  cantidad <- as.integer(k)
  seleccion <- .orden_prioridad_huella(
    filas$h1, filas$h2, cantidad, desempate
  )$indices
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
