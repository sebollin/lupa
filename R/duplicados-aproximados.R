.validar_limite_duplicados <- function(x, nombre) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 ||
      (!is.infinite(x) && x != floor(x))) {
    stop("`", nombre, "` debe ser un entero positivo o Inf.", call. = FALSE)
  }
  if (is.infinite(x)) Inf else as.integer(x)
}

.stringdist_disponible <- function() {
  requireNamespace("stringdist", quietly = TRUE)
}

.vacio_duplicados_aproximados <- function(
    n_filas, columnas, metodo, umbral, muestra, max_pares, max_resultados,
    disponible = TRUE, razon = "", bloque = 1000L, n_bloques = 0L,
    modo_comparacion = "sin_comparacion") {
  pares <- data.frame(
    fila_1 = integer(), fila_2 = integer(), distancia = numeric(),
    tipo_par = character(), metodo = character(), umbral = numeric(),
    evidencia_1 = character(),
    evidencia_2 = character(), proteccion_evidencia = character(),
    stringsAsFactors = FALSE
  )
  posibles <- as.numeric(n_filas) * (as.numeric(n_filas) - 1) / 2
  alcance <- data.frame(
    n_filas_total = as.numeric(n_filas),
    n_filas_muestra = 0,
    n_filas_validas = 0,
    n_pares_posibles = posibles,
    n_pares_comparados = 0,
    n_pares_sin_comparar = posibles,
    n_pares_hallados = 0,
    n_pares_exactos = 0,
    n_pares_aproximados = 0,
    n_pares_mostrados = 0,
    limite_pares = max_pares,
    limite_pares_configurado = max_pares,
    limite_pares_aplica = FALSE,
    limite_resultados = max_resultados,
    muestra = muestra,
    muestra_efectiva = 0,
    estrategia = if (disponible) "sin_pares_comparables" else "no_disponible",
    modo_comparacion = modo_comparacion,
    tamano_bloque = bloque,
    n_bloques = n_bloques,
    comparacion_exhaustiva = FALSE,
    muestreado = FALSE,
    truncado = FALSE,
    disponible = disponible,
    razon = razon,
    stringsAsFactors = FALSE
  )
  hallazgos <- data.frame(
    columna = character(), tipo_hallazgo = character(), severidad = character(),
    descripcion = character(), evidencia = character(), sugerencia = character(),
    stringsAsFactors = FALSE
  )
  estructura <- list(
    pares = pares, hallazgos = hallazgos,
    alcance = alcance, columnas = columnas, metodo = metodo,
    umbral = umbral, disponible = disponible, razon = razon,
    proteccion_aplicada = FALSE
  )
  class(estructura) <- c("duplicados_aproximados", "list")
  estructura
}

.validar_bloque_duplicados <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x < 1 || x != floor(x)) {
    stop("`bloque` debe ser un entero positivo y finito.", call. = FALSE)
  }
  as.integer(x)
}

.validar_parametro_lsh <- function(x, nombre, minimo = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x < minimo || x != floor(x)) {
    stop("`", nombre, "` debe ser un entero finito de al menos ", minimo,
         ".", call. = FALSE)
  }
  as.integer(x)
}

.nuevo_acumulador_duplicados <- function(max_resultados) {
  list(
    pares = data.frame(
      fila_1 = integer(), fila_2 = integer(), distancia = numeric(),
      stringsAsFactors = FALSE
    ),
    n_hallados = 0, n_exactos = 0, n_aproximados = 0,
    max_resultados = max_resultados, lotes = list()
  )
}

.pares_acumulador_duplicados <- function(acumulador) {
  if (!is.infinite(acumulador$max_resultados) ||
      !length(acumulador$lotes)) return(acumulador$pares)
  lotes <- c(list(acumulador$pares), acumulador$lotes)
  lotes <- lotes[vapply(lotes, nrow, integer(1L)) > 0L]
  if (!length(lotes)) return(acumulador$pares)
  data.frame(
    fila_1 = as.integer(unlist(lapply(lotes, `[[`, "fila_1"),
                         use.names = FALSE)),
    fila_2 = as.integer(unlist(lapply(lotes, `[[`, "fila_2"),
                         use.names = FALSE)),
    distancia = as.numeric(unlist(lapply(lotes, `[[`, "distancia"),
                              use.names = FALSE)),
    stringsAsFactors = FALSE
  )
}

#' Acumula un lote de pares candidatos ya filtrados
#'
#' El contrato del generador es entregar cada par una sola vez. El acumulador
#' no guarda un índice de todos los pares vistos: conserva sólo los mejores
#' `max_resultados` y sus contadores. Esto permite alimentar la función con
#' lotes provenientes de teselas o de un generador futuro sin que el estado
#' persistente crezca con el número de candidatos.
#' @noRd
.acumular_pares_duplicados <- function(
    acumulador, fila_1, fila_2, distancia) {
  if (!length(fila_1)) return(acumulador)
  if (length(fila_1) != length(fila_2) ||
      length(fila_1) != length(distancia)) {
    stop("El lote de pares debe tener filas y distancias de igual longitud.",
         call. = FALSE)
  }
  validos <- is.finite(distancia)
  if (!any(validos)) return(acumulador)
  lote <- data.frame(
    fila_1 = as.integer(fila_1[validos]),
    fila_2 = as.integer(fila_2[validos]),
    distancia = as.numeric(distancia[validos]),
    stringsAsFactors = FALSE
  )
  acumulador$n_hallados <- acumulador$n_hallados + nrow(lote)
  acumulador$n_exactos <- acumulador$n_exactos + sum(lote$distancia == 0)
  acumulador$n_aproximados <- acumulador$n_aproximados +
    sum(lote$distancia > 0)
  # Con `Inf` se conserva todo y ordenar cada lote vuelve cuadratica la
  # acumulacion. El orden canonico se establece una sola vez al cerrar el
  # generador; con un limite finito se mantiene el recorte incremental.
  if (is.infinite(acumulador$max_resultados)) {
    acumulador$lotes[[length(acumulador$lotes) + 1L]] <- lote
    return(acumulador)
  }
  acumulador$pares <- rbind(acumulador$pares, lote)
  acumulador$pares <- acumulador$pares[
    order(acumulador$pares$distancia,
          acumulador$pares$fila_1, acumulador$pares$fila_2),
    , drop = FALSE
  ]
  limite <- acumulador$max_resultados
  if (!is.infinite(limite) && nrow(acumulador$pares) > limite) {
    acumulador$pares <- acumulador$pares[seq_len(limite), , drop = FALSE]
  }
  rownames(acumulador$pares) <- NULL
  acumulador
}

#' Compara por teselas y conserva sólo los mejores resultados
#'
#' La matriz de distancias de cada tesela se descarta antes de pasar a la
#' siguiente. Así la memoria temporal depende de `bloque`, no de la cantidad
#' total de filas, sin cambiar qué pares se comparan.
#' @noRd
.comparar_bloques_duplicados <- function(
    valores, filas, metodo, umbral, bloque, max_resultados,
    acumulador = NULL, on_pairs = NULL) {
  n <- length(valores)
  acumular_en_externo <- !is.null(acumulador)
  if (is.null(acumulador)) {
    acumulador <- .nuevo_acumulador_duplicados(max_resultados)
  }
  if (n < 2L) {
    return(list(
      pares = if (acumular_en_externo) acumulador$pares else
        .pares_acumulador_duplicados(acumulador),
      n_hallados = acumulador$n_hallados,
      n_exactos = acumulador$n_exactos,
      n_aproximados = acumulador$n_aproximados,
      n_bloques = 0L, acumulador = acumulador
    ))
  }
  inicios <- seq.int(1L, n, by = bloque)
  n_bloques <- 0L
  for (i in seq_along(inicios)) {
    fin_i <- min(n, inicios[[i]] + bloque - 1L)
    filas_i <- inicios[[i]]:fin_i
    for (j in i:length(inicios)) {
      fin_j <- min(n, inicios[[j]] + bloque - 1L)
      filas_j <- inicios[[j]]:fin_j
      matriz <- as.matrix(stringdist::stringdistmatrix(
        valores[filas_i], valores[filas_j], method = metodo
      ))
      n_bloques <- n_bloques + 1L
      candidatas <- if (i == j) {
        which(upper.tri(matriz) & matriz >= 0 & matriz <= umbral,
              arr.ind = TRUE)
      } else {
        which(matriz >= 0 & matriz <= umbral, arr.ind = TRUE)
      }
      if (!nrow(candidatas)) next
      distancias <- as.numeric(matriz[candidatas])
      f1 <- filas[filas_i[candidatas[, 1L]]]
      f2 <- filas[filas_j[candidatas[, 2L]]]
      if (!is.null(on_pairs)) on_pairs(f1, f2, distancias)
      acumulador <- .acumular_pares_duplicados(
        acumulador, f1, f2, distancias
      )
    }
  }
  pares_acumulados <- if (acumular_en_externo) acumulador$pares else
    .pares_acumulador_duplicados(acumulador)
  pares_acumulados <- pares_acumulados[
    order(pares_acumulados$distancia,
          pares_acumulados$fila_1, pares_acumulados$fila_2),
    , drop = FALSE
  ]
  list(
    pares = pares_acumulados,
    n_hallados = acumulador$n_hallados,
    n_exactos = acumulador$n_exactos,
    n_aproximados = acumulador$n_aproximados, n_bloques = n_bloques,
    acumulador = acumulador
  )
}

.qgramas_lsh <- function(x, q) {
  lapply(seq_along(x), function(i) {
    texto <- x[[i]]
    if (!nzchar(texto)) return(character())
    largo <- nchar(texto, type = "chars")
    if (largo < q) return(texto)
    unique(substring(texto, seq_len(largo - q + 1L),
                     seq_len(largo - q + 1L) + q - 1L))
  })
}

.matriz_ids_qgramas <- function(gramas, vocabulario) {
  n <- length(gramas)
  largos <- lengths(gramas)
  maximo <- if (n) max(1L, largos) else 1L
  ids <- matrix(0L, nrow = n, ncol = maximo)
  if (n && sum(largos)) {
    filas <- rep(seq_len(n), largos)
    columnas <- sequence(largos)
    ids[cbind(filas, columnas)] <- match(unlist(gramas, use.names = FALSE),
                                         vocabulario)
  }
  ids
}

.ids_qgramas_por_bloques <- function(valores, q, bloque = 10000L) {
  n <- length(valores)
  largos_maximos <- pmax(nchar(valores, type = "chars") - q + 1L, 1L)
  maximo <- if (n) max(largos_maximos) else 1L
  ids <- matrix(0L, nrow = n, ncol = maximo)
  vocabulario <- character()
  if (!n) return(list(ids = ids, vocabulario = vocabulario))
  inicios <- seq.int(1L, n, by = bloque)
  for (inicio in inicios) {
    fin <- min(n, inicio + bloque - 1L)
    gramas <- .qgramas_lsh(valores[inicio:fin], q)
    nuevos <- unique(unlist(gramas, use.names = FALSE))
    if (length(nuevos)) {
      nuevos <- nuevos[!nuevos %in% vocabulario]
      if (length(nuevos)) vocabulario <- c(vocabulario, nuevos)
    }
    bloque_ids <- .matriz_ids_qgramas(gramas, vocabulario)
    ids[inicio:fin, seq_len(ncol(bloque_ids))] <- bloque_ids
  }
  list(ids = ids, vocabulario = vocabulario)
}

.firmas_minhash_lsh <- function(ids, n_hashes) {
  n <- nrow(ids)
  vocabulario <- max(ids, na.rm = TRUE)
  if (!is.finite(vocabulario) || vocabulario < 1L) vocabulario <- 1L
  # Los coeficientes no se generan como una sucesion afin del indice del hash:
  # las bandas necesitan una familia suficientemente desacoplada para que la
  # probabilidad teorica 1 - (1 - s^r)^b sea una aproximacion alcanzable.
  # `semilla` es interna y fija; se restaura el estado de RNG del llamador para
  # que el resultado no dependa de `set.seed()` ni lo modifique.
  primo <- 1000000007
  semilla <- 1L
  habia_semilla <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (habia_semilla) semilla_anterior <- get(".Random.seed", .GlobalEnv)
  set.seed(semilla)
  on.exit({
    if (habia_semilla) {
      assign(".Random.seed", semilla_anterior, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  # `a` queda acotado para que `a * id` sea exacto en double incluso con
  # vocabularios grandes; `b` recorre el primo completo. El muestreo interno
  # produce coeficientes no afines entre hashes y es reproducible.
  a <- as.numeric(sample.int(1000003L, n_hashes))
  b <- as.numeric(sample.int(primo, n_hashes)) - 1
  firmas <- matrix(Inf, nrow = n, ncol = n_hashes)
  for (h in seq_len(n_hashes)) {
    # El indice 0 es padding y el id k usa exactamente la permutacion k.
    # Antes un centinela antepuesto desplazaba todos los ids una posicion.
    hash <- c(NA_real_,
              (a[[h]] * as.numeric(seq_len(vocabulario)) + b[[h]]) %% primo)
    minimo <- rep(Inf, n)
    for (j in seq_len(ncol(ids))) {
        ids_col <- ids[, j]
        valores <- hash[ids_col + 1L]
        valores[ids_col == 0L] <- Inf
        minimo <- pmin(minimo, valores)
    }
    firmas[, h] <- minimo
  }
  firmas
}

.jaccard_qgramas <- function(a, b) {
  if (!length(a) && !length(b)) return(1)
  union <- union(a, b)
  length(intersect(a, b)) / length(union)
}

.garantia_lsh <- function(s, bandas, filas_banda, pares_descartados = 0) {
  if (isTRUE(pares_descartados > 0)) return(NA_real_)
  1 - (1 - s^filas_banda)^bandas
}

.estado_garantia_lsh <- function(pares_descartados) {
  if (isTRUE(pares_descartados > 0)) {
    "no_valida_hay_cubetas_descartadas"
  } else {
    "valida_generacion_lsh_sin_cubetas_descartadas"
  }
}

#' Genera candidatos con MinHash y bandas LSH
#'
#' Cada candidato se emite en la primera banda donde colisiona. El generador
#' entrega pares únicos al acumulador; así no se guarda un registro de todos
#' los pares ya vistos. La garantía calculada es la de candidatos con Jaccard
#' de q-gramas, no una garantía de la medida final.
#' @noRd
.comparar_lsh_duplicados <- function(
    valores, filas, metodo, umbral, bandas, filas_banda, q,
    max_cubeta, max_resultados) {
  n <- length(valores)
  n_hashes <- bandas * filas_banda
  indice <- .ids_qgramas_por_bloques(valores, q)
  ids <- indice$ids
  vocabulario <- indice$vocabulario
  firmas <- .firmas_minhash_lsh(ids, n_hashes)
  # Las listas de q-gramas y la matriz de ids sólo son necesarias para
  # construir la firma. Liberarlas antes del recorrido evita que la memoria
  # del índice se sume a la de las cubetas; el Jaccard de los pocos pares que
  # pasan el umbral se recalcula bajo demanda.
  ids <- NULL
  vocabulario <- NULL
  acumulador <- .nuevo_acumulador_duplicados(max_resultados)
  candidatos_generados <- 0
  candidatos_unicos <- 0
  candidatos_descartados_bandas <- 0
  pares_comparados <- 0
  cubetas_grandes <- 0L
  pares_descartados_cubetas <- 0
  pares_cubetas_troceadas <- 0
  teselas_cubetas_grandes <- 0L
  lotes_cubetas_grandes <- 0L
  jaccard <- numeric()
  n_jaccard <- 0
  n_jaccard_pares <- 0
  n_jaccard_bajo <- 0
  muestra_jaccard <- 10000L
  claves_previas <- list()
  posiciones <- integer(max(filas))
  posiciones[filas] <- seq_along(filas)
  cache_gramas <- new.env(hash = TRUE, parent = emptyenv())
  obtener_gramas <- function(indice) {
    texto <- valores[[indice]]
    clave <- paste0("v:", texto)
    if (exists(clave, envir = cache_gramas, inherits = FALSE)) {
      return(get(clave, envir = cache_gramas, inherits = FALSE))
    }
    gramas <- .qgramas_lsh(texto, q)[[1L]]
    # Las cubetas densas suelen repetir exactamente el mismo valor. Cachear
    # hasta diez mil valores evita recalcular sus q-gramas sin retener toda la
    # columna en memoria en un padrón de alta cardinalidad.
    if (length(cache_gramas) < 10000L) {
      assign(clave, gramas, envir = cache_gramas)
    }
    gramas
  }
  registrar_jaccard <- function(indices_1, indices_2) {
    if (!length(indices_1)) return(invisible(NULL))
    n_jaccard_pares <<- n_jaccard_pares + length(indices_1)
    disponibles <- muestra_jaccard - n_jaccard
    if (disponibles <= 0) return(invisible(NULL))
    indices <- seq_len(min(length(indices_1), disponibles))
    for (indice in indices) {
      valor_j <- .jaccard_qgramas(
        obtener_gramas(indices_1[[indice]]),
        obtener_gramas(indices_2[[indice]])
      )
      n_jaccard <<- n_jaccard + 1L
      n_jaccard_bajo <<- n_jaccard_bajo + (valor_j < 0.7)
      if (length(jaccard) < muestra_jaccard) {
        jaccard <<- c(jaccard, valor_j)
      }
    }
    invisible(NULL)
  }
  registrar_jaccard_filas <- function(filas_1, filas_2, distancias) {
    # El comparador por teselas entrega indices de fila originales; la tabla
    # inversa evita un `match()` por par en una cubeta densa.
    registrar_jaccard(posiciones[filas_1], posiciones[filas_2])
  }
  for (banda in seq_len(bandas)) {
    columnas <- ((banda - 1L) * filas_banda + 1L):(banda * filas_banda)
    claves <- do.call(paste, c(
      lapply(columnas, function(j) firmas[, j]), sep = ":"
    ))
    orden <- order(claves)
    corridas <- rle(claves[orden])
    finales <- cumsum(corridas$lengths)
    inicios <- c(1L, utils::head(finales, -1L) + 1L)
    for (grupo in seq_along(corridas$lengths)) {
      indices <- orden[inicios[[grupo]]:finales[[grupo]]]
      tamano <- length(indices)
      if (tamano < 2L) next
      posibles_grupo <- as.numeric(tamano) * (tamano - 1) / 2
      if (tamano > max_cubeta) {
        cubetas_grandes <- cubetas_grandes + 1L
      }
      # En la primera banda no hay pares previos que deduplicar. Para una
      # cubeta grande usamos el comparador por teselas del camino exhaustivo:
      # compara todos sus pares, pero descarta cada matriz antes de continuar.
      if (tamano > max_cubeta && banda == 1L) {
        pares_cubetas_troceadas <- pares_cubetas_troceadas + posibles_grupo
        por_teselas <- .comparar_bloques_duplicados(
          valores[indices], filas[indices], metodo, umbral,
          bloque = min(2000L, tamano), max_resultados = max_resultados,
          acumulador = acumulador, on_pairs = registrar_jaccard_filas
        )
        acumulador <- por_teselas$acumulador
        teselas_cubetas_grandes <- teselas_cubetas_grandes +
          por_teselas$n_bloques
        candidatos_generados <- candidatos_generados + posibles_grupo
        candidatos_unicos <- candidatos_unicos + posibles_grupo
        pares_comparados <- pares_comparados + posibles_grupo
        next
      }
      # Si toda la cubeta tambien colisiono dentro de una banda anterior,
      # cada par ya fue considerado y se puede omitir el bloque completo sin
      # construir sus combinaciones. La comprobacion es exacta: solo se omite
      # cuando una clave previa es constante para todas sus filas.
      if (banda > 1L && any(vapply(
        claves_previas[seq_len(banda - 1L)],
        function(clave) length(unique(clave[indices])) == 1L,
        logical(1L)
      ))) {
        candidatos_generados <- candidatos_generados + posibles_grupo
        candidatos_descartados_bandas <- candidatos_descartados_bandas +
          posibles_grupo
        next
      }
      if (tamano > max_cubeta) {
        pares_cubetas_troceadas <- pares_cubetas_troceadas + posibles_grupo
        lotes_cubetas_grandes <- lotes_cubetas_grandes +
          ceiling((tamano - 1) / 100)
      }
      # Las cubetas grandes no se descartan: se procesan con el mismo recorrido
      # por lotes de filas que una cubeta normal. Es equivalente a las teselas
      # del camino exhaustivo, conserva la deduplicacion por primera banda y
      # mantiene acotada la memoria temporal.
      for (inicio_izquierda in seq.int(1L, tamano - 1L, by = 100L)) {
        izquierda <- inicio_izquierda:min(inicio_izquierda + 99L, tamano - 1L)
        derechas <- lapply(izquierda, function(i) (i + 1L):tamano)
        p1 <- rep(indices[izquierda], times = lengths(derechas))
        p2 <- unlist(lapply(derechas, function(i) indices[i]), use.names = FALSE)
        candidatos_generados <- candidatos_generados + length(p1)
        ya <- rep(FALSE, length(p1))
        if (banda > 1L) {
          for (previa in seq_len(banda - 1L)) {
            iguales <- claves_previas[[previa]][p1] ==
              claves_previas[[previa]][p2]
            ya <- ya | iguales
          }
        }
        keep <- !ya
        candidatos_unicos <- candidatos_unicos + sum(keep)
        candidatos_descartados_bandas <- candidatos_descartados_bandas +
          sum(ya)
        if (!any(keep)) next
        p1 <- p1[keep]
        p2 <- p2[keep]
        distancias <- stringdist::stringdist(
          valores[p1], valores[p2], method = metodo
        )
        pares_comparados <- pares_comparados + length(p1)
        pasan <- is.finite(distancias) & distancias <= umbral
        if (any(pasan)) {
          indices_pasan <- which(pasan)
          registrar_jaccard(p1[indices_pasan], p2[indices_pasan])
          filas_a <- filas[p1[indices_pasan]]
          filas_b <- filas[p2[indices_pasan]]
          intercambiar <- filas_a > filas_b
          if (any(intercambiar)) {
            temporal <- filas_a[intercambiar]
            filas_a[intercambiar] <- filas_b[intercambiar]
            filas_b[intercambiar] <- temporal
          }
          acumulador <- .acumular_pares_duplicados(
            acumulador, filas_a, filas_b, distancias[indices_pasan]
          )
        }
      }
    }
    claves_previas[[banda]] <- claves
  }
  garantia_07 <- .garantia_lsh(
    0.7, bandas, filas_banda, pares_descartados_cubetas
  )
  resumen_jaccard <- if (length(jaccard)) {
    quant <- stats::quantile(jaccard, probs = c(0, .25, .5, .75, 1),
                             names = FALSE, na.rm = TRUE)
    quant
  } else rep(NA_real_, 5L)
  acumulador$pares <- .pares_acumulador_duplicados(acumulador)
  acumulador$pares <- acumulador$pares[
    order(acumulador$pares$distancia,
          acumulador$pares$fila_1, acumulador$pares$fila_2),
    , drop = FALSE
  ]
  acumulador$lotes <- list()
  rownames(acumulador$pares) <- NULL
  list(
    pares = acumulador$pares,
    n_hallados = acumulador$n_hallados,
    n_exactos = acumulador$n_exactos,
    n_aproximados = acumulador$n_aproximados,
    n_bloques = 0L,
    alcance = data.frame(
      lsh_bandas = bandas, lsh_filas = filas_banda,
      lsh_tamano_firma = n_hashes, lsh_q = q,
      lsh_semilla_hash = 1L,
      lsh_hash_familia = "coeficientes_deterministas_no_afines",
      lsh_max_cubeta = max_cubeta,
      lsh_garantia_jaccard_09 = .garantia_lsh(
        0.9, bandas, filas_banda, pares_descartados_cubetas
      ),
      lsh_garantia_jaccard_08 = .garantia_lsh(
        0.8, bandas, filas_banda, pares_descartados_cubetas
      ),
      lsh_garantia_jaccard_07 = garantia_07,
      lsh_garantia_aplica_a = "Jaccard de q-gramas; no garantiza la medida final",
      lsh_garantia_estado = .estado_garantia_lsh(pares_descartados_cubetas),
      lsh_cubetas_grandes = cubetas_grandes,
      lsh_pares_descartados_cubetas = pares_descartados_cubetas,
      lsh_pares_cubetas_troceadas = pares_cubetas_troceadas,
      lsh_teselas_cubetas_grandes = teselas_cubetas_grandes,
      lsh_lotes_cubetas_grandes = lotes_cubetas_grandes,
      lsh_candidatos_generados = candidatos_generados,
      lsh_candidatos_unicos = candidatos_unicos,
      lsh_candidatos_descartados_bandas = candidatos_descartados_bandas,
      lsh_pares_comparados = pares_comparados,
      lsh_jaccard_evaluados = n_jaccard,
      lsh_jaccard_pares_elegibles = n_jaccard_pares,
      lsh_jaccard_alcance = if (n_jaccard_pares > n_jaccard) {
        paste0("muestra_determinista_de_", n_jaccard,
               "_pares_de_", n_jaccard_pares,
               "; incluye_teselas_y_lotes")
      } else "todos_los_pares_que_pasaron_el_umbral",
      lsh_jaccard_bajo_07 = n_jaccard_bajo,
      lsh_prop_jaccard_bajo_07 = if (n_jaccard) n_jaccard_bajo / n_jaccard else NA_real_,
      lsh_jaccard_minimo = resumen_jaccard[[1L]],
      lsh_jaccard_q1 = resumen_jaccard[[2L]],
      lsh_jaccard_mediana = resumen_jaccard[[3L]],
      lsh_jaccard_q3 = resumen_jaccard[[4L]],
      lsh_jaccard_maximo = resumen_jaccard[[5L]],
      lsh_jaccard_muestra = length(jaccard),
      stringsAsFactors = FALSE
    )
  )
}

.indices_duplicados_aproximados <- function(n, limite) {
  if (!n) return(integer())
  if (is.infinite(limite) || n <= limite) return(seq_len(n))
  limite <- as.integer(limite)
  if (limite <= 3L) return(seq_len(limite))
  # Mantener ambos extremos evita que una tabla ordenada pierda un par
  # duplicado que aparezca al principio o al final; el resto queda distribuido.
  interiores <- limite - 4L
  indices <- if (interiores) {
    c(1L, 2L, n - 1L, n,
      as.integer(round(seq.int(3L, n - 2L, length.out = interiores))))
  } else c(1L, 2L, n - 1L, n)
  sort(unique(indices))
}

.columnas_duplicados_aproximados <- function(datos, columnas) {
  if (is.null(columnas)) {
    analizables <- vapply(datos, function(x) {
      (is.character(x) || is.factor(x)) &&
        !is.matrix(x) && !is.list(x)
    }, logical(1L))
    candidatas <- names(datos)[analizables]
    if (length(candidatas) > 2L) {
      nombres <- tolower(gsub("[^[:alnum:]]+", "_", candidatas, perl = TRUE))
      es_identificador <- grepl(
        "(^|_)(id|identificador|codigo|code|uuid|clave|key|llave|nro|numero)(_|$)",
        nombres, perl = TRUE
      )
      candidatas <- candidatas[!es_identificador]
    }
    if (length(candidatas) > 2L) {
      candidatas <- structure(
        character(),
        motivo_sin_columnas = paste0(
          "Hay ", length(candidatas), " columnas de texto; indique `columnas` ",
          "para evitar combinar campos que pueden diluir la similitud."
        )
      )
    }
    return(candidatas)
  }
  if (!is.character(columnas) || !length(columnas) ||
      anyNA(columnas) || any(!nzchar(columnas)) || anyDuplicated(columnas) ||
      any(!columnas %in% names(datos))) {
    stop("`columnas` debe nombrar columnas atomicas existentes y sin repetir.",
         call. = FALSE)
  }
  if (any(vapply(datos[columnas], function(x) is.matrix(x) || is.list(x),
                 logical(1L)))) {
    stop("Las columnas aproximadas no pueden ser matrices ni listas.",
         call. = FALSE)
  }
  columnas
}

.texto_fila_aproximada <- function(datos, columnas, normalizar) {
  valores <- lapply(datos[columnas], function(x) {
    # Reutilizar el saneamiento del perfil: los bytes UTF-8 invalidos no
    # deben abortar una comparacion ni entrar como evidencia.
    salida <- suppressWarnings(as.character(.texto_analizable(x)$valores))
    salida[is.na(salida)] <- ""
    if (normalizar) {
      salida <- trimws(tolower(salida))
      salida <- gsub("[[:space:]]+", " ", salida, perl = TRUE)
    }
    salida
  })
  filas <- do.call(paste, c(valores, sep = " | "))
  presentes <- vapply(seq_along(filas), function(i) {
    any(vapply(valores, function(x) nzchar(x[[i]]), logical(1L)))
  }, logical(1L))
  list(valores = filas, presentes = presentes)
}

.evidencia_fila_aproximada <- function(datos, columnas, fila, protegidas) {
  valores <- vapply(columnas, function(columna) {
    if (columna %in% protegidas) return("[valor protegido]")
    valor <- suppressWarnings(as.character(datos[[columna]][[fila]]))
    if (!length(valor) || is.na(valor)) "[ausente]" else valor
  }, character(1L))
  paste0(columnas, "=", valores, collapse = "; ")
}

.detectar_duplicados_aproximados <- function(
    datos, columnas = NULL, metodo = "jw", umbral = 0.12,
    muestra = Inf, max_pares = 50000000L, max_resultados = 100L,
    normalizar = TRUE, clasificacion = NULL,
    proteger_datos_personales = TRUE, bloque = 1000L,
    estrategia = "auto", lsh_bandas = 12L, lsh_filas = 3L, lsh_q = 3L,
    lsh_max_cubeta = 1000L) {
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe heredar de data.frame.", call. = FALSE)
  }
  columnas <- .columnas_duplicados_aproximados(datos, columnas)
  muestra <- .validar_limite_duplicados(muestra, "muestra")
  max_pares <- .validar_limite_duplicados(max_pares, "max_pares")
  max_resultados <- .validar_limite_duplicados(max_resultados, "max_resultados")
  bloque <- .validar_bloque_duplicados(bloque)
  estrategia <- match.arg(estrategia, c("auto", "teselas", "muestra", "lsh"))
  lsh_bandas <- .validar_parametro_lsh(lsh_bandas, "lsh_bandas")
  lsh_filas <- .validar_parametro_lsh(lsh_filas, "lsh_filas")
  lsh_q <- .validar_parametro_lsh(lsh_q, "lsh_q")
  lsh_max_cubeta <- .validar_parametro_lsh(
    lsh_max_cubeta, "lsh_max_cubeta", minimo = 2L
  )
  if (!is.character(metodo) || length(metodo) != 1L || is.na(metodo) ||
      !nzchar(metodo) || !metodo %in% c(
        "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine",
        "jaccard", "jw", "soundex"
      )) {
    stop(
      "`metodo` debe ser una medida admitida: osa, lv, dl, hamming, lcs, ",
      "qgram, cosine, jaccard, jw o soundex.", call. = FALSE
    )
  }
  if (!is.numeric(umbral) || length(umbral) != 1L || is.na(umbral) ||
      !is.finite(umbral) || umbral < 0) {
    stop("`umbral` debe ser un numero finito no negativo.", call. = FALSE)
  }
  if (!is.logical(normalizar) || length(normalizar) != 1L || is.na(normalizar) ||
      !is.logical(proteger_datos_personales) ||
      length(proteger_datos_personales) != 1L ||
      is.na(proteger_datos_personales)) {
    stop("`normalizar` y `proteger_datos_personales` deben ser l\u00f3gicos escalares.",
         call. = FALSE)
  }
  if (!.stringdist_disponible()) {
    return(.vacio_duplicados_aproximados(
      nrow(datos), columnas, metodo, umbral, muestra, max_pares,
      max_resultados, disponible = FALSE, bloque = bloque,
      razon = "No esta instalado el paquete opcional 'stringdist'."
    ))
  }
  if (!length(columnas)) {
    motivo <- attr(columnas, "motivo_sin_columnas", exact = TRUE)
    if (is.null(motivo)) {
      motivo <- "No hay columnas de texto comparables; indique `columnas` explicitamente."
    }
    return(.vacio_duplicados_aproximados(
      nrow(datos), columnas, metodo, umbral, muestra, max_pares,
      max_resultados, disponible = TRUE, bloque = bloque,
      razon = motivo
    ))
  }
  if (is.null(clasificacion)) {
    perfil <- perfilar(
      datos, analizar_dependencias = FALSE,
      proteger_datos_personales = TRUE,
      duplicados_aproximados = FALSE
    )
    clasificacion <- perfil$datos_personales
  }
  protegidas <- if (proteger_datos_personales) {
    .columnas_personales_protegidas(clasificacion)
  } else character()
  textos <- .texto_fila_aproximada(datos, columnas, normalizar)
  # Sobre el tope exhaustivo, `auto` reemplaza el muestreo de filas por LSH.
  # `max_pares` sigue gobernando el camino exacto; en LSH el alcance se expresa
  # mediante candidatos, cubetas y garantía de colisión.
  usar_lsh <- estrategia == "lsh" || (
    estrategia == "auto" && nrow(datos) > 10000L &&
      is.infinite(muestra) &&
      (is.infinite(max_pares) || max_pares >= 50000000L)
  )
  if (usar_lsh) {
    limite_filas <- if (is.infinite(muestra)) nrow(datos) else {
      min(as.numeric(muestra), nrow(datos))
    }
    indices <- .indices_duplicados_aproximados(nrow(datos), limite_filas)
    estrategia_salida <- "lsh_min_hash"
  } else {
    max_filas_por_pares <- if (is.infinite(max_pares)) Inf else {
      floor((1 + sqrt(1 + 8 * max_pares)) / 2)
    }
    limite_filas <- min(muestra, max_filas_por_pares)
    indices <- .indices_duplicados_aproximados(nrow(datos), limite_filas)
    seleccion <- if (limite_filas <= 3L) "primeras_n_filas" else {
      "muestra_sistematica"
    }
    estrategia_salida <- if (length(indices) >= nrow(datos)) {
      "todas_las_filas_en_bloques"
    } else if (nrow(datos) > muestra &&
               !is.infinite(max_filas_por_pares) &&
               max_filas_por_pares < muestra) {
      paste0(seleccion, "_por_muestra_y_limite_de_pares")
    } else if (nrow(datos) > muestra) {
      paste0(seleccion, "_por_muestra")
    } else {
      paste0(seleccion, "_por_limite_de_pares")
    }
  }
  validos <- indices[textos$presentes[indices]]
  n_pares_comparados <- if (usar_lsh) 0 else as.numeric(length(validos)) *
    (as.numeric(length(validos)) - 1) / 2
  posibles <- as.numeric(nrow(datos)) * (as.numeric(nrow(datos)) - 1) / 2
  if (length(validos) < 2L) {
    resultado <- .vacio_duplicados_aproximados(
      nrow(datos), columnas, metodo, umbral, muestra, max_pares,
      max_resultados, disponible = TRUE, bloque = bloque,
      razon = "No hay dos filas con valores comparables."
    )
    resultado$alcance$n_filas_muestra <- length(indices)
    resultado$alcance$n_filas_validas <- length(validos)
    resultado$alcance$n_pares_comparados <- n_pares_comparados
    resultado$alcance$n_pares_sin_comparar <- posibles
    resultado$alcance$muestreado <- length(indices) < nrow(datos)
    resultado$alcance$estrategia <- estrategia_salida
    resultado$alcance$muestra_efectiva <- length(indices)
    resultado$alcance$modo_comparacion <- "sin_pares_comparables"
    resultado$alcance$tamano_bloque <- bloque
    return(resultado)
  }
  lsh_alcance <- NULL
  if (usar_lsh) {
    lsh <- .comparar_lsh_duplicados(
      textos$valores[validos], validos, metodo, umbral, lsh_bandas,
      lsh_filas, lsh_q, lsh_max_cubeta, max_resultados
    )
    bloques <- lsh
    lsh_alcance <- lsh$alcance
    n_pares_comparados <- lsh$alcance$lsh_pares_comparados[[1L]]
  } else {
    bloques <- .comparar_bloques_duplicados(
      textos$valores[validos], validos, metodo, umbral, bloque, max_resultados
    )
  }
  n_hallados <- bloques$n_hallados
  n_exactos <- bloques$n_exactos
  n_aproximados <- bloques$n_aproximados
  mostrados <- nrow(bloques$pares)
  pares <- if (mostrados) {
    distancias <- bloques$pares$distancia
    data.frame(
      fila_1 = bloques$pares$fila_1,
      fila_2 = bloques$pares$fila_2,
      distancia = distancias,
      tipo_par = ifelse(distancias == 0, "exacto", "aproximado"),
      metodo = metodo, umbral = umbral,
      evidencia_1 = vapply(
        bloques$pares$fila_1,
        function(fila) .evidencia_fila_aproximada(
          datos, columnas, fila, protegidas
        ), character(1L)
      ),
      evidencia_2 = vapply(
        bloques$pares$fila_2,
        function(fila) .evidencia_fila_aproximada(
          datos, columnas, fila, protegidas
        ), character(1L)
      ),
      proteccion_evidencia = if (length(protegidas)) {
        rep("[valores personales protegidos]", mostrados)
      } else rep("ninguna", mostrados),
      stringsAsFactors = FALSE
    )
  } else {
    .vacio_duplicados_aproximados(
      nrow(datos), columnas, metodo, umbral, muestra, max_pares,
      max_resultados, disponible = TRUE, bloque = bloque
    )$pares
  }
  hallazgos <- if (nrow(pares)) {
    do.call(rbind, lapply(seq_len(nrow(pares)), function(i) {
      exacto <- identical(pares$tipo_par[[i]], "exacto")
      .nuevo_hallazgo(
        paste(columnas, collapse = ", "),
        if (exacto) "duplicados_exactos_columnas" else "duplicados_aproximados",
        "sospechoso",
        if (exacto) {
          "Dos filas tienen los mismos valores en las columnas comparadas; esto no demuestra identidad."
        } else {
          "Dos filas presentan similitud; esto no demuestra identidad."
        },
        paste0(
          "Filas ", pares$fila_1[[i]], " y ", pares$fila_2[[i]],
          "; distancia ", format(pares$distancia[[i]], digits = 6),
          " con ", pares$metodo[[i]], " (umbral ", pares$umbral[[i]], "). ",
          pares$evidencia_1[[i]], " / ", pares$evidencia_2[[i]]
        ),
        "Revisar manualmente; no eliminar ni fusionar filas por esta senal."
      )
    }))
  } else {
    data.frame(
      columna = character(), tipo_hallazgo = character(), severidad = character(),
      descripcion = character(), evidencia = character(), sugerencia = character(),
      stringsAsFactors = FALSE
    )
  }
  hallazgos$severidad <- factor(
    hallazgos$severidad, levels = c("ok", "sospechoso", "error"), ordered = TRUE
  )
  alcance <- data.frame(
    n_filas_total = as.numeric(nrow(datos)),
    n_filas_muestra = length(indices), n_filas_validas = length(validos),
    n_pares_posibles = posibles, n_pares_comparados = n_pares_comparados,
    n_pares_sin_comparar = max(0, posibles - n_pares_comparados),
    n_pares_hallados = n_hallados, n_pares_mostrados = mostrados,
    n_pares_exactos = n_exactos,
    n_pares_aproximados = n_aproximados,
    limite_pares = if (usar_lsh) NA_real_ else max_pares,
    limite_pares_configurado = max_pares,
    limite_pares_aplica = !usar_lsh,
    limite_resultados = max_resultados,
    muestra = muestra,
    muestra_efectiva = length(indices),
    estrategia = estrategia_salida,
    modo_comparacion = if (usar_lsh) "lsh_minhash" else if (length(indices) >= nrow(datos)) {
      "exhaustiva_por_bloques"
    } else "muestreada_por_bloques",
    tamano_bloque = if (usar_lsh) NA_integer_ else bloque,
    n_bloques = bloques$n_bloques,
    comparacion_exhaustiva = !usar_lsh && length(indices) >= nrow(datos),
    muestreado = length(indices) < nrow(datos), truncado = mostrados < n_hallados,
    disponible = TRUE, razon = "", stringsAsFactors = FALSE
  )
  if (usar_lsh) alcance <- cbind(alcance, lsh_alcance)
  estructura <- list(
    pares = pares, hallazgos = hallazgos, alcance = alcance,
    columnas = columnas, metodo = metodo, umbral = umbral,
    disponible = TRUE, razon = "",
    proteccion_aplicada = proteger_datos_personales
  )
  class(estructura) <- c("duplicados_aproximados", "list")
  estructura
}

#' Detectar pares de filas con similitud aproximada
#'
#' Compara filas seleccionadas de una tabla con `stringdist` y devuelve pares cuya
#' distancia esta bajo el umbral. El resultado describe similitud, distancia,
#' medida y alcance; nunca afirma que dos filas representen la misma entidad.
#' Por omision se combinan como maximo dos columnas de texto o factores, despues
#' de excluir nombres que parecen identificadores (`id`, `codigo`, `uuid`, entre
#' otros). Si quedan mas de dos columnas, la funcion pide indicar `columnas`
#' explicitamente en vez de mezclar campos que pueden diluir la similitud. La
#' medida predeterminada es Jaro--Winkler (`"jw"`), adecuada para transposiciones
#' y pequenas erratas; el umbral predeterminado `0.12` es deliberadamente mas
#' conservador que `0.15`, que sobre cadenas cortas y estructuradas produce
#' demasiados pares. Ambos argumentos se pueden cambiar.
#'
#' Los pares con distancia cero se incluyen como `tipo_par = "exacto"`; los
#' restantes son `"aproximado"`. Ninguno demuestra identidad.
#'
#' La comparacion usa teselas de `bloque` filas: cada matriz temporal se
#' descarta antes de continuar, por lo que la memoria no crece con el tamaño
#' de la tabla. El recorrido es exacto para las filas seleccionadas. `muestra`
#' y `max_pares` recortan el camino exacto; en el camino LSH, `muestra = Inf`
#' conserva todas las filas y `max_pares` no se aplica a las comparaciones.
#' En ese camino `alcance$limite_pares` es `NA`, mientras
#' `limite_pares_configurado` conserva el valor pedido como referencia y
#' `limite_pares_aplica` permite distinguir ambos casos sin inferirlo.
#' El objeto informa cuantos pares eran posibles, cuantos se compararon, el
#' modo, el tamaño de las teselas o los parámetros LSH y los que quedaron fuera. Solo se muestran
#' `max_resultados` coincidencias; el truncamiento tambien queda declarado.
#'
#' `stringdist` es una dependencia opcional. Si no esta instalado, la funcion
#' devuelve un objeto con `disponible = FALSE`, una tabla vacia y el motivo
#' explicito; no falla ni presenta silencio como si se hubieran comparado todos
#' los pares.
#'
#' Para tablas que superan el tope exhaustivo, `estrategia = "auto"` usa
#' MinHash con bandas LSH sobre todas las filas cuando `muestra = Inf`. La
#' salida declara las bandas, las filas por banda, las cubetas descartadas y la
#' probabilidad teorica de colision. Esa probabilidad se refiere al Jaccard de
#' los q-gramas, no a la medida final (`metodo`). `estrategia = "lsh"` fuerza
#' este camino; `"teselas"` y `"muestra"` conservan el camino exacto o
#' muestreado de las versiones anteriores. Las cubetas que superan
#' `lsh_max_cubeta` se procesan igualmente por lotes; el parámetro identifica
#' cubetas potencialmente costosas, pero no descarta sus pares. El alcance
#' separa `lsh_teselas_cubetas_grandes` (matrices del primer tramo) de
#' `lsh_lotes_cubetas_grandes` (lotes de filas de las bandas posteriores), pues
#' no son la misma unidad. Los pares aceptados por ambos recorridos entran al
#' resumen de Jaccard. Si alguna implementación futura descarta una cubeta,
#' la garantía se devuelve como `NA` y `lsh_garantia_estado` lo deja explícito.
#' La familia MinHash usa una semilla interna fija y restaura el estado global
#' del generador de R; por eso es determinista sin depender de `set.seed()`.
#'
#' @param datos Tabla con una fila por entidad observada.
#' @param columnas Columnas atomicas a combinar. `NULL` aplica la seleccion
#'   automatica descrita arriba; no se incluyen matrices ni listas.
#' @param metodo Medida admitida por `stringdist::stringdistmatrix()`. Por
#'   defecto, `"jw"`.
#' @param umbral Distancia maxima para informar un par. Por defecto `0.12`.
#' @param muestra Máximo de filas candidatas. En el camino exacto queda sujeto
#'   a `max_pares`; con LSH, `Inf` usa todas las filas.
#' @param max_pares Máximo de pares comparados en el camino exacto. Por defecto
#'   `50000000`, que permite recorrer exhaustivamente hasta 10.000 filas con el
#'   método y el bloque predeterminados; se puede reducir para limitar el tiempo.
#'   En LSH el alcance se expresa con candidatos y cubetas, por lo que este
#'   límite no se usa para recortar filas; el resultado lo marca explícitamente.
#' @param max_resultados Maximo de pares devueltos. Por defecto `100`.
#' @param bloque Cantidad de filas por tesela de comparación. Por defecto
#'   `1000`; controla la memoria temporal, no el número de pares comparados.
#' @param normalizar Si se recortan espacios, se pasa a minusculas y se
#'   colapsan espacios antes de calcular la distancia.
#' @param perfil Perfil de los mismos datos para reutilizar su clasificacion de
#'   datos personales y no volver a inferirla.
#' @param proteger_datos_personales Si la evidencia de columnas protegidas se
#'   reemplaza por `[valor protegido]`. La supresion queda indicada en cada par.
#' @param estrategia Estrategia de comparación: `"auto"` (por omisión),
#'   `"teselas"`, `"muestra"` o `"lsh"`. MinHash/LSH sólo se activa
#'   automáticamente por encima del tope exhaustivo; se puede forzar con
#'   `"lsh"`.
#' @param lsh_bandas Número de bandas del esquema LSH. Por defecto, 12.
#' @param lsh_filas Número de filas de firma por banda. Por defecto, 3.
#' @param lsh_q Longitud de los q-gramas usados para MinHash. Por defecto, 3.
#' @param lsh_max_cubeta Umbral a partir del cual una cubeta se considera
#'   grande y se procesa por el mismo troceo acotado del camino exhaustivo.
#'   No se descartan pares por este umbral; el alcance informa cuántas cubetas
#'   y cuántos pares se procesaron de esta forma. Por defecto, 1000.
#'
#' @return Lista de clase `duplicados_aproximados` con `pares`, `hallazgos`,
#'   `alcance`, `columnas`, `metodo`, `umbral`, `disponible` y `razon`.
#' @export
#' @seealso [perfilar()], [reportar()], [planificar_limpieza()]
#' @references Broder, A. Z. (1997). On the resemblance and containment of
#'   documents. En *Compression and Complexity of Sequences*, 21--29.
#'   Leskovec, J., Rajaraman, A. y Ullman, J. D. (2020). *Mining of Massive
#'   Datasets* (3.ª ed.), capítulo 3.
#'
#' @examples
#' datos <- data.frame(
#'   nombre = c("Ana Perez", "Ana Peres", "Luis Diaz"),
#'   domicilio = c("Calle 1", "Calle 1", "Calle 9")
#' )
#' pares <- detectar_duplicados_aproximados(datos)
#' if (!pares$disponible) pares$razon
detectar_duplicados_aproximados <- function(
    datos, columnas = NULL, metodo = "jw", umbral = 0.12,
    muestra = Inf, max_pares = 50000000L, max_resultados = 100L,
    normalizar = TRUE, perfil = NULL, proteger_datos_personales = TRUE,
    bloque = 1000L, estrategia = "auto", lsh_bandas = 12L,
    lsh_filas = 3L, lsh_q = 3L, lsh_max_cubeta = 1000L) {
  if (!is.null(perfil) && (!inherits(perfil, "perfil") ||
      !identical(names(datos), perfil$columnas$columna))) {
    stop("`perfil` debe corresponder a las columnas de `datos`.", call. = FALSE)
  }
  if (!is.null(perfil)) {
    return(.detectar_duplicados_aproximados(
      datos, columnas, metodo, umbral, muestra, max_pares, max_resultados,
      normalizar, clasificacion = perfil$datos_personales,
      proteger_datos_personales = proteger_datos_personales, bloque = bloque,
      estrategia = estrategia, lsh_bandas = lsh_bandas, lsh_filas = lsh_filas,
      lsh_q = lsh_q, lsh_max_cubeta = lsh_max_cubeta
    ))
  }
  .detectar_duplicados_aproximados(
    datos, columnas, metodo, umbral, muestra, max_pares, max_resultados,
    normalizar, proteger_datos_personales = proteger_datos_personales,
    bloque = bloque, estrategia = estrategia, lsh_bandas = lsh_bandas,
    lsh_filas = lsh_filas, lsh_q = lsh_q, lsh_max_cubeta = lsh_max_cubeta
  )
}
