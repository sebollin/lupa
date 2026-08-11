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

.distancias_pares_duplicados <- function(a, b, metodo, nucleos, p = 0.1) {
  stringdist::stringdist(a, b, method = metodo, p = p, nthread = nucleos)
}

.matriz_distancias_duplicados <- function(a, b, metodo, nucleos, p = 0.1) {
  stringdist::stringdistmatrix(a, b, method = metodo, p = p, nthread = nucleos)
}

.nucleos_disponibles_lupa <- function() {
  disponibles <- tryCatch(
    parallel::detectCores(logical = TRUE),
    error = function(e) NA_integer_
  )
  if (!length(disponibles) || is.na(disponibles) || !is.finite(disponibles) ||
      disponibles < 1) 1L else as.integer(disponibles)
}

.resolver_nucleos_lupa <- function(x = getOption("lupa.nucleos", 2L)) {
  if (is.null(x)) x <- getOption("lupa.nucleos", 2L)
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || x != floor(x)) {
    stop("`nucleos` debe ser un entero positivo o NULL.", call. = FALSE)
  }
  min(as.integer(x), .nucleos_disponibles_lupa())
}

.vacio_duplicados_aproximados <- function(
    n_filas, columnas, metodo, umbral, muestra, max_pares, max_resultados,
    disponible = TRUE, razon = "", bloque = 1000L, n_bloques = 0L,
    modo_comparacion = "sin_comparacion", nucleos_usados = NA_integer_,
    p = 0.1) {
  pares <- data.frame(
    fila_1 = integer(), fila_2 = integer(), distancia = numeric(),
    tipo_par = character(), metodo = character(), p = numeric(), umbral = numeric(),
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
    nucleos_usados = nucleos_usados,
    metodo = metodo,
    p = p,
    razon = razon,
    stringsAsFactors = FALSE
  )
  hallazgos <- data.frame(
    columna = character(), tipo_hallazgo = character(), severidad = character(),
    descripcion = character(), evidencia = character(), sugerencia = character(),
    n_evaluados = numeric(), n_afectados = numeric(),
    unidad_conteo = character(),
    estado_reparacion = character(),
    trazabilidad = I(list()),
    stringsAsFactors = FALSE
  )
  estructura <- list(
    pares = pares, hallazgos = hallazgos,
    alcance = alcance, columnas = columnas, metodo = metodo, p = p,
    umbral = umbral, disponible = disponible, razon = razon,
    proteccion_aplicada = FALSE, estimacion = NULL
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

.validar_bloquear_por <- function(datos, x) {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x) ||
      !x %in% names(datos)) {
    stop("`bloquear_por` debe nombrar una columna existente.", call. = FALSE)
  }
  if (is.matrix(datos[[x]]) || is.list(datos[[x]])) {
    stop("`bloquear_por` debe nombrar una columna atomica.", call. = FALSE)
  }
  x
}

.validar_lotes <- function(x) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`lotes` debe ser TRUE o FALSE.", call. = FALSE)
  }
  isTRUE(x)
}

.validar_directorio_lotes <- function(x, crear = TRUE) {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`directorio_lotes` debe ser una ruta de texto o NULL.", call. = FALSE)
  }
  if (isTRUE(crear) && !dir.exists(x) &&
      !dir.create(x, recursive = TRUE, showWarnings = FALSE)) {
    stop("No se pudo crear `directorio_lotes`.", call. = FALSE)
  }
  x
}

.controlar_presupuesto_pares <- function(estimados, presupuesto) {
  if (!is.finite(presupuesto) || !is.finite(estimados) ||
      estimados <= presupuesto) return(invisible(TRUE))
  continuar <- FALSE
  if (isTRUE(interactive())) {
    respuesta <- readline(paste0(
      "La estimaci\u00f3n exacta es de ", .formato_pares_lsh(estimados),
      " pares (presupuesto ", .formato_pares_lsh(presupuesto),
      "). \u00bfContinuar? [s/N] "
    ))
    continuar <- tolower(trimws(respuesta)) %in% c(
      "s", "si", "s\u00ed", "y", "yes"
    )
  }
  if (!continuar) {
    stop(
      "La estimaci\u00f3n exacta (", .formato_pares_lsh(estimados),
      " pares) supera `presupuesto_pares` (",
      .formato_pares_lsh(presupuesto),
      "). No se inici\u00f3 la comparaci\u00f3n; aumente el presupuesto, reduzca los datos, ",
      "suba el umbral o divida el conjunto por una clave.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.resumir_bloqueo <- function(datos, columna) {
  if (is.null(columna)) return(NULL)
  clave <- datos[[columna]]
  textos <- suppressWarnings(as.character(.texto_analizable(clave)$valores))
  ausentes <- is.na(textos)
  # Los ausentes forman un bloque propio: no desaparecen del alcance, pero
  # tampoco se mezclan con valores observados.
  textos[ausentes] <- "<NA bloque propio>"
  ids <- match(textos, unique(textos))
  tamanos <- tabulate(ids, nbins = if (length(ids)) max(ids) else 0L)
  posibles <- as.numeric(nrow(datos)) * (as.numeric(nrow(datos)) - 1) / 2
  alcanzables <- sum(tamanos * pmax(tamanos - 1, 0) / 2)
  cuantiles <- if (length(tamanos)) {
    stats::quantile(
      tamanos, probs = c(0, 0.25, 0.5, 0.75, 1), names = FALSE
    )
  } else rep(0, 5L)
  list(
    ids = ids,
    alcance = data.frame(
      bloqueo_por = columna,
      bloqueo_tratamiento_na = "bloque_propio",
      bloqueo_n_bloques = length(tamanos),
      bloqueo_n_filas_na = sum(ausentes),
      bloqueo_tamano_minimo = cuantiles[[1L]],
      bloqueo_tamano_q1 = cuantiles[[2L]],
      bloqueo_tamano_mediana = cuantiles[[3L]],
      bloqueo_tamano_q3 = cuantiles[[4L]],
      bloqueo_tamano_maximo = cuantiles[[5L]],
      bloqueo_pares_alcanzables = alcanzables,
      bloqueo_pares_fuera_alcance = max(0, posibles - alcanzables),
      bloqueo_prop_pares_alcanzables = if (posibles) alcanzables / posibles else NA_real_,
      bloqueo_perdida = alcanzables < posibles,
      bloqueo_severidad = if (alcanzables < posibles) "sospechoso" else "ok",
      stringsAsFactors = FALSE
    )
  )
}

.estimar_perdida_bloqueo <- function(
    valores, bloqueos, metodo, umbral, tamano_muestra, nucleos = 2L,
    p = 0.1) {
  if (length(valores) < 2L || is.null(bloqueos)) {
    return(data.frame(
      bloqueo_muestra_pares = 0L,
      bloqueo_muestra_candidatos = 0L,
      bloqueo_muestra_candidatos_fuera_alcance = 0L,
      bloqueo_prop_candidatos_fuera_alcance = NA_real_,
      bloqueo_candidatos_sin_bloqueo_estimados = NA_real_,
      bloqueo_candidatos_perdidos_estimados = NA_real_,
      bloqueo_perdida_estimacion_estado = "sin_muestra",
      stringsAsFactors = FALSE
    ))
  }
  pares <- .muestra_pares_lsh(length(valores), tamano_muestra)
  if (!nrow(pares)) {
    return(data.frame(
      bloqueo_muestra_pares = 0L,
      bloqueo_muestra_candidatos = 0L,
      bloqueo_muestra_candidatos_fuera_alcance = 0L,
      bloqueo_prop_candidatos_fuera_alcance = NA_real_,
      bloqueo_candidatos_sin_bloqueo_estimados = NA_real_,
      bloqueo_candidatos_perdidos_estimados = NA_real_,
      bloqueo_perdida_estimacion_estado = "sin_muestra",
      stringsAsFactors = FALSE
    ))
  }
  distancias <- stringdist::stringdist(
    valores[pares$fila_1], valores[pares$fila_2], method = metodo,
    p = p, nthread = nucleos
  )
  candidatos <- is.finite(distancias) & distancias <= umbral
  fuera <- bloqueos[pares$fila_1] != bloqueos[pares$fila_2]
  n_candidatos <- sum(candidatos)
  n_fuera <- sum(candidatos & fuera)
  total <- as.numeric(length(valores)) * (as.numeric(length(valores)) - 1) / 2
  estimados_sin_bloqueo <- if (total && nrow(pares)) {
    n_candidatos / nrow(pares) * total
  } else NA_real_
  proporcion <- if (n_candidatos) n_fuera / n_candidatos else NA_real_
  perdidos <- if (is.finite(estimados_sin_bloqueo) && is.finite(proporcion)) {
    estimados_sin_bloqueo * proporcion
  } else NA_real_
  data.frame(
    bloqueo_muestra_pares = nrow(pares),
    bloqueo_muestra_candidatos = n_candidatos,
    bloqueo_muestra_candidatos_fuera_alcance = n_fuera,
    bloqueo_prop_candidatos_fuera_alcance = proporcion,
    bloqueo_candidatos_sin_bloqueo_estimados = estimados_sin_bloqueo,
    bloqueo_candidatos_perdidos_estimados = perdidos,
    bloqueo_perdida_estimacion_estado = if (n_candidatos) {
      "estimada_por_muestra"
    } else "sin_candidatos_en_muestra",
    stringsAsFactors = FALSE
  )
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
    max_resultados = max_resultados, lotes = list(),
    iguales = logical(), lotes_iguales = list()
  )
}

.ordenar_pares_con_igualdad <- function(pares, iguales) {
  if (!inherits(pares, "data.frame") ||
      !all(c("distancia", "fila_1", "fila_2") %in% names(pares))) {
    stop("Los pares deben ser un data frame con filas y distancia.",
         call. = FALSE)
  }
  if (length(iguales) != nrow(pares)) {
    stop("La igualdad debe contener una entrada por par.", call. = FALSE)
  }
  if (!nrow(pares)) {
    rownames(pares) <- NULL
    return(list(pares = pares, iguales = logical()))
  }
  orden <- order(pares$distancia, pares$fila_1, pares$fila_2)
  salida <- pares[orden, , drop = FALSE]
  rownames(salida) <- NULL
  list(pares = salida, iguales = as.logical(iguales)[orden])
}

.pares_acumulador_con_igualdad <- function(acumulador) {
  if (!is.infinite(acumulador$max_resultados) ||
      !length(acumulador$lotes)) {
    return(.ordenar_pares_con_igualdad(
      acumulador$pares, as.logical(acumulador$iguales)
    ))
  }
  lotes_todos <- c(list(acumulador$pares), acumulador$lotes)
  indices_lotes <- which(vapply(lotes_todos, nrow, integer(1L)) > 0L)
  lotes <- lotes_todos[indices_lotes]
  if (!length(lotes)) return(.ordenar_pares_con_igualdad(
    acumulador$pares, logical()
  ))
  iguales_lotes <- lapply(indices_lotes, function(indice) {
    iguales <- if (indice == 1L) acumulador$iguales else {
      lote <- indice - 1L
      if (length(acumulador$lotes_iguales) >= lote) {
        acumulador$lotes_iguales[[lote]]
      } else logical()
    }
    if (length(iguales) != nrow(lotes_todos[[indice]])) {
      stop("El acumulador no contiene la igualdad de los textos para cada par.",
           call. = FALSE)
    }
    as.logical(iguales)
  })
  pares <- data.frame(
    fila_1 = as.integer(unlist(lapply(lotes, `[[`, "fila_1"),
                         use.names = FALSE)),
    fila_2 = as.integer(unlist(lapply(lotes, `[[`, "fila_2"),
                         use.names = FALSE)),
    distancia = as.numeric(unlist(lapply(lotes, `[[`, "distancia"),
                              use.names = FALSE)),
    stringsAsFactors = FALSE
  )
  iguales <- unlist(iguales_lotes, use.names = FALSE)
  .ordenar_pares_con_igualdad(pares, iguales)
}

.pares_acumulador_duplicados <- function(acumulador) {
  .pares_acumulador_con_igualdad(acumulador)$pares
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
    acumulador, fila_1, fila_2, distancia, iguales = NULL) {
  if (!length(fila_1)) return(acumulador)
  if (is.null(iguales)) {
    stop("El lote debe entregar la igualdad de los textos para cada par.",
         call. = FALSE)
  }
  if (length(fila_1) != length(fila_2) ||
      length(fila_1) != length(distancia) ||
      length(fila_1) != length(iguales)) {
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
  iguales <- as.logical(iguales[validos])
  iguales[is.na(iguales)] <- FALSE
  acumulador$n_hallados <- acumulador$n_hallados + nrow(lote)
  acumulador$n_exactos <- acumulador$n_exactos + sum(iguales)
  acumulador$n_aproximados <- acumulador$n_aproximados +
    sum(!iguales)
  # Con `Inf` se conserva todo y ordenar cada lote vuelve cuadratica la
  # acumulacion. El orden canonico se establece una sola vez al cerrar el
  # generador; con un limite finito se mantiene el recorte incremental.
  if (is.infinite(acumulador$max_resultados)) {
    acumulador$lotes[[length(acumulador$lotes) + 1L]] <- lote
    acumulador$lotes_iguales[[length(acumulador$lotes_iguales) + 1L]] <- iguales
    return(acumulador)
  }
  acumulador$pares <- rbind(acumulador$pares, lote)
  acumulador$iguales <- c(acumulador$iguales, iguales)
  ordenados <- .ordenar_pares_con_igualdad(
    acumulador$pares, acumulador$iguales
  )
  acumulador$pares <- ordenados$pares
  acumulador$iguales <- ordenados$iguales
  limite <- acumulador$max_resultados
  if (!is.infinite(limite) && nrow(acumulador$pares) > limite) {
    acumulador$pares <- acumulador$pares[seq_len(limite), , drop = FALSE]
    acumulador$iguales <- acumulador$iguales[seq_len(limite)]
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
    acumulador = NULL, on_pairs = NULL, bloqueos = NULL, nucleos = 2L,
    p = 0.1) {
  n <- length(valores)
  acumular_en_externo <- !is.null(acumulador)
  if (is.null(acumulador)) {
    acumulador <- .nuevo_acumulador_duplicados(max_resultados)
  }
  if (n < 2L) {
    salida <- if (acumular_en_externo) {
      list(pares = acumulador$pares, iguales = acumulador$iguales)
    } else {
      .pares_acumulador_con_igualdad(acumulador)
    }
    return(list(
      pares = salida$pares, iguales = salida$iguales,
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
      matriz <- as.matrix(.matriz_distancias_duplicados(
        valores[filas_i], valores[filas_j], metodo, nucleos, p
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
      posiciones_1 <- filas_i[candidatas[, 1L]]
      posiciones_2 <- filas_j[candidatas[, 2L]]
      f1 <- filas[posiciones_1]
      f2 <- filas[posiciones_2]
      iguales <- valores[posiciones_1] == valores[posiciones_2]
      if (!is.null(bloqueos)) {
        dentro <- bloqueos[posiciones_1] == bloqueos[posiciones_2]
        f1 <- f1[dentro]
        f2 <- f2[dentro]
        distancias <- distancias[dentro]
        iguales <- iguales[dentro]
      }
      if (!length(f1)) next
      if (!is.null(on_pairs)) on_pairs(f1, f2, distancias)
      acumulador <- .acumular_pares_duplicados(
        acumulador, f1, f2, distancias, iguales
      )
    }
  }
  acumulados <- if (acumular_en_externo) {
    list(pares = acumulador$pares, iguales = acumulador$iguales)
  } else {
    .pares_acumulador_con_igualdad(acumulador)
  }
  pares_acumulados <- acumulados$pares
  iguales_acumulados <- acumulados$iguales
  list(
    pares = pares_acumulados, iguales = iguales_acumulados,
    n_hallados = acumulador$n_hallados,
    n_exactos = acumulador$n_exactos,
    n_aproximados = acumulador$n_aproximados, n_bloques = n_bloques,
    acumulador = acumulador
  )
}

.preparar_directorio_lotes <- function(directorio_lotes) {
  base <- if (is.null(directorio_lotes)) tempdir() else directorio_lotes
  directorio <- tempfile("lupa-lotes-", tmpdir = base)
  if (!dir.create(directorio, recursive = TRUE, showWarnings = FALSE)) {
    stop("No se pudo crear el directorio de parciales.", call. = FALSE)
  }
  directorio
}

.comparar_por_lotes_duplicados <- function(
    valores, filas, metodo, umbral, bloque, tamano_lote, max_resultados,
    bloqueos = NULL, directorio_lotes, nucleos = 2L, p = 0.1) {
  n <- length(valores)
  grupos <- split(seq_len(n), ceiling(seq_len(n) / tamano_lote))
  acumulador <- .nuevo_acumulador_duplicados(max_resultados)
  n_hallados <- 0
  n_exactos <- 0
  n_aproximados <- 0
  n_bloques_archivo <- 0L
  archivos <- character()
  bytes <- numeric()
  parcial_id <- 0L
  for (i in seq_along(grupos)) {
    for (j in i:length(grupos)) {
      parcial_id <- parcial_id + 1L
      indices_i <- grupos[[i]]
      indices_j <- grupos[[j]]
      indices <- if (i == j) indices_i else c(indices_i, indices_j)
      parcial <- .comparar_bloques_duplicados(
        valores[indices], filas[indices], metodo, umbral, bloque, Inf,
        bloqueos = if (is.null(bloqueos)) NULL else {
          bloqueos[indices]
        }, nucleos = nucleos, p = p
      )
      # El comparador anterior compara también el rectángulo cruzado dentro
      # de la concatenación. Para i != j eso incluiría pares internos de cada
      # grupo; conservar sólo el rectángulo pedido evita duplicarlos.
      if (i != j && nrow(parcial$pares)) {
        en_i <- parcial$pares$fila_1 %in% filas[indices_i]
        en_j <- parcial$pares$fila_2 %in% filas[indices_j]
        al_reves <- parcial$pares$fila_1 %in% filas[indices_j] &
          parcial$pares$fila_2 %in% filas[indices_i]
        dentro <- (en_i & en_j) | al_reves
        parcial$pares <- parcial$pares[dentro, , drop = FALSE]
        parcial$iguales <- parcial$iguales[dentro]
      }
      parcial$n_hallados <- nrow(parcial$pares)
      parcial$n_exactos <- sum(parcial$iguales)
      parcial$n_aproximados <- sum(!parcial$iguales)
      ruta <- file.path(
        directorio_lotes, sprintf("parcial-%06d.rds", parcial_id)
      )
      guardado <- list(
        version_esquema = 1L, parcial = parcial_id,
        grupo_1 = i, grupo_2 = j, pares = parcial$pares,
        iguales = parcial$iguales,
        n_hallados = parcial$n_hallados,
        n_exactos = parcial$n_exactos,
        n_aproximados = parcial$n_aproximados,
        n_bloques = parcial$n_bloques
      )
      saveRDS(guardado, ruta, version = 3L)
      parcial_disco <- readRDS(ruta)
      archivos <- c(archivos, ruta)
      bytes <- c(bytes, as.numeric(file.info(ruta)$size))
      n_hallados <- n_hallados + parcial$n_hallados
      n_exactos <- n_exactos + parcial$n_exactos
      n_aproximados <- n_aproximados + parcial$n_aproximados
      n_bloques_archivo <- n_bloques_archivo + parcial$n_bloques
      if (nrow(parcial_disco$pares)) {
        acumulador <- .acumular_pares_duplicados(
          acumulador, parcial_disco$pares$fila_1,
          parcial_disco$pares$fila_2, parcial_disco$pares$distancia,
          parcial_disco$iguales
        )
      }
    }
  }
  # La cantidad publicada es la que habría producido el recorrido completo
  # con la tesela configurada, no una cuenta dependiente de los cortes en disco.
  # Con bloqueo, el recorrido entero reinicia las teselas dentro de cada clave;
  # reproducimos esa misma cuenta para que `alcance` sea indistinguible.
  tamanos_bloque <- if (is.null(bloqueos)) {
    n
  } else {
    as.numeric(table(bloqueos))
  }
  n_bloques <- sum(vapply(tamanos_bloque, function(tamano) {
    if (tamano < 2) return(0)
    n_teselas <- ceiling(tamano / bloque)
    n_teselas * (n_teselas + 1L) / 2L
  }, numeric(1L)))
  n_bloques <- as.integer(n_bloques)
  acumulados <- .pares_acumulador_con_igualdad(acumulador)
  pares_finales <- acumulados$pares
  if (nrow(pares_finales)) {
    rownames(pares_finales) <- NULL
  }
  list(
    pares = pares_finales, iguales = acumulados$iguales,
    n_hallados = n_hallados, n_exactos = n_exactos,
    n_aproximados = n_aproximados, n_bloques = n_bloques,
    metadata = list(
      activo = TRUE, directorio = directorio_lotes,
      n_parciales = length(archivos), archivos = archivos,
      tamanos_bytes = bytes, bytes_totales = sum(bytes),
      tamano_lote = tamano_lote, reanudable = FALSE, completo = TRUE,
      perdida = FALSE, pares_fuera_alcance = 0,
      n_bloques_parciales = n_bloques_archivo,
      estrategia = "exacta_cruzada_sin_perdida"
    )
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

.con_rng_interno_lsh <- function(semilla, funcion) {
  rng_anterior <- RNGkind()
  habia_semilla <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (habia_semilla) semilla_anterior <- get(".Random.seed", .GlobalEnv)
  # El esquema de hashes no debe depender del RNGkind del llamador. Se usa una
  # combinación estable y se restaura primero la clase de RNG (que puede crear
  # un nuevo `.Random.seed`) y después el estado exacto del llamador.
  suppressWarnings(RNGkind(
    kind = "Mersenne-Twister", normal.kind = "Inversion",
    sample.kind = "Rejection"
  ))
  set.seed(semilla)
  on.exit({
    suppressWarnings(do.call(RNGkind, list(
      kind = rng_anterior[[1L]], normal.kind = rng_anterior[[2L]],
      sample.kind = rng_anterior[[3L]]
    )))
    if (habia_semilla) {
      assign(".Random.seed", semilla_anterior, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  funcion()
}

.muestra_pares_lsh <- function(n, tamano) {
  total <- as.numeric(n) * (as.numeric(n) - 1) / 2
  if (n < 2L || tamano < 1 || total < 1) {
    return(data.frame(
      fila_1 = integer(), fila_2 = integer(), stringsAsFactors = FALSE
    ))
  }
  cantidad <- min(as.numeric(tamano), total, .Machine$integer.max)
  if (cantidad >= total) {
    fila_1 <- rep(seq_len(n - 1L), times = rev(seq_len(n - 1L)))
    fila_2 <- unlist(lapply(seq_len(n - 1L), function(i) (i + 1L):n),
                     use.names = FALSE)
    return(data.frame(
      fila_1 = fila_1, fila_2 = fila_2, stringsAsFactors = FALSE
    ))
  }
  pares <- .con_rng_interno_lsh(173L, function() {
    # Se sortea el rango triangular de pares, no dos filas independientes:
    # así la muestra no repite pares y mantiene una cobertura uniforme incluso
    # cuando las filas contienen muchos valores repetidos.
    rangos <- floor(stats::runif(as.integer(cantidad)) * total)
    comienzos <- c(0, cumsum(as.numeric(seq.int(n - 1L, 1L, by = -1L))))
    comienzos <- comienzos[seq_len(n - 1L)]
    fila_1 <- findInterval(rangos, c(-1, comienzos)) - 1L
    fila_2 <- fila_1 + 1L + as.integer(rangos - comienzos[fila_1])
    data.frame(
      fila_1 = pmin(fila_1, fila_2), fila_2 = pmax(fila_1, fila_2),
      stringsAsFactors = FALSE
    )
  })
  unique(pares)
}

.estimar_lsh <- function(
    firmas, valores, metodo, bandas, filas_banda, tamano_muestra,
    bloqueos = NULL, nucleos = 2L, p = 0.1) {
  n <- nrow(firmas)
  pares <- .muestra_pares_lsh(n, tamano_muestra)
  total <- as.numeric(n) * (as.numeric(n) - 1) / 2
  if (!nrow(pares)) {
    return(list(
      candidatos_previstos = 0, probabilidad = NA_real_,
      muestra_usada = 0L, pares_benchmark = 0L,
      tiempo_benchmark = NA_real_, velocidad = NA_real_, tiempo = NA_real_
    ))
  }
  iguales <- firmas[pares$fila_1, , drop = FALSE] ==
    firmas[pares$fila_2, , drop = FALSE]
  colisiones <- do.call(cbind, lapply(seq_len(bandas), function(banda) {
    columnas <- ((banda - 1L) * filas_banda + 1L):(banda * filas_banda)
    rowSums(iguales[, columnas, drop = FALSE]) == filas_banda
  }))
  candidatos <- rowSums(colisiones) > 0L
  if (!is.null(bloqueos)) {
    candidatos <- candidatos & bloqueos[pares$fila_1] == bloqueos[pares$fila_2]
  }
  probabilidad <- mean(candidatos)
  candidatos_previstos <- probabilidad * total
  pares_benchmark <- if (is.null(bloqueos)) {
    pares
  } else {
    pares[bloqueos[pares$fila_1] == bloqueos[pares$fila_2], , drop = FALSE]
  }
  n_benchmark <- min(5000L, nrow(pares_benchmark))
  if (!n_benchmark) {
    return(list(
      candidatos_previstos = candidatos_previstos,
      probabilidad = probabilidad, muestra_usada = nrow(pares),
      pares_benchmark = 0L, tiempo_benchmark = NA_real_,
      velocidad = NA_real_, tiempo = NA_real_
    ))
  }
  # Calentar la resolución de la medida antes de cronometrar; de lo contrario
  # el primer llamado puede incluir la carga perezosa de `stringdist` y
  # describir la infraestructura en lugar de los pares por segundo.
  invisible(stringdist::stringdist(
    valores[pares_benchmark$fila_1[[1L]]],
    valores[pares_benchmark$fila_2[[1L]]], method = metodo,
    p = p, nthread = nucleos
  ))
  indices_benchmark <- seq_len(n_benchmark)
  inicio <- proc.time()[["elapsed"]]
  transcurrido <- 0
  repeticiones <- 0L
  pares_cronometrados <- 0L
  # Un solo llamado puede durar menos que la resolucion del reloj. Repetir el
  # mismo lote hasta 50 ms evita velocidades cuantizadas en 5000/k; el numero
  # total de pares cronometrados queda declarado en el mensaje informativo.
  while (transcurrido < 0.05 && repeticiones < 10000L) {
    invisible(stringdist::stringdist(
      valores[pares_benchmark$fila_1[indices_benchmark]],
      valores[pares_benchmark$fila_2[indices_benchmark]], method = metodo,
      p = p, nthread = nucleos
    ))
    repeticiones <- repeticiones + 1L
    pares_cronometrados <- pares_cronometrados + n_benchmark
    transcurrido <- proc.time()[["elapsed"]] - inicio
  }
  velocidad <- if (is.finite(transcurrido) && transcurrido > 0) {
    pares_cronometrados / transcurrido
  } else NA_real_
  list(
    candidatos_previstos = candidatos_previstos,
    probabilidad = probabilidad, muestra_usada = nrow(pares),
    pares_benchmark = pares_cronometrados, tiempo_benchmark = transcurrido,
    velocidad = velocidad,
    tiempo = if (is.finite(velocidad)) candidatos_previstos / velocidad else NA_real_
  )
}

.formato_pares_lsh <- function(x) {
  if (!length(x) || is.na(x)) return("NA")
  if (is.infinite(x)) return("Inf")
  format(round(x), big.mark = ".", decimal.mark = ",",
         scientific = FALSE, trim = TRUE)
}

.texto_tiempo_lsh <- function(estimacion, nucleos = 2L) {
  if (is.finite(estimacion$tiempo)) {
    paste0(
      "LSH: ", .formato_pares_lsh(estimacion$candidatos_previstos),
      " candidatos previstos; referencia de ",
      format(round(estimacion$tiempo, 3), nsmall = 3,
        decimal.mark = ","),
      " s (piso, no incluye firma ni cubetas; subir nucleos puede acortar ",
      "esta etapa; hoy usa ", nucleos, " hilos), medida con ",
      .formato_pares_lsh(estimacion$pares_benchmark), " pares en ",
      format(round(estimacion$tiempo_benchmark, 3), nsmall = 3,
        decimal.mark = ","), " s."
    )
  } else {
    paste0(
      "LSH: no se pudo medir una velocidad con ",
      .formato_pares_lsh(estimacion$pares_benchmark),
      " pares; el tiempo queda sin estimar (subir nucleos puede acortar ",
      "esta etapa; hoy usa ", nucleos, " hilos)."
    )
  }
}

.emitir_tiempo_lsh <- function(texto) {
  condicion <- structure(
    list(message = texto, call = NULL),
    class = c("lupa_tiempo_lsh", "message", "condition")
  )
  withRestarts(
    signalCondition(condicion),
    muffleMessage = function() NULL
  )
  invisible(NULL)
}

.firmas_minhash_lsh <- function(ids, n_hashes) {
  n <- nrow(ids)
  if (!n_hashes) return(matrix(numeric(), nrow = n, ncol = 0L))
  # La semilla es interna y fija; se restaura el estado del llamador para que
  # el resultado no dependa de `set.seed()` ni lo modifique.
  # Cada hash es una permutacion aleatoria explicita del vocabulario. Por eso
  # es inyectivo para cualquier tamano de vocabulario y conserva la propiedad
  # MinHash sin depender de los nombres o del orden de los q-gramas. La
  # permutacion completa evita que una numeracion consecutiva de q-gramas
  # produzca progresiones aritmeticas con minimos artificialmente coincidentes.
  # `semilla` es interna y fija; dos corridas iguales son identicas.
  .con_rng_interno_lsh(1L, function() {
    vocabulario <- if (length(ids)) suppressWarnings(max(ids, na.rm = TRUE)) else 0
    if (!is.finite(vocabulario) || vocabulario < 1) vocabulario <- 0
    vocabulario <- as.integer(vocabulario)
    firmas <- matrix(Inf, nrow = n, ncol = n_hashes)
    for (h in seq_len(n_hashes)) {
      consulta <- if (vocabulario) {
        c(NA_real_, as.numeric(sample.int(vocabulario)))
      } else {
        NA_real_
      }
      minimo <- rep(Inf, n)
      for (j in seq_len(ncol(ids))) {
        ids_col <- as.integer(ids[, j])
        valores <- rep(Inf, n)
        validos <- ids_col > 0L & ids_col <= vocabulario
        if (any(validos)) valores[validos] <- consulta[ids_col[validos] + 1L]
        minimo <- pmin(minimo, valores)
      }
      firmas[, h] <- minimo
    }
    firmas
  })
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
    max_cubeta, max_resultados, muestra_estimacion = 400000L,
    presupuesto_pares = Inf, bloqueos = NULL, solo_estimacion = FALSE,
    nucleos = 2L, p = 0.1) {
  n <- length(valores)
  n_hashes <- bandas * filas_banda
  indice <- .ids_qgramas_por_bloques(valores, q)
  ids <- indice$ids
  vocabulario <- indice$vocabulario
  vocabulario_n <- length(vocabulario)
  firmas <- .firmas_minhash_lsh(ids, n_hashes)
  # El índice de q-gramas ya no participa en la estimación: liberarlo antes
  # del benchmark evita que una recolección de basura se contabilice como
  # velocidad de la medida final.
  ids <- NULL
  vocabulario <- NULL
  indice <- NULL
  estimacion <- .estimar_lsh(
    firmas, valores, metodo, bandas, filas_banda, muestra_estimacion,
    bloqueos = bloqueos, nucleos = nucleos, p = p
  )
  mensaje_tiempo <- .texto_tiempo_lsh(estimacion, nucleos)
  if (isTRUE(interactive())) {
    # nocov start: la salida visual sólo existe en una sesión interactiva.
    cli::cli_alert_info(mensaje_tiempo)
    # nocov end
  } else {
    .emitir_tiempo_lsh(mensaje_tiempo)
  }
  .controlar_presupuesto_pares(
    estimacion$candidatos_previstos, presupuesto_pares
  )
  if (isTRUE(solo_estimacion)) {
    return(list(
      estimacion = list(
        candidatos_previstos = estimacion$candidatos_previstos,
        probabilidad_candidato_estimada = estimacion$probabilidad,
        muestra_estimacion = estimacion$muestra_usada,
        vocabulario = vocabulario_n,
        pares_benchmark = estimacion$pares_benchmark,
        tiempo_benchmark = estimacion$tiempo_benchmark,
        velocidad_comparacion = estimacion$velocidad,
        tiempo_estimado_segundos = estimacion$tiempo,
        tiempo_estimado_etapa = "comparacion_stringdist",
        tiempo_estimado_es_piso = TRUE,
        tiempo_determinista = FALSE
      ),
      alcance = data.frame(
        lsh_vocabulario = vocabulario_n,
        lsh_candidatos_previstos = estimacion$candidatos_previstos,
        lsh_candidatos_previstos_es_estimacion = TRUE,
        lsh_probabilidad_candidato_estimada = estimacion$probabilidad,
        lsh_muestra_estimacion = estimacion$muestra_usada,
        nucleos_usados = nucleos,
        stringsAsFactors = FALSE
      )
    ))
  }
  # Las listas de q-gramas y la matriz de ids sólo son necesarias para
  # construir la firma. Liberarlas antes del recorrido evita que la memoria
  # del índice se sume a la de las cubetas; el Jaccard de los pocos pares que
  # pasan el umbral se recalcula bajo demanda.
  acumulador <- .nuevo_acumulador_duplicados(max_resultados)
  candidatos_generados <- 0
  candidatos_unicos <- 0
  candidatos_descartados_bandas <- 0
  candidatos_descartados_bloque <- 0
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
  progreso <- isTRUE(interactive())
  # nocov start: la barra sólo se dibuja en una sesión interactiva.
  if (progreso) {
    cli::cli_progress_bar("Generando candidatos LSH", total = bandas)
    on.exit(cli::cli_progress_done(), add = TRUE)
  }
  # nocov end
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
        posibles_bloque <- if (is.null(bloqueos)) {
          posibles_grupo
        } else {
          sum(table(bloqueos[indices]) *
                pmax(table(bloqueos[indices]) - 1, 0) / 2)
        }
        pares_cubetas_troceadas <- pares_cubetas_troceadas + posibles_bloque
        por_teselas <- .comparar_bloques_duplicados(
          valores[indices], filas[indices], metodo, umbral,
          bloque = min(2000L, tamano), max_resultados = max_resultados,
          acumulador = acumulador, on_pairs = registrar_jaccard_filas,
          bloqueos = if (is.null(bloqueos)) NULL else bloqueos[indices],
          nucleos = nucleos, p = p
        )
        acumulador <- por_teselas$acumulador
        teselas_cubetas_grandes <- teselas_cubetas_grandes +
          por_teselas$n_bloques
        candidatos_generados <- candidatos_generados + posibles_grupo
        candidatos_unicos <- candidatos_unicos + posibles_bloque
        pares_comparados <- pares_comparados + posibles_bloque
        candidatos_descartados_bloque <- candidatos_descartados_bloque +
          posibles_grupo - posibles_bloque
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
        if (!is.null(bloqueos)) {
          dentro <- bloqueos[p1] == bloqueos[p2]
          candidatos_descartados_bloque <- candidatos_descartados_bloque +
            sum(!dentro)
          p1 <- p1[dentro]
          p2 <- p2[dentro]
        }
        if (!length(p1)) next
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
        distancias <- .distancias_pares_duplicados(
          valores[p1], valores[p2], metodo, nucleos, p
        )
        pares_comparados <- pares_comparados + length(p1)
        pasan <- is.finite(distancias) & distancias <= umbral
        if (any(pasan)) {
          indices_pasan <- which(pasan)
          registrar_jaccard(p1[indices_pasan], p2[indices_pasan])
          filas_a <- filas[p1[indices_pasan]]
          filas_b <- filas[p2[indices_pasan]]
          iguales <- valores[p1[indices_pasan]] == valores[p2[indices_pasan]]
          intercambiar <- filas_a > filas_b
          if (any(intercambiar)) {
            temporal <- filas_a[intercambiar]
            filas_a[intercambiar] <- filas_b[intercambiar]
            filas_b[intercambiar] <- temporal
          }
          acumulador <- .acumular_pares_duplicados(
            acumulador, filas_a, filas_b, distancias[indices_pasan], iguales
          )
        }
      }
    }
    claves_previas[[banda]] <- claves
    # nocov start
    if (progreso) cli::cli_progress_update()
    # nocov end
  }
  garantia_07 <- .garantia_lsh(
    0.7, bandas, filas_banda, pares_descartados_cubetas
  )
  resumen_jaccard <- if (length(jaccard)) {
    quant <- stats::quantile(jaccard, probs = c(0, .25, .5, .75, 1),
                             names = FALSE, na.rm = TRUE)
    quant
  } else rep(NA_real_, 5L)
  acumulados <- .pares_acumulador_con_igualdad(acumulador)
  acumulador$pares <- acumulados$pares
  acumulador$iguales <- acumulados$iguales
  acumulador$lotes <- list()
  rownames(acumulador$pares) <- NULL
  list(
    pares = acumulador$pares,
    iguales = acumulados$iguales,
    n_hallados = acumulador$n_hallados,
    n_exactos = acumulador$n_exactos,
    n_aproximados = acumulador$n_aproximados,
    n_bloques = 0L,
    estimacion = list(
      candidatos_previstos = estimacion$candidatos_previstos,
      probabilidad_candidato_estimada = estimacion$probabilidad,
      muestra_estimacion = estimacion$muestra_usada,
      vocabulario = vocabulario_n,
      pares_benchmark = estimacion$pares_benchmark,
      tiempo_benchmark = estimacion$tiempo_benchmark,
      velocidad_comparacion = estimacion$velocidad,
      tiempo_estimado_segundos = estimacion$tiempo,
      tiempo_estimado_etapa = "comparacion_stringdist",
      tiempo_estimado_es_piso = TRUE,
      tiempo_determinista = FALSE
    ),
    alcance = data.frame(
      lsh_bandas = bandas, lsh_filas = filas_banda,
      lsh_tamano_firma = n_hashes, lsh_q = q,
      lsh_vocabulario = vocabulario_n,
      lsh_semilla_hash = 1L,
      lsh_hash_familia = "permutacion_aleatoria_determinista_inyectiva",
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
      lsh_candidatos_descartados_bloque = candidatos_descartados_bloque,
      lsh_candidatos_previstos = estimacion$candidatos_previstos,
      lsh_candidatos_previstos_es_estimacion = TRUE,
      lsh_probabilidad_candidato_estimada = estimacion$probabilidad,
      lsh_muestra_estimacion = estimacion$muestra_usada,
      lsh_muestra_estimacion_configurada = muestra_estimacion,
      nucleos_usados = nucleos,
      lsh_presupuesto_pares = presupuesto_pares,
      lsh_candidatos_descartados_bandas = candidatos_descartados_bandas,
      lsh_pares_comparados = pares_comparados,
      lsh_jaccard_evaluados = n_jaccard,
      lsh_jaccard_pares_elegibles = n_jaccard_pares,
      lsh_jaccard_alcance = if (n_jaccard_pares > n_jaccard) {
        paste0("primeros_del_recorrido_de_", n_jaccard,
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

.texto_fila_aproximada <- function(datos, columnas, normalizar = TRUE) {
  normalizacion_resuelta <- .resolver_normalizacion(normalizar)
  valores <- Map(function(x, columna) {
    # Reutilizar el saneamiento del perfil: los bytes UTF-8 invalidos no
    # deben abortar una comparacion ni entrar como evidencia.
    salida <- suppressWarnings(as.character(.texto_analizable(x)$valores))
    presentes <- !is.na(salida) & nzchar(salida)
    salida[is.na(salida)] <- ""
    list(
      valores = .normalizacion_aplicar(
        salida, .normalizacion_para_columna(normalizacion_resuelta, columna)
      ),
      presentes = presentes
    )
  }, datos[columnas], columnas)
  filas <- do.call(paste, c(lapply(valores, `[[`, "valores"), sep = " | "))
  presentes <- Reduce(`|`, lapply(valores, `[[`, "presentes"),
                      init = rep(FALSE, nrow(datos)))
  fusiones <- stats::setNames(lapply(seq_along(columnas), function(i) {
    crudos <- suppressWarnings(as.character(.texto_analizable(
      datos[[columnas[[i]]]]
    )$valores))
    crudos <- unique(crudos[!is.na(crudos)])
    .normalizacion_fusiones_vocabulario(
      crudos, .normalizacion_para_columna(normalizacion_resuelta, columnas[[i]])
    )
  }), columnas)
  list(valores = filas, presentes = presentes,
       normalizacion = normalizacion_resuelta, fusiones = fusiones)
}

.evidencia_fila_aproximada <- function(datos, columnas, fila, protegidas) {
  valores <- vapply(columnas, function(columna) {
    if (columna %in% protegidas) return("[valor protegido]")
    valor <- suppressWarnings(
      as.character(.texto_analizable(datos[[columna]][[fila]])$valores)
    )
    if (!length(valor) || is.na(valor)) "[ausente]" else valor
  }, character(1L))
  paste0(columnas, "=", valores, collapse = "; ")
}

.evidencia_filas_aproximada <- function(datos, columnas, filas, protegidas) {
  n <- length(filas)
  if (!n) return(character())
  partes <- lapply(columnas, function(columna) {
    valores <- if (columna %in% protegidas) {
      rep("[valor protegido]", n)
    } else {
      valores <- suppressWarnings(
        as.character(.texto_analizable(datos[[columna]][filas])$valores)
      )
      valores[is.na(valores) | !length(valores)] <- "[ausente]"
      valores
    }
    paste0(columna, "=", valores)
  })
  salida <- partes[[1L]]
  if (length(partes) > 1L) {
    for (i in 2:length(partes)) salida <- paste0(salida, "; ", partes[[i]])
  }
  salida
}

.detectar_duplicados_aproximados <- function(
    datos, columnas = NULL, metodo = "jw", umbral = 0.10, p = 0.1,
    muestra = Inf, max_pares = 50000000L, max_resultados = 100L,
    normalizar = TRUE, clasificacion = NULL,
    proteger_datos_personales = TRUE, bloque = 1000L,
    estrategia = "auto", lsh_bandas = 12L, lsh_filas = 3L, lsh_q = 3L,
    lsh_max_cubeta = 1000L, lsh_muestra_estimacion = 400000L,
    presupuesto_pares = Inf, bloquear_por = NULL, solo_estimacion = FALSE,
    lotes = FALSE, tamano_lote = 1000L, directorio_lotes = NULL,
    nucleos = getOption("lupa.nucleos", 2L)) {
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe heredar de data.frame.", call. = FALSE)
  }
  columnas <- .columnas_duplicados_aproximados(datos, columnas)
  normalizacion_resuelta <- .resolver_normalizacion(normalizar)
  muestra <- .validar_limite_duplicados(muestra, "muestra")
  max_pares <- .validar_limite_duplicados(max_pares, "max_pares")
  max_resultados <- .validar_limite_duplicados(max_resultados, "max_resultados")
  nucleos <- .resolver_nucleos_lupa(nucleos)
  bloque <- .validar_bloque_duplicados(bloque)
  bloquear_por <- .validar_bloquear_por(datos, bloquear_por)
  lotes <- .validar_lotes(lotes)
  tamano_lote <- .validar_bloque_duplicados(tamano_lote)
  directorio_lotes <- if (lotes) {
    .validar_directorio_lotes(directorio_lotes)
  } else {
    .validar_directorio_lotes(directorio_lotes, crear = FALSE)
  }
  resumen_bloqueo <- .resumir_bloqueo(datos, bloquear_por)
  estrategia <- match.arg(estrategia, c("auto", "teselas", "muestra", "lsh"))
  lsh_bandas <- .validar_parametro_lsh(lsh_bandas, "lsh_bandas")
  lsh_filas <- .validar_parametro_lsh(lsh_filas, "lsh_filas")
  lsh_q <- .validar_parametro_lsh(lsh_q, "lsh_q")
  lsh_max_cubeta <- .validar_parametro_lsh(
    lsh_max_cubeta, "lsh_max_cubeta", minimo = 2L
  )
  lsh_muestra_estimacion <- .validar_parametro_lsh(
    lsh_muestra_estimacion, "lsh_muestra_estimacion"
  )
  presupuesto_pares <- .validar_limite_duplicados(
    presupuesto_pares, "presupuesto_pares"
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
  if (!is.numeric(p) || length(p) != 1L || is.na(p) || !is.finite(p) ||
      p < 0 || p > 0.25) {
    stop("`p` debe ser un numero finito entre 0 y 0.25.", call. = FALSE)
  }
  if (!is.logical(proteger_datos_personales) ||
      length(proteger_datos_personales) != 1L ||
      is.na(proteger_datos_personales)) {
    stop("`normalizar` y `proteger_datos_personales` deben ser l\u00f3gicos escalares.",
         call. = FALSE)
  }
  if (!.stringdist_disponible()) {
    resultado <- .vacio_duplicados_aproximados(
      nrow(datos), columnas, metodo, umbral, muestra, max_pares,
      max_resultados, disponible = FALSE, bloque = bloque, p = p,
      razon = "No esta instalado el paquete opcional 'stringdist'.",
      nucleos_usados = nucleos
    )
    if (!is.null(resumen_bloqueo)) {
      resultado$alcance <- cbind(resultado$alcance, resumen_bloqueo$alcance)
    }
    resultado$normalizacion <- c(
      .normalizacion_resumen(normalizacion_resuelta),
      list(fusiones = NULL)
    )
    return(resultado)
  }
  if (!length(columnas)) {
    motivo <- attr(columnas, "motivo_sin_columnas", exact = TRUE)
    if (is.null(motivo)) {
      motivo <- "No hay columnas de texto comparables; indique `columnas` explicitamente."
    }
    resultado <- .vacio_duplicados_aproximados(
      nrow(datos), columnas, metodo, umbral, muestra, max_pares,
      max_resultados, disponible = TRUE, bloque = bloque, p = p,
      razon = motivo, nucleos_usados = nucleos
    )
    if (!is.null(resumen_bloqueo)) {
      resultado$alcance <- cbind(resultado$alcance, resumen_bloqueo$alcance)
    }
    resultado$normalizacion <- c(
      .normalizacion_resumen(normalizacion_resuelta), list(fusiones = NULL)
    )
    return(resultado)
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
  textos <- .texto_fila_aproximada(datos, columnas, normalizacion_resuelta)
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
  if (!is.null(resumen_bloqueo)) {
    validos_bloqueo <- which(textos$presentes)
    perdida_bloqueo <- .estimar_perdida_bloqueo(
      textos$valores[validos_bloqueo],
      resumen_bloqueo$ids[validos_bloqueo], metodo, umbral,
      lsh_muestra_estimacion, nucleos = nucleos, p = p
    )
    resumen_bloqueo$alcance <- cbind(
      resumen_bloqueo$alcance, perdida_bloqueo
    )
  }
  n_pares_comparados <- if (usar_lsh) 0 else as.numeric(length(validos)) *
    (as.numeric(length(validos)) - 1) / 2
  if (!usar_lsh && !is.null(resumen_bloqueo)) {
    tamanos_validos <- table(resumen_bloqueo$ids[validos])
    n_pares_comparados <- sum(tamanos_validos *
      pmax(tamanos_validos - 1, 0) / 2)
  }
  posibles <- as.numeric(nrow(datos)) * (as.numeric(nrow(datos)) - 1) / 2
  if (length(validos) < 2L) {
    resultado <- .vacio_duplicados_aproximados(
      nrow(datos), columnas, metodo, umbral, muestra, max_pares,
      max_resultados, disponible = TRUE, bloque = bloque, p = p,
      razon = "No hay dos filas con valores comparables.",
      nucleos_usados = nucleos
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
    if (!is.null(resumen_bloqueo)) {
      resultado$alcance <- cbind(resultado$alcance, resumen_bloqueo$alcance)
    }
    resultado$normalizacion <- c(
      .normalizacion_resumen(normalizacion_resuelta), list(fusiones = NULL)
    )
    return(resultado)
  }
  if (lotes && usar_lsh) {
    stop(
      "`lotes = TRUE` s\u00f3lo est\u00e1 disponible para la comparaci\u00f3n exacta; ",
      "el camino LSH ya procesa sus cubetas por bloques en memoria.",
      call. = FALSE
    )
  }
  if (!isTRUE(solo_estimacion) && !usar_lsh) {
    .controlar_presupuesto_pares(n_pares_comparados, presupuesto_pares)
  }
  if (isTRUE(solo_estimacion) && !(
      estrategia == "lsh" ||
      (estrategia == "auto" && nrow(datos) > 10000L &&
       is.infinite(muestra) &&
       (is.infinite(max_pares) || max_pares >= 50000000L)))) {
    dentro <- n_pares_comparados
    alcance <- data.frame(
      modo_comparacion = "exhaustiva_por_bloques",
      candidatos_previstos = dentro,
      pares_alcanzables = dentro,
      nucleos_usados = nucleos,
      stringsAsFactors = FALSE
    )
    if (!is.null(resumen_bloqueo)) {
      alcance <- cbind(alcance, resumen_bloqueo$alcance)
    }
    return(list(
      estimacion = list(
        candidatos_previstos = dentro,
        probabilidad_candidato_estimada = NA_real_,
        muestra_estimacion = dentro,
        vocabulario = NA_integer_,
        pares_benchmark = NA_integer_,
        tiempo_benchmark = NA_real_,
        velocidad_comparacion = NA_real_,
        tiempo_estimado_segundos = NA_real_,
        tiempo_estimado_etapa = NA_character_,
        tiempo_estimado_es_piso = FALSE,
        tiempo_determinista = TRUE
      ), alcance = alcance, disponible = TRUE, razon = "",
      normalizacion = .normalizacion_resumen(normalizacion_resuelta)
    ))
  }
  lsh_alcance <- NULL
  lotes_metadata <- NULL
  if (usar_lsh) {
    lsh <- .comparar_lsh_duplicados(
      textos$valores[validos], validos, metodo, umbral, lsh_bandas,
      lsh_filas, lsh_q, lsh_max_cubeta, max_resultados,
      lsh_muestra_estimacion, presupuesto_pares,
      bloqueos = if (is.null(resumen_bloqueo)) NULL else {
        resumen_bloqueo$ids[validos]
      }, solo_estimacion = solo_estimacion, nucleos = nucleos, p = p
    )
    if (isTRUE(solo_estimacion)) {
      alcance <- lsh$alcance
      if (!is.null(resumen_bloqueo)) {
        alcance <- cbind(alcance, resumen_bloqueo$alcance)
      }
      return(list(
        estimacion = lsh$estimacion, alcance = alcance,
        disponible = TRUE, razon = "",
        normalizacion = .normalizacion_resumen(normalizacion_resuelta)
      ))
    }
    bloques <- lsh
    lsh_alcance <- lsh$alcance
    estimacion_resultado <- lsh$estimacion
    n_pares_comparados <- lsh$alcance$lsh_pares_comparados[[1L]]
  } else {
    if (lotes) {
      directorio_parciales <- .preparar_directorio_lotes(directorio_lotes)
      bloques <- .comparar_por_lotes_duplicados(
        textos$valores[validos], validos, metodo, umbral, bloque,
        tamano_lote, max_resultados,
        bloqueos = if (is.null(resumen_bloqueo)) NULL else {
          resumen_bloqueo$ids[validos]
        }, directorio_lotes = directorio_parciales, nucleos = nucleos, p = p
      )
      lotes_metadata <- bloques$metadata
    } else if (is.null(resumen_bloqueo)) {
      bloques <- .comparar_bloques_duplicados(
        textos$valores[validos], validos, metodo, umbral, bloque, max_resultados,
        nucleos = nucleos, p = p
      )
    } else {
      grupos <- split(seq_along(validos), resumen_bloqueo$ids[validos])
      acumulador <- .nuevo_acumulador_duplicados(max_resultados)
      n_bloques <- 0L
      for (grupo in grupos) {
        parcial <- .comparar_bloques_duplicados(
          textos$valores[validos[grupo]], validos[grupo], metodo, umbral,
          bloque, max_resultados, acumulador = acumulador,
          nucleos = nucleos, p = p
        )
        acumulador <- parcial$acumulador
        n_bloques <- n_bloques + parcial$n_bloques
      }
      acumulados_bloqueados <- .pares_acumulador_con_igualdad(acumulador)
      bloques <- list(
        pares = acumulados_bloqueados$pares,
        iguales = acumulados_bloqueados$iguales,
        n_hallados = acumulador$n_hallados,
        n_exactos = acumulador$n_exactos,
        n_aproximados = acumulador$n_aproximados,
        n_bloques = n_bloques
      )
    }
    estimacion_resultado <- NULL
  }
  n_hallados <- bloques$n_hallados
  n_exactos <- bloques$n_exactos
  n_aproximados <- bloques$n_aproximados
  mostrados <- nrow(bloques$pares)
  pares <- if (mostrados) {
    distancias <- bloques$pares$distancia
    iguales <- as.logical(bloques$iguales)
    data.frame(
      fila_1 = bloques$pares$fila_1,
      fila_2 = bloques$pares$fila_2,
      distancia = distancias,
      tipo_par = ifelse(iguales, "exacto", "aproximado"),
      metodo = metodo, p = p, umbral = umbral,
      evidencia_1 = .evidencia_filas_aproximada(
        datos, columnas, bloques$pares$fila_1, protegidas
      ),
      evidencia_2 = .evidencia_filas_aproximada(
        datos, columnas, bloques$pares$fila_2, protegidas
      ),
      proteccion_evidencia = if (length(protegidas)) {
        rep("[valores personales protegidos]", mostrados)
      } else rep("ninguna", mostrados),
      stringsAsFactors = FALSE
    )
  } else {
    .vacio_duplicados_aproximados(
      nrow(datos), columnas, metodo, umbral, muestra, max_pares,
      max_resultados, disponible = TRUE, bloque = bloque, p = p,
      nucleos_usados = nucleos
    )$pares
  }
  hallazgos <- if (nrow(pares)) {
    n <- nrow(pares)
    exactos <- pares$tipo_par == "exacto"
    data.frame(
      columna = rep(paste(columnas, collapse = ", "), n),
      tipo_hallazgo = ifelse(
        exactos, "duplicados_exactos_columnas", "duplicados_aproximados"
      ),
      severidad = rep("sospechoso", n),
      descripcion = ifelse(
        exactos,
        "Dos filas tienen los mismos valores en las columnas comparadas; esto no demuestra identidad.",
        "Dos filas presentan similitud; esto no demuestra identidad."
      ),
      evidencia = paste0(
        "Filas ", pares$fila_1, " y ", pares$fila_2,
        "; distancia ",
        vapply(pares$distancia, format, character(1L), digits = 6L),
        " con ", pares$metodo, " (umbral ", pares$umbral, "). ",
        pares$evidencia_1, " / ", pares$evidencia_2
      ),
      sugerencia = rep(
        "Revisar manualmente; no eliminar ni fusionar filas por esta senal.", n
      ),
      n_evaluados = rep(n_pares_comparados, n),
      n_afectados = rep(1, n),
      unidad_conteo = rep("par", n),
      estado_reparacion = rep(NA_character_, n),
      trazabilidad = I(rep(list(.trazabilidad_vacia()), n)),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      columna = character(), tipo_hallazgo = character(), severidad = character(),
      descripcion = character(), evidencia = character(), sugerencia = character(),
      n_evaluados = numeric(), n_afectados = numeric(),
      unidad_conteo = character(),
      estado_reparacion = character(),
      trazabilidad = I(list()),
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(resumen_bloqueo) &&
      resumen_bloqueo$alcance$bloqueo_pares_fuera_alcance[[1L]] > 0) {
    hallazgo_bloqueo <- .nuevo_hallazgo(
      bloquear_por, "bloqueo_por_con_perdida", "sospechoso",
      "El bloqueo excluye pares cuya clave no coincide; no se evaluaron.",
      paste0(
        resumen_bloqueo$alcance$bloqueo_pares_fuera_alcance[[1L]],
        " pares quedaron fuera del alcance de `bloquear_por`.",
        " Las filas con NA forman un bloque propio."
      ),
      "Revisar la clave y no interpretar la ausencia de un par como evidencia de que no existe.",
      resumen_bloqueo$alcance$bloqueo_pares_alcanzables[[1L]] +
        resumen_bloqueo$alcance$bloqueo_pares_fuera_alcance[[1L]],
      resumen_bloqueo$alcance$bloqueo_pares_fuera_alcance[[1L]], "par"
    )
    hallazgos <- rbind(hallazgos, hallazgo_bloqueo)
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
    presupuesto_pares = presupuesto_pares,
    presupuesto_pares_aplica = usar_lsh || is.finite(presupuesto_pares),
    n_bloques = bloques$n_bloques,
    nucleos_usados = nucleos,
    metodo = metodo,
    p = p,
    comparacion_exhaustiva = !usar_lsh && length(indices) >= nrow(datos),
    muestreado = length(indices) < nrow(datos), truncado = mostrados < n_hallados,
    disponible = TRUE, razon = "", stringsAsFactors = FALSE
  )
  if (usar_lsh) {
    lsh_alcance_final <- lsh_alcance
    lsh_alcance_final$nucleos_usados <- NULL
    alcance <- cbind(alcance, lsh_alcance_final)
  }
  if (!is.null(resumen_bloqueo)) {
    alcance <- cbind(alcance, resumen_bloqueo$alcance)
  }
  estructura <- list(
    pares = pares, hallazgos = hallazgos, alcance = alcance,
    columnas = columnas, metodo = metodo, p = p, umbral = umbral,
    normalizacion = c(
      .normalizacion_resumen(normalizacion_resuelta),
      list(fusiones = textos$fusiones)
    ),
    disponible = TRUE, razon = "",
    proteccion_aplicada = proteger_datos_personales,
    estimacion = estimacion_resultado
  )
  if (!is.null(lotes_metadata)) estructura$lotes <- lotes_metadata
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
#' medida predeterminada es Jaro--Winkler (`"jw"`), con `p = 0.1`: Winkler
#' favorece coincidencias con el mismo prefijo y, por eso, premia una errata al
#' final mas que una al comienzo. El umbral predeterminado es `0.10`. Estos dos
#' valores cambian respecto de versiones anteriores y pueden cambiar los pares
#' informados; el umbral se eligio como una precision prudente, no como una
#' calibracion fina. Ambos argumentos se pueden cambiar.
#'
#' Los pares cuyos textos comparados son iguales despues de la normalizacion se
#' incluyen como `tipo_par = "exacto"`; los restantes son `"aproximado"`.
#' La clasificacion no depende de que una medida de distancia devuelva cero:
#' por ejemplo, `soundex` puede dar distancia cero para textos distintos.
#' Ningun par demuestra identidad.
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
#' Las comparaciones que delegan en `stringdist` usan `nucleos` hilos como
#' máximo. El valor por omisión es `getOption("lupa.nucleos", 2L)`, se limita a
#' los núcleos disponibles y se publica como `alcance$nucleos_usados`. Cambiar
#' la cantidad de hilos no cambia los pares ni los hallazgos, aunque sí puede
#' cambiar el tiempo de ejecución.
#' `p` es el factor de prefijo de Jaro--Winkler y admite valores entre 0 y
#' 0.25; sólo tiene efecto cuando `metodo = "jw"`. Para los demás métodos se
#' valida pero `stringdist` no lo utiliza.
#' El aviso interactivo señala esta perilla: subir `nucleos` puede acortar la
#' etapa de comparación, pero la ganancia depende de la máquina y de los datos.
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
#' La familia MinHash usa una permutación aleatoria inyectiva del vocabulario,
#' con una semilla interna fija, y restaura el estado global del generador de R;
#' por eso es determinista sin depender de `set.seed()` ni de `RNGkind()`. La
#' tabla de consulta de cada hash evita repetir trabajo para cada celda de la
#' matriz de q-gramas y hace que el resultado no dependa de cómo se numeraron
#' esos q-gramas. Antes de recorrer las bandas se toma una muestra interna y se
#' publica una estimación reproducible de candidatos. El cronómetro de la medida
#' se ejecuta durante al menos 50 ms y queda en `resultado$estimacion`, separado
#' de `alcance` y marcado como no determinista. Es un piso de la medida aislada
#' y no estima la firma, las cubetas ni el troceo; sólo se muestra como aviso en
#' una sesión interactiva.
#' Fuera de una sesión interactiva se señala una condición silenciosa de clase
#' `lupa_tiempo_lsh`, sólo en el camino LSH: no se imprime en `stdout` ni
#' `stderr`. Se puede capturar con
#' `withCallingHandlers(resultado <- detectar_duplicados_aproximados(...),
#' lupa_tiempo_lsh = function(c) { ... })`. Como hereda de `message`, un
#' `tryCatch(..., message = ...)` puede interrumpir la corrida y devolver el
#' valor del manejador; para observar sin interrumpir use `withCallingHandlers`
#' y, si corresponde, `invokeRestart("muffleMessage")`.
#' `presupuesto_pares` permite rechazar el recorrido antes de iniciarlo; sólo se
#' pregunta en una sesión interactiva. El diagnóstico de
#' Jaccard puede quedar limitado a los primeros
#' pares del recorrido determinista; `lsh_jaccard_alcance` lo dice literalmente
#' y no presenta ese prefijo como una muestra representativa.
#' Con `bloquear_por`, `alcance` separa los pares estructuralmente fuera del
#' bloqueo de una estimación de los candidatos que se habrían informado y que
#' quedaron fuera según una muestra determinista. La segunda cantidad es una
#' estimación, no una cuenta exacta.
#'
#' @param datos Tabla con una fila por entidad observada.
#' @param columnas Columnas atomicas a combinar. `NULL` aplica la seleccion
#'   automatica descrita arriba; no se incluyen matrices ni listas.
#' @param metodo Medida admitida por `stringdist::stringdistmatrix()`. Por
#'   defecto, `"jw"`.
#' @param p Factor de prefijo de Jaro--Winkler, entre 0 y 0.25. Por defecto
#'   `0.1`; sólo tiene efecto con `metodo = "jw"`.
#' @param umbral Distancia maxima para informar un par. Por defecto `0.10`.
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
#' @param nucleos Cantidad máxima de hilos que `stringdist` puede usar. Por
#'   defecto es `getOption("lupa.nucleos", 2L)`; `NULL` usa esa misma opción y
#'   un valor mayor que los núcleos disponibles se limita de forma segura. El
#'   resultado no depende de esta cantidad, pero el tiempo sí. El valor efectivo
#'   queda declarado en `alcance$nucleos_usados`.
#' @param normalizar Perfil de comparación. `TRUE` conserva el perfil
#'   predeterminado, `FALSE` desactiva sus pasos configurables, `"amplio"`
#'   activa puntuación, ligaduras y ancho, y [normalizacion()] permite declarar
#'   cada paso. Una lista nombrada puede resolver perfiles por columna. `NULL`
#'   hereda el perfil guardado en `perfil`; si no se recibe uno, usa `TRUE`.
#'   La normalización cambia sólo la representación usada para comparar, no los
#'   datos guardados. El umbral se aplica sobre esa cadena normalizada.
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
#' @param lsh_muestra_estimacion Cantidad máxima de pares de filas usados para
#'   estimar la proporción de candidatos, el tiempo del camino LSH y, si hay
#'   `bloquear_por`, la pérdida de candidatos del bloqueo. La muestra es
#'   interna, reproducible y su tamaño efectivo queda en `alcance`. Por defecto
#'   se intentan 400.000 pares.
#' @param presupuesto_pares Presupuesto de pares candidatos. Por
#'   defecto es `Inf`; si la estimación previa lo supera, una sesión no
#'   interactiva aborta antes del recorrido y una interactiva pregunta si se
#'   continúa. También limita la comparación exacta: allí el número de pares
#'   se conoce antes de empezar.
#' @param bloquear_por Nombre de una columna declarada por el usuario para
#'   restringir la comparación a filas con la misma clave. La clave no tiene
#'   significado incorporado en `lupa`; sus tamaños, ausentes y pares que
#'   quedan fuera se registran en `alcance`. Los `NA` forman un bloque propio.
#' @param lotes Si es `TRUE`, procesa la comparación exacta por pares de grupos
#'   de filas y guarda cada resultado parcial en RDS. Por omisión es `FALSE`;
#'   el camino LSH ya administra sus cubetas en memoria y no admite este modo.
#' @param tamano_lote Cantidad de filas por grupo de trabajo cuando `lotes` es
#'   `TRUE`. Por defecto, `1000`.
#' @param directorio_lotes Directorio base elegido por el usuario para los
#'   parciales. Si es `NULL`, se crea un subdirectorio dentro de `tempdir()`;
#'   nunca se escribe en el directorio de trabajo por omisión. Los parciales no
#'   son reanudables y se conservan al terminar para auditoría; si el directorio
#'   base fue elegido por el usuario, éste debe eliminarlos cuando ya no los
#'   necesite. Su ruta, cantidad y tamaño quedan en `resultado$lotes`.
#'
#' @return Lista de clase `duplicados_aproximados` con `pares`, `hallazgos`,
#'   `alcance`, `columnas`, `metodo`, `umbral`, `disponible`, `razon` y
#'   `estimacion`. `alcance` es reproducible; en el camino LSH,
#'   `estimacion$tiempo_determinista` es `FALSE` y reúne la velocidad, duración
#'   y tiempo de referencia medidos en esa corrida. El campo
#'   `estimacion$tiempo_estimado_etapa` indica que ese piso cubre sólo la
#'   comparación `stringdist`, no la firma, las cubetas ni el troceo. En el camino exacto o cuando
#'   no se puede comparar, `estimacion` es `NULL`. Si `lotes = TRUE`, se agrega
#'   `lotes` con el directorio, los archivos RDS, sus tamaños, el estado de
#'   completitud y `reanudable = FALSE`. El loteo cruza todos los grupos, por lo
#'   que no pierde pares; su resultado de `pares`, `hallazgos` y `alcance` es el
#'   mismo que el recorrido exacto sin lotes. Cada fila de `hallazgos` declara
#'   `n_evaluados`, `n_afectados` y `unidad_conteo`; los conteos desconocidos
#'   son `NA`, nunca cero.
#' @export
#' @seealso [perfilar()], [reportar()], [planificar_limpieza()]
#' @references Broder, A. Z. (1997) \doi{10.1109/SEQUEN.1997.666900}.
#'   On the resemblance and containment of documents. En *Compression and
#'   Complexity of Sequences*, 21--29.
#'   [Leskovec, J., Rajaraman, A. y Ullman, J. D. (2020)](http://www.mmds.org).
#'   *Mining of Massive Datasets* (3.ª ed.), capítulo 3.
#'
#' @examples
#' datos <- data.frame(
#'   nombre = c("Ana Perez", "Ana Peres", "Luis Diaz"),
#'   domicilio = c("Calle 1", "Calle 1", "Calle 9")
#' )
#' pares <- detectar_duplicados_aproximados(datos)
#' if (!pares$disponible) pares$razon
detectar_duplicados_aproximados <- function(
    datos, columnas = NULL, metodo = "jw", umbral = 0.10, p = 0.1,
    muestra = Inf, max_pares = 50000000L, max_resultados = 100L,
    normalizar = NULL, perfil = NULL, proteger_datos_personales = TRUE,
    bloque = 1000L, estrategia = "auto", lsh_bandas = 12L,
    lsh_filas = 3L, lsh_q = 3L, lsh_max_cubeta = 1000L,
    lsh_muestra_estimacion = 400000L, presupuesto_pares = Inf,
    bloquear_por = NULL, lotes = FALSE, tamano_lote = 1000L,
    directorio_lotes = NULL, nucleos = getOption("lupa.nucleos", 2L)) {
  if (!is.null(perfil) && (!inherits(perfil, "perfil") ||
      !identical(names(datos), perfil$columnas$columna))) {
    stop("`perfil` debe corresponder a las columnas de `datos`.", call. = FALSE)
  }
  normalizar <- .resolver_normalizacion(normalizar, perfil)
  if (!is.null(perfil)) {
    return(.detectar_duplicados_aproximados(
      datos, columnas, metodo, umbral, p, muestra, max_pares, max_resultados,
      normalizar, clasificacion = perfil$datos_personales,
      proteger_datos_personales = proteger_datos_personales, bloque = bloque,
      estrategia = estrategia, lsh_bandas = lsh_bandas, lsh_filas = lsh_filas,
      lsh_q = lsh_q, lsh_max_cubeta = lsh_max_cubeta,
      lsh_muestra_estimacion = lsh_muestra_estimacion,
      presupuesto_pares = presupuesto_pares, bloquear_por = bloquear_por,
      lotes = lotes, tamano_lote = tamano_lote,
      directorio_lotes = directorio_lotes, nucleos = nucleos
    ))
  }
  .detectar_duplicados_aproximados(
    datos, columnas, metodo, umbral, p, muestra, max_pares, max_resultados,
    normalizar, proteger_datos_personales = proteger_datos_personales,
    bloque = bloque, estrategia = estrategia, lsh_bandas = lsh_bandas,
    lsh_filas = lsh_filas, lsh_q = lsh_q, lsh_max_cubeta = lsh_max_cubeta,
    lsh_muestra_estimacion = lsh_muestra_estimacion,
    presupuesto_pares = presupuesto_pares, bloquear_por = bloquear_por,
    lotes = lotes, tamano_lote = tamano_lote,
    directorio_lotes = directorio_lotes, nucleos = nucleos
  )
}

#' Estimar el costo de una comparación de duplicados
#'
#' Construye las firmas y calcula el pronóstico de candidatos sin recorrer las
#' cubetas ni comparar los pares. Es una operación deliberada: la medición del
#' reloj queda en el resultado de esta función y no es necesaria para obtener
#' `alcance` reproducible en `detectar_duplicados_aproximados()`.
#'
#' En el camino LSH, `candidatos_previstos` es una estimación reproducible a
#' partir de una muestra de firmas. `tiempo_estimado_segundos` es un piso de la
#' medida aislada y no incluye firmas, cubetas ni troceo; sus campos de reloj
#' tienen `tiempo_determinista = FALSE`. Con `bloquear_por`, los pares entre
#' bloques no entran en el pronóstico y la pérdida estructural y estimada queda
#' en `alcance`. En el camino exacto, `candidatos_previstos` es la cantidad de
#' pares que se compararán, sujeta a `muestra`, `max_pares` y `bloquear_por`.
#'
#' La función no escribe archivos ni modifica el estado del generador de R.
#'
#' @inheritParams detectar_duplicados_aproximados
#' @param lotes Se acepta por simetría de la firma, pero la estimación no
#'   escribe parciales ni modifica el directorio indicado.
#' @param tamano_lote Se acepta por simetría; no cambia el pronóstico.
#' @param directorio_lotes Se acepta por simetría y no se crea ni se usa al
#'   estimar.
#' @return Lista de clase `estimacion_costo_lupa` con los campos de la
#'   estimación, `alcance`, `disponible` y `razon`.
#' @export
#' @seealso [detectar_duplicados_aproximados()]
#'
#' @examples
#' datos <- data.frame(
#'   nombre = c("Ana Perez", "Ana Peres", "Luis Diaz"),
#'   grupo = c("A", "A", "B")
#' )
#' if (requireNamespace("stringdist", quietly = TRUE)) {
#'   costo <- estimar_costo(datos, columnas = "nombre", estrategia = "lsh")
#'   costo$candidatos_previstos
#' }
estimar_costo <- function(
    datos, columnas = NULL, metodo = "jw", umbral = 0.10, p = 0.1,
    muestra = Inf, max_pares = 50000000L, max_resultados = 100L,
    normalizar = NULL, perfil = NULL, proteger_datos_personales = TRUE,
    bloque = 1000L, estrategia = "auto", lsh_bandas = 12L,
    lsh_filas = 3L, lsh_q = 3L, lsh_max_cubeta = 1000L,
    lsh_muestra_estimacion = 400000L, presupuesto_pares = Inf,
    bloquear_por = NULL, lotes = FALSE, tamano_lote = 1000L,
    directorio_lotes = NULL, nucleos = getOption("lupa.nucleos", 2L)) {
  if (!is.null(perfil) && (!inherits(perfil, "perfil") ||
      !identical(names(datos), perfil$columnas$columna))) {
    stop("`perfil` debe corresponder a las columnas de `datos`.", call. = FALSE)
  }
  normalizar <- .resolver_normalizacion(normalizar, perfil)
  clasificacion <- if (is.null(perfil)) NULL else perfil$datos_personales
  interno <- suppressMessages(.detectar_duplicados_aproximados(
    datos, columnas, metodo, umbral, p, muestra, max_pares, max_resultados,
    normalizar, clasificacion = clasificacion,
    proteger_datos_personales = proteger_datos_personales, bloque = bloque,
    estrategia = estrategia, lsh_bandas = lsh_bandas, lsh_filas = lsh_filas,
    lsh_q = lsh_q, lsh_max_cubeta = lsh_max_cubeta,
    lsh_muestra_estimacion = lsh_muestra_estimacion,
    presupuesto_pares = Inf, bloquear_por = bloquear_por,
    solo_estimacion = TRUE, lotes = FALSE, tamano_lote = tamano_lote,
    directorio_lotes = NULL, nucleos = nucleos
  ))
  if (inherits(interno, "duplicados_aproximados")) {
    candidatos <- if (nrow(interno$alcance)) {
      interno$alcance$n_pares_comparados[[1L]]
    } else NA_real_
    salida <- list(
      candidatos_previstos = candidatos,
      probabilidad_candidato_estimada = NA_real_,
      muestra_estimacion = 0L, vocabulario = NA_integer_,
      pares_benchmark = NA_integer_, tiempo_benchmark = NA_real_,
      velocidad_comparacion = NA_real_, tiempo_estimado_segundos = NA_real_,
      tiempo_estimado_etapa = NA_character_,
      tiempo_estimado_es_piso = FALSE, tiempo_determinista = TRUE,
      alcance = interno$alcance, disponible = interno$disponible,
      razon = interno$razon,
      normalizacion = interno$normalizacion
    )
  } else {
    salida <- c(
      interno$estimacion,
      list(alcance = interno$alcance, disponible = interno$disponible,
           razon = interno$razon, normalizacion = interno$normalizacion)
    )
  }
  class(salida) <- c("estimacion_costo_lupa", "list")
  salida
}
