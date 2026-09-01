# Estado interno para el recorrido por bloques. Estas funciones no forman parte
# de la API publica: la API conserva la pasada en memoria y sus argumentos.

.VERSION_CONTRATO_BLOQUES <- "1"
.VERSION_CANONICALIZACION_R <- "igualdad-R-1"
.MAX_ENTRADAS_DISTINTOS <- 5000L
.MAX_BYTES_ESTADO_BLOQUES <- 512 * 1024^2
.MAX_FILAS_TRAZABILIDAD_BLOQUES <- 1000L
.MAX_EJEMPLOS_BLOQUES <- 3L
.EVENTO_PRESION_MEMORIA <- "presion_memoria_proceso"

.validar_tope_bloques <- function(x, nombre, permitir_inf = TRUE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 ||
      (!permitir_inf && !is.finite(x)) ||
      (is.finite(x) && x != floor(x))) {
    stop("`", nombre, "` debe ser un entero positivo",
         if (permitir_inf) " o Inf" else "", ".", call. = FALSE)
  }
  if (is.infinite(x)) Inf else as.numeric(x)
}

.normalizar_configuracion_bloques <- function(columna, tipo,
                                              configuracion = list(),
                                              fuente_id = "memoria",
                                              snapshot_id = "memoria",
                                              universo_id = "tabla_completa",
                                              muestra_id = NULL,
                                              orden_id = "orden_entrada") {
  if (!is.list(configuracion)) {
    stop("`configuracion` debe ser una lista.", call. = FALSE)
  }
  list(
    columna = as.character(columna),
    tipo = as.character(tipo),
    configuracion = configuracion,
    fuente_id = as.character(fuente_id),
    snapshot_id = as.character(snapshot_id),
    universo_id = as.character(universo_id),
    muestra_id = if (is.null(muestra_id)) NULL else as.character(muestra_id),
    orden_id = as.character(orden_id),
    version = .VERSION_CONTRATO_BLOQUES,
    version_canonicalizacion = .VERSION_CANONICALIZACION_R
  )
}

.nuevo_entorno_acumulador <- function(familia, columna, tipo, configuracion,
                                      max_entradas = Inf,
                                      max_bytes = Inf,
                                      requiere_orden = FALSE,
                                      incluir_ausentes = FALSE) {
  acumulador <- new.env(parent = baseenv())
  class(acumulador) <- c("acumulador_bloques", "environment")
  acumulador$familia <- familia
  acumulador$configuracion <- configuracion
  acumulador$max_entradas <- max_entradas
  acumulador$max_bytes <- max_bytes
  acumulador$requiere_orden <- isTRUE(requiere_orden)
  if (familia %in% c("trazabilidad", "ejemplos", "muestra")) {
    acumulador$requiere_orden <- TRUE
  }
  acumulador$incluir_ausentes <- isTRUE(incluir_ausentes)
  acumulador$estado <- "iniciado"
  acumulador$fallo <- NULL
  acumulador$resultado <- NULL
  acumulador$ultimo_ordinal <- 0
  acumulador$intervalos <- list()
  acumulador
}

.iniciar_acumulador <- function(columna, tipo, familia = "conteos",
                                configuracion = list(),
                                fuente_id = "memoria",
                                snapshot_id = "memoria",
                                universo_id = "tabla_completa",
                                muestra_id = NULL,
                                orden_id = "orden_entrada",
                                max_entradas = Inf,
                                max_bytes = Inf,
                                requiere_orden = FALSE,
                                incluir_ausentes = FALSE) {
  if (familia %in% c("distintos", "hueco") && is.infinite(max_entradas)) {
    max_entradas <- .MAX_ENTRADAS_DISTINTOS
  }
  if (identical(familia, "filas_distintos") && is.infinite(max_entradas)) {
    max_entradas <- .MAX_ENTRADAS_DISTINTOS
  }
  if (identical(familia, "centinela") && is.infinite(max_entradas)) {
    max_entradas <- .MAX_ENTRADAS_DISTINTOS
  }
  if (identical(familia, "ejemplos") && is.infinite(max_entradas)) {
    max_entradas <- .MAX_ENTRADAS_DISTINTOS
  }
  if (is.infinite(max_bytes)) max_bytes <- .MAX_BYTES_ESTADO_BLOQUES
  max_entradas <- .validar_tope_bloques(max_entradas, "max_entradas")
  max_bytes <- .validar_tope_bloques(max_bytes, "max_bytes")
  configuracion <- .normalizar_configuracion_bloques(
    columna, tipo, configuracion, fuente_id, snapshot_id, universo_id,
    muestra_id, orden_id
  )
  acumulador <- .nuevo_entorno_acumulador(
    familia, columna, tipo, configuracion, max_entradas, max_bytes,
    requiere_orden, incluir_ausentes
  )
  es_indice <- familia %in% c("trazabilidad", "ejemplos", "muestra", "lsh")
  orden_valido <- length(configuracion$orden_id) == 1L &&
    !is.na(configuracion$orden_id) && nzchar(configuracion$orden_id)
  snapshot_valido <- length(configuracion$snapshot_id) == 1L &&
    !is.na(configuracion$snapshot_id) && nzchar(configuracion$snapshot_id)
  if (es_indice && (!orden_valido || !snapshot_valido)) {
    acumulador <- .marcar_fallo_acumulador(
      acumulador,
      if (!orden_valido) {
        "orden_estable_no_disponible:orden_id_ausente"
      } else {
        "snapshot_inestable:snapshot_id_ausente"
      }
    )
  }
  acumulador$estado_familia <- switch(
    familia,
    conteos = list(
      n = 0, n_aplicables = 0, n_no_aplica = 0,
      n_indeterminada = 0, n_faltantes = 0, n_validos = 0,
      n_nan = 0, n_infinito_positivo = 0, n_infinito_negativo = 0,
      n_ceros = 0, n_negativos = 0
    ),
    cuantitativos = list(
      n = 0, media = 0, m2 = 0, minimo = Inf, maximo = -Inf,
      n_nan = 0, n_infinito_positivo = 0, n_infinito_negativo = 0,
      n_ceros = 0, n_negativos = 0, contar_signos = TRUE
    ),
    longitudes = list(n = 0, suma = 0, minimo = Inf, maximo = -Inf),
    distintos = list(
      representantes = NULL, frecuencias = numeric(), primeros = numeric(),
      n = 0, n_validos = 0, n_claves_omitidas = 0,
      truncado = FALSE, causa_tope = NULL
    ),
    hueco = list(
      representantes = NULL, frecuencias = numeric(), primeros = numeric(),
      n = 0, n_validos = 0, n_claves_omitidas = 0,
      truncado = FALSE, causa_tope = NULL
    ),
    filas_distintos = list(
      representantes = NULL, frecuencias = numeric(), primeros = numeric(),
      grupos = integer(), n = 0, n_claves_omitidas = 0,
      truncado = FALSE, causa_tope = NULL
    ),
    outliers = list(n = 0, n_outliers = 0L, limite_inferior = NA_real_,
                    limite_superior = NA_real_),
    centinela = list(
      n = 0, n_outliers = 0L, representantes = NULL,
      frecuencias = numeric(), n_claves_omitidas = 0L,
      truncado = FALSE, causa_tope = NULL,
      limite_inferior = NA_real_, limite_superior = NA_real_
    ),
    trazabilidad = list(
      indices = integer(), total = 0L, truncado = FALSE,
      indices_omitidos = 0L, causa_tope = NULL
    ),
    ejemplos = list(
      representantes = NULL, frecuencias = integer(), primeros = numeric(),
      valores = list(), ordinales = list(), n = 0L, n_filas = 0L,
      n_claves_omitidas = 0L,
      truncado = FALSE, causa_tope = NULL
    ),
    muestra = list(
      indices = integer(), n = 0L, n_total = NA_real_,
      limite = NA_real_
    ),
    lsh = list(
      directorio = NULL, version_derrame = NULL, fase = "iniciada",
      bloques = list(), runs_qgramas = character(),
      runs_diccionario = character(), runs_firmas = character(),
      vocabulario = 0, checksum_diccionario = NA_character_,
      checksum_derrame = NA_character_, bytes_derrame = 0,
      factor_pico = 30, factor_pico_fuente = NULL,
      residentes_lsh = list(
        buffers_runs = 0, cache_diccionario = 0,
        estado_fila = 0, otros = 0
      ),
      bytes_residentes_lsh = 0, maximo_residentes_lsh = 0,
      maximo_cache_diccionario = 0, maximo_intervalo = 0,
      rss_maximo = NA_real_, salida = NULL, derrame = NULL
    ),
    aritmetica = list(
      n = 0L, n_cumplen = 0L, n_incumplen = 0L,
      limite_inferior = NA_real_, limite_superior = NA_real_,
      k = NA_real_, tolerancia = NA_real_
    ),
    generica = list(n = 0L),
    stop("Familia de acumulador desconocida: `", familia, "`.", call. = FALSE)
  )
  if (familia %in% c("outliers", "centinela")) {
    acumulador$estado_familia$limite_inferior <-
      as.numeric(configuracion$configuracion$limite_inferior %||% NA_real_)
    acumulador$estado_familia$limite_superior <-
      as.numeric(configuracion$configuracion$limite_superior %||% NA_real_)
  }
  if (identical(familia, "aritmetica")) {
    acumulador$estado_familia$k <-
      as.numeric(configuracion$configuracion$k %||% NA_real_)
    acumulador$estado_familia$tolerancia <-
      as.numeric(configuracion$configuracion$tolerancia %||% NA_real_)
  }
  if (identical(familia, "muestra")) {
    acumulador$estado_familia$n_total <- as.numeric(
      configuracion$configuracion$n_total %||% NA_real_
    )
    acumulador$estado_familia$limite <- as.numeric(
      configuracion$configuracion$limite %||% NA_real_
    )
  }
  if (identical(familia, "lsh") && exists(
      ".lsh_iniciar_estado", mode = "function")) {
    acumulador <- .lsh_iniciar_estado(acumulador)
  }
  acumulador
}

.extraer_bloque <- function(bloque) {
  if (inherits(bloque, "data.frame")) {
    valores <- bloque
    inicio <- NA_real_
    fin <- NA_real_
    aplicable <- NULL
    aplicable_declarada <- FALSE
    ordinales <- NULL
    claves <- NULL
    ejemplos <- NULL
    seleccion <- NULL
    indices_fila <- NULL
    bloqueos <- NULL
  } else if (is.list(bloque) && !is.null(names(bloque)) &&
      "valores" %in% names(bloque)) {
    valores <- bloque$valores
    inicio <- bloque$ordinal_inicio
    fin <- bloque$ordinal_fin
    if (is.null(inicio)) inicio <- NA_real_
    if (is.null(fin)) fin <- NA_real_
    aplicable <- bloque$aplicable
    aplicable_declarada <- !is.null(aplicable)
    ordinales <- bloque$ordinales
    claves <- bloque$claves
    ejemplos <- bloque$ejemplos
    seleccion <- bloque$seleccion
    indices_fila <- bloque$indices_fila
    bloqueos <- bloque$bloqueos
  } else {
    valores <- bloque
    inicio <- NA_real_
    fin <- NA_real_
    aplicable <- NULL
    aplicable_declarada <- FALSE
    ordinales <- NULL
    claves <- NULL
    ejemplos <- NULL
    seleccion <- NULL
    indices_fila <- NULL
    bloqueos <- NULL
  }
  n_filas <- if (inherits(valores, "data.frame")) nrow(valores) else {
    length(valores)
  }
  if (is.null(aplicable)) aplicable <- rep(TRUE, n_filas)
  if (!is.logical(aplicable) || length(aplicable) != n_filas) {
    stop("La mascara `aplicable` debe ser logica y tener el largo del bloque.",
         call. = FALSE)
  }
  if (!is.null(ordinales) && length(ordinales) != n_filas) {
    stop("`ordinales` debe tener el largo del bloque.", call. = FALSE)
  }
  if (!is.null(claves) && length(claves) != n_filas) {
    stop("`claves` debe tener el largo del bloque.", call. = FALSE)
  }
  if (!is.null(ejemplos) && length(ejemplos) != n_filas) {
    stop("`ejemplos` debe tener el largo del bloque.", call. = FALSE)
  }
  list(valores = valores, ordinal_inicio = inicio, ordinal_fin = fin,
       ordinales = ordinales, claves = claves, ejemplos = ejemplos,
       seleccion = seleccion, indices_fila = indices_fila, bloqueos = bloqueos,
       aplicable = aplicable, aplicable_declarada = aplicable_declarada)
}

.ordinales_bloque <- function(bloque) {
  n <- if (inherits(bloque$valores, "data.frame")) nrow(bloque$valores) else {
    length(bloque$valores)
  }
  if (!is.null(bloque$ordinales)) return(as.numeric(bloque$ordinales))
  if (!n) return(numeric())
  seq.int(bloque$ordinal_inicio, length.out = n)
}

.registrar_intervalo_bloque <- function(acumulador, bloque) {
  n <- if (inherits(bloque$valores, "data.frame")) nrow(bloque$valores) else {
    length(bloque$valores)
  }
  inicio <- bloque$ordinal_inicio
  fin <- bloque$ordinal_fin
  ordinales <- bloque$ordinales
  if (!is.null(ordinales) && length(ordinales) &&
      all(is.finite(ordinales)) && all(ordinales == floor(ordinales))) {
    if (is.na(inicio)) inicio <- min(ordinales)
    if (is.na(fin)) fin <- max(ordinales)
  }
  if (is.na(inicio)) inicio <- acumulador$ultimo_ordinal + 1
  if (is.na(fin)) fin <- inicio + n - 1
  if (n == 0L) {
    fin <- inicio - 1
  }
  if (length(inicio) != 1L || length(fin) != 1L || is.na(inicio) ||
      is.na(fin) || inicio < 1 || fin < inicio - 1 ||
      (n && is.null(ordinales) && fin - inicio + 1 != n)) {
    stop("El bloque no tiene ordinales globales validos.", call. = FALSE)
  }
  if (!is.null(ordinales) && length(ordinales)) {
    if (any(!is.finite(ordinales)) || any(ordinales != floor(ordinales)) ||
        any(ordinales < 1) || anyDuplicated(ordinales) ||
        any(diff(ordinales) <= 0) || min(ordinales) < inicio ||
        max(ordinales) > fin) {
      stop("El bloque no tiene ordinales globales validos.", call. = FALSE)
    }
  }
  if (acumulador$requiere_orden && length(acumulador$intervalos)) {
    anterior <- acumulador$intervalos[[length(acumulador$intervalos)]]
    if (inicio <= anterior[[2L]]) {
      stop("Los intervalos globales del acumulador se solapan.", call. = FALSE)
    }
  }
  acumulador$intervalos[[length(acumulador$intervalos) + 1L]] <-
    c(inicio, fin)
  acumulador$ultimo_ordinal <- max(acumulador$ultimo_ordinal, fin)
  bloque$ordinal_inicio <- as.numeric(inicio)
  bloque$ordinal_fin <- as.numeric(fin)
  bloque
}

.comparar_ausentes_R <- function(x, y) {
  x_na <- isTRUE(tryCatch(is.na(x), error = function(e) FALSE))
  y_na <- isTRUE(tryCatch(is.na(y), error = function(e) FALSE))
  if (!x_na && !y_na) return(NULL)
  if (x_na != y_na) return(FALSE)
  x_nan <- isTRUE(tryCatch(is.nan(x), error = function(e) FALSE))
  y_nan <- isTRUE(tryCatch(is.nan(y), error = function(e) FALSE))
  identical(x_nan, y_nan)
}

.iguales_R <- function(x, y) {
  ausentes <- .comparar_ausentes_R(x, y)
  if (!is.null(ausentes)) return(ausentes)
  resultado <- tryCatch(x == y, error = function(e) e)
  if (inherits(resultado, "condition") || length(resultado) != 1L ||
      is.na(resultado)) return(NULL)
  isTRUE(resultado)
}

.match_R <- function(valores, representantes) {
  if (!length(valores)) return(integer())
  if (!length(representantes)) return(integer(length(valores)))
  normalizar_dobles <- function(x) {
    if (is.double(x) && is.null(attr(x, "class"))) {
      ceros <- !is.na(x) & !is.nan(x) & x == 0
      if (any(ceros)) x[ceros] <- 0
      nans <- is.nan(x)
      if (any(nans)) x[nans] <- NaN
    }
    x
  }
  valores <- normalizar_dobles(valores)
  representantes <- normalizar_dobles(representantes)
  resultado <- tryCatch(
    match(valores, representantes, nomatch = 0L),
    error = function(e) NULL
  )
  if (!is.null(resultado) && length(resultado) == length(valores) &&
      !anyNA(resultado)) return(as.integer(resultado))
  salida <- integer(length(valores))
  for (i in seq_along(valores)) {
    for (j in seq_along(representantes)) {
      igual <- .iguales_R(valores[i], representantes[j])
      if (is.null(igual)) return(NULL)
      if (igual) {
        salida[[i]] <- j
        break
      }
    }
  }
  salida
}

.clave_identidad_R <- function(x) {
  if (length(x) != 1L) {
    stop("La clave de identidad requiere un solo valor.", call. = FALSE)
  }
  es_integer64 <- inherits(x, "integer64")
  es_na <- isTRUE(tryCatch(is.na(x), error = function(e) FALSE))
  es_nan <- isTRUE(tryCatch(is.nan(x), error = function(e) FALSE))
  clase <- class(x)
  tipo <- if (length(clase)) paste(clase, collapse = "/") else typeof(x)
  if (es_na && es_nan) {
    return(list(tipo = tipo, clase = clase, categoria = "NaN",
                valor = NA_real_))
  }
  if (es_na) {
    return(list(tipo = tipo, clase = clase, categoria = "NA", valor = x))
  }
  if (!es_integer64 && is.double(x) && isTRUE(x == 0)) x <- 0
  list(tipo = tipo, clase = clase, categoria = "valor", valor = x)
}

.append_typed_R <- function(x, valor) {
  if (!length(x)) return(valor)
  tryCatch(c(x, valor), error = function(e) NULL)
}

.bytes_estado_acumulador <- function(acumulador) {
  if (!inherits(acumulador, "acumulador_bloques")) return(NA_real_)
  if (identical(acumulador$familia, "lsh") && exists(
      ".lsh_bytes_retenidos_acumulador", mode = "function")) {
    return(.lsh_bytes_retenidos_acumulador(acumulador))
  }
  objetos <- list(
    configuracion = acumulador$configuracion,
    estado = acumulador$estado_familia,
    resultado = acumulador$resultado
  )
  as.numeric(utils::object.size(objetos))
}

.marcar_fallo_acumulador <- function(acumulador, motivo) {
  acumulador$estado <- "no_disponible"
  acumulador$fallo <- as.character(motivo)
  acumulador
}

.entrada_atomica_acumulador <- function(x) {
  is.atomic(x) && is.null(dim(x))
}

.marcar_tope_acumulador <- function(acumulador, causa) {
  acumulador$estado_familia$truncado <- TRUE
  acumulador$estado_familia$causa_tope <- causa
  acumulador$estado <- "truncado"
  acumulador
}

.absorber_conteos <- function(acumulador, bloque) {
  x <- bloque$valores
  aplicable <- bloque$aplicable
  n <- length(x)
  indeterminada <- is.na(aplicable)
  aplica <- !indeterminada & aplicable
  estado <- acumulador$estado_familia
  estado$n <- estado$n + n
  estado$n_aplicables <- estado$n_aplicables + sum(aplica)
  estado$n_no_aplica <- estado$n_no_aplica + sum(!indeterminada & !aplicable)
  estado$n_indeterminada <- estado$n_indeterminada + sum(indeterminada)
  if (!any(aplica)) {
    acumulador$estado_familia <- estado
    return(acumulador)
  }
  valores <- x[aplica]
  ausentes <- is.na(valores)
  estado$n_faltantes <- estado$n_faltantes + sum(ausentes)
  estado$n_validos <- estado$n_validos + sum(!ausentes)
  if (is.numeric(valores)) {
    estado$n_nan <- estado$n_nan + sum(is.nan(valores))
    estado$n_infinito_positivo <- estado$n_infinito_positivo +
      sum(is.infinite(valores) & valores > 0, na.rm = TRUE)
    estado$n_infinito_negativo <- estado$n_infinito_negativo +
      sum(is.infinite(valores) & valores < 0, na.rm = TRUE)
    finitos <- valores[is.finite(valores)]
    estado$n_ceros <- estado$n_ceros + sum(finitos == 0)
    estado$n_negativos <- estado$n_negativos + sum(finitos < 0)
  }
  acumulador$estado_familia <- estado
  acumulador
}

.absorber_cuantitativos <- function(acumulador, bloque) {
  valores <- bloque$valores
  aplicable <- bloque$aplicable
  if (!is.numeric(valores)) {
    return(.marcar_fallo_acumulador(
      acumulador, "valores_cuantitativos_no_numericos"
    ))
  }
  valores <- valores[!is.na(aplicable) & aplicable]
  estado <- acumulador$estado_familia
  if (!length(valores)) {
    acumulador$estado_familia <- estado
    return(acumulador)
  }
  estado$n_nan <- estado$n_nan + sum(is.nan(valores))
  estado$n_infinito_positivo <- estado$n_infinito_positivo +
    sum(is.infinite(valores) & valores > 0, na.rm = TRUE)
  estado$n_infinito_negativo <- estado$n_infinito_negativo +
    sum(is.infinite(valores) & valores < 0, na.rm = TRUE)
  finitos <- valores[is.finite(valores)]
  if (!length(finitos)) {
    acumulador$estado_familia <- estado
    return(acumulador)
  }
  n_bloque <- length(finitos)
  media_bloque <- mean(finitos)
  diferencias <- finitos - media_bloque
  m2_bloque <- sum(diferencias * diferencias)
  if (estado$n == 0L) {
    estado$media <- media_bloque
    estado$m2 <- m2_bloque
    estado$n <- n_bloque
  } else {
    total <- estado$n + n_bloque
    diferencia <- media_bloque - estado$media
    estado$m2 <- estado$m2 + m2_bloque +
      diferencia * diferencia * estado$n * n_bloque / total
    estado$media <- estado$media + diferencia * n_bloque / total
    estado$n <- total
  }
  estado$minimo <- min(estado$minimo, min(finitos))
  estado$maximo <- max(estado$maximo, max(finitos))
  if (isTRUE(estado$contar_signos)) {
    estado$n_ceros <- estado$n_ceros + sum(finitos == 0)
    estado$n_negativos <- estado$n_negativos + sum(finitos < 0)
  }
  acumulador$estado_familia <- estado
  acumulador
}

.absorber_longitudes <- function(acumulador, bloque) {
  x <- bloque$valores
  if (!is.character(x) && !is.factor(x)) {
    return(.marcar_fallo_acumulador(acumulador, "longitudes_no_textuales"))
  }
  longitudes <- nchar(as.character(x), type = "chars", allowNA = TRUE)
  longitudes <- longitudes[!is.na(bloque$aplicable) & bloque$aplicable]
  longitudes <- longitudes[!is.na(longitudes)]
  estado <- acumulador$estado_familia
  if (length(longitudes)) {
    estado$n <- estado$n + length(longitudes)
    estado$suma <- estado$suma + sum(longitudes)
    estado$minimo <- min(estado$minimo, min(longitudes))
    estado$maximo <- max(estado$maximo, max(longitudes))
  }
  acumulador$estado_familia <- estado
  acumulador
}

.absorber_outliers <- function(acumulador, bloque) {
  valores <- bloque$valores
  if (!is.numeric(valores)) {
    return(.marcar_fallo_acumulador(
      acumulador, "valores_outliers_no_numericos"
    ))
  }
  aplica <- !is.na(bloque$aplicable) & bloque$aplicable
  valores <- valores[aplica]
  finitos <- valores[is.finite(valores)]
  estado <- acumulador$estado_familia
  estado$n <- estado$n + length(finitos)
  if (length(finitos)) {
    fuera <- finitos < estado$limite_inferior |
      finitos > estado$limite_superior
    estado$n_outliers <- estado$n_outliers + sum(fuera)
  }
  acumulador$estado_familia <- estado
  acumulador
}

.absorber_centinela <- function(acumulador, bloque) {
  valores <- bloque$valores
  if (!is.numeric(valores)) {
    return(.marcar_fallo_acumulador(
      acumulador, "valores_centinela_no_numericos"
    ))
  }
  aplica <- !is.na(bloque$aplicable) & bloque$aplicable
  valores <- valores[aplica]
  finitos <- valores[is.finite(valores)]
  estado <- acumulador$estado_familia
  estado$n <- estado$n + length(finitos)
  if (!length(finitos) || isTRUE(estado$truncado)) {
    acumulador$estado_familia <- estado
    return(acumulador)
  }
  fuera <- finitos < estado$limite_inferior |
    finitos > estado$limite_superior
  estado$n_outliers <- estado$n_outliers + sum(fuera)
  candidatos <- finitos[fuera]
  if (!length(candidatos)) {
    acumulador$estado_familia <- estado
    return(acumulador)
  }
  for (valor in candidatos) {
    posicion <- .match_R(valor, estado$representantes)
    if (is.null(posicion)) {
      return(.marcar_fallo_acumulador(
        acumulador, "igualdad_s3_no_reproducible"
      ))
    }
    posicion <- posicion[[1L]]
    if (posicion > 0L) {
      estado$frecuencias[[posicion]] <-
        estado$frecuencias[[posicion]] + 1L
      next
    }
    if (length(estado$representantes) >= acumulador$max_entradas) {
      estado$n_claves_omitidas <- estado$n_claves_omitidas + 1L
      acumulador$estado_familia <- estado
      return(.marcar_tope_acumulador(acumulador, "entradas"))
    }
    estado$representantes <- .append_typed_R(
      estado$representantes, valor
    )
    if (is.null(estado$representantes)) {
      return(.marcar_fallo_acumulador(
        acumulador, "igualdad_s3_no_reproducible"
      ))
    }
    estado$frecuencias <- c(estado$frecuencias, 1L)
    acumulador$estado_familia <- estado
    if (.bytes_estado_acumulador(acumulador) > acumulador$max_bytes) {
      ultimo <- length(estado$representantes)
      estado$representantes <- if (ultimo == 1L) NULL else {
        estado$representantes[-ultimo]
      }
      estado$frecuencias <- utils::head(estado$frecuencias, -1L)
      estado$n_claves_omitidas <- estado$n_claves_omitidas + 1L
      acumulador$estado_familia <- estado
      return(.marcar_tope_acumulador(acumulador, "bytes"))
    }
  }
  acumulador$estado_familia <- estado
  acumulador
}

# Las familias de índice no pueden usar la posición local del bloque como
# evidencia. Un bloque muestreado, por ejemplo, puede contener las filas 10 y
# 1001 y aun así representar un intervalo continuo de la fuente. La máscara se
# resuelve sobre las filas que llegaron y los ordinales se llevan aparte.
.seleccion_indices_bloque <- function(acumulador, bloque) {
  n <- length(.ordinales_bloque(bloque))
  seleccion <- bloque$seleccion
  if (is.null(seleccion) && !is.null(bloque$indices_fila)) {
    candidatos <- bloque$indices_fila
    if (is.logical(candidatos)) {
      seleccion <- candidatos
    } else {
      candidatos <- as.numeric(candidatos)
      ordinales <- .ordinales_bloque(bloque)
      if (!length(candidatos)) {
        seleccion <- rep(FALSE, n)
      } else if (all(candidatos %in% ordinales)) {
        seleccion <- ordinales %in% candidatos
      } else if (all(candidatos == floor(candidatos)) &&
                 all(candidatos >= 1L & candidatos <= n)) {
        seleccion <- seq_len(n) %in% candidatos
      } else {
        return(NULL)
      }
    }
  }
  if (is.null(seleccion) && isTRUE(bloque$aplicable_declarada)) {
    seleccion <- bloque$aplicable
  }
  if (is.null(seleccion)) {
    funcion <- acumulador$configuracion$configuracion$predicado
    if (is.function(funcion)) {
      n <- length(.ordinales_bloque(bloque))
      respuestas <- lapply(seq_len(n), function(i) {
        fila <- if (inherits(bloque$valores, "data.frame")) {
          bloque$valores[i, , drop = FALSE]
        } else {
          bloque$valores[[i]]
        }
        tryCatch(funcion(fila), error = function(e) e)
      })
      if (any(vapply(respuestas, inherits, logical(1L),
                     what = "condition")) ||
          any(!vapply(respuestas, is.logical, logical(1L)) ||
              vapply(respuestas, length, integer(1L)) != 1L)) {
        return(NULL)
      }
      seleccion <- vapply(respuestas, function(respuesta) respuesta[[1L]],
                          logical(1L))
    }
  }
  if (is.null(seleccion)) seleccion <- rep(TRUE, n)
  if (!is.logical(seleccion) || length(seleccion) != n) return(NULL)
  !is.na(seleccion) & seleccion
}

.absorber_trazabilidad <- function(acumulador, bloque) {
  seleccion <- .seleccion_indices_bloque(acumulador, bloque)
  if (is.null(seleccion)) {
    return(.marcar_fallo_acumulador(
      acumulador, "trazabilidad_predicado_no_logico"
    ))
  }
  ordinales <- .ordinales_bloque(bloque)[seleccion]
  estado <- acumulador$estado_familia
  estado$total <- estado$total + length(ordinales)
  if (!length(ordinales)) {
    acumulador$estado_familia <- estado
    return(acumulador)
  }
  if (isTRUE(estado$truncado)) {
    estado$indices_omitidos <- estado$indices_omitidos + length(ordinales)
    acumulador$estado_familia <- estado
    return(acumulador)
  }
  disponibles <- acumulador$max_entradas - length(estado$indices)
  if (is.finite(disponibles) && disponibles < length(ordinales)) {
    conservar <- max(0L, as.integer(disponibles))
    if (conservar) {
      estado$indices <- c(estado$indices, utils::head(ordinales, conservar))
    }
    estado$indices_omitidos <- estado$indices_omitidos +
      length(ordinales) - conservar
    estado$truncado <- TRUE
    estado$causa_tope <- "entradas"
    acumulador$estado_familia <- estado
    return(acumulador)
  }
  estado$indices <- c(estado$indices, ordinales)
  acumulador$estado_familia <- estado
  if (.bytes_estado_acumulador(acumulador) > acumulador$max_bytes) {
    # El índice que cruza el tope no forma parte de la evidencia retenida. El
    # total sigue siendo exacto y el sobre publica que los índices quedaron
    # truncados, en vez de presentar el prefijo como si fuera completo.
    estado$indices <- utils::head(estado$indices, -1L)
    estado$indices_omitidos <- estado$indices_omitidos + 1L
    estado$truncado <- TRUE
    estado$causa_tope <- "bytes"
    acumulador$estado_familia <- estado
  }
  acumulador
}

.valor_ejemplo_igual <- function(x, y) {
  igual <- .iguales_R(x, y)
  isTRUE(igual)
}

.primeros_ejemplos_ordinales <- function(valores, ordinales, limite) {
  if (!length(valores)) {
    return(list(valores = list(), ordinales = numeric()))
  }
  orden <- order(ordinales)
  valores <- valores[orden]
  ordinales <- ordinales[orden]
  retenidos <- list()
  ordinales_retenidos <- numeric()
  for (i in seq_along(valores)) {
    if (any(vapply(retenidos, .valor_ejemplo_igual, logical(1L),
                   y = valores[[i]]))) next
    retenidos[[length(retenidos) + 1L]] <- valores[[i]]
    ordinales_retenidos <- c(ordinales_retenidos, ordinales[[i]])
    if (length(retenidos) >= limite) break
  }
  list(valores = retenidos, ordinales = ordinales_retenidos)
}

.absorber_ejemplos <- function(acumulador, bloque) {
  ordinales <- .ordinales_bloque(bloque)
  aplica <- !is.na(bloque$aplicable) & bloque$aplicable
  claves <- bloque$claves
  if (is.null(claves)) {
    funcion <- acumulador$configuracion$configuracion$funcion_clave
    claves <- if (is.function(funcion)) {
      tryCatch(funcion(bloque$valores), error = function(e) NULL)
    } else bloque$valores
  }
  if (is.null(claves) || length(claves) != length(ordinales)) {
    return(.marcar_fallo_acumulador(
      acumulador, "ejemplos_clave_no_reproducible"
    ))
  }
  valores <- bloque$ejemplos
  if (is.null(valores)) valores <- bloque$valores
  if (length(valores) != length(ordinales)) {
    return(.marcar_fallo_acumulador(
      acumulador, "ejemplos_valores_no_reproducibles"
    ))
  }
  estado <- acumulador$estado_familia
  estado$n_filas <- estado$n_filas + length(ordinales)
  posiciones <- which(aplica)
  estado$n <- estado$n + length(posiciones)
  if (!length(posiciones)) {
    acumulador$estado_familia <- estado
    return(acumulador)
  }
  if (isTRUE(estado$truncado)) {
    acumulador$estado_familia <- estado
    return(acumulador)
  }
  limite_ejemplos <- acumulador$configuracion$configuracion$max_ejemplos %||%
    .MAX_EJEMPLOS_BLOQUES
  for (i in posiciones) {
    clave <- claves[[i]]
    if (isTRUE(tryCatch(is.na(clave), error = function(e) FALSE))) next
    posicion <- .match_R(clave, estado$representantes)
    if (is.null(posicion)) {
      return(.marcar_fallo_acumulador(
        acumulador, "igualdad_s3_no_reproducible"
      ))
    }
    posicion <- posicion[[1L]]
    if (!posicion) {
      if (length(estado$representantes) >= acumulador$max_entradas) {
        estado$n_claves_omitidas <- estado$n_claves_omitidas + 1L
        estado$truncado <- TRUE
        estado$causa_tope <- "entradas"
        acumulador$estado <- "truncado"
        break
      }
      estado$representantes <- .append_typed_R(
        estado$representantes, clave
      )
      if (is.null(estado$representantes)) {
        return(.marcar_fallo_acumulador(
          acumulador, "igualdad_s3_no_reproducible"
        ))
      }
      estado$frecuencias <- c(estado$frecuencias, 1L)
      estado$primeros <- c(estado$primeros, ordinales[[i]])
      es_ausente <- isTRUE(tryCatch(
        is.na(valores[[i]]), error = function(e) FALSE
      ))
      estado$valores[[length(estado$valores) + 1L]] <- if (es_ausente) {
        list()
      } else list(valores[[i]])
      estado$ordinales[[length(estado$ordinales) + 1L]] <- if (es_ausente) {
        numeric()
      } else ordinales[[i]]
      posicion <- length(estado$representantes)
    } else {
      estado$frecuencias[[posicion]] <-
        estado$frecuencias[[posicion]] + 1L
      vistos <- estado$valores[[posicion]]
      if (length(vistos) < limite_ejemplos &&
          !any(vapply(vistos, .valor_ejemplo_igual, logical(1L),
                      y = valores[[i]]))) {
        estado$valores[[posicion]] <- c(vistos, list(valores[[i]]))
        estado$ordinales[[posicion]] <- c(
          estado$ordinales[[posicion]], ordinales[[i]]
        )
      }
    }
    acumulador$estado_familia <- estado
    if (.bytes_estado_acumulador(acumulador) > acumulador$max_bytes) {
      ultimo <- length(estado$representantes)
      estado$representantes <- if (ultimo == 1L) NULL else {
        estado$representantes[-ultimo]
      }
      estado$frecuencias <- utils::head(estado$frecuencias, -1L)
      estado$primeros <- utils::head(estado$primeros, -1L)
      estado$valores <- utils::head(estado$valores, -1L)
      estado$ordinales <- utils::head(estado$ordinales, -1L)
      estado$n_claves_omitidas <- estado$n_claves_omitidas + 1L
      estado$truncado <- TRUE
      estado$causa_tope <- "bytes"
      acumulador$estado <- "truncado"
      break
    }
  }
  acumulador$estado_familia <- estado
  acumulador
}

.absorber_muestra <- function(acumulador, bloque) {
  ordinales <- .ordinales_bloque(bloque)
  objetivo <- acumulador$configuracion$configuracion$indices_objetivo
  if (is.null(objetivo)) {
    return(.marcar_fallo_acumulador(acumulador, "muestra_n_desconocido"))
  }
  seleccion <- ordinales %in% objetivo
  estado <- acumulador$estado_familia
  elegidos <- ordinales[seleccion]
  estado$indices <- c(estado$indices, elegidos)
  estado$n <- length(estado$indices)
  acumulador$estado_familia <- estado
  acumulador
}

.absorber_aritmetica <- function(acumulador, bloque) {
  datos <- bloque$valores
  if (!inherits(datos, "data.frame") || ncol(datos) < 2L) {
    return(.marcar_fallo_acumulador(
      acumulador, "valores_aritmeticos_no_pareados"
    ))
  }
  base <- suppressWarnings(as.numeric(datos[[1L]]))
  respuesta <- suppressWarnings(as.numeric(datos[[2L]]))
  if (length(base) != length(respuesta)) {
    return(.marcar_fallo_acumulador(
      acumulador, "valores_aritmeticos_no_pareados"
    ))
  }
  aplica <- !is.na(bloque$aplicable) & bloque$aplicable
  comparables <- aplica & is.finite(base) & is.finite(respuesta)
  estado <- acumulador$estado_familia
  if (any(comparables)) {
    esperado <- base[comparables] * estado$k
    cumple <- .dentro_tolerancia_bloques(
      respuesta[comparables], esperado, estado$tolerancia
    )
    estado$n <- estado$n + sum(comparables)
    estado$n_cumplen <- estado$n_cumplen + sum(cumple)
    estado$n_incumplen <- estado$n_incumplen + sum(!cumple)
  }
  acumulador$estado_familia <- estado
  acumulador
}

.absorber_distintos <- function(acumulador, bloque) {
  x <- bloque$valores
  aplica <- !is.na(bloque$aplicable) & bloque$aplicable
  if (!acumulador$incluir_ausentes) aplica <- aplica & !is.na(x)
  ordinales <- .ordinales_bloque(bloque)
  indices <- which(aplica)
  ordinales <- ordinales[aplica]
  x <- x[aplica]
  if (!length(x)) {
    acumulador$estado_familia$n <- acumulador$estado_familia$n +
      length(bloque$valores)
    return(acumulador)
  }
  estado <- acumulador$estado_familia
  estado$n <- estado$n + length(bloque$valores)
  estado$n_validos <- estado$n_validos + length(x)
  if (isTRUE(estado$truncado)) {
    acumulador$estado_familia <- estado
    return(acumulador)
  }
  # `match()` sobre el bloque entero conserva la igualdad de R y evita una
  # llamada de dispatch por celda. El camino lento de abajo queda para clases
  # con semántica propia; el caso atomico sin clase es el que domina las
  # columnas grandes de texto y numeros.
  if (is.atomic(x) && is.null(dim(x)) && is.null(attr(x, "class"))) {
    n_representantes <- length(estado$representantes)
    posiciones <- if (n_representantes) {
      # Para atomicos sin clase, `match` es exactamente la igualdad que usa
      # `.match_R`; se evita el tryCatch y la normalizacion por celda del
      # adaptador general.
      match(x, estado$representantes, nomatch = 0L)
    } else integer(length(x))
    if (n_representantes) {
      estado$frecuencias <- estado$frecuencias + tabulate(
        posiciones[posiciones > 0L], nbins = n_representantes
      )
    }
    nuevos <- x[posiciones == 0L]
    if (!length(nuevos)) {
      acumulador$estado_familia <- estado
      return(acumulador)
    }
    nuevos <- unique(nuevos)
    correspondencias <- match(x, nuevos, nomatch = 0L)
    if (length(correspondencias) == length(x)) {
      frecuencias_nuevas <- tabulate(
        correspondencias, nbins = length(nuevos)
      )
      primeros_nuevos <- vapply(
        seq_along(nuevos), function(i) {
          min(ordinales[correspondencias == i])
        }, numeric(1L)
      )
      acumulador$estado_familia <- estado
      for (i in seq_along(nuevos)) {
        if (length(estado$representantes) >= acumulador$max_entradas) {
          estado$n_claves_omitidas <- estado$n_claves_omitidas +
            length(nuevos) - i + 1L
          acumulador$estado_familia <- estado
          return(.marcar_tope_acumulador(acumulador, "entradas"))
        }
        estado$representantes <- .append_typed_R(
          estado$representantes, nuevos[[i]]
        )
        if (is.null(estado$representantes)) {
          return(.marcar_fallo_acumulador(
            acumulador, "igualdad_s3_no_reproducible"
          ))
        }
        estado$frecuencias <- c(
          estado$frecuencias, frecuencias_nuevas[[i]]
        )
        estado$primeros <- c(estado$primeros, primeros_nuevos[[i]])
        acumulador$estado_familia <- estado
        if (.bytes_estado_acumulador(acumulador) > acumulador$max_bytes) {
          estado$representantes <- if (length(estado$representantes) == 1L) {
            NULL
          } else estado$representantes[-length(estado$representantes)]
          estado$frecuencias <- utils::head(estado$frecuencias, -1L)
          estado$primeros <- utils::head(estado$primeros, -1L)
          estado$n_claves_omitidas <- estado$n_claves_omitidas +
            length(nuevos) - i + 1L
          acumulador$estado_familia <- estado
          return(.marcar_tope_acumulador(acumulador, "bytes"))
        }
      }
      acumulador$estado_familia <- estado
      return(acumulador)
    }
  }
  for (i in seq_along(x)) {
    posicion <- .match_R(x[i], estado$representantes)
    if (is.null(posicion)) {
      return(.marcar_fallo_acumulador(
        acumulador,
        paste0("igualdad_s3_no_reproducible:", paste(class(x), collapse = "/"))
      ))
    }
    posicion <- posicion[[1L]]
    # El ordinal se calcula por posicion en el bloque. El camino por `==` de
    # arriba no puede usarse: NA y NaN tienen clases separadas en R.
    ordinal <- ordinales[[i]]
    if (posicion > 0L) {
      estado$frecuencias[[posicion]] <-
        estado$frecuencias[[posicion]] + 1
      next
    }
    if (length(estado$representantes) >= acumulador$max_entradas) {
      estado$n_claves_omitidas <- estado$n_claves_omitidas + 1
      acumulador <- .marcar_tope_acumulador(acumulador, "entradas")
      estado <- acumulador$estado_familia
      next
    }
    representante <- if (!length(estado$representantes)) x[[i]] else {
      .append_typed_R(estado$representantes, x[[i]])
    }
    if (is.null(representante)) {
      return(.marcar_fallo_acumulador(
        acumulador,
        paste0("igualdad_s3_no_reproducible:", paste(class(x), collapse = "/"))
      ))
    }
    estado$representantes <- representante
    estado$frecuencias <- c(estado$frecuencias, 1)
    estado$primeros <- c(estado$primeros, ordinal)
    if (.bytes_estado_acumulador(acumulador) > acumulador$max_bytes) {
      estado$representantes <- if (length(estado$representantes) == 1L) {
        NULL
      } else estado$representantes[-length(estado$representantes)]
      estado$frecuencias <- utils::head(estado$frecuencias, -1L)
      estado$primeros <- utils::head(estado$primeros, -1L)
      estado$n_claves_omitidas <- estado$n_claves_omitidas + 1
      acumulador <- .marcar_tope_acumulador(acumulador, "bytes")
    }
    acumulador$estado_familia <- estado
  }
  acumulador$estado_familia <- estado
  acumulador
}

.expandir_datos_identidad <- function(datos) {
  if (!inherits(datos, "data.frame")) {
    stop("La clave de fila requiere un data.frame.", call. = FALSE)
  }
  columnas_matriciales <- vapply(
    datos,
    function(x) is.matrix(x) || (is.array(x) && length(dim(x)) > 1L),
    logical(1L)
  )
  if (!any(columnas_matriciales)) return(datos)
  columnas <- list()
  for (i in seq_along(datos)) {
    columna <- datos[[i]]
    if (columnas_matriciales[[i]]) {
      componentes <- as.data.frame(unclass(columna), stringsAsFactors = FALSE)
      for (j in seq_along(componentes)) {
        columnas[[length(columnas) + 1L]] <- componentes[[j]]
      }
    } else {
      columnas[[length(columnas) + 1L]] <- columna
    }
  }
  as.data.frame(columnas, check.names = FALSE, stringsAsFactors = FALSE)
}

.fila_igual_R <- function(datos, i, representantes, j) {
  if (ncol(datos) != ncol(representantes)) return(FALSE)
  for (columna in seq_len(ncol(datos))) {
    igual <- .iguales_R(
      datos[[columna]][i], representantes[[columna]][j]
    )
    if (is.null(igual)) return(NULL)
    if (!igual) return(FALSE)
  }
  TRUE
}

.absorber_filas_distintos <- function(acumulador, bloque) {
  datos <- .expandir_datos_identidad(bloque$valores)
  aplica <- !is.na(bloque$aplicable) & bloque$aplicable
  ordinales <- .ordinales_bloque(bloque)
  datos <- datos[aplica, , drop = FALSE]
  ordinales <- ordinales[aplica]
  if (!nrow(datos)) return(acumulador)
  estado <- acumulador$estado_familia
  for (i in seq_len(nrow(datos))) {
    encontrado <- 0L
    if (!is.null(estado$representantes) && nrow(estado$representantes)) {
      for (j in seq_len(nrow(estado$representantes))) {
        igual <- .fila_igual_R(datos, i, estado$representantes, j)
        if (is.null(igual)) {
          return(.marcar_fallo_acumulador(
            acumulador,
            paste0("igualdad_s3_no_reproducible:",
                   paste(vapply(datos, function(x) paste(class(x), collapse = "/"),
                                character(1L)), collapse = ","))
          ))
        }
        if (igual) {
          encontrado <- j
          break
        }
      }
    }
    estado$n <- estado$n + 1
    if (encontrado) {
      estado$frecuencias[[encontrado]] <-
        estado$frecuencias[[encontrado]] + 1
      estado$grupos <- c(estado$grupos, encontrado)
      next
    }
    if (length(estado$frecuencias) >= acumulador$max_entradas) {
      estado$n_claves_omitidas <- estado$n_claves_omitidas + 1
      acumulador$estado_familia <- estado
      return(.marcar_tope_acumulador(acumulador, "entradas"))
    }
    fila <- datos[i, , drop = FALSE]
    estado$representantes <- if (is.null(estado$representantes)) fila else {
      tryCatch(rbind(estado$representantes, fila), error = function(e) NULL)
    }
    if (is.null(estado$representantes)) {
      return(.marcar_fallo_acumulador(
        acumulador, "igualdad_s3_no_reproducible"
      ))
    }
    estado$frecuencias <- c(estado$frecuencias, 1)
    estado$primeros <- c(estado$primeros, ordinales[[i]])
    estado$grupos <- c(estado$grupos, length(estado$frecuencias))
    acumulador$estado_familia <- estado
    if (.bytes_estado_acumulador(acumulador) > acumulador$max_bytes) {
      ultimo <- nrow(estado$representantes)
      estado$representantes <- estado$representantes[-ultimo, , drop = FALSE]
      estado$frecuencias <- utils::head(estado$frecuencias, -1L)
      estado$primeros <- utils::head(estado$primeros, -1L)
      estado$grupos <- utils::head(estado$grupos, -1L)
      estado$n_claves_omitidas <- estado$n_claves_omitidas + 1
      acumulador$estado_familia <- estado
      return(.marcar_tope_acumulador(acumulador, "bytes"))
    }
  }
  acumulador$estado_familia <- estado
  acumulador
}

.absorber_acumulador <- function(acumulador, bloque) {
  if (!inherits(acumulador, "acumulador_bloques")) {
    stop("`acumulador` no es un acumulador de bloques.", call. = FALSE)
  }
  if (identical(acumulador$estado, "finalizado")) {
    stop("No se puede absorber despues de `finalizar()`.", call. = FALSE)
  }
  if (identical(acumulador$estado, "no_disponible")) return(acumulador)
  bloque <- .extraer_bloque(bloque)
  if (acumulador$familia %in% c("distintos", "hueco") &&
      !.entrada_atomica_acumulador(bloque$valores)) {
    return(.marcar_fallo_acumulador(
      acumulador, "entrada_no_soportada:no_atomica"
    ))
  }
  bloque <- .registrar_intervalo_bloque(acumulador, bloque)
  acumulador <- switch(
    acumulador$familia,
    conteos = .absorber_conteos(acumulador, bloque),
    cuantitativos = .absorber_cuantitativos(acumulador, bloque),
    longitudes = .absorber_longitudes(acumulador, bloque),
    distintos = .absorber_distintos(acumulador, bloque),
    hueco = .absorber_distintos(acumulador, bloque),
    filas_distintos = .absorber_filas_distintos(acumulador, bloque),
    trazabilidad = .absorber_trazabilidad(acumulador, bloque),
    ejemplos = .absorber_ejemplos(acumulador, bloque),
    muestra = .absorber_muestra(acumulador, bloque),
    lsh = .absorber_lsh(acumulador, bloque),
    outliers = .absorber_outliers(acumulador, bloque),
    centinela = .absorber_centinela(acumulador, bloque),
    aritmetica = .absorber_aritmetica(acumulador, bloque),
    generica = {
      acumulador$estado_familia$n <- acumulador$estado_familia$n +
        length(bloque$valores)
      acumulador
    }
  )
  acumulador
}

.compatible_acumuladores <- function(x, y) {
  identical(x$configuracion, y$configuracion) &&
    identical(x$familia, y$familia) &&
    identical(x$max_entradas, y$max_entradas) &&
    identical(x$max_bytes, y$max_bytes) &&
    identical(x$requiere_orden, y$requiere_orden) &&
    identical(x$incluir_ausentes, y$incluir_ausentes)
}

.fusionar_estado_cuantitativo <- function(a, b) {
  a$n_nan <- a$n_nan + b$n_nan
  a$n_infinito_positivo <- a$n_infinito_positivo + b$n_infinito_positivo
  a$n_infinito_negativo <- a$n_infinito_negativo + b$n_infinito_negativo
  a$n_ceros <- a$n_ceros + b$n_ceros
  a$n_negativos <- a$n_negativos + b$n_negativos
  if (!b$n) return(a)
  if (!a$n) {
    b$n_nan <- a$n_nan
    b$n_infinito_positivo <- a$n_infinito_positivo
    b$n_infinito_negativo <- a$n_infinito_negativo
    b$n_ceros <- a$n_ceros
    b$n_negativos <- a$n_negativos
    return(b)
  }
  total <- a$n + b$n
  diferencia <- b$media - a$media
  a$m2 <- a$m2 + b$m2 + diferencia * diferencia * a$n * b$n / total
  a$media <- a$media + diferencia * b$n / total
  a$n <- total
  a$minimo <- min(a$minimo, b$minimo)
  a$maximo <- max(a$maximo, b$maximo)
  a
}

.fusionar_distintos <- function(acumulador, otro) {
  a <- acumulador$estado_familia
  b <- otro$estado_familia
  acumulador$estado_familia$n <- a$n + b$n
  acumulador$estado_familia$n_validos <- a$n_validos + b$n_validos
  if (isTRUE(b$truncado)) {
    acumulador <- .marcar_tope_acumulador(acumulador, b$causa_tope)
  }
  if (!isTRUE(a$truncado) && !isTRUE(b$truncado)) {
    for (i in seq_along(b$representantes)) {
      posicion <- .match_R(b$representantes[[i]], a$representantes)
      if (is.null(posicion)) {
        return(.marcar_fallo_acumulador(
          acumulador,
          paste0("igualdad_s3_no_reproducible:",
                 paste(class(b$representantes), collapse = "/"))
        ))
      }
      posicion <- posicion[[1L]]
      if (!posicion) {
        if (length(a$representantes) >= acumulador$max_entradas) {
          a$n_claves_omitidas <- a$n_claves_omitidas + 1
          acumulador <- .marcar_tope_acumulador(acumulador, "entradas")
          break
        }
        a$representantes <- .append_typed_R(a$representantes,
                                            b$representantes[[i]])
        if (is.null(a$representantes)) {
          return(.marcar_fallo_acumulador(
            acumulador, "igualdad_s3_no_reproducible"
          ))
        }
        a$frecuencias <- c(a$frecuencias, b$frecuencias[[i]])
        a$primeros <- c(a$primeros, b$primeros[[i]])
      } else {
        a$frecuencias[[posicion]] <-
          a$frecuencias[[posicion]] + b$frecuencias[[i]]
        a$primeros[[posicion]] <- min(a$primeros[[posicion]], b$primeros[[i]])
      }
    }
    acumulador$estado_familia <- a
  }
  acumulador
}

.fusionar_filas_distintos <- function(acumulador, otro) {
  a <- acumulador$estado_familia
  b <- otro$estado_familia
  acumulador$estado_familia$n <- a$n + b$n
  if (isTRUE(b$truncado)) {
    return(.marcar_tope_acumulador(acumulador, b$causa_tope))
  }
  if (isTRUE(a$truncado)) return(acumulador)
  correspondencias <- integer(length(b$frecuencias))
  for (i in seq_len(nrow(b$representantes %||% data.frame()))) {
    encontrado <- 0L
    if (!is.null(a$representantes) && nrow(a$representantes)) {
      for (j in seq_len(nrow(a$representantes))) {
        igual <- .fila_igual_R(
          b$representantes, i, a$representantes, j
        )
        if (is.null(igual)) {
          return(.marcar_fallo_acumulador(
            acumulador, "igualdad_s3_no_reproducible"
          ))
        }
        if (igual) {
          encontrado <- j
          break
        }
      }
    }
    if (encontrado) {
      a$frecuencias[[encontrado]] <- a$frecuencias[[encontrado]] +
        b$frecuencias[[i]]
      a$primeros[[encontrado]] <- min(a$primeros[[encontrado]], b$primeros[[i]])
      correspondencias[[i]] <- encontrado
    } else {
      if (length(a$frecuencias) >= acumulador$max_entradas) {
        a$n_claves_omitidas <- a$n_claves_omitidas + 1
        acumulador$estado_familia <- a
        return(.marcar_tope_acumulador(acumulador, "entradas"))
      }
      a$representantes <- if (is.null(a$representantes)) {
        b$representantes[i, , drop = FALSE]
      } else {
        rbind(a$representantes, b$representantes[i, , drop = FALSE])
      }
      a$frecuencias <- c(a$frecuencias, b$frecuencias[[i]])
      a$primeros <- c(a$primeros, b$primeros[[i]])
      correspondencias[[i]] <- length(a$frecuencias)
    }
  }
  if (length(b$grupos)) {
    a$grupos <- c(a$grupos, correspondencias[b$grupos])
  }
  acumulador$estado_familia <- a
  acumulador
}

.fusionar_estado_centinela <- function(acumulador, otro) {
  a <- acumulador$estado_familia
  b <- otro$estado_familia
  a$n <- a$n + b$n
  a$n_outliers <- a$n_outliers + b$n_outliers
  if (isTRUE(b$truncado)) {
    acumulador$estado_familia <- a
    return(.marcar_tope_acumulador(acumulador, b$causa_tope))
  }
  if (isTRUE(a$truncado)) {
    acumulador$estado_familia <- a
    return(acumulador)
  }
  for (i in seq_along(b$representantes)) {
    posicion <- .match_R(b$representantes[[i]], a$representantes)
    if (is.null(posicion)) {
      acumulador$estado_familia <- a
      return(.marcar_fallo_acumulador(
        acumulador, "igualdad_s3_no_reproducible"
      ))
    }
    posicion <- posicion[[1L]]
    if (posicion > 0L) {
      a$frecuencias[[posicion]] <- a$frecuencias[[posicion]] +
        b$frecuencias[[i]]
      next
    }
    if (length(a$representantes) >= acumulador$max_entradas) {
      a$n_claves_omitidas <- a$n_claves_omitidas + 1L
      acumulador$estado_familia <- a
      return(.marcar_tope_acumulador(acumulador, "entradas"))
    }
    a$representantes <- .append_typed_R(a$representantes,
                                         b$representantes[[i]])
    if (is.null(a$representantes)) {
      acumulador$estado_familia <- a
      return(.marcar_fallo_acumulador(
        acumulador, "igualdad_s3_no_reproducible"
      ))
    }
    a$frecuencias <- c(a$frecuencias, b$frecuencias[[i]])
  }
  acumulador$estado_familia <- a
  acumulador
}

.fusionar_trazabilidad <- function(acumulador, otro) {
  a <- acumulador$estado_familia
  b <- otro$estado_familia
  a$total <- a$total + b$total
  a$indices <- sort(unique(c(a$indices, b$indices)))
  if (isTRUE(a$truncado) || isTRUE(b$truncado)) {
    a$truncado <- TRUE
    a$causa_tope <- a$causa_tope %||% b$causa_tope %||% "entradas"
  }
  if (is.finite(acumulador$max_entradas) &&
      length(a$indices) > acumulador$max_entradas) {
    a$indices_omitidos <- a$indices_omitidos +
      length(a$indices) - acumulador$max_entradas
    a$indices <- utils::head(a$indices, acumulador$max_entradas)
    a$truncado <- TRUE
    a$causa_tope <- a$causa_tope %||% "entradas"
  }
  acumulador$estado_familia <- a
  acumulador
}

.fusionar_ejemplos <- function(acumulador, otro) {
  a <- acumulador$estado_familia
  b <- otro$estado_familia
  a$n <- a$n + b$n
  a$n_filas <- a$n_filas + b$n_filas
  if (isTRUE(a$truncado) || isTRUE(b$truncado)) {
    a$truncado <- TRUE
    a$causa_tope <- a$causa_tope %||% b$causa_tope %||% "entradas"
    acumulador$estado_familia <- a
    acumulador$estado <- "truncado"
    return(acumulador)
  }
  limite_ejemplos <- acumulador$configuracion$configuracion$max_ejemplos %||%
    .MAX_EJEMPLOS_BLOQUES
  for (i in seq_along(b$representantes)) {
    posicion <- .match_R(b$representantes[[i]], a$representantes)
    if (is.null(posicion)) {
      return(.marcar_fallo_acumulador(
        acumulador, "igualdad_s3_no_reproducible"
      ))
    }
    posicion <- posicion[[1L]]
    if (!posicion) {
      if (length(a$representantes) >= acumulador$max_entradas) {
        a$n_claves_omitidas <- a$n_claves_omitidas + 1L
        a$truncado <- TRUE
        a$causa_tope <- "entradas"
        break
      }
      a$representantes <- .append_typed_R(
        a$representantes, b$representantes[[i]]
      )
      if (is.null(a$representantes)) {
        return(.marcar_fallo_acumulador(
          acumulador, "igualdad_s3_no_reproducible"
        ))
      }
      a$frecuencias <- c(a$frecuencias, b$frecuencias[[i]])
      a$primeros <- c(a$primeros, b$primeros[[i]])
      a$valores[[length(a$valores) + 1L]] <- b$valores[[i]]
      a$ordinales[[length(a$ordinales) + 1L]] <- b$ordinales[[i]]
    } else {
      a$frecuencias[[posicion]] <- a$frecuencias[[posicion]] +
        b$frecuencias[[i]]
      a$primeros[[posicion]] <- min(a$primeros[[posicion]], b$primeros[[i]])
      candidatos <- .primeros_ejemplos_ordinales(
        c(a$valores[[posicion]], b$valores[[i]]),
        c(a$ordinales[[posicion]], b$ordinales[[i]]), limite_ejemplos
      )
      a$valores[[posicion]] <- candidatos$valores
      a$ordinales[[posicion]] <- candidatos$ordinales
    }
  }
  if (length(a$representantes)) {
    for (i in seq_along(a$representantes)) {
      candidatos <- .primeros_ejemplos_ordinales(
        a$valores[[i]], a$ordinales[[i]], limite_ejemplos
      )
      a$valores[[i]] <- candidatos$valores
      a$ordinales[[i]] <- candidatos$ordinales
    }
  }
  acumulador$estado_familia <- a
  if (isTRUE(a$truncado)) acumulador$estado <- "truncado"
  acumulador
}

.fusionar_muestra <- function(acumulador, otro) {
  a <- acumulador$estado_familia
  b <- otro$estado_familia
  a$indices <- sort(unique(c(a$indices, b$indices)))
  a$n <- length(a$indices)
  acumulador$estado_familia <- a
  acumulador
}

.fusionar_acumuladores <- function(acumulador, otro) {
  if (!inherits(acumulador, "acumulador_bloques") ||
      !inherits(otro, "acumulador_bloques")) {
    stop("Ambos objetos deben ser acumuladores de bloques.", call. = FALSE)
  }
  if (!.compatible_acumuladores(acumulador, otro)) {
    return(.marcar_fallo_acumulador(
      acumulador, "configuracion_acumulador_incompatible"
    ))
  }
  if (identical(acumulador$estado, "finalizado") ||
      identical(otro$estado, "finalizado")) {
    return(.marcar_fallo_acumulador(
      acumulador, "acumulador_finalizado_no_fusionable"
    ))
  }
  if (identical(acumulador$estado, "no_disponible") ||
      identical(otro$estado, "no_disponible")) {
    return(.marcar_fallo_acumulador(
      acumulador, acumulador$fallo %||% otro$fallo %||% "acumulador_no_disponible"
    ))
  }
  if (acumulador$requiere_orden && length(acumulador$intervalos) &&
      length(otro$intervalos)) {
    todos <- c(acumulador$intervalos, otro$intervalos)
    todos <- todos[order(vapply(todos, `[[`, numeric(1L), 1L))]
    if (any(vapply(seq_len(length(todos) - 1L), function(i) {
      todos[[i]][[2L]] >= todos[[i + 1L]][[1L]]
    }, logical(1L)))) {
      return(.marcar_fallo_acumulador(
        acumulador, "intervalos_acumulador_solapados"
      ))
    }
  }
  acumulador$intervalos <- c(acumulador$intervalos, otro$intervalos)
  if (length(acumulador$intervalos)) {
    orden_intervalos <- order(vapply(
      acumulador$intervalos, `[[`, numeric(1L), 1L
    ))
    acumulador$intervalos <- acumulador$intervalos[orden_intervalos]
  }
  acumulador$ultimo_ordinal <- max(acumulador$ultimo_ordinal,
                                   otro$ultimo_ordinal)
  acumulador$estado_familia <- switch(
    acumulador$familia,
    conteos = {
      a <- acumulador$estado_familia
      b <- otro$estado_familia
      for (nombre in names(a)) a[[nombre]] <- a[[nombre]] + b[[nombre]]
      a
    },
    cuantitativos = .fusionar_estado_cuantitativo(
      acumulador$estado_familia, otro$estado_familia
    ),
    longitudes = {
      a <- acumulador$estado_familia
      b <- otro$estado_familia
      a$n <- a$n + b$n
      a$suma <- a$suma + b$suma
      a$minimo <- min(a$minimo, b$minimo)
      a$maximo <- max(a$maximo, b$maximo)
      a
    },
    distintos = acumulador$estado_familia,
    hueco = acumulador$estado_familia,
    filas_distintos = acumulador$estado_familia,
    outliers = {
      a <- acumulador$estado_familia
      b <- otro$estado_familia
      a$n <- a$n + b$n
      a$n_outliers <- a$n_outliers + b$n_outliers
      a
    },
    centinela = acumulador$estado_familia,
    trazabilidad = acumulador$estado_familia,
    ejemplos = acumulador$estado_familia,
    muestra = acumulador$estado_familia,
    lsh = acumulador$estado_familia,
    aritmetica = {
      a <- acumulador$estado_familia
      b <- otro$estado_familia
      a$n <- a$n + b$n
      a$n_cumplen <- a$n_cumplen + b$n_cumplen
      a$n_incumplen <- a$n_incumplen + b$n_incumplen
      a
    },
    generica = acumulador$estado_familia
  )
  if (acumulador$familia %in% c("distintos", "hueco")) {
    acumulador <- .fusionar_distintos(acumulador, otro)
  }
  if (identical(acumulador$familia, "centinela")) {
    acumulador <- .fusionar_estado_centinela(acumulador, otro)
  }
  if (identical(acumulador$familia, "filas_distintos")) {
    acumulador <- .fusionar_filas_distintos(acumulador, otro)
  }
  if (identical(acumulador$familia, "trazabilidad")) {
    acumulador <- .fusionar_trazabilidad(acumulador, otro)
  }
  if (identical(acumulador$familia, "ejemplos")) {
    acumulador <- .fusionar_ejemplos(acumulador, otro)
  }
  if (identical(acumulador$familia, "muestra")) {
    acumulador <- .fusionar_muestra(acumulador, otro)
  }
  if (identical(acumulador$familia, "lsh")) {
    acumulador <- .marcar_fallo_acumulador(
      acumulador, "lsh_fusion_no_disponible:usar_un_spool_global"
    )
  }
  acumulador
}

.resultado_acumulador <- function(acumulador) {
  estado <- acumulador$estado_familia
  familia <- acumulador$familia
  if (identical(acumulador$estado, "no_disponible")) {
    return(NULL)
  }
  switch(
    familia,
    conteos = estado,
    cuantitativos = {
      if (!estado$n) {
        list(minimo = NA_real_, maximo = NA_real_, media = NA_real_,
             desvio = NA_real_, n_evaluados = 0L,
             n_nan = estado$n_nan,
             n_infinito_positivo = estado$n_infinito_positivo,
             n_infinito_negativo = estado$n_infinito_negativo,
             n_ceros = estado$n_ceros, n_negativos = estado$n_negativos)
      } else {
        list(
          minimo = estado$minimo, maximo = estado$maximo,
          media = estado$media,
          desvio = if (estado$n > 1L) sqrt(estado$m2 / (estado$n - 1)) else NA_real_,
          n_evaluados = estado$n, n_nan = estado$n_nan,
          n_infinito_positivo = estado$n_infinito_positivo,
          n_infinito_negativo = estado$n_infinito_negativo,
          n_ceros = estado$n_ceros, n_negativos = estado$n_negativos
        )
      }
    },
    longitudes = if (!estado$n) {
      c(minimo = NA_real_, maximo = NA_real_, media = NA_real_)
    } else {
      c(minimo = estado$minimo, maximo = estado$maximo,
        media = estado$suma / estado$n)
    },
    distintos = if (isTRUE(estado$truncado)) NULL else {
      orden <- order(estado$primeros)
      representantes <- estado$representantes
      if (is.null(representantes)) {
        representantes <- switch(
          acumulador$configuracion$tipo,
          character = character(), logical = logical(), integer = integer(),
          double = numeric(), raw = raw(), numeric()
        )
      }
      data.frame(
        representante = representantes[orden],
        frecuencia = as.integer(estado$frecuencias[orden]),
        primer_ordinal = as.numeric(estado$primeros[orden]),
        stringsAsFactors = FALSE
      )
    },
    hueco = if (isTRUE(estado$truncado)) NULL else {
      orden <- order(estado$primeros)
      representantes <- estado$representantes
      if (is.null(representantes)) representantes <- numeric()
      data.frame(
        representante = representantes[orden],
        frecuencia = as.integer(estado$frecuencias[orden]),
        primer_ordinal = as.numeric(estado$primeros[orden]),
        stringsAsFactors = FALSE
      )
    },
    filas_distintos = if (isTRUE(estado$truncado)) NULL else {
      frecuencias <- as.integer(estado$frecuencias)
      list(
        representantes = estado$representantes,
        frecuencias = frecuencias,
        primeros = as.numeric(estado$primeros),
        filas_duplicadas = as.integer(sum(frecuencias - 1L)),
        filas_en_grupos_duplicados = as.integer(sum(frecuencias[frecuencias > 1L])),
        grupos = estado$grupos
      )
    },
    outliers = list(
      n_outliers = as.integer(estado$n_outliers),
      n_evaluados = as.integer(estado$n)
    ),
    centinela = if (isTRUE(estado$truncado)) NULL else {
      list(
        representantes = estado$representantes,
        frecuencias = as.integer(estado$frecuencias),
        n_outliers = as.integer(estado$n_outliers),
        n_evaluados = as.integer(estado$n)
      )
    },
    trazabilidad = {
      indices <- sort(unique(as.integer(estado$indices)))
      list(
        indices_fila = indices,
        total = as.numeric(estado$total),
        mostrados = length(indices),
        truncado = isTRUE(estado$truncado) ||
          length(indices) < estado$total,
        limite = acumulador$max_entradas,
        indices_omitidos = as.integer(estado$indices_omitidos)
      )
    },
    ejemplos = if (isTRUE(estado$truncado)) NULL else {
      orden <- order(estado$primeros)
      representantes <- estado$representantes
      if (is.null(representantes)) {
        representantes <- switch(
          acumulador$configuracion$tipo,
          character = character(), logical = logical(), integer = integer(),
          double = numeric(), raw = raw(), numeric()
        )
      }
      texto_ejemplos <- vapply(seq_along(estado$valores), function(i) {
        valores <- estado$valores[[i]]
        ordinales <- estado$ordinales[[i]]
        if (length(ordinales) == length(valores) && length(ordinales)) {
          valores <- valores[order(ordinales)]
        }
        paste(vapply(valores, function(valor) {
          tryCatch(as.character(valor), error = function(e) "")
        }, character(1L)), collapse = " | ")
      }, character(1L))
      data.frame(
        clave = representantes[orden],
        n = as.integer(estado$frecuencias[orden]),
        primer_ordinal = as.numeric(estado$primeros[orden]),
        ejemplos = texto_ejemplos[orden],
        stringsAsFactors = FALSE
      )
    },
    muestra = list(
      indices = as.integer(sort(unique(estado$indices))),
      n = as.integer(length(unique(estado$indices))),
      n_total = as.numeric(estado$n_total),
      limite = as.numeric(estado$limite)
    ),
    lsh = if (is.null(estado$salida)) NULL else estado$salida,
    aritmetica = list(
      k = as.numeric(estado$k),
      n_evaluados = as.integer(estado$n),
      n_cumplen = as.integer(estado$n_cumplen),
      n_incumplen = as.integer(estado$n_incumplen),
      proporcion = if (estado$n) estado$n_cumplen / estado$n else NA_real_
    ),
    list(n = estado$n)
  )
}

.sobre_acumulador <- function(acumulador) {
  if (identical(acumulador$familia, "lsh") && exists(
      ".sobre_lsh_acumulador", mode = "function")) {
    return(.sobre_lsh_acumulador(acumulador))
  }
  exacto <- identical(acumulador$estado, "iniciado") ||
    identical(acumulador$estado, "finalizado")
  estado <- if (identical(acumulador$estado, "no_disponible")) {
    "no_disponible"
  } else if (identical(acumulador$estado, "truncado")) {
    if (acumulador$familia %in% c("distintos", "hueco")) "cota" else {
      "no_disponible"
    }
  } else if (identical(acumulador$familia, "trazabilidad") &&
             isTRUE(acumulador$estado_familia$truncado)) {
    "calculado"
  } else "calculado"
  resultado <- .resultado_acumulador(acumulador)
  cota <- NULL
  if (identical(estado, "cota")) {
    minimo <- length(acumulador$estado_familia$representantes) +
      as.integer(acumulador$estado_familia$n_claves_omitidas > 0)
    cota <- list(
      direccion = ">=",
      valor = minimo,
      significado = "cantidad minima de valores distintos observados"
    )
  }
  list(
    resultado = resultado,
    estado = estado,
    exacto = if (identical(estado, "calculado")) TRUE else if (
      identical(estado, "cota")
    ) FALSE else NA,
    motivo = if (is.null(acumulador$fallo)) {
      if (identical(acumulador$familia, "trazabilidad") &&
          isTRUE(acumulador$estado_familia$truncado)) {
        paste0("trazabilidad_truncada:",
               acumulador$estado_familia$causa_tope %||% "desconocido")
      } else if (identical(estado, "cota")) {
        paste0("mapa_distintos_truncado:", acumulador$estado_familia$causa_tope)
      } else if (identical(estado, "no_disponible")) {
        if (identical(acumulador$familia, "outliers") ||
            identical(acumulador$familia, "centinela")) {
          paste0("segunda_pasada_valor:mapa_distintos_truncado:",
                 acumulador$estado_familia$causa_tope %||% "desconocido")
        } else "resultado_no_disponible"
      } else NA_character_
    } else acumulador$fallo,
    como_resolverlo = if (identical(estado, "cota")) {
      "Aumentar el tope o habilitar un derrame exacto en una etapa posterior."
    } else if (identical(acumulador$familia, "trazabilidad") &&
               isTRUE(acumulador$estado_familia$truncado)) {
      "Aumentar `max_filas_hallazgo` para conservar mas indices de evidencia."
    } else if (identical(estado, "no_disponible")) {
      if (identical(acumulador$familia, "outliers") ||
          identical(acumulador$familia, "centinela")) {
        "Aumentar el tope del mapa o habilitar una segunda pasada estable."
      } else "Revisar el tipo y la igualdad declarada para la familia."
    } else NA_character_,
    cota = cota,
    tope = list(
      entradas = acumulador$max_entradas,
      bytes = acumulador$max_bytes,
      nombre = if (acumulador$familia %in% c("distintos", "hueco")) {
        "E_distintos/P_estado"
      } else if (identical(acumulador$familia, "trazabilidad")) {
        "max_filas_hallazgo/P_estado"
      } else if (identical(acumulador$familia, "ejemplos")) {
        "L_ejemplos/P_estado"
      } else "P_estado"
    ),
    almacenamiento = "memoria",
    derrame = NULL,
    alcance = list(
      filas = acumulador$ultimo_ordinal,
      valores = if (identical(acumulador$familia, "trazabilidad")) {
        acumulador$estado_familia$total
      } else if (identical(acumulador$familia, "muestra")) {
        acumulador$estado_familia$n
      } else if (identical(acumulador$familia, "ejemplos")) {
        acumulador$estado_familia$n_filas
      } else {
        acumulador$estado_familia$n_validos %||% NA_real_
      },
      orden = acumulador$configuracion$orden_id,
      snapshot = acumulador$configuracion$snapshot_id,
      muestra = acumulador$configuracion$muestra_id
    ),
    bytes_retenidos = .bytes_retenidos(acumulador)
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

.finalizar_acumulador <- function(acumulador) {
  if (!inherits(acumulador, "acumulador_bloques")) {
    stop("`acumulador` no es un acumulador de bloques.", call. = FALSE)
  }
  if (identical(acumulador$estado, "finalizado")) return(acumulador$resultado)
  if (identical(acumulador$familia, "lsh") && exists(
      ".finalizar_lsh", mode = "function")) {
    acumulador <- .finalizar_lsh(acumulador)
    sobre <- .sobre_acumulador(acumulador)
    acumulador$resultado <- sobre
    acumulador$estado <- "finalizado"
    return(sobre)
  }
  if (identical(acumulador$estado, "iniciado") ||
      identical(acumulador$estado, "truncado") ||
      identical(acumulador$estado, "no_disponible")) {
    sobre <- .sobre_acumulador(acumulador)
    acumulador$resultado <- sobre
    acumulador$estado <- "finalizado"
    return(sobre)
  }
  .sobre_acumulador(acumulador)
}

.bytes_retenidos <- function(acumulador) {
  valor <- .bytes_estado_acumulador(acumulador)
  if (!is.finite(valor) || valor < 0) return(0)
  as.numeric(valor)
}

.ejecutar_acumulador_bloques <- function(acumulador, bloques,
                                         vigilante = NULL,
                                         familia = acumulador$familia) {
  if (!is.list(bloques)) stop("`bloques` debe ser una lista.", call. = FALSE)
  for (i in seq_along(bloques)) {
    acumulador <- .absorber_acumulador(acumulador, bloques[[i]])
    if (!is.null(vigilante)) {
      gc(verbose = FALSE)
      .registrar_barrera_vigilante(
        vigilante, "bloque", familia, i, acumulador = acumulador
      )
    }
  }
  resultado <- .finalizar_acumulador(acumulador)
  if (!is.null(vigilante)) {
    gc(verbose = FALSE)
    .registrar_barrera_vigilante(
      vigilante, "finalizar", familia, length(bloques),
      acumulador = acumulador, resultado = resultado
    )
  }
  list(acumulador = acumulador, resultado = resultado,
       vigilante = vigilante)
}

.particionar_bloques <- function(n, k = NULL, tamano = 10000L) {
  if (length(n) != 1L || is.na(n) || n < 0 || n != floor(n)) {
    stop("`n` debe ser un entero no negativo.", call. = FALSE)
  }
  n <- as.integer(n)
  if (!n) return(list())
  if (is.null(k)) {
    tamano <- .validar_tope_bloques(tamano, "tamano")
    k <- ceiling(n / tamano)
  } else {
    if (length(k) != 1L || is.na(k) || k < 1 || k != floor(k)) {
      stop("`k` debe ser un entero positivo.", call. = FALSE)
    }
    k <- min(as.integer(k), n)
  }
  bordes <- unique(as.integer(round(seq.int(1L, n + 1L, length.out = k + 1L))))
  lapply(seq_len(length(bordes) - 1L), function(i) {
    inicio <- bordes[[i]]
    fin <- bordes[[i + 1L]] - 1L
    list(
      valores = seq.int(inicio, fin), ordinal_inicio = inicio,
      ordinal_fin = fin, n_filas = fin - inicio + 1L
    )
  })
}

.bloques_de_vector <- function(x, k = NULL, tamano = 10000L) {
  indices <- .particionar_bloques(length(x), k = k, tamano = tamano)
  lapply(indices, function(indice) {
    indice$valores <- x[indice$valores]
    indice
  })
}

.ejecutar_vector_bloques <- function(x, familia, k = NULL, tamano = 10000L,
                                     configuracion = list(),
                                     max_entradas = Inf, max_bytes = Inf,
                                     requiere_orden = FALSE,
                                     incluir_ausentes = FALSE,
                                     aplicable = NULL, vigilante = NULL,
                                     fuente_id = "memoria",
                                     snapshot_id = "memoria",
                                     universo_id = "tabla_completa",
                                     muestra_id = NULL,
                                     orden_id = "orden_entrada") {
  if (!is.null(aplicable) &&
      (!is.logical(aplicable) || length(aplicable) != length(x))) {
    stop("`aplicable` debe ser logico y tener el largo del vector.",
         call. = FALSE)
  }
  acumulador <- .iniciar_acumulador(
    "vector", typeof(x), familia = familia,
    configuracion = configuracion,
    max_entradas = max_entradas, max_bytes = max_bytes,
    requiere_orden = requiere_orden,
    incluir_ausentes = incluir_ausentes,
    fuente_id = fuente_id, snapshot_id = snapshot_id,
    universo_id = universo_id, muestra_id = muestra_id, orden_id = orden_id
  )
  if (identical(familia, "cuantitativos") &&
      "contar_signos" %in% names(configuracion)) {
    acumulador$estado_familia$contar_signos <-
      isTRUE(configuracion$contar_signos)
  }
  if (familia %in% c("distintos", "hueco") &&
      !.entrada_atomica_acumulador(x)) {
    acumulador <- .marcar_fallo_acumulador(
      acumulador, "entrada_no_soportada:no_atomica"
    )
    resultado <- .finalizar_acumulador(acumulador)
    return(list(acumulador = acumulador, resultado = resultado,
                vigilante = vigilante))
  }
  indices <- .particionar_bloques(length(x), k = k, tamano = tamano)
  for (i in seq_along(indices)) {
    bloque <- indices[[i]]
    posiciones <- bloque$valores
    bloque$valores <- x[posiciones]
    if (!is.null(aplicable)) {
      if (identical(familia, "trazabilidad")) {
        bloque$seleccion <- aplicable[posiciones]
      } else {
        bloque$aplicable <- aplicable[posiciones]
      }
    }
    acumulador <- .absorber_acumulador(acumulador, bloque)
    if (!is.null(vigilante)) {
      rm(bloque)
      gc(verbose = FALSE)
      .registrar_barrera_vigilante(
        vigilante, "bloque", familia, i, acumulador = acumulador
      )
    }
  }
  resultado <- .finalizar_acumulador(acumulador)
  if (!is.null(vigilante)) {
    gc(verbose = FALSE)
    .registrar_barrera_vigilante(
      vigilante, "finalizar", familia, length(indices),
      acumulador = acumulador, resultado = resultado
    )
  }
  list(acumulador = acumulador, resultado = resultado,
       vigilante = vigilante)
}

.mapa_distintos_bloques <- function(x, k = NULL, tamano = 10000L,
                                    max_entradas = Inf, max_bytes = Inf,
                                    incluir_ausentes = FALSE) {
  .ejecutar_vector_bloques(
    x, "distintos", k = k, tamano = tamano,
    max_entradas = max_entradas, max_bytes = max_bytes,
    incluir_ausentes = incluir_ausentes
  )$resultado
}

.moda_mapa_distintos <- function(sobre) {
  if (is.null(sobre) || !identical(sobre$estado, "calculado") ||
      !is.data.frame(sobre$resultado) || !nrow(sobre$resultado)) {
    return(NULL)
  }
  mapa <- sobre$resultado
  candidatos <- which(mapa$frecuencia == max(mapa$frecuencia))
  if (length(candidatos) > 1L) {
    valores <- mapa$representante[candidatos]
    orden <- if (is.character(valores)) {
      match(.ordenar_por_bytes(valores), valores)
    } else {
      tryCatch(order(valores), error = function(e) integer())
    }
    if (length(orden) == length(candidatos)) candidatos <-
      candidatos[orden]
  }
  posicion <- candidatos[[1L]]
  list(
    valor = .texto_valor(mapa$representante[posicion]),
    frecuencia = as.integer(mapa$frecuencia[[posicion]])
  )
}

.resumen_longitudes_bloques <- function(x, k = NULL, tamano = 10000L,
                                        max_bytes = Inf) {
  .ejecutar_vector_bloques(
    x, "longitudes", k = k, tamano = tamano, max_bytes = max_bytes
  )$resultado$resultado
}

.resumen_cuantitativo_bloques <- function(x, k = NULL, tamano = 10000L,
                                          contar_signos = TRUE,
                                          max_bytes = Inf) {
  ejecucion <- .ejecutar_vector_bloques(
    x, "cuantitativos", k = k, tamano = tamano, max_bytes = max_bytes,
    configuracion = list(contar_signos = isTRUE(contar_signos))
  )
  salida <- ejecucion$resultado$resultado
  if (!isTRUE(contar_signos)) {
    salida$n_ceros <- 0L
    salida$n_negativos <- 0L
  }
  salida
}

.iniciar_vigilante <- function(corrida_id = paste0("memoria-", Sys.getpid()),
                               tope_bytes = Inf) {
  tope_bytes <- .validar_tope_bloques(tope_bytes, "tope_bytes")
  vigilante <- new.env(parent = baseenv())
  class(vigilante) <- c("vigilante_bloques", "environment")
  vigilante$corrida_id <- as.character(corrida_id)
  vigilante$tope_bytes <- tope_bytes
  vigilante$eventos <- data.frame(
    corrida_id = character(), fase = character(), familia = character(),
    bloque_id = integer(), bytes_retenidos = numeric(), bytes_resultado = numeric(),
    gc = logical(), lectura_proceso = numeric(), factor_pico = numeric(),
    memoria_max_intervalo = numeric(), sonda_proceso = character(),
    residentes_lsh = I(list()), evento_id = character(),
    tipo_evento = character(), motivo = character(),
    stringsAsFactors = FALSE
  )
  vigilante
}

.registrar_barrera_vigilante <- function(vigilante, fase, familia, bloque_id,
                                         acumulador, resultado = NULL) {
  if (!inherits(vigilante, "vigilante_bloques")) {
    stop("`vigilante` no es un vigilante de bloques.", call. = FALSE)
  }
  bytes <- .bytes_retenidos(acumulador)
  bytes_resultado <- if (is.null(resultado)) 0 else {
    as.numeric(utils::object.size(resultado))
  }
  estado_lsh <- if (identical(familia, "lsh")) {
    acumulador$estado_familia
  } else NULL
  residentes_lsh <- if (!is.null(estado_lsh)) {
    estado_lsh$residentes_lsh %||% list()
  } else list()
  rss <- if (exists(".rss_proceso_lsh", mode = "function")) {
    .rss_proceso_lsh()
  } else NA_real_
  en_presion <- is.finite(vigilante$tope_bytes) &&
    (bytes + bytes_resultado > vigilante$tope_bytes)
  evento_id <- paste0(vigilante$corrida_id, "-", nrow(vigilante$eventos) + 1L)
  fila <- data.frame(
    corrida_id = vigilante$corrida_id, fase = as.character(fase),
    familia = as.character(familia), bloque_id = as.integer(bloque_id),
    bytes_retenidos = bytes, bytes_resultado = bytes_resultado,
    gc = TRUE, lectura_proceso = rss,
    factor_pico = if (is.null(estado_lsh)) NA_real_ else {
      estado_lsh$factor_pico %||% NA_real_
    },
    memoria_max_intervalo = if (is.null(estado_lsh)) NA_real_ else {
      estado_lsh$maximo_intervalo %||% 0
    },
    sonda_proceso = if (is.finite(rss)) "rss" else "no_disponible",
    residentes_lsh = I(list(residentes_lsh)),
    evento_id = evento_id,
    tipo_evento = if (en_presion) .EVENTO_PRESION_MEMORIA else NA_character_,
    motivo = if (en_presion) "tope_artificial" else NA_character_,
    stringsAsFactors = FALSE
  )
  vigilante$eventos <- rbind(vigilante$eventos, fila)
  invisible(fila)
}

.eventos_vigilante <- function(vigilante) {
  if (!inherits(vigilante, "vigilante_bloques")) return(NULL)
  vigilante$eventos
}

.conteos_filas_duplicadas_bloques <- function(datos, k = NULL,
                                              max_entradas = Inf,
                                              max_bytes = Inf) {
  datos <- .expandir_datos_identidad(datos)
  bloques <- .particionar_bloques(nrow(datos), k = k)
  bloques <- lapply(bloques, function(bloque) {
    bloque$valores <- datos[bloque$valores, , drop = FALSE]
    bloque
  })
  acumulador <- .iniciar_acumulador(
    "filas", "data.frame", familia = "filas_distintos",
    max_entradas = max_entradas, max_bytes = max_bytes,
    requiere_orden = TRUE
  )
  resultado <- .ejecutar_acumulador_bloques(acumulador, bloques)$resultado
  if (!identical(resultado$estado, "calculado")) return(resultado)
  mapa <- resultado$resultado
  frecuencias <- mapa$frecuencias
  list(
    filas_duplicadas = as.integer(sum(frecuencias - 1L)),
    filas_en_grupos_duplicados = as.integer(sum(frecuencias[frecuencias > 1L])),
    estado = resultado$estado,
    exacto = resultado$exacto,
    mapa = mapa,
    sobre = resultado
  )
}

# Nombres del contrato, deliberadamente internos y no exportados.
iniciar <- function(...) .iniciar_acumulador(...)
absorber <- function(acumulador, bloque) .absorber_acumulador(acumulador, bloque)
fusionar <- function(acumulador, otro) .fusionar_acumuladores(acumulador, otro)
finalizar <- function(acumulador) .finalizar_acumulador(acumulador)
bytes_retenidos <- function(acumulador) .bytes_retenidos(acumulador)

# ---------------------------------------------------------------------------
# Etapa 2: familias de VALOR
#
# El mapa de `distintos` es el estado central. Las funciones de abajo lo
# consumen sin expandir `rep(valor, frecuencia)`. Cuando el mapa no llega
# completo, las familias que dependen de su multiset se declaran no disponibles;
# no reutilizan el prefijo residente como si fuera el universo.

.dentro_tolerancia_bloques <- function(observado, esperado, tolerancia) {
  abs(observado - esperado) <=
    tolerancia * pmax(1, abs(observado), abs(esperado))
}

.sobre_valor_bloques <- function(resultado, estado = "calculado",
                                 exacto = TRUE, motivo = NA_character_,
                                 como_resolverlo = NA_character_,
                                 cota = NULL, max_entradas = 5000L,
                                 max_bytes = .MAX_BYTES_ESTADO_BLOQUES,
                                 alcance = list(), almacenamiento = "memoria",
                                 derrame = NULL, bytes_retenidos = NULL) {
  if (is.infinite(max_entradas)) max_entradas <- .MAX_ENTRADAS_DISTINTOS
  if (is.infinite(max_bytes)) max_bytes <- .MAX_BYTES_ESTADO_BLOQUES
  if (is.null(bytes_retenidos)) {
    bytes_retenidos <- as.numeric(utils::object.size(resultado))
  }
  if (!is.finite(bytes_retenidos) || bytes_retenidos < 0) {
    bytes_retenidos <- 0
  }
  motivo <- if (is.null(motivo) || !length(motivo) ||
      (is.na(motivo[[1L]]) && !identical(estado, "calculado"))) {
    if (identical(estado, "cota")) "resultado_acotado" else {
      if (identical(estado, "no_disponible")) "resultado_no_disponible" else NA_character_
    }
  } else as.character(motivo)
  como_resolverlo <- if (is.null(como_resolverlo) || !length(como_resolverlo) ||
      (is.na(como_resolverlo[[1L]]) && !identical(estado, "calculado"))) {
    if (identical(estado, "cota")) "Aumentar el tope o habilitar un derrame exacto." else {
      if (identical(estado, "no_disponible")) "Revisar el tipo, el alcance o el tope declarado." else NA_character_
    }
  } else as.character(como_resolverlo)
  list(
    resultado = resultado,
    estado = estado,
    exacto = if (identical(estado, "calculado")) isTRUE(exacto) else {
      if (identical(estado, "cota")) FALSE else NA
    },
    motivo = motivo,
    como_resolverlo = como_resolverlo,
    cota = cota,
    tope = list(
      entradas = max_entradas, bytes = max_bytes,
      nombre = "E_distintos/P_estado"
    ),
    almacenamiento = almacenamiento,
    derrame = derrame,
    alcance = alcance,
    bytes_retenidos = as.numeric(bytes_retenidos)
  )
}

.alcance_valor_bloques <- function(sobre, n_valores = NA_real_) {
  alcance <- if (is.null(sobre)) list() else sobre$alcance
  alcance$valores <- n_valores
  alcance
}

.mapa_consultable_bloques <- function(sobre) {
  !is.null(sobre) && identical(sobre$estado, "calculado") &&
    isTRUE(sobre$exacto) && is.data.frame(sobre$resultado) &&
    all(c("representante", "frecuencia") %in% names(sobre$resultado))
}

.ordenar_mapa_numerico_bloques <- function(sobre) {
  if (!.mapa_consultable_bloques(sobre)) return(NULL)
  mapa <- sobre$resultado
  representantes <- mapa$representante
  finitos <- tryCatch(is.finite(representantes), error = function(e) NULL)
  if (is.null(finitos) || length(finitos) != nrow(mapa)) return(NULL)
  if (!all(finitos)) {
    mapa <- mapa[finitos, , drop = FALSE]
    representantes <- mapa$representante
  }
  if (!is.numeric(representantes) || !length(representantes)) {
    return(list(representantes = numeric(), frecuencias = numeric()))
  }
  orden <- tryCatch(order(representantes), error = function(e) NULL)
  if (is.null(orden) || length(orden) != length(representantes)) return(NULL)
  list(
    representantes = as.numeric(representantes[orden]),
    frecuencias = as.numeric(mapa$frecuencia[orden])
  )
}

.valor_orden_ponderado_bloques <- function(representantes, frecuencias,
                                           posicion) {
  n <- sum(frecuencias)
  if (!length(representantes) || !is.finite(n) || n < 1 ||
      length(representantes) != length(frecuencias)) return(NA_real_)
  posicion <- max(1, min(n, as.numeric(posicion)))
  indice <- findInterval(posicion - 1, cumsum(frecuencias)) + 1L
  as.numeric(representantes[[indice]])
}

.cuantil_ponderado_type7_bloques <- function(representantes, frecuencias,
                                             prob) {
  n <- sum(frecuencias)
  if (!length(representantes) || !is.finite(n) || n < 1) return(NA_real_)
  h <- (n - 1) * prob + 1
  j <- floor(h)
  gamma <- h - j
  x_j <- .valor_orden_ponderado_bloques(representantes, frecuencias, j)
  if (j >= n) return(x_j)
  x_j1 <- .valor_orden_ponderado_bloques(
    representantes, frecuencias, j + 1
  )
  # Esta es la fórmula de type = 7, incluida la secuencia de operaciones de
  # `quantile.default()`. Aunque sea algebraicamente equivalente a
  # `x[j] + gamma * (x[j + 1] - x[j])`, R evalúa la interpolación como
  # `(1 - h) * x[j] + h * x[j + 1]`; los dos caminos pueden diferir un bit.
  if (!gamma || identical(x_j, x_j1)) return(x_j)
  (1 - gamma) * x_j + gamma * x_j1
}

.mediana_ponderada_bloques <- function(representantes, frecuencias) {
  n <- sum(frecuencias)
  if (!length(representantes) || !is.finite(n) || n < 1) return(NA_real_)
  if (n %% 2) {
    return(.valor_orden_ponderado_bloques(
      representantes, frecuencias, (n + 1) / 2
    ))
  }
  inferior <- .valor_orden_ponderado_bloques(
    representantes, frecuencias, n / 2
  )
  superior <- .valor_orden_ponderado_bloques(
    representantes, frecuencias, n / 2 + 1
  )
  # `median.default()` llama a `mean()` sobre los dos centrales. En ciertos
  # pares de doubles, `mean(c(...))` y `(a + b) / 2` difieren en el ultimo
  # bit; conservar la primera secuencia es parte de la identidad prometida.
  mean(c(inferior, superior))
}

.estadisticos_orden_mapa_bloques <- function(sobre) {
  ordenado <- .ordenar_mapa_numerico_bloques(sobre)
  if (is.null(ordenado)) return(NULL)
  n <- sum(ordenado$frecuencias)
  if (!length(ordenado$representantes)) {
    return(list(
      q1 = NA_real_, mediana = NA_real_, q3 = NA_real_, iqr = NA_real_,
      n_evaluados = 0L
    ))
  }
  q1 <- .cuantil_ponderado_type7_bloques(
    ordenado$representantes, ordenado$frecuencias, 0.25
  )
  q3 <- .cuantil_ponderado_type7_bloques(
    ordenado$representantes, ordenado$frecuencias, 0.75
  )
  list(
    q1 = q1, mediana = .mediana_ponderada_bloques(
      ordenado$representantes, ordenado$frecuencias
    ), q3 = q3, iqr = q3 - q1,
    n_evaluados = as.numeric(n)
  )
}

.cuantiles_desde_mapa_bloques <- function(sobre, max_entradas = 5000L,
                                          max_bytes = .MAX_BYTES_ESTADO_BLOQUES) {
  estadisticos <- .estadisticos_orden_mapa_bloques(sobre)
  if (is.null(estadisticos)) {
    return(.sobre_valor_bloques(
      NULL, estado = "no_disponible",
      motivo = "mapa_distintos_truncado",
      como_resolverlo = "Aumentar el tope del mapa o habilitar un derrame exacto.",
      max_entradas = max_entradas, max_bytes = max_bytes,
      alcance = .alcance_valor_bloques(sobre), bytes_retenidos = 0
    ))
  }
  .sobre_valor_bloques(
    estadisticos, max_entradas = max_entradas, max_bytes = max_bytes,
    alcance = .alcance_valor_bloques(sobre, estadisticos$n_evaluados),
    bytes_retenidos = as.numeric(utils::object.size(estadisticos))
  )
}

.mediana_desde_mapa_bloques <- function(sobre, ...) {
  cuantiles <- .cuantiles_desde_mapa_bloques(sobre, ...)
  if (!identical(cuantiles$estado, "calculado")) return(cuantiles)
  cuantiles$resultado <- list(
    mediana = cuantiles$resultado$mediana,
    n_evaluados = cuantiles$resultado$n_evaluados
  )
  cuantiles$bytes_retenidos <- as.numeric(utils::object.size(cuantiles$resultado))
  cuantiles
}

.cuartiles_desde_mapa_bloques <- function(sobre, ...) {
  cuantiles <- .cuantiles_desde_mapa_bloques(sobre, ...)
  if (!identical(cuantiles$estado, "calculado")) return(cuantiles)
  cuantiles$resultado <- list(
    q1 = cuantiles$resultado$q1,
    q3 = cuantiles$resultado$q3,
    iqr = cuantiles$resultado$iqr,
    n_evaluados = cuantiles$resultado$n_evaluados
  )
  cuantiles$bytes_retenidos <- as.numeric(utils::object.size(cuantiles$resultado))
  cuantiles
}

# Alias internos descriptivos usados por los consumidores de la etapa.
.reconstruir_mediana_mapa <- .mediana_desde_mapa_bloques
.reconstruir_cuartiles_mapa <- .cuartiles_desde_mapa_bloques

.limites_tukey_bloques <- function(cuantiles) {
  if (is.null(cuantiles) || !identical(cuantiles$estado, "calculado") ||
      !is.finite(cuantiles$resultado$q1) ||
      !is.finite(cuantiles$resultado$q3)) return(NULL)
  iqr <- cuantiles$resultado$q3 - cuantiles$resultado$q1
  if (!is.finite(iqr)) return(NULL)
  list(
    q1 = cuantiles$resultado$q1,
    q3 = cuantiles$resultado$q3,
    iqr = iqr,
    inferior = cuantiles$resultado$q1 - 1.5 * iqr,
    superior = cuantiles$resultado$q3 + 1.5 * iqr
  )
}

.cuantiles_fijos_bloques <- function(q1, q3, n_filas, max_entradas,
                                     max_bytes) {
  q1 <- suppressWarnings(as.numeric(q1))
  q3 <- suppressWarnings(as.numeric(q3))
  if (length(q1) != 1L || length(q3) != 1L) {
    return(.sobre_valor_bloques(
      NULL, estado = "no_disponible", motivo = "cuartiles_no_disponibles",
      como_resolverlo = "Proveer Q1 y Q3 escalares y finitos.",
      max_entradas = max_entradas, max_bytes = max_bytes,
      alcance = list(filas = n_filas, valores = NA_real_)
    ))
  }
  .sobre_valor_bloques(
    list(q1 = q1, q3 = q3, iqr = q3 - q1, n_evaluados = NA_real_),
    alcance = list(filas = n_filas, valores = NA_real_),
    max_entradas = max_entradas, max_bytes = max_bytes
  )
}

.n_outliers_valor_bloques <- function(x, cuantiles = NULL, q1 = NULL, q3 = NULL,
                                      k = NULL, tamano = 10000L,
                                      aplicable = NULL,
                                      max_entradas = Inf, max_bytes = Inf,
                                      mapa = NULL, vigilante = NULL) {
  if (is.infinite(max_entradas)) max_entradas <- .MAX_ENTRADAS_DISTINTOS
  if (is.infinite(max_bytes)) max_bytes <- .MAX_BYTES_ESTADO_BLOQUES
  if (!is.numeric(x)) {
    return(.sobre_valor_bloques(
      NULL, estado = "no_disponible",
      motivo = "valores_outliers_no_numericos"
    ))
  }
  if (is.null(cuantiles)) {
    if (!is.null(q1) || !is.null(q3)) {
      cuantiles <- .cuantiles_fijos_bloques(
        q1, q3, length(x), max_entradas, max_bytes
      )
    } else {
      if (is.null(mapa)) {
        mapa <- .ejecutar_vector_bloques(
          x, "distintos", k = k, tamano = tamano,
          max_entradas = max_entradas, max_bytes = max_bytes,
          aplicable = aplicable, vigilante = vigilante
        )$resultado
      }
      cuantiles <- .cuantiles_desde_mapa_bloques(
        mapa, max_entradas = max_entradas, max_bytes = max_bytes
      )
    }
  }
  limites <- .limites_tukey_bloques(cuantiles)
  if (is.null(limites)) {
    return(.sobre_valor_bloques(
      NULL, estado = "no_disponible",
      motivo = if (is.null(cuantiles) ||
          !identical(cuantiles$estado, "calculado")) {
        "mapa_distintos_truncado"
      } else "cuartiles_no_disponibles",
      como_resolverlo = "Conservar un multiset completo o habilitar una segunda pasada estable.",
      max_entradas = max_entradas, max_bytes = max_bytes,
      alcance = cuantiles$alcance %||% list()
    ))
  }
  ejecucion <- .ejecutar_vector_bloques(
    x, "outliers", k = k, tamano = tamano,
    configuracion = list(
      limite_inferior = limites$inferior,
      limite_superior = limites$superior
    ), max_entradas = max_entradas, max_bytes = max_bytes,
    aplicable = aplicable, vigilante = vigilante
  )
  sobre <- ejecucion$resultado
  sobre$alcance$cuartiles <- c(q1 = limites$q1, q3 = limites$q3,
                               iqr = limites$iqr)
  sobre
}

.resultado_centinela_mapa_bloques <- function(mapa, n_evaluados, iqr,
                                              q1, q3,
                                              sentinelas_numericos = NULL) {
  vacio <- list(valor = NA_real_, n = NA_integer_,
                densidad_sin_centinela = NA_real_)
  if (!is.finite(iqr) || iqr <= 0 || n_evaluados < 20L ||
      !is.data.frame(mapa) || !nrow(mapa)) return(vacio)
  fuera <- mapa$representante < q1 - 1.5 * iqr |
    mapa$representante > q3 + 1.5 * iqr
  fuera[is.na(fuera)] <- FALSE
  if (!any(fuera)) return(vacio)
  candidatos <- mapa[fuera, c("representante", "frecuencia"), drop = FALSE]
  candidatos <- candidatos[candidatos$frecuencia >= .MIN_REPETICIONES_CENTINELA,
                           , drop = FALSE]
  if (!nrow(candidatos)) return(vacio)
  con_forma <- candidatos$representante[
    grepl("^-?([0-9])\\1{2,}$", as.character(candidatos$representante))
  ]
  if (!length(con_forma)) return(vacio)
  declarados <- suppressWarnings(as.numeric(sentinelas_numericos))
  declarados <- declarados[is.finite(declarados)]
  con_forma <- setdiff(con_forma, declarados)
  if (!length(con_forma)) return(vacio)
  orden <- tryCatch(order(con_forma), error = function(e) seq_along(con_forma))
  con_forma <- con_forma[orden]
  elegido <- con_forma[[which.max(abs(con_forma))]]
  posicion <- which(candidatos$representante == elegido)[[1L]]
  restantes <- mapa$representante[mapa$representante != elegido]
  restantes <- restantes[is.finite(restantes)]
  distintos <- unique(restantes)
  densidad <- if (length(distintos) > 1L) {
    rango <- max(distintos) - min(distintos) + 1
    if (is.finite(rango) && rango > 0) length(distintos) / rango else NA_real_
  } else NA_real_
  list(
    valor = as.numeric(elegido),
    n = as.integer(candidatos$frecuencia[[posicion]]),
    densidad_sin_centinela = as.numeric(densidad)
  )
}

.centinela_valor_bloques <- function(x, cuantiles = NULL, q1 = NULL, q3 = NULL,
                                     iqr = NULL, k = NULL, tamano = 10000L,
                                     aplicable = NULL,
                                     sentinelas_numericos = NULL,
                                     max_entradas = Inf, max_bytes = Inf,
                                     mapa = NULL, vigilante = NULL) {
  if (is.infinite(max_entradas)) max_entradas <- .MAX_ENTRADAS_DISTINTOS
  if (is.infinite(max_bytes)) max_bytes <- .MAX_BYTES_ESTADO_BLOQUES
  if (!is.numeric(x)) {
    return(.sobre_valor_bloques(
      NULL, estado = "no_disponible",
      motivo = "valores_centinela_no_numericos"
    ))
  }
  mapa_central <- mapa
  if (is.null(cuantiles)) {
    if (!is.null(q1) || !is.null(q3)) {
      cuantiles <- .cuantiles_fijos_bloques(
        q1, q3, length(x), max_entradas, max_bytes
      )
    } else {
      if (is.null(mapa)) {
        mapa <- .ejecutar_vector_bloques(
          x, "distintos", k = k, tamano = tamano,
          max_entradas = max_entradas, max_bytes = max_bytes,
          aplicable = aplicable, vigilante = vigilante
        )$resultado
      }
      mapa_central <- mapa
      cuantiles <- .cuantiles_desde_mapa_bloques(
        mapa, max_entradas = max_entradas, max_bytes = max_bytes
      )
    }
  }
  # La frecuencia del candidato ya vive en el acumulador de la segunda
  # pasada. La densidad, en cambio, es una salida de la señal existente y
  # necesita el mapa completo sin el centinela; se intenta conservarla cuando
  # el tope permite ese mapa, sin confundir un prefijo truncado con el total.
  if (is.null(mapa_central)) {
    mapa_central <- .ejecutar_vector_bloques(
      x, "distintos", k = k, tamano = tamano,
      max_entradas = max_entradas, max_bytes = max_bytes,
      aplicable = aplicable, vigilante = vigilante
    )$resultado
  }
  limites <- .limites_tukey_bloques(cuantiles)
  if (is.null(limites)) {
    return(.sobre_valor_bloques(
      NULL, estado = "no_disponible", motivo = "mapa_distintos_truncado",
      como_resolverlo = "Conservar un multiset completo o habilitar una segunda pasada estable.",
      max_entradas = max_entradas, max_bytes = max_bytes,
      alcance = cuantiles$alcance %||% list()
    ))
  }
  if (is.null(iqr)) iqr <- limites$iqr
  ejecucion <- .ejecutar_vector_bloques(
    x, "centinela", k = k, tamano = tamano,
    configuracion = list(
      limite_inferior = limites$inferior,
      limite_superior = limites$superior
    ), max_entradas = max_entradas, max_bytes = max_bytes,
    aplicable = aplicable, vigilante = vigilante
  )
  sobre <- ejecucion$resultado
  if (!identical(sobre$estado, "calculado")) return(sobre)
  mapa_para_densidad <- if (.mapa_consultable_bloques(mapa_central)) {
    mapa_central$resultado
  } else {
    data.frame(
      representante = sobre$resultado$representantes,
      frecuencia = sobre$resultado$frecuencias,
      stringsAsFactors = FALSE
    )
  }
  sobre$resultado <- .resultado_centinela_mapa_bloques(
    mapa_para_densidad, sobre$resultado$n_evaluados, iqr,
    limites$q1, limites$q3, sentinelas_numericos
  )
  sobre$bytes_retenidos <- as.numeric(utils::object.size(sobre$resultado))
  sobre$alcance$cuartiles <- c(q1 = limites$q1, q3 = limites$q3, iqr = iqr)
  sobre
}

.hueco_tipico_desde_mapa_bloques <- function(sobre,
                                             max_entradas = 5000L,
                                             max_bytes = .MAX_BYTES_ESTADO_BLOQUES) {
  ordenado <- .ordenar_mapa_numerico_bloques(sobre)
  if (is.null(ordenado)) {
    return(.sobre_valor_bloques(
      NULL, estado = "no_disponible", motivo = "mapa_distintos_truncado",
      como_resolverlo = "Aumentar el tope del mapa o habilitar un derrame exacto.",
      max_entradas = max_entradas, max_bytes = max_bytes,
      alcance = .alcance_valor_bloques(sobre), bytes_retenidos = 0
    ))
  }
  distintos <- ordenado$representantes
  if (!length(distintos)) {
    resultado <- list(hueco_tipico = NA_real_, hueco_maximo = NA_real_,
                      n_huecos = NA_real_, n_distintos = 0L)
  } else if (any(distintos != floor(distintos))) {
    resultado <- list(hueco_tipico = NA_real_, hueco_maximo = NA_real_,
                      n_huecos = NA_real_, n_distintos = length(distintos))
  } else if (length(distintos) == 1L) {
    resultado <- list(hueco_tipico = NA_real_, hueco_maximo = 0,
                      n_huecos = 0, n_distintos = 1L)
  } else {
    huecos <- diff(distintos)
    resultado <- list(
      hueco_tipico = stats::median(huecos),
      hueco_maximo = max(huecos),
      n_huecos = sum(huecos - 1),
      n_distintos = length(distintos)
    )
  }
  .sobre_valor_bloques(
    resultado, max_entradas = max_entradas, max_bytes = max_bytes,
    alcance = .alcance_valor_bloques(sobre, length(distintos)),
    bytes_retenidos = as.numeric(utils::object.size(resultado))
  )
}

.hueco_tipico_mapa_distintos <- .hueco_tipico_desde_mapa_bloques

.hueco_tipico_valor_bloques <- function(x, k = NULL, tamano = 10000L,
                                        aplicable = NULL,
                                        max_entradas = Inf, max_bytes = Inf,
                                        vigilante = NULL) {
  if (is.infinite(max_entradas)) max_entradas <- .MAX_ENTRADAS_DISTINTOS
  if (is.infinite(max_bytes)) max_bytes <- .MAX_BYTES_ESTADO_BLOQUES
  if (!is.numeric(x)) {
    return(.sobre_valor_bloques(
      NULL, estado = "no_disponible", motivo = "valores_hueco_no_numericos",
      max_entradas = max_entradas, max_bytes = max_bytes
    ))
  }
  ejecucion <- .ejecutar_vector_bloques(
    x, "hueco", k = k, tamano = tamano,
    max_entradas = max_entradas, max_bytes = max_bytes,
    aplicable = aplicable, vigilante = vigilante
  )
  sobre <- .hueco_tipico_desde_mapa_bloques(
    ejecucion$resultado, max_entradas = max_entradas,
    max_bytes = max_bytes
  )
  sobre$acumulador <- ejecucion$acumulador
  sobre
}

.hueco_tipico_bloques <- .hueco_tipico_valor_bloques

.k_aritmetica_bloques <- function(base, respuesta, k_bloques = NULL,
                                  tamano = 10000L, min_filas = 3L,
                                  tolerancia = 1e-8, aplicable = NULL,
                                  max_entradas = Inf, max_bytes = Inf,
                                  vigilante = NULL) {
  if (is.infinite(max_entradas)) max_entradas <- .MAX_ENTRADAS_DISTINTOS
  if (is.infinite(max_bytes)) max_bytes <- .MAX_BYTES_ESTADO_BLOQUES
  if (length(base) != length(respuesta) ||
      !is.numeric(base) || !is.numeric(respuesta)) {
    return(.sobre_valor_bloques(
      NULL, estado = "no_disponible",
      motivo = "valores_aritmeticos_no_pareados"
    ))
  }
  if (length(aplicable) &&
      (!is.logical(aplicable) || length(aplicable) != length(base))) {
    stop("`aplicable` debe ser logico y tener el largo de los vectores.",
         call. = FALSE)
  }
  if (is.null(aplicable)) aplicable <- rep(TRUE, length(base))
  utilizables <- !is.na(aplicable) & aplicable & is.finite(base) &
    is.finite(respuesta) & base != 0
  if (sum(utilizables) < min_filas) {
    return(.sobre_valor_bloques(
      NULL, estado = "no_disponible",
      motivo = "relacion_aritmetica:min_filas_comparables",
      como_resolverlo = "Aumentar el universo o bajar `min_filas`.",
      alcance = list(filas = length(base), valores = sum(utilizables))
    ))
  }
  ratios <- respuesta[utilizables] / base[utilizables]
  mapa_ratios <- .ejecutar_vector_bloques(
    ratios, "distintos", k = k_bloques, tamano = tamano,
    max_entradas = max_entradas, max_bytes = max_bytes,
    vigilante = vigilante
  )$resultado
  mediana_ratio <- .mediana_desde_mapa_bloques(
    mapa_ratios, max_entradas = max_entradas, max_bytes = max_bytes
  )
  if (!identical(mediana_ratio$estado, "calculado")) {
    mediana_ratio$motivo <- "mapa_distintos_truncado"
    mediana_ratio$como_resolverlo <-
      "Aumentar el tope del mapa o habilitar una segunda pasada estable."
    return(mediana_ratio)
  }
  k <- mediana_ratio$resultado$mediana
  if (!is.finite(k) || k == 0 || abs(abs(k) - 1) <= tolerancia) {
    return(.sobre_valor_bloques(
      list(k = as.numeric(k), n_evaluados = 0L, n_cumplen = 0L,
           n_incumplen = 0L, proporcion = NA_real_),
      alcance = list(filas = length(base), valores = sum(utilizables)),
      max_entradas = max_entradas, max_bytes = max_bytes
    ))
  }
  base_evaluada <- base
  respuesta_evaluada <- respuesta
  if (abs(k) > 1) {
    base_evaluada <- respuesta
    respuesta_evaluada <- base
    utilizables <- !is.na(aplicable) & aplicable & is.finite(base_evaluada) &
      is.finite(respuesta_evaluada) & base_evaluada != 0
    ratios <- respuesta_evaluada[utilizables] / base_evaluada[utilizables]
    mapa_ratios <- .ejecutar_vector_bloques(
      ratios, "distintos", k = k_bloques, tamano = tamano,
      max_entradas = max_entradas, max_bytes = max_bytes,
      vigilante = vigilante
    )$resultado
    mediana_ratio <- .mediana_desde_mapa_bloques(
      mapa_ratios, max_entradas = max_entradas, max_bytes = max_bytes
    )
    if (!identical(mediana_ratio$estado, "calculado")) {
      mediana_ratio$motivo <- "mapa_distintos_truncado"
      return(mediana_ratio)
    }
    k <- mediana_ratio$resultado$mediana
  }
  comparables <- !is.na(aplicable) & aplicable & is.finite(base_evaluada) &
    is.finite(respuesta_evaluada)
  if (sum(comparables) < min_filas) {
    return(.sobre_valor_bloques(
      NULL, estado = "no_disponible",
      motivo = "relacion_aritmetica:min_filas_comparables",
      alcance = list(filas = length(base), valores = sum(comparables))
    ))
  }
  variar <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 3L) return(FALSE)
    extremos <- range(x)
    (extremos[[2L]] - extremos[[1L]]) >
      tolerancia * max(1, abs(extremos))
  }
  if (!variar(base_evaluada[comparables]) ||
      !variar(respuesta_evaluada[comparables])) {
    return(.sobre_valor_bloques(
      list(k = as.numeric(k), n_evaluados = 0L, n_cumplen = 0L,
           n_incumplen = 0L, proporcion = NA_real_),
      alcance = list(filas = length(base), valores = sum(comparables)),
      max_entradas = max_entradas, max_bytes = max_bytes
    ))
  }
  indices <- .particionar_bloques(length(base), k = k_bloques, tamano = tamano)
  bloques <- lapply(indices, function(indice) {
    posiciones <- indice$valores
    indice$valores <- data.frame(
      base = base_evaluada[posiciones],
      respuesta = respuesta_evaluada[posiciones],
      stringsAsFactors = FALSE
    )
    if (length(aplicable)) indice$aplicable <- aplicable[posiciones]
    indice
  })
  acumulador <- .iniciar_acumulador(
    "aritmetica", "data.frame", familia = "aritmetica",
    configuracion = list(k = as.numeric(k), tolerancia = as.numeric(tolerancia)),
    max_entradas = max_entradas, max_bytes = max_bytes
  )
  ejecucion <- .ejecutar_acumulador_bloques(
    acumulador, bloques, vigilante = vigilante, familia = "k_aritmetica"
  )
  sobre <- ejecucion$resultado
  sobre$resultado$k <- as.numeric(k)
  sobre$alcance <- list(
    filas = length(base), valores = sobre$resultado$n_evaluados,
    orden = "orden_entrada", snapshot = "memoria", muestra = NULL
  )
  sobre
}

.relacion_proporcional_bloques <- .k_aritmetica_bloques

.relaciones_aritmeticas_bloques <- function(datos, columnas = NULL,
                                            k = NULL, ...) {
  if (!inherits(datos, "data.frame") || ncol(datos) < 2L) {
    return(list())
  }
  if (is.null(columnas)) {
    columnas <- which(vapply(datos, is.numeric, logical(1L)))
  }
  if (length(columnas) < 2L) return(list())
  pares <- utils::combn(columnas, 2L, simplify = FALSE)
  nombres <- names(datos)
  salida <- lapply(pares, function(par) {
    .k_aritmetica_bloques(datos[[par[[1L]]]], datos[[par[[2L]]]],
                          k_bloques = k, ...)
  })
  names(salida) <- vapply(pares, function(par) {
    paste(nombres[par], collapse = " ~ ")
  }, character(1L))
  salida
}

.ejecutar_valor_bloques <- function(x, k = NULL, tamano = 10000L,
                                    aplicable = NULL,
                                    sentinelas_numericos = NULL,
                                    max_entradas = Inf, max_bytes = Inf,
                                    vigilante = NULL) {
  if (is.infinite(max_entradas)) max_entradas <- .MAX_ENTRADAS_DISTINTOS
  if (is.infinite(max_bytes)) max_bytes <- .MAX_BYTES_ESTADO_BLOQUES
  if (!is.numeric(x)) {
    fallo <- .sobre_valor_bloques(
      NULL, estado = "no_disponible", motivo = "valores_no_numericos",
      max_entradas = max_entradas, max_bytes = max_bytes
    )
    return(list(mapa = fallo, cuantiles = fallo, mediana = fallo,
                n_outliers = fallo, centinela = fallo, hueco_tipico = fallo,
                vigilante = vigilante))
  }
  central <- .ejecutar_vector_bloques(
    x, "distintos", k = k, tamano = tamano,
    max_entradas = max_entradas, max_bytes = max_bytes,
    aplicable = aplicable, vigilante = vigilante
  )
  mapa <- central$resultado
  cuantiles <- .cuantiles_desde_mapa_bloques(
    mapa, max_entradas = max_entradas, max_bytes = max_bytes
  )
  if (!is.null(vigilante)) {
    gc(verbose = FALSE)
    .registrar_barrera_vigilante(
      vigilante, "finalizar", "cuantiles", length(.particionar_bloques(
        length(x), k = k, tamano = tamano
      )), acumulador = central$acumulador,
      resultado = cuantiles
    )
  }
  outliers <- .n_outliers_valor_bloques(
    x, cuantiles = cuantiles, k = k, tamano = tamano,
    aplicable = aplicable, max_entradas = max_entradas,
    max_bytes = max_bytes, vigilante = vigilante
  )
  centinela <- .centinela_valor_bloques(
    x, cuantiles = cuantiles, k = k, tamano = tamano,
    aplicable = aplicable, sentinelas_numericos = sentinelas_numericos,
    max_entradas = max_entradas, max_bytes = max_bytes,
    mapa = mapa, vigilante = vigilante
  )
  hueco <- .hueco_tipico_desde_mapa_bloques(
    mapa, max_entradas = max_entradas, max_bytes = max_bytes
  )
  if (!is.null(vigilante)) {
    gc(verbose = FALSE)
    .registrar_barrera_vigilante(
      vigilante, "finalizar", "hueco_tipico", length(.particionar_bloques(
        length(x), k = k, tamano = tamano
      )), acumulador = central$acumulador, resultado = hueco
    )
  }
  list(
    mapa = mapa, cuantiles = cuantiles,
    mediana = .mediana_desde_mapa_bloques(mapa,
      max_entradas = max_entradas, max_bytes = max_bytes),
    n_outliers = outliers, centinela = centinela,
    hueco_tipico = hueco, vigilante = vigilante,
    acumulador_central = central$acumulador
  )
}

# ---------------------------------------------------------------------------
# Etapa 3: familias de ÍNDICE
#
# Estas familias tienen una propiedad que no comparten los contadores: el
# orden de llegada forma parte del resultado. Un bloque sólo conoce su rango
# global; por eso los acumuladores guardan ordinales y no posiciones locales.
# La presentación (head, primeros ejemplos) se decide una vez, sobre el estado
# completo, después de absorber todos los bloques.

.indices_muestra_globales <- function(n, muestra) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || !is.finite(n) ||
      n < 0 || n != floor(n)) {
    stop("`n` debe ser un entero no negativo y conocido.", call. = FALSE)
  }
  n <- as.numeric(n)
  limite <- .validar_muestra(muestra)
  if (!n || !limite) return(integer())
  limite <- min(n, limite)
  if (n <= limite) return(seq_len(as.integer(n)))
  unique(as.integer(round(seq.int(1L, n, length.out = limite))))
}

.indices_muestra_bloque <- function(ordinal_inicio, ordinal_fin,
                                    n, muestra) {
  if (length(ordinal_inicio) != 1L || length(ordinal_fin) != 1L ||
      is.na(ordinal_inicio) || is.na(ordinal_fin) ||
      ordinal_inicio < 1L || ordinal_fin < ordinal_inicio - 1L) {
    stop("El intervalo del bloque no es valido.", call. = FALSE)
  }
  indices <- .indices_muestra_globales(n, muestra)
  if (!length(indices) || ordinal_fin < ordinal_inicio) return(integer())
  as.integer(indices[indices >= ordinal_inicio & indices <= ordinal_fin])
}

# Alias con nombres explícitos para los adaptadores que construyen bloques.
.indices_muestreo_bloque <- .indices_muestra_bloque
.indices_muestreo_globales <- .indices_muestra_globales

.bloques_de_vector_muestreados <- function(x, muestra, k = NULL,
                                           tamano = 10000L) {
  n <- length(x)
  indices_globales <- .indices_muestra_globales(n, muestra)
  bloques <- .particionar_bloques(n, k = k, tamano = tamano)
  lapply(bloques, function(bloque) {
    seleccionados <- if (length(indices_globales)) {
      indices_globales[
        indices_globales >= bloque$ordinal_inicio &
          indices_globales <= bloque$ordinal_fin
      ]
    } else integer()
    bloque$valores <- x[seleccionados]
    bloque$ordinales <- as.numeric(seleccionados)
    # `ordinal_inicio`/`ordinal_fin` siguen describiendo el bloque de la
    # fuente; `ordinales` describe las filas efectivamente entregadas.
    bloque$filas_seleccionadas <- length(seleccionados)
    bloque
  })
}

.muestrear_vector_bloques <- function(x, muestra, k = NULL,
                                      tamano = 10000L) {
  n <- length(x)
  indices <- .indices_muestra_globales(n, muestra)
  if (length(indices) == n) {
    # Además de ser más barato, esto conserva exactamente la representación
    # que devuelve `.muestrear_vector()` cuando no hay muestreo.
    return(.muestrear_vector(x, muestra))
  }
  bloques <- .bloques_de_vector_muestreados(
    x, muestra, k = k, tamano = tamano
  )
  aplicados <- unlist(lapply(bloques, function(bloque) {
    bloque$ordinales
  }), use.names = FALSE)
  aplicados <- as.integer(aplicados)
  if (!identical(aplicados, indices)) {
    stop("La particion no reprodujo los indices globales de la muestra.",
         call. = FALSE)
  }
  list(
    valores = x[aplicados], total = n,
    analizados = length(aplicados), muestreado = TRUE
  )
}

# Variantes descriptivas conservadas para llamadores internos y pruebas del
# contrato. Todas delegan en la misma fórmula global.
.muestra_sistematica_bloques <- .muestrear_vector_bloques
.particionar_muestra_bloques <- .bloques_de_vector_muestreados

.ejecutar_muestra_bloques <- function(
    x, muestra, k = NULL, tamano = 10000L,
    fuente_id = "memoria", snapshot_id = "memoria",
    universo_id = "tabla_completa", muestra_id = "muestra_memoria",
    orden_id = "orden_entrada", max_bytes = Inf, vigilante = NULL) {
  indices <- .indices_muestra_globales(length(x), muestra)
  bloques <- .bloques_de_vector_muestreados(
    x, muestra, k = k, tamano = tamano
  )
  acumulador <- .iniciar_acumulador(
    "muestra", typeof(x), familia = "muestra",
    configuracion = list(
      indices_objetivo = indices, n_total = length(x),
      limite = .validar_muestra(muestra)
    ),
    fuente_id = fuente_id, snapshot_id = snapshot_id,
    universo_id = universo_id, muestra_id = muestra_id, orden_id = orden_id,
    max_entradas = max(1, length(indices)), max_bytes = max_bytes,
    requiere_orden = TRUE
  )
  ejecucion <- .ejecutar_acumulador_bloques(
    acumulador, bloques, vigilante = vigilante, familia = "muestra"
  )
  ejecucion$resultado
}

.muestra_bloques <- .ejecutar_muestra_bloques
.muestrear_vector_por_bloques <- .muestrear_vector_bloques

.trazabilidad_bloques <- function(
    x, indices = NULL, mascara = NULL, criterio = NULL,
    aplicable = NULL, k = NULL, tamano = 10000L,
    max_filas_hallazgo = .MAX_FILAS_TRAZABILIDAD_BLOQUES,
    max_bytes = Inf, fuente_id = "memoria", snapshot_id = "memoria",
    universo_id = "tabla_completa", muestra_id = NULL,
    orden_id = "orden_entrada", vigilante = NULL) {
  if (!is.numeric(max_filas_hallazgo) || length(max_filas_hallazgo) != 1L ||
      is.na(max_filas_hallazgo) || max_filas_hallazgo < 1L ||
      (!is.infinite(max_filas_hallazgo) &&
       max_filas_hallazgo != floor(max_filas_hallazgo))) {
    stop("`max_filas_hallazgo` debe ser un entero positivo o Inf.",
         call. = FALSE)
  }
  n <- length(x)
  if (!is.null(mascara) &&
      (!is.logical(mascara) || length(mascara) != n)) {
    stop("`mascara` debe ser logica y tener el largo del vector.",
         call. = FALSE)
  }
  if (!is.null(aplicable) &&
      (!is.logical(aplicable) || length(aplicable) != n)) {
    stop("`aplicable` debe ser logica y tener el largo del vector.",
         call. = FALSE)
  }
  if (!is.null(criterio) && !is.function(criterio)) {
    stop("`criterio` debe ser una funcion.", call. = FALSE)
  }
  if (!is.null(indices)) {
    if (is.logical(indices)) {
      if (length(indices) != n) {
        stop("`indices` logicos debe tener el largo del vector.", call. = FALSE)
      }
      indices <- which(!is.na(indices) & indices)
    } else {
      if (!is.numeric(indices) || anyNA(indices) ||
          any(indices != floor(indices)) || any(indices < 1L | indices > n)) {
        stop("`indices` debe contener posiciones globales validas.",
             call. = FALSE)
      }
      indices <- sort(unique(as.integer(indices)))
    }
  }
  bloques <- .particionar_bloques(n, k = k, tamano = tamano)
  bloques <- lapply(bloques, function(bloque) {
    posiciones <- bloque$valores
    bloque$valores <- x[posiciones]
    seleccion <- if (!is.null(indices)) {
      posiciones %in% indices
    } else if (!is.null(mascara)) {
      mascara[posiciones]
    } else NULL
    if (!is.null(aplicable)) {
      aplicable_bloque <- aplicable[posiciones]
      seleccion <- if (is.null(seleccion)) aplicable_bloque else {
        seleccion & !is.na(aplicable_bloque) & aplicable_bloque
      }
    }
    if (!is.null(seleccion)) bloque$seleccion <- seleccion
    bloque
  })
  acumulador <- .iniciar_acumulador(
    "traza", typeof(x), familia = "trazabilidad",
    configuracion = list(predicado = criterio),
    fuente_id = fuente_id, snapshot_id = snapshot_id,
    universo_id = universo_id, muestra_id = muestra_id, orden_id = orden_id,
    max_entradas = max_filas_hallazgo, max_bytes = max_bytes,
    requiere_orden = TRUE
  )
  ejecucion <- .ejecutar_acumulador_bloques(
    acumulador, bloques, vigilante = vigilante, familia = "trazabilidad"
  )
  ejecucion$resultado
}

.indices_hallazgo_bloques <- .trazabilidad_bloques
.ejecutar_trazabilidad_bloques <- .trazabilidad_bloques

.ejecutar_ejemplos_bloques <- function(
    x, claves = NULL, ejemplos = NULL, k = NULL, tamano = 10000L,
    max_ejemplos = .MAX_EJEMPLOS_BLOQUES,
    max_entradas = .MAX_ENTRADAS_DISTINTOS, max_bytes = Inf,
    fuente_id = "memoria", snapshot_id = "memoria",
    universo_id = "tabla_completa", muestra_id = NULL,
    orden_id = "orden_entrada", vigilante = NULL) {
  if (!is.numeric(max_ejemplos) || length(max_ejemplos) != 1L ||
      is.na(max_ejemplos) || max_ejemplos < 1L ||
      max_ejemplos != floor(max_ejemplos)) {
    stop("`max_ejemplos` debe ser un entero positivo.", call. = FALSE)
  }
  n <- length(x)
  if (is.function(claves)) claves <- claves(x)
  if (is.null(claves)) claves <- x
  if (length(claves) != n) {
    stop("`claves` debe tener el largo del vector.", call. = FALSE)
  }
  if (is.function(ejemplos)) ejemplos <- ejemplos(x)
  if (is.null(ejemplos)) ejemplos <- x
  if (length(ejemplos) != n) {
    stop("`ejemplos` debe tener el largo del vector.", call. = FALSE)
  }
  bloques <- .particionar_bloques(n, k = k, tamano = tamano)
  bloques <- lapply(bloques, function(bloque) {
    posiciones <- bloque$valores
    bloque$valores <- x[posiciones]
    bloque$claves <- claves[posiciones]
    bloque$ejemplos <- ejemplos[posiciones]
    bloque
  })
  acumulador <- .iniciar_acumulador(
    "ejemplos", typeof(claves), familia = "ejemplos",
    configuracion = list(max_ejemplos = as.integer(max_ejemplos)),
    fuente_id = fuente_id, snapshot_id = snapshot_id,
    universo_id = universo_id, muestra_id = muestra_id, orden_id = orden_id,
    max_entradas = max_entradas, max_bytes = max_bytes,
    requiere_orden = TRUE
  )
  ejecucion <- .ejecutar_acumulador_bloques(
    acumulador, bloques, vigilante = vigilante, familia = "ejemplos"
  )
  ejecucion$resultado
}

.ejemplos_bloques <- .ejecutar_ejemplos_bloques
.ejemplos_indices_bloques <- .ejecutar_ejemplos_bloques
.ejemplos_patrones_bloques <- .ejecutar_ejemplos_bloques
.ejecutar_patrones_bloques <- .ejecutar_ejemplos_bloques

.descubrir_patrones_bloques <- function(
    x, distinguir_mayusculas = TRUE, expandir = FALSE,
    max_patrones = 20, na.rm = TRUE, muestra = 1e5,
    umbral_raro = 0.05, k = NULL, tamano = 10000L,
    max_entradas = .MAX_ENTRADAS_DISTINTOS, max_bytes = Inf,
    vigilante = NULL) {
  if (!is.numeric(max_patrones) || length(max_patrones) != 1L ||
      is.na(max_patrones) || max_patrones < 1L ||
      max_patrones != floor(max_patrones)) {
    stop("`max_patrones` debe ser un entero positivo.", call. = FALSE)
  }
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    stop("`na.rm` debe ser TRUE o FALSE.", call. = FALSE)
  }
  if (!is.numeric(umbral_raro) || length(umbral_raro) != 1L ||
      is.na(umbral_raro) || umbral_raro < 0 || umbral_raro > 1) {
    stop("`umbral_raro` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.atomic(x) && !is.factor(x)) {
    stop("`x` debe ser un vector atomico.", call. = FALSE)
  }
  n <- length(x)
  bloques <- if (n <= .validar_muestra(muestra)) {
    .bloques_de_vector(x, k = k, tamano = tamano)
  } else {
    .bloques_de_vector_muestreados(x, muestra, k = k, tamano = tamano)
  }
  marcador_na <- "\001valor_ausente\001"
  bloques <- lapply(bloques, function(bloque) {
    valores <- .texto_analizable(bloque$valores)$valores
    valores <- as.character(valores)
    validos <- !is.na(valores)
    claves <- rep(NA_character_, length(valores))
    if (any(validos)) {
      claves[validos] <- .generalizar_a_patron(
        valores[validos], distinguir_mayusculas, expandir
      )
    }
    if (!na.rm) claves[!validos] <- marcador_na
    bloque$valores <- valores
    bloque$claves <- claves
    bloque$ejemplos <- valores
    bloque$aplicable <- if (na.rm) validos else rep(TRUE, length(valores))
    bloque
  })
  acumulador <- .iniciar_acumulador(
    "patron", "character", familia = "ejemplos",
    configuracion = list(max_ejemplos = .MAX_EJEMPLOS_BLOQUES),
    max_entradas = max_entradas, max_bytes = max_bytes,
    requiere_orden = TRUE
  )
  ejecucion <- .ejecutar_acumulador_bloques(
    acumulador, bloques, vigilante = vigilante, familia = "patrones"
  )
  sobre <- ejecucion$resultado
  if (!identical(sobre$estado, "calculado")) return(sobre)
  mapa <- sobre$resultado
  if (!nrow(mapa)) {
    frecuencias <- numeric()
    orden <- integer()
  } else {
    claves <- as.character(mapa$clave)
    # `table()` ordena sus niveles antes de `sort()`; en particular, el orden
    # de empate no es el byte-order de `order(..., method = "radix")` para
    # mayusculas y caracteres de control. Reproducir los niveles de `table`
    # mantiene la identidad de la pasada unica tambien en esos empates.
    niveles <- sort(unique(claves))
    orden_niveles <- match(claves, niveles)
    orden <- order(-mapa$n, orden_niveles)
    frecuencias <- mapa$n[orden]
  }
  denominador <- sum(frecuencias)
  proporciones <- if (denominador) as.numeric(frecuencias) / denominador else {
    numeric()
  }
  nombres <- if (length(orden)) as.character(mapa$clave[orden]) else character()
  nombres_publicos <- nombres
  nombres_publicos[nombres_publicos == marcador_na] <- NA_character_
  ejemplos <- if (length(orden)) mapa$ejemplos[orden] else character()
  crear_tabla <- function(indices) {
    data.frame(
      patron = nombres_publicos[indices],
      n = as.integer(frecuencias[indices]),
      proporcion = proporciones[indices],
      ejemplos = ejemplos[indices],
      stringsAsFactors = FALSE
    )
  }
  limite <- min(length(frecuencias), floor(max_patrones))
  indices_salida <- if (limite) seq_len(limite) else integer()
  indices_raros <- which(seq_along(frecuencias) > 1L &
                          proporciones < umbral_raro)
  indices_excluidos <- which(seq_along(frecuencias) > 1L &
                              proporciones >= umbral_raro)
  indices_resumen <- unique(c(
    if (length(frecuencias)) 1L else integer(),
    utils::head(indices_raros, 6L)
  ))
  resultado <- crear_tabla(indices_salida)
  resumen <- crear_tabla(indices_resumen)
  rownames(resultado) <- NULL
  rownames(resumen) <- NULL
  class(resultado) <- c("patrones", "data.frame")
  attr(resultado, "total") <- n
  attr(resultado, "analizados") <- as.integer(sobre$alcance$valores %||% 0)
  attr(resultado, "filas_analizadas") <- attr(resultado, "analizados")
  attr(resultado, "muestreado") <- attr(resultado, "analizados") < n
  attr(resultado, "n_patrones_distintos") <- length(frecuencias)
  attr(resultado, "n_patrones_raros") <- length(indices_raros)
  raros <- nombres[indices_raros]
  if (length(frecuencias)) {
    attr(resultado, "patrones_raros_trazabilidad") <- raros
  }
  attr(resultado, "n_patrones_raros_trazabilidad") <- length(indices_raros)
  attr(resultado, "limite_patrones_raros_trazabilidad") <-
    .limite_patrones_raros_trazabilidad
  attr(resultado, "n_filas_patrones_no_dominantes_excluidos") <- if (
    length(indices_excluidos)
  ) sum(frecuencias[indices_excluidos]) else 0L
  attr(resultado, "resumen_patrones") <- resumen
  resultado
}

.patrones_bloques <- .descubrir_patrones_bloques
