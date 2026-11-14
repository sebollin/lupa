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
    .detener("`", nombre, "` debe ser un entero positivo",
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
    .detener("`configuracion` debe ser una lista.", call. = FALSE)
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

.nuevo_entorno_acumulador <- function(familia, configuracion,
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
    familia, configuracion, max_entradas, max_bytes,
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
      n = 0, media = 0, media_abs = 0, suma = 0,
      suma_compensacion = 0, partes_suma = numeric(), exponente_partes = 0,
      partes_suma_chicas = numeric(),
      m2 = 0,
      centro = NA_real_, minimo = Inf, maximo = -Inf,
      desplazamiento = if (length(configuracion$configuracion$desplazamiento) == 1L &&
                           is.finite(configuracion$configuracion$desplazamiento)) {
        as.numeric(configuracion$configuracion$desplazamiento)
      } else 0,
      escala = if (length(configuracion$configuracion$escala) == 1L &&
                   is.finite(configuracion$configuracion$escala) &&
                   configuracion$configuracion$escala > 0) {
        as.numeric(configuracion$configuracion$escala)
      } else 1,
      escala_adaptativa = isTRUE(
        configuracion$configuracion$escala_adaptativa
      ),
      n_nan = 0, n_infinito_positivo = 0, n_infinito_negativo = 0,
      n_ceros = 0, n_negativos = 0, contar_signos = TRUE,
      # El desvio: ver `.desvio_mas_cercano_bloques()`. "exacto" salvo que la
      # configuracion pida "rapido" (`perfilar()` en memoria).
      desvio_modo = if (identical(configuracion$configuracion$desvio,
                                  "rapido")) "rapido" else "exacto",
      desvio_valido = TRUE, centro_desvio = NA_real_,
      partes_cuadrados = numeric(), exponente_cuadrados = 0,
      cota_cuadrados = 0
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
    .detener("Familia de acumulador desconocida: `", familia, "`.", call. = FALSE)
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
    .detener("La m\u00e1scara `aplicable` debe ser l\u00f3gica y tener el largo del bloque.",
         call. = FALSE)
  }
  if (!is.null(ordinales) && length(ordinales) != n_filas) {
    .detener("`ordinales` debe tener el largo del bloque.", call. = FALSE)
  }
  if (!is.null(claves) && length(claves) != n_filas) {
    .detener("`claves` debe tener el largo del bloque.", call. = FALSE)
  }
  if (!is.null(ejemplos) && length(ejemplos) != n_filas) {
    .detener("`ejemplos` debe tener el largo del bloque.", call. = FALSE)
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
    .detener("El bloque no tiene ordinales globales v\u00e1lidos.", call. = FALSE)
  }
  if (!is.null(ordinales) && length(ordinales)) {
    if (any(!is.finite(ordinales)) || any(ordinales != floor(ordinales)) ||
        any(ordinales < 1) || anyDuplicated(ordinales) ||
        any(diff(ordinales) <= 0) || min(ordinales) < inicio ||
        max(ordinales) > fin) {
      .detener("El bloque no tiene ordinales globales v\u00e1lidos.", call. = FALSE)
    }
  }
  if (acumulador$requiere_orden && length(acumulador$intervalos)) {
    anterior <- acumulador$intervalos[[length(acumulador$intervalos)]]
    if (inicio <= anterior[[2L]]) {
      .detener("Los intervalos globales del acumulador se solapan.", call. = FALSE)
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

.tipo_match_R <- function(x) {
  if (inherits(x, "integer64")) return("integer64")
  if (is.factor(x)) return("factor")
  if (inherits(x, "Date")) return("Date")
  if (inherits(x, "POSIXct")) return("POSIXct")
  if (inherits(x, "difftime")) return("difftime")
  if (is.null(oldClass(x))) return("")
  NULL
}

.puede_vectorizar_match_R <- function(valores, representantes) {
  tipo_valores <- .tipo_match_R(valores)
  tipo_representantes <- .tipo_match_R(representantes)
  if (is.null(tipo_valores) || is.null(tipo_representantes) ||
      !identical(tipo_valores, tipo_representantes)) {
    return(FALSE)
  }
  if (identical(tipo_valores, "integer64")) {
    return(.integer64_con_match_confiable())
  }
  if (identical(tipo_valores, "factor")) {
    niveles_validos <- function(x) {
      niveles <- tryCatch(levels(x), error = function(e) NULL)
      !is.null(niveles) && !anyDuplicated(niveles)
    }
    if (!niveles_validos(valores) || !niveles_validos(representantes)) {
      return(FALSE)
    }
  }
  if (identical(tipo_valores, "difftime")) {
    unidades_validas <- function(x) {
      unidades <- attr(x, "units", exact = TRUE)
      length(unidades) == 1L && is.character(unidades) &&
        !is.na(unidades) && nzchar(unidades)
    }
    if (!unidades_validas(valores) || !unidades_validas(representantes)) {
      return(FALSE)
    }
  }
  tipo_valores %in% c("", "factor", "Date", "POSIXct", "difftime")
}

.claves_match_R <- function(valores, representantes, tipo) {
  if (identical(tipo, "factor")) {
    niveles <- unique(c(levels(valores), levels(representantes)))
    return(list(
      valores = match(as.character(valores), niveles, nomatch = 0L),
      representantes = match(as.character(representantes), niveles,
                             nomatch = 0L)
    ))
  }
  if (identical(tipo, "difftime")) {
    return(list(
      # `match()` historicamente compara la representacion numerica de
      # `difftime`, no convierte las unidades. Mantenerla evita que el camino
      # rapido cambie la identidad de un perfil.
      valores = unclass(valores),
      representantes = unclass(representantes)
    ))
  }
  if (tipo %in% c("Date", "POSIXct")) {
    return(list(
      valores = unclass(valores),
      representantes = unclass(representantes)
    ))
  }
  list(valores = valores, representantes = representantes)
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
  tipo <- .tipo_match_R(valores)
  if (.puede_vectorizar_match_R(valores, representantes)) {
    claves <- tryCatch(
      .claves_match_R(valores, representantes, tipo),
      error = function(e) NULL
    )
    if (!is.null(claves)) {
      valores <- claves$valores
      representantes <- claves$representantes
    }
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

.append_typed_R <- function(x, valor) {
  if (!length(x)) return(valor)
  unidos <- .unificar_enteros_64_R(x, valor)
  if (is.null(unidos)) return(NULL)
  tryCatch(c(unidos$a, unidos$b), error = function(e) NULL)
}

# Un controlador puede entregar la misma columna con otra clase en cada bloque:
# RSQLite da `integer` a un bloque de valores chicos e `integer64` al que trae
# uno que no entra en 32 bits. `c()` despacha por su primer argumento, y
# `c(1:2, bit64::as.integer64(3e9))` pega los bits del `integer64` como si
# fueran un doble: el mapa de distintos de `{1, 2, 3e9, 3e9}` con
# `bloque_filas = 2` contaba 4 distintos, ninguna moda y una mediana de 0,5
# -es 1500000001-, todo `calculado`. Los dos lados se llevan a `integer64`, que
# representa exacto todo entero de 32 bits y todo doble entero por debajo de
# 2^53; un doble que no es entero o pasa de ahi no tiene `integer64` exacto, y
# la union no se hace (`NULL`).
.unificar_enteros_64_R <- function(a, b) {
  en_a <- inherits(a, "integer64")
  en_b <- inherits(b, "integer64")
  if (en_a == en_b) return(list(a = a, b = b))
  convertir <- function(x) {
    # Un bloque de nulos llega `logical`: su `NA` es el de cualquier clase.
    if ((is.integer(x) || (is.logical(x) && all(is.na(x)))) &&
        is.null(oldClass(x))) {
      return(bit64::as.integer64(x))
    }
    if (is.double(x) && is.null(oldClass(x)) &&
        all(is.na(x) | (is.finite(x) & x == trunc(x) &
                          abs(x) < .MAX_ENTERO_EXACTO_DBI))) {
      return(bit64::as.integer64(x))
    }
    NULL
  }
  if (en_a) {
    b <- convertir(b)
    if (is.null(b)) return(NULL)
  } else {
    a <- convertir(a)
    if (is.null(a)) return(NULL)
  }
  list(a = a, b = b)
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

# La media que se publica es el doble mas cercano de la media exacta, y para
# eso hace falta la suma exacta de la columna. Acumulada en doble -centrada en
# la media del primer bloque, y con Welford entre bloques- se equivocaba sin
# ruido: sobre `c(-2^51, 1, 2, 2^51)` publicaba 0,6875 por 0,75, porque al
# restarle el centro 0,75 a 2^51 se pierde la fraccion; sobre quince mil
# valores de 2^51, quince mil de -2^51 y `1:3` le cambiaba el signo, y sobre
# `rnorm(2e6)`, sin cancelacion, quedaba a 19 ulps. `sum()` de R tampoco es la
# salida: acumula en `long double` donde lo hay, y en macOS ARM64 es un doble.
#
# Cada bloque se parte en sumandos que se suman sin redondeo (la extraccion de
# Rump, Ogita y Oishi, "Accurate floating-point summation", 2008): con `sigma`
# una potencia de dos de al menos `2 * m * max(abs(x))`, `(sigma + x) - sigma`
# cae en la grilla de `sigma * 2^-53`, su suma es exacta en cualquier orden y
# con cualquier acumulador, y `x - q` es exacto y queda por debajo de la grilla;
# el resto se vuelve a partir hasta que es cero. Son operaciones vectoriales:
# dos pasos por bloque en datos comunes, uno en enteros. Lo que se guarda es
# una lista corta de dobles cuya suma exacta es la de la columna. `NULL` dice
# que no se pudo (un `sigma` que desborda; con `.exponente_partes_bloques()` no
# llega).
.partes_suma_exacta_bloques <- function(x, maximo = NULL) {
  m <- length(x)
  if (!m) return(numeric())
  # La holgura crece con el largo y cada paso saca `53 - log2(2 m)` cifras:
  # un vector de mas de 2^20 valores se parte en trozos.
  if (m > 1048576) {
    partes <- lapply(
      split(x, ceiling(seq_len(m) / 1048576)), .partes_suma_exacta_bloques
    )
    if (any(vapply(partes, is.null, logical(1)))) return(NULL)
    return(.partes_suma_exacta_bloques(unlist(partes, use.names = FALSE)))
  }
  holgura <- 2^(ceiling(log2(m)) + 1)
  if (is.null(maximo)) maximo <- max(-min(x), max(x))
  partes <- numeric()
  while (maximo > 0) {
    exponente <- ceiling(log2(maximo))
    if (2^exponente < maximo) exponente <- exponente + 1
    sigma <- 2^exponente * holgura
    if (!is.finite(sigma)) return(NULL)
    q <- (sigma + x) - sigma
    partes[length(partes) + 1L] <- sum(q)
    x <- x - q
    maximo <- max(-min(x), max(x))
  }
  partes
}

# Las partes se guardan en la unidad `x / 2^exponente_partes`. El exponente es
# 0 mientras los valores no pasan de 2^900, y si pasan, el justo para que ni
# `sigma` ni la suma de 2^53 valores desborden. En esa unidad los valores
# menores que `2^(exponente - 1022)` caerian en los subnormales y perderian
# cifras: con 2^1000 en la columna, los de 2^-921 quedaban a 1 ulp y los de
# 2^-1000 daban la media 0. Esos van a `partes_suma_chicas`, sumadas en la
# unidad 1, donde son exactas (`.media_mas_cercana_bloques()` junta las dos).
# Una columna comun nunca llega aca: el exponente es 0.
.exponente_partes_necesario_bloques <- function(maximo_abs) {
  if (!is.finite(maximo_abs) || maximo_abs <= 2^900) return(0)
  ceiling(log2(maximo_abs)) - 900
}

# Las partes de la unidad `2^viejo` llevadas a `2^nuevo` (`nuevo > viejo`). Las
# que en la nueva quedarian subnormales pasan a las chicas, en la unidad 1:
# cada parte es un doble de la unidad vieja, y multiplicarla por `2^viejo`
# solo le sube el exponente.
.llevar_partes_suma_bloques <- function(partes, viejo, nuevo) {
  if (is.null(partes) || viejo == nuevo) {
    return(list(partes = partes, chicas = numeric()))
  }
  chicas <- partes != 0 & abs(partes) < 2^(nuevo - viejo - 1022)
  list(
    partes = partes[!chicas] * 2^(viejo - nuevo),
    chicas = partes[chicas] * 2^viejo
  )
}

.exponente_partes_bloques <- function(estado) {
  exponente <- estado$exponente_partes
  if (length(exponente) != 1L || !is.finite(exponente)) 0 else exponente
}

# Las partes de muchos bloques se vuelven a partir cuando la lista crece: la
# suma exacta no cambia y el estado queda chico.
.agregar_partes_suma_bloques <- function(partes, nuevas) {
  if (is.null(partes) || is.null(nuevas)) return(NULL)
  partes <- c(partes, nuevas)
  if (length(partes) > 32L) partes <- .partes_suma_exacta_bloques(partes)
  partes
}

# La misma suma como expansion no solapada (Shewchuk, "Adaptive precision
# floating-point arithmetic", 1997): componentes de magnitud creciente en los
# que cada uno es mayor que la suma de los anteriores, de modo que el signo de
# la suma es el del ultimo. Solo se usa al publicar, sobre unas pocas partes.
.expansion_no_solapada_bloques <- function(valores) {
  expansion <- numeric()
  for (b in valores) {
    if (b == 0) next
    q <- b
    h <- numeric()
    for (componente in expansion) {
      s <- q + componente
      parte_b <- s - q
      error <- (q - (s - parte_b)) + (componente - parte_b)
      if (error != 0) h[length(h) + 1L] <- error
      q <- s
    }
    expansion <- if (q != 0) c(h, q) else h
  }
  expansion
}

.signo_expansion_bloques <- function(expansion) {
  if (length(expansion)) sign(expansion[[length(expansion)]]) else 0
}

# `a * b` exacto como dos dobles (Dekker): cada producto parcial entra en 53
# bits. Cada operacion de R es una llamada aparte, asi que no hay `fma` que
# cambie un redondeo.
.producto_exacto_bloques <- function(a, b) {
  p <- a * b
  t <- 134217729 * a
  a_alto <- t - (t - a)
  a_bajo <- a - a_alto
  t <- 134217729 * b
  b_alto <- t - (t - b)
  b_bajo <- b - b_alto
  c(p, ((a_alto * b_alto - p) + a_alto * b_bajo + a_bajo * b_alto) +
      a_bajo * b_bajo)
}

# Distancia de un doble normal a su vecino hacia el cero o alejandose de el:
# son distintas en una potencia de dos.
.espaciado_doble_bloques <- function(x, hacia_cero) {
  a <- abs(x)
  exponente <- floor(log2(a))
  if (2^exponente > a) exponente <- exponente - 1
  if (2^(exponente + 1) <= a) exponente <- exponente + 1
  if (exponente < -1022) return(2^-1074)
  ulp <- 2^(exponente - 52)
  if (hacia_cero && a == 2^exponente && exponente > -1022) ulp / 2 else ulp
}

# El doble mas cercano de `sum(partes) * 2^exponente / n`, con empate a par,
# comparando restos exactos: el candidato `c` es el bueno cuando
# `sum(partes) - c * n` queda dentro de medio espaciado por `n` a cada lado. Las
# partes estan en la unidad en que se opero (`x / 2^exponente`), y el espaciado
# es el de los dobles de la unidad publicada: el de `c`, o `2^-1074` llevado a
# esa unidad cuando la media publicada es subnormal. Asi `c * 2^exponente` es
# exacto y la media subnormal tambien es la mas cercana. Una suma diminuta se
# lleva a los dobles normales por otra potencia de dos, que pasa al exponente.
.cociente_mas_cercano_bloques <- function(partes, n, exponente = 0,
                                          desempate = 0) {
  s <- .expansion_no_solapada_bloques(partes)
  publicar <- function(x) {
    # En dos factores: `2^exponente` solo puede no ser representable.
    mitad <- exponente %/% 2
    x * 2^mitad * 2^(exponente - mitad)
  }
  if (!length(s)) return(0)
  tope <- abs(s[[length(s)]])
  if (!is.finite(tope)) return(publicar(sum(partes) / n))
  if (tope < 2^-900) {
    return(.cociente_mas_cercano_bloques(
      s * 2^600, n, exponente - 600, desempate
    ))
  }
  # El espaciado de un doble subnormal publicado, en la unidad de las partes;
  # con `exponente > 0` es mas fino que el de cualquier doble de esta unidad.
  grilla <- if (exponente <= 0) 2^(-1074 - exponente) else 0
  espaciado <- function(x, hacia_cero) {
    max(.espaciado_doble_bloques(x, hacia_cero), grilla)
  }
  aproximada <- 0
  for (componente in s) aproximada <- aproximada + componente
  candidato <- aproximada / n
  if (!is.finite(candidato) || tope > 2^960 || abs(candidato) < 2^-1000) {
    return(publicar(candidato))
  }
  if (grilla > 0 && abs(candidato) < grilla * 2^52) {
    candidato <- round(candidato / grilla) * grilla
  }
  for (intento in seq_len(16L)) {
    resto <- .expansion_no_solapada_bloques(
      c(s, -.producto_exacto_bloques(candidato, n))
    )
    signo <- .signo_expansion_bloques(resto)
    if (signo == 0) return(publicar(candidato))
    if (signo > 0) {
      paso <- espaciado(candidato, hacia_cero = candidato < 0)
      lado <- .signo_expansion_bloques(
        .expansion_no_solapada_bloques(c(resto, -n * paso / 2))
      )
      vecino <- candidato + paso
    } else {
      paso <- espaciado(candidato, hacia_cero = candidato > 0)
      lado <- -.signo_expansion_bloques(
        .expansion_no_solapada_bloques(c(resto, n * paso / 2))
      )
      vecino <- candidato - paso
    }
    if (lado < 0) return(publicar(candidato))
    if (lado == 0) {
      # Un empate exacto lo rompe `desempate`, el signo de lo que se sumo
      # aparte (ver `.media_mas_cercana_bloques()`); si no hay, va a par.
      if (desempate != 0) {
        return(publicar(if ((desempate > 0) == (signo > 0)) vecino else {
          candidato
        }))
      }
      mantisa <- abs(candidato) / espaciado(candidato, hacia_cero = FALSE)
      return(publicar(if (mantisa %% 2 == 0) candidato else vecino))
    }
    candidato <- if (intento <= 2L) vecino else {
      correccion <- 0
      for (componente in resto) correccion <- correccion + componente
      siguiente <- candidato + correccion / n
      if (grilla > 0 && abs(siguiente) < grilla * 2^52) {
        siguiente <- round(siguiente / grilla) * grilla
      }
      siguiente
    }
  }
  publicar(candidato)
}

# La media de las dos listas de partes: las de la unidad `2^exponente_partes` y
# las chicas, en la unidad 1 (`.exponente_partes_necesario_bloques()`). Si la
# suma de las primeras cabe en la unidad 1 por debajo de 2^950, todo se junta
# ahi y el cociente es exacto. Si no, la de las chicas -menor que 2^-845- solo
# puede decidir un empate exacto, y entra como su signo.
.media_mas_cercana_bloques <- function(estado) {
  n <- estado$n
  exponente <- .exponente_partes_bloques(estado)
  chicas <- estado$partes_suma_chicas
  if (!length(chicas) || all(chicas == 0)) {
    return(.cociente_mas_cercano_bloques(estado$partes_suma, n, exponente))
  }
  grandes <- .expansion_no_solapada_bloques(estado$partes_suma)
  if (!length(grandes)) return(.cociente_mas_cercano_bloques(chicas, n, 0))
  if (abs(grandes[[length(grandes)]]) < 2^(950 - exponente)) {
    return(.cociente_mas_cercano_bloques(
      c(grandes * 2^exponente, chicas), n, 0
    ))
  }
  .cociente_mas_cercano_bloques(
    grandes, n, exponente,
    desempate = .signo_expansion_bloques(.expansion_no_solapada_bloques(chicas))
  )
}

# ---------------------------------------------------------------------------
# El desvio publicado es el doble mas cercano del desvio exacto (`n - 1`).
#
# Con Welford en doble -el centrado, los cuadrados y la fusion de 200 bloques,
# cada paso con su redondeo- el desvio de una columna comun de 2.000.000 de
# filas quedaba a 1-3 ulps del mas cercano. El exacto sale de
# `S n = n Q - X^2`, con `X = sum(x - c)` y `Q = sum((x - c)^2)` para un
# centro `c` fijo del estado: `X` ya es exacta (las partes de la media y
# `n c` con Dekker) y queda `Q`. El estado guarda `Q` como partes cuya suma
# exacta se conoce, mas una cota de lo que no se sumo exacto; al publicar,
# `.desvio_mas_cercano_bloques()` compara `S n` contra los puntos medios entre
# dobles vecinos con aritmetica exacta, y si la cota no deja decidir lo dice.
#
# Hay dos maneras de llenar ese estado, con el mismo resultado:
# - "exacto": `Q = sum(x^2)` sin centro, cada cuadrado partido sin error con
#   Dekker y sumado con `.partes_suma_exacta_bloques()`. La cota es cero salvo
#   en una columna con valores 2^885 veces menores que el mayor. Unas 40
#   operaciones vectoriales por bloque: es la manera por omision, la de
#   `bloque_filas`, donde lo que cuesta es leer del motor.
# - "rapido": la de `perfilar()` en memoria, que tiene la columna entera y
#   puede volver a recorrerla. `Q` se calcula con cuatro sumas, tres exactas
#   y una con cota demostrada (`.cuadrados_rapidos_bloques()`); la cota deja
#   decidir el redondeo salvo que el desvio exacto caiga a unos 2^-70
#   relativos de un punto medio, y entonces `.resumen_cuantitativo_bloques()`
#   recorre la columna otra vez por la manera exacta.
# Ninguna de las dos depende de la precision extendida de `sum()`: lo que se
# suma exacto son multiplos de una grilla que entran en 53 bits, y las cotas
# son las de una suma en doble en cualquier orden.

# `x * 2^k` en factores que no desbordan; exacto mientras el resultado no
# quede subnormal.
.por_potencia_dos_bloques <- function(x, k) {
  while (k > 1000) {
    x <- x * 2^1000
    k <- k - 1000
  }
  while (k < -1000) {
    x <- x * 2^-1000
    k <- k + 1000
  }
  x * 2^k
}

# El exponente de la menor potencia de dos que no es menor que `maximo` (> 0).
.exponente_cota_bloques <- function(maximo) {
  exponente <- ceiling(log2(maximo))
  if (2^exponente < maximo) exponente <- exponente + 1
  exponente
}

# `y^2` sin error como `p + e` (Dekker), vectorial. Exacto si `|y|` esta entre
# 2^-485 -por debajo `e` cae en los subnormales- y 2^996.
.cuadrado_exacto_bloques <- function(y) {
  p <- y * y
  t <- 134217729 * y
  alto <- t - (t - y)
  bajo <- y - alto
  list(p = p, e = ((alto * alto - p) + 2 * alto * bajo) + bajo * bajo)
}

# `.producto_exacto_bloques()` con la cota de su error: Dekker es exacto salvo
# que el producto caiga cerca de los subnormales, y ahi el error es de unos
# pocos 2^-1074.
.producto_cota_bloques <- function(a, b) {
  producto <- .producto_exacto_bloques(a, b)
  p <- producto[[1L]]
  exacto <- if (p == 0) a == 0 || b == 0 else abs(p) >= 2^-968
  list(valores = producto, cota = if (exacto) 0 else 2^-1070)
}

# El signo exacto de una suma de dobles. Primero la cuenta en doble con la cota
# de una suma en cualquier orden; si no alcanza, la expansion de Shewchuk.
.signo_suma_exacta_bloques <- function(valores) {
  valores <- valores[valores != 0]
  k <- length(valores)
  if (!k) return(0)
  aproximada <- sum(valores)
  cota <- sum(abs(valores)) * (k + 2) * 2^-52 + 2^-1060
  if (abs(aproximada) > cota) return(sign(aproximada))
  .signo_expansion_bloques(.expansion_no_solapada_bloques(valores))
}

# El centro de la manera rapida se elige con el primer bloque: si sus valores
# son de un mismo signo y estan dentro de un factor 4, un centro entre la mitad
# del mayor y el doble del menor hace exacta cada resta `x - c` (Sterbenz),
# y se toma el mas cercano a la media del bloque. Si no, 0: `x - 0` es exacta.
# Un bloque constante se centra en su valor: cada `x - c` es cero y la suma no
# tiene cota que la aleje del cero. Centrado en su media en doble -que puede no
# ser el valor- la cota no dejaba decidir que el desvio es 0, y una columna
# constante se recorria dos veces.
.elegir_centro_desvio_bloques <- function(minimo, maximo, media) {
  if (minimo == maximo) return(minimo)
  if (minimo > 0 && maximo <= 4 * minimo) {
    return(min(max(media, maximo / 2), 2 * minimo))
  }
  if (maximo < 0 && minimo >= 4 * maximo) {
    return(min(max(media, 2 * maximo), minimo / 2))
  }
  0
}

# La suma de `(x - c)^2` de un bloque, partida en dobles cuya suma exacta se
# conoce, y la cota de la diferencia con la verdadera. `d = x - c` se lleva a
# una grilla fija por la extraccion de Rump, Ogita y Oishi: con `2^E` cota de
# `|d|`, `alto` cae en la grilla 2^(E - 19) con a lo sumo 19 bits, `medio` en
# la de 2^(E - 38) con otros 19, y `|bajo| <= 2^(E - 38)`. Con a lo sumo 2^15
# valores, `sum(alto^2)` y `sum(alto * medio)` son multiplos de su grilla que
# no pasan de 2^53 grillas: exactas en cualquier orden y acumulador.
# `(alto + medio + bajo)^2` deja dos sumas en doble, `sum(alto * bajo)` y
# `sum(resto^2)`, que pesan 2^-36 y 2^-34 de la suma, y su error es el de una
# suma en doble de `m` terminos (`(m + 2) 2^-52` de la suma de sus modulos,
# con `sum(|alto bajo|)` acotada por Cauchy-Schwarz), mas el de los productos
# que caigan en los subnormales. Sobre un bloque comun la cota queda en 2^-73
# de la suma. Si `x - c` no es exacta, `d + resto_d` lo es (TwoSum) y el
# cruce `2 sum(d resto_d)` entra con su cota. `ok = FALSE`: fuera del rango
# en que vale la cuenta.
.cuadrados_rapidos_bloques <- function(x, centro, minimo, maximo) {
  m <- length(x)
  maximo_d <- max(maximo - centro, centro - minimo) * (1 + 2^-50)
  if (!(maximo_d > 0)) return(list(partes = numeric(), cota = 0, ok = TRUE))
  exponente <- .exponente_cota_bloques(maximo_d)
  if (m > 32768L || !is.finite(exponente) || exponente > 460 ||
      exponente < -460) {
    return(list(partes = NULL, cota = Inf, ok = FALSE))
  }
  exacta <- centro == 0 ||
    (centro > 0 && minimo >= centro / 2 && maximo <= 2 * centro) ||
    (centro < 0 && maximo <= centro / 2 && minimo >= 2 * centro)
  d <- if (centro == 0) x else x - centro
  resto_d <- NULL
  if (!exacta) {
    b <- d - x
    resto_d <- (x - (d - b)) + (-centro - b)
  }
  s1 <- 2^(exponente + 34)
  alto <- (s1 + d) - s1
  resto <- d - alto
  s2 <- 2^(exponente + 15)
  medio <- (s2 + resto) - s2
  bajo <- resto - medio
  cuadrados_alto <- sum(alto * alto)
  cruce_medio <- sum(alto * medio)
  cruce_bajo <- sum(alto * bajo)
  cuadrados_resto <- sum(resto * resto)
  gamma <- (m + 2) * 2^-52
  subnormales <- m * 2^-1074
  cota <- 2 * (gamma * sqrt(cuadrados_alto * m) * 2^(exponente - 38) *
                 (1 + 2^-50) + subnormales) +
    2 * gamma * cuadrados_resto + subnormales
  partes <- c(cuadrados_alto, 2 * cruce_medio, 2 * cruce_bajo, cuadrados_resto)
  if (!is.null(resto_d)) {
    cruce_d <- sum(d * resto_d)
    cuadrados_d <- cuadrados_alto + 2 * cruce_medio + 2 * abs(cruce_bajo) +
      cuadrados_resto + cota
    cota <- cota + 2 * (gamma * 2^-53 * cuadrados_d + subnormales) +
      2^-106 * cuadrados_d
    partes <- c(partes, 2 * cruce_d)
  }
  list(partes = partes, cota = cota * (1 + 2^-50), ok = TRUE)
}

.absorber_desvio_rapido_bloques <- function(estado, finitos, minimo, maximo,
                                            partes_bloque) {
  if (!isTRUE(estado$desvio_valido)) return(estado)
  # Tambien los valores, y no solo `x - c`, tienen que estar en el rango: `X`
  # se arma en la unidad 1, y con una constante de 1,8e308 desbordaba.
  if (max(-minimo, maximo) > 2^460) {
    estado$desvio_valido <- FALSE
    return(estado)
  }
  centro <- estado$centro_desvio
  primero <- length(centro) != 1L || is.na(centro)
  m <- length(finitos)
  if (primero) {
    media <- 0
    for (parte in partes_bloque) media <- media + parte
    media <- media / m
    centro <- .elegir_centro_desvio_bloques(minimo, maximo, media)
  }
  cuenta <- .cuadrados_rapidos_bloques(finitos, centro, minimo, maximo)
  # Con centro 0, `S = Q - X^2 / n` cancela cuando la media es grande frente al
  # desvio (valores de 1000 +- 1 y algun cero), y la cota relativa de `S` crece
  # en la misma proporcion. Si el primer bloque ya cancela mas de 2^10, se
  # centra en su media y cada resta inexacta va por TwoSum.
  if (primero && isTRUE(cuenta$ok) && centro == 0 && is.finite(media) &&
      media != 0) {
    q <- sum(cuenta$partes)
    if (!(q - media * media * m > q * 2^-10)) {
      centro <- media
      cuenta <- .cuadrados_rapidos_bloques(finitos, centro, minimo, maximo)
    }
  }
  if (!isTRUE(cuenta$ok)) {
    estado$desvio_valido <- FALSE
    return(estado)
  }
  estado$centro_desvio <- centro
  estado$partes_cuadrados <- .agregar_partes_suma_bloques(
    estado$partes_cuadrados, cuenta$partes
  )
  estado$cota_cuadrados <- (estado$cota_cuadrados + cuenta$cota) *
    (1 + 2^-52)
  estado
}

# La manera exacta: `sum(x^2)` en la unidad `2^(2 exponente_cuadrados)`. El
# exponente deja el mayor del primer bloque en 2^400 y sube si llega un bloque
# 2^50 veces mayor. Un valor que en esa unidad quede por debajo de 2^-485 -uno
# 2^885 veces menor que el mayor- no se eleva exacto (Dekker caeria en los
# subnormales): se cuenta en la cota, que frente a `S` pesa menos de 2^-1700 y
# solo puede cambiar un empate.
.absorber_desvio_exacto_bloques <- function(estado, finitos, minimo, maximo) {
  maximo_abs <- max(-minimo, maximo)
  if (!(maximo_abs > 0)) return(estado)
  exponente_maximo <- .exponente_cota_bloques(maximo_abs)
  exponente <- estado$exponente_cuadrados
  if (!length(estado$partes_cuadrados) && estado$cota_cuadrados == 0) {
    exponente <- exponente_maximo - 400
  } else if (exponente_maximo - exponente > 450) {
    nuevo <- exponente_maximo - 400
    k <- 2 * (exponente - nuevo)
    viejas <- estado$partes_cuadrados
    llevadas <- .por_potencia_dos_bloques(viejas, k)
    perdidas <- sum(.por_potencia_dos_bloques(llevadas, -k) != viejas)
    estado$cota_cuadrados <- .por_potencia_dos_bloques(
      estado$cota_cuadrados, k
    ) * (1 + 2^-52) + perdidas * 2^-1073
    estado$partes_cuadrados <- llevadas
    exponente <- nuevo
  }
  estado$exponente_cuadrados <- exponente
  y <- if (exponente != 0) .por_potencia_dos_bloques(finitos, -exponente) else {
    finitos
  }
  minimo_abs <- if (minimo > 0) minimo else if (maximo < 0) -maximo else 0
  if (.por_potencia_dos_bloques(minimo_abs, -exponente) < 2^-485) {
    diminutos <- y != 0 & abs(y) < 2^-485
    k <- sum(diminutos)
    if (k) {
      y[diminutos] <- 0
      estado$cota_cuadrados <- (estado$cota_cuadrados + k * 2^-970) *
        (1 + 2^-52)
    }
  }
  cuadrado <- .cuadrado_exacto_bloques(y)
  estado$partes_cuadrados <- .agregar_partes_suma_bloques(
    estado$partes_cuadrados,
    c(.partes_suma_exacta_bloques(cuadrado$p),
      .partes_suma_exacta_bloques(cuadrado$e))
  )
  estado
}

# Las partes de `sum((x - c_b)^2)` llevadas al centro `c_a`: se suma
# `2 (c_b - c_a) sum(x - c_b) + n (c_b - c_a)^2`, todo con productos exactos.
.recentrar_cuadrados_bloques <- function(b, centro_a) {
  centro_b <- b$centro_desvio
  if (length(centro_b) != 1L || is.na(centro_b) || centro_b == centro_a) {
    return(list(partes = b$partes_cuadrados, cota = b$cota_cuadrados))
  }
  if (.exponente_partes_bloques(b) != 0 || length(centro_a) != 1L ||
      is.na(centro_a)) {
    return(list(partes = NULL, cota = Inf))
  }
  # `centro_b - centro_a` sin error, por TwoSum.
  delta <- centro_b - centro_a
  bb <- delta - centro_b
  resto_delta <- (centro_b - (delta - bb)) + (-centro_a - bb)
  deltas <- c(delta, resto_delta)
  suma_b <- c(b$partes_suma, -.producto_exacto_bloques(b$n, centro_b))
  terminos <- numeric()
  cota <- b$cota_cuadrados
  for (u in deltas) {
    for (s in suma_b) {
      producto <- .producto_cota_bloques(u, s)
      terminos <- c(terminos, 2 * producto$valores)
      cota <- cota + 2 * producto$cota
    }
    for (v in deltas) {
      producto <- .producto_cota_bloques(u, v)
      for (w in producto$valores) {
        otro <- .producto_cota_bloques(b$n, w)
        terminos <- c(terminos, otro$valores)
        cota <- cota + otro$cota + b$n * producto$cota
      }
    }
  }
  list(
    partes = .agregar_partes_suma_bloques(b$partes_cuadrados, terminos),
    cota = cota * (1 + 2^-50)
  )
}

# El doble mas cercano de `2^exponente sqrt(W / (n (n - 1)))`, con
# `W = n Q - X^2` en la unidad `2^(2 exponente)`, comparando `W` contra
# `n (n - 1) m^2` para los puntos medios `m` a cada lado del candidato. `W` se
# conoce exacta salvo la cota; si un punto medio cae dentro de ella,
# `decidido = FALSE` y el valor es el candidato. Con cota cero el empate va a
# par, como el resto del paquete.
.desvio_mas_cercano_bloques <- function(estado) {
  # En doble: `n` llega `integer`, y `n * n` desborda desde 46.341.
  n <- as.numeric(estado$n)
  if (!isTRUE(n > 1)) return(list(valor = NA_real_, decidido = TRUE))
  if (!isTRUE(estado$desvio_valido) || is.null(estado$partes_suma) ||
      is.null(estado$partes_cuadrados)) {
    return(list(valor = NA_real_, decidido = FALSE))
  }
  exponente <- estado$exponente_cuadrados
  centro <- estado$centro_desvio
  if (length(centro) != 1L || is.na(centro)) centro <- 0
  # `X` en la unidad `2^exponente`. Un cambio de unidad que deja una parte en
  # los subnormales se cuenta en la cota de `X`.
  desplazamiento <- .exponente_partes_bloques(estado) - exponente
  partes_x <- estado$partes_suma
  cota_x <- 0
  if (desplazamiento != 0) {
    llevadas <- .por_potencia_dos_bloques(partes_x, desplazamiento)
    cota_x <- sum(.por_potencia_dos_bloques(llevadas, -desplazamiento) !=
                    partes_x) * 2^-1073
    partes_x <- llevadas
  }
  chicas <- estado$partes_suma_chicas
  if (length(chicas)) {
    llevadas <- .por_potencia_dos_bloques(chicas, -exponente)
    cota_x <- cota_x + sum(.por_potencia_dos_bloques(llevadas, exponente) !=
                             chicas) * 2^-1073
    partes_x <- c(partes_x, llevadas)
  }
  if (centro != 0) {
    producto <- .producto_cota_bloques(n, centro)
    partes_x <- c(partes_x, -.por_potencia_dos_bloques(
      producto$valores, -exponente
    ))
    cota_x <- cota_x + .por_potencia_dos_bloques(producto$cota, -exponente)
  }
  if (!all(is.finite(partes_x))) {
    return(list(valor = NA_real_, decidido = FALSE))
  }
  xs <- .expansion_no_solapada_bloques(partes_x)
  qs <- .expansion_no_solapada_bloques(estado$partes_cuadrados)
  if (!all(is.finite(c(xs, qs)))) {
    return(list(valor = NA_real_, decidido = FALSE))
  }
  terminos <- numeric()
  cota_redondeo <- 0
  for (q in qs) {
    producto <- .producto_cota_bloques(n, q)
    terminos <- c(terminos, producto$valores)
    cota_redondeo <- cota_redondeo + producto$cota
  }
  for (i in seq_along(xs)) {
    for (j in seq_len(i)) {
      producto <- .producto_cota_bloques(xs[[i]], xs[[j]])
      factor <- if (i == j) 1 else 2
      terminos <- c(terminos, -factor * producto$valores)
      cota_redondeo <- cota_redondeo + factor * producto$cota
    }
  }
  if (!all(is.finite(terminos))) {
    return(list(valor = NA_real_, decidido = FALSE))
  }
  w <- .expansion_no_solapada_bloques(terminos)
  modulo_x <- sum(abs(xs))
  cota_w <- (n * estado$cota_cuadrados + (2 * modulo_x + cota_x) * cota_x +
               cota_redondeo) * (1 + 2^-40)
  if (!is.finite(cota_w)) return(list(valor = NA_real_, decidido = FALSE))
  # `W` exacta y cero: todos los valores son iguales. Se dice aca porque el
  # punto medio sobre el 0, 2^-1075, no es un doble en la unidad 1.
  if (cota_w == 0 && !length(w)) return(list(valor = 0, decidido = TRUE))
  pares <- .producto_exacto_bloques(n, n - 1)
  aproximada <- 0
  for (componente in w) aproximada <- aproximada + componente
  v <- aproximada / (n * (n - 1))
  candidato <- .por_potencia_dos_bloques(if (v > 0) sqrt(v) else 0, exponente)
  if (!is.finite(candidato)) return(list(valor = candidato, decidido = TRUE))
  # El signo de `W - n (n - 1) m^2 - margen`, con `m = c + h` en la unidad y
  # `h` una potencia de dos: `m^2 = c^2 + 2 c h + h^2`.
  signo_frente <- function(cm, h, margen) {
    cuadrado <- .producto_cota_bloques(cm, cm)
    medio <- c(cuadrado$valores, 2 * cm * h, h * h)
    # `2 c h` y `h^2` son exactos salvo que caigan en los subnormales.
    if ((cm != 0 && abs(2 * cm * h) < 2^-1000) || h * h < 2^-1000) {
      margen <- margen + n * n * 2^-1060
    }
    restar <- numeric()
    for (factor in pares) {
      for (componente in medio) {
        producto <- .producto_cota_bloques(factor, componente)
        restar <- c(restar, producto$valores)
        margen <- margen + producto$cota
      }
    }
    margen <- margen + n * n * cuadrado$cota
    list(
      arriba = .signo_suma_exacta_bloques(c(w, -restar, -margen)),
      abajo = .signo_suma_exacta_bloques(c(w, -restar, margen))
    )
  }
  en_unidad <- function(x) .por_potencia_dos_bloques(x, -exponente)
  vecino <- function(x, arriba) {
    if (x == 0) return(if (arriba) 2^-1074 else 0)
    paso <- .espaciado_doble_bloques(x, hacia_cero = !arriba)
    x + if (arriba) paso else -paso
  }
  par <- function(a, b) {
    if (a == 0) return(a)
    mantisa <- abs(a) / .espaciado_doble_bloques(a, hacia_cero = FALSE)
    if (mantisa %% 2 == 0) a else b
  }
  for (intento in seq_len(8L)) {
    cm <- en_unidad(candidato)
    paso_arriba <- if (candidato == 0) 2^-1074 else {
      .espaciado_doble_bloques(candidato, hacia_cero = FALSE)
    }
    h_arriba <- .por_potencia_dos_bloques(paso_arriba, -exponente - 1)
    if (!(h_arriba > 0)) return(list(valor = candidato, decidido = FALSE))
    frente <- signo_frente(cm, h_arriba, cota_w)
    if (frente$arriba > 0) {
      candidato <- vecino(candidato, TRUE)
      next
    }
    if (frente$abajo >= 0) {
      if (cota_w == 0 && frente$arriba == 0) {
        return(list(valor = par(candidato, vecino(candidato, TRUE)),
                    decidido = TRUE))
      }
      return(list(valor = candidato, decidido = FALSE))
    }
    if (candidato == 0) return(list(valor = 0, decidido = TRUE))
    paso_abajo <- .espaciado_doble_bloques(candidato, hacia_cero = TRUE)
    h_abajo <- .por_potencia_dos_bloques(paso_abajo, -exponente - 1)
    if (!(h_abajo > 0)) return(list(valor = candidato, decidido = FALSE))
    frente <- signo_frente(cm, -h_abajo, cota_w)
    if (frente$abajo < 0) {
      candidato <- vecino(candidato, FALSE)
      next
    }
    if (frente$arriba <= 0) {
      if (cota_w == 0 && frente$abajo == 0) {
        return(list(valor = par(candidato, vecino(candidato, FALSE)),
                    decidido = TRUE))
      }
      return(list(valor = candidato, decidido = FALSE))
    }
    return(list(valor = candidato, decidido = TRUE))
  }
  list(valor = candidato, decidido = FALSE)
}

.absorber_cuantitativos <- function(acumulador, bloque) {
  valores <- bloque$valores
  aplicable <- bloque$aplicable
  if (!is.numeric(valores)) {
    return(.marcar_fallo_acumulador(
      acumulador, "valores_cuantitativos_no_numericos"
    ))
  }
  # Con la mascara entera en `TRUE` -la de un bloque sin centinelas- el
  # subconjunto es el mismo vector: no se copia.
  if (!is.null(attributes(valores)) || !isTRUE(all(aplicable))) {
    valores <- valores[!is.na(aplicable) & aplicable]
  }
  estado <- acumulador$estado_familia
  if (!length(valores)) {
    acumulador$estado_familia <- estado
    return(acumulador)
  }
  # Un estado sin `desvio_modo` se guardo antes de que el desvio fuera el mas
  # cercano: conserva su cuenta de Welford.
  welford <- is.null(estado$desvio_modo)
  # `min()`, `max()`, `mean()` y la resta de base leen los bits de un
  # `integer64` como si fueran un doble: con `bloque_filas`, una clave
  # `bigserial` publicaba `minimo = 4.94e-324` y su `media` `integer64` truncaba
  # las de toda la tabla. Se opera como doble cuando el doble lo sostiene; si
  # algun valor llega a 2^53, la condicion es de la columna y el acumulador
  # entero queda `no_disponible` con el motivo.
  como_doble <- .integer64_como_doble_dbi(valores)
  if (!isTRUE(como_doble$ok)) {
    return(.marcar_fallo_acumulador(acumulador, como_doble$motivo))
  }
  valores <- como_doble$valores
  # Un bloque sin `NaN` ni infinitos -el caso comun- no los cuenta: son seis
  # pasadas por el vector que pagan la suma exacta de abajo.
  finito <- is.finite(valores)
  if (all(finito) && is.null(attributes(valores))) {
    finitos <- valores
  } else {
    estado$n_nan <- estado$n_nan + sum(is.nan(valores))
    estado$n_infinito_positivo <- estado$n_infinito_positivo +
      sum(is.infinite(valores) & valores > 0, na.rm = TRUE)
    estado$n_infinito_negativo <- estado$n_infinito_negativo +
      sum(is.infinite(valores) & valores < 0, na.rm = TRUE)
    finitos <- valores[finito]
  }
  if (!length(finitos)) {
    acumulador$estado_familia <- estado
    return(acumulador)
  }
  if (welford && isTRUE(estado$escala_adaptativa)) {
    extremos_previos <- c(estado$minimo, estado$maximo)
    extremos_previos <- extremos_previos[is.finite(extremos_previos)]
    max_abs <- max(c(abs(extremos_previos), abs(finitos)), na.rm = TRUE)
    extremos_nuevos <- c(extremos_previos, finitos)
    minimo_nuevo <- min(extremos_nuevos)
    maximo_nuevo <- max(extremos_nuevos)
    rango_nuevo <- suppressWarnings(maximo_nuevo - minimo_nuevo)
    escala_nueva <- .escala_extrema_bloques(
      max_abs, rango = rango_nuevo
    )
    desplazamiento_nuevo <- if (.rango_requiere_escala(rango_nuevo)) {
      minimo_nuevo
    } else 0
    escala_actual <- if (length(estado$escala) == 1L &&
                         is.finite(estado$escala) && estado$escala > 0) {
      estado$escala
    } else 1
    desplazamiento_actual <- if (length(estado$desplazamiento) == 1L &&
                                 is.finite(estado$desplazamiento)) {
      estado$desplazamiento
    } else 0
    if (!identical(escala_nueva, escala_actual) ||
        !identical(desplazamiento_nuevo, desplazamiento_actual)) {
      estado <- .reescalar_estado_cuantitativo_bloques(
        estado, escala_nueva, desplazamiento_nuevo
      )
    }
  }
  minimo_bloque <- min(finitos)
  maximo_bloque <- max(finitos)
  # La media de Welford es buena para los momentos, no para publicar la media:
  # la media sale de la suma exacta (`.partes_suma_exacta_bloques()`), sobre los
  # valores y no sobre los de los momentos. Con `rnorm(n) * 2^-1000` la escala
  # va por la dispersion y el desplazamiento es el minimo: restarlo no es exacto
  # entre valores diminutos de magnitudes distintas, y `media * escala +
  # desplazamiento` cancelaba (35 ulps). Y dividir por la escala de la magnitud
  # extrema dejaba bajo el menor doble a los valores 2^1023 veces menores que el
  # mayor: con 2^1000, -2^1000 y valores de 2^-100 la media salia 0. Las partes
  # se dividen solo por la potencia de dos que evita el desborde
  # (`.exponente_partes_bloques()`), y el desplazamiento queda para los momentos
  # del desvio de un estado guardado sin `desvio_modo`, como `suma` y su
  # compensacion de Neumaier.
  maximo_abs_bloque <- max(-minimo_bloque, maximo_bloque)
  exponente_partes <- .exponente_partes_bloques(estado)
  necesario <- .exponente_partes_necesario_bloques(maximo_abs_bloque)
  if (necesario > exponente_partes) {
    llevadas <- .llevar_partes_suma_bloques(
      estado$partes_suma, exponente_partes, necesario
    )
    estado$partes_suma <- .agregar_partes_suma_bloques(
      llevadas$partes, numeric()
    )
    if (length(llevadas$chicas)) {
      estado$partes_suma_chicas <- .agregar_partes_suma_bloques(
        estado$partes_suma_chicas, llevadas$chicas
      )
    }
    exponente_partes <- necesario
    estado$exponente_partes <- necesario
  }
  partes_bloque <- if (exponente_partes > 0) {
    chicos <- abs(finitos) < 2^(exponente_partes - 1022)
    if (any(chicos)) {
      estado$partes_suma_chicas <- .agregar_partes_suma_bloques(
        estado$partes_suma_chicas,
        .partes_suma_exacta_bloques(finitos[chicos])
      )
    }
    .partes_suma_exacta_bloques(finitos[!chicos] * 2^-exponente_partes)
  } else {
    .partes_suma_exacta_bloques(finitos, maximo_abs_bloque)
  }
  estado$partes_suma <- .agregar_partes_suma_bloques(
    estado$partes_suma, partes_bloque
  )
  n_bloque <- length(finitos)
  if (welford) {
    # Los momentos se acumulan centrados en una constante -la media del primer
    # bloque-, y la media publicada es `centro + media`. Sin centrar, la fusion
    # de bloques operaba con medias absolutas: sobre 61 marcas de tiempo en
    # milisegundos de una ventana de un segundo el desvio salia 2,3e-8 relativo
    # lejos del de R, porque la diferencia entre dos medias de 1,76e12 conserva
    # pocas cifras. Centrado, esa diferencia es la de dos medias
    # chicas. Ver `.formas_desvio_dbi()`, la misma propiedad en el motor.
    escala <- if (length(estado$escala) == 1L && is.finite(estado$escala) &&
                  estado$escala > 0) estado$escala else 1
    escalado <- !identical(escala, 1)
    desplazamiento <- if (escalado && length(estado$desplazamiento) == 1L &&
                          is.finite(estado$desplazamiento)) {
      estado$desplazamiento
    } else 0
    valores_operacion <- if (escalado) {
      (finitos - desplazamiento) / escala
    } else finitos
    suma_bloque <- sum(valores_operacion)
    total_suma <- estado$suma + suma_bloque
    correccion <- if (abs(estado$suma) >= abs(suma_bloque)) {
      (estado$suma - total_suma) + suma_bloque
    } else {
      (suma_bloque - total_suma) + estado$suma
    }
    if (is.finite(correccion)) {
      estado$suma_compensacion <- estado$suma_compensacion + correccion
    }
    estado$suma <- total_suma
    centro <- if (escalado && !isTRUE(estado$n > 0)) {
      sum(valores_operacion) / length(valores_operacion)
    } else {
      .centro_cuantitativo_bloques(estado, valores_operacion)
    }
    estado$centro <- centro
    centrados <- valores_operacion - centro
    n_bloque <- length(centrados)
    media_bloque <- if (escalado) sum(centrados) / n_bloque else {
      mean(centrados)
    }
    media_bloque_abs <- if (escalado) {
      sum(valores_operacion) / n_bloque + desplazamiento / escala
    } else NULL
    diferencias <- centrados - media_bloque
    m2_bloque <- sum(diferencias * diferencias)
    if (estado$n == 0L) {
      estado$media <- media_bloque
      if (escalado) estado$media_abs <- media_bloque_abs
      estado$m2 <- m2_bloque
      estado$n <- n_bloque
    } else {
      total <- estado$n + n_bloque
      diferencia <- media_bloque - estado$media
      estado$m2 <- estado$m2 + m2_bloque +
        diferencia * diferencia * estado$n * n_bloque / total
      estado$media <- estado$media + diferencia * n_bloque / total
      if (escalado) {
        estado$media_abs <- estado$media_abs +
          (media_bloque_abs - estado$media_abs) * n_bloque / total
      }
      estado$n <- total
    }
  } else {
    estado <- if (identical(estado$desvio_modo, "rapido")) {
      .absorber_desvio_rapido_bloques(
        estado, finitos, minimo_bloque, maximo_bloque, partes_bloque
      )
    } else {
      .absorber_desvio_exacto_bloques(
        estado, finitos, minimo_bloque, maximo_bloque
      )
    }
    # El mismo tipo que la cuenta de Welford: `integer` desde el primer bloque
    # (`n_evaluados` se publica en `meta`), y doble solo si no entra.
    estado$n <- if (!isTRUE(estado$n > 0)) n_bloque else if (
      is.integer(estado$n) && estado$n <= .Machine$integer.max - n_bloque
    ) estado$n + n_bloque else as.numeric(estado$n) + n_bloque
  }
  estado$minimo <- min(estado$minimo, minimo_bloque)
  estado$maximo <- max(estado$maximo, maximo_bloque)
  # Los signos solo se cuentan en un bloque que cruza el cero: si no, el
  # minimo y el maximo ya los dicen.
  if (isTRUE(estado$contar_signos)) {
    if (maximo_bloque < 0) {
      estado$n_negativos <- estado$n_negativos + n_bloque
    } else if (minimo_bloque <= 0) {
      estado$n_ceros <- estado$n_ceros + sum(finitos == 0)
      if (minimo_bloque < 0) {
        estado$n_negativos <- estado$n_negativos + sum(finitos < 0)
      }
    }
  }
  acumulador$estado_familia <- estado
  acumulador
}

.absorber_longitudes <- function(acumulador, bloque) {
  x <- bloque$valores
  if (!is.character(x) && !is.factor(x)) {
    return(.marcar_fallo_acumulador(acumulador, "longitudes_no_textuales"))
  }
  aplica <- !is.na(bloque$aplicable) & bloque$aplicable
  valores <- as.character(x)[aplica]
  if (isTRUE(
    acumulador$configuracion$configuracion$representacion_binaria
  )) {
    capturas <- regmatches(
      valores, regexec("^([0-9]+):[0-9A-Fa-f]{32}$", valores, perl = TRUE)
    )
    longitudes <- suppressWarnings(as.numeric(vapply(capturas, function(x) {
      if (length(x) > 1L) x[[2L]] else NA_character_
    }, character(1L))))
  } else {
    longitudes <- .nchar_texto_seguro(valores)
  }
  # Lo que se CUENTA aca es sobre cuantos valores se calculo la longitud, y eso es
  # lo que declara el universo: `nchar()` devuelve `NA` cuando no puede medir el
  # valor -bytes que no son UTF-8 valido-, y esos se descartaban sin que ningun
  # campo dijera cuantos quedaron afuera. Con tres valores, uno invalido, la fila
  # publicaba `longitud_media = 4.5` -el promedio de DOS- y `n` decia 3.
  #
  # NO se publica un conteo de "no medibles" aparte, y el motivo es que en esta capa
  # no se puede medir sin mentir: dentro de `perfilar()` la columna llega despues de
  # `.texto_analizable()`, que ya reemplazo el valor invalido por `NA`, asi que un
  # valor no medible es indistinguible de una ausencia. El primer intento los conto
  # juntos y un fixture de la suite con un `NA_character_` lo delato. La causa ya
  # viaja aparte, en `n_codificacion_invalida`, medida sobre la columna cruda.
  estado <- acumulador$estado_familia
  longitudes <- longitudes[!is.na(longitudes)]
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
  orden <- .orden_por_bytes(ordinales)
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
  # El bloque y los representantes en la misma clase antes de compararlos: ver
  # `.unificar_enteros_64_R()`.
  if (length(estado$representantes)) {
    unidos <- .unificar_enteros_64_R(estado$representantes, x)
    if (is.null(unidos)) {
      return(.marcar_fallo_acumulador(
        acumulador, "igualdad_s3_no_reproducible"
      ))
    }
    estado$representantes <- unidos$a
    x <- unidos$b
  }
  # `match()` sobre el bloque entero conserva la igualdad de R y evita una
  # llamada de dispatch por celda. El camino lento de abajo queda para clases
  # con semántica propia; el caso atomico sin clase es el que domina las
  # columnas grandes de texto y numeros.
  puede_vectorizar <- is.atomic(x) && is.null(dim(x)) && (
    !length(estado$representantes) ||
      .puede_vectorizar_match_R(x, estado$representantes)
  )
  if (puede_vectorizar) {
    n_representantes <- length(estado$representantes)
    posiciones <- if (n_representantes) {
      .match_R(x, estado$representantes)
    } else integer(length(x))
    if (length(posiciones) == length(x) && !anyNA(posiciones) &&
        n_representantes) {
      estado$frecuencias <- estado$frecuencias + tabulate(
        posiciones[posiciones > 0L], nbins = n_representantes
      )
    }
    if (length(posiciones) == length(x) && !anyNA(posiciones)) {
      nuevos <- x[posiciones == 0L]
      if (!length(nuevos)) {
        acumulador$estado_familia <- estado
        return(acumulador)
      }
      claves_nuevos <- tryCatch(
        .claves_match_R(nuevos, nuevos, .tipo_match_R(nuevos))$valores,
        error = function(e) NULL
      )
      if (is.null(claves_nuevos)) {
        correspondencias <- NULL
      } else {
        # `unique.factor()` distingue codigos, mientras `.match_R()` compara
        # etiquetas. Usar la misma clave evita representantes fantasma cuando
        # un factor trae niveles repetidos.
        nuevos <- nuevos[!duplicated(claves_nuevos)]
        correspondencias <- .match_R(x, nuevos)
      }
    } else {
      correspondencias <- NULL
    }
    if (length(correspondencias) == length(x) && !anyNA(correspondencias)) {
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
    .detener("La clave de fila requiere un data.frame.", call. = FALSE)
  }
  columnas_matriciales <- vapply(
    datos,
    .es_columna_compuesta,
    logical(1L)
  )
  if (!any(columnas_matriciales)) return(datos)
  columnas <- list()
  for (i in seq_along(datos)) {
    columna <- datos[[i]]
    if (columnas_matriciales[[i]]) {
      # Una tabla anidada se aplana por la misma regla, recursiva: sus columnas
      # pueden ser a su vez matrices, listas o tablas.
      componentes <- if (is.data.frame(columna)) {
        .expandir_datos_identidad(columna)
      } else {
        as.data.frame(unclass(columna), stringsAsFactors = FALSE)
      }
      for (j in seq_along(componentes)) {
        columnas[[length(columnas) + 1L]] <- componentes[[j]]
      }
    } else {
      columnas[[length(columnas) + 1L]] <- columna
    }
  }
  # `as.data.frame(columnas)` no sirve para volver a armar la tabla: si alguna
  # columna es una LISTA con un elemento `NULL`, `as.data.frame.list()` recursa
  # sobre ella y ABORTA -"los argumentos implican un numero diferente de filas:
  # 2, 0"-. Medido: una tabla con una columna matriz y una columna de lista con
  # un `NULL` adentro mataba `perfilar()` al contar filas duplicadas, con datos
  # que el usuario puede construir sin hacer nada raro.
  #
  # Las columnas ya tienen el largo de la tabla -las componentes de la matriz
  # tambien-, asi que armar el `data.frame` por estructura no coacciona ninguna
  # y no hay nada que verificar. Los nombres no se usan: las dos vias que
  # consumen esto comparan columnas por posicion.
  structure(
    columnas,
    names = paste0("v", seq_along(columnas)),
    class = "data.frame",
    row.names = seq_len(nrow(datos))
  )
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
    .detener("`acumulador` no es un acumulador de bloques.", call. = FALSE)
  }
  if (identical(acumulador$estado, "finalizado")) {
    .detener("No se puede absorber despu\u00e9s de `finalizar()`.", call. = FALSE)
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

# El centro de un estado de momentos: la constante a la que estan referidos su
# media y su `m2`. Un estado sin centro -vacio, o guardado sin ese campo-
# acumula sin centrar, que es centro 0.
.centro_estado_bloques <- function(estado) {
  centro <- estado$centro
  if (is.null(centro) || length(centro) != 1L || !is.finite(centro)) {
    return(0)
  }
  centro
}

# El centro se fija con el primer bloque que trae valores y no se mueve: todos
# los bloques de un mismo estado se restan la misma constante. Es la media de ese
# bloque, o su primer valor si la media no es finita.
.centro_cuantitativo_bloques <- function(estado, finitos) {
  centro <- estado$centro
  if (!is.null(centro) && length(centro) == 1L && is.finite(centro)) {
    return(centro)
  }
  if (isTRUE(estado$n > 0)) return(0)
  centro <- mean(finitos)
  if (!is.finite(centro)) centro <- finitos[[1L]]
  centro
}

# Cambiar la unidad de los momentos no cambia el numero representado: ambas
# escalas son potencias de dos. Esto permite que la fuente DBI descubra una
# magnitud extrema despues de haber absorbido bloques anteriores, sin volver a
# leerlos. `media_abs` conserva la media en la unidad normalizada para que la
# salida no tenga que reconstruirla con una resta grande.
.reescalar_estado_cuantitativo_bloques <- function(
    estado, escala_nueva, desplazamiento_nuevo = NULL) {
  escala_vieja <- if (length(estado$escala) == 1L &&
                      is.finite(estado$escala) && estado$escala > 0) {
    estado$escala
  } else 1
  desplazamiento_viejo <- if (length(estado$desplazamiento) == 1L &&
                              is.finite(estado$desplazamiento)) {
    estado$desplazamiento
  } else 0
  if (length(desplazamiento_nuevo) != 1L ||
      !is.finite(desplazamiento_nuevo)) {
    desplazamiento_nuevo <- desplazamiento_viejo
  }
  if (length(escala_nueva) != 1L || !is.finite(escala_nueva) ||
      escala_nueva <= 0) {
    estado$escala <- if (length(escala_nueva) == 1L &&
                         is.finite(escala_nueva) && escala_nueva > 0) {
      escala_nueva
    } else escala_vieja
    estado$desplazamiento <- desplazamiento_nuevo
    return(estado)
  }
  if (identical(escala_vieja, escala_nueva) &&
      identical(desplazamiento_viejo, desplazamiento_nuevo)) {
    estado$escala <- escala_nueva
    estado$desplazamiento <- desplazamiento_nuevo
    return(estado)
  }
  if (!isTRUE(estado$n > 0)) {
    estado$escala <- escala_nueva
    estado$desplazamiento <- desplazamiento_nuevo
    return(estado)
  }
  factor <- escala_vieja / escala_nueva
  corrimiento <- (desplazamiento_viejo - desplazamiento_nuevo) / escala_nueva
  centro <- .centro_estado_bloques(estado)
  media <- estado$media
  media_abs <- if (!identical(escala_vieja, 1) &&
                   length(estado$media_abs) == 1L &&
                   is.finite(estado$media_abs)) {
    estado$media_abs
  } else centro + media
  estado$centro <- centro * factor + corrimiento
  estado$media <- media * factor
  estado$media_abs <- media_abs * factor
  estado$suma <- estado$suma * factor + corrimiento * estado$n
  # Las partes de la suma exacta no estan en la unidad de los momentos (ver
  # `.absorber_cuantitativos()`): no cambian.
  estado$suma_compensacion <- estado$suma_compensacion * factor
  estado$m2 <- estado$m2 * factor * factor
  estado$escala <- escala_nueva
  estado$desplazamiento <- desplazamiento_nuevo
  estado
}

# Las partes de `Q` de dos estados con valores. `b$partes_suma` y `b$n` son
# todavia los de `b`. Un estado guardado sin `desvio_modo` no se puede juntar
# con uno que lo tiene: el desvio queda sin calcular.
.fusionar_desvio_bloques <- function(a, b) {
  if (is.null(a$desvio_modo) || !identical(a$desvio_modo, b$desvio_modo) ||
      !isTRUE(a$desvio_valido) || !isTRUE(b$desvio_valido)) {
    a$desvio_modo <- a$desvio_modo %||% b$desvio_modo
    a$desvio_valido <- FALSE
    return(a)
  }
  if (identical(a$desvio_modo, "rapido")) {
    llevado <- .recentrar_cuadrados_bloques(b, a$centro_desvio)
    a$partes_cuadrados <- .agregar_partes_suma_bloques(
      a$partes_cuadrados, llevado$partes
    )
    a$cota_cuadrados <- (a$cota_cuadrados + llevado$cota) * (1 + 2^-52)
  } else {
    # La unidad de un estado sin cuadrados todavia no se eligio.
    sin_cuadrados <- function(e) {
      !length(e$partes_cuadrados) && e$cota_cuadrados == 0
    }
    exponente <- if (sin_cuadrados(a)) b$exponente_cuadrados else
      if (sin_cuadrados(b)) a$exponente_cuadrados else
        max(a$exponente_cuadrados, b$exponente_cuadrados)
    llevar <- function(estado) {
      k <- 2 * (estado$exponente_cuadrados - exponente)
      partes <- estado$partes_cuadrados
      if (k == 0) return(list(partes = partes, cota = estado$cota_cuadrados))
      llevadas <- .por_potencia_dos_bloques(partes, k)
      perdidas <- sum(.por_potencia_dos_bloques(llevadas, -k) != partes)
      list(
        partes = llevadas,
        cota = .por_potencia_dos_bloques(estado$cota_cuadrados, k) *
          (1 + 2^-52) + perdidas * 2^-1073
      )
    }
    de_a <- llevar(a)
    de_b <- llevar(b)
    a$exponente_cuadrados <- exponente
    a$partes_cuadrados <- .agregar_partes_suma_bloques(
      de_a$partes, de_b$partes
    )
    a$cota_cuadrados <- (de_a$cota + de_b$cota) * (1 + 2^-52)
  }
  if (is.null(a$partes_cuadrados) || !is.finite(a$cota_cuadrados)) {
    a$desvio_valido <- FALSE
  }
  a
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
  escala_a <- if (length(a$escala) == 1L && is.finite(a$escala) &&
                  a$escala > 0) a$escala else 1
  escala_b <- if (length(b$escala) == 1L && is.finite(b$escala) &&
                  b$escala > 0) b$escala else 1
  factor_b <- escala_b / escala_a
  suma_b <- b$suma * factor_b
  suma_a <- a$suma
  total_suma <- suma_a + suma_b
  if (abs(suma_a) >= abs(suma_b)) {
    correccion <- (suma_a - total_suma) + suma_b
  } else {
    correccion <- (suma_b - total_suma) + suma_a
  }
  a$suma <- total_suma
  # Las partes de cada estado, llevadas al mayor de los dos exponentes.
  exponente_a <- .exponente_partes_bloques(a)
  exponente_b <- .exponente_partes_bloques(b)
  exponente_ab <- max(exponente_a, exponente_b)
  if (is.null(a$partes_suma) || is.null(b$partes_suma)) {
    a$partes_suma <- NULL
  } else {
    de_a <- .llevar_partes_suma_bloques(
      a$partes_suma, exponente_a, exponente_ab
    )
    de_b <- .llevar_partes_suma_bloques(
      b$partes_suma, exponente_b, exponente_ab
    )
    a$partes_suma <- .agregar_partes_suma_bloques(de_a$partes, de_b$partes)
    chicas <- c(de_a$chicas, de_b$chicas, b$partes_suma_chicas)
    if (length(chicas)) {
      a$partes_suma_chicas <- .agregar_partes_suma_bloques(
        a$partes_suma_chicas %||% numeric(), chicas
      )
    }
  }
  a$exponente_partes <- exponente_ab
  if (!is.null(a$desvio_modo) || !is.null(b$desvio_modo)) {
    a <- .fusionar_desvio_bloques(a, b)
  }
  a$suma_compensacion <- a$suma_compensacion +
    b$suma_compensacion * factor_b + correccion
  # Cada estado acumula centrado en su propia constante: la media de `b` se
  # lleva al centro de `a` antes de fusionar. La resta de dos centros cercanos es
  # exacta.
  media_b <- b$media + (.centro_estado_bloques(b) - .centro_estado_bloques(a))
  diferencia <- media_b - a$media
  a$m2 <- a$m2 + b$m2 + diferencia * diferencia * a$n * b$n / total
  a$media <- a$media + diferencia * b$n / total
  if (!identical(escala_a, 1)) {
    media_a_abs <- if (length(a$media_abs) == 1L) a$media_abs else {
      .centro_estado_bloques(a) + a$media
    }
    media_b_abs <- if (length(b$media_abs) == 1L) b$media_abs else {
      .centro_estado_bloques(b) + b$media
    }
    a$media_abs <- media_a_abs + (media_b_abs - media_a_abs) * b$n / total
  }
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
  a$indices <- .sort_por_bytes(unique(c(a$indices, b$indices)))
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
  a$indices <- .sort_por_bytes(unique(c(a$indices, b$indices)))
  a$n <- length(a$indices)
  acumulador$estado_familia <- a
  acumulador
}

.fusionar_acumuladores <- function(acumulador, otro) {
  if (!inherits(acumulador, "acumulador_bloques") ||
      !inherits(otro, "acumulador_bloques")) {
    .detener("Ambos objetos deben ser acumuladores de bloques.", call. = FALSE)
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
    todos <- todos[.orden_por_bytes(vapply(todos, `[[`, numeric(1L), 1L))]
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
    orden_intervalos <- .orden_por_bytes(vapply(
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
        escala <- if (length(estado$escala) == 1L &&
                      is.finite(estado$escala) && estado$escala > 0) {
          estado$escala
        } else 1
        desplazamiento <- if (length(estado$desplazamiento) == 1L &&
                              is.finite(estado$desplazamiento)) {
          estado$desplazamiento
        } else 0
        # El doble mas cercano de la media exacta de lo absorbido. Un estado
        # sin partes -guardado antes de que existieran- conserva la cuenta
        # anterior.
        exacta <- !is.null(estado$partes_suma)
        # El doble mas cercano del desvio exacto. Un estado guardado sin
        # `desvio_modo` conserva la cuenta de Welford; uno nuevo no la lleva.
        welford <- is.null(estado$desvio_modo)
        media <- if (exacta) {
          .media_mas_cercana_bloques(estado)
        } else if (!welford) {
          NA_real_
        } else if (identical(estado$escala, 1)) {
          .centro_estado_bloques(estado) + estado$media
        } else {
          (estado$suma + estado$suma_compensacion) / estado$n
        }
        desvio <- if (!welford) {
          decision <- .desvio_mas_cercano_bloques(estado)
          # Fuera del estado, para que `bytes_retenidos` no cambie: lo lee
          # `.resumen_cuantitativo_bloques()` para saber si hay que recorrer
          # la columna otra vez por la manera exacta.
          acumulador$desvio_decidido <- decision$decidido
          decision$valor
        } else if (estado$n > 1L) {
          sqrt(estado$m2 / (estado$n - 1))
        } else NA_real_
        if (welford && !identical(escala, 1)) {
          # Con escala por dispersion los momentos se acumularon desplazados
          # por el minimo (ver `.resumen_cuantitativo_bloques()`); la media
          # exacta ya esta en la unidad publicada.
          if (!exacta) media <- media * escala + desplazamiento
          desvio <- desvio * escala
        }
        list(
          minimo = estado$minimo, maximo = estado$maximo,
          media = media, desvio = desvio,
          n_evaluados = estado$n, n_nan = estado$n_nan,
          n_infinito_positivo = estado$n_infinito_positivo,
          n_infinito_negativo = estado$n_infinito_negativo,
          n_ceros = estado$n_ceros, n_negativos = estado$n_negativos
        )
      }
    },
    longitudes = if (!estado$n) {
      c(minimo = NA_real_, maximo = NA_real_, media = NA_real_,
        n_resumidas = 0)
    } else {
      c(minimo = estado$minimo, maximo = estado$maximo,
        media = estado$suma / estado$n, n_resumidas = estado$n)
    },
    distintos = if (isTRUE(estado$truncado)) NULL else {
      orden <- .orden_por_bytes(estado$primeros)
      representantes <- estado$representantes
      if (is.null(representantes)) {
        representantes <- switch(
          acumulador$configuracion$tipo,
          character = character(), logical = logical(), integer = integer(),
          double = numeric(), raw = raw(), numeric()
        )
      }
      .data_frame_simple(
        representante = representantes[orden],
        frecuencia = as.integer(estado$frecuencias[orden]),
        primer_ordinal = as.numeric(estado$primeros[orden]),
        stringsAsFactors = FALSE
      )
    },
    hueco = if (isTRUE(estado$truncado)) NULL else {
      orden <- .orden_por_bytes(estado$primeros)
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
      indices <- .sort_por_bytes(unique(as.integer(estado$indices)))
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
      orden <- .orden_por_bytes(estado$primeros)
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
          valores <- valores[.orden_por_bytes(ordinales)]
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
      indices = as.integer(.sort_por_bytes(unique(estado$indices))),
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
      significado = "cantidad m\u00ednima de valores distintos observados"
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
      "Aumentar `max_filas_hallazgo` para conservar m\u00e1s \u00edndices de evidencia."
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
    .detener("`acumulador` no es un acumulador de bloques.", call. = FALSE)
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
  if (!is.list(bloques)) .detener("`bloques` debe ser una lista.", call. = FALSE)
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
    .detener("`n` debe ser un entero no negativo.", call. = FALSE)
  }
  n <- as.integer(n)
  if (!n) return(list())
  if (is.null(k)) {
    tamano <- .validar_tope_bloques(tamano, "tamano")
    k <- ceiling(n / tamano)
  } else {
    if (length(k) != 1L || is.na(k) || k < 1 || k != floor(k)) {
      .detener("`k` debe ser un entero positivo.", call. = FALSE)
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
    .detener("`aplicable` debe ser l\u00f3gico y tener el largo del vector.",
         call. = FALSE)
  }
  if (identical(familia, "cuantitativos") &&
      (is.null(configuracion$escala) || !length(configuracion$escala))) {
    finitos <- suppressWarnings(x[is.finite(x)])
    max_abs <- if (length(finitos)) max(abs(finitos)) else 0
    configuracion$escala <- .escala_extrema_bloques(max_abs)
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

.escala_extrema_bloques <- function(max_abs, rango = NA_real_) {
  max_abs <- suppressWarnings(as.numeric(max_abs))
  rango <- suppressWarnings(as.numeric(rango))
  por_dispersion <- .rango_requiere_escala(rango)
  if (length(max_abs) != 1L || !is.finite(max_abs) || max_abs == 0 ||
      (!por_dispersion && max_abs >= 1e-150 && max_abs <= 1e150)) {
    return(1)
  }
  base <- if (por_dispersion) rango else max_abs
  escala <- 2^(floor(log2(base)) - 1)
  if (!is.finite(escala) || escala == 0) 1 else escala
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
  moda <- .moda_de_mapa(sobre$resultado)
  if (is.null(moda)) NULL else moda[c("valor", "frecuencia")]
}

# El desempate de la moda sobre un mapa de distintos, escrito UNA vez. La via
# por bloques de `perfilar_dbi()` tomaba `which.max(mapa$frecuencia)` -el
# primero en el orden del mapa, que para dobles es el de su texto y para texto
# no es por bytes- y publicaba otra moda que la via en memoria y la SQL sobre
# la misma tabla. Medido en SQLite: cuatro dobles empatados daban `3.25` en
# `perfilar()` y en SQL, y `10.608` con `bloque_filas`; tres textos daban `Mu`
# y `alfa`. `posicion` viaja para que quien publique con otro formato use la
# misma fila.
.moda_de_mapa <- function(mapa) {
  if (!is.data.frame(mapa) || !nrow(mapa) ||
      !any(!is.na(mapa$frecuencia))) {
    return(NULL)
  }
  candidatos <- which(mapa$frecuencia == max(mapa$frecuencia, na.rm = TRUE))
  if (length(candidatos) > 1L) {
    valores <- mapa$representante[candidatos]
    # `.orden_seguro()` y no `order()`: sobre `integer64` el `order()` de base
    # -que es el que rige dentro del espacio de nombres, aunque el usuario tenga
    # `bit64` adjunto- ordena por los bits del `double` y manda los negativos al
    # final. Ese era el desempate que hacia que la misma columna diera moda `1`
    # en `integer64` y `-999` en `double`.
    orden <- if (is.character(valores)) {
      match(.ordenar_por_bytes(valores), valores)
    } else {
      .orden_seguro(valores)
    }
    if (length(orden) == length(candidatos)) candidatos <-
      candidatos[orden]
  }
  posicion <- candidatos[[1L]]
  list(
    valor = .texto_moda(mapa$representante[posicion]),
    frecuencia = as.integer(mapa$frecuencia[[posicion]]),
    posicion = posicion
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
                                          max_bytes = Inf,
                                          excluir = NULL) {
  aplicable <- if (length(excluir)) {
    coincide <- tryCatch(x %in% excluir, error = function(e) NULL)
    if (is.null(coincide) || length(coincide) != length(x)) {
      NULL
    } else {
      !coincide
    }
  } else {
    NULL
  }
  # La escala y el desplazamiento de la columna eran para los momentos de
  # Welford; el desvio de un estado nuevo no los usa, y calcularlos eran seis
  # pasadas por la columna entera. En memoria el desvio va por la manera
  # rapida, y si su cota no deja decidir el redondeo la columna se recorre
  # otra vez por la exacta (ver `.desvio_mas_cercano_bloques()`).
  ejecutar <- function(modo) {
    .ejecutar_vector_bloques(
      x, "cuantitativos", k = k, tamano = tamano, max_bytes = max_bytes,
      configuracion = list(
        contar_signos = isTRUE(contar_signos), escala = 1,
        desplazamiento = 0, desvio = modo
      ),
      aplicable = aplicable
    )
  }
  ejecucion <- ejecutar("rapido")
  if (identical(ejecucion$resultado$estado, "calculado") &&
      isFALSE(ejecucion$acumulador$desvio_decidido)) {
    ejecucion <- ejecutar("exacto")
  }
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
    .detener("`vigilante` no es un vigilante de bloques.", call. = FALSE)
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
  orden <- tryCatch(.orden_por_bytes(representantes), error = function(e) NULL)
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
  orden <- tryCatch(.orden_por_bytes(con_forma), error = function(e) seq_along(con_forma))
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
    .detener("`aplicable` debe ser l\u00f3gico y tener el largo de los vectores.",
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
    .detener("`n` debe ser un entero no negativo y conocido.", call. = FALSE)
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
    .detener("El intervalo del bloque no es v\u00e1lido.", call. = FALSE)
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
    .detener("La partici\u00f3n no reprodujo los \u00edndices globales de la muestra.",
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
    .detener("`max_filas_hallazgo` debe ser un entero positivo o Inf.",
         call. = FALSE)
  }
  n <- length(x)
  if (!is.null(mascara) &&
      (!is.logical(mascara) || length(mascara) != n)) {
    .detener("`mascara` debe ser l\u00f3gica y tener el largo del vector.",
         call. = FALSE)
  }
  if (!is.null(aplicable) &&
      (!is.logical(aplicable) || length(aplicable) != n)) {
    .detener("`aplicable` debe ser l\u00f3gica y tener el largo del vector.",
         call. = FALSE)
  }
  if (!is.null(criterio) && !is.function(criterio)) {
    .detener("`criterio` debe ser una funci\u00f3n.", call. = FALSE)
  }
  if (!is.null(indices)) {
    if (is.logical(indices)) {
      if (length(indices) != n) {
        .detener("`indices` l\u00f3gicos debe tener el largo del vector.", call. = FALSE)
      }
      indices <- which(!is.na(indices) & indices)
    } else {
      if (!is.numeric(indices) || anyNA(indices) ||
          any(indices != floor(indices)) || any(indices < 1L | indices > n)) {
        .detener("`indices` debe contener posiciones globales v\u00e1lidas.",
             call. = FALSE)
      }
      indices <- .sort_por_bytes(unique(as.integer(indices)))
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
    .detener("`max_ejemplos` debe ser un entero positivo.", call. = FALSE)
  }
  n <- length(x)
  if (is.function(claves)) claves <- claves(x)
  if (is.null(claves)) claves <- x
  if (length(claves) != n) {
    .detener("`claves` debe tener el largo del vector.", call. = FALSE)
  }
  if (is.function(ejemplos)) ejemplos <- ejemplos(x)
  if (is.null(ejemplos)) ejemplos <- x
  if (length(ejemplos) != n) {
    .detener("`ejemplos` debe tener el largo del vector.", call. = FALSE)
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
    .detener("`max_patrones` debe ser un entero positivo.", call. = FALSE)
  }
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    .detener("`na.rm` debe ser TRUE o FALSE.", call. = FALSE)
  }
  if (!is.numeric(umbral_raro) || length(umbral_raro) != 1L ||
      is.na(umbral_raro) || umbral_raro < 0 || umbral_raro > 1) {
    .detener("`umbral_raro` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.atomic(x) && !is.factor(x)) {
    .detener("`x` debe ser un vector at\u00f3mico.", call. = FALSE)
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
    # El empate debe seguir el mismo orden por bytes que el resto del paquete;
    # `sort()` sobre texto usa la intercalacion del locale.
    niveles <- .ordenar_por_bytes(unique(claves))
    orden_niveles <- match(claves, niveles)
    orden <- .orden_por_bytes(-mapa$n, orden_niveles)
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
  # Frontera inclusiva, como en `descubrir_patrones()`: ver el comentario alli.
  indices_raros <- which(seq_along(frecuencias) > 1L &
                          proporciones <= umbral_raro)
  indices_excluidos <- which(seq_along(frecuencias) > 1L &
                              proporciones > umbral_raro)
  indices_resumen <- unique(c(
    if (length(frecuencias)) 1L else integer(),
    utils::head(indices_raros, .limite_patrones_raros_evidencia)
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
  # Mismo campo y misma definicion que en `descubrir_patrones()`.
  attr(resultado, "n_evaluados") <- as.integer(denominador)
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
