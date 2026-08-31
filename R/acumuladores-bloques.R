# Estado interno para el recorrido por bloques. Estas funciones no forman parte
# de la API publica: la API conserva la pasada en memoria y sus argumentos.

.VERSION_CONTRATO_BLOQUES <- "1"
.VERSION_CANONICALIZACION_R <- "igualdad-R-1"
.MAX_ENTRADAS_DISTINTOS <- 5000L
.MAX_BYTES_ESTADO_BLOQUES <- 512 * 1024^2
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
  if (identical(familia, "distintos") && is.infinite(max_entradas)) {
    max_entradas <- .MAX_ENTRADAS_DISTINTOS
  }
  if (identical(familia, "filas_distintos") && is.infinite(max_entradas)) {
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
    filas_distintos = list(
      representantes = NULL, frecuencias = numeric(), primeros = numeric(),
      grupos = integer(), n = 0, n_claves_omitidas = 0,
      truncado = FALSE, causa_tope = NULL
    ),
    generica = list(n = 0L),
    stop("Familia de acumulador desconocida: `", familia, "`.", call. = FALSE)
  )
  acumulador
}

.extraer_bloque <- function(bloque) {
  if (inherits(bloque, "data.frame")) {
    valores <- bloque
    inicio <- NA_real_
    fin <- NA_real_
    aplicable <- NULL
  } else if (is.list(bloque) && !is.null(names(bloque)) &&
      "valores" %in% names(bloque)) {
    valores <- bloque$valores
    inicio <- bloque$ordinal_inicio
    fin <- bloque$ordinal_fin
    if (is.null(inicio)) inicio <- NA_real_
    if (is.null(fin)) fin <- NA_real_
    aplicable <- bloque$aplicable
  } else {
    valores <- bloque
    inicio <- NA_real_
    fin <- NA_real_
    aplicable <- NULL
  }
  n_filas <- if (inherits(valores, "data.frame")) nrow(valores) else {
    length(valores)
  }
  if (is.null(aplicable)) aplicable <- rep(TRUE, n_filas)
  if (!is.logical(aplicable) || length(aplicable) != n_filas) {
    stop("La mascara `aplicable` debe ser logica y tener el largo del bloque.",
         call. = FALSE)
  }
  list(valores = valores, ordinal_inicio = inicio, ordinal_fin = fin,
       aplicable = aplicable)
}

.registrar_intervalo_bloque <- function(acumulador, bloque) {
  n <- if (inherits(bloque$valores, "data.frame")) nrow(bloque$valores) else {
    length(bloque$valores)
  }
  inicio <- bloque$ordinal_inicio
  fin <- bloque$ordinal_fin
  if (is.na(inicio)) inicio <- acumulador$ultimo_ordinal + 1
  if (is.na(fin)) fin <- inicio + n - 1
  if (n == 0L) {
    fin <- inicio - 1
  }
  if (length(inicio) != 1L || length(fin) != 1L || is.na(inicio) ||
      is.na(fin) || inicio < 1 || fin < inicio - 1 ||
      (n && fin - inicio + 1 != n)) {
    stop("El bloque no tiene ordinales globales validos.", call. = FALSE)
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

.absorber_distintos <- function(acumulador, bloque) {
  x <- bloque$valores
  aplica <- !is.na(bloque$aplicable) & bloque$aplicable
  if (!acumulador$incluir_ausentes) aplica <- aplica & !is.na(x)
  indices <- which(aplica)
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
  inicio <- bloque$ordinal_inicio
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
    ordinal <- inicio + indices[[i]] - 1
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
  datos <- datos[aplica, , drop = FALSE]
  if (!nrow(datos)) return(acumulador)
  estado <- acumulador$estado_familia
  inicio <- bloque$ordinal_inicio
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
    estado$primeros <- c(estado$primeros, inicio + i - 1)
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
  bloque <- .registrar_intervalo_bloque(acumulador, .extraer_bloque(bloque))
  acumulador <- switch(
    acumulador$familia,
    conteos = .absorber_conteos(acumulador, bloque),
    cuantitativos = .absorber_cuantitativos(acumulador, bloque),
    longitudes = .absorber_longitudes(acumulador, bloque),
    distintos = .absorber_distintos(acumulador, bloque),
    filas_distintos = .absorber_filas_distintos(acumulador, bloque),
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
    filas_distintos = acumulador$estado_familia,
    generica = acumulador$estado_familia
  )
  if (identical(acumulador$familia, "distintos")) {
    acumulador <- .fusionar_distintos(acumulador, otro)
  }
  if (identical(acumulador$familia, "filas_distintos")) {
    acumulador <- .fusionar_filas_distintos(acumulador, otro)
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
      data.frame(
        representante = estado$representantes[orden],
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
    list(n = estado$n)
  )
}

.sobre_acumulador <- function(acumulador) {
  exacto <- identical(acumulador$estado, "iniciado") ||
    identical(acumulador$estado, "finalizado")
  estado <- if (identical(acumulador$estado, "no_disponible")) {
    "no_disponible"
  } else if (identical(acumulador$estado, "truncado")) {
    if (identical(acumulador$familia, "distintos")) "cota" else "no_disponible"
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
      if (identical(estado, "cota")) {
        paste0("mapa_distintos_truncado:", acumulador$estado_familia$causa_tope)
      } else if (identical(estado, "no_disponible")) {
        "resultado_no_disponible"
      } else NA_character_
    } else acumulador$fallo,
    como_resolverlo = if (identical(estado, "cota")) {
      "Aumentar el tope o habilitar un derrame exacto en una etapa posterior."
    } else if (identical(estado, "no_disponible")) {
      "Revisar el tipo y la igualdad declarada para la familia."
    } else NA_character_,
    cota = cota,
    tope = list(
      entradas = acumulador$max_entradas,
      bytes = acumulador$max_bytes,
      nombre = if (identical(acumulador$familia, "distintos")) {
        "E_distintos/P_estado"
      } else "P_estado"
    ),
    almacenamiento = "memoria",
    derrame = NULL,
    alcance = list(
      filas = acumulador$ultimo_ordinal,
      valores = acumulador$estado_familia$n_validos %||% NA_real_,
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
                                     aplicable = NULL, vigilante = NULL) {
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
    incluir_ausentes = incluir_ausentes
  )
  if (identical(familia, "cuantitativos") &&
      "contar_signos" %in% names(configuracion)) {
    acumulador$estado_familia$contar_signos <-
      isTRUE(configuracion$contar_signos)
  }
  indices <- .particionar_bloques(length(x), k = k, tamano = tamano)
  for (i in seq_along(indices)) {
    bloque <- indices[[i]]
    posiciones <- bloque$valores
    bloque$valores <- x[posiciones]
    if (!is.null(aplicable)) bloque$aplicable <- aplicable[posiciones]
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
    evento_id = character(), tipo_evento = character(), motivo = character(),
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
  en_presion <- is.finite(vigilante$tope_bytes) &&
    (bytes + bytes_resultado > vigilante$tope_bytes)
  evento_id <- paste0(vigilante$corrida_id, "-", nrow(vigilante$eventos) + 1L)
  fila <- data.frame(
    corrida_id = vigilante$corrida_id, fase = as.character(fase),
    familia = as.character(familia), bloque_id = as.integer(bloque_id),
    bytes_retenidos = bytes, bytes_resultado = bytes_resultado,
    gc = TRUE, lectura_proceso = NA_real_, factor_pico = NA_real_,
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
