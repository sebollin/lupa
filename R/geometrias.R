.sf_disponible <- function() {
  requireNamespace("sf", quietly = TRUE)
}

.metricas_geometria_vacias <- function() {
  list(
    aplica = FALSE,
    sf_evaluado = NA,
    crs_declarado = NA_character_,
    tipo_geometria = NA_character_,
    tipos_geometria = character(),
    familias_geometria = character(),
    tipos_geometria_mixtos = NA,
    dimension_geometria = NA_character_,
    dimensiones_no_evaluadas = NA_character_,
    dimensiones_omitidas = character(),
    n_geometrias_vacias = NA_integer_,
    n_geometrias_invalidas = NA_integer_,
    validez_criterio = NA_character_,
    n_fuera_de_dominio = NA_integer_,
    n_bbox_evaluados = NA_integer_,
    bbox_xmin = NA_real_,
    bbox_xmax = NA_real_,
    bbox_ymin = NA_real_,
    bbox_ymax = NA_real_,
    indices_vacias = integer(),
    indices_invalidas = integer(),
    indices_fuera_de_dominio = integer(),
    validez_evaluada = NA,
    crs_geografico = NA,
    dominio_evaluado = NA,
    n_dominio_evaluados = NA_integer_,
    motivo_validez = NA_character_,
    motivo_dominio = NA_character_
  )
}

.familia_geometria <- function(tipo) {
  switch(
    tipo,
    MULTIPOINT = "POINT",
    MULTILINESTRING = "LINESTRING",
    MULTIPOLYGON = "POLYGON",
    tipo
  )
}

.dimension_sfg <- function(x) {
  dimension <- intersect(c("XYZM", "XYZ", "XYM", "XY"), class(x))
  if (length(dimension)) dimension[[1L]] else NA_character_
}

.dimensiones_omitidas <- function(dimensiones) {
  omitidas <- character()
  if (any(grepl("Z", dimensiones, fixed = TRUE))) omitidas <- c(omitidas, "Z")
  if (any(grepl("M", dimensiones, fixed = TRUE))) omitidas <- c(omitidas, "M")
  omitidas
}

.coordenadas_sfg <- function(x) {
  if (is.matrix(x)) {
    if (!nrow(x) || ncol(x) < 2L) {
      return(matrix(numeric(), nrow = 0L, ncol = 2L))
    }
    return(matrix(as.numeric(x[, seq_len(2L), drop = FALSE]), ncol = 2L))
  }
  if (is.numeric(x)) {
    if (length(x) < 2L) {
      return(matrix(numeric(), nrow = 0L, ncol = 2L))
    }
    return(matrix(as.numeric(x[seq_len(2L)]), nrow = 1L))
  }
  if (is.list(x)) {
    partes <- lapply(x, .coordenadas_sfg)
    partes <- Filter(function(y) nrow(y) > 0L, partes)
    if (!length(partes)) {
      return(matrix(numeric(), nrow = 0L, ncol = 2L))
    }
    return(do.call(rbind, partes))
  }
  matrix(numeric(), nrow = 0L, ncol = 2L)
}

.etiqueta_crs <- function(crs) {
  entrada <- tryCatch(as.character(crs$input[[1L]]), error = function(e) NA_character_)
  if (length(entrada) != 1L || is.na(entrada) || !nzchar(entrada)) {
    entrada <- NA_character_
  }
  if (!is.na(entrada) && grepl("^EPSG:[0-9]+$", entrada)) {
    return(sub("^EPSG:", "", entrada))
  }
  if (!is.na(entrada) && !grepl("^\\+", entrada)) return(entrada)
  wkt <- tryCatch(as.character(crs$wkt[[1L]]), error = function(e) NA_character_)
  if (length(wkt) == 1L && !is.na(wkt) && nzchar(wkt)) {
    nombre <- sub('^[A-Z]+CRS\\["([^"]+)".*$', "\\1", wkt)
    if (!identical(nombre, wkt) && nzchar(nombre)) return(nombre)
  }
  entrada
}

.coordenadas_fuera_longlat <- function(coordenadas) {
  if (!nrow(coordenadas)) return(FALSE)
  any(
    !is.finite(coordenadas[, 1L]) |
      !is.finite(coordenadas[, 2L]) |
      coordenadas[, 1L] < -180 | coordenadas[, 1L] > 180 |
      coordenadas[, 2L] < -90 | coordenadas[, 2L] > 90
  )
}

.evaluar_dominio_geometria <- function(x, crs, vacias) {
  if (length(vacias) != length(x) || anyNA(vacias)) {
    return(list(
      evaluado = FALSE, fuera = logical(), n_evaluados = NA_integer_,
      motivo = "No se pudo identificar el universo de geometrias no vacias."
    ))
  }
  coordenadas <- lapply(unclass(x), .coordenadas_sfg)
  evaluables <- which(!vacias)
  fuera <- logical(length(x))
  es_longlat <- tryCatch(
    suppressWarnings(sf::st_is_longlat(crs)),
    error = function(e) NA
  )
  if (isTRUE(es_longlat)) {
    fuera[evaluables] <- vapply(
      coordenadas[evaluables], .coordenadas_fuera_longlat, logical(1L)
    )
    return(list(
      evaluado = TRUE, fuera = fuera,
      n_evaluados = as.integer(length(evaluables)), motivo = NA_character_
    ))
  }

  for (i in evaluables) {
    # Evita entregar a PROJ magnitudes que no representan una posicion
    # terrestre y que, en algunas versiones, disparan iteraciones muy costosas.
    if (any(!is.finite(coordenadas[[i]])) ||
        any(abs(coordenadas[[i]]) > 1e15)) {
      fuera[[i]] <- TRUE
      next
    }
    transformada <- tryCatch(
      suppressWarnings(sf::st_transform(
        sf::st_sfc(x[[i]], crs = crs), 4326, partial = TRUE
      )),
      error = function(e) e
    )
    if (inherits(transformada, "error")) {
      return(list(
        evaluado = FALSE, fuera = logical(), n_evaluados = NA_integer_,
        motivo = conditionMessage(transformada)
      ))
    }
    coordenadas_transformadas <- .coordenadas_sfg(transformada[[1L]])
    fuera[[i]] <- !nrow(coordenadas_transformadas) ||
      .coordenadas_fuera_longlat(coordenadas_transformadas)
  }
  list(
    evaluado = TRUE, fuera = fuera,
    n_evaluados = as.integer(length(evaluables)), motivo = NA_character_
  )
}

.perfilar_geometria <- function(x) {
  salida <- .metricas_geometria_vacias()
  if (!inherits(x, "sfc")) return(salida)
  salida$aplica <- TRUE
  salida$sf_evaluado <- .sf_disponible()
  if (!salida$sf_evaluado) return(salida)

  crs <- tryCatch(sf::st_crs(x), error = function(e) NA)
  tiene_crs <- !isTRUE(is.na(crs))
  if (tiene_crs) {
    salida$crs_declarado <- .etiqueta_crs(crs)
    salida$crs_geografico <- tryCatch(
      suppressWarnings(sf::st_is_longlat(crs)), error = function(e) NA
    )
  }

  tipos <- tryCatch(
    as.character(sf::st_geometry_type(x, by_geometry = TRUE)),
    error = function(e) character()
  )
  salida$tipos_geometria <- unique(tipos[!is.na(tipos) & nzchar(tipos)])
  if (length(salida$tipos_geometria)) {
    salida$tipo_geometria <- paste(salida$tipos_geometria, collapse = ", ")
    salida$familias_geometria <- unique(vapply(
      salida$tipos_geometria, .familia_geometria, character(1L)
    ))
    salida$tipos_geometria_mixtos <-
      length(salida$familias_geometria) > 1L ||
      "GEOMETRYCOLLECTION" %in% salida$tipos_geometria
  }

  dimensiones <- unique(vapply(unclass(x), .dimension_sfg, character(1L)))
  dimensiones <- dimensiones[!is.na(dimensiones) & nzchar(dimensiones)]
  if (length(dimensiones)) {
    salida$dimension_geometria <- paste(dimensiones, collapse = ", ")
    salida$dimensiones_omitidas <- .dimensiones_omitidas(dimensiones)
    if (length(salida$dimensiones_omitidas)) {
      salida$dimensiones_no_evaluadas <- paste(
        salida$dimensiones_omitidas, collapse = ", "
      )
    }
  }

  vacias <- tryCatch(sf::st_is_empty(x), error = function(e) NULL)
  if (!is.null(vacias) && length(vacias) == length(x) && !anyNA(vacias)) {
    salida$n_geometrias_vacias <- as.integer(sum(vacias))
    salida$indices_vacias <- as.integer(which(vacias))
  } else {
    vacias <- rep(NA, length(x))
  }

  # GEOS recibe coordenadas sin CRS para que la disponibilidad de s2 no cambie
  # el resultado. El criterio aplicado se publica y los CRS geograficos se
  # interpretan de forma cauta al construir el hallazgo.
  salida$validez_criterio <- "planar"
  validas <- tryCatch(
    suppressWarnings(sf::st_is_valid(
      suppressWarnings(sf::st_set_crs(x, NA)), NA_on_exception = TRUE
    )),
    error = function(e) e
  )
  salida$validez_evaluada <- !inherits(validas, "error") &&
    length(validas) == length(x) && !anyNA(validas)
  if (isTRUE(salida$validez_evaluada)) {
    salida$n_geometrias_invalidas <- as.integer(sum(!validas))
    salida$indices_invalidas <- as.integer(which(!validas))
  } else {
    salida$motivo_validez <- if (inherits(validas, "error")) {
      conditionMessage(validas)
    } else {
      "st_is_valid() no devolvio un resultado para todas las geometrias."
    }
  }

  bbox <- tryCatch(suppressWarnings(sf::st_bbox(x)), error = function(e) NULL)
  if (!is.null(bbox) && length(bbox) == 4L) {
    bbox <- as.numeric(bbox)
    bbox[!is.finite(bbox)] <- NA_real_
    if (!anyNA(vacias)) {
      salida$n_bbox_evaluados <- as.integer(sum(!vacias))
    }
    salida$bbox_xmin <- bbox[[1L]]
    salida$bbox_ymin <- bbox[[2L]]
    salida$bbox_xmax <- bbox[[3L]]
    salida$bbox_ymax <- bbox[[4L]]
  }

  if (tiene_crs) {
    dominio <- .evaluar_dominio_geometria(x, crs, vacias)
    salida$dominio_evaluado <- dominio$evaluado
    if (isTRUE(dominio$evaluado)) {
      salida$n_dominio_evaluados <- dominio$n_evaluados
      salida$n_fuera_de_dominio <- as.integer(sum(dominio$fuera))
      salida$indices_fuera_de_dominio <- as.integer(which(dominio$fuera))
    } else {
      salida$motivo_dominio <- dominio$motivo
    }
  } else {
    salida$dominio_evaluado <- FALSE
    salida$motivo_dominio <- "La columna no declara un CRS."
  }
  salida
}
