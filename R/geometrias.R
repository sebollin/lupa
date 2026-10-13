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
    n_validez_evaluados = NA_integer_,
    validez_criterio = NA_character_,
    validez_preprocesamiento = NA_character_,
    n_fuera_de_dominio = NA_integer_,
    n_bbox_evaluados = NA_integer_,
    bbox_alcance = NA_character_,
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
    motivo_dominio = NA_character_,
    representacion_geometria = NA_character_,
    geometria_convertida = NA,
    motivo_representacion = NA_character_,
    n_geometrias = NA_integer_,
    n_geometrias_analizadas = NA_integer_,
    n_vertices_analizados = NA_real_,
    geometrias_recortadas = NA,
    motivo_recorte = NA_character_,
    n_validez_no_evaluados = NA_integer_,
    indices_validez_no_evaluados = integer(),
    n_dominio_no_evaluados = NA_integer_,
    indices_dominio_no_evaluados = integer(),
    # Que posiciones se midieron, ademas de cuantas. Sin esto, el recorte al
    # universo no podia saber que filas del universo nunca llegaron a analizarse
    # -las que el presupuesto dejo afuera, los `NA` de una columna de texto, los
    # valores que no convirtieron- y las publicaba como evaluadas.
    indices_presentes = integer(),
    indices_geometrias = integer(),
    n_geometrias_medidas = NA_integer_,
    indices_analizados = integer(),
    n_valores_no_convertidos = NA_integer_,
    motivo_conversion = NA_character_
  )
}

# Presupuesto del analisis geometrico. El resto del paquete tiene techo en todos
# lados -`muestra`, `max_patrones`, `max_columnas_dependencias`- y la geometria
# era el unico analisis sin ninguno, justo el que escala con vertices y no con
# filas. Los topes se pueden subir con opciones porque el costo lo paga quien
# los sube.
.max_geometrias_analizadas <- function() {
  .entero_positivo_opcion("lupa.max_geometrias", 100000L)
}

.max_vertices_analizados <- function() {
  .entero_positivo_opcion("lupa.max_vertices", 5e6)
}

.entero_positivo_opcion <- function(nombre, predeterminado) {
  valor <- getOption(nombre, predeterminado)
  if (!is.numeric(valor) || length(valor) != 1L || is.na(valor) || valor < 1) {
    valor <- predeterminado
  }
  floor(as.numeric(valor))
}

# Cuenta vertices sin materializar coordenadas: recorre la estructura de anillos
# y suma filas. Es O(anillos), no O(vertices), asi que medir el presupuesto no
# cuesta lo que el presupuesto quiere evitar.
.n_vertices_sfg <- function(x) {
  if (is.matrix(x)) return(as.numeric(nrow(x)))
  if (is.numeric(x)) return(if (length(x) >= 2L) 1 else 0)
  if (is.list(x) && length(x)) {
    return(sum(vapply(x, .n_vertices_sfg, numeric(1L))))
  }
  0
}

# Elige, entre `posiciones`, las que entran en el presupuesto: primero el tope de
# geometrias por muestreo uniforme y despues el de vertices. `forzar_uno` analiza
# una geometria aunque ella sola exceda el tope de vertices -el analisis no se
# queda en cero-; con `FALSE`, lo que no entra no se analiza.
.seleccionar_presupuesto <- function(x, posiciones, max_geometrias,
                                     max_vertices, forzar_uno = TRUE) {
  n <- length(posiciones)
  if (!n) return(list(indices = integer(), n_vertices = 0, recortado = FALSE))
  if (is.finite(max_geometrias) && max_geometrias < 1) {
    return(list(indices = integer(), n_vertices = 0, recortado = TRUE))
  }
  indices <- posiciones
  recorte_geometrias <- is.finite(max_geometrias) && n > max_geometrias
  if (recorte_geometrias) {
    indices <- posiciones[unique(as.integer(round(
      seq.int(1, n, length.out = as.integer(max_geometrias))
    )))]
  }
  vertices <- vapply(unclass(x)[indices], .n_vertices_sfg, numeric(1L))
  total_vertices <- sum(vertices)
  recorte_vertices <- is.finite(max_vertices) && total_vertices > max_vertices
  if (recorte_vertices) {
    objetivo <- max(1L, as.integer(floor(
      length(indices) * max(max_vertices, 0) / total_vertices
    )))
    seleccion <- unique(as.integer(round(
      seq.int(1, length(indices), length.out = objetivo)
    )))
    indices <- indices[seleccion]
    vertices <- vertices[seleccion]
    dentro <- which(cumsum(vertices) <= max_vertices)
    if (!length(dentro) && forzar_uno) dentro <- 1L
    indices <- indices[dentro]
    vertices <- vertices[dentro]
    total_vertices <- sum(vertices)
  }
  list(
    indices = as.integer(indices), n_vertices = total_vertices,
    recortado = recorte_geometrias || recorte_vertices
  )
}

# Con un universo aplicable declarado (`prioridad`), el presupuesto se gasta
# primero en el universo -con el mismo muestreo uniforme si el universo solo ya
# excede el tope- y lo que sobra, si sobra, va al resto de la columna. Antes se
# repartia entre todas las filas: con los topes por omision, 1.100 poligonos de
# 5.000 vertices y un universo de 120 que entraba entero en el tope, se
# analizaban 980 filas de afuera y 20 del universo, y 50 invalidas del universo
# salian "0 de 20". Lo de afuera no alimenta ningun conteo del universo; solo
# el trabajo publicado sobre la columna entera. Sin universo, el camino es el de
# siempre.
.presupuesto_geometrias <- function(x, max_geometrias, max_vertices,
                                    prioridad = NULL) {
  n <- length(x)
  indices <- seq_len(n)
  sin_recorte <- list(
    indices = indices, recortado = FALSE, motivo = NA_character_,
    n_total = n, n_vertices = NA_real_
  )
  if (!n) return(sin_recorte)

  prioritario <- !is.null(prioridad) && length(prioridad) == n &&
    !anyNA(prioridad) && any(prioridad) && !all(prioridad)
  if (!prioritario) {
    seleccion <- .seleccionar_presupuesto(
      x, indices, max_geometrias, max_vertices
    )
  } else {
    universo <- .seleccionar_presupuesto(
      x, which(prioridad), max_geometrias, max_vertices
    )
    resto <- if (universo$recortado) {
      list(indices = integer(), n_vertices = 0, recortado = TRUE)
    } else {
      .seleccionar_presupuesto(
        x, which(!prioridad), max_geometrias - length(universo$indices),
        max_vertices - universo$n_vertices, forzar_uno = FALSE
      )
    }
    seleccion <- list(
      indices = .sort_por_bytes(c(universo$indices, resto$indices)),
      n_vertices = universo$n_vertices + resto$n_vertices,
      recortado = universo$recortado || resto$recortado
    )
  }
  if (!seleccion$recortado) {
    sin_recorte$n_vertices <- seleccion$n_vertices
    return(sin_recorte)
  }
  list(
    indices = seleccion$indices, recortado = TRUE, n_total = n,
    n_vertices = seleccion$n_vertices,
    motivo = paste0(
      "El presupuesto geometrico recorto el analisis por geometria: se ",
      "evaluaron ", length(seleccion$indices), " de ", n, " geometrias, con ",
      format(seleccion$n_vertices, scientific = FALSE), " vertices. Los topes son ",
      "max_geometrias=", format(max_geometrias, scientific = FALSE),
      " y max_vertices=", format(max_vertices, scientific = FALSE),
      "; se suben con options(lupa.max_geometrias=) y options(lupa.max_vertices=).",
      if (prioritario) {
        paste0(
          " El presupuesto se aplico primero a las ", sum(prioridad),
          " geometrias del universo aplicable declarado y lo que sobro, al resto."
        )
      } else ""
    )
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
  if (any(grepl("Z", dimensiones, fixed = TRUE, useBytes = TRUE))) omitidas <- c(omitidas, "Z")
  if (any(grepl("M", dimensiones, fixed = TRUE, useBytes = TRUE))) omitidas <- c(omitidas, "M")
  omitidas
}

# Un componente vacio no es una coordenada. `sf` escribe `POINT EMPTY` como el
# par `NaN NaN` -dentro de un MULTIPOINT es una fila `NaN NaN`, dentro de una
# coleccion un punto `NaN NaN`- y contarlo como coordenada no finita acusaba de
# fuera de dominio a `GEOMETRYCOLLECTION (POINT EMPTY, POINT (10 10))`, cuya
# unica coordenada esta dentro. Solo se descarta el par entero ausente: un punto
# con UNA coordenada no finita sigue siendo una coordenada invalida.
.coordenadas_sfg <- function(x) {
  if (is.matrix(x)) {
    if (!nrow(x) || ncol(x) < 2L) {
      return(matrix(numeric(), nrow = 0L, ncol = 2L))
    }
    xy <- matrix(as.numeric(x[, seq_len(2L), drop = FALSE]), ncol = 2L)
    vacio <- is.na(xy[, 1L]) & is.na(xy[, 2L])
    return(xy[!vacio, , drop = FALSE])
  }
  if (is.numeric(x)) {
    if (length(x) < 2L || all(is.na(x[seq_len(2L)]))) {
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

.bbox_area_uso_crs <- function(crs) {
  wkt <- tryCatch(as.character(crs$wkt[[1L]]), error = function(e) NA_character_)
  if (length(wkt) != 1L || is.na(wkt) || !nzchar(wkt)) {
    return(list(
      evaluada = FALSE, bbox = numeric(), global = NA,
      motivo = "El WKT del CRS no esta disponible para extraer su BBOX de uso."
    ))
  }

  # Recorre la estructura delimitada del WKT en lugar de depender de su
  # espaciado o de una expresion regular. Los textos entre comillas se ignoran.
  caracteres <- strsplit(wkt, "", fixed = TRUE)[[1L]]
  n <- length(caracteres)
  i <- 1L
  en_comillas <- FALSE
  while (i <= n) {
    if (caracteres[[i]] == "\"") {
      if (en_comillas && i < n && caracteres[[i + 1L]] == "\"") {
        i <- i + 2L
        next
      }
      en_comillas <- !en_comillas
      i <- i + 1L
      next
    }
    es_bbox <- !en_comillas && i + 3L <= n &&
      .mayusculas_ascii(paste0(caracteres[i:(i + 3L)], collapse = "")) == "BBOX"
    if (!es_bbox) {
      i <- i + 1L
      next
    }
    caracteres_identificador <- c(letters, LETTERS, as.character(0:9), "_")
    anterior_valido <- i == 1L ||
      !caracteres[[i - 1L]] %in% caracteres_identificador
    j <- i + 4L
    while (j <= n && caracteres[[j]] %in% c(" ", "\t", "\r", "\n", "\f")) {
      j <- j + 1L
    }
    if (!anterior_valido || j > n || !caracteres[[j]] %in% c("[", "(")) {
      i <- i + 1L
      next
    }

    apertura <- caracteres[[j]]
    cierre <- if (apertura == "[") "]" else ")"
    profundidad <- 1L
    k <- j + 1L
    en_texto <- FALSE
    while (k <= n && profundidad > 0L) {
      actual <- caracteres[[k]]
      if (actual == "\"") {
        if (en_texto && k < n && caracteres[[k + 1L]] == "\"") {
          k <- k + 2L
          next
        }
        en_texto <- !en_texto
      } else if (!en_texto && actual == apertura) {
        profundidad <- profundidad + 1L
      } else if (!en_texto && actual == cierre) {
        profundidad <- profundidad - 1L
      }
      k <- k + 1L
    }
    if (profundidad != 0L) break

    contenido <- paste0(caracteres[(j + 1L):(k - 2L)], collapse = "")
    partes <- trimws(strsplit(contenido, ",", fixed = TRUE)[[1L]])
    valores <- suppressWarnings(as.numeric(partes))
    if (length(valores) == 4L && all(is.finite(valores)) &&
        valores[[1L]] >= -90 && valores[[1L]] <= 90 &&
        valores[[3L]] >= -90 && valores[[3L]] <= 90 &&
        valores[[1L]] <= valores[[3L]] &&
        valores[[2L]] >= -180 && valores[[2L]] <= 180 &&
        valores[[4L]] >= -180 && valores[[4L]] <= 180) {
      bbox <- c(
        xmin = valores[[2L]], ymin = valores[[1L]],
        xmax = valores[[4L]], ymax = valores[[3L]]
      )
      global <- bbox[["ymin"]] <= -90 && bbox[["ymax"]] >= 90 &&
        bbox[["xmin"]] <= -180 && bbox[["xmax"]] >= 180
      return(list(
        evaluada = TRUE, bbox = bbox, global = global,
        motivo = NA_character_
      ))
    }
    break
  }
  list(
    evaluada = FALSE, bbox = numeric(), global = NA,
    motivo = paste0(
      "El WKT del CRS no contiene una BBOX de area de uso extraible; ",
      "no se supuso una cobertura mundial."
    )
  )
}

.coordenadas_fuera_bbox <- function(coordenadas, bbox) {
  if (!nrow(coordenadas)) return(FALSE)
  longitud <- coordenadas[, 1L]
  latitud <- coordenadas[, 2L]
  fuera_longitud <- if (bbox[["xmin"]] <= bbox[["xmax"]]) {
    longitud < bbox[["xmin"]] | longitud > bbox[["xmax"]]
  } else {
    longitud > bbox[["xmax"]] & longitud < bbox[["xmin"]]
  }
  any(
    !is.finite(longitud) | !is.finite(latitud) | fuera_longitud |
      latitud < bbox[["ymin"]] | latitud > bbox[["ymax"]]
  )
}

.dominio_no_evaluado <- function(motivo) {
  list(
    evaluado = FALSE, fuera = logical(), n_evaluados = NA_integer_,
    no_evaluados = integer(), n_no_evaluados = NA_integer_, motivo = motivo
  )
}

# Entrega a PROJ una sola coleccion en vez de una geometria por vez. Rearmar el
# pipeline de PROJ en cada iteracion costaba entre 105 y 228 veces mas que la
# llamada unica, medido sobre 100, 1.000 y 5.000 filas.
.transformar_a_longlat <- function(x, indices, crs) {
  tryCatch(
    suppressWarnings(sf::st_transform(
      sf::st_sfc(unclass(x)[indices], crs = crs), 4326, partial = TRUE
    )),
    error = function(e) e
  )
}

# Un CRS geografico no siempre mide en grados desde Greenwich: MGI y S-JTSK
# cuentan la longitud desde Ferro, Batavia desde Jakarta y la NTF desde Paris y
# en gradianes. La BBOX del area de uso viene en grados Greenwich, asi que
# compararle esos numeros crudos acusaba de fuera de dominio a la columna sana
# entera -el centro del area de uso de EPSG:4805 es 33.95 en su propio marco-.
# Se le pregunta a PROJ como lleva tres puntos al marco WGS84: si la ida es la
# identidad, el CRS es el de siempre y nada cambia; si no, de la misma respuesta
# salen la escala de cada eje -para el rango en las unidades del propio CRS- y
# la necesidad de transformar antes de comparar con la BBOX. Si PROJ no
# responde, se conserva la comparacion cruda de siempre.
.marco_longlat <- function(crs) {
  estandar <- list(estandar = TRUE, limite_x = 180, limite_y = 90)
  puntos <- tryCatch(
    suppressWarnings(sf::st_transform(
      sf::st_sfc(
        sf::st_point(c(0, 0)), sf::st_point(c(1, 0)), sf::st_point(c(0, 1)),
        crs = crs
      ),
      4326
    )),
    error = function(e) NULL
  )
  if (is.null(puntos) || length(puntos) != 3L) return(estandar)
  xy <- tryCatch(
    do.call(rbind, lapply(unclass(puntos), function(p) as.numeric(p)[1:2])),
    error = function(e) NULL
  )
  if (is.null(xy) || !all(dim(xy) == c(3L, 2L)) || any(!is.finite(xy))) {
    return(estandar)
  }
  escala_x <- xy[2L, 1L] - xy[1L, 1L]
  escala_y <- xy[3L, 2L] - xy[1L, 2L]
  if (!isTRUE(escala_x > 0) || !isTRUE(escala_y > 0)) return(estandar)
  tolerancia <- 1e-9
  if (all(abs(xy[1L, ]) < tolerancia) && abs(escala_x - 1) < tolerancia &&
      abs(escala_y - 1) < tolerancia) {
    return(estandar)
  }
  list(estandar = FALSE, limite_x = 180 / escala_x, limite_y = 90 / escala_y)
}

.evaluar_dominio_geometria <- function(x, crs, vacias, srids = NULL) {
  if (length(vacias) != length(x) || anyNA(vacias)) {
    return(.dominio_no_evaluado(
      "No se pudo identificar el universo de geometrias no vacias."
    ))
  }
  srids_validos <- if (is.null(srids) || length(srids) != length(x)) {
    integer()
  } else {
    suppressWarnings(as.integer(srids))
  }
  if (length(srids_validos) == length(x) &&
      length(unique(srids_validos)) > 1L) {
    fuera <- logical(length(x))
    evaluados <- 0L
    no_evaluados <- integer()
    motivos <- character()
    for (srid in unique(srids_validos)) {
      posiciones <- which(
        if (is.na(srid)) is.na(srids_validos) else srids_validos == srid
      )
      grupo_vacias <- vacias[posiciones]
      grupo_crs <- if (is.na(srid) || srid == 0L) {
        NA
      } else {
        tryCatch(sf::st_crs(srid), error = function(e) e)
      }
      if (inherits(grupo_crs, "error") || isTRUE(is.na(grupo_crs))) {
        presentes <- posiciones[which(!grupo_vacias)]
        no_evaluados <- c(no_evaluados, presentes)
        motivo <- if (inherits(grupo_crs, "error")) {
          conditionMessage(grupo_crs)
        } else {
          "el valor no declara un SRID utilizable"
        }
        motivos <- c(motivos, paste0("SRID ", srid, ": ", motivo))
        next
      }
      grupo <- sf::st_sfc(unclass(x)[posiciones], crs = grupo_crs)
      resultado <- .evaluar_dominio_geometria(
        grupo, grupo_crs, grupo_vacias
      )
      if (isTRUE(resultado$evaluado)) {
        fuera[posiciones] <- resultado$fuera
        evaluados <- evaluados + resultado$n_evaluados
        no_evaluados <- c(
          no_evaluados, posiciones[resultado$no_evaluados]
        )
        if (length(resultado$no_evaluados)) {
          motivos <- c(motivos, paste0("SRID ", srid, ": ", resultado$motivo))
        }
      } else {
        presentes <- posiciones[which(!grupo_vacias)]
        no_evaluados <- c(no_evaluados, presentes)
        motivos <- c(motivos, paste0("SRID ", srid, ": ", resultado$motivo))
      }
    }
    if (!evaluados && length(which(!vacias))) {
      return(.dominio_no_evaluado(paste(
        "No se pudo evaluar el dominio para ninguna geometria con SRID mixto:",
        paste(motivos, collapse = "; ")
      )))
    }
    motivo <- if (length(no_evaluados)) {
      paste0(
        "El dominio se evaluo por SRID para ", evaluados,
        " geometrias; quedaron sin evaluar ", length(no_evaluados),
        ". ", paste(motivos, collapse = "; ")
      )
    } else {
      NA_character_
    }
    return(list(
      evaluado = TRUE, fuera = fuera, n_evaluados = as.integer(evaluados),
      no_evaluados = as.integer(no_evaluados),
      n_no_evaluados = as.integer(length(no_evaluados)), motivo = motivo
    ))
  }
  area_uso <- .bbox_area_uso_crs(crs)
  if (!isTRUE(area_uso$evaluada)) {
    return(.dominio_no_evaluado(area_uso$motivo))
  }
  dimensiones <- vapply(unclass(x), .dimension_sfg, character(1L))
  x_transformacion <- x
  if (any(!is.na(dimensiones) & dimensiones != "XY")) {
    x_transformacion <- tryCatch(
      suppressWarnings(sf::st_zm(x, drop = TRUE, what = "ZM")),
      error = function(e) e
    )
    if (inherits(x_transformacion, "error")) {
      return(.dominio_no_evaluado(paste0(
        "No se pudieron retirar las dimensiones Z/M antes de transformar al",
        " sistema geografico: ", conditionMessage(x_transformacion)
      )))
    }
  }
  coordenadas <- lapply(unclass(x), .coordenadas_sfg)
  evaluables <- which(!vacias)
  fuera <- logical(length(x))
  es_longlat <- tryCatch(
    suppressWarnings(sf::st_is_longlat(crs)),
    error = function(e) NA
  )
  marco <- if (isTRUE(es_longlat)) .marco_longlat(crs) else NULL
  if (isTRUE(es_longlat) && isTRUE(marco$estandar)) {
    fuera[evaluables] <- vapply(coordenadas[evaluables], function(y) {
      .coordenadas_fuera_longlat(y) ||
        (!isTRUE(area_uso$global) && .coordenadas_fuera_bbox(y, area_uso$bbox))
    }, logical(1L))
    return(list(
      evaluado = TRUE, fuera = fuera,
      n_evaluados = as.integer(length(evaluables)),
      no_evaluados = integer(), n_no_evaluados = 0L, motivo = NA_character_
    ))
  }

  # La guarda se evalua antes de transformar. Evita entregar a PROJ magnitudes
  # que no representan una posicion terrestre y que, en algunas versiones,
  # disparan iteraciones muy costosas; y, evaluada aparte, deja el resto de la
  # columna disponible para una unica llamada vectorizada.
  # En un CRS geografico de otro meridiano o de otra unidad, la guarda es el
  # rango en SUS unidades, y va antes de transformar porque PROJ envuelve la
  # longitud: 250 gradianes de Paris salen como -132.66 grados y el error se
  # borraria en la ida.
  guardadas <- vapply(coordenadas[evaluables], function(y) {
    if (any(!is.finite(y))) return(TRUE)
    if (!is.null(marco)) {
      return(any(abs(y[, 1L]) > marco$limite_x | abs(y[, 2L]) > marco$limite_y))
    }
    any(abs(y) > 1e15)
  }, logical(1L))
  fuera[evaluables[guardadas]] <- TRUE
  transformables <- evaluables[!guardadas]
  fuera_del_area <- function(y) {
    !nrow(y) || .coordenadas_fuera_longlat(y) ||
      (!isTRUE(area_uso$global) && .coordenadas_fuera_bbox(y, area_uso$bbox))
  }

  no_evaluados <- integer()
  ultimo_error <- NA_character_
  if (length(transformables)) {
    transformada <- .transformar_a_longlat(
      x_transformacion, transformables, crs
    )
    if (inherits(transformada, "error")) {
      # Antes, una sola geometria intransformable descartaba la evaluacion de
      # toda la columna. Ahora se reintenta una por una y se conserva lo medido,
      # declarando cuales quedaron sin evaluar.
      ultimo_error <- conditionMessage(transformada)
      for (i in transformables) {
        una <- .transformar_a_longlat(x_transformacion, i, crs)
        if (inherits(una, "error")) {
          ultimo_error <- conditionMessage(una)
          no_evaluados <- c(no_evaluados, i)
          next
        }
        fuera[[i]] <- fuera_del_area(.coordenadas_sfg(una[[1L]]))
      }
    } else {
      fuera[transformables] <- vapply(
        lapply(unclass(transformada), .coordenadas_sfg),
        fuera_del_area, logical(1L)
      )
    }
  }

  n_evaluados <- length(evaluables) - length(no_evaluados)
  if (!n_evaluados && length(evaluables)) {
    return(.dominio_no_evaluado(ultimo_error))
  }
  motivo <- if (length(no_evaluados)) {
    paste0(
      "No se pudo transformar ", length(no_evaluados), " de ",
      length(evaluables), " geometrias al sistema geografico; el resultado ",
      "cubre las ", n_evaluados, " restantes. Ultimo error: ", ultimo_error
    )
  } else {
    NA_character_
  }
  list(
    evaluado = TRUE, fuera = fuera, n_evaluados = as.integer(n_evaluados),
    no_evaluados = as.integer(no_evaluados),
    n_no_evaluados = length(no_evaluados), motivo = motivo
  )
}

# Deteccion de geometria que no llega como `sfc`. Sin esto, una columna WKT,
# WKB o hexadecimal deja todas las metricas espaciales en NA sin declarar una
# sola fila, y `cobertura_analisis()` llega a afirmar "no aplica" sobre datos
# que si son geometricos.
.patron_wkt <- paste0(
  "^\\s*(?:SRID=[0-9]+;)?\\s*",
  "(?:POINT|LINESTRING|POLYGON|MULTIPOINT|MULTILINESTRING|MULTIPOLYGON|",
  "GEOMETRYCOLLECTION|CIRCULARSTRING|COMPOUNDCURVE|CURVEPOLYGON|MULTICURVE|",
  "MULTISURFACE|POLYHEDRALSURFACE|TRIANGLE|TIN)",
  "\\s*(?:ZM|Z|M)?\\s*(?:\\(|EMPTY)"
)

.muestra_para_reconocer <- function(x, limite = 20L) {
  n <- length(x)
  if (!n) return(integer())
  indices <- if (n <= limite) {
    seq_len(n)
  } else {
    unique(as.integer(round(seq.int(1, n, length.out = limite))))
  }
  indices
}

# La muestra se filtra por UTF-8 valido: reconocer no puede fallar sobre una
# columna con bytes rotos, que es justo donde el paquete tiene que seguir vivo.
.muestra_texto_geometria <- function(x) {
  muestra <- x[.muestra_para_reconocer(x)]
  muestra <- muestra[!is.na(muestra)]
  if (!length(muestra)) return(character())
  muestra[validUTF8(muestra)]
}

# Se reconoce por MAYORIA de la muestra, no por unanimidad. Con unanimidad, un
# solo valor corrupto dejaba de reconocer la columna -tres WKT validos y una
# basura se perfilaban como texto y `cobertura_analisis()` decia "no se
# identificaron columnas de geometria"-, y como la muestra es de 20, la respuesta
# dependia de si la basura caia en ella: con mil WKT y una basura afuera, la
# columna si se reconocia y la perdida se declaraba. Lo que no convierte lo
# declara la conversion, igual en los dos casos.
.mayoria_geometria <- function(es_geometria) {
  length(es_geometria) > 0L && mean(es_geometria) > 0.5
}

.parece_wkt <- function(x) {
  if (!is.character(x)) return(FALSE)
  muestra <- .muestra_texto_geometria(x)
  .mayoria_geometria(grepl(.regex_sin_caja(.patron_wkt), muestra, perl = TRUE))
}

.parece_wkb_hexadecimal <- function(x) {
  if (!is.character(x)) return(FALSE)
  muestra <- .muestra_texto_geometria(x)
  if (!length(muestra)) return(FALSE)
  largos <- nchar(muestra, type = "bytes")
  .mayoria_geometria(
    largos >= 18L & largos %% 2L == 0L &
      grepl("^[0-9A-Fa-f]+$", muestra, perl = TRUE) &
      substr(muestra, 1L, 2L) %in% c("00", "01")
  )
}

# Esta guarda lee solo el encabezado, no intenta validar el cuerpo recursivo de
# la geometria. Un encabezado plausible con un cuerpo corrupto todavia puede
# llegar a GDAL y fallar alli. No se duplica el parser completo de sf: ademas de
# ser mas costoso, dejaria dos implementaciones de la gramatica WKB que podrian
# divergir; esta guarda solo evita entregar bytes que ni siquiera pueden ser WKB.
.wkb_plausible <- function(valor) {
  if (!is.raw(valor) || length(valor) < 5L) return(FALSE)
  .wkb_encabezado_plausible(as.integer(valor[1L:5L]), length(valor))
}

# La misma aritmetica para el WKB crudo y para el hexadecimal: recibe los cinco
# primeros bytes como enteros y el largo total en bytes. Escrita una sola vez
# para que las dos guardas no puedan divergir.
.wkb_encabezado_plausible <- function(bytes, largo) {
  if (length(bytes) != 5L || anyNA(bytes) || is.na(largo)) return(FALSE)
  orden <- bytes[[1L]]
  if (!orden %in% c(0L, 1L)) return(FALSE)

  bytes_tipo <- bytes[2L:5L]
  potencias <- if (orden == 1L) 0:3 else 3:0
  tipo <- sum(bytes_tipo * 256 ^ potencias)

  banderas_ewkb <- c(srid = 2 ^ 29, m = 2 ^ 30, z = 2 ^ 31)
  presentes <- vapply(banderas_ewkb, function(bandera) {
    floor(tipo / bandera) %% 2 == 1
  }, logical(1L))
  tipo_sin_banderas <- tipo - sum(banderas_ewkb[presentes])
  tipo_base <- tipo_sin_banderas %% 1000
  if (!tipo_base %in% 1:7) return(FALSE)

  # El encabezado plausible no alcanza: un build de GDAL de win-builder
  # revento (segfault, no error) al recibir un WKB de encabezado valido y
  # cuerpo trunco, y un tryCatch no puede atrapar eso. El piso de largo por
  # tipo sigue siendo aritmetica pura: un punto lleva dos dobles (16 bytes;
  # con Z o M lleva mas, asi que el piso vale igual) y todo lo demas lleva al
  # menos su conteo de cuatro bytes. Ningun WKB genuino queda por debajo.
  encabezado <- if (presentes[["srid"]]) 9L else 5L
  cuerpo_minimo <- if (tipo_base == 1L) 16L else 4L
  largo >= encabezado + cuerpo_minimo
}

# La guarda del WKB hexadecimal, que no existia: el hexadecimal pasaba entero a
# sf, y un solo `""`, `"zz"` o un encabezado trunco hacia fallar la conversion
# de la columna entera, que entonces dejaba de ser geometria sin dejar rastro.
# Ahora cada valor se lee como hexadecimal par y su encabezado pasa por la misma
# aritmetica que el WKB crudo; solo lo que la supera llega a sf. Vectorizada:
# lee cinco bytes por valor, no el cuerpo.
.wkb_hexadecimal_plausible <- function(valores) {
  valores <- as.character(valores)
  largo <- nchar(valores, type = "bytes", allowNA = TRUE)
  candidato <- !is.na(valores) & !is.na(largo) & largo >= 10L &
    largo %% 2L == 0L
  if (any(candidato)) {
    candidato[candidato] <- grepl(
      "^[0-9A-Fa-f]+$", valores[candidato], perl = TRUE, useBytes = TRUE
    )
  }
  plausible <- logical(length(valores))
  if (!any(candidato)) return(plausible)
  cabeza <- substr(valores[candidato], 1L, 10L)
  bytes <- matrix(vapply(seq_len(5L), function(k) {
    strtoi(substr(cabeza, 2L * k - 1L, 2L * k), 16L)
  }, integer(length(cabeza))), ncol = 5L)
  largos <- largo[candidato] %/% 2L
  plausible[candidato] <- vapply(seq_len(nrow(bytes)), function(i) {
    .wkb_encabezado_plausible(bytes[i, ], largos[[i]])
  }, logical(1L))
  plausible
}

.srid_wkb <- function(valor) {
  if (is.character(valor)) {
    texto <- as.character(valor)
    if (length(texto) != 1L || is.na(texto) ||
        nchar(texto, type = "bytes") %% 2L != 0L ||
        !grepl("^[0-9A-Fa-f]+$", texto, perl = TRUE)) {
      return(NA_integer_)
    }
    pares <- substring(
      texto, seq.int(1L, nchar(texto, type = "bytes"), by = 2L),
      seq.int(2L, nchar(texto, type = "bytes"), by = 2L)
    )
    valor <- suppressWarnings(as.raw(strtoi(pares, base = 16L)))
  }
  if (!is.raw(valor) || length(valor) < 9L) return(NA_integer_)
  orden <- as.integer(valor[[1L]])
  if (!orden %in% c(0L, 1L)) return(NA_integer_)
  bytes_tipo <- as.integer(valor[2L:5L])
  potencias <- if (orden == 1L) 0:3 else 3:0
  tipo <- sum(bytes_tipo * 256 ^ potencias)
  if (floor(tipo / (2 ^ 29)) %% 2 != 1) return(NA_integer_)
  bytes_srid <- as.integer(valor[6L:9L])
  srid <- sum(bytes_srid * 256 ^ potencias)
  if (!is.finite(srid) || srid < 0 || srid > .Machine$integer.max) {
    return(NA_integer_)
  }
  as.integer(srid)
}

.srid_wkt <- function(valor) {
  if (length(valor) != 1L || is.na(valor)) return(NA_integer_)
  capturado <- regexec(.regex_sin_caja("^\\s*SRID=([0-9]+);"),
                       as.character(valor))
  partes <- regmatches(as.character(valor), capturado)[[1L]]
  if (length(partes) != 2L) return(NA_integer_)
  srid <- suppressWarnings(as.numeric(partes[[2L]]))
  if (!is.finite(srid) || srid < 0 || srid > .Machine$integer.max) {
    return(NA_integer_)
  }
  as.integer(srid)
}

.srids_representacion_geometrica <- function(x, representacion, indices) {
  valores <- unclass(x)[indices]
  if (identical(representacion, "WKT")) {
    return(vapply(valores, .srid_wkt, integer(1L)))
  }
  vapply(valores, .srid_wkb, integer(1L))
}

.parece_wkb_crudo <- function(x) {
  if (!is.list(x) || inherits(x, "sfc")) return(FALSE)
  if (inherits(x, "WKB")) return(TRUE)
  muestra <- unclass(x)[.muestra_para_reconocer(x)]
  muestra <- muestra[!vapply(muestra, is.null, logical(1L))]
  length(muestra) > 0L && any(vapply(muestra, .wkb_plausible, logical(1L)))
}

.representacion_geometrica <- function(x) {
  if (.parece_wkt(x)) return("WKT")
  if (.parece_wkb_hexadecimal(x)) return("WKB hexadecimal")
  if (.parece_wkb_crudo(x)) return("WKB")
  NA_character_
}

# Las posiciones con valor, con el mismo criterio con el que el perfil cuenta
# los faltantes: `is.na()`, que sobre una lista marca el `NA` suelto -y, con
# la clase `blob`, el `NULL`-. Lo ausente no es una geometria que no convirtio:
# ya se cuenta en `n_faltantes`. Un `NULL` en una lista sin clase no es un
# faltante para el perfil, asi que tampoco se calla aca: llega a la guarda, no
# la pasa y se declara como no convertido.
.posiciones_presentes <- function(x) {
  which(!is.na(x))
}

# De las posiciones dadas, las que pasan la guarda de su representacion: solo
# esas llegan a sf. Las demas no borran la medicion de la columna; se descartan
# y se declaran con su cifra.
.posiciones_convertibles <- function(x, representacion, posiciones) {
  if (!length(posiciones)) return(integer())
  valores <- unclass(x)[posiciones]
  plausibles <- if (identical(representacion, "WKT")) {
    .wkt_plausible(valores)
  } else if (identical(representacion, "WKB hexadecimal")) {
    .wkb_hexadecimal_plausible(valores)
  } else {
    vapply(valores, .wkb_plausible, logical(1L))
  }
  as.integer(posiciones[plausibles])
}

# Un WKT plausible lleva la palabra del tipo y, despues, solo numeros,
# separadores, parentesis balanceados y las etiquetas de tipo, dimension y
# `EMPTY` que la gramatica admite adentro. Es la misma doctrina de la guarda WKB:
# el GDAL de win-builder demostro que puede reventar -segfault, no error- con
# entrada corrupta, y un tryCatch no atrapa eso; el texto podrido no llega a sf,
# y se declara por aritmetica propia.
#
# La version anterior no admitia la dimension dentro de los parentesis ni una
# coleccion anidada, y lo que `ST_AsText` de PostGIS escribe -
# `GEOMETRYCOLLECTION Z (POINT Z (1 2 3))`, la variante M,
# `GEOMETRYCOLLECTION(GEOMETRYCOLLECTION(POINT(1 2)))`- salia como "WKT
# estructuralmente invalido", que es falso. Los tipos curvos y poliedricos siguen
# afuera porque el perfil no los mide; quedan declarados como no convertidos.
# Vectorizada: recibe todos los valores de una vez.
.TIPOS_WKT_MEDIDOS <- paste(
  "POINT", "LINESTRING", "POLYGON", "MULTIPOINT", "MULTILINESTRING",
  "MULTIPOLYGON", "GEOMETRYCOLLECTION", sep = "|"
)

.wkt_plausible <- function(valores) {
  textos <- as.character(valores)
  plausible <- !is.na(textos)
  plausible[plausible] <- validUTF8(textos[plausible])
  if (!any(plausible)) return(plausible)
  texto <- trimws(textos[plausible])
  forma <- paste0(
    "^(?:SRID=[0-9]+;)?\\s*(?:", .TIPOS_WKT_MEDIDOS, ")\\s*(?:ZM|Z|M)?\\s*",
    "(?:EMPTY|\\(.*\\))$"
  )
  ok <- grepl(.regex_sin_caja(forma), texto, perl = TRUE)
  # Lo que queda despues de retirar el prefijo SRID, cada etiqueta de tipo con su
  # dimension -solo si la sigue un parentesis o EMPTY- y cada EMPTY tiene que ser
  # solo numeros, separadores y parentesis.
  resto <- sub(.regex_sin_caja("^SRID=[0-9]+;"), "", texto, perl = TRUE)
  resto <- gsub(
    .regex_sin_caja(paste0(
      "(?:", .TIPOS_WKT_MEDIDOS, ")\\s*(?:ZM|Z|M)?\\s*(?=\\(|EMPTY)"
    )),
    " ", resto, perl = TRUE
  )
  resto <- gsub(.regex_sin_caja("EMPTY"), " ", resto)
  ok <- ok & grepl("^[-+0-9eE.,()[:space:]]*$", resto, perl = TRUE)
  # Parentesis balanceados de verdad, y un solo cuerpo: el primer parentesis abre
  # y solo el ultimo vuelve a profundidad cero. Contar aperturas y cierres dejaba
  # pasar `)(` y dos geometrias pegadas, `POINT (1 2) POINT (3 4)`.
  ok[ok] <- vapply(trimws(resto[ok]), function(s) {
    if (!nzchar(s)) return(TRUE)
    codigos <- utf8ToInt(s)
    profundidad <- cumsum((codigos == 40L) - (codigos == 41L))
    n <- length(profundidad)
    codigos[[1L]] == 40L && profundidad[[n]] == 0L &&
      (n == 1L || all(profundidad[-n] >= 1L))
  }, logical(1L), USE.NAMES = FALSE)
  plausible[plausible] <- ok
  plausible
}

# Convierte las posiciones dadas, que ya pasaron la guarda, y devuelve lo que
# convirtio con sus posiciones. Antes era todo o nada: un valor que sf no podia
# leer tiraba la columna entera. Ahora se intenta una sola llamada -el caso de
# siempre- y, si falla, se biseca: el costo crece con los valores que fallan, no
# con la columna, y lo que no convierte queda nombrado. La biseccion tiene techo:
# una columna entera de cuerpos corruptos pedia dos llamadas a sf por valor -dos
# mil valores, 12,7 segundos-, asi que tras `max_intentos_fallidos` llamadas
# fallidas se deja de partir: cada tramo pendiente se intenta una vez y el que
# falla se cuenta entero como no convertido, y se dice.
#
# El EWKT entra a sf sin su prefijo `SRID=n;`. `sf` rechaza una columna con
# varios SRID -"sf does not support multiple crs"- y con un valor sin SRID al
# lado de otro con SRID, mientras la misma mezcla escrita como EWKB se mide por
# SRID. El SRID de cada valor ya se lee con `.srid_wkt()` y viaja aparte; a sf se
# le declara el primero, como hace con el EWKB.
.convertir_a_sfc <- function(x, representacion, indices,
                             max_intentos_fallidos = 256L) {
  valores <- unclass(x)[indices]
  if (identical(representacion, "WKT")) {
    srids <- vapply(valores, .srid_wkt, integer(1L), USE.NAMES = FALSE)
    # A sf el WKT le llega en mayusculas ASCII. La gramatica no distingue la
    # caja, pero el lector de GDAL compara las palabras con la caja del locale:
    # bajo uno turco `point(1 2)` daba "Unsupported geometry type" y la columna
    # perdia tres de cuatro valores (ronda 31-S). Lo que llega aca ya paso la
    # guarda: palabras de la gramatica, numeros y separadores.
    textos <- .mayusculas_ascii(sub(
      .regex_sin_caja("^\\s*SRID=[0-9]+;\\s*"), "", as.character(valores),
      perl = TRUE
    ))
    crs <- sf::NA_crs_
    for (srid in unique(srids[!is.na(srids) & srids > 0L])) {
      candidato <- tryCatch(
        suppressWarnings(sf::st_crs(srid)), error = function(e) sf::NA_crs_
      )
      if (!isTRUE(is.na(candidato))) {
        crs <- candidato
        break
      }
    }
    convertir <- function(posiciones) {
      sf::st_as_sfc(textos[posiciones], crs = crs)
    }
  } else {
    convertir <- function(posiciones) {
      sf::st_as_sfc(structure(valores[posiciones], class = "WKB"), EWKB = TRUE)
    }
  }
  ultimo_error <- NA_character_
  piezas <- list()
  fallidas <- integer()
  sin_aislar <- 0L
  intentos_fallidos <- 0L
  pendientes <- list(seq_along(valores))
  while (length(pendientes)) {
    tramo <- pendientes[[1L]]
    pendientes <- pendientes[-1L]
    convertida <- tryCatch(
      suppressWarnings(convertir(tramo)), error = function(e) e
    )
    if (!inherits(convertida, "error") && inherits(convertida, "sfc") &&
        length(convertida) == length(tramo)) {
      piezas[[length(piezas) + 1L]] <- list(posiciones = tramo, sfc = convertida)
      next
    }
    ultimo_error <- if (inherits(convertida, "error")) {
      conditionMessage(convertida)
    } else {
      "la conversion no devolvio una geometria por valor"
    }
    intentos_fallidos <- intentos_fallidos + 1L
    if (length(tramo) == 1L) {
      fallidas <- c(fallidas, tramo)
      next
    }
    if (intentos_fallidos >= max_intentos_fallidos) {
      fallidas <- c(fallidas, tramo)
      sin_aislar <- sin_aislar + length(tramo)
      next
    }
    mitad <- length(tramo) %/% 2L
    pendientes <- c(
      list(tramo[seq_len(mitad)], tramo[-seq_len(mitad)]), pendientes
    )
  }
  if (sin_aislar > 0L) {
    ultimo_error <- paste0(
      ultimo_error, "; tras ", max_intentos_fallidos, " intentos fallidos se ",
      "dejo de aislar valor por valor y ", sin_aislar, " se cuentan por el ",
      "tramo que los contenia"
    )
  }
  if (!length(piezas)) {
    return(list(
      geometria = NULL, indices = integer(),
      fallidas = as.integer(indices[fallidas]), error = ultimo_error
    ))
  }
  if (length(piezas) == 1L) {
    geometria <- piezas[[1L]]$sfc
    convertidas <- piezas[[1L]]$posiciones
  } else {
    # Cada tramo puede traer su propio CRS -sf toma el de sus valores-; se unen
    # sin CRS y se declara el primero, igual que en una sola llamada.
    crs_unido <- sf::NA_crs_
    for (pieza in piezas) {
      crs_pieza <- sf::st_crs(pieza$sfc)
      if (!isTRUE(is.na(crs_pieza))) {
        crs_unido <- crs_pieza
        break
      }
    }
    convertidas <- unlist(lapply(piezas, `[[`, "posiciones"), use.names = FALSE)
    geometria <- do.call(c, lapply(piezas, function(pieza) {
      suppressWarnings(sf::st_set_crs(pieza$sfc, sf::NA_crs_))
    }))
    orden <- .orden_por_bytes(convertidas)
    convertidas <- convertidas[orden]
    geometria <- suppressWarnings(sf::st_set_crs(geometria[orden], crs_unido))
  }
  list(
    geometria = geometria, indices = as.integer(indices[convertidas]),
    fallidas = as.integer(indices[.sort_por_bytes(fallidas)]), error = ultimo_error
  )
}

# El detalle de lo que no convirtio, con su cifra y su causa: lo que la guarda
# descarto no llego a sf; lo que sf no pudo leer lleva su ultimo error.
.detalle_no_convertidos <- function(representacion, n_descartados, n_fallidos,
                                    error, n_nulos = 0L) {
  forma <- if (identical(representacion, "WKT")) {
    paste(
      "una forma WKT que el paquete mida (texto corrupto, vacio o un tipo",
      "que no mide, como las curvas)"
    )
  } else {
    "el encabezado de un WKB"
  }
  partes <- c(
    if (n_descartados > 0L) {
      paste0(
        n_descartados,
        if (n_descartados == 1L) " no tiene " else " no tienen ", forma,
        if (n_descartados == 1L) " y no se entrego" else " y no se entregaron",
        " a sf",
        if (n_nulos > 0L) {
          paste0(" (", n_nulos, if (n_nulos == 1L) " es NULL)" else " son NULL)")
        } else ""
      )
    },
    if (n_fallidos > 0L) {
      paste0(
        n_fallidos,
        if (n_fallidos == 1L) " no lo pudo leer sf" else " no los pudo leer sf",
        " (ultimo error: ", error, ")"
      )
    }
  )
  paste(partes, collapse = "; ")
}

# Devuelve NULL cuando la columna no es geometrica. Para WKT y para la clase
# `WKB` la etiqueta es inequivoca y una conversion que no da nada se declara como
# perdida; para el WKB crudo sin clase y el hexadecimal, en cambio, solo se
# afirma que la columna es geometrica si al menos un valor convierte, porque el
# parecido de un identificador hexadecimal con un WKB no alcanza para acusar a la
# columna de nada.
#
# Lo que no convierte ya no borra la columna. Antes, un solo `""` en una columna
# EWKB hexadecimal la dejaba sin representacion, sin motivo y sin un solo campo
# geometrico -diez invalidas y diez fuera de dominio desaparecian-, en WKB crudo
# el valor perdido no se declaraba en ningun lado y en WKT uno solo dejaba la
# columna entera sin medir. Ahora se mide lo que convirtio y se declara lo que
# no, con su cifra, en `motivo_representacion`: lo que `?perfilar` promete.
.geometria_alternativa <- function(x, max_geometrias, prioridad = NULL) {
  representacion <- .representacion_geometrica(x)
  if (is.na(representacion)) return(NULL)
  autodeclarada <- identical(representacion, "WKT") || inherits(x, "WKB")

  if (!.sf_disponible()) {
    if (!autodeclarada) return(NULL)
    return(list(
      representacion = representacion, sf_evaluado = FALSE, geometria = NULL,
      indices = integer(), n_total = length(x),
      motivo = paste0(
        "La columna llega como ", representacion,
        " y no se convirtio a geometria porque falta el paquete opcional 'sf'."
      )
    ))
  }

  presentes <- .posiciones_presentes(x)
  presupuesto <- .muestrear_posiciones_geometria(
    presentes, max_geometrias,
    prioridad = if (length(prioridad) == length(x)) prioridad[presentes]
  )
  intentadas <- presupuesto$indices
  plausibles <- .posiciones_convertibles(x, representacion, intentadas)
  conversion <- if (length(plausibles)) {
    .convertir_a_sfc(x, representacion, plausibles)
  } else {
    list(
      geometria = NULL, indices = integer(), fallidas = integer(),
      error = NA_character_
    )
  }
  n_descartados <- length(intentadas) - length(plausibles)
  n_fallidos <- length(conversion$fallidas)
  n_no_convertidos <- as.integer(n_descartados + n_fallidos)
  n_nulos <- if (is.list(x) && n_descartados > 0L) {
    sum(vapply(unclass(x)[setdiff(intentadas, plausibles)], is.null, logical(1L)))
  } else {
    0L
  }
  detalle <- .detalle_no_convertidos(
    representacion, n_descartados, n_fallidos, conversion$error, n_nulos
  )
  universo_intento <- if (presupuesto$recortado) {
    " valores que se intentaron convertir"
  } else {
    " valores presentes"
  }

  if (!length(conversion$indices)) {
    if (!autodeclarada) return(NULL)
    srids <- .srids_representacion_geometrica(x, representacion, intentadas)
    return(list(
      representacion = representacion, sf_evaluado = TRUE, geometria = NULL,
      indices = integer(), n_total = length(x), presentes = presentes,
      srids_declarados = unique(srids[!is.na(srids)]),
      n_no_convertidos = n_no_convertidos,
      motivo = paste0(
        "La columna llega como ", representacion,
        " y no se pudo convertir a geometria ninguno de sus ",
        length(intentadas), universo_intento,
        if (nzchar(detalle)) paste0(": ", detalle) else "",
        ". Las metricas espaciales quedan sin medir."
      )
    ))
  }

  motivo_conversion <- if (n_no_convertidos > 0L) {
    paste0(
      "La columna llega como ", representacion, " y ", n_no_convertidos,
      " de ", length(intentadas), universo_intento,
      if (n_no_convertidos == 1L) " no se pudo" else " no se pudieron",
      " convertir a geometria: ", detalle,
      ". Las metricas espaciales cubren las ", length(conversion$indices),
      " restantes."
    )
  } else {
    NA_character_
  }
  motivo_recorte <- if (presupuesto$recortado) presupuesto$motivo else NA_character_
  motivos <- c(motivo_conversion, motivo_recorte)
  motivos <- motivos[!is.na(motivos)]
  list(
    representacion = representacion, sf_evaluado = TRUE,
    geometria = conversion$geometria, indices = conversion$indices,
    n_total = length(x), presentes = presentes,
    srids = .srids_representacion_geometrica(
      x, representacion, conversion$indices
    ),
    recortado = presupuesto$recortado,
    motivo_recorte = motivo_recorte,
    n_no_convertidos = n_no_convertidos,
    motivo_conversion = motivo_conversion,
    motivo = if (length(motivos)) paste(motivos, collapse = " ") else NA_character_
  )
}

# El tope de la conversion sigue la misma regla que el del analisis: con un
# universo declarado (`prioridad`, alineado con `posiciones`), primero sus
# valores -muestreados de forma uniforme si ellos solos exceden el tope- y lo que
# sobra, al resto.
.muestrear_posiciones_geometria <- function(posiciones, max_geometrias,
                                            prioridad = NULL) {
  n <- length(posiciones)
  if (!is.finite(max_geometrias) || n <= max_geometrias) {
    return(list(indices = posiciones, recortado = FALSE, motivo = NA_character_))
  }
  uniforme <- function(candidatas, cupo) {
    if (cupo < 1L || !length(candidatas)) return(candidatas[0L])
    if (length(candidatas) <= cupo) return(candidatas)
    candidatas[unique(as.integer(round(
      seq.int(1, length(candidatas), length.out = as.integer(cupo))
    )))]
  }
  prioritario <- !is.null(prioridad) && length(prioridad) == n &&
    !anyNA(prioridad) && any(prioridad) && !all(prioridad)
  indices <- if (prioritario) {
    primero <- uniforme(posiciones[prioridad], max_geometrias)
    .sort_por_bytes(c(primero, uniforme(
      posiciones[!prioridad], max_geometrias - length(primero)
    )))
  } else {
    uniforme(posiciones, max_geometrias)
  }
  list(
    indices = indices, recortado = TRUE,
    motivo = paste0(
      "La conversion a geometria se limito a ", length(indices), " de ", n,
      " valores; se sube con options(lupa.max_geometrias=).",
      if (prioritario) {
        paste0(
          " Se convirtieron primero los ", sum(prioridad), " valores del ",
          "universo aplicable declarado y lo que sobro del tope, del resto."
        )
      } else ""
    )
  )
}

#' Medir la geometría de una columna
#'
#' Reconoce la columna como geometría cuando llega como `sfc` y también cuando
#' llega como WKT, como WKB crudo o como WKB hexadecimal, casos en los que la
#' convierte antes de medir. Si la columna es geométrica y la conversión no se
#' puede hacer, la pérdida se declara: nunca se afirma que la geometría no
#' aplica sobre datos que sí son geométricos.
#'
#' Lo que escala con la cantidad de vértices —la validez topológica y el
#' dominio del CRS— se acota con un presupuesto y el recorte se declara. Lo que
#' escala con filas se sigue midiendo sobre la columna entera.
#'
#' Ante un fallo parcial se devuelve lo medido con su alcance declarado. Una
#' geometría que no se puede transformar no descarta la evaluación de las
#' demás, y un `NA` de `st_is_valid()` no borra el conteo de las que sí se
#' pudieron evaluar.
#'
#' @param x Columna que se quiere medir.
#' @param max_geometrias Máximo de geometrías que reciben los análisis por
#'   geometría. Se mueve con `options(lupa.max_geometrias = )`.
#' @param max_vertices Máximo de vértices que se recorren. Se mueve con
#'   `options(lupa.max_vertices = )`.
#' @param aplicable Máscara lógica del universo aplicable, o `NULL`. No recorta
#'   el análisis: decide que el presupuesto se gaste primero en el universo.
#'
#' @return Lista de métricas geométricas. Además de las medidas trae
#'   `representacion_geometria`, `geometria_convertida`, los conteos de lo
#'   evaluado y de lo no evaluado, y los motivos de cada recorte o pérdida.
#' @noRd
.perfilar_geometria <- function(x,
                                max_geometrias = .max_geometrias_analizadas(),
                                max_vertices = .max_vertices_analizados(),
                                aplicable = NULL) {
  salida <- .metricas_geometria_vacias()
  # El universo aplicable no recorta el analisis -los tipos, el CRS, las vacias
  # y la bbox se miden sobre la columna entera-, pero decide en que se gasta el
  # presupuesto: primero en el universo.
  if (!is.null(aplicable) &&
      (length(aplicable) != length(x) || anyNA(aplicable) || all(aplicable))) {
    aplicable <- NULL
  }
  posiciones <- NULL
  alternativa <- NULL
  if (inherits(x, "sfc")) {
    salida$aplica <- TRUE
    salida$representacion_geometria <- "sfc"
    salida$geometria_convertida <- FALSE
    salida$sf_evaluado <- .sf_disponible()
    salida$n_geometrias <- length(x)
    if (!salida$sf_evaluado) return(salida)
  } else {
    alternativa <- .geometria_alternativa(
      x, max_geometrias, prioridad = aplicable
    )
    if (is.null(alternativa)) return(salida)
    salida$aplica <- TRUE
    salida$representacion_geometria <- alternativa$representacion
    salida$sf_evaluado <- alternativa$sf_evaluado
    salida$n_geometrias <- alternativa$n_total
    salida$motivo_representacion <- alternativa$motivo
    salida$geometria_convertida <- !is.null(alternativa$geometria)
    if (!is.null(alternativa$n_no_convertidos)) {
      salida$n_valores_no_convertidos <- alternativa$n_no_convertidos
    }
    if (is.null(alternativa$geometria)) {
      # Se declara la perdida en vez de dejar las metricas en NA sin motivo.
      salida$validez_evaluada <- FALSE
      salida$motivo_validez <- alternativa$motivo
      salida$dominio_evaluado <- FALSE
      salida$motivo_dominio <- alternativa$motivo
      # Si los valores declaran su SRID, la columna declara un CRS aunque no se
      # haya podido convertir: acusar `crs_no_declarado` afirmaba algo falso
      # sobre lo que no se midio.
      if (length(alternativa$srids_declarados)) {
        salida$crs_declarado <- paste(alternativa$srids_declarados, collapse = ", ")
      }
      return(salida)
    }
    x <- alternativa$geometria
    posiciones <- alternativa$indices
  }
  metricas <- .remapear_indices_geometria(
    .metricas_sfc(
      x, salida, max_geometrias, max_vertices,
      srids = if (is.null(alternativa)) NULL else alternativa$srids,
      prioridad = if (is.null(aplicable)) {
        NULL
      } else if (is.null(posiciones)) {
        as.logical(aplicable)
      } else {
        as.logical(aplicable)[posiciones]
      }
    ), posiciones
  )
  if (isTRUE(alternativa$recortado)) {
    # El recorte de la conversion es un recorte del analisis, y se declara como
    # tal aunque la `sfc` resultante entre entera en el presupuesto.
    metricas$geometrias_recortadas <- TRUE
    metricas$motivo_recorte <- if (is.na(metricas$motivo_recorte)) {
      alternativa$motivo_recorte
    } else {
      paste(alternativa$motivo_recorte, metricas$motivo_recorte)
    }
  }
  if (!is.null(alternativa)) {
    metricas$motivo_conversion <- alternativa$motivo_conversion
    metricas$indices_presentes <- as.integer(alternativa$presentes)
  } else {
    metricas$indices_presentes <- seq_along(x)
  }
  metricas
}

# Los campos de indices, en un solo lugar: los leen el remapeo y el recorte
# al universo aplicable, y si cada uno los escribiera por su lado, agregar un
# diagnostico con indices dejaria a uno de los dos sin enterarse.
.CAMPOS_INDICES_GEOMETRIA <- c(
  "indices_vacias", "indices_invalidas", "indices_fuera_de_dominio",
  "indices_validez_no_evaluados", "indices_dominio_no_evaluados",
  "indices_geometrias", "indices_analizados"
)

# Las metricas se calculan sobre la `sfc` efectiva; los indices que viajan a la
# trazabilidad tienen que apuntar a las filas de la columna original.
.remapear_indices_geometria <- function(salida, posiciones) {
  if (is.null(posiciones)) return(salida)
  for (campo in .CAMPOS_INDICES_GEOMETRIA) {
    salida[[campo]] <- as.integer(posiciones[salida[[campo]]])
  }
  salida
}

# El universo aplicable gobierna TODOS los conteos, y las metricas de geometria se
# calculan antes de que la mascara exista: `.perfilar_geometria()` corre sobre la
# columna cruda. Medido: con seis geometrias y dos declaradas fuera del universo,
# `coordenada_fuera_dominio` publicaba tres afectadas y la traza -que si recorta-
# nombraba una, y el propio paquete avisaba de la incoherencia sobre un perfil
# intacto. El camino de texto ya respetaba el universo; el de geometria no.
#
# Se recorta POR INDICES, que es la unica forma de conservar las coordenadas de la
# tabla original, y cada contador se recalcula de su recorte.
.restringir_geometria_al_universo <- function(salida, aplicable) {
  if (is.null(aplicable) || all(aplicable)) return(salida)
  aplicables <- which(aplicable)
  contadores <- c(
    indices_vacias = "n_geometrias_vacias",
    indices_invalidas = "n_geometrias_invalidas",
    indices_fuera_de_dominio = "n_fuera_de_dominio",
    indices_validez_no_evaluados = "n_validez_no_evaluados",
    indices_dominio_no_evaluados = "n_dominio_no_evaluados",
    indices_geometrias = "n_geometrias_medidas"
  )
  for (campo in .CAMPOS_INDICES_GEOMETRIA) {
    indices <- salida[[campo]]
    conservados <- if (length(indices)) {
      as.integer(indices[indices %in% aplicables])
    } else {
      integer()
    }
    salida[[campo]] <- conservados
    if (!campo %in% names(contadores)) next
    contador <- contadores[[campo]]
    if (!is.null(salida[[contador]]) && !is.na(salida[[contador]])) {
      salida[[contador]] <- length(conservados)
    }
  }
  # `n_geometrias` es el universo. Los dos "evaluados" cuentan lo que de verdad
  # se evaluo DENTRO de el, y eso sale de las posiciones analizadas, no de las
  # filas aplicables: antes se derivaban de `length(aplicables)` menos los no
  # evaluados con indice, y las filas que el presupuesto no analizo, los `NA` de
  # una columna de texto y las vacias -para el dominio- no tenian indice, asi que
  # se publicaban como evaluadas. Medido con los topes por omision: 50 poligonos
  # invalidos del universo, todos fuera del presupuesto, publicaban "0 invalidas
  # de 120 evaluadas" y el hallazgo no se emitia.
  if (!is.null(salida$n_geometrias) && !is.na(salida$n_geometrias)) {
    salida$n_geometrias <- length(aplicables)
  }
  # La mezcla de tipos es de la columna -ver `.conteo_hallazgo_columna()`-, pero
  # su hallazgo cuenta las geometrias del universo, y un universo sin ninguna
  # no tiene que mezclar: se emitia con 0 afectadas y el propio paquete avisaba
  # de la incoherencia.
  if (isTRUE(salida$tipos_geometria_mixtos) &&
      isTRUE(salida$n_geometrias_medidas == 0L)) {
    salida$tipos_geometria_mixtos <- FALSE
  }
  analizadas <- salida$indices_analizados
  if (!is.null(salida$n_validez_evaluados) && !is.na(salida$n_validez_evaluados)) {
    salida$n_validez_evaluados <- as.integer(
      length(analizadas) - length(salida$indices_validez_no_evaluados)
    )
  }
  if (!is.null(salida$n_dominio_evaluados) && !is.na(salida$n_dominio_evaluados)) {
    salida$n_dominio_evaluados <- as.integer(
      length(setdiff(analizadas, salida$indices_vacias)) -
        length(salida$indices_dominio_no_evaluados)
    )
  }
  # Lo que el presupuesto dejo afuera DENTRO del universo se declara con su
  # cifra: la traza del recorte habla de la columna entera ("1000 de 1100") y no
  # dejaba saber que del universo se habia analizado 20 de 120.
  presentes <- salida$indices_presentes
  presentes <- if (length(presentes)) {
    as.integer(presentes[presentes %in% aplicables])
  } else {
    salida$indices_geometrias
  }
  salida$indices_presentes <- presentes
  no_analizadas <- length(presentes) - length(analizadas)
  if (isTRUE(salida$geometrias_recortadas)) {
    salida$motivo_recorte <- paste0(
      salida$motivo_recorte,
      if (no_analizadas > 0L) {
        paste0(
          " Del universo aplicable declarado se analizaron ",
          length(analizadas), " de sus ", length(presentes),
          " valores presentes; los ", no_analizadas, " restantes quedaron sin ",
          "evaluar y no integran los conteos de validez ni de dominio."
        )
      } else {
        paste0(
          " Del universo aplicable declarado se analizaron sus ",
          length(presentes), " valores presentes: el recorte cayo fuera de el y ",
          "solo alcanza a lo que se publica de la columna entera."
        )
      }
    )
  }
  # Y los tres que NO se recortan, porque no cuentan filas de un universo sino
  # TRABAJO hecho: `n_bbox_evaluados`, `n_geometrias_analizadas` y
  # `n_vertices_analizados` describen cuanto se analizo, y el analisis corrio antes
  # de que el universo existiera. Dejarlos como se midieron y decirlo es mas honesto
  # que recortarlos a ojo: no hay conjunto de indices del que derivarlos. Queda
  # escrito en `?perfilar`, junto a los campos.
  salida
}

# Con varios SRID, el CRS de la columna es geografico si TODOS lo son. Antes
# cualquier mezcla dejaba `crs_geografico` en NA, y una columna 4326 + 4258 con
# un poligono invalido publicaba `error` donde la misma columna toda en 4326
# publica `sospechoso`: el criterio planar no afirma invalidez esferica en
# ninguno de los dos. Un SRID que no se puede leer deja la respuesta en NA.
.srids_geograficos <- function(srids) {
  geograficos <- vapply(srids, function(srid) {
    crs <- tryCatch(suppressWarnings(sf::st_crs(srid)), error = function(e) NA)
    if (isTRUE(is.na(crs))) return(NA)
    tryCatch(
      isTRUE(suppressWarnings(sf::st_is_longlat(crs))),
      error = function(e) NA
    )
  }, logical(1L))
  if (!length(geograficos) || anyNA(geograficos)) return(NA)
  if (all(geograficos)) return(TRUE)
  if (!any(geograficos)) return(FALSE)
  NA
}

.metricas_sfc <- function(x, salida, max_geometrias, max_vertices,
                          srids = NULL, prioridad = NULL) {
  crs <- tryCatch(sf::st_crs(x), error = function(e) NA)
  tiene_crs <- !isTRUE(is.na(crs))
  if (tiene_crs) {
    salida$crs_declarado <- .etiqueta_crs(crs)
    salida$crs_geografico <- tryCatch(
      suppressWarnings(sf::st_is_longlat(crs)), error = function(e) NA
    )
  }
  srids_validos <- if (is.null(srids) || length(srids) != length(x)) {
    integer()
  } else {
    suppressWarnings(as.integer(srids))
  }
  srids_declarados <- unique(srids_validos[!is.na(srids_validos)])
  if (length(srids_declarados)) {
    salida$crs_declarado <- paste(srids_declarados, collapse = ", ")
    if (length(srids_declarados) > 1L) {
      salida$crs_geografico <- .srids_geograficos(srids_declarados)
    }
  }

  tipos <- tryCatch(
    as.character(sf::st_geometry_type(x, by_geometry = TRUE)),
    error = function(e) character()
  )
  salida$indices_geometrias <- seq_along(x)
  salida$n_geometrias_medidas <- length(x)
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

  # El presupuesto acota lo que escala con vertices -validez topologica y
  # dominio-; lo que escala con filas se sigue midiendo sobre la columna entera.
  presupuesto <- .presupuesto_geometrias(
    x, max_geometrias, max_vertices, prioridad = prioridad
  )
  indices <- presupuesto$indices
  salida$n_geometrias_analizadas <- as.integer(length(indices))
  salida$indices_analizados <- as.integer(indices)
  salida$n_vertices_analizados <- presupuesto$n_vertices
  salida$geometrias_recortadas <- presupuesto$recortado
  salida$motivo_recorte <- presupuesto$motivo
  x_analisis <- if (presupuesto$recortado) x[indices] else x
  vacias_analisis <- vacias[indices]

  # GEOS recibe coordenadas sin CRS para que la disponibilidad de s2 no cambie
  # el resultado. El criterio aplicado se publica y los CRS geograficos se
  # interpretan de forma cauta al construir el hallazgo.
  salida$validez_criterio <- "planar"
  salida$validez_preprocesamiento <- "ninguno"
  x_validez <- x_analisis
  if ("M" %in% salida$dimensiones_omitidas) {
    salida$validez_preprocesamiento <- "st_zm(x)"
    x_validez <- tryCatch(
      suppressWarnings(sf::st_zm(x_analisis)), error = function(e) e
    )
  }
  validas <- if (inherits(x_validez, "error")) {
    x_validez
  } else {
    tryCatch(
      suppressWarnings(sf::st_is_valid(
        suppressWarnings(sf::st_set_crs(x_validez, NA)),
        NA_on_exception = TRUE
      )),
      error = function(e) e
    )
  }
  if (inherits(validas, "error") || length(validas) != length(x_analisis)) {
    salida$validez_evaluada <- FALSE
    salida$motivo_validez <- if (inherits(validas, "error")) {
      conditionMessage(validas)
    } else {
      "st_is_valid() no devolvio un resultado para todas las geometrias."
    }
  } else {
    # Antes, un solo NA de st_is_valid() descartaba el conteo y los indices de
    # todas las geometrias. Ahora se conserva lo medido y se declara el alcance.
    evaluadas <- !is.na(validas)
    salida$validez_evaluada <- !length(validas) || any(evaluadas)
    salida$n_validez_evaluados <- as.integer(sum(evaluadas))
    salida$n_geometrias_invalidas <- as.integer(sum(!validas[evaluadas]))
    salida$indices_invalidas <- as.integer(indices[which(evaluadas & !validas)])
    salida$n_validez_no_evaluados <- as.integer(sum(!evaluadas))
    salida$indices_validez_no_evaluados <- as.integer(indices[which(!evaluadas)])
    if (any(!evaluadas)) {
      salida$motivo_validez <- paste0(
        "st_is_valid() no devolvio un resultado para ", sum(!evaluadas), " de ",
        length(validas), " geometrias; el conteo cubre las ", sum(evaluadas),
        " restantes."
      )
    }
    if (isFALSE(salida$validez_evaluada)) {
      salida$n_geometrias_invalidas <- NA_integer_
      salida$indices_invalidas <- integer()
    }
  }

  salida$bbox_alcance <- "coordenadas_crudas_de_geometrias_no_vacias"
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
    srids_analisis <- if (length(srids_validos) == length(x)) {
      srids_validos[indices]
    } else NULL
    dominio <- .evaluar_dominio_geometria(
      x_analisis, crs, vacias_analisis, srids = srids_analisis
    )
    salida$dominio_evaluado <- dominio$evaluado
    if (isTRUE(dominio$evaluado)) {
      salida$n_dominio_evaluados <- dominio$n_evaluados
      salida$n_fuera_de_dominio <- as.integer(sum(dominio$fuera))
      salida$indices_fuera_de_dominio <-
        as.integer(indices[which(dominio$fuera)])
      salida$n_dominio_no_evaluados <- as.integer(dominio$n_no_evaluados)
      salida$indices_dominio_no_evaluados <-
        as.integer(indices[dominio$no_evaluados])
      if (length(dominio$no_evaluados)) {
        salida$motivo_dominio <- dominio$motivo
      }
    } else {
      salida$motivo_dominio <- dominio$motivo
    }
  } else {
    salida$dominio_evaluado <- FALSE
    salida$motivo_dominio <- "La columna no declara un CRS."
  }
  salida
}

# Una columna de texto que el paquete RECONOCIO como geometria -WKT, WKB o su
# forma hexadecimal- se sigue perfilando como texto: sus espacios sobrantes, su
# codificacion o sus invisibles son defectos reales de esa escritura. Pero tres
# diagnosticos leen los valores como palabras o como claves, y sobre una
# geometria no dicen nada: `POINT (0 0)` y `POINT (1 1)` no son variantes de un
# vocabulario, un WKB en hexadecimal no es un identificador de negocio y la forma
# de unas coordenadas no es un patron raro. Los mismos tres puntos como `sfc`
# daban un solo hallazgo; como WKT, ademas ruido de texto. Se retiran y se
# declaran en la cobertura, con el nombre del diagnostico.
#
# La retirada alcanza tambien a la COBERTURA. Cuando el diagnostico corria sin
# producir hallazgo dejaba su propia fila -con 2.500 WKT, "el vocabulario excede
# el alcance de comparacion" o "el grupo candidato mayor abarca 0.572"- y la
# columna recibia un motivo de medicion donde con 50 valores recibia "no aplica".
# Que no aplique no puede depender de cuantos valores haya: esas filas se
# reemplazan por la misma declaracion. Con `cobertura`, devuelve tambien
# `cobertura_diagnosticos`, la cobertura recibida sin esas filas y con las nuevas.
.retirar_hallazgos_texto_de_geometria <- function(hallazgos, columnas,
                                                  cobertura = NULL) {
  previa <- if (is.null(cobertura)) .cobertura_diagnosticos_vacia() else cobertura
  sin_cambios <- list(
    hallazgos = hallazgos, cobertura = .cobertura_diagnosticos_vacia(),
    cobertura_diagnosticos = previa
  )
  if (is.null(columnas$representacion_geometria)) return(sin_cambios)
  geometricas <- columnas$columna[!is.na(columnas$representacion_geometria)]
  if (!length(geometricas)) return(sin_cambios)
  claves <- .nombres_para_operar(geometricas)
  diagnostico_de <- c(
    casi_duplicados_vocabulario = "proximidad_vocabulario",
    variantes_equifrecuentes_vocabulario = "proximidad_vocabulario",
    posible_identificador = "posible_identificador",
    patron_raro = "patron_raro"
  )
  retirar <- if (nrow(hallazgos)) {
    hallazgos$tipo_hallazgo %in% names(diagnostico_de) &
      !is.na(hallazgos$columna) &
      .nombres_para_operar(hallazgos$columna) %in% claves
  } else {
    logical()
  }
  quitar <- if (nrow(previa) && all(c("diagnostico", "columna") %in% names(previa))) {
    previa$diagnostico %in% unique(diagnostico_de) & !is.na(previa$columna) &
      .nombres_para_operar(previa$columna) %in% claves
  } else {
    logical(nrow(previa))
  }
  if (!any(retirar) && !any(quitar)) return(sin_cambios)
  retirados <- data.frame(
    columna = c(hallazgos$columna[retirar], previa$columna[quitar]),
    diagnostico = c(
      unname(diagnostico_de[hallazgos$tipo_hallazgo[retirar]]),
      previa$diagnostico[quitar]
    ),
    stringsAsFactors = FALSE
  )
  retirados <- retirados[!duplicated(data.frame(
    .nombres_para_operar(retirados$columna), retirados$diagnostico
  )), , drop = FALSE]
  representacion <- columnas$representacion_geometria[
    match(.nombres_para_operar(retirados$columna),
          .nombres_para_operar(columnas$columna))
  ]
  nueva <- do.call(rbind, lapply(seq_len(nrow(retirados)), function(i) {
    .nuevo_diagnostico_no_evaluado(
      retirados$diagnostico[[i]], retirados$columna[[i]],
      paste0(
        "La columna es una geometria escrita como texto (", representacion[[i]],
        "): sus valores no son palabras ni claves, y este diagnostico de texto ",
        "no aplica."
      ),
      paste(
        "Convertirla con `sf::st_as_sfc()` para perfilarla como geometria. Los",
        "diagnosticos de la escritura -espacios, codificacion, invisibles- se",
        "siguen publicando."
      )
    )
  }))
  hallazgos <- hallazgos[!retirar, , drop = FALSE]
  rownames(hallazgos) <- NULL
  previa <- previa[!quitar, , drop = FALSE]
  rownames(previa) <- NULL
  list(
    hallazgos = hallazgos, cobertura = nueva,
    cobertura_diagnosticos = rbind(previa, nueva)
  )
}
