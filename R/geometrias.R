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
    indices_dominio_no_evaluados = integer()
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

.presupuesto_geometrias <- function(x, max_geometrias, max_vertices) {
  n <- length(x)
  indices <- seq_len(n)
  sin_recorte <- list(
    indices = indices, recortado = FALSE, motivo = NA_character_,
    n_total = n, n_vertices = NA_real_
  )
  if (!n) return(sin_recorte)

  recorte_geometrias <- is.finite(max_geometrias) && n > max_geometrias
  if (recorte_geometrias) {
    indices <- unique(as.integer(round(
      seq.int(1, n, length.out = as.integer(max_geometrias))
    )))
  }
  vertices <- vapply(unclass(x)[indices], .n_vertices_sfg, numeric(1L))
  total_vertices <- sum(vertices)
  recorte_vertices <- is.finite(max_vertices) && total_vertices > max_vertices
  if (recorte_vertices) {
    objetivo <- max(1L, as.integer(floor(
      length(indices) * max_vertices / total_vertices
    )))
    seleccion <- unique(as.integer(round(
      seq.int(1, length(indices), length.out = objetivo)
    )))
    indices <- indices[seleccion]
    vertices <- vertices[seleccion]
    dentro <- which(cumsum(vertices) <= max_vertices)
    if (!length(dentro)) dentro <- 1L
    indices <- indices[dentro]
    vertices <- vertices[dentro]
    total_vertices <- sum(vertices)
  }
  if (!recorte_geometrias && !recorte_vertices) {
    sin_recorte$n_vertices <- total_vertices
    return(sin_recorte)
  }
  list(
    indices = indices, recortado = TRUE, n_total = n,
    n_vertices = total_vertices,
    motivo = paste0(
      "El presupuesto geometrico recorto el analisis por geometria: se ",
      "evaluaron ", length(indices), " de ", n, " geometrias, con ",
      format(total_vertices, scientific = FALSE), " vertices. Los topes son ",
      "max_geometrias=", format(max_geometrias, scientific = FALSE),
      " y max_vertices=", format(max_vertices, scientific = FALSE),
      "; se suben con options(lupa.max_geometrias=) y options(lupa.max_vertices=)."
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
      toupper(paste0(caracteres[i:(i + 3L)], collapse = "")) == "BBOX"
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
          "el EWKB no declara un CRS utilizable"
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
  if (isTRUE(es_longlat)) {
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
  guardadas <- vapply(coordenadas[evaluables], function(y) {
    any(!is.finite(y)) || any(abs(y) > 1e15)
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

.parece_wkt <- function(x) {
  if (!is.character(x)) return(FALSE)
  muestra <- .muestra_texto_geometria(x)
  length(muestra) > 0L &&
    all(grepl(.patron_wkt, muestra, perl = TRUE, ignore.case = TRUE))
}

.parece_wkb_hexadecimal <- function(x) {
  if (!is.character(x)) return(FALSE)
  muestra <- .muestra_texto_geometria(x)
  if (!length(muestra)) return(FALSE)
  largos <- nchar(muestra, type = "bytes")
  all(largos >= 18L) && all(largos %% 2L == 0L) &&
    all(grepl("^[0-9A-Fa-f]+$", muestra, perl = TRUE)) &&
    all(substr(muestra, 1L, 2L) %in% c("00", "01"))
}

# Esta guarda lee solo el encabezado, no intenta validar el cuerpo recursivo de
# la geometria. Un encabezado plausible con un cuerpo corrupto todavia puede
# llegar a GDAL y fallar alli. No se duplica el parser completo de sf: ademas de
# ser mas costoso, dejaria dos implementaciones de la gramatica WKB que podrian
# divergir; esta guarda solo evita entregar bytes que ni siquiera pueden ser WKB.
.wkb_plausible <- function(valor) {
  if (!is.raw(valor) || length(valor) < 5L) return(FALSE)

  orden <- as.integer(valor[[1L]])
  if (!orden %in% c(0L, 1L)) return(FALSE)

  bytes_tipo <- as.integer(valor[2L:5L])
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
  length(valor) >= encabezado + cuerpo_minimo
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
  capturado <- regexec("^\\s*SRID=([0-9]+);", as.character(valor),
                       ignore.case = TRUE)
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

.posiciones_convertibles <- function(x, representacion) {
  if (identical(representacion, "WKB")) {
    return(which(vapply(unclass(x), .wkb_plausible, logical(1L))))
  }
  which(!is.na(x))
}

# Un WKT plausible lleva la palabra del tipo y, despues, solo numeros,
# separadores y parentesis balanceados. Es la misma doctrina de la guarda WKB:
# el GDAL de win-builder demostro que puede reventar -segfault, no error- con
# entrada corrupta, y un tryCatch no atrapa eso; el texto podrido no llega a
# sf, se declara la perdida por aritmetica propia.
.wkt_plausible <- function(valor) {
  if (is.na(valor)) return(FALSE)
  texto <- trimws(as.character(valor))
  patron <- paste0(
    "^(SRID=[0-9]+;)?\\s*",
    "(POINT|LINESTRING|POLYGON|MULTIPOINT|MULTILINESTRING|MULTIPOLYGON|",
    "GEOMETRYCOLLECTION)\\s*(Z|M|ZM)?\\s*",
    "(EMPTY|\\((?:[-+0-9eE., ()\\s]|EMPTY|POINT|LINESTRING|POLYGON|",
    "MULTIPOINT|MULTILINESTRING|MULTIPOLYGON)*\\))$"
  )
  if (!grepl(patron, texto, perl = TRUE, ignore.case = TRUE)) return(FALSE)
  abre <- lengths(regmatches(texto, gregexpr("(", texto, fixed = TRUE)))
  cierra <- lengths(regmatches(texto, gregexpr(")", texto, fixed = TRUE)))
  abre == cierra
}

.convertir_a_sfc <- function(x, representacion, indices) {
  valores <- unclass(x)[indices]
  if (identical(representacion, "WKT")) {
    plausibles <- vapply(as.character(valores), .wkt_plausible, logical(1L))
    if (any(!plausibles)) {
      return(simpleError(paste0(
        "La columna contiene WKT estructuralmente invalido; no se pudo ",
        "convertir a geometria y el texto corrupto no se entrega a sf."
      )))
    }
  }
  tryCatch(
    suppressWarnings(
      if (identical(representacion, "WKT")) {
        sf::st_as_sfc(as.character(valores))
      } else {
        sf::st_as_sfc(structure(valores, class = "WKB"), EWKB = TRUE)
      }
    ),
    error = function(e) e
  )
}

# Devuelve NULL cuando la columna no es geometrica. Para WKT la etiqueta es
# inequivoca y una conversion fallida se declara como perdida; para WKB y
# hexadecimal, en cambio, solo se afirma que la columna es geometrica si la
# conversion funciona, porque el parecido de un identificador hexadecimal con
# un WKB no alcanza para acusar a la columna de nada.
.geometria_alternativa <- function(x, max_geometrias) {
  representacion <- .representacion_geometrica(x)
  if (is.na(representacion)) return(NULL)
  autodeclarada <- identical(representacion, "WKT") || inherits(x, "WKB")

  if (identical(representacion, "WKB") && inherits(x, "WKB")) {
    plausibles <- vapply(unclass(x), .wkb_plausible, logical(1L))
    if (any(!plausibles)) {
      return(list(
        representacion = representacion, sf_evaluado = TRUE, geometria = NULL,
        indices = integer(), n_total = length(x),
        motivo = paste0(
          "La columna llega como WKB y contiene valores estructuralmente ",
          "invalidos; no se pudo convertir a geometria. Las metricas ",
          "espaciales quedan sin medir."
        )
      ))
    }
  }

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

  convertibles <- .posiciones_convertibles(x, representacion)
  presupuesto <- .muestrear_posiciones_geometria(convertibles, max_geometrias)
  indices <- presupuesto$indices
  convertida <- if (length(indices)) {
    .convertir_a_sfc(x, representacion, indices)
  } else {
    simpleError("La columna no tiene ningun valor convertible.")
  }
  if (inherits(convertida, "error") || !inherits(convertida, "sfc") ||
      length(convertida) != length(indices)) {
    if (!autodeclarada) return(NULL)
    mensaje <- if (inherits(convertida, "error")) {
      conditionMessage(convertida)
    } else {
      "La conversion no devolvio una geometria por valor."
    }
    return(list(
      representacion = representacion, sf_evaluado = TRUE, geometria = NULL,
      indices = integer(), n_total = length(x),
      motivo = paste0(
        "La columna llega como ", representacion,
        " y no se pudo convertir a geometria: ", mensaje,
        " Las metricas espaciales quedan sin medir."
      )
    ))
  }
  list(
    representacion = representacion, sf_evaluado = TRUE,
    geometria = convertida, indices = indices, n_total = length(x),
    srids = .srids_representacion_geometrica(x, representacion, indices),
    recortado = presupuesto$recortado,
    motivo = if (presupuesto$recortado) presupuesto$motivo else NA_character_
  )
}

.muestrear_posiciones_geometria <- function(posiciones, max_geometrias) {
  n <- length(posiciones)
  if (!is.finite(max_geometrias) || n <= max_geometrias) {
    return(list(indices = posiciones, recortado = FALSE, motivo = NA_character_))
  }
  seleccion <- unique(as.integer(round(
    seq.int(1, n, length.out = as.integer(max_geometrias))
  )))
  list(
    indices = posiciones[seleccion], recortado = TRUE,
    motivo = paste0(
      "La conversion a geometria se limito a ", length(seleccion), " de ", n,
      " valores; se sube con options(lupa.max_geometrias=)."
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
#'
#' @return Lista de métricas geométricas. Además de las medidas trae
#'   `representacion_geometria`, `geometria_convertida`, los conteos de lo
#'   evaluado y de lo no evaluado, y los motivos de cada recorte o pérdida.
#' @noRd
.perfilar_geometria <- function(x,
                                max_geometrias = .max_geometrias_analizadas(),
                                max_vertices = .max_vertices_analizados()) {
  salida <- .metricas_geometria_vacias()
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
    alternativa <- .geometria_alternativa(x, max_geometrias)
    if (is.null(alternativa)) return(salida)
    salida$aplica <- TRUE
    salida$representacion_geometria <- alternativa$representacion
    salida$sf_evaluado <- alternativa$sf_evaluado
    salida$n_geometrias <- alternativa$n_total
    salida$motivo_representacion <- alternativa$motivo
    salida$geometria_convertida <- !is.null(alternativa$geometria)
    if (is.null(alternativa$geometria)) {
      # Se declara la perdida en vez de dejar las metricas en NA sin motivo.
      salida$validez_evaluada <- FALSE
      salida$motivo_validez <- alternativa$motivo
      salida$dominio_evaluado <- FALSE
      salida$motivo_dominio <- alternativa$motivo
      return(salida)
    }
    x <- alternativa$geometria
    posiciones <- alternativa$indices
  }
  metricas <- .remapear_indices_geometria(
    .metricas_sfc(
      x, salida, max_geometrias, max_vertices,
      srids = if (is.null(alternativa)) NULL else alternativa$srids
    ), posiciones
  )
  if (isTRUE(alternativa$recortado)) {
    # El recorte de la conversion es un recorte del analisis, y se declara como
    # tal aunque la `sfc` resultante entre entera en el presupuesto.
    metricas$geometrias_recortadas <- TRUE
    metricas$motivo_recorte <- if (is.na(metricas$motivo_recorte)) {
      alternativa$motivo
    } else {
      paste(alternativa$motivo, metricas$motivo_recorte)
    }
  }
  metricas
}

# Las metricas se calculan sobre la `sfc` efectiva; los indices que viajan a la
# trazabilidad tienen que apuntar a las filas de la columna original.
.remapear_indices_geometria <- function(salida, posiciones) {
  if (is.null(posiciones)) return(salida)
  campos <- c(
    "indices_vacias", "indices_invalidas", "indices_fuera_de_dominio",
    "indices_validez_no_evaluados", "indices_dominio_no_evaluados"
  )
  for (campo in campos) {
    salida[[campo]] <- as.integer(posiciones[salida[[campo]]])
  }
  salida
}

.metricas_sfc <- function(x, salida, max_geometrias, max_vertices,
                          srids = NULL) {
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
    if (length(srids_declarados) > 1L) salida$crs_geografico <- NA
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

  # El presupuesto acota lo que escala con vertices -validez topologica y
  # dominio-; lo que escala con filas se sigue midiendo sobre la columna entera.
  presupuesto <- .presupuesto_geometrias(x, max_geometrias, max_vertices)
  indices <- presupuesto$indices
  salida$n_geometrias_analizadas <- as.integer(length(indices))
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
