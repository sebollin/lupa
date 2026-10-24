## Utilidades comunes para los bancos externos.
## Solo usa R base y utils. No copia datos al repositorio.

.huella_adler32 <- function(ruta) {
  conexion <- file(ruta, open = "rb")
  on.exit(close(conexion), add = TRUE)
  a <- 1
  b <- 0
  repeat {
    bloque <- readBin(conexion, what = "raw", n = 5552L)
    if (!length(bloque)) break
    for (valor in as.integer(bloque)) {
      a <- (a + valor) %% 65521
      b <- (b + a) %% 65521
    }
  }
  sprintf("%04x%04x", b, a)
}

.leer_csv_texto <- function(ruta, nrows = -1L) {
  argumentos <- list(
    file = ruta,
    colClasses = "character",
    na.strings = NULL,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    encoding = "UTF-8"
  )
  if (is.finite(nrows) && nrows >= 0) argumentos$nrows <- as.integer(nrows)
  do.call(utils::read.csv, argumentos)
}

.rutas_locales <- function(directorio, candidatos) {
  if (!nzchar(directorio)) return(character())
  rutas <- file.path(directorio, candidatos)
  existentes <- rutas[file.exists(rutas)]
  if (length(existentes)) return(existentes[[1L]])
  character()
}

.obtener_archivo <- function(url, directorio, candidatos, destino, etiqueta) {
  local <- .rutas_locales(directorio, candidatos)
  if (length(local)) {
    if (!file.copy(local, destino, overwrite = TRUE)) {
      return(list(ok = FALSE, razon = paste0("no se pudo copiar ", local)))
    }
    return(list(
      ok = TRUE, ruta = destino, url = url, origen = "local",
      archivo = etiqueta
    ))
  }

  if (nzchar(directorio)) {
    return(list(
      ok = FALSE,
      razon = paste0("no existe en ", directorio, ": ",
                     paste(candidatos, collapse = ", "))
    ))
  }

  descarga <- tryCatch(
    utils::download.file(url, destino, mode = "wb", quiet = TRUE),
    error = function(e) e
  )
  if (inherits(descarga, "error") || !identical(as.integer(descarga), 0L) ||
      !file.exists(destino) || is.na(file.info(destino)$size) ||
      file.info(destino)$size <= 0) {
    razon <- if (inherits(descarga, "error")) {
      conditionMessage(descarga)
    } else {
      paste0("fallo la descarga (codigo ", descarga, ")")
    }
    return(list(ok = FALSE, razon = razon))
  }
  list(ok = TRUE, ruta = destino, url = url, origen = "descarga",
       archivo = etiqueta)
}

.version_archivo <- function(info) {
  if (!isTRUE(info$ok)) return(data.frame())
  metadatos <- file.info(info$ruta)
  data.frame(
    archivo = info$archivo,
    url = info$url,
    origen = info$origen,
    bytes = as.numeric(metadatos$size),
    adler32 = .huella_adler32(info$ruta),
    stringsAsFactors = FALSE
  )
}

.comparar_dirty_clean <- function(dataset, sucia, limpia, versiones = data.frame(),
                                  fuente = NA_character_) {
  if (!identical(dim(sucia), dim(limpia))) {
    stop(dataset, ": dirty y clean tienen dimensiones distintas.",
         call. = FALSE)
  }
  nombres <- names(sucia)
  names(limpia) <- nombres
  matriz_sucia <- as.matrix(sucia)
  matriz_limpia <- as.matrix(limpia)
  distintas <- matriz_sucia != matriz_limpia
  faltante_sucio <- is.na(matriz_sucia)
  faltante_limpio <- is.na(matriz_limpia)
  distintas[is.na(distintas)] <-
    (faltante_sucio != faltante_limpio)[is.na(distintas)]
  por_columna <- colSums(distintas)
  posiciones <- which(distintas, arr.ind = TRUE)
  verdad <- if (length(posiciones)) {
    data.frame(
      fila = posiciones[, "row"],
      columna_indice = posiciones[, "col"],
      columna = nombres[posiciones[, "col"]],
      valor_sucio = matriz_sucia[posiciones],
      valor_limpio = matriz_limpia[posiciones],
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      fila = integer(), columna_indice = integer(), columna = character(),
      valor_sucio = character(), valor_limpio = character(),
      stringsAsFactors = FALSE
    )
  }
  list(
    dataset = dataset,
    fuente = fuente,
    disponible = TRUE,
    sucia = sucia,
    limpia = limpia,
    verdad = verdad,
    columnas_afectadas = names(por_columna)[por_columna > 0L],
    por_columna = por_columna,
    resumen = data.frame(
      banco = dataset,
      filas = nrow(sucia),
      columnas = ncol(sucia),
      celdas_verdad = sum(distintas),
      filas_afectadas = sum(rowSums(distintas) > 0L),
      tasa_celdas_verdad = sum(distintas) / length(distintas),
      columnas_afectadas = sum(por_columna > 0L),
      duplicados_sucio = sum(duplicated(sucia)),
      vacias_sucio_con_limpio = sum(
        matriz_sucia == "" & matriz_limpia != "", na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    ),
    versiones = versiones
  )
}

.no_disponible <- function(banco, fuente, razon) {
  list(
    banco = banco,
    fuente = fuente,
    disponible = FALSE,
    razon = razon,
    resumen = data.frame(
      banco = banco, filas = NA_integer_, columnas = NA_integer_,
      celdas_verdad = NA_integer_, filas_afectadas = NA_integer_,
      tasa_celdas_verdad = NA_real_, columnas_afectadas = NA_integer_,
      duplicados_sucio = NA_integer_, vacias_sucio_con_limpio = NA_integer_,
      stringsAsFactors = FALSE
    ),
    versiones = data.frame()
  )
}

.imprimir_estado <- function(referencia, silencioso = FALSE) {
  if (isTRUE(silencioso)) return(invisible(NULL))
  if (!isTRUE(referencia$disponible)) {
    cat(referencia$banco, ": no medido: ", referencia$razon, "\n", sep = "")
    return(invisible(NULL))
  }
  x <- referencia$resumen
  cat(sprintf(
    "%s %d x %d; celdas verdad: %d; filas afectadas: %d; columnas afectadas: %d\n",
    x$banco, x$filas, x$columnas, x$celdas_verdad, x$filas_afectadas,
    x$columnas_afectadas
  ))
  if (nrow(referencia$versiones)) print(referencia$versiones, row.names = FALSE)
}

.mascara_verdad_larga <- function(ruta, nfilas, ncolumnas,
                                  nombres_columnas = character()) {
  lecturas <- list(
    tryCatch(.leer_csv_texto(ruta), error = function(e) NULL),
    tryCatch(utils::read.csv(
      ruta, header = FALSE, colClasses = "character", na.strings = NULL,
      check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8"
    ), error = function(e) NULL)
  )
  dimensiones <- c(as.integer(nfilas), as.integer(ncolumnas))
  exacta <- vapply(lecturas, function(y) {
    !is.null(y) && all(dim(y) == dimensiones)
  }, logical(1L))
  x <- if (any(exacta)) lecturas[[which(exacta)[[1L]]]] else lecturas[[1L]]
  if (is.null(x)) return(NULL)
  atajo_dimensiones_exactas <- function() {
    valores <- tolower(trimws(as.character(as.matrix(x))))
    marcado <- valores %in% c("1", "true", "error", "anomalia", "anomaly")
    ## Devolver el vector pelado dejaba la mascara sin `dim`, y quien la recibe
    ## compara dimensiones: el conjunto se descartaba entero.
    matrix(marcado, nrow = as.integer(nfilas), ncol = as.integer(ncolumnas))
  }
  nombres <- tolower(names(x))
  ## Formato ancho parcial: una fila por registro, una columna por atributo
  ## EVALUADO -no por todos-, mas una columna de indice. Es lo que publica
  ## RIOLU: marca anomalias solo en los atributos que su metodo mira, y el
  ## resto de la tabla queda fuera de la verdad. Tratarlo como verdad completa
  ## contaria como acierto lo que nadie etiqueto.
  ## La columna de indice se elige por prioridad y se VALIDA. Confiar en el
  ## nombre descarta conjuntos enteros: `gt_movies.csv` trae `Index` y tambien
  ## un `id` que es una columna de datos con verdad propia, y exigir una unica
  ## coincidencia hacia que la rama no se ejecutara y el conjunto se perdiera
  ## sin declarar el motivo.
  .indice_plausible <- function(valores, nfilas) {
    enteros <- suppressWarnings(as.integer(trimws(as.character(valores))))
    if (!length(enteros) || anyNA(enteros)) return(FALSE)
    if (anyDuplicated(enteros)) return(FALSE)
    corridos <- if (min(enteros) == 0L) enteros + 1L else enteros
    all(corridos >= 1L) && all(corridos <= as.integer(nfilas))
  }
  candidatos <- c(which(nombres %in% c("index", "indice", "row_index")),
                  which(nombres == "id"))
  columna_indice <- integer()
  for (candidato in candidatos) {
    if (.indice_plausible(x[[candidato]], nfilas)) {
      columna_indice <- candidato
      break
    }
  }
  if (length(columna_indice) == 1L && length(nombres_columnas)) {
    atributos <- setdiff(names(x), names(x)[columna_indice])
    if (length(atributos) && all(atributos %in% nombres_columnas)) {
      mascara <- matrix(FALSE, nrow = as.integer(nfilas),
                        ncol = as.integer(ncolumnas))
      colnames(mascara) <- nombres_columnas
      filas <- suppressWarnings(as.integer(x[[columna_indice]]))
      ## El indice puede empezar en 0 o en 1; se detecta en vez de suponerlo.
      if (length(filas) && !anyNA(filas) && min(filas) == 0L) {
        filas <- filas + 1L
      }
      validas <- !is.na(filas) & filas >= 1L & filas <= nrow(mascara)
      for (atributo in atributos) {
        marcado <- tolower(trimws(as.character(x[[atributo]]))) %in%
          c("1", "true", "error", "anomalia", "anomaly")
        mascara[filas[validas], atributo] <- marcado[validas]
      }
      attr(mascara, "columnas_evaluadas") <- atributos
      attr(mascara, "columnas_sin_verdad") <-
        setdiff(nombres_columnas, atributos)
      return(mascara)
    }
  }
  ## El atajo va DESPUES del formato ancho parcial. `gt_movies.csv` tiene tantas
  ## columnas como la tabla -indice mas cuatro atributos contra cinco columnas de
  ## datos-, y esa coincidencia hacia que se leyera como verdad completa,
  ## tomando el indice por una columna de datos.
  if (all(dim(x) == dimensiones)) return(atajo_dimensiones_exactas())
  columna_fila <- which(nombres %in% c("row", "fila", "row_id", "tuple_id"))
  columna_col <- which(nombres %in% c("column", "columna", "attribute"))
  columna_valor <- which(nombres %in% c("error", "label", "value", "is_error"))
  if (!length(columna_fila) || !length(columna_col) || !length(columna_valor)) {
    return(NULL)
  }
  salida <- matrix(FALSE, nrow = nfilas, ncol = ncolumnas)
  filas <- suppressWarnings(as.integer(x[[columna_fila[[1L]]]]))
  columnas <- x[[columna_col[[1L]]]]
  valores <- tolower(trimws(as.character(x[[columna_valor[[1L]]]])))
  columnas_texto <- trimws(as.character(columnas))
  columnas_numericas <- suppressWarnings(as.integer(columnas_texto))
  si_numericas <- !is.na(columnas_numericas) & columnas_texto != ""
  columnas_indice <- columnas_numericas
  if (any(!si_numericas)) {
    if (!length(nombres_columnas)) return(NULL)
    columnas_indice[!si_numericas] <- match(
      tolower(columnas_texto[!si_numericas]),
      tolower(nombres_columnas)
    )
  }
  conservar <- !is.na(filas) & !is.na(columnas_indice) &
    filas >= 1L & filas <= nfilas & columnas_indice >= 1L &
    columnas_indice <= ncolumnas & valores %in% c(
      "1", "true", "error", "anomalia", "anomaly"
    )
  salida[cbind(filas[conservar], columnas_indice[conservar])] <- TRUE
  salida
}

## PED publica difference.csv como una lista de celdas, con las columnas
## `Index` y `Attribute`; no hay una columna de etiqueta porque cada fila de
## ese archivo ya es una celda con error. Index es cero basado en la fuente.
.mascara_ped_difference <- function(ruta, nfilas, ncolumnas,
                                    nombres_columnas = character(),
                                    desplazamiento_fila = 1L) {
  x <- tryCatch(.leer_csv_texto(ruta), error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(NULL)
  nombres <- tolower(trimws(names(x)))
  columna_fila <- which(nombres %in% c(
    "index", "row", "fila", "row_id", "tuple_id"
  ))
  columna_col <- which(nombres %in% c(
    "attribute", "column", "columna", "attribute_name"
  ))
  if (!length(columna_fila) || !length(columna_col)) return(NULL)

  filas_texto <- trimws(as.character(x[[columna_fila[[1L]]]]))
  filas <- suppressWarnings(as.integer(filas_texto))
  atributos <- trimws(as.character(x[[columna_col[[1L]]]]))
  columnas_numericas <- suppressWarnings(as.integer(atributos))
  columnas <- match(atributos, nombres_columnas)
  columnas[is.na(columnas)] <- match(
    tolower(atributos[is.na(columnas)]),
    tolower(nombres_columnas)
  )
  numericas <- !is.na(columnas_numericas) & nzchar(atributos)
  columnas[numericas] <- columnas_numericas[numericas]

  ## Los indices de PED son cero basados; R los necesita uno basados. El
  ## desplazamiento permite validar copias locales preparadas con indices R.
  filas <- filas + as.integer(desplazamiento_fila)
  if (any(columnas == 0L, na.rm = TRUE)) columnas <- columnas + 1L
  validas <- !is.na(filas) & !is.na(columnas) &
    filas >= 1L & filas <= nfilas & columnas >= 1L & columnas <= ncolumnas
  if (!all(validas)) return(NULL)

  salida <- matrix(FALSE, nrow = nfilas, ncol = ncolumnas)
  salida[cbind(filas, columnas)] <- TRUE
  salida
}

.aplicar_mascara_verdad <- function(referencia, mascara, datos) {
  if (!identical(dim(mascara), dim(datos))) return(NULL)
  posiciones <- which(mascara, arr.ind = TRUE)
  nombres <- names(datos)
  matriz <- as.matrix(datos)
  referencia$mascara_patron <- mascara
  referencia$verdad <- if (length(posiciones)) {
    data.frame(
      fila = posiciones[, "row"],
      columna_indice = posiciones[, "col"],
      columna = nombres[posiciones[, "col"]],
      valor_sucio = matriz[posiciones],
      valor_limpio = if (!is.null(referencia$limpia)) {
        as.matrix(referencia$limpia)[posiciones]
      } else {
        rep(NA_character_, nrow(posiciones))
      },
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      fila = integer(), columna_indice = integer(), columna = character(),
      valor_sucio = character(), valor_limpio = character(),
      stringsAsFactors = FALSE
    )
  }
  por_columna <- colSums(mascara)
  referencia$columnas_afectadas <- nombres[por_columna > 0L]
  referencia$resumen$celdas_verdad <- sum(mascara)
  referencia$resumen$filas_afectadas <- sum(rowSums(mascara) > 0L)
  referencia$resumen$tasa_celdas_verdad <- sum(mascara) / length(mascara)
  referencia$resumen$columnas_afectadas <- sum(por_columna > 0L)
  referencia
}

.celdas_desde_hallazgos <- function(perfil, nombres_columnas, tipos = NULL) {
  vacio <- data.frame(
    fila = integer(), columna_indice = integer(), columna = character(),
    stringsAsFactors = FALSE
  )
  hallazgos <- perfil$hallazgos
  if (is.null(hallazgos) || !nrow(hallazgos)) return(vacio)
  severidad <- as.character(hallazgos$severidad)
  indices <- which(!is.na(hallazgos$columna) & severidad != "ok")
  if (!is.null(tipos)) indices <- indices[
    as.character(hallazgos$tipo_hallazgo[indices]) %in% tipos
  ]
  if (!length(indices)) return(vacio)
  celdas <- list()
  k <- 0L
  for (i in indices) {
    columnas <- trimws(unlist(strsplit(
      as.character(hallazgos$columna[[i]]), ",", fixed = TRUE),
      use.names = FALSE
    ))
    trazabilidad <- hallazgos$trazabilidad[[i]]
    filas <- if (is.list(trazabilidad)) trazabilidad$indices_fila else integer()
    if (!length(filas)) next
    for (nombre in columnas) {
      if (!nzchar(nombre)) next
      k <- k + 1L
      celdas[[k]] <- data.frame(
        fila = as.integer(filas),
        columna_indice = match(nombre, nombres_columnas),
        columna = nombre,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(celdas)) return(vacio)
  salida <- do.call(rbind, celdas)
  salida <- salida[!is.na(salida$columna_indice), , drop = FALSE]
  unique(salida)
}

.perfil_datos <- function(datos, tipos = NULL, muestra = Inf) {
  perfil <- lupa::perfilar(
    datos,
    muestra = muestra,
    max_filas_hallazgo = Inf,
    analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  predichas <- .celdas_desde_hallazgos(perfil, names(datos), tipos = tipos)
  hallazgos <- perfil$hallazgos
  no_ok <- hallazgos[!is.na(hallazgos$columna) &
    as.character(hallazgos$severidad) != "ok", , drop = FALSE]
  columnas <- unique(trimws(unlist(strsplit(
    as.character(no_ok$columna), ",", fixed = TRUE), use.names = FALSE
  )))
  list(perfil = perfil, celdas = predichas, columnas = columnas[nzchar(columnas)])
}

.medir_contra_mascara <- function(referencia, mascara, datos, tipos = NULL,
                                  alcance = "completo") {
  prediccion <- .perfil_datos(datos, tipos = tipos)
  verdad <- which(mascara, arr.ind = TRUE)
  celdas_verdad <- if (length(verdad)) data.frame(
    fila = verdad[, "row"], columna_indice = verdad[, "col"],
    columna = names(datos)[verdad[, "col"]], stringsAsFactors = FALSE
  ) else data.frame(
    fila = integer(), columna_indice = integer(), columna = character(),
    stringsAsFactors = FALSE
  )
  claves <- function(x) paste(x$fila, x$columna_indice, sep = ":")
  verdad_claves <- claves(celdas_verdad)
  pred_claves <- claves(prediccion$celdas)
  aciertos <- intersect(verdad_claves, pred_claves)
  columnas_verdad <- unique(celdas_verdad$columna)
  columnas_hallazgo <- intersect(names(datos), prediccion$columnas)
  nombre_banco <- referencia$banco
  if (is.null(nombre_banco) || !length(nombre_banco)) {
    nombre_banco <- referencia$resumen$banco
  }
  data.frame(
    banco = nombre_banco,
    dataset = referencia$dataset %||% nombre_banco,
    alcance = alcance,
    filas = nrow(datos),
    columnas = ncol(datos),
    celdas_verdad = length(verdad_claves),
    celdas_con_hallazgo_trazable = length(pred_claves),
    celdas_acertadas_trazables = length(aciertos),
    precision_celdas_trazables = if (length(pred_claves)) {
      length(aciertos) / length(pred_claves)
    } else NA_real_,
    cobertura_celdas_verdad = if (length(verdad_claves)) {
      length(aciertos) / length(verdad_claves)
    } else NA_real_,
    columnas_verdad = length(columnas_verdad),
    columnas_con_hallazgo = length(columnas_hallazgo),
    columnas_verdad_con_hallazgo = length(intersect(
      columnas_verdad, columnas_hallazgo
    )),
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
