.es_fecha_valida <- function(x, formato, expresion) {
  coincide <- grepl(expresion, x, perl = TRUE)
  valido <- rep(FALSE, length(x))
  if (any(coincide)) {
    convertido <- strptime(x[coincide], format = formato, tz = "UTC")
    valido[coincide] <- !is.na(convertido)
  }
  valido
}

.especificaciones_fecha <- function() {
  bases <- data.frame(
    formato = c(
      "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y",
      "%Y/%m/%d", "%d.%m.%Y", "%Y%m%d"
    ),
    expresion = c(
      "[0-9]{4}-[0-9]{2}-[0-9]{2}",
      "[0-9]{2}/[0-9]{2}/[0-9]{4}",
      "[0-9]{2}/[0-9]{2}/[0-9]{4}",
      "[0-9]{2}-[0-9]{2}-[0-9]{4}",
      "[0-9]{4}/[0-9]{2}/[0-9]{2}",
      "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}",
      "[0-9]{8}"
    ),
    ambiguo = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  sufijos <- data.frame(
    formato = c("", " %H:%M", " %H:%M:%S", "T%H:%M", "T%H:%M:%S"),
    expresion = c(
      "", " [0-9]{2}:[0-9]{2}", " [0-9]{2}:[0-9]{2}:[0-9]{2}",
      "T[0-9]{2}:[0-9]{2}", "T[0-9]{2}:[0-9]{2}:[0-9]{2}"
    ),
    stringsAsFactors = FALSE
  )

  resultado <- vector("list", nrow(bases) * nrow(sufijos))
  k <- 0L
  for (i in seq_len(nrow(bases))) {
    for (j in seq_len(nrow(sufijos))) {
      k <- k + 1L
      resultado[[k]] <- data.frame(
        formato = paste0(bases$formato[[i]], sufijos$formato[[j]]),
        expresion = paste0("^", bases$expresion[[i]], sufijos$expresion[[j]], "$"),
        ambiguo = bases$ambiguo[[i]],
        grupo_ambiguo = if (bases$ambiguo[[i]]) paste0("barra", j) else "",
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, resultado)
}

.fila_formato <- function(formato, n, total, estado = "confirmado",
                          n_inequivocos = n, n_ambiguos = 0L,
                          grupo_ambiguo = "") {
  data.frame(
    formato = formato,
    n = as.integer(n),
    proporcion = if (total) n / total else NA_real_,
    estado = estado,
    n_inequivocos = as.integer(n_inequivocos),
    n_ambiguos = as.integer(n_ambiguos),
    grupo_ambiguo = grupo_ambiguo,
    stringsAsFactors = FALSE
  )
}

#' Detectar formatos de fecha
#'
#' Reconoce formatos de fecha y fecha-hora sin escoger arbitrariamente entre
#' día/mes y mes/día. Cuando todos los valores con barras son ambiguos, devuelve
#' ambos formatos con estado `"candidato"`. El atributo `formatos_mixtos`
#' indica si hay evidencia de dos o más representaciones en la columna.
#'
#' @param x Vector de texto, fechas o fechas-hora.
#' @param muestra Máximo de valores que se analizan.
#'
#' @return Data frame con formato, frecuencia, proporción, estado y conteos de
#'   casos inequívocos y ambiguos. Los atributos informan el muestreo, la
#'   cantidad de valores compatibles y la presencia de formatos mixtos.
#' @export
#'
#' @examples
#' detectar_formatos_fecha(c("2020-01-31", "31/01/2020"))
#' detectar_formatos_fecha(c("01/02/2020", "02/03/2020"))
detectar_formatos_fecha <- function(x, muestra = 1e5) {
  muestra_x <- .muestrear_vector(x, muestra)
  valores_originales <- muestra_x$valores

  if (inherits(valores_originales, "POSIXt")) {
    n <- sum(!is.na(valores_originales))
    resultado <- .fila_formato("%Y-%m-%d %H:%M:%S", n, n)
    resultado$grupo_ambiguo <- NULL
    class(resultado) <- c("formatos_fecha", "data.frame")
    attr(resultado, "total") <- muestra_x$total
    attr(resultado, "analizados") <- muestra_x$analizados
    attr(resultado, "muestreado") <- muestra_x$muestreado
    attr(resultado, "compatibles") <- n
    attr(resultado, "formatos_mixtos") <- FALSE
    return(resultado)
  }
  if (inherits(valores_originales, "Date")) {
    n <- sum(!is.na(valores_originales))
    resultado <- .fila_formato("%Y-%m-%d", n, n)
    resultado$grupo_ambiguo <- NULL
    class(resultado) <- c("formatos_fecha", "data.frame")
    attr(resultado, "total") <- muestra_x$total
    attr(resultado, "analizados") <- muestra_x$analizados
    attr(resultado, "muestreado") <- muestra_x$muestreado
    attr(resultado, "compatibles") <- n
    attr(resultado, "formatos_mixtos") <- FALSE
    return(resultado)
  }

  valores <- trimws(as.character(valores_originales))
  valores <- valores[!is.na(valores) & nzchar(valores)]
  total <- length(valores)
  especificaciones <- .especificaciones_fecha()
  mascaras <- lapply(seq_len(nrow(especificaciones)), function(i) {
    .es_fecha_valida(
      valores,
      especificaciones$formato[[i]],
      especificaciones$expresion[[i]]
    )
  })
  names(mascaras) <- especificaciones$formato
  cubiertos <- rep(FALSE, total)
  filas <- list()
  k <- 0L

  no_ambiguos <- which(!especificaciones$ambiguo)
  for (i in no_ambiguos) {
    mascara <- mascaras[[i]]
    if (any(mascara)) {
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[i]], sum(mascara), total
      )
      cubiertos <- cubiertos | mascara
    }
  }

  grupos <- unique(especificaciones$grupo_ambiguo[especificaciones$ambiguo])
  for (grupo in grupos) {
    indices <- which(especificaciones$grupo_ambiguo == grupo)
    indice_dmy <- indices[grepl("^%d/", especificaciones$formato[indices])]
    indice_mdy <- indices[grepl("^%m/", especificaciones$formato[indices])]
    mascara_dmy <- mascaras[[indice_dmy]]
    mascara_mdy <- mascaras[[indice_mdy]]
    solo_dmy <- mascara_dmy & !mascara_mdy
    solo_mdy <- mascara_mdy & !mascara_dmy
    ambiguos <- mascara_dmy & mascara_mdy

    if (!any(mascara_dmy | mascara_mdy)) {
      next
    }
    cubiertos <- cubiertos | mascara_dmy | mascara_mdy

    if (!any(solo_dmy) && !any(solo_mdy)) {
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[indice_dmy]], sum(ambiguos), total,
        estado = "candidato", n_inequivocos = 0L,
        n_ambiguos = sum(ambiguos), grupo_ambiguo = grupo
      )
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[indice_mdy]], sum(ambiguos), total,
        estado = "candidato", n_inequivocos = 0L,
        n_ambiguos = sum(ambiguos), grupo_ambiguo = grupo
      )
    } else if (any(solo_dmy) && !any(solo_mdy)) {
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[indice_dmy]], sum(mascara_dmy), total,
        n_inequivocos = sum(solo_dmy), n_ambiguos = sum(ambiguos)
      )
    } else if (!any(solo_dmy) && any(solo_mdy)) {
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[indice_mdy]], sum(mascara_mdy), total,
        n_inequivocos = sum(solo_mdy), n_ambiguos = sum(ambiguos)
      )
    } else {
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[indice_dmy]], sum(solo_dmy), total,
        n_inequivocos = sum(solo_dmy), n_ambiguos = sum(ambiguos)
      )
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[indice_mdy]], sum(solo_mdy), total,
        n_inequivocos = sum(solo_mdy), n_ambiguos = sum(ambiguos)
      )
    }
  }

  if (length(filas)) {
    resultado <- do.call(rbind, filas)
    resultado <- resultado[order(-resultado$n, resultado$formato), , drop = FALSE]
    rownames(resultado) <- NULL
  } else {
    resultado <- data.frame(
      formato = character(), n = integer(), proporcion = numeric(),
      estado = character(), n_inequivocos = integer(), n_ambiguos = integer(),
      grupo_ambiguo = character(), stringsAsFactors = FALSE
    )
  }

  unidades <- character()
  if (nrow(resultado)) {
    confirmados <- resultado$estado == "confirmado"
    unidades <- c(unidades, resultado$formato[confirmados])
    candidatos <- resultado$grupo_ambiguo[resultado$estado == "candidato"]
    unidades <- c(unidades, unique(candidatos[nzchar(candidatos)]))
  }
  mixtos <- length(unique(unidades)) >= 2L
  resultado$grupo_ambiguo <- NULL
  class(resultado) <- c("formatos_fecha", "data.frame")
  attr(resultado, "total") <- muestra_x$total
  attr(resultado, "analizados") <- muestra_x$analizados
  attr(resultado, "muestreado") <- muestra_x$muestreado
  attr(resultado, "compatibles") <- sum(cubiertos)
  attr(resultado, "formatos_mixtos") <- mixtos
  resultado
}

.parsear_fechas <- function(x, formatos = detectar_formatos_fecha(x)) {
  valores <- trimws(as.character(x))
  salida <- rep(as.POSIXct(NA, tz = "UTC"), length(valores))
  if (!nrow(formatos)) {
    return(salida)
  }
  confirmados <- formatos$formato[formatos$estado == "confirmado"]
  for (formato in confirmados) {
    pendientes <- is.na(salida) & !is.na(valores)
    if (!any(pendientes)) {
      break
    }
    convertido <- strptime(valores[pendientes], format = formato, tz = "UTC")
    valido <- !is.na(convertido)
    indices <- which(pendientes)[valido]
    salida[indices] <- as.POSIXct(convertido[valido], tz = "UTC")
  }
  salida
}
