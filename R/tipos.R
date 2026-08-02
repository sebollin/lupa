.inferencia <- function(tipo, compatibles, n, muestreo, candidatos,
                        formatos_fecha = NULL) {
  estructura <- list(
    tipo = tipo,
    proporcion = if (n) compatibles / n else NA_real_,
    compatibles = as.integer(compatibles),
    n_analizados = as.integer(n),
    muestreado = muestreo$muestreado,
    candidatos = candidatos,
    formatos_fecha = formatos_fecha
  )
  class(estructura) <- "inferencia_tipo"
  estructura
}

#' Inferir el tipo implícito de un vector
#'
#' Para columnas de texto distingue valores lógicos, enteros, dobles, fechas,
#' fechas-hora, horas, identificadores y texto. Además de la etiqueta devuelve
#' la proporción de valores compatibles, siempre en la escala `[0, 1]`.
#' Los textos compuestos únicamente por `0` y `1` se consideran enteros; para
#' inferir un lógico debe aparecer al menos un literal alfabético como `sí`,
#' `no`, `true` o `false`.
#'
#' @param x Vector que se desea examinar.
#' @param umbral Proporción mínima para asignar un tipo implícito.
#' @param muestra Máximo de valores que se analizan.
#'
#' @return Lista de clase `inferencia_tipo` con `tipo`, `proporcion`, conteos,
#'   candidatos evaluados y, cuando corresponde, formatos de fecha.
#' @export
#' @seealso [detectar_formatos_fecha()], [descubrir_patrones()], [perfilar()]
#'
#' @examples
#' inferir_tipo(c("1", "2", "3"))
#' inferir_tipo(c("2020-01-01", "31/12/2020"))
inferir_tipo <- function(x, umbral = 0.8, muestra = 1e5) {
  if (length(umbral) != 1L || is.na(umbral) || umbral < 0 || umbral > 1) {
    stop("`umbral` debe estar entre 0 y 1.", call. = FALSE)
  }
  muestreo <- .muestrear_vector(x, muestra)
  declarada <- .tipo_declarado(x)

  if (!is.character(x) && !is.factor(x)) {
    n <- sum(!is.na(muestreo$valores))
    candidatos <- data.frame(
      tipo = declarada, compatibles = n,
      proporcion = if (n) 1 else NA_real_, stringsAsFactors = FALSE
    )
    return(.inferencia(declarada, n, n, muestreo, candidatos))
  }

  valores <- trimws(as.character(muestreo$valores))
  valores <- valores[!is.na(valores) & nzchar(valores)]
  n <- length(valores)
  if (!n) {
    candidatos <- data.frame(
      tipo = character(), compatibles = integer(), proporcion = numeric(),
      stringsAsFactors = FALSE
    )
    return(.inferencia("desconocido", 0L, 0L, muestreo, candidatos))
  }

  logicos_base <- tolower(valores) %in% c(
    "true", "false", "t", "f", "si", "s\u00ed", "s", "no", "n", "0", "1"
  )
  contiene_logico_alfabetico <- any(
    logicos_base & grepl("[[:alpha:]]", valores, perl = TRUE)
  )
  logicos <- if (contiene_logico_alfabetico) {
    logicos_base
  } else {
    rep(FALSE, length(valores))
  }
  enteros <- grepl("^[+-]?[0-9]+$", valores, perl = TRUE)
  dobles <- grepl(
    "^[+-]?(?:[0-9]+(?:[.,][0-9]+)?|[.,][0-9]+)(?:[eE][+-]?[0-9]+)?$",
    valores, perl = TRUE
  )
  punto_ambiguo <- grepl("^[+-]?[0-9]+\\.[0-9]{3}$", valores, perl = TRUE)
  if (!any(grepl(",", valores, fixed = TRUE))) {
    dobles[punto_ambiguo] <- FALSE
  }
  horas <- grepl(
    "^(?:[01][0-9]|2[0-3]):[0-5][0-9](?::[0-5][0-9])?$",
    valores, perl = TRUE
  )

  formatos <- detectar_formatos_fecha(valores, muestra = muestra)
  n_fechas <- attr(formatos, "compatibles")
  formatos_hora <- if (nrow(formatos)) grepl("%H", formatos$formato) else logical()
  tipo_fecha <- if (any(formatos_hora)) "fecha-hora" else "fecha"

  proporcion_distintos <- length(unique(valores)) / n
  forma_id <- grepl("^[[:alnum:]_.:/-]+$", valores, perl = TRUE)
  tiene_cero_inicial <- grepl("^0[0-9]+$", valores, perl = TRUE)
  tiene_letras_numeros <- grepl("[[:alpha:]]", valores, perl = TRUE) &
    grepl("[[:digit:]]", valores, perl = TRUE)
  estructura_id <- forma_id & (tiene_cero_inicial | tiene_letras_numeros)
  n_id <- sum(estructura_id)

  conteos <- c(
    logico = sum(logicos),
    entero = sum(enteros),
    doble = sum(dobles),
    fecha = if (identical(tipo_fecha, "fecha")) n_fechas else 0L,
    `fecha-hora` = if (identical(tipo_fecha, "fecha-hora")) n_fechas else 0L,
    hora = sum(horas),
    identificador = n_id,
    texto = n
  )
  candidatos <- data.frame(
    tipo = names(conteos),
    compatibles = as.integer(conteos),
    proporcion = as.numeric(conteos) / n,
    stringsAsFactors = FALSE
  )

  cumple <- function(nombre) conteos[[nombre]] / n >= umbral
  es_id <- proporcion_distintos >= 0.9 && n_id / n >= umbral
  if (cumple("logico")) {
    tipo <- "logico"
  } else if (n_fechas / n >= umbral) {
    tipo <- tipo_fecha
  } else if (cumple("hora")) {
    tipo <- "hora"
  } else if (es_id) {
    tipo <- "identificador"
  } else if (cumple("entero")) {
    tipo <- "entero"
  } else if (cumple("doble")) {
    tipo <- "doble"
  } else {
    tipo <- "texto"
  }

  compatibles <- if (identical(tipo, "fecha") || identical(tipo, "fecha-hora")) {
    n_fechas
  } else {
    conteos[[tipo]]
  }
  .inferencia(tipo, compatibles, n, muestreo, candidatos, formatos)
}

#' @export
print.inferencia_tipo <- function(x, ...) {
  cat(sprintf(
    "%s (%.1f%%; %d de %d valores compatibles)\n",
    x$tipo, 100 * x$proporcion, x$compatibles, x$n_analizados
  ))
  invisible(x)
}
