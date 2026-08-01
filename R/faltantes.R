.cadenas_na_locales <- c(
  "", "na", "n/a", "#n/a", "n.a", "n.a.", "nan", "null", "none", "nil",
  "missing", "not available", "s/d", "sd", "s.i", "s.i.", "s/i",
  "sin dato", "sin datos", "sin informaci\u00f3n", "sin informacion",
  "no corresponde", "n/c", "nc", "-", ".", "..", "...", "[]"
)

.numeros_na_locales <- c(-9, -99, -999, -9999, 9999, 66, 77, 88)

.cadenas_na <- function() {
  valores <- .cadenas_na_locales
  if (requireNamespace("naniar", quietly = TRUE)) {
    externos <- tryCatch(
      getExportedValue("naniar", "common_na_strings"),
      error = function(e) character()
    )
    valores <- c(valores, externos)
  }
  unique(tolower(trimws(as.character(valores))))
}

.numeros_na <- function() {
  valores <- .numeros_na_locales
  if (requireNamespace("naniar", quietly = TRUE)) {
    externos <- tryCatch(
      getExportedValue("naniar", "common_na_numbers"),
      error = function(e) numeric()
    )
    valores <- c(valores, externos)
  }
  unique(as.numeric(valores))
}

.detectar_faltantes_disfrazados <- function(x) {
  n <- length(x)
  if (!n) {
    return(list(n = 0L, proporcion = 0, mascara = logical(), evidencia = ""))
  }

  mascara <- rep(FALSE, n)
  if (is.character(x) || is.factor(x)) {
    normalizados <- tolower(trimws(as.character(x)))
    mascara <- !is.na(normalizados) & normalizados %in% .cadenas_na()
    numericos <- suppressWarnings(as.numeric(normalizados))
    mascara <- mascara |
      (!is.na(normalizados) & !is.na(numericos) & numericos %in% .numeros_na())
    etiquetas <- as.character(x[mascara])
    etiquetas[trimws(etiquetas) == ""] <- "<blanco>"
  } else if (is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) {
    mascara <- !is.na(x) & x %in% .numeros_na()
    etiquetas <- as.character(x[mascara])
  } else {
    etiquetas <- character()
  }

  tabla <- sort(table(etiquetas), decreasing = TRUE)
  evidencia <- if (length(tabla)) {
    paste0(names(utils::head(tabla, 6L)), " (", as.integer(utils::head(tabla, 6L)), ")",
           collapse = "; ")
  } else {
    ""
  }
  cantidad <- sum(mascara)
  list(
    n = cantidad,
    proporcion = cantidad / n,
    mascara = mascara,
    evidencia = evidencia
  )
}
