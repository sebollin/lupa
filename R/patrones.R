#' Descubrir patrones de formato
#'
#' Generaliza un vector de texto mediante la convención del *Pattern Finder*
#' de DataCleaner: `9` representa un dígito, `a` una letra minúscula y `A` una
#' letra mayúscula. Los símbolos y espacios se conservan literalmente.
#'
#' El cálculo aplica reemplazos vectorizados sobre el vector completo. Si el
#' vector supera `muestra`, usa una muestra sistemática reproducible y registra
#' esa decisión en los atributos del resultado.
#'
#' @param x Vector que se convertirá a texto.
#' @param distinguir_mayusculas Si es `TRUE`, distingue `a` de `A`.
#' @param expandir Si es `FALSE`, colapsa tokens repetidos (`9999` a `9+`).
#' @param max_patrones Número máximo de patrones que se muestran.
#' @param na.rm Si es `TRUE`, excluye los valores ausentes.
#' @param muestra Máximo de valores que se analizan.
#'
#' @return Un data frame de clase `patrones` con patrón, frecuencia, proporción
#'   y ejemplos. Los atributos `total`, `analizados` y `muestreado` describen
#'   el posible muestreo. Las proporciones siempre están en `[0, 1]`.
#' @export
#'
#' @examples
#' descubrir_patrones(
#'   c("2020-01-31", "2021-12-01", "31/01/2020"),
#'   expandir = TRUE
#' )
descubrir_patrones <- function(x,
                               distinguir_mayusculas = TRUE,
                               expandir = FALSE,
                               max_patrones = 20,
                               na.rm = TRUE,
                               muestra = 1e5) {
  if (length(max_patrones) != 1L || is.na(max_patrones) || max_patrones < 1) {
    stop("`max_patrones` debe ser un entero positivo.", call. = FALSE)
  }
  if (!is.atomic(x) && !is.factor(x)) {
    stop("`x` debe ser un vector at\u00f3mico.", call. = FALSE)
  }

  muestra_x <- .muestrear_vector(x, muestra)
  valores <- muestra_x$valores
  es_na <- is.na(valores)
  if (na.rm) {
    valores <- valores[!es_na]
    es_na <- rep(FALSE, length(valores))
  }

  textos <- as.character(valores)
  indices_validos <- which(!es_na)
  patrones <- rep(NA_character_, length(textos))

  if (length(indices_validos)) {
    generalizados <- textos[indices_validos]
    generalizados <- gsub("[[:digit:]]", "9", generalizados, perl = TRUE)
    if (isTRUE(distinguir_mayusculas)) {
      generalizados <- gsub("[[:lower:]]", "a", generalizados, perl = TRUE)
      generalizados <- gsub("[[:upper:]]", "A", generalizados, perl = TRUE)
    } else {
      generalizados <- gsub("[[:alpha:]]", "a", generalizados, perl = TRUE)
    }
    if (!isTRUE(expandir)) {
      generalizados <- gsub("9{2,}", "9+", generalizados, perl = TRUE)
      generalizados <- gsub("a{2,}", "a+", generalizados, perl = TRUE)
      generalizados <- gsub("A{2,}", "A+", generalizados, perl = TRUE)
    }
    patrones[indices_validos] <- generalizados
  }

  marcador_na <- "\001valor_ausente\001"
  para_tabla <- patrones
  para_tabla[is.na(para_tabla)] <- marcador_na
  frecuencias <- sort(table(para_tabla, useNA = "no"), decreasing = TRUE)
  denominador <- length(para_tabla)

  completo <- data.frame(
    patron = names(frecuencias),
    n = as.integer(frecuencias),
    proporcion = if (denominador) as.numeric(frecuencias) / denominador else numeric(),
    ejemplos = character(length(frecuencias)),
    stringsAsFactors = FALSE
  )

  if (nrow(completo)) {
    completo$patron[completo$patron == marcador_na] <- NA_character_
    for (i in seq_len(nrow(completo))) {
      patron_i <- completo$patron[[i]]
      coincide <- if (is.na(patron_i)) is.na(patrones) else patrones == patron_i
      ejemplos <- unique(textos[which(coincide)])
      ejemplos <- ejemplos[!is.na(ejemplos)]
      ejemplos <- utils::head(ejemplos, 3L)
      completo$ejemplos[[i]] <- paste(ejemplos, collapse = " | ")
    }
  }

  limite <- min(nrow(completo), floor(max_patrones))
  resultado <- completo[seq_len(limite), , drop = FALSE]
  rownames(resultado) <- NULL
  class(resultado) <- c("patrones", "data.frame")
  attr(resultado, "total") <- muestra_x$total
  attr(resultado, "analizados") <- muestra_x$analizados
  attr(resultado, "muestreado") <- muestra_x$muestreado
  attr(resultado, "patrones_completos") <- completo
  resultado
}
