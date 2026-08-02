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
#' @param umbral_raro Umbral usado para conservar un resumen acotado de
#'   patrones raros para los hallazgos.
#'
#' @return Un data frame de clase `patrones` con patrón, frecuencia, proporción
#'   y ejemplos. Los atributos `total`, `analizados` y `muestreado` describen
#'   el posible muestreo. `resumen_patrones` conserva sólo el patrón dominante
#'   y hasta seis patrones raros; nunca guarda la distribución completa. Las
#'   proporciones siempre están en `[0, 1]`. `n_patrones_distintos` registra el
#'   total antes de truncar la tabla para informar omisiones sin retenerla.
#' @export
#' @seealso [perfilar()], [inferir_tipo()], [detectar_formatos_fecha()]
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
                               muestra = 1e5,
                               umbral_raro = 0.05) {
  if (length(max_patrones) != 1L || is.na(max_patrones) || max_patrones < 1) {
    stop("`max_patrones` debe ser un entero positivo.", call. = FALSE)
  }
  if (!is.atomic(x) && !is.factor(x)) {
    stop("`x` debe ser un vector at\u00f3mico.", call. = FALSE)
  }
  if (length(umbral_raro) != 1L || is.na(umbral_raro) ||
      umbral_raro < 0 || umbral_raro > 1) {
    stop("`umbral_raro` debe estar entre 0 y 1.", call. = FALSE)
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
  proporciones <- if (denominador) {
    as.numeric(frecuencias) / denominador
  } else {
    numeric()
  }
  limite <- min(length(frecuencias), floor(max_patrones))
  indices_salida <- seq_len(limite)
  indices_raros <- which(
    seq_along(frecuencias) > 1L & proporciones < umbral_raro
  )
  indices_raros <- utils::head(indices_raros, 6L)
  indices_resumen <- unique(c(
    if (length(frecuencias)) 1L else integer(),
    indices_raros
  ))
  indices_objetivo <- sort(unique(c(indices_salida, indices_resumen)))
  nombres_objetivo <- names(frecuencias)[indices_objetivo]
  ejemplos_objetivo <- rep("", length(indices_objetivo))

  if (length(indices_objetivo)) {
    grupos <- match(para_tabla, nombres_objetivo, nomatch = 0L)
    posiciones <- which(grupos > 0L & !is.na(textos))
    if (length(posiciones)) {
      textos_por_patron <- split(textos[posiciones], grupos[posiciones])
      indices_grupo <- as.integer(names(textos_por_patron))
      ejemplos_objetivo[indices_grupo] <- vapply(
        textos_por_patron,
        function(valores_grupo) {
          paste(utils::head(unique(valores_grupo), 3L), collapse = " | ")
        },
        character(1L)
      )
    }
  }

  crear_tabla <- function(indices) {
    nombres <- names(frecuencias)[indices]
    nombres[nombres == marcador_na] <- NA_character_
    data.frame(
      patron = nombres,
      n = as.integer(frecuencias[indices]),
      proporcion = proporciones[indices],
      ejemplos = ejemplos_objetivo[match(indices, indices_objetivo)],
      stringsAsFactors = FALSE
    )
  }

  resultado <- crear_tabla(indices_salida)
  resumen <- crear_tabla(indices_resumen)
  rownames(resultado) <- NULL
  rownames(resumen) <- NULL
  class(resultado) <- c("patrones", "data.frame")
  attr(resultado, "total") <- muestra_x$total
  attr(resultado, "analizados") <- muestra_x$analizados
  attr(resultado, "muestreado") <- muestra_x$muestreado
  attr(resultado, "n_patrones_distintos") <- length(frecuencias)
  attr(resultado, "resumen_patrones") <- resumen
  resultado
}
