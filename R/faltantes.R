.cadenas_na_naniar_1_1_0 <- c(
  "missing", "NA", "N A", "N/A", "#N/A", "NA ", " NA", "N /A",
  "N / A", " N / A", "N / A ", "na", "n a", "n/a", "na ", " na",
  "n /a", "n / a", " a / a", "n / a ", "NULL", "null", "", "?",
  "*", "."
)
# [naniar](https://github.com/njtierney/naniar) publica los tres últimos valores
# escapados para expresiones regulares.
# Aquí se adaptan a literales porque la detección usa igualdad con %in%.

# La familia "no disponible" faltaba entera: la lista tenia `s/d`, `s/i` y `n/c`
# con sus formas, y `n/d` -de las abreviaturas mas comunes del espanol- no
# estaba en ninguna.
.cadenas_na_locales <- c(
  "n.a", "n.a.", "nan", "none", "nil", "not available",
  "s/d", "s.d", "s.d.", "s.i", "s.i.", "s/i",
  "n/d", "n.d", "n.d.", "no disponible",
  "sin dato", "sin datos", "sin informaci\u00f3n", "sin informacion",
  "no corresponde", "n/c", "-", ".", "..", "...", "[]"
)

# Las formas de DOS LETRAS SIN SEPARADOR son ambiguas y su veredicto depende de
# la COLUMNA, no del valor. Medido en el banco de datos reales: en una columna
# de codigos de estado de EE.UU. -51 valores distintos- `perfilar()` publicaba
# `faltantes_disfrazados` con severidad **error** y evidencia "NC (59); SD (7)",
# acusando a Carolina del Norte y Dakota del Sur. Pero en una columna de dos
# valores, `c("SI", "SD")`, ese mismo `SD` es "sin dato" y retirarlo del
# vocabulario es correcto: hay un test que lo exige desde antes.
#
# Lo que separa los dos casos es el TAMANO DEL VOCABULARIO: un sistema de
# codigos tiene muchos valores distintos; una columna con un centinela tiene
# pocos. Se reusa el mismo piso que el paquete ya usa para decidir si una
# cardinalidad es alta.
.cadenas_na_ambiguas <- c("sd", "nc", "nd")

.cadenas_na_locales_ambiguas_aplican <- function(normalizados) {
  distintos <- length(unique(normalizados[!is.na(normalizados)]))
  isTRUE(distintos < .min_distintos_alta_cardinalidad)
}

.numeros_na_locales <- c(-9, -99, -999, -9999, 999)

#' Sentinelas numéricos publicados por [naniar](https://github.com/njtierney/naniar)
#'
#' Vector para solicitar explícitamente la lista numérica completa publicada
#' por [naniar](https://github.com/njtierney/naniar). Sus autores incluyen a
#' [Nicholas Tierney](https://github.com/njtierney), [Di Cook](https://github.com/dicook),
#' [Miles McBain](https://github.com/milesmcbain) y [Colin Fay](https://github.com/ColinFay).
#' Incluye `66`, `77`,
#' `88` y `9999`, que pueden ser edades,
#' códigos o años legítimos y por eso no se aplican de forma predeterminada.
#' Se usa como `perfilar(datos, sentinelas_numericos = sentinelas_naniar)`.
#' Tanto este vector como las cadenas de ausencia incorporadas en el paquete
#' están congelados con referencia a [naniar](https://github.com/njtierney/naniar)
#' 1.1.0; no cambian según la versión instalada.
#'
#' @format Vector numérico de ocho elementos.
#' @source `naniar::common_na_numbers`, versión 1.1.0. Véase el
#' [repositorio de naniar](https://github.com/njtierney/naniar).
#' @docType data
#' @export
#' @seealso [perfilar()], [planificar_limpieza()]
#'
#' @examples
#' sentinelas_naniar
#' datos <- data.frame(codigo = c(1, 66, 9999))
#' perfil <- perfilar(
#'   datos, sentinelas_numericos = sentinelas_naniar,
#'   analizar_dependencias = FALSE
#' )
#' perfil$columnas[, c("columna", "n_faltantes_disfrazados")]
sentinelas_naniar <- c(-9, -99, -999, -9999, 9999, 66, 77, 88)

.cadenas_na <- function() {
  valores <- c(
    .cadenas_na_naniar_1_1_0, .cadenas_na_locales, .cadenas_na_ambiguas
  )
  unique(tolower(trimws(as.character(valores))))
}

.normalizar_sentinelas_numericos <- function(valores) {
  if (is.null(valores) || !length(valores)) return(numeric())
  numericos <- suppressWarnings(as.numeric(valores))
  numericos <- numericos[!is.na(numericos)]
  sort(unique(numericos))
}

.numeros_na <- function(valores = .numeros_na_locales) {
  .normalizar_sentinelas_numericos(valores)
}

.detectar_faltantes_disfrazados <- function(
    x, sentinelas_numericos = .numeros_na_locales,
    detectar_sentinelas_numericos = TRUE,
    cadenas_ausencia = NULL) {
  n <- length(x)
  if (!n) {
    return(list(
      n = 0L, proporcion = 0, mascara = logical(), evidencia = "",
      n_textuales = 0L, n_numericos = 0L
    ))
  }

  mascara <- rep(FALSE, n)
  mascara_textual <- rep(FALSE, n)
  mascara_numerica <- rep(FALSE, n)
  numeros_na <- .numeros_na(sentinelas_numericos)
  if (is.character(x) || is.factor(x)) {
    textos <- .texto_analizable(x)$valores
    normalizados <- tolower(trimws(textos))
    cadenas <- .cadenas_na()
    if (!.cadenas_na_locales_ambiguas_aplican(normalizados)) {
      cadenas <- setdiff(cadenas, .cadenas_na_ambiguas)
    }
    # Lo declarado atraviesa la guarda del vocabulario, igual que un centinela
    # numerico declarado atraviesa la de la secuencia densa: el paquete no
    # tiene con que contradecir a quien conoce el dato.
    if (length(cadenas_ausencia)) {
      cadenas <- unique(c(
        cadenas, tolower(trimws(as.character(cadenas_ausencia)))
      ))
    }
    mascara_textual <- !is.na(normalizados) & normalizados %in% cadenas
    numericos <- suppressWarnings(as.numeric(normalizados))
    mascara_numerica <- detectar_sentinelas_numericos &
      !is.na(normalizados) & !is.na(numericos) & numericos %in% numeros_na
    mascara <- mascara_textual | mascara_numerica
    etiquetas <- textos[mascara]
    etiquetas[trimws(etiquetas) == ""] <- "<blanco>"
  } else if (is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) {
    mascara_numerica <- detectar_sentinelas_numericos &
      !is.na(x) & x %in% numeros_na
    mascara <- mascara_numerica
    etiquetas <- as.character(x[mascara])
  } else {
    etiquetas <- character()
  }

  # El mismo defecto que `R/patrones.R:87`, una funcion mas alla: `table()`
  # ordena sus niveles con la intercalacion del locale y `sort()` conserva ese
  # orden en los empates, asi que con ocho etiquetas de igual frecuencia y seis
  # lugares en la evidencia el desempate lo decidia la maquina. Medido: una
  # sesion publicaba `SIN DATO` y la otra no. Desempate por bytes.
  conteo_etiquetas <- table(etiquetas)
  tabla <- conteo_etiquetas[order(-as.integer(conteo_etiquetas),
                                 .clave_bytes(names(conteo_etiquetas)),
                                 method = "radix")]
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
    evidencia = evidencia,
    n_textuales = sum(mascara_textual),
    n_numericos = sum(mascara_numerica & !mascara_textual),
    # Las submascaras salen para que un universo aplicable declarado pueda
    # recortar los conteos sin volver a detectar.
    mascara_textual = mascara_textual,
    mascara_numerica = mascara_numerica & !mascara_textual
  )
}
