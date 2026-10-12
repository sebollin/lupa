.estado_tipo_inferido_desde_formatos <- function(tipo, formatos) {
  if (!tipo %in% c("fecha", "fecha-hora") ||
      !is.data.frame(formatos) ||
      !all(c("n", "estado") %in% names(formatos))) {
    return(NA_character_)
  }
  con_valores <- !is.na(formatos$n) & formatos$n > 0
  if (!any(con_valores)) return(NA_character_)
  estados <- as.character(formatos$estado[con_valores])
  if (any(estados == "candidato", na.rm = TRUE)) {
    return("candidato")
  }
  if (!anyNA(estados) && all(estados == "confirmado")) {
    "confirmado"
  } else {
    NA_character_
  }
}

.inferencia <- function(tipo, compatibles, n, muestreo, candidatos,
                        formatos_fecha = NULL) {
  estructura <- list(
    tipo = tipo,
    proporcion = if (n) compatibles / n else NA_real_,
    compatibles = as.integer(compatibles),
    n_analizados = as.integer(n),
    muestreado = muestreo$muestreado,
    candidatos = candidatos,
    formatos_fecha = formatos_fecha,
    estado_tipo_inferido = .estado_tipo_inferido_desde_formatos(
      tipo, formatos_fecha
    )
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
#' `no`, `true` o `false`. Una letra suelta (`T`, `F`, `S`, `N`) cuenta como
#' literal sólo si aparece también su opuesto —`T` con `F` o `false`, `S` con
#' `N` o `no`, y al revés—: sola es un código, y una columna de `F` y `M` no es
#' lógica.
#' Un separador seguido de exactamente tres dígitos (`1.500`, `1,500`) cuenta
#' como número sólo si la columna aporta evidencia de una sola convención
#' decimal, con punto o con coma por igual.
#'
#' @param x Vector que se desea examinar.
#' @param umbral Proporción mínima para asignar un tipo implícito.
#' @param muestra Máximo de valores que se analizan.
#'
#' @return Lista de clase `inferencia_tipo` con `tipo`, `proporcion`, conteos,
#'   candidatos evaluados y, cuando corresponde, formatos de fecha. `n_analizados`
#'   y `muestreado` declaran cuántos valores sostienen la proporción. El resultado
#'   público no conserva cachés internos del perfilado.
#' @export
#' @seealso [detectar_formatos_fecha()], [descubrir_patrones()], [perfilar()]
#'
#' @examples
#' inferir_tipo(c("1", "2", "3"))
#' inferir_tipo(c("2020-01-01", "31/12/2020"))
inferir_tipo <- function(x, umbral = 0.8, muestra = 1e5) {
  .inferir_tipo_interno(x, umbral, muestra, conservar_cache = FALSE)
}

.inferir_tipo_interno <- function(x, umbral = 0.8, muestra = 1e5,
                                  conservar_cache = TRUE) {
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

  valores <- .texto_analizable(muestreo$valores)$valores
  valores <- trimws(valores)
  valores <- valores[!is.na(valores) & nzchar(valores)]
  n <- length(valores)
  if (!n) {
    candidatos <- data.frame(
      tipo = character(), compatibles = integer(), proporcion = numeric(),
      stringsAsFactors = FALSE
    )
    return(.inferencia("desconocido", 0L, 0L, muestreo, candidatos))
  }

  # La clasificacion barata -siete pasadas de expresiones regulares- se hace
  # sobre las FORMAS distintas y se pesa por frecuencia, porque las proporciones
  # cuentan filas y no formas. `.vocabulario_texto()` decide si conviene: cuando
  # no, devuelve el vector entero con `indices = seq_len(n)`, los pesos son todos
  # 1 y la cuenta es la de siempre. Por eso hay UN camino que a veces trabaja
  # sobre menos valores, y no dos: dos copias de los mismos siete patrones es
  # pedir que algun dia difieran.
  vocabulario <- .vocabulario_texto(valores, .umbral_vocabulario_barato)
  formas <- vocabulario$valores
  pesos <- tabulate(vocabulario$indices, nbins = length(formas))

  formas_minusculas <- tolower(formas)
  logicos_base <- formas_minusculas %in% c(
    "true", "false", "t", "f", "si", "s\u00ed", "s", "no", "n", "0", "1"
  )
  # Una letra suelta es un literal logico solo con su par -`T` con `F` o
  # `false`, `S` con `N` o `no`, y al reves-. Sola es un codigo: `F` y `M` es el
  # sexo de un plantel, no un logico, y con el umbral de 0,8 dieciseis `F` y
  # cuatro `M` salian `logico`, con un `tipo_declarado_distinto` falso; una
  # columna de puras `F` se habria convertido en `FALSE`. Es la regla del
  # `0`/`1` -una forma que suele ser codigo no prueba un logico por si sola-
  # aplicada a las letras.
  verdadero_tf <- any(formas_minusculas %in% c("t", "true"))
  falso_tf <- any(formas_minusculas %in% c("f", "false"))
  verdadero_sn <- any(formas_minusculas %in% c("s", "si", "s\u00ed"))
  falso_sn <- any(formas_minusculas %in% c("n", "no"))
  sin_par <- (formas_minusculas == "t" & !falso_tf) |
    (formas_minusculas == "f" & !verdadero_tf) |
    (formas_minusculas == "s" & !falso_sn) |
    (formas_minusculas == "n" & !verdadero_sn)
  logicos_base[sin_par] <- FALSE
  contiene_logico_alfabetico <- any(
    logicos_base & grepl("[[:alpha:]]", formas, perl = TRUE)
  )
  logicos <- if (contiene_logico_alfabetico) {
    logicos_base
  } else {
    rep(FALSE, length(formas))
  }
  enteros <- grepl("^[+-]?[0-9]+$", formas, perl = TRUE)
  dobles <- grepl(
    "^[+-]?(?:[0-9]+(?:[.,][0-9]+)?|[.,][0-9]+)(?:[eE][+-]?[0-9]+)?$",
    formas, perl = TRUE
  )
  # Un separador seguido por exactamente tres digitos -`1.500`, `1,500`- es un
  # numero solo si la columna decide la convencion: puede ser mil quinientos o
  # uno y medio, y elegir seria inventar. La evidencia es la MISMA que publica
  # `numero_texto_convencion` -`.componentes_numero_texto()`- y vale para los
  # dos separadores por igual. Antes la guarda existia solo para el punto, y la
  # decidia CUALQUIER coma de la columna: un `2,000` -tan ambiguo como el
  # `1.500`- bastaba, y `1,500` no tenia guarda. La columna de miles con coma
  # salia `doble` con su resumen leido como decimales, mientras la gemela con
  # punto salia `texto`, sin resumen. Con evidencia de una sola convencion esas
  # formas si son numeros, y el resumen las lee con ella.
  separador_ambiguo <- dobles &
    grepl("^[+-]?[0-9]{1,3}[.,][0-9]{3}$", formas, perl = TRUE)
  if (any(separador_ambiguo)) {
    con_separador <- grepl("[.,]", formas, perl = TRUE)
    partes_numero <- .componentes_numero_texto(formas[con_separador])
    if (!xor(any(partes_numero$evidencia_coma),
             any(partes_numero$evidencia_punto))) {
      dobles[separador_ambiguo] <- FALSE
    }
  }
  horas <- grepl(
    "^(?:[01][0-9]|2[0-3]):[0-5][0-9](?::[0-5][0-9])?$",
    formas, perl = TRUE
  )

  formatos <- .detectar_formatos_fecha_interno(
    valores, muestra = muestra, vocabulario = vocabulario
  )
  # `valores` ya es la muestra de la columna. Restituir el alcance exterior
  # evita que el resultado del perfil declare como completo un vector
  # intermedio que ya perdió las filas no muestreadas.
  attr(formatos, "total") <- muestreo$total
  attr(formatos, "analizados") <- muestreo$analizados
  attr(formatos, "muestreado") <- muestreo$muestreado
  n_fechas <- attr(formatos, "compatibles")
  formatos_hora <- if (nrow(formatos)) grepl("%H", formatos$formato) else logical()
  tipo_fecha <- if (any(formatos_hora)) "fecha-hora" else "fecha"

  # `n_distintos` sale de `.vocabulario_texto()` ANTES de que decida si usa el
  # vocabulario, asi que vale en los dos casos.
  proporcion_distintos <- vocabulario$n_distintos / n
  forma_id <- grepl("^[[:alnum:]_.:/-]+$", formas, perl = TRUE)
  tiene_cero_inicial <- grepl("^0[0-9]+$", formas, perl = TRUE)
  tiene_letras_numeros <- grepl("[[:alpha:]]", formas, perl = TRUE) &
    grepl("[[:digit:]]", formas, perl = TRUE)
  estructura_id <- forma_id & (tiene_cero_inicial | tiene_letras_numeros)
  n_id <- sum(estructura_id * pesos)

  conteos <- c(
    logico = sum(logicos * pesos),
    entero = sum(enteros * pesos),
    doble = sum(dobles * pesos),
    fecha = if (identical(tipo_fecha, "fecha")) n_fechas else 0L,
    `fecha-hora` = if (identical(tipo_fecha, "fecha-hora")) n_fechas else 0L,
    hora = sum(horas * pesos),
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
  if (!conservar_cache) attr(formatos, "meses_texto") <- NULL
  .inferencia(tipo, compatibles, n, muestreo, candidatos, formatos)
}

#' @export
print.inferencia_tipo <- function(x, ...) {
  .cat_publicado(
    x$tipo, sprintf(
      " (%.1f%%; %d de %d valores compatibles)\n",
      100 * x$proporcion, x$compatibles, x$n_analizados
    )
  )
  invisible(x)
}

# La reversibilidad de una conversión se evalúa sobre la representación que
# recibe el usuario, no sobre una igualdad numérica. Así se conservan ceros
# iniciales, separadores y formatos de fecha cuando una acción se ofrece como
# no destructiva.
.texto_representacion_conversion <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(format(x, tz = "UTC", usetz = TRUE))
  }
  as.character(x)
}
