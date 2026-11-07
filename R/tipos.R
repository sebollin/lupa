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
#' lógica. Los literales se reconocen sin distinguir mayúsculas —`SÍ` es `sí`—
#' con el pliegue de caja de [normalizacion()], que no depende del locale.
#' Un separador seguido de exactamente tres dígitos (`1.500`, `1,500`) cuenta
#' como número sólo si la columna aporta evidencia de una sola convención
#' decimal, con punto o con coma por igual. Un número con los miles agrupados
#' —`1.234,56`, `1,234.56`, `10.000,00`— cuenta como número con la misma
#' condición: que la columna aporte evidencia de una sola convención, y que con
#' ella el valor agrupe los miles de a tres, que es como lo lee el resumen de
#' [perfilar()]. Sin esa evidencia, o con evidencia de las dos convenciones, no
#' cuenta.
#'
#' @param x Vector que se desea examinar. Una tabla —también la de una columna
#'   que da `tibble[, "f"]`— se rechaza: se pasa la columna, `tabla[["f"]]`.
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
  # En la funcion exportada: es la regla de la entrada publica. Los llamadores
  # internos ya le pasan una columna.
  .validar_vector_no_tabla(x)
  .inferir_tipo_interno(x, umbral, muestra, conservar_cache = FALSE)
}

.inferir_tipo_interno <- function(x, umbral = 0.8, muestra = 1e5,
                                  conservar_cache = TRUE) {
  if (length(umbral) != 1L || is.na(umbral) || umbral < 0 || umbral > 1) {
    .detener("`umbral` debe estar entre 0 y 1.", call. = FALSE)
  }
  muestreo <- .muestrear_vector(x, muestra)
  declarada <- .tipo_declarado(x)

  if (!is.character(x) && !is.factor(x)) {
    n <- sum(!is.na(muestreo$valores))
    candidatos <- .data_frame_simple(
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

  formas_minusculas <- .minusculas_para_comparar(formas, recortar = FALSE)
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
  # Un numero con los miles agrupados -`1.234,56`, `1,234.56`, `1.234.567`- no
  # casa con `dobles`, que admite un solo separador. Es la escritura corriente
  # de un importe en Uruguay, y una columna entera de ellos salia `texto`, sin
  # resumen, al lado de `numero_texto_seguro = TRUE` y de la convencion que la
  # misma fila publicaba; con nueve `1234,56` y un `1.234,56` salia `doble` y el
  # resumen leia ese `1.234,56` como 1234,56. Cuenta como numero lo que el
  # resumen lee: la decision es la de `.leer_numeros_texto()` con la convencion
  # de la misma evidencia, no una regla aparte que algun dia difiera.
  #
  # Solo con evidencia de UNA convencion, la misma condicion que la guarda de
  # arriba. Sin evidencia -una columna de `1.234.567`- no hay lectura. Con las
  # dos -`"mixta"`- el plan bloquea la conversion por falta de evidencia, y una
  # columna de importes agrupados de las dos maneras que pasara a `doble`
  # publicaria un `tipo_declarado_distinto` que dice que convertirla no pierde
  # nada, al lado de esa conversion bloqueada; sigue `texto`, como antes.
  agrupado <- !dobles & (
    grepl("^[+-]?[0-9]{1,3}(?:\\.[0-9]{3})+(?:,[0-9]+)?$", formas, perl = TRUE) |
      grepl("^[+-]?[0-9]{1,3}(?:,[0-9]{3})+(?:\\.[0-9]+)?$", formas, perl = TRUE)
  )
  if (any(separador_ambiguo) || any(agrupado)) {
    con_separador <- grepl("[.,]", formas, perl = TRUE)
    partes_numero <- .componentes_numero_texto(formas[con_separador])
    evidencia_coma <- any(partes_numero$evidencia_coma)
    evidencia_punto <- any(partes_numero$evidencia_punto)
    if (!xor(evidencia_coma, evidencia_punto)) {
      dobles[separador_ambiguo] <- FALSE
    }
    if (any(agrupado) && xor(evidencia_coma, evidencia_punto)) {
      # El nombre que `.interpretar_numeros_texto()` publica en
      # `numero_texto_convencion` con esa evidencia.
      convencion <- if (evidencia_coma) "decimal_coma" else "decimal_punto"
      dobles[agrupado] <- !is.na(
        .leer_numeros_texto(formas[agrupado], convencion)
      )
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
  candidatos <- .data_frame_simple(
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

# Lo que convertir la columna al tipo inferido perderia aunque todos sus valores
# sean compatibles. La evidencia de `tipo_declarado_distinto` decia "la
# conversion no pierde ningun valor" solo porque la proporcion compatible era 1,
# al lado de un plan que declaraba la misma conversion destructiva o la
# bloqueaba. Son las razones de ese plan, leidas del perfil y de la columna:
#
# - un numero escrito con cero inicial (`007`) lo pierde como numero: la regla
#   de `.evaluar_conversion()`, sobre la misma representacion;
# - un numero con mas cifras de las que guarda un `double` (el resumen ya lo
#   declara: `omitidos_precision`);
# - una convencion decimal no decidida (`ambigua`, `mixta`) obliga a elegir;
# - lecturas de fecha sin decidir -dia/mes, el siglo de un ano de dos cifras-;
# - dos escrituras distintas que quedan en el mismo valor -`2020-01-31` y
#   `31/01/2020`, `1` y `1.0`-: la conversion no es inyectiva. Mezclar formatos
#   sin que ninguno choque no pierde nada, y el plan la deja `lista`.
#
# Devuelve las frases para la evidencia, vacio si no hay ninguna, y `NULL` si no
# puede saberlo -sin la columna-: entonces la evidencia no afirma nada.
.riesgos_conversion_tipo <- function(x, fila, formatos = NULL) {
  tipo <- as.character(fila$tipo_inferido[[1L]])
  numero <- tipo %in% c("entero", "doble", "integer64")
  fecha <- tipo %in% c("fecha", "fecha-hora")
  if (!numero && !fecha) return(character())
  if (is.null(x) || !(is.character(x) || is.factor(x))) return(NULL)
  riesgos <- character()
  antes <- .texto_representacion_conversion(x)
  plural <- function(n, uno, varios) {
    paste0(.miles_trabajo(n), if (n == 1L) uno else varios)
  }
  if (numero) {
    ceros <- !is.na(antes) &
      grepl("^0[0-9]", .recortar_bytes(antes), perl = TRUE, useBytes = TRUE)
    if (any(ceros)) {
      riesgos <- c(riesgos, plural(
        sum(ceros), " valor tiene cero inicial y como n\u00famero lo pierde",
        " valores tienen cero inicial y como n\u00famero lo pierden"
      ))
    }
    if ("estado_resumen_cuantitativo" %in% names(fila) &&
        identical(as.character(fila$estado_resumen_cuantitativo[[1L]]),
                  "omitidos_precision")) {
      riesgos <- c(riesgos, paste0(
        "hay valores con m\u00e1s cifras de las que guarda un n\u00famero de ",
        "doble precisi\u00f3n"
      ))
    }
    convencion <- if ("numero_texto_convencion" %in% names(fila)) {
      as.character(fila$numero_texto_convencion[[1L]])
    } else ""
    if (length(convencion) == 1L && !is.na(convencion) &&
        convencion %in% c("ambigua", "mixta")) {
      riesgos <- c(riesgos, paste0(
        "la convenci\u00f3n decimal no est\u00e1 decidida (", convencion, ")"
      ))
      despues <- NULL
    } else {
      despues <- suppressWarnings(.leer_numeros_texto(
        x, if (nzchar(convencion)) convencion else NULL
      ))
    }
  } else {
    if (!is.data.frame(formatos) || !all(c("n", "estado") %in% names(formatos))) {
      return(NULL)
    }
    con_valores <- !is.na(formatos$n) & formatos$n > 0
    if (any(con_valores & formatos$estado %in% "candidato")) {
      riesgos <- c(riesgos, "hay valores con m\u00e1s de una lectura de fecha")
    }
    despues <- tryCatch(
      as.numeric(.parsear_fechas(x, formatos)), error = function(e) NULL
    )
    # `fecha` se guarda como `Date`: dos instantes del mismo dia son el mismo.
    if (!is.null(despues) && identical(tipo, "fecha")) {
      despues <- floor(despues / 86400)
    }
  }
  # Sin la lectura no se sabe si dos escrituras chocan: no se afirma nada.
  if (is.null(despues) || length(despues) != length(antes)) {
    return(if (length(riesgos)) riesgos else NULL)
  }
  # La misma cuenta que `.evaluar_conversion()`: cada valor de llegada con mas
  # de una escritura de partida.
  presentes <- !is.na(antes) & nzchar(.recortar_bytes(antes)) &
    !is.na(despues)
  if (any(presentes)) {
    claves <- .nombres_para_operar(antes)
    pares <- unique(data.frame(
      original = claves[presentes], convertido = despues[presentes],
      stringsAsFactors = FALSE
    ))
    repetidos <- pares$convertido[duplicated(pares$convertido)]
    chocan <- presentes & despues %in% repetidos
    if (any(chocan)) {
      riesgos <- c(riesgos, plural(
        sum(chocan),
        " valor escrito de otra forma queda igual a otro al convertir",
        " valores escritos de formas distintas quedan iguales al convertir"
      ))
    }
  }
  riesgos
}

# La reversibilidad de una conversión se evalúa sobre la representación que
# recibe el usuario, no sobre una igualdad numérica. Así se conservan ceros
# iniciales, separadores y formatos de fecha cuando una acción se ofrece como
# no destructiva.
# Un instante, con todas sus cifras: `format()` sin formato sigue a
# `options(digits.secs)`, y con el valor por omision dos instantes del mismo
# segundo eran una colision. Ver `.texto_instante_utc()`.
.texto_representacion_conversion <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(.texto_instante_utc(x))
  }
  # Declarada: se compara con `.nombres_para_operar()` y se recorta, y un
  # `unknown` junto a un `UTF-8` avisaba bajo `C`.
  .declarar_utf8_valido(as.character(x))
}
