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
#' —`1.234,56`, `1,234.56`, `12.500.000`— cuenta como número si agrupa los miles
#' de a tres con la convención de la columna o, cuando la columna aporta
#' evidencia de las dos, con la que trae el propio valor (`1.234,56` sólo admite
#' la coma decimal): es como lo lee el resumen de [perfilar()]. Sin evidencia de
#' convención no cuenta, y un primer grupo de miles que empieza con cero
#' (`099.123.456`, la forma de un teléfono) no es un número en ninguna.
#' Un separador decimal que abre el número (`.5`, `,5`) se lee igual que `0.5`
#' o `0,5` y aporta la misma evidencia de convención; mezclado con la
#' convención contraria, la columna queda `mixta`. La notación científica
#' (`1e5`, `1.2E-05`, `1,5e3`) es un número, y no la forma de un identificador,
#' cualquiera sea el signo del exponente; un exponente que desborda el doble
#' (`1e400`) no lo es. Si la proporción de números alcanza el umbral y alguno
#' trae decimales, el tipo inferido es `doble` aunque la mayoría sean enteros:
#' es el tipo que los cubre a todos. Un número con unidad o moneda (`12 kg`,
#' `USD 35`) no cuenta como entero ni como doble: [perfilar()] lo informa como
#' número escrito como texto.
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
                                  conservar_cache = TRUE,
                                  convencion_numero_texto = NULL) {
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
  # Es doble lo que el lector comun lee -`.leer_numeros_texto()`, el mismo
  # del resumen y de la conversion de tipo-, con la convencion decimal de la
  # columna: la que el perfil publica en `numero_texto_convencion` cuando
  # llama `perfilar()`, o la de los valores analizados. La inferencia contaba
  # con una expresion propia y se desincronizo del resumen: `.5` junto a
  # `1.234,56` daba compatibilidad 1 con cuatro de nueve valores fuera del
  # resumen, y `3.5e2` en una columna de coma decimal se contaba sin leerse.
  # Lo que el lector no lee no cuenta: un separador seguido de tres digitos
  # sin evidencia de convencion (`1.500`), un grupo de miles que empieza con
  # cero (`099.123.456`) o un exponente que desborda el doble (`1e400`).
  # La convencion solo decide la lectura de una forma de numero que trae
  # separador: si no hay ninguna, no se calcula.
  if (is.null(convencion_numero_texto)) {
    con_separador <- grepl("[.,]", formas, perl = TRUE)
    convencion_numero_texto <- if (
      any(con_separador) && any(.formas_numero_texto(formas[con_separador]))
    ) {
      .analizar_numeros_texto(valores)$convencion
    } else ""
  }
  dobles <- is.finite(suppressWarnings(
    .leer_numeros_texto(formas, convencion_numero_texto)
  ))
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
    grepl("[[:digit:]]", formas, perl = TRUE) &
    !.forma_cientifica_numero_texto(formas)
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
  } else if (cumple("doble") && any(dobles & !enteros)) {
    # Con un decimal entre enteros el tipo es `doble`, que es el que cubre a
    # todos. Por ser enteros el 80 % salia `entero` con compatibilidad 0,8 y el
    # hallazgo anunciaba "convertir dejaria 1 de 5 valores en NA" sobre una
    # columna que el resumen leia entera y el plan convertia sin ningun `NA`.
    tipo <- "doble"
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
  if (!conservar_cache) {
    # Los dos intermedios de la deteccion: ninguno es parte de lo que
    # `inferir_tipo()` publica.
    attr(formatos, "meses_texto") <- NULL
    attr(formatos, "asignacion") <- NULL
  }
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
# - un numero con mas cifras de las que guarda un `double`: la regla del plan,
#   `.cifras_que_redondea_un_doble()`;
# - una convencion decimal no decidida (`ambigua`, `mixta`) obliga a elegir;
# - lecturas de fecha sin decidir -dia/mes, el siglo de un ano de dos cifras-;
# - dos escrituras distintas que quedan en el mismo valor -`2020-01-31` y
#   `31/01/2020`, `1` y `1.0`-: la conversion no es inyectiva. Mezclar formatos
#   sin que ninguno choque no pierde nada, y el plan la deja `lista`.
#
# `meses_texto`, los meses escritos que la inferencia ya leyo, para no volver a
# detectarlos.
#
# Devuelve las frases para la evidencia, vacio si no hay ninguna, y `NULL` si no
# puede saberlo -sin la columna-: entonces la evidencia no afirma nada.
.riesgos_conversion_tipo <- function(x, fila, formatos = NULL,
                                     meses_texto = NULL) {
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
    convencion <- if ("numero_texto_convencion" %in% names(fila)) {
      as.character(fila$numero_texto_convencion[[1L]])
    } else ""
    if (length(convencion) != 1L || is.na(convencion)) convencion <- ""
    indecisa <- convencion %in% c("ambigua", "mixta")
    leidos <- suppressWarnings(.leer_numeros_texto(
      x, if (nzchar(convencion)) convencion else NULL
    ))
    # Un entero fuera del rango de `integer` es compatible con el tipo y la
    # conversion del plan no lo representa: queda bloqueada, con los mismos
    # limites que `.convertir_tipo()`. La frase decia "no pierde ningun valor".
    if (identical(tipo, "entero")) {
      fuera_rango <- !is.na(leidos) & is.finite(leidos) &
        (leidos < -.Machine$integer.max | leidos > .Machine$integer.max)
      if (any(fuera_rango)) {
        riesgos <- c(
          riesgos,
          paste0(
            plural(
              sum(fuera_rango),
              " valor no entra en un entero",
              " valores no entran en un entero"
            ),
            "; la conversi\u00f3n a entero no se recomienda"
          )
        )
      }
    }
    # La precision con la regla del plan, sobre los numeros que el resumen lee:
    # esta frase miraba solo `omitidos_precision`, que el resumen declara para
    # los enteros mayores que 2^53, y dejaba afuera los decimales de 18 cifras
    # que el plan si marca destructivos. Ver `.cifras_que_redondea_un_doble()`.
    redondeados <- .cifras_que_redondea_un_doble(
      trimws(as.character(x)), leidos
    )
    if (any(redondeados)) {
      doble <- " de las que guarda un n\u00famero de doble precisi\u00f3n"
      riesgos <- c(riesgos, plural(
        sum(redondeados), paste0(" valor tiene m\u00e1s cifras", doble),
        paste0(" valores tienen m\u00e1s cifras", doble)
      ))
    } else if ("estado_resumen_cuantitativo" %in% names(fila) &&
               identical(as.character(fila$estado_resumen_cuantitativo[[1L]]),
                         "omitidos_precision")) {
      riesgos <- c(riesgos, paste0(
        "hay valores con m\u00e1s cifras de las que guarda un n\u00famero de ",
        "doble precisi\u00f3n"
      ))
    }
    if (indecisa) {
      riesgos <- c(riesgos, paste0(
        "la convenci\u00f3n decimal no est\u00e1 decidida (", convencion, ")"
      ))
    }
    # Con `numero_como_texto` el plan convierte por la via regional, que lee
    # menos que la inferencia: la notacion cientifica es un numero para el
    # lector comun -por eso la compatibilidad puede ser 1- y no responde al
    # formato regional. El perfil ya lo midio: son los valores que le faltan a
    # `proporcion_numeros_texto` para llegar a 1, y con uno solo la conversion
    # regional no se ejecuta. La frase decia "no pierde ningun valor" al lado
    # de esa accion bloqueada.
    if (.fila_con_numeros_como_texto(fila) &&
        isTRUE(fila[["proporcion_numeros_texto"]] < 1)) {
      riesgos <- c(riesgos, paste0(
        "hay valores presentes que no responden al formato num\u00e9rico ",
        "regional -la notaci\u00f3n cient\u00edfica, por ejemplo- y la ",
        "conversi\u00f3n regional no los lee"
      ))
    }
    # Con la convencion sin decidir el plan ya bloquea: no se cuentan choques.
    despues <- if (indecisa) NULL else leidos
  } else {
    if (!is.data.frame(formatos) || !all(c("n", "estado") %in% names(formatos))) {
      return(NULL)
    }
    con_valores <- !is.na(formatos$n) & formatos$n > 0
    if (any(con_valores & formatos$estado %in% "candidato")) {
      riesgos <- c(riesgos, "hay valores con m\u00e1s de una lectura de fecha")
    }
    despues <- tryCatch(
      as.numeric(.parsear_fechas(x, formatos, meses_texto = meses_texto)),
      error = function(e) NULL
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
    claves <- .nombres_para_operar(antes)[presentes]
    convertidos <- despues[presentes]
    # Los pares distintos por la posicion de cada lado, sin `unique()` sobre un
    # `data.frame`, que arma una lista por fila. La clave es un entero exacto
    # mientras el producto quepa en la mantisa; un complejo no sirve: su hash
    # combina las dos partes y, con las dos iguales, todas las claves chocan.
    n_pares <- length(claves)
    lado_a <- match(claves, claves)
    lado_b <- match(convertidos, convertidos)
    par <- if (n_pares <= 2^26) {
      lado_a + (lado_b - 1) * n_pares
    } else {
      paste(lado_a, lado_b)
    }
    convertidos <- convertidos[!duplicated(par)]
    repetidos <- convertidos[duplicated(convertidos)]
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

# Los valores que un `double` no guarda con las cifras con que vinieron escritos:
# la regla con que `.evaluar_conversion()` declara destructiva una conversion
# (`n_redondeados`). Un entero se compara contra su valor exacto -por encima de
# 2^53 el `double` guarda otro-; un decimal, a la precision con que vino
# escrito -`0,123456789012345678` vuelve `0,123456789012345677`-. Los digitos
# que cuentan son los de la mantisa, sin ceros en los bordes: los separadores y
# el exponente no cambian cuales son.
.cifras_que_redondea_un_doble <- function(texto, valores) {
  redondeados <- rep(FALSE, length(valores))
  if (length(texto) != length(valores)) return(redondeados)
  finitos <- !is.na(texto) & !is.na(valores) & is.finite(valores)
  # Corre sobre cada valor de la columna: con TRE y tres pasadas eran 0,3 s por
  # columna de cien mil filas.
  significativos <- function(x) {
    x <- gsub("[^0-9]", "", sub("[eE].*$", "", x, perl = TRUE), perl = TRUE)
    gsub("^0+|0+$", "", x, perl = TRUE)
  }
  enteros <- which(finitos & valores == floor(valores))
  if (length(enteros)) {
    antes <- significativos(texto[enteros])
    despues <- significativos(sprintf("%.0f", abs(valores[enteros])))
    redondeados[enteros] <- nzchar(antes) & antes != despues
  }
  decimales <- which(finitos & valores != floor(valores))
  if (length(decimales)) {
    antes <- significativos(texto[decimales])
    cifras <- pmax(1L, nchar(antes))
    despues <- significativos(sprintf("%.*g", cifras, abs(valores[decimales])))
    redondeados[decimales] <- nzchar(antes) & antes != despues
  }
  redondeados
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
