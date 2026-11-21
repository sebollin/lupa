# Horas y fechas escritas como texto que tienen la forma pero no existen.
#
# `"25:10"`, `"12:61"` o `"2022-02-30"` no son horas ni fechas: son errores de
# carga. El perfil ya los veia -`tipo_declarado_distinto` cuenta cuantos valores
# quedarian en NA al convertir y `patron_raro` los saca como forma rara-, pero
# ninguno dice que esos valores no existen ni da sus filas.
#
# Es una pregunta sobre valores que YA se leen como hora o fecha, y se contesta
# solo cuando la inferencia de tipo de la columna dice `hora`, `fecha` o
# `fecha-hora`: la columna es, en su inmensa mayoria, hora o fecha. Un texto
# cualquiera con forma de fecha -un codigo de lote `12.13.2020`- no entra, y una
# columna mezclada tampoco: ahi no hay un tipo de dato del que decir que algunos
# valores no existen.
#
# La hora valida es la misma que usa la inferencia de tipo; esta es la unica
# copia de esa expresion. El segundo 60 solo existe como intercalar a las
# 23:59 y `strptime()` lo lee: que la inferencia lo cuente compatible evita
# que un unico 23:59:60 vuelva `tipo_declarado_distinto` una columna sana.
.EXPRESION_HORA_VALIDA <-
  "^(?:(?:[01][0-9]|2[0-3]):[0-5][0-9](?::[0-5][0-9])?|23:59:60)$"
.EXPRESION_FORMA_HORA <- "^[0-9]{2}:[0-9]{2}(?::[0-9]{2})?$"
# `24:00` y `24:00:00` son el fin del dia en ISO 8601 y algunos sistemas lo
# escriben asi: no se acusan, porque no son un error sino otra convencion.
.EXPRESION_FIN_DEL_DIA <- "^24:00(?::00)?$"

# Las palabras que dicen que la columna es una hora del dia y no una duracion:
# una hora de 24 o mas con minutos validos es un error en la primera y un dato
# normal en la segunda, y desde los valores no se distingue.
.PALABRAS_HORA_DEL_DIA <- c("hora", "horas", "horario", "hour", "hours", "time",
                            "hs", "hh")

# `24:00` exacto tambien es el fin del dia en una fecha-hora, con `T` o espacio
# y con zona horaria. La regla de horas ya lo acepta y no lo acusa; aca se
# normaliza a la medianoche del mismo dia solo para comprobar el calendario.
# Una fecha imposible con `24:00` sigue siendo imposible.
.EXPRESION_FIN_DEL_DIA_FECHA_HORA <-
  "(?:[ T])24:00(?::00(?:\\.[0-9]+)?)?(?:Z|[+-][0-9]{2}:[0-9]{2})?$"

# Con menos valores con forma de hora o fecha la proporcion no dice nada.
.MIN_VALORES_TEMPORAL_IMPOSIBLE <- 5L

# Una columna compacta es una fecha y no un codigo cuando casi todos sus
# valores de ocho cifras existen en el calendario, con un ano plausible. El
# corte se midio sobre codigos, telefonos y montos de ocho cifras: la
# proporcion de fechas validas por azar no llego al 1 %, y una columna de
# fechas con un 2 % de imposibles da 98 %. El 90 % deja margen para las dos
# cosas y sigue exigiendo que la columna sea, sin duda, una fecha.
.UMBRAL_FECHAS_COMPACTAS <- 0.9
# En una columna numerica, ademas, casi todos los valores presentes tienen que
# ser de ocho cifras: una columna mezclada con codigos cortos no se lee como
# fecha compacta.
.UMBRAL_COMPACTAS_NUMERICAS_OCHO <- 0.95
.MIN_FECHAS_COMPACTAS <- 5L

# La validez de una fecha compacta se comprueba con el mismo calendario que
# `.es_fecha_valida()` usa para `%Y%m%d`, pero sin pasar por `strptime()` una
# vez por valor: el texto ya tiene las tres partes en posiciones fijas.
.fechas_compactas_validas <- function(textos) {
  anios <- suppressWarnings(as.integer(substr(textos, 1L, 4L)))
  meses <- suppressWarnings(as.integer(substr(textos, 5L, 6L)))
  dias <- suppressWarnings(as.integer(substr(textos, 7L, 8L)))
  .fecha_mes_valida(dias, meses, anios, rep(FALSE, length(textos)))
}

# Forma y validez de los valores de texto que podrian ser una fecha compacta.
.fechas_compactas_texto <- function(valores) {
  forma <- !is.na(valores) &
    grepl("^[0-9]{8}$", valores, perl = TRUE, useBytes = TRUE)
  valida <- rep(FALSE, length(valores))
  if (any(forma)) {
    valida[forma] <- .fechas_compactas_validas(valores[forma])
  }
  list(forma = forma, valida = valida)
}

# Lo mismo para una columna numerica entera. Un valor fuera del rango de ocho
# cifras no es una fecha compacta: no entra en la forma ni en el denominador de
# la proporcion.
.fechas_compactas_numericas <- function(x) {
  valores <- suppressWarnings(as.numeric(x))
  presente <- !is.na(valores) & is.finite(valores) &
    valores == trunc(valores)
  forma <- presente & valores >= 1e7 & valores <= 99999999
  valida <- rep(FALSE, length(valores))
  # La proporcion de ocho cifras se mide antes de validar: en una columna
  # numerica comun -un monto, un conteo- los valores de ocho cifras son una
  # minoria y no hay por que probar el calendario de todos ellos.
  if (sum(forma) >= .MIN_FECHAS_COMPACTAS &&
      sum(forma) / max(1L, sum(presente)) >=
        .UMBRAL_COMPACTAS_NUMERICAS_OCHO) {
    valida[forma] <- .fechas_compactas_validas(
      sprintf("%08.0f", valores[forma])
    )
  }
  list(
    valores = valores, presente = presente, forma = forma, valida = valida
  )
}

# La columna es una fecha compacta si hay suficientes valores de ocho cifras y
# casi todos son fechas validas. En texto se mide solo entre los que tienen la
# forma; en numerico, ademas, los de ocho cifras tienen que ser casi toda la
# columna.
.es_fecha_compacta_confiable <- function(compactas, numerica = FALSE,
                                         pesos = NULL) {
  forma <- compactas$forma
  if (is.null(pesos)) pesos <- rep(1, length(forma))
  pesos <- rep(pesos, length.out = length(forma))
  n_forma <- sum(pesos[forma])
  if (n_forma < .MIN_FECHAS_COMPACTAS) return(FALSE)
  if (isTRUE(numerica) &&
      n_forma / max(1, sum(pesos[compactas$presente])) <
        .UMBRAL_COMPACTAS_NUMERICAS_OCHO) {
    return(FALSE)
  }
  sum(pesos[forma & compactas$valida]) / n_forma >= .UMBRAL_FECHAS_COMPACTAS
}

# La rama numerica de las fechas compactas. No hay formas escritas: el valor
# entero es la fecha. Solo se informa cuando la columna entera se lee como
# fecha compacta; un codigo, un telefono o un monto no llegan al umbral.
.fechas_compactas_imposibles <- function(x, tipo) {
  if (inherits(x, "integer64") && !.bit64_disponible()) return(NULL)
  compactas <- .fechas_compactas_numericas(x)
  if (!.es_fecha_compacta_confiable(compactas, numerica = TRUE)) return(NULL)
  imposible <- compactas$forma & !compactas$valida
  if (!any(imposible)) return(NULL)
  list(
    es_hora = FALSE,
    indices = which(imposible),
    n_evaluados = as.integer(sum(compactas$forma)),
    ejemplos = utils::head(sprintf("%.0f", compactas$valores[imposible]), 5L)
  )
}

# `nombre` decide si una hora de 24 o mas con minutos validos se acusa: solo en
# una columna que se llama como una hora del dia. Los minutos o segundos
# mayores que 59 son invalidos en cualquier columna.
.temporales_imposibles <- function(x, tipo_inferido, formatos = NULL,
                                   nombre = NA_character_) {
  if (.es_columna_compuesta(x) || is.list(x)) return(NULL)
  tipo <- as.character(tipo_inferido)
  if (length(tipo) != 1L ||
      !tipo %in% c("hora", "fecha", "fecha-hora", "entero", "doble",
                   "integer64")) {
    return(NULL)
  }
  if (!(is.character(x) || is.factor(x))) {
    if (!tipo %in% c("entero", "doble", "integer64")) return(NULL)
    # Un rango que no toca las ocho cifras no puede ser una fecha compacta. La
    # guarda evita convertir y recorrer enteros comunes en cada columna.
    if (!inherits(x, "integer64")) {
      rango <- suppressWarnings(range(x, na.rm = TRUE))
      if (!length(rango) || !is.finite(rango[[1L]]) ||
          !is.finite(rango[[2L]]) || rango[[2L]] < 1e7 ||
          rango[[1L]] > 99999999) {
        return(NULL)
      }
    }
    return(.fechas_compactas_imposibles(x, tipo))
  }
  # Los bytes que no son texto valido quedan en `NA` y no se evaluan: las
  # lecturas de meses y de codigos con forma de fecha usan expresiones que
  # abortan con ellos. Es la misma preparacion que usa la deteccion de formatos.
  valores <- tryCatch(.texto_analizable(x)$valores, error = function(e) NULL)
  if (is.null(valores)) return(NULL)
  posiciones <- which(!is.na(valores))
  if (length(posiciones) < .MIN_VALORES_TEMPORAL_IMPOSIBLE) return(NULL)
  texto <- valores[posiciones]
  unicos <- unique(texto)
  codigo <- match(texto, unicos)
  # En `perfilar_por()` la repeticion del centinela se mide en la columna
  # entera, como en el detector de faltantes: ver
  # `.CONTEXTO_POR$centinelas_fecha_columna`.
  centinelas_contexto <- NULL
  if (tipo %in% c("fecha", "fecha-hora")) {
    consultar <- .CONTEXTO_POR$centinelas_fecha_columna
    if (is.function(consultar) && length(nombre) == 1L && !is.na(nombre)) {
      centinelas_contexto <- consultar(nombre)
    }
  }

  if (identical(tipo, "hora")) {
    forma <- grepl(.EXPRESION_FORMA_HORA, unicos, perl = TRUE, useBytes = TRUE)
    valida <- grepl(.EXPRESION_HORA_VALIDA, unicos, perl = TRUE, useBytes = TRUE)
    fin_dia <- grepl(.EXPRESION_FIN_DEL_DIA, unicos, perl = TRUE, useBytes = TRUE)
    horas_u <- suppressWarnings(as.integer(substr(unicos, 1L, 2L)))
    resto_u <- suppressWarnings(as.integer(substr(unicos, 4L, 5L)))
    segundos_u <- suppressWarnings(as.integer(substr(unicos, 7L, 8L)))
    # El segundo 60 es el intercalar de ISO 8601 y solo existe a las 23:59.
    intercalar <- forma & !is.na(horas_u) & !is.na(resto_u) &
      !is.na(segundos_u) & horas_u == 23L & resto_u == 59L &
      segundos_u == 60L
    mala_minutos <- forma & (resto_u > 59L |
      (!is.na(segundos_u) & segundos_u > 59L & !intercalar))
    mala_minutos[is.na(mala_minutos)] <- FALSE
    mala_hora <- forma & !valida & !fin_dia & horas_u >= 24L & !mala_minutos
    mala_hora[is.na(mala_hora)] <- FALSE
    nombre_de_hora <- any(.dominio_tokens(nombre) %in% .PALABRAS_HORA_DEL_DIA)
    imposible <- mala_minutos | (mala_hora & nombre_de_hora)
  } else {
    # Se prueba la validez contra todas las formas de fecha, no solo los
    # formatos que la deteccion confirmo en la columna: una fecha imposible
    # puede estar escrita en un formato que ningun otro valor usa. Las
    # compactas `%Y%m%d` entran solo cuando la columna es, sin duda, una fecha
    # compacta: si no, un codigo de ocho digitos se acusaria como fecha.
    detectados <- character()
    if (is.data.frame(formatos) && "formato" %in% names(formatos)) {
      detectados <- unique(as.character(formatos$formato))
      detectados <- detectados[!is.na(detectados) & nzchar(detectados)]
    }
    incluir_compactas <- FALSE
    if (any(startsWith(detectados, "%Y%m%d"))) {
      incluir_compactas <- .es_fecha_compacta_confiable(
        .fechas_compactas_texto(unicos),
        pesos = tabulate(codigo, nbins = length(unicos))
      )
    }
    especificaciones <- .especificaciones_fecha()
    if (!incluir_compactas) {
      especificaciones <- especificaciones[
        !startsWith(especificaciones$formato, "%Y%m%d"), , drop = FALSE
      ]
    }
    if (!nrow(especificaciones)) return(NULL)
    # La `t` y la `z` de RFC 3339 en minuscula se suben como en la deteccion;
    # asi la forma y la validacion miran la misma escritura.
    forma_texto <- if (any(grepl(
      "[tz]", unicos, perl = TRUE, useBytes = TRUE
    ))) {
      .designadores_fecha_hora_mayuscula(unicos)
    } else {
      unicos
    }
    # El fin del dia se valida como la medianoche del mismo dia. La busqueda
    # barata evita la expresion completa en las columnas que no lo usan.
    fin_dia <- rep(FALSE, length(unicos))
    if (any(grepl("24:00", forma_texto, fixed = TRUE, useBytes = TRUE))) {
      fin_dia <- grepl(
        .EXPRESION_FIN_DEL_DIA_FECHA_HORA, forma_texto,
        perl = TRUE, useBytes = TRUE
      )
    }
    validables <- forma_texto
    if (any(fin_dia)) {
      validables[fin_dia] <- sub("24:00", "00:00", forma_texto[fin_dia],
                                 fixed = TRUE)
    }
    forma <- rep(FALSE, length(unicos))
    valida <- rep(FALSE, length(unicos))
    # Los meses escritos se leen con la misma deteccion que la inferencia:
    # `solo_forma = TRUE` conserva la forma aunque la fecha no exista, que es
    # justo lo que hay que acusar (`30 de febrero de 2022`). Sin una letra en
    # los valores no hay mes escrito que buscar.
    if (any(grepl("[[:alpha:]]", unicos, useBytes = TRUE))) {
      meses <- .parsear_meses_texto(unicos, solo_forma = TRUE)
      forma_mes <- !is.na(meses$dias) & !is.na(meses$meses) &
        !is.na(meses$anios)
      if (any(forma_mes)) {
        forma[forma_mes] <- TRUE
        valida[forma_mes & meses$validos] <- TRUE
      }
    }
    # Marca la forma y valida solo los valores de `objetivo`. Se llama primero
    # con los formatos detectados -y sus lecturas hermanas, como dia/mes y
    # mes/dia, que comparten expresion- y despues con todas las formas solo
    # para los valores que ningun formato detectado cubrio.
    marcar <- function(tabla, objetivo) {
      for (expresion in unique(as.character(tabla$expresion))) {
        filas <- tabla[tabla$expresion == expresion, , drop = FALSE]
        coincide <- grepl(
          expresion, forma_texto[objetivo], perl = TRUE, useBytes = TRUE
        )
        if (any(coincide)) forma[objetivo[coincide]] <<- TRUE
        indices <- objetivo[coincide & !valida[objetivo]]
        if (!length(indices)) next
        for (k in seq_len(nrow(filas))) {
          indices <- indices[!valida[indices]]
          if (!length(indices)) break
          ok <- .es_fecha_valida(
            validables[indices], filas$formato[[k]], expresion,
            filas$granularidad[[k]], segundos_intercalares = TRUE
          )
          valida[indices[ok]] <<- TRUE
        }
      }
    }
    if (length(detectados)) {
      expresiones <- unique(
        especificaciones$expresion[
          especificaciones$formato %in% detectados
        ]
      )
      marcar(
        especificaciones[
          especificaciones$expresion %in% expresiones, , drop = FALSE
        ],
        seq_along(unicos)
      )
    }
    restantes <- which(!forma)
    if (length(restantes)) marcar(especificaciones, restantes)
    imposible <- forma & !valida
    # Un `00-00-00` repetido es una marca de ausencia, no una fecha mal
    # escrita: la informa `faltantes_disfrazados`. Una aparicion sola sigue
    # siendo una fecha imposible. Fuera de `perfilar_por()` la repeticion se
    # mide sobre los valores unicos con su frecuencia; dentro, manda la columna
    # entera.
    centinelas_u <- if (is.null(centinelas_contexto)) {
      .centinelas_fecha_texto(
        unicos, pesos = tabulate(codigo, nbins = length(unicos))
      )
    } else {
      !is.na(unicos) & trimws(unicos) %in% centinelas_contexto
    }
    if (any(centinelas_u)) imposible <- imposible & !centinelas_u
  }
  n_forma <- sum(forma[codigo])
  if (n_forma < .MIN_VALORES_TEMPORAL_IMPOSIBLE || !any(imposible)) return(NULL)
  marca <- imposible[codigo]
  resultado <- list(
    es_hora = identical(tipo, "hora"),
    indices = posiciones[marca],
    n_evaluados = as.integer(n_forma),
    ejemplos = utils::head(unicos[imposible], 5L)
  )
  if (identical(tipo, "hora")) {
    cuenta <- tabulate(codigo, nbins = length(unicos))
    resultado$n_horas_altas <- sum(cuenta[imposible & !mala_minutos])
    resultado$n_minutos_segundos <- sum(cuenta[mala_minutos])
    resultado$n_fin_dia <- sum(cuenta[fin_dia])
  }
  resultado
}
