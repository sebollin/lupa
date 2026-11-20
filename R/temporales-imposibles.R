# Horas y fechas escritas como texto que tienen la forma pero no existen.
#
# `"25:10"`, `"12:61"` o `"2022-02-30"` no son horas ni fechas: son errores de
# carga. El perfil ya los veia -`tipo_declarado_distinto` cuenta cuantos valores
# quedarian en NA al convertir y `patron_raro` los saca como forma rara-, pero
# ninguno dice que esos valores no existen ni da sus filas.
#
# Es una pregunta sobre valores que YA se leen como hora o fecha, y se contesta
# solo cuando la inferencia de tipo de la columna dice `hora` o `fecha`: la
# columna es, en su inmensa mayoria, hora o fecha. Un texto cualquiera con forma
# de fecha -un codigo de lote `12.13.2020`- no entra, y una columna mezclada
# tampoco: ahi no hay un tipo de dato del que decir que algunos valores no
# existen.
#
# La hora valida es la misma que usa la inferencia de tipo; esta es la unica
# copia de esa expresion.
.EXPRESION_HORA_VALIDA <-
  "^(?:[01][0-9]|2[0-3]):[0-5][0-9](?::[0-5][0-9])?$"
.EXPRESION_FORMA_HORA <- "^[0-9]{2}:[0-9]{2}(?::[0-9]{2})?$"
# `24:00` y `24:00:00` son el fin del dia en ISO 8601 y algunos sistemas lo
# escriben asi: no se acusan, porque no son un error sino otra convencion.
.EXPRESION_FIN_DEL_DIA <- "^24:00(?::00)?$"

# Las palabras que dicen que la columna es una hora del dia y no una duracion:
# una hora de 24 o mas con minutos validos es un error en la primera y un dato
# normal en la segunda, y desde los valores no se distingue.
.PALABRAS_HORA_DEL_DIA <- c("hora", "horas", "horario", "hour", "hours", "time",
                            "hs", "hh")

# Con menos valores con forma de hora o fecha la proporcion no dice nada.
.MIN_VALORES_TEMPORAL_IMPOSIBLE <- 5L

# `nombre` decide si una hora de 24 o mas con minutos validos se acusa: solo en
# una columna que se llama como una hora del dia. Los minutos o segundos
# mayores que 59 son invalidos en cualquier columna.
.temporales_imposibles <- function(x, tipo_inferido, formatos = NULL,
                                   nombre = NA_character_) {
  if (!(is.character(x) || is.factor(x)) || .es_columna_compuesta(x) ||
      is.list(x)) {
    return(NULL)
  }
  tipo <- as.character(tipo_inferido)
  if (length(tipo) != 1L || !tipo %in% c("hora", "fecha")) return(NULL)
  valores <- tryCatch(as.character(x), error = function(e) NULL)
  if (is.null(valores)) return(NULL)
  posiciones <- which(!is.na(valores))
  if (length(posiciones) < .MIN_VALORES_TEMPORAL_IMPOSIBLE) return(NULL)
  texto <- valores[posiciones]
  unicos <- unique(texto)
  codigo <- match(texto, unicos)

  if (identical(tipo, "hora")) {
    forma <- grepl(.EXPRESION_FORMA_HORA, unicos, perl = TRUE, useBytes = TRUE)
    valida <- grepl(.EXPRESION_HORA_VALIDA, unicos, perl = TRUE, useBytes = TRUE)
    fin_dia <- grepl(.EXPRESION_FIN_DEL_DIA, unicos, perl = TRUE, useBytes = TRUE)
    horas_u <- suppressWarnings(as.integer(substr(unicos, 1L, 2L)))
    resto_u <- suppressWarnings(as.integer(substr(unicos, 4L, 5L)))
    segundos_u <- suppressWarnings(as.integer(substr(unicos, 7L, 8L)))
    mala_minutos <- forma & (resto_u > 59L | (!is.na(segundos_u) & segundos_u > 59L))
    mala_minutos[is.na(mala_minutos)] <- FALSE
    mala_hora <- forma & !valida & !fin_dia & horas_u >= 24L & !mala_minutos
    mala_hora[is.na(mala_hora)] <- FALSE
    nombre_de_hora <- any(.dominio_tokens(nombre) %in% .PALABRAS_HORA_DEL_DIA)
    imposible <- mala_minutos | (mala_hora & nombre_de_hora)
  } else {
    # Solo los formatos de dia con anio de cuatro cifras y sin hora: el anio de
    # dos cifras y la fecha compacta `20220230` se parecen demasiado a un codigo
    # o a un numero cualquiera, y la parte de hora tiene su propia regla.
    if (!is.data.frame(formatos) ||
        !all(c("formato", "granularidad", "anio_dos_digitos") %in%
               names(formatos))) {
      return(NULL)
    }
    elegidos <- formatos$granularidad == "dia" &
      !formatos$anio_dos_digitos &
      !grepl("%H", formatos$formato, fixed = TRUE, useBytes = TRUE) &
      !startsWith(formatos$formato, "%Y%m%d")
    elegidos[is.na(elegidos)] <- FALSE
    especificaciones <- .especificaciones_fecha()
    formas_elegidas <- unique(as.character(formatos$formato[elegidos]))
    formas_elegidas <- formas_elegidas[formas_elegidas %in% especificaciones$formato]
    if (!length(formas_elegidas)) return(NULL)
    forma <- rep(FALSE, length(unicos))
    valida <- rep(FALSE, length(unicos))
    for (formato in formas_elegidas) {
      expresion <- especificaciones$expresion[match(formato, especificaciones$formato)]
      forma <- forma | .es_fecha_valida(
        unicos, formato, expresion, solo_forma = TRUE
      )
      valida <- valida | .es_fecha_valida(unicos, formato, expresion)
    }
    imposible <- forma & !valida
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
