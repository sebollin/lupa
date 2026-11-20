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

# `24:00` exacto tambien es el fin del dia en una fecha-hora, con `T` o espacio
# y con zona horaria. La regla de horas ya lo acepta y no lo acusa; aca se
# normaliza a la medianoche del mismo dia solo para comprobar el calendario.
# Una fecha imposible con `24:00` sigue siendo imposible.
.EXPRESION_FIN_DEL_DIA_FECHA_HORA <-
  "(?:[ T])24:00(?::00(?:\\.[0-9]+)?)?(?:Z|[+-][0-9]{2}:[0-9]{2})?$"

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
  if (length(tipo) != 1L ||
      !tipo %in% c("hora", "fecha", "fecha-hora")) {
    return(NULL)
  }
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
    # compactas `%Y%m%d` quedan fuera: un codigo de ocho digitos se parece
    # demasiado a una fecha y ya se excluia a proposito.
    especificaciones <- .especificaciones_fecha()
    especificaciones <- especificaciones[
      !startsWith(especificaciones$formato, "%Y%m%d"), , drop = FALSE
    ]
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
    detectados <- character()
    if (is.data.frame(formatos) && "formato" %in% names(formatos)) {
      detectados <- unique(as.character(formatos$formato))
      detectados <- detectados[!is.na(detectados) & nzchar(detectados)]
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
