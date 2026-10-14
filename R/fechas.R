.preparar_fecha_parseo <- function(x, formato) {
  if (!grepl("%z", formato, fixed = TRUE, useBytes = TRUE)) return(x)
  x <- sub("Z$", "+0000", x, perl = TRUE)
  sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x, perl = TRUE)
}

.es_fecha_valida <- function(x, formato, expresion, granularidad = "dia") {
  # Declarado, y solo lo legible: un texto que no es UTF-8 valido no es una
  # fecha en ninguna sesion, y bajo un locale UTF-8 hacia abortar el `grepl()`
  # (ronda 30-L).
  x <- .declarar_utf8_valido(as.character(x))
  legibles <- !is.na(x) & (Encoding(x) == "latin1" | validUTF8(x))
  coincide <- rep(FALSE, length(x))
  coincide[legibles] <- grepl(expresion, x[legibles], perl = TRUE)
  valido <- rep(FALSE, length(x))
  if (any(coincide)) {
    valores <- x[coincide]
    if (identical(granularidad, "mes")) {
      separador <- if (grepl("-", formato, fixed = TRUE, useBytes = TRUE)) "-" else "/"
      partes <- do.call(rbind, strsplit(valores, separador, fixed = TRUE))
      anios <- if (startsWith(formato, "%Y")) {
        suppressWarnings(as.integer(partes[, 1L]))
      } else {
        suppressWarnings(as.integer(partes[, 2L]))
      }
      meses <- if (startsWith(formato, "%Y")) {
        suppressWarnings(as.integer(partes[, 2L]))
      } else {
        suppressWarnings(as.integer(partes[, 1L]))
      }
      valido_convertido <- !is.na(anios) & !is.na(meses) &
        anios >= 1800L & anios <= 2100L & meses >= 1L & meses <= 12L
    } else {
      preparados <- .preparar_fecha_parseo(valores, formato)
      # `strptime()` avisa en ingles sobre un huso fuera de +/-14:00 y devuelve
      # NA: el aviso salia crudo de `perfilar()`, sin nombrar la columna. El NA
      # ya dice que el valor no es una fecha.
      convertido <- suppressWarnings(.strptime_utc(preparados, formato))
      valido_convertido <- !is.na(convertido)
      # Y acepta segundos hasta 61 -los intercalares-, que despues normaliza al
      # minuto siguiente: `23:59:60` del 2024-01-31 se publicaba como el
      # 1 de febrero, un dia que nadie escribio. Un segundo 60 o 61 no es una
      # hora que `POSIXct` pueda guardar sin cambiarla.
      if (grepl("%S", formato, fixed = TRUE, useBytes = TRUE) || grepl("%T", formato, fixed = TRUE, useBytes = TRUE)) {
        valido_convertido <- valido_convertido & !is.na(convertido$sec) &
          convertido$sec < 60
      }
      # La hora 24 es el mismo caso: `strptime()` acepta `24:00` y lo normaliza
      # a la medianoche del dia siguiente, y `2024-01-31 24:00` se publicaba
      # como el 1 de febrero. ISO 8601 la admite como fin del dia, pero el
      # paquete no publica una fecha civil que nadie escribio. Ya normalizado,
      # `convertido$hour` dice 0: la hora se lee del texto, los dos digitos
      # antes del primer `:` -la parte de fecha no tiene ninguno-.
      if (grepl("%H", formato, fixed = TRUE, useBytes = TRUE)) {
        horas <- suppressWarnings(as.integer(
          sub("^[^:]*([0-9]{2}):.*$", "\\1", valores, perl = TRUE)
        ))
        valido_convertido <- valido_convertido & !is.na(horas) & horas < 24L
      }
      if (startsWith(formato, "%Y%m%d")) {
        anios <- suppressWarnings(as.integer(substr(valores, 1L, 4L)))
        valido_convertido <- valido_convertido &
          anios >= 1800L & anios <= 2100L
      }
    }
    valido[coincide] <- valido_convertido
  }
  valido
}

.especificaciones_fecha <- function() {
  bases <- data.frame(
    formato = c(
      "%Y-%m", "%m/%Y", "%Y/%m",
      "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y",
      "%m-%d-%Y", "%Y/%m/%d", "%d.%m.%Y", "%m.%d.%Y",
      "%Y%m%d", "%d/%m/%y", "%m/%d/%y", "%d-%m-%y",
      "%m-%d-%y", "%d.%m.%y", "%m.%d.%y", "%y-%m-%d"
    ),
    expresion = c(
      "[0-9]{4}-[0-9]{1,2}",
      "[0-9]{1,2}/[0-9]{4}",
      "[0-9]{4}/[0-9]{1,2}",
      "[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}",
      "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}",
      "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}",
      "[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}",
      "[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}",
      "[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}",
      "[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}",
      "[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}",
      "[0-9]{8}",
      "[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}",
      "[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}",
      "[0-9]{1,2}-[0-9]{1,2}-[0-9]{2}",
      "[0-9]{1,2}-[0-9]{1,2}-[0-9]{2}",
      "[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{2}",
      "[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{2}",
      "[0-9]{2}-[0-9]{1,2}-[0-9]{1,2}"
    ),
    # `%y-%m-%d` va en el grupo de `%d-%m-%y` y `%m-%d-%y`: con guion y dos
    # cifras en cada parte, `01-02-03` es UNA escritura con tres lecturas. Fuera
    # del grupo contaba como una representacion aparte -`formatos_fecha_mixtos`,
    # severidad error, sobre una columna homogenea- y publicaba
    # `n_inequivocos = n` para valores que los otros dos formatos leen igual.
    grupo_base = c(
      "", "", "",
      "", "barra4", "barra4", "guion4", "guion4", "", "punto4",
      "punto4", "", "barra2", "barra2", "guion2", "guion2",
      "punto2", "punto2", "guion2"
    ),
    anio_dos_digitos = c(rep(FALSE, 12L), rep(TRUE, 7L)),
    granularidad = c(rep("mes", 3L), rep("dia", 16L)),
    stringsAsFactors = FALSE
  )
  sufijos <- data.frame(
    formato = c(
      "", " %H:%M", " %H:%M:%S", " %H:%M:%OS",
      "T%H:%M", "T%H:%M:%S", "T%H:%M:%OS",
      "T%H:%M:%S%z", "T%H:%M:%OS%z"
    ),
    expresion = c(
      "", " [0-9]{2}:[0-9]{2}", " [0-9]{2}:[0-9]{2}:[0-9]{2}",
      " [0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]+",
      "T[0-9]{2}:[0-9]{2}", "T[0-9]{2}:[0-9]{2}:[0-9]{2}",
      "T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]+",
      "T[0-9]{2}:[0-9]{2}:[0-9]{2}(?:Z|[+-][0-9]{2}:[0-9]{2})",
      "T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]+(?:Z|[+-][0-9]{2}:[0-9]{2})"
    ),
    stringsAsFactors = FALSE
  )

  resultado <- list()
  k <- 0L
  for (i in seq_len(nrow(bases))) {
    sufijos_aplicables <- if (identical(bases$granularidad[[i]], "mes")) {
      1L
    } else {
      seq_len(nrow(sufijos))
    }
    for (j in sufijos_aplicables) {
      k <- k + 1L
      resultado[[k]] <- data.frame(
        formato = paste0(bases$formato[[i]], sufijos$formato[[j]]),
        expresion = paste0("^", bases$expresion[[i]], sufijos$expresion[[j]], "$"),
        ambiguo = nzchar(bases$grupo_base[[i]]),
        anio_dos_digitos = bases$anio_dos_digitos[[i]],
        grupo_ambiguo = if (nzchar(bases$grupo_base[[i]])) {
          paste0(bases$grupo_base[[i]], "-", j)
        } else {
          ""
        },
        granularidad = bases$granularidad[[i]],
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, resultado)
}

.meses_fecha_lupa <- c(
  enero = 1L, ene = 1L, january = 1L, jan = 1L,
  febrero = 2L, feb = 2L, february = 2L,
  marzo = 3L, mar = 3L, march = 3L,
  abril = 4L, abr = 4L, april = 4L, apr = 4L,
  mayo = 5L, may = 5L,
  junio = 6L, jun = 6L, june = 6L,
  julio = 7L, jul = 7L, july = 7L,
  agosto = 8L, ago = 8L, august = 8L, aug = 8L,
  septiembre = 9L, sep = 9L, september = 9L,
  setiembre = 9L, set = 9L,
  octubre = 10L, oct = 10L, october = 10L,
  noviembre = 11L, nov = 11L, november = 11L,
  diciembre = 12L, dic = 12L, december = 12L, dec = 12L
)

.meses_fecha_regex <- paste(names(.meses_fecha_lupa), collapse = "|")

.fecha_mes_valida <- function(dia, mes, anio, dos_digitos) {
  anio_comprobacion <- ifelse(dos_digitos, 2000L + anio, anio)
  texto <- sprintf("%04d-%02d-%02d", anio_comprobacion, mes, dia)
  convertido <- as.Date(texto, format = "%Y-%m-%d")
  en_rango <- anio_comprobacion >= 1800L & anio_comprobacion <= 2100L
  !is.na(convertido) & en_rango & format(convertido, "%Y-%m-%d") == texto
}

.detectar_meses_regexec <- function(expresion, texto) {
  regexec(expresion, texto, perl = TRUE)
}

.detectar_meses_texto <- function(valores) {
  n <- length(valores)
  formatos <- rep("", n)
  dias <- rep(NA_integer_, n)
  meses <- rep(NA_integer_, n)
  anios <- rep(NA_integer_, n)
  dos_digitos <- rep(FALSE, n)
  validos <- rep(FALSE, n)
  nombres_mes <- rep(NA_character_, n)
  patrones <- list(
    list(
      expresion = paste0(
        "^([0-9]{1,2})[[:space:]]+de[[:space:]]+(",
        .meses_fecha_regex,
        ")[[:space:]]+de[[:space:]]+([0-9]{2}|[0-9]{4})$"
      ),
      formato = "de"
    ),
    list(
      expresion = paste0(
        "^([0-9]{1,2})[-[:space:]](", .meses_fecha_regex,
        ")[-[:space:]]([0-9]{2}|[0-9]{4})$"
      ),
      formato = "separado"
    ),
    list(
      expresion = paste0(
        "^(", .meses_fecha_regex,")[[:space:]]+([0-9]{1,2})",
        "(?:,)?[[:space:]]+([0-9]{2}|[0-9]{4})$"
      ),
      formato = "ingles"
    ),
    list(
      expresion = paste0(
        "^(", .meses_fecha_regex,")[[:space:]]+([0-9]{4})$"
      ),
      formato = "mes_anio"
    )
  )
  texto_original <- trimws(as.character(valores))
  # La fecha debe empezar por un día o por el nombre del mes. Además de
  # expresar la estructura completa, esta guarda evita cuatro pasadas de
  # expresiones regulares sobre texto libre que sólo menciona un mes.
  candidatos <- !is.na(texto_original) & grepl(
    .regex_sin_caja(paste0(
      "^(?:[0-9]{1,2}[[:space:]]+de[[:space:]]+|",
      "[0-9]{1,2}[-[:space:]]|(?:", .meses_fecha_regex,
      ")[[:space:]]+)"
    )), texto_original, perl = TRUE
  )
  if (any(candidatos)) {
    indices_candidatos <- which(candidatos)
    # Los nombres de mes son ASCII; se pliega la caja sólo en los candidatos,
    # sin consultar `LC_TIME` ni recorrer de nuevo el resto de la columna.
    texto <- texto_original[indices_candidatos]
    texto <- chartr(
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", texto
    )
    for (patron in patrones) {
      coincidencias <- regmatches(
        texto,
        .detectar_meses_regexec(patron$expresion, texto)
      )
      indices_locales <- which(
        !validos[indices_candidatos] & lengths(coincidencias) > 1L
      )
      if (!length(indices_locales)) next
      indices <- indices_candidatos[indices_locales]
      capturas <- coincidencias[indices_locales]
      if (identical(patron$formato, "de") ||
          identical(patron$formato, "separado")) {
        dias_candidatos <- as.integer(vapply(capturas, `[[`, character(1L), 2L))
        nombres_candidatos <- vapply(capturas, `[[`, character(1L), 3L)
        anios_texto <- vapply(capturas, `[[`, character(1L), 4L)
      } else if (identical(patron$formato, "ingles")) {
        nombres_candidatos <- vapply(capturas, `[[`, character(1L), 2L)
        dias_candidatos <- as.integer(vapply(capturas, `[[`, character(1L), 3L))
        anios_texto <- vapply(capturas, `[[`, character(1L), 4L)
      } else {
        nombres_candidatos <- vapply(capturas, `[[`, character(1L), 2L)
        dias_candidatos <- rep.int(1L, length(indices))
        anios_texto <- vapply(capturas, `[[`, character(1L), 3L)
      }
      meses_candidatos <- unname(.meses_fecha_lupa[nombres_candidatos])
      anios_candidatos <- as.integer(anios_texto)
      dos_candidatos <- nchar(anios_texto) == 2L
      validos_candidatos <- .fecha_mes_valida(
        dias_candidatos, meses_candidatos, anios_candidatos, dos_candidatos
      )
      if (!any(validos_candidatos)) next
      indices_validos <- indices[validos_candidatos]
      nombres_validos <- nombres_candidatos[validos_candidatos]
      dos_validos <- dos_candidatos[validos_candidatos]
      nombre_formato <- if (identical(patron$formato, "de")) {
        paste0("%d de %", ifelse(nchar(nombres_validos) > 3L, "B", "b"),
               " de %", ifelse(dos_validos, "y", "Y"))
      } else if (identical(patron$formato, "separado")) {
        separadores <- lapply(
          regmatches(
            texto[match(indices_validos, indices_candidatos)],
            regexec("^[0-9]{1,2}([-[:space:]])[^-[:space:]]+([-[:space:]])",
                    texto[match(indices_validos, indices_candidatos)], perl = TRUE)
          ),
          function(partes) partes[-1L]
        )
        paste0(
          "%d", vapply(separadores, `[[`, character(1L), 1L), "%",
          ifelse(nchar(nombres_validos) > 3L, "B", "b"),
          vapply(separadores, `[[`, character(1L), 2L), "%",
          ifelse(dos_validos, "y", "Y")
        )
      } else if (identical(patron$formato, "ingles")) {
        comas <- grepl(",", texto[match(indices_validos, indices_candidatos)],
                       fixed = TRUE, useBytes = TRUE)
        paste0(
          "%", ifelse(nchar(nombres_validos) > 3L, "B", "b"),
          " %d", ifelse(comas, ",", ""), " %",
          ifelse(dos_validos, "y", "Y")
        )
      } else {
        paste0("%", ifelse(nchar(nombres_validos) > 3L, "B", "b"), " %Y")
      }
      formatos[indices_validos] <- nombre_formato
      dias[indices_validos] <- dias_candidatos[validos_candidatos]
      meses[indices_validos] <- meses_candidatos[validos_candidatos]
      anios[indices_validos] <- anios_candidatos[validos_candidatos]
      dos_digitos[indices_validos] <- dos_validos
      validos[indices_validos] <- TRUE
      nombres_mes[indices_validos] <- nombres_validos
    }
  }
  # `%B` o `%b` se decide por el largo del nombre, y `may` es las dos cosas: el
  # nombre completo en ingles y la abreviatura de `mayo`. Etiquetado `%b`, una
  # columna inglesa de nombres completos con un mayo publicaba dos
  # representaciones -`formatos_fecha_mixtos`, severidad error-. Se le da la
  # etiqueta de sus vecinos: si la misma estructura aparece con `%B` y no con
  # `%b` en el resto de la columna, `may` es nombre completo; si no, queda como
  # estaba.
  es_may <- validos & !is.na(nombres_mes) & nombres_mes == "may"
  if (any(es_may) && any(validos & !es_may)) {
    resto <- unique(formatos[validos & !es_may])
    for (formato in unique(formatos[es_may])) {
      completo <- sub("%b", "%B", formato, fixed = TRUE)
      if (!identical(completo, formato) && completo %in% resto &&
          !formato %in% resto) {
        formatos[es_may & formatos == formato] <- completo
      }
    }
  }
  filas <- list()
  formatos_validos <- unique(formatos[validos])
  for (formato in formatos_validos) {
    indices <- which(validos & formatos == formato)
    dos <- any(dos_digitos[indices])
    filas[[length(filas) + 1L]] <- .fila_formato(
      formato, length(indices), n,
      estado = if (dos) "candidato" else "confirmado",
      anio_dos_digitos = dos,
      granularidad = if (formato %in% c("%B %Y", "%b %Y")) {
        "mes"
      } else {
        "dia"
      }
    )
  }
  list(
    formatos = formatos, dias = dias, meses = meses, anios = anios,
    anio_dos_digitos = dos_digitos, validos = validos,
    filas = if (length(filas)) do.call(rbind, filas) else NULL
  )
}

.ponderar_meses_texto <- function(meses, indices, pesos, total) {
  if (!length(indices)) return(meses)
  expandido <- meses
  for (campo in c(
    "formatos", "dias", "meses", "anios", "anio_dos_digitos", "validos"
  )) {
    expandido[[campo]] <- meses[[campo]][indices]
  }
  if (!is.null(meses$filas)) {
    n <- vapply(meses$filas$formato, function(formato) {
      sum(pesos[meses$validos & meses$formatos == formato])
    }, integer(1L))
    expandido$filas$n <- n
    expandido$filas$proporcion <- if (total) n / total else NA_real_
    expandido$filas$n_inequivocos <- n
  }
  expandido
}

.fila_formato <- function(formato, n, total, estado = "confirmado",
                          n_inequivocos = n, n_ambiguos = 0L,
                          grupo_ambiguo = "", anio_dos_digitos = FALSE,
                          granularidad = "dia") {
  data.frame(
    formato = formato,
    n = as.integer(n),
    proporcion = if (total) n / total else NA_real_,
    estado = estado,
    n_inequivocos = as.integer(n_inequivocos),
    n_ambiguos = as.integer(n_ambiguos),
    grupo_ambiguo = grupo_ambiguo,
    anio_dos_digitos = anio_dos_digitos,
    granularidad = granularidad,
    stringsAsFactors = FALSE
  )
}

#' Detectar formatos de fecha
#'
#' Reconoce formatos de fecha y fecha-hora sin escoger arbitrariamente entre
#' día/mes y mes/día. Cuando todos los valores con barra, guion o punto son
#' ambiguos, devuelve todas sus lecturas posibles con estado `"candidato"`:
#' día/mes y mes/día y, con guion y dos dígitos en cada parte (`01-02-03`),
#' también año/mes/día. Esas lecturas son una sola representación. El atributo
#' `formatos_mixtos`
#' indica si hay evidencia de dos o más representaciones en la columna.
#' El nombre `May` es a la vez el mes completo en inglés y la abreviatura de
#' `mayo`: se cuenta como `%B` o `%b` según cómo escriba el mes el resto de la
#' columna, y no como una representación aparte.
#' Se aceptan días y meses con uno o dos dígitos. Los años de dos dígitos se
#' detectan, pero siempre quedan como candidatos y se señalan en la columna
#' `anio_dos_digitos`: el siglo no se interpreta en silencio.
#' También reconoce meses escritos en español rioplatense (`setiembre` y
#' `set`) y en inglés, con la estructura completa de una fecha o como mes y
#' año. La tabla interna de nombres no usa `LC_TIME`, por lo que el resultado
#' es independiente del locale del proceso; los nombres de mes dentro de una
#' oración no se reconocen. Un mes escrito desambigua día/mes, pero un año de
#' dos dígitos sigue siendo candidato. Los formatos escritos de mes y año,
#' igual que las fechas compactas, exigen un año entre 1800 y 2100.
#' El formato compacto `%Y%m%d` exige un año entre 1800 y 2100 para evitar que
#' identificadores de ocho dígitos se clasifiquen parcialmente como fechas.
#' Una hora con segundo 60 no se acepta: R la leería como el minuto siguiente.
#' Tampoco la hora `24:00`: R la leería como la medianoche del día siguiente,
#' y el perfil publicaría una fecha que nadie escribió.
#'
#' @param x Vector de texto, fechas o fechas-hora.
#' @param muestra Máximo de valores que se analizan.
#'
#' @return Data frame con formato, frecuencia, proporción, estado, granularidad
#'   (`"dia"` o `"mes"`) y conteos de casos inequívocos y ambiguos. Los
#'   atributos informan el muestreo, la cantidad de valores compatibles y la
#'   presencia de formatos mixtos.
#' @export
#' @seealso [inferir_tipo()], [perfilar()]
#'
#' @examples
#' detectar_formatos_fecha(c("2020-01-31", "31/01/2020"))
#' detectar_formatos_fecha(c("01/02/2020", "02/03/2020"))
detectar_formatos_fecha <- function(x, muestra = 1e5) {
  resultado <- .detectar_formatos_fecha_interno(x, muestra)
  attr(resultado, "meses_texto") <- NULL
  resultado
}

# El resultado público no lleva el parser intermedio de meses. El recorrido
# interno lo conserva sólo hasta pasarlo al resumen de la columna.
.detectar_formatos_fecha_interno <- function(x, muestra = 1e5,
                                              vocabulario = NULL) {
  .validar_vector_no_tabla(x)
  muestra_x <- .muestrear_vector(x, muestra)
  valores_originales <- muestra_x$valores

  if (inherits(valores_originales, "POSIXt")) {
    n <- sum(!is.na(valores_originales))
    resultado <- .fila_formato("%Y-%m-%d %H:%M:%S", n, n)
    resultado$grupo_ambiguo <- NULL
    class(resultado) <- c("formatos_fecha", "data.frame")
    attr(resultado, "total") <- muestra_x$total
    attr(resultado, "analizados") <- muestra_x$analizados
    attr(resultado, "muestreado") <- muestra_x$muestreado
    attr(resultado, "compatibles") <- n
    attr(resultado, "formatos_mixtos") <- FALSE
    attr(resultado, "zona_horaria_origen") <- .zona_horaria_origen(valores_originales)
    attr(resultado, "n_filas_fecha_civil_distinta_utc") <-
      .fechas_civiles_distintas_utc(valores_originales)
    return(resultado)
  }
  if (inherits(valores_originales, "Date")) {
    n <- sum(!is.na(valores_originales))
    resultado <- .fila_formato("%Y-%m-%d", n, n)
    resultado$grupo_ambiguo <- NULL
    class(resultado) <- c("formatos_fecha", "data.frame")
    attr(resultado, "total") <- muestra_x$total
    attr(resultado, "analizados") <- muestra_x$analizados
    attr(resultado, "muestreado") <- muestra_x$muestreado
    attr(resultado, "compatibles") <- n
    attr(resultado, "formatos_mixtos") <- FALSE
    return(resultado)
  }

  valores <- .texto_analizable(valores_originales)$valores
  valores <- trimws(valores)
  valores <- valores[!is.na(valores) & nzchar(valores)]
  total <- length(valores)
  if (is.null(vocabulario)) {
    vocabulario <- .vocabulario_texto(valores, .umbral_vocabulario_barato)
  }
  formas <- vocabulario$valores
  pesos <- tabulate(vocabulario$indices, nbins = length(formas))
  meses_formas <- .detectar_meses_texto(formas)
  meses_texto <- if (isTRUE(vocabulario$usar)) {
    .ponderar_meses_texto(meses_formas, vocabulario$indices, pesos, total)
  } else {
    meses_formas
  }
  cubiertos <- meses_formas$validos
  filas_meses <- if (!is.null(meses_texto$filas)) {
    split(meses_texto$filas, seq_len(nrow(meses_texto$filas)))
  } else list()
  filas <- filas_meses
  # Los identificadores numéricos de ocho dígitos son frecuentes y no deben
  # atravesar las decenas de formatos de fecha si ni siquiera contienen un
  # año plausible. La guarda conserva la misma semántica del formato compacto
  # (%Y%m%d), pero evita trabajo costoso antes de llamar a `strptime()`.
  if (total && !any(cubiertos) &&
      all(grepl("^[0-9]{8}$", formas, perl = TRUE))) {
    anios <- suppressWarnings(as.integer(substr(formas, 1L, 4L)))
    plausibles <- anios >= 1800L & anios <= 2100L
    convertido <- rep(as.POSIXct(NA, tz = "UTC"), length(formas))
    if (any(plausibles)) {
      convertido[plausibles] <- .strptime_utc(formas[plausibles], "%Y%m%d")
    }
    valido <- !is.na(convertido)
    n_validos <- sum(pesos[valido])
    resultado <- if (any(valido)) {
      .fila_formato("%Y%m%d", n_validos, total)
    } else {
      .fila_formato("%Y%m%d", 0L, total)
    }
    if (!any(valido)) resultado <- resultado[0, , drop = FALSE]
    resultado$grupo_ambiguo <- NULL
    class(resultado) <- c("formatos_fecha", "data.frame")
    attr(resultado, "total") <- muestra_x$total
    attr(resultado, "analizados") <- muestra_x$analizados
    attr(resultado, "muestreado") <- muestra_x$muestreado
    attr(resultado, "compatibles") <- n_validos
    attr(resultado, "formatos_mixtos") <- FALSE
    attr(resultado, "meses_texto") <- meses_texto
    return(resultado)
  }
  indices_restantes <- which(!cubiertos)
  valores_numericos <- formas[indices_restantes]
  base_fecha <- paste0(
    "(?:[0-9]{4}[-/.][0-9]{1,2}[-/.][0-9]{1,2}|",
    "[0-9]{4}(?:-|/)[0-9]{1,2}|[0-9]{1,2}/[0-9]{4}|",
    "[0-9]{1,2}[-/.][0-9]{1,2}[-/.][0-9]{4}|",
    "[0-9]{1,2}[-/.][0-9]{1,2}[-/.][0-9]{2}|",
    "[0-9]{2}-[0-9]{1,2}-[0-9]{1,2}|[0-9]{8})"
  )
  sufijo_hora <- paste0(
    "(?:[ T][0-9]{2}:[0-9]{2}(?::[0-9]{2}(?:\\.[0-9]+)?)?",
    "(?:Z|[+-][0-9]{2}:[0-9]{2})?)?"
  )
  candidatos_fecha <- grepl(
    paste0("^", base_fecha, sufijo_hora, "$"), valores_numericos, perl = TRUE
  )
  indices_numericos <- indices_restantes[candidatos_fecha]
  valores <- valores_numericos[candidatos_fecha]
  pesos_numericos <- pesos[indices_numericos]
  especificaciones <- .especificaciones_fecha()
  mascaras <- lapply(seq_len(nrow(especificaciones)), function(i) {
    .es_fecha_valida(
      valores,
      especificaciones$formato[[i]],
      especificaciones$expresion[[i]],
      especificaciones$granularidad[[i]]
    )
  })
  names(mascaras) <- especificaciones$formato
  k <- length(filas)

  no_ambiguos <- which(!especificaciones$ambiguo)
  for (i in no_ambiguos) {
    mascara <- mascaras[[i]]
    if (any(mascara)) {
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[i]], sum(pesos_numericos[mascara]), total,
        estado = if (especificaciones$anio_dos_digitos[[i]]) {
          "candidato"
        } else {
          "confirmado"
        },
        anio_dos_digitos = especificaciones$anio_dos_digitos[[i]],
        granularidad = especificaciones$granularidad[[i]]
      )
      cubiertos[indices_numericos] <- cubiertos[indices_numericos] | mascara
    }
  }

  grupos <- unique(especificaciones$grupo_ambiguo[especificaciones$ambiguo])
  # Un grupo reune las lecturas posibles de una MISMA escritura: dia/mes y
  # mes/dia, y con guion y dos cifras tambien ano/mes/dia. Estaba escrito para
  # dos lecturas -`indice_dmy` e `indice_mdy`-; con la tercera se cuenta, para
  # cada valor, cuantas lecturas del grupo lo aceptan. Con dos lecturas el
  # resultado es el de siempre.
  for (grupo in grupos) {
    indices <- which(especificaciones$grupo_ambiguo == grupo)
    mascaras_grupo <- mascaras[indices]
    lecturas <- Reduce(`+`, lapply(mascaras_grupo, as.integer))
    ambiguos <- lecturas >= 2L
    solos <- lapply(mascaras_grupo, function(mascara) mascara & lecturas == 1L)
    con_solo <- vapply(solos, any, logical(1L))
    con_valores <- vapply(mascaras_grupo, any, logical(1L))
    pesos_grupo <- pesos_numericos
    anio_dos <- any(especificaciones$anio_dos_digitos[indices])
    estado_resuelto <- if (anio_dos) "candidato" else "confirmado"

    if (!any(con_valores)) {
      next
    }
    cubiertos[indices_numericos] <- cubiertos[indices_numericos] |
      lecturas > 0L

    if (!any(con_solo)) {
      # Ningun valor tiene una sola lectura: todas las que leen algo son
      # candidatas, y lo que cada una lee es ambiguo.
      for (posicion in which(con_valores)) {
        n_lee <- sum(pesos_grupo[mascaras_grupo[[posicion]]])
        k <- k + 1L
        filas[[k]] <- .fila_formato(
          especificaciones$formato[[indices[[posicion]]]], n_lee, total,
          estado = "candidato", n_inequivocos = 0L, n_ambiguos = n_lee,
          grupo_ambiguo = grupo, anio_dos_digitos = anio_dos
        )
      }
    } else if (sum(con_solo) == 1L) {
      posicion <- which(con_solo)
      mascara <- mascaras_grupo[[posicion]]
      # Con tres lecturas puede quedar algo que la resuelta NO lee y otras dos
      # si -`25-12-31` junto a `01-25-95`-: sigue siendo ambiguo entre ellas, se
      # publica como tal en vez de quedar cubierto sin ninguna fila que lo
      # cuente, y es una segunda representacion: la resuelta deja de compartir
      # unidad con el grupo.
      otras <- setdiff(which(con_valores), posicion)
      hay_resto <- any(vapply(otras, function(otra) {
        any(mascaras_grupo[[otra]] & !mascara)
      }, logical(1L)))
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[indices[[posicion]]]],
        sum(pesos_grupo[mascara]), total,
        estado = estado_resuelto,
        n_inequivocos = sum(pesos_grupo[solos[[posicion]]]),
        n_ambiguos = sum(pesos_grupo[mascara & ambiguos]),
        grupo_ambiguo = if (anio_dos && !hay_resto) grupo else "",
        anio_dos_digitos = anio_dos
      )
      for (otra in otras) {
        resto <- mascaras_grupo[[otra]] & !mascara
        if (!any(resto)) next
        k <- k + 1L
        filas[[k]] <- .fila_formato(
          especificaciones$formato[[indices[[otra]]]],
          sum(pesos_grupo[resto]), total,
          estado = "candidato", n_inequivocos = 0L,
          n_ambiguos = sum(pesos_grupo[resto]),
          grupo_ambiguo = grupo, anio_dos_digitos = anio_dos
        )
      }
    } else {
      # Dos o mas lecturas probadas cada una por valores que solo ella lee: son
      # representaciones distintas, y cuentan como tales aunque el ano de dos
      # cifras las deje candidatas. Con la unidad del grupo, `13/01/20` y
      # `01/13/20` -o `25-12-95` y `95-12-25`- no eran formatos mixtos, y los
      # mismos con ano de cuatro cifras si.
      for (posicion in which(con_solo)) {
        k <- k + 1L
        filas[[k]] <- .fila_formato(
          especificaciones$formato[[indices[[posicion]]]],
          sum(pesos_grupo[solos[[posicion]]]), total,
          estado = estado_resuelto,
          n_inequivocos = sum(pesos_grupo[solos[[posicion]]]),
          n_ambiguos = sum(pesos_grupo[ambiguos]),
          grupo_ambiguo = "", anio_dos_digitos = anio_dos
        )
      }
    }
  }

  if (length(filas)) {
    resultado <- do.call(rbind, filas)
    resultado <- resultado[.orden_por_bytes(-resultado$n, resultado$formato), , drop = FALSE]
    rownames(resultado) <- NULL
  } else {
    resultado <- data.frame(
      formato = character(), n = integer(), proporcion = numeric(),
      estado = character(), n_inequivocos = integer(), n_ambiguos = integer(),
      grupo_ambiguo = character(), anio_dos_digitos = logical(),
      granularidad = character(),
      stringsAsFactors = FALSE
    )
  }

  unidades <- character()
  if (nrow(resultado)) {
    confirmados <- resultado$estado == "confirmado"
    unidades <- c(unidades, resultado$formato[confirmados])
    candidatos <- resultado[resultado$estado == "candidato", , drop = FALSE]
    unidades_candidatas <- ifelse(
      nzchar(candidatos$grupo_ambiguo), candidatos$grupo_ambiguo,
      candidatos$formato
    )
    unidades <- c(unidades, unique(unidades_candidatas))
  }
  mixtos <- length(unique(unidades)) >= 2L
  resultado$grupo_ambiguo <- NULL
  class(resultado) <- c("formatos_fecha", "data.frame")
  attr(resultado, "total") <- muestra_x$total
  attr(resultado, "analizados") <- muestra_x$analizados
  attr(resultado, "muestreado") <- muestra_x$muestreado
  attr(resultado, "compatibles") <- sum(pesos[cubiertos])
  attr(resultado, "formatos_mixtos") <- mixtos
  attr(resultado, "meses_texto") <- meses_texto
  resultado
}

.meses_texto_para_formas <- function(meses_texto, textos, formas) {
  campos <- c(
    "formatos", "dias", "meses", "anios", "anio_dos_digitos", "validos"
  )
  valido <- !is.null(meses_texto) &&
    all(campos %in% names(meses_texto)) &&
    length(meses_texto$validos) == length(textos)
  if (!valido) return(.detectar_meses_texto(formas))
  indices <- match(formas, textos)
  if (anyNA(indices)) return(.detectar_meses_texto(formas))
  salida <- meses_texto
  for (campo in campos) salida[[campo]] <- meses_texto[[campo]][indices]
  salida$filas <- NULL
  salida
}

.parsear_fechas <- function(x, formatos = detectar_formatos_fecha(x),
                            meses_texto = NULL, vocabulario = NULL,
                            valores_preparados = NULL) {
  valores <- if (is.null(valores_preparados)) {
    .texto_analizable(x)$valores
  } else {
    valores_preparados
  }
  valores <- trimws(valores)
  salida <- rep(as.POSIXct(NA, tz = "UTC"), length(valores))
  if (!nrow(formatos)) {
    return(salida)
  }
  presentes <- !is.na(valores) & nzchar(valores)
  textos <- valores[presentes]
  posiciones <- which(presentes)
  if (is.null(vocabulario)) {
    vocabulario <- .vocabulario_texto(
      textos, .umbral_vocabulario_barato
    )
  }
  formas <- vocabulario$valores
  indices_vocabulario <- vocabulario$indices
  convertidas <- rep(as.POSIXct(NA, tz = "UTC"), length(formas))
  granularidades <- if ("granularidad" %in% names(formatos)) {
    formatos$granularidad
  } else {
    rep("dia", nrow(formatos))
  }
  formatos_meses <- unique(formatos$formato[
    grepl("%[Bb]", formatos$formato) &
      granularidades != "mes"
  ])
  meses <- if (length(formatos_meses)) {
    .meses_texto_para_formas(meses_texto, textos, formas)
  } else NULL
  for (formato in formatos_meses[formatos$estado[
      match(formatos_meses, formatos$formato)
    ] == "confirmado"]) {
    indices <- which(meses$validos & meses$formatos == formato)
    if (!length(indices)) next
    fechas <- as.Date(sprintf(
      "%04d-%02d-%02d", meses$anios[indices], meses$meses[indices],
      meses$dias[indices]
    ), format = "%Y-%m-%d")
    convertidas[indices] <- as.POSIXct(fechas, tz = "UTC")
  }
  confirmados <- formatos$formato[
    formatos$estado == "confirmado" & granularidades != "mes"
  ]
  especificaciones <- .especificaciones_fecha()
  # Dos cambios, y los dos sacan una regla escrita dos veces.
  #
  # La VALIDEZ es la misma de la deteccion -`.es_fecha_valida()`-. Esta copia
  # convertia todo lo que `strptime()` aceptaba, sin el filtro de ano del formato
  # compacto: `99991231` -que la deteccion y la documentacion excluyen- entraba al
  # resumen como `maximo_fecha = 9999-12-31`.
  #
  # Y un valor se convierte solo si los formatos confirmados que lo leen
  # COINCIDEN. Antes ganaba el primero de la tabla -el de mas filas, y en empate
  # el que va primero alfabeticamente-, asi que en una columna con `13/06/2020` y
  # `06/30/2020` el `01/12/2020` era el 1 de diciembre o el 12 de enero segun una
  # fila que no tenia nada que ver. Si dos formatos lo leen distinto, queda sin
  # convertir: elegir seria inventar.
  elegida <- rep(NA_real_, length(formas))
  conflicto <- rep(FALSE, length(formas))
  for (formato in confirmados) {
    indice_especificacion <- match(formato, especificaciones$formato)
    if (is.na(indice_especificacion)) next
    patron <- especificaciones$expresion[[indice_especificacion]]
    aplica <- is.na(convertidas) & grepl(patron, formas, perl = TRUE)
    if (!any(aplica)) next
    validos <- .es_fecha_valida(formas[aplica], formato, patron)
    if (!any(validos)) next
    preparados <- .preparar_fecha_parseo(formas[aplica][validos], formato)
    convertido <- as.numeric(as.POSIXct(suppressWarnings(
      .strptime_utc(preparados, formato)
    ), tz = "UTC"))
    indices <- which(aplica)[validos]
    choca <- !is.na(elegida[indices]) & !is.na(convertido) &
      elegida[indices] != convertido
    conflicto[indices[choca]] <- TRUE
    toma <- is.na(elegida[indices]) & !is.na(convertido)
    elegida[indices[toma]] <- convertido[toma]
  }
  usar <- is.na(convertidas) & !is.na(elegida) & !conflicto
  convertidas[usar] <- as.POSIXct(elegida[usar], origin = "1970-01-01", tz = "UTC")
  if (length(posiciones)) salida[posiciones] <- convertidas[indices_vocabulario]
  salida
}
