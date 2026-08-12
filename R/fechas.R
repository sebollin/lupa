.preparar_fecha_parseo <- function(x, formato) {
  if (!grepl("%z", formato, fixed = TRUE)) return(x)
  x <- sub("Z$", "+0000", x, perl = TRUE)
  sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x, perl = TRUE)
}

.es_fecha_valida <- function(x, formato, expresion) {
  coincide <- grepl(expresion, x, perl = TRUE)
  valido <- rep(FALSE, length(x))
  if (any(coincide)) {
    preparados <- .preparar_fecha_parseo(x[coincide], formato)
    convertido <- strptime(preparados, format = formato, tz = "UTC")
    valido_convertido <- !is.na(convertido)
    if (startsWith(formato, "%Y%m%d")) {
      anios <- suppressWarnings(as.integer(substr(x[coincide], 1L, 4L)))
      valido_convertido <- valido_convertido & anios >= 1800L & anios <= 2100L
    }
    valido[coincide] <- valido_convertido
  }
  valido
}

.especificaciones_fecha <- function() {
  bases <- data.frame(
    formato = c(
      "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y",
      "%m-%d-%Y", "%Y/%m/%d", "%d.%m.%Y", "%m.%d.%Y",
      "%Y%m%d", "%d/%m/%y", "%m/%d/%y", "%d-%m-%y",
      "%m-%d-%y", "%d.%m.%y", "%m.%d.%y", "%y-%m-%d"
    ),
    expresion = c(
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
    grupo_base = c(
      "", "barra4", "barra4", "guion4", "guion4", "", "punto4",
      "punto4", "", "barra2", "barra2", "guion2", "guion2",
      "punto2", "punto2", ""
    ),
    anio_dos_digitos = c(rep(FALSE, 9L), rep(TRUE, 7L)),
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

  resultado <- vector("list", nrow(bases) * nrow(sufijos))
  k <- 0L
  for (i in seq_len(nrow(bases))) {
    for (j in seq_len(nrow(sufijos))) {
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
  !is.na(convertido) & format(convertido, "%Y-%m-%d") == texto
}

.detectar_meses_texto <- function(valores) {
  n <- length(valores)
  formatos <- rep("", n)
  dias <- rep(NA_integer_, n)
  meses <- rep(NA_integer_, n)
  anios <- rep(NA_integer_, n)
  dos_digitos <- rep(FALSE, n)
  validos <- rep(FALSE, n)
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
  # Los nombres de mes de la tabla son ASCII. `chartr()` hace el plegado de
  # caja sin consultar `LC_CTYPE`/`LC_TIME` (a diferencia de `tolower()`).
  texto <- chartr(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz",
    trimws(as.character(valores))
  )
  # La fecha debe empezar por un día o por el nombre del mes. Además de
  # expresar la estructura completa, esta guarda evita cuatro pasadas de
  # expresiones regulares sobre texto libre que sólo menciona un mes.
  candidatos <- !is.na(texto) & grepl(
    paste0(
      "^(?:[0-9]{1,2}[[:space:]]+de[[:space:]]+|",
      "[0-9]{1,2}[-[:space:]]|(?:", .meses_fecha_regex,
      ")[[:space:]]+)"
    ), texto, perl = TRUE
  )
  if (any(candidatos)) {
    for (patron in patrones) {
      coincidencias <- regmatches(
        texto, regexec(patron$expresion, texto, perl = TRUE)
      )
      indices <- which(!validos & candidatos & lengths(coincidencias) > 1L)
      if (!length(indices)) next
      capturas <- coincidencias[indices]
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
        separadores <- ifelse(grepl("-", texto[indices_validos], fixed = TRUE),
                              "-", " ")
        paste0("%d", separadores, "%",
               ifelse(nchar(nombres_validos) > 3L, "B", "b"), separadores,
               "%", ifelse(dos_validos, "y", "Y"))
      } else if (identical(patron$formato, "ingles")) {
        paste0("%", ifelse(nchar(nombres_validos) > 3L, "B", "b"),
               " %d, %", ifelse(dos_validos, "y", "Y"))
      } else {
        paste0("%", ifelse(nchar(nombres_validos) > 3L, "B", "b"), " %Y")
      }
      formatos[indices_validos] <- nombre_formato
      dias[indices_validos] <- dias_candidatos[validos_candidatos]
      meses[indices_validos] <- meses_candidatos[validos_candidatos]
      anios[indices_validos] <- anios_candidatos[validos_candidatos]
      dos_digitos[indices_validos] <- dos_validos
      validos[indices_validos] <- TRUE
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
      anio_dos_digitos = dos
    )
  }
  list(
    formatos = formatos, dias = dias, meses = meses, anios = anios,
    anio_dos_digitos = dos_digitos, validos = validos,
    filas = if (length(filas)) do.call(rbind, filas) else NULL
  )
}

.fila_formato <- function(formato, n, total, estado = "confirmado",
                          n_inequivocos = n, n_ambiguos = 0L,
                          grupo_ambiguo = "", anio_dos_digitos = FALSE) {
  data.frame(
    formato = formato,
    n = as.integer(n),
    proporcion = if (total) n / total else NA_real_,
    estado = estado,
    n_inequivocos = as.integer(n_inequivocos),
    n_ambiguos = as.integer(n_ambiguos),
    grupo_ambiguo = grupo_ambiguo,
    anio_dos_digitos = anio_dos_digitos,
    stringsAsFactors = FALSE
  )
}

#' Detectar formatos de fecha
#'
#' Reconoce formatos de fecha y fecha-hora sin escoger arbitrariamente entre
#' día/mes y mes/día. Cuando todos los valores con barra, guion o punto son
#' ambiguos, devuelve ambos formatos con estado `"candidato"`. El atributo
#' `formatos_mixtos`
#' indica si hay evidencia de dos o más representaciones en la columna.
#' Se aceptan días y meses con uno o dos dígitos. Los años de dos dígitos se
#' detectan, pero siempre quedan como candidatos y se señalan en la columna
#' `anio_dos_digitos`: el siglo no se interpreta en silencio.
#' También reconoce meses escritos en español rioplatense (`setiembre` y
#' `set`) y en inglés, con la estructura completa de una fecha o como mes y
#' año. La tabla interna de nombres no usa `LC_TIME`, por lo que el resultado
#' es independiente del locale del proceso; los nombres de mes dentro de una
#' oración no se reconocen. Un mes escrito desambigua día/mes, pero un año de
#' dos dígitos sigue siendo candidato.
#' El formato compacto `%Y%m%d` exige un año entre 1800 y 2100 para evitar que
#' identificadores de ocho dígitos se clasifiquen parcialmente como fechas.
#'
#' @param x Vector de texto, fechas o fechas-hora.
#' @param muestra Máximo de valores que se analizan.
#'
#' @return Data frame con formato, frecuencia, proporción, estado y conteos de
#'   casos inequívocos y ambiguos. Los atributos informan el muestreo, la
#'   cantidad de valores compatibles y la presencia de formatos mixtos.
#' @export
#' @seealso [inferir_tipo()], [perfilar()]
#'
#' @examples
#' detectar_formatos_fecha(c("2020-01-31", "31/01/2020"))
#' detectar_formatos_fecha(c("01/02/2020", "02/03/2020"))
detectar_formatos_fecha <- function(x, muestra = 1e5) {
  if (inherits(x, "data.frame")) {
    stop("`x` debe ser un vector, no un data.frame.", call. = FALSE)
  }
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
  meses_texto <- .detectar_meses_texto(valores)
  cubiertos <- meses_texto$validos
  filas <- if (!is.null(meses_texto$filas)) {
    split(meses_texto$filas, seq_len(nrow(meses_texto$filas)))
  } else list()
  # Los identificadores numéricos de ocho dígitos son frecuentes y no deben
  # atravesar las decenas de formatos de fecha si ni siquiera contienen un
  # año plausible. La guarda conserva la misma semántica del formato compacto
  # (%Y%m%d), pero evita trabajo costoso antes de llamar a `strptime()`.
  if (total && !any(cubiertos) && all(grepl("^[0-9]{8}$", valores, perl = TRUE))) {
    anios <- suppressWarnings(as.integer(substr(valores, 1L, 4L)))
    plausibles <- anios >= 1800L & anios <= 2100L
    convertido <- rep(as.POSIXct(NA, tz = "UTC"), total)
    if (any(plausibles)) {
      convertido[plausibles] <- strptime(
        valores[plausibles], format = "%Y%m%d", tz = "UTC"
      )
    }
    valido <- !is.na(convertido)
    resultado <- if (any(valido)) {
      .fila_formato("%Y%m%d", sum(valido), total)
    } else {
      .fila_formato("%Y%m%d", 0L, total)
    }
    if (!any(valido)) resultado <- resultado[0, , drop = FALSE]
    resultado$grupo_ambiguo <- NULL
    class(resultado) <- c("formatos_fecha", "data.frame")
    attr(resultado, "total") <- muestra_x$total
    attr(resultado, "analizados") <- muestra_x$analizados
    attr(resultado, "muestreado") <- muestra_x$muestreado
    attr(resultado, "compatibles") <- sum(valido)
    attr(resultado, "formatos_mixtos") <- FALSE
    return(resultado)
  }
  indices_restantes <- which(!cubiertos)
  valores_numericos <- valores[indices_restantes]
  base_fecha <- paste0(
    "(?:[0-9]{4}[-/.][0-9]{1,2}[-/.][0-9]{1,2}|",
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
  especificaciones <- .especificaciones_fecha()
  mascaras <- lapply(seq_len(nrow(especificaciones)), function(i) {
    .es_fecha_valida(
      valores,
      especificaciones$formato[[i]],
      especificaciones$expresion[[i]]
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
        especificaciones$formato[[i]], sum(mascara), total,
        estado = if (especificaciones$anio_dos_digitos[[i]]) {
          "candidato"
        } else {
          "confirmado"
        },
        anio_dos_digitos = especificaciones$anio_dos_digitos[[i]]
      )
      cubiertos[indices_numericos] <- cubiertos[indices_numericos] | mascara
    }
  }

  grupos <- unique(especificaciones$grupo_ambiguo[especificaciones$ambiguo])
  for (grupo in grupos) {
    indices <- which(especificaciones$grupo_ambiguo == grupo)
    indice_dmy <- indices[grepl("^%d[-/.]", especificaciones$formato[indices])]
    indice_mdy <- indices[grepl("^%m[-/.]", especificaciones$formato[indices])]
    mascara_dmy <- mascaras[[indice_dmy]]
    mascara_mdy <- mascaras[[indice_mdy]]
    solo_dmy <- mascara_dmy & !mascara_mdy
    solo_mdy <- mascara_mdy & !mascara_dmy
    ambiguos <- mascara_dmy & mascara_mdy
    anio_dos <- especificaciones$anio_dos_digitos[[indice_dmy]]
    estado_resuelto <- if (anio_dos) "candidato" else "confirmado"

    if (!any(mascara_dmy | mascara_mdy)) {
      next
    }
    cubiertos[indices_numericos] <- cubiertos[indices_numericos] |
      mascara_dmy | mascara_mdy

    if (!any(solo_dmy) && !any(solo_mdy)) {
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[indice_dmy]], sum(ambiguos), total,
        estado = "candidato", n_inequivocos = 0L,
        n_ambiguos = sum(ambiguos), grupo_ambiguo = grupo,
        anio_dos_digitos = anio_dos
      )
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[indice_mdy]], sum(ambiguos), total,
        estado = "candidato", n_inequivocos = 0L,
        n_ambiguos = sum(ambiguos), grupo_ambiguo = grupo,
        anio_dos_digitos = anio_dos
      )
    } else if (any(solo_dmy) && !any(solo_mdy)) {
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[indice_dmy]], sum(mascara_dmy), total,
        estado = estado_resuelto,
        n_inequivocos = sum(solo_dmy), n_ambiguos = sum(ambiguos),
        grupo_ambiguo = if (anio_dos) grupo else "",
        anio_dos_digitos = anio_dos
      )
    } else if (!any(solo_dmy) && any(solo_mdy)) {
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[indice_mdy]], sum(mascara_mdy), total,
        estado = estado_resuelto,
        n_inequivocos = sum(solo_mdy), n_ambiguos = sum(ambiguos),
        grupo_ambiguo = if (anio_dos) grupo else "",
        anio_dos_digitos = anio_dos
      )
    } else {
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[indice_dmy]], sum(solo_dmy), total,
        estado = estado_resuelto,
        n_inequivocos = sum(solo_dmy), n_ambiguos = sum(ambiguos),
        grupo_ambiguo = grupo, anio_dos_digitos = anio_dos
      )
      k <- k + 1L
      filas[[k]] <- .fila_formato(
        especificaciones$formato[[indice_mdy]], sum(solo_mdy), total,
        estado = estado_resuelto,
        n_inequivocos = sum(solo_mdy), n_ambiguos = sum(ambiguos),
        grupo_ambiguo = grupo, anio_dos_digitos = anio_dos
      )
    }
  }

  if (length(filas)) {
    resultado <- do.call(rbind, filas)
    resultado <- resultado[order(-resultado$n, resultado$formato), , drop = FALSE]
    rownames(resultado) <- NULL
  } else {
    resultado <- data.frame(
      formato = character(), n = integer(), proporcion = numeric(),
      estado = character(), n_inequivocos = integer(), n_ambiguos = integer(),
      grupo_ambiguo = character(), anio_dos_digitos = logical(),
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
  attr(resultado, "compatibles") <- sum(cubiertos)
  attr(resultado, "formatos_mixtos") <- mixtos
  resultado
}

.parsear_fechas <- function(x, formatos = detectar_formatos_fecha(x)) {
  valores <- .texto_analizable(x)$valores
  valores <- trimws(valores)
  salida <- rep(as.POSIXct(NA, tz = "UTC"), length(valores))
  if (!nrow(formatos)) {
    return(salida)
  }
  meses <- .detectar_meses_texto(valores)
  formatos_meses <- unique(formatos$formato[
    grepl("%[Bb]", formatos$formato)
  ])
  for (formato in formatos_meses[formatos$estado[
      match(formatos_meses, formatos$formato)
    ] == "confirmado"]) {
    indices <- which(meses$validos & meses$formatos == formato)
    if (!length(indices)) next
    fechas <- as.Date(sprintf(
      "%04d-%02d-%02d", meses$anios[indices], meses$meses[indices],
      meses$dias[indices]
    ), format = "%Y-%m-%d")
    salida[indices] <- as.POSIXct(fechas, tz = "UTC")
  }
  confirmados <- formatos$formato[formatos$estado == "confirmado"]
  especificaciones <- .especificaciones_fecha()
  for (formato in confirmados) {
    indice_especificacion <- match(formato, especificaciones$formato)
    if (is.na(indice_especificacion)) next
    patron <- especificaciones$expresion[[indice_especificacion]]
    pendientes <- is.na(salida) & !is.na(valores) &
      grepl(patron, valores, perl = TRUE)
    if (!any(pendientes)) {
      next
    }
    preparados <- .preparar_fecha_parseo(valores[pendientes], formato)
    convertido <- strptime(preparados, format = formato, tz = "UTC")
    valido <- !is.na(convertido)
    indices <- which(pendientes)[valido]
    salida[indices] <- as.POSIXct(convertido[valido], tz = "UTC")
  }
  salida
}
