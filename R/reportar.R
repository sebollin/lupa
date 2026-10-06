# Lo marcado UTF-8 que no es UTF-8 valido -lo que deja `fread(encoding = "UTF-8")`
# sobre un CSV latin1- tampoco es texto: se trata como lo marcado `bytes`. Antes
# pasaba de largo y el informe entero abortaba en `nchar()` o en `gsub()`, aunque
# `print()` del mismo perfil funcionara. Medido en una refutacion.
.declarar_utf8_roto <- function(x) {
  if (!is.character(x) || !length(x)) return(x)
  rotos <- !is.na(x) & Encoding(x) == "UTF-8" & !validUTF8(x)
  if (any(rotos)) {
    declarados <- x[rotos]
    Encoding(declarados) <- "bytes"
    x[rotos] <- declarados
  }
  x
}

.html_utf8 <- function(x) {
  x <- .declarar_utf8_roto(as.character(x))
  # `bytes` NO entra aca, y antes entraba. Es la declaracion explicita de que
  # eso no se interprete como texto; el reporte HTML lo marcaba UTF-8 y
  # publicaba el caracter, mientras `print()` del mismo dato -y `test-N68`, que
  # lo fija- publicaba `a\xc3\xb1o`. Dos salidas del paquete decian cosas
  # distintas sobre el mismo valor.
  #
  # Se rinde con `format()`, que es lo que R usa para mostrarlo, y el resultado
  # es ASCII: puede seguir al resto del camino sin que `enc2utf8()` aborte
  # -sobre la marca `bytes` aborta, y debe hacerlo-.
  # Lo sin marca que no es UTF-8 tampoco es texto: se rinde como lo declarado.
  crudos <- !is.na(x) & Encoding(x) == "bytes"
  sin_marca <- .sin_marca_ilegible(x)
  if (any(sin_marca)) {
    declarados <- x[sin_marca]
    Encoding(declarados) <- "bytes"
    x[sin_marca] <- declarados
    crudos <- crudos | sin_marca
  }
  if (any(crudos)) x[crudos] <- format(x[crudos])
  marcables <- !is.na(x) & Encoding(x) == "unknown" & validUTF8(x)
  if (any(marcables)) {
    declarados <- x[marcables]
    Encoding(declarados) <- "UTF-8"
    x[marcables] <- declarados
  }
  enc2utf8(x)
}

# Un caracter de control -C0 salvo tabulador y saltos, DEL y C1- en el texto de
# un HTML es un error de analisis, y un navegador no lo dibuja: `ctl\x01x` y
# `ctlx` se veian iguales en la tabla de patrones, en la moda y en los ejemplos,
# mientras la evidencia del hallazgo del mismo informe lo escribia `<U+0001>`
# "para que se vea". Se escribe con su codigo, como en la evidencia, y el
# encabezado lo declara. Ronda 26.
.html_controles_visibles <- function(x) {
  con_control <- !is.na(x) & grepl(
    "[\\x01-\\x08\\x0B\\x0C\\x0E-\\x1F\\x7F]|\\xC2[\\x80-\\x9F]", x,
    perl = TRUE, useBytes = TRUE
  )
  if (!any(con_control)) return(x)
  x[con_control] <- vapply(x[con_control], function(texto) {
    codigos <- utf8ToInt(texto)
    control <- (codigos >= 1L & codigos <= 31L & !codigos %in% c(9L, 10L, 13L)) |
      (codigos >= 127L & codigos <= 159L)
    piezas <- intToUtf8(codigos, multiple = TRUE)
    piezas[control] <- sprintf("<U+%04X>", codigos[control])
    salida <- paste(piezas, collapse = "")
    Encoding(salida) <- "UTF-8"
    salida
  }, character(1L), USE.NAMES = FALSE)
  x
}

.html_escapar <- function(x) {
  x <- .html_controles_visibles(.html_utf8(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  # Tambien se neutralizan secuencias que podrian parecer atributos o recursos.
  x <- gsub("=", "&#61;", x, fixed = TRUE)
  x <- gsub("@", "&#64;", x, fixed = TRUE)
  gsub(":", "&#58;", x, fixed = TRUE)
}

.html_texto <- function(x) {
  x <- .html_escapar(x)
  gsub("\n", "<br>", gsub("\r\n?", "\n", x, perl = TRUE), fixed = TRUE)
}

# La misma nota que ya lleva la seccion de patrones: los conteos de formatos de
# fecha tambien salen de la muestra, y la tabla los publicaba sin decirlo.
.nota_formatos_muestreados <- function(formatos) {
  if (!is.list(formatos) || !length(formatos)) return("")
  muestreados <- Filter(function(f) isTRUE(attr(f, "muestreado", exact = TRUE)) &&
                          is.data.frame(f) && nrow(f), formatos)
  if (!length(muestreados)) return("")
  primero <- muestreados[[1L]]
  paste0(
    "<p class=\"nota\">Formatos estimados sobre ",
    .html_texto(attr(primero, "analizados", exact = TRUE)), " de ",
    .html_texto(attr(primero, "total", exact = TRUE)),
    " valores: los conteos son de la muestra.</p>"
  )
}

# Las cifras del informe, con la regla de los numeros publicados del paquete
# -sin notacion cientifica, sin depender de `digits` ni de `scipen`- y con las
# cifras que hacen falta para volver al valor del objeto.
#
# Eran quince: `0.1 + 0.2` salia `0.3` al lado de "no cumple x <= 0.3" -el
# objeto tiene 0.30000000000000004, y `<=` dice FALSE-, y `1/3` salia
# 0.333333333333333, que no es el doble del objeto. `perfilar_por()` ya habia
# decidido que un numero publicado vuelve a su valor; la regla de cuantas cifras
# es una sola, `.cifras_reversibles()`.
#
# Y una cifra no se abrevia como un texto. Escrita sin exponente, |x| >= 1e240 o
# |x| < 1e-238 pasa los 240 caracteres, y el corte de los textos dejaba otra
# magnitud: el ausente de Stata 8.988e307 se publicaba como 239 cifras y "...",
# que se leen 8.988e238, igual que su media cien veces menor. La que no cabe se
# escribe con exponente, con las mismas cifras: es el mismo valor, entero.
# Medido en la ronda 26.
.formatear_numeros_reporte <- function(x, max_caracteres = 240L) {
  texto <- .formatear_numeros_uno_a_uno(as.numeric(x))
  valores <- as.double(x) + 0
  escribir <- function(cuales, cifras, cientifica) {
    vapply(seq_along(cuales), function(k) {
      format(valores[[cuales[[k]]]], digits = cifras[[k]],
             scientific = cientifica, trim = TRUE)
    }, character(1L))
  }
  cifras <- .cifras_reversibles(valores, texto)
  mas <- which(!is.na(cifras))
  if (length(mas)) texto[mas] <- escribir(mas, cifras[mas], FALSE)
  largos <- which(!is.na(texto) & nchar(texto, type = "bytes") > max_caracteres)
  if (length(largos)) {
    cifras <- .cifras_reversibles(valores[largos])
    cifras[is.na(cifras)] <- 15L
    texto[largos] <- escribir(largos, cifras, TRUE)
  }
  texto
}

# Varias cifras en una celda se abrevian por cifras enteras: cortar el texto
# partia la ultima.
.unir_cifras_reporte <- function(textos, max_caracteres) {
  textos[is.na(textos)] <- "NA"
  unido <- paste(textos, collapse = ", ")
  if (nchar(unido, type = "chars") <= max_caracteres) return(unido)
  largos <- cumsum(nchar(textos, type = "chars") + 2L)
  caben <- max(1L, sum(largos + 1L <= max_caracteres))
  paste0(paste(textos[seq_len(caben)], collapse = ", "), ", \u2026")
}

.resumir_valor_reporte <- function(x, max_caracteres = 240L) {
  if (is.null(x) || !length(x)) return("")
  if (is.function(x)) return("<funci\u00f3n>")
  if (inherits(x, "data.frame")) {
    return(paste0("<tabla: ", nrow(x), " filas \u00d7 ", ncol(x), " columnas>"))
  }
  if (is.list(x) && !is.null(x$estado) &&
      "indices_fila" %in% names(x) && "mostrados" %in% names(x)) {
    total <- if (length(x$total) && is.na(x$total)) "NA" else as.character(x$total)
    return(paste0(
      "estado=", x$estado,
      "; filas mostradas=", x$mostrados,
      " de ", total,
      "; alcance=", x$alcance
    ))
  }
  if (inherits(x, "POSIXt")) {
    texto <- format(x, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
  } else if (inherits(x, "Date")) {
    texto <- format(x, "%Y-%m-%d")
  } else if (is.list(x)) {
    limite <- min(length(x), 8L)
    partes <- vapply(seq_len(limite), function(i) {
      valor <- .resumir_valor_reporte(x[[i]], max_caracteres = 80L)
      nombre <- names(x)[i]
      if (!is.null(nombre) && !is.na(nombre) && nzchar(nombre)) {
        paste0(nombre, "=", valor)
      } else {
        valor
      }
    }, character(1L))
    texto <- paste(partes, collapse = "; ")
    if (length(x) > limite) texto <- paste0(texto, "; \u2026")
  } else if (is.logical(x)) {
    texto <- .texto_logico_reporte(x)
  } else if (inherits(x, "integer64")) {
    texto <- as.character(x)
  } else if (is.numeric(x)) {
    # Con la regla de numeros publicados del paquete -sin depender de `digits`
    # ni de `scipen`-, no con `digits = 8`: la mediana 123456789.5 se publicaba
    # 123456790, que no esta en el objeto ni en la tabla. Medido en una
    # refutacion. Y con las cifras que vuelven al valor: ver
    # `.formatear_numeros_reporte()`.
    texto <- .formatear_numeros_reporte(x, max_caracteres)
    texto[is.na(x)] <- NA_character_
  } else {
    texto <- as.character(x)
  }
  # Se rinde ANTES de pegar y de medir: `nchar(type = "chars")` ABORTA sobre un
  # valor declarado `bytes` -"number of characters is not computable"-, y el
  # reporte entero no se generaba. Basta con que ese valor llegue a una celda,
  # por ejemplo siendo la moda de una columna.
  texto <- .publicar_sin_marca_ilegible(
    .texto_publicable(.declarar_utf8_roto(texto))
  )
  if (inherits(x, "integer64") || (is.numeric(x) && !is.logical(x))) {
    return(.unir_cifras_reporte(texto, max_caracteres))
  }
  texto <- paste(texto, collapse = ", ")
  if (nchar(texto, type = "chars") > max_caracteres) {
    texto <- paste0(substr(texto, 1L, max_caracteres - 1L), "\u2026")
  }
  texto
}

.valor_celda_reporte <- function(x, i) {
  valor <- if (is.list(x) && !inherits(x, c("POSIXt", "Date"))) x[[i]] else x[i]
  if (!length(valor) || (length(valor) == 1L && is.atomic(valor) && is.na(valor))) {
    return("<span class=\"sin-dato\">\u2014</span>")
  }
  .html_texto(.resumir_valor_reporte(valor))
}

.validar_limite_reporte <- function(x, nombre) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1) {
    stop("`", nombre, "` debe ser un n\u00famero positivo o Inf.", call. = FALSE)
  }
  if (is.infinite(x)) Inf else floor(x)
}

.nota_truncamiento <- function(mostradas, total, unidad = "filas") {
  if (mostradas >= total) return("")
  paste0(
    "<p class=\"nota\">Se muestran ", .html_texto(mostradas), " de ",
    .html_texto(total), " ",
    .html_texto(unidad), ".</p>"
  )
}

.html_tabla <- function(x, max_filas, columnas = names(x)) {
  if (!inherits(x, "data.frame")) x <- as.data.frame(x, stringsAsFactors = FALSE)
  x <- .tabla_base(x)
  indices <- .indice_nombre(columnas, names(x))
  columnas <- names(x)[unique(indices[!is.na(indices)])]
  x <- .seleccionar_columnas(x, columnas)
  total <- nrow(x)
  limite <- if (is.infinite(max_filas)) total else min(total, max_filas)
  if (!total || !length(columnas)) {
    return(paste0(
      "<p class=\"sin-registros\">No hay registros para mostrar.</p>",
      .nota_truncamiento(limite, total)
    ))
  }
  vista <- x[seq_len(limite), , drop = FALSE]
  encabezado <- paste0(
    "<thead><tr>",
    paste0("<th>", .html_texto(names(vista)), "</th>", collapse = ""),
    "</tr></thead>"
  )
  filas <- vapply(seq_len(nrow(vista)), function(i) {
    celdas <- vapply(seq_along(vista), function(j) {
      clase <- ""
      if (names(vista)[[j]] == "severidad") {
        nivel <- as.character(vista[[j]][i])
        if (!is.na(nivel) && nivel %in% c("ok", "sospechoso", "error")) {
          clase <- paste0(" class=\"nivel-", nivel, "\"")
        }
      }
      paste0("<td", clase, ">", .valor_celda_reporte(vista[[j]], i), "</td>")
    }, character(1L))
    paste0("<tr>", paste0(celdas, collapse = ""), "</tr>")
  }, character(1L))
  paste0(
    "<div class=\"tabla-contenedor\"><table>", encabezado,
    "<tbody>", paste0(filas, collapse = ""), "</tbody></table></div>",
    .nota_truncamiento(limite, total)
  )
}

.tarjeta_reporte <- function(etiqueta, valor, clase = "") {
  paste0(
    "<div class=\"tarjeta ", clase, "\"><span>",
    .html_texto(etiqueta), "</span><strong>", .html_texto(valor),
    "</strong></div>"
  )
}

.resumen_severidades <- function(x, no_evaluados = 0L) {
  severidades <- as.character(x)
  # Una severidad `NA` es una fila SIN veredicto -la deriva publica eso cuando
  # uno de los dos resultados no se evaluo- y las tres tarjetas la dejaban
  # afuera con `na.rm = TRUE`: un informe de cinco filas mostraba "Errores 3,
  # Sospechosos 0, Correctos 0" y ninguna tarjeta decia donde estaban las otras
  # dos. Antes esas filas se contaban entre las correctas, que era peor; no
  # contarlas en ningun lado tampoco es informar. La tarjeta "No evaluados" ya
  # existia para los diagnosticos de cobertura: cuenta las dos cosas, que son
  # la misma -no hay veredicto-, y asi las tarjetas suman las filas de la tabla.
  diagnosticos <- (if (length(no_evaluados)) no_evaluados[[1L]] else 0L) +
    sum(is.na(severidades))
  paste0(
    "<div class=\"tarjetas\">",
    .tarjeta_reporte("Errores", sum(severidades == "error", na.rm = TRUE), "error"),
    .tarjeta_reporte(
      "Sospechosos", sum(severidades == "sospechoso", na.rm = TRUE),
      "sospechoso"
    ),
    .tarjeta_reporte("Correctos", sum(severidades == "ok", na.rm = TRUE), "ok"),
    if (isTRUE(diagnosticos > 0L)) {
      .tarjeta_reporte("No evaluados", diagnosticos, "informativo")
    } else "",
    "</div>"
  )
}

# Una tabla de indicadores -"Resumen general", "Alcance y recortes"- con un valor
# de cada tipo. Se armaba con `c()`, que lleva todo al tipo comun: al lado de un
# texto, el doble 100000 salia "1e+05" -`as.character()`- en la fila de debajo
# de "Filas 10000"; entre enteros, el logico salia 0/1 donde el resto del
# informe escribe "si"/"no"; y un atributo NULL corria las filas una posicion.
# Cada valor conserva su tipo y lo escribe la regla del informe. Ronda 26.
.tabla_indicadores_reporte <- function(indicador, valor) {
  tabla <- data.frame(indicador = indicador, stringsAsFactors = FALSE)
  tabla$valor <- I(unname(valor))
  tabla
}

# La memoria, con sus bytes: `format(units = "auto")` es el redondeo de la
# consola -401904 bytes salian "392.5 Kb"-, y queda al lado, entre parentesis,
# como lectura.
.memoria_publicada_reporte <- function(bytes) {
  if (!is.numeric(bytes) || length(bytes) != 1L || is.na(bytes)) {
    return(NA_character_)
  }
  paste0(
    .formatear_numeros_reporte(bytes), " bytes (",
    format(structure(bytes, class = "object_size"), units = "auto"), ")"
  )
}

.seccion_perfil <- function(x, max_filas, max_patrones,
                            proteger_datos_personales = TRUE,
                            cobertura = NULL) {
  if (proteger_datos_personales) x <- .proteger_perfil(x)
  n_protegidas <- length(.columnas_personales_protegidas(x))
  general <- .tabla_indicadores_reporte(
    c(
      "Filas", "Columnas", "Celdas", "Filas completas",
      "Filas duplicadas", "Memoria de los datos"
    ),
    list(
      x$general$filas, x$general$columnas, x$general$celdas,
      x$general$filas_completas, x$general$filas_duplicadas,
      .memoria_publicada_reporte(x$general$memoria_bytes)
    )
  )
  hallazgos <- x$hallazgos
  # El informe es la superficie donde un hallazgo editado a mano circula fuera
  # del equipo: la misma guarda que corre al construirlo y al publicarlo por
  # `hallazgos()` tiene que correr aca.
  if (inherits(hallazgos, "data.frame")) {
    .advertir_incoherencias_trazabilidad(hallazgos)
  }
  severidades <- if ("severidad" %in% names(hallazgos)) hallazgos$severidad else {
    character()
  }
  cobertura_diagnosticos <- x$cobertura_diagnosticos
  if (!inherits(cobertura_diagnosticos, "data.frame")) {
    cobertura_diagnosticos <- .cobertura_diagnosticos_vacia()
  }
  con_patrones <- which(vapply(
    x$patrones,
    function(tabla) inherits(tabla, "data.frame") && nrow(tabla) > 0L,
    logical(1L)
  ))
  n_columnas_patrones <- length(con_patrones)
  indices_patrones <- if (is.infinite(max_filas)) con_patrones else {
    utils::head(con_patrones, max_filas)
  }
  bloques_patrones <- vapply(indices_patrones, function(i) {
    patrones <- x$patrones[[i]]
    columna <- names(x$patrones)[[i]]
    total <- attr(patrones, "n_patrones_distintos", exact = TRUE)
    if (is.null(total) || !length(total) || is.na(total)) total <- nrow(patrones)
    limite <- if (is.infinite(max_patrones)) nrow(patrones) else {
      min(nrow(patrones), max_patrones)
    }
    muestreo <- if (isTRUE(attr(patrones, "muestreado", exact = TRUE))) {
      paste0(
        "<p class=\"nota\">Patrones estimados sobre ",
        .html_texto(attr(patrones, "analizados", exact = TRUE)), " de ",
        .html_texto(attr(patrones, "total", exact = TRUE)), " valores.</p>"
      )
    } else {
      ""
    }
    paste0(
      "<h4>", .html_texto(columna), "</h4>",
      .html_tabla(patrones, max_patrones),
      if (limite < total) {
        paste0(
          "<p class=\"nota\">Se muestran ", .html_texto(limite), " de ",
          .html_texto(total),
          " patrones distintos.</p>"
        )
      } else {
        ""
      },
      muestreo
    )
  }, character(1L))
  nota_columnas_patron <- .nota_truncamiento(
    length(indices_patrones), n_columnas_patrones, "columnas con patrones"
  )
  formatos <- lapply(seq_along(x$formatos_fecha), function(i) {
    tabla <- x$formatos_fecha[[i]]
    if (!inherits(tabla, "data.frame") || !nrow(tabla)) return(NULL)
    tabla$columna <- names(x$formatos_fecha)[[i]]
    tabla[c("columna", setdiff(names(tabla), "columna"))]
  })
  formatos <- formatos[!vapply(formatos, is.null, logical(1L))]
  tabla_formatos <- if (length(formatos)) do.call(rbind, formatos) else {
    data.frame(stringsAsFactors = FALSE)
  }
  dependencias <- x$dependencias
  if (is.null(dependencias)) dependencias <- data.frame(stringsAsFactors = FALSE)
  nota_dependencias <- if (isTRUE(attr(dependencias, "truncado", exact = TRUE))) {
    paste0(
      "<p class=\"nota\">La b\u00fasqueda de dependencias se limit\u00f3 a ",
      .html_texto(length(attr(dependencias, "columnas_analizadas", exact = TRUE))),
      " columnas; quedaron fuera ",
      .html_texto(length(attr(dependencias, "columnas_omitidas", exact = TRUE))),
      ".</p>"
    )
  } else {
    ""
  }
  # La muestra tambien se declara aca. El objeto y la consola la decian; el
  # informe -lo que se comparte- publicaba "exacta" sobre una tabla de 200.000
  # filas donde la relacion se cumplia en la mitad, porque la muestra solo veia
  # las filas que la cumplian.
  if (isTRUE(attr(dependencias, "muestreado", exact = TRUE))) {
    nota_dependencias <- paste0(
      nota_dependencias,
      "<p class=\"nota\">Las dependencias se buscaron sobre una muestra de ",
      .html_texto(format(attr(dependencias, "filas_analizadas", exact = TRUE),
                         big.mark = ".", decimal.mark = ",", scientific = FALSE)),
      " de ",
      .html_texto(format(attr(dependencias, "filas_totales", exact = TRUE),
                         big.mark = ".", decimal.mark = ",", scientific = FALSE)),
      " filas: el cumplimiento y la exactitud son los de la muestra, no los de ",
      "la tabla entera.</p>"
    )
  }
  paste0(
    "<section><h2>Perfil de datos: ", .html_texto(x$meta$nombre), "</h2>",
    "<p class=\"meta\">Corrida: ",
    .html_texto(.resumir_valor_reporte(x$meta$fecha_hora)), "</p>",
    if (proteger_datos_personales && n_protegidas) {
      paste0(
        "<p class=\"nota\">Se protegieron modas, ejemplos, evidencia y ",
        "estadisticos de orden de ",
        .html_texto(n_protegidas),
        " columna(s) con evidencia suficiente de datos personales.</p>"
      )
    } else "",
    .resumen_severidades(severidades, nrow(cobertura_diagnosticos)),
    "<h3>Resumen general</h3>", .html_tabla(general, Inf),
    "<h3>Hallazgos por severidad</h3>", .html_tabla(hallazgos, max_filas),
    "<h3>Resumen por columna</h3>", .html_tabla(x$columnas, max_filas),
    "<h3>Clasificaci\u00f3n de posibles datos personales</h3>",
    "<p class=\"nota\">La clasificaci\u00f3n informa todos los casos posibles; s\u00f3lo la evidencia discriminante activa la protecci\u00f3n y no se juzga si esos datos deben existir en la entrega.</p>",
    .html_tabla(x$datos_personales, max_filas),
    "<h3>Cobertura del an\u00e1lisis</h3>",
    "<p class=\"nota\">La ausencia de hallazgos no implica que todos los factores se hayan evaluado.</p>",
    .html_tabla(if (is.null(cobertura)) cobertura_analisis(x) else cobertura, Inf),
    "<h3>Cobertura de diagn\u00f3sticos</h3>",
    "<p class=\"nota\">Estos diagn\u00f3sticos no se evaluaron; un perfil sin hallazgos no es un perfil limpio si esta tabla tiene filas.</p>",
    .html_tabla(cobertura_diagnosticos, Inf),
    .seccion_alcance_del_perfil(x),
    "<h3>Patrones de formato</h3>",
    if (length(bloques_patrones)) paste0(bloques_patrones, collapse = "") else {
      "<p class=\"sin-registros\">No hay patrones para mostrar.</p>"
    },
    nota_columnas_patron,
    "<h3>Formatos de fecha</h3>", .html_tabla(tabla_formatos, max_filas),
    .nota_formatos_muestreados(x$formatos_fecha),
    "<h3>Dependencias funcionales</h3>",
    .html_tabla(dependencias, max_filas), nota_dependencias,
    if (!is.null(x$duplicados_aproximados)) {
      .seccion_duplicados_aproximados(
        x$duplicados_aproximados, max_filas, proteger_datos_personales
      )
    } else "",
    "</section>"
  )
}

# Las coberturas que viajan pegadas al objeto tienen que llegar al informe.
#
# El informe es la salida que MAS LEJOS llega: es la que se comparte. Y hasta el
# 2026-09-05 no publicaba ninguna de las que el objeto trae:
# `cobertura_metricas` -que dice que metrica no se pudo medir y por que- ni
# `cobertura_coleccion` -sobre cuantas tablas de la coleccion declarada se
# calculo el numero-. Una medicion donde una tabla tenia cero filas producia un
# informe identico al de una donde todo se midio.
#
# Es la cuarta aparicion en la jornada de la misma forma: el paquete declara
# algo y el consumidor siguiente lo tira.
# El objeto declara en `meta` cosas que acotan lo que se midio, y el informe
# -que es lo que se comparte con quien no corre R- las callaba. Medido: un
# perfil cuya entrada era una matriz declara `entrada_convertida` y el informe
# no decia ni "convertida" ni "matriz"; un perfil con 25 columnas numericas
# declara que el analisis de orden dejo cinco afuera y el informe no lo
# mencionaba. El muestreo y `cobertura_diagnosticos` si se publicaban: son la
# misma clase resuelta antes, y esto la termina.
.seccion_alcance_del_perfil <- function(x) {
  meta <- x$meta
  if (!is.list(meta)) return("")
  partes <- character()
  convertida <- meta$entrada_convertida
  if (is.character(convertida) && length(convertida) == 1L &&
        !is.na(convertida) && nzchar(convertida)) {
    partes <- c(partes, paste0(
      "<p class=\"nota\">Entrada convertida: ", .html_texto(convertida),
      ". El perfil describe la tabla resultante.</p>"
    ))
  }
  orden <- meta$orden_columnas
  if (is.list(orden) && isTRUE(orden$truncado) &&
        length(orden$columnas_omitidas)) {
    partes <- c(partes, paste0(
      "<p class=\"nota\">La comparaci\u00f3n de orden entre columnas se limit\u00f3 a ",
      .html_texto(as.character(orden$max_columnas)),
      " columnas: qued\u00f3 sin comparar ",
      .html_texto(paste(orden$columnas_omitidas, collapse = ", ")),
      ". Su ausencia entre las relaciones de orden no significa que no las ",
      "tengan.</p>"
    ))
  }
  if (!length(partes)) return("")
  paste0(
    "<h3>Alcance de la corrida</h3>", paste0(partes, collapse = "")
  )
}

# Una cobertura de frontera en el informe: el resumen, el motivo de cada parte
# sin medir y la advertencia. Lee lo mismo que la impresion
# (`.resumen_cobertura_frontera()`), con las columnas nombradas por la parte que
# cuenta cada nivel: tablas, colecciones u organizaciones.
.html_cobertura_frontera <- function(cc, atributo) {
  nivel <- .nivel_cobertura_frontera(atributo)
  resumen <- .resumen_cobertura_frontera(cc, atributo)
  nombres <- names(resumen)
  basicas <- nombres %in% c("declaradas", "en_el_numero", "sin_medir")
  nombres[basicas] <- paste0(nivel$partes, "_", nombres[basicas])
  tabla <- as.data.frame(
    as.list(stats::setNames(unname(resumen), nombres)),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  detalle <- .detalle_cobertura_frontera(cc, atributo)
  c(
    paste0("<h3>", .html_texto(nivel$titulo), "</h3>"), .html_tabla(tabla, Inf),
    if (!is.null(detalle)) {
      paste0(
        "<h4>", .html_texto(paste0(nivel$Partes, " sin medir, y por qu\u00e9")),
        "</h4>", .html_tabla(detalle, Inf)
      )
    } else "",
    if (!is.null(cc$advertencia)) {
      paste0("<p class=\"nota\">", .html_texto(cc$advertencia), "</p>")
    } else ""
  )
}

.seccion_coberturas_del_objeto <- function(x) {
  partes <- character()
  cobertura_metricas <- attr(x, "cobertura_metricas", exact = TRUE)
  if (inherits(cobertura_metricas, "data.frame") && nrow(cobertura_metricas)) {
    partes <- c(partes,
      "<h3>Cobertura de m\u00e9tricas</h3>",
      "<p class=\"nota\">Estas m\u00e9tricas no se pudieron medir. Su ausencia no es conformidad.</p>",
      .html_tabla(cobertura_metricas, Inf)
    )
  }
  # Una regla que declara mas metricas de las que la medicion trae deja su hueco
  # en `cobertura_reglas`. La consola lo avisa y lo imprime desde la vuelta
  # anterior; el informe -que es el que se manda a otra persona- publicaba el
  # `resultado` de la regla y nada mas, asi que el veredicto se leia como
  # completo. Una declaracion vale en todas las salidas, no solo en la primera.
  alcance_medidas <- attr(x, "alcance_medidas", exact = TRUE)
  if (inherits(alcance_medidas, "data.frame") && nrow(alcance_medidas)) {
    partes <- c(partes,
      "<h3>Alcance de las medidas</h3>",
      paste0(
        "<p class=\"nota\">Estas m\u00e9tricas midieron menos elementos de los ",
        "que hay en su universo aplicable: lo que no tiene valor no produce ",
        "medida y no cuenta como incumplimiento. El agregado se calcula sobre ",
        "las medidas publicadas.</p>"
      ),
      .html_tabla(alcance_medidas, Inf)
    )
  }
  cobertura_reglas <- attr(x, "cobertura_reglas", exact = TRUE)
  if (inherits(cobertura_reglas, "data.frame") && nrow(cobertura_reglas)) {
    partes <- c(partes,
      "<h3>Cobertura de las reglas</h3>",
      paste0(
        "<p class=\"nota\">Estas m\u00e9tricas est\u00e1n declaradas por una regla y ",
        "esta medici\u00f3n no trae ninguna medida de ellas: el veredicto de esa ",
        "regla cubre menos de lo que la regla dice.</p>"
      ),
      .html_tabla(cobertura_reglas, Inf)
    )
  }
  # Las cuatro coberturas de frontera, con el lector de la impresion. Solo se
  # publicaba la de la coleccion, y la de una organizacion -la que dice que una
  # de sus colecciones no se midio- se perdia; en su lugar quedaba la union de
  # las colecciones que SI entraron, que se lee como completa. Ronda 26.
  #
  # El informe publicaba el NOMBRE de cada parte sin medir y tiraba su MOTIVO,
  # que el objeto si trae: quien lee el HTML -que es quien no abre R- veia que
  # una tabla quedo afuera y no por que. Una tabla que no existe y una tabla
  # vacia no son el mismo problema, y el objeto las distingue. La capa final no
  # puede conservar menos que la que la alimenta.
  coberturas <- .coberturas_frontera_de(x)
  for (atributo in names(coberturas)) {
    partes <- c(partes, .html_cobertura_frontera(coberturas[[atributo]], atributo))
  }
  if (!length(partes)) return("")
  paste0(partes, collapse = "")
}

.seccion_medicion <- function(x, max_filas) {
  # Una medicion recortada conserva su clase y pasa la puerta del informe. Sin su
  # `id_medicion`, `length(unique(NULL))` es cero y la seccion publicaba "2 medidas
  # en 0 corrida(s)" al lado de una tabla con dos filas: un numero que el objeto no
  # sostiene. Es la misma puerta que `.seccion_historico()` ya cierra -declarar el
  # campo que falta-, y de las diecisiete columnas es la unica que alimenta una
  # cifra de la seccion.
  corridas <- if ("id_medicion" %in% names(x)) {
    paste0(
      " medidas en ", .html_texto(length(.identificadores_unicos(x$id_medicion))),
      " corrida(s)."
    )
  } else {
    paste0(
      " medidas. La cantidad de corridas no se puede establecer: a la ",
      "medici\u00f3n le falta la columna <code>id_medicion</code>."
    )
  }
  paste0(
    "<section><h2>Medidas de calidad</h2>",
    "<p class=\"meta\">", .html_texto(nrow(x)), corridas, "</p>",
    .html_tabla(x, max_filas),
    .seccion_coberturas_del_objeto(x), "</section>"
  )
}

# El alcance CUENTA los pares medidos que el marco no declara
# (`medidos_fuera_del_marco`) y el atributo `pares_fuera_del_marco` los NOMBRA;
# `print()` los nombra y el informe solo los contaba: "4" sin decir cuales.
# Ronda 26.
.html_pares_fuera_del_marco <- function(x) {
  fuera <- attr(x, "pares_fuera_del_marco", exact = TRUE)
  if (!length(fuera)) return("")
  paste0(
    "<h3>Pares medidos fuera del marco</h3>",
    "<p class=\"nota\">Estos pares dimensi\u00f3n-factor se midieron y el marco ",
    "no los declara: no entran en la cobertura de abajo.</p>",
    .html_tabla(data.frame(
      par_dimension_factor = as.character(fuera), stringsAsFactors = FALSE
    ), Inf)
  )
}

.seccion_tablero <- function(x, max_filas) {
  alcance <- attr(x, "alcance", exact = TRUE)
  cobertura <- attr(x, "cobertura", exact = TRUE)
  cobertura_metricas <- attr(x, "cobertura_metricas", exact = TRUE)
  paste0(
    "<section><h2>Tablero de calidad</h2>",
    "<p class=\"nota\">Cada fila declara la agregaci\u00f3n usada; el alcance ",
    "impide leer estas medidas como si cubrieran todo el marco.</p>",
    .html_tabla(x, max_filas),
    "<h3>Alcance del marco</h3>", .html_tabla(alcance, Inf),
    .html_pares_fuera_del_marco(x),
    "<h3>Detalle de cobertura</h3>", .html_tabla(cobertura, Inf),
    if (inherits(cobertura_metricas, "data.frame") &&
        nrow(cobertura_metricas)) paste0(
      "<h3>Cobertura de m\u00e9tricas</h3>",
      .html_tabla(cobertura_metricas, Inf)
    ) else "",
    "</section>"
  )
}

.clave_medida_reporte <- function(id_medicion, id_medida,
                                  metrica_instanciada) {
  .clave_bytes(do.call(paste, c(
    lapply(
      list(id_medicion, id_medida, metrica_instanciada), .clave_bytes
    ),
    sep = "\r"
  )))
}

# Las supresiones de TODO el informe, de todos los objetos que las declaran: los
# desenlaces de cada evaluacion -y de la de un analisis- y las medidas que un
# historico ya trae suprimidas. El historico se quedaba afuera, y
# `reportar(historico_calidad(ev), med)` publicaba en la medicion el valor que el
# historico de al lado tapaba. Se reunen solo las columnas que leen los que
# enmascaran -la medida, la corrida, la metrica-, que son las que todos traen.
# Ronda 26.
.desenlaces_reporte <- function(objetos) {
  columnas <- c("id_medida", "id_medicion", "metrica_instanciada", "regla", "desenlace")
  partes <- lapply(objetos, function(x) {
    propios <- if (inherits(x, "historico_calidad")) {
      .desenlaces_suprimidos_historico(x)
    } else {
      .desenlaces_de_objeto(x)
    }
    if (!inherits(propios, "data.frame") || !nrow(propios) ||
        !all(c("id_medida", "id_medicion", "metrica_instanciada") %in% names(propios))) {
      return(NULL)
    }
    propios <- as.data.frame(unclass(propios), stringsAsFactors = FALSE)
    for (columna in setdiff(columnas, names(propios))) {
      propios[[columna]] <- rep(NA_character_, nrow(propios))
    }
    as.data.frame(lapply(propios[columnas], as.character), stringsAsFactors = FALSE)
  })
  partes <- partes[!vapply(partes, is.null, logical(1L))]
  if (!length(partes)) return(NULL)
  resultado <- do.call(rbind, partes)
  rownames(resultado) <- NULL
  clave <- paste(
    .nombres_para_operar(resultado$id_medicion),
    .nombres_para_operar(resultado$id_medida),
    .nombres_para_operar(resultado$regla), sep = "\r"
  )
  resultado[!duplicated(clave), , drop = FALSE]
}

# Lo que el enmascarado vuelve texto -la columna `resultado` de una medicion con
# una medida suprimida- conserva en sus DEMAS celdas la escritura del informe.
# Se convertia con `as.character()`: el logico salia "TRUE" donde la misma tabla
# sin supresion dice "si", y el doble con quince cifras, que no es la regla de
# las cifras publicadas. Medido en la ronda 26.
.textos_celdas_reporte <- function(x) {
  if (is.logical(x)) return(.texto_logico_reporte(x))
  if (is.numeric(x) && !inherits(x, "integer64")) {
    salida <- .formatear_numeros_reporte(x)
    salida[is.na(x)] <- NA_character_
    return(salida)
  }
  as.character(x)
}

.texto_logico_reporte <- function(x) {
  ifelse(is.na(x), NA_character_, ifelse(x, "s\u00ed", "no"))
}

.desenlaces_suprimidos_historico <- function(x) {
  requeridas <- c(
    "nivel", "resultado", "objeto_medible", "id_medida", "id_medicion",
    "metrica_instanciada"
  )
  if (!inherits(x, "data.frame") || !nrow(x) || !all(requeridas %in% names(x))) {
    return(NULL)
  }
  marcadas <- .filas_suprimidas_historico(x) & !is.na(x$id_medida)
  if (!any(marcadas)) return(NULL)
  data.frame(
    id_medida = as.character(x$id_medida[marcadas]),
    id_medicion = as.character(x$id_medicion[marcadas]),
    metrica_instanciada = as.character(x$metrica_instanciada[marcadas]),
    regla = if ("regla" %in% names(x)) as.character(x$regla[marcadas]) else NA_character_,
    desenlace = "suprimir",
    stringsAsFactors = FALSE
  )
}

.proteger_objeto_desenlaces <- function(x, desenlaces) {
  if (inherits(x, "medicion")) {
    return(.proteger_medicion_desenlaces(
      x, desenlaces, como_texto = .textos_celdas_reporte
    ))
  }
  # El historico trae las mismas claves que la medicion -`id_medicion`,
  # `id_medida`, `metrica_instanciada`- y su `resultado`: una medida suprimida en
  # la evaluacion se publicaba en la seccion Historico del mismo informe. La
  # promesa es sobre "las mismas medidas incluidas en el documento", no sobre una
  # lista de clases. Medido en una refutacion.
  if (inherits(x, "historico_calidad")) {
    return(.proteger_medicion_desenlaces(
      x, desenlaces, como_texto = .textos_celdas_reporte
    ))
  }
  if (inherits(x, "evaluacion_calidad")) {
    return(.proteger_evaluacion_desenlaces(
      x, desenlaces = desenlaces, como_texto = .textos_celdas_reporte
    ))
  }
  if (inherits(x, "analisis")) {
    if (!is.null(x$medicion)) {
      x$medicion <- .proteger_medicion_desenlaces(
        x$medicion, desenlaces, como_texto = .textos_celdas_reporte
      )
    }
    if (!is.null(x$detalle_medicion)) {
      x$detalle_medicion <- .proteger_medicion_desenlaces(
        x$detalle_medicion, desenlaces, como_texto = .textos_celdas_reporte
      )
    }
    if (!is.null(x$tablero)) {
      x$tablero <- .proteger_tablero_desenlaces(
        x$tablero, desenlaces, como_texto = .textos_celdas_reporte
      )
    }
    if (!is.null(x$evaluacion)) {
      x$evaluacion <- .proteger_evaluacion_desenlaces(
        x$evaluacion, desenlaces = desenlaces,
        como_texto = .textos_celdas_reporte
      )
    }
  }
  x
}

.seccion_duplicados_aproximados <- function(
    x, max_filas, proteger_datos_personales = TRUE) {
  alcance <- x$alcance
  pares <- x$pares
  estimacion <- if (is.list(x$estimacion) && length(x$estimacion)) {
    data.frame(
      indicador = names(x$estimacion),
      valor = vapply(seq_along(x$estimacion), function(i) {
        valor <- x$estimacion[[i]]
        if (length(valor) != 1L || is.na(valor)) return("NA")
        if (names(x$estimacion)[[i]] %in% c(
          "candidatos_previstos", "muestra_estimacion", "pares_benchmark",
          "vocabulario"
        )) {
          return(format(round(as.numeric(valor)), big.mark = ".",
                         decimal.mark = ",", scientific = FALSE, trim = TRUE))
        }
        as.character(valor)
      }, character(1L)),
      stringsAsFactors = FALSE
    )
  } else NULL
  if (proteger_datos_personales &&
      isFALSE(x$proteccion_aplicada) && nrow(pares)) {
    pares$evidencia_1 <- "[valor protegido]"
    pares$evidencia_2 <- "[valor protegido]"
    pares$proteccion_evidencia <- "[valores personales protegidos]"
  }
  nota <- if (!isTRUE(x$disponible)) {
    paste0(
      "<p class=\"nota\">No se ejecut\u00f3 la comparaci\u00f3n aproximada: ",
      .html_texto(x$razon), "</p>"
    )
  } else {
    paste0(
      "<p class=\"nota\">La similitud no demuestra identidad. Se us\u00f3 ",
      .html_texto(x$metodo), " con umbral ", .html_texto(x$umbral),
      "; el alcance y los pares omitidos se muestran abajo.</p>",
      if (identical(x$alcance$modo_comparacion[[1L]], "lsh_minhash")) paste0(
        "<p class=\"nota\">Con MinHash/LSH el conjunto de <em>candidatos</em> ",
        "depende del orden de las filas: el vocabulario de q-gramas se numera ",
        "por orden de primera aparicion y esa numeracion alimenta las firmas. ",
        "Ocurre dentro de la garantia declarada en <code>lsh_garantia_jaccard_*</code>",
        " -medido, ningun par con Jaccard de q-gramas sobre 0,8 se pierde al ",
        "reordenar-, pero conviene saberlo antes de comparar dos corridas sobre ",
        "el mismo contenido exportado en distinto orden.</p>"
      ) else ""
    )
  }
  paste0(
    "<section><h2>Duplicados aproximados</h2>", nota,
    "<p class=\"nota\">`muestra` es el limite solicitado; `muestra_efectiva` y ",
    "`n_filas_muestra` indican cuantas filas entraron realmente en la comparacion. ",
    "La comparacion por bloques es exhaustiva para esas filas; el tamano del bloque ",
    "y los pares que quedaron fuera se declaran en el alcance.</p>",
    "<p class=\"nota\"><code>tipo_par</code> distingue texto guardado ",
    "igual (<code>exacto</code>), coincidencia creada por la normalizacion ",
    "(<code>exacto_normalizado</code>) y similitud (<code>aproximado</code>). ",
    "<code>igualo_normalizar</code> marca solo el segundo caso.</p>",
    "<h3>Alcance de la comparaci\u00f3n</h3>", .html_tabla(alcance, Inf),
    if (!is.null(estimacion)) paste0(
      "<h3>Referencia temporal</h3><p class=\"nota\">",
      "La referencia se midio en esta corrida, no es determinista y solo ",
      "estima la medida aislada; no incluye firmas, cubetas ni troceo.</p>",
      .html_tabla(estimacion, Inf)
    ) else "",
    "<h3>Pares detectados</h3>", .html_tabla(pares, max_filas),
    if (isTRUE(x$alcance$truncado[[1L]])) {
      paste0(
        "<p class=\"nota\">La tabla de pares mostrados est\u00e1 truncada; el ",
        "total hallado permanece en el alcance.",
        if (isTRUE(x$alcance$corte_en_empate[[1L]])) paste0(
          " <strong>El corte cay\u00f3 dentro de un empate</strong>: ",
          x$alcance$n_en_distancia_corte[[1L]], " de los pares conservados ",
          "estan a la misma distancia (", x$alcance$distancia_corte[[1L]],
          "), asi que quedaron afuera pares igual de cercanos. Cuales se ",
          "conservan se decide por el orden canonico de los valores, no por el ",
          "orden en que llegaron las filas, de modo que el resultado no cambia ",
          "si se reordena la tabla; pero sigue siendo un subconjunto. Para no ",
          "dejar afuera pares empatados, suba <code>max_resultados</code> por ",
          "encima del empate."
        ) else "",
        "</p>"
      )
    } else "",
    "</section>"
  )
}

.seccion_evaluacion <- function(x, max_filas) {
  paste0(
    "<section><h2>Evaluaci\u00f3n de calidad</h2>",
    if (!is.null(x$desenlaces)) {
      paste0(
        "<h3>Plan de desenlaces</h3>",
        "<p class=\"nota\">Los desenlaces provienen exclusivamente de reglas ",
        "declaradas; este reporte no modifica la medici\u00f3n ni los datos.</p>",
        .html_tabla(x$desenlaces, max_filas)
      )
    } else "",
    "<h3>Evaluaciones de medidas</h3>", .html_tabla(x$medidas, max_filas),
    "<h3>Evaluaciones de reglas</h3>", .html_tabla(x$reglas, max_filas),
    "<h3>Perfiles de madurez</h3>", .html_tabla(x$perfiles, max_filas),
    .seccion_coberturas_del_objeto(x), "</section>"
  )
}

.evolucion_historico <- function(x) {
  perfiles <- .seleccionar_columnas(
    x, c("id_medicion", "fecha", "perfil", "resultado"),
    filas = x$nivel == "evaluacion_perfil"
  )
  if (!nrow(perfiles)) return(perfiles)
  perfiles <- perfiles[order(
    .nombres_para_operar(perfiles$perfil), perfiles$fecha,
    .nombres_para_operar(perfiles$id_medicion), method = "radix"
  ), ]
  # El delta sale de `detectar_deriva_calidad()` y no de un `diff()` propio. El
  # `diff()` era la misma regla escrita dos veces, y la copia no sabia lo que la
  # original decide: la deriva declara NO COMPARABLE un par con cambio de marco o
  # de `tipo_resultado` -"no se publica la comparacion del resultado"- y esta
  # tabla publicaba igual su delta, dos secciones mas abajo y sin marca. Medido:
  # r1 (marco A) -> r2 (marco B) salia con `-0.667` al lado de esa declaracion.
  # La copia tampoco separaba las tablas: la deriva compara dentro de la misma
  # tabla y el `diff()` restaba corridas de tablas distintas bajo el mismo perfil.
  #
  # `comparacion` dice por que un delta falta, con la palabra de la deriva; y
  # `comparado_con`, contra que corrida se resto, que ya no es siempre la fila de
  # arriba.
  perfiles$delta <- NA_real_
  perfiles$comparado_con <- NA_character_
  perfiles$comparacion <- NA_character_
  deriva <- tryCatch(
    as.data.frame(detectar_deriva_calidad(x, nivel = "perfil")),
    error = function(e) e
  )
  if (inherits(deriva, "error")) {
    perfiles$comparacion <- paste0(
      "La evoluci\u00f3n no se pudo calcular: ", conditionMessage(deriva)
    )
  } else {
    clave <- function(perfil, id) {
      .clave_bytes(paste(
        .clave_bytes(as.character(perfil)), .clave_bytes(as.character(id)),
        sep = "\034"
      ))
    }
    claves <- clave(perfiles$perfil, perfiles$id_medicion)
    claves_deriva <- clave(deriva$perfil, deriva$id_medicion_actual)
    perfil_de <- .clave_bytes(as.character(perfiles$perfil))
    configuracion <- attr(x, "configuracion_evaluacion", exact = TRUE)
    # Todas las filas del par -la de resultado y las que declaran un cambio-, y
    # una sola lectura, la de `comparar_evaluaciones()`. Mirar solo la etiqueta
    # `no_comparable` borraba el delta de un cambio de modelo que la deriva
    # declara comparable -"se mantienen las comparaciones"- y publica. Ronda 25.
    #
    # Y una corrida que la deriva no junta con ninguna -la anterior del perfil
    # mide otra tabla- dejaba delta, `comparado_con` y `comparacion` vacios, y la
    # serie 0.75 -> 1 de dos tablas se leia como una mejora. Se nombra la corrida
    # anterior del perfil y la lectura dice por que no se comparan. Ronda 26.
    for (k in seq_along(claves)) {
      filas <- which(claves_deriva == claves[[k]])
      if (!length(filas)) {
        previa <- k - 1L
        if (previa >= 1L && identical(perfil_de[[previa]], perfil_de[[k]])) {
          perfiles$comparado_con[[k]] <- as.character(perfiles$id_medicion[[previa]])
          perfiles$comparacion[[k]] <- .lectura_par_deriva(
            deriva, filas, configuracion = configuracion,
            perfil = perfiles$perfil[[k]],
            ids = c(perfiles$id_medicion[[previa]], perfiles$id_medicion[[k]])
          )$comparacion
        }
        next
      }
      lectura <- .lectura_par_deriva(deriva, filas)
      perfiles$delta[[k]] <- lectura$delta
      perfiles$comparado_con[[k]] <- as.character(
        deriva$id_medicion_anterior[[filas[[1L]]]]
      )
      perfiles$comparacion[[k]] <- lectura$comparacion
    }
  }
  rownames(perfiles) <- NULL
  perfiles
}

.seccion_historico <- function(x, max_filas) {
  # Un historico al que le falta una columna mataba el informe ENTERO -perfil y
  # mediciones incluidos- con el mensaje de R base que sale de `order(NULL)`:
  # "argumento 1 no es un vector". Ni la seccion, ni el campo, ni que hacer.
  #
  # El subset es legitimo y el objeto CONSERVA su clase, asi que `reportar()` lo
  # acepta y recien revienta adentro. Recorri las clases reportables quitandoles
  # una columna por vez: de las tabulares, `medicion` aguanta las 17 y un
  # `plan_limpieza` recortado PIERDE su clase, asi que ahi el usuario ya recibe el
  # mensaje deliberado de "esto no es un objeto reportable". El historico es el
  # unico que pasa la puerta roto, y son cinco de sus veintidos columnas.
  #
  # No se arma una version parcial de la seccion: se declara que no se pudo armar
  # y se nombra el campo que falta, que es lo que el paquete hace en cada lugar
  # donde no puede medir. Una omision se informa DENTRO del informe.
  requeridas <- c("fecha", "id_medicion", "nivel", "perfil", "resultado")
  faltantes <- setdiff(requeridas, names(x))
  if (length(faltantes)) {
    return(paste0(
      "<section><h2>Hist\u00f3rico de calidad</h2>",
      "<p class=\"meta\">Esta secci\u00f3n no se pudo armar: al hist\u00f3rico le ",
      "falta(n) la(s) columna(s) ",
      .html_texto(paste(faltantes, collapse = ", ")),
      ". Su ausencia no es conformidad. Para armarla, reportar el objeto que ",
      "devuelve <code>historico_calidad()</code> sin quitarle columnas.</p>",
      "</section>"
    ))
  }
  x <- x[order(
    x$fecha, .nombres_para_operar(x$id_medicion), x$nivel, method = "radix"
  ), , drop = FALSE]
  # Lo que NO se midio -una metrica sin valores, una parte de la frontera que no
  # entro al numero- vive en el historico como una fila mas, y ordenada por
  # `nivel` caia detras de las medidas: una medicion de 120 celdas con una
  # metrica que no se pudo medir dejaba esa declaracion en la fila 121, afuera
  # del corte de `max_filas` por omision, y el informe solo decia "se muestran
  # 100 de 121 filas". La misma medicion reportada directamente la publica en su
  # cobertura, sin tope. Aca tambien: aparte y sin tope, como las coberturas de
  # las demas secciones. Ronda 26.
  ausencias <- as.character(x$nivel) %in% c("metrica_no_evaluada", "parte_no_medida")
  no_medido <- if (any(ausencias)) {
    paste0(
      "<h3>Lo que no se midi\u00f3</h3>",
      "<p class=\"nota\">M\u00e9tricas que no se pudieron medir y partes ",
      "declaradas de una frontera que no entraron al n\u00famero, por corrida: ",
      "la serie de esas corridas no cubre lo que su modelo declara. Su ",
      "ausencia no es conformidad.</p>",
      .html_tabla(
        x[ausencias, , drop = FALSE], Inf,
        columnas = c(
          "id_medicion", "fecha", "nivel", "id_registro", "metrica_instanciada",
          "granularidad", "entidad", "atributo", "objeto_medible", "agregacion"
        )
      )
    )
  } else ""
  paste0(
    "<section><h2>Hist\u00f3rico de calidad</h2>",
    "<p class=\"meta\">", .html_texto(length(.identificadores_unicos(x$id_medicion))),
    " corrida(s); esquema ", .html_texto(attr(x, "version_esquema")), ".</p>",
    "<h3>Evoluci\u00f3n de perfiles de madurez</h3>",
    .html_tabla(.evolucion_historico(x), max_filas),
    no_medido,
    "<h3>Registros hist\u00f3ricos</h3>", .html_tabla(x, max_filas),
    "</section>"
  )
}

.seccion_deriva <- function(x, max_filas, titulo) {
  severidades <- if ("severidad" %in% names(x)) x$severidad else character()
  # Lo que la deriva declara que NO pudo comparar viaja tambien aca, por la misma
  # razon que lo hace el plan de limpieza dos funciones mas abajo: la declaracion
  # vale en todas las salidas, no solo en la impresion. Una tabla de deriva vacia
  # porque no hay con que comparar y una vacia porque nada cambio son la misma
  # pantalla, y la diferencia es justo lo que el lector necesita.
  cobertura <- attr(x, "cobertura_diagnosticos", exact = TRUE)
  declaracion <- if (inherits(cobertura, "data.frame") && nrow(cobertura)) {
    paste0(
      "<h3>Lo que no se pudo comparar</h3>",
      "<p class=\"nota\">Su ausencia no es conformidad.</p>",
      .html_tabla(cobertura, max_filas)
    )
  } else {
    ""
  }
  paste0(
    "<section><h2>", titulo, "</h2>",
    .resumen_severidades(severidades),
    .html_tabla(x, max_filas), declaracion, "</section>"
  )
}

.seccion_plan <- function(x, max_filas) {
  resumen <- data.frame(
    indicador = c(
      "Acciones propuestas", "Acciones activas", "Acciones recomendadas",
      "Acciones destructivas"
    ),
    valor = c(
      nrow(x), sum(x$aplicar, na.rm = TRUE), sum(x$recomendada, na.rm = TRUE),
      sum(x$destructiva, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  # Lo que el plan declara que NO cubre tiene que viajar tambien aca. El
  # informe publicaba "Acciones propuestas: 5" y nada mas: un hallazgo medido
  # que no produjo accion quedaba invisible justo en la salida que se manda a
  # otra persona. La declaracion vale en todas las salidas, no solo en la
  # impresion de consola.
  paste0(
    "<section><h2>Plan de limpieza</h2>",
    "<p>El plan es una propuesta editable; este informe no aplica cambios.</p>",
    .html_tabla(resumen, Inf),
    "<h3>Acciones y justificaci\u00f3n</h3>", .html_tabla(x, max_filas),
    .html_huecos_plan(x, max_filas),
    "</section>"
  )
}

.html_huecos_plan <- function(x, max_filas) {
  partes <- character()
  # Las TRES declaraciones de lo que el plan no cubre (`?planificar_limpieza`).
  # El informe publicaba dos: `cobertura_diagnosticos` -los diagnosticos que el
  # perfil no pudo evaluar- se quedaba en la consola, y un plan mandado solo, sin
  # su perfil al lado, se leia como "lo demas esta bien". Sin tope, como la
  # cobertura de diagnosticos del perfil: es lo que no se midio. Ronda 26.
  no_evaluados <- attr(x, "cobertura_diagnosticos", exact = TRUE)
  if (inherits(no_evaluados, "data.frame") && nrow(no_evaluados)) {
    partes <- c(partes, paste0(
      "<h3>Diagn\u00f3sticos no evaluados</h3>",
      "<p>El perfil no pudo evaluarlos y por eso el plan no tiene acci\u00f3n ",
      "para ellos: su ausencia entre las acciones no dice que esas columnas ",
      "est\u00e9n bien.</p>",
      .html_tabla(no_evaluados, Inf)
    ))
  }
  sin_accion <- attr(x, "hallazgos_sin_accion", exact = TRUE)
  if (inherits(sin_accion, "data.frame") && nrow(sin_accion)) {
    partes <- c(partes, paste0(
      "<h3>Hallazgos medidos sin acci\u00f3n</h3>",
      "<p>El perfil los midi\u00f3 y el plan no propone una acci\u00f3n para ",
      "ellos: leer las acciones de arriba no dice que lo dem\u00e1s est\u00e9 ",
      "bien.</p>",
      .html_tabla(sin_accion, max_filas)
    ))
  }
  ambiguos <- attr(x, "hallazgos_sin_accion_por_columna_ambigua", exact = TRUE)
  if (inherits(ambiguos, "data.frame") && nrow(ambiguos)) {
    partes <- c(partes, paste0(
      "<h3>Hallazgos sin acci\u00f3n por columna ambigua</h3>",
      "<p>Su columna comparte nombre con otra, as\u00ed que no hay forma de ",
      "saber sobre cu\u00e1l actuar\u00eda la limpieza.</p>",
      .html_tabla(ambiguos, max_filas)
    ))
  }
  paste(partes, collapse = "")
}

.nota_propuesta_releida <- function(x) {
  if (!inherits(x$advertencias, "data.frame") ||
      !all(c("tipo", "descripcion") %in% names(x$advertencias))) {
    return("")
  }
  aviso <- x$advertencias[
    as.character(x$advertencias$tipo) ==
      "persistencia_funciones_sustituidas", , drop = FALSE
  ]
  if (!nrow(aviso)) return("")
  paste0(
    "<p class=\"nota\">", .html_texto(aviso$descripcion[[1L]]), "</p>"
  )
}

.seccion_analisis <- function(x, max_filas, max_patrones,
                              proteger_datos_personales) {
  if (proteger_datos_personales) x <- .proteger_analisis(x)
  alcance_asociaciones <- .tabla_indicadores_reporte(
    c(
      "Filas analizadas", "Muestreado", "Columnas analizadas",
      "Columnas no analizables", "Columnas omitidas por limite",
      "Pares examinables", "Pares omitidos por dependencia",
      "Asociaciones antes del recorte", "Salida truncada", "Umbral"
    ),
    list(
      attr(x$asociaciones, "filas_analizadas", exact = TRUE),
      attr(x$asociaciones, "muestreado", exact = TRUE),
      length(attr(x$asociaciones, "columnas_analizadas", exact = TRUE)),
      length(attr(x$asociaciones, "columnas_no_analizables", exact = TRUE)),
      length(attr(x$asociaciones, "columnas_omitidas_limite", exact = TRUE)),
      attr(x$asociaciones, "pares_posibles", exact = TRUE),
      attr(x$asociaciones, "pares_omitidos_dependencia", exact = TRUE),
      attr(x$asociaciones, "total_informadas", exact = TRUE),
      attr(x$asociaciones, "truncado", exact = TRUE),
      attr(x$asociaciones, "umbral", exact = TRUE)
    )
  )
  alcance_temporal <- .tabla_indicadores_reporte(
    c("Columnas analizadas", "Columnas omitidas", "Salida truncada"),
    list(
      length(attr(x$temporal, "columnas_analizadas", exact = TRUE)),
      length(attr(x$temporal, "columnas_omitidas", exact = TRUE)),
      attr(x$temporal, "truncado", exact = TRUE)
    )
  )
  distribuciones <- paste0(
    "<section><h2>Distribuciones y cuantiles</h2>",
    if (proteger_datos_personales) {
      paste0(
        "<p class=\"nota\">Los cuantiles personales conservan su ",
        "probabilidad, pero muestran valor ausente y estado ",
        "valor_protegido.</p>"
      )
    } else "",
    "<h3>Alcance por columna</h3>",
    .html_tabla(x$distribuciones$alcance, max_filas),
    "<h3>Frecuencias principales</h3>",
    .html_tabla(x$distribuciones$frecuencias, max_filas),
    "<h3>Cuantiles</h3>", .html_tabla(x$distribuciones$cuantiles, max_filas),
    "</section>"
  )
  asociaciones <- paste0(
    "<section><h2>Asociaciones entre columnas</h2>",
    "<p class=\"nota\">Metodo y soporte se declaran por par; el resultado no implica causalidad.</p>",
    "<h3>Alcance y recortes</h3>", .html_tabla(alcance_asociaciones, Inf),
    "<h3>Asociaciones informadas</h3>",
    .html_tabla(x$asociaciones, max_filas), "</section>"
  )
  temporal <- paste0(
    "<section><h2>Analisis temporal</h2>",
    paste0(
      "<p class=\"nota\">Las frecuencias inferidas son propuestas no ",
      "confirmadas; los rangos y huecos personales se marcan como ",
      "protegidos.</p>"
    ),
    "<h3>Alcance y recortes</h3>", .html_tabla(alcance_temporal, Inf),
    "<h3>Resumen</h3>", .html_tabla(x$temporal$resumen, max_filas),
    "<h3>Propuestas de frecuencia</h3>",
    .html_tabla(x$temporal$propuestas, max_filas),
    "<h3>Distribucion por dia de semana</h3>",
    .html_tabla(x$temporal$dias_semana, max_filas),
    "<h3>Huecos</h3>", .html_tabla(x$temporal$huecos, max_filas),
    "</section>"
  )
  variables <- paste0(
    "<section><h2>Escalas y roles propuestos</h2>",
    "<p class=\"nota\">Una escala basada solo en valores requiere confirmacion.</p>",
    .html_tabla(x$variables, max_filas), "</section>"
  )
  propuesta <- paste0(
    "<section><h2>Propuesta de modelo</h2>",
    .nota_propuesta_releida(x),
    "<p class=\"nota\">",
    if (isTRUE(x$meta$propuesta_confirmada)) {
      "La selecci\u00f3n de medici\u00f3n fue confirmada por quien realiz\u00f3 el an\u00e1lisis. "
    } else {
      "La propuesta es de lupa y nadie la confirm\u00f3. "
    },
    "La tabla siguiente declara qu\u00e9 m\u00e9tricas se midieron, cu\u00e1les ",
    "quedaron afuera y por qu\u00e9.</p>",
    "<h3>Decisi\u00f3n de medici\u00f3n</h3>",
    .html_tabla(x$decision_medicion, max_filas),
    "<h3>Propuesta completa</h3>",
    .html_tabla(x$propuesta_modelo, max_filas), "</section>"
  )
  advertencias <- paste0(
    "<section><h2>Advertencias de alcance</h2>",
    .resumen_severidades(x$advertencias$severidad),
    .html_tabla(x$advertencias, max_filas), "</section>"
  )
  paste0(
    "<section><h2>Analisis integral</h2><p class=\"meta\">Esquema ",
    .html_texto(x$meta$version_esquema), "; datos conservados: ",
    .html_texto(x$meta$datos_conservados), ".</p></section>",
    advertencias,
    .seccion_perfil(
      x$perfil, max_filas, max_patrones, proteger_datos_personales,
      cobertura = x$cobertura
    ),
    distribuciones, asociaciones, temporal, variables, propuesta,
    .seccion_tablero(x$tablero, max_filas),
    if (!is.null(x$medicion)) {
      .seccion_medicion(x$medicion, max_filas)
    } else "",
    if (!is.null(x$detalle_medicion)) {
      paste0(
        "<section><h2>Detalle de medici\u00f3n conservado</h2>",
        "<p class=\"nota\">Este detalle fila a fila se conserv\u00f3 por pedido ",
        "expl\u00edcito.</p>", .html_tabla(x$detalle_medicion, max_filas),
        "</section>"
      )
    } else "",
    if (!is.null(x$evaluacion)) .seccion_evaluacion(x$evaluacion, max_filas) else "",
    .seccion_plan(x$plan_limpieza, max_filas)
  )
}

.clase_objeto_reporte <- function(x) {
  clases <- c(
    "analisis", "perfil", "medicion", "evaluacion_calidad", "historico_calidad",
    "deriva_perfil", "deriva_calidad", "plan_limpieza",
    "duplicados_aproximados"
  )
  coincidencias <- clases[vapply(clases, inherits, logical(1L), x = x)]
  if (length(coincidencias)) coincidencias[[1L]] else NA_character_
}

.aplanar_objetos_reporte <- function(objetos) {
  salida <- list()
  recorrer <- function(x) {
    clase <- .clase_objeto_reporte(x)
    if (!is.na(clase)) {
      salida[[length(salida) + 1L]] <<- x
    } else if (inherits(x, "perfil_dbi")) {
      # Sin esto, el objeto de `perfilar_dbi()` caia en la rama de lista, se
      # recorria por dentro y el error salia sobre alguno de sus componentes:
      # el usuario leia una enumeracion de clases que no le decia que hacer.
      stop(
        "Un objeto de `perfilar_dbi()` no se reporta entero: pase su perfil ",
        "de muestra, `x$perfil_muestra`. El resumen SQL de la tabla completa ",
        "esta en `x$resumen_tabla` y no es un perfil.",
        call. = FALSE
      )
    } else if (is.list(x) && !inherits(x, "data.frame")) {
      for (elemento in x) recorrer(elemento)
    } else {
      stop(
        "Cada objeto debe ser un analisis, perfil, medicion, evaluacion_calidad, ",
        "historico_calidad, deriva_perfil, deriva_calidad, plan_limpieza o ",
        "duplicados_aproximados.",
        call. = FALSE
      )
    }
  }
  for (objeto in objetos) recorrer(objeto)
  if (!length(salida)) stop("Se necesita al menos un objeto para el reporte.", call. = FALSE)
  salida
}

# Una seccion que no se puede armar se declara dentro del informe y no lo
# interrumpe, como promete `?reportar`: un perfil al que le faltaba
# `dependencias` o `hallazgos` mataba el informe entero con un mensaje de R base
# -"argumento tiene longitud cero"-, sin nombrar el objeto. Medido en una
# refutacion.
.renderizar_objeto_reporte_declarado <- function(x, ...) {
  tryCatch(
    .renderizar_objeto_reporte(x, ...),
    error = function(e) {
      paste0(
        "<section><h2>Secci\u00f3n no armada</h2><p class=\"nota\">",
        .html_texto(paste0(
          "Un objeto de clase `", class(x)[[1L]], "` no se pudo armar: ",
          conditionMessage(e), ". Puede faltarle un componente que la ",
          "secci\u00f3n necesita. El resto del informe se escribi\u00f3 igual."
        )),
        "</p></section>"
      )
    }
  )
}

.renderizar_objeto_reporte <- function(x, max_filas, max_patrones,
                                       proteger_datos_personales = TRUE,
                                       desenlaces = NULL) {
  # Un plan, una deriva, una medicion o un historico armados con la proteccion APAGADA sobre
  # columnas personales llevan sus valores en claro, y el informe -que protege por
  # omision- no los puede volver a tapar: no tiene la clasificacion ni la tabla.
  # Se publicaban al lado de un perfil enmascarado del mismo archivo; medido en
  # una refutacion. La seccion se omite y se dice por que: para publicarla, hay
  # que pedirlo tambien aca, con `proteger_datos_personales = FALSE`.
  sin_proteger <- attr(x, "datos_personales_sin_proteger", exact = TRUE)
  if (isTRUE(proteger_datos_personales) && length(sin_proteger)) {
    return(paste0(
      "<section><h2>Secci\u00f3n omitida</h2><p class=\"nota\">",
      .html_texto(paste0(
        "Un objeto de clase `", class(x)[[1L]], "` se arm\u00f3 con la ",
        "protecci\u00f3n de datos personales desactivada y lleva en claro ",
        length(sin_proteger), " columna(s) personal(es). El informe protege y ",
        "no puede volver a tapar ese objeto, as\u00ed que no lo publica. Para ",
        "incluirlo, genere el objeto con la protecci\u00f3n activa o use ",
        "`reportar(..., proteger_datos_personales = FALSE)`."
      )),
      "</p></section>"
    ))
  }
  x <- .proteger_objeto_desenlaces(x, desenlaces)
  # Un perfil -o un analisis sin `datos`- armado con la proteccion apagada se
  # vuelve a proteger con lo que conserva: modas, ejemplos, estadisticos. Sin la
  # tabla, una variante de un valor protegido escrita en otra columna -la cedula
  # con puntos dentro de un texto libre- puede quedar, cuando `perfilar()` con la
  # proteccion puesta la tapa. Medido en una refutacion. No se omite -la
  # re-proteccion tapa casi todo y esta documentada-, pero se dice en la seccion.
  nota <- ""
  if (isTRUE(proteger_datos_personales)) {
    origen <- if (inherits(x, "analisis")) x$perfil else if (inherits(x, "perfil")) x
    sin_tabla <- inherits(x, "perfil") || is.null(x$datos)
    if (is.list(origen) && is.list(origen$meta) &&
        isFALSE(origen$meta$proteger_datos_personales) && sin_tabla &&
        length(.columnas_personales_protegidas(origen))) {
      nota <- paste0("<p class=\"nota\">", .html_texto(paste0(
        "Este perfil se arm\u00f3 con la protecci\u00f3n de datos personales ",
        "desactivada. El informe lo vuelve a proteger con lo que el perfil ",
        "conserva, sin la tabla: una variante de un valor protegido escrita en ",
        "otra columna puede quedar a la vista. Para la protecci\u00f3n completa, ",
        "genere el perfil con la protecci\u00f3n activa."
      )), "</p>")
    }
  }
  seccion <- switch(
    .clase_objeto_reporte(x),
    analisis = .seccion_analisis(
      x, max_filas, max_patrones, proteger_datos_personales
    ),
    perfil = .seccion_perfil(
      x, max_filas, max_patrones, proteger_datos_personales
    ),
    medicion = .seccion_medicion(x, max_filas),
    evaluacion_calidad = .seccion_evaluacion(x, max_filas),
    historico_calidad = .seccion_historico(x, max_filas),
    deriva_perfil = .seccion_deriva(x, max_filas, "Deriva del perfil de datos"),
    deriva_calidad = .seccion_deriva(x, max_filas, "Deriva del modelo de calidad"),
    plan_limpieza = .seccion_plan(x, max_filas),
    duplicados_aproximados = .seccion_duplicados_aproximados(
      x, max_filas, proteger_datos_personales
    )
  )
  if (nzchar(nota)) seccion <- sub("</h2>", paste0("</h2>", nota), seccion, fixed = TRUE)
  seccion
}

.css_reporte <- function() {
  paste0(
    "*{box-sizing:border-box}body{margin:0;background:#f4f6f8;color:#1e293b;",
    "font-family:system-ui,-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif;",
    "line-height:1.45}main{max-width:1180px;margin:0 auto;padding:2rem}",
    "header,section{background:#fff;border:1px solid #dbe2ea;border-radius:10px;",
    "padding:1.4rem;margin-bottom:1rem}header{border-top:6px solid #335c81}",
    "h1{margin:.1rem 0 .4rem;color:#17324d}h2{color:#234e70;margin-top:0}",
    "h3{margin-top:1.6rem}h4{margin-bottom:.45rem}.meta,.nota{color:#526373}",
    ".nota{font-size:.9rem}.sin-registros,.sin-dato{color:#6b7280;font-style:italic}",
    ".tabla-contenedor{overflow-x:auto;border:1px solid #dbe2ea;border-radius:6px}",
    "table{border-collapse:collapse;width:100%;font-size:.88rem;background:#fff}",
    "th,td{padding:.5rem .65rem;border-bottom:1px solid #e5e9ee;text-align:left;",
    "vertical-align:top;white-space:normal}th{background:#edf3f7;position:sticky;top:0}",
    "tbody tr:nth-child(even){background:#fafbfc}.tarjetas{display:flex;gap:.7rem;",
    "flex-wrap:wrap;margin:1rem 0}.tarjeta{min-width:150px;padding:.75rem 1rem;",
    "border-left:5px solid #64748b;background:#f8fafc}.tarjeta span{display:block;",
    "font-size:.82rem}.tarjeta strong{font-size:1.5rem}.tarjeta.error{border-color:#b42318}",
    ".tarjeta.sospechoso{border-color:#b7791f}.tarjeta.ok{border-color:#27864a}",
    ".nivel-error{color:#8a1c13;font-weight:700}.nivel-sospechoso{color:#8a5800;",
    "font-weight:700}.nivel-ok{color:#176b38;font-weight:700}footer{text-align:center;",
    "color:#66788a;padding:1rem}@media(max-width:700px){main{padding:.6rem}",
    "header,section{padding:1rem}}@media print{body{background:#fff;color:#000}",
    "main{max-width:none;padding:0}header,section{break-inside:avoid;border-color:#aaa;",
    "box-shadow:none}.tabla-contenedor{overflow:visible}th{position:static}",
    "footer{display:none}}"
  )
}

.copiar_archivo_reporte <- function(origen, destino, sobrescribir) {
  file.copy(origen, destino, overwrite = sobrescribir)
}

.escribir_reporte <- function(contenido, archivo, sobrescribir) {
  if (!.es_texto_escalar(archivo)) {
    stop("`archivo` debe ser una ruta no vac\u00eda.", call. = FALSE)
  }
  if (!is.logical(sobrescribir) || length(sobrescribir) != 1L ||
      is.na(sobrescribir)) {
    stop("`sobrescribir` debe ser TRUE o FALSE.", call. = FALSE)
  }
  directorio <- .validar_destino_archivo(archivo, sobrescribir)
  temporal <- tempfile(".lupa-reporte-", tmpdir = directorio, fileext = ".html")
  on.exit(unlink(temporal), add = TRUE)

  bytes <- tryCatch({
    if (!is.character(contenido) || length(contenido) != 1L ||
        is.na(contenido)) {
      stop("el contenido no es una cadena escalar")
    }
    utf8 <- enc2utf8(contenido)
    if (!isTRUE(validUTF8(utf8))) {
      stop("el contenido no es UTF-8 valido")
    }
    c(charToRaw(utf8), as.raw(0x0a))
  }, error = function(e) {
    stop("No se pudo escribir el reporte completo: ", conditionMessage(e),
         call. = FALSE)
  })

  conexion <- NULL
  verificar <- function(ruta, etapa) {
    tamano <- file.info(ruta)$size
    if (length(tamano) != 1L || is.na(tamano) || tamano != length(bytes)) {
      stop(
        "tamano inesperado en ", etapa, " (esperados ", length(bytes),
        ", escritos ", if (length(tamano)) tamano else "NA", ")"
      )
    }
    leidos <- readBin(ruta, what = "raw", n = tamano)
    if (!identical(leidos, bytes)) {
      stop("los bytes escritos no coinciden con el contenido completo")
    }
    invisible(NULL)
  }
  tryCatch(
    withCallingHandlers({
      conexion <- file(temporal, open = "wb")
      writeBin(bytes, conexion)
      close(conexion)
      conexion <- NULL
      verificar(temporal, "el temporal")
    }, warning = function(e) {
      stop(conditionMessage(e), call. = FALSE)
    }),
    error = function(e) {
      if (!is.null(conexion) && isOpen(conexion)) {
        try(close(conexion), silent = TRUE)
      }
      stop("No se pudo escribir el reporte completo: ", conditionMessage(e),
           call. = FALSE)
    }
  )

  tryCatch(
    withCallingHandlers({
      if (!isTRUE(.copiar_archivo_reporte(temporal, archivo, sobrescribir))) {
        stop("la copia al destino devolvio FALSE", call. = FALSE)
      }
      verificar(archivo, "el destino")
    }, warning = function(e) {
      stop(conditionMessage(e), call. = FALSE)
    }),
    error = function(e) {
      stop("No se pudo escribir el reporte completo: ", conditionMessage(e),
           call. = FALSE)
    }
  )
  invisible(normalizePath(archivo, winslash = "/", mustWork = TRUE))
}

#' Crear un reporte HTML autocontenido
#'
#' Genera un unico archivo HTML en espanol usando solo funciones de R base. El
#' estilo se incluye dentro del documento: no requiere red, navegador especial,
#' conversor externo ni archivos auxiliares. Cada valor dinamico se escapa antes
#' de incorporarlo al documento.
#'
#' Se pueden combinar objetos producidos por el profiling, la medicion, la
#' evaluacion, el historico, las comparaciones de deriva y la planificacion de
#' limpieza. Cada tipo anade su seccion; el reporte no modifica datos ni aplica
#' planes. Si una evaluacion contiene desenlaces de supresion declarados por
#' reglas, el reporte enmascara su `valor_medido` y el `resultado` de las mismas
#' medidas incluidas en el documento, también en las otras evaluaciones del
#' informe: la supresión de una vale para todas, en cualquier orden, y la de
#' una medida que un histórico del informe ya trae suprimida, también. El
#' enmascarado se hace sobre copias y no modifica los objetos recibidos.
#'
#' Una seccion que no se puede armar **se declara dentro del informe** y no lo
#' interrumpe: si a un `historico_calidad` le faltan campos que su seccion
#' necesita -`fecha`, `id_medicion`, `nivel`, `perfil` o `resultado`-, la seccion
#' dice cuales faltan y el resto del documento se escribe igual. Lo mismo vale
#' para cualquier otra seccion que falle al armarse -un perfil al que le falta un
#' componente-: queda una seccion que lo dice. Un texto con bytes que no son
#' UTF-8 valido se muestra con esos bytes en hexadecimal, como los muestra la
#' consola, y no interrumpe el informe. Un carácter de control —que un
#' navegador no dibuja— se escribe con su código, `<U+0001>`, como en la
#' evidencia de los hallazgos. Las cifras se escriben con todos sus digitos significativos, sin el
#' redondeo de la consola: con las cifras que vuelven al valor del objeto
#' —quince, o dieciséis o diecisiete cuando hacen falta: `0.1 + 0.2` se escribe
#' `0.30000000000000004`— y sin notación científica. Una cifra no se abrevia
#' como un texto: la que sin exponente pasaría de 240 caracteres
#' —`8.98846567431158e+307`, el ausente de Stata leído como número— se escribe
#' con exponente y las mismas cifras, y una celda con varias se abrevia por
#' cifras enteras.
#'
#' @param x Un objeto compatible o una lista de objetos compatibles.
#' @param ... Objetos adicionales de clase `analisis`, `perfil`, `medicion`,
#'   `evaluacion_calidad`, `historico_calidad`, `deriva_perfil`,
#'   `deriva_calidad`, `plan_limpieza` o `duplicados_aproximados`.
#' @param archivo Ruta de salida. De forma predeterminada crea un archivo en
#'   `tempdir()`. Una ruta que ya es un directorio se rechaza.
#' @param sobrescribir Si se permite reemplazar un archivo existente.
#' @param titulo Titulo visible del reporte.
#' @param fecha Fecha y hora de generacion, inyectable para obtener resultados
#'   reproducibles. Se normaliza a UTC.
#' @param max_filas Maximo de filas por tabla y de columnas del perfil cuyos
#'   patrones se detallan. Las omisiones se informan dentro del reporte.
#' @param max_patrones Maximo de patrones mostrados por columna. Las omisiones
#'   se informan dentro del reporte.
#' @param proteger_datos_personales Si se enmascaran modas, ejemplos, evidencia,
#'   estadisticos de orden, cuantiles y rangos temporales de columnas
#'   cuya clasificacion activa proteccion automatica. Es `TRUE` por defecto.
#'   Las coincidencias debiles se informan sin suprimir. Para ver valores
#'   concretos deben haberse conservado tambien con
#'   `perfilar(..., proteger_datos_personales = FALSE)`.
#'
#'   El enmascarado es el mismo que el de [perfilar()] -ver su
#'   `proteger_datos_personales`-: la forma exacta, la que solo difiere en
#'   separadores, tildes, caja o escritura, y en los digitos, el documento
#'   entero con cualquier agrupacion, sin su verificador, o sin su primer digito
#'   detras de un comodin. La comparacion parcial se limita a digitos a proposito:
#'   aplicada al texto taparia una palabra corriente por compartir un tramo con
#'   un apellido, y eso silencia contenido del informe en vez de proteger un
#'   dato.
#'
#'   Un plan de limpieza, una medicion, un historico que incluye una medicion
#'   o una comparacion de deriva armados con la proteccion desactivada no se
#'   publican en un informe protegido: su seccion se reemplaza por una que dice
#'   por que se omitio y como pedirla. Lo sabe por una marca que el objeto
#'   lleva, y que `[`, `subset()` y `rbind()` conservan. En el
#'   historico, una medida suprimida por una regla se enmascara como en la
#'   evaluacion. Un perfil -o un analisis que no conserva la tabla- armado con
#'   la proteccion desactivada se vuelve a proteger con lo que conserva, y su
#'   seccion lo declara: sin la tabla, una variante de un valor protegido
#'   escrita en otra columna puede quedar a la vista. Para la proteccion
#'   completa, el perfil se arma con la proteccion activa.
#'
#' @return La ruta normalizada del archivo, de forma invisible.
#' @export
#' @seealso [perfilar()], [medir()], [evaluar()], [historico_calidad()],
#'   [comparar_perfiles()], [planificar_limpieza()]
#'
#' @examples
#' perfil <- perfilar(datos_administrativos)
#' archivo <- reportar(perfil)
#' unlink(archivo)
reportar <- function(x, ...,
                     archivo = tempfile("reporte-lupa-", fileext = ".html"),
                     sobrescribir = FALSE,
                     titulo = "Reporte de calidad de datos",
                     fecha = Sys.time(), max_filas = 100L,
                     max_patrones = 20L,
                     proteger_datos_personales = TRUE) {
  objetos <- .aplanar_objetos_reporte(c(list(x), list(...)))
  if (!.es_texto_escalar(titulo)) {
    stop("`titulo` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  max_filas <- .validar_limite_reporte(max_filas, "max_filas")
  max_patrones <- .validar_limite_reporte(max_patrones, "max_patrones")
  if (!is.logical(proteger_datos_personales) ||
      length(proteger_datos_personales) != 1L ||
      is.na(proteger_datos_personales)) {
    stop("`proteger_datos_personales` debe ser TRUE o FALSE.", call. = FALSE)
  }
  fecha <- tryCatch(.fecha_utc(fecha), error = function(e) NA)
  if (length(fecha) != 1L || is.na(fecha) || !is.finite(as.numeric(fecha))) {
    stop("`fecha` debe contener una fecha y hora v\u00e1lida.", call. = FALSE)
  }
  secciones <- vapply(objetos, .renderizar_objeto_reporte_declarado, character(1L),
                      max_filas = max_filas, max_patrones = max_patrones,
                      proteger_datos_personales = proteger_datos_personales,
                      desenlaces = .desenlaces_reporte(objetos))
  documento <- paste0(
    "<!doctype html><html lang=\"es\"><head><meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    # En `<title>` no hay marcado: un salto de linea se volvia `<br>` y se veia
    # como texto en la pestana. Ahi va un espacio.
    "<title>", .html_escapar(gsub("[\r\n]+", " ", titulo)), "</title><style>",
    .css_reporte(),
    "</style></head><body><main><header><h1>", .html_texto(titulo), "</h1>",
    "<p class=\"meta\">Generado: ",
    .html_texto(.resumir_valor_reporte(fecha)), " \u00b7 Archivo: ",
    .html_texto(basename(archivo)), "</p>",
    "<p class=\"nota\">Los textos de m\u00e1s de 240 caracteres se abrevian con ",
    "un punto suspensivo; las cifras no: la que no cabe se escribe con ",
    "exponente. Los caracteres de control, que un navegador no dibuja, se ",
    "escriben con su c\u00f3digo, como &lt;U+0001&gt;. Cada truncamiento de ",
    "filas o patrones se indica en su secci\u00f3n.</p></header>",
    paste0(secciones, collapse = ""),
    "<footer>Reporte autocontenido generado con lupa ",
    .html_texto(.version_paquete()), ".</footer></main></body></html>"
  )
  .escribir_reporte(documento, archivo, sobrescribir)
}
