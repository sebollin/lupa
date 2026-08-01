.deriva_perfil_vacia <- function() {
  resultado <- data.frame(
    columna = character(), aspecto = character(), cambio = character(),
    severidad = character(), valor_anterior = character(),
    valor_actual = character(), delta = numeric(),
    cambio_relativo = numeric(), significativo = logical(),
    fecha_anterior = as.POSIXct(character(), tz = "UTC"),
    fecha_actual = as.POSIXct(character(), tz = "UTC"),
    descripcion = character(), evidencia = character(),
    stringsAsFactors = FALSE
  )
  resultado$severidad <- factor(
    resultado$severidad, levels = c("ok", "sospechoso", "error"),
    ordered = TRUE
  )
  class(resultado) <- c("deriva_perfil", "data.frame")
  resultado
}

.validar_perfil_deriva <- function(x, nombre) {
  if (!inherits(x, "perfil") || !inherits(x$columnas, "data.frame") ||
      !is.list(x$patrones) || !inherits(x$hallazgos, "data.frame") ||
      is.null(x$meta$fecha_hora)) {
    stop("`", nombre, "` debe ser un objeto producido por perfilar().",
         call. = FALSE)
  }
  x
}

.mapa_columnas_perfil <- function(perfil) {
  nombres <- as.character(perfil$columnas$columna)
  nombres[is.na(nombres) | !nzchar(nombres)] <- "<sin_nombre>"
  data.frame(
    clave = make.unique(nombres), original = nombres,
    indice = seq_along(nombres), stringsAsFactors = FALSE
  )
}

.distinto_deriva <- function(a, b) {
  if (length(a) != 1L || length(b) != 1L) return(TRUE)
  if (is.na(a) && is.na(b)) return(FALSE)
  if (is.na(a) || is.na(b)) return(TRUE)
  !isTRUE(all.equal(a, b, check.attributes = FALSE))
}

.texto_deriva <- function(x) {
  if (!length(x) || is.na(x)) return(NA_character_)
  if (is.numeric(x)) return(format(x, digits = 8L, trim = TRUE))
  as.character(x)
}

.configuracion_patrones_perfil <- function(perfil) {
  campos <- c(
    "muestra", "max_patrones", "distinguir_mayusculas", "expandir",
    "umbral_patron_raro"
  )
  if (!all(campos %in% names(perfil$meta))) return(NULL)
  perfil$meta[campos]
}

.rango_perfil <- function(fila) {
  if (is.finite(fila$minimo) && is.finite(fila$maximo)) {
    return(list(
      tipo = "numerico", minimo = fila$minimo, maximo = fila$maximo,
      texto = paste0("[", .texto_deriva(fila$minimo), ", ",
                     .texto_deriva(fila$maximo), "]")
    ))
  }
  if (!is.na(fila$minimo_fecha) && !is.na(fila$maximo_fecha)) {
    a <- as.numeric(as.POSIXct(fila$minimo_fecha, tz = "UTC"))
    b <- as.numeric(as.POSIXct(fila$maximo_fecha, tz = "UTC"))
    return(list(
      tipo = "fecha", minimo = a, maximo = b,
      texto = paste0("[", fila$minimo_fecha, ", ", fila$maximo_fecha, "]")
    ))
  }
  NULL
}

.magnitud_rango <- function(anterior, actual) {
  if (is.null(anterior) || is.null(actual) ||
      anterior$tipo != actual$tipo) return(Inf)
  ancho <- abs(anterior$maximo - anterior$minimo)
  escala <- if (is.finite(ancho) && ancho > 0) ancho else {
    max(abs(c(anterior$minimo, anterior$maximo)), 1)
  }
  max(
    abs(actual$minimo - anterior$minimo),
    abs(actual$maximo - anterior$maximo)
  ) / escala
}

.patrones_perfil <- function(perfil, indice) {
  x <- perfil$patrones[[indice]]
  if (!inherits(x, "data.frame") || !all(c("patron", "proporcion") %in% names(x))) {
    return(data.frame(
      patron = character(), proporcion = numeric(), stringsAsFactors = FALSE
    ))
  }
  x[c("patron", "proporcion")]
}

.clave_patron <- function(x) {
  ifelse(is.na(x), "<NA>", as.character(x))
}

.resumir_hallazgos_deriva <- function(perfil) {
  x <- perfil$hallazgos
  if (!nrow(x)) {
    return(data.frame(
      clave = character(), columna = character(), tipo_hallazgo = character(),
      severidad = character(), evidencia = character(), stringsAsFactors = FALSE
    ))
  }
  columna <- ifelse(is.na(x$columna), "<tabla>", as.character(x$columna))
  clave <- paste(columna, x$tipo_hallazgo, sep = "\034")
  grupos <- split(seq_len(nrow(x)), clave, drop = TRUE)
  partes <- lapply(grupos, function(indices) {
    nivel <- match(as.character(x$severidad[indices]),
                   c("ok", "sospechoso", "error"))
    elegido <- indices[which.max(nivel)]
    data.frame(
      clave = clave[[elegido]], columna = columna[[elegido]],
      tipo_hallazgo = x$tipo_hallazgo[[elegido]],
      severidad = as.character(x$severidad[[elegido]]),
      evidencia = x$evidencia[[elegido]], stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, partes)
  rownames(resultado) <- NULL
  resultado
}

#' Comparar dos perfiles y detectar deriva estructural
#'
#' Compara entregas sin exigir que tengan las mismas columnas. Devuelve cambios
#' de esquema, tipos, faltantes, cardinalidad, rango, patrones y hallazgos como
#' un objeto de datos filtrable.
#'
#' @param anterior,actual Objetos producidos por [perfilar()].
#' @param umbral_cambio Diferencia mínima para considerar significativo un
#'   cambio de proporción, cardinalidad o rango relativo. Cinco puntos
#'   porcentuales evita elevar variaciones pequeñas a hallazgo.
#' @param umbral_error Diferencia a partir de la cual un aumento de faltantes o
#'   un patrón nuevo se clasifica como `error`.
#'
#' @return Data frame `deriva_perfil`. `severidad` usa el factor ordenado
#'   `ok < sospechoso < error`; los cambios menores permanecen como filas `ok`
#'   para que la serie sea exportable sin ocultar diferencias.
#'
#' @details
#' Los patrones se comparan sobre el resumen acotado que conserva cada perfil,
#' no sobre los valores originales ni una distribución completa. Si las dos
#' corridas usaron configuraciones de patrones diferentes, se informa un error
#' de comparabilidad y esa parte de la comparación se omite.
#'
#' Las columnas que aparecen o desaparecen generan cambios estructurales de
#' severidad `error`, pero no impiden comparar las columnas compartidas. Un
#' hallazgo de una columna retirada no se presenta como resuelto.
#'
#' @export
#'
#' @examples
#' anterior <- perfilar(data.frame(codigo = c("AA1", "AA2")),
#'                      fecha = as.POSIXct("2026-01-01", tz = "UTC"))
#' actual <- perfilar(data.frame(codigo = c("AA1", "B-2"), nueva = 1:2),
#'                    fecha = as.POSIXct("2026-02-01", tz = "UTC"))
#' comparar_perfiles(anterior, actual)
comparar_perfiles <- function(anterior, actual, umbral_cambio = 0.05,
                              umbral_error = 0.20) {
  anterior <- .validar_perfil_deriva(anterior, "anterior")
  actual <- .validar_perfil_deriva(actual, "actual")
  umbrales <- c(umbral_cambio, umbral_error)
  if (!is.numeric(umbrales) || anyNA(umbrales) ||
      any(!is.finite(umbrales)) || any(umbrales < 0 | umbrales > 1) ||
      umbral_error < umbral_cambio) {
    stop(
      "Los umbrales deben estar en [0, 1] y `umbral_error` no puede ser menor.",
      call. = FALSE
    )
  }
  fecha_anterior <- .fecha_utc(anterior$meta$fecha_hora)
  fecha_actual <- .fecha_utc(actual$meta$fecha_hora)
  mapa_a <- .mapa_columnas_perfil(anterior)
  mapa_b <- .mapa_columnas_perfil(actual)
  cambios <- list()
  k <- 0L
  agregar <- function(columna, aspecto, cambio, severidad, valor_anterior,
                      valor_actual, delta = NA_real_, cambio_relativo = NA_real_,
                      significativo = TRUE, descripcion, evidencia = "") {
    k <<- k + 1L
    cambios[[k]] <<- data.frame(
      columna = columna, aspecto = aspecto, cambio = cambio,
      severidad = severidad, valor_anterior = .texto_deriva(valor_anterior),
      valor_actual = .texto_deriva(valor_actual), delta = delta,
      cambio_relativo = cambio_relativo, significativo = significativo,
      fecha_anterior = fecha_anterior, fecha_actual = fecha_actual,
      descripcion = descripcion, evidencia = evidencia,
      stringsAsFactors = FALSE
    )
  }

  desaparecidas <- setdiff(mapa_a$clave, mapa_b$clave)
  aparecidas <- setdiff(mapa_b$clave, mapa_a$clave)
  for (columna in desaparecidas) {
    agregar(
      columna, "columna", "desaparecida", "error", columna, NA_character_,
      descripcion = "La columna desapareci\u00f3 de la entrega actual.",
      evidencia = "No se comparan sus m\u00e9tricas, patrones ni rango."
    )
  }
  for (columna in aparecidas) {
    agregar(
      columna, "columna", "aparecida", "error", NA_character_, columna,
      descripcion = "Apareci\u00f3 una columna que no estaba en la entrega anterior.",
      evidencia = "No existe una base anterior para sus m\u00e9tricas."
    )
  }

  comunes <- intersect(mapa_a$clave, mapa_b$clave)
  for (columna in comunes) {
    indice_a <- mapa_a$indice[match(columna, mapa_a$clave)]
    indice_b <- mapa_b$indice[match(columna, mapa_b$clave)]
    a <- anterior$columnas[indice_a, , drop = FALSE]
    b <- actual$columnas[indice_b, , drop = FALSE]
    for (campo in c("tipo_declarado", "tipo_inferido")) {
      if (.distinto_deriva(a[[campo]], b[[campo]])) {
        agregar(
          columna, campo, "modificado", "error", a[[campo]], b[[campo]],
          descripcion = paste0(
            "Cambi\u00f3 el ", gsub("_", " ", campo, fixed = TRUE),
            " de la columna."
          )
        )
      }
    }
    for (especificacion in list(
      c(campo = "prop_faltantes_totales", aspecto = "faltantes"),
      c(campo = "tasa_distintos", aspecto = "cardinalidad")
    )) {
      campo <- especificacion[["campo"]]
      aspecto <- especificacion[["aspecto"]]
      if (.distinto_deriva(a[[campo]], b[[campo]])) {
        delta <- b[[campo]] - a[[campo]]
        significativo <- is.finite(delta) && abs(delta) >= umbral_cambio
        severidad <- if (!significativo || delta < 0) {
          "ok"
        } else if (aspecto == "faltantes" && delta >= umbral_error) {
          "error"
        } else {
          "sospechoso"
        }
        evidencia <- if (aspecto == "cardinalidad") {
          paste0(
            "Valores distintos: ", a$n_distintos, " -> ", b$n_distintos,
            "; tasas: ", sprintf("%.4f", a[[campo]]), " -> ",
            sprintf("%.4f", b[[campo]])
          )
        } else {
          paste0("Cambio de ", sprintf("%+.4f", delta), " en escala [0, 1].")
        }
        descripcion <- if (aspecto == "faltantes") {
          "Cambi\u00f3 la proporci\u00f3n de faltantes de la columna."
        } else {
          "Cambi\u00f3 la tasa de valores distintos de la columna."
        }
        agregar(
          columna, aspecto, "modificado", severidad, a[[campo]], b[[campo]],
          delta = delta, significativo = significativo,
          descripcion = descripcion,
          evidencia = evidencia
        )
      }
    }
    rango_a <- .rango_perfil(a)
    rango_b <- .rango_perfil(b)
    if ((!is.null(rango_a) || !is.null(rango_b)) &&
        (is.null(rango_a) || is.null(rango_b) ||
         rango_a$texto != rango_b$texto || rango_a$tipo != rango_b$tipo)) {
      magnitud <- .magnitud_rango(rango_a, rango_b)
      significativo <- is.infinite(magnitud) || magnitud >= umbral_cambio
      agregar(
        columna, "rango", "modificado",
        if (significativo) "sospechoso" else "ok",
        if (is.null(rango_a)) NA_character_ else rango_a$texto,
        if (is.null(rango_b)) NA_character_ else rango_b$texto,
        cambio_relativo = magnitud, significativo = significativo,
        descripcion = "Cambi\u00f3 el rango observado de la columna."
      )
    }
  }

  config_a <- .configuracion_patrones_perfil(anterior)
  config_b <- .configuracion_patrones_perfil(actual)
  patrones_comparables <- !is.null(config_a) && !is.null(config_b) &&
    isTRUE(all.equal(config_a, config_b, check.attributes = FALSE))
  if (!patrones_comparables) {
    agregar(
      NA_character_, "configuracion_patrones", "no_comparable", "error",
      paste(unlist(config_a), collapse = "; "),
      paste(unlist(config_b), collapse = "; "),
      descripcion = paste0(
        "Las corridas no declaran la misma configuraci\u00f3n de patrones; ",
        "sus patrones no se comparan."
      )
    )
  } else {
    for (columna in comunes) {
      indice_a <- mapa_a$indice[match(columna, mapa_a$clave)]
      indice_b <- mapa_b$indice[match(columna, mapa_b$clave)]
      pa <- .patrones_perfil(anterior, indice_a)
      pb <- .patrones_perfil(actual, indice_b)
      claves_a <- .clave_patron(pa$patron)
      claves_b <- .clave_patron(pb$patron)
      for (patron in setdiff(claves_b, claves_a)) {
        proporcion <- pb$proporcion[match(patron, claves_b)]
        agregar(
          columna, "patron", "aparecido",
          if (proporcion >= umbral_error) "error" else "sospechoso",
          NA_character_, patron, delta = proporcion, significativo = TRUE,
          descripcion = "Apareci\u00f3 un patr\u00f3n de formato nuevo.",
          evidencia = paste0("Proporci\u00f3n actual: ", sprintf("%.4f", proporcion))
        )
      }
      for (patron in setdiff(claves_a, claves_b)) {
        proporcion <- pa$proporcion[match(patron, claves_a)]
        agregar(
          columna, "patron", "desaparecido",
          if (proporcion >= umbral_error) "error" else "sospechoso",
          patron, NA_character_, delta = -proporcion, significativo = TRUE,
          descripcion = "Desapareci\u00f3 un patr\u00f3n de formato anterior.",
          evidencia = paste0("Proporci\u00f3n anterior: ", sprintf("%.4f", proporcion))
        )
      }
    }
  }

  hallazgos_a <- .resumir_hallazgos_deriva(anterior)
  hallazgos_b <- .resumir_hallazgos_deriva(actual)
  nuevos <- setdiff(hallazgos_b$clave, hallazgos_a$clave)
  resueltos <- setdiff(hallazgos_a$clave, hallazgos_b$clave)
  nombres_actuales <- unique(mapa_b$original)
  for (clave in nuevos) {
    x <- hallazgos_b[match(clave, hallazgos_b$clave), , drop = FALSE]
    agregar(
      if (x$columna == "<tabla>") NA_character_ else x$columna,
      "hallazgo", "aparecido", x$severidad, NA_character_, x$tipo_hallazgo,
      descripcion = "Apareci\u00f3 un hallazgo que no estaba en el perfil anterior.",
      evidencia = x$evidencia
    )
  }
  for (clave in resueltos) {
    x <- hallazgos_a[match(clave, hallazgos_a$clave), , drop = FALSE]
    if (x$columna != "<tabla>" && !x$columna %in% nombres_actuales) next
    agregar(
      if (x$columna == "<tabla>") NA_character_ else x$columna,
      "hallazgo", "resuelto", "ok", x$tipo_hallazgo, NA_character_,
      descripcion = "Un hallazgo del perfil anterior ya no est\u00e1 presente.",
      evidencia = x$evidencia
    )
  }
  compartidos <- intersect(hallazgos_a$clave, hallazgos_b$clave)
  for (clave in compartidos) {
    indice_a <- match(clave, hallazgos_a$clave)
    indice_b <- match(clave, hallazgos_b$clave)
    sa <- hallazgos_a$severidad[[indice_a]]
    sb <- hallazgos_b$severidad[[indice_b]]
    if (sa != sb) {
      columna <- hallazgos_b$columna[[indice_b]]
      agregar(
        if (columna == "<tabla>") NA_character_ else columna,
        "severidad_hallazgo",
        if (match(sb, c("ok", "sospechoso", "error")) >
            match(sa, c("ok", "sospechoso", "error"))) "agravado" else "atenuado",
        sb, sa, sb,
        descripcion = "Cambi\u00f3 la severidad de un hallazgo persistente.",
        evidencia = hallazgos_b$tipo_hallazgo[[indice_b]]
      )
    }
  }

  if (!length(cambios)) return(.deriva_perfil_vacia())
  resultado <- do.call(rbind, cambios)
  resultado$severidad <- factor(
    resultado$severidad, levels = c("ok", "sospechoso", "error"),
    ordered = TRUE
  )
  rownames(resultado) <- NULL
  class(resultado) <- c("deriva_perfil", "data.frame")
  resultado
}
