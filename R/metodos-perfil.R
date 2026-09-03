#' @export
print.perfil <- function(x, ...) {
  errores <- sum(x$hallazgos$severidad == "error")
  sospechosos <- sum(x$hallazgos$severidad == "sospechoso")
  correctos <- sum(x$hallazgos$severidad == "ok")
  no_evaluados <- if (inherits(x$cobertura_diagnosticos, "data.frame")) {
    nrow(x$cobertura_diagnosticos)
  } else 0L

  cli::cli_h1(paste("Perfil de datos:", x$meta$nombre))
  cli::cli_alert_danger(paste(errores, "hallazgos con severidad error"))
  cli::cli_alert_warning(paste(sospechosos, "hallazgos sospechosos"))
  cli::cli_alert_success(paste(correctos, "hallazgos informativos ok"))
  cli::cli_alert_info(paste(no_evaluados, "diagnosticos no evaluados"))

  cli::cli_h2("Resumen general")
  cli::cli_dl(c(
    "Filas" = format(
      x$general$filas, big.mark = ".", decimal.mark = ",", scientific = FALSE
    ),
    "Columnas" = x$general$columnas,
    "Celdas" = format(
      x$general$celdas, big.mark = ".", decimal.mark = ",", scientific = FALSE
    ),
    "Filas completas" = x$general$filas_completas,
    "Filas duplicadas" = x$general$filas_duplicadas,
    "Memoria" = format(
      structure(x$general$memoria_bytes, class = "object_size"), units = "auto"
    )
  ))

  cli::cli_h2("Resumen por columna")
  vista <- x$columnas[c(
    "columna", "tipo_inferido", "prop_faltantes_totales", "n_distintos",
    "n_outliers"
  )]
  if ("estado_tipo_inferido" %in% names(x$columnas)) {
    candidatos <- which(
      !is.na(x$columnas$estado_tipo_inferido) &
        x$columnas$estado_tipo_inferido == "candidato"
    )
    declarados <- as.character(x$columnas$tipo_declarado)
    inferidos <- as.character(x$columnas$tipo_inferido)
    vista$tipo_inferido[candidatos] <- paste0(
      declarados[candidatos], " ", intToUtf8(8594L), " ",
      inferidos[candidatos], " (candidato)"
    )
  }
  print(vista, row.names = FALSE)
  invisible(x)
}

#' @export
summary.perfil <- function(object, ...) {
  object$columnas
}

#' @export
as.data.frame.perfil <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x$columnas, row.names = row.names, optional = optional, ...)
}

#' Convertir un perfil a tibble
#'
#' Método opcional para [tibble::as_tibble()]. Requiere que `tibble` esté
#' instalado.
#'
#' @param x Objeto de clase `perfil`.
#' @param ... Argumentos enviados a [tibble::as_tibble()].
#'
#' @return Un `tibble` con una fila por columna del perfil y las mismas
#'   variables que `x$columnas`: es la tabla de columnas del perfil, no el
#'   perfil entero -los hallazgos, la cobertura y los metadatos no viajan-.
#' @keywords internal
#' @exportS3Method tibble::as_tibble
#'
#' @examples
#' perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
#' if (requireNamespace("tibble", quietly = TRUE)) {
#'   tibble::as_tibble(perfil)
#' }
as_tibble.perfil <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Para esta conversi\u00f3n se necesita instalar el paquete 'tibble'.", call. = FALSE)
  }
  tibble::as_tibble(x$columnas, ...)
}

# Las tres formas de salida del paquete -`perfil`, `perfil_dbi` y
# `perfil_coleccion`- guardan lo mismo en lugares distintos, y eso convirtio la
# lectura en un acertijo: `perfil$general$filas` funciona en la primera y
# devuelve `NULL` en la segunda, donde el conteo vive en
# `resumen_tabla$meta$filas`. Un `NULL` silencioso en un guion de medicion es la
# peor forma de fallar: no avisa, y lo que sigue calcula sobre nada.
#
# Estos accesores no agregan informacion; le ponen un solo nombre a la que ya
# esta. La forma concreta de cada objeto sigue disponible y documentada para
# quien la necesite.

.fuente_hallazgos <- function(x) {
  if (inherits(x, "perfil")) return(x$hallazgos)
  if (inherits(x, "analisis")) return(x$perfil$hallazgos)
  if (inherits(x, "perfil_dbi")) {
    return(if (is.null(x$perfil_muestra)) NULL else x$perfil_muestra$hallazgos)
  }
  NULL
}

.estado_bloque_muestra_dbi <- function(x) {
  if (!inherits(x, "perfil_dbi")) return(NA_character_)
  cobertura <- tryCatch(x$resumen_tabla$cobertura, error = function(e) NULL)
  if (is.data.frame(cobertura) && nrow(cobertura) &&
      all(c("bloque", "estado") %in% names(cobertura))) {
    fila <- cobertura[cobertura$bloque == "perfil_muestra", , drop = FALSE]
    if (nrow(fila)) return(as.character(fila$estado[[1L]]))
  }
  if (is.null(x$perfil_muestra)) "no_disponible" else "disponible"
}

.fuente_columnas <- function(x) {
  if (inherits(x, "perfil")) return(x$columnas)
  if (inherits(x, "analisis")) return(x$perfil$columnas)
  if (inherits(x, "perfil_dbi")) return(x$resumen_tabla$columnas)
  if (inherits(x, "perfil_coleccion")) return(x$resumen_coleccion)
  NULL
}

.fuente_cobertura <- function(x) {
  if (inherits(x, "perfil")) return(x$cobertura_diagnosticos)
  if (inherits(x, "analisis")) return(x$perfil$cobertura_diagnosticos)
  if (inherits(x, "perfil_dbi")) return(x$resumen_tabla$cobertura)
  if (inherits(x, "perfil_coleccion")) return(x$cobertura_coleccion)
  NULL
}

# La forma vacia sale de la misma fabrica que las filas reales y se recorta a
# cero. Construirla a mano invitaba a que se desincronizara con el dia en que se
# agregue una columna al hallazgo.
.hallazgos_sin_filas <- function() {
  .nuevo_hallazgo("", "", "ok", "", "", "")[0L, , drop = FALSE]
}

.exigir_forma_conocida <- function(x, que) {
  formas <- c("perfil", "analisis", "perfil_dbi", "perfil_coleccion")
  if (!inherits(x, formas)) {
    stop(
      "`x` debe ser un objeto de perfilar(), analizar(), perfilar_dbi() o ",
      "perfilar_coleccion(); recibi un objeto de clase '",
      paste(class(x), collapse = "/"), "'.", call. = FALSE
    )
  }
  invisible(que)
}

#' Leer un perfil sin depender de su forma
#'
#' `perfilar()`, `perfilar_dbi()` y `perfilar_coleccion()` devuelven objetos con
#' formas distintas, y eso convertía la lectura en un acertijo:
#' `perfil$general$filas` funciona sobre la salida en memoria y devuelve `NULL`
#' sobre la salida DBI, donde el conteo vive en `resumen_tabla$meta$filas`. Un
#' `NULL` silencioso en un guion de medición es la peor forma de fallar: no
#' avisa, y lo que sigue calcula sobre nada.
#'
#' Estos accesores no agregan información: le ponen un solo nombre a la que ya
#' está. La forma concreta de cada objeto sigue disponible y documentada.
#'
#' **Lo que no hay, no se inventa.** Un `perfilar_dbi()` sin muestra leída no
#' tiene hallazgos por fila, y `hallazgos()` devuelve una tabla vacía **con su
#' aviso**, no una tabla que aparente que se midió y no había nada.
#'
#' @param x Objeto de [perfilar()], [analizar()], [perfilar_dbi()] o
#'   [perfilar_coleccion()].
#' @param ... Sin uso, para compatibilidad de métodos.
#'
#' @return `hallazgos()` y `columnas()` devuelven `data.frame`; `cobertura()`
#'   devuelve la tabla de diagnósticos no evaluados; `n_filas()` devuelve el
#'   conteo de filas del alcance, o `NA` con su motivo cuando el objeto no lo
#'   conoce.
#' @export
#' @name accesores_perfil
#' @seealso [perfilar()], [perfilar_dbi()], [perfilar_coleccion()]
#'
#' @examples
#' perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
#' nrow(hallazgos(perfil))
#' n_filas(perfil)
#' nrow(cobertura(perfil))
hallazgos <- function(x, ...) {
  .exigir_forma_conocida(x, "hallazgos")
  salida <- .fuente_hallazgos(x)
  if (is.null(salida)) {
    if (inherits(x, "perfil_dbi")) {
      if (identical(.estado_bloque_muestra_dbi(x), "no_solicitado")) {
        warning(
          "Este perfil DBI no solicito la muestra, asi que no hay hallazgos por ",
          "fila. El resumen por columna sigue disponible en columnas(); el ",
          "detalle, en cobertura().", call. = FALSE
        )
      } else {
        warning(
          "Este perfil DBI no trae muestra leida, asi que no hay hallazgos por ",
          "fila. El resumen por columna sigue disponible en columnas(); el ",
          "motivo, en cobertura().", call. = FALSE
        )
      }
      return(.hallazgos_sin_filas())
    }
    return(.hallazgos_sin_filas())
  }
  salida
}

#' @rdname accesores_perfil
#' @export
columnas <- function(x, ...) {
  .exigir_forma_conocida(x, "columnas")
  .fuente_columnas(x)
}

#' @rdname accesores_perfil
#' @export
cobertura <- function(x, ...) {
  .exigir_forma_conocida(x, "cobertura")
  salida <- .fuente_cobertura(x)
  if (is.null(salida)) .cobertura_diagnosticos_vacia() else salida
}

#' @rdname accesores_perfil
#' @export
n_filas <- function(x, ...) {
  .exigir_forma_conocida(x, "n_filas")
  if (inherits(x, "analisis")) x <- x$perfil
  if (inherits(x, "perfil")) return(x$general$filas)
  if (inherits(x, "perfil_dbi")) return(x$resumen_tabla$meta$filas)
  if (inherits(x, "perfil_coleccion")) {
    filas <- x$resumen_coleccion$filas
    if (is.null(filas)) {
      # Una coleccion no tiene "un" numero de filas, y sumarlas seria inventar
      # un total que ninguna tabla tiene.
      return(NA_real_)
    }
    return(filas)
  }
  NA_real_
}

#' @rdname accesores_perfil
#' @export
sql_perfil <- function(x, ...) {
  .exigir_forma_conocida(x, "sql_perfil")
  if (inherits(x, "perfil_dbi")) return(x$resumen_tabla$sql)
  if (inherits(x, "perfil_coleccion")) return(x$meta$lecturas)
  # Un perfil en memoria no emitio SQL, y decirlo con una tabla vacia seria
  # sugerir que emitio y no encontro.
  NULL
}
