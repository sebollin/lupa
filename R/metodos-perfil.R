#' @export
print.perfil <- function(x, ...) {
  errores <- sum(x$hallazgos$severidad == "error")
  sospechosos <- sum(x$hallazgos$severidad == "sospechoso")
  correctos <- sum(x$hallazgos$severidad == "ok")

  cli::cli_h1(paste("Perfil de datos:", x$meta$nombre))
  cli::cli_alert_danger(paste(errores, "hallazgos con severidad error"))
  cli::cli_alert_warning(paste(sospechosos, "hallazgos sospechosos"))
  cli::cli_alert_success(paste(correctos, "hallazgos informativos ok"))

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
    "columna", "tipo_declarado", "tipo_inferido", "prop_faltantes_totales",
    "n_distintos", "n_outliers"
  )]
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
#' @return Un `tibble` con una fila por columna.
#' @keywords internal
#' @exportS3Method tibble::as_tibble
as_tibble.perfil <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Para esta conversi\u00f3n se necesita instalar el paquete 'tibble'.", call. = FALSE)
  }
  tibble::as_tibble(x$columnas, ...)
}
