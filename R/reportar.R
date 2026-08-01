#' Crear un reporte de perfil
#'
#' Interfaz reservada para una versión futura. El MVP devuelve resultados
#' reutilizables en consola y como data frames, pero no genera archivos HTML.
#'
#' @param x Objeto de clase `perfil`.
#' @param ... Argumentos reservados.
#'
#' @return `NULL`, de forma invisible.
#' @export
reportar <- function(x, ...) {
  if (!inherits(x, "perfil")) {
    stop("`x` debe ser un objeto de clase perfil.", call. = FALSE)
  }
  warning(
    "El reporte HTML estar\u00e1 disponible en una pr\u00f3xima versi\u00f3n.",
    call. = FALSE
  )
  invisible(NULL)
}
