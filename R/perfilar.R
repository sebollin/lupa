#' Perfilar un conjunto de datos
#'
#' Examina un `data.frame`, `tibble` o `data.table` y devuelve estadísticas
#' generales, métricas por columna, patrones, formatos de fecha y hallazgos
#' accionables. Todas las proporciones se expresan en `[0, 1]`.
#'
#' Los umbrales de faltantes se aplican a la suma de ausentes reales y
#' faltantes disfrazados. La lista de cadenas se basa en
#' `naniar::common_na_strings`, con extensiones habituales en datos
#' administrativos uruguayos. Los sentinelas numéricos predeterminados son
#' `-9`, `-99`, `-999`, `-9999` y `999`. La lista es deliberadamente más corta
#' que `naniar::common_na_numbers`: `66`, `77`, `88` y `9999` también pueden ser
#' edades, códigos o años legítimos. Para solicitar explícitamente la lista
#' completa de naniar, use `sentinelas_numericos = sentinelas_naniar`.
#'
#' @param datos Objeto que hereda de `data.frame`.
#' @param nombre Nombre descriptivo del objeto.
#' @param muestra Máximo de filas usadas para patrones e inferencia de tipos.
#' @param max_patrones Máximo de patrones mostrados por columna.
#' @param distinguir_mayusculas Si se distinguen mayúsculas y minúsculas.
#' @param expandir Si se emite un token por carácter en los patrones.
#' @param umbral_alta_cardinalidad Umbral para columnas categóricas.
#' @param umbral_faltantes_sospechoso Umbral inferior de faltantes.
#' @param umbral_faltantes_error Umbral a partir del cual son un error.
#' @param umbral_patron_raro Máxima frecuencia de un patrón raro.
#' @param umbral_patron_dominante Frecuencia mínima del patrón dominante.
#' @param columnas_sin_ceros Nombres de columnas donde cero no es admisible.
#' @param columnas_no_negativas Nombres de columnas que deben ser no negativas.
#' @param sentinelas_numericos Vector de sentinelas numéricos adicionales que
#'   representan ausencia. Se combina con la lista predeterminada.
#'
#' @return Objeto S3 de clase `perfil`.
#' @export
#'
#' @examples
#' perfil <- perfilar(datos_administrativos)
#' perfil
#' summary(perfil)
perfilar <- function(datos,
                     nombre = deparse(substitute(datos)),
                     muestra = 1e5,
                     max_patrones = 20,
                     distinguir_mayusculas = TRUE,
                     expandir = FALSE,
                     umbral_alta_cardinalidad = 0.5,
                     umbral_faltantes_sospechoso = 0.1,
                     umbral_faltantes_error = 0.4,
                     umbral_patron_raro = 0.05,
                     umbral_patron_dominante = 0.5,
                     columnas_sin_ceros = character(),
                     columnas_no_negativas = character(),
                     sentinelas_numericos = numeric()) {
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe ser un data.frame, tibble o data.table.", call. = FALSE)
  }
  umbrales <- c(
    umbral_alta_cardinalidad, umbral_faltantes_sospechoso,
    umbral_faltantes_error, umbral_patron_raro, umbral_patron_dominante
  )
  if (anyNA(umbrales) || any(umbrales < 0 | umbrales > 1)) {
    stop("Todos los umbrales deben estar entre 0 y 1.", call. = FALSE)
  }
  if (umbral_faltantes_error < umbral_faltantes_sospechoso) {
    stop("El umbral de error no puede ser menor que el sospechoso.", call. = FALSE)
  }
  if (!is.numeric(sentinelas_numericos) || anyNA(sentinelas_numericos) ||
      any(!is.finite(sentinelas_numericos))) {
    stop("`sentinelas_numericos` debe ser un vector num\u00e9rico finito.", call. = FALSE)
  }

  nombres <- names(datos)
  if (is.null(nombres)) {
    nombres <- paste0("V", seq_len(ncol(datos)))
  }
  nombres_lista <- make.unique(nombres)
  resultados <- lapply(seq_len(ncol(datos)), function(i) {
    .perfilar_columna(
      datos[[i]], nombres[[i]], muestra, max_patrones,
      distinguir_mayusculas, expandir, umbral_patron_raro,
      sentinelas_numericos
    )
  })

  columnas <- if (length(resultados)) {
    do.call(rbind, lapply(resultados, `[[`, "fila"))
  } else {
    .perfilar_columna(
      character(), "", muestra, max_patrones,
      distinguir_mayusculas, expandir, umbral_patron_raro,
      sentinelas_numericos
    )$fila[0, , drop = FALSE]
  }
  rownames(columnas) <- NULL
  patrones <- lapply(resultados, `[[`, "patrones")
  formatos_fecha <- lapply(resultados, `[[`, "formatos")
  names(patrones) <- nombres_lista
  names(formatos_fecha) <- nombres_lista

  n_filas_duplicadas <- tryCatch(
    sum(duplicated(datos)),
    error = function(e) NA_integer_
  )
  filas_completas <- tryCatch(
    sum(stats::complete.cases(datos)),
    error = function(e) NA_integer_
  )
  duplicadas <- .columnas_duplicadas(datos, nombres)
  tipos <- table(vapply(seq_along(datos), function(i) {
    .tipo_declarado(datos[[i]])
  }, character(1L)))
  tipos_columnas <- data.frame(
    tipo = names(tipos), n = as.integer(tipos), stringsAsFactors = FALSE
  )
  general <- list(
    filas = nrow(datos),
    columnas = ncol(datos),
    celdas = as.numeric(nrow(datos)) * as.numeric(ncol(datos)),
    memoria_bytes = as.numeric(utils::object.size(datos)),
    filas_completas = filas_completas,
    filas_duplicadas = n_filas_duplicadas,
    tipos_columnas = tipos_columnas,
    columnas_duplicadas = duplicadas
  )
  hallazgos <- .construir_hallazgos(
    datos, resultados, nombres, duplicadas,
    umbral_alta_cardinalidad, umbral_faltantes_sospechoso,
    umbral_faltantes_error, umbral_patron_raro,
    umbral_patron_dominante, columnas_sin_ceros,
    columnas_no_negativas,
    if (is.na(n_filas_duplicadas)) 0L else n_filas_duplicadas
  )
  meta <- list(
    nombre = nombre,
    fecha_hora = Sys.time(),
    version = .version_paquete(),
    filas_totales = nrow(datos),
    filas_analizadas = min(nrow(datos), floor(muestra)),
    muestreo = nrow(datos) > muestra
  )
  estructura <- list(
    general = general,
    columnas = columnas,
    patrones = patrones,
    formatos_fecha = formatos_fecha,
    hallazgos = hallazgos,
    meta = meta
  )
  class(estructura) <- "perfil"
  estructura
}
