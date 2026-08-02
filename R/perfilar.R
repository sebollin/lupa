#' Perfilar un conjunto de datos
#'
#' Examina un `data.frame`, `tibble` o `data.table` y devuelve estadísticas
#' generales, métricas por columna, patrones, formatos de fecha y hallazgos
#' accionables. Todas las proporciones se expresan en `[0, 1]`.
#'
#' Los umbrales de faltantes se aplican a la suma de ausentes reales y
#' faltantes disfrazados y son estrictos: la proporción debe superar el umbral
#' para generar el nivel correspondiente. La lista de cadenas está congelada con referencia a
#' `naniar::common_na_strings` 1.1.0 y suma extensiones habituales en datos
#' administrativos uruguayos. Las entradas que naniar expresa como patrones
#' escapados se adaptan a los signos literales de interrogación, asterisco y
#' punto porque aquí se comparan por igualdad. La lista no depende de la
#' versión instalada.
#' Los sentinelas numéricos predeterminados son `-9`, `-99`, `-999`, `-9999` y
#' `999`. La lista es deliberadamente más corta que
#' `naniar::common_na_numbers` 1.1.0: `66`, `77`, `88` y `9999` también pueden
#' ser edades, códigos o años legítimos. Para solicitar explícitamente esa
#' lista completa, use `sentinelas_numericos = sentinelas_naniar`.
#'
#' `muestra` limita sólo el descubrimiento de patrones, la inferencia de tipos y
#' la detección de formatos de fecha. Las demás métricas y hallazgos se calculan
#' sobre todas las filas. Por eso `meta$filas_analizadas` describe el máximo
#' usado por los análisis muestreados, no el alcance del perfil completo.
#'
#' Una columna cuyo año se expresa con dos dígitos se informa con su
#' `tipo_inferido` —`"fecha"` o `"fecha-hora"`— pero deja `minimo_fecha`,
#' `maximo_fecha`, `media_fecha` y `mediana_fecha` en `NA`. No es una omisión:
#' `23` puede ser 1923 o 2023, y elegir el siglo para calcular un rango sería
#' inventarlo. El hallazgo `anio_de_dos_digitos` señala esas columnas, y el
#' rango aparece una vez que el usuario resuelve la ambigüedad.
#'
#' @param datos Objeto que hereda de `data.frame`.
#' @param nombre Nombre descriptivo del objeto.
#' @param fecha Fecha y hora de la corrida. Se puede fijar para construir series
#'   reproducibles; se normaliza a UTC.
#' @param muestra Máximo de filas usadas para patrones e inferencia de tipos.
#'   Use `Inf` para analizar todas las filas.
#' @param max_patrones Máximo de patrones mostrados por columna.
#' @param distinguir_mayusculas Si se distinguen mayúsculas y minúsculas.
#' @param expandir Si se emite un token por carácter en los patrones.
#' @param umbral_alta_cardinalidad Umbral para columnas categóricas.
#' @param umbral_faltantes_sospechoso Umbral inferior de faltantes. El
#'   hallazgo se activa al superarlo en sentido estricto.
#' @param umbral_faltantes_error Umbral por encima del cual los faltantes son
#'   un error; la igualdad conserva la severidad sospechosa.
#' @param umbral_patron_raro Máxima frecuencia de un patrón raro.
#' @param umbral_patron_dominante Frecuencia mínima del patrón dominante.
#' @param columnas_sin_ceros Nombres de columnas donde cero no es admisible.
#' @param columnas_no_negativas Nombres de columnas que deben ser no negativas.
#' @param sentinelas_numericos Vector de sentinelas numéricos adicionales que
#'   representan ausencia. Se combina con la lista predeterminada.
#' @param analizar_dependencias Si se buscan dependencias funcionales entre
#'   pares de columnas. Se aplica una sola muestra común a toda la tabla.
#' @param umbral_dependencia Cumplimiento mínimo para informar una dependencia.
#' @param umbral_casi_clave_dependencia Tasa de valores distintos a partir de
#'   la cual un determinante se descarta como casi-clave antes de agrupar.
#' @param max_columnas_dependencias Máximo de columnas que intervienen en la
#'   búsqueda, cuyo costo crece cuadráticamente.
#'
#' @return Objeto S3 de clase `perfil`.
#' @export
#' @seealso [descubrir_patrones()], [detectar_dependencias()],
#'   [proponer_modelo()], [planificar_limpieza()]
#'
#' @examples
#' perfil <- perfilar(datos_administrativos)
#' perfil
#' summary(perfil)
perfilar <- function(datos,
                     nombre = deparse(substitute(datos)),
                     fecha = Sys.time(),
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
                     sentinelas_numericos = numeric(),
                     analizar_dependencias = TRUE,
                     umbral_dependencia = 0.995,
                     umbral_casi_clave_dependencia = 0.8,
                     max_columnas_dependencias = 100L) {
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe ser un data.frame, tibble o data.table.", call. = FALSE)
  }
  fecha_hora <- tryCatch(.fecha_utc(fecha), error = function(e) NA)
  if (length(fecha_hora) != 1L || is.na(fecha_hora) ||
      !is.finite(as.numeric(fecha_hora))) {
    stop("`fecha` debe contener una fecha y hora v\u00e1lida.", call. = FALSE)
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
  if (!is.logical(analizar_dependencias) || length(analizar_dependencias) != 1L ||
      is.na(analizar_dependencias)) {
    stop("`analizar_dependencias` debe ser un l\u00f3gico escalar sin NA.",
         call. = FALSE)
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
  dependencias <- if (analizar_dependencias) {
    detectar_dependencias(
      datos, umbral = umbral_dependencia, muestra = muestra,
      max_columnas = max_columnas_dependencias,
      umbral_casi_clave = umbral_casi_clave_dependencia
    )
  } else {
    detectar_dependencias(
      datos[0, 0, drop = FALSE], umbral = umbral_dependencia,
      max_columnas = 1L,
      umbral_casi_clave = umbral_casi_clave_dependencia
    )
  }

  n_filas_duplicadas <- tryCatch(
    sum(duplicated(datos)),
    error = function(e) NA_integer_
  )
  n_filas_en_grupos_duplicados <- tryCatch(
    sum(duplicated(datos) | duplicated(datos, fromLast = TRUE)),
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
    filas_en_grupos_duplicados = n_filas_en_grupos_duplicados,
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
    fecha_hora = fecha_hora,
    version = .version_paquete(),
    filas_totales = nrow(datos),
    filas_analizadas = min(nrow(datos), floor(muestra)),
    muestreo = nrow(datos) > muestra,
    muestra = muestra,
    max_patrones = max_patrones,
    distinguir_mayusculas = distinguir_mayusculas,
    expandir = expandir,
    umbral_patron_raro = umbral_patron_raro,
    analizar_dependencias = analizar_dependencias,
    umbral_dependencia = umbral_dependencia,
    umbral_casi_clave_dependencia = umbral_casi_clave_dependencia,
    max_columnas_dependencias = max_columnas_dependencias,
    sentinelas_numericos = .numeros_na(sentinelas_numericos)
  )
  estructura <- list(
    general = general,
    columnas = columnas,
    patrones = patrones,
    formatos_fecha = formatos_fecha,
    dependencias = dependencias,
    hallazgos = hallazgos,
    meta = meta
  )
  class(estructura) <- "perfil"
  estructura
}
