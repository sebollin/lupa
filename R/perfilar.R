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
#' ser edades, códigos o años legítimos. `sentinelas_numericos` representa la
#' política completa, no una lista que se agrega silenciosamente: use
#' `numeric()` para desactivar todos los sentinelas numéricos, o
#' `sentinelas_naniar` para solicitar explícitamente la lista de naniar.
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
#' Para números ordinarios, los estadísticos cuantitativos se calculan sólo con
#' valores finitos; `n_nan`, `n_infinito_positivo` y `n_infinito_negativo`
#' declaran lo excluido. En columnas `integer64` que exceden el entero máximo
#' representable exactamente por `double`, `minimo` y `maximo` quedan en `NA` y
#' los extremos exactos se conservan en `minimo_exacto` y `maximo_exacto`.
#' Una columna de listas intenta contar sus valores distintos; si la clase no
#' admite comparación, informa `NA` en lugar de afirmar cero.
#' Las columnas matriciales se conservan como una unidad por fila: `n` informa
#' las filas de la tabla, pero los estadísticos por valor quedan en `NA` y un
#' hallazgo explica que deben separarse en columnas con semántica explícita.
#' Los valores de texto que no forman UTF-8 válido tampoco se convierten: se
#' cuentan, se excluyen de los análisis textuales y generan un hallazgo con sus
#' posiciones.
#'
#' Los resúmenes de fecha-hora se expresan siempre en UTC y llevan el sufijo
#' `UTC` en el texto para hacer visible la zona aplicada. El instante se
#' conserva aunque la columna de entrada use otra zona horaria.
#'
#' La normalización Unicode se compara sin modificar el texto y requiere el
#' paquete opcional `stringi` sólo cuando existen caracteres no ASCII. La
#' clasificación de posibles datos personales es informativa: por defecto no
#' juzga su presencia y protege los valores concretos que el perfil publicaría.
#' La protección sustituye modas, ejemplos, evidencia y extremos o medianas que
#' corresponden a observaciones reales. Las medias y desvíos se conservan como
#' síntesis no ligadas a una fila; `proteccion_estadisticos` hace visible la
#' supresión. En fechas de nacimiento, un hallazgo separado conserva el
#' diagnóstico de valores anteriores a 1900 o posteriores a la corrida sin
#' publicar las fechas concretas.
#' Los números escritos como texto reconocen tanto coma como punto decimal y
#' sus separadores de miles simétricos. Los prefijos de tres letras separados
#' del número, con forma de código ISO 4217, y los símbolos monetarios se
#' conservan como evidencia; una columna sin datos suficientes para desambiguar
#' un separador de tres dígitos no se convierte automáticamente.
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
#' @param sentinelas_numericos Vector completo de valores numéricos que se
#'   interpretan como ausencia. `numeric()` los desactiva; las cadenas de
#'   ausencia se siguen evaluando por separado.
#' @param analizar_dependencias Si se buscan dependencias funcionales entre
#'   pares de columnas. Se aplica una sola muestra común a toda la tabla.
#' @param umbral_dependencia Cumplimiento mínimo para informar una dependencia.
#' @param umbral_casi_clave_dependencia Tasa de valores distintos a partir de
#'   la cual un determinante se descarta como casi-clave antes de agrupar.
#' @param max_columnas_dependencias Máximo de columnas que intervienen en la
#'   búsqueda, cuyo costo crece cuadráticamente.
#' @param datos_personales_permitidos Si la entrega admite datos personales.
#'   El valor predeterminado no juzga su presencia: la clasificación se informa
#'   con severidad `"ok"`. Use `FALSE` sólo cuando el contrato de la entrega
#'   declare que no deben existir.
#' @param proteger_datos_personales Si se reemplazan modas, ejemplos, evidencia
#'   y estadísticos de orden concretos de columnas clasificadas como posibles
#'   datos personales. Para conservarlos en el objeto debe desactivarse
#'   explícitamente; [reportar()] aplica además su propia protección
#'   predeterminada.
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
                     sentinelas_numericos = c(-9, -99, -999, -9999, 999),
                     analizar_dependencias = TRUE,
                     umbral_dependencia = 0.995,
                     umbral_casi_clave_dependencia = 0.8,
                     max_columnas_dependencias = 100L,
                     datos_personales_permitidos = TRUE,
                     proteger_datos_personales = TRUE) {
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
  for (argumento in c("datos_personales_permitidos", "proteger_datos_personales")) {
    valor <- get(argumento)
    if (!is.logical(valor) || length(valor) != 1L || is.na(valor)) {
      stop("`", argumento, "` debe ser TRUE o FALSE.", call. = FALSE)
    }
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
  datos_personales <- .detectar_datos_personales(datos, nombres, resultados)
  indice_personal <- match(columnas$columna, datos_personales$columna)
  columnas$dato_personal_posible <- !is.na(indice_personal)
  columnas$tipo_dato_personal <- datos_personales$tipo[indice_personal]
  columnas$proporcion_dato_personal <-
    datos_personales$proporcion_compatible[indice_personal]
  hallazgos_personales <- .hallazgos_datos_personales(
    datos_personales, datos_personales_permitidos
  )
  hallazgos_personales <- c(
    hallazgos_personales,
    .hallazgos_rango_nacimiento(columnas, datos_personales, fecha_hora)
  )
  if (length(hallazgos_personales)) {
    hallazgos <- do.call(rbind, c(list(hallazgos), hallazgos_personales))
    hallazgos$severidad <- factor(
      as.character(hallazgos$severidad),
      levels = c("ok", "sospechoso", "error"), ordered = TRUE
    )
    rownames(hallazgos) <- NULL
  }
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
    sentinelas_numericos = .numeros_na(sentinelas_numericos),
    datos_personales_permitidos = datos_personales_permitidos,
    proteger_datos_personales = proteger_datos_personales
  )
  estructura <- list(
    general = general,
    columnas = columnas,
    patrones = patrones,
    formatos_fecha = formatos_fecha,
    dependencias = dependencias,
    hallazgos = hallazgos,
    datos_personales = datos_personales,
    meta = meta
  )
  class(estructura) <- "perfil"
  if (proteger_datos_personales) estructura <- .proteger_perfil(estructura)
  estructura
}
