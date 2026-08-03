#' Datos administrativos sintéticos con problemas sembrados
#'
#' Conjunto enteramente sintético para demostrar el motor de profiling. Incluye
#' cédulas con formatos heterogéneos, fechas mezcladas, faltantes disfrazados,
#' una columna constante, valores extremos, una fila duplicada y registros con
#' el mismo identificador pero contenidos distintos. No representa personas ni
#' registros de ningún organismo.
#'
#' @format Un data frame con 13 filas y 10 variables:
#' \describe{
#'   \item{id_persona}{Identificador interno, con una repetición contradictoria.}
#'   \item{cedula}{Documento sintético con formatos correctos e incorrectos.}
#'   \item{fecha_nacimiento}{Fechas en varios formatos y un faltante disfrazado.}
#'   \item{sexo}{Categoría sintética con un faltante disfrazado.}
#'   \item{ingreso}{Importes con sentinelas numéricos y un valor extremo.}
#'   \item{departamento}{Categoría administrativa.}
#'   \item{pais}{Columna constante.}
#'   \item{correo}{Direcciones sintéticas y un patrón anómalo.}
#'   \item{id_copia}{Copia redundante del identificador interno.}
#'   \item{id_tramite}{Identificador sintético de alta unicidad.}
#' }
#' @source Generación sintética incluida con el paquete.
#' @seealso [perfilar()], [planificar_limpieza()]
#'
#' @examples
#' data(datos_administrativos)
#' perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
#' perfil$columnas[, c("columna", "tipo_inferido", "prop_faltantes_totales")]
"datos_administrativos"

#' Datos operativos sintéticos y neutrales
#'
#' Conjunto enteramente sintético, sin vocabulario ni convenciones de un país,
#' para demostrar el recorrido general de `lupa`. Siembra los mismos tipos de
#' problemas que el ejemplo administrativo: formatos heterogéneos, fechas
#' mezcladas, ausentes disfrazados, espacios y mayúsculas inconsistentes,
#' valores extremos, una columna constante, una columna duplicada, una fila
#' duplicada y un identificador repetido con datos contradictorios. No representa
#' personas, operaciones ni registros reales.
#'
#' @format Un data frame con 13 filas y 10 variables:
#' \describe{
#'   \item{id_registro}{Identificador interno, con una repetición contradictoria.}
#'   \item{codigo_usuario}{Código sintético con formatos heterogéneos y un ausente disfrazado.}
#'   \item{fecha_evento}{Fechas en varios formatos y un ausente disfrazado.}
#'   \item{canal}{Categoría con variantes de capitalización, espacios y ausentes disfrazados.}
#'   \item{monto}{Importes con sentinelas, cero, un negativo y un valor extremo.}
#'   \item{zona}{Categoría operativa ficticia.}
#'   \item{sistema}{Columna constante.}
#'   \item{contacto}{Direcciones sintéticas y un patrón anómalo.}
#'   \item{id_copia}{Copia redundante del identificador interno.}
#'   \item{id_evento}{Identificador sintético de alta unicidad.}
#' }
#' @source Generación sintética incluida en `data-raw/datos_operativos.R`.
#' @seealso [datos_administrativos], [analizar()], [perfilar()]
#'
#' @examples
#' data(datos_operativos)
#' analisis <- analizar(datos_operativos, analizar_dependencias = FALSE)
#' analisis$perfil$hallazgos[, c("columna", "tipo_hallazgo", "severidad")]
"datos_operativos"
