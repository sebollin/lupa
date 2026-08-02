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
