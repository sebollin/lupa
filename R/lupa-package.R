#' lupa: perfilar y medir la calidad de datos con evidencia
#'
#' `lupa` examina una tabla o una base de datos antes de confiar en ella:
#' encuentra faltantes disfrazados, el mismo dato escrito de varias formas,
#' fechas en formatos mezclados, filas y columnas repetidas y estructura que
#' nadie declaró; dice en qué filas está cada problema y qué controles no pudo
#' hacer. Cada resultado conserva su alcance y su evidencia, y ningún
#' diagnóstico modifica los datos que recibe.
#'
#' La puerta de entrada es [perfilar()]. Sus hallazgos ([hallazgos()]) traen
#' tipo, severidad (`ok < sospechoso < error`), evidencia y trazabilidad hasta
#' las filas; su cobertura ([cobertura()]) enumera los diagnósticos que no se
#' pudieron evaluar, con su motivo, y que nunca cuentan como cero. La viñeta
#' "Empezar en diez minutos" (`vignette("lupa")`) recorre el primer uso, y
#' "Leer un perfil" (`vignette("leer-un-perfil")`) explica cada parte.
#'
#' Desde el perfil, el recorrido sigue estas etapas, cada una con su guía:
#'
#' 1. explorar columnas y reunir el diagnóstico en un objeto con [analizar()],
#'    y compartirlo en un HTML autocontenido con [reportar()];
#' 2. perfilar dentro de una base con [perfilar_dbi()], o una colección de
#'    tablas con [perfilar_coleccion()];
#' 3. limpiar una copia con un plan editable: [planificar_limpieza()] y
#'    [aplicar()];
#' 4. convertir lo confirmado en mediciones explícitas y evaluarlas:
#'    [proponer_modelo()], [modelo()], [medir()] y [evaluar()];
#' 5. comparar entregas y seguir la calidad en el tiempo con
#'    [comparar_perfiles()] y [historico_calidad()].
#'
#' Un marco de calidad es una taxonomía que se declara con [marco_calidad()].
#' El paquete incluye tres, [marco_agesic()], [marco_cepal()] y
#' [marco_iso25012()], y ninguno restringe las declaraciones de quien lo usa:
#' un factor se mide cuando los datos aportan evidencia o cuando se declaran el
#' requisito y la métrica. [cobertura_analisis()] informa qué factores quedaron
#' medidos. No hay un puntaje de fábrica: [indice_calidad()] calcula un índice
#' sólo con los pesos que declara el proyecto.
#'
#' @references Batini C, Scannapieco M (2016) \doi{10.1007/978-3-319-24106-7}.
#'   *Data and Information Quality: Dimensions, Principles and Techniques*.
#'   Springer.
#'
#'   [AGESIC (2020)](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/).
#'   *Marco de trabajo para la Gestión de la Calidad de Datos en Gobierno
#'   Digital*, versión 1.6.
#'
#'   [CEA/CEPAL (2022)](https://repositorio.cepal.org/handle/11362/47464).
#'   *Guía para la implementación del marco de aseguramiento de la calidad para
#'   procesos y productos estadísticos*. LC/CEA.11/19. Santiago.
#'
#'   [ISO/IEC (2008)](https://www.iso.org/standard/35736.html).
#'   *ISO/IEC 25012:2008 Software engineering — Software product
#'   Quality Requirements and Evaluation (SQuaRE) — Data quality model*.
#'
#' @seealso [datos_operativos], [datos_administrativos]
#' @keywords internal
#'
#' @examples
#' perfil <- perfilar(datos_operativos)
#' subset(hallazgos(perfil), severidad == "error",
#'        c(columna, tipo_hallazgo, evidencia))
#' cobertura(perfil)[, c("diagnostico", "columna")]
"_PACKAGE"
