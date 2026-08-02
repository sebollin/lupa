#' lupa: examinar, medir y mejorar la calidad de datos
#'
#' `lupa` implementa un recorrido auditable para datos administrativos, basado
#' en el Marco de trabajo para la Gestión de la Calidad de Datos en Gobierno
#' Digital de AGESIC. El paquete nunca modifica datos como efecto del
#' diagnóstico: cada etapa devuelve objetos de datos inspeccionables.
#'
#' El punto de entrada es [perfilar()]. Desde allí se puede:
#'
#' 1. examinar estructura, tipos, patrones, ausencias y dependencias;
#'    [cobertura_analisis()] explicita qué factores no fueron evaluados;
#' 2. convertir el diagnóstico en una propuesta editable con
#'    [proponer_modelo()];
#' 3. declarar y ejecutar métricas mediante [modelo()] y [medir()];
#' 4. evaluar reglas y perfiles de madurez con [evaluar()];
#' 5. planificar y aplicar mejoras auditables mediante
#'    [planificar_limpieza()] y [aplicar()];
#' 6. acumular corridas y detectar deriva con [historico_calidad()],
#'    [detectar_deriva_calidad()] y [comparar_perfiles()];
#' 7. producir un archivo HTML autocontenido con [reportar()].
#'
#' Los padrones externos se declaran con [referencial()], y los contratos que
#' no se pueden inferir se expresan con [vigencia()] y [escala()]. La correspondencia
#' exacta con las 49 entradas del marco se consulta en [catalogo_agesic()]. No
#' se calcula un índice global: la jerarquía dimensión–factor–métrica es
#' taxonómica y el marco no define esa agregación.
#'
#' @references AGESIC (2020). *Marco de trabajo para la Gestión de la Calidad
#'   de Datos en Gobierno Digital*, versión 1.6, Presidencia de la República,
#'   Uruguay.
#'
#' @seealso [datos_administrativos]
#' @keywords internal
#'
#' @examples
#' perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
#' subset(perfil$hallazgos, severidad != "ok")
"_PACKAGE"
