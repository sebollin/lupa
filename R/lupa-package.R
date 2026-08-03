#' lupa: examinar, medir y mejorar la calidad de datos
#'
#' `lupa` implementa un modelo de calidad de datos de uso general: dimensiones
#' y factores declarables, métricas con granularidad explícita, agregación
#' tipada y una cadena de evaluación auditable. El paquete nunca modifica datos
#' como efecto del diagnóstico: cada etapa devuelve objetos inspeccionables.
#' [marco_agesic()] y [catalogo_agesic()] aportan de fábrica la implementación
#' trazable del marco uruguayo, sin restringir las taxonomías del usuario.
#'
#' El punto de entrada es [analizar()]. En una llamada reúne el diagnóstico
#' descriptivo y su cobertura, sin medir requisitos observados automáticamente.
#' Sus componentes también se pueden construir por separado. El recorrido es:
#'
#' 1. examinar estructura, tipos, patrones, ausencias, distribuciones,
#'    asociaciones y comportamiento temporal; [cobertura_analisis()] explicita
#'    qué factores no fueron evaluados;
#' 2. convertir el diagnóstico en una propuesta editable con
#'    [proponer_modelo()];
#' 3. declarar y ejecutar métricas mediante [modelo()] y [medir()];
#' 4. evaluar reglas y perfiles de madurez con [evaluar()];
#' 5. planificar y aplicar mejoras auditables mediante
#'    [planificar_limpieza()] y [aplicar()];
#' 6. acumular corridas y detectar deriva con [historico_calidad()],
#'    [detectar_deriva_calidad()] y [comparar_perfiles()];
#' 7. persistir el recorrido con [guardar_analisis()] y producir un archivo HTML
#'    autocontenido con [reportar()].
#'
#' Las taxonomías se declaran con [marco_calidad()]. Los padrones externos se
#' declaran con [referencial()], y los contratos que
#' no se pueden inferir se expresan con [vigencia()] y [escala()]. La correspondencia
#' exacta con las 49 entradas de AGESIC se consulta en [catalogo_agesic()]. No
#' se calcula un índice global: la jerarquía dimensión–factor–métrica es
#' taxonómica y requiere un contrato adicional para producirlo.
#'
#' @references Batini C, Scannapieco M (2016). *Data and Information Quality:
#'   Dimensions, Principles and Techniques*. Springer.
#'
#'   AGESIC (2020). *Marco de trabajo para la Gestión de la Calidad
#'   de Datos en Gobierno Digital*, versión 1.6, Presidencia de la República,
#'   Uruguay.
#'
#' @seealso [datos_administrativos]
#' @keywords internal
#'
#' @examples
#' resultado <- analizar(datos_administrativos, analizar_dependencias = FALSE)
#' subset(resultado$perfil$hallazgos, severidad != "ok")
"_PACKAGE"
