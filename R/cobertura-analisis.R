.resolver_factor <- function(dimension, factor) {
  clave <- paste(dimension, factor, sep = "|")
  switch(
    clave,
    "Exactitud|Correctitud sem\u00e1ntica" =
      "Crear referencial() e instanciar metricas_referencial().",
    "Exactitud|Correctitud sint\u00e1ctica" =
      "Especializar Formato con expresi\u00f3n, diccionario o validador.",
    "Exactitud|Precisi\u00f3n" =
      "Declarar escala() o medir ErrorEstandar sobre el atributo.",
    "Consistencia|Integridad inter-entidad" =
      "Instanciar ReglaIntegridadInterEntidad con claves confirmadas.",
    "Consistencia|Integridad intra-entidad" =
      "Confirmar una regla y especializar ReglaIntegridadIntraEntidad.",
    "Consistencia|Integridad de dominio" =
      "Proveer un dominio a ValoresPosiblesPorExtension o Comprension.",
    "Completitud|Cobertura" =
      "Crear un referencial(completo = TRUE) y medir RatioCobertura.",
    "Completitud|Densidad" = "Usar NoNulo o DensidadPonderada.",
    "Unicidad|No-duplicaci\u00f3n" =
      "Usar las m\u00e9tricas de duplicaci\u00f3n o el perfil autom\u00e1tico.",
    "Frescura|Actualidad" =
      "Declarar vigencia() y medir DesactualizacionPorFecha o PorCambios.",
    "Frescura|Oportunidad" =
      "Declarar vigencia() y medir una m\u00e9trica Oportunidad*.",
    "Requiere un backend o referencial especializado que no integra esta versi\u00f3n."
  )
}

.perfil_tiene_geometria <- function(perfil) {
  # El tipo declarado no alcanza: una columna WKT declara "texto" y una de WKB
  # declara "lista", y aun asi el perfil ya trae su CRS, su tipo y su bbox.
  # Decidir por la etiqueta hacia que la cobertura afirmara que la geometria no
  # aplica sobre datos que si son geometricos, que es exactamente lo que esta
  # tabla existe para evitar.
  if (any(grepl("^(sfc|sfg|sf$)", perfil$columnas$tipo_declarado, perl = TRUE))) {
    return(TRUE)
  }
  if (!is.null(perfil$columnas$tipo_geometria) &&
      any(!is.na(perfil$columnas$tipo_geometria))) {
    return(TRUE)
  }
  # Y tambien cuando se reconocio la columna como geometrica pero no se pudo
  # convertir: ahi la geometria aplica y lo que falta es la medicion.
  !is.null(perfil$columnas$representacion_geometria) &&
    any(!is.na(perfil$columnas$representacion_geometria))
}

.perfil_tiene_tiempo <- function(perfil) {
  tipos <- c("fecha", "fecha-hora")
  any(perfil$columnas$tipo_declarado %in% tipos |
        perfil$columnas$tipo_inferido %in% tipos)
}

#' Informar la cobertura conceptual de un análisis
#'
#' Devuelve una fila por dimensión y factor del [marco_calidad()] elegido.
#' Usa [marco_agesic()] por omisión, pero acepta cualquier taxonomía declarada.
#' Distingue
#' lo efectivamente medido de lo que no fue declarado, lo que no aplica a los
#' tipos presentes y lo que queda fuera del alcance actual. La tabla evita que
#' la ausencia de un hallazgo se interprete como evidencia de calidad.
#'
#' En el marco incluido, el profiling automático mide densidad y no
#' duplicación. Un marco propio puede marcar otros factores mediante la columna
#' `perfil_mide`. Los demás sólo pasan a `"medida"` cuando `medicion` contiene
#' una métrica del factor; descubrir un patrón o una dependencia no los
#' convierte por sí solo en un requisito confirmado.
#'
#' @param perfil **Primer argumento.** Objeto creado por [perfilar()]; es el
#'   perfil descriptivo sobre cuyos factores se informa cobertura.
#' @param medicion **Segundo argumento, opcional.** Objeto creado por
#'   [medir()], con métricas ejecutadas que pueden completar esa cobertura.
#'   No es un perfil creado por [perfilar()].
#' @param modelo **Tercer argumento.** Objeto creado por [marco_calidad()] que
#'   actúa como referencia conceptual. El nombre enfatiza que no es el modelo
#'   operativo creado por [modelo()] que recibe [medir()].
#'
#' @return Data frame con `marco`, `dimension`, `factor`, `estado`, `motivo` y
#'   `como_resolverlo`. `marco` identifica explícitamente la taxonomía contra la
#'   que se calculó la tabla. `estado` es un factor con niveles `"medida"`,
#'   `"no_declarada"`, `"no_aplica"` y `"fuera_de_alcance"`.
#' @export
#' @seealso [marco_calidad()], [perfilar()], [medir()], [vigencia()], [escala()],
#'   [reportar()]
#'
#' @examples
#' perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
#' cobertura_analisis(perfil)
cobertura_analisis <- function(perfil, medicion = NULL,
                               modelo = marco_agesic()) {
  if (!inherits(perfil, "perfil")) {
    stop(
      "El primer argumento `perfil` debe ser un objeto creado por perfilar(); ",
      "no es un perfil de evaluacion.", call. = FALSE
    )
  }
  if (!is.null(medicion) && !inherits(medicion, "medicion")) {
    stop(
      "El segundo argumento `medicion` debe ser NULL o un objeto creado por ",
      "medir().", call. = FALSE
    )
  }
  if (!inherits(modelo, "marco_calidad")) {
    stop(
      "El tercer argumento `modelo` debe provenir de marco_calidad(); ",
      "no es el modelo operativo creado por modelo().", call. = FALSE
    )
  }
  factores <- modelo$factores
  factores$estado <- "no_declarada"
  factores$motivo <- "El perfil describe evidencia, pero no recibi\u00f3 un requisito para este factor."

  claves <- paste(factores$dimension, factores$factor, sep = "|")
  solo_fuera <- factores$disponibilidad == "fuera_de_alcance"
  factores$estado[solo_fuera] <- "fuera_de_alcance"
  factores$motivo[solo_fuera] <-
    "Las m\u00e9tricas del factor requieren capacidades no implementadas en esta versi\u00f3n."

  medidas_perfil <- factores$perfil_mide
  factores$estado[medidas_perfil] <- "medida"
  factores$motivo[medidas_perfil] <-
    "El profiling autom\u00e1tico examina evidencia de este factor."
  factores$motivo[claves == "Completitud|Densidad"] <-
    "El perfil cont\u00f3 ausentes reales y disfrazados en todas las columnas."
  factores$motivo[claves == "Unicidad|No-duplicaci\u00f3n"] <-
    "El perfil examin\u00f3 duplicaci\u00f3n de valores, columnas y filas exactas."

  if (!.perfil_tiene_tiempo(perfil)) {
    indices <- factores$aplicabilidad == "temporal"
    factores$estado[indices] <- "no_aplica"
    factores$motivo[indices] <-
      "No hay columnas declaradas o inferidas como fecha o fecha-hora."
  }
  if (!.perfil_tiene_geometria(perfil)) {
    indices <- factores$aplicabilidad == "geometria"
    factores$estado[indices] <- "no_aplica"
    factores$motivo[indices] <- "No se identificaron columnas de geometr\u00eda."
  }

  if (!is.null(medicion)) {
    medidos <- unique(paste(medicion$dimension, medicion$factor, sep = "|"))
    indices <- claves %in% medidos
    factores$estado[indices] <- "medida"
    factores$motivo[indices] <-
      "La corrida contiene al menos una m\u00e9trica instanciada para este factor."
  }
  factores$estado <- factor(
    factores$estado,
    levels = c("medida", "no_declarada", "no_aplica", "fuera_de_alcance")
  )
  factores$marco <- modelo$nombre
  factores <- factores[c(
    "marco", "dimension", "factor", "estado", "motivo", "como_resolverlo"
  )]
  rownames(factores) <- NULL
  factores
}
