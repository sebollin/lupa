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
  any(grepl("^(sfc|sfg|sf$)", perfil$columnas$tipo_declarado, perl = TRUE))
}

.perfil_tiene_tiempo <- function(perfil) {
  tipos <- c("fecha", "fecha-hora")
  any(perfil$columnas$tipo_declarado %in% tipos |
        perfil$columnas$tipo_inferido %in% tipos)
}

#' Informar la cobertura conceptual de un análisis
#'
#' Devuelve una fila por dimensión y factor del catálogo de AGESIC. Distingue
#' lo efectivamente medido de lo que no fue declarado, lo que no aplica a los
#' tipos presentes y lo que queda fuera del alcance actual. La tabla evita que
#' la ausencia de un hallazgo se interprete como evidencia de calidad.
#'
#' El profiling automático mide densidad y no duplicación. Los demás factores
#' sólo pasan a `"medida"` cuando `medicion` contiene una métrica de ese factor;
#' descubrir un patrón o una dependencia no los convierte por sí solo en un
#' requisito confirmado.
#'
#' @param perfil Objeto creado por [perfilar()].
#' @param medicion Objeto opcional creado por [medir()].
#'
#' @return Data frame con `dimension`, `factor`, `estado`, `motivo` y
#'   `como_resolverlo`. `estado` es un factor con niveles `"medida"`,
#'   `"no_declarada"`, `"no_aplica"` y `"fuera_de_alcance"`.
#' @export
#' @seealso [perfilar()], [medir()], [vigencia()], [escala()], [reportar()]
#'
#' @examples
#' perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
#' cobertura_analisis(perfil)
cobertura_analisis <- function(perfil, medicion = NULL) {
  if (!inherits(perfil, "perfil")) {
    stop("`perfil` debe ser un objeto creado por perfilar().", call. = FALSE)
  }
  if (!is.null(medicion) && !inherits(medicion, "medicion")) {
    stop("`medicion` debe ser NULL o un objeto creado por medir().", call. = FALSE)
  }
  catalogo <- catalogo_agesic()
  factores <- unique(catalogo[c("dimension", "factor")])
  factores$estado <- "no_declarada"
  factores$motivo <- "El perfil describe evidencia, pero no recibi\u00f3 un requisito para este factor."
  factores$como_resolverlo <- mapply(
    .resolver_factor, factores$dimension, factores$factor,
    USE.NAMES = FALSE
  )

  claves <- paste(factores$dimension, factores$factor, sep = "|")
  estados_catalogo <- split(as.character(catalogo$estado),
                             paste(catalogo$dimension, catalogo$factor, sep = "|"))
  solo_fuera <- vapply(claves, function(clave) {
    estados <- unique(estados_catalogo[[clave]])
    length(estados) && all(estados == "fuera_de_alcance")
  }, logical(1L))
  factores$estado[solo_fuera] <- "fuera_de_alcance"
  factores$motivo[solo_fuera] <-
    "Las m\u00e9tricas del factor requieren capacidades no implementadas en esta versi\u00f3n."

  medidas_perfil <- claves %in% c(
    "Completitud|Densidad", "Unicidad|No-duplicaci\u00f3n"
  )
  factores$estado[medidas_perfil] <- "medida"
  factores$motivo[claves == "Completitud|Densidad"] <-
    "El perfil cont\u00f3 ausentes reales y disfrazados en todas las columnas."
  factores$motivo[claves == "Unicidad|No-duplicaci\u00f3n"] <-
    "El perfil examin\u00f3 duplicaci\u00f3n de valores, columnas y filas exactas."

  sin_tiempo <- !.perfil_tiene_tiempo(perfil)
  if (sin_tiempo) {
    indices <- factores$dimension == "Frescura"
    factores$estado[indices] <- "no_aplica"
    factores$motivo[indices] <-
      "No hay columnas declaradas o inferidas como fecha o fecha-hora."
  }
  sin_geometria <- !.perfil_tiene_geometria(perfil)
  if (sin_geometria) {
    indices <- factores$factor %in% c(
      "Exactitud posicional absoluta", "Exactitud posicional relativa",
      "Consistencia topol\u00f3gica", "Comisi\u00f3n"
    )
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
  rownames(factores) <- NULL
  factores
}
