.resolver_factor <- function(dimension, factor) {
  clave <- .clave_par_identificador(dimension, factor)
  claves_especificas <- .clave_par_identificador(
    c(
      "Exactitud", "Exactitud", "Exactitud", "Consistencia", "Consistencia",
      "Consistencia", "Completitud", "Completitud", "Unicidad", "Frescura",
      "Frescura"
    ),
    c(
      "Correctitud sem\u00e1ntica", "Correctitud sint\u00e1ctica", "Precisi\u00f3n",
      "Integridad inter-entidad", "Integridad intra-entidad",
      "Integridad de dominio", "Cobertura", "Densidad", "No-duplicaci\u00f3n",
      "Actualidad", "Oportunidad"
    )
  )
  indice <- match(clave, claves_especificas)
  switch(
    as.character(indice),
    "1" =
      "Crear referencial() e instanciar metricas_referencial().",
    "2" =
      "Especializar Formato con expresi\u00f3n, diccionario o validador.",
    "3" =
      "Declarar escala() o medir ErrorEstandar sobre el atributo.",
    "4" =
      "Instanciar ReglaIntegridadInterEntidad con claves confirmadas.",
    "5" =
      "Confirmar una regla y especializar ReglaIntegridadIntraEntidad.",
    "6" =
      "Proveer un dominio a ValoresPosiblesPorExtension o Comprension.",
    "7" =
      "Crear un referencial(completo = TRUE) y medir RatioCobertura.",
    "8" = "Usar NoNulo o DensidadPonderada.",
    "9" =
      "Usar las m\u00e9tricas de duplicaci\u00f3n o el perfil autom\u00e1tico.",
    "10" =
      "Declarar vigencia() y medir DesactualizacionPorFecha o PorCambios.",
    "11" =
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

  claves <- .clave_par_identificador(factores$dimension, factores$factor)
  solo_fuera <- factores$disponibilidad == "fuera_de_alcance"
  factores$estado[solo_fuera] <- "fuera_de_alcance"
  factores$motivo[solo_fuera] <-
    "Las m\u00e9tricas del factor requieren capacidades no implementadas en esta versi\u00f3n."

  medidas_perfil <- factores$perfil_mide
  factores$estado[medidas_perfil] <- "medida"
  factores$motivo[medidas_perfil] <-
    "El profiling autom\u00e1tico examina evidencia de este factor."
  factores$motivo[.identificadores_en(claves, "Completitud|Densidad")] <-
    "El perfil cont\u00f3 ausentes reales y disfrazados en todas las columnas."
  factores$motivo[.identificadores_en(claves, "Unicidad|No-duplicaci\u00f3n")] <-
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

  # ANTES del bloque de la medicion, no despues: puesta despues, esta guarda
  # pisaba un factor que una medicion REAL habia marcado como medido y
  # publicaba 'no hubo nada que examinar' sobre dos filas medidas. Lo que
  # el perfil no pudo examinar no lo midio el perfil; si la corrida trae una
  # metrica para ese factor, eso manda.
  # `medida` se asignaba por el mapa de capacidades del perfil, sin mirar si
  # hubo observaciones: un perfil de CERO filas informaba "Completitud /
  # Densidad: medida -- el perfil conto ausentes reales en todas las columnas",
  # y no habia contado nada. El tablero corregia su propia vista, pero la
  # cobertura que acompana al analisis seguia afirmando que se midio. Lo no
  # medido tiene que aparecer como no medido en la capa donde se publica.
  filas <- suppressWarnings(as.numeric(perfil$meta$filas_totales))
  if (length(filas) == 1L && !is.na(filas) && filas == 0) {
    sin_observaciones <- factores$estado == "medida"
    factores$estado[sin_observaciones] <- "no_aplica"
    factores$motivo[sin_observaciones] <-
      "La tabla no tiene filas: no hubo nada que examinar para este factor."
  }

  if (!is.null(medicion)) {
    medidos <- .identificadores_unicos(.clave_par_identificador(
      medicion$dimension, medicion$factor
    ))
    indices <- .identificadores_en(claves, medidos)
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
