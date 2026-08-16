.normalizar_factores_marco <- function(factores) {
  if (is.list(factores) && !inherits(factores, "data.frame")) {
    if (!length(factores) || is.null(names(factores)) ||
        anyNA(names(factores)) || any(!nzchar(names(factores))) ||
        anyDuplicated(names(factores))) {
      stop(
        "Una lista de factores debe tener dimensiones con nombres \u00fanicos.",
        call. = FALSE
      )
    }
    filas <- lapply(seq_along(factores), function(i) {
      data.frame(
        dimension = names(factores)[[i]], factor = factores[[i]],
        stringsAsFactors = FALSE
      )
    })
    factores <- do.call(rbind, filas)
  }
  if (!inherits(factores, "data.frame") || !nrow(factores) ||
      !all(c("dimension", "factor") %in% names(factores))) {
    stop(
      "`factores` debe ser un data frame con `dimension` y `factor`, o una lista con nombres.",
      call. = FALSE
    )
  }
  factores <- as.data.frame(factores, stringsAsFactors = FALSE)
  factores[] <- lapply(factores, function(x) {
    if (is.factor(x)) as.character(x) else x
  })
  factores$dimension <- as.character(factores$dimension)
  factores$factor <- as.character(factores$factor)
  if (anyNA(factores$dimension) || anyNA(factores$factor) ||
      any(!nzchar(factores$dimension)) || any(!nzchar(factores$factor))) {
    stop("Las dimensiones y los factores no pueden ser ausentes ni vac\u00edos.",
         call. = FALSE)
  }
  clave <- paste(factores$dimension, factores$factor, sep = "|")
  if (anyDuplicated(clave)) {
    stop("Cada par dimensi\u00f3n-factor debe ser \u00fanico.", call. = FALSE)
  }

  predeterminados <- list(
    como_resolverlo = "Declarar e instanciar una m\u00e9trica de este factor.",
    perfil_mide = FALSE,
    aplicabilidad = "siempre",
    disponibilidad = "disponible"
  )
  for (nombre in names(predeterminados)) {
    if (!nombre %in% names(factores)) {
      factores[[nombre]] <- predeterminados[[nombre]]
    }
  }
  factores$como_resolverlo <- as.character(factores$como_resolverlo)
  factores$perfil_mide <- as.logical(factores$perfil_mide)
  factores$aplicabilidad <- as.character(factores$aplicabilidad)
  factores$disponibilidad <- as.character(factores$disponibilidad)
  if (anyNA(factores$como_resolverlo) || any(!nzchar(factores$como_resolverlo))) {
    stop("`como_resolverlo` debe contener texto no vac\u00edo.", call. = FALSE)
  }
  if (anyNA(factores$perfil_mide)) {
    stop("`perfil_mide` debe contener valores l\u00f3gicos sin NA.", call. = FALSE)
  }
  if (anyNA(factores$aplicabilidad) ||
      any(!factores$aplicabilidad %in% c("siempre", "temporal", "geometria"))) {
    stop(
      "`aplicabilidad` debe usar 'siempre', 'temporal' o 'geometria'.",
      call. = FALSE
    )
  }
  if (anyNA(factores$disponibilidad) ||
      any(!factores$disponibilidad %in% c("disponible", "fuera_de_alcance"))) {
    stop(
      "`disponibilidad` debe usar 'disponible' o 'fuera_de_alcance'.",
      call. = FALSE
    )
  }
  if (any(factores$perfil_mide &
          factores$disponibilidad == "fuera_de_alcance")) {
    stop(
      "Un factor fuera de alcance no puede declarar `perfil_mide = TRUE`.",
      call. = FALSE
    )
  }
  rownames(factores) <- NULL
  factores
}

#' Declarar una taxonomía de calidad de datos
#'
#' Un `marco_calidad` enumera las dimensiones y factores contra los que se
#' interpreta la cobertura de un análisis. No contiene métricas instanciadas:
#' esa función sigue correspondiendo a [modelo()]. Tampoco es un catálogo de
#' métricas; [catalogo_agesic()] conserva esa correspondencia específica.
#'
#' `marco_agesic()` devuelve la taxonomía incluida de fábrica. Un marco propio
#' puede construirse con un data frame o con una lista cuyos nombres son
#' dimensiones y cuyos valores son factores. La forma lista aplica a todas las
#' filas los valores predeterminados descritos abajo; en particular,
#' `perfil_mide = FALSE`.
#'
#' `marco_iso25012()` adapta las dos perspectivas de ISO/IEC 25012:2008 a la
#' estructura dimensión-factor. Usa tres grupos disjuntos como dimensiones:
#' características inherentes, características inherentes y dependientes del
#' sistema, y características dependientes del sistema. Este agrupamiento es
#' una representación operativa para `lupa`, no afirma que la norma defina una
#' jerarquía dimensión-factor. Los nombres de las quince características y su
#' clasificación siguen la norma; las descripciones son redacción propia. Las
#' quince filas declaran `perfil_mide = FALSE`: a diferencia de la política
#' incluida para dos factores de AGESIC, el profiling genérico no demuestra por
#' sí solo que una característica ISO satisfaga el uso declarado.
#'
#' `marco_cepal()` representa los cuatro niveles y los diecinueve principios
#' del marco nacional de aseguramiento de la calidad de las Naciones Unidas,
#' adoptados y adaptados para América Latina y el Caribe por la CEA/CEPAL. La
#' columna `principio` conserva la numeración de la fuente. Los nombres de los
#' niveles y principios son textuales; las descripciones y las instrucciones
#' `como_resolverlo` de este paquete son redacción propia. Los principios 1 a
#' 13 quedan fuera del alcance de una tabla: se refieren al sistema, al entorno
#' institucional o al proceso estadístico. Los principios 14 a 19 describen
#' productos estadísticos y quedan disponibles, aunque ninguno se considera
#' medido por el profiling genérico.
#'
#' @param nombre Nombre del marco.
#' @param factores Data frame con `dimension` y `factor`, o lista con nombres.
#'   El data frame puede añadir estos campos de contrato:
#'   * `como_resolverlo`: instrucción que muestra [cobertura_analisis()] cuando
#'     el factor todavía no fue medido;
#'   * `perfil_mide`: lógico que declara si el profiling por sí solo aporta una
#'     medición suficiente del factor. No ejecuta métricas ni se infiere del
#'     nombre; su valor predeterminado es `FALSE`;
#'   * `aplicabilidad`: `"siempre"`, `"temporal"` o `"geometria"`. Las dos
#'     últimas permiten informar `"no_aplica"` cuando el perfil no contiene
#'     columnas temporales o geometrías, respectivamente;
#'   * `disponibilidad`: `"disponible"` o `"fuera_de_alcance"`. Esta última
#'     declara una limitación del motor y no puede combinarse con
#'     `perfil_mide = TRUE`.
#'
#'   Las columnas adicionales se conservan como metadatos y no cambian por sí
#'   solas la cobertura. Si se usa una lista, `como_resolverlo` recibe una
#'   instrucción genérica, `perfil_mide = FALSE`, `aplicabilidad = "siempre"` y
#'   `disponibilidad = "disponible"` para todas las filas.
#'
#' @return `marco_calidad()`, `marco_agesic()`, `marco_iso25012()` y
#'   `marco_cepal()` devuelven un objeto S3 `marco_calidad`.
#'   `as.data.frame()` devuelve su tabla de factores.
#' @export
#' @seealso [catalogo_agesic()], [modelo()], [cobertura_analisis()]
#' @references [ISO/IEC (2008)](https://www.iso.org/standard/35736.html).
#'   *ISO/IEC 25012:2008 Software engineering —
#'   Software product Quality Requirements and Evaluation (SQuaRE) — Data
#'   quality model*. <https://www.iso.org/standard/35736.html>.
#'
#'   [Naciones Unidas (2019)](https://unstats.un.org/unsd/methodology/dataquality/).
#'   *Manual del marco nacional de aseguramiento de calidad en las estadísticas
#'   oficiales*. Estudios en Métodos, serie M, N° 100
#'   (ST/ESA/STAT/SER.M/100), Nueva York.
#'
#'   [Grupo de Trabajo de la Conferencia Estadística de las Américas (CEA),
#'   coordinado por Colombia (DANE) y México (INEGI), Secretaría Técnica:
#'   División de Estadísticas de la CEPAL (2022)](https://repositorio.cepal.org/handle/11362/47464).
#'   *Guía para la implementación del marco de aseguramiento de la calidad para
#'   procesos y productos estadísticos*. LC/CEA.11/19. Comisión Económica para
#'   América Latina y el Caribe (CEPAL), Naciones Unidas, Santiago.
#'
#' @examples
#' propio <- marco_calidad("Marco operativo", list(
#'   Trazabilidad = c("Origen documentado", "Linaje reproducible"),
#'   Pertinencia = "Adecuación al uso"
#' ))
#' propio
#' as.data.frame(propio)
#' marco_agesic()
#' iso <- marco_iso25012()
#' table(as.data.frame(iso)$dimension)
#' marco_cepal()
marco_calidad <- function(nombre, factores) {
  if (!.es_texto_escalar(nombre)) {
    stop("`nombre` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  estructura <- list(
    nombre = nombre,
    factores = .normalizar_factores_marco(factores),
    origen = "usuario"
  )
  class(estructura) <- "marco_calidad"
  estructura
}

#' @rdname marco_calidad
#' @export
marco_agesic <- function() {
  catalogo <- catalogo_agesic()
  factores <- unique(catalogo[c("dimension", "factor")])
  claves <- paste(factores$dimension, factores$factor, sep = "|")
  estados <- split(
    as.character(catalogo$estado),
    paste(catalogo$dimension, catalogo$factor, sep = "|")
  )
  factores$como_resolverlo <- mapply(
    .resolver_factor, factores$dimension, factores$factor,
    USE.NAMES = FALSE
  )
  factores$perfil_mide <- claves %in% c(
    "Completitud|Densidad", "Unicidad|No-duplicaci\u00f3n"
  )
  factores$aplicabilidad <- "siempre"
  factores$aplicabilidad[factores$dimension == "Frescura"] <- "temporal"
  factores$aplicabilidad[factores$factor %in% c(
    "Exactitud posicional absoluta", "Exactitud posicional relativa",
    "Consistencia topol\u00f3gica", "Comisi\u00f3n"
  )] <- "geometria"
  factores$disponibilidad <- vapply(claves, function(clave) {
    disponibles <- unique(estados[[clave]])
    if (length(disponibles) && all(disponibles == "fuera_de_alcance")) {
      "fuera_de_alcance"
    } else {
      "disponible"
    }
  }, character(1L))
  estructura <- marco_calidad(
    "Marco de calidad de datos de AGESIC", factores
  )
  estructura$origen <- "AGESIC 2020, versi\u00f3n 1.6"
  estructura
}

#' @rdname marco_calidad
#' @export
marco_iso25012 <- function() {
  dimension <- c(
    rep("Inherente", 5L),
    rep("Inherente y dependiente del sistema", 7L),
    rep("Dependiente del sistema", 3L)
  )
  factor <- c(
    "Exactitud", "Completitud", "Consistencia", "Credibilidad", "Actualidad",
    "Accesibilidad", "Conformidad", "Confidencialidad", "Eficiencia",
    "Precisi\u00f3n", "Trazabilidad", "Comprensibilidad",
    "Disponibilidad", "Portabilidad", "Recuperabilidad"
  )
  descripcion <- c(
    "Considera si los datos representan correctamente los hechos o valores que pretenden describir.",
    "Considera si est\u00e1n presentes los valores y registros necesarios para el uso declarado.",
    "Revisa que los datos no se contradigan entre s\u00ed ni con reglas acordadas.",
    "Expresa la confianza respaldada por el origen y las evidencias disponibles.",
    "Considera si los datos conservan vigencia para el momento y uso declarados.",
    "Considera si las personas o procesos autorizados pueden obtener y usar los datos.",
    "Revisa la adhesi\u00f3n a normas, contratos o convenciones aplicables.",
    "Considera si el acceso y la divulgaci\u00f3n respetan las autorizaciones definidas.",
    "Considera los recursos y tiempos necesarios para procesar los datos en el contexto previsto.",
    "Considera si el detalle y la resoluci\u00f3n de los valores bastan para el uso previsto.",
    "Considera si puede reconstruirse el origen, los cambios y el recorrido de los datos.",
    "Considera si el significado, la estructura y la representaci\u00f3n pueden interpretarse correctamente.",
    "Considera si los datos pueden recuperarse cuando los necesitan usuarios o procesos autorizados.",
    "Considera si los datos pueden trasladarse entre entornos conservando su utilidad y calidad.",
    "Considera si los datos y su calidad pueden restaurarse despu\u00e9s de fallas o p\u00e9rdidas."
  )
  factores <- data.frame(
    dimension = dimension,
    factor = factor,
    descripcion = descripcion,
    como_resolverlo = paste0(
      "Declarar requisitos y m\u00e9tricas para la caracter\u00edstica ", factor, "."
    ),
    perfil_mide = FALSE,
    aplicabilidad = "siempre",
    disponibilidad = "disponible",
    stringsAsFactors = FALSE
  )
  estructura <- marco_calidad("Marco ISO/IEC 25012:2008", factores)
  estructura$origen <- "ISO/IEC 25012:2008"
  estructura
}

#' @rdname marco_calidad
#' @export
marco_cepal <- function() {
  dimension <- c(
    rep("Nivel A. Gesti\u00f3n del sistema estad\u00edstico", 3L),
    rep("Nivel B. Gesti\u00f3n del entorno institucional", 6L),
    rep("Nivel C. Gesti\u00f3n del proceso estad\u00edstico", 4L),
    rep("Nivel D. Gesti\u00f3n de los productos estad\u00edsticos", 6L)
  )
  factor <- c(
    "Coordinaci\u00f3n del sistema estad\u00edstico nacional",
    "Gesti\u00f3n de las relaciones con los usuarios de datos, los proveedores de datos y otros grupos de inter\u00e9s",
    "Gesti\u00f3n de normas y est\u00e1ndares estad\u00edsticos",
    "Asegurar la independencia profesional",
    "Asegurar la imparcialidad y la objetividad",
    "Asegurar la transparencia",
    "Asegurar la confidencialidad estad\u00edstica y la seguridad de los datos",
    "Asegurar el compromiso con la calidad",
    "Asegurar la suficiencia de los recursos",
    "Asegurar la solidez metodol\u00f3gica",
    "Asegurar una buena relaci\u00f3n costo-eficiencia",
    "Asegurar procedimientos estad\u00edsticos apropiados",
    "Manejo de la carga del encuestado",
    "Asegurar la relevancia",
    "Asegurar la precisi\u00f3n y la confiabilidad",
    "Asegurar la oportunidad y la puntualidad",
    "Asegurar la accesibilidad y la claridad",
    "Asegurar la coherencia y la comparabilidad",
    "Gesti\u00f3n de los metadatos"
  )
  descripcion <- c(
    "Considera c\u00f3mo se articulan los organismos y las unidades del sistema estad\u00edstico nacional para sostener una producci\u00f3n estad\u00edstica coordinada.",
    "Considera c\u00f3mo se gestionan las relaciones que permiten identificar necesidades, recibir datos y atender a otros grupos de inter\u00e9s.",
    "Considera c\u00f3mo se adoptan, mantienen y utilizan normas y est\u00e1ndares estad\u00edsticos comunes.",
    "Considera si las decisiones y m\u00e9todos estad\u00edsticos pueden desarrollarse con independencia profesional frente a interferencias indebidas.",
    "Considera si las decisiones estad\u00edsticas se toman y comunican con imparcialidad y objetividad.",
    "Considera si las decisiones, m\u00e9todos y resultados relevantes se exponen de forma transparente.",
    "Considera si la confidencialidad estad\u00edstica y la seguridad de los datos se protegen durante su gesti\u00f3n y uso.",
    "Considera si existe un compromiso institucional expl\u00edcito y sostenido con la calidad estad\u00edstica.",
    "Considera si los recursos humanos, financieros, tecnol\u00f3gicos y organizacionales son suficientes para el trabajo estad\u00edstico.",
    "Considera si el desarrollo estad\u00edstico se apoya en conocimientos, m\u00e9todos y est\u00e1ndares metodol\u00f3gicos s\u00f3lidos.",
    "Considera si los recursos utilizados guardan una relaci\u00f3n razonable con los resultados y objetivos estad\u00edsticos.",
    "Considera si las etapas y actividades del proceso estad\u00edstico se ejecutan mediante procedimientos apropiados.",
    "Considera si la informaci\u00f3n solicitada y las formas de recolecci\u00f3n gestionan razonablemente la carga del encuestado.",
    "Considera si el producto estad\u00edstico responde a las necesidades de informaci\u00f3n de sus usuarios.",
    "Considera si el producto estad\u00edstico representa de manera fiable aquello que busca describir y medir.",
    "Considera si el producto estad\u00edstico se publica cuando resulta \u00fatil y conforme al calendario comprometido.",
    "Considera si el producto estad\u00edstico puede encontrarse, obtenerse e interpretarse con claridad.",
    "Considera si los conceptos, m\u00e9todos y resultados del producto estad\u00edstico pueden relacionarse entre operaciones y momentos.",
    "Considera si la documentaci\u00f3n que explica el producto estad\u00edstico se mantiene completa, accesible y utilizable."
  )
  como_resolverlo <- c(
    rep(
      "No es una limitaci\u00f3n transitoria del motor: se documenta en el marco institucional y no se establece sobre una tabla, porque el objeto del principio es el sistema estad\u00edstico, el entorno institucional o el proceso estad\u00edstico.",
      13L
    ),
    "Declarar las necesidades del uso estad\u00edstico y la evidencia y m\u00e9tricas con las que se verificar\u00e1 la relevancia del producto.",
    "Definir las fuentes, m\u00e9todos de validaci\u00f3n y m\u00e9tricas que respalden la precisi\u00f3n y la confiabilidad del producto.",
    "Definir el momento de referencia, el calendario y las m\u00e9tricas que permitan verificar la oportunidad y la puntualidad.",
    "Definir los canales, formatos, metadatos y m\u00e9tricas que permitan verificar la accesibilidad y la claridad.",
    "Definir conceptos, m\u00e9todos, per\u00edodos y m\u00e9tricas que permitan verificar la coherencia y la comparabilidad.",
    "Definir los metadatos necesarios para interpretar, reproducir y mantener el producto estad\u00edstico."
  )
  factores <- data.frame(
    principio = seq_along(factor),
    dimension = dimension,
    factor = factor,
    descripcion = descripcion,
    como_resolverlo = como_resolverlo,
    perfil_mide = FALSE,
    aplicabilidad = "siempre",
    disponibilidad = c(rep("fuera_de_alcance", 13L), rep("disponible", 6L)),
    stringsAsFactors = FALSE
  )
  estructura <- marco_calidad(
    "Marco de aseguramiento de la calidad estad\u00edstica de Naciones Unidas, adaptado por CEA/CEPAL",
    factores
  )
  estructura$origen <- paste0(
    "Marco de Naciones Unidas (2019): Manual del marco nacional de aseguramiento de calidad en las estad\u00edsticas oficiales, ",
    "Estudios en M\u00e9todos, serie M, N\u00b0 100 (ST/ESA/STAT/SER.M/100), Nueva York. ",
    "Adaptaci\u00f3n regional CEA/CEPAL: Grupo de Trabajo de la Conferencia Estad\u00edstica de las Am\u00e9ricas (CEA), ",
    "coordinado por Colombia (DANE) y M\u00e9xico (INEGI), Secretar\u00eda T\u00e9cnica: Divisi\u00f3n de Estad\u00edsticas de la CEPAL (2022), ",
    "Gu\u00eda para la implementaci\u00f3n del marco de aseguramiento de la calidad para procesos y productos estad\u00edsticos ",
    "(LC/CEA.11/19), Comisi\u00f3n Econ\u00f3mica para Am\u00e9rica Latina y el Caribe (CEPAL), Naciones Unidas, Santiago. ",
    "Fuentes: https://unstats.un.org/unsd/methodology/dataquality/; ",
    "https://repositorio.cepal.org/handle/11362/47464"
  )
  estructura
}

#' @export
print.marco_calidad <- function(x, ...) {
  cli::cli_h2(x$nombre)
  cli::cli_dl(c(
    "Dimensiones" = length(unique(x$factores$dimension)),
    "Factores" = nrow(x$factores),
    "Origen" = x$origen
  ))
  invisible(x)
}

#' @export
as.data.frame.marco_calidad <- function(x, row.names = NULL, optional = FALSE,
                                        ...) {
  as.data.frame(x$factores, row.names = row.names, optional = optional, ...)
}
