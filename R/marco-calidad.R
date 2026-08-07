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
#' @return `marco_calidad()`, `marco_agesic()` y `marco_iso25012()` devuelven un objeto S3
#'   `marco_calidad`. `as.data.frame()` devuelve su tabla de factores.
#' @export
#' @seealso [catalogo_agesic()], [modelo()], [cobertura_analisis()]
#' @references ISO/IEC (2008). *ISO/IEC 25012:2008 Software engineering —
#'   Software product Quality Requirements and Evaluation (SQuaRE) — Data
#'   quality model*. <https://www.iso.org/standard/35736.html>.
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
