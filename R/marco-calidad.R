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
#' dimensiones y cuyos valores son factores.
#'
#' @param nombre Nombre del marco.
#' @param factores Data frame con `dimension` y `factor`, o lista con nombres.
#'   El data frame puede añadir `como_resolverlo`, `perfil_mide`,
#'   `aplicabilidad` (`"siempre"`, `"temporal"` o `"geometria"`) y
#'   `disponibilidad` (`"disponible"` o `"fuera_de_alcance"`).
#'
#' @return `marco_calidad()` y `marco_agesic()` devuelven un objeto S3
#'   `marco_calidad`. `as.data.frame()` devuelve su tabla de factores.
#' @export
#' @seealso [catalogo_agesic()], [modelo()], [cobertura_analisis()]
#'
#' @examples
#' propio <- marco_calidad("Marco operativo", list(
#'   Trazabilidad = c("Origen documentado", "Linaje reproducible"),
#'   Pertinencia = "Adecuación al uso"
#' ))
#' propio
#' as.data.frame(propio)
#' marco_agesic()
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
