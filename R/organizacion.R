# La frontera institucional, que es una declaracion y no un dato.
#
# Las granularidades 9 y 10 del marco -`organizacion` y `conjuntoOrganizaciones`-
# estuvieron declaradas y sin medir durante todo el desarrollo, y el motivo
# escrito era correcto: no falta codigo, falta el objeto. Que bases pertenecen a
# un organismo no esta en las tablas, ni en el catalogo del motor, ni se deduce
# de nada. Es un hecho institucional que vive en un documento de gobernanza o en
# la cabeza de alguien.
#
# La salida es la misma que ya se uso un piso mas abajo con el conjunto de
# colecciones: lo declara quien lo sabe. `organizacion()` toma un nombre y las
# colecciones que le pertenecen, y con eso `agregar()` puede subir.
#
# **Y es opcional, en el sentido fuerte.** Un analisis de calidad no siempre
# tiene una organizacion detras: puede ser una entrega suelta, un archivo que
# alguien mando, una base sin duenno institucional declarado. Nada en el paquete
# obliga a pasar por estos dos niveles; existen para quien los necesita y no
# aparecen para quien no. Por eso la frontera no tiene valor por omision: sin
# declaracion, `agregar()` a estos niveles se niega y dice como declararla, que
# es distinto de inventar una organizacion que nadie nombro.
#
# `organizacion()` no pide una conexion. Una coleccion es una cosa viva -tablas
# de un motor- pero una organizacion es un enunciado sobre colecciones, y puede
# reunir colecciones medidas en momentos distintos o contra motores distintos.
# Exigir una conexion habria atado la declaracion institucional a una sesion.

.nombre_de_frontera <- function(x) {
  if (inherits(x, "coleccion_lupa")) return(x$nombre)
  if (inherits(x, "perfil_coleccion")) return(x$meta$nombre)
  if (inherits(x, "organizacion_lupa")) return(x$nombre)
  if (is.character(x) && length(x) == 1L) return(x)
  NA_character_
}

.nombres_declarados_frontera <- function(elementos, argumento,
                                        con_alias = FALSE) {
  if (is.character(elementos)) elementos <- as.list(elementos)
  if (!is.list(elementos) || !length(elementos)) {
    stop(
      "`", argumento, "` debe ser un vector de nombres o una lista no vacia.",
      call. = FALSE
    )
  }
  desde_objeto <- vapply(elementos, .nombre_de_frontera, character(1L))
  etiquetas <- names(elementos)
  # El nombre de la lista renombra la parte para el informe; el nombre del
  # objeto sigue siendo su identidad. Los dos tienen que servir para
  # reconocerla, porque `agregar()` escribe la del objeto y el nivel de arriba
  # compara contra la declarada: sin el alias, la composicion documentada no
  # corria y el conjunto rechazaba una medida que si le pertenecia.
  nombres <- if (is.null(etiquetas)) {
    desde_objeto
  } else {
    ifelse(nzchar(etiquetas) & !is.na(etiquetas), etiquetas, desde_objeto)
  }
  if (anyNA(nombres) || any(!nzchar(nombres))) {
    stop(
      "Cada elemento de `", argumento, "` tiene que quedar identificado: un ",
      "nombre de texto, o un objeto de coleccion(), perfilar_coleccion() u ",
      "organizacion() del que se pueda tomar el nombre.", call. = FALSE
    )
  }
  if (anyDuplicated(nombres)) {
    repetidos <- unique(nombres[duplicated(nombres)])
    stop(
      "`", argumento, "` repite nombres: ", paste(repetidos, collapse = ", "),
      ". Los nombres son la identidad de cada parte dentro de la frontera.",
      call. = FALSE
    )
  }
  if (!con_alias) return(nombres)
  alias <- desde_objeto
  alias[is.na(alias) | !nzchar(alias)] <- nombres[is.na(alias) | !nzchar(alias)]
  list(nombres = nombres, alias = alias)
}

# Traduce lo que trae una medida al nombre con el que la frontera la declara.
# Una medida puede venir con el nombre propio del objeto y la frontera haberla
# renombrado para el informe; las dos formas identifican a la misma parte.
.resolver_partes_frontera <- function(entidades, frontera) {
  alias <- frontera$alias
  if (is.null(alias)) return(entidades)
  posicion <- match(entidades, alias)
  ifelse(is.na(posicion), entidades, frontera$declaradas[posicion])
}

#' Declarar una organización y las colecciones que le pertenecen
#'
#' Las granularidades novena y décima del marco —una organización y un conjunto
#' de organizaciones— necesitan un dato que **no está en los datos**: qué bases
#' pertenecen a qué organismo. `lupa` no lo adivina, así que lo declara quien lo
#' sabe, con el mismo mecanismo que ya usa el conjunto de colecciones.
#'
#' **Es opcional.** Un análisis de calidad no siempre tiene una organización
#' detrás, y nada obliga a pasar por estos niveles: existen para quien los
#' necesita. Sin declaración, [agregar()] a `"organizacion"` se niega y explica
#' cómo declararla, que es distinto de inventar una frontera que nadie nombró.
#'
#' No pide una conexión. Una colección es una cosa viva —tablas de un motor—,
#' pero una organización es un enunciado *sobre* colecciones, y puede reunir
#' colecciones medidas en momentos distintos o contra motores distintos.
#'
#' @param nombre Nombre de la organización. Es su identidad dentro de un
#'   conjunto de organizaciones.
#' @param colecciones Colecciones que le pertenecen: un vector de nombres, o una
#'   lista de objetos de [coleccion()] o [perfilar_coleccion()]. Si la lista
#'   tiene nombres, esos nombres mandan sobre el del objeto.
#'
#' @return Objeto S3 `organizacion_lupa` con `nombre`, `declaradas` y
#'   `n_declaradas`.
#' @export
#' @seealso [agregar()], [coleccion()], [granularidades()]
#'
#' @examples
#' organismo <- organizacion("MIDES", c("padron", "tramites"))
#' organismo
#' organismo$declaradas
organizacion <- function(nombre, colecciones) {
  if (!is.character(nombre) || length(nombre) != 1L || is.na(nombre) ||
      !nzchar(nombre)) {
    stop("`nombre` debe ser un texto no vacio.", call. = FALSE)
  }
  declaradas <- .nombres_declarados_frontera(colecciones, "colecciones")
  estructura <- list(
    nombre = nombre,
    declaradas = declaradas,
    n_declaradas = length(declaradas)
  )
  class(estructura) <- "organizacion_lupa"
  estructura
}

#' @export
print.organizacion_lupa <- function(x, ...) {
  cli::cli_text("Organizaci\u00f3n declarada: {.strong {x$nombre}}")
  cli::cli_text(
    "{x$n_declaradas} colecci{?\u00f3n/ones}: {.val {x$declaradas}}"
  )
  cli::cli_text(
    "La frontera es declarada: `lupa` no infiere a qu\u00e9 organismo pertenece ",
    "una base."
  )
  invisible(x)
}

.validar_organizacion_destino <- function(organizacion) {
  if (!inherits(organizacion, "organizacion_lupa")) {
    stop(
      "`organizacion` debe provenir de organizacion(): la granularidad ",
      "'organizacion' agrega sobre una frontera declarada, y sin ella el ",
      "numero no describiria a ningun organismo.", call. = FALSE
    )
  }
  list(nombre = organizacion$nombre, declaradas = organizacion$declaradas)
}

.validar_conjunto_organizaciones <- function(organizaciones) {
  if (!is.list(organizaciones) || !length(organizaciones)) {
    stop(
      "`organizaciones` debe ser una lista no vacia de objetos creados por ",
      "organizacion().", call. = FALSE
    )
  }
  validas <- vapply(
    organizaciones, inherits, logical(1L), what = "organizacion_lupa"
  )
  if (any(!validas)) {
    stop(
      "Cada elemento de `organizaciones` debe provenir de organizacion().",
      call. = FALSE
    )
  }
  identidad <- .nombres_declarados_frontera(
    organizaciones, "organizaciones", con_alias = TRUE
  )
  list(
    nombre = "conjuntoOrganizaciones", declaradas = identidad$nombres,
    alias = identidad$alias
  )
}
