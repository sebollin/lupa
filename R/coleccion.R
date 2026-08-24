# Nivel 7 del marco: la coleccion, una base de datos entera.
#
# La granularidad estaba declarada y no se media, y el mensaje decia que faltaba
# implementarla. Lo que faltaba era el objeto: una base es un conjunto de tablas
# y nadie le decia a `lupa` cuales la componen.
#
# El diseno esta gobernado por lo que se midio en bases reales, y las cifras
# rompen cualquier enfoque exploratorio: una coleccion puede tener ~1.730 tablas
# en 64 esquemas, y otra ~1.291 tablas en 21 esquemas con miles de millones de
# filas. Explorar todos los pares serian 1,5 millones de comparaciones.
#
# De ahi salen las cinco decisiones que sostienen todo lo demas:
#
#   1. La frontera la DECLARA el usuario. `lupa` no recorre el catalogo para
#      descubrir tablas: eso convertiria un error de permisos en un resultado.
#   2. El esquema es parte de la identidad de la tabla, no un adorno.
#   3. Los permisos parciales son el caso normal, no el borde: una credencial
#      que lee 9 tablas de 897 objetos es lo habitual. Lo que no se pudo leer va
#      a `cobertura_coleccion` con su motivo, nunca a cero.
#   4. Cada tabla declara su propio muestreo. No se promedian alcances distintos
#      como si fueran uno.
#   5. No hay snapshot: perfilar una coleccion son muchas consultas y la base
#      puede cambiar entre ellas. Cada tabla declara el momento en que se midio,
#      y el objeto declara que no hubo lectura instantanea.
#
# La sexta decision salio de correr el paquete contra bases reales, y es la que
# gobierna a las cinco anteriores cuando algo falla a medias:
#
#   6. Ante un fallo PARCIAL se devuelve lo medido con su alcance declarado.
#      Nunca el todo descartado, y nunca `0` -ni `-Inf`- por ausencia de
#      medicion. En el vocabulario de este paquete `0` significa "medido y
#      ninguno"; usarlo para "no se midio" es una afirmacion falsa. Por eso no
#      queda ni un `na.rm = TRUE` que pueda colapsar un conjunto entero de `NA`
#      en un numero: donde no hubo medicion va `NA` y su motivo va a la
#      cobertura.

# ---------------------------------------------------------------------------
# Identificadores
# ---------------------------------------------------------------------------
#
# `strsplit(tablas, ".", fixed = TRUE)` mas "primer y ultimo componente" perdia
# el medio: `catalogo.esquema.tabla` quedaba como `catalogo`.`tabla`, un
# identificador sintacticamente valido contra una tabla que no existe. El error
# de parseo se le devolvia al usuario como *"la tabla no existe"* con un
# `como_resolverlo` que lo mandaba a pedirle permisos al DBA sobre una tabla que
# ya podia leer: la inversion exacta de la decision 1.
#
# Ahora el parseo respeta el entrecomillado del motor, rechaza lo que no puede
# interpretar NOMBRANDO LA CAUSA REAL, y deja registrado si el nombre se partio,
# para que un fallo posterior pueda decirlo.

.aperturas_comilla <- c("\"", "`", "[")
.cierres_comilla <- c("\"", "`", "]")

# Separa un identificador de texto en sus partes. Un punto dentro de comillas
# dobles, comillas invertidas o corchetes pertenece al nombre y no separa nada:
# `"mi.tabla"` es UNA tabla llamada `mi.tabla`, no un esquema `mi` con una tabla
# `tabla`.
.partir_identificador <- function(texto) {
  caracteres <- strsplit(texto, "", fixed = TRUE)[[1L]]
  partes <- character()
  actual <- character()
  cierre <- NA_character_
  for (caracter in caracteres) {
    if (!is.na(cierre)) {
      actual <- c(actual, caracter)
      if (identical(caracter, cierre)) cierre <- NA_character_
      next
    }
    indice <- match(caracter, .aperturas_comilla)
    if (!is.na(indice)) {
      cierre <- .cierres_comilla[[indice]]
      actual <- c(actual, caracter)
      next
    }
    if (identical(caracter, ".")) {
      partes <- c(partes, paste(actual, collapse = ""))
      actual <- character()
      next
    }
    actual <- c(actual, caracter)
  }
  list(
    partes = c(partes, paste(actual, collapse = "")),
    abierto = !is.na(cierre)
  )
}

# Quita el entrecomillado externo y deshace el escape doblado de adentro. Sin
# esto `"esquema"."tabla"` terminaba citado otra vez y el SQL era
# `` `"esquema"`.`"tabla"` ``: dos identificadores que no existen.
.quitar_comillas_identificador <- function(parte) {
  largo <- nchar(parte)
  if (largo < 2L) return(parte)
  primero <- substr(parte, 1L, 1L)
  ultimo <- substr(parte, largo, largo)
  indice <- match(primero, .aperturas_comilla)
  if (is.na(indice) || !identical(ultimo, .cierres_comilla[[indice]])) {
    return(parte)
  }
  interior <- substr(parte, 2L, largo - 1L)
  doble <- paste0(primero, .cierres_comilla[[indice]])
  gsub(doble, .cierres_comilla[[indice]], interior, fixed = TRUE)
}

.error_identificador <- function(entrada, causa, salida) {
  stop(
    "`lupa` no puede interpretar el nombre de tabla ", encodeString(entrada, quote = "'"),
    ": ", causa, " ", salida,
    call. = FALSE
  )
}

.salida_data_frame_identificador <- paste(
  "Declare la tabla con `data.frame(esquema = , tabla = )`, que conserva los",
  "nombres literales -puntos, espacios y comillas incluidos- y no los parsea,",
  "o pase un `DBI::Id`."
)

# Devuelve un data.frame de una fila por entrada con `esquema`, `tabla` y si el
# nombre se partio en un punto.
.parsear_identificadores_texto <- function(tablas) {
  esquema <- rep(NA_character_, length(tablas))
  tabla <- rep(NA_character_, length(tablas))
  partido <- rep(FALSE, length(tablas))
  for (i in seq_along(tablas)) {
    entrada <- tablas[[i]]
    cortado <- .partir_identificador(entrada)
    if (cortado$abierto) {
      .error_identificador(
        entrada, "quedo una comilla o un corchete sin cerrar.",
        .salida_data_frame_identificador
      )
    }
    partes <- vapply(cortado$partes, .quitar_comillas_identificador, character(1L),
                     USE.NAMES = FALSE)
    if (length(partes) > 2L) {
      .error_identificador(
        entrada,
        paste0(
          "tiene ", length(partes), " partes separadas por puntos y `lupa` solo ",
          "interpreta la forma 'esquema.tabla'. No se adivina cual parte es el ",
          "catalogo, cual el esquema y cual pertenece al nombre: partirlo mal ",
          "produce un identificador valido contra una tabla que no existe, y el ",
          "fallo posterior parece un problema de permisos."
        ),
        .salida_data_frame_identificador
      )
    }
    if (any(!nzchar(partes))) {
      .error_identificador(
        entrada,
        paste0(
          "una de las partes separadas por puntos quedo vacia (un punto al ",
          "principio, al final o repetido)."
        ),
        .salida_data_frame_identificador
      )
    }
    if (length(partes) == 2L) {
      esquema[[i]] <- partes[[1L]]
      tabla[[i]] <- partes[[2L]]
      partido[[i]] <- TRUE
    } else {
      tabla[[i]] <- partes[[1L]]
    }
  }
  data.frame(
    esquema = esquema, tabla = tabla, partido = partido,
    stringsAsFactors = FALSE
  )
}

# `DBI::Id` es la forma canonica de nombrar una tabla en DBI y es la unica que
# no necesita parseo. Se acepta un `Id` suelto o una lista de `Id`.
.partes_id_dbi <- function(id) {
  nombres <- tryCatch(id@name, error = function(e) NULL)
  if (!length(nombres) || !is.character(nombres)) {
    stop(
      "Un `DBI::Id` de la coleccion no trae componentes de nombre utilizables.",
      call. = FALSE
    )
  }
  etiquetas <- names(nombres)
  # Tres componentes SI se admiten desde que el catalogo es parte de la
  # identidad. Cuatro no: no hay un nivel por encima del catalogo en el modelo
  # de DBI, asi que un identificador de cuatro partes es un nombre mal formado y
  # se rechaza **nombrando la causa**, que es lo que evita que el usuario lo lea
  # como un problema suyo de permisos.
  if (length(nombres) > 3L) {
    stop(
      "`lupa` no admite un `DBI::Id` de mas de tres componentes: por encima del ",
      "catalogo no hay un nivel que la coleccion sepa declarar. Revise el ",
      "identificador; si el nombre lleva puntos, decl\u00e1relo con ",
      "`data.frame(catalogo = , esquema = , tabla = )`.",
      call. = FALSE
    )
  }
  if (length(nombres) == 3L) {
    return(c(
      catalogo = unname(nombres[[1L]]), esquema = unname(nombres[[2L]]),
      tabla = unname(nombres[[3L]])
    ))
  }
  if (length(nombres) == 2L) {
    return(c(esquema = unname(nombres[[1L]]), tabla = unname(nombres[[2L]])))
  }
  if (!is.null(etiquetas) && identical(etiquetas[[1L]], "schema")) {
    stop(
      "Un `DBI::Id` con `schema` y sin `table` no nombra una tabla.",
      call. = FALSE
    )
  }
  c(esquema = NA_character_, tabla = unname(nombres[[1L]]))
}

.es_id_dbi <- function(x) {
  isS4(x) && inherits(x, "Id")
}

.exigir_texto_declarado <- function(valores, campo, permitir_na = FALSE) {
  if (is.factor(valores)) valores <- as.character(valores)
  # `data.frame(esquema = NA)` produce un vector logico de `NA`: es la forma
  # normal de declarar "sin esquema" y no un tipo equivocado.
  if (is.logical(valores) && all(is.na(valores))) {
    valores <- as.character(valores)
  }
  if (!is.character(valores)) {
    stop(
      "La columna `", campo, "` de `tablas` debe ser de texto.", call. = FALSE
    )
  }
  if (!permitir_na && anyNA(valores)) {
    stop(
      "La columna `", campo, "` de `tablas` no admite `NA`.", call. = FALSE
    )
  }
  vacias <- !is.na(valores) & !nzchar(valores)
  if (any(vacias)) {
    stop(
      "La columna `", campo, "` de `tablas` no admite cadenas vacias.",
      call. = FALSE
    )
  }
  valores
}

.normalizar_tablas_coleccion <- function(tablas) {
  if (.es_id_dbi(tablas)) tablas <- list(tablas)
  if (inherits(tablas, "data.frame")) {
    if (!"tabla" %in% names(tablas) || !nrow(tablas)) {
      stop(
        "`tablas` como data.frame debe traer una columna `tabla` y al menos ",
        "una fila.", call. = FALSE
      )
    }
    nombre_tabla <- .exigir_texto_declarado(tablas$tabla, "tabla")
    esquema <- if ("esquema" %in% names(tablas)) {
      .exigir_texto_declarado(tablas$esquema, "esquema", permitir_na = TRUE)
    } else {
      rep(NA_character_, nrow(tablas))
    }
    tipo <- if ("tipo" %in% names(tablas)) {
      .exigir_texto_declarado(tablas$tipo, "tipo")
    } else {
      rep("tabla", nrow(tablas))
    }
    # El catalogo es la tercera parte que usan SQL Server y otros motores. Va
    # como columna estructurada y siempre presente -`NA` cuando no existe- en
    # vez de dentro del texto: asi no hay que volver a partir nada, que es de
    # donde salian los defectos de esta seccion.
    catalogo <- if ("catalogo" %in% names(tablas)) {
      .exigir_texto_declarado(tablas$catalogo, "catalogo", permitir_na = TRUE)
    } else {
      rep(NA_character_, nrow(tablas))
    }
    return(data.frame(
      catalogo = as.character(catalogo),
      esquema = as.character(esquema), tabla = as.character(nombre_tabla),
      tipo = as.character(tipo),
      # Un nombre declarado nunca se partio: viaja literal.
      declaracion = rep("declarado", nrow(tablas)),
      stringsAsFactors = FALSE
    ))
  }
  if (is.list(tablas) && length(tablas) &&
      all(vapply(tablas, .es_id_dbi, logical(1L)))) {
    partes <- lapply(tablas, .partes_id_dbi)
    return(data.frame(
      # `.partes_id_dbi()` devuelve un vector con nombres, asi que pedirle una
      # etiqueta que no trae **sale de rango** en vez de dar NULL. Hay que
      # mirar los nombres antes de indexar.
      catalogo = vapply(partes, function(x) {
        if ("catalogo" %in% names(x)) unname(x[["catalogo"]]) else NA_character_
      }, character(1L)),
      esquema = vapply(partes, function(x) unname(x[["esquema"]]), character(1L)),
      tabla = vapply(partes, function(x) unname(x[["tabla"]]), character(1L)),
      tipo = rep("tabla", length(tablas)),
      declaracion = rep("dbi_id", length(tablas)),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.character(tablas) || !length(tablas) || anyNA(tablas) ||
      !all(nzchar(tablas))) {
    stop(
      "`tablas` debe ser un vector de nombres, una lista de `DBI::Id` o un ",
      "data.frame con columnas `esquema` y `tabla`.", call. = FALSE
    )
  }
  parseadas <- .parsear_identificadores_texto(tablas)
  data.frame(
    catalogo = if (!is.null(parseadas$catalogo)) {
      parseadas$catalogo
    } else {
      rep(NA_character_, length(tablas))
    },
    esquema = parseadas$esquema,
    tabla = parseadas$tabla,
    tipo = rep("tabla", length(tablas)),
    declaracion = ifelse(parseadas$partido, "texto_partido", "texto_simple"),
    stringsAsFactors = FALSE
  )
}

# El identificador completo de una tabla dentro de una coleccion. Es la UNICA
# forma de armarlo: `agregacion.R` tenia una copia identica como cierre local, y
# las dos **tienen que coincidir** o la cobertura de una coleccion deja de
# cuadrar —la frontera se declara con una y se lee con la otra—.
#
# Por que lleva el esquema: usar el nombre pelado hacia que `public.personas` y
# `auditoria.personas` colapsaran en una sola tabla, y entonces medir una de las
# dos daba cobertura 1 de 1 en vez de 1 de 2. El esquema es parte de la
# identidad, no un adorno.
# Se unen las partes que existen, en orden. Sigue siendo inyectivo: `t`,
# `esq.t`, `cat.esq.t` y `cat.t` son cuatro identidades distintas, y dos tablas
# con el mismo nombre en catalogos distintos no colapsan —que era el defecto que
# tenia el nombre pelado con los esquemas—.
.identificadores_tabla <- function(esquema, tabla, catalogo = NULL) {
  if (is.null(catalogo)) catalogo <- rep(NA_character_, length(tabla))
  vapply(seq_along(tabla), function(i) {
    partes <- c(catalogo[[i]], esquema[[i]], tabla[[i]])
    paste(partes[!is.na(partes)], collapse = ".")
  }, character(1L))
}

# `dbQuoteIdentifier(con, "public.personas")` cita UN identificador que contiene
# un punto -`"public.personas"`- y no el compuesto `"public"."personas"`. Sobre
# un motor con esquemas eso consulta una tabla que no existe y el par termina
# declarado como ilegible aunque los permisos esten bien. El identificador
# compuesto se construye con `DBI::Id`.
.referencia_de_partes <- function(esquema, tabla, catalogo = NA_character_) {
  if (!is.na(catalogo)) {
    if (is.na(esquema)) {
      return(DBI::Id(catalog = catalogo, table = tabla))
    }
    return(DBI::Id(catalog = catalogo, schema = esquema, table = tabla))
  }
  if (is.na(esquema)) tabla else DBI::Id(schema = esquema, table = tabla)
}

# Una coleccion guardada antes de que existiera la columna `catalogo` se
# deserializa bien, pero al perfilarla fallaba: el codigo nuevo la lee y no esta.
# Medido: sin este adaptador el objeto viejo se lee y no se puede perfilar.
#
# Se completa con `NA`, que es exactamente lo que significa -esa coleccion se
# declaro sin catalogos- y deja el identificador igual al que tenia.
.completar_catalogo_coleccion <- function(tablas) {
  if (inherits(tablas, "data.frame") && !"catalogo" %in% names(tablas)) {
    tablas$catalogo <- rep(NA_character_, nrow(tablas))
  }
  tablas
}

#' Declarar la frontera de una colección
#'
#' Una colección es una base de datos entera: el séptimo nivel de granularidad
#' del marco. `lupa` no la descubre recorriendo el catálogo — **la frontera la
#' declara quien conoce la base**, igual que los pesos de [indice_calidad()] o
#' el marco de [cobertura_analisis()]. Explorar el catálogo convertiría un error
#' de permisos en un resultado, y en bases reales de más de mil tablas
#' recorrerlas todas no es viable.
#'
#' Esta función no consulta nada: sólo declara. Lo que se mide viene después,
#' con [perfilar_coleccion()].
#'
#' El **esquema es parte de la identidad de la tabla**, y el **catálogo también**
#' cuando existe: `catalogo.esquema.tabla` es el nombre de tres partes que usan
#' SQL Server y otros motores. Se declara con la columna `catalogo`, y donde no
#' hay catálogo se omite —o va `NA`— y la identidad queda igual que siempre. Dos
#' tablas con el mismo nombre y esquema en catálogos distintos son **dos tablas
#' distintas**, y se cuentan como dos. Una tercera columna
#' `tipo` permite declarar qué es cada objeto —`"tabla"`, `"vista"`,
#' `"temporal"`—, porque el conteo bruto de un catálogo mezcla tablas base con
#' vistas, índices y secuencias, y no todas se perfilan igual.
#'
#' @section Cómo declarar el nombre:
#'
#' Hay tres formas, y **no son equivalentes**:
#'
#' \describe{
#'   \item{`data.frame(esquema =, tabla =)`}{**La forma recomendada para
#'     cualquier nombre no trivial.** Los nombres viajan literales: puntos,
#'     espacios y comillas incluidos. No hay parseo y por lo tanto no hay nada
#'     que se pueda parsear mal.}
#'   \item{`DBI::Id`, suelto o en una lista}{La forma canónica de DBI. Tampoco
#'     se parsea. Se admiten hasta tres componentes —catálogo, esquema y tabla—.
#'     Con cuatro o más se rechaza **nombrando la causa**: por encima del
#'     catálogo no hay un nivel que la colección sepa declarar, y devolver ese
#'     error como si fuera un problema de permisos mandaría a pedir un acceso
#'     que ya se tiene.}
#'   \item{Texto `"esquema.tabla"`}{Atajo cómodo para el caso simple. El texto
#'     **se parte en el punto**, respetando el entrecomillado del motor:
#'     un nombre entrecomillado con un punto adentro queda entero, como una
#'     sola tabla. Un nombre de tres o más
#'     partes, un punto al principio o al final, o una comilla sin cerrar se
#'     **rechazan acá**, con el motivo real. No se aceptan para fallar más tarde
#'     como si fueran un problema de permisos.}
#' }
#'
#' El atajo de texto no puede resolver una ambigüedad genuina: `"informe.2024"`
#' puede ser la tabla `2024` del esquema `informe` o una tabla llamada
#' `informe.2024`. `lupa` elige la primera lectura y lo deja anotado, de modo
#' que si después no encuentra la tabla dice que el nombre se partió. Para el
#' caso ambiguo use el data frame.
#'
#' @param conexion Conexión abierta compatible con DBI.
#' @param tablas Nombres de las tablas que componen la colección: un data frame
#'   con columnas `esquema`, `tabla` y opcionalmente `tipo`; un `DBI::Id` o una
#'   lista de `DBI::Id`; o un vector de texto `"esquema.tabla"`.
#' @param nombre Etiqueta de la colección.
#'
#' @return Objeto de clase `coleccion_lupa`.
#' @export
#' @seealso [perfilar_coleccion()], [perfilar_dbi()], [granularidades()]
#'
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE) &&
#'     requireNamespace("DBI", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(con, "personas", data.frame(id = 1:3, nombre = letters[1:3]))
#'   coleccion(con, "personas", nombre = "padron")
#'   # La forma literal, sin parseo, para nombres con puntos:
#'   coleccion(con, data.frame(esquema = NA, tabla = "personas"))
#'   DBI::dbDisconnect(con)
#' }
coleccion <- function(conexion, tablas, nombre = NULL) {
  .validar_conexion_dbi(conexion, accion = "declarar una coleccion")
  declaradas <- .normalizar_tablas_coleccion(tablas)
  # La unicidad es por la identidad COMPLETA, catalogo incluido. Mirando solo
  # `esquema.tabla`, dos tablas legitimas en catalogos distintos -el caso de
  # `cat1.esq.personas` y `cat2.esq.personas`- se rechazaban como repetidas, que
  # es rechazar una frontera valida. Es el mismo razonamiento por el que el
  # esquema entro en la identidad: lo que distingue dos tablas tiene que
  # distinguirlas tambien al contarlas.
  if (anyDuplicated(declaradas[, c("catalogo", "esquema", "tabla")])) {
    stop("`tablas` repite una tabla.", call. = FALSE)
  }
  if (!is.null(nombre) && !.es_texto_escalar(nombre)) {
    stop("`nombre` debe ser NULL o una cadena no vacia.", call. = FALSE)
  }
  declaradas$identificador <- .identificadores_tabla(
    declaradas$esquema, declaradas$tabla, declaradas$catalogo
  )
  declaradas$referencia <- I(lapply(
    seq_len(nrow(declaradas)),
    function(i) .referencia_de_partes(
      declaradas$esquema[[i]], declaradas$tabla[[i]], declaradas$catalogo[[i]]
    )
  ))
  estructura <- list(
    nombre = if (is.null(nombre)) "coleccion" else nombre,
    conexion = conexion,
    tablas = declaradas,
    motor = tryCatch(
      as.character(class(conexion)[[1L]]), error = function(e) NA_character_
    ),
    n_declaradas = nrow(declaradas)
  )
  class(estructura) <- "coleccion_lupa"
  estructura
}

#' @export
print.coleccion_lupa <- function(x, ...) {
  cli::cli_text("Colecci\u00f3n declarada: {.strong {x$nombre}}")
  cli::cli_text("Motor: {x$motor}")
  cli::cli_text("Tablas declaradas: {x$n_declaradas}")
  esquemas <- unique(x$tablas$esquema[!is.na(x$tablas$esquema)])
  if (length(esquemas)) {
    cli::cli_text("Esquemas: {paste(esquemas, collapse = ', ')}")
  }
  partidas <- sum(x$tablas$declaracion %in% "texto_partido")
  if (partidas) {
    cli::cli_text(
      "Nombres partidos en el punto: {partidas} (declare con data.frame para ",
      "conservarlos literales)."
    )
  }
  cli::cli_text(
    "La frontera es declarada: `lupa` no recorre el cat\u00e1logo."
  )
  invisible(x)
}

.cobertura_coleccion_vacia <- function() {
  data.frame(
    tabla = character(), esquema = character(), tipo = character(),
    alcance = character(), motivo = character(), como_resolverlo = character(),
    stringsAsFactors = FALSE
  )
}

.cobertura_metricas_vacia <- function() {
  data.frame(
    tabla = character(), esquema = character(), identificador = character(),
    columna = character(), metrica = character(), estado = character(),
    motivo = character(), como_resolverlo = character(),
    stringsAsFactors = FALSE
  )
}

.fila_cobertura_coleccion <- function(fila, alcance, motivo, como_resolverlo) {
  data.frame(
    tabla = fila$tabla, esquema = fila$esquema, tipo = fila$tipo,
    alcance = alcance, motivo = motivo, como_resolverlo = como_resolverlo,
    stringsAsFactors = FALSE
  )
}

# El `como_resolverlo` cambia segun POR QUE no se midio. El mensaje generico de
# permisos, servido ante un error de parseo, mandaba al usuario a pedirle al DBA
# acceso a una tabla que ya podia leer.
.como_resolver_tabla_ilegible <- function(fila) {
  if (identical(fila$declaracion, "texto_partido")) {
    return(paste0(
      "Antes de sospechar de los permisos: el nombre se declaro como texto y ",
      "`lupa` lo partio en el punto, tomando '", fila$esquema,
      "' como esquema y '", fila$tabla, "' como tabla. Si el punto pertenece ",
      "al nombre, declarelo con data.frame(esquema = , tabla = ), que no lo ",
      "parsea. Si la particion es correcta, comprobar que la tabla existe y ",
      "que la credencial la puede leer."
    ))
  }
  paste(
    "Comprobar que la tabla existe y que la credencial la puede leer;",
    "los permisos parciales son frecuentes y no se suplen adivinando."
  )
}

.como_resolver_metrica <- function(estado) {
  salidas <- c(
    no_disponible = paste(
      "El motor rechazo la consulta. Comprobar si el agregado existe en este",
      "motor y si la credencial puede ejecutarlo; el SQL exacto queda en",
      "`resumen_tabla$sql` del perfil (`conservar_perfiles = TRUE`)."
    ),
    no_aplica = paste(
      "No corresponde: el tipo con que el driver expone la columna no admite",
      "ese agregado. Si el motor si puede calcularlo, revisar como el driver",
      "mapea el tipo de la columna."
    ),
    sin_valores = paste(
      "La columna no tiene valores no nulos: no hay nada que agregar. No es",
      "un fallo, es un alcance vacio."
    )
  )
  salida <- unname(salidas[estado])
  ifelse(
    is.na(salida),
    paste(
      "Revisar el estado y el motivo del registro por metrica en",
      "`resumen_tabla$sql` del perfil."
    ),
    salida
  )
}

# Resumen por metrica de UNA tabla. Es lo que se sube al nivel coleccion antes
# de descartar el perfil: `n_columnas x n_metricas` filas de estado, acotado,
# frente a las decenas de miles de campos del perfil de la muestra. Sin esto,
# un motor que rechaza un agregado producia un informe INDISTINGUIBLE de uno
# donde todo se midio.
.resumen_metricas_tabla <- function(registro) {
  vacio <- list(
    n = NA_real_, calculadas = NA_real_, no_disponibles = NA_real_,
    no_medidas = NA_real_, detalle = NULL
  )
  if (!is.data.frame(registro) || !nrow(registro) ||
      !all(c("columna", "metrica", "estado") %in% names(registro))) {
    return(vacio)
  }
  estado <- as.character(registro$estado)
  no_calculada <- !(estado %in% "calculado")
  list(
    n = as.numeric(nrow(registro)),
    calculadas = as.numeric(sum(estado %in% "calculado")),
    no_disponibles = as.numeric(sum(estado %in% "no_disponible")),
    no_medidas = as.numeric(sum(no_calculada)),
    detalle = registro[no_calculada, , drop = FALSE]
  )
}

#' Perfilar una colección declarada
#'
#' Recorre las tablas declaradas en [coleccion()] y devuelve **una fila por
#' tabla** con su resumen exacto, más la cobertura de lo que no se pudo medir.
#'
#' Conserva el resumen calculado en SQL sobre toda la tabla, que es acotado, y
#' **no** retiene el perfil de la muestra de cada tabla, que es el objeto
#' pesado: con cientos de tablas eso no entra en memoria. `conservar_perfiles`
#' permite retenerlos cuando la colección es chica y se los necesita.
#'
#' **Lo que no se pudo medir se declara, y nunca a cero.** Una tabla que la
#' credencial no puede leer, un objeto que no es una tabla base y una tabla
#' vacía van a `cobertura_coleccion` con su motivo y su `como_resolverlo`. Un
#' motor que rechaza un agregado —o una columna cuyo tipo no lo admite— va a
#' `cobertura_metricas`, con una fila por columna y métrica, y deja además una
#' línea de resumen en `cobertura_coleccion`. Esa cobertura por métrica **se
#' calcula antes de descartar el perfil**, así que existe también con
#' `conservar_perfiles = FALSE`. En bases institucionales los permisos parciales
#' son el caso normal, no el borde.
#'
#' **Donde no hubo medición va `NA`, no `0`.** Una tabla vacía deja
#' `prop_faltantes` en `NA` en todas sus columnas: antes eso producía
#' `prop_faltantes_maxima = -Inf` —fuera del `[0, 1]` que el paquete promete, y
#' ordenada como la tabla de mejor calidad de la base— y
#' `n_columnas_sin_faltantes = 0`, que afirmaba tres columnas sin ausencias
#' sobre una tabla de la que no se sabía nada. `n_columnas_medidas` declara
#' sobre cuántas columnas se conoce la proporción de ausentes, para que los dos
#' números anteriores se lean con su alcance.
#'
#' **No hay lectura instantánea.** Perfilar una colección son muchas consultas y
#' la base puede cambiar entre ellas: una tabla puede truncarse después de que se
#' contaron sus filas. Por eso cada fila declara el `momento` en que se midió y
#' `meta$snapshot` declara que no lo hubo.
#'
#' @param coleccion Objeto creado por [coleccion()].
#' @param muestra Filas solicitadas por tabla para el bloque en memoria.
#' @param conservar_perfiles Si se retienen los objetos `perfil_dbi` completos.
#'   Por omisión `FALSE`.
#' @param cobertura_metricas Qué se sube a `cobertura_metricas`: `"no_medidas"`
#'   (por omisión) sólo las métricas que no se calcularon, `"completa"` todas
#'   con su estado, `"ninguna"` para omitirla. Los conteos por tabla de
#'   `resumen_coleccion` no dependen de esta elección.
#' @param tope_cobertura_metricas Máximo de filas de `cobertura_metricas`. El
#'   total real queda siempre en `meta$n_metricas_no_medidas`, aunque la tabla
#'   se haya recortado.
#' @param ... Argumentos enviados a [perfilar_dbi()].
#'
#' @return Objeto de clase `perfil_coleccion` con `resumen_coleccion`,
#'   `cobertura_coleccion`, `cobertura_metricas`, `meta` y, si se pidió,
#'   `perfiles`.
#' @export
#' @seealso [coleccion()], [perfilar_dbi()]
#'
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE) &&
#'     requireNamespace("DBI", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(con, "personas", data.frame(id = 1:3, nombre = letters[1:3]))
#'   perfilar_coleccion(coleccion(con, "personas", nombre = "padron"))
#'   DBI::dbDisconnect(con)
#' }
perfilar_coleccion <- function(coleccion, muestra = 1000L,
                               conservar_perfiles = FALSE,
                               cobertura_metricas = c("no_medidas", "completa",
                                                      "ninguna"),
                               tope_cobertura_metricas = 20000L, ...) {
  if (!inherits(coleccion, "coleccion_lupa")) {
    stop(
      "`coleccion` debe ser una coleccion declarada por coleccion().",
      call. = FALSE
    )
  }
  if (!is.logical(conservar_perfiles) || length(conservar_perfiles) != 1L ||
      is.na(conservar_perfiles)) {
    stop("`conservar_perfiles` debe ser TRUE o FALSE.", call. = FALSE)
  }
  cobertura_metricas <- match.arg(cobertura_metricas)
  if (!is.numeric(tope_cobertura_metricas) ||
      length(tope_cobertura_metricas) != 1L ||
      is.na(tope_cobertura_metricas) || tope_cobertura_metricas < 0) {
    stop(
      "`tope_cobertura_metricas` debe ser un numero no negativo.", call. = FALSE
    )
  }
  tope_cobertura_metricas <- as.numeric(tope_cobertura_metricas)
  conexion <- coleccion$conexion
  .validar_conexion_dbi(conexion, accion = "perfilar una coleccion")
  declaradas <- coleccion$tablas
  inicio <- Sys.time()
  cobertura <- list()
  metricas <- list()
  resumenes <- list()
  perfiles <- list()
  n_metricas_no_medidas <- 0
  filas_metricas <- 0
  metricas_truncadas <- FALSE

  for (i in seq_len(nrow(declaradas))) {
    fila <- declaradas[i, , drop = FALSE]
    identificador <- fila$identificador
    if (!identical(fila$tipo, "tabla")) {
      cobertura[[length(cobertura) + 1L]] <- .fila_cobertura_coleccion(
        fila, "tabla",
        paste0(
          "El objeto se declaro como '", fila$tipo,
          "' y no como tabla base; no se perfilo."
        ),
        paste(
          "Declarar el objeto con tipo 'tabla' si corresponde perfilarlo, o",
          "perfilar la tabla base que lo respalda."
        )
      )
      next
    }
    momento <- Sys.time()
    referencia <- declaradas$referencia[[i]]
    perfil <- tryCatch(
      perfilar_dbi(conexion, referencia, muestra = muestra, ...),
      error = function(e) e
    )
    if (inherits(perfil, "condition")) {
      cobertura[[length(cobertura) + 1L]] <- .fila_cobertura_coleccion(
        fila, "tabla",
        paste0("No se pudo perfilar la tabla: ", conditionMessage(perfil)),
        .como_resolver_tabla_ilegible(fila)
      )
      next
    }
    # Desestructurar tambien va dentro del `tryCatch`. Si `perfilar_dbi()` no
    # falla pero devuelve algo con otra forma -una version distinta del paquete,
    # un objeto a medio construir-, el error al leer sus piezas no estaba
    # capturado y **rompia el bucle entero**: se perdian todas las tablas ya
    # perfiladas y el usuario no recibia ni el resumen ni la cobertura. Un
    # silencio total es el peor resultado posible para este paquete.
    piezas <- tryCatch({
      resumen <- perfil$resumen_tabla
      if (is.null(resumen) || is.null(resumen$columnas)) {
        stop("el perfil no trae `resumen_tabla$columnas`", call. = FALSE)
      }
      list(
        filas = resumen$meta$filas,
        analizadas = perfil$perfil_muestra$meta$filas_analizadas,
        prop_faltantes = resumen$columnas$prop_faltantes,
        n_columnas = nrow(resumen$columnas),
        # Se lee ANTES de descartar el perfil: es la unica oportunidad.
        sql = resumen$sql
      )
    }, error = function(e) e)
    if (inherits(piezas, "condition")) {
      cobertura[[length(cobertura) + 1L]] <- .fila_cobertura_coleccion(
        fila, "tabla",
        paste0(
          "La tabla se perfilo pero su resultado no tiene la forma esperada: ",
          conditionMessage(piezas)
        ),
        paste(
          "Comprobar que la version instalada de lupa y la que produjo el",
          "perfil sean la misma."
        )
      )
      next
    }
    filas_tabla <- piezas$filas
    analizadas <- piezas$analizadas
    prop_faltantes <- piezas$prop_faltantes
    n_filas_tabla <- as.numeric(
      if (length(filas_tabla)) filas_tabla[[1L]] else NA_real_
    )

    # Nunca `na.rm = TRUE` sobre un conjunto que puede ser entero de `NA`:
    # `max()` devuelve `-Inf` y `sum()` devuelve `0`, y las dos cosas son
    # afirmaciones sobre algo que no se midio.
    conocidas <- prop_faltantes[!is.na(prop_faltantes)]
    n_columnas_medidas <- as.numeric(length(conocidas))
    prop_maxima <- if (length(conocidas)) max(conocidas) else NA_real_
    sin_faltantes <- if (length(conocidas)) {
      as.numeric(sum(conocidas %in% 0))
    } else NA_real_

    estado_metricas <- .resumen_metricas_tabla(piezas$sql)
    if (!is.na(estado_metricas$no_medidas)) {
      n_metricas_no_medidas <- n_metricas_no_medidas + estado_metricas$no_medidas
    }

    resumenes[[length(resumenes) + 1L]] <- data.frame(
      tabla = fila$tabla,
      esquema = fila$esquema,
      identificador = identificador,
      n_columnas = as.numeric(piezas$n_columnas),
      n_filas = n_filas_tabla,
      # Agregados exactos sobre toda la tabla, calculados en SQL: se conservan
      # porque son acotados. El perfil de la muestra, que es el objeto pesado,
      # no se retiene salvo que se pida.
      prop_faltantes_maxima = prop_maxima,
      n_columnas_sin_faltantes = sin_faltantes,
      n_columnas_medidas = n_columnas_medidas,
      n_metricas_declaradas = estado_metricas$n,
      n_metricas_calculadas = estado_metricas$calculadas,
      n_metricas_no_disponibles = estado_metricas$no_disponibles,
      muestra_solicitada = as.numeric(muestra),
      muestra_analizada = as.numeric(
        if (length(analizadas)) analizadas[[1L]] else NA_real_
      ),
      momento = momento,
      stringsAsFactors = FALSE
    )

    # Una tabla vacia no es un caso exotico: es rutina institucional -tabla
    # recien creada, staging truncada, particion del mes que viene-. Antes se
    # informaba como exito con `-Inf` y ceros; ahora se declara.
    if (!is.na(n_filas_tabla) && n_filas_tabla == 0) {
      cobertura[[length(cobertura) + 1L]] <- .fila_cobertura_coleccion(
        fila, "tabla_vacia",
        paste(
          "La tabla tiene cero filas: se leyo su estructura pero no hay nada",
          "que medir. Las proporciones quedan en NA, no en cero."
        ),
        paste(
          "Comprobar si la tabla deberia tener datos -staging truncada,",
          "particion futura, carga pendiente-. Si esta vacia por diseno, no",
          "hay nada que corregir."
        )
      )
    } else if (n_columnas_medidas == 0 && piezas$n_columnas > 0) {
      cobertura[[length(cobertura) + 1L]] <- .fila_cobertura_coleccion(
        fila, "medicion_incompleta",
        paste0(
          "La tabla se perfilo pero no se pudo medir la proporcion de ",
          "ausentes en ninguna de sus ", piezas$n_columnas, " columnas."
        ),
        paste(
          "Revisar `cobertura_metricas` para ver que rechazo el motor en cada",
          "columna."
        )
      )
    }

    if (!is.na(estado_metricas$no_disponibles) &&
        estado_metricas$no_disponibles > 0) {
      detalle <- estado_metricas$detalle
      rechazadas <- detalle[detalle$estado %in% "no_disponible", , drop = FALSE]
      ejemplos <- utils::head(
        paste0(rechazadas$columna, "/", rechazadas$metrica), 3L
      )
      cobertura[[length(cobertura) + 1L]] <- .fila_cobertura_coleccion(
        fila, "metricas",
        paste0(
          "El motor rechazo ", estado_metricas$no_disponibles, " de ",
          estado_metricas$n, " agregados de esta tabla (por ejemplo ",
          paste(ejemplos, collapse = ", "),
          "). La tabla se perfilo, pero no completa."
        ),
        paste(
          "El detalle por columna y metrica esta en `cobertura_metricas`, con",
          "el motivo que devolvio el motor."
        )
      )
    }

    if (!identical(cobertura_metricas, "ninguna") &&
        is.data.frame(piezas$sql) && nrow(piezas$sql)) {
      seleccion <- if (identical(cobertura_metricas, "completa")) {
        piezas$sql
      } else {
        estado_metricas$detalle
      }
      if (!is.null(seleccion) && nrow(seleccion)) {
        disponibles <- max(0, tope_cobertura_metricas - filas_metricas)
        if (nrow(seleccion) > disponibles) {
          seleccion <- seleccion[seq_len(disponibles), , drop = FALSE]
          metricas_truncadas <- TRUE
        }
      }
      if (!is.null(seleccion) && nrow(seleccion)) {
        filas_metricas <- filas_metricas + nrow(seleccion)
        metricas[[length(metricas) + 1L]] <- data.frame(
          tabla = rep(fila$tabla, nrow(seleccion)),
          esquema = rep(fila$esquema, nrow(seleccion)),
          identificador = rep(identificador, nrow(seleccion)),
          columna = as.character(seleccion$columna),
          metrica = as.character(seleccion$metrica),
          estado = as.character(seleccion$estado),
          motivo = as.character(seleccion$motivo),
          como_resolverlo = .como_resolver_metrica(as.character(seleccion$estado)),
          stringsAsFactors = FALSE
        )
      }
    }

    if (conservar_perfiles) perfiles[[identificador]] <- perfil
  }

  resumen_coleccion <- if (length(resumenes)) {
    do.call(rbind, resumenes)
  } else {
    data.frame(
      tabla = character(), esquema = character(), identificador = character(),
      n_columnas = numeric(), n_filas = numeric(),
      prop_faltantes_maxima = numeric(), n_columnas_sin_faltantes = numeric(),
      n_columnas_medidas = numeric(), n_metricas_declaradas = numeric(),
      n_metricas_calculadas = numeric(), n_metricas_no_disponibles = numeric(),
      muestra_solicitada = numeric(), muestra_analizada = numeric(),
      momento = as.POSIXct(character()), stringsAsFactors = FALSE
    )
  }
  cobertura_coleccion <- if (length(cobertura)) {
    do.call(rbind, cobertura)
  } else {
    .cobertura_coleccion_vacia()
  }
  tabla_metricas <- if (length(metricas)) {
    do.call(rbind, metricas)
  } else {
    .cobertura_metricas_vacia()
  }
  rownames(resumen_coleccion) <- NULL
  rownames(cobertura_coleccion) <- NULL
  rownames(tabla_metricas) <- NULL

  # `n_sin_perfilar` cuenta tablas que no se perfilaron. Las filas de alcance
  # `tabla_vacia`, `medicion_incompleta` y `metricas` describen tablas que SI
  # estan en `resumen_coleccion`: sumarlas romperia la identidad
  # `n_declaradas = n_perfiladas + n_sin_perfilar`.
  alcance <- cobertura_coleccion$alcance
  estructura <- list(
    resumen_coleccion = resumen_coleccion,
    cobertura_coleccion = cobertura_coleccion,
    cobertura_metricas = tabla_metricas,
    meta = list(
      nombre = coleccion$nombre,
      motor = coleccion$motor,
      n_declaradas = nrow(declaradas),
      n_perfiladas = nrow(resumen_coleccion),
      n_sin_perfilar = sum(alcance %in% "tabla"),
      n_vacias = sum(alcance %in% "tabla_vacia"),
      n_con_metricas_rechazadas = sum(alcance %in% "metricas"),
      n_metricas_no_medidas = n_metricas_no_medidas,
      cobertura_metricas_modo = cobertura_metricas,
      cobertura_metricas_truncada = metricas_truncadas,
      tope_cobertura_metricas = tope_cobertura_metricas,
      inicio = inicio,
      fin = Sys.time(),
      snapshot = FALSE,
      nota_snapshot = paste(
        "No hubo lectura instantanea: cada tabla se midio en su propio",
        "momento y la base pudo cambiar entre consultas. La columna `momento`",
        "de `resumen_coleccion` declara cuando se midio cada una."
      ),
      nota_cobertura = paste(
        "Donde no hubo medicion va NA, nunca 0 ni -Inf.",
        "`n_columnas_medidas` declara sobre cuantas columnas se conoce la",
        "proporcion de ausentes, y `cobertura_metricas` que agregado no se",
        "pudo calcular en cada columna."
      ),
      frontera = "declarada por el usuario"
    )
  )
  if (conservar_perfiles) estructura$perfiles <- perfiles
  class(estructura) <- "perfil_coleccion"
  estructura
}

#' @export
print.perfil_coleccion <- function(x, ...) {
  cli::cli_text("Perfil de colecci\u00f3n: {.strong {x$meta$nombre}}")
  cli::cli_text(
    "Tablas: {x$meta$n_perfiladas} perfiladas de {x$meta$n_declaradas} declaradas"
  )
  if (x$meta$n_sin_perfilar) {
    cli::cli_text(
      "Sin perfilar: {x$meta$n_sin_perfilar} (ver `cobertura_coleccion`)"
    )
  }
  if (isTRUE(x$meta$n_vacias > 0)) {
    cli::cli_text(
      "Vac\u00edas: {x$meta$n_vacias} (cero filas: nada que medir, no cero faltantes)"
    )
  }
  if (isTRUE(x$meta$n_con_metricas_rechazadas > 0)) {
    cli::cli_text(
      "Con agregados rechazados por el motor: {x$meta$n_con_metricas_rechazadas} (ver `cobertura_metricas`)"
    )
  }
  cli::cli_text("Sin lectura instant\u00e1nea: cada tabla trae su `momento`.")
  invisible(x)
}

# Las relaciones entre tablas son el punto donde el costo explota. Con 1.730
# tablas hay 1.495.585 pares no dirigidos, y una clave foranea **es dirigida**,
# asi que son casi tres millones de direcciones, mas las autorreferenciales. Y
# el costo real no depende del numero de tablas sino de las combinaciones de
# columnas entre ellas.
#
# Por eso los pares se declaran, igual que la frontera. Explorar todos no es una
# opcion cara: es una opcion imposible.
#
# Y estimar el costo no puede costar mas que medirlo. Materializar los pares y
# recorrerlos uno a uno pagaba 22 segundos y 245 MB con 1700 tablas para
# devolver cuatro numeros. El mismo numero sale por formula cerrada:
# con `c_i` columnas por tabla, la suma de `c_i * c_j` sobre los pares dirigidos
# `i != j` es `(sum c_i)^2 - sum c_i^2`.

# Lee el ancho de cada tabla por metadatos. `dbListFields()` es la via
# declarada por DBI y no ejecuta una consulta contra los datos; el
# `SELECT * WHERE 1 = 0` queda como reserva para los drivers que no la
# implementan. Se consultan SOLO las tablas que hacen falta.
.columnas_por_tabla_coleccion <- function(coleccion, identificadores) {
  indices <- match(identificadores, coleccion$tablas$identificador)
  anchos <- vapply(seq_along(indices), function(j) {
    k <- indices[[j]]
    if (is.na(k)) return(NA_real_)
    referencia <- coleccion$tablas$referencia[[k]]
    campos <- tryCatch(
      DBI::dbListFields(coleccion$conexion, referencia),
      error = function(e) NULL
    )
    if (!is.null(campos) && length(campos)) return(as.numeric(length(campos)))
    tryCatch({
      esquema <- DBI::dbGetQuery(
        coleccion$conexion,
        paste0(
          "SELECT * FROM ",
          as.character(DBI::dbQuoteIdentifier(coleccion$conexion, referencia)),
          " WHERE 1 = 0"
        )
      )
      as.numeric(ncol(esquema))
    }, error = function(e) NA_real_)
  }, numeric(1L))
  names(anchos) <- identificadores
  anchos
}

#' Estimar el costo de buscar relaciones en una colección
#'
#' Cuenta cuántas comparaciones de columnas implicaría buscar claves foráneas
#' entre los pares indicados, **antes** de hacerlas. El número que importa no es
#' la cantidad de tablas sino la de pares de columnas: dos tablas de cincuenta
#' columnas son dos mil quinientas comparaciones.
#'
#' Sin `pares` estima sobre todos los pares dirigidos de la colección, que es
#' justamente lo que suele mostrar por qué hay que declararlos. Ese caso **no
#' materializa los pares**: la suma de `c_i * c_j` sobre los pares dirigidos es
#' `sum(c)^2 - sum(c^2)`, así que el trabajo es lineal en el número de
#' tablas y no cuadrático. Estimar el costo no puede costar más que medirlo.
#'
#' Sólo se consulta el ancho de las tablas que los pares nombran, por metadatos
#' (`DBI::dbListFields()`), no leyendo datos.
#'
#' **Una tabla cuyo esquema no se puede leer no se suma como cero.** Queda
#' declarada en `tablas_sin_esquema` y los pares que la involucran se cuentan en
#' `n_pares_sin_estimar`: el número devuelto cubre `n_pares_estimados`, no todos
#' los pares.
#'
#' @param coleccion Objeto creado por [coleccion()].
#' @param pares Data frame con columnas `tabla_1` y `tabla_2`. Un data frame
#'   con cero filas es una declaración válida de «ningún par». Sin `pares`
#'   —`NULL`—, todos los pares dirigidos.
#' @param columnas_candidatas Lista nombrada por identificador de tabla. Cada
#'   entrada declara las columnas que pueden participar de una relación; las
#'   tablas no nombradas conservan todas sus columnas. La declaración reduce
#'   tanto la lectura como el número de comparaciones.
#'
#' @return Lista con `n_tablas`, `n_pares_dirigidos`, `n_pares_declarados`,
#'   `n_pares_estimados`, `n_pares_sin_estimar`, `n_comparaciones_columnas`,
#'   `n_tablas_sin_esquema`, `tablas_sin_esquema` y `alcance`. También publica
#'   `columnas_candidatas` y el alcance real de las comparaciones.
#' @export
#' @seealso [relaciones_coleccion()], [coleccion()]
#'
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE) &&
#'     requireNamespace("DBI", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(con, "personas", data.frame(id = 1:3, nombre = letters[1:3]))
#'   DBI::dbWriteTable(con, "visitas", data.frame(persona_id = 1:3, dia = 1:3))
#'   estimar_costo_coleccion(
#'     coleccion(con, c("personas", "visitas")),
#'     columnas_candidatas = list(personas = "id", visitas = "persona_id")
#'   )
#'   DBI::dbDisconnect(con)
#' }
estimar_costo_coleccion <- function(coleccion, pares = NULL,
                                    columnas_candidatas = NULL) {
  if (!inherits(coleccion, "coleccion_lupa")) {
    stop("`coleccion` debe venir de coleccion().", call. = FALSE)
  }
  columnas_candidatas <- .validar_columnas_candidatas_coleccion(
    coleccion, columnas_candidatas
  )
  n <- coleccion$n_declaradas
  todos <- is.null(pares)
  # `n * (n - 1L)` con `n` entero desborda a partir de 46.342 tablas y devuelve
  # `NA` con un warning. El catalogo de un data lake lo alcanza.
  n_pares_dirigidos <- as.numeric(n) * (as.numeric(n) - 1)

  if (todos) {
    identificadores <- coleccion$tablas$identificador
    anchos <- .columnas_por_tabla_coleccion(coleccion, identificadores)
    if (!is.null(columnas_candidatas)) {
      anchos <- vapply(names(anchos), function(identificador) {
        candidatas <- columnas_candidatas[[identificador]]
        if (is.na(anchos[[identificador]]) || is.null(candidatas)) {
          anchos[[identificador]]
        } else {
          as.numeric(length(candidatas))
        }
      }, numeric(1L))
      names(anchos) <- identificadores
    }
    conocidos <- anchos[!is.na(anchos)]
    k <- length(conocidos)
    n_pares_estimados <- as.numeric(k) * (as.numeric(k) - 1)
    comparaciones <- if (k >= 2L) {
      sum(conocidos)^2 - sum(conocidos^2)
    } else if (n < 2L) {
      # Menos de dos tablas son cero pares dirigidos: eso si se midio.
      0
    } else {
      NA_real_
    }
    sin_esquema <- names(anchos)[is.na(anchos)]
    n_pares_declarados <- NA_real_
    n_pares_sin_estimar <- n_pares_dirigidos - n_pares_estimados
    alcance <- "todos los pares dirigidos de la coleccion"
  } else {
    pares <- .validar_pares_coleccion(coleccion, pares, exigir = FALSE)
    identificadores <- unique(c(pares$tabla_1, pares$tabla_2))
    anchos <- if (length(identificadores)) {
      .columnas_por_tabla_coleccion(coleccion, identificadores)
    } else {
      stats::setNames(numeric(), character())
    }
    if (!is.null(columnas_candidatas) && length(anchos)) {
      anchos <- vapply(names(anchos), function(identificador) {
        candidatas <- columnas_candidatas[[identificador]]
        if (is.na(anchos[[identificador]]) || is.null(candidatas)) {
          anchos[[identificador]]
        } else {
          as.numeric(length(candidatas))
        }
      }, numeric(1L))
      names(anchos) <- identificadores
    }
    a <- anchos[match(pares$tabla_1, names(anchos))]
    b <- anchos[match(pares$tabla_2, names(anchos))]
    producto <- as.numeric(a) * as.numeric(b)
    estimables <- !is.na(producto)
    n_pares_estimados <- as.numeric(sum(estimables))
    # Cero pares declarados son cero comparaciones -eso si se midio-. Pares
    # declarados de los que no se pudo leer ni uno son `NA`, no cero.
    comparaciones <- if (!nrow(pares)) {
      0
    } else if (any(estimables)) {
      sum(producto[estimables])
    } else {
      NA_real_
    }
    sin_esquema <- names(anchos)[is.na(anchos)]
    n_pares_declarados <- as.numeric(nrow(pares))
    n_pares_sin_estimar <- as.numeric(sum(!estimables))
    alcance <- "los pares declarados"
  }

  list(
    n_tablas = n,
    n_pares_dirigidos = n_pares_dirigidos,
    n_pares_declarados = n_pares_declarados,
    n_pares_estimados = n_pares_estimados,
    n_pares_sin_estimar = n_pares_sin_estimar,
    n_comparaciones_columnas = comparaciones,
    columnas_candidatas = columnas_candidatas,
    n_tablas_sin_esquema = as.numeric(length(sin_esquema)),
    tablas_sin_esquema = sin_esquema,
    alcance = alcance,
    nota = paste(
      "Una clave foranea es dirigida, asi que los pares son n*(n-1) y no",
      "n*(n-1)/2. El costo real lo dan las comparaciones de columnas, no el",
      "numero de tablas. Las tablas sin esquema legible no se suman como cero:",
      "quedan en `tablas_sin_esquema` y sus pares en `n_pares_sin_estimar`."
    )
  )
}

# `pares` con cero filas significaba "error" y el mensaje decia que faltaban las
# columnas `tabla_1` y `tabla_2` cuando el data.frame SI las traia: mentia sobre
# la causa. Cero pares es una declaracion legitima -"no compares nada"- y ahora
# se acepta. `NULL` sigue significando "todos".
.validar_pares_coleccion <- function(coleccion, pares, exigir = TRUE) {
  declaradas <- coleccion$tablas$identificador
  if (is.null(pares)) {
    if (exigir) {
      stop(
        "`pares` debe declarar que tablas se comparan. Explorar todos los ",
        "pares no es viable: una coleccion de mil tablas tiene casi un millon ",
        "de pares dirigidos. Use estimar_costo_coleccion() para verlo.",
        call. = FALSE
      )
    }
    if (length(declaradas) < 2L) {
      return(.pares_vacios_coleccion())
    }
    combinaciones <- expand.grid(
      tabla_1 = declaradas, tabla_2 = declaradas,
      stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE
    )
    salida <- combinaciones[combinaciones$tabla_1 != combinaciones$tabla_2, ]
    rownames(salida) <- NULL
    attr(salida, "repetidos_descartados") <- 0
    return(salida)
  }
  if (!inherits(pares, "data.frame") ||
      !all(c("tabla_1", "tabla_2") %in% names(pares))) {
    stop(
      "`pares` debe ser un data.frame con columnas `tabla_1` y `tabla_2`.",
      call. = FALSE
    )
  }
  if (!nrow(pares)) return(.pares_vacios_coleccion())
  tabla_1 <- as.character(pares$tabla_1)
  tabla_2 <- as.character(pares$tabla_2)
  if (anyNA(tabla_1) || anyNA(tabla_2) ||
      !all(nzchar(tabla_1)) || !all(nzchar(tabla_2))) {
    stop(
      "`pares` no admite `NA` ni cadenas vacias en `tabla_1` ni en `tabla_2`.",
      call. = FALSE
    )
  }
  desconocidas <- setdiff(c(tabla_1, tabla_2), declaradas)
  if (length(desconocidas)) {
    stop(
      "`pares` nombra tablas que no estan declaradas en la coleccion: ",
      paste(unique(desconocidas), collapse = ", "),
      ". Declaradas: ", paste(declaradas, collapse = ", "), ".",
      call. = FALSE
    )
  }
  # Un par de una tabla consigo misma produce una "relacion" trivial de cada
  # columna con ella misma y una relectura identica. No se acepta en silencio.
  autorreferenciales <- tabla_1 == tabla_2
  if (any(autorreferenciales)) {
    stop(
      "`pares` declara pares de una tabla consigo misma: ",
      paste(unique(tabla_1[autorreferenciales]), collapse = ", "),
      ". Comparar una tabla contra si misma devuelve cada columna emparejada ",
      "con ella misma y vuelve a leer la tabla dos veces; las claves foraneas ",
      "autorreferenciales no se detectan por este camino.",
      call. = FALSE
    )
  }
  clave <- paste(tabla_1, tabla_2, sep = "\r")
  unicos <- !duplicated(clave)
  salida <- data.frame(
    tabla_1 = tabla_1[unicos], tabla_2 = tabla_2[unicos],
    stringsAsFactors = FALSE
  )
  attr(salida, "repetidos_descartados") <- sum(!unicos)
  salida
}

.pares_vacios_coleccion <- function() {
  salida <- data.frame(
    tabla_1 = character(), tabla_2 = character(), stringsAsFactors = FALSE
  )
  attr(salida, "repetidos_descartados") <- 0
  salida
}

.relaciones_coleccion_vacias <- function() {
  data.frame(
    tabla_1 = character(), tabla_2 = character(),
    columna_tabla1 = character(), columna_tabla2 = character(),
    cardinalidad = character(), n_valores_comunes = numeric(),
    cobertura_tabla1_en_tabla2 = numeric(),
    cobertura_tabla2_en_tabla1 = numeric(),
    filas_leidas_1 = numeric(), filas_leidas_2 = numeric(),
    momento = as.POSIXct(character()), stringsAsFactors = FALSE
  )
}

.cobertura_podas_vacia <- function() {
  data.frame(
    tabla_1 = character(), tabla_2 = character(),
    columna_tabla1 = character(), columna_tabla2 = character(),
    motivo = character(), detalle = character(), stringsAsFactors = FALSE
  )
}

.validar_columnas_candidatas_coleccion <- function(coleccion, columnas) {
  if (is.null(columnas)) return(NULL)
  if (!is.list(columnas) || is.null(names(columnas)) ||
      anyNA(names(columnas)) || any(!nzchar(names(columnas))) ||
      anyDuplicated(names(columnas))) {
    stop(
      "`columnas_candidatas` debe ser una lista nombrada por identificador de tabla.",
      call. = FALSE
    )
  }
  declaradas <- coleccion$tablas$identificador
  desconocidas <- setdiff(names(columnas), declaradas)
  if (length(desconocidas)) {
    stop(
      "`columnas_candidatas` nombra tablas no declaradas en la coleccion: ",
      paste(desconocidas, collapse = ", "), ".", call. = FALSE
    )
  }
  for (identificador in names(columnas)) {
    candidatas <- columnas[[identificador]]
    if (!is.character(candidatas) || !length(candidatas) ||
        anyNA(candidatas) || any(!nzchar(candidatas))) {
      stop(
        "`columnas_candidatas` para `", identificador,
        "` debe ser un vector de nombres no vacio.", call. = FALSE
      )
    }
    k <- match(identificador, declaradas)
    campos <- tryCatch(
      DBI::dbListFields(coleccion$conexion, coleccion$tablas$referencia[[k]]),
      error = function(e) NULL
    )
    if (!is.null(campos)) {
      desconocidas <- setdiff(candidatas, campos)
      if (length(desconocidas)) {
        stop(
          "`columnas_candidatas` nombra columnas inexistentes en `",
          identificador, "`: ", paste(desconocidas, collapse = ", "), ".",
          call. = FALSE
        )
      }
    }
    columnas[[identificador]] <- unique(candidatas)
  }
  columnas
}

# Lector con cache. `leer()` se llamaba DOS VECES POR PAR, sin cache: dos pares
# que comparten una tabla la leian dos veces, y con veinte pares sobre cinco
# tablas se pagaban cuarenta lecturas para cinco tablas distintas.
#
# El `LIMIT` literal tampoco es portable: sobre un motor que no lo acepta
# **todos** los pares caian a `sin_comparar` -honesto, e inutil-. Ahora el
# `LIMIT` es el primer intento y `dbSendQuery()` + `dbFetch(n = )`, que es
# DBI puro y acota la lectura en el cliente, es la reserva.
.lector_tablas_coleccion <- function(conexion, coleccion, muestra, orden,
                                     tope_cache_mb, columnas_candidatas = NULL) {
  cache <- new.env(parent = emptyenv())
  bitacora <- list()
  usados_mb <- 0
  lecturas <- 0
  reutilizaciones <- 0
  cache_completa <- TRUE

  referencia_de <- function(identificador) {
    k <- match(identificador, coleccion$tablas$identificador)
    if (is.na(k)) identificador else coleccion$tablas$referencia[[k]]
  }
  orden_de <- function(identificador) {
    if (is.null(orden)) return(character())
    columnas <- if (is.character(orden)) orden else orden[[identificador]]
    if (is.null(columnas)) character() else as.character(columnas)
  }

  candidatas_de <- function(identificador) {
    if (is.null(columnas_candidatas)) return(NULL)
    columnas <- columnas_candidatas[[identificador]]
    if (is.null(columnas)) NULL else as.character(columnas)
  }

  leer <- function(identificador) {
    if (exists(identificador, envir = cache, inherits = FALSE)) {
      reutilizaciones <<- reutilizaciones + 1L
      return(get(identificador, envir = cache, inherits = FALSE))
    }
    referencia <- referencia_de(identificador)
    tabla_sql <- tryCatch(
      as.character(DBI::dbQuoteIdentifier(conexion, referencia)),
      error = function(e) NA_character_
    )
    columnas_orden <- orden_de(identificador)
    columnas_seleccionadas <- candidatas_de(identificador)
    columnas_lectura <- unique(c(columnas_seleccionadas, columnas_orden))
    seleccion_sql <- if (length(columnas_lectura)) {
      paste(
        as.character(DBI::dbQuoteIdentifier(conexion, columnas_lectura)),
        collapse = ", "
      )
    } else "*"
    orden_sql <- if (length(columnas_orden)) {
      paste0(
        " ORDER BY ",
        paste(as.character(DBI::dbQuoteIdentifier(conexion, columnas_orden)),
              collapse = ", ")
      )
    } else ""
    momento <- Sys.time()
    sql_limite <- paste0(
      "SELECT ", seleccion_sql, " FROM ", tabla_sql, orden_sql,
      " LIMIT ", format(muestra, scientific = FALSE)
    )
    resultado <- tryCatch(
      list(datos = DBI::dbGetQuery(conexion, sql_limite), sql = sql_limite,
           via = "limit"),
      error = function(e) e
    )
    motivo_limite <- NA_character_
    if (inherits(resultado, "condition")) {
      motivo_limite <- conditionMessage(resultado)
       sql_llano <- paste0(
         "SELECT ", seleccion_sql, " FROM ", tabla_sql, orden_sql
       )
      resultado <- tryCatch({
        consulta <- DBI::dbSendQuery(conexion, sql_llano)
        on.exit(DBI::dbClearResult(consulta), add = TRUE)
        list(datos = DBI::dbFetch(consulta, n = muestra), sql = sql_llano,
             via = "fetch_acotado")
      }, error = function(e) e)
    }
    lecturas <<- lecturas + 1L
    if (inherits(resultado, "condition")) {
      # Fallaron las dos vias: se declaran las dos, y se nombra el `ORDER BY`
      # como causa posible cuando se declaro uno. Un motivo que solo cuenta el
      # segundo intento esconde por que se llego hasta ahi.
      motivo <- paste0(
        "con LIMIT: ", motivo_limite, "; sin LIMIT: ",
        conditionMessage(resultado),
        if (length(columnas_orden)) {
          paste0(
            ". Se declaro `orden` (", paste(columnas_orden, collapse = ", "),
            "): comprobar que esas columnas existen en la tabla."
          )
        } else ""
      )
      bitacora[[length(bitacora) + 1L]] <<- data.frame(
        tabla = identificador, sql = NA_character_, via = NA_character_,
        filas_leidas = NA_real_, orden_declarado = length(columnas_orden) > 0L,
        momento = momento, ok = FALSE,
        motivo = motivo, stringsAsFactors = FALSE
      )
      return(simpleError(motivo))
    }
    bitacora[[length(bitacora) + 1L]] <<- data.frame(
      tabla = identificador, sql = resultado$sql, via = resultado$via,
      filas_leidas = as.numeric(nrow(resultado$datos)),
      orden_declarado = length(columnas_orden) > 0L,
      momento = momento, ok = TRUE, motivo = NA_character_,
      stringsAsFactors = FALSE
    )
    tamano_mb <- as.numeric(utils::object.size(resultado$datos)) / 1024^2
    if (usados_mb + tamano_mb <= tope_cache_mb) {
      assign(identificador, resultado$datos, envir = cache)
      usados_mb <<- usados_mb + tamano_mb
    } else {
      cache_completa <<- FALSE
    }
    resultado$datos
  }

  list(
    leer = leer,
    bitacora = function() {
      if (length(bitacora)) {
        salida <- do.call(rbind, bitacora)
        rownames(salida) <- NULL
        salida
      } else {
        data.frame(
          tabla = character(), sql = character(), via = character(),
          filas_leidas = numeric(), orden_declarado = logical(),
          momento = as.POSIXct(character()), ok = logical(),
          motivo = character(), stringsAsFactors = FALSE
        )
      }
    },
    estado = function() {
      list(
        lecturas = lecturas, reutilizaciones = reutilizaciones,
        tablas_en_cache = length(ls(cache)), memoria_cache_mb = usados_mb,
        cache_completa = cache_completa
      )
    }
  )
}

#' Buscar claves foráneas candidatas entre pares declarados
#'
#' Corre [detectar_relaciones()] sobre los pares de tablas que se declaren, y
#' devuelve las relaciones candidatas junto con la cobertura de los pares que no
#' se pudieron comparar.
#'
#' **Los pares se declaran, igual que la frontera.** Con mil tablas hay casi un
#' millón de pares dirigidos, así que explorar todos no es una opción cara sino
#' una opción imposible. [estimar_costo_coleccion()] permite verlo antes.
#'
#' **Cada tabla se lee una sola vez.** Los pares repetidos se descartan, los
#' pares de una tabla consigo misma se rechazan, y las lecturas se guardan en
#' una caché con presupuesto de memoria declarado: dos pares que comparten una
#' tabla ya no la leen dos veces. `meta$lecturas` deja el SQL exacto, la vía
#' usada, las filas leídas y el momento de cada lectura.
#'
#' **El orden no se supone.** Sin `orden`, una lectura acotada devuelve un
#' subconjunto arbitrario y el resultado declara `meta$estable = FALSE`. Declarar
#' columnas de orden hace la lectura repetible.
#'
#' Cada par se compara sobre una muestra de filas de cada tabla, y el resultado
#' declara ese alcance: una relación candidata sobre una muestra **no es una
#' clave foránea comprobada**, es un indicio que hay que confirmar contra el
#' diccionario de datos.
#'
#' Las columnas candidatas se podan antes de materializar cada comparación. Las
#' podas quedan declaradas en `cobertura_podas`, con su motivo y conteo.
#'
#' @param coleccion Objeto creado por [coleccion()].
#' @param pares Data frame con columnas `tabla_1` y `tabla_2`. Cero filas es una
#'   declaración válida de «ningún par».
#' @param columnas_candidatas Lista nombrada por identificador de tabla. Cada
#'   vector declara las columnas que pueden participar; una tabla no nombrada
#'   conserva todas sus columnas.
#' @param muestra Filas traídas por tabla para comparar.
#' @param umbral_cobertura Cobertura mínima para informar una relación.
#' @param orden Columnas para `ORDER BY`: un vector de texto que se aplica a
#'   todas las tablas, o una lista nombrada por identificador de tabla. Sin él
#'   la lectura no es repetible y el objeto lo declara.
#' @param tope_cache_mb Presupuesto de memoria para la caché de lecturas, en
#'   megabytes. Al agotarse se sigue leyendo sin cachear y `meta` lo declara.
#' @param tope_memoria_mb Presupuesto de memoria para resultados de relaciones,
#'   en megabytes. Al agotarse, las combinaciones pendientes se declaran en
#'   `cobertura_podas` y los pares pendientes en `cobertura_pares`.
#'
#' @return Objeto `relaciones_coleccion` con `relaciones`, `cobertura_pares`,
#'   `cobertura_podas` y `meta`.
#' @export
#' @param podar Si se aplican las podas que cambiarían lo informado —tipos
#'   incompatibles y cardinalidades imposibles—, tal como en
#'   [detectar_relaciones()]. `FALSE` por omisión; la poda cierta por rangos
#'   disjuntos se aplica siempre porque no cambia ninguna fila.
#' @seealso [coleccion()], [estimar_costo_coleccion()], [detectar_relaciones()]
#'
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE) &&
#'     requireNamespace("DBI", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(con, "personas", data.frame(id = 1:20, nombre = letters[1:20]))
#'   DBI::dbWriteTable(con, "visitas", data.frame(persona_id = c(1:15, 1:5)))
#'   col <- coleccion(con, c("personas", "visitas"), nombre = "padron")
#'   relaciones_coleccion(
#'     col, pares = data.frame(tabla_1 = "personas", tabla_2 = "visitas"),
#'     columnas_candidatas = list(personas = "id", visitas = "persona_id"),
#'     orden = list(personas = "id", visitas = "persona_id")
#'   )
#'   DBI::dbDisconnect(con)
#' }
relaciones_coleccion <- function(coleccion, pares, muestra = 1e4,
                                 umbral_cobertura = 0.9, orden = NULL,
                                 tope_cache_mb = 512,
                                 columnas_candidatas = NULL,
                                 podar = FALSE,
                                 tope_memoria_mb = 512) {
  if (!inherits(coleccion, "coleccion_lupa")) {
    stop("`coleccion` debe venir de coleccion().", call. = FALSE)
  }
  if (!is.numeric(umbral_cobertura) || length(umbral_cobertura) != 1L ||
      is.na(umbral_cobertura) || umbral_cobertura < 0 ||
      umbral_cobertura > 1) {
    stop("`umbral_cobertura` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.numeric(tope_cache_mb) || length(tope_cache_mb) != 1L ||
      is.na(tope_cache_mb) || tope_cache_mb < 0) {
    stop("`tope_cache_mb` debe ser un numero no negativo.", call. = FALSE)
  }
  if (!is.numeric(tope_memoria_mb) || length(tope_memoria_mb) != 1L ||
      is.na(tope_memoria_mb) || tope_memoria_mb < 0) {
    stop("`tope_memoria_mb` debe ser un numero no negativo.", call. = FALSE)
  }
  if (!is.null(orden) && !is.character(orden) && !is.list(orden)) {
    stop(
      "`orden` debe ser NULL, un vector de nombres de columnas o una lista ",
      "nombrada por identificador de tabla.", call. = FALSE
    )
  }
  pares <- .validar_pares_coleccion(coleccion, pares, exigir = TRUE)
  columnas_candidatas <- .validar_columnas_candidatas_coleccion(
    coleccion, columnas_candidatas
  )
  repetidos <- attr(pares, "repetidos_descartados")
  if (is.null(repetidos)) repetidos <- 0
  conexion <- coleccion$conexion
  muestra <- .validar_muestra_dbi(muestra)

  lector <- .lector_tablas_coleccion(
    conexion, coleccion, muestra, orden, tope_cache_mb, columnas_candidatas
  )

  encontradas <- list()
  sin_comparar <- list()
  podas <- list()
  memoria_resultado_mb <- 0
  combinaciones_comparadas <- 0
  pares_parciales <- 0L
  pares_presupuesto <- 0L
  for (i in seq_len(nrow(pares))) {
    t1 <- pares$tabla_1[[i]]
    t2 <- pares$tabla_2[[i]]
    if (is.finite(tope_memoria_mb) &&
        memoria_resultado_mb >= tope_memoria_mb) {
      pares_presupuesto <- pares_presupuesto + 1L
      sin_comparar[[length(sin_comparar) + 1L]] <- data.frame(
        tabla_1 = t1, tabla_2 = t2,
        motivo = paste0(
          "No se comparo el par: se agoto `tope_memoria_mb` (",
          tope_memoria_mb, ")."
        ),
        como_resolverlo = paste(
          "Aumentar `tope_memoria_mb` o procesar los pares en varias corridas;",
          "las combinaciones pendientes no se interpretan como ausencia de relacion."
        ), stringsAsFactors = FALSE
      )
      next
    }
    momento <- Sys.time()
    d1 <- lector$leer(t1)
    d2 <- lector$leer(t2)
    fallo <- if (inherits(d1, "condition")) {
      d1
    } else if (inherits(d2, "condition")) d2 else NULL
    if (!is.null(fallo)) {
      sin_comparar[[length(sin_comparar) + 1L]] <- data.frame(
        tabla_1 = t1, tabla_2 = t2,
        motivo = paste0("No se pudo leer el par: ", conditionMessage(fallo)),
        como_resolverlo = paste(
          "Comprobar que las dos tablas existen y que la credencial las puede",
          "leer. El SQL exacto de cada intento queda en `meta$lecturas`."
        ),
        stringsAsFactors = FALSE
      )
      next
    }
    candidatas_1 <- if (is.null(columnas_candidatas)) {
      names(d1)
    } else {
      columnas_candidatas[[t1]]
    }
    candidatas_2 <- if (is.null(columnas_candidatas)) {
      names(d2)
    } else {
      columnas_candidatas[[t2]]
    }
    relacion <- tryCatch(
      detectar_relaciones(
        d1, d2, columnas_candidatas = list(candidatas_1, candidatas_2),
        umbral_cobertura = umbral_cobertura, podar = podar,
        tope_memoria_mb = if (is.finite(tope_memoria_mb)) {
          max(0, tope_memoria_mb - memoria_resultado_mb)
        } else Inf
      ), error = function(e) e
    )
    if (inherits(relacion, "condition")) {
      sin_comparar[[length(sin_comparar) + 1L]] <- data.frame(
        tabla_1 = t1, tabla_2 = t2,
        motivo = paste0("No se pudo comparar: ", conditionMessage(relacion)),
        como_resolverlo = "Revisar los tipos de las columnas comparadas.",
        stringsAsFactors = FALSE
      )
      next
    }
    podas_relacion <- attr(relacion, "podas", exact = TRUE)
    if (nrow(podas_relacion)) {
      podas[[length(podas) + 1L]] <- data.frame(
        tabla_1 = rep(t1, nrow(podas_relacion)),
        tabla_2 = rep(t2, nrow(podas_relacion)),
        podas_relacion, stringsAsFactors = FALSE
      )
    }
    if (isTRUE(attr(relacion, "presupuesto_memoria_agotado", exact = TRUE))) {
      pares_parciales <- pares_parciales + 1L
    }
    combinaciones_comparadas <- combinaciones_comparadas +
      as.numeric(attr(relacion, "n_pares_comparados", exact = TRUE))
    memoria_relacion_mb <- attr(relacion, "memoria_resultado_mb", exact = TRUE)
    if (length(memoria_relacion_mb) && is.finite(memoria_relacion_mb)) {
      memoria_resultado_mb <- max(memoria_resultado_mb, memoria_relacion_mb)
    }
    if (!nrow(relacion)) next
    # `which()` y no la mascara: un par no comparado trae cobertura `NA`, y
    # `datos[NA, ]` devuelve una fila entera de `NA` en vez de ninguna. Lo que
    # no se comparo ya esta declarado en `cobertura_podas`; aca no corresponde.
    candidatas <- relacion[
      which(relacion$cobertura_tabla2_en_tabla1 >= umbral_cobertura), ,
      drop = FALSE
    ]
    if (!nrow(candidatas)) next
    candidatas$tabla_1 <- t1
    candidatas$tabla_2 <- t2
    candidatas$filas_leidas_1 <- nrow(d1)
    candidatas$filas_leidas_2 <- nrow(d2)
    candidatas$momento <- momento
    encontradas[[length(encontradas) + 1L]] <- candidatas
    memoria_resultado_mb <- memoria_resultado_mb +
      as.numeric(utils::object.size(candidatas)) / 1024^2
  }

  relaciones <- if (length(encontradas)) {
    do.call(rbind, encontradas)
  } else {
    .relaciones_coleccion_vacias()
  }
  cobertura_pares <- if (length(sin_comparar)) {
    do.call(rbind, sin_comparar)
  } else {
    data.frame(
      tabla_1 = character(), tabla_2 = character(), motivo = character(),
      como_resolverlo = character(), stringsAsFactors = FALSE
    )
  }
  cobertura_podas <- if (length(podas)) {
    do.call(rbind, podas)
  } else {
    .cobertura_podas_vacia()
  }
  rownames(relaciones) <- NULL
  rownames(cobertura_pares) <- NULL
  rownames(cobertura_podas) <- NULL
  estado <- lector$estado()
  bitacora <- lector$bitacora()
  estructura <- list(
    relaciones = relaciones,
    cobertura_pares = cobertura_pares,
    meta = list(
      coleccion = coleccion$nombre,
      pares_declarados = nrow(pares),
      pares_comparados = nrow(pares) - nrow(cobertura_pares),
      pares_faltantes = nrow(cobertura_pares),
      pares_parciales = pares_parciales,
      pares_sin_comparar_por_presupuesto = pares_presupuesto,
      pares_repetidos_descartados = repetidos,
      muestra_por_tabla = muestra,
      umbral_cobertura = umbral_cobertura,
      tablas_distintas = length(unique(c(pares$tabla_1, pares$tabla_2))),
      lecturas_realizadas = estado$lecturas,
      lecturas_evitadas_por_cache = estado$reutilizaciones,
      tablas_en_cache = estado$tablas_en_cache,
      memoria_cache_mb = estado$memoria_cache_mb,
      cache_completa = estado$cache_completa,
      tope_cache_mb = tope_cache_mb,
      tope_memoria_mb = tope_memoria_mb,
      memoria_resultado_mb = memoria_resultado_mb,
      combinaciones_comparadas = combinaciones_comparadas,
      combinaciones_podadas = nrow(cobertura_podas),
      columnas_candidatas = columnas_candidatas,
      podas_por_motivo = if (nrow(cobertura_podas)) {
        as.list(table(cobertura_podas$motivo))
      } else list(),
      lecturas = bitacora,
      orden_declarado = orden,
      estable = !is.null(orden),
      nota_orden = if (is.null(orden)) {
        paste(
          "Sin `orden`, una lectura acotada devuelve un subconjunto arbitrario:",
          "el resultado no es repetible entre corridas ni entre motores."
        )
      } else {
        paste(
          "Se declaro `orden`: la lectura es repetible mientras las columnas",
          "de orden identifiquen las filas de forma unica."
        )
      },
      alcance = paste(
        "Cada par se comparo sobre una muestra de filas de cada tabla. Una",
        "relacion candidata sobre una muestra no es una clave foranea",
        "comprobada: es un indicio que hay que confirmar contra el diccionario",
        "de datos."
      )
    )
  )
  estructura$cobertura_podas <- cobertura_podas
  class(estructura) <- "relaciones_coleccion"
  estructura
}
