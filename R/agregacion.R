.catalogo_granularidades <- data.frame(
  nivel = seq_len(10L),
  granularidad = c(
    "instanciaAtributo", "atributo", "conjuntoAtributos",
    "instanciaEntidad", "entidad", "conjuntoEntidades", "coleccion",
    "conjuntoColecciones", "organizacion", "conjuntoOrganizaciones"
  ),
  relacional = c(
    "celda", "columna", "conjunto de columnas", "tupla", "tabla",
    "conjunto de tablas", "base de datos", NA, NA, NA
  ),
  # Los diez niveles se miden, y los cuatro de arriba solo cuando el usuario
  # declara la frontera: que tablas componen una coleccion, que bases un
  # conjunto, que colecciones una organizacion, que organizaciones un conjunto.
  # `lupa` no infiere ninguna de las cuatro. Que esten implementadas no obliga a
  # usarlas: un analisis sin organizacion detras se detiene donde corresponda.
  implementada = rep(TRUE, 10L),
  stringsAsFactors = FALSE
)

.transiciones_granularidad <- data.frame(
  origen = c(
    "instanciaAtributo", "instanciaAtributo", "instanciaEntidad",
    "atributo", "entidad", "entidad", "coleccion", "coleccion",
    "organizacion"
  ),
  destino = c(
    "atributo", "instanciaEntidad", "entidad", "entidad",
    "conjuntoEntidades", "coleccion", "conjuntoColecciones",
    "organizacion", "conjuntoOrganizaciones"
  ),
  fuente = c(
    "marco", "extension_documentada", "marco", "marco", "marco", "marco",
    "marco", "marco", "marco"
  ),
  stringsAsFactors = FALSE
)

.texto_vocabularios_granularidad <- function() {
  ontologia <- .catalogo_granularidades$granularidad
  relacional <- stats::na.omit(.catalogo_granularidades$relacional)
  paste0(
    " Ontolog\u00eda: ", paste(ontologia, collapse = ", "),
    ". Relacional: ", paste(relacional, collapse = ", "), "."
  )
}

.validar_granularidad <- function(x, aceptar_relacional = FALSE) {
  if (.es_texto_escalar(x)) {
    if (x %in% .catalogo_granularidades$granularidad) return(x)
    if (aceptar_relacional &&
        x %in% stats::na.omit(.catalogo_granularidades$relacional)) {
      return(.catalogo_granularidades$granularidad[
        match(x, .catalogo_granularidades$relacional)
      ])
    }
  }
  stop(
    "Granularidad no reconocida: ", paste(x, collapse = ", "), ".",
    .texto_vocabularios_granularidad(), call. = FALSE
  )
}

.granularidad_implementada <- function(x) {
  .validar_granularidad(x)
  .catalogo_granularidades$implementada[
    match(x, .catalogo_granularidades$granularidad)
  ]
}

.mensaje_granularidad_sin_frontera <- function(destino) {
  detalle <- switch(
    destino,
    coleccion = paste0(
      "una colecci\u00f3n declarada: qu\u00e9 tablas la componen"
    ),
    conjuntoColecciones = paste0(
      "un conjunto de colecciones declarado: qu\u00e9 bases lo componen"
    ),
    organizacion = paste0(
      "una organizaci\u00f3n declarada: qu\u00e9 bases le pertenecen"
    ),
    conjuntoOrganizaciones = paste0(
      "un conjunto de organizaciones declarado: qu\u00e9 organizaciones se comparan"
    ),
    "el objeto declarado y su frontera"
  )
  # La frontera de una coleccion ya se puede declarar con `coleccion()`, asi que
  # el mensaje lo dice en vez de sugerir que no hay forma. Lo que falta para
  # agregar a ese nivel es la politica de pesos: promediar entre tablas sin
  # declararlos seria inventar un juicio.
  faltante <- switch(
    destino,
    coleccion = paste0(
      " la frontera se declara con `coleccion()` y se mide con ",
      "`perfilar_coleccion()`, que devuelve un tablero por tabla. Agregar a un ",
      "solo numero en este nivel exige ademas declarar los pesos: promediar ",
      "entre tablas de universos distintos sin declararlos seria inventar un ",
      "juicio."
    ),
    conjuntoColecciones = paste0(
      " se declara con el argumento `colecciones` de `agregar()`, una lista ",
      "nombrada de objetos de `coleccion()` o `perfilar_coleccion()`."
    ),
    organizacion = paste0(
      " se declara con `organizacion(nombre, colecciones)` y se pasa en el ",
      "argumento `organizacion` de `agregar()`. Es opcional: un analisis sin ",
      "organismo detras no necesita este nivel."
    ),
    conjuntoOrganizaciones = paste0(
      " se declara con el argumento `organizaciones` de `agregar()`, una lista ",
      "de objetos de `organizacion()`. Es opcional, igual que el nivel anterior."
    ),
    " `lupa` no recibe hoy esa frontera."
  )
  paste0(
    "La granularidad '", destino, "' requiere ", detalle, ";", faltante
  )
}

#' Granularidades y transiciones de agregación
#'
#' `granularidades()` declara los diez niveles del marco, y los diez se miden.
#' Los cuatro de arriba —colección, conjunto de colecciones, organización y
#' conjunto de organizaciones— sólo cuando el usuario **declara la frontera**:
#' qué tablas componen una colección, qué bases un conjunto, qué colecciones una
#' organización, qué organizaciones un conjunto. `lupa` no infiere ninguna de las
#' cuatro, porque ninguna está en los datos.
#'
#' Que estén implementadas no obliga a usarlas. Un análisis que no tiene una
#' organización detrás se detiene donde corresponda; los niveles superiores
#' existen para quien los necesita.
#'
#' `transiciones_granularidad()` devuelve el grafo dirigido de agregaciones.
#' La transición `instanciaAtributo` a `instanciaEntidad` se incorpora porque
#' el propio marco la usa aunque no aparezca en su tabla no exhaustiva.
#'
#' @return Data frames con niveles o aristas del grafo de granularidad.
#' @export
#' @seealso [modelo()], [medir()], [evaluar()]
#'
#' @examples
#' granularidades()
#' transiciones_granularidad()
#' @name granularidades
NULL

#' @rdname granularidades
#' @export
granularidades <- function() {
  .catalogo_granularidades
}

#' @rdname granularidades
#' @export
transiciones_granularidad <- function() {
  .transiciones_granularidad
}

.validar_medidas_agregacion <- function(medidas) {
  requeridas <- c(
    "id_medicion", "fecha", "metrica", "metrica_especifica",
    "dimension", "factor", "granularidad", "tipo_resultado", "entidad",
    "atributo", "fila", "resultado"
  )
  if (!inherits(medidas, "data.frame") || !nrow(medidas) ||
      !all(requeridas %in% names(medidas))) {
    stop("`medidas` debe ser un data frame no vac\u00edo producido por medir() o agregar().",
         call. = FALSE)
  }
  medidas <- .tabla_base(medidas)
  medidas$orientacion <- .orientacion_medidas(medidas)
  campos_unicos <- c(
    "id_medicion", "metrica", "metrica_especifica", "granularidad",
    "tipo_resultado", "orientacion"
  )
  no_unicos <- campos_unicos[vapply(
    .seleccionar_columnas(medidas, campos_unicos),
    function(x) length(unique(x)) != 1L, logical(1L)
  )]
  if (length(no_unicos)) {
    stop(
      "Las medidas deben compartir: ", paste(no_unicos, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!is.numeric(medidas$resultado) || anyNA(medidas$resultado) ||
      any(!is.finite(medidas$resultado)) ||
      any(medidas$resultado < 0 | medidas$resultado > 1)) {
    stop("Los resultados que se agregan deben estar en [0, 1].", call. = FALSE)
  }
  medidas
}

.indices_grupos_agregacion <- function(medidas, destino) {
  claves <- switch(
    destino,
    atributo = list(medidas$entidad, medidas$atributo),
    instanciaEntidad = list(medidas$entidad, medidas$fila),
    entidad = list(medidas$entidad),
    conjuntoEntidades = list(rep("conjunto", nrow(medidas))),
    # La coleccion entera es un solo objeto: todas las medidas de las tablas
    # declaradas caen en el mismo grupo.
    coleccion = list(rep("coleccion", nrow(medidas))),
    conjuntoColecciones = list(rep("conjunto_colecciones", nrow(medidas))),
    organizacion = list(rep("organizacion", nrow(medidas))),
    conjuntoOrganizaciones = list(rep("conjunto_organizaciones", nrow(medidas))),
    stop("La granularidad de destino todav\u00eda no admite agregaci\u00f3n.",
         call. = FALSE)
  )
  clave <- do.call(
    interaction,
    c(lapply(claves, function(x) addNA(as.factor(x))),
      list(drop = TRUE, lex.order = TRUE))
  )
  split(seq_len(nrow(medidas)), clave, drop = TRUE)
}

.calcular_agregacion <- function(valores, funcion, umbral, pesos) {
  switch(
    funcion,
    ratio = mean(valores == 1),
    ratio_umbral = mean(valores >= umbral),
    promedio = mean(valores),
    promedio_ponderado = sum(valores * pesos)
  )
}

.objeto_agregado <- function(medidas, indices, destino) {
  entidades <- unique(medidas$entidad[indices])
  switch(
    destino,
    atributo = paste0(entidades[[1L]], "$", medidas$atributo[indices[[1L]]]),
    instanciaEntidad = paste0(
      entidades[[1L]], "[", medidas$fila[indices[[1L]]], ",]"
    ),
    entidad = entidades[[1L]],
    conjuntoEntidades = paste(sort(entidades), collapse = ", "),
    coleccion = paste(sort(entidades), collapse = ", "),
    conjuntoColecciones = paste(sort(entidades), collapse = ", "),
    organizacion = paste(sort(entidades), collapse = ", "),
    conjuntoOrganizaciones = paste(sort(entidades), collapse = ", ")
  )
}
# Estos dos ayudantes van ANTES del bloque `roxygen` de `agregar()`, y no entre
# el bloque y su definicion. Ponerlos en el medio hizo que `roxygen` le pegara
# el `@export` y la documentacion de `agregar()` al ayudante: `agregar()` dejo
# de exportarse y un interno con punto quedo exportado. La suite no lo vio
# porque `pkgload::load_all()` expone todo; en un paquete instalado
# `lupa::agregar()` no habria existido.

.validar_coleccion_destino <- function(coleccion) {
  if (is.null(coleccion)) {
    stop(
      "Agregar a la granularidad 'coleccion' exige declarar la frontera: ",
      "pase `coleccion = ` con el objeto de coleccion() o el perfil de ",
      "perfilar_coleccion(). Sin la frontera no se sabe sobre que tablas se ",
      "esta agregando.", call. = FALSE
    )
  }
  # La frontera se lee por el IDENTIFICADOR completo, con esquema, y con la
  # MISMA funcion con que se arma en `coleccion.R`. Habia una copia identica
  # aca, y dos copias que tienen que coincidir son una divergencia esperando:
  # si una cambiara, la frontera declarada y la leida dejarian de cruzar.
  identificador <- .identificadores_tabla
  if (inherits(coleccion, "coleccion_lupa")) {
    # Una coleccion guardada antes de que existiera la columna `catalogo` llega
    # sin ella. Se completa con `NA` para que su identificador siga siendo el
    # mismo que tenia y la frontera cruce igual.
    coleccion$tablas <- .completar_catalogo_coleccion(coleccion$tablas)
    return(list(
      nombre = coleccion$nombre,
      declaradas = coleccion$tablas$identificador,
      motivo_faltantes = stats::setNames(
        rep("No hay una medida de esta tabla en la entrada.",
            nrow(coleccion$tablas)),
        coleccion$tablas$identificador
      )
    ))
  }
  if (inherits(coleccion, "perfil_coleccion")) {
    faltantes <- coleccion$cobertura_coleccion
    # `cobertura_coleccion` ya no es solo la lista de tablas sin perfilar:
    # tambien declara tablas vacias, mediciones incompletas y metricas
    # rechazadas sobre tablas que si se perfilaron. Aca interesan solo las que
    # faltan como tabla, y confiar en la deduplicacion posterior seria apoyarse
    # en un efecto lateral.
    if (!is.null(faltantes$alcance)) {
      faltantes <- faltantes[faltantes$alcance == "tabla", , drop = FALSE]
    }
    ids_faltantes <- identificador(faltantes$esquema, faltantes$tabla)
    return(list(
      nombre = coleccion$meta$nombre,
      declaradas = c(
        coleccion$resumen_coleccion$identificador, ids_faltantes
      ),
      motivo_faltantes = stats::setNames(faltantes$motivo, ids_faltantes)
    ))
  }
  stop(
    "`coleccion` debe venir de coleccion() o de perfilar_coleccion().",
    call. = FALSE
  )
}

# El hallazgo central de refutar este diseno: un numero sobre "la coleccion"
# calculado solo con las tablas que se pudieron medir informa como medido lo que
# no se midio. El peso de la tabla ausente desaparece en vez de manifestar la
# falta de cobertura. Por eso la cobertura viaja pegada al numero, igual que en
# indice_calidad().
.cobertura_agregacion_coleccion <- function(frontera, entidades_medidas) {
  declaradas <- unique(frontera$declaradas)
  medidas <- unique(entidades_medidas)
  sin_medir <- setdiff(declaradas, medidas)
  motivos <- unname(frontera$motivo_faltantes[sin_medir])
  motivos[is.na(motivos)] <-
    "No hay una medida de esta tabla en la entrada; no se midio en este alcance."
  list(
    coleccion = frontera$nombre,
    tablas_declaradas = length(declaradas),
    tablas_en_el_numero = length(intersect(declaradas, medidas)),
    tablas_sin_medir = sin_medir,
    motivo_sin_medir = motivos,
    cobertura = if (length(declaradas)) {
      length(intersect(declaradas, medidas)) / length(declaradas)
    } else NA_real_,
    advertencia = paste(
      "El numero cubre las tablas medidas, no la coleccion declarada.",
      "Leerlo sin su cobertura seria informar como medido lo que no se midio."
    )
  )
}

.validar_conjunto_colecciones <- function(colecciones) {
  if (!is.list(colecciones) || !length(colecciones)) {
    stop(
      "`colecciones` debe ser una lista no vacia de objetos creados por ",
      "coleccion() o perfilar_coleccion().", call. = FALSE
    )
  }
  clases_validas <- vapply(
    colecciones, function(x) inherits(x, c("coleccion_lupa", "perfil_coleccion")),
    logical(1L)
  )
  if (any(!clases_validas)) {
    stop(
      "Cada elemento de `colecciones` debe provenir de coleccion() o ",
      "perfilar_coleccion().", call. = FALSE
    )
  }
  nombres <- names(colecciones)
  if (is.null(nombres) || anyNA(nombres) || any(!nzchar(nombres))) {
    nombres <- vapply(colecciones, function(x) {
      if (inherits(x, "coleccion_lupa")) x$nombre else x$meta$nombre
    }, character(1L))
  }
  if (anyNA(nombres) || any(!nzchar(nombres)) || anyDuplicated(nombres)) {
    stop(
      "`colecciones` debe tener nombres unicos y no vacios; esos nombres son",
      " la identidad de cada coleccion en el conjunto.", call. = FALSE
    )
  }
  names(colecciones) <- nombres
  list(nombre = "conjuntoColecciones", declaradas = nombres)
}

# La cobertura de una frontera declarada es siempre la misma pregunta -cuantas
# de las partes declaradas entraron en el numero- y cambia solo como se llama la
# parte. Se generaliza para que los cuatro niveles con frontera la respondan
# igual, en vez de tener cuatro copias que se desincronizan.
.cobertura_frontera_declarada <- function(frontera, entidades_medidas, parte) {
  declaradas <- unique(frontera$declaradas)
  medidas <- unique(entidades_medidas)
  presentes <- intersect(declaradas, medidas)
  sin_medir <- setdiff(declaradas, medidas)
  salida <- list(
    conjunto = frontera$nombre,
    declaradas = length(declaradas),
    en_el_numero = length(presentes),
    sin_medir = sin_medir,
    motivo_sin_medir = rep(
      paste0(
        "No hay una medida de esta ", parte, " en la entrada; no se midio en ",
        "este alcance."
      ),
      length(sin_medir)
    ),
    cobertura = if (length(declaradas)) {
      length(presentes) / length(declaradas)
    } else NA_real_,
    advertencia = paste0(
      "El numero cubre las partes medidas, no todas las declaradas. Leerlo sin ",
      "su cobertura seria informar como medido lo que no se midio."
    )
  )
  # Los nombres historicos del conjunto de colecciones se conservan para no
  # romper a quien ya los lee.
  salida$colecciones_declaradas <- salida$declaradas
  salida$colecciones_en_el_numero <- salida$en_el_numero
  salida$colecciones_sin_medir <- salida$sin_medir
  salida
}

# Una agregacion hereda lo que sus partes declararon. Sin esto, la cobertura de
# cada organizacion se perdia al armar el conjunto y el numero de arriba salia
# diciendo que estaba completo.
# Una parte con peso cero entra al numero sin aportarle nada, y la cobertura la
# contaba como si hubiera entrado. No es falso -entro- pero leerlo sin saber que
# no pesa es leer otra cosa. Se declara.
.declarar_partes_sin_peso <- function(resultado, medidas, destino, pesos) {
  if (is.null(pesos)) return(resultado)
  propio <- switch(
    destino,
    organizacion = "cobertura_organizacion",
    conjuntoOrganizaciones = "cobertura_conjunto_organizaciones",
    coleccion = "cobertura_coleccion",
    conjuntoColecciones = "cobertura_conjunto_colecciones",
    NULL
  )
  if (is.null(propio) || is.null(attr(resultado, propio, exact = TRUE))) {
    return(resultado)
  }
  sin_peso <- unique(medidas$entidad[!is.na(pesos) & pesos == 0])
  if (!length(sin_peso)) return(resultado)
  cobertura <- attr(resultado, propio, exact = TRUE)
  cobertura$partes_con_peso_cero <- sin_peso
  cobertura$advertencia <- paste0(
    cobertura$advertencia, " ", length(sin_peso),
    " parte(s) entraron con peso cero: estan contadas en la cobertura y no",
    " aportan al numero."
  )
  attr(resultado, propio) <- cobertura
  resultado
}

.heredar_cobertura_de_partes <- function(resultado, medidas, destino) {
  atributos <- c(
    "cobertura_coleccion", "cobertura_conjunto_colecciones",
    "cobertura_organizacion", "cobertura_conjunto_organizaciones"
  )
  heredadas <- list()
  for (nombre in atributos) {
    previa <- attr(medidas, nombre, exact = TRUE)
    if (!is.null(previa)) heredadas[[nombre]] <- previa
  }
  anteriores <- attr(medidas, "cobertura_de_partes", exact = TRUE)
  if (!is.null(anteriores)) heredadas <- c(anteriores, heredadas)
  if (!length(heredadas)) return(resultado)
  attr(resultado, "cobertura_de_partes") <- heredadas
  # Y la cobertura propia de este nivel se corrige: si alguna parte venia
  # incompleta, el numero de arriba tampoco esta completo.
  propio <- switch(
    destino,
    organizacion = "cobertura_organizacion",
    conjuntoOrganizaciones = "cobertura_conjunto_organizaciones",
    coleccion = "cobertura_coleccion",
    conjuntoColecciones = "cobertura_conjunto_colecciones",
    NULL
  )
  if (is.null(propio) || is.null(attr(resultado, propio, exact = TRUE))) {
    return(resultado)
  }
  parciales <- vapply(
    heredadas,
    function(x) isTRUE(is.finite(x$cobertura)) && x$cobertura < 1,
    logical(1L)
  )
  if (!any(parciales)) return(resultado)
  cobertura <- attr(resultado, propio, exact = TRUE)
  cobertura$partes_incompletas <- names(heredadas)[parciales]
  cobertura$completo <- FALSE
  cobertura$advertencia <- paste0(
    cobertura$advertencia,
    " Ademas, ", sum(parciales), " de las partes que entraron al numero venian",
    " incompletas: su cobertura esta en `cobertura_de_partes`."
  )
  attr(resultado, propio) <- cobertura
  resultado
}

.cobertura_agregacion_conjunto <- function(frontera, entidades_medidas) {
  .cobertura_frontera_declarada(frontera, entidades_medidas, "coleccion")
}


#' Agregar medidas entre granularidades
#'
#' Aplica exactamente una de las cuatro agregaciones del marco: `ratio`,
#' `ratio_umbral`, `promedio` o `promedio_ponderado`. La transición se valida
#' contra el grafo de `transiciones_granularidad()`.
#'
#' `ratio` sólo acepta medidas booleanas. `ratio_umbral` sólo acepta medidas
#' reales. Los promedios aceptan ambos tipos y siempre producen resultado real
#' en `[0, 1]`. Para el promedio ponderado, los pesos deben estar en `[0, 1]` y
#' sumar uno dentro de cada objeto de destino. La columna `orientacion` se
#' conserva sin invertir el resultado: un ratio de una métrica de defecto sigue
#' siendo la proporción de defectos.
#'
#' No existe una transición hacia factor, dimensión o modelo: esos campos son
#' taxonómicos y esta función no calcula un índice global.
#'
#' @param medidas Data frame producido por `medir()` o una agregación anterior.
#'   Debe contener una sola métrica específica, una corrida y una granularidad.
#' @param destino Granularidad de destino.
#' @param funcion Una de `"ratio"`, `"ratio_umbral"`, `"promedio"` o
#'   `"promedio_ponderado"`.
#' @param umbral Umbral en `[0, 1]` requerido por `ratio_umbral`.
#' @param pesos Vector numérico requerido por `promedio_ponderado`, con una
#'   entrada por fila de `medidas`.
#' @param coleccion Frontera declarada, exigida cuando `destino` es
#'   `"coleccion"`: el objeto de [coleccion()] o el perfil de
#'   [perfilar_coleccion()]. Sin ella no se sabe sobre qué tablas se está
#'   agregando, y el número resultante no describiría nada.
#' @param colecciones Lista nombrada de objetos de [coleccion()] o
#'   [perfilar_coleccion()], exigida cuando `destino` es
#'   `"conjuntoColecciones"`. Los nombres declaran la identidad y la frontera
#'   del conjunto; no se agregan organizaciones ni otros alcances implícitos.
#' @param organizacion Frontera institucional declarada con [organizacion()],
#'   exigida cuando `destino` es `"organizacion"`. Qué bases pertenecen a un
#'   organismo **no está en los datos**, así que lo declara quien lo sabe.
#' @param organizaciones Lista de objetos de [organizacion()], exigida cuando
#'   `destino` es `"conjuntoOrganizaciones"`.
#'
#'   Los dos niveles institucionales son **opcionales**: un análisis de calidad
#'   no siempre tiene una organización detrás —una entrega suelta, un archivo
#'   que alguien mandó, una base sin dueño declarado—, y nada obliga a pasar por
#'   ellos. Existen para quien los necesita, y sin declaración `agregar()` se
#'   niega y explica cómo declararla, que es distinto de inventar una frontera
#'   que nadie nombró.
#'
#' @return Objeto `medicion` agregado, con una fila por objeto de destino.
#' @export
#'
#' @examples
#' nucleo <- metricas_nucleo()
#' especifica <- especializar(nucleo$NoNulo)
#' instancia <- instanciar(especifica, "personas", "edad")
#' medidas <- medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
#' agregar(medidas, "atributo", "ratio")
agregar <- function(medidas, destino,
                    funcion = c(
                      "ratio", "ratio_umbral", "promedio",
                      "promedio_ponderado"
                    ),
                    umbral = NULL, pesos = NULL, coleccion = NULL,
                    colecciones = NULL, organizacion = NULL,
                    organizaciones = NULL) {
  medidas <- .validar_medidas_agregacion(medidas)
  # Se acepta el nombre relacional —`tabla`, `columna`, `celda`— igual que en
  # `metrica()`: el mensaje de error ya los enumera, asi que rechazarlos aca era
  # una inconsistencia. El objeto sigue guardando el nombre canonico del marco.
  destino <- .validar_granularidad(destino, aceptar_relacional = TRUE)
  origen <- unique(medidas$granularidad)
  transicion <- .transiciones_granularidad$origen == origen &
    .transiciones_granularidad$destino == destino
  if (!any(transicion)) {
    stop(
      "No existe una transici\u00f3n de agregaci\u00f3n de '", origen,
      "' a '", destino, "'.", call. = FALSE
    )
  }
  if (identical(destino, "coleccion")) {
    coleccion <- .validar_coleccion_destino(coleccion)
    # Y la otra mitad de la frontera, que faltaba: ninguna medida puede venir
    # de una entidad que no este declarada. Sin esto, un numero calculado a
    # medias sobre una tabla ajena se presentaba como medida de la coleccion,
    # con cobertura 1 de 1. Es el mismo invariante roto en la direccion
    # contraria.
    ajenas <- setdiff(unique(medidas$entidad), coleccion$declaradas)
    if (length(ajenas)) {
      stop(
        "Hay medidas de entidades que no estan declaradas en la coleccion '",
        coleccion$nombre, "': ", paste(sort(ajenas), collapse = ", "),
        ". Declaradas: ", paste(sort(coleccion$declaradas), collapse = ", "),
        ". Un numero que mezcle objetos de fuera de la frontera no describe la ",
        "coleccion.", call. = FALSE
      )
    }
  }
  conjunto <- NULL
  if (identical(destino, "conjuntoColecciones")) {
    conjunto <- .validar_conjunto_colecciones(colecciones)
    ajenas <- setdiff(unique(medidas$entidad), conjunto$declaradas)
    if (length(ajenas)) {
      stop(
        "Hay medidas de colecciones que no estan declaradas en el conjunto: ",
        paste(sort(ajenas), collapse = ", "), ". Declaradas: ",
        paste(sort(conjunto$declaradas), collapse = ", "), ".", call. = FALSE
      )
    }
  }
  organismo <- NULL
  if (identical(destino, "organizacion")) {
    if (is.null(organizacion)) {
      stop(.mensaje_granularidad_sin_frontera(destino), call. = FALSE)
    }
    organismo <- .validar_organizacion_destino(organizacion)
    ajenas <- setdiff(unique(medidas$entidad), organismo$declaradas)
    if (length(ajenas)) {
      stop(
        "Hay medidas de colecciones que no pertenecen a la organizacion '",
        organismo$nombre, "': ", paste(sort(ajenas), collapse = ", "),
        ". Declaradas: ", paste(sort(organismo$declaradas), collapse = ", "),
        ". Un numero que mezcle objetos de fuera de la frontera no describe a ",
        "ese organismo.", call. = FALSE
      )
    }
  }
  conjunto_organismos <- NULL
  if (identical(destino, "conjuntoOrganizaciones")) {
    if (is.null(organizaciones)) {
      stop(.mensaje_granularidad_sin_frontera(destino), call. = FALSE)
    }
    conjunto_organismos <- .validar_conjunto_organizaciones(organizaciones)
    entidades_resueltas <- .resolver_partes_frontera(
      medidas$entidad, conjunto_organismos
    )
    ajenas <- setdiff(unique(entidades_resueltas), conjunto_organismos$declaradas)
    if (length(ajenas)) {
      stop(
        "Hay medidas de organizaciones que no estan declaradas en el conjunto: ",
        paste(sort(ajenas), collapse = ", "), ". Declaradas: ",
        paste(sort(conjunto_organismos$declaradas), collapse = ", "), ".",
        call. = FALSE
      )
    }
  }
  if (!.granularidad_implementada(destino)) {
    stop(.mensaje_granularidad_sin_frontera(destino), call. = FALSE)
  }
  funcion <- match.arg(funcion)
  .niveles_con_frontera <- c(
    "coleccion", "conjuntoColecciones", "organizacion",
    "conjuntoOrganizaciones"
  )
  if (destino %in% .niveles_con_frontera &&
      !identical(funcion, "promedio_ponderado")) {
    # Sin esta restriccion la politica de pesos se esquiva por la puerta de al
    # lado: `promedio` daria un numero entre tablas sin declarar nada, que es
    # exactamente el juicio que el paquete se niega a inventar. Vale igual para
    # los dos niveles institucionales: promediar organismos de tamano distinto
    # sin declararlo es el mismo juicio inventado, un piso mas arriba.
    stop(
      "En las granularidades '", paste(.niveles_con_frontera, collapse = "', '"),
      "' solo se admite 'promedio_ponderado': combinar alcances distintos exige ",
      "declarar los pesos. Sin pesos, se devuelve el tablero por parte.",
      call. = FALSE
    )
  }
  tipo <- unique(medidas$tipo_resultado)
  if (funcion == "ratio" && tipo != "booleano") {
    stop("`ratio` s\u00f3lo admite m\u00e9tricas de resultado booleano.", call. = FALSE)
  }
  if (funcion == "ratio_umbral" && tipo != "real") {
    stop("`ratio_umbral` s\u00f3lo admite m\u00e9tricas de resultado real.",
         call. = FALSE)
  }
  if (funcion == "ratio_umbral") {
    if (!is.numeric(umbral) || length(umbral) != 1L || is.na(umbral) ||
        !is.finite(umbral) || umbral < 0 || umbral > 1) {
      stop("`umbral` debe ser un n\u00famero entre 0 y 1.", call. = FALSE)
    }
  }
  if (funcion == "promedio_ponderado") {
    if (!is.numeric(pesos) || length(pesos) != nrow(medidas) || anyNA(pesos) ||
        any(!is.finite(pesos)) || any(pesos < 0 | pesos > 1)) {
      stop("`pesos` debe tener una entrada en [0, 1] por medida.", call. = FALSE)
    }
  }
  grupos <- .indices_grupos_agregacion(medidas, destino)
  if (funcion == "promedio_ponderado") {
    sumas <- vapply(grupos, function(i) sum(pesos[i]), numeric(1L))
    if (any(abs(sumas - 1) > sqrt(.Machine$double.eps))) {
      stop("Los pesos deben sumar 1 dentro de cada objeto de destino.", call. = FALSE)
    }
  }
  partes <- lapply(seq_along(grupos), function(k) {
    indices <- grupos[[k]]
    primera <- indices[[1L]]
    valor <- .calcular_agregacion(
      medidas$resultado[indices], funcion, umbral,
      if (is.null(pesos)) NULL else pesos[indices]
    )
    entidad <- if (destino == "conjuntoEntidades") {
      paste(sort(unique(medidas$entidad[indices])), collapse = ", ")
    } else if (destino == "coleccion") {
      coleccion$nombre
    } else if (destino == "conjuntoColecciones") {
      conjunto$nombre
    } else if (destino == "organizacion") {
      organismo$nombre
    } else if (destino == "conjuntoOrganizaciones") {
      conjunto_organismos$nombre
    } else {
      medidas$entidad[[primera]]
    }
    atributo <- if (destino == "atributo") {
      medidas$atributo[[primera]]
    } else {
      NA_character_
    }
    fila <- if (destino == "instanciaEntidad") {
      medidas$fila[[primera]]
    } else {
      NA_integer_
    }
    data.frame(
      id_medicion = medidas$id_medicion[[primera]],
      fecha = medidas$fecha[[primera]],
      metrica = medidas$metrica[[primera]],
      metrica_especifica = medidas$metrica_especifica[[primera]],
      metrica_instanciada = paste0(
        "agregada:", funcion, ":", medidas$metrica_especifica[[primera]]
      ),
      dimension = medidas$dimension[[primera]],
      factor = medidas$factor[[primera]],
      orientacion = medidas$orientacion[[primera]],
      granularidad = destino,
      tipo_resultado = "real",
      entidad = entidad,
      atributo = atributo,
      fila = fila,
      objeto_medible = .objeto_agregado(medidas, indices, destino),
      resultado = valor,
      agregacion = funcion,
      stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, partes)
  rownames(resultado) <- NULL
  resultado$id_medida <- paste0(
    resultado$id_medicion, "-agg-", funcion, "-",
    sprintf("%06d", seq_len(nrow(resultado)))
  )
  resultado <- resultado[c(
    "id_medida", "id_medicion", "fecha", "metrica", "metrica_especifica",
    "metrica_instanciada", "dimension", "factor", "orientacion", "granularidad",
    "tipo_resultado", "entidad", "atributo", "fila", "objeto_medible",
    "resultado", "agregacion"
  )]
  class(resultado) <- c("medicion", "data.frame")
  if (identical(destino, "coleccion")) {
    attr(resultado, "cobertura_coleccion") <- .cobertura_agregacion_coleccion(
      coleccion, medidas$entidad
    )
  }
  if (identical(destino, "conjuntoColecciones")) {
    attr(resultado, "cobertura_conjunto_colecciones") <-
      .cobertura_agregacion_conjunto(conjunto, medidas$entidad)
  }
  if (identical(destino, "organizacion")) {
    attr(resultado, "cobertura_organizacion") <-
      .cobertura_frontera_declarada(organismo, medidas$entidad, "coleccion")
  }
  if (identical(destino, "conjuntoOrganizaciones")) {
    attr(resultado, "cobertura_conjunto_organizaciones") <-
      .cobertura_frontera_declarada(
        conjunto_organismos,
        .resolver_partes_frontera(medidas$entidad, conjunto_organismos),
        "organizacion"
      )
  }
  # La cobertura de las partes no se pierde al subir de nivel. Un conjunto
  # armado con una organizacion a la que le falto una coleccion **no esta
  # completo**, y decir cobertura 1 seria informar como completo lo que es
  # parcial: el mismo defecto que el paquete persigue, un piso mas arriba.
  resultado <- .heredar_cobertura_de_partes(resultado, medidas, destino)
  resultado <- .declarar_partes_sin_peso(resultado, medidas, destino, pesos)
  resultado
}
