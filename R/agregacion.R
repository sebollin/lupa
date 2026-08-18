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
  # El septimo nivel se mide desde que existe `coleccion()` para declarar la
  # frontera. Los tres ultimos siguen sin objeto: un conjunto de bases, una
  # organizacion y un conjunto de organizaciones son decisiones de
  # gobernanza que no estan en ningun dato.
  implementada = c(rep(TRUE, 7L), rep(FALSE, 3L)),
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
  faltante <- if (identical(destino, "coleccion")) {
    paste0(
      " la frontera se declara con `coleccion()` y se mide con ",
      "`perfilar_coleccion()`, que devuelve un tablero por tabla. Agregar a un ",
      "solo numero en este nivel exige ademas declarar los pesos: promediar ",
      "entre tablas de universos distintos sin declararlos seria inventar un ",
      "juicio."
    )
  } else {
    " `lupa` no recibe hoy esa frontera."
  }
  paste0(
    "La granularidad '", destino, "' requiere ", detalle, ";", faltante
  )
}

#' Granularidades y transiciones de agregación
#'
#' `granularidades()` declara los diez niveles del marco. Los primeros seis
#' están implementados. Los cuatro restantes se registran, pero no se miden
#' porque falta declarar la frontera del objeto: qué tablas componen una
#' colección, qué bases componen un conjunto de colecciones, qué bases
#' pertenecen a una organización y qué organizaciones se comparan.
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
  medidas$orientacion <- .orientacion_medidas(medidas)
  campos_unicos <- c(
    "id_medicion", "metrica", "metrica_especifica", "granularidad",
    "tipo_resultado", "orientacion"
  )
  no_unicos <- campos_unicos[vapply(
    medidas[campos_unicos], function(x) length(unique(x)) != 1L, logical(1L)
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
    coleccion = paste(sort(entidades), collapse = ", ")
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
  # La frontera se lee por el IDENTIFICADOR completo, con esquema. Usar el
  # nombre pelado hacia que `public.personas` y `auditoria.personas` colapsaran
  # en una sola tabla, y entonces medir una de las dos daba cobertura 1 de 1 en
  # vez de 1 de 2. Contradecia la afirmacion central de que el esquema es parte
  # de la identidad.
  identificador <- function(esquema, tabla) {
    ifelse(is.na(esquema), tabla, paste0(esquema, ".", tabla))
  }
  if (inherits(coleccion, "coleccion_lupa")) {
    return(list(
      nombre = coleccion$nombre,
      declaradas = coleccion$tablas$identificador,
      motivo_faltantes = character()
    ))
  }
  if (inherits(coleccion, "perfil_coleccion")) {
    faltantes <- coleccion$cobertura_coleccion
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
  list(
    coleccion = frontera$nombre,
    tablas_declaradas = length(declaradas),
    tablas_en_el_numero = length(intersect(declaradas, medidas)),
    tablas_sin_medir = sin_medir,
    motivo_sin_medir = unname(frontera$motivo_faltantes[sin_medir]),
    cobertura = if (length(declaradas)) {
      length(intersect(declaradas, medidas)) / length(declaradas)
    } else NA_real_,
    advertencia = paste(
      "El numero cubre las tablas medidas, no la coleccion declarada.",
      "Leerlo sin su cobertura seria informar como medido lo que no se midio."
    )
  )
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
                    umbral = NULL, pesos = NULL, coleccion = NULL) {
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
  if (!.granularidad_implementada(destino)) {
    stop(.mensaje_granularidad_sin_frontera(destino), call. = FALSE)
  }
  funcion <- match.arg(funcion)
  if (identical(destino, "coleccion") &&
      !identical(funcion, "promedio_ponderado")) {
    # Sin esta restriccion la politica de pesos se esquiva por la puerta de al
    # lado: `promedio` daria un numero entre tablas sin declarar nada, que es
    # exactamente el juicio que el paquete se niega a inventar.
    stop(
      "En la granularidad 'coleccion' solo se admite 'promedio_ponderado': ",
      "combinar tablas de universos distintos exige declarar los pesos. ",
      "Sin pesos, `perfilar_coleccion()` devuelve el tablero por tabla.",
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
  resultado
}
