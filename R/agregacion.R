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
  implementada = c(rep(TRUE, 6L), rep(FALSE, 4L)),
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

.validar_granularidad <- function(x) {
  if (!.es_texto_escalar(x) ||
      !x %in% .catalogo_granularidades$granularidad) {
    stop("Granularidad no reconocida: ", paste(x, collapse = ", "), ".",
         call. = FALSE)
  }
  x
}

.granularidad_implementada <- function(x) {
  .validar_granularidad(x)
  .catalogo_granularidades$implementada[
    match(x, .catalogo_granularidades$granularidad)
  ]
}

#' Granularidades y transiciones de agregación
#'
#' `granularidades()` declara los diez niveles del marco. Los primeros seis
#' están implementados; los restantes quedan registrados para extender el
#' modelo sin convertir la granularidad en una escala lineal.
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
  campos_unicos <- c(
    "id_medicion", "metrica", "metrica_especifica", "granularidad",
    "tipo_resultado"
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
    conjuntoEntidades = paste(sort(entidades), collapse = ", ")
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
#' sumar uno dentro de cada objeto de destino.
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
                    umbral = NULL, pesos = NULL) {
  medidas <- .validar_medidas_agregacion(medidas)
  destino <- .validar_granularidad(destino)
  if (!.granularidad_implementada(destino)) {
    stop(
      "La granularidad '", destino,
      "' est\u00e1 declarada pero todav\u00eda no est\u00e1 implementada.", call. = FALSE
    )
  }
  funcion <- match.arg(funcion)
  origen <- unique(medidas$granularidad)
  transicion <- .transiciones_granularidad$origen == origen &
    .transiciones_granularidad$destino == destino
  if (!any(transicion)) {
    stop(
      "No existe una transici\u00f3n de agregaci\u00f3n de '", origen,
      "' a '", destino, "'.", call. = FALSE
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
    "metrica_instanciada", "dimension", "factor", "granularidad",
    "tipo_resultado", "entidad", "atributo", "fila", "objeto_medible",
    "resultado", "agregacion"
  )]
  class(resultado) <- c("medicion", "data.frame")
  resultado
}
