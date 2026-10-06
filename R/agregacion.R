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
  # `implementada` declara que el nivel se puede medir. `agregable` declara
  # que el grafo ofrece al menos una entrada o salida para ese nivel; son
  # capacidades distintas y no se deben resumir en una sola etiqueta.
  agregable = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  motivo_agregacion = c(
    rep(NA_character_, 2L),
    "No hay transiciones de agregacion hacia ni desde este nivel.",
    rep(NA_character_, 7L)
  ),
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
    canonicas <- .catalogo_granularidades$granularidad
    indice <- match(.nombres_para_operar(x), .nombres_para_operar(canonicas))
    if (!is.na(indice)) return(canonicas[[indice]])
    if (aceptar_relacional) {
      indices_relacionales <- which(
        !is.na(.catalogo_granularidades$relacional)
      )
      indice_relacional <- match(
        .nombres_para_operar(x),
        .nombres_para_operar(.catalogo_granularidades$relacional[
          indices_relacionales
        ])
      )
      if (!is.na(indice_relacional)) {
        return(canonicas[[indices_relacionales[[indice_relacional]]]])
      }
    }
  }
  stop(
    "Granularidad no reconocida: ", paste(x, collapse = ", "), ".",
    .texto_vocabularios_granularidad(), call. = FALSE
  )
}

.granularidad_implementada <- function(x) {
  x <- .validar_granularidad(x)
  .catalogo_granularidades$implementada[
    match(.nombres_para_operar(x),
          .nombres_para_operar(.catalogo_granularidades$granularidad))
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
#' La columna `implementada` significa que se puede instanciar una metrica y
#' medir ese nivel. La columna `agregable` es distinta: indica que el grafo
#' ofrece al menos una transicion de agregacion hacia o desde el nivel. Por eso
#' `conjuntoAtributos` puede estar implementado para medir una metrica y a la
#' vez no ser agregable con las transiciones disponibles; `motivo_agregacion`
#' deja esa razon escrita.
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
  # La entrada vacia se separa del resto: viene de `medir()` y su cobertura dice
  # POR QUE quedo sin medidas. Ver el mismo arreglo en `.validar_medicion_evaluacion()`.
  if (!inherits(medidas, "data.frame") ||
      !all(requeridas %in% names(medidas))) {
    stop("`medidas` debe ser un data frame producido por medir() o agregar().",
         call. = FALSE)
  }
  if (!nrow(medidas)) {
    .error_medicion_sin_medidas(medidas, "medidas", "`medir()` o `agregar()`")
  }
  medidas <- .tabla_base(medidas)
  medidas$orientacion <- .orientacion_declarada_medidas(medidas)
  # `fecha` tambien: `medir()` pone UNA por corrida, y dos corridas con el mismo
  # `id_medicion` y fechas distintas se mezclaban en silencio -el agregado
  # publicaba una sola fecha para medidas de dos momentos-.
  campos_unicos <- c(
    "id_medicion", "fecha", "metrica", "metrica_especifica", "granularidad",
    "tipo_resultado", "orientacion"
  )
  no_unicos <- campos_unicos[vapply(
    .seleccionar_columnas(medidas, campos_unicos),
    function(x) length(unique(x)) != 1L, logical(1L)
  )]
  if (length(no_unicos)) {
    # Dos corridas no son una: el agregado describe un momento. El mensaje decia
    # solo "deben compartir: fecha", y la ayuda de `organizacion()` prometia
    # reunir colecciones de momentos distintos. Ronda 24.
    corridas <- if (any(c("id_medicion", "fecha") %in% no_unicos)) {
      paste0(
        " Las medidas vienen de ", length(unique(paste(medidas$id_medicion,
                                                       medidas$fecha))),
        " corridas: un agregado describe un momento, as\u00ed que sus partes ",
        "se miden en una misma corrida, y las de momentos distintos se siguen ",
        "cada una con `historico_calidad()`."
      )
    } else ""
    stop(
      "Las medidas deben compartir: ", paste(no_unicos, collapse = ", "), ".",
      corridas, call. = FALSE
    )
  }
  # Una medida repetida se contaba dos veces, y dos datos con el mismo
  # identificador salian como UNA celda mezclada. `evaluar()` ya lo rechazaba:
  # mismo criterio. Medido en la ronda 23.
  if ("id_medida" %in% names(medidas)) {
    repetidas <- duplicated(.nombres_para_operar(as.character(medidas$id_medida)))
    if (any(repetidas)) {
      stop(
        "Las medidas repiten `id_medida` -la primera, `",
        as.character(medidas$id_medida)[which(repetidas)[[1L]]], "`-: cada ",
        "medida se cuenta una vez. Una medici\u00f3n unida consigo misma, o dos ",
        "corridas con el mismo `id_medicion`, la repiten.", call. = FALSE
      )
    }
  }
  if (!is.numeric(medidas$resultado) || anyNA(medidas$resultado) ||
      any(!is.finite(medidas$resultado)) ||
      any(medidas$resultado < 0 | medidas$resultado > 1)) {
    # El mensaje nombra la metrica y la causa: decia solo "[0, 1]", y la misma
    # metrica no acotada daba otro mensaje segun sus valores -con 0 y 0 la
    # guarda por tipo, con 0 y 91 esta-. Medido en la ronda 23.
    nombres <- if ("metrica_instanciada" %in% names(medidas)) {
      medidas$metrica_instanciada
    } else medidas$metrica_especifica
    tipo <- as.character(medidas$tipo_resultado[[1L]])
    causa <- if (!is.numeric(medidas$resultado) || anyNA(medidas$resultado)) {
      "trae resultados ausentes -una medida suprimida o que no se midi\u00f3-, que no se agregan"
    } else if (!tipo %in% c("booleano", "real")) {
      paste0("es de resultado '", tipo, "': valores no acotados, no una proporci\u00f3n")
    } else {
      "trae valores fuera de ese rango"
    }
    stop(
      "Los resultados que se agregan deben estar en [0, 1]: la m\u00e9trica '",
      paste(.identificadores_unicos(as.character(nombres)), collapse = "', '"),
      "' ", causa, ".", call. = FALSE
    )
  }
  medidas$orientacion <- .orientacion_medidas(medidas)
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
  # La clave se arma con el CODIGO de cada nivel, no con su nombre:
  # `interaction()` pega los nombres con un punto, y la tabla `ventas.total` con
  # la columna `mes` y la tabla `ventas` con la columna `total.mes` caian en el
  # mismo grupo -un solo atributo con 0,625 donde habia dos, 1 y 0,25-. Los
  # codigos van rellenados para que el orden lexicografico siga siendo el de los
  # niveles. Medido en la ronda 24.
  clave <- do.call(
    interaction,
    c(lapply(claves, function(x) {
      valores <- if (is.character(x) || is.factor(x)) {
        .nombres_para_operar(as.character(x))
      } else x
      sprintf("%09d", as.integer(addNA(as.factor(valores))))
    }),
      list(drop = TRUE, lex.order = TRUE))
  )
  split(seq_len(nrow(medidas)), clave, drop = TRUE)
}

.calcular_agregacion <- function(valores, funcion, umbral, pesos) {
  switch(
    funcion,
    ratio = mean(valores == 1),
    # Con tolerancia de redondeo: `1 - 0.9` da 0,0999...9, que se imprime 0,1 y
    # no alcanzaba el umbral 0,1. Medido en la ronda 23.
    ratio_umbral = mean(valores >= umbral - 64 * .Machine$double.eps),
    promedio = mean(valores),
    promedio_ponderado = sum(valores * pesos)
  )
}

# Las partes de un objeto de varias, unidas con coma. Un nombre que ya trae una
# coma va entre comillas invertidas: `a, b` y `c` daban "a, b, c", lo mismo que
# `a` y `b, c`, y el tablero no podia distinguir las dos celdas. Ronda 24.
.unir_nombres_partes <- function(x) {
  x <- .identificadores_ordenados(x)
  con_coma <- grepl(",", x, fixed = TRUE)
  x[con_coma] <- paste0("`", x[con_coma], "`")
  paste(x, collapse = ", ")
}

.objeto_agregado <- function(medidas, indices, destino) {
  entidades <- .identificadores_unicos(medidas$entidad[indices])
  switch(
    destino,
    atributo = paste0(entidades[[1L]], "$", medidas$atributo[indices[[1L]]]),
    instanciaEntidad = paste0(
      entidades[[1L]], "[", medidas$fila[indices[[1L]]], ",]"
    ),
    entidad = entidades[[1L]],
    .unir_nombres_partes(entidades)
  )
}
# Estos dos ayudantes van ANTES del bloque `roxygen` de `agregar()`, y no entre
# el bloque y su definicion. Ponerlos en el medio hizo que `roxygen` le pegara
# el `@export` y la documentacion de `agregar()` al ayudante: `agregar()` dejo
# de exportarse y un interno con punto quedo exportado. La suite no lo vio
# porque `pkgload::load_all()` expone todo; en un paquete instalado
# `lupa::agregar()` no habria existido.

# El objeto de un `id_medida` agregado. Cada nombre se escapa -la barra
# invertida, el punto, la coma, `#`, `@` y los corchetes- antes de unirlo, para
# que la tabla `ventas.total` con la columna `mes` y la tabla `ventas` con la
# columna `total.mes` no den el mismo identificador. Y los niveles de arriba
# nombran sus PARTES: el conjunto se llamaba siempre `conjuntoColecciones`, una
# coleccion sin nombre siempre `coleccion`, y dos objetos distintos salian con
# el mismo `id_medida` -la union se rechazaba como "unida consigo misma"-.
# Medido en la ronda 24.
.escapar_parte_id <- function(x) {
  gsub("([\\]\\[\\\\.,#@])", "\\\\\\1", as.character(x), perl = TRUE)
}

.id_objeto_agregado <- function(medidas, indices, destino, nombre = NULL) {
  primera <- indices[[1L]]
  partes <- function() {
    paste0("[", paste(.escapar_parte_id(.identificadores_ordenados(
      .identificadores_unicos(medidas$entidad[indices])
    )), collapse = ","), "]")
  }
  switch(
    destino,
    atributo = paste0(
      .escapar_parte_id(medidas$entidad[[primera]]), ".",
      .escapar_parte_id(medidas$atributo[[primera]])
    ),
    instanciaEntidad = paste0(
      .escapar_parte_id(medidas$entidad[[primera]]), "#", medidas$fila[[primera]]
    ),
    entidad = .escapar_parte_id(medidas$entidad[[primera]]),
    coleccion = ,
    organizacion = paste0(.escapar_parte_id(nombre), partes()),
    partes()
  )
}

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
    # El filtro por `alcance == "tabla"` decide cuales se DECLARAN, y esta bien:
    # una tabla vacia si se perfilo, asi que no falta como tabla. Pero se
    # llevaba tambien los MOTIVOS, y ahi perdia: si esa tabla despues no aporta
    # una medida, su motivo especifico -"La tabla tiene cero filas: se leyo su
    # estructura pero no hay nada que medir"- se reemplazaba por el generico "no
    # hay una medida de esta tabla en la entrada". Las dos frases son ciertas y
    # la segunda dice menos: una tabla que no existe y una tabla vacia no son el
    # mismo problema, y el objeto de una capa antes las distinguia.
    # Una tabla puede traer VARIAS filas de cobertura, y no todas explican por que
    # no puede aportar: `muestra_no_solicitada` habla de la configuracion elegida
    # y le toca hasta a las tablas que si se midieron. Quedarse con la primera
    # publicaba esa -medido-, que es peor que el generico. Sirven las que hablan
    # de la tabla entera: no se pudo perfilar (`tabla`) o no hay nada que medir
    # (`tabla_vacia`). Lo demas cae al generico, que al menos no desvia.
    # Los SEIS alcances que `perfilar_coleccion()` puede emitir, y por que cada
    # uno entra o no. La lista estaba a medias -faltaba `medicion_incompleta`- y
    # la enumere despues de escribirla, que es al reves: una lista incompleta no
    # se ve, porque el caso que falta cae al motivo generico sin hacer ruido.
    #
    #   tabla                 -> SI. No se pudo perfilar: no hay nada que aportar.
    #   tabla_vacia           -> SI. Cero filas: no hay nada que medir.
    #   medicion_incompleta   -> SI. Se perfilo pero no se pudo medir la
    #                            proporcion de ausentes en NINGUNA columna. Es el
    #                            analogo, a nivel coleccion, del perfil que no
    #                            pudo resumir ninguna columna.
    #   metricas              -> NO. Metricas sueltas rechazadas sobre una tabla
    #                            que si se perfilo: es parcial, no total.
    #   muestra_no_solicitada -> NO. Decision de configuracion; le toca tambien a
    #                            las tablas que si se midieron.
    #   muestra_no_disponible -> NO. Fallo la consulta de muestra y el resumen SQL
    #                            sobrevivio: la tabla puede aportar igual.
    explican_la_tabla <- c("tabla", "tabla_vacia", "medicion_incompleta")
    con_motivo <- if (is.null(faltantes$alcance)) {
      faltantes
    } else {
      faltantes[faltantes$alcance %in% explican_la_tabla, , drop = FALSE]
    }
    if (!is.null(faltantes$alcance)) {
      faltantes <- faltantes[faltantes$alcance == "tabla", , drop = FALSE]
    }
    # Con el catalogo: la identidad completa es `catalogo.esquema.tabla`, y
    # reconstruirla sin el catalogo publica otra tabla.
    ids_faltantes <- identificador(
      faltantes$esquema, faltantes$tabla, faltantes$catalogo
    )
    ids_con_motivo <- identificador(
      con_motivo$esquema, con_motivo$tabla, con_motivo$catalogo
    )
    primeros <- !duplicated(.nombres_para_operar(ids_con_motivo))
    return(list(
      nombre = coleccion$meta$nombre,
      declaradas = c(
        coleccion$resumen_coleccion$identificador, ids_faltantes
      ),
      motivo_faltantes = stats::setNames(
        con_motivo$motivo[primeros], ids_con_motivo[primeros]
      )
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
.cobertura_agregacion_coleccion <- function(frontera, entidades_medidas,
                                            cobertura_metricas = NULL) {
  declaradas <- .identificadores_unicos(frontera$declaradas)
  medidas <- .identificadores_unicos(entidades_medidas)
  sin_medir <- .identificadores_setdiff(declaradas, medidas)
  indices_sin_medir <- .indice_identificador(
    sin_medir, names(frontera$motivo_faltantes)
  )
  motivos <- unname(frontera$motivo_faltantes[indices_sin_medir])
  motivos[is.na(motivos)] <-
    "No hay una medida de esta tabla en la entrada; no se midio en este alcance."
  # Con una frontera de `coleccion()` -una lista de nombres, sin perfil- el motivo
  # era siempre el generico, y una tabla VACIA y una que no existe salian con la
  # misma frase. Pero la medicion SI sabe por que no aporto una tabla que estaba en
  # el modelo: lo dejo en `cobertura_metricas` -"la entidad `b` tiene cero filas"-,
  # y ese atributo viaja hasta aca. Se usa donde el motivo es el generico, y solo
  # el de la tabla que no aporto. Una tabla que no esta en el modelo sigue con el
  # generico, que es cierto: la entrada no trae nada de ella y no se sabe mas.
  generico <- c(
    "No hay una medida de esta tabla en la entrada.",
    "No hay una medida de esta tabla en la entrada; no se midio en este alcance."
  )
  if (inherits(cobertura_metricas, "data.frame") && nrow(cobertura_metricas) &&
      all(c("entidad", "motivo") %in% names(cobertura_metricas))) {
    for (i in which(motivos %in% generico)) {
      fila <- .indice_identificador(
        sin_medir[[i]], as.character(cobertura_metricas$entidad)
      )
      if (!is.na(fila)) {
        motivos[[i]] <- paste0(
          "Ninguna m\u00e9trica de esta tabla aport\u00f3 una medida. ",
          as.character(cobertura_metricas$motivo[[fila]])
        )
      }
    }
  }
  list(
    coleccion = frontera$nombre,
    tablas_declaradas = length(declaradas),
    tablas_en_el_numero = length(.identificadores_intersect(declaradas, medidas)),
    tablas_sin_medir = sin_medir,
    motivo_sin_medir = motivos,
    cobertura = if (length(declaradas)) {
      length(.identificadores_intersect(declaradas, medidas)) / length(declaradas)
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
  desde_objeto <- vapply(colecciones, function(x) {
    if (inherits(x, "coleccion_lupa")) x$nombre else x$meta$nombre
  }, character(1L))
  nombres <- names(colecciones)
  if (is.null(nombres) || anyNA(nombres) || any(!nzchar(nombres))) {
    nombres <- desde_objeto
  }
  if (anyNA(nombres) || any(!nzchar(nombres)) ||
      anyDuplicated(.nombres_para_operar(nombres))) {
    stop(
      "`colecciones` debe tener nombres unicos y no vacios; esos nombres son",
      " la identidad de cada coleccion en el conjunto.", call. = FALSE
    )
  }
  names(colecciones) <- nombres
  # El nombre de lista manda, y el del objeto -el que `agregar()` escribio en la
  # coleccion- la sigue reconociendo: el alias que ya tenia el conjunto de
  # organizaciones. Sin el, la lista que renombraba sus colecciones abortaba.
  # Medido en la ronda 24.
  list(nombre = "conjuntoColecciones", declaradas = nombres,
       alias = unname(desde_objeto))
}

# La cobertura de una frontera declarada es siempre la misma pregunta -cuantas
# de las partes declaradas entraron en el numero- y cambia solo como se llama la
# parte. Se generaliza para que los cuatro niveles con frontera la respondan
# igual, en vez de tener cuatro copias que se desincronizan.
.cobertura_frontera_declarada <- function(frontera, entidades_medidas, parte) {
  declaradas <- .identificadores_unicos(frontera$declaradas)
  medidas <- .identificadores_unicos(entidades_medidas)
  presentes <- .identificadores_intersect(declaradas, medidas)
  sin_medir <- .identificadores_setdiff(declaradas, medidas)
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

# El alcance de las medidas se REEXPRESA en la clave del agregado; no se arrastra
# tal cual.
#
# La tabla que pone `medir()` esta indexada por `metrica_instanciada` -por
# ejemplo `Formato@t.cod`- y el agregado renombra la metrica a
# `agregada:ratio:Formato`, asi que la tabla arrastrada quedaba cierta sobre una
# clave que no aparece en ninguna fila del objeto: lo atrapo la prueba que exigia
# `alcance$metrica_instanciada %in% resultado$metrica_instanciada`, y era FALSE.
# Una declaracion que no se puede atribuir a ninguna fila no cumple la promesa,
# que es distinguir el agregado de tres celdas del de cuatro.
#
# Se suma por grupo -el MISMO `grupos` con el que se calculo cada numero- y se
# exige que la unidad sea unica dentro del grupo: sumar celdas con filas daria un
# total que no esta en ninguna unidad. Cuando no lo es, se declara la mezcla en
# lugar de sumar.
.alcance_agregado <- function(alcance, medidas, resultado, grupos) {
  # Desde la base -celdas o filas- una medida es una unidad del universo, asi
  # que las partes completas se cuentan aunque `medir()` no publique alcance
  # para ellas. Mas arriba, una medida es un agregado de muchas, y sus unidades
  # solo se conocen por las `completas` del paso anterior. Antes, sin ninguna
  # parte parcial, se devolvia NULL y las completas se perdian: el conjunto de
  # una coleccion completa y una parcial publicaba "5 de 9" donde eran 12 de 16.
  # Medido en la ronda 24.
  desde_la_base <- NROW(medidas) > 0L && all(
    as.character(medidas$granularidad) %in% c("instanciaAtributo", "instanciaEntidad")
  )
  unidad_base <- if (desde_la_base &&
                     all(as.character(medidas$granularidad) == "instanciaAtributo")) {
    "celda"
  } else "fila"
  if (is.null(alcance) || !NROW(alcance)) {
    completas_previas <- if (is.null(alcance)) NULL else attr(alcance, "completas", exact = TRUE)
    hay_previas <- inherits(completas_previas, "data.frame") && NROW(completas_previas) > 0L
    if (!desde_la_base && !hay_previas) return(NULL)
    vacia <- data.frame(
      metrica_instanciada = character(), entidad = character(),
      atributo = character(), unidad = character(), en_el_universo = numeric(),
      medidas = numeric(), motivo = character(), stringsAsFactors = FALSE
    )
    if (hay_previas) attr(vacia, "completas") <- completas_previas
    alcance <- vacia
  }
  # El alcance se empareja con las medidas del grupo por el PAR metrica y
  # entidad, no por la metrica sola. Desde el segundo nivel todas las entidades
  # comparten el nombre de la metrica agregada -`agregada:ratio:Formato`-, y
  # emparejar por el nombre le atribuia a cada entidad la suma de todas: "6 de 8"
  # en cada una cuando cada una midio 3 de 4, y se duplicaba en cada nivel.
  #
  # Y por el ATRIBUTO cuando lo hay: desde el segundo nivel las filas de dos
  # columnas comparten metrica y entidad, y sin el atributo no se distinguian.
  par <- function(metrica, entidad, atributo) {
    atributo <- as.character(atributo)
    atributo[is.na(atributo)] <- ""
    .clave_bytes(paste(
      .nombres_para_operar(as.character(metrica)),
      .nombres_para_operar(as.character(entidad)),
      .nombres_para_operar(atributo), sep = "\r"
    ))
  }
  # Las partes COMPLETAS del paso anterior viajan aparte, en un atributo de esta
  # misma tabla: no se publican como alcance parcial, pero el paso siguiente las
  # necesita para sumar.
  completas_previas <- attr(alcance, "completas", exact = TRUE)
  if (inherits(completas_previas, "data.frame") && nrow(completas_previas)) {
    comunes <- intersect(names(alcance), names(completas_previas))
    alcance <- rbind(
      .seleccionar_columnas(alcance, comunes),
      .seleccionar_columnas(completas_previas, comunes)
    )
  }
  clave_alcance <- par(alcance$metrica_instanciada, alcance$entidad, alcance$atributo)
  clave_medidas <- par(medidas$metrica_instanciada, medidas$entidad, medidas$atributo)
  unidades_declaradas <- unique(as.character(alcance$unidad))
  if (!length(unidades_declaradas) && desde_la_base) unidades_declaradas <- unidad_base
  # A una FILA -`instanciaEntidad` desde celdas- no se le atribuye el alcance
  # de la columna entera: cada fila cuenta cuantas de las instancias de su tabla
  # tienen medida en ella, y se declaran las que no estan completas. Antes las
  # filas completas decian "5 de 6" -lo de la columna- y la unica parcial no
  # decia nada. Medido en la ronda 23.
  por_fila <- identical(as.character(resultado$granularidad[[1L]]), "instanciaEntidad") &&
    all(as.character(medidas$granularidad) == "instanciaAtributo")
  instancias_por_entidad <- tapply(
    clave_medidas, .nombres_para_operar(as.character(medidas$entidad)),
    function(v) length(unique(v))
  )
  fila_alcance <- function(k, medidas_grupo, universo_grupo, unidades) {
    homogenea <- length(unidades) == 1L
    data.frame(
      metrica_instanciada = resultado$metrica_instanciada[[k]],
      entidad = resultado$entidad[[k]],
      atributo = resultado$atributo[[k]],
      unidad = if (homogenea) unidades else NA_character_,
      en_el_universo = if (homogenea) universo_grupo else NA_real_,
      medidas = if (homogenea) medidas_grupo else NA_real_,
      motivo = if (homogenea) {
        paste0(
          "El agregado se calcul\u00f3 sobre ", medidas_grupo, " de ",
          universo_grupo, " ", unidades, " del universo aplicable: las que no ",
          "tienen valor no producen medida y no cuentan como incumplimiento."
        )
      } else {
        paste0(
          "Las partes declaran unidades distintas (",
          paste(unidades, collapse = ", "), "): no se suman, porque el total ",
          "no estar\u00eda en ninguna unidad."
        )
      },
      stringsAsFactors = FALSE
    )
  }
  filas <- lapply(seq_along(grupos), function(k) {
    indices <- grupos[[k]]
    if (por_fila) {
      entidad <- .nombres_para_operar(as.character(medidas$entidad[indices[[1L]]]))
      esperadas <- unname(instancias_por_entidad[entidad])
      presentes <- length(unique(clave_medidas[indices]))
      if (is.na(esperadas)) return(NULL)
      salida <- fila_alcance(k, presentes, esperadas, unidades_declaradas)
      if (presentes >= esperadas) {
        attr(salida, "completa") <- TRUE
      } else if ("fila" %in% names(resultado) && !is.na(resultado$fila[[k]])) {
        salida$motivo <- paste0("Fila ", resultado$fila[[k]], ": ", salida$motivo)
      }
      return(salida)
    }
    instancias <- unique(clave_medidas[indices])
    declaradas <- instancias[instancias %in% clave_alcance]
    if (!length(declaradas)) {
      # Completa: se guarda para el paso siguiente, no se publica.
      completa <- fila_alcance(k, length(indices), length(indices),
                               unidades_declaradas[1L])
      attr(completa, "completa") <- TRUE
      return(completa)
    }
    cuales <- clave_alcance %in% declaradas
    unidades <- unique(as.character(alcance$unidad[cuales]))
    # Las instancias COMPLETAS del grupo no tienen fila de alcance -`medir()`
    # solo la publica para la parcial- y suman sus medidas como medidas y como
    # universo. Antes no sumaban: "5 de 6" donde el numero uso 11 de 12.
    completas <- setdiff(instancias, declaradas)
    n_completas <- sum(clave_medidas[indices] %in% completas)
    fila_alcance(
      k,
      sum(alcance$medidas[cuales]) + n_completas,
      sum(alcance$en_el_universo[cuales]) + n_completas,
      unidades
    )
  })
  filas <- filas[!vapply(filas, is.null, logical(1L))]
  if (!length(filas)) return(NULL)
  es_completa <- vapply(filas, function(f) isTRUE(attr(f, "completa", exact = TRUE)),
                        logical(1L))
  parciales <- filas[!es_completa]
  # Una fila "completa" del paso anterior con medidas == universo tampoco se
  # publica como parcial.
  salida <- if (length(parciales)) do.call(rbind, parciales) else NULL
  if (!is.null(salida)) {
    sigue_parcial <- is.na(salida$medidas) | salida$medidas < salida$en_el_universo
    completas_suma <- salida[!sigue_parcial, , drop = FALSE]
    salida <- salida[sigue_parcial, , drop = FALSE]
  } else completas_suma <- NULL
  completas <- c(filas[es_completa], if (!is.null(completas_suma) && nrow(completas_suma)) {
    list(completas_suma)
  })
  tabla_completas <- if (length(completas)) {
    tabla <- do.call(rbind, lapply(completas, function(f) {
      attr(f, "completa") <- NULL
      f
    }))
    rownames(tabla) <- NULL
    tabla
  }
  # Sin partes parciales no se publica alcance -todo esta completo-, pero las
  # completas viajan igual, en una tabla vacia, para que el paso siguiente sume.
  if (is.null(salida) || !nrow(salida)) {
    if (is.null(tabla_completas)) return(NULL)
    salida <- tabla_completas[0L, , drop = FALSE]
  }
  rownames(salida) <- NULL
  if (!is.null(tabla_completas)) attr(salida, "completas") <- tabla_completas
  salida
}

# Lo que una agregacion arrastra de la medicion que la alimenta, y lo que NO.
#
# Las dos listas viven juntas y la suite las recorre contra los atributos que
# `medir()` pone de verdad: si aparece uno que no esta en ninguna, la prueba
# falla y hay que decidir. Antes esto era una sola lista dentro de `agregar()`,
# y tres atributos se perdieron de a uno -`cobertura_metricas`, despues
# `alcance_medidas` y `fecha_declarada`- cada vez con el mismo diagnostico
# escrito al lado: la declaracion existe y el paso siguiente la tira.
.ATRIBUTOS_TRASLADADOS_AGREGACION <- c(
  "configuracion_modelo", "configuracion_aplicabilidad", "cobertura_metricas",
  "marco_calidad",
  # Una fila por metrica instanciada, con `entidad` y `atributo`: apareo con las
  # filas del agregado y sigue siendo cierto de las medidas que lo alimentaron.
  "alcance_medidas",
  # Sin esto la guarda del orden temporal se apaga y el delta sale con el signo
  # al reves. Ver `.exigir_orden_temporal()`.
  "fecha_declarada"
)

# Los que a proposito NO viajan, cada uno con su motivo. Estar aca es una
# decision declarada, no un olvido.
.ATRIBUTOS_NO_TRASLADADOS_AGREGACION <- c(
  # Las coberturas de frontera las calcula `agregar()` para SU destino, con las
  # partes que entraron a ESTE numero: arrastrar la del paso anterior publicaria
  # la cobertura de otro universo.
  "cobertura_coleccion", "cobertura_conjunto_colecciones",
  "cobertura_organizacion", "cobertura_conjunto_organizaciones"
)

# Una agregacion hereda lo que sus partes declararon. Sin esto, la cobertura de
# cada organizacion se perdia al armar el conjunto y el numero de arriba salia
# diciendo que estaba completo.
# Una parte con peso cero entra al numero sin aportarle nada, y la cobertura la
# contaba como si hubiera entrado. No es falso -entro- pero leerlo sin saber que
# no pesa es leer otra cosa. Se declara.
.declarar_partes_sin_peso <- function(resultado, medidas, destino, pesos,
                                      etiquetas = NULL) {
  if (is.null(pesos)) return(resultado)
  # `etiquetas` es la parte a la que corresponde cada peso -entidad u objeto
  # medible, segun con que nombres se declararon-. Sin ella el peso publicado no
  # vuelve a su parte, y la declaracion del peso cero nombraba `medidas$entidad`
  # incluso cuando los pesos se habian declarado por objeto.
  if (is.null(etiquetas)) etiquetas <- medidas$entidad
  # Los pesos son del que llama y se PUBLICAN: sin ellos en el objeto, el numero
  # no se puede rehacer, que es justo lo que el nivel promete.
  publicados <- pesos
  names(publicados) <- as.character(etiquetas)
  attr(resultado, "pesos_declarados") <- publicados
  sin_peso <- .identificadores_unicos(
    etiquetas[!is.na(pesos) & pesos == 0]
  )
  if (!length(sin_peso)) return(resultado)
  # La declaracion se decide por la PROPIEDAD -"alguna parte pesa cero"- y no por
  # el destino. Antes colgaba de un atributo de cobertura, y en los destinos que
  # no tienen ninguno -`conjuntoEntidades`, entre otros- no habia donde colgarla:
  # el peso cero entraba al numero, quedaba contado en la identidad de la fila
  # (`entidad = "t1, t2"`) y nada lo decia. Medido: el MISMO peso cero se
  # declaraba a nivel `coleccion` y se callaba a nivel `conjuntoEntidades`.
  attr(resultado, "partes_con_peso_cero") <- sin_peso
  propio <- switch(
    destino,
    organizacion = "cobertura_organizacion",
    conjuntoOrganizaciones = "cobertura_conjunto_organizaciones",
    coleccion = "cobertura_coleccion",
    conjuntoColecciones = "cobertura_conjunto_colecciones",
    NULL
  )
  # Y donde hay cobertura de frontera, la declaracion tambien entra ahi, porque
  # es la que se imprime y la que viaja al informe.
  if (is.null(propio) || is.null(attr(resultado, propio, exact = TRUE))) {
    return(resultado)
  }
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

.advertencia_agregacion <- function(medidas, destino, funcion) {
  if (destino != "conjuntoEntidades" || funcion != "promedio") {
    return(NA_character_)
  }
  paste0(
    "promedio_sin_pesos: el alcance de cada parte no es conocido por agregar()",
    "."
  )
}

# Las cuatro coberturas de frontera, en un solo lugar: las leen `agregar()`,
# `rbind()` de mediciones y `evaluar()`.
.ATRIBUTOS_COBERTURA_FRONTERA <- c(
  "cobertura_coleccion", "cobertura_conjunto_colecciones",
  "cobertura_organizacion", "cobertura_conjunto_organizaciones"
)

# Una lista de coberturas sin repetidas: la misma parte puede llegar por dos
# caminos -el atributo directo y `cobertura_de_partes`- y se cuenta una vez. Los
# nombres de la lista son el atributo de origen y PUEDEN repetirse: dos
# colecciones unidas con `rbind()` traen dos `cobertura_coleccion`.
.coberturas_sin_repetir <- function(coberturas) {
  if (!length(coberturas)) return(list())
  repetida <- vapply(seq_along(coberturas), function(i) {
    i > 1L && any(vapply(coberturas[seq_len(i - 1L)], identical, logical(1L),
                         coberturas[[i]]))
  }, logical(1L))
  coberturas[!repetida]
}

# Como se llama la parte de una cobertura, para decir cual vino incompleta. Se
# publicaba el nombre del ATRIBUTO -"cobertura_coleccion"-, no el de la parte.
.etiqueta_cobertura_parte <- function(atributo, cobertura, inicial = FALSE) {
  nombre <- if (!is.null(cobertura$coleccion)) cobertura$coleccion else cobertura$conjunto
  nombre <- if (is.null(nombre) || !length(nombre)) "" else as.character(nombre[[1L]])
  # Al principio de una frase -el motivo de cada parte sin medir- va con
  # mayuscula. Se arma del prefijo fijo y no cortando el texto: el nombre es del
  # usuario y puede no ser UTF-8 valido.
  switch(
    atributo,
    cobertura_coleccion = paste0(
      if (inicial) "Colecci\u00f3n " else "colecci\u00f3n ", nombre
    ),
    cobertura_organizacion = paste0(
      if (inicial) "Organizaci\u00f3n " else "organizaci\u00f3n ", nombre
    ),
    cobertura_conjunto_colecciones = if (inicial) {
      "Conjunto de colecciones"
    } else "conjunto de colecciones",
    cobertura_conjunto_organizaciones = if (inicial) {
      "Conjunto de organizaciones"
    } else "conjunto de organizaciones",
    atributo
  )
}

.heredar_cobertura_de_partes <- function(resultado, medidas, destino) {
  heredadas <- list()
  for (nombre in .ATRIBUTOS_COBERTURA_FRONTERA) {
    previa <- attr(medidas, nombre, exact = TRUE)
    if (!is.null(previa)) heredadas <- c(heredadas, stats::setNames(list(previa), nombre))
  }
  anteriores <- attr(medidas, "cobertura_de_partes", exact = TRUE)
  if (!is.null(anteriores)) heredadas <- c(anteriores, heredadas)
  heredadas <- .coberturas_sin_repetir(heredadas)
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
  cobertura$partes_incompletas <- unname(mapply(
    .etiqueta_cobertura_parte, names(heredadas)[parciales], heredadas[parciales]
  ))
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
#' reales. Los promedios aceptan **esos dos tipos y ningún otro**, y siempre
#' producen resultado real en `[0, 1]`. Los tres tipos no acotados
#' —`numero_real`, `entero` y `duracion`— se rechazan nombrando la métrica que
#' los declara: el promedio de una duración en días es un número de días, y
#' publicarlo como resultado real en `[0, 1]` lo presentaría como una
#' proporción. La guarda es por **tipo declarado**, no por el valor observado:
#' antes una duración de `0,25` y `0,75` días pasaba y la misma métrica con
#' `1,5` días abortaba, así que el mismo modelo cambiaba de conducta según los
#' datos que le tocaran. Para el promedio ponderado, los pesos deben estar en
#' `[0, 1]` y sumar uno dentro de cada objeto de destino. La columna
#' `orientacion` se conserva sin invertir el resultado: un ratio de una métrica
#' de defecto sigue siendo la proporción de defectos.
#'
#' @section Lo que el agregado conserva de la medición:
#'
#' Una agregación hereda lo que sus partes declararon: `configuracion_modelo`,
#' `configuracion_aplicabilidad`, `cobertura_metricas`, `marco_calidad` y
#' `fecha_declarada` viajan en el objeto. Lo último no es decorativo: sin
#' `fecha_declarada`, la guarda que impide comparar dos entregas en orden
#' invertido queda desactivada y [comparar_evaluaciones()] publicaría el delta
#' con el signo al revés.
#'
#' `alcance_medidas` —«midió tres celdas de cuatro»— también viaja, pero
#' **reexpresado en la clave del agregado**: la tabla que publica [medir()] está
#' indexada por `metrica_instanciada` (`Formato@t.cod`) y el agregado renombra la
#' métrica (`agregada:ratio:Formato`), así que arrastrarla tal cual dejaba una
#' declaración que no se podía atribuir a ninguna fila. Los conteos se **suman**
#' por objeto de destino, y sólo cuando todas las partes declaran la misma
#' unidad; si no, la fila declara la mezcla en lugar de publicar un total que no
#' estaría en ninguna unidad. Las partes **completas** también suman —una
#' columna de 6 de 6 junto a una de 5 de 6 da 11 de 12—, también cuando se
#' agregaron por separado y se unieron con `rbind()`; y en un destino por
#' fila (`instanciaEntidad`) cada fila cuenta sus propias celdas: se declaran
#' sólo las incompletas, con su número de fila en el motivo.
#'
#' Una medida repetida —el mismo `id_medida` dos veces, como deja una medición
#' unida consigo misma— se rechaza, como en [evaluar()]. El `id_medida` de cada
#' fila agregada nombra lo que agrega —destino, función, métrica y objeto, como
#' `M-agg-atributo-ratio-NoNulo@t.x`—, así que dos partes agregadas por separado
#' se pueden unir y subir de nivel, y el mismo agregado repetido sigue
#' repitiendo su identificador. Cada nombre lleva escapados los signos que lo
#' separan del siguiente —el punto, la coma—, y desde la colección para arriba
#' el identificador nombra también las partes que reúne: dos conjuntos
#' distintos, o dos colecciones sin nombre, no comparten identificador. Un valor que no se
#' puede agregar se rechaza nombrando la métrica y la causa: un tipo no
#' acotado, una medida ausente o suprimida, o un valor fuera de `[0, 1]`.
#' `umbral` sólo se acepta con `ratio_umbral`, y el borde se compara con
#' tolerancia de redondeo: `1 - 0.9` alcanza el umbral `0.1`.
#'
#' Las coberturas de frontera (`cobertura_coleccion` y sus hermanas) **no** se
#' arrastran: cada agregación calcula la de su propio destino con las partes que
#' entraron a ese número, y hereda las de sus partes en `cobertura_de_partes`.
#' Dos partes agregadas por separado y unidas con `rbind()` conservan cada una
#' la suya, en cualquier orden de la unión, y el tablero, el índice, el
#' informe, [evaluar()] e [historico_calidad()] las publican todas. Unidas
#' dos corridas distintas, cada cobertura lleva además su `id_medicion`, y el
#' histórico registra la parte no medida en la corrida a la que le faltó. Una
#' colección lleva en `cobertura_metricas` sólo la de sus propias tablas.
#'
#' @section Los pesos se publican:
#'
#' Cuando hay `pesos`, el objeto los publica en `pesos_declarados`, con el nombre
#' de la parte que recibió cada uno, de modo que el número se pueda rehacer con
#' lo que el objeto trae. Y si alguna parte entró con peso cero, se declara en
#' `partes_con_peso_cero`: entró al número sin aportarle nada, queda contada en
#' la identidad de la fila y leerlo sin saberlo es leer otra cosa. La
#' declaración depende de que **haya** un peso cero, no del destino: antes sólo
#' existía en los cuatro destinos que tienen cobertura de frontera, y en
#' `conjuntoEntidades` el mismo peso cero pasaba en silencio.
#'
#' Cuando el destino es `conjuntoEntidades`, `promedio` combina las partes sin
#' pesos porque las medidas de nivel `entidad` no llevan su cantidad de filas.
#' El resultado lo declara en `advertencia_agregacion`: es un promedio sin pesos
#' y `agregar()` no conoce el alcance de cada parte. En `coleccion`, la frontera
#' se declara y por eso sólo se admite `promedio_ponderado`: sin pesos no se
#' puede decidir cuanto debe aportar cada tabla de esa coleccion.
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
#'   entrada por fila de `medidas`. Si trae nombres, se emparejan con
#'   `objeto_medible` o con el nombre de la parte —el que declaró la frontera,
#'   o el del objeto que la lista renombró— y se falla nombrando lo que sobra o
#'   falta —igual que en [indice_calidad()], también cuando un nombre sólo
#'   difiere en su forma Unicode—, así que la misma declaración escrita en otro
#'   orden da el mismo número. Si los nombres casan con los dos vocabularios y
#'   reparten distinto, se rechaza: no se puede saber a cuáles se refieren. Sin
#'   nombres se leen por posición.
#' @param coleccion Frontera declarada, exigida cuando `destino` es
#'   `"coleccion"`: el objeto de [coleccion()] o el perfil de
#'   [perfilar_coleccion()]. Sin ella no se sabe sobre qué tablas se está
#'   agregando, y el número resultante no describiría nada.
#' @param colecciones Lista nombrada de objetos de [coleccion()] o
#'   [perfilar_coleccion()], exigida cuando `destino` es
#'   `"conjuntoColecciones"`. Los nombres declaran la identidad y la frontera
#'   del conjunto, y mandan sobre el del objeto, que la sigue reconociendo; no
#'   se agregan organizaciones ni otros alcances implícitos.
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
    ajenas <- .identificadores_setdiff(
      .identificadores_unicos(medidas$entidad), coleccion$declaradas
    )
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
    medidas$entidad <- .resolver_partes_frontera(medidas$entidad, conjunto)
    ajenas <- .identificadores_setdiff(
      .identificadores_unicos(medidas$entidad), conjunto$declaradas
    )
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
    medidas$entidad <- .resolver_partes_frontera(medidas$entidad, organismo)
    ajenas <- .identificadores_setdiff(
      .identificadores_unicos(medidas$entidad), organismo$declaradas
    )
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
    # Las medidas pasan a llamarse como las declara la frontera: los pesos, la
    # cobertura y el objeto del agregado hablan ese idioma. Antes solo el chequeo
    # de ajenas lo traducia, y los pesos con el nombre declarado se rechazaban.
    medidas$entidad <- .resolver_partes_frontera(
      medidas$entidad, conjunto_organismos
    )
    ajenas <- .identificadores_setdiff(
      .identificadores_unicos(medidas$entidad),
      conjunto_organismos$declaradas
    )
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
  # Un `umbral` con otra funcion se aceptaba en silencio. Medido en la ronda 23.
  if (!is.null(umbral) && !identical(funcion, "ratio_umbral")) {
    stop(
      "`umbral` s\u00f3lo se usa con `ratio_umbral`; la agregaci\u00f3n pedida es `",
      funcion, "`.", call. = FALSE
    )
  }
  tipo <- unique(medidas$tipo_resultado)
  if (funcion == "ratio" && tipo != "booleano") {
    stop("`ratio` s\u00f3lo admite m\u00e9tricas de resultado booleano.", call. = FALSE)
  }
  # El tipo declarado no alcanza: `ratio` cuenta los unos, asi que un valor que
  # no es 0 ni 1 se contaba como falso en silencio. Medido antes de poner la
  # guarda: 0.5 en una medida booleana devolvia 0.6 sin decir nada, mientras la
  # hermana `promedio` si rechaza lo que sale de su rango. Y medido tambien que
  # ninguna metrica legitima la viola: doce instancias booleanas del nucleo,
  # cero valores fuera de {0, 1}.
  if (funcion == "ratio" && !all(medidas$resultado %in% c(0, 1))) {
    stop(
      "Una medida booleana solo puede valer 0 o 1; `ratio` cuenta los unos y ",
      "no puede contar un valor intermedio.", call. = FALSE
    )
  }
  if (funcion == "ratio_umbral" && tipo != "real") {
    stop("`ratio_umbral` s\u00f3lo admite m\u00e9tricas de resultado real.",
         call. = FALSE)
  }
  # Los tres tipos no acotados -`numero_real`, `entero`, `duracion`- no admiten
  # ninguna de las cuatro agregaciones normalizadas, y eso lo promete `metrica()`.
  # `ratio` y `ratio_umbral` lo cumplian por su guarda de tipo; `promedio` y
  # `promedio_ponderado` no tenian ninguna, y pasaban.
  #
  # La confusion estaba escrita en el comentario de la guarda de `ratio`: "la
  # hermana `promedio` si rechaza lo que sale de su rango". Rechaza VALORES, no
  # tipos, asi que la conducta dependia del dato: una duracion de 0,25 y 0,75
  # dias se promediaba y se publicaba como `tipo_resultado = "real"` -o sea, como
  # una proporcion-, y la misma metrica con 1,5 dias abortaba por el rango. El
  # mismo modelo cambiaba de conducta segun los numeros que le tocaran.
  #
  # Medido sobre la matriz de cuatro agregaciones por cinco tipos, con los
  # valores dentro de [0, 1] para que la guarda por valor no tapara la de tipo:
  # las dos puertas eran `promedio` y `promedio_ponderado`.
  if (funcion %in% c("promedio", "promedio_ponderado") &&
      !tipo %in% c("booleano", "real")) {
    stop(
      "`", funcion, "` no admite m\u00e9tricas de resultado '", tipo,
      "': son valores no acotados y el promedio los publicar\u00eda como una ",
      "proporci\u00f3n en [0, 1]. La m\u00e9trica es '",
      paste(.identificadores_unicos(medidas$metrica_instanciada),
            collapse = "', '"),
      "'.", call. = FALSE
    )
  }
  if (funcion == "ratio_umbral") {
    if (!is.numeric(umbral) || length(umbral) != 1L || is.na(umbral) ||
        !is.finite(umbral) || umbral < 0 || umbral > 1) {
      stop("`umbral` debe ser un n\u00famero entre 0 y 1.", call. = FALSE)
    }
  }
  if (funcion == "promedio_ponderado") {
    # El emparejamiento por nombre va ANTES de la comprobacion de largo: con
    # nombres, un largo equivocado es que falta o sobra una parte, y decir cual
    # es mas util que decir cuantas hay. Medido: `c(t1 = 1)` sobre dos medidas
    # daba "debe tener una entrada por medida" -generico- en vez de "Faltan
    # pesos para: t2", y un nombre que sobraba no se nombraba nunca.
    if (!is.null(names(pesos)) && is.numeric(pesos)) {
      if (any(!nzchar(names(pesos)))) {
        stop(
          "`pesos` mezcla entradas con nombre y sin nombre. Corresponde una ",
          "sola forma: o todas con el nombre de su parte, o todas por ",
          "posici\u00f3n.", call. = FALSE
        )
      }
      # Se acepta cualquiera de las DOS identidades que la medida publica: el
      # `objeto_medible` -que en los niveles altos es la lista de partes unida
      # con coma, una construccion interna- y la `entidad`, que es el nombre que
      # el usuario DECLARO al armar la frontera. Medido al agregar dos
      # colecciones a una organizacion: los pesos escritos `c(padron_a = 0.5,
      # padron_b = 0.5)` -los mismos nombres que `organizacion(colecciones =)`
      # exige- se rechazaban, y habia que escribir `c("t1, t3" = 0.5, ...)`, que
      # nadie declaro en ningun lado. La frontera y sus pesos tienen que hablar
      # el mismo idioma.
      # El nombre del objeto sigue reconociendo a la parte que la lista renombro:
      # los pesos escritos con el se traducen al nombre declarado, como las
      # medidas. Ronda 24.
      frontera_con_alias <- if (!is.null(conjunto)) {
        conjunto
      } else if (!is.null(organismo)) {
        organismo
      } else conjunto_organismos
      if (!is.null(frontera_con_alias$alias)) {
        names(pesos) <- .resolver_partes_frontera(names(pesos), frontera_con_alias)
      }
      partes_medidas <- as.character(medidas$objeto_medible)
      entidades_medidas <- if ("entidad" %in% names(medidas)) {
        as.character(medidas$entidad)
      } else {
        rep(NA_character_, length(partes_medidas))
      }
      por_objeto <- !length(.identificadores_setdiff(partes_medidas, names(pesos)))
      por_entidad <- !anyNA(entidades_medidas) &&
        !length(.identificadores_setdiff(entidades_medidas, names(pesos)))
      # Si los nombres casan con los DOS vocabularios y reparten distinto, no hay
      # como saber a cual se refieren: ganaba `objeto_medible` en silencio, y una
      # coleccion `a` con la tabla `b` recibia el peso escrito para `b`. Medido
      # en la ronda 24: 0,925 publicado donde el peso por coleccion daba 0,325.
      if (por_objeto && por_entidad) {
        segun_objeto <- unname(pesos[.indice_identificador(partes_medidas, names(pesos))])
        segun_entidad <- unname(pesos[.indice_identificador(entidades_medidas, names(pesos))])
        if (!isTRUE(all.equal(segun_objeto, segun_entidad))) {
          stop(
            "Los nombres de `pesos` casan a la vez con los objetos medidos (",
            paste0("`", .identificadores_unicos(partes_medidas), "`", collapse = ", "),
            ") y con las partes declaradas (",
            paste0("`", .identificadores_unicos(entidades_medidas), "`", collapse = ", "),
            "), y reparten distinto: no se puede saber a cu\u00e1les se refieren. ",
            "Decl\u00e1relos por posici\u00f3n, en el orden de las medidas.", call. = FALSE
          )
        }
      }
      partes_elegidas <- if (!por_objeto && por_entidad) {
        entidades_medidas
      } else {
        partes_medidas
      }
      faltan <- .identificadores_setdiff(partes_elegidas, names(pesos))
      sobran <- .identificadores_setdiff(names(pesos), partes_elegidas)
      if (length(faltan) || length(sobran)) {
        # Los nombres se entrecomillan: en los niveles altos una parte se llama
        # `t1, t3`, y una lista separada por comas de nombres que llevan comas
        # nombra cuatro cosas donde hay dos.
        entre_comillas <- function(x) paste0("`", x, "`", collapse = ", ")
        stop(
          if (length(faltan)) {
            paste0("Faltan pesos para: ", entre_comillas(faltan), ".")
          } else "",
          if (length(faltan) && length(sobran)) " " else "",
          if (length(sobran)) {
            paste0("Sobran pesos para: ", entre_comillas(sobran), ".")
          } else "",
          if (!anyNA(entidades_medidas) &&
              length(.identificadores_setdiff(entidades_medidas, partes_medidas))) {
            paste0(
              " Se aceptan los nombres de ", entre_comillas(partes_medidas),
              " o los de ", entre_comillas(entidades_medidas), "."
            )
          } else "",
          .pista_forma_distinta(faltan, sobran),
          call. = FALSE
        )
      }
      claves_pesos <- .nombres_para_operar(names(pesos))
      if (anyDuplicated(claves_pesos)) {
        stop("`pesos` repite una parte: ",
             paste(unique(names(pesos)[duplicated(claves_pesos)]),
                   collapse = ", "), ".", call. = FALSE)
      }
      # Se guarda la parte a la que corresponde cada peso ANTES de que el
      # alineado borre los nombres: un peso publicado sin la etiqueta que
      # vuelve a su parte no se puede leer ni rehacer. Y la etiqueta es
      # `partes_elegidas`, que segun el caso son las entidades o los objetos
      # medibles: usar `medidas$entidad` a ciegas nombraba otra cosa.
      etiquetas_pesos <- partes_elegidas
      pesos <- unname(pesos[.indice_identificador(partes_elegidas, names(pesos))])
    }
    if (!is.numeric(pesos) || length(pesos) != nrow(medidas) || anyNA(pesos) ||
        any(!is.finite(pesos)) || any(pesos < 0 | pesos > 1)) {
      stop("`pesos` debe tener una entrada en [0, 1] por medida.", call. = FALSE)
    }
  }
  # Por la via POSICIONAL -documentada: "sin nombres se leen por posicion"- no hay
  # `partes_elegidas`, y la etiqueta caia al defecto `medidas$entidad`. Sobre un
  # origen `instancia*`, donde la misma entidad ocupa varias filas, eso publicaba
  # seis pesos con DOS nombres: `t1=0.1, t1=0.2, t1=0.3, t1=0.4, t3=0.6, t3=0.4`.
  # Alinear por el nombre publicado -el unico camino que la promesa declara- daba
  # 0,9 contra el 0,8 publicado, y `partes_con_peso_cero` nombraba una entidad que
  # si habia aportado con sus otras filas.
  #
  # La etiqueta correcta es la misma que usa la via nombrada cuando los pesos son
  # por objeto: `objeto_medible`, que es unico por fila. No se elige por defecto
  # `entidad` porque la pregunta es "¿de que parte es este peso?", y con pesos por
  # posicion la parte es la FILA.
  if (!exists("etiquetas_pesos", inherits = FALSE)) etiquetas_pesos <- NULL
  if (!is.null(pesos) && is.null(etiquetas_pesos) &&
      "objeto_medible" %in% names(medidas)) {
    etiquetas_pesos <- as.character(medidas$objeto_medible)
  }
  grupos <- .indices_grupos_agregacion(medidas, destino)
  nombre_frontera <- if (identical(destino, "coleccion")) {
    coleccion$nombre
  } else if (identical(destino, "organizacion")) {
    organismo$nombre
  } else NULL
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
      .unir_nombres_partes(.identificadores_unicos(medidas$entidad[indices]))
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
      advertencia_agregacion = .advertencia_agregacion(
        medidas, destino, funcion
      ),
      stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, partes)
  rownames(resultado) <- NULL
  # El identificador nombra lo que agrega: destino, funcion, metrica y objeto.
  # Numeraba desde 1 en cada llamada, y dos colecciones agregadas por separado
  # -el camino documentado para subir a `conjuntoColecciones`- salian con el
  # mismo `id_medida`: `evaluar()` rechazaba la union. El mismo agregado repetido
  # sigue repitiendo su identificador. Medido en la ronda 23.
  objeto_id <- vapply(grupos, function(indices) {
    .id_objeto_agregado(medidas, indices, destino, nombre_frontera)
  }, character(1L), USE.NAMES = FALSE)
  resultado$id_medida <- paste0(
    resultado$id_medicion, "-agg-", destino, "-", funcion, "-",
    resultado$metrica_especifica, "@", objeto_id
  )
  repetidos <- duplicated(resultado$id_medida) |
    duplicated(resultado$id_medida, fromLast = TRUE)
  if (any(repetidos)) {
    resultado$id_medida[repetidos] <- paste0(
      resultado$id_medida[repetidos], "-", sprintf("%06d", which(repetidos))
    )
  }
  resultado <- resultado[c(
    "id_medida", "id_medicion", "fecha", "metrica", "metrica_especifica",
    "metrica_instanciada", "dimension", "factor", "orientacion", "granularidad",
    "tipo_resultado", "entidad", "atributo", "fila", "objeto_medible",
    "resultado", "agregacion", "advertencia_agregacion"
  )]
  class(resultado) <- c("medicion", "data.frame")
  # `cobertura_metricas` viaja con el numero, igual que las dos configuraciones.
  #
  # `medir()` declara ahi las metricas que NO se pudieron medir y por que -"la
  # entidad dependiente `b` tiene cero filas"-, y `agregar()` lo descartaba en el
  # primer salto. De ahi en mas esa tabla era invisible: el conjunto se reportaba
  # como si nunca hubiera existido, con el mismo valor y la misma evaluacion.
  #
  # Es la misma forma que la cobertura de coleccion y que el alcance de los
  # resumenes: la declaracion existe y el paso siguiente la tira.
  #
  # Y la lista era la guarda, que es lo que fallo dos veces mas: `alcance_medidas`
  # -"midio 3 de 4 celdas"- y `fecha_declarada` quedaron afuera de la lista que
  # arreglaba al hermano. El segundo no es una etiqueta: `.exigir_orden_temporal()`
  # empieza con `if (!isTRUE(declaradas)) return()`, asi que sin el atributo la
  # guarda del orden temporal se APAGA. Medido: las mismas dos fechas declaradas
  # en orden invertido detienen `comparar_evaluaciones()` por el camino de
  # `medir()` y publican `delta = -1` por el camino de `agregar()`.
  #
  # `.ATRIBUTOS_TRASLADADOS_AGREGACION` y `.ATRIBUTOS_NO_TRASLADADOS_AGREGACION`
  # se declaran juntos a proposito: la suite recorre los atributos que `medir()`
  # pone de verdad y exige que cada uno este en una de las dos listas, asi que un
  # atributo nuevo no puede volver a perderse en silencio.
  for (nombre_atributo in .ATRIBUTOS_TRASLADADOS_AGREGACION) {
    valor_atributo <- attr(medidas, nombre_atributo, exact = TRUE)
    if (!is.null(valor_atributo)) attr(resultado, nombre_atributo) <- valor_atributo
  }
  # La coleccion lleva la cobertura de SUS tablas. Medidas tomadas de una
  # `medir()` que reunia dos colecciones traian la de la otra -"la entidad `a3`
  # tiene cero filas" en la coleccion que no tiene `a3`-, y con una regla sin
  # `metricas` el veredicto de una coleccion completa salia NA. Medido en la
  # ronda 24.
  cobertura_metricas <- attr(resultado, "cobertura_metricas", exact = TRUE)
  if (identical(destino, "coleccion") &&
      inherits(cobertura_metricas, "data.frame") && nrow(cobertura_metricas) &&
      "entidad" %in% names(cobertura_metricas)) {
    propias <- .identificadores_en(
      as.character(cobertura_metricas$entidad), coleccion$declaradas
    )
    propias[is.na(propias)] <- FALSE
    cobertura_metricas <- cobertura_metricas[propias, , drop = FALSE]
    rownames(cobertura_metricas) <- NULL
    attr(resultado, "cobertura_metricas") <- if (nrow(cobertura_metricas)) {
      cobertura_metricas
    } else NULL
  }
  # Y el alcance, que es el unico que no viaja igual: se reexpresa en la clave del
  # agregado. Ver `.alcance_agregado()`.
  attr(resultado, "alcance_medidas") <- .alcance_agregado(
    attr(medidas, "alcance_medidas", exact = TRUE), medidas, resultado, grupos
  )
  if (identical(destino, "coleccion")) {
    attr(resultado, "cobertura_coleccion") <- .cobertura_agregacion_coleccion(
      coleccion, medidas$entidad,
      attr(medidas, "cobertura_metricas", exact = TRUE)
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
        conjunto_organismos, medidas$entidad, "organizacion"
      )
  }
  # La cobertura de las partes no se pierde al subir de nivel. Un conjunto
  # armado con una organizacion a la que le falto una coleccion **no esta
  # completo**, y decir cobertura 1 seria informar como completo lo que es
  # parcial: el mismo defecto que el paquete persigue, un piso mas arriba.
  resultado <- .heredar_cobertura_de_partes(resultado, medidas, destino)
  resultado <- .declarar_partes_sin_peso(
    resultado, medidas, destino, pesos, etiquetas_pesos
  )
  resultado
}
