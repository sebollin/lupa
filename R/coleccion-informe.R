# El informe de una coleccion: el indice de tablas con su estado, la matriz
# tabla x factor del marco, lo no medido y los hallazgos. Con cientos de tablas
# el objeto de `perfilar_coleccion()` no alcanzaba para entregar: `reportar()`
# lo rechazaba y no habia una tabla plana que exportar. Cada parte se arma UNA
# vez aca; `as.data.frame()` la devuelve y `reportar()` publica la misma tabla,
# asi que lo que se comparte y lo que se exporta no pueden divergir.
#
# Nada se inventa: el estado de cada tabla sale de `cobertura_coleccion`, los
# numeros de `resumen_coleccion`, la ubicacion en el marco de
# `cobertura_analisis()` de su perfil y los hallazgos de `hallazgos()`. Y nada
# se combina: la jerarquia dimension-factor es taxonomica, ninguna cifra resume
# una tabla ni la coleccion.

.ESTADOS_TABLA_COLECCION <- c(
  "perfilada", "vacia", "medicion_incompleta", "con_error", "no_medida"
)

# Una columna de texto de una tabla que puede no traerla -un objeto guardado
# antes de que existiera, o una tabla de cero filas-: `NA` en su lugar, con el
# largo de la tabla.
.campo_texto_coleccion <- function(tabla, nombre) {
  n <- if (is.data.frame(tabla)) nrow(tabla) else 0L
  if (n && nombre %in% names(tabla)) return(as.character(tabla[[nombre]]))
  rep(NA_character_, n)
}

# Un objeto guardado antes de que `cobertura_coleccion` tuviera `alcance` solo
# listaba las tablas que no se perfilaron: todas sus filas son del alcance
# `tabla`, como las lee la frontera de `agregar()`.
.alcances_cobertura_coleccion <- function(cobertura) {
  if (!is.data.frame(cobertura)) return(character())
  if (is.null(cobertura$alcance)) return(rep("tabla", nrow(cobertura)))
  as.character(cobertura$alcance)
}

# La identidad de cada fila de `cobertura_coleccion`, con el catalogo: la que
# arma `coleccion()` (`.identificadores_tabla()`).
.identificadores_cobertura_coleccion <- function(cobertura) {
  if (!is.data.frame(cobertura) || !nrow(cobertura)) return(character())
  .identificadores_tabla(
    as.character(cobertura$esquema), as.character(cobertura$tabla),
    if (is.null(cobertura$catalogo)) NULL else as.character(cobertura$catalogo)
  )
}

# Las tablas declaradas de un `perfil_coleccion`: las perfiladas, en el orden
# en que se midieron, y despues las del alcance `tabla` -las que no se
# perfilaron-. Cada declarada esta en exactamente una de las dos, que es la
# identidad `n_declaradas = n_perfiladas + n_sin_perfilar` del objeto. La lee la
# frontera de `agregar()` y la lee el informe: una sola regla.
.tablas_declaradas_perfil_coleccion <- function(x) {
  resumen <- x$resumen_coleccion
  cobertura <- x$cobertura_coleccion
  n_resumen <- if (is.data.frame(resumen)) nrow(resumen) else 0L
  faltan <- which(.alcances_cobertura_coleccion(cobertura) %in% "tabla")
  ids_cobertura <- .identificadores_cobertura_coleccion(cobertura)
  data.frame(
    tabla = c(.campo_texto_coleccion(resumen, "tabla"),
              .campo_texto_coleccion(cobertura, "tabla")[faltan]),
    esquema = c(.campo_texto_coleccion(resumen, "esquema"),
                .campo_texto_coleccion(cobertura, "esquema")[faltan]),
    identificador = c(.campo_texto_coleccion(resumen, "identificador"),
                      ids_cobertura[faltan]),
    # Solo se perfila lo declarado `tipo = "tabla"`: el resto se saltea.
    tipo = c(rep("tabla", n_resumen),
             .campo_texto_coleccion(cobertura, "tipo")[faltan]),
    fila_resumen = c(seq_len(n_resumen), rep(NA_integer_, length(faltan))),
    fila_cobertura = c(rep(NA_integer_, n_resumen), faltan),
    stringsAsFactors = FALSE
  )
}

# El perfil conservado de cada tabla del indice, o `NULL`.
.perfiles_por_tabla_coleccion <- function(x, identificadores) {
  perfiles <- x$perfiles
  if (!length(perfiles) || is.null(names(perfiles))) {
    return(vector("list", length(identificadores)))
  }
  posicion <- .indice_nombre(identificadores, names(perfiles))
  lapply(posicion, function(i) if (is.na(i)) NULL else perfiles[[i]])
}

# (a) El indice: una fila por tabla declarada, con su estado y su porque.
#
# El estado lo decide la fila de cobertura que habla de la tabla ENTERA -las
# mismas tres que `.validar_coleccion_destino()` reconoce como "explican la
# tabla"-: `tabla` (no se perfilo: `con_error` si se declaro tabla y el motor o
# el perfil fallaron, `no_medida` si se declaro como otro tipo de objeto y se
# salteo por diseno), `tabla_vacia` y `medicion_incompleta`. Las demas filas
# -metricas sueltas, muestra, permisos, divergencias- describen una tabla que SI
# aporta y quedan nombradas en `alcances_cobertura`, con su detalle en lo no
# medido. Los numeros son los de `resumen_coleccion`, `NA` donde la tabla no se
# perfilo, y los hallazgos se cuentan solo donde hay perfil con muestra y la
# tabla tiene filas: sin perfil, `NA` -no se conto-; y en una tabla vacia el
# perfil no examino nada, asi que su cero se leeria como una tabla limpia.
.indice_tablas_coleccion <- function(x) {
  declaradas <- .tablas_declaradas_perfil_coleccion(x)
  n <- nrow(declaradas)
  cobertura <- x$cobertura_coleccion
  alcances <- .alcances_cobertura_coleccion(cobertura)
  motivos <- .campo_texto_coleccion(cobertura, "motivo")
  remedios <- .campo_texto_coleccion(cobertura, "como_resolverlo")
  posicion <- .indice_nombre(
    .identificadores_cobertura_coleccion(cobertura), declaradas$identificador
  )

  sin_perfilar <- is.na(declaradas$fila_resumen)
  estado <- rep("perfilada", n)
  declarada_otra <- !is.na(declaradas$tipo) & declaradas$tipo != "tabla"
  estado[sin_perfilar] <- ifelse(
    declarada_otra[sin_perfilar], "no_medida", "con_error"
  )
  motivo <- rep(NA_character_, n)
  como_resolverlo <- rep(NA_character_, n)
  motivo[sin_perfilar] <- motivos[declaradas$fila_cobertura[sin_perfilar]]
  como_resolverlo[sin_perfilar] <-
    remedios[declaradas$fila_cobertura[sin_perfilar]]
  for (alcance in c("tabla_vacia", "medicion_incompleta")) {
    filas <- which(alcances %in% alcance & !is.na(posicion))
    filas <- filas[!sin_perfilar[posicion[filas]]]
    destino <- posicion[filas]
    estado[destino] <- if (alcance == "tabla_vacia") "vacia" else alcance
    motivo[destino] <- motivos[filas]
    como_resolverlo[destino] <- remedios[filas]
  }

  con_tabla <- !is.na(posicion)
  grupos <- split(
    alcances[con_tabla], factor(posicion[con_tabla], levels = seq_len(n))
  )
  alcances_cobertura <- vapply(grupos, function(a) {
    if (length(a)) paste(unique(a), collapse = ", ") else NA_character_
  }, character(1L), USE.NAMES = FALSE)

  resumen <- x$resumen_coleccion
  propias <- setdiff(names(resumen), c("tabla", "esquema", "identificador"))
  numeros <- .seleccionar_columnas(resumen, propias, declaradas$fila_resumen)
  sin_listar <- .metricas_sin_listar_coleccion(x, declaradas, numeros)

  n_error <- rep(NA_real_, n)
  n_sospechoso <- rep(NA_real_, n)
  perfiles <- .perfiles_por_tabla_coleccion(x, declaradas$identificador)
  for (i in which(estado != "vacia")) {
    h <- if (is.null(perfiles[[i]])) NULL else .fuente_hallazgos(perfiles[[i]])
    if (!is.data.frame(h) || !"severidad" %in% names(h)) next
    severidad <- as.character(h$severidad)
    n_error[[i]] <- as.numeric(sum(severidad %in% "error"))
    n_sospechoso[[i]] <- as.numeric(sum(severidad %in% "sospechoso"))
  }

  salida <- cbind(
    data.frame(
      tabla = declaradas$tabla, esquema = declaradas$esquema,
      identificador = declaradas$identificador, tipo = declaradas$tipo,
      estado = factor(estado, levels = .ESTADOS_TABLA_COLECCION),
      motivo = motivo, como_resolverlo = como_resolverlo,
      alcances_cobertura = alcances_cobertura,
      stringsAsFactors = FALSE
    ),
    numeros,
    data.frame(
      n_metricas_sin_listar = sin_listar,
      n_hallazgos_error = n_error, n_hallazgos_sospechoso = n_sospechoso
    )
  )
  rownames(salida) <- NULL
  salida
}

# Las metricas no calculadas de cada tabla que `cobertura_metricas` no lista
# porque se recorto a `tope_cobertura_metricas`. El tope es global: corta la
# tabla donde se alcanza y deja sin filas a las siguientes, y el objeto solo lo
# decia en `meta$cobertura_metricas_truncada`, para la coleccion entera -el
# indice decia `perfilada` sin mas-. Se cuenta con lo que el objeto publica:
# la tabla subio `n_metricas_declaradas - n_metricas_calculadas` filas -las no
# calculadas, en el modo `"no_medidas"`- o `n_metricas_declaradas` -todas, en
# `"completa"-, y se compara con las que quedaron, sin las de divergencia, que
# no son metricas. En el modo `"ninguna"` no se lista nada a proposito y en una
# tabla que no se perfilo no hay metricas: `NA`, no 0.
.metricas_sin_listar_coleccion <- function(x, declaradas, numeros) {
  n <- nrow(declaradas)
  modo <- x$meta$cobertura_metricas_modo
  declaradas_metricas <- if ("n_metricas_declaradas" %in% names(numeros)) {
    as.numeric(numeros$n_metricas_declaradas)
  } else rep(NA_real_, n)
  calculadas <- if ("n_metricas_calculadas" %in% names(numeros)) {
    as.numeric(numeros$n_metricas_calculadas)
  } else rep(NA_real_, n)
  esperadas <- if (identical(modo, "completa")) {
    declaradas_metricas
  } else if (identical(modo, "no_medidas")) {
    declaradas_metricas - calculadas
  } else rep(NA_real_, n)
  metricas <- x$cobertura_metricas
  de_metricas <- !.campo_texto_coleccion(metricas, "estado") %in% "divergencia"
  posicion <- .indice_nombre(
    .campo_texto_coleccion(metricas, "identificador")[de_metricas],
    declaradas$identificador
  )
  listadas <- tabulate(posicion[!is.na(posicion)], nbins = n)
  salida <- pmax(esperadas - listadas, 0)
  salida[is.na(declaradas$fila_resumen)] <- NA_real_
  salida
}

# (b) La ubicacion de cada tabla en el marco, en largo: una fila por tabla y
# factor, con el `estado` y el `motivo` de `cobertura_analisis()` sobre su
# perfil -la misma tabla que la seccion de un perfil publica como "Cobertura del
# analisis"-. Sin perfil no hay de donde sacarla, y la celda queda `NA` con su
# porque: inferir el estado desde el resumen SQL afirmaria, por ejemplo, que la
# frescura no aplica cuando lo que falta es saber si hay fechas.
.dimensiones_coleccion <- function(x, indice, marco) {
  factores <- marco$factores
  n_factores <- nrow(factores)
  claves <- .clave_par_identificador(factores$dimension, factores$factor)
  perfiles <- .perfiles_por_tabla_coleccion(x, indice$identificador)
  sin_dato <- function(motivo) {
    list(estado = rep(NA_character_, n_factores),
         motivo = rep(motivo, n_factores))
  }
  bloques <- lapply(seq_len(nrow(indice)), function(i) {
    if (as.character(indice$estado[[i]]) %in% c("con_error", "no_medida")) {
      return(sin_dato(paste(
        "La tabla no se perfil\u00f3: no hay evidencia de ning\u00fan factor.",
        "El motivo est\u00e1 en el \u00edndice de tablas."
      )))
    }
    if (is.null(perfiles[[i]])) {
      return(sin_dato(paste(
        "La colecci\u00f3n no conserv\u00f3 el perfil de esta tabla. Se conserva",
        "con perfilar_coleccion(..., conservar_perfiles = TRUE)."
      )))
    }
    cobertura <- tryCatch(
      cobertura_analisis(perfiles[[i]], modelo = marco),
      error = function(e) e
    )
    if (inherits(cobertura, "error")) {
      return(sin_dato(paste0(
        "No se pudo ubicar el perfil de la tabla en el marco: ",
        conditionMessage(cobertura)
      )))
    }
    orden <- .indice_nombre(
      claves, .clave_par_identificador(cobertura$dimension, cobertura$factor)
    )
    list(estado = as.character(cobertura$estado)[orden],
         motivo = as.character(cobertura$motivo)[orden])
  })
  n <- nrow(indice)
  cada_factor <- function(v) rep(as.character(v), each = n_factores)
  salida <- data.frame(
    tabla = cada_factor(indice$tabla),
    esquema = cada_factor(indice$esquema),
    identificador = cada_factor(indice$identificador),
    estado_tabla = factor(
      cada_factor(indice$estado), levels = .ESTADOS_TABLA_COLECCION
    ),
    marco = rep(as.character(marco$nombre), n * n_factores),
    dimension = rep(as.character(factores$dimension), n),
    factor = rep(as.character(factores$factor), n),
    estado = factor(
      as.character(unlist(lapply(bloques, `[[`, "estado"))),
      levels = .ESTADOS_COBERTURA_ANALISIS
    ),
    motivo = as.character(unlist(lapply(bloques, `[[`, "motivo"))),
    stringsAsFactors = FALSE
  )
  rownames(salida) <- NULL
  salida
}

# (b) La matriz: lo mismo, una fila por tabla y una columna por factor, con el
# nombre `"dimension / factor"`. Dos pares distintos del marco que se escriban
# igual con la barra -un nombre que ya la lleva- se distinguen con
# `make.unique()`; las columnas nunca se funden. Se arma con `list2DF()`: con
# `as.data.frame()` sobre la lista, los nombres pasaban por argumentos de una
# llamada a `data.frame()` y bajo `LC_CTYPE = C` cada factor con tilde del
# marco avisaba "unable to translate ... to native encoding". Medido.
.matriz_coleccion <- function(dimensiones, indice, marco) {
  factores <- marco$factores
  nombres <- make.unique(.pegar_texto(
    factores$dimension, factores$factor, sep = " / "
  ))
  valores <- matrix(
    as.character(dimensiones$estado), nrow = nrow(indice),
    ncol = nrow(factores), byrow = TRUE
  )
  celdas <- lapply(seq_len(ncol(valores)), function(j) {
    factor(valores[, j], levels = .ESTADOS_COBERTURA_ANALISIS)
  })
  columnas <- c(
    list(
      tabla = indice$tabla, esquema = indice$esquema,
      identificador = indice$identificador, estado_tabla = indice$estado
    ),
    celdas
  )
  names(columnas)[-(1:4)] <- nombres
  list2DF(columnas, nrow = nrow(indice))
}

.no_medido_vacio <- function(n = 0L) {
  data.frame(
    tabla = rep(NA_character_, n), esquema = rep(NA_character_, n),
    identificador = rep(NA_character_, n), origen = rep(NA_character_, n),
    alcance = rep(NA_character_, n), columna = rep(NA_character_, n),
    metrica = rep(NA_character_, n), diagnostico = rep(NA_character_, n),
    estado = rep(NA_character_, n), motivo = rep(NA_character_, n),
    como_resolverlo = rep(NA_character_, n), stringsAsFactors = FALSE
  )
}

# (c) Lo no medido y por que, con el motivo que el paquete ya registro, una
# fila por cosa: todas las filas de `cobertura_coleccion`, las metricas de
# `cobertura_metricas` que no se calcularon -en el modo `"completa"` trae
# tambien las calculadas, que no son "no medido"- y, con perfiles conservados,
# los diagnosticos no evaluados de cada perfil de muestra. `origen` nombra el
# campo de donde sale cada fila. Agrupado por tabla en el orden del indice; lo
# que no corresponde a ninguna tabla del indice va al final, no se pierde.
.no_medido_coleccion <- function(x, indice) {
  cobertura <- x$cobertura_coleccion
  desde_cobertura <- .no_medido_vacio(
    if (is.data.frame(cobertura)) nrow(cobertura) else 0L
  )
  if (nrow(desde_cobertura)) {
    desde_cobertura$tabla <- .campo_texto_coleccion(cobertura, "tabla")
    desde_cobertura$esquema <- .campo_texto_coleccion(cobertura, "esquema")
    desde_cobertura$identificador <-
      .identificadores_cobertura_coleccion(cobertura)
    desde_cobertura$origen <- "cobertura_coleccion"
    desde_cobertura$alcance <- .alcances_cobertura_coleccion(cobertura)
    desde_cobertura$motivo <- .campo_texto_coleccion(cobertura, "motivo")
    desde_cobertura$como_resolverlo <-
      .campo_texto_coleccion(cobertura, "como_resolverlo")
  }

  metricas <- x$cobertura_metricas
  no_calculadas <- which(
    !.campo_texto_coleccion(metricas, "estado") %in% "calculado"
  )
  desde_metricas <- .no_medido_vacio(length(no_calculadas))
  if (length(no_calculadas)) {
    campo <- function(nombre) {
      .campo_texto_coleccion(metricas, nombre)[no_calculadas]
    }
    desde_metricas$tabla <- campo("tabla")
    desde_metricas$esquema <- campo("esquema")
    desde_metricas$identificador <- campo("identificador")
    desde_metricas$origen <- "cobertura_metricas"
    desde_metricas$columna <- campo("columna")
    desde_metricas$metrica <- campo("metrica")
    desde_metricas$estado <- campo("estado")
    desde_metricas$motivo <- campo("motivo")
    desde_metricas$como_resolverlo <- campo("como_resolverlo")
  }

  perfiles <- .perfiles_por_tabla_coleccion(x, indice$identificador)
  desde_diagnosticos <- lapply(seq_len(nrow(indice)), function(i) {
    muestra <- if (is.null(perfiles[[i]])) NULL else perfiles[[i]]$perfil_muestra
    diagnosticos <- if (is.list(muestra)) muestra$cobertura_diagnosticos
    if (!is.data.frame(diagnosticos) || !nrow(diagnosticos)) return(NULL)
    filas <- .no_medido_vacio(nrow(diagnosticos))
    filas$tabla <- indice$tabla[[i]]
    filas$esquema <- indice$esquema[[i]]
    filas$identificador <- indice$identificador[[i]]
    filas$origen <- "cobertura_diagnosticos"
    filas$columna <- .campo_texto_coleccion(diagnosticos, "columna")
    filas$diagnostico <- .campo_texto_coleccion(diagnosticos, "diagnostico")
    filas$motivo <- .campo_texto_coleccion(diagnosticos, "motivo")
    filas$como_resolverlo <-
      .campo_texto_coleccion(diagnosticos, "como_resolverlo")
    filas
  })
  todo <- do.call(rbind, c(
    list(desde_cobertura, desde_metricas),
    desde_diagnosticos[!vapply(desde_diagnosticos, is.null, logical(1L))]
  ))
  n <- nrow(indice)
  posicion <- .indice_nombre(todo$identificador, indice$identificador)
  posicion[is.na(posicion)] <- n + 1L
  filas <- unlist(
    split(seq_len(nrow(todo)), factor(posicion, levels = seq_len(n + 1L))),
    use.names = FALSE
  )
  salida <- todo[filas, , drop = FALSE]
  rownames(salida) <- NULL
  salida
}

# (d) Los hallazgos de las tablas, una fila por tabla y hallazgo, sin las
# columnas-lista: `trazabilidad` lleva las claves de cada fila y
# `write.csv()` no la escribe. `columna` sigue siendo el texto legible. La
# guarda de coherencia corre aca como en `hallazgos()`: esta tambien es una
# puerta de publicacion.
.hallazgos_planos_coleccion <- function(x, avisar = TRUE) {
  completos <- .hallazgos_coleccion(x, avisar = avisar)
  .advertir_incoherencias_trazabilidad(completos)
  planas <- which(!vapply(completos, is.list, logical(1L)))
  salida <- .seleccionar_columnas(completos, planas)
  marca <- attr(completos, "datos_personales_sin_proteger", exact = TRUE)
  if (length(marca)) attr(salida, "datos_personales_sin_proteger") <- marca
  rownames(salida) <- NULL
  salida
}

.parte_informe_coleccion <- function(x, parte, marco, avisar = TRUE) {
  # Un `marco` que no es un marco se rechaza al entrar, para toda parte, como en
  # `analizar()`: solo `"matriz"` y `"dimensiones"` lo leen, y validarlo ahi
  # dejaba pasar `marco = 1` en las otras tres sin decir nada.
  if (!is.null(marco)) marco <- .marco_para_tablero(NULL, marco)
  if (identical(parte, "hallazgos")) {
    return(.hallazgos_planos_coleccion(x, avisar = avisar))
  }
  indice <- .indice_tablas_coleccion(x)
  if (identical(parte, "tablas")) return(indice)
  if (identical(parte, "no_medido")) return(.no_medido_coleccion(x, indice))
  # El marco se resuelve como en el tablero: uno declarado se valida y, sin
  # medicion de la que tomarlo, `NULL` es el marco por omision de
  # `cobertura_analisis()`. Escrito una vez, en `.marco_para_tablero()`.
  marco <- .marco_para_tablero(NULL, marco)
  if (avisar && !length(x$perfiles) && nrow(indice)) {
    .avisar(
      "Esta colecci\u00f3n no conserv\u00f3 los perfiles de sus tablas, as\u00ed que ",
      "no se pueden ubicar en el marco y la matriz queda en NA. Se conservan ",
      "con perfilar_coleccion(..., conservar_perfiles = TRUE); el estado de ",
      "cada tabla sigue en as.data.frame(x, parte = \"tablas\").", call. = FALSE
    )
  }
  dimensiones <- .dimensiones_coleccion(x, indice, marco)
  if (identical(parte, "dimensiones")) return(dimensiones)
  .matriz_coleccion(dimensiones, indice, marco)
}

#' Tablas planas de un perfil de colección
#'
#' Un perfil de colección reúne el resumen, la cobertura y, si se conservaron,
#' los perfiles de cientos de tablas. `as.data.frame()` lo baja a tablas
#' planas —sin columnas-lista, listas para [utils::write.csv()]—, y
#' [reportar()] publica las mismas tablas en su sección de la colección.
#' `summary()` es `as.data.frame()`: sin más argumentos, el índice.
#'
#' Ninguna parte calcula un puntaje: no se promedian factores, dimensiones ni
#' tablas. La jerarquía dimensión-factor del marco es taxonómica. Los únicos
#' números son los de cada tabla —los agregados SQL de `resumen_coleccion` y
#' los hallazgos contados por severidad—, y donde no hubo medición va `NA`,
#' nunca 0.
#'
#' @section Las partes:
#' - `"tablas"`, el índice: una fila por tabla declarada, primero las
#'   perfiladas —en el orden en que se midieron— y después las que no. Lleva
#'   la identidad (`tabla`, `esquema`, `identificador`, `tipo`), el `estado`
#'   y su `motivo` y `como_resolverlo`, tomados de `cobertura_coleccion`:
#'   `perfilada`; `vacia`, cero filas; `medicion_incompleta`, se perfiló pero
#'   no se pudo medir la proporción de ausentes de ninguna columna;
#'   `con_error`, se declaró como tabla y no se pudo perfilar —el motivo
#'   conserva la respuesta del motor—; y `no_medida`, se declaró como otro
#'   tipo de objeto (una vista) y no se perfila. `alcances_cobertura` nombra
#'   todas las filas de la tabla en `cobertura_coleccion`, también las de una
#'   tabla perfilada con métricas no calculadas o sin muestra. Siguen las
#'   columnas de `resumen_coleccion`; `n_metricas_sin_listar`, las métricas
#'   no calculadas de la tabla que `cobertura_metricas` no lista porque se
#'   recortó a `tope_cobertura_metricas` (0 si están todas; `NA` con
#'   `cobertura_metricas = "ninguna"`, que no lista nada a propósito); y
#'   `n_hallazgos_error` y `n_hallazgos_sospechoso`, que son `NA` sin perfil
#'   conservado, sin muestra leída o en una tabla vacía, donde no hubo filas
#'   que examinar.
#' - `"matriz"`: una fila por tabla y una columna por factor del `marco`,
#'   llamada `"dimensión / factor"`, con el `estado` que
#'   [cobertura_analisis()] da al perfil de la tabla: `medida`,
#'   `no_declarada`, `no_aplica` o `fuera_de_alcance`. Es la «Cobertura del
#'   análisis» de la sección de un perfil en [reportar()], tabla por tabla.
#'   Necesita los perfiles: sin `conservar_perfiles = TRUE`, y en las tablas
#'   que no se perfilaron, la celda es `NA`.
#' - `"dimensiones"`: lo mismo en largo, una fila por tabla y factor, con
#'   `marco`, `dimension`, `factor`, `estado` y el `motivo` de cada celda
#'   —el alcance parcial de un factor medido en algunas columnas, por qué no
#'   aplica, por qué es `NA`—.
#' - `"no_medido"`: una fila por cosa que no se midió, con el motivo que el
#'   paquete registró: las filas de `cobertura_coleccion`, las métricas de
#'   `cobertura_metricas` que no se calcularon y, con perfiles conservados,
#'   los diagnósticos no evaluados de cada perfil de muestra. `origen` nombra
#'   el campo de donde sale la fila; `alcance`, `columna`, `metrica`,
#'   `diagnostico` y `estado` son los de ese campo y `NA` en los otros. Con
#'   `perfilar_coleccion(cobertura_metricas = "ninguna")`, o con la tabla
#'   recortada por su tope, faltan métricas: el total real queda en
#'   `meta$n_metricas_no_medidas`, y cuántas faltan de cada tabla, en
#'   `n_metricas_sin_listar` del índice.
#' - `"hallazgos"`: los de [hallazgos()] sobre la colección, una fila por
#'   tabla y hallazgo, sin la columna-lista `trazabilidad`. Sin perfiles
#'   conservados, vacía y con el mismo aviso.
#'
#' @param x,object Objeto de [perfilar_coleccion()].
#' @param row.names,optional Como en [as.data.frame()].
#' @param ... Sin uso: un argumento desconocido —`partes` por `parte`— se
#'   rechaza en vez de ignorarse.
#' @param parte Qué tabla devolver: `"tablas"` (por omisión), `"matriz"`,
#'   `"dimensiones"`, `"no_medido"` o `"hallazgos"`.
#' @param marco Taxonomía de [marco_calidad()] contra la que se ubica cada
#'   tabla en `"matriz"` y `"dimensiones"`. `NULL`, por omisión, usa el marco
#'   que [cobertura_analisis()] usa cuando no recibe uno, [marco_agesic()];
#'   las demás partes no lo leen.
#'
#' @return Un `data.frame`.
#' @seealso [perfilar_coleccion()], [reportar()], [cobertura_analisis()],
#'   [hallazgos()]
#' @export
#'
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE) &&
#'     requireNamespace("DBI", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(con, "personas", data.frame(id = 1:3, nombre = letters[1:3]))
#'   DBI::dbWriteTable(con, "bajas", data.frame(id = integer()))
#'   base <- coleccion(con, c("personas", "bajas", "pagos"), nombre = "padron")
#'   perfil <- perfilar_coleccion(base, conservar_perfiles = TRUE)
#'   print(summary(perfil)[, c("tabla", "estado", "n_filas")])
#'   print(as.data.frame(perfil, parte = "matriz")[, 1:6])
#'   print(as.data.frame(perfil, parte = "no_medido")[, c("tabla", "origen", "alcance")])
#'   DBI::dbDisconnect(con)
#' }
as.data.frame.perfil_coleccion <- function(x, row.names = NULL,
                                           optional = FALSE, ...,
                                           parte = c("tablas", "matriz",
                                                     "dimensiones",
                                                     "no_medido",
                                                     "hallazgos"),
                                           marco = NULL) {
  .validar_objeto_lupa(
    x, "perfil_coleccion", c("resumen_coleccion", "cobertura_coleccion"),
    "perfilar_coleccion()"
  )
  extras <- names(list(...))
  if (is.null(extras)) extras <- rep("", ...length())
  # `stringsAsFactors` lo pasan por costumbre `data.frame()` y otros: no cambia
  # nada aca. Cualquier otro nombre es una errata que, ignorada, devolveria el
  # indice en vez de la parte pedida sin decirlo.
  desconocidos <- setdiff(extras[nzchar(extras)], "stringsAsFactors")
  if (!all(nzchar(extras)) || length(desconocidos)) {
    .detener(
      "`as.data.frame()` sobre un perfil de colecci\u00f3n solo acepta `parte` y ",
      "`marco`",
      if (length(desconocidos)) {
        paste0("; recibi\u00f3 `", paste(desconocidos, collapse = "`, `"), "`")
      } else "; recibi\u00f3 un argumento sin nombre",
      ".", call. = FALSE
    )
  }
  parte <- .elegir_opcion(parte)
  salida <- .parte_informe_coleccion(x, parte, marco)
  if (!is.null(row.names)) {
    salida <- as.data.frame(salida, row.names = row.names, optional = optional)
  }
  salida
}

#' @rdname as.data.frame.perfil_coleccion
#' @export
# `parte` va segunda para que `summary(x, "no_medido")` la tome por posicion:
# pasada por `...`, caeria en `row.names` de `as.data.frame()`. Las opciones
# las valida `as.data.frame()`, que es donde estan escritas.
summary.perfil_coleccion <- function(object, parte = "tablas",
                                     marco = NULL, ...) {
  as.data.frame(object, parte = parte, marco = marco, ...)
}

# La seccion de una coleccion en `reportar()`. Publica las tablas de
# `as.data.frame()` -el indice y la matriz con menos columnas, para que se
# lean en una pantalla; la tabla entera es la exportada- y declara lo que no
# puede publicar: sin perfiles no hay matriz ni hallazgos.
#
# Con la proteccion pedida en el informe, cada perfil de muestra se vuelve a
# proteger antes de leer sus hallazgos, como la seccion de un perfil
# (`.seccion_perfil()`): un perfil no lleva la marca de los objetos armados sin
# proteccion, asi que mirar la marca no habria tapado nada. Y se dice cuantos
# se armaron sin ella, con la misma salvedad: sin la tabla, una variante de un
# valor protegido escrita en otra columna puede quedar.
.seccion_coleccion <- function(x, max_filas, proteger_datos_personales = FALSE) {
  .validar_objeto_lupa(
    x, "perfil_coleccion", c("resumen_coleccion", "cobertura_coleccion"),
    "perfilar_coleccion()"
  )
  n_reprotegidos <- 0L
  if (isTRUE(proteger_datos_personales) && length(x$perfiles)) {
    x$perfiles <- lapply(x$perfiles, function(perfil) {
      muestra <- if (is.list(perfil)) perfil$perfil_muestra
      if (!inherits(muestra, "perfil")) return(perfil)
      if (isFALSE(muestra$meta$proteger_datos_personales) &&
          length(.columnas_personales_protegidas(muestra))) {
        n_reprotegidos <<- n_reprotegidos + 1L
      }
      perfil$perfil_muestra <- .proteger_perfil(muestra)
      perfil
    })
  }
  marco <- .marco_para_tablero(NULL, NULL)
  indice <- .indice_tablas_coleccion(x)
  no_medido <- .no_medido_coleccion(x, indice)
  con_perfiles <- length(x$perfiles) > 0L
  nota <- function(texto) {
    paste0("<p class=\"nota\">", .html_texto(texto), "</p>")
  }
  estados <- as.character(indice$estado)
  cuenta <- function(estado) sum(estados %in% estado)
  tarjetas <- paste0(
    "<div class=\"tarjetas\">",
    .tarjeta_reporte("Declaradas", nrow(indice)),
    .tarjeta_reporte("Perfiladas", cuenta("perfilada")),
    .tarjeta_reporte("Vac\u00edas", cuenta("vacia")),
    .tarjeta_reporte("Medici\u00f3n incompleta", cuenta("medicion_incompleta")),
    .tarjeta_reporte("Con error", cuenta("con_error"), "error"),
    .tarjeta_reporte("No medidas", cuenta("no_medida")),
    "</div>"
  )
  matriz <- if (con_perfiles) {
    dimensiones <- .dimensiones_coleccion(x, indice, marco)
    tabla <- .matriz_coleccion(dimensiones, indice, marco)
    paste0(
      nota(paste0(
        "Cada celda es el estado del factor en cobertura_analisis() del ",
        "perfil de la tabla, contra el ", marco$nombre, ". Las celdas no se ",
        "combinan: la jerarqu\u00eda dimensi\u00f3n-factor es taxon\u00f3mica y ",
        "ninguna cifra resume una tabla. Un guion es una celda sin perfil del ",
        "que leerla; el motivo de cada celda est\u00e1 en ",
        "as.data.frame(x, parte = \"dimensiones\")."
      )),
      # Sin `tabla` ni `esquema`: el identificador ya los lleva, y la matriz
      # tiene una columna por factor.
      .html_tabla(tabla, max_filas, columnas = names(tabla)[-(1:2)])
    )
  } else {
    nota(paste(
      "La colecci\u00f3n no conserv\u00f3 los perfiles de sus tablas: sin ellos",
      "no se pueden ubicar en el marco. Se conservan con",
      "perfilar_coleccion(..., conservar_perfiles = TRUE)."
    ))
  }
  hallazgos <- if (!con_perfiles) {
    nota(paste(
      "Sin perfiles conservados no hay hallazgos por fila. El resumen por",
      "tabla est\u00e1 en el \u00edndice y lo no medido, abajo."
    ))
  } else {
    paste0(
      if (n_reprotegidos) {
        nota(paste0(
          n_reprotegidos,
          if (n_reprotegidos == 1L) " perfil de la colecci\u00f3n se arm\u00f3" else
            " perfiles de la colecci\u00f3n se armaron",
          " con la protecci\u00f3n de datos personales desactivada. El informe ",
          "vuelve a proteger sus hallazgos con lo que cada perfil conserva, sin ",
          "la tabla: una variante de un valor protegido escrita en otra columna ",
          "puede quedar a la vista. Para la protecci\u00f3n completa, perfilar la ",
          "colecci\u00f3n con la protecci\u00f3n activa."
        ))
      } else "",
      .html_tabla(.hallazgos_planos_coleccion(x, avisar = FALSE), max_filas)
    )
  }
  meta <- x$meta
  notas_metricas <- c(
    if (identical(meta$cobertura_metricas_modo, "ninguna")) {
      "Se perfil\u00f3 con cobertura_metricas = \"ninguna\": las m\u00e9tricas no calculadas no se listan; su n\u00famero por tabla est\u00e1 en el \u00edndice."
    },
    if (isTRUE(meta$cobertura_metricas_truncada)) {
      paste0(
        "La cobertura de m\u00e9tricas se recort\u00f3 a su tope de ",
        .formatear_numero_publicado(meta$tope_cobertura_metricas),
        " filas: el total de m\u00e9tricas no medidas es ",
        .formatear_numero_publicado(meta$n_metricas_no_medidas), "."
      )
    }
  )
  columnas_indice <- c(
    "identificador", "tipo", "estado", "n_filas", "n_columnas",
    "n_columnas_medidas", "prop_faltantes_maxima", "n_metricas_sin_listar",
    "n_hallazgos_error", "n_hallazgos_sospechoso", "alcances_cobertura",
    "motivo"
  )
  paste0(
    "<section><h2>Perfil de colecci\u00f3n: ", .html_texto(meta$nombre), "</h2>",
    "<p class=\"meta\">Motor: ", .html_texto(.resumir_valor_reporte(meta$motor)),
    " \u00b7 Corrida: ", .html_texto(.resumir_valor_reporte(meta$inicio)),
    " a ", .html_texto(.resumir_valor_reporte(meta$fin)), "</p>",
    tarjetas,
    if (length(meta$nota_snapshot)) nota(meta$nota_snapshot) else "",
    "<h3>\u00cdndice de tablas</h3>",
    nota(paste(
      "Una fila por tabla declarada: primero las perfiladas y despu\u00e9s las",
      "que no. Donde no hubo medici\u00f3n va un guion, nunca cero. La tabla",
      "entera es as.data.frame(x, parte = \"tablas\")."
    )),
    .html_tabla(indice, max_filas, columnas = columnas_indice),
    "<h3>Tablas por dimensi\u00f3n de calidad</h3>", matriz,
    "<h3>Lo no medido y por qu\u00e9</h3>",
    nota(paste(
      "Cada fila sale de una cobertura del objeto, nombrada en origen. Su",
      "ausencia no es conformidad."
    )),
    paste0(vapply(notas_metricas, nota, character(1L)), collapse = ""),
    .html_tabla(no_medido, max_filas),
    "<h3>Hallazgos por tabla</h3>", hallazgos,
    "</section>"
  )
}
