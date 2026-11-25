# Declaracion de ausencias por columna.
#
# `sentinelas_numericos` y `cadenas_ausencia` aceptan, ademas del vector que
# vale para todas las columnas, una lista con nombre columna -> valores. Cada
# entrada vale para su columna lo que el vector valdria para ella: la de
# `sentinelas_numericos` es el conjunto completo de esa columna -reemplaza la
# lista por omision, y `numeric()` la apaga-; la de `cadenas_ausencia` se suma a
# la lista incorporada en esa columna. Las columnas que la lista no nombra reciben
# el valor por omision del argumento, como las que `normalizar` no nombra reciben
# `normalizacion()`.
#
# Un `99` puede ser "no sabe" en una columna y una edad en la de al lado: con un
# solo vector, el plan de limpieza armado con `list(x = 99)` aplanado convertia en
# `NA` los `99` legitimos de `y`. La lista se resuelve una vez por llamada, y los
# normalizadores de bajo nivel rechazan una lista sin resolver: un lector que no
# selecciona la entrada de su columna aborta, en vez de aplanarla en silencio y
# aplicar los valores de una columna a todas.

.es_ausencias_por_columna <- function(x) {
  is.list(x) && !is.data.frame(x)
}

.validar_nombres_ausencias <- function(x, argumento) {
  nombres <- names(x)
  if (length(x) && (is.null(nombres) || anyNA(nombres) || any(!nzchar(nombres)) ||
                    anyDuplicated(.nombres_para_operar(nombres)))) {
    .detener(
      "La lista de `", argumento, "` debe nombrar columnas \u00fanicas y no ",
      "vac\u00edas: cada elemento son los valores de una columna.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

.validar_vector_sentinelas <- function(valores) {
  if (!is.numeric(valores) || any(!is.na(valores) & !is.finite(valores))) {
    .detener(
      "`sentinelas_numericos` debe ser un vector num\u00e9rico finito; se permiten NA.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

# La declaracion se lee como se leen los datos, por `.texto_analizable()`: una
# cadena sin marca con un byte que no es UTF-8 valido -copiada de un CSV en
# latin-1- hacia abortar `trimws()` en una sesion UTF-8. No es vacia: trae
# bytes. Queda declarada y no coincide con ningun texto analizado, igual que las
# celdas que la repiten.
.validar_vector_cadenas <- function(valores) {
  if (!is.null(valores) &&
      (!is.character(valores) || anyNA(valores) ||
       any(!nzchar(trimws(.texto_analizable(valores)$valores))))) {
    .detener(
      "`cadenas_ausencia` debe ser un vector de texto sin ausentes ni cadenas ",
      "vac\u00edas.", call. = FALSE
    )
  }
  invisible(NULL)
}

.validar_cadenas_ausencia <- function(cadenas_ausencia) {
  if (.es_ausencias_por_columna(cadenas_ausencia)) {
    .validar_nombres_ausencias(cadenas_ausencia, "cadenas_ausencia")
    for (valores in cadenas_ausencia) .validar_vector_cadenas(valores)
    return(invisible(NULL))
  }
  .validar_vector_cadenas(cadenas_ausencia)
}

# El valor de una columna. `por_omision` es lo que la columna recibe cuando la
# lista no la nombra: el valor por omision del argumento.
.ausencia_de_columna <- function(declaracion, columna, por_omision) {
  if (!.es_ausencias_por_columna(declaracion)) return(declaracion)
  if (!length(declaracion)) return(por_omision)
  indice <- .indice_nombre(columna, names(declaracion))
  if (length(indice) != 1L || is.na(indice)) return(por_omision)
  declaracion[[indice]]
}

.sentinelas_de_columna <- function(declaracion, columna) {
  .ausencia_de_columna(declaracion, columna, .numeros_na_locales)
}

.cadenas_de_columna <- function(declaracion, columna) {
  .ausencia_de_columna(declaracion, columna, NULL)
}

# Las entradas de la lista con el nombre que la columna tiene en la tabla. Un
# nombre que la tabla no tiene es un error, como en `columnas_codigo`: una
# declaracion que no llega a ninguna columna no se calla.
.resolver_ausencias_columnas <- function(declaracion, nombres, argumento) {
  if (!.es_ausencias_por_columna(declaracion)) return(NULL)
  if (!length(declaracion)) return(stats::setNames(list(), character()))
  indices <- .indice_nombre(names(declaracion), nombres)
  if (anyNA(indices)) {
    .detener(
      "`", argumento, "` nombra columnas inexistentes: ",
      .pegar_texto(names(declaracion)[is.na(indices)], collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (anyDuplicated(indices)) {
    .detener(
      "`", argumento, "` nombra dos veces la columna `",
      nombres[indices[duplicated(indices)][[1L]]], "`.", call. = FALSE
    )
  }
  # Una tabla con dos columnas del mismo nombre: la entrada no dice a cual se
  # refiere, y elegir la primera publicaba el perfil y el plan de la otra en
  # silencio. Se pide desambiguar; una lista que no nombra el repetido sigue.
  operables <- .nombres_para_operar(nombres)
  repetidos <- duplicated(operables) | duplicated(operables, fromLast = TRUE)
  if (any(repetidos[indices])) {
    .detener(
      "`", argumento, "` nombra la columna `", nombres[indices[repetidos[indices]][[1L]]],
      "`, que la tabla repite: no se puede saber a cu\u00e1l se refiere. ",
      "Renombrar las columnas para que cada nombre sea \u00fanico.", call. = FALSE
    )
  }
  salida <- unname(declaracion)
  names(salida) <- nombres[indices]
  salida
}

# Lo que el perfil guarda: los dos campos de siempre con el valor de las
# columnas sin entrada propia -el vector declarado, o el por omision cuando se
# declaro por columna- y, solo si hubo una lista, `ausencias_por_columna`. Con
# vectores el perfil es el de antes, campo por campo.
.meta_ausencias <- function(sentinelas_numericos, cadenas_ausencia, nombres) {
  sent_pc <- .resolver_ausencias_columnas(
    sentinelas_numericos, nombres, "sentinelas_numericos"
  )
  cad_pc <- .resolver_ausencias_columnas(
    cadenas_ausencia, nombres, "cadenas_ausencia"
  )
  global_sent <- if (is.null(sent_pc)) sentinelas_numericos else .numeros_na_locales
  global_cad <- if (is.null(cad_pc)) cadenas_ausencia else NULL
  por_columna <- if (!is.null(sent_pc) || !is.null(cad_pc)) {
    list(
      sentinelas_numericos = lapply(
        sent_pc %||% stats::setNames(list(), character()),
        .normalizar_sentinelas_numericos
      ),
      cadenas_ausencia = cad_pc %||% stats::setNames(list(), character())
    )
  }
  list(
    sentinelas_numericos = .normalizar_sentinelas_numericos(global_sent),
    cadenas_ausencia = global_cad,
    ausencias_por_columna = por_columna
  )
}

# La politica de una columna leida de un perfil. `[[` y no `$`: con `$`, un perfil
# guardado sin `sentinelas_numericos` leeria por coincidencia parcial otro campo.
.ausencias_de_columna <- function(meta, columna) {
  sentinelas <- if (is.list(meta)) meta[["sentinelas_numericos"]] else NULL
  cadenas <- if (is.list(meta)) meta[["cadenas_ausencia"]] else NULL
  propia_sentinelas <- FALSE
  propia_cadenas <- FALSE
  por_columna <- if (is.list(meta)) meta[["ausencias_por_columna"]] else NULL
  if (is.list(por_columna) && length(columna) == 1L && !is.na(columna)) {
    lista_s <- por_columna[["sentinelas_numericos"]]
    if (length(lista_s)) {
      i <- .indice_nombre(columna, names(lista_s))
      if (!is.na(i)) {
        sentinelas <- lista_s[[i]]
        propia_sentinelas <- TRUE
      }
    }
    lista_c <- por_columna[["cadenas_ausencia"]]
    if (length(lista_c)) {
      i <- .indice_nombre(columna, names(lista_c))
      if (!is.na(i)) {
        cadenas <- lista_c[[i]]
        propia_cadenas <- TRUE
      }
    }
  }
  list(
    sentinelas = sentinelas, cadenas = cadenas,
    propia_sentinelas = propia_sentinelas, propia_cadenas = propia_cadenas
  )
}

# Una lista que llega a un normalizador de bajo nivel es un lector que no
# selecciono la entrada de su columna. Se aborta: aplanarla aplicaba los valores
# de una columna a todas.
.rechazar_ausencias_sin_resolver <- function(valores, argumento) {
  if (.es_ausencias_por_columna(valores)) {
    .detener(structure(
      class = c("lupa_error_interno", "error", "condition"),
      list(
        message = paste0(
          "Error interno de lupa: `", argumento, "` lleg\u00f3 como lista por ",
          "columna a una funci\u00f3n que espera los valores de una sola columna."
        ),
        call = NULL
      )
    ))
  }
  invisible(NULL)
}

# La declaracion de un perfil en la forma en que la recibe una funcion que la
# acepta: la lista por columna si el perfil la guardo, el vector si no. Las
# columnas que la lista no nombra reciben el por omision, que es justo lo que el
# perfil guardo en el campo de siempre.
.declaracion_desde_meta <- function(meta, argumento) {
  if (!is.list(meta)) return(NULL)
  por_columna <- meta[["ausencias_por_columna"]]
  lista <- if (is.list(por_columna)) por_columna[[argumento]] else NULL
  if (length(lista)) return(lista)
  meta[[argumento]]
}

# Si hay algun centinela declarado -no la lista por omision ni la de naniar-: en
# la forma por columna, en alguna columna.
.hay_sentinelas_declarados <- function(declaracion) {
  if (.es_ausencias_por_columna(declaracion)) {
    return(any(vapply(declaracion, function(valores) {
      length(.sentinelas_numericos_declarados(valores)) > 0L
    }, logical(1L))))
  }
  length(tryCatch(
    .sentinelas_numericos_declarados(declaracion),
    error = function(e) numeric()
  )) > 0L
}

# Las dos declaraciones con la regla de `perfilar()`, en sus dos formas. La
# forma no depende de la tabla: las vias del motor la validan al entrar, antes
# de la primera consulta. Los vectores pasaban como llegaron y los validaba
# `perfilar()` sobre la muestra: con `sentinelas_numericos = "a"` se pagaba el
# resumen SQL entero para perder la muestra, y sin muestra -`bloque_muestra =
# "solo_agregados"`- pasaban en silencio.
.validar_ausencias_dbi <- function(sentinelas_numericos, cadenas_ausencia) {
  .validar_sentinelas_numericos(sentinelas_numericos)
  .validar_cadenas_ausencia(cadenas_ausencia)
  invisible(NULL)
}

# Las vias del motor validan las dos declaraciones y resuelven la lista contra
# los campos de la tabla.
.resolver_ausencias_dbi <- function(sentinelas_numericos, cadenas_ausencia,
                                    campos) {
  .validar_ausencias_dbi(sentinelas_numericos, cadenas_ausencia)
  if (.es_ausencias_por_columna(sentinelas_numericos)) {
    sentinelas_numericos <- .resolver_ausencias_columnas(
      sentinelas_numericos, campos, "sentinelas_numericos"
    )
  }
  if (.es_ausencias_por_columna(cadenas_ausencia)) {
    cadenas_ausencia <- .resolver_ausencias_columnas(
      cadenas_ausencia, campos, "cadenas_ausencia"
    )
  }
  list(sentinelas = sentinelas_numericos, cadenas = cadenas_ausencia)
}

# En `perfilar_coleccion()` la lista es por tabla. Devuelve `NULL` si ninguno de
# los dos argumentos llego como lista -la coleccion reenvia `...` como siempre-,
# o una funcion que arma los argumentos de una tabla: los de `...` con la
# entrada de esa tabla en lugar de la lista. Un nombre que no es tabla de la
# coleccion es un error antes de leer, con la pista de como anidar.
.ausencias_por_tabla_coleccion <- function(extras, coleccion) {
  if (!length(extras) || is.null(names(extras))) return(NULL)
  formales <- c("sentinelas_numericos", "cadenas_ausencia")
  completos <- formales[pmatch(names(extras), formales, duplicates.ok = FALSE)]
  es_lista <- vapply(extras, .es_ausencias_por_columna, logical(1L))
  # Un vector vale igual para todas las tablas y se valida aca, una vez, con la
  # regla de `perfilar()`: tabla por tabla, cada una pagaba su resumen SQL para
  # perder la muestra.
  for (j in which(!is.na(completos) & !es_lista)) {
    .argumento_dbi(if (identical(completos[[j]], "sentinelas_numericos")) {
      .validar_sentinelas_numericos(extras[[j]])
    } else {
      .validar_cadenas_ausencia(extras[[j]])
    })
  }
  posiciones <- which(!is.na(completos) & es_lista)
  if (!length(posiciones)) return(NULL)
  declaradas <- coleccion$tablas$identificador
  for (j in posiciones) {
    lista <- extras[[j]]
    argumento <- completos[[j]]
    .argumento_dbi({
      if (length(lista) && (is.null(names(lista)) || anyNA(names(lista)) ||
                            any(!nzchar(names(lista))) ||
                            anyDuplicated(.nombres_para_operar(names(lista))))) {
        .detener(
          "`", argumento, "` en una colecci\u00f3n debe ser un vector o una lista ",
          "nombrada por identificador de tabla.", call. = FALSE
        )
      }
      desconocidas <- .identificadores_setdiff(names(lista), declaradas)
      if (length(desconocidas)) {
        .detener(
          "`", argumento, "` nombra tablas no declaradas en la colecci\u00f3n: ",
          paste(desconocidas, collapse = ", "), ". Para declarar por columna ",
          "dentro de una tabla, anidar: `list(<tabla> = list(<columna> = ",
          "valores))`.", call. = FALSE
        )
      }
      for (elemento in lista) {
        if (identical(argumento, "sentinelas_numericos")) {
          .validar_sentinelas_numericos(elemento)
        } else {
          .validar_cadenas_ausencia(elemento)
        }
      }
    })
  }
  # `base`: la lista de argumentos donde se reemplaza. Es `extras`, o la que la
  # reanudacion arma con ellos -los mismos nombres, con la `fecha` de la
  # corrida-: sin esto la reanudacion le pasaba la lista entera a cada tabla,
  # que la leia como una lista por columna y la rechazaba.
  function(identificador, base = extras) {
    argumentos <- base
    cuales <- if (is.null(names(base))) character() else {
      formales[pmatch(names(base), formales, duplicates.ok = FALSE)]
    }
    for (j in posiciones) {
      k <- match(completos[[j]], cuales)
      if (is.na(k)) next
      lista <- extras[[j]]
      indice <- if (length(lista)) {
        .indice_nombre(identificador, names(lista))
      } else NA_integer_
      if (length(indice) == 1L && !is.na(indice)) {
        argumentos[k] <- list(lista[[indice]])
      } else if (identical(completos[[j]], "sentinelas_numericos")) {
        argumentos[[k]] <- .numeros_na_locales
      } else {
        argumentos[k] <- list(NULL)
      }
    }
    argumentos
  }
}

# En una coleccion `columnas_opcionales` y `aplicabilidad` llegan iguales a
# todas las tablas, y no todas tienen las columnas que nombran. Una tabla que
# no tiene la columna no es un error: ahi la declaracion no se aplica y se
# dice. Si ninguna tabla la tiene, es un nombre mal escrito, y se rechaza antes
# de leer. Devuelve `NULL` si no hay nada declarado -la coleccion sigue como
# siempre-, o una lista:
# * `argumentos(identificador, base)`: los argumentos de una tabla, con las dos
#   declaraciones reducidas a lo que esa tabla tiene.
# * `omitidas(identificador)`: lo que no se aplico en esa tabla y por que.
# * `avisos`: un aviso por declaracion que no vale en todas las tablas.
# Las columnas de cada tabla se leen por metadatos, una vez por tabla y solo
# con alguna de las dos declaraciones.
.universo_por_tabla_coleccion <- function(extras, coleccion) {
  if (!length(extras) || is.null(names(extras))) return(NULL)
  formales <- c("columnas_opcionales", "aplicabilidad")
  completar <- function(nombres) {
    formales[pmatch(nombres, formales, duplicates.ok = FALSE)]
  }
  completos <- completar(names(extras))
  de <- function(nombre) {
    j <- match(nombre, completos)
    if (is.na(j)) NULL else extras[[j]]
  }
  opcionales <- de("columnas_opcionales")
  aplicabilidad <- de("aplicabilidad")
  if (!length(opcionales) && !length(aplicabilidad)) return(NULL)
  # Una declaracion mal formada la rechaza cada `perfilar_dbi()`, como siempre.
  if ((length(opcionales) && !is.character(opcionales)) ||
      (length(aplicabilidad) && (!is.list(aplicabilidad) ||
                                 is.null(names(aplicabilidad)) ||
                                 any(!nzchar(names(aplicabilidad)))))) {
    return(NULL)
  }
  opcionales <- as.character(opcionales)
  identificadores <- as.character(coleccion$tablas$identificador)
  campos_de <- lapply(seq_along(identificadores), function(k) {
    .columnas_tabla_coleccion(
      coleccion$conexion, coleccion$tablas$referencia[[k]], catalogo = FALSE
    )$campos
  })
  leidas <- which(vapply(campos_de, length, integer(1L)) > 0L)
  # Sin las columnas de ninguna tabla no hay con que repartir.
  if (!length(leidas)) return(NULL)
  todas <- unique(unlist(campos_de[leidas], use.names = FALSE))
  # Donde vale cada declaracion: una columna opcional, donde esta; una regla,
  # donde `perfilar()` la aceptaria.
  opcional_en <- lapply(opcionales, function(columna) {
    vapply(campos_de, function(campos) {
      length(campos) > 0L && !is.na(.indice_nombre(columna, campos))
    }, logical(1L))
  })
  regla_en <- lapply(seq_along(aplicabilidad), function(j) {
    lapply(campos_de, function(campos) {
      if (!length(campos)) return(NA_character_)
      tryCatch({
        .validar_universo_declarado(campos, character(), aplicabilidad[j])
        ""
      }, error = function(e) conditionMessage(e))
    })
  })
  vale_regla <- function(j, k) identical(regla_en[[j]][[k]], "")
  .argumento_dbi({
    en_ninguna <- opcionales[!vapply(opcional_en, any, logical(1L))]
    if (length(en_ninguna)) {
      .detener(
        "`columnas_opcionales` nombra columnas que no est\u00e1n en ninguna ",
        "tabla de la colecci\u00f3n: ", paste(en_ninguna, collapse = ", "), ".",
        call. = FALSE
      )
    }
    sin_tabla <- names(aplicabilidad)[is.na(.indice_nombre(
      names(aplicabilidad), todas
    ))]
    if (length(sin_tabla)) {
      .detener(
        "`aplicabilidad` nombra columnas que no est\u00e1n en ninguna tabla ",
        "de la colecci\u00f3n: ", paste(sin_tabla, collapse = ", "), ".",
        call. = FALSE
      )
    }
    # Lo que no depende de la tabla -las dos declaraciones sobre la misma
    # columna, una regla que no es una formula- vale para la coleccion entera.
    .validar_aplicabilidad(todas, opcionales, aplicabilidad)
    for (j in seq_along(aplicabilidad)) {
      if (any(vapply(leidas, function(k) vale_regla(j, k), logical(1L)))) next
      # La regla no se puede evaluar en ninguna tabla: el error de la primera.
      .detener(regla_en[[j]][[leidas[[1L]]]], call. = FALSE)
    }
  })
  omitidas_de <- lapply(seq_along(identificadores), function(k) {
    vacia <- data.frame(
      politica = character(), columna = character(), motivo = character(),
      stringsAsFactors = FALSE
    )
    if (!length(campos_de[[k]])) return(vacia)
    filas <- list(vacia)
    for (i in seq_along(opcionales)) {
      if (opcional_en[[i]][[k]]) next
      filas[[length(filas) + 1L]] <- data.frame(
        politica = "columnas_opcionales", columna = opcionales[[i]],
        motivo = "la tabla no tiene esa columna", stringsAsFactors = FALSE
      )
    }
    for (j in seq_along(aplicabilidad)) {
      if (vale_regla(j, k)) next
      columna <- names(aplicabilidad)[[j]]
      filas[[length(filas) + 1L]] <- data.frame(
        politica = "aplicabilidad", columna = columna,
        motivo = if (is.na(.indice_nombre(columna, campos_de[[k]]))) {
          "la tabla no tiene esa columna"
        } else {
          sub("[.[:space:]]+$", "", regla_en[[j]][[k]], perl = TRUE)
        },
        stringsAsFactors = FALSE
      )
    }
    do.call(rbind, filas)
  })
  indice_de <- function(identificador) {
    .indice_identificador(identificador, identificadores)
  }
  n_omitidas <- function(politica, columna) {
    sum(vapply(omitidas_de, function(x) {
      any(x$politica == politica & x$columna == columna)
    }, logical(1L)))
  }
  declaradas <- rbind(
    data.frame(politica = rep("columnas_opcionales", length(opcionales)),
               columna = opcionales, stringsAsFactors = FALSE),
    data.frame(politica = rep("aplicabilidad", length(aplicabilidad)),
               columna = as.character(names(aplicabilidad)),
               stringsAsFactors = FALSE)
  )
  avisos <- list()
  for (i in seq_len(nrow(declaradas))) {
    n <- n_omitidas(declaradas$politica[[i]], declaradas$columna[[i]])
    if (!n) next
    avisos[[length(avisos) + 1L]] <- simpleWarning(paste0(
      "`", declaradas$politica[[i]], "` declara `", declaradas$columna[[i]],
      "`, y en ", n, " de las ", length(leidas),
      " tablas de la colecci\u00f3n no se aplic\u00f3. Esas tablas se ",
      "perfilaron completas; `cobertura_coleccion` las nombra con el alcance ",
      "`declaracion_no_aplicada`."
    ))
  }
  list(
    argumentos = function(identificador, base) {
      k <- indice_de(identificador)
      if (is.na(k) || !length(campos_de[[k]]) || !length(base)) return(base)
      cuales <- completar(names(base))
      posicion <- match("columnas_opcionales", cuales)
      if (!is.na(posicion)) {
        base[[posicion]] <- opcionales[vapply(
          opcional_en, function(en) en[[k]], logical(1L)
        )]
      }
      posicion <- match("aplicabilidad", cuales)
      if (!is.na(posicion)) {
        quedan <- aplicabilidad[vapply(
          seq_along(aplicabilidad), function(j) vale_regla(j, k), logical(1L)
        )]
        base[posicion] <- list(if (length(quedan)) quedan else NULL)
      }
      base
    },
    omitidas = function(identificador) {
      k <- indice_de(identificador)
      if (is.na(k)) return(NULL)
      omitidas_de[[k]]
    },
    avisos = avisos
  )
}

# La fila de `cobertura_coleccion` de una tabla donde una declaracion de la
# coleccion no se aplico (ver `.universo_por_tabla_coleccion()`).
.motivo_declaracion_no_aplicada <- function(omitidas) {
  paste0(
    "La colecci\u00f3n declara ",
    paste0(
      "`", omitidas$politica, "` para `", omitidas$columna, "` (",
      omitidas$motivo, ")", collapse = "; "
    ),
    ": en esta tabla no se aplic\u00f3. La tabla se perfil\u00f3 completa."
  )
}
