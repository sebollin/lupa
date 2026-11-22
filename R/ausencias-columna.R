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

# Las vias del motor validan y resuelven la lista contra los campos de la
# tabla. Los vectores pasan como llegaron: su validacion es la de `perfilar()`
# sobre la muestra, como antes.
.resolver_ausencias_dbi <- function(sentinelas_numericos, cadenas_ausencia,
                                    campos) {
  if (.es_ausencias_por_columna(sentinelas_numericos)) {
    .validar_sentinelas_numericos(sentinelas_numericos)
    sentinelas_numericos <- .resolver_ausencias_columnas(
      sentinelas_numericos, campos, "sentinelas_numericos"
    )
  }
  if (.es_ausencias_por_columna(cadenas_ausencia)) {
    .validar_cadenas_ausencia(cadenas_ausencia)
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
  posiciones <- which(!is.na(completos) &
    vapply(extras, .es_ausencias_por_columna, logical(1L)))
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
  function(identificador) {
    argumentos <- extras
    for (j in posiciones) {
      lista <- extras[[j]]
      indice <- if (length(lista)) {
        .indice_nombre(identificador, names(lista))
      } else NA_integer_
      if (length(indice) == 1L && !is.na(indice)) {
        argumentos[j] <- list(lista[[indice]])
      } else if (identical(completos[[j]], "sentinelas_numericos")) {
        argumentos[[j]] <- .numeros_na_locales
      } else {
        argumentos[j] <- list(NULL)
      }
    }
    argumentos
  }
}
