.es_clave <- function(datos, indices) {
  columnas <- lapply(indices, function(i) datos[[i]])
  if (any(vapply(columnas, anyNA, logical(1L)))) {
    return(FALSE)
  }
  if (length(indices) == 1L) {
    return(anyDuplicated(columnas[[1L]]) == 0L)
  }
  combinado <- as.data.frame(columnas, optional = TRUE, stringsAsFactors = FALSE)
  anyDuplicated(combinado) == 0L
}

.pares_redundantes <- function(datos, indices_clave, nombres) {
  if (length(indices_clave) < 2L) {
    return(data.frame(
      columna_1 = character(), columna_2 = character(),
      stringsAsFactors = FALSE
    ))
  }
  pares <- utils::combn(indices_clave, 2L)
  iguales <- apply(pares, 2L, function(indice) {
    x <- datos[[indice[[1L]]]]
    y <- datos[[indice[[2L]]]]
    identical(is.na(x), is.na(y)) &&
      identical(as.character(x), as.character(y))
  })
  pares <- pares[, iguales, drop = FALSE]
  if (!ncol(pares)) {
    return(data.frame(
      columna_1 = character(), columna_2 = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    columna_1 = nombres[pares[1L, ]],
    columna_2 = nombres[pares[2L, ]],
    stringsAsFactors = FALSE
  )
}

#' Detectar claves candidatas
#'
#' Busca primero claves simples y luego combinaciones mínimas de dos o tres
#' columnas. No prueba una combinación si ya contiene una clave candidata más
#' pequeña. Una clave exige ausencia de `NA` y unicidad en todas las filas.
#'
#' Dos claves simples se marcan como redundantes cuando sus contenidos son
#' idénticos, aunque tengan nombres distintos. Los pares también quedan en el
#' atributo `claves_redundantes`.
#'
#' @param datos Objeto que hereda de `data.frame`.
#' @param max_combinacion Máximo de columnas por combinación, entre 1 y 3.
#'
#' @return Data frame de claves candidatas con las columnas combinadas,
#'   cantidad de columnas y marcas de redundancia.
#' @export
#' @seealso [detectar_dependencias()], [detectar_relaciones()]
#'
#' @examples
#' detectar_claves(data.frame(id = 1:4, grupo = c("a", "a", "b", "b")))
detectar_claves <- function(datos, max_combinacion = 3) {
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe heredar de data.frame.", call. = FALSE)
  }
  if (length(max_combinacion) != 1L || is.na(max_combinacion) ||
      max_combinacion < 1L || max_combinacion > 3L) {
    stop("`max_combinacion` debe ser un entero entre 1 y 3.", call. = FALSE)
  }
  nombres <- make.unique(names(datos))
  encontradas <- list()
  k <- 0L
  limite <- min(floor(max_combinacion), ncol(datos))

  if (nrow(datos) > 0L && limite > 0L) {
    for (tamano in seq_len(limite)) {
      combinaciones <- utils::combn(seq_len(ncol(datos)), tamano, simplify = FALSE)
      for (combinacion in combinaciones) {
        contiene_clave <- any(vapply(encontradas, function(clave) {
          all(clave %in% combinacion)
        }, logical(1L)))
        if (!contiene_clave && .es_clave(datos, combinacion)) {
          k <- k + 1L
          encontradas[[k]] <- combinacion
        }
      }
    }
  }

  indices_simples <- unlist(encontradas[lengths(encontradas) == 1L], use.names = FALSE)
  redundantes <- .pares_redundantes(datos, indices_simples, nombres)
  if (!length(encontradas)) {
    resultado <- data.frame(
      columnas = character(), n_columnas = integer(), n_filas = integer(),
      redundante = logical(), equivalente_a = character(),
      stringsAsFactors = FALSE
    )
  } else {
    resultado <- do.call(rbind, lapply(encontradas, function(indices) {
      nombres_clave <- nombres[indices]
      relacionadas <- character()
      if (length(indices) == 1L && nrow(redundantes)) {
        relacionadas <- c(
          redundantes$columna_2[redundantes$columna_1 == nombres_clave],
          redundantes$columna_1[redundantes$columna_2 == nombres_clave]
        )
      }
      data.frame(
        columnas = .pegar_nombres(nombres_clave),
        n_columnas = length(indices),
        n_filas = nrow(datos),
        redundante = length(relacionadas) > 0L,
        equivalente_a = paste(relacionadas, collapse = ", "),
        stringsAsFactors = FALSE
      )
    }))
    rownames(resultado) <- NULL
  }
  class(resultado) <- c("claves_candidatas", "data.frame")
  attr(resultado, "claves_redundantes") <- redundantes
  resultado
}

.valores_relacion <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  as.character(x)
}

.resumir_columna_relacion <- function(x, muestra) {
  valores <- .valores_relacion(x)
  valores_muestra <- .muestrear_vector(valores, muestra)$valores
  valores_completos <- valores[!is.na(valores)]
  list(
    muestra = valores_muestra[!is.na(valores_muestra)],
    unicos = unique(valores_completos),
    unico = anyDuplicated(valores_completos) == 0L
  )
}

#' Detectar relaciones entre dos tablas
#'
#' Examina todos los pares de columnas y describe su cardinalidad a partir de
#' la unicidad completa de cada lado. La cobertura `tabla1_en_tabla2` es la
#' proporción de valores no ausentes de la primera columna que existe en la
#' segunda; la cobertura inversa se informa de forma simétrica. Así se puede
#' escoger la dirección PK/FK sin imponerla de antemano.
#'
#' Cuando una tabla supera `muestra`, la función estima cada cobertura con una
#' muestra sistemática del lado que se verifica y conserva completo el conjunto
#' de referencia. La cardinalidad y la cantidad de valores comunes siempre se
#' calculan con ambas columnas completas.
#'
#' @param tabla1,tabla2 Objetos que heredan de `data.frame`.
#' @param muestra Máximo de filas del lado verificado que se usan para estimar
#'   cada cobertura. El muestreo es sistemático y reproducible; el lado de
#'   referencia no se muestrea. Use `Inf` para calcular todo sin muestreo.
#'
#' @return Data frame con columnas comparadas, cardinalidad, coincidencias y
#'   coberturas de integridad referencial en ambas direcciones. Los atributos
#'   `filas_totales`, `filas_analizadas` y `muestreado` documentan el muestreo.
#' @export
#' @seealso [detectar_claves()], [referencial()], [proponer_modelo()]
#'
#' @examples
#' personas <- data.frame(id = 1:3)
#' tramites <- data.frame(persona_id = c(1, 1, 3, 4))
#' detectar_relaciones(personas, tramites)
detectar_relaciones <- function(tabla1, tabla2, muestra = 1e5) {
  if (!inherits(tabla1, "data.frame") || !inherits(tabla2, "data.frame")) {
    stop("`tabla1` y `tabla2` deben heredar de data.frame.", call. = FALSE)
  }
  limite_muestra <- .validar_muestra(muestra)
  nombres_1 <- make.unique(names(tabla1))
  nombres_2 <- make.unique(names(tabla2))
  columnas_1 <- lapply(seq_len(ncol(tabla1)), function(i) {
    .resumir_columna_relacion(tabla1[[i]], limite_muestra)
  })
  columnas_2 <- lapply(seq_len(ncol(tabla2)), function(i) {
    .resumir_columna_relacion(tabla2[[i]], limite_muestra)
  })
  filas <- vector("list", ncol(tabla1) * ncol(tabla2))
  k <- 0L

  for (i in seq_len(ncol(tabla1))) {
    x <- columnas_1[[i]]
    for (j in seq_len(ncol(tabla2))) {
      y <- columnas_2[[j]]
      k <- k + 1L
      n_valores_comunes <- length(intersect(x$unicos, y$unicos))
      unico_x <- x$unico
      unico_y <- y$unico
      cardinalidad <- if (!n_valores_comunes) {
        "sin_coincidencias"
      } else if (unico_x && unico_y) {
        "1:1"
      } else if (unico_x && !unico_y) {
        "1:m"
      } else if (!unico_x && unico_y) {
        "m:1"
      } else {
        "m:m"
      }
      filas[[k]] <- data.frame(
        columna_tabla1 = nombres_1[[i]],
        columna_tabla2 = nombres_2[[j]],
        cardinalidad = cardinalidad,
        n_valores_comunes = n_valores_comunes,
        cobertura_tabla1_en_tabla2 = if (length(x$muestra)) {
          mean(x$muestra %in% y$unicos)
        } else {
          NA_real_
        },
        cobertura_tabla2_en_tabla1 = if (length(y$muestra)) {
          mean(y$muestra %in% x$unicos)
        } else {
          NA_real_
        },
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(filas)) {
    resultado <- do.call(rbind, filas)
    rownames(resultado) <- NULL
  } else {
    resultado <- data.frame(
      columna_tabla1 = character(), columna_tabla2 = character(),
      cardinalidad = character(), n_valores_comunes = integer(),
      cobertura_tabla1_en_tabla2 = numeric(),
      cobertura_tabla2_en_tabla1 = numeric(), stringsAsFactors = FALSE
    )
  }
  class(resultado) <- c("relaciones_detectadas", "data.frame")
  attr(resultado, "filas_totales") <- c(
    tabla1 = nrow(tabla1), tabla2 = nrow(tabla2)
  )
  attr(resultado, "filas_analizadas") <- c(
    tabla1 = min(nrow(tabla1), limite_muestra),
    tabla2 = min(nrow(tabla2), limite_muestra)
  )
  attr(resultado, "muestreado") <- c(
    tabla1 = nrow(tabla1) > limite_muestra,
    tabla2 = nrow(tabla2) > limite_muestra
  )
  resultado
}
