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

#' Detectar relaciones entre dos tablas
#'
#' Examina todos los pares de columnas y describe su cardinalidad a partir de
#' la unicidad de cada lado. La cobertura `tabla1_en_tabla2` es la proporción de
#' valores no ausentes de la primera columna que existe en la segunda; la
#' cobertura inversa se informa de forma simétrica. Así se puede escoger la
#' dirección PK/FK sin imponerla de antemano.
#'
#' @param tabla1,tabla2 Objetos que heredan de `data.frame`.
#'
#' @return Data frame con columnas comparadas, cardinalidad, coincidencias y
#'   coberturas de integridad referencial en ambas direcciones.
#' @export
#'
#' @examples
#' personas <- data.frame(id = 1:3)
#' tramites <- data.frame(persona_id = c(1, 1, 3, 4))
#' detectar_relaciones(personas, tramites)
detectar_relaciones <- function(tabla1, tabla2) {
  if (!inherits(tabla1, "data.frame") || !inherits(tabla2, "data.frame")) {
    stop("`tabla1` y `tabla2` deben heredar de data.frame.", call. = FALSE)
  }
  nombres_1 <- make.unique(names(tabla1))
  nombres_2 <- make.unique(names(tabla2))
  filas <- vector("list", ncol(tabla1) * ncol(tabla2))
  k <- 0L

  for (i in seq_len(ncol(tabla1))) {
    x <- .valores_relacion(tabla1[[i]])
    x <- x[!is.na(x)]
    for (j in seq_len(ncol(tabla2))) {
      y <- .valores_relacion(tabla2[[j]])
      y <- y[!is.na(y)]
      k <- k + 1L
      unicos_x <- unique(x)
      unicos_y <- unique(y)
      n_valores_comunes <- length(intersect(unicos_x, unicos_y))
      unico_x <- anyDuplicated(x) == 0L
      unico_y <- anyDuplicated(y) == 0L
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
        cobertura_tabla1_en_tabla2 = if (length(x)) mean(x %in% unicos_y) else NA_real_,
        cobertura_tabla2_en_tabla1 = if (length(y)) mean(y %in% unicos_x) else NA_real_,
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
  resultado
}
