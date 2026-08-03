.resumen_dependencia <- function(x, y) {
  validos <- !is.na(x) & !is.na(y)
  x <- .valores_relacion(x[validos])
  y <- .valores_relacion(y[validos])
  n <- length(x)
  if (!n) {
    return(list(cumplimiento = NA_real_, n = 0L, grupos = 0L,
                violaciones = 0L, grupos_conflicto = integer()))
  }
  grupo <- match(x, unique(x))
  pareja <- as.integer(interaction(
    factor(grupo), factor(y, exclude = NULL), drop = TRUE, lex.order = TRUE
  ))
  conteos <- tabulate(pareja)
  primera <- !duplicated(pareja)
  grupo_pareja <- integer(length(conteos))
  grupo_pareja[pareja[primera]] <- grupo[primera]
  maximos <- tapply(conteos, grupo_pareja, max)
  conformes <- sum(maximos)
  n_valores_por_grupo <- tabulate(grupo_pareja, nbins = max(grupo))
  grupos_conflicto <- which(n_valores_por_grupo > 1L)
  list(
    cumplimiento = conformes / n,
    n = n,
    grupos = length(unique(grupo)),
    violaciones = n - conformes,
    grupos_conflicto = grupos_conflicto,
    grupo = grupo,
    x = x,
    y = y
  )
}

.evidencia_dependencia <- function(resumen, max_ejemplos = 5L) {
  if (!length(resumen$grupos_conflicto)) return("")
  ejemplos <- utils::head(resumen$grupos_conflicto, max_ejemplos)
  paste(vapply(ejemplos, function(grupo) {
    indices <- which(resumen$grupo == grupo)
    paste0(
      encodeString(resumen$x[[indices[[1L]]]], quote = '"'), " -> ",
      paste(encodeString(unique(resumen$y[indices]), quote = '"'), collapse = " | ")
    )
  }, character(1L)), collapse = "; ")
}

#' Detectar dependencias funcionales entre columnas
#'
#' Busca pares ordenados `determinante -> dependiente`. El cumplimiento es la
#' proporción de filas que coincide con el valor modal del dependiente para
#' cada valor del determinante; por eso una dependencia aproximada señala
#' directamente las filas minoritarias que pueden ser errores de carga.
#'
#' Para evitar resultados vacíos o triviales, se omiten por defecto las claves
#' únicas, los determinantes cuya tasa de valores distintos alcanza
#' `umbral_casi_clave`, los determinantes cuya moda alcanza
#' `umbral_casi_constante` y los dependientes constantes. El
#' valor predeterminado de `umbral = 0.995` exige que como máximo 5 de cada
#' 1.000 filas contradigan la relación. Los ausentes de cualquiera de las dos
#' columnas no integran el cálculo.
#' El descarte ocurre antes de construir agrupaciones. El valor predeterminado
#' `umbral_casi_clave = 0.8` excluye determinantes con menos de 1,25 filas por
#' valor distinto en promedio: aun si cumplen, suelen describir una casi-clave
#' y no una regla reutilizable.
#'
#' El costo crece con el cuadrado de las columnas. `max_columnas` conserva las
#' primeras columnas analizables y `muestra` aplica una única muestra
#' sistemática a toda la tabla, de modo que las relaciones entre filas no se
#' rompen. Los atributos del resultado declaran ambos recortes.
#'
#' @param datos Tabla que se desea examinar.
#' @param umbral Cumplimiento mínimo en `[0, 1]`.
#' @param muestra Máximo de filas; `Inf` desactiva el muestreo.
#' @param max_columnas Máximo de columnas analizadas.
#' @param umbral_casi_constante Proporción modal a partir de la cual un
#'   determinante se descarta por casi constante.
#' @param umbral_casi_clave Tasa de valores distintos a partir de la cual un
#'   determinante se descarta por casi clave, salvo que `incluir_claves` sea
#'   verdadero.
#' @param incluir_claves Si se incluyen determinantes únicos, que satisfacen
#'   dependencias de forma trivial.
#' @param min_observaciones Mínimo de pares presentes para informar una
#'   dependencia.
#' @param max_ejemplos Máximo de contradicciones concretas en `evidencia`.
#'
#' @return Data frame de clase `dependencias_funcionales`, ordenado por
#'   cumplimiento y soporte. Los atributos `muestreado`, `filas_analizadas`,
#'   `columnas_analizadas`, `columnas_omitidas`, `columnas_descartadas` y
#'   `truncado` documentan el alcance efectivo. `columnas_descartadas` es un
#'   data frame que explica por qué una columna no se usó como determinante.
#' @export
#'
#' @seealso [detectar_claves()], [proponer_modelo()], [perfilar()]
#'
#' @examples
#' datos <- data.frame(
#'   codigo = rep(1:3, each = 4),
#'   descripcion = rep(c("A", "B", "C"), each = 4),
#'   valor = seq_len(12)
#' )
#' detectar_dependencias(datos, min_observaciones = 4)
detectar_dependencias <- function(datos, umbral = 0.995, muestra = 1e5,
                                  max_columnas = 100L,
                                  umbral_casi_constante = 0.95,
                                  umbral_casi_clave = 0.8,
                                  incluir_claves = FALSE,
                                  min_observaciones = 10L,
                                  max_ejemplos = 5L) {
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe heredar de data.frame.", call. = FALSE)
  }
  proporciones <- c(umbral, umbral_casi_constante, umbral_casi_clave)
  if (anyNA(proporciones) || any(!is.finite(proporciones)) ||
      any(proporciones < 0 | proporciones > 1)) {
    stop("Los umbrales deben ser proporciones finitas en [0, 1].",
         call. = FALSE)
  }
  limite <- .validar_muestra(muestra)
  enteros <- c(max_columnas, min_observaciones, max_ejemplos)
  if (anyNA(enteros) || any(!is.finite(enteros)) || any(enteros < 1) ||
      any(enteros != floor(enteros))) {
    stop("Los l\u00edmites deben ser enteros positivos.", call. = FALSE)
  }
  if (!is.logical(incluir_claves) || length(incluir_claves) != 1L ||
      is.na(incluir_claves)) {
    stop("`incluir_claves` debe ser un l\u00f3gico escalar sin NA.", call. = FALSE)
  }
  analizables <- which(!vapply(datos, function(x) {
    is.list(x) || is.matrix(x)
  }, logical(1L)))
  seleccion <- utils::head(analizables, as.integer(max_columnas))
  nombres <- make.unique(names(datos))
  muestreo <- .muestrear_vector(seq_len(nrow(datos)), limite)
  muestra_datos <- datos[muestreo$valores, seleccion, drop = FALSE]
  normalizados <- lapply(muestra_datos, .valores_relacion)
  estadisticas <- lapply(normalizados, function(x) {
    presentes <- x[!is.na(x)]
    n <- length(presentes)
    if (!n) {
      return(list(
        n = 0L, proporcion_moda = NA_real_, tasa_distintos = NA_real_,
        es_clave = FALSE, n_distintos = 0L
      ))
    }
    unicos <- unique(presentes)
    frecuencias <- tabulate(match(presentes, unicos), nbins = length(unicos))
    list(
      n = n,
      proporcion_moda = max(frecuencias) / n,
      tasa_distintos = length(unicos) / n,
      es_clave = length(unicos) == n,
      n_distintos = length(unicos)
    )
  })
  motivos <- vapply(estadisticas, function(x) {
    if (x$n < min_observaciones) return("soporte_insuficiente")
    if (x$proporcion_moda >= umbral_casi_constante) return("casi_constante")
    if (!incluir_claves &&
        (x$es_clave || x$tasa_distintos >= umbral_casi_clave)) {
      return("casi_clave")
    }
    ""
  }, character(1L))
  determinantes <- which(!nzchar(motivos))
  dependientes_variables <- vapply(
    estadisticas, function(x) x$n_distintos > 1L, logical(1L)
  )
  filas <- list()
  k <- 0L

  if (length(seleccion) >= 2L && length(determinantes)) {
    for (i in determinantes) {
      x <- muestra_datos[[i]]
      for (j in seq_along(seleccion)) {
        if (i == j || !dependientes_variables[[j]]) next
        resumen <- .resumen_dependencia(x, muestra_datos[[j]])
        if (resumen$n < min_observaciones ||
            !is.finite(resumen$cumplimiento) ||
            resumen$cumplimiento < umbral) next
        k <- k + 1L
        filas[[k]] <- data.frame(
          determinante = nombres[[seleccion[[i]]]],
          dependiente = nombres[[seleccion[[j]]]],
          cumplimiento = resumen$cumplimiento,
          n_evaluados = resumen$n,
          n_grupos = resumen$grupos,
          n_violaciones = resumen$violaciones,
          exacta = resumen$violaciones == 0L,
          evidencia = .evidencia_dependencia(resumen, max_ejemplos),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  resultado <- if (length(filas)) {
    do.call(rbind, filas)
  } else {
    data.frame(
      determinante = character(), dependiente = character(),
      cumplimiento = numeric(), n_evaluados = integer(),
      n_grupos = integer(), n_violaciones = integer(), exacta = logical(),
      evidencia = character(), stringsAsFactors = FALSE
    )
  }
  if (nrow(resultado)) {
    resultado <- resultado[order(
      -resultado$cumplimiento, -resultado$n_evaluados,
      resultado$determinante, resultado$dependiente
    ), , drop = FALSE]
    rownames(resultado) <- NULL
  }
  class(resultado) <- c("dependencias_funcionales", "data.frame")
  attr(resultado, "filas_totales") <- nrow(datos)
  attr(resultado, "filas_analizadas") <- muestreo$analizados
  attr(resultado, "muestreado") <- muestreo$muestreado
  attr(resultado, "columnas_analizadas") <- nombres[seleccion]
  attr(resultado, "columnas_omitidas") <- nombres[setdiff(seq_along(datos), seleccion)]
  attr(resultado, "columnas_descartadas") <- data.frame(
    columna = nombres[seleccion[nzchar(motivos)]],
    motivo = unname(motivos[nzchar(motivos)]),
    stringsAsFactors = FALSE
  )
  attr(resultado, "truncado") <- length(seleccion) < length(analizables)
  attr(resultado, "umbral") <- umbral
  resultado
}

.mapa_dependencia <- function(datos, determinante, dependiente,
                              soporte_minimo = 2L) {
  x <- datos[[determinante]]
  y <- datos[[dependiente]]
  validos <- !is.na(x) & !is.na(y)
  pares <- data.frame(
    determinante = x[validos], dependiente = y[validos],
    stringsAsFactors = FALSE
  )
  if (!nrow(pares)) return(pares)
  codigos <- .codigos_filas(pares["determinante"])
  grupos <- split(seq_len(nrow(pares)), codigos)
  filas <- lapply(grupos, function(indices) {
    valores <- unique(.valores_relacion(pares$dependiente[indices]))
    if (length(valores) != 1L || length(indices) < soporte_minimo) return(NULL)
    pares[indices[[1L]], , drop = FALSE]
  })
  filas <- Filter(Negate(is.null), filas)
  if (length(filas)) do.call(rbind, filas) else pares[0, , drop = FALSE]
}
