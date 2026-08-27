.limite_clave_dependencia <- 2^53

.clave_dependencia_segura <- function(k_x, k_y) {
  as.numeric(k_x) * as.numeric(k_y) <= .limite_clave_dependencia
}

.codificar_parejas_dependencia <- function(grupo, y) {
  valores_y <- unique(y)
  k_x <- length(unique(grupo))
  k_y <- length(valores_y)
  if (.clave_dependencia_segura(k_x, k_y)) {
    clave <- (as.numeric(grupo) - 1) * k_y + match(y, valores_y)
    return(match(clave, unique(clave)))
  }
  as.integer(interaction(
    factor(grupo), factor(y, exclude = NULL), drop = TRUE, lex.order = TRUE
  ))
}

.particion_dependencia <- function(x, y) {
  validos <- !is.na(x) & !is.na(y)
  x <- .valores_relacion(x[validos])
  y <- .valores_relacion(y[validos])
  n <- length(x)
  if (!n) {
    return(list(
      n = 0L, x = x, y = y, grupo = integer(), pareja = integer(),
      conteos = integer(), grupos = 0L
    ))
  }
  valores_x <- unique(x)
  grupo <- match(x, valores_x)
  pareja <- .codificar_parejas_dependencia(grupo, y)
  list(
    n = n, x = x, y = y, grupo = grupo, pareja = pareja,
    conteos = tabulate(pareja), grupos = length(valores_x)
  )
}

.resumen_dependencia <- function(x, y, particion = NULL) {
  if (is.null(particion)) particion <- .particion_dependencia(x, y)
  n <- particion$n
  if (!n) {
    return(list(cumplimiento = NA_real_, n = 0L, grupos = 0L,
                violaciones = 0L, grupos_conflicto = integer()))
  }
  grupo <- particion$grupo
  pareja <- particion$pareja
  conteos <- particion$conteos
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
    grupos = particion$grupos,
    violaciones = n - conformes,
    grupos_conflicto = grupos_conflicto,
    grupo = grupo,
    x = particion$x,
    y = particion$y
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

# Si faltan valores en cualquiera de las dos columnas, las cardinalidades de
# `estadisticas` no pertenecen al mismo subconjunto que `.resumen_dependencia`
# y no sirven para acotar nada. Sin ausentes, en cambio, ya son exactamente las
# cardinalidades del subconjunto valido y la cota es segura: cada grupo de X
# puede aportar como maximo un valor modal de Y, asi que al menos `k_y - k_x`
# apariciones contradicen X -> Y. La cota de pares distinta es mas fuerte, pero
# reutiliza la particion que de todos modos necesita el resumen cuando el par
# sobrevive: si hay `P` pares distintos, cada uno de los `k_x` grupos puede
# conservar como maximo una pareja modal, asi que hay al menos `P - k_x`
# violaciones. `P` se calcula sobre el subconjunto valido.
.poda_dependencia_cardinalidad <- function(estadistica_x, estadistica_y,
                                           n, umbral) {
  if (estadistica_x$n != n || estadistica_y$n != n) return(FALSE)
  # La comparacion NO se escribe `d > n * (1 - umbral)`: `1 - 0.8` da
  # 0,19999999999999996 y `5 * (1 - 0.8)` da 0,99999999999999978, asi que con
  # `d = 1` la resta dispara la poda y se pierde un par cuyo cumplimiento vale
  # exactamente el umbral -y el filtro de informe es `cumplimiento < umbral`,
  # o sea que igualarlo SI se informa-. Escrita como el maximo alcanzable
  # contra lo que el umbral exige, el borde no se pierde: medido sobre 200.000
  # casos, la forma anterior podaba de mas 88 veces y esta ninguna.
  n - (estadistica_y$n_distintos - estadistica_x$n_distintos) < n * umbral
}

.poda_dependencia_pares <- function(particion, umbral) {
  if (!particion$n) return(FALSE)
  # Misma forma que la cota debil, y por el mismo motivo de exactitud.
  particion$n - (length(particion$conteos) - particion$grupos) <
    particion$n * umbral
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
#' El costo crece aproximadamente como `columnas^2 * filas`. `max_columnas`
#' conserva las primeras columnas analizables, `max_comparaciones` limita el
#' número de pares columna a columna y `muestra` aplica una única muestra
#' sistemática a toda la tabla, de modo que las relaciones entre filas no se
#' rompen. `max_trabajo` acota el producto entre pares posibles y filas
#' analizadas. Es un presupuesto estimado, no una medición de segundos: permite
#' que el tope efectivo de pares baje cuando la tabla tiene muchas filas. Los
#' atributos del resultado declaran todos los recortes.
#'
#' @param datos Tabla que se desea examinar.
#' @param umbral Cumplimiento mínimo en `[0, 1]`.
#' @param muestra Máximo de filas; `Inf` desactiva el muestreo.
#' @param max_columnas Máximo de columnas analizadas.
#' @param max_comparaciones Máximo de pares determinante-dependiente que se
#'   comparan. `Inf` desactiva el presupuesto.
#' @param max_trabajo Máximo de unidades fila-par estimadas. `Inf` desactiva el
#'   presupuesto que escala con las filas; se combina con
#'   `max_comparaciones` y se aplica el límite más restrictivo.
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
#'   `truncado` documentan el alcance efectivo. `n_pares_posibles`,
#'   `n_pares_comparados`, `n_pares_sin_comparar` y `max_comparaciones`
#'   documentan el presupuesto de comparaciones. `trabajo_estimado`,
#'   `trabajo_comparado`, `trabajo_sin_comparar`, `unidad_trabajo` y
#'   `max_trabajo` documentan el presupuesto por filas. `columnas_descartadas`
#'   es un data frame que explica por qué una columna no se usó como
#'   determinante.
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
                                  max_ejemplos = 5L,
                                   max_comparaciones = 200000L,
                                   max_trabajo = 100000000) {
  .validar_datos_tabla(datos)
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
  if (!is.numeric(max_comparaciones) || length(max_comparaciones) != 1L ||
      is.na(max_comparaciones) || max_comparaciones <= 0 ||
      (!is.infinite(max_comparaciones) &&
       max_comparaciones != floor(max_comparaciones))) {
    stop(
      "`max_comparaciones` debe ser un entero positivo o Inf.",
      call. = FALSE
  )
  }
  max_comparaciones <- if (is.infinite(max_comparaciones)) {
    Inf
  } else as.numeric(max_comparaciones)
  if (!is.numeric(max_trabajo) || length(max_trabajo) != 1L ||
      is.na(max_trabajo) || max_trabajo <= 0 ||
      (!is.infinite(max_trabajo) && max_trabajo != floor(max_trabajo))) {
    stop(
      "`max_trabajo` debe ser un entero positivo o Inf.",
      call. = FALSE
  )
  }
  max_trabajo <- if (is.infinite(max_trabajo)) Inf else as.numeric(max_trabajo)
  if (!is.logical(incluir_claves) || length(incluir_claves) != 1L ||
      is.na(incluir_claves)) {
    stop("`incluir_claves` debe ser un l\u00f3gico escalar sin NA.", call. = FALSE)
  }
  # `raw` entra por aca aunque sea un vector atomico: agrupar exige ordenar, y
  # `order()` no implementa ese tipo. Sin este filtro, una columna `raw` no
  # produce un diagnostico sino un error crudo de R -"tipo no implementado 'raw'
  # en 'orderVector1'"- que aborta el perfil entero. Un tipo que no se puede
  # agrupar se declara, no revienta.
  analizables <- which(!vapply(datos, function(x) {
    is.list(x) || is.matrix(x) || is.raw(x)
  }, logical(1L)))
  seleccion <- utils::head(analizables, as.integer(max_columnas))
  nombres <- make.unique(names(datos))
  muestreo <- .muestrear_vector(seq_len(nrow(datos)), limite)
  # `as.data.frame()` antes de recortar: sobre un objeto `sf`, seleccionar
  # columnas vuelve a pegar la geometria que `seleccion` habia excluido, y
  # entonces `muestra_datos` queda con mas columnas que `seleccion`. Ademas de
  # traer al analisis una columna que no corresponde, desalinea los indices con
  # los que despues se buscan los nombres.
  muestra_datos <- as.data.frame(datos)[muestreo$valores, seleccion, drop = FALSE]
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
  comparaciones_posibles <- if (length(determinantes)) {
    sum(vapply(determinantes, function(i) {
      sum(dependientes_variables[seq_along(seleccion)] &
        seq_along(seleccion) != i)
    }, numeric(1L)))
  } else 0
  filas_trabajo <- muestreo$analizados
  trabajo_estimado <- as.numeric(comparaciones_posibles) * filas_trabajo
  max_comparaciones_trabajo <- if (is.infinite(max_trabajo) ||
      filas_trabajo == 0L) {
    Inf
  } else floor(max_trabajo / filas_trabajo)
  max_comparaciones_efectivo <- min(
    max_comparaciones, max_comparaciones_trabajo
  )
  filas <- list()
  k <- 0L
  comparaciones <- 0
  presupuesto_agotado <- FALSE

  if (length(seleccion) >= 2L && length(determinantes)) {
    for (i in determinantes) {
      x <- muestra_datos[[i]]
      for (j in seq_along(seleccion)) {
        if (i == j || !dependientes_variables[[j]]) next
        if (is.finite(max_comparaciones_efectivo) &&
            comparaciones >= max_comparaciones_efectivo) {
          presupuesto_agotado <- TRUE
          break
        }
        comparaciones <- comparaciones + 1
        if (.poda_dependencia_cardinalidad(
            estadisticas[[i]], estadisticas[[j]], filas_trabajo, umbral
        )) next
        particion <- .particion_dependencia(x, muestra_datos[[j]])
        if (.poda_dependencia_pares(particion, umbral)) next
        resumen <- .resumen_dependencia(
          x, muestra_datos[[j]], particion = particion
        )
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
      if (presupuesto_agotado) break
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
  # Quedar fuera por el tope y quedar fuera porque el tipo no se puede agrupar
  # son dos hechos distintos, y el motivo que se informa tiene que decir cual
  # fue: subir `max_columnas` resuelve el primero y no hace nada con el segundo.
  attr(resultado, "columnas_no_analizables") <-
    nombres[setdiff(seq_along(datos), analizables)]
  attr(resultado, "columnas_descartadas") <- data.frame(
    columna = nombres[seleccion[nzchar(motivos)]],
    motivo = unname(motivos[nzchar(motivos)]),
    stringsAsFactors = FALSE
  )
  attr(resultado, "n_pares_posibles") <- as.numeric(comparaciones_posibles)
  attr(resultado, "n_pares_comparados") <- as.numeric(comparaciones)
  attr(resultado, "n_pares_sin_comparar") <- as.numeric(
    max(0, comparaciones_posibles - comparaciones)
   )
  attr(resultado, "max_comparaciones") <- max_comparaciones
   attr(resultado, "max_comparaciones_efectivo") <-
     max_comparaciones_efectivo
   attr(resultado, "trabajo_estimado") <- trabajo_estimado
   attr(resultado, "trabajo_comparado") <- as.numeric(comparaciones) * filas_trabajo
   attr(resultado, "trabajo_sin_comparar") <- as.numeric(
     max(0, trabajo_estimado - attr(resultado, "trabajo_comparado"))
   )
   attr(resultado, "unidad_trabajo") <- "fila-par"
   attr(resultado, "max_trabajo") <- max_trabajo
  attr(resultado, "presupuesto_agotado") <- presupuesto_agotado
  attr(resultado, "truncado") <- length(seleccion) < length(analizables) ||
    presupuesto_agotado
  # El tope aplicado se conserva: sin el, quien ve columnas omitidas no puede
  # saber contra que se recortaron ni que valor pasar para evitarlo.
  attr(resultado, "max_columnas") <- as.integer(max_columnas)
  attr(resultado, "seleccion_posicional") <- TRUE
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

# El recorte por cantidad de columnas quedaba solo en los atributos del objeto
# de dependencias, mientras el recorte hermano de la busqueda aritmetica si
# declaraba el suyo en `cobertura_diagnosticos`. Es el mismo hecho y el usuario
# lo busca en el mismo lugar.
.cobertura_dependencias <- function(dependencias) {
  no_analizables <- attr(
    dependencias, "columnas_no_analizables", exact = TRUE
  )
  truncado_columnas <- length(setdiff(
    attr(dependencias, "columnas_omitidas", exact = TRUE), no_analizables
  )) > 0L
  presupuesto_agotado <- isTRUE(attr(
    dependencias, "presupuesto_agotado", exact = TRUE
  ))
  if (!truncado_columnas && !presupuesto_agotado &&
      !length(no_analizables)) {
    return(NULL)
  }
  omitidas <- attr(dependencias, "columnas_omitidas", exact = TRUE)
  analizadas <- attr(dependencias, "columnas_analizadas", exact = TRUE)
  tope <- attr(dependencias, "max_columnas", exact = TRUE)
  diagnostico_columna <- if (length(omitidas)) {
    paste(omitidas, collapse = ",")
  } else {
    paste(analizadas, collapse = ",")
  }
  motivos <- character()
  soluciones <- character()
  if (length(no_analizables)) {
    motivos <- c(motivos, paste0(
      "No se buscaron dependencias en ", length(no_analizables),
      " columnas cuyo tipo no se puede agrupar -listas, matrices y vectores ",
      "de bytes-: ", paste(no_analizables, collapse = ", "), "."
    ))
    soluciones <- c(soluciones, paste0(
      "Convertir la columna a un tipo comparable si tiene que intervenir en ",
      "el diagn\u00f3stico; el tope de columnas no cambia esto."
    ))
  }
  if (truncado_columnas) {
    motivos <- c(motivos, paste0(
      "La b\u00fasqueda de dependencias se limit\u00f3 a las primeras ",
      if (is.null(tope)) length(analizadas) else tope,
      " columnas analizables por posici\u00f3n: analiz\u00f3 ", length(analizadas),
      " y dej\u00f3 ", length(omitidas), " fuera del diagn\u00f3stico."
    ))
    soluciones <- c(soluciones, paste0(
      "Aumentar `max_columnas_dependencias` o perfilar por bloques si las ",
      "columnas omitidas deben intervenir. La selecci\u00f3n es por posici\u00f3n, ",
      "as\u00ed que reordenar las columnas cambia cu\u00e1les entran."
    ))
  }
  if (presupuesto_agotado) {
    motivos <- c(motivos, paste0(
      "El presupuesto de trabajo se agot\u00f3: se compararon ",
      attr(dependencias, "n_pares_comparados", exact = TRUE), " de ",
      attr(dependencias, "n_pares_posibles", exact = TRUE),
      " pares determinante-dependiente y ",
      attr(dependencias, "trabajo_comparado", exact = TRUE), " de ",
      attr(dependencias, "trabajo_estimado", exact = TRUE),
      " unidades fila-par; quedaron ",
      attr(dependencias, "n_pares_sin_comparar", exact = TRUE),
      " pares y ", attr(dependencias, "trabajo_sin_comparar", exact = TRUE),
      " unidades sin comparar."
    ))
    soluciones <- c(soluciones, paste0(
      "Aumentar `max_trabajo` o `max_comparaciones`, o reducir `muestra`, si se ",
      "necesita cubrir m\u00e1s pares. El costo aproximado es O(columnas^2 x filas)."
    ))
  }
  .nuevo_diagnostico_no_evaluado(
    "dependencias_funcionales",
    diagnostico_columna,
    paste(motivos, collapse = " "),
    paste(soluciones, collapse = " ")
  )
}
