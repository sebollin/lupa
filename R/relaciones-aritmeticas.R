.es_columna_aritmetica <- function(x) {
  is.numeric(x) &&
    !inherits(x, c("Date", "POSIXt", "difftime", "integer64"))
}

.texto_tolerancia_aritmetica <- function(tolerancia) {
  format(tolerancia, scientific = TRUE, trim = TRUE, digits = 15L)
}

.dentro_tolerancia_aritmetica <- function(observado, esperado, tolerancia) {
  abs(observado - esperado) <=
    tolerancia * pmax(1, abs(observado), abs(esperado))
}

.varia_columna_aritmetica <- function(x, tolerancia) {
  x <- x[is.finite(x)]
  if (length(x) < 3L) return(FALSE)
  extremos <- range(x)
  (extremos[[2L]] - extremos[[1L]]) >
    tolerancia * max(1, abs(extremos))
}

.cumple_criterio_aritmetica <- function(proporcion, umbral) {
  is.finite(proporcion) && proporcion >= umbral
}

.texto_criterio_aritmetica <- function(umbral, min_filas) {
  paste0(
    "Criterio de reconocimiento declarado: cumplimiento >= ",
    format(umbral, scientific = FALSE, trim = TRUE, digits = 15L),
    " dentro de la tolerancia, con al menos ", min_filas,
    " filas comparables. Una vez reconocida la relaci\u00f3n, se informan ",
    "todas sus discrepancias. "
  )
}

.alcance_aritmetica_columnas <- function(datos, numericas, seleccion,
                                          umbral, min_filas, tolerancia,
                                          max_columnas) {
  n_numericas <- length(numericas)
  n_analizadas <- length(seleccion)
  identidades <- function(n) if (n < 3L) 0 else 3 * choose(n, 3L)
  pares <- function(n) if (n < 2L) 0 else choose(n, 2L)
  list(
    filas_totales = as.numeric(nrow(datos)),
    columnas_numericas = names(datos)[numericas],
    columnas_analizadas = names(datos)[seleccion],
    columnas_omitidas = names(datos)[setdiff(numericas, seleccion)],
    identidades_aditivas_posibles = as.numeric(identidades(n_numericas)),
    identidades_aditivas_analizadas = as.numeric(identidades(n_analizadas)),
    pares_proporcionales_posibles = as.numeric(pares(n_numericas)),
    pares_proporcionales_analizados = as.numeric(pares(n_analizadas)),
    truncado = n_numericas > max_columnas,
    max_columnas = as.integer(max_columnas),
    umbral_cumplimiento = as.numeric(umbral),
    tolerancia = as.numeric(tolerancia),
    criterio_tolerancia = paste0(
      "|observado - esperado| <= tolerancia * ",
      "max(1, |observado|, |esperado|)"
    ),
    minimo_filas_comparables = as.integer(min_filas),
    clases_excluidas = c("Date", "POSIXt", "difftime", "integer64")
  )
}

.cobertura_aritmetica_columnas <- function(alcance) {
  # Sin filas suficientes la busqueda no puede correr. Callarse dejaria un
  # diagnostico ausente sin rastro, que es justo lo que el paquete no hace.
  #
  # Solo se declara cuando habia algo que buscar: si no hay combinaciones de
  # columnas numericas candidatas, no hay diagnostico que dejar de evaluar y
  # anunciarlo seria ruido en vez de alcance.
  habia_que_buscar <- alcance$identidades_aditivas_posibles > 0 ||
    alcance$pares_proporcionales_posibles > 0
  sin_filas <- habia_que_buscar &&
    is.finite(alcance$filas_totales) &&
    alcance$filas_totales < alcance$minimo_filas_comparables
  if (sin_filas) {
    faltante <- .nuevo_diagnostico_no_evaluado(
      "relacion_aritmetica_columnas",
      paste(alcance$columnas_numericas, collapse = ","),
      paste0(
        "No se buscaron relaciones aritmeticas: la tabla tiene ",
        alcance$filas_totales,
        if (identical(alcance$filas_totales, 1)) " fila" else " filas",
        " y se necesitan al menos ",
        alcance$minimo_filas_comparables, " filas comparables."
      ),
      paste0(
        "Perfilar una tabla con al menos ",
        alcance$minimo_filas_comparables,
        " filas, o bajar `min_filas_aritmetica`."
      )
    )
    if (!isTRUE(alcance$truncado)) return(faltante)
  }
  if (!isTRUE(alcance$truncado)) return(.cobertura_diagnosticos_vacia())
  omitidas <- alcance$columnas_omitidas
  .nuevo_diagnostico_no_evaluado(
    "relacion_aritmetica_columnas",
    paste(omitidas, collapse = ","),
    paste0(
      "La b\u00fasqueda aritm\u00e9tica se limit\u00f3 a las primeras ",
      alcance$max_columnas, " columnas num\u00e9ricas: analiz\u00f3 ",
      alcance$identidades_aditivas_analizadas, " de ",
      alcance$identidades_aditivas_posibles, " identidades aditivas y ",
      alcance$pares_proporcionales_analizados, " de ",
      alcance$pares_proporcionales_posibles, " pares proporcionales."
    ),
    paste0(
      "Aumentar `max_columnas_aritmetica` o perfilar por bloques si las ",
      "columnas omitidas deben intervenir en el diagn\u00f3stico."
    )
  )
}

.evidencia_filas_aritmetica <- function(datos, indices, columnas, esperado,
                                         etiqueta_esperado) {
  ejemplos <- utils::head(indices, 5L)
  if (!length(ejemplos)) return("sin filas discrepantes")
  paste(vapply(seq_along(ejemplos), function(i) {
    fila <- ejemplos[[i]]
    valores <- paste(vapply(columnas, function(columna) {
      paste0(names(datos)[[columna]], "=", .texto_valor(datos[[columna]][fila]))
    }, character(1L)), collapse = "; ")
    paste0(
      "fila ", fila, ": ", valores, "; ", etiqueta_esperado, "=",
      .texto_valor(esperado[[i]])
    )
  }, character(1L)), collapse = " | ")
}

.hallazgo_identidad_aditiva <- function(datos, sumandos, total, umbral,
                                         min_filas, tolerancia) {
  x <- datos[[sumandos[[1L]]]]
  y <- datos[[sumandos[[2L]]]]
  z <- datos[[total]]
  comparables <- is.finite(x) & is.finite(y) & is.finite(z)
  n_evaluados <- sum(comparables)
  if (n_evaluados < min_filas ||
      !all(vapply(list(x[comparables], y[comparables], z[comparables]),
                  .varia_columna_aritmetica, logical(1L),
                  tolerancia = tolerancia))) {
    return(NULL)
  }
  esperado <- x[comparables] + y[comparables]
  cumple <- .dentro_tolerancia_aritmetica(
    z[comparables], esperado, tolerancia
  )
  proporcion <- mean(cumple)
  if (!.cumple_criterio_aritmetica(proporcion, umbral)) return(NULL)
  filas_comparables <- which(comparables)
  indices_incumplen <- filas_comparables[!cumple]
  esperados_incumplen <- esperado[!cumple]
  nombres <- names(datos)
  expresion <- paste0(
    nombres[[sumandos[[1L]]]], " + ", nombres[[sumandos[[2L]]]],
    " ~= ", nombres[[total]]
  )
  alternativa <- paste0(
    nombres[[sumandos[[1L]]]], " ~= ", nombres[[total]], " - ",
    nombres[[sumandos[[2L]]]]
  )
  tolerancia_texto <- .texto_tolerancia_aritmetica(tolerancia)
  ejemplos <- .evidencia_filas_aritmetica(
    datos, indices_incumplen, c(sumandos, total), esperados_incumplen,
    paste0(nombres[[total]], " esperado")
  )
  hallazgo <- .nuevo_hallazgo(
    paste(nombres[c(sumandos, total)], collapse = ","),
    "relacion_aritmetica_columnas",
    if (length(indices_incumplen)) "sospechoso" else "ok",
    paste0(
      "Se observ\u00f3 la regularidad aritm\u00e9tica ", expresion,
      if (length(indices_incumplen)) {
        " y se rompe en una minor\u00eda de las filas comparables."
      } else " en todas las filas comparables."
    ),
    paste0(
      sprintf("%.3f de cumplimiento; %d de %d filas discrepantes (universo: %d de %d filas con valores finitos en las tres columnas). ",
              proporcion, length(indices_incumplen), n_evaluados,
              n_evaluados, nrow(datos)),
      .texto_criterio_aritmetica(umbral, min_filas),
      "Forma equivalente observada: ", alternativa, ". ",
      "Tolerancia declarada: ", tolerancia_texto,
      "; criterio |observado - esperado| <= ", tolerancia_texto,
      " * max(1, |observado|, |esperado|). ", ejemplos
    ),
    paste0(
      "Evaluar si la regularidad observada corresponde a una regla del ",
      "dominio; ",
      if (length(indices_incumplen)) {
        "revisar las filas se\u00f1aladas antes de corregirlas."
      } else "no convertirla en regla sin confirmaci\u00f3n del dominio."
    ),
    n_evaluados, length(indices_incumplen), "fila"
  )
  hallazgo$trazabilidad[[1L]] <- .trazabilidad_indices(
    indices_incumplen, "filas_finitas_en_las_columnas_involucradas",
    limite = Inf
  )
  attr(hallazgo, "columnas_aritmetica") <- list(
    sumandos = sumandos, total = total
  )
  hallazgo
}

.hallazgo_proporcional <- function(datos, par, umbral, min_filas,
                                    tolerancia) {
  primero <- datos[[par[[1L]]]]
  segundo <- datos[[par[[2L]]]]
  base <- primero
  respuesta <- segundo
  indice_base <- par[[1L]]
  indice_respuesta <- par[[2L]]
  utilizables <- is.finite(base) & is.finite(respuesta) & base != 0
  if (sum(utilizables) < min_filas) return(NULL)
  constante <- stats::median(respuesta[utilizables] / base[utilizables])
  if (!is.finite(constante) || constante == 0) return(NULL)
  if (abs(constante) > 1) {
    base <- segundo
    respuesta <- primero
    indice_base <- par[[2L]]
    indice_respuesta <- par[[1L]]
    utilizables <- is.finite(base) & is.finite(respuesta) & base != 0
    if (sum(utilizables) < min_filas) return(NULL)
    constante <- stats::median(respuesta[utilizables] / base[utilizables])
  }
  if (!is.finite(constante) ||
      abs(abs(constante) - 1) <= tolerancia ||
      !.varia_columna_aritmetica(base[utilizables], tolerancia) ||
      !.varia_columna_aritmetica(respuesta[utilizables], tolerancia)) {
    return(NULL)
  }
  comparables <- is.finite(base) & is.finite(respuesta)
  n_evaluados <- sum(comparables)
  if (n_evaluados < min_filas) return(NULL)
  esperado <- base[comparables] * constante
  cumple <- .dentro_tolerancia_aritmetica(
    respuesta[comparables], esperado, tolerancia
  )
  proporcion <- mean(cumple)
  if (!.cumple_criterio_aritmetica(proporcion, umbral)) return(NULL)
  filas_comparables <- which(comparables)
  indices_incumplen <- filas_comparables[!cumple]
  esperados_incumplen <- esperado[!cumple]
  nombres <- names(datos)
  constante_texto <- format(
    constante, scientific = FALSE, trim = TRUE, digits = 15L
  )
  expresion <- paste0(
    nombres[[indice_respuesta]], " ~= ", nombres[[indice_base]], " * ",
    constante_texto
  )
  inversa <- paste0(
    nombres[[indice_base]], " ~= ", nombres[[indice_respuesta]], " / ",
    constante_texto
  )
  tolerancia_texto <- .texto_tolerancia_aritmetica(tolerancia)
  ejemplos <- .evidencia_filas_aritmetica(
    datos, indices_incumplen, c(indice_base, indice_respuesta),
    esperados_incumplen, paste0(nombres[[indice_respuesta]], " esperado")
  )
  hallazgo <- .nuevo_hallazgo(
    paste(nombres[c(indice_base, indice_respuesta)], collapse = ","),
    "relacion_aritmetica_columnas",
    if (length(indices_incumplen)) "sospechoso" else "ok",
    paste0(
      "Se observ\u00f3 la regularidad proporcional ", expresion,
      if (length(indices_incumplen)) {
        " y se rompe en una minor\u00eda de las filas comparables."
      } else " en todas las filas comparables."
    ),
    paste0(
      sprintf("%.3f de cumplimiento; %d de %d filas discrepantes (universo: %d de %d filas con valores finitos en ambas columnas). ",
              proporcion, length(indices_incumplen), n_evaluados,
              n_evaluados, nrow(datos)),
      .texto_criterio_aritmetica(umbral, min_filas),
      "Constante observada k=", constante_texto,
      " (mediana de respuesta/base en ", sum(utilizables),
      " filas finitas con base distinta de cero); forma equivalente: ",
      inversa, ". Tolerancia declarada: ", tolerancia_texto,
      "; criterio |observado - esperado| <= ", tolerancia_texto,
      " * max(1, |observado|, |esperado|). ", ejemplos
    ),
    paste0(
      "Evaluar si la proporcionalidad observada corresponde a una regla del ",
      "dominio; ",
      if (length(indices_incumplen)) {
        "revisar las filas se\u00f1aladas antes de corregirlas."
      } else "no convertirla en regla sin confirmaci\u00f3n del dominio."
    ),
    n_evaluados, length(indices_incumplen), "fila"
  )
  hallazgo$trazabilidad[[1L]] <- .trazabilidad_indices(
    indices_incumplen, "filas_finitas_en_las_columnas_involucradas",
    limite = Inf
  )
  hallazgo
}

.detectar_aritmetica_columnas <- function(datos, umbral = 0.9,
                                           min_filas = 3L,
                                           tolerancia = 1e-8,
                                           max_columnas = 20L) {
  numericas <- which(vapply(datos, .es_columna_aritmetica, logical(1L)))
  seleccion <- utils::head(numericas, max_columnas)
  alcance <- .alcance_aritmetica_columnas(
    datos, numericas, seleccion, umbral, min_filas, tolerancia, max_columnas
  )
  cobertura <- .cobertura_aritmetica_columnas(alcance)
  hallazgos_aditivos <- list()
  objetivos_aditivos <- list()
  if (length(seleccion) >= 3L && nrow(datos)) {
    ternas <- utils::combn(seleccion, 3L, simplify = FALSE)
    for (terna in ternas) {
      candidatos <- lapply(seq_len(3L), function(objetivo) {
        .hallazgo_identidad_aditiva(
          datos, terna[-objetivo], terna[[objetivo]], umbral,
          min_filas, tolerancia
        )
      })
      candidatos <- Filter(Negate(is.null), candidatos)
      if (!length(candidatos)) next
      afectados <- vapply(candidatos, function(x) x$n_afectados[[1L]], numeric(1L))
      elegido <- candidatos[[which.min(afectados)]]
      columnas_elegidas <- attr(elegido, "columnas_aritmetica", exact = TRUE)
      attr(elegido, "columnas_aritmetica") <- NULL
      hallazgos_aditivos[[length(hallazgos_aditivos) + 1L]] <- elegido
      objetivos_aditivos[[length(objetivos_aditivos) + 1L]] <- columnas_elegidas
    }
  }

  hallazgos_proporcionales <- list()
  if (length(seleccion) >= 2L && nrow(datos)) {
    pares <- utils::combn(seleccion, 2L, simplify = FALSE)
    for (par in pares) {
      # Si una identidad aditiva ya vincula la terna, se omiten las dos
      # proporcionalidades redundantes que incluyen su total. La relación
      # entre los sumandos se conserva (por ejemplo, iva = neto * 0.22).
      redundante <- any(vapply(objetivos_aditivos, function(x) {
        x$total %in% par && any(x$sumandos %in% par)
      }, logical(1L)))
      if (redundante) next
      hallazgo <- .hallazgo_proporcional(
        datos, par, umbral, min_filas, tolerancia
      )
      if (!is.null(hallazgo)) {
        hallazgos_proporcionales[[length(hallazgos_proporcionales) + 1L]] <-
          hallazgo
      }
    }
  }
  list(
    hallazgos = c(hallazgos_aditivos, hallazgos_proporcionales),
    alcance = alcance,
    cobertura = cobertura
  )
}
