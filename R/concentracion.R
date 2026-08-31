# M2: valor modal frente al segundo valor mas frecuente. La medicion de
# .trabajo-agente/medicion-concentracion.md encontro cero falsos positivos en 114 columnas
# limpias y un margen de 2,2x con k = 5 y fraccion minima = 0,15.
#
# La elegibilidad es parte de la senal: una columna numerica necesita al menos
# 20 valores validos y 10 valores distintos. Con menos categorias la moda es la
# distribucion y no hay una concentracion que tenga sentido diagnosticar.
.MIN_VALIDOS_VALOR_CONCENTRADO <- 20L
.MIN_DISTINTOS_VALOR_CONCENTRADO <- 10L
.FACTOR_VALOR_CONCENTRADO <- 5
.MIN_FRACCION_VALOR_CONCENTRADO <- 0.15

.estadisticos_valor_concentrado <- function(x) {
  if (!is.numeric(x)) return(NULL)
  validos <- !is.na(x)
  n_validos <- sum(validos)
  if (n_validos < .MIN_VALIDOS_VALOR_CONCENTRADO) return(NULL)

  valores <- x[validos]
  unicos <- unique(valores)
  if (length(unicos) < .MIN_DISTINTOS_VALOR_CONCENTRADO) return(NULL)
  indices <- match(valores, unicos)
  frecuencias <- tabulate(indices, nbins = length(unicos))
  orden <- tryCatch(
    order(-frecuencias, unicos),
    error = function(e) NULL
  )
  if (is.null(orden) || length(orden) < 2L) return(NULL)

  frecuencia_moda <- as.numeric(frecuencias[[orden[[1L]]]])
  frecuencia_segundo <- as.numeric(frecuencias[[orden[[2L]]]])
  cociente <- frecuencia_moda / frecuencia_segundo
  fraccion <- frecuencia_moda / n_validos
  list(
    valor = unicos[[orden[[1L]]]],
    frecuencia_moda = frecuencia_moda,
    frecuencia_segundo = frecuencia_segundo,
    cociente = cociente,
    fraccion = fraccion,
    n_validos = as.numeric(n_validos),
    n_distintos = as.numeric(length(unicos)),
    dispara = isTRUE(cociente >= .FACTOR_VALOR_CONCENTRADO) &&
      isTRUE(fraccion >= .MIN_FRACCION_VALOR_CONCENTRADO)
  )
}
