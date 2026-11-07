# M2: valor modal frente al segundo valor mas frecuente. Medido: cero falsos
# positivos en 114 columnas limpias y un margen de 2,2x con k = 5 y fraccion
# minima = 0,15.
#
# La elegibilidad es parte de la senal: una columna numerica necesita al menos
# 20 valores validos y 10 valores distintos. Con menos categorias la moda es la
# distribucion y no hay una concentracion que tenga sentido diagnosticar.
.MIN_VALIDOS_VALOR_CONCENTRADO <- 20L
.MIN_DISTINTOS_VALOR_CONCENTRADO <- 10L
# Cuantas posiciones de la distribucion de frecuencias se miran al buscar el
# acantilado que delata un centinela. El grupo que sobresale esta arriba; en la
# cola una caida de 2 a 1 da un salto de 2 que no dice nada de la columna.
.TOPE_SALTO_FRECUENCIAS <- 5L

.FACTOR_VALOR_CONCENTRADO <- 5
.MIN_FRACCION_VALOR_CONCENTRADO <- 0.15

# `sentinelas_declarados`: los valores que el USUARIO declaro ausencia
# (`.sentinelas_numericos_declarados()`). Salen de la cuenta como salen del
# resumen: con `-99` declarado en 180 de 1.000 filas, el resumen los dejaba
# afuera -minimo 1245,84, media sobre 820 valores- y este hallazgo publicaba la
# moda `-99` con 180 casos y una "fraccion sobre validos" de 0,180 calculada
# sobre las 1.000. La misma celda era ausencia en el resumen y valor aca. La
# lista por omision no es una declaracion y no saca nada, igual que en el
# resumen.
.estadisticos_valor_concentrado <- function(x, sentinelas_declarados = NULL) {
  if (!is.numeric(x)) return(NULL)
  validos <- !is.na(x)
  if (length(sentinelas_declarados) && .es_numerico_pelado(x) &&
      !.es_columna_compuesta(x)) {
    validos <- validos &
      !.mascara_sentinelas_resumen(as.numeric(x), sentinelas_declarados)
  }
  n_validos <- sum(validos)
  if (n_validos < .MIN_VALIDOS_VALOR_CONCENTRADO) return(NULL)

  valores <- x[validos]
  unicos <- unique(valores)
  if (length(unicos) < .MIN_DISTINTOS_VALOR_CONCENTRADO) return(NULL)
  indices <- match(valores, unicos)
  frecuencias <- tabulate(indices, nbins = length(unicos))
  orden <- tryCatch(
    .orden_por_bytes(-frecuencias, unicos),
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
