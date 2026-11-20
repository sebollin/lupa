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

# Columna casi constante: un valor ocupa casi todos los valores presentes y los
# demas son pocos. Es la version "casi" de `constante`, y la senal que
# `caret::nearZeroVar()` llama varianza casi nula: razon entre la frecuencia del
# valor mas comun y la del segundo (por omision, mas de 19) y porcentaje de
# valores unicos (por omision, menos del 10 %). Esa regla marca a una de cada
# veinte filas distintas; aca se pide una proporcion modal de 0,99 porque el
# perfil prefiere callar a acusar una columna con un 5 % de datos propios.
#
# Es una propiedad observada, no un defecto: una tabla de un solo anio, pais o
# estado, una bandera `fallecido` con el 1 % de "si" o un conteo con el 99 % de
# ceros tienen esta forma y son informacion. El hallazgo se publica siempre con
# severidad `ok`, y la columna se describe sin calificarla.
#
# Con diez valores distintos o mas y una moda cinco veces mayor que la segunda
# el perfil ya emite `valor_concentrado`: no se repite el aviso.
.UMBRAL_CASI_CONSTANTE <- 0.99
.MIN_VALIDOS_CASI_CONSTANTE <- 100L

.casi_constante <- function(x, fila) {
  n_validos <- suppressWarnings(as.numeric(fila$n_aplicables - fila$n_faltantes))
  frecuencia <- suppressWarnings(as.numeric(fila$frecuencia_moda))
  distintos <- suppressWarnings(as.numeric(fila$n_distintos))
  if (length(n_validos) != 1L || length(frecuencia) != 1L ||
      length(distintos) != 1L ||
      !isTRUE(is.finite(n_validos) && is.finite(frecuencia) &&
                is.finite(distintos)) ||
      n_validos < .MIN_VALIDOS_CASI_CONSTANTE || distintos < 2 ||
      frecuencia / n_validos < .UMBRAL_CASI_CONSTANTE) {
    return(NULL)
  }
  if (!is.atomic(x) || is.raw(x) || is.list(x) || .es_columna_compuesta(x)) {
    return(NULL)
  }
  validos <- which(!is.na(x))
  if (length(validos) < .MIN_VALIDOS_CASI_CONSTANTE) return(NULL)
  claves <- .valores_relacion(x[validos])
  unicos <- unique(claves)
  if (length(unicos) < 2L) return(NULL)
  codigo <- match(claves, unicos)
  cuentas <- tabulate(codigo, nbins = length(unicos))
  moda <- which.max(cuentas)
  n <- length(claves)
  if (cuentas[[moda]] / n < .UMBRAL_CASI_CONSTANTE) return(NULL)
  fuera <- validos[codigo != moda]
  valor <- x[validos[match(moda, codigo)]]
  segunda <- max(cuentas[-moda])
  list(
    valor = valor, frecuencia_moda = as.numeric(cuentas[[moda]]),
    n_validos = as.numeric(n), n_distintos = as.numeric(length(unicos)),
    razon_frecuencias = as.numeric(cuentas[[moda]]) / as.numeric(segunda),
    porcentaje_unicos = 100 * length(unicos) / n,
    indices = fuera
  )
}
