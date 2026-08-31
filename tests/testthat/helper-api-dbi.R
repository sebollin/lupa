# Traduccion explicita de los cinco nombres historicos a las dimensiones de la
# API nueva. Mantener este mapa junto a la suite hace visible por que cada caso
# sigue existiendo y evita volver a pasar un selector compuesto a la funcion.
.argumentos_caso_dbi <- function(caso, muestra = Inf) {
  switch(
    caso,
    exacto = list(
      universo = "tabla_completa", estrategia_mediana = "exacta"
    ),
    seguro = list(
      universo = "tabla_completa",
      metricas = c("validos", "basicos", "desvio"),
      estrategia_mediana = "exacta"
    ),
    conteos = list(
      universo = "tabla_completa", metricas = "validos",
      estrategia_mediana = "exacta"
    ),
    muestreado = list(
      universo = "muestra_motor", muestra_motor = muestra, muestra = muestra,
      estrategia_mediana = "exacta"
    ),
    aproximado = list(
      universo = "tabla_completa", estrategia_mediana = "aproximada_motor"
    ),
    stop("Caso de API desconocido: ", caso, call. = FALSE)
  )
}
