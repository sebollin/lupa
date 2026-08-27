# Tercer intento sobre el mismo borde, y la razon por la que hubo tres: las dos
# primeras veces el barrido de comprobacion uso umbrales "representativos" y el
# defecto no vive ahi.
#
# 1. `d > n * (1 - umbral)`  ->  `1 - 0.8` es 0,19999999999999996.
# 2. `n - d < n * umbral`    ->  `25 * 0.56` es 14.000000000000002.
#
# Las dos podan un par cuyo cumplimiento maximo IGUALA el umbral, y el filtro de
# informe descarta solo lo que esta por debajo. La forma que resiste divide en
# vez de multiplicar: `(n - d) / n < umbral`.
#
# La verdad de referencia de esta prueba no es otra formula en punto flotante:
# es aritmetica entera. Para un umbral escrito con hasta seis decimales,
# `u = k / 1e6`, y podar equivale a `(n - d) * 1e6 < n * k`.

.ronda146_debe_podar <- function(n, d, k, escala = 1000000) {
  (as.numeric(n - d) * escala) < (as.numeric(n) * k)
}

test_that("ninguna poda descarta un par que iguala el umbral", {
  escala <- 1000000
  # Los doce umbrales de dos decimales que disparaban la forma anterior, mas un
  # recorrido de los cien.
  #
  # Los umbrales se construyen COMO `k / escala` y no con `seq(0.01, 1, 0.01)`.
  # No es lo mismo: `seq()` devuelve 0.96000000000000008 donde el literal 0.96
  # es 0.95999999999999996, dos dobles distintos. Comparar la funcion contra una
  # referencia entera que supone el segundo mientras se le pasa el primero
  # inventa fallos que no existen -paso mientras se escribia esta prueba-.
  disparadores <- c(7L, 14L, 17L, 27L, 28L, 34L, 54L, 55L, 56L, 67L, 68L, 81L)
  enteros <- sort(unique(c(disparadores * (escala %/% 100L),
                           seq_len(100L) * (escala %/% 100L))))
  umbrales <- enteros / escala
  podas_indebidas <- 0L
  conservas_indebidas <- 0L
  for (n in c(3L, 7L, 25L, 50L, 100L, 300L, 1000L)) {
    for (d in unique(round(seq(0L, n, length.out = min(n + 1L, 25L))))) {
      for (indice in seq_along(umbrales)) {
        umbral <- umbrales[[indice]]
        k <- enteros[[indice]]
        verdad <- .ronda146_debe_podar(n, d, k, escala)
        poda <- lupa:::.poda_dependencia_cardinalidad(
          list(n = n, n_distintos = 1L), list(n = n, n_distintos = 1L + d),
          n, umbral
        )
        if (isTRUE(poda) && !verdad) podas_indebidas <- podas_indebidas + 1L
        if (!isTRUE(poda) && verdad) conservas_indebidas <- conservas_indebidas + 1L
      }
    }
  }
  # Podar de mas calla un hallazgo; conservar de mas solo gasta un calculo. Se
  # exigen cero de las dos, porque con la division no hace falta transigir.
  expect_identical(podas_indebidas, 0L)
  expect_identical(conservas_indebidas, 0L)
})

test_that("el caso exacto que delato la segunda forma sigue informandose", {
  # n = 25, d = 11, umbral = 0,56: el cumplimiento maximo es 14/25 = 0,56.
  expect_false(lupa:::.poda_dependencia_cardinalidad(
    list(n = 25L, n_distintos = 7L), list(n = 25L, n_distintos = 18L),
    25L, 0.56
  ))
  # Y la comprobacion de que el borde es real y no una casualidad del fixture.
  expect_identical((25 - 11) / 25, 0.56)
  expect_true(25 * 0.56 > 14)
})

test_that("la poda de relaciones tampoco descarta la cobertura que iguala", {
  # 7 distintos contra 25: la cobertura maxima es 7/25 = 0,28, o sea el umbral.
  tabla1 <- data.frame(a = sprintf("a%02d", seq_len(7L)), stringsAsFactors = FALSE)
  tabla2 <- data.frame(b = sprintf("b%02d", seq_len(25L)), stringsAsFactors = FALSE)
  relaciones <- detectar_relaciones(
    tabla1, tabla2, umbral_cobertura = 0.28, podar = TRUE
  )
  expect_false(any(
    as.character(relaciones$motivo_poda) %in% "cardinalidades_imposibles"
  ))
  # Con un umbral que la cobertura NO alcanza, la poda tiene que seguir podando.
  imposible <- detectar_relaciones(
    tabla1, tabla2, umbral_cobertura = 0.9, podar = TRUE
  )
  expect_true(any(
    as.character(imposible$motivo_poda) %in% "cardinalidades_imposibles"
  ))
})
