# Un `999` de la lista por omision no es una declaracion: se queda en el resumen
# y en la prueba de Benford. Pero si la desviacion la producen ellos, el
# hallazgo tiene que decirlo, como lo dice el paquete de cualquier centinela que
# sospecha y nadie declaro.

chi2_primer_digito <- function(v) {
  v <- v[is.finite(v) & v > 0]
  # Independiente de `.primer_digito_significativo()`: el primer caracter de la
  # notacion cientifica.
  digito <- as.integer(substr(formatC(v, format = "e", digits = 10), 1, 1))
  observados <- tabulate(digito, 9L)
  esperados <- length(v) * log10(1 + 1 / 1:9)
  sum((observados - esperados)^2 / esperados)
}

montos_con_999 <- function() {
  set.seed(7)
  m <- round(10^stats::runif(4000, 2, 7), 2)
  marcados <- sample(4000, 200)
  m[marcados] <- 999
  list(monto = m, marcados = marcados)
}

test_that("la desviacion que fabrican los 999 dice que sin ellos no hay", {
  caso <- montos_con_999()
  perfil <- perfilar(data.frame(monto = caso$monto))
  h <- perfil$hallazgos
  expect_true(any(h$tipo_hallazgo == "faltantes_disfrazados"))
  benford <- h[h$tipo_hallazgo == "desviacion_benford", ]
  expect_equal(nrow(benford), 1L)
  expect_match(benford$evidencia, "Sin los 200 valores", fixed = TRUE)
  expect_match(benford$evidencia, "sin ellos la columna no se desv",
               fixed = TRUE)
  expect_match(benford$descripcion, "la desviaci\u00f3n la producen ellos",
               fixed = TRUE)
  expect_match(benford$sugerencia,
               "Si 999 es un c\u00f3digo de ausencia, declararlo", fixed = TRUE)

  sin <- perfil$meta$benford$resultados$monto$sin_faltantes_disfrazados
  expect_equal(sin$n_excluidos, 200L)
  expect_equal(sin$valores_excluidos, 999)
  expect_equal(sin$n_positivos, 3800L)
  expect_equal(sin$estadistico, chi2_primer_digito(caso$monto[-caso$marcados]),
               tolerance = 1e-9)
  expect_false(sin$desviacion)
  # La prueba sobre los datos tal como vienen no cambia.
  r <- perfil$meta$benford$resultados$monto
  expect_equal(r$estadistico, chi2_primer_digito(caso$monto), tolerance = 1e-9)
  expect_true(r$desviacion)
})

test_that("declarado, el centinela sale de la prueba y no queda nada que decir", {
  caso <- montos_con_999()
  perfil <- perfilar(data.frame(monto = caso$monto), sentinelas_numericos = 999)
  expect_false(any(perfil$hallazgos$tipo_hallazgo == "desviacion_benford"))
  expect_null(perfil$meta$benford$resultados$monto$sin_faltantes_disfrazados)
})

test_that("si la desviacion no depende de ellos, se mantiene y lo dice", {
  set.seed(8)
  m <- c(rep(c(5000, 6000, 7000), 300), round(10^stats::runif(3000, 2, 6), 2))
  m[sample(length(m), 100)] <- 999
  benford <- perfilar(data.frame(monto = m))$hallazgos
  benford <- benford[benford$tipo_hallazgo == "desviacion_benford", ]
  expect_equal(nrow(benford), 1L)
  expect_match(benford$evidencia, "la desviaci\u00f3n se mantiene sin ellos",
               fixed = TRUE)
  expect_false(grepl("la producen ellos", benford$descripcion, fixed = TRUE))
})

test_that("sin faltantes disfrazados la evidencia no cambia", {
  set.seed(9)
  m <- c(rep(5000, 500), round(10^stats::runif(3000, 2, 6), 2))
  perfil <- perfilar(data.frame(monto = m))
  benford <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "desviacion_benford", ]
  expect_equal(nrow(benford), 1L)
  expect_false(grepl("faltantes disfrazados", benford$evidencia, fixed = TRUE))
  expect_null(perfil$meta$benford$resultados$monto$sin_faltantes_disfrazados)
})
