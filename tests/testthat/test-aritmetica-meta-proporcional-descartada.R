# Una proporcionalidad descartada por la resolucion de sus columnas deja su
# conteo en la meta aunque sea la unica candidata de la tabla.

test_that("la meta se publica con una proporcionalidad descartada por la cota", {
  set.seed(11)
  n <- 300L
  neto <- as.numeric(sprintf("%.2f", stats::runif(n, 1e9, 1e10)))
  iva <- as.numeric(sprintf("%.2f", neto * 0.22))
  j <- sample(n, 120L)
  iva[j] <- as.numeric(sprintf("%.2f", iva[j] + 0.01))
  perfil <- perfilar(data.frame(neto = neto, iva = iva), analizar_dependencias = FALSE,
                     fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  h <- perfil$hallazgos
  expect_equal(sum(h$tipo_hallazgo == "relacion_aritmetica_columnas"), 0L)
  alcance <- perfil$meta$aritmetica_columnas
  expect_false(is.null(alcance))
  expect_equal(alcance$proporcionalidades_descartadas_cuanto, 1)
  expect_equal(alcance$identidades_descartadas_cuanto, 0)
})
