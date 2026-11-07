# Un centinela declarado sale del resumen, y tiene que salir tambien de la
# concentracion: la misma celda no puede ser ausencia en uno y valor en otra.

test_that("la moda declarada como ausencia no es una concentracion", {
  set.seed(8)
  w <- round(stats::runif(1000, 1000, 99999), 2)
  w[1:180] <- -99
  perfil <- perfilar(data.frame(w = w), sentinelas_numericos = -99)
  h <- perfil$hallazgos
  expect_false(any(h$tipo_hallazgo == "valor_concentrado"))
  # El resumen ya los dejaba afuera.
  expect_equal(perfil$columnas$n_valores_excluidos_resumen, 180L)
})

test_that("una concentracion real se mide sobre los valores validos", {
  set.seed(81)
  w <- round(stats::runif(1000, 1000, 99999), 2)
  w[1:180] <- -99
  w[181:380] <- 5000
  h <- perfilar(data.frame(w = w), sentinelas_numericos = -99)$hallazgos
  concentrado <- h[h$tipo_hallazgo == "valor_concentrado", ]
  expect_equal(nrow(concentrado), 1L)
  expect_match(concentrado$evidencia, "Valor modal: 5000", fixed = TRUE)
  expect_match(concentrado$evidencia, "frecuencia de la moda: 200",
               fixed = TRUE)
  # 200 sobre los 820 que no son el centinela declarado.
  expect_match(concentrado$evidencia,
               paste0("sobre v\u00e1lidos: ", formatC(200 / 820, format = "f",
                                                     digits = 3)),
               fixed = TRUE)
  expect_equal(concentrado$trazabilidad[[1L]]$indices_fila, 181:380)
})

test_that("la lista por omision no es una declaracion y no saca nada", {
  set.seed(8)
  w <- round(stats::runif(1000, 1000, 99999), 2)
  w[1:180] <- -99
  h <- perfilar(data.frame(w = w))$hallazgos
  concentrado <- h[h$tipo_hallazgo == "valor_concentrado", ]
  expect_equal(nrow(concentrado), 1L)
  expect_match(concentrado$evidencia, "Valor modal: -99", fixed = TRUE)
})
