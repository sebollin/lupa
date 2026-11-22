# Con dos columnas del mismo nombre, una entrada por columna que nombra el
# repetido no dice a cual se refiere: el paquete pide renombrar en vez de elegir
# una en silencio. Una lista que no nombra el repetido sigue andando.

testthat::skip_on_cran()

test_that("una lista que nombra una columna repetida se rechaza y otra sigue", {
  d <- data.frame(x = c(99, 1, 2), x = c(1, 99, 99), y = c(99, 99, 3),
                  check.names = FALSE)
  expect_error(perfilar(d, sentinelas_numericos = list(x = 99)), "Renombrar")
  expect_error(perfilar(d, cadenas_ausencia = list(x = "NC")), "Renombrar")
  expect_error(detectar_dependencias(d, sentinelas_numericos = list(x = 99)), "Renombrar")
  p <- perfilar(d, sentinelas_numericos = list(y = 99))
  expect_identical(p$columnas$n_faltantes_disfrazados, c(0L, 0L, 2L))
  # El vector, que vale para todas, no es ambiguo.
  expect_s3_class(perfilar(d, sentinelas_numericos = 99), "perfil")
})
