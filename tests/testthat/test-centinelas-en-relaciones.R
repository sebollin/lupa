# Un centinela declarado en `sentinelas_numericos` se interpreta como ausencia,
# y el perfil ya lo sacaba del resumen, de Benford y de las relaciones
# aritmeticas. Las dependencias funcionales y las asociaciones lo contaban como
# un valor mas: con un `-999` declarado en una columna de palabras, el perfil
# perdia `x -> y` (el centinela era un tercer valor de `y`), y con un `-999` en
# una columna numerica, la media salia sin el pero `analizar()` no publicaba la
# asociacion de 0,92 que tenian las otras 99 filas. Una fila con el valor
# declarado no se evalua en el par, igual que una con `NA`.

testthat::skip_on_cran()

testthat::test_that("un centinela declarado queda fuera de ambos sentidos", {
  x <- rep(c("a", "b"), each = 50L)
  y <- rep(c("diez", "veinte"), each = 50L)
  y[[1L]] <- "-999"
  datos <- data.frame(x = x, y = y, stringsAsFactors = FALSE)

  resultado <- detectar_dependencias(
    datos, sentinelas_numericos = -999
  )

  actual <- data.frame(
    determinante = resultado$determinante,
    dependiente = resultado$dependiente,
    n_evaluados = resultado$n_evaluados,
    stringsAsFactors = FALSE
  )
  testthat::expect_equal(
    actual,
    data.frame(
      determinante = c("x", "y"), dependiente = c("y", "x"),
      n_evaluados = c(99L, 99L), stringsAsFactors = FALSE
    )
  )
})

testthat::test_that("perfilar y analizar transmiten la declaracion", {
  x <- rep(c("a", "b"), each = 50L)
  y <- rep(c("diez", "veinte"), each = 50L)
  y[[1L]] <- "-999"
  datos <- data.frame(x = x, y = y, stringsAsFactors = FALSE)

  perfil <- suppressWarnings(suppressMessages(perfilar(
    datos, sentinelas_numericos = -999
  )))
  testthat::expect_equal(perfil$dependencias$n_evaluados, c(99L, 99L))

  x <- c(-999, seq_len(99L))
  datos <- data.frame(x = x, y = 2 * x, stringsAsFactors = FALSE)
  resultado <- suppressWarnings(suppressMessages(analizar(
    datos, sentinelas_numericos = -999, analizar_dependencias = FALSE,
    medir_propuesta = FALSE
  )))
  testthat::expect_equal(resultado$asociaciones$n_pares, 99L)
})

testthat::test_that("detectar_asociaciones acepta la declaracion", {
  x <- c(-999, seq_len(99L))
  datos <- data.frame(x = x, y = 2 * x, stringsAsFactors = FALSE)

  resultado <- detectar_asociaciones(
    datos, sentinelas_numericos = -999, umbral = 0.9
  )

  testthat::expect_equal(resultado$n_pares, 99L)
  testthat::expect_equal(resultado$asociacion, 1)
})
