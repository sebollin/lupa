test_that("un millón de valores se perfila en pocos segundos", {
  skip_on_cran()
  invisible(perfilar(data.frame(codigo = "AB1234")))
  datos <- data.frame(
    codigo = rep(c("AB1234", "CD5678", "EF9012", "GH3456"), length.out = 1e6)
  )

  tiempo <- system.time(resultado <- perfilar(datos))[["elapsed"]]

  expect_lt(unname(tiempo), 8)
  expect_true(resultado$meta$muestreo)
  expect_equal(resultado$meta$filas_analizadas, 1e5)
})
