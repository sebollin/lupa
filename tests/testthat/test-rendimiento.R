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

test_that("texto libre de cardinalidad alta no degrada el perfil", {
  skip_on_cran()
  set.seed(3)
  n <- 1e4
  libre <- vapply(seq_len(n), function(i) {
    paste(sample(
      c(letters, LETTERS, 0:9, " ", ".", ",", "-"),
      sample(10:40, 1), TRUE
    ), collapse = "")
  }, character(1L))
  datos <- data.frame(obs1 = libre, obs2 = rev(libre), obs3 = libre)

  tiempo <- system.time(resultado <- perfilar(datos))[["elapsed"]]

  expect_lt(unname(tiempo), 5)
  expect_lt(as.numeric(object.size(resultado)), 5 * 1024^2)
  expect_true(all(
    attr(resultado$dependencias, "columnas_descartadas")$motivo == "casi_clave"
  ))
  expect_true(all(vapply(
    resultado$patrones,
    function(x) nrow(attr(x, "resumen_patrones")) <= 7L,
    logical(1L)
  )))
})
