test_that("los topes grandes conservan el valor y llegan al resolver", {
  testthat::skip_on_cran()
  tope <- 2^31 + 1
  expect_silent(valor <- lupa:::.validar_limite_duplicados(tope, "tope"))
  expect_type(valor, "double")
  expect_identical(valor, tope)

  datos <- data.frame(x = seq_len(3L), y = letters[seq_len(3L)])
  for (argumento in c("max_bytes_muestra", "max_celdas_muestra")) {
    alcance <- expect_silent(lupa:::.resolver_muestra_perfilado(
      datos, muestra = 3L,
      max_celdas_muestra = if (argumento == "max_celdas_muestra") tope else Inf,
      max_bytes_muestra = if (argumento == "max_bytes_muestra") tope else Inf
    ))
    expect_identical(alcance[[argumento]], tope)
    if (argumento == "max_bytes_muestra") {
      expect_lte(alcance$bytes_muestra, tope)
    } else {
      expect_lte(alcance$celdas_efectivas, tope)
    }
  }
})

test_that("un tope de filas huerfanas mayor que el entero llega a la traza", {
  testthat::skip_on_cran()
  tope <- 2^31 + 1
  t1 <- data.frame(id = c(1:6, 7:10), x = c(rep("a", 6), rep("b", 4)),
                   stringsAsFactors = FALSE)
  t2 <- data.frame(id = c(1:4, 20:21), x = rep("a", 6),
                   stringsAsFactors = FALSE)
  expect_silent(relaciones <- detectar_relaciones(
    t1, t2, max_filas_huerfanas = tope, umbral_cobertura = 0
  ))
  limites <- vapply(
    c(relaciones$huerfanas_tabla1, relaciones$huerfanas_tabla2),
    function(traza) as.numeric(traza$limite), numeric(1L)
  )
  expect_true(length(limites) > 0L)
  expect_true(all(limites == tope))
})
