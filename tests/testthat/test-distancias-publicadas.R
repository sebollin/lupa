test_that("Jaro-Winkler coincide con los ejemplos publicados", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = c("MARTHA", "MARHTA", "DWAYNE", "DUANE", "DIXON", "DICKSONX"),
    stringsAsFactors = FALSE
  )
  referencias <- c(0.0388888888888889, 0.16, 0.186666666666667)
  obtenidas <- stringdist::stringdist(
    c("MARTHA", "DWAYNE", "DIXON"),
    c("MARHTA", "DUANE", "DICKSONX"), method = "jw", p = 0.1
  )
  expect_true(all(abs(obtenidas - referencias) <= 1e-12))
  resultado <- detectar_duplicados_aproximados(
    datos, columnas = "nombre", p = 0.1, umbral = 1,
    estrategia = "teselas", max_resultados = Inf
  )
  expect_equal(resultado$p, 0.1)
  expect_equal(resultado$alcance$p, 0.1)
  expect_equal(unique(resultado$pares$umbral), 1)

  predeterminado <- detectar_duplicados_aproximados(
    data.frame(nombre = c("MARTHA", "MARHTA"), stringsAsFactors = FALSE),
    columnas = "nombre", estrategia = "teselas", max_resultados = Inf
  )
  expect_equal(unique(predeterminado$pares$umbral), 0.1)
  expect_equal(predeterminado$alcance$p, 0.1)
})

test_that("p solo modifica Jaro-Winkler y se valida", {
  skip_if_not_installed("stringdist")
  expect_error(
    detectar_duplicados_aproximados(data.frame(x = c("a", "b")), p = 0.3),
    "entre 0 y 0.25"
  )
  jw_0 <- stringdist::stringdist("GONZALEZ", "GONZALEX", method = "jw", p = 0)
  jw_1 <- stringdist::stringdist("GONZALEZ", "GONZALEX", method = "jw", p = 0.1)
  expect_lt(jw_1, jw_0)
  lv_0 <- stringdist::stringdist("GONZALEZ", "GONZALEX", method = "lv", p = 0)
  lv_1 <- stringdist::stringdist("GONZALEZ", "GONZALEX", method = "lv", p = 0.1)
  expect_equal(lv_0, lv_1)
})

test_that("las medidas sin valor canónico quedan cubiertas por la integración", {
  skip_if_not_installed("stringdist")
  metodos <- c("osa", "lv", "dl", "hamming", "lcs", "qgram",
               "cosine", "jaccard", "soundex")
  datos <- data.frame(x = c("MARTHA", "MARHTA"), stringsAsFactors = FALSE)
  resultados <- lapply(metodos, function(metodo) {
    detectar_duplicados_aproximados(
      datos, columnas = "x", metodo = metodo, p = 0.1,
      umbral = 1, estrategia = "teselas", max_resultados = Inf
    )
  })
  expect_length(resultados, length(metodos))
  expect_true(all(vapply(resultados, inherits, logical(1L),
                         what = "duplicados_aproximados")))
})
