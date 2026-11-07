# La cobertura de una relacion se estimaba con una muestra sistematica del lado
# verificado, y la muestra podia caer justo sobre los valores que la otra tabla
# tiene: la fila publicaba cobertura 1 al lado de `1:1` y de un
# `n_valores_comunes` que obligaban a otra cifra.

test_that("la cobertura no depende de donde caiga la muestra", {
  tabla1 <- data.frame(id = seq_len(1000L))
  # Los 100 valores que una muestra sistematica de 100 filas tomaria.
  tomados <- unique(as.integer(round(seq.int(1, 1000, length.out = 100))))
  tabla2 <- data.frame(id = tomados)
  r <- detectar_relaciones(tabla1, tabla2, muestra = 100)
  expect_equal(r$cardinalidad, "1:1")
  expect_equal(r$n_valores_comunes, 100L)
  expect_equal(r$cobertura_tabla1_en_tabla2, mean(tabla1$id %in% tabla2$id))
  expect_equal(r$cobertura_tabla1_en_tabla2, 0.1)
  expect_equal(r$cobertura_tabla2_en_tabla1, 1)
})

test_that("la cobertura por filas cuenta las repeticiones de cada valor", {
  set.seed(2582)
  fk <- sample(c(1:50, 900:949), 3000L, TRUE, prob = rep(c(3, 1), each = 50))
  fk[sample(3000L, 40L)] <- NA
  personas <- data.frame(id = 1:500)
  tramites <- data.frame(fk = fk)
  r <- detectar_relaciones(tramites, personas, muestra = 200)
  # A mano, sobre las filas no ausentes.
  presentes <- fk[!is.na(fk)]
  expect_equal(r$cobertura_tabla1_en_tabla2, mean(presentes %in% personas$id))
  expect_equal(r$cobertura_tabla2_en_tabla1, mean(personas$id %in% presentes))
  expect_equal(r$cardinalidad, "m:1")
})

test_that("la poda por filas usa las mismas frecuencias que la cobertura", {
  # Un valor comun que cubre 91 de 100 filas: la cota por filas no lo poda.
  a <- data.frame(x = 1)
  b <- data.frame(y = c(rep(1, 91), 2:10))
  r <- detectar_relaciones(a, b, podar = TRUE, umbral_cobertura = 0.9,
                           muestra = 10)
  expect_equal(r$cardinalidad, "1:m")
  expect_equal(r$cobertura_tabla2_en_tabla1, 0.91)
})
