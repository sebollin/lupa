test_that("se encuentran claves simples y combinadas con corte temprano", {
  simple <- data.frame(id = 1:4, grupo = c("a", "a", "b", "b"), valor = 1:4)
  claves_simples <- detectar_claves(simple)
  expect_true("id" %in% claves_simples$columnas)
  expect_false(any(grepl("id \\+", claves_simples$columnas)))

  compuesta <- data.frame(
    grupo = c("a", "a", "b", "b"),
    periodo = c(1, 2, 1, 2),
    valor = c("x", "x", "y", "y")
  )
  claves_compuestas <- detectar_claves(compuesta, max_combinacion = 2)
  expect_true("grupo + periodo" %in% claves_compuestas$columnas)
  expect_equal(
    claves_compuestas$n_columnas[claves_compuestas$columnas == "grupo + periodo"],
    2L
  )

  triple <- data.frame(
    a = c(1, 1, 1, 1, 2, 2, 2, 2),
    b = c(1, 1, 2, 2, 1, 1, 2, 2),
    c = c(1, 2, 1, 2, 1, 2, 1, 2)
  )
  claves_triples <- detectar_claves(triple, max_combinacion = 3)
  expect_true("a + b + c" %in% claves_triples$columnas)
})

test_that("se reportan claves redundantes de contenido idéntico", {
  datos <- data.frame(id = 1:4, copia = 1:4, categoria = letters[1:4])
  claves <- detectar_claves(datos)
  redundantes <- attr(claves, "claves_redundantes")

  expect_true(any(claves$columnas == "id" & claves$redundante))
  expect_true(any(
    redundantes$columna_1 == "id" & redundantes$columna_2 == "copia"
  ))
})

test_that("los ausentes impiden que una combinación sea clave", {
  datos <- data.frame(a = c(1, 2, NA), b = c("x", "y", "z"))
  claves <- detectar_claves(datos, max_combinacion = 1)
  expect_false("a" %in% claves$columnas)
  expect_true("b" %in% claves$columnas)

  expect_error(detectar_claves(1:3), "data.frame")
  expect_error(detectar_claves(datos, max_combinacion = 4), "entre 1 y 3")
})

test_that("se calculan cardinalidad y cobertura en ambas direcciones", {
  personas <- data.frame(id = 1:3)
  tramites <- data.frame(persona_id = c(1, 1, 3, 4))
  relacion <- detectar_relaciones(personas, tramites)

  expect_equal(relacion$cardinalidad, "1:m")
  expect_equal(relacion$n_valores_comunes, 2L)
  expect_equal(relacion$cobertura_tabla1_en_tabla2, 2 / 3)
  expect_equal(relacion$cobertura_tabla2_en_tabla1, 3 / 4)
})

test_that("se distinguen las cuatro cardinalidades y la falta de coincidencias", {
  uno_uno <- detectar_relaciones(data.frame(x = 1:2), data.frame(y = 1:2))
  muchos_uno <- detectar_relaciones(data.frame(x = c(1, 1)), data.frame(y = 1:2))
  muchos_muchos <- detectar_relaciones(
    data.frame(x = c(1, 1)), data.frame(y = c(1, 1))
  )
  ninguna <- detectar_relaciones(data.frame(x = 1:2), data.frame(y = 3:4))

  expect_equal(uno_uno$cardinalidad, "1:1")
  expect_equal(muchos_uno$cardinalidad, "m:1")
  expect_equal(muchos_muchos$cardinalidad, "m:m")
  expect_equal(ninguna$cardinalidad, "sin_coincidencias")
  expect_error(detectar_relaciones(1:3, data.frame(x = 1:3)), "data.frame")
})

test_that("las relaciones documentan y respetan el muestreo", {
  tabla1 <- data.frame(id = seq_len(1000))
  tabla2 <- data.frame(id = seq_len(1000))
  resultado <- detectar_relaciones(tabla1, tabla2, muestra = 100)

  expect_equal(attr(resultado, "filas_totales"), c(tabla1 = 1000, tabla2 = 1000))
  expect_equal(attr(resultado, "filas_analizadas"), c(tabla1 = 100, tabla2 = 100))
  expect_true(all(attr(resultado, "muestreado")))
  expect_error(detectar_relaciones(tabla1, tabla2, muestra = 0), "positivo")
})
