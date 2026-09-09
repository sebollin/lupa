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
  expect_error(detectar_relaciones(tabla1, tabla2, muestra = "100"), "positivo")

  completo <- detectar_relaciones(tabla1, tabla2, muestra = Inf)
  expect_false(any(attr(completo, "muestreado")))
  expect_equal(attr(completo, "filas_analizadas"), c(tabla1 = 1000, tabla2 = 1000))
})

test_that("el muestreo conserva cobertura y cardinalidad con tamaños distintos", {
  tabla1 <- data.frame(id = seq_len(2000L))
  tabla2 <- data.frame(fk = rep(seq_len(2000L), length.out = 3500L))
  relacion <- detectar_relaciones(tabla1, tabla2, muestra = 100)

  expect_equal(relacion$cobertura_tabla1_en_tabla2, 1)
  expect_equal(relacion$cobertura_tabla2_en_tabla1, 1)
  expect_equal(relacion$cardinalidad, "1:m")
  expect_equal(relacion$n_valores_comunes, 2000L)

  ids <- seq_len(2000L)
  ids[4L] <- 5L
  duplicado_no_muestreado <- detectar_relaciones(
    data.frame(id = ids), data.frame(fk = seq_len(2000L)), muestra = 100
  )
  expect_equal(duplicado_no_muestreado$cardinalidad, "m:1")
})

# El motivo de una poda dice lo que se midio, no lo que no se midio. Se llamaba
# `tipos_incompatibles`, y esa etiqueta afirma una incompatibilidad que el
# propio `.Rd` niega: "Familias distintas parece decisivo y no lo es: una
# columna de texto puede guardar '2020-01-05' y coincidir con una de fecha".
test_that("una poda por familias no afirma que los tipos sean incompatibles", {
  fechas <- data.frame(fecha = as.Date(c("2020-01-05", "2021-06-01")))
  textos <- data.frame(
    fecha = c("2020-01-05", "2022-01-01"), stringsAsFactors = FALSE
  )

  # El par SI empareja por el camino por omision: es el ejemplo del propio .Rd.
  sin_podar <- detectar_relaciones(fechas, textos)
  expect_equal(as.character(sin_podar$cardinalidad), "1:1")
  expect_equal(sin_podar$n_valores_comunes, 1L)

  podado <- detectar_relaciones(fechas, textos, podar = TRUE)
  expect_equal(as.character(podado$cardinalidad), "sin_comparar")
  expect_equal(podado$motivo_poda, "familias_distintas")
  podas <- attr(podado, "podas", exact = TRUE)
  expect_equal(podas$motivo, "familias_distintas")
  expect_match(podas$detalle, "familias fecha y texto")
})

# El control: la poda que SI es cierta -rangos numericos disjuntos, que no
# comparten ningun valor y eso se sabe sin comparar- conserva su nombre y su
# significado.
test_that("la poda por rangos disjuntos sigue diciendo lo que dice", {
  bajos <- data.frame(n = 1:5)
  altos <- data.frame(n = 100:105)
  resultado <- detectar_relaciones(bajos, altos)
  podas <- attr(resultado, "podas", exact = TRUE)
  expect_true("rangos_disjuntos" %in% podas$motivo)
  # Y esa poda no oculta la fila: sale con cobertura cero, no `sin_comparar`.
  expect_equal(as.character(resultado$cardinalidad), "sin_coincidencias")
})

test_that("el presupuesto agotado deja fila, no una tabla vacia", {
  # Una tabla de cero filas AFIRMA: se lee como "no hay relaciones entre estas
  # tablas", y lo que paso fue "no se comparo ninguna". Las podas por tipo ya
  # dejaban el par con `cardinalidad = "sin_comparar"`; la de presupuesto lo
  # guardaba solo en un atributo, asi que quien imprimia el resultado veia
  # encabezados y nada.
  set.seed(3)
  t1 <- data.frame(
    a = 1:300, b = sample(letters, 300, TRUE), c = sample(100, 300, TRUE)
  )
  t2 <- data.frame(
    a = 1:300, b = sample(letters, 300, TRUE), c = sample(100, 300, TRUE)
  )

  completo <- detectar_relaciones(t1, t2)
  agotado <- detectar_relaciones(t1, t2, tope_memoria_mb = 0)

  # Los mismos pares en las dos corridas: lo que cambia es que no se compararon.
  expect_equal(nrow(agotado), nrow(completo))
  expect_true(nrow(agotado) > 0L)
  expect_true(all(agotado$cardinalidad == "sin_comparar"))
  expect_true(all(agotado$motivo_poda == "presupuesto_memoria_agotado"))
  # Y no se inventa un resultado donde no se midio.
  expect_true(all(is.na(agotado$n_valores_comunes)))
  expect_true(all(is.na(agotado$cobertura_tabla1_en_tabla2)))

  # El control por el otro lado: con presupuesto, se compara de verdad.
  expect_false(any(completo$cardinalidad == "sin_comparar"))
  expect_true(all(is.na(completo$motivo_poda)))
})
