test_that("validar_url distingue esquema, IDN, puertos y entradas peligrosas", {
  valores <- c(
    "https://ejemplo.uy",
    "http://ejemplo.uy:8080/ruta?x=1#fragmento",
    "https://münich.example/á",
    "https://xn--mnich-kva.example",
    "https://ejemplo.uy:65535",
    "https://ejemplo.uy:0",
    "https://ejemplo.uy:65536",
    "https://ejemplo.uy:abc",
    "https://ejemplo.uy:",
    "ejemplo.uy",
    "javascript:alert(1)",
    "data:text/plain,ok",
    "https://ejemplo.uy/a b",
    paste0("https://ejemplo.uy/a", intToUtf8(1L), "b"),
    "https://ejemplo.uy/%20",
    "https://",
    "ftp://ejemplo.uy",
    NA_character_
  )
  expect_equal(
    validar_url(valores),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, NA)
  )
  expect_equal(
    validar_url(valores, esquema_obligatorio = FALSE),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE,
      FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, NA)
  )
  expect_error(
    validar_url("https://ejemplo.uy", esquema_obligatorio = NA),
    "TRUE o FALSE"
  )
  expect_true(all(c("validar_url") %in% getNamespaceExports("lupa")))
})

test_that("las unidades mixtas se informan sin convertir y no confunden codigos", {
  mixtas <- perfilar(
    data.frame(peso = c("12 kg", "13500 g", "9 kg", "800 g", "11 kg")),
    analizar_dependencias = FALSE
  )
  hallazgo <- mixtas$hallazgos[
    mixtas$hallazgos$tipo_hallazgo == "unidades_mixtas", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(hallazgo$evidencia, "kg (3)", fixed = TRUE)
  expect_match(hallazgo$evidencia, "g (2)", fixed = TRUE)
  expect_equal(hallazgo$n_evaluados, 5)
  expect_equal(hallazgo$n_afectados, 5)
  expect_equal(hallazgo$unidad_conteo, "fila")
  expect_equal(mixtas$columnas$numero_texto_unidad, "")

  una <- perfilar(
    data.frame(peso = c("12 kg", "9 kg", "11 kg")),
    analizar_dependencias = FALSE
  )
  expect_false("unidades_mixtas" %in% una$hallazgos$tipo_hallazgo)

  codigos <- perfilar(
    data.frame(codigo = c("12A", "13B", "14A", "15B")),
    analizar_dependencias = FALSE
  )
  expect_false("unidades_mixtas" %in% codigos$hallazgos$tipo_hallazgo)
  expect_equal(codigos$columnas$n_numeros_texto, 0L)
})

test_that("las celdas multivaluadas exigen homogeneidad y delimitador", {
  telefonos <- perfilar(
    data.frame(tel = c(
      "099111222", "099111222; 24001234", "24005678",
      "099333444, 24009999"
    )),
    analizar_dependencias = FALSE
  )
  hallazgo <- telefonos$hallazgos[
    telefonos$hallazgos$tipo_hallazgo == "celdas_multivaluadas", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(hallazgo$evidencia, "Delimitador: , / ;", fixed = TRUE)
  expect_match(hallazgo$evidencia, "celdas: 2", fixed = TRUE)
  expect_match(hallazgo$evidencia, "2 valores: 2 celdas", fixed = TRUE)
  expect_equal(hallazgo$n_evaluados, 4)
  expect_equal(hallazgo$n_afectados, 2)
  expect_equal(hallazgo$unidad_conteo, "fila")
  expect_equal(hallazgo$trazabilidad[[1L]]$indices_fila, c(2L, 4L))

  direcciones <- perfilar(
    data.frame(direccion = c(
      "18 de Julio 1234, Piso 3", "Rivera 2020, Apto 4", "Colonia 100"
    )),
    analizar_dependencias = FALSE
  )
  expect_false("celdas_multivaluadas" %in% direcciones$hallazgos$tipo_hallazgo)

  nombres <- perfilar(
    data.frame(nombre = c("Pérez, Juan", "Gómez, Ana", "López")),
    analizar_dependencias = FALSE
  )
  expect_false("celdas_multivaluadas" %in% nombres$hallazgos$tipo_hallazgo)
})
