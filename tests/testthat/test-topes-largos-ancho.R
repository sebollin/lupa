test_that("el tope de largo declara una columna completa fuera de alcance", {
  skip_if_not_installed("stringdist")
  largo <- paste(rep("a", 10001L), collapse = "")
  alcance <- .grupos_casi_duplicados_vocabulario(
    c(largo, "corto"), NULL, "texto", max_valores = 100L,
    max_pares = Inf, max_trabajo = Inf
  )$alcance
  expect_true(alcance$largo_excedido)
  expect_equal(alcance$max_largo_valor, 10000L)
  expect_equal(alcance$n_valores_largos, 1L)
  expect_equal(alcance$largo_maximo, 10001L)
  expect_equal(alcance$motivo_presupuesto, "max_largo_valor")
})

test_that("la cobertura explica el largo y el umbral elegido", {
  datos <- data.frame(
    texto = c(paste(rep("a", 10001L), collapse = ""), "corto", "otro"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, avisar_costo_tabla_ancha = FALSE
  )
  fila <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "proximidad_vocabulario" &
      perfil$cobertura_diagnosticos$columna == "texto", , drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$motivo[[1L]], "max_largo_valor_vocabulario = 10000")
  expect_match(fila$motivo[[1L]], "10001")
  expect_match(fila$como_resolverlo[[1L]], "Inf")
  expect_equal(perfil$meta$max_largo_valor_vocabulario, 10000L)
})

test_that("Inf recupera explicitamente la comparacion de valores largos", {
  base <- paste0("a", paste(rep("x", 10000L), collapse = ""))
  vecino <- paste0("a", paste(rep("x", 9999L), collapse = ""), "y")
  datos <- data.frame(texto = c(base, vecino), stringsAsFactors = FALSE)
  limitado <- detectar_duplicados_aproximados(
    datos, columnas = "texto", proteger_datos_personales = FALSE,
    max_largo_valor = 10000L
  )
  anterior <- detectar_duplicados_aproximados(
    datos, columnas = "texto", proteger_datos_personales = FALSE,
    max_largo_valor = Inf
  )
  expect_equal(limitado$alcance$n_columnas_excluidas_largo, 1L)
  expect_equal(anterior$alcance$n_columnas_excluidas_largo, 0L)
  expect_equal(nrow(limitado$pares), 0L)
  expect_equal(nrow(anterior$pares), 1L)
})

test_that("el tope no cambia resultados para valores por debajo", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = c("Ana Perez", "Ana Peres", "Luis Diaz", "Luis Dias"),
    stringsAsFactors = FALSE
  )
  predeterminado <- detectar_duplicados_aproximados(
    datos, columnas = "nombre", proteger_datos_personales = FALSE
  )
  explicito <- detectar_duplicados_aproximados(
    datos, columnas = "nombre", proteger_datos_personales = FALSE,
    max_largo_valor = Inf
  )
  expect_identical(predeterminado$pares, explicito$pares)
  expect_identical(predeterminado$hallazgos, explicito$hallazgos)
  for (campo in c(
    "n_pares_comparados", "n_pares_hallados", "n_pares_exactos",
    "n_pares_aproximados"
  )) {
    expect_identical(predeterminado$alcance[[campo]], explicito$alcance[[campo]])
  }
})

test_that("el aviso de tabla ancha respeta umbral e interactividad", {
  pequeno <- .proyectar_costo_tabla_ancha(500L, 50L)
  grande <- .proyectar_costo_tabla_ancha(500L, 300L)
  expect_silent(.avisar_costo_tabla_ancha(pequeno, interactiva = TRUE))
  expect_silent(.avisar_costo_tabla_ancha(grande, interactiva = FALSE))
  expect_message(
    .avisar_costo_tabla_ancha(grande, interactiva = TRUE),
    "Costo estimado.*150.000 celdas.*Fuente:.*Es una estimacion"
  )
})
