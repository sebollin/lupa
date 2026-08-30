test_that("la muestra queda acotada por celdas y lo declara", {
  datos <- data.frame(
    id = seq_len(20L),
    texto = paste0("fila-", seq_len(20L)),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, muestra = 20L, max_celdas_muestra = 10L,
    max_bytes_muestra = Inf, analizar_dependencias = FALSE,
    ausencia_estructural = FALSE, casi_duplicados_vocabulario = FALSE
  )

  expect_equal(perfil$meta$muestra, 20)
  expect_equal(perfil$meta$muestra_efectiva, 5)
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "muestra_perfilado", ,
    drop = FALSE
  ]
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$motivo, "celdas observadas: 40")
  expect_match(cobertura$motivo, "umbral = 10")
})

test_that("la muestra queda acotada por bytes observados", {
  datos <- data.frame(
    id = seq_len(40L),
    texto = paste0("valor-", formatC(seq_len(40L), width = 3L, flag = "0"),
                   paste(rep("x", 80L), collapse = "")),
    stringsAsFactors = FALSE
  )
  limite <- as.numeric(utils::object.size(datos[seq_len(5L), , drop = FALSE]))
  perfil <- perfilar(
    datos, muestra = 40L, max_celdas_muestra = Inf,
    max_bytes_muestra = limite, analizar_dependencias = FALSE,
    ausencia_estructural = FALSE, casi_duplicados_vocabulario = FALSE
  )

  expect_lte(perfil$meta$bytes_muestra, limite)
  expect_lte(perfil$meta$muestra_efectiva, 5)
  expect_lt(perfil$meta$muestra_efectiva, 40)
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "muestra_perfilado", ,
    drop = FALSE
  ]
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$motivo, "bytes observados")
  expect_match(cobertura$motivo, paste0("umbral ", limite), fixed = TRUE)
})

test_that("Inf desactiva ambos topes de la muestra", {
  datos <- data.frame(id = seq_len(4L), texto = letters[1:4])
  perfil <- perfilar(
    datos, muestra = Inf, max_celdas_muestra = Inf,
    max_bytes_muestra = Inf, analizar_dependencias = FALSE,
    ausencia_estructural = FALSE, casi_duplicados_vocabulario = FALSE
  )

  expect_equal(perfil$meta$muestra_efectiva, 4)
  expect_equal(nrow(perfil$cobertura_diagnosticos), 0L)
})

test_that("un tope de celdas menor que una fila se rechaza explicitamente", {
  datos <- data.frame(id = 1:2, texto = c("a", "b"))
  expect_error(
    perfilar(
      datos, max_celdas_muestra = 1L, max_bytes_muestra = Inf,
      analizar_dependencias = FALSE, ausencia_estructural = FALSE
    ),
    "al menos una fila"
  )
})
