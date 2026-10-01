# Ronda 17-B: los diagnosticos numericos del perfil.

test_that("una numeracion por encima de 2^53 no se declara no densa", {
  skip_if_not_installed("bit64")
  grande <- bit64::as.integer64("9000000000000000000") + bit64::as.integer64(1:1000)
  p <- suppressWarnings(perfilar(data.frame(ids = grande)))
  # Antes: tres veredictos FALSE y ninguna fila en la cobertura.
  expect_true(is.na(p$columnas$secuencia_entera_densa))
  expect_true(is.na(p$columnas$moda_sobresale_secuencia_entera))
  cobertura <- p$cobertura_diagnosticos
  expect_true(any(cobertura$diagnostico == "secuencia_entera" &
                    grepl("2^53", cobertura$motivo, fixed = TRUE)))
  # Control: dentro de la precision se mide.
  chico <- suppressWarnings(perfilar(data.frame(ids = bit64::as.integer64(1:1000))))
  expect_true(isTRUE(chico$columnas$secuencia_entera_densa))
})

test_that("la descripcion de enteros fuera de precision dice como se guardan", {
  skip_if_not_installed("bit64")
  g <- bit64::as.integer64("9000000000000000000") + bit64::as.integer64(1:50)
  p <- suppressWarnings(perfilar(data.frame(
    i64 = g, txt = as.character(g), stringsAsFactors = FALSE
  )))
  h <- p$hallazgos[p$hallazgos$tipo_hallazgo == "integer64_fuera_precision_double", ]
  # Antes: "La columna integer64" tambien sobre la de texto.
  expect_match(h$descripcion[h$columna == "txt"], "Los enteros de la columna",
               fixed = TRUE)
  expect_match(h$descripcion[h$columna == "i64"], "La columna integer64",
               fixed = TRUE)
})

test_that("el motivo de Benford no publica una comparacion que se lee falsa", {
  set.seed(1)
  d <- data.frame(
    porcentaje = c(0.001, 0.999, stats::runif(298, 0.001, 0.999)),
    mixto = c(-stats::runif(30), stats::runif(270, 1, 1e6))
  )
  cobertura <- suppressWarnings(perfilar(d))$cobertura_diagnosticos
  motivos <- cobertura$motivo[cobertura$diagnostico == "ley_benford"]
  # Antes: "ordenes de magnitud log10(max/min) 3.000 < 3".
  expect_false(any(grepl("3.000 < 3", motivos, fixed = TRUE)))
  expect_true(any(grepl("2.9996 < 3", motivos, fixed = TRUE)))
  # Control: lo que ya se distinguia con tres decimales no cambia.
  expect_true(any(grepl("proporcion de positivos 0.900 < 1.000", motivos,
                        fixed = TRUE)))
})
