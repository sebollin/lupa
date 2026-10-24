# 13 s: ejercita el derrame a disco con bloques reales. Es comprobacion de
# escala, no de comportamiento en el caso comun; se saltea en CRAN y corre en la
# CI y en `revalidar.sh`.
test_that("LSH externo conserva pares al cortar entre bloques", {
  skip_on_cran()
  skip_if_not_installed("stringdist")
  set.seed(42)
  base <- c(
    "juan carlos perez rodriguez montevideo 12345",
    "maria elena gomez fernandez salto 98765",
    "pedro antonio lopez martinez canelones 45678",
    "ana luisa ramirez ortiz punta del este 11111"
  )
  ruido <- replicate(
    32, paste(sample(c(letters, " ", "0", "9"), 30, replace = TRUE),
              collapse = "")
  )
  valores <- c(base, ruido[seq_len(14L)], base, ruido[15:32])
  parametros <- list(
    metodo = "osa", umbral = 0.2, bandas = 4L, filas_banda = 4L,
    q = 3L, max_cubeta = 1000L, max_resultados = 100L,
    muestra_estimacion = 1000L, nucleos = 1L
  )
  referencia <- do.call(
    lupa:::.comparar_lsh_duplicados,
    c(list(valores = valores, filas = seq_along(valores)), parametros)
  )
  externo <- do.call(
    lupa:::.ejecutar_lsh_bloques,
    c(list(
      bloques = lupa:::.bloques_de_vector(valores, tamano = 18L),
      chunk_qgramas = 17L
    ), parametros)
  )
  sobre <- externo$resultado
  expect_identical(sobre$estado, "calculado")
  expect_identical(sobre$resultado$pares, referencia$pares)
  expect_equal(
    sobre$alcance[c(
      "lsh_candidatos_generados", "lsh_candidatos_unicos",
      "lsh_candidatos_descartados_bandas"
    )],
    referencia$alcance[c(
      "lsh_candidatos_generados", "lsh_candidatos_unicos",
      "lsh_candidatos_descartados_bandas"
    )]
  )
  expect_true(sobre$alcance$lsh_externo)
  expect_false(sobre$alcance$lsh_diccionario_completo_en_memoria)
  expect_identical(sobre$alcance$lsh_factor_pico, 30)
  expect_identical(sobre$alcance$lsh_piso_fila, 1L)
  expect_gt(sobre$alcance$lsh_derrame_bytes, 0)
})

test_that("LSH externo desglosa el diccionario y el derrame", {
  skip_if_not_installed("stringdist")
  valores <- c("alpha", "beta", "gamma", "alpha", "beta", "gamma")
  externo <- lupa:::.ejecutar_lsh_bloques(
    lupa:::.bloques_de_vector(valores, tamano = 2L),
    metodo = "osa", umbral = 0, bandas = 4L, filas_banda = 3L,
    q = 3L, max_cubeta = 1000L, max_resultados = Inf,
    muestra_estimacion = 100L, chunk_qgramas = 3L, nucleos = 1L
  )
  sobre <- externo$resultado
  residentes <- unlist(sobre$residentes_lsh, use.names = FALSE)
  expect_identical(names(sobre$residentes_lsh), c(
    "buffers_runs", "cache_diccionario", "estado_fila", "otros"
  ))
  expect_lte(sum(residentes), sobre$bytes_retenidos)
  expect_gt(sobre$memoria_diccionario$maximo_cache, 0)
  expect_false(sobre$memoria_diccionario$diccionario_completo_en_memoria)
  expect_equal(
    sobre$memoria_diccionario$maximo_intervalo,
    sobre$alcance$lsh_memoria_maximo_intervalo
  )
  expect_gt(sobre$derrame$bytes, 0)
  expect_identical(sobre$derrame$version, "lsh-runs-1")
})

test_that("LSH no publica un degradado sin snapshot u orden estable", {
  valores <- c("alpha", "beta")
  sin_snapshot <- lupa:::.ejecutar_lsh_bloques(
    lupa:::.bloques_de_vector(valores, tamano = 1L),
    snapshot_id = NA_character_, orden_id = "orden-entrada",
    bandas = 2L, filas_banda = 2L, max_resultados = Inf,
    muestra_estimacion = 10L, chunk_qgramas = 3L, nucleos = 1L
  )
  expect_identical(sin_snapshot$resultado$estado, "no_disponible")
  expect_match(sin_snapshot$resultado$motivo, "snapshot_inestable")

  sin_backend <- testthat::local_mocked_bindings(
    .lsh_backend_externo_disponible = function(...) FALSE,
    .package = "lupa"
  )
  expect_identical(
    lupa:::.ejecutar_lsh_bloques(
      lupa:::.bloques_de_vector(valores, tamano = 1L),
      bandas = 2L, filas_banda = 2L, max_resultados = Inf,
      muestra_estimacion = 10L, chunk_qgramas = 3L, nucleos = 1L
    )$resultado$estado,
    "no_disponible"
  )
  invisible(sin_backend)
})

test_that("el vigilante registra memoria LSH y factor de dimensionamiento", {
  skip_if_not_installed("stringdist")
  vigilante <- lupa:::.iniciar_vigilante("lsh-test")
  ejecucion <- lupa:::.ejecutar_lsh_bloques(
    lupa:::.bloques_de_vector(c("alpha", "beta", "alpha"), tamano = 1L),
    bandas = 2L, filas_banda = 2L, max_resultados = Inf,
    muestra_estimacion = 10L, chunk_qgramas = 3L, nucleos = 1L,
    vigilante = vigilante
  )
  eventos <- lupa:::.eventos_vigilante(vigilante)
  expect_true(all(c("memoria_max_intervalo", "sonda_proceso",
                    "residentes_lsh") %in% names(eventos)))
  expect_true(all(eventos$factor_pico == 30))
  expect_true(all(vapply(eventos$residentes_lsh, is.list, logical(1L))))
  expect_identical(ejecucion$resultado$estado, "calculado")
})
