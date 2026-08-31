partir_fixture_bloques <- function(x, k) {
  indices <- lupa:::.particionar_bloques(length(x), k = k)
  lapply(indices, function(bloque) {
    bloque$valores <- x[bloque$valores]
    bloque
  })
}

hacer_nan_fixture <- function(payload) {
  bits <- writeBin(0, raw(), size = 8, endian = "little")
  bits[1:8] <- as.raw(c(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF8, 0x7F))
  carga <- writeBin(payload, raw(), size = 8, endian = "little")
  bits[1:6] <- carga[1:6]
  bits[6] <- as.raw(bitwOr(as.integer(bits[6]), 0x08))
  readBin(bits, "double", n = 1, size = 8, endian = "little")
}

ejecutar_fixture <- function(x, familia, k, ...) {
  lupa:::.ejecutar_vector_bloques(
    x, familia, k = k, ...
  )$resultado
}

test_that("las familias acotadas conservan el resultado por bloques", {
  valores <- c(-Inf, -2, 0, 1, NaN, NA_real_, Inf)
  conteos <- lapply(c(1L, 2L, 7L, 31L), function(k) {
    ejecutar_fixture(valores, "conteos", k)
  })
  expect_true(all(vapply(
    conteos, function(salida) identical(salida$resultado, conteos[[1L]]$resultado),
    logical(1L)
  )))
  expect_identical(conteos[[1L]]$resultado$n, 7)
  expect_identical(conteos[[1L]]$resultado$n_faltantes, 2)
  expect_identical(conteos[[1L]]$resultado$n_validos, 5)
  expect_identical(conteos[[1L]]$resultado$n_nan, 1)
  expect_identical(conteos[[1L]]$resultado$n_infinito_positivo, 1)
  expect_identical(conteos[[1L]]$resultado$n_infinito_negativo, 1)
  expect_identical(conteos[[1L]]$resultado$n_ceros, 1)
  expect_identical(conteos[[1L]]$resultado$n_negativos, 1)

  finitos <- valores[is.finite(valores)]
  cuantitativos <- lapply(c(1L, 2L, 7L, 31L), function(k) {
    ejecutar_fixture(finitos, "cuantitativos", k)
  })
  expect_true(all(vapply(
    cuantitativos,
    function(salida) isTRUE(all.equal(
      salida$resultado, cuantitativos[[1L]]$resultado, tolerance = 1e-14
    )),
    logical(1L)
  )))
  referencia <- list(
    minimo = min(finitos), maximo = max(finitos), media = mean(finitos),
    desvio = stats::sd(finitos)
  )
  expect_equal(cuantitativos[[1L]]$resultado[names(referencia)], referencia,
               tolerance = 1e-14)

  textos <- c("a", "abcd", NA_character_, "xy")
  longitudes <- lapply(c(1L, 2L, 7L, 31L), function(k) {
    ejecutar_fixture(textos, "longitudes", k)
  })
  expect_true(all(vapply(
    longitudes, function(salida) identical(salida$resultado, longitudes[[1L]]$resultado),
    logical(1L)
  )))
  expect_identical(longitudes[[1L]]$resultado,
                   c(minimo = 1, maximo = 4, media = 7 / 3))
})

test_that("el mapa central usa la igualdad de R y conserva ordinales", {
  valores <- c(-0, 0, 1, 1, 2, hacer_nan_fixture(1L),
               hacer_nan_fixture(2L), NA_real_)
  salidas <- lapply(c(1L, 2L, 7L, 31L), function(k) {
    ejecutar_fixture(valores, "distintos", k, incluir_ausentes = TRUE)
  })
  expect_true(all(vapply(
    salidas, function(salida) identical(salida$resultado, salidas[[1L]]$resultado),
    logical(1L)
  )))
  mapa <- salidas[[1L]]$resultado
  expect_identical(mapa$frecuencia, c(2L, 2L, 1L, 2L, 1L))
  expect_identical(mapa$primer_ordinal, c(1, 3, 5, 6, 8))
  expect_true(any(mapa$representante == 0))
  expect_true(any(is.nan(mapa$representante)))
  expect_true(any(is.na(mapa$representante) & !is.nan(mapa$representante)))
})

test_that("la clave de filas reproduce duplicated.data.frame por bloques", {
  fixture <- data.frame(
    doble = c(-0, 0, hacer_nan_fixture(1L), hacer_nan_fixture(2L),
              NA_real_, NA_real_),
    factor = factor(c("a", "a", "b", "b", "a", "a")),
    stringsAsFactors = FALSE
  )
  referencia <- base::duplicated.data.frame(fixture)
  esperado <- c(sum(referencia), sum(referencia | rev(
    base::duplicated.data.frame(fixture[rev(seq_len(nrow(fixture))), , drop = FALSE])
  )))
  for (k in c(1L, 2L, 7L, 31L)) {
    actual <- lupa:::.conteos_filas_duplicadas_bloques(fixture, k = k)
    expect_identical(actual$filas_duplicadas, as.integer(esperado[[1L]]))
    expect_identical(actual$filas_en_grupos_duplicados,
                    as.integer(esperado[[2L]]))
    expect_identical(actual$estado, "calculado")
    expect_true(isTRUE(actual$exacto))
  }

  skip_if_not_installed("bit64")
  grande <- data.frame(
    x = bit64::as.integer64(c("9007199254740993", "9007199254740994",
                              "9007199254740993"))
  )
  actual <- lupa:::.conteos_filas_duplicadas_bloques(grande, k = 2L)
  expect_identical(actual$filas_duplicadas, 1L)
  expect_identical(actual$filas_en_grupos_duplicados, 2L)

  matriz <- data.frame(id = c(1, 2, 1))
  matriz$componentes <- I(cbind(c("x", "y", "x"), c(1, 2, 1)))
  actual <- lupa:::.conteos_filas_duplicadas_bloques(matriz, k = 2L)
  expect_identical(actual$filas_duplicadas, 1L)
  expect_identical(actual$filas_en_grupos_duplicados, 2L)

  alta_precision <- data.frame(
    x = c(0.12345678901234567, 0.12345678901234568,
          0.12345678901234567)
  )
  actual <- lupa:::.conteos_filas_duplicadas_bloques(alta_precision, k = 2L)
  expect_identical(actual$filas_duplicadas,
                   sum(base::duplicated.data.frame(alta_precision)))

  fechas <- data.frame(
    x = as.Date(c("2020-01-01", "2020-01-01", "2020-01-02"))
  )
  actual <- lupa:::.conteos_filas_duplicadas_bloques(fechas, k = 2L)
  expect_identical(actual$filas_duplicadas,
                   sum(base::duplicated.data.frame(fechas)))
})

test_that("fusionar exige la configuracion y suma estados asociativos", {
  valores <- c(-10, -2, 0, 1, 20, 100)
  bloques <- partir_fixture_bloques(valores, 2L)
  izquierda <- lupa:::iniciar(
    "x", "numeric", familia = "cuantitativos",
    configuracion = list(contar_signos = TRUE)
  )
  derecha <- lupa:::iniciar(
    "x", "numeric", familia = "cuantitativos",
    configuracion = list(contar_signos = TRUE)
  )
  lupa:::absorber(izquierda, bloques[[1L]])
  lupa:::absorber(derecha, bloques[[2L]])
  fusionada <- lupa:::finalizar(lupa:::fusionar(izquierda, derecha))
  referencia <- ejecutar_fixture(valores, "cuantitativos", 1L,
                                 configuracion = list(contar_signos = TRUE))
  expect_equal(fusionada$resultado[names(referencia$resultado)],
               referencia$resultado, tolerance = 1e-14)

  incompatible <- lupa:::iniciar("otra", "numeric", familia = "cuantitativos")
  fallida <- lupa:::fusionar(izquierda, incompatible)
  expect_identical(fallida$estado, "no_disponible")
  expect_match(fallida$fallo, "incompatible")
})

test_that("la fusion del mapa conserva igualdad, frecuencias y ordinales", {
  valores <- c("b", "a", "b", "c", "a", "c")
  bloques <- partir_fixture_bloques(valores, 2L)
  izquierda <- lupa:::iniciar(
    "x", "character", familia = "distintos", requiere_orden = TRUE
  )
  derecha <- lupa:::iniciar(
    "x", "character", familia = "distintos", requiere_orden = TRUE
  )
  lupa:::absorber(izquierda, bloques[[1L]])
  lupa:::absorber(derecha, bloques[[2L]])
  fusionada <- lupa:::finalizar(lupa:::fusionar(izquierda, derecha))
  referencia <- ejecutar_fixture(valores, "distintos", 1L)
  expect_identical(fusionada$resultado, referencia$resultado)
})

test_that("la fusion de filas conserva el conteo de grupos", {
  datos <- data.frame(x = c("a", "b", "a", "c", "b", "c"))
  bloques <- partir_fixture_bloques(datos$x, 2L)
  izquierda <- lupa:::iniciar(
    "fila", "data.frame", familia = "filas_distintos", requiere_orden = TRUE
  )
  derecha <- lupa:::iniciar(
    "fila", "data.frame", familia = "filas_distintos", requiere_orden = TRUE
  )
  bloques <- lapply(bloques, function(bloque) {
    bloque$valores <- data.frame(x = bloque$valores)
    bloque
  })
  lupa:::absorber(izquierda, bloques[[1L]])
  lupa:::absorber(derecha, bloques[[2L]])
  resultado <- lupa:::finalizar(lupa:::fusionar(izquierda, derecha))
  expect_identical(resultado$resultado$filas_duplicadas, 3L)
  expect_identical(resultado$resultado$filas_en_grupos_duplicados, 6L)
})

test_that("la aplicabilidad rechaza estadisticos globales y conserva filas", {
  datos <- data.frame(v = 1:10)
  expect_error(
    lupa:::.resolver_aplicabilidad(
      datos, names(datos), aplicabilidad = list(v = ~ v > mean(v))
    ),
    "aplicabilidad_no_fila"
  )
  mascara <- lupa:::.evaluar_predicado_aplicabilidad(
    datos, "v", ~ v %% 2 == 0
  )
  expect_identical(mascara, c(FALSE, TRUE, FALSE, TRUE, FALSE,
                              TRUE, FALSE, TRUE, FALSE, TRUE))
  for (k in c(1L, 2L, 7L, 31L)) {
    salida <- lupa:::.ejecutar_vector_bloques(
      datos$v, "conteos", k = k, aplicable = mascara
    )$resultado
    expect_identical(salida$estado, "calculado")
    expect_identical(salida$resultado$n_aplicables, 5)
    expect_identical(salida$resultado$n_no_aplica, 5)
  }
})

test_that("el vigilante registra bloques y la barrera de finalizar", {
  vigilante <- lupa:::.iniciar_vigilante("corrida-test", tope_bytes = 1)
  ejecucion <- lupa:::.ejecutar_vector_bloques(
    1:10, "cuantitativos", k = 2L
  )
  acumulador <- lupa:::iniciar("x", "numeric", familia = "cuantitativos")
  bloques <- partir_fixture_bloques(1:10, 2L)
  observada <- lupa:::.ejecutar_acumulador_bloques(
    acumulador, bloques, vigilante = vigilante
  )
  eventos <- lupa:::.eventos_vigilante(vigilante)
  expect_identical(nrow(eventos), 3L)
  expect_true(all(is.finite(eventos$bytes_retenidos)))
  expect_true(all(eventos$bytes_retenidos >= 0))
  expect_identical(tail(eventos$fase, 1L), "finalizar")
  expect_true(all(eventos$tipo_evento == "presion_memoria_proceso"))
  expect_identical(observada$resultado$estado, "calculado")
  expect_identical(ejecucion$resultado$estado, "calculado")
})

test_that("un tope del mapa publica una cota visible", {
  ejecucion <- lupa:::.ejecutar_vector_bloques(
    letters[1:5], "distintos", k = 2L, max_entradas = 2L
  )
  expect_identical(ejecucion$resultado$estado, "cota")
  expect_false(ejecucion$resultado$exacto)
  expect_match(ejecucion$resultado$motivo, "mapa_distintos_truncado")
  expect_identical(ejecucion$resultado$cota$direccion, ">=")
  expect_true(is.finite(ejecucion$resultado$bytes_retenidos))
})
