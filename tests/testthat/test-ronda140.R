.ronda140_codificar_viejo <- function(grupo, y) {
  as.integer(interaction(
    factor(grupo), factor(y, exclude = NULL), drop = TRUE, lex.order = TRUE
  ))
}

.ronda140_argumentos_dependencia <- function(datos, umbral, muestra) {
  list(
    datos = datos, umbral = umbral, muestra = muestra,
    min_observaciones = 3L, max_comparaciones = Inf, max_trabajo = Inf
  )
}

.ronda140_comparar_dependencia <- function(datos, umbral, muestra) {
  argumentos <- .ronda140_argumentos_dependencia(datos, umbral, muestra)
  referencia <- testthat::with_mocked_bindings(
    do.call(detectar_dependencias, argumentos),
    .codificar_parejas_dependencia = .ronda140_codificar_viejo,
    .poda_dependencia_pares = function(...) FALSE,
    .package = "lupa"
  )
  sin_codificacion_nueva <- testthat::with_mocked_bindings(
    do.call(detectar_dependencias, argumentos),
    .codificar_parejas_dependencia = .ronda140_codificar_viejo,
    .package = "lupa"
  )
  sin_poda_nueva <- testthat::with_mocked_bindings(
    do.call(detectar_dependencias, argumentos),
    .poda_dependencia_pares = function(...) FALSE,
    .package = "lupa"
  )
  nueva <- do.call(detectar_dependencias, argumentos)

  testthat::expect_identical(sin_codificacion_nueva, referencia)
  testthat::expect_identical(sin_poda_nueva, referencia)
  testthat::expect_identical(nueva, referencia)
  invisible(nueva)
}

test_that("la clave entera corta en el borde exacto y conserva la particion", {
  expect_true(.clave_dependencia_segura(2, 2^52))
  expect_false(.clave_dependencia_segura(2, 2^52 + 1))

  grupo <- c(1L, 1L, 2L, 2L, 1L, 2L)
  y <- c("a", "b", "a", "b", "a", "b")
  nueva <- .codificar_parejas_dependencia(grupo, y)
  vieja <- .ronda140_codificar_viejo(grupo, y)
  expect_identical(sort(unique(nueva)), seq_len(max(nueva)))
  expect_identical(tabulate(nueva), tabulate(vieja))

  forzada <- testthat::with_mocked_bindings(
    .codificar_parejas_dependencia(grupo, y),
    .clave_dependencia_segura = function(...) FALSE,
    .package = "lupa"
  )
  expect_identical(forzada, vieja)

  datos <- data.frame(
    x = c("a", "a", "a", "b", "b", "b", NA),
    y = c("u", "u", "v", "u", "v", "v", "u"),
    stringsAsFactors = FALSE
  )
  particion <- .particion_dependencia(datos$x, datos$y)
  resumen_nuevo <- .resumen_dependencia(datos$x, datos$y)
  resumen_viejo <- testthat::with_mocked_bindings(
    .resumen_dependencia(datos$x, datos$y),
    .codificar_parejas_dependencia = .ronda140_codificar_viejo,
    .package = "lupa"
  )
  expect_identical(sort(unique(particion$pareja)), seq_len(max(particion$pareja)))
  expect_identical(particion$conteos, tabulate(particion$pareja))
  expect_identical(particion$conteos, tabulate(.ronda140_codificar_viejo(
    particion$grupo, particion$y
  )))
  expect_identical(resumen_nuevo, resumen_viejo)
  expect_identical(
    resumen_nuevo$grupos_conflicto, resumen_viejo$grupos_conflicto
  )
})

test_that("la cota de pares usa P en el subconjunto valido", {
  datos <- data.frame(
    x = c("a", "a", "a", "b", "b", "b", NA),
    y = c("u", "u", "v", "u", "v", "v", "u"),
    stringsAsFactors = FALSE
  )
  particion <- .particion_dependencia(datos$x, datos$y)
  expect_equal(length(particion$conteos), 4L)
  expect_equal(particion$grupos, 2L)
  expect_true(.poda_dependencia_pares(particion, 1))
  expect_false(.poda_dependencia_pares(particion, 0.5))
})

test_that("la bateria completa de dependencias no cambia con los dos atajos", {
  tablas <- list(
    texto = data.frame(
      grupo = rep(c("g1", "g2", "g3"), each = 8L),
      valor = rep(c("a", "b", "c"), each = 8L),
      ruido = rep(letters[1:4], 6L),
      stringsAsFactors = FALSE
    ),
    ausentes = data.frame(
      grupo = c(rep("g1", 6L), rep("g2", 6L), NA, "g2", "g2"),
      valor = c(rep("a", 5L), "b", rep("c", 5L), "d", "a", NA, "c"),
      marca = c(rep("m", 7L), rep("n", 7L), "m"),
      stringsAsFactors = FALSE
    ),
    empates = data.frame(
      grupo = rep(c("g1", "g2"), each = 10L),
      valor = rep(c("a", "b"), 10L),
      otra = rep(c("u", "v", "w", "z"), 5L),
      stringsAsFactors = FALSE
    ),
    fechas_factores = data.frame(
      fecha = as.Date(c(
        rep("2024-01-01", 5L), rep("2024-01-02", 5L),
        rep("2024-01-03", 5L), rep("2024-01-04", 5L)
      )),
      grupo = factor(rep(c("g1", "g2"), each = 10L)),
      valor = factor(rep(c("a", "b"), each = 10L)),
      stringsAsFactors = FALSE
    ),
    mezcla = data.frame(
      grupo = rep(c(" A ", "A", " B ", "B"), each = 5L),
      valor = rep(c("x", "x", "y", "y"), each = 5L),
      numero = rep(c(1L, 2L, 3L, 4L), each = 5L),
      stringsAsFactors = FALSE
    )
  )

  for (datos in tablas) {
    for (umbral in c(1, 0.995, 0.8, 0.5)) {
      for (muestra in list(Inf, 12L)) {
        .ronda140_comparar_dependencia(datos, umbral, muestra)
      }
    }
  }
})

test_that("la bateria tambien conserva el detector con normalizar prendido o apagado", {
  datos <- data.frame(
    grupo = rep(c(" A ", "a", " B ", "b"), each = 10L),
    valor = rep(c("x", "x", "y", "y"), each = 10L),
    stringsAsFactors = FALSE
  )
  argumentos <- list(
    datos = datos, muestra = Inf, analizar_dependencias = TRUE,
    duplicados_aproximados = FALSE, casi_duplicados_vocabulario = FALSE
  )
  for (normalizar in c(TRUE, FALSE)) {
    argumentos$normalizar <- normalizar
    referencia <- testthat::with_mocked_bindings(
      do.call(perfilar, argumentos),
      .codificar_parejas_dependencia = .ronda140_codificar_viejo,
      .poda_dependencia_pares = function(...) FALSE,
      .package = "lupa"
    )$dependencias
    nueva <- do.call(perfilar, argumentos)$dependencias
    expect_identical(nueva, referencia)
  }
})
