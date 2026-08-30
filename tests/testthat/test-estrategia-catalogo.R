test_that("catalogo interpreta la semantica positiva y negativa de n_distinct", {
  unico <- lupa:::.interpretar_n_distintos_catalogo_dbi(-1, 20000, "id")
  cinco <- lupa:::.interpretar_n_distintos_catalogo_dbi(5, 20000, "grupo")

  expect_true(unico$disponible)
  expect_equal(unico$n_distintos, 20000)
  expect_match(unico$motivo, "fraccion")
  expect_match(unico$motivo, "reltuples")
  expect_equal(cinco$n_distintos, 5)
  expect_match(cinco$motivo, "Estimacion de catalogo")
})

test_that("catalogo no convierte la falta de reltuples en cero", {
  sin_filas <- lupa:::.interpretar_n_distintos_catalogo_dbi(-0.5, NA_real_, "id")

  expect_false(sin_filas$disponible)
  expect_true(is.na(sin_filas$n_distintos))
  expect_match(sin_filas$motivo, "No se supone cero")
})

test_that("catalogo no esconde un valor negativo no interpretable", {
  sin_estadistica <- lupa:::.interpretar_n_distintos_catalogo_dbi(
    NA_real_, NA_real_, "id"
  )

  expect_false(sin_estadistica$disponible)
  expect_true(is.na(sin_estadistica$n_distintos))
  expect_match(sin_estadistica$motivo, "ANALYZE")
})
