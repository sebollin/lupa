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

.estimar_catalogo_falso <- function(datos) {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  llamada <- new.env(parent = emptyenv())
  llamada$sql <- NA_character_
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "PqConnection",
    .consultar_dbi = function(conexion, sql, presupuesto = NULL,
                              filas = -1L, etapa = "consulta") {
      llamada$sql <- sql
      list(ok = TRUE, datos = datos, motivo = NA_character_)
    },
    .package = "lupa"
  )
  resultado <- lupa:::.estimar_distintos_catalogo_dbi(
    conexion, "public.tabla", "categoria", lupa:::.presupuesto_dbi(Inf)
  )
  list(resultado = resultado, sql = llamada$sql)
}

test_that("catalogo pide inherited y recorre la jerarquia en su SQL", {
  datos <- data.frame(
    lupa_columna = character(), lupa_n_distinct = numeric(),
    lupa_n_filas = numeric(), lupa_inherited = logical(),
    lupa_sin_hijas = logical(), stringsAsFactors = FALSE
  )
  salida <- .estimar_catalogo_falso(datos)

  expect_match(salida$sql, "s.inherited", fixed = TRUE)
  expect_match(salida$sql, "pg_catalog.pg_inherits", fixed = TRUE)
  expect_match(salida$sql, "filas_jerarquia", fixed = TRUE)
  expect_match(salida$sql, "relkind <> 'p'", fixed = TRUE)
  expect_match(salida$sql, "lupa_sin_hijas", fixed = TRUE)
})

test_that("catalogo elige inherited TRUE y usa las filas de la jerarquia", {
  datos <- data.frame(
    lupa_columna = c("categoria", "categoria"),
    lupa_n_distinct = c(2, 50), lupa_n_filas = c(1000, 2000),
    lupa_inherited = c(FALSE, TRUE), lupa_sin_hijas = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  salida <- .estimar_catalogo_falso(datos)$resultado
  estimacion <- salida$estimaciones$categoria

  expect_true(salida$disponible)
  expect_identical(estimacion$inherited, TRUE)
  expect_equal(estimacion$n_distintos, 50)
  expect_equal(estimacion$n_filas, 2000)
  expect_equal(estimacion$proporcion_distintos, 0.025)
})

test_that("catalogo convierte una fraccion heredada con el denominador total", {
  datos <- data.frame(
    lupa_columna = c("categoria", "categoria"),
    lupa_n_distinct = c(-0.002, -0.025), lupa_n_filas = c(1000, 2000),
    lupa_inherited = c(FALSE, TRUE), lupa_sin_hijas = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  estimacion <- .estimar_catalogo_falso(datos)$resultado$estimaciones$categoria

  expect_equal(estimacion$n_distintos, 50)
  expect_equal(estimacion$n_filas, 2000)
  expect_equal(estimacion$proporcion_distintos, 0.025)
})

test_that("catalogo no publica una fraccion sin denominador de jerarquia", {
  datos <- data.frame(
    lupa_columna = c("categoria", "categoria"),
    lupa_n_distinct = c(2, -0.025), lupa_n_filas = c(1000, NA_real_),
    lupa_inherited = c(FALSE, TRUE), lupa_sin_hijas = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  estimacion <- .estimar_catalogo_falso(datos)$resultado$estimaciones$categoria

  expect_false(estimacion$disponible)
  expect_true(is.na(estimacion$n_distintos))
  expect_match(estimacion$motivo, "reltuples")
})

test_that("catalogo conserva la fila propia cuando no hay hijas", {
  datos <- data.frame(
    lupa_columna = "categoria", lupa_n_distinct = -0.5,
    lupa_n_filas = 1000, lupa_inherited = FALSE, lupa_sin_hijas = TRUE,
    stringsAsFactors = FALSE
  )
  estimacion <- .estimar_catalogo_falso(datos)$resultado$estimaciones$categoria

  expect_true(estimacion$disponible)
  expect_identical(estimacion$inherited, FALSE)
  expect_equal(estimacion$n_distintos, 500)
  expect_equal(estimacion$n_filas, 1000)
})

test_that("catalogo no elige en silencio dos filas indistinguibles", {
  datos <- data.frame(
    lupa_columna = c("categoria", "categoria"),
    lupa_n_distinct = c(2, 50), lupa_n_filas = c(1000, 1000),
    lupa_inherited = c(FALSE, FALSE), lupa_sin_hijas = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  estimacion <- .estimar_catalogo_falso(datos)$resultado$estimaciones$categoria

  expect_false(estimacion$disponible)
  expect_true(is.na(estimacion$n_distintos))
  expect_match(estimacion$motivo, "no se puede decidir")
})

test_that("catalogo no usa la fila propia si hay hijas sin estadistica heredada", {
  datos <- data.frame(
    lupa_columna = "categoria", lupa_n_distinct = 2,
    lupa_n_filas = 1000, lupa_inherited = FALSE, lupa_sin_hijas = FALSE,
    stringsAsFactors = FALSE
  )
  estimacion <- .estimar_catalogo_falso(datos)$resultado$estimaciones$categoria

  expect_false(estimacion$disponible)
  expect_match(estimacion$motivo, "tiene hijas")
})
