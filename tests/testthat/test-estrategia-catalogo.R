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

test_that("catalogo se degrada fuera del universo completo", {
  for (modo in c("muestreado", "aproximado")) {
    estrategia <- lupa:::.estrategia_distintos_dbi(
      "distintos", list(nombre = "todas"), TRUE, "catalogo"
    )
    salida <- lupa:::.resolver_estrategia_distintos_dbi(
      conexion = NULL, estrategia = estrategia, presupuesto = NULL,
      hay_metrica = TRUE, tabla = "tabla", columnas = "valor", modo = modo
    )

    expect_identical(salida$estado, "no_disponible")
    expect_false(salida$disponible)
    expect_true(is.na(salida$estrategia_resuelta))
    expect_match(salida$motivo, "relacion entera")
    expect_match(salida$motivo, "subconjunto")
    expect_match(salida$motivo, "universo")
  }
})

test_that("el plan publica la degradacion de catalogo antes de correr", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "tabla_plan_catalogo", data.frame(
    id = 1:20, valor = letters[1:20]
  ))

  plan <- lupa::plan_perfilado_dbi(
    conexion, "tabla_plan_catalogo", modo = "muestreado", muestra = 5,
    metricas = "distintos", estrategia_distintos = "catalogo",
    bloque_muestra = "solo_agregados"
  )
  estrategia <- attr(plan, "estrategia_distintos", exact = TRUE)

  expect_identical(estrategia$estado, "no_disponible")
  expect_match(estrategia$motivo, "subconjunto")
  expect_identical(attr(plan, "metricas_ejecucion", exact = TRUE), character())
})

test_that("catalogo sigue publicando una estimacion en modo exacto", {
  estrategia <- lupa:::.estrategia_distintos_dbi(
    "distintos", list(nombre = "todas"), TRUE, "catalogo"
  )
  testthat::local_mocked_bindings(
    .estimar_distintos_catalogo_dbi = function(...) list(
      disponible = TRUE, estimaciones = list(), fuentes = list(),
      sql = "SELECT pg_stats", motivo = "estimacion disponible"
    ),
    .package = "lupa"
  )

  salida <- lupa:::.resolver_estrategia_distintos_dbi(
    conexion = NULL, estrategia = estrategia, presupuesto = NULL,
    hay_metrica = TRUE, tabla = "tabla", columnas = "valor", modo = "exacto"
  )
  publicada <- lupa:::.publicar_estrategia_distintos_dbi(salida)

  expect_true(salida$disponible)
  expect_identical(salida$estado, "estimado_catalogo")
  expect_identical(salida$estrategia_resuelta, "pg_stats.n_distinct")
  expect_identical(publicada$estado, "estimado_catalogo")
  expect_identical(publicada$fuente, "pg_stats.n_distinct")
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
