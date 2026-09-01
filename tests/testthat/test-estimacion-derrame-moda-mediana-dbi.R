# Pruebas propias de las dos familias nuevas de estimacion.

.stats_familias_derrame <- function(columnas = "x", n = 100000,
                                    width = 4, tipo = "integer",
                                    n_distinct = 50) {
  data.frame(
    relacion_oid = rep("1", length(columnas)),
    schemaname = rep("public", length(columnas)),
    tablename = rep("t", length(columnas)),
    reltuples = rep(n, length(columnas)), es_raiz = TRUE, hoja = TRUE,
    attname = columnas, n_distinct = rep(n_distinct, length(columnas)),
    avg_width = rep(width, length(columnas)), null_frac = 0,
    tipo = rep(tipo, length(columnas)), stringsAsFactors = FALSE
  )
}

.estimar_familia_derrame_falsa <- function(stats,
                                           estrategia = "Hashed",
                                           familia = "moda",
                                           forma = NA_character_) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "PqConnection",
    .escalar_dbi = function(conexion, sql, campo, presupuesto = NULL,
                            etapa = "consulta") {
      if (grepl("work_mem", sql, fixed = TRUE) &&
          !grepl("hash_mem_multiplier", sql, fixed = TRUE)) {
        return(list(ok = TRUE, valor = "4MB"))
      }
      list(ok = TRUE, valor = "2")
    },
    .consultar_dbi = function(conexion, sql, presupuesto = NULL, filas = -1L,
                              etapa = "consulta") {
      if (grepl("^EXPLAIN", sql)) {
        json <- paste0(
          "[{\"Plan\":{\"Node Type\":\"Limit\",\"Plans\":[",
          "{\"Node Type\":\"Aggregate\",\"Strategy\":\"",
          estrategia, "\"}]}}]"
        )
        return(list(
          ok = TRUE, datos = data.frame(plan = json), motivo = NA_character_
        ))
      }
      list(ok = TRUE, datos = stats, motivo = NA_character_)
    },
    .package = "lupa"
  )
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  prototipo <- data.frame(x = integer())
  lupa:::.estimar_derrame_familia_postgresql_dbi(
    conexion, "public.t", unique(stats$attname),
    lupa:::.presupuesto_dbi(Inf, instrumentar = FALSE), familia,
    forma = forma, dialecto = lupa:::.dialectos_dbi()$limit,
    tipos = stats$tipo, prototipo = prototipo, tabla_sql = "public.t"
  )
}

test_that("la normalizacion es posicional y conserva identificadores", {
  normalizada <- lupa:::.normalizar_sql_derrame_dbi(
    "SELECT 1, 1.50, 'a', 'a', \"x1\" FROM t WHERE z = -2"
  )
  expect_identical(
    normalizada,
    "SELECT $1, $2, $3, $4, \"x1\" FROM t WHERE z = $5"
  )
  expect_false(grepl("$3, $3", normalizada, fixed = TRUE))
  expect_identical(
    lupa:::.normalizar_sql_derrame_dbi("SELECT generate_series(2, 2)"),
    "SELECT generate_series($1, $2)"
  )
})

test_that("la moda publica el metodo del Aggregate explicado", {
  hash <- .estimar_familia_derrame_falsa(
    .stats_familias_derrame(n = 100000, n_distinct = 50000),
    estrategia = "Hashed", familia = "moda"
  )
  sort <- .estimar_familia_derrame_falsa(
    .stats_familias_derrame(n = 100000, width = 97, n_distinct = 100000),
    estrategia = "Sorted", familia = "moda"
  )

  expect_identical(hash$metodo, "hash")
  expect_true(is.finite(hash$columnas$estado_hash_bytes[[1L]]))
  expect_true(is.na(hash$columnas$estado_sort_bytes[[1L]]))
  expect_identical(sort$metodo, "sort")
  expect_true(isTRUE(sort$supera_memoria))
  expect_true(is.finite(sort$columnas$estado_memoria_bytes[[1L]]))
  expect_true(is.na(sort$columnas$estado_hash_bytes[[1L]]))
  expect_match(hash$fuente, "EXPLAIN")
})

test_that("los pisos distinguen numeric de tipos fijos", {
  fijo <- .estimar_familia_derrame_falsa(
    .stats_familias_derrame(n = 100000, width = 4, tipo = "integer"),
    familia = "mediana", forma = "subconsulta_escalar"
  )
  numerico <- .estimar_familia_derrame_falsa(
    .stats_familias_derrame(n = 100000, width = 5, tipo = "numeric"),
    familia = "mediana", forma = "subconsulta_escalar"
  )

  expect_false(isTRUE(fijo$supera_memoria))
  expect_true(isTRUE(numerico$supera_memoria))
  expect_equal(numerico$columnas$estado_memoria_bytes[[1L]], 4200000)
  expect_identical(numerico$metodo, "sort")
  expect_true(all(is.na(numerico$lotes$estado_io_total_bytes)))
})

test_that("la consolidada decide por maximo y publica la suma como I/O", {
  estimacion <- .estimar_familia_derrame_falsa(
    .stats_familias_derrame(c("x", "y"), n = 90000, width = 5,
                            tipo = "numeric"),
    familia = "mediana", forma = "consolidada"
  )
  lote <- estimacion$lotes[1L, , drop = FALSE]

  expect_false(isTRUE(lote$supera_memoria))
  expect_equal(lote$tamano_estimado_bytes[[1L]], 3780000)
  expect_equal(lote$estado_io_total_bytes[[1L]], 2340000)
  expect_equal(estimacion$columnas$estado_io_total_bytes, c(2340000, 2340000))
  expect_true(lote$estado_io_total_bytes[[1L]] > lote$tamano_estimado_bytes[[1L]] / 2)
})

test_that("la medicion de validos reemplaza al catalogo en meta", {
  estimacion <- .estimar_familia_derrame_falsa(
    .stats_familias_derrame(n = 100000, width = 5, tipo = "numeric"),
    familia = "mediana", forma = "subconsulta_escalar"
  )
  actualizada <- lupa:::.actualizar_n_validos_estimacion_dbi(
    estimacion,
    list(conteos = list(x = list(validos = list(ok = TRUE, valor = 1100000)))),
    metricas = c("validos", "mediana"), salida = "meta"
  )

  expect_equal(actualizada$columnas$n_validos_catalogo[[1L]], 100000)
  expect_equal(actualizada$columnas$n_validos_medido[[1L]], 1100000)
  expect_true(isTRUE(actualizada$supera_memoria))
  expect_match(actualizada$fuente, "n_validos medido")
  expect_match(actualizada$motivo, "salida")
})

test_that("fuera de PostgreSQL y en muestra las estimaciones no inventan", {
  afuera <- lupa:::.estimar_derrame_familia_postgresql_dbi(
    structure(list(), class = "SQLiteConnection"), "t", "x",
    lupa:::.presupuesto_dbi(Inf), "moda"
  )
  muestra <- lupa:::.estimar_derrame_familia_postgresql_dbi(
    structure(list(), class = "PqConnection"), "t", "x",
    lupa:::.presupuesto_dbi(Inf), "mediana", universo = "muestra_motor"
  )

  expect_identical(afuera$estado, "no_disponible")
  expect_match(afuera$motivo, "PostgreSQL")
  expect_identical(muestra$estado, "no_disponible")
  expect_match(muestra$motivo, "no se inventa")
})
