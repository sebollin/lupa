testthat::skip_on_cran()
skip_if_not_installed("DBI")

.plan_consultas_condicionales <- function(con, tabla) {
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla,
    " (id BIGINT, monto DECIMAL(18,2), codigo VARCHAR(40),",
    " descripcion VARCHAR(80))"
  ))
  n <- 10000L
  datos <- data.frame(
    id = seq_len(n),
    monto = c(999, ((seq_len(n - 1L) - 1L) %% 1000L) / 100),
    codigo = paste0("C", seq_len(n) %% 17L),
    descripcion = paste0("D", seq_len(n) %% 23L),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, tabla, datos, append = TRUE)
  argumentos <- list(
    conexion = con, tabla = tabla, muestra = 1000L,
    bloque_muestra = "con_muestra", metricas = "validos",
    incluir_valores = FALSE, proteger_datos_personales = FALSE
  )
  plan <- plan_perfilado_dbi(
    con, tabla, muestra = 1000L, bloque_muestra = "con_muestra",
    metricas = "validos", incluir_valores = FALSE
  )
  perfil <- do.call(perfilar_dbi, argumentos)
  plan_publicado <- perfil$resumen_tabla$meta$plan
  clase <- "conteos de faltantes disfrazados"
  fila <- plan[plan$clase_consulta == clase, , drop = FALSE]
  fila_publicada <- plan_publicado[
    plan_publicado$clase_consulta == clase, , drop = FALSE
  ]
  conciliacion <- perfil$resumen_tabla$meta$consultas$conciliacion
  detalle <- conciliacion$detalle[
    conciliacion$detalle$clase_consulta == clase, , drop = FALSE
  ]
  expect_false(any(grepl(
    "magnitud (MIN)", plan$clase_consulta, fixed = TRUE
  )))
  expect_false(any(grepl(
    "magnitud (MIN)", plan_publicado$clase_consulta, fixed = TRUE
  )))
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$n_consultas, 2)
  expect_gte(fila$n_consultas_max, fila$n_consultas + 1)
  expect_equal(nrow(fila_publicada), 1L)
  expect_equal(fila_publicada$n_consultas, 2)
  expect_gte(fila_publicada$n_consultas_max, fila_publicada$n_consultas + 1)
  expect_identical(conciliacion$estado, "conciliado")
  expect_equal(nrow(detalle), 1L)
  expect_identical(detalle$direccion, "condicional")
  expect_gt(detalle$n_emitidas, detalle$n_anunciadas)
  expect_lte(detalle$n_emitidas, fila_publicada$n_consultas_max)
  expect_match(detalle$motivo, "caso esperado.*peor caso")
  expect_lte(
    perfil$resumen_tabla$meta$consultas$emitidas,
    sum(plan_publicado$n_consultas_max)
  )
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  invisible(perfil)
}

test_that("SQLite publica el rango de las consultas condicionales", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .plan_consultas_condicionales(con, "lupa_condicional_sqlite")
})

test_that("DuckDB publica el rango de las consultas condicionales", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .plan_consultas_condicionales(con, "lupa_condicional_duckdb")
})
