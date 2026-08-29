# El plan no puede prometer métricas de una muestra cuya consulta no se puede
# construir. SQLite acepta las sondas de capacidad, pero con `muestra = Inf` no
# existe un subconjunto muestreado que escribir.

test_that("el plan declara una forma muestreada no construible", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "tabla166", data.frame(
    id = seq_len(200L),
    numero = seq_len(200L),
    indicador = rep(c(0, 1), 100L),
    vacia = rep(NA_real_, 200L)
  ))

  plan <- plan_perfilado_dbi(
    conexion, "tabla166", modo = "muestreado", muestra = Inf
  )
  resultado <- suppressWarnings(perfilar_dbi(
    conexion, "tabla166", modo = "muestreado", muestra = Inf,
    instrumentar = FALSE, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  ))
  estado <- attr(plan, "muestreo", exact = TRUE)
  sql <- resultado$resumen_tabla$sql

  expect_equal(estado$estado, "no_disponible")
  expect_false(estado$disponible)
  expect_false(estado$forma_construible)
  expect_match(estado$motivo, "no pudo construir una consulta", fixed = TRUE)
  expect_match(
    attr(plan, "supuesto", exact = TRUE),
    "La forma muestreada no se puede construir",
    fixed = TRUE
  )
  expect_length(attr(plan, "metricas_ejecucion", exact = TRUE), 0L)
  expect_equal(attr(plan, "total"), attr(plan, "total_maximo"))
  expect_equal(
    resultado$resumen_tabla$meta$consultas$emitidas,
    attr(plan, "total")
  )
  expect_equal(sum(sql$estado == "no_disponible"), 56L)
  expect_true(all(is.na(sql$sql[sql$estado == "no_disponible"])))
})
