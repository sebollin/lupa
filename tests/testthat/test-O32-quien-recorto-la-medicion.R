# O32: el resumen de la coleccion atribuia al motor un recorte que podia venir
# del presupuesto declarado por quien llama. Con `max_consultas = 3` ninguna
# consulta la rechaza el motor: se emiten exactamente las tres permitidas.

test_that("el resumen no le imputa al motor un recorte del presupuesto", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(id = seq_len(20L)))

  perfilado <- suppressWarnings(
    perfilar_coleccion(coleccion(con, "t"), max_consultas = 3)
  )
  cobertura <- as.data.frame(perfilado$cobertura_coleccion)
  fila <- cobertura[cobertura$alcance == "metricas", , drop = FALSE]

  expect_equal(nrow(fila), 1L)
  expect_false(grepl("El motor rechazo", fila$motivo[[1L]], fixed = TRUE))
  expect_match(fila$motivo[[1L]], "No se calcularon", fixed = TRUE)
  expect_match(fila$como_resolverlo[[1L]], "presupuesto declarado",
               fixed = TRUE)

  # Y el motivo real sigue estando, por columna y metrica.
  metricas <- as.data.frame(perfilado$cobertura_metricas)
  expect_true(any(grepl("max_consultas", metricas$motivo, fixed = TRUE)))

  salida <- capture.output(print(perfilado), type = "message")
  expect_false(any(grepl("rechazados por el motor", salida, fixed = TRUE)))
  expect_true(any(grepl("agregados no calculados", salida, fixed = TRUE)))
})

test_that("sin recorte no aparece la fila de agregados", {
  # Control: la fila existe por el recorte, no por perfilar una coleccion.
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(id = seq_len(20L)))

  perfilado <- perfilar_coleccion(coleccion(con, "t"))
  cobertura <- as.data.frame(perfilado$cobertura_coleccion)

  expect_equal(sum(cobertura$alcance == "metricas"), 0L)
})
