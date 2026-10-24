test_that("la validacion separada de clave no pertenece al contrato vigente", {
  argumentos <- names(formals(perfilar_dbi))

  expect_true(all(c("estrategia_distintos", "politica_costo") %in% argumentos))
  expect_false("politica_validacion_clave" %in% argumentos)
  expect_false("validar_clave" %in% argumentos)
})

test_that("procedencia y costo se publican como decisiones distintas", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "tabla_pendiente_2134", data.frame(
    id = 1:4, grupo = c("a", "a", "b", "b")
  ))

  plan <- plan_perfilado_dbi(
    con, "tabla_pendiente_2134", metricas = c("distintos", "moda"),
    estrategia_distintos = "catalogo", politica_costo = "por_cardinalidad",
    bloque_muestra = "solo_agregados"
  )

  expect_identical(
    attr(plan, "estrategia_distintos")$estrategia_solicitada,
    "catalogo"
  )
  expect_identical(attr(plan, "politica_costo")$nombre, "por_cardinalidad")
  expect_true(is.list(attr(plan, "fuente_cardinalidad_costo")))
})
