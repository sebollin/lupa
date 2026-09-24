test_that("un rango de muestra no poda un solape del universo", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t1", data.frame(id = 501:1500))
  DBI::dbWriteTable(con, "t2", data.frame(id = 1:1000))

  resultado <- relaciones_coleccion(
    coleccion(con, c("t1", "t2"), nombre = "solape"),
    data.frame(tabla_1 = "t1", tabla_2 = "t2"),
    muestra = 500, orden = "id"
  )

  # La consulta de universo ve rangos que se solapan; no hay poda cierta.
  expect_equal(resultado$meta$consultas_rangos, 2L)
  expect_false(any(resultado$cobertura_podas$motivo == "rangos_disjuntos"))
  expect_false(any(
    resultado$relaciones$cardinalidad == "sin_coincidencias" &
      resultado$relaciones$n_valores_comunes == 0L
  ))
  # El par fue comparado; no se lo publico como ausencia de relacion podada.
  expect_equal(nrow(resultado$cobertura_pares), 0L)
})

test_that("rangos disjuntos del universo conservan la poda cierta", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "a", data.frame(id = 501:1500))
  DBI::dbWriteTable(con, "b", data.frame(id = 1:500))

  resultado <- relaciones_coleccion(
    coleccion(con, c("a", "b"), nombre = "disjuntos"),
    data.frame(tabla_1 = "a", tabla_2 = "b"),
    muestra = 100, orden = "id", umbral_cobertura = 0
  )

  expect_equal(resultado$cobertura_podas$motivo, "rangos_disjuntos")
  expect_equal(resultado$relaciones$cardinalidad, "sin_coincidencias")
  expect_equal(resultado$relaciones$n_valores_comunes, 0L)
})

test_that("la via en memoria conserva el rango de la columna completa", {
  t1 <- data.frame(id = 501:1500)
  t2 <- data.frame(id = 1:1000)

  resultado <- detectar_relaciones(t1, t2, muestra = 500)

  expect_equal(resultado$cardinalidad, "1:1")
  expect_equal(resultado$n_valores_comunes, 500L)
  expect_equal(resultado$cobertura_tabla1_en_tabla2, 0.5)
  expect_equal(resultado$cobertura_tabla2_en_tabla1, 0.5)
})
