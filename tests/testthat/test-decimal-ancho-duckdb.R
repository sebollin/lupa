test_that("DuckDB publica la moda decimal con texto exacto", {
  skip_on_cran()
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  conexion <- DBI::dbConnect(
    duckdb::duckdb(shared_home = TRUE), dbdir = ":memory:"
  )
  on.exit(try(duckdb::duckdb_shutdown(conexion), silent = TRUE), add = TRUE)
  DBI::dbExecute(conexion, paste0(
    "CREATE TABLE a2561_decimal_duck (d DECIMAL(38,0))"
  ))
  DBI::dbExecute(conexion, paste0(
    "INSERT INTO a2561_decimal_duck VALUES ",
    "(12345678901234567890), (12345678901234567890),",
    "(9007199254740993), (NULL)"
  ))
  resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, "a2561_decimal_duck", universo = "tabla_completa",
    bloque_muestra = "solo_agregados",
    metricas = c("moda", "basicos", "desvio", "mediana"),
    proteger_datos_personales = FALSE
  )))
  fila <- resultado$resumen_tabla$columnas
  fila <- fila[fila$columna == "d", , drop = FALSE]
  expect_identical(as.character(fila$moda), "12345678901234567890")
  expect_equal(as.numeric(fila$frecuencia_moda), 2)
  sql <- resultado$resumen_tabla$sql
  magnitudes <- sql[
    sql$columna == "d" & sql$metrica %in%
      c("minimo", "maximo", "mediana", "media", "desvio"),
    , drop = FALSE
  ]
  expect_true(all(magnitudes$estado == "no_disponible"))
  expect_false(any(magnitudes$estado == "calculado"))
  expect_true(all(grepl("2^53", magnitudes$motivo, fixed = TRUE)))
})
