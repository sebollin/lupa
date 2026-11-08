# Cuando las sumas exactas de un decimal llegaron y no hay media o desvio, lo
# dice la cuenta, como promete la ayuda: sin valores no nulos, `sin_valores` por
# la tabla completa y `muestra_inestable:sin_valores_validos` en la muestra del
# motor; el desvio con un solo valor, `no_aplica`. Y cuando no llegaron, el
# motivo dice por que.
#
# Lo que pasaba: con `metricas = c("basicos", "desvio")`, una columna toda
# `NULL` o una tabla sin filas publicaban la media y el desvio `no_disponible`
# con "salen de las sumas exactas en el motor, que no llegaron", y el desvio de
# una sola fila, lo mismo; el motivo de un lote rechazado era generico.
testthat::skip_on_cran()

test_that("DuckDB: sin valores, con uno solo y un lote rechazado, el motivo es el verdadero", {
  con <- .dlot_duck()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  .dsv_probar(con, "dsv", "DECIMAL(18,2)")
  # El motor rechaza la consulta de los agregados que lleva las sumas de `c`:
  # el lote se parte, `c` se pide sola sin sumas, y el motivo lo dice, con el
  # mensaje del motor.
  .dlot_tabla(con, "dsv_rechazo", c(a = "DECIMAL(18,2)", c = "DECIMAL(18,2)"),
              list(a = c("1.10", "2.20"),
                   c = c("999999999999999.99", "-999999999999999.98")))
  real <- lupa:::.consultar_dbi
  testthat::local_mocked_bindings(
    .consultar_dbi = function(conexion, sql, ...) {
      if (grepl("lupa_suma_", sql, fixed = TRUE, useBytes = TRUE) &&
          grepl("TRUNC(c)", sql, fixed = TRUE, useBytes = TRUE)) {
        return(list(ok = FALSE, datos = NULL, motivo = "rechazo de prueba"))
      }
      real(conexion, sql, ...)
    },
    .package = "lupa"
  )
  perfil <- .dlot_perfil(con, "dsv_rechazo")
  leido <- .dsv_estado(perfil, "c", "media")
  expect_identical(leido$estado, "no_disponible")
  expect_true(grepl("rechaz\u00f3 la consulta de los agregados con las sumas",
                    leido$motivo, fixed = TRUE, useBytes = TRUE))
  expect_true(grepl("rechazo de prueba", leido$motivo, fixed = TRUE,
                    useBytes = TRUE))
})

test_that("PostgreSQL: sin valores y con uno solo, el motivo es el verdadero", {
  con <- .dlot_pg()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dsv_probar(con, "lupa_dsv_pg", "numeric(18,2)")
})

test_that("SQL Server: sin valores y con uno solo, el motivo es el verdadero", {
  con <- .dlot_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dsv_probar(con, "lupa_dsv_mssql", "decimal(18,2)")
})
