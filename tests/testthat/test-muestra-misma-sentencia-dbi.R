# Con `bloque_muestra = "solo_agregados"`, cada sentencia sobre la muestra del
# motor sortea su muestra. El desvio sale del lote de agregados, pero la sonda
# de magnitud y el repliegue escalado eran sentencias propias: si a la sonda le
# tocaba una muestra vacia el desvio se publicaba `no_aplica` ("requiere al
# menos dos valores") con un lote de cien valores, y el repliegue, que en SQL
# Server se forzaba con `metricas = "desvio"` por faltar extremos que no se
# pidieron, dejaba `no_disponible` o el desvio de otra muestra. Ahora la
# magnitud viaja en el lote y el repliegue no corre bajo la muestra.
testthat::skip_on_cran()

test_that("DuckDB: el desvio de la muestra no depende de otra sentencia", {
  con <- .dlot_duck()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  .dms_probar(con, "dms", c(id = "INTEGER", x = "DOUBLE"))
})

test_that("SQLite: el desvio de la muestra no depende de otra sentencia", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dms_probar(con, "dms", c(id = "INTEGER", x = "REAL"))
})

test_that("PostgreSQL: el desvio de la muestra no depende de otra sentencia", {
  con <- .dlot_pg()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dms_probar(con, "lupa_dms_pg", c(id = "integer", x = "double precision"))
})

test_that("SQL Server: el desvio de la muestra no depende de otra sentencia", {
  con <- .dlot_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dms_probar(con, "lupa_dms_mssql", c(id = "int", x = "float"),
              repliegue = TRUE)
})
