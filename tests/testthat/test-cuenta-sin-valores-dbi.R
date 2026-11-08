# Sin `validos`, la media, el desvio y los extremos de un doble o un entero no
# sabian cuantos valores vieron: una columna toda `NULL` daba la media
# `no_disponible` con "La consulta termino sin un valor para la metrica" y el
# desvio `no_aplica`, y el desvio de una sola fila, `no_disponible` con "El
# motor no devolvio un desvio finito", donde la ayuda promete `sin_valores` y
# `no_aplica`. La cuenta de los no nulos va ahora en el mismo lote de agregados
# y decide como la de las sumas exactas de un decimal. Y una tabla sin filas por
# el spool publicaba ceros como `observado_muestra` y lo demas con
# `muestra_inestable:sin_valores_validos`, donde la ayuda pide
# `muestra_vacia:<metodo>`.
testthat::skip_on_cran()

.dcv_tipos <- list(
  duck = c(id = "INTEGER", x = "DOUBLE", e = "INTEGER"),
  sqlite = c(id = "INTEGER", x = "REAL", e = "INTEGER"),
  pg = c(id = "integer", x = "double precision", e = "integer"),
  mssql = c(id = "int", x = "float", e = "int")
)

test_that("DuckDB: sin valores o con uno solo, sin validos, decide la cuenta", {
  con <- .dlot_duck()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  .dcv_probar(con, "dcv", .dcv_tipos$duck)
  .dcv_spool_vacia(con, "dcv", .dcv_tipos$duck)
})

test_that("SQLite: sin valores o con uno solo, sin validos, decide la cuenta", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dcv_probar(con, "dcv", .dcv_tipos$sqlite)
  .dcv_spool_vacia(con, "dcv", .dcv_tipos$sqlite)
})

test_that("PostgreSQL: sin valores o con uno solo, sin validos, decide la cuenta", {
  con <- .dlot_pg()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dcv_probar(con, "lupa_dcv_pg", .dcv_tipos$pg)
  .dcv_spool_vacia(con, "lupa_dcv_pg", .dcv_tipos$pg)
})

test_that("SQL Server: sin valores o con uno solo, sin validos, decide la cuenta", {
  con <- .dlot_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dcv_probar(con, "lupa_dcv_mssql", .dcv_tipos$mssql)
  .dcv_spool_vacia(con, "lupa_dcv_mssql", .dcv_tipos$mssql)
})
