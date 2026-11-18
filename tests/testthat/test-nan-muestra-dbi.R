testthat::skip_on_cran()

.nan_muestra_esperar <- function(con, tabla) {
  for (bloque in c("solo_agregados", "con_muestra")) {
    perfil <- .nf_dbi(
      con, tabla, universo = "muestra_motor", muestra_motor = 100L,
      bloque_muestra = bloque
    )
    fila <- .nf_fila(perfil)
    expect_equal(fila$n_faltantes, 2)
    expect_equal(fila$n_validos, 2)
    expect_equal(fila$n_distintos, 1)
    expect_equal(suppressWarnings(as.numeric(fila$moda)), 1.5)
    expect_equal(fila$frecuencia_moda, 2)
  }
}

test_that("NaN se cuenta dentro de la misma muestra en DuckDB", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  tabla <- "nf_nan_muestra_duckdb"
  on.exit(.nf_drop(con, tabla), add = TRUE)
  .nf_crear_nan(con, tabla, c(
    "CAST('NaN' AS DOUBLE)", "1.5", "1.5", "NULL"
  ))
  .nan_muestra_esperar(con, tabla)
})

test_that("NaN se cuenta dentro de la misma muestra en PostgreSQL", {
  disponible <- FALSE
  for (puerto in c(55432L, 55493L)) {
    con <- .nf_pg(puerto)
    if (is.null(con)) next
    disponible <- TRUE
    tabla <- paste0("nf_nan_muestra_pg_", puerto)
    local({
      on.exit(.nf_drop(con, tabla), add = TRUE)
      .nf_crear_nan(con, tabla, c(
        "'NaN'::double precision", "1.5", "1.5", "NULL"
      ), tipo = "DOUBLE PRECISION")
      .nan_muestra_esperar(con, tabla)
    })
    .nf_drop(con, tabla)
    DBI::dbDisconnect(con)
  }
  if (!disponible) skip("PostgreSQL no disponible en 55432 ni 55493.")
})
