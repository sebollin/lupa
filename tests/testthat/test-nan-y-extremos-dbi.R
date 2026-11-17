testthat::skip_on_cran()

test_that("DuckDB alinea NaN con la via en memoria", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  tabla <- "nf_nan_duckdb"
  on.exit(.nf_drop(con, tabla), add = TRUE)

  .nf_comparar_nan(
    con, tabla,
    c("CAST('NaN' AS DOUBLE)", "1.5", "1.5", "NULL"),
    c(NaN, 1.5, 1.5, NA_real_)
  )
  .nf_crear_nan(con, tabla, rep("CAST('NaN' AS DOUBLE)", 30L))
  todo <- .nf_dbi(con, tabla)
  fila <- .nf_fila(todo)
  expect_equal(fila$n_faltantes, 30)
  expect_equal(fila$n_validos, 0)
  expect_equal(fila$n_distintos, 0)
  expect_true(is.na(fila$moda))
  expect_equal(fila$frecuencia_moda, 0)
  expect_true(all(is.na(fila[c("minimo", "maximo", "media", "mediana", "desvio")])))
  expect_true(all(todo$resumen_tabla$sql$estado[
    todo$resumen_tabla$sql$columna == "x" &
      todo$resumen_tabla$sql$metrica %in% c(
        "minimo", "maximo", "media", "mediana", "desvio"
      )
  ] == "sin_valores"))
})

test_that("PostgreSQL alinea NaN con la via en memoria", {
  disponible <- FALSE
  for (puerto in c(55432L, 55493L)) {
    con <- .nf_pg(puerto)
    if (is.null(con)) next
    disponible <- TRUE
    tabla <- paste0("nf_nan_pg_", puerto)
    # Sin `try()`: en testthat 3 una comprobacion fallida es una condicion de
    # error, y `try()` la tragaba; la prueba no podia fallar nunca.
    local({
      on.exit(.nf_drop(con, tabla), add = TRUE)
      .nf_comparar_nan(
        con, tabla,
        c("'NaN'::double precision", "1.5", "1.5", "NULL"),
        c(NaN, 1.5, 1.5, NA_real_), tipo = "DOUBLE PRECISION"
      )
      .nf_crear_nan(
        con, tabla, rep("'NaN'::double precision", 30L),
        tipo = "DOUBLE PRECISION"
      )
      todo <- .nf_dbi(con, tabla)
      fila <- .nf_fila(todo)
      expect_equal(fila$n_faltantes, 30)
      expect_equal(fila$n_validos, 0)
      expect_equal(fila$n_distintos, 0)
      expect_true(is.na(fila$moda))
      expect_equal(fila$frecuencia_moda, 0)
      expect_true(all(is.na(fila[c(
        "minimo", "maximo", "media", "mediana", "desvio"
      )])))
    })
    .nf_drop(con, tabla)
    DBI::dbDisconnect(con)
  }
  if (!disponible) {
    skip("PostgreSQL no disponible en 55432 ni 55493.")
  }
})

test_that("los infinitos siguen presentes y fuera de las magnitudes", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  valores <- c(Inf, -Inf, 1.5, 1.5)
  memoria <- .nf_memoria(valores)
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  tabla <- "nf_inf_duckdb"
  on.exit(.nf_drop(con, tabla), add = TRUE)
  .nf_crear_nan(con, tabla, c(
    "CAST('Infinity' AS DOUBLE)", "CAST('-Infinity' AS DOUBLE)", "1.5", "1.5"
  ))
  dbi <- .nf_dbi(con, tabla)
  observado <- .nf_fila(dbi)
  expect_equal(observado$n_faltantes, memoria$n_faltantes)
  expect_equal(observado$n_validos, memoria$n - memoria$n_faltantes)
  expect_equal(observado$n_distintos, memoria$n_distintos)
  expect_equal(observado$moda, memoria$moda)
  expect_equal(observado$frecuencia_moda, memoria$frecuencia_moda)
  for (campo in c("minimo", "maximo", "media", "mediana", "desvio")) {
    expect_equal(observado[[campo]], memoria[[campo]], info = campo)
  }
})

.nf_con_mssql <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  if (!nzchar(clave)) return(NULL)
  tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
  ), error = function(e) NULL)
}

test_that("el desvio extremo queda calculado y finito en cada motor disponible", {
  motores <- list()
  if (requireNamespace("DBI", quietly = TRUE) &&
      requireNamespace("duckdb", quietly = TRUE)) {
    motores$duckdb <- DBI::dbConnect(
      duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:"
    )
  }
  if (requireNamespace("DBI", quietly = TRUE) &&
      requireNamespace("RSQLite", quietly = TRUE)) {
    motores$sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  }
  for (puerto in c(55432L, 55493L)) {
    con <- .nf_pg(puerto)
    if (!is.null(con)) motores[[paste0("postgres_", puerto)]] <- con
  }
  mssql <- .nf_con_mssql()
  if (!is.null(mssql)) motores$mssql <- mssql
  if (!length(motores)) skip("No hay motores DBI disponibles.")
  .nf_comprobar_extremos(motores)
})
