# La guarda de numeracion -un centinela incorporado dentro de una columna que
# parece una numeracion densa no es un faltante disfrazado- decide la densidad
# con los mismos umbrales en memoria y en las consultas agregadas. Con densidad
# entre 0,5 y 0,8 la base la tomaba como densa y callaba los 999 repetidos que
# la memoria si cuenta.
testthat::skip_on_cran()
testthat::skip_if_not_installed("RSQLite")

.disfrazados_memoria_y_sql <- function(valores, con) {
  datos <- data.frame(v = as.numeric(valores))
  DBI::dbWriteTable(con, "t", datos)
  sql <- suppressWarnings(perfilar_dbi(
    con, "t", analizar_dependencias = FALSE, muestra = 1e6,
    fecha = as.Date("2026-01-01")
  ))$resumen_tabla$columnas
  mem <- perfilar(
    datos, analizar_dependencias = FALSE, fecha = as.Date("2026-01-01")
  )$columnas
  c(sql = sql$n_faltantes_disfrazados, memoria = mem$n_faltantes_disfrazados)
}

.motores_guarda <- function() {
  motores <- list(sqlite = function() DBI::dbConnect(RSQLite::SQLite(), ":memory:"))
  if (requireNamespace("duckdb", quietly = TRUE)) {
    motores$duckdb <- function() {
      DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), ":memory:")
    }
  }
  motores
}

test_that("densidad entre 0,5 y 0,8: el SQL decide igual que la memoria", {
  for (motor in names(.motores_guarda())) {
    con <- .motores_guarda()[[motor]]()
    # 1001 distintos sobre 2000 posiciones: densidad 0,5005, no es densa con
    # el umbral de la memoria, asi que el 999 repetido es un centinela.
    r <- .disfrazados_memoria_y_sql(c(1:1000, rep(999, 3), -999), con)
    DBI::dbDisconnect(con)
    expect_equal(unname(r[["memoria"]]), 5, info = motor)
    expect_equal(unname(r[["sql"]]), unname(r[["memoria"]]), info = motor)
  }
})

test_that("una numeracion realmente densa sigue protegida en el SQL", {
  for (motor in names(.motores_guarda())) {
    con <- .motores_guarda()[[motor]]()
    r <- .disfrazados_memoria_y_sql(c(1:1000, rep(999, 3)), con)
    DBI::dbDisconnect(con)
    expect_equal(unname(r[["sql"]]), unname(r[["memoria"]]), info = motor)
  }
})
