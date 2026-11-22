# El caso que el control de lecturas no puede ver sin un contador del motor: un
# UPDATE entre dos lecturas con el mismo COUNT(*). La celda puede tener color,
# pero el semaforo dice que su control fue limitado.

.scl_fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")

.scl_modelo <- function() {
  nuc <- metricas_nucleo()
  modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "x"),
         instanciar(especializar(nuc$ValoresPosiblesPorComprension, "Rng",
                                 minimo = 0, maximo = 3), "d", "x"))
}

.scl_cortes <- list(Completitud = c(0.5, 0.9), Consistencia = c(0.5, 0.7))

test_that("un UPDATE entre lecturas con el mismo conteo: color con control limitado, y lo dice", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  original <- lupa:::.traer_consulta_dbi
  hecho <- FALSE
  local_mocked_bindings(.traer_consulta_dbi = function(conexion, sql, ...) {
    r <- original(conexion, sql, ...)
    if (!hecho && grepl("AS n_filas", sql, fixed = TRUE)) {
      hecho <<- TRUE
      DBI::dbExecute(conexion, "UPDATE d SET x = 9 WHERE x = 1")
    }
    r
  })
  for (motor in c("sqlite", "duckdb")) {
    hecho <- FALSE
    con <- if (identical(motor, "sqlite")) {
      DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    } else {
      DBI::dbConnect(duckdb::duckdb(), ":memory:")
    }
    DBI::dbWriteTable(con, "d", data.frame(x = c(1, 2, 3, 2)))
    md <- medir_dbi(.scl_modelo(), con, id_medicion = "p", fecha = .scl_fecha,
                    tamano_lote = 1L)
    if (identical(motor, "duckdb")) DBI::dbDisconnect(con, shutdown = TRUE) else DBI::dbDisconnect(con)
    # Las dos cifras cierran por conteo y vienen de dos estados de la tabla.
    expect_identical(md$estado_cuentas, c("cierran", "cierran"), info = motor)
    expect_identical(md$resultado, c(1, 0.75), info = motor)
    s <- semaforo_calidad(md, cortes = .scl_cortes)
    expect_false(anyNA(s$nivel), info = motor)
    expect_identical(s$control_lecturas, c("limitado", "limitado"), info = motor)
    utils::capture.output(lineas <- cli::cli_fmt(print(s)))
    texto <- paste(lineas, collapse = " ")
    expect_true(grepl("2 celdas con color tienen un control de lecturas limitado", texto,
                      fixed = TRUE), info = motor)
    expect_true(grepl("UPDATE", texto, fixed = TRUE), info = motor)
  }
})

test_that("una sola lectura, en memoria o en una sentencia, es una_lectura; dos con huella, completo", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  d <- data.frame(x = c(1, 2, 3, 2))
  mr <- medir(.scl_modelo(), list(d = d), id_medicion = "p", fecha = .scl_fecha,
              detalle = "resumen")
  s <- semaforo_calidad(mr, cortes = .scl_cortes)
  expect_identical(s$control_lecturas, c("una_lectura", "una_lectura"))
  utils::capture.output(lineas <- cli::cli_fmt(print(s)))
  texto <- paste(lineas, collapse = " ")
  expect_false(grepl("control de lecturas limitado", texto, fixed = TRUE))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "d", d)
  md <- medir_dbi(.scl_modelo(), con, id_medicion = "p", fecha = .scl_fecha)
  expect_identical(semaforo_calidad(md, cortes = .scl_cortes)$control_lecturas,
                   c("una_lectura", "una_lectura"))
  mod <- .scl_modelo()
  a <- medir(modelo(mod$metricas[1]), list(d = d), id_medicion = "p", fecha = .scl_fecha,
             detalle = "resumen")
  b <- medir(modelo(mod$metricas[2]), list(d = d), id_medicion = "p", fecha = .scl_fecha,
             detalle = "resumen")
  expect_identical(semaforo_calidad(rbind(a, b), cortes = .scl_cortes)$control_lecturas,
                   c("completo", "completo"))
})
