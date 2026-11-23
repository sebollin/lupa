# Si la base es un archivo, la tabla de cada lectura lleva la ruta del archivo:
# un `DETACH` y un `ATTACH` de otra base con el mismo alias entre dos llamadas
# son dos tablas, no una que cambio. Unidas, el rotulo da dos tablas y el
# control de sus cifras es `desconocido`. En memoria, como antes, cuenta el
# nombre.

.arch_fecha <- as.POSIXct("2026-06-30", tz = "UTC")

.arch_medir <- function(con, nombre) {
  medir_dbi(
    modelo(instanciar(especializar(metricas_nucleo()$NoNulo, nombre), "Adjunta", "x")),
    con, tablas = list(Adjunta = DBI::Id(schema = "otra", table = "ventas")),
    id_medicion = "j", fecha = .arch_fecha
  )
}

.arch_crear <- function(con, archivo, filas, motor) {
  ruta <- as.character(DBI::dbQuoteString(con, archivo))
  DBI::dbExecute(con, paste0("ATTACH ", ruta, " AS otra"))
  DBI::dbExecute(con, "CREATE TABLE otra.ventas (x INTEGER)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO otra.ventas VALUES ",
    paste0("(", seq_len(filas), ")", collapse = ", ")
  ))
}

.arch_con <- function(motor) {
  if (identical(motor, "duckdb")) {
    DBI::dbConnect(duckdb::duckdb())
  } else {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  }
}

.arch_cerrar <- function(con, motor) {
  if (identical(motor, "duckdb")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
}

.arch_motores <- function() {
  c("sqlite", if (requireNamespace("duckdb", quietly = TRUE)) "duckdb")
}

test_that("otra base con el mismo alias es otra tabla", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  for (motor in .arch_motores()) {
    archivos <- c(tempfile(fileext = ".db"), tempfile(fileext = ".db"))
    con <- .arch_con(motor)
    .arch_crear(con, archivos[[1L]], 4L, motor)
    a <- .arch_medir(con, "Antes")
    DBI::dbExecute(con, "DETACH otra")
    .arch_crear(con, archivos[[2L]], 7L, motor)
    b <- .arch_medir(con, "Despues")
    .arch_cerrar(con, motor)
    unlink(c(archivos, paste0(archivos, ".wal")))
    u <- rbind(a, b)
    l <- attr(u, "lecturas", exact = TRUE)
    expect_identical(length(unique(l$tabla)), 2L, label = motor)
    for (k in seq_along(archivos)) {
      ruta <- normalizePath(archivos[[k]], winslash = "/", mustWork = FALSE)
      expect_true(any(grepl(ruta, l$tabla, fixed = TRUE, useBytes = TRUE)),
                  label = paste(motor, k))
    }
    # Cuatro filas en una y siete en la otra: no es un cambio entre lecturas.
    expect_identical(u$estado_cuentas, c("cierran", "cierran"), label = motor)
    # El rotulo `Adjunta` da dos tablas en la union: no se sabe cual leyo cada
    # fila, y el control lo declara.
    s <- semaforo_calidad(u, cortes = list(Completitud = c(0.5, 0.9)))
    expect_identical(s$control_lecturas, c("desconocido", "desconocido"),
                     label = motor)
  }
})

test_that("el control: la misma base adjunta otra vez es la misma tabla", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  for (motor in .arch_motores()) {
    archivo <- tempfile(fileext = ".db")
    con <- .arch_con(motor)
    .arch_crear(con, archivo, 4L, motor)
    a <- .arch_medir(con, "Antes")
    DBI::dbExecute(con, "DETACH otra")
    ruta <- as.character(DBI::dbQuoteString(con, archivo))
    DBI::dbExecute(con, paste0("ATTACH ", ruta, " AS otra"))
    DBI::dbExecute(con, "INSERT INTO otra.ventas VALUES (5)")
    b <- .arch_medir(con, "Despues")
    .arch_cerrar(con, motor)
    unlink(c(archivo, paste0(archivo, ".wal")))
    u <- suppressWarnings(rbind(a, b))
    expect_identical(length(unique(attr(u, "lecturas", exact = TRUE)$tabla)), 1L,
                     label = motor)
    expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"), label = motor)
  }
})
