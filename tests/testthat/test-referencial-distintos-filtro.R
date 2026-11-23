# La via que compara en R los valores distintos de una metrica referencial
# (`estrategia_no_sql = "bloques"`) cuenta los grupos en el motor. Con una
# `aplicabilidad`, los grupos suman solo las filas que aplican, no la tabla: la
# lectura de esa sentencia registra las filas de la tabla, que trae en la misma
# sentencia. Con la suma de los grupos, la tabla parecia tener menos filas en
# una lectura que en la siguiente y la cifra dejaba de cerrar sin que nada
# hubiera cambiado.

.dfil_fecha <- as.POSIXct("2026-06-30", tz = "UTC")

.dfil_datos <- function() {
  data.frame(k = c(1, 2, 3, 9, NA, 1, 2, 3), activo = c(1, 1, 1, 1, 1, 0, 0, 0))
}

.dfil_modelo <- function() {
  padron <- referencial(data.frame(k = 1:3, v = c("a", "b", "c")), clave = "k",
                        valor = "v", completo = TRUE, alcance = "e",
                        nombre = "padron")
  modelo(instanciar(
    especializar(metricas_referencial()$CorrectitudSemFuerte, "Existe"),
    "d", "k", referencial = padron
  ))
}

.dfil_con <- function(motor) {
  con <- if (identical(motor, "duckdb")) {
    DBI::dbConnect(duckdb::duckdb())
  } else {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  }
  DBI::dbWriteTable(con, "d", .dfil_datos())
  con
}

.dfil_cerrar <- function(con, motor) {
  if (identical(motor, "duckdb")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
}

.dfil_motores <- function() {
  c("sqlite", if (requireNamespace("duckdb", quietly = TRUE)) "duckdb")
}

test_that("con aplicabilidad, los valores distintos registran las filas de la tabla", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  aplica <- list(k = ~ activo == 1)
  en_memoria <- medir(.dfil_modelo(), list(d = .dfil_datos()), id_medicion = "j",
                      fecha = .dfil_fecha, aplicabilidad = aplica,
                      detalle = "resumen")
  for (motor in .dfil_motores()) {
    con <- .dfil_con(motor)
    m <- medir_dbi(.dfil_modelo(), con, id_medicion = "j", fecha = .dfil_fecha,
                   aplicabilidad = aplica, estrategia_no_sql = "bloques")
    .dfil_cerrar(con, motor)
    expect_identical(unname(attr(m, "ejecucion_dbi")$vias), "distintos_r",
                     label = motor)
    # Las dos lecturas de `d` vieron sus ocho filas, no las cinco que aplican.
    l <- attr(m, "lecturas", exact = TRUE)
    expect_identical(l$n_filas[l$entidad == "d"], c(8, 8), label = motor)
    expect_identical(m$n_filas_leidas, 8, label = motor)
    expect_identical(m$estado_cuentas, "cierran", label = motor)
    # Y la cifra es la de memoria sobre las filas que aplican.
    for (columna in c("resultado", "numerador", "denominador", "n_universo",
                      "n_filas_leidas", "estado_cuentas")) {
      expect_identical(m[[columna]], en_memoria[[columna]],
                       label = paste(motor, columna))
    }
  }
})

test_that("el control: sin aplicabilidad los grupos suman la tabla", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  for (motor in .dfil_motores()) {
    con <- .dfil_con(motor)
    m <- medir_dbi(.dfil_modelo(), con, id_medicion = "j", fecha = .dfil_fecha,
                   estrategia_no_sql = "bloques")
    .dfil_cerrar(con, motor)
    l <- attr(m, "lecturas", exact = TRUE)
    expect_identical(l$n_filas[l$entidad == "d"], c(8, 8), label = motor)
    expect_identical(m$estado_cuentas, "cierran", label = motor)
  }
})
