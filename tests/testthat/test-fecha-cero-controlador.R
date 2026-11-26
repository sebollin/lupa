# RMariaDB lee la fecha cero de MySQL y MariaDB como una fecha del ano -1.
# Sin servidor: lo que el paquete hace con lo que el controlador entrega.
testthat::skip_on_cran()

.fcc_conexion <- function(clase) structure(list(), class = clase)

test_that("una columna Date con fecha cero se entrega como texto con la fecha cero escrita", {
  fechas <- as.Date("2024-01-01") + 0:3
  # El valor que RMariaDB da para `0000-00-00`.
  cero <- structure(-719530, class = "Date")
  datos <- data.frame(id = 1:5)
  datos$f <- c(cero, fechas[1:3], NA)
  salida <- lupa:::.fecha_cero_del_controlador_dbi(.fcc_conexion("MariaDBConnection"), datos)
  expect_identical(salida$f, c("0000-00-00", "2024-01-01", "2024-01-02", "2024-01-03", NA))
  expect_identical(salida$id, 1:5)
})

test_that("un instante con fecha cero se entrega como texto, con y sin fracciones", {
  cero <- structure(-62135596800 - 86400, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  normal <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC")
  datos <- data.frame(id = 1:2)
  datos$t <- c(cero, normal)
  salida <- lupa:::.fecha_cero_del_controlador_dbi(.fcc_conexion("MariaDBConnection"), datos)
  expect_identical(salida$t, c("0000-00-00 00:00:00", "2024-01-01 10:00:00"))
  datos$t <- c(cero, normal + 0.5)
  salida <- lupa:::.fecha_cero_del_controlador_dbi(.fcc_conexion("MariaDBConnection"), datos)
  expect_identical(salida$t, c("0000-00-00 00:00:00", "2024-01-01 10:00:00.500000"))
})

test_that("sin fecha cero, o en otro motor, la tabla pasa tal cual", {
  datos <- data.frame(id = 1:3)
  datos$f <- as.Date("2024-01-01") + 0:2
  datos$t <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + 0:2
  expect_identical(
    lupa:::.fecha_cero_del_controlador_dbi(.fcc_conexion("MariaDBConnection"), datos), datos
  )
  con_cero <- datos
  con_cero$f[2L] <- structure(-719530, class = "Date")
  expect_identical(
    lupa:::.fecha_cero_del_controlador_dbi(.fcc_conexion("SQLiteConnection"), con_cero),
    con_cero
  )
  expect_identical(lupa:::.fecha_cero_del_controlador_dbi(.fcc_conexion("MariaDBConnection"), data.frame()),
                   data.frame())
})

test_that("una lectura por tandas que trae la fecha cero lo dice con su causa", {
  cero <- structure(-719530, class = "Date")
  datos <- data.frame(id = 1:2)
  datos$f <- c(cero, as.Date("2024-01-01"))
  lectura <- lupa:::.vigilar_fecha_cero_dbi(
    datos, .fcc_conexion("MariaDBConnection")
  )
  expect_identical(lectura$fecha_cero, "f")
  expect_match(lupa:::.motivo_lectura_dbi(lectura, "bloque"), "fechas cero", fixed = TRUE)
  expect_match(lupa:::.motivo_lectura_dbi(lectura, "bloque"), "`f`", fixed = TRUE)
})
