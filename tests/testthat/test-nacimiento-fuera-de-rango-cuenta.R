# `fecha_nacimiento_fuera_rango` y `fecha_fallecimiento_fuera_rango` publicaban
# `n_afectados = NA` con los datos a la vista: la regla -anteriores a 1900 o
# posteriores a la corrida- se puede contar, y el `NA` del paquete dice "no se
# puede saber".

.fecha_rango_personal <- as.POSIXct("2026-01-01", tz = "UTC")

.hallazgo_rango <- function(datos, columna, tipo) {
  p <- perfilar(datos, fecha = .fecha_rango_personal)
  h <- as.data.frame(p$hallazgos)
  h[h$columna == columna & h$tipo_hallazgo == tipo, , drop = FALSE]
}

test_that("el hallazgo cuenta las fechas de nacimiento fuera de rango", {
  x <- as.Date(c("1899-12-31", "1900-01-01", "2000-01-01", "2999-12-31"))
  # A mano: 1899-12-31 es anterior a 1900 y 2999-12-31 posterior a la corrida;
  # 1900-01-01 y 2000-01-01 no.
  esperado <- sum(x < as.Date("1900-01-01") | x > as.Date("2026-01-01"))
  expect_identical(esperado, 2L)
  h <- .hallazgo_rango(data.frame(fecha_nacimiento = x), "fecha_nacimiento",
                       "fecha_nacimiento_fuera_rango")
  expect_identical(nrow(h), 1L)
  expect_identical(as.character(h$severidad), "error")
  expect_identical(h$n_evaluados, 4)
  expect_identical(h$n_afectados, 2)
  # Y sin publicar las fechas.
  expect_false(grepl("1899|2999", h$evidencia))

  y <- as.Date(c(rep("2000-01-01", 99L), "1899-12-31"))
  h <- .hallazgo_rango(data.frame(fecha_nacimiento = y), "fecha_nacimiento",
                       "fecha_nacimiento_fuera_rango")
  expect_identical(as.character(h$severidad), "sospechoso")
  expect_identical(h$n_afectados, 1)
})

test_that("la cuenta lee el texto y la fecha-hora como el resumen", {
  texto <- c("31/12/1899", "15/03/1950", "20/07/1980", "01/01/2030",
             "02/01/2030")
  h <- .hallazgo_rango(
    data.frame(fecha_nacimiento = texto, stringsAsFactors = FALSE),
    "fecha_nacimiento", "fecha_nacimiento_fuera_rango"
  )
  expect_identical(h$n_afectados, 3)
  # Una fecha-hora se cuenta por su dia en UTC, el de `maximo_fecha`: el 1 de
  # enero de 2026 a las 23:00 no es posterior al dia de la corrida.
  instantes <- as.POSIXct(c("1850-06-01 10:00:00", "2026-01-01 23:00:00",
                            "2026-01-02 00:30:00", "1990-01-01 00:00:00"),
                          tz = "UTC")
  h <- .hallazgo_rango(data.frame(fecha_nacimiento = instantes),
                       "fecha_nacimiento", "fecha_nacimiento_fuera_rango")
  expect_identical(h$n_afectados, 2)
  h <- .hallazgo_rango(
    data.frame(fecha_fallecimiento = as.Date(c("1890-01-01", "2010-05-05",
                                               "2100-01-01"))),
    "fecha_fallecimiento", "fecha_fallecimiento_fuera_rango"
  )
  expect_identical(h$n_afectados, 2)
})

test_that("sin fechas fuera de rango no hay hallazgo", {
  h <- .hallazgo_rango(
    data.frame(fecha_nacimiento = as.Date(c("1900-01-01", "2025-12-31"))),
    "fecha_nacimiento", "fecha_nacimiento_fuera_rango"
  )
  expect_identical(nrow(h), 0L)
})
