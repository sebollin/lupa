# Un hallazgo de la tabla -columna NA- cuyo diagnostico el perfil nuevo declara
# recortado en `cobertura_diagnosticos` no se presenta como resuelto: es lo que
# promete `?comparar_perfiles` ("Si el tope de un diagnostico deja un hallazgo
# fuera del alcance nuevo, se publica `no_evaluado`"). La busqueda de fechas
# partidas corta a las 10.000 combinaciones de anio x mes x dia y lo declara con
# `columna = NA`. La comparacion armaba la clave del hallazgo con la tabla
# escrita como la cadena `"<tabla>"` y la de esa cobertura con `NA`: no casaban,
# y salia `resuelto` con severidad `ok` sobre una tabla que seguia teniendo la
# fecha partida. Las cuentas, a mano: 22 columnas por rol son 22^3 = 10.648
# combinaciones; la valida del anterior es la primera que se recorre y la del
# actual la ultima, que queda fuera. ASCII.
testthat::skip_on_cran()

.tabla_fechas_partidas_dct <- function(valida, k = 22L, n = 6L) {
  d <- list(id = seq_len(n))
  # La primera fila forma una fecha en toda combinacion y las demas no, salvo
  # en la combinacion valida: ninguna otra alcanza el 80 %.
  primera <- c(anio = 2000L, mes = 1L, dia = 1L)
  for (rol in names(primera)) {
    for (j in seq_len(k)) {
      d[[paste0(rol, "_", j)]] <- c(primera[[rol]], rep(0L, n - 1L))
    }
  }
  d <- as.data.frame(d)
  d[[paste0("anio_", valida)]] <- 2000L + seq_len(n)
  d[[paste0("mes_", valida)]] <- seq_len(n)
  d[[paste0("dia_", valida)]] <- seq_len(n) + 1L
  d
}

test_that("un hallazgo de la tabla que el perfil nuevo no volvio a buscar sale no_evaluado", {
  perfil <- function(d, fecha) suppressWarnings(suppressMessages(perfilar(
    d, fecha = as.POSIXct(fecha, tz = "UTC"), analizar_dependencias = FALSE
  )))
  anterior <- perfil(.tabla_fechas_partidas_dct(1L), "2026-01-01")
  actual <- perfil(.tabla_fechas_partidas_dct(22L), "2026-02-01")
  expect_true("fecha_partida_columnas" %in% anterior$hallazgos$tipo_hallazgo)
  expect_false("fecha_partida_columnas" %in% actual$hallazgos$tipo_hallazgo)
  recorte <- actual$cobertura_diagnosticos[
    actual$cobertura_diagnosticos$diagnostico == "fecha_partida_columnas", ,
    drop = FALSE
  ]
  expect_equal(nrow(recorte), 1L)
  expect_true(is.na(recorte$columna[[1L]]))

  deriva <- comparar_perfiles(anterior, actual)
  fila <- deriva[deriva$aspecto == "hallazgo" &
                   deriva$valor_anterior %in% "fecha_partida_columnas", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_true(is.na(fila$columna[[1L]]))
  expect_identical(as.character(fila$cambio), "no_evaluado")
  expect_identical(as.character(fila$severidad), "sospechoso")

  # Control: sin recorte -cuatro columnas-, la misma fecha partida que se
  # arregla de verdad sigue saliendo resuelta. Arreglada, solo la primera fila
  # forma una fecha: 1/6 no alcanza el 80 %.
  chica <- function(valida) {
    d <- data.frame(id = 1:6, anio = 2000L + 1:6, mes = 1:6, dia = 2:7)
    if (!valida) d$mes <- c(1L, rep(0L, 5L))
    d
  }
  resuelta <- comparar_perfiles(
    perfil(chica(TRUE), "2026-01-01"), perfil(chica(FALSE), "2026-02-01")
  )
  fila <- resuelta[resuelta$aspecto == "hallazgo" &
                     resuelta$valor_anterior %in% "fecha_partida_columnas", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$cambio), "resuelto")
  expect_identical(as.character(fila$severidad), "ok")
})
