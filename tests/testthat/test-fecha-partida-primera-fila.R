# La busqueda de fechas partidas en anio, mes y dia arma `"%04d-%02d-%02d"` y lo
# convierte con `as.Date()`. Sin el formato dicho, `as.Date()` lo adivina por el
# PRIMER elemento y, si ese no es una fecha, aborta: un mes `0` -el codigo
# habitual de "mes desconocido"-, un 30 de febrero o un `NA` arriba tiraban
# `perfilar()` y `analizar()` enteros, y `perfilar_dbi()` perdia la muestra.
# Las cuentas, a mano: once filas; en la primera la fecha no existe y las otras
# diez si, 10/11 = 0,909 >= 0,8, asi que la tabla publica la fecha partida.
testthat::skip_on_cran()

.fecha_partida_pfp <- as.POSIXct("2026-01-01", tz = "UTC")

test_that("una primera fila que no es fecha no aborta el perfil y la fecha partida se publica", {
  casos <- list(
    mes_cero = data.frame(id = 1:11, anio = 2020:2030, mes = c(0L, 2:11), dia = 1:11),
    treinta_de_febrero = data.frame(
      id = 1:11, anio = 2020:2030, mes = c(2L, 2:11), dia = c(30L, 2:11)
    ),
    ingles_treinta_y_uno_de_abril = data.frame(
      id = 1:11, year = 2020:2030, month = c(4L, 2:11), day = c(31L, 2:11)
    )
  )
  for (nombre in names(casos)) {
    p <- perfilar(casos[[nombre]], fecha = .fecha_partida_pfp)
    h <- p$hallazgos[p$hallazgos$tipo_hallazgo == "fecha_partida_columnas", , drop = FALSE]
    expect_equal(nrow(h), 1L, info = nombre)
    expect_true(is.na(h$columna[[1L]]), info = nombre)
    expect_equal(h$n_afectados[[1L]], 3, info = nombre)
    expect_true(grepl("0.909", h$evidencia[[1L]], fixed = TRUE, useBytes = TRUE),
                info = nombre)
  }
  # Un NA en la primera fila: `sprintf()` lo escribe `  NA`. La proporcion se
  # mide sobre las filas con las tres partes, diez de diez.
  d <- data.frame(id = 1:11, anio = c(NA, 2021:2030), mes = 1:11, dia = 1:11)
  p <- perfilar(d, fecha = .fecha_partida_pfp)
  h <- p$hallazgos[p$hallazgos$tipo_hallazgo == "fecha_partida_columnas", , drop = FALSE]
  expect_equal(nrow(h), 1L)
  expect_true(grepl("1.000", h$evidencia[[1L]], fixed = TRUE, useBytes = TRUE))
  a <- analizar(d, fecha = .fecha_partida_pfp, analizar_dependencias = FALSE)
  expect_s3_class(a, "analisis")
})

test_that("la muestra de perfilar_dbi no se pierde por una primera fila que no es fecha", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "nacimientos", data.frame(
    id = 1:11, nac_anio = 1980:1990, nac_mes = c(0L, 2:11), nac_dia = 1:11
  ))
  r <- expect_no_warning(perfilar_dbi(
    con, "nacimientos", fecha = .fecha_partida_pfp,
    proteger_datos_personales = FALSE
  ))
  expect_s3_class(r$perfil_muestra, "perfil")
  expect_true("fecha_partida_columnas" %in% r$perfil_muestra$hallazgos$tipo_hallazgo)
})
