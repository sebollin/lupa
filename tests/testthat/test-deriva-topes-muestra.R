test_that("los topes que cambian la muestra explican la deriva de patrones", {
  testthat::skip_on_cran()
  n <- 62L
  datos <- data.frame(
    ciudad = rep("Montevideo", n),
    a = rep(1, n), b = rep(2, n), c = rep(3, n), d = rep(4, n),
    e = rep(5, n), f = rep(6, n), stringsAsFactors = FALSE
  )
  datos$ciudad[1:4] <- sprintf("%04d", 1:4)
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  base <- suppressWarnings(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE
  ))
  movido <- suppressWarnings(perfilar(
    datos, fecha = fecha + 60, max_celdas_muestra = 200,
    analizar_dependencias = FALSE
  ))

  expect_identical(as.integer(movido$meta$muestra_efectiva), 28L)
  hallazgo <- movido$hallazgos[
    movido$hallazgos$tipo_hallazgo == "patron_raro" &
      movido$hallazgos$columna == "ciudad", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  deriva <- suppressWarnings(comparar_perfiles(base, movido))
  expect_true(any(deriva$aspecto == "configuracion_patrones"))
  expect_true(any(
    deriva$aspecto == "hallazgo" & deriva$valor_actual == "patron_raro"
  ))
})
