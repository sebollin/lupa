testthat::skip_on_cran()

# Un centinela declarado vale aunque no aparezca en la columna, pero la evidencia
# del hallazgo solo puede nombrar los que se observaron.
testthat::test_that("la evidencia de un centinela declarado nombra solo los observados", {
  declarada <- data.frame(
    fecha_inicio = as.Date(c(
      "1900-01-01", "1900-01-01", "2020-01-01", "2020-01-02",
      "2020-01-03"
    ))
  )
  p <- suppressWarnings(perfilar(
    declarada, fecha = as.Date("2026-11-19"), analizar_dependencias = FALSE,
    dominios = list(fecha_inicio = list(
      centinelas = c("1900-01-01", "9999-12-31", "1970-01-01")
    ))
  ))
  h <- p$hallazgos[p$hallazgos$tipo_hallazgo == "fecha_centinela", ,
                   drop = FALSE]
  testthat::expect_equal(nrow(h), 1L)
  testthat::expect_equal(h$n_afectados, 2L)
  testthat::expect_true(grepl(
    "1900-01-01", h$evidencia, fixed = TRUE, useBytes = TRUE
  ))
  testthat::expect_false(grepl(
    "9999-12-31", h$evidencia, fixed = TRUE, useBytes = TRUE
  ))
  testthat::expect_false(grepl(
    "1970-01-01", h$evidencia, fixed = TRUE, useBytes = TRUE
  ))
})
