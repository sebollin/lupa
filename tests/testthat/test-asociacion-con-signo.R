testthat::skip_on_cran()

testthat::test_that("la asociacion numerica conserva su magnitud y agrega signo", {
  datos <- data.frame(
    creciente = 1:6,
    decreciente = 6:1,
    grupo = rep(c("A", "B"), 3),
    stringsAsFactors = FALSE
  )
  asociaciones <- detectar_asociaciones(datos, umbral = 0)
  reversa <- asociaciones[
    asociaciones$columna_1 == "creciente" &
      asociaciones$columna_2 == "decreciente", , drop = FALSE
  ]
  testthat::expect_equal(nrow(reversa), 1L)
  testthat::expect_identical(reversa$metodo, "pearson_absoluto")
  testthat::expect_identical(reversa$asociacion, 1)
  testthat::expect_identical(reversa$asociacion_con_signo, -1)

  analisis <- analizar(datos, analizar_dependencias = FALSE,
                       umbral_asociacion = 0)
  testthat::expect_true("asociacion_con_signo" %in% names(analisis$asociaciones))
  testthat::expect_equal(
    analisis$asociaciones$asociacion_con_signo[
      analisis$asociaciones$columna_1 == "creciente" &
        analisis$asociaciones$columna_2 == "decreciente"
    ], -1
  )
  archivo <- tempfile(fileext = ".html")
  reportar(analisis, archivo = archivo)
  html <- paste(readLines(archivo, encoding = "UTF-8", warn = FALSE),
                collapse = "\n")
  testthat::expect_match(html, "asociacion_con_signo", fixed = TRUE)
})
