test_that("los datos de ejemplo son sintéticos y contienen problemas sembrados", {
  expect_s3_class(datos_administrativos, "data.frame")
  expect_equal(nrow(datos_administrativos), 13L)
  expect_equal(ncol(datos_administrativos), 9L)

  resultado <- perfilar(datos_administrativos)
  expect_true(any(resultado$hallazgos$tipo_hallazgo == "formatos_fecha_mixtos"))
  expect_true(any(resultado$hallazgos$tipo_hallazgo == "constante"))
  expect_equal(resultado$general$filas_duplicadas, 1L)
})
