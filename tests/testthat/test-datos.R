test_that("los datos de ejemplo son sintéticos y contienen problemas sembrados", {
  expect_s3_class(datos_administrativos, "data.frame")
  expect_equal(nrow(datos_administrativos), 13L)
  expect_equal(ncol(datos_administrativos), 10L)

  resultado <- perfilar(datos_administrativos)
  expect_true(any(resultado$hallazgos$tipo_hallazgo == "formatos_fecha_mixtos"))
  expect_true(any(resultado$hallazgos$tipo_hallazgo == "constante"))
  expect_true(any(resultado$hallazgos$severidad == "ok"))
  expect_equal(resultado$general$filas_duplicadas, 1L)
})

test_that("el segundo conjunto de ejemplo es neutral y reproduce el recorrido", {
  expect_s3_class(datos_operativos, "data.frame")
  expect_equal(dim(datos_operativos), c(13L, 10L))
  expect_false(any(c(
    "cedula", "departamento", "pais", "rut", "dni"
  ) %in% names(datos_operativos)))

  resultado <- perfilar(datos_operativos)
  expect_true(any(resultado$hallazgos$tipo_hallazgo == "formatos_fecha_mixtos"))
  expect_true(any(resultado$hallazgos$tipo_hallazgo == "constante"))
  expect_true(any(resultado$hallazgos$tipo_hallazgo == "filas_duplicadas"))
  expect_true(any(resultado$hallazgos$tipo_hallazgo == "columnas_duplicadas"))
  expect_equal(resultado$general$filas_duplicadas, 1L)
  expect_true(identical(datos_operativos$id_registro, datos_operativos$id_copia))

  analisis <- analizar(
    datos_operativos, analizar_dependencias = FALSE,
    max_columnas_asociacion = 3, max_pares_asociacion = 3,
    max_columnas_temporales = 2
  )
  expect_s3_class(analisis, "analisis")
  expect_equal(analisis$perfil$general$filas, nrow(datos_operativos))
})
