# Un modelo es un conjunto de instrumentos, no de nombres.
#
# `modelo()` rechazaba solo nombres repetidos, asi que el MISMO instrumento pasaba
# dos veces si quien llama le ponia otro nombre a una instancia, y la medicion
# publicaba cada celda dos veces sin decirlo. La identidad es lo que se mide y
# como; el nombre es una etiqueta.

test_that("el mismo instrumento con otro nombre se rechaza", {
  nn <- especializar(metricas_nucleo()$NoNulo)
  uno <- instanciar(nn, "t", "edad")
  expect_error(
    modelo(uno, instanciar(nn, "t", "edad", nombre_instancia = "otra")),
    "mismo instrumento"
  )
  # Tambien con otro nombre de especializacion: sigue siendo la misma medida.
  renombrada <- especializar(metricas_nucleo()$NoNulo, nombre_especifico = "Otra")
  expect_error(modelo(uno, instanciar(renombrada, "t", "edad")),
               "mismo instrumento")
})

test_that("el control: lo que mide otra cosa no se rechaza", {
  nucleo <- metricas_nucleo()
  nn <- especializar(nucleo$NoNulo)
  uno <- instanciar(nn, "t", "edad")
  # Otra columna, otra entidad, otra configuracion.
  expect_s3_class(modelo(uno, instanciar(nn, "t", "peso")), "modelo_calidad")
  expect_s3_class(modelo(uno, instanciar(nn, "u", "edad")), "modelo_calidad")
  con_nulos <- especializar(nucleo$NoNulo, nombre_especifico = "ConNulos",
                            valores_nulos = -1)
  expect_s3_class(modelo(uno, instanciar(con_nulos, "t", "edad")),
                  "modelo_calidad")
  # Y el caso que una comparacion por TEXTO habria rechazado en falso: dos reglas
  # con el mismo cuerpo y entornos distintos miden cosas distintas.
  reglas <- lapply(1:2, function(u) {
    instanciar(
      especializar(nucleo$ReglaIntegridadIntraEntidad,
                   regla = function(x) x$a > u, nombre_especifico = paste0("R", u)),
      "t", "a"
    )
  })
  m <- modelo(reglas)
  expect_s3_class(m, "modelo_calidad")
  medicion <- medir(m, data.frame(a = c(1, 2, 3)))
  expect_equal(sum(medicion$resultado[medicion$metrica_especifica == "R1"]), 2)
  expect_equal(sum(medicion$resultado[medicion$metrica_especifica == "R2"]), 1)
})
