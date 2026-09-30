# `EntidadContradictoria` tiene una PROPIEDAD `metodo` -la medida de distancia- y
# `instanciar()` tiene un ARGUMENTO `metodo` -la funcion que mide-. Quien pasaba
# `metodo = "lv"` al instanciar recibia "la instancia requiere un metodo de
# medicion", un mensaje sobre otra cosa.

test_that("pasar la propiedad al instanciar dice donde se fija", {
  especifica <- especializar(
    metricas_nucleo()$EntidadContradictoria, nombre_especifico = "EC",
    metodo = "lv", umbral = 0.2
  )
  expect_error(
    especifica(entidad = "t", atributos = "nombre", metodo = "lv"),
    "las propiedades se fijan al especializar", fixed = TRUE
  )
  expect_error(
    instanciar(especifica, "t", "nombre", metodo = "lv"),
    "EntidadContradictoria", fixed = TRUE
  )
  # El control: una metrica sin esa propiedad conserva el mensaje de siempre, y la
  # propiedad fijada al especializar llega a la configuracion.
  expect_error(
    instanciar(especializar(metricas_nucleo()$NoNulo), "t", "x", metodo = "lv"),
    "requiere un `metodo`", fixed = TRUE
  )
  instancia <- instanciar(especifica, "t", "nombre")
  expect_identical(instancia$configuracion$metodo, "lv")
})
