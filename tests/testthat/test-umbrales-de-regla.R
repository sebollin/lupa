# Un umbral pertenece a una regla, no a una métrica. Antes quedaba encerrado en
# el *closure* de `condicion`: para mover un número había que escribir otra
# regla, y nadie podía consultar cuál era.
#
# `umbrales` los deja afuera, y `propiedades_regla()` los muestra.

test_that("la misma condición evalúa distinto con dos umbrales", {
  condicion <- function(x, minimo) x >= minimo
  valores <- c(0.4, 0.6, 0.95)

  laxa <- regla_evaluacion("cobertura", condicion, umbrales = list(minimo = 0.5))
  estricta <- regla_evaluacion("cobertura", condicion, umbrales = list(minimo = 0.9))

  # Es literalmente la misma función: no se reconstruyó la lógica.
  expect_identical(laxa$condicion, estricta$condicion)

  resultado_laxo <- lupa:::.aplicar_condicion_regla(
    laxa$condicion, valores, "conformidad", laxa$umbrales
  )
  resultado_estricto <- lupa:::.aplicar_condicion_regla(
    estricta$condicion, valores, "conformidad", estricta$umbrales
  )
  expect_equal(resultado_laxo, c(FALSE, TRUE, TRUE))
  expect_equal(resultado_estricto, c(FALSE, FALSE, TRUE))
})

test_that("propiedades_regla() muestra los umbrales y el nivel", {
  regla <- regla_evaluacion(
    "cobertura minima", function(x, minimo) x >= minimo,
    metricas = "NoNulo", umbrales = list(minimo = 0.9)
  )
  propiedades <- propiedades_regla(regla)

  expect_true(is.data.frame(propiedades))
  expect_equal(names(propiedades), c("propiedad", "valor"))
  expect_true("umbral:minimo" %in% propiedades$propiedad)
  expect_equal(
    propiedades$valor[propiedades$propiedad == "umbral:minimo"], "0.9"
  )
  expect_equal(propiedades$valor[propiedades$propiedad == "nivel"], "medida")
  expect_equal(propiedades$valor[propiedades$propiedad == "metricas"], "NoNulo")
})

test_that("una regla agregada declara su nivel y su proporción", {
  regla <- regla_evaluacion(
    "el 70 por ciento pasa", function(x) x > 0, proporcion_minima = 0.7
  )
  propiedades <- propiedades_regla(regla)
  expect_equal(propiedades$valor[propiedades$propiedad == "nivel"], "agregado")
  expect_equal(
    propiedades$valor[propiedades$propiedad == "proporcion_minima"], "0.7"
  )
})

test_that("un umbral que la condición no recibe se rechaza diciendo qué acepta", {
  expect_error(
    regla_evaluacion("x", function(x) x > 0, umbrales = list(minimo = 0.9)),
    "no recibe estos umbrales"
  )
  expect_error(
    regla_evaluacion("x", function(x) x > 0, umbrales = list(minimo = 0.9)),
    "Sus argumentos son"
  )
  # Una condición con `...` los acepta todos.
  expect_silent(
    regla_evaluacion("x", function(x, ...) x > 0, umbrales = list(minimo = 0.9))
  )
})

test_that("umbrales mal formados se rechazan", {
  condicion <- function(x, minimo) x >= minimo
  expect_error(regla_evaluacion("x", condicion, umbrales = 0.9), "una lista")
  expect_error(
    regla_evaluacion("x", condicion, umbrales = list(0.9)),
    "nombres unicos"
  )
})

test_that("propiedades_regla() rechaza lo que no es una regla", {
  expect_error(propiedades_regla(list(nombre = "x")), "regla_evaluacion")
})
