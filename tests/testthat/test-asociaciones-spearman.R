# Pearson mide asociación lineal. Una relación creciente pero curva la
# subestima, y quien lea el número puede concluir que las columnas están menos
# relacionadas de lo que están. Spearman mide asociación monótona sobre los
# rangos y no supone linealidad.
#
# El método elegido viaja en la columna `metodo` y su supuesto en `supuesto`,
# así que ninguna lectura depende de recordar cuál se pidió.

test_that("Spearman reconoce una relación monótona que Pearson subestima", {
  set.seed(7)
  x <- 1:60
  datos <- data.frame(x = x, curva = x^3 + stats::rnorm(60L, 0, 500))

  pearson <- detectar_asociaciones(datos, umbral = 0)
  spearman <- detectar_asociaciones(datos, umbral = 0, metodo_numerico = "spearman")

  expect_equal(pearson$metodo, "pearson_absoluto")
  expect_equal(spearman$metodo, "spearman_absoluto")

  # La relación es monótona exacta salvo ruido: Spearman tiene que verla mejor.
  expect_gt(spearman$asociacion, pearson$asociacion)
  expect_gt(spearman$asociacion, 0.99)

  # Las dos medidas siguen en la escala declarada del paquete.
  expect_true(all(c(pearson$asociacion, spearman$asociacion) >= 0))
  expect_true(all(c(pearson$asociacion, spearman$asociacion) <= 1))

  # Y cada una declara su supuesto, que no es el mismo.
  expect_true(grepl("linealidad", spearman$supuesto, fixed = TRUE))
  expect_false(identical(pearson$supuesto, spearman$supuesto))
})

test_that("el método por omisión no cambia", {
  datos <- data.frame(a = 1:20, b = 2 * (1:20))
  expect_equal(detectar_asociaciones(datos, umbral = 0)$metodo, "pearson_absoluto")
  # Pedirlo explícitamente da lo mismo que no pedir nada.
  expect_identical(
    detectar_asociaciones(datos, umbral = 0),
    detectar_asociaciones(datos, umbral = 0, metodo_numerico = "pearson")
  )
})

test_that("un método desconocido se rechaza en vez de adivinarse", {
  datos <- data.frame(a = 1:20, b = 2 * (1:20))
  expect_error(
    detectar_asociaciones(datos, metodo_numerico = "kendall"),
    "arg"
  )
})

test_that("analizar() traslada el método y lo declara en la salida", {
  set.seed(7)
  x <- 1:60
  datos <- data.frame(x = x, curva = x^3 + stats::rnorm(60L, 0, 500))

  por_omision <- analizar(datos)
  monotono <- analizar(datos, metodo_asociacion_numerica = "spearman")

  expect_equal(por_omision$asociaciones$metodo, "pearson_absoluto")
  expect_equal(monotono$asociaciones$metodo, "spearman_absoluto")
  expect_gt(monotono$asociaciones$asociacion, por_omision$asociaciones$asociacion)
})
