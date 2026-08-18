# Varias columnas que codifican el mismo hecho y se contradicen dentro de la
# misma fila. El valor está en que **ninguna columna por separado lo muestra**:
# los tres años de una fila pueden ser todos plausibles y aun así uno
# contradecir a los otros.
#
# El grupo lo declara quien conoce los datos. Adivinarlo sería suponer dominio:
# dos columnas de año pueden ser el de nacimiento y el de ingreso.

test_that("una discordancia aparece con su fila y su universo", {
  datos <- data.frame(
    anio_fiscal = c(2023L, 2023L, 2022L),
    anio_archivo = c(2023L, 2023L, 2023L)
  )
  resultado <- detectar_discordancias(
    datos, senal_redundante(c("anio_fiscal", "anio_archivo"))
  )

  expect_equal(nrow(resultado), 1L)
  expect_equal(resultado$n_discordantes, 1)
  expect_equal(resultado$n_evaluadas, 3)
  expect_equal(resultado$proporcion, 1 / 3)
  expect_true(grepl("fila 3", resultado$evidencia, fixed = TRUE))
  expect_true(grepl("universo: 3 de 3", resultado$evidencia, fixed = TRUE))
})

test_that("una transformación lleva las columnas a una escala comparable", {
  datos <- data.frame(
    fecha = as.Date(c("2023-05-01", "2022-07-01")),
    anio_fiscal = c(2023L, 2023L)
  )
  resultado <- detectar_discordancias(
    datos,
    senal_redundante(
      c("fecha", "anio_fiscal"),
      transformacion = list(fecha = function(x) as.integer(format(x, "%Y")))
    )
  )
  expect_equal(resultado$n_discordantes, 1)
  # La evidencia muestra el valor comparado, no la fecha cruda.
  expect_true(grepl("fecha=2022", resultado$evidencia, fixed = TRUE))
})

test_that("la ventana es la tolerancia declarada, en las unidades del valor", {
  datos <- data.frame(a = c(2023L, 2023L), b = c(2023L, 2024L))
  sin_tolerancia <- detectar_discordancias(datos, senal_redundante(c("a", "b")))
  con_tolerancia <- detectar_discordancias(
    datos, senal_redundante(c("a", "b"), ventana = 1)
  )
  expect_equal(sin_tolerancia$n_discordantes, 1)
  expect_equal(con_tolerancia$n_discordantes, 0)
  expect_true(grepl("ventana declarada: 1", con_tolerancia$evidencia, fixed = TRUE))
})

test_that("una fila con un valor ausente no cuenta como desacuerdo", {
  # Ausencia no es discordancia: la fila sale del universo y se declara.
  datos <- data.frame(a = c(2023L, NA_integer_), b = c(2023L, 2019L))
  resultado <- detectar_discordancias(datos, senal_redundante(c("a", "b")))
  expect_equal(resultado$n_evaluadas, 1)
  expect_equal(resultado$n_discordantes, 0)
  expect_true(grepl("universo: 1 de 2", resultado$evidencia, fixed = TRUE))
})

test_that("sin ninguna fila comparable la señal se declara no evaluada", {
  datos <- data.frame(a = c(NA_integer_, 1L), b = c(2L, NA_integer_))
  resultado <- detectar_discordancias(datos, senal_redundante(c("a", "b")))
  expect_equal(resultado$n_evaluadas, 0)
  # No se afirma cero discordancias: no se midió.
  expect_true(is.na(resultado$n_discordantes))
  expect_true(is.na(resultado$proporcion))
  expect_true(grepl("no se evaluo", resultado$evidencia, fixed = TRUE))
})

test_that("tres o más columnas se comparan entre sí", {
  datos <- data.frame(
    a = c(2023L, 2023L), b = c(2023L, 2023L), c = c(2023L, 2021L)
  )
  resultado <- detectar_discordancias(datos, senal_redundante(c("a", "b", "c")))
  expect_equal(resultado$n_discordantes, 1)
  expect_true(grepl("c=2021", resultado$evidencia, fixed = TRUE))
})

test_that("las columnas de texto se comparan por igualdad exacta", {
  datos <- data.frame(
    depto = c("Montevideo", "Canelones"),
    depto_ficha = c("Montevideo", "Maldonado"),
    stringsAsFactors = FALSE
  )
  resultado <- detectar_discordancias(
    datos, senal_redundante(c("depto", "depto_ficha"))
  )
  expect_equal(resultado$n_discordantes, 1)
  expect_true(grepl("comparacion textual exacta", resultado$evidencia, fixed = TRUE))
})

test_that("una señal mal declarada se rechaza diciendo qué hay", {
  datos <- data.frame(a = 1:3, b = 1:3)
  expect_error(senal_redundante("a"), "al menos dos")
  expect_error(senal_redundante(c("a", "a")), "repite")
  expect_error(senal_redundante(c("a", "b"), ventana = -1), "no negativo")
  expect_error(
    senal_redundante(c("a", "b"), transformacion = list(z = identity)),
    "no estan en la senal"
  )
  expect_error(
    senal_redundante(c("a", "b"), transformacion = list(a = 1)),
    "debe ser una funcion"
  )
  expect_error(
    detectar_discordancias(datos, senal_redundante(c("a", "z"))),
    "Disponibles"
  )
  expect_error(detectar_discordancias(datos, "a"), "senal_redundante")
})

test_that("una transformación que cambia el largo se rechaza", {
  datos <- data.frame(a = 1:3, b = 1:3)
  expect_error(
    detectar_discordancias(datos, senal_redundante(
      c("a", "b"), transformacion = list(a = function(x) x[1])
    )),
    "devolvio 1 valores"
  )
})
