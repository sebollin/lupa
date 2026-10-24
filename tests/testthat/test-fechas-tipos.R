test_that("se reconocen los formatos de fecha mínimos", {
  valores <- c(
    "2020-01-31", "31/01/2020", "12/31/2020", "31-01-2020",
    "2020/01/31", "31.01.2020", "20200131",
    "2020-01-31 12:30", "31/01/2020 12:30:45"
  )
  resultado <- detectar_formatos_fecha(valores)

  expect_true(all(c(
    "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y",
    "%Y/%m/%d", "%d.%m.%Y", "%Y%m%d", "%Y-%m-%d %H:%M",
    "%d/%m/%Y %H:%M:%S"
  ) %in% resultado$formato))
  expect_equal(attr(resultado, "compatibles"), length(valores))
  expect_true(attr(resultado, "formatos_mixtos"))
})

test_that("las fechas con barras ambiguas conservan ambos candidatos", {
  resultado <- detectar_formatos_fecha(c("01/02/2020", "02/03/2020"))

  expect_setequal(resultado$formato, c("%d/%m/%Y", "%m/%d/%Y"))
  expect_true(all(resultado$estado == "candidato"))
  expect_false(attr(resultado, "formatos_mixtos"))

  resuelto <- detectar_formatos_fecha(c("13/02/2020", "02/03/2020"))
  expect_equal(resuelto$formato, "%d/%m/%Y")
  expect_equal(resuelto$n, 2L)
  expect_equal(resuelto$n_ambiguos, 1L)
})

test_that("no se aceptan fechas u horas imposibles", {
  fechas <- detectar_formatos_fecha(c("2020-13-01", "31/02/2020", "texto"))
  expect_equal(nrow(fechas), 0L)
  expect_equal(attr(fechas, "compatibles"), 0L)

  hora <- inferir_tipo(c("23:59", "00:00:01"))
  hora_mala <- inferir_tipo(c("25:00", "texto"))
  expect_equal(hora$tipo, "hora")
  expect_equal(hora_mala$tipo, "texto")
})

test_that("la inferencia informa tipo y compatibilidad", {
  expect_equal(inferir_tipo(c("1", "2", "3"))$tipo, "entero")
  expect_equal(inferir_tipo(c("1.2", "2,5", "3"))$tipo, "doble")
  expect_equal(inferir_tipo(c("sí", "no", "TRUE"))$tipo, "logico")
  expect_equal(inferir_tipo(c("001", "002", "003"))$tipo, "identificador")
  expect_equal(inferir_tipo(c("AB01", "AB02", "AB03"))$tipo, "identificador")
  expect_equal(inferir_tipo(c("uno dos", "tres cuatro"))$tipo, "texto")

  parcial <- inferir_tipo(c("1", "2", "x"), umbral = 0.6)
  expect_equal(parcial$tipo, "entero")
  expect_equal(parcial$proporcion, 2 / 3)
  expect_equal(parcial$compatibles, 2L)
})

test_that("cero y uno sin literales alfabéticos se infieren como enteros", {
  resultado <- inferir_tipo(c(rep("0", 14), "1"))
  mixto <- inferir_tipo(c("0", "1", "sí", "no"))

  expect_equal(resultado$tipo, "entero")
  expect_equal(mixto$tipo, "logico")
})

test_that("identificadores de ocho dígitos no se confunden con fechas compactas", {
  cedulas <- c("45031155", "38765432", "51234567", "29876543", "12345678")
  resultado <- detectar_formatos_fecha(cedulas)
  fecha_real <- detectar_formatos_fecha("19850518")

  expect_false("%Y%m%d" %in% resultado$formato)
  expect_equal(fecha_real$formato, "%Y%m%d")
})

test_that("la inferencia reconoce fechas, fecha-hora y tipos declarados", {
  fecha <- inferir_tipo(c("2020-01-31", "31/01/2020"))
  fecha_hora <- inferir_tipo(c("2020-01-31 10:20", "2020-02-01 11:21"))
  declarada <- inferir_tipo(as.Date(c("2020-01-01", NA)))

  expect_equal(fecha$tipo, "fecha")
  expect_equal(fecha$proporcion, 1)
  expect_equal(fecha_hora$tipo, "fecha-hora")
  expect_equal(declarada$tipo, "fecha")
  expect_equal(declarada$proporcion, 1)
  expect_output(print(fecha), "fecha.*100.0%")
})

test_that("un vector sin datos válidos queda como desconocido", {
  resultado <- inferir_tipo(c(NA_character_, "  "))
  expect_equal(resultado$tipo, "desconocido")
  expect_true(is.na(resultado$proporcion))
  expect_error(inferir_tipo("1", umbral = 2), "entre 0 y 1")
})
