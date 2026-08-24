# El universo aplicable llegaba al perfilado y se cortaba en la medicion.

test_that("medir respeta el universo declarado", {
  # Una columna condicionada de mil filas cuyo universo son trescientas, con
  # treinta vacias de verdad. Sin declarar el universo, la completitud se mide
  # contra la tabla entera y da 0,270 cuando la respuesta es 0,900. El historico
  # y la deriva consumen mediciones, asi que arrastraban ese numero.
  set.seed(3)
  datos <- data.frame(
    tiene_auto = c(rep("Si", 300L), rep("No", 700L)),
    marca_auto = c(
      sample(c("Ford", "Fiat", "VW"), 270L, replace = TRUE),
      rep(NA, 30L), rep(NA, 700L)
    ),
    stringsAsFactors = FALSE
  )
  marco <- marco_calidad("m", list(Completitud = "Densidad"))
  modelo_medicion <- modelo(
    list(instanciar(especializar(metricas_nucleo()$NoNulo), "d", "marca_auto")),
    marco = marco
  )

  sin_universo <- medir(modelo_medicion, list(d = datos), id_medicion = "sin")
  con_universo <- medir(
    modelo_medicion, list(d = datos), id_medicion = "con",
    aplicabilidad = list(marca_auto = ~ tiene_auto == "Si")
  )

  # Se miden las filas del universo, no las de la tabla.
  expect_equal(nrow(sin_universo), 1000L)
  expect_equal(nrow(con_universo), 300L)

  expect_equal(
    agregar(sin_universo, "atributo", "ratio")$resultado[[1L]], 0.27
  )
  expect_equal(
    agregar(con_universo, "atributo", "ratio")$resultado[[1L]], 0.9
  )
})

test_that("sin declaracion la medicion no cambia", {
  datos <- data.frame(x = c("a", "b", NA, "d"), stringsAsFactors = FALSE)
  marco <- marco_calidad("m", list(Completitud = "Densidad"))
  modelo_medicion <- modelo(
    list(instanciar(especializar(metricas_nucleo()$NoNulo), "d", "x")),
    marco = marco
  )
  antes <- medir(modelo_medicion, list(d = datos), id_medicion = "a")
  despues <- medir(
    modelo_medicion, list(d = datos), id_medicion = "a", aplicabilidad = NULL
  )
  expect_equal(antes$resultado, despues$resultado)
  expect_equal(nrow(antes), 4L)
})

test_that("la regla solo recorta la columna que mide", {
  # Una metrica sobre otra columna de la misma tabla no se toca aunque haya una
  # regla declarada para su vecina.
  datos <- data.frame(
    tiene_auto = c("Si", "No", "Si", "No"),
    marca_auto = c("Ford", NA, "VW", NA),
    documento = c("1", "2", NA, "4"),
    stringsAsFactors = FALSE
  )
  marco <- marco_calidad("m", list(Completitud = "Densidad"))
  modelo_medicion <- modelo(
    list(instanciar(especializar(metricas_nucleo()$NoNulo), "d", "documento")),
    marco = marco
  )
  medido <- medir(
    modelo_medicion, list(d = datos), id_medicion = "x",
    aplicabilidad = list(marca_auto = ~ tiene_auto == "Si")
  )
  # `documento` se mide sobre las cuatro filas: la regla es de `marca_auto`.
  expect_equal(nrow(medido), 4L)
  expect_equal(agregar(medido, "atributo", "ratio")$resultado[[1L]], 0.75)
})

test_that("una aplicabilidad mal formada se rechaza", {
  datos <- data.frame(x = 1:4)
  marco <- marco_calidad("m", list(Completitud = "Densidad"))
  modelo_medicion <- modelo(
    list(instanciar(especializar(metricas_nucleo()$NoNulo), "d", "x")),
    marco = marco
  )
  expect_error(
    medir(modelo_medicion, list(d = datos), aplicabilidad = list(~ x > 1)),
    "lista con nombre"
  )
})
