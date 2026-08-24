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

test_that("analizar propaga el universo del perfil a la medicion", {
  # El universo se declara una sola vez, en `argumentos_perfil`, y tiene que
  # gobernar todo el analisis. Sin propagarlo, el mismo objeto informaba el
  # dato de dos maneras: el perfil decia 0,100 de faltantes -sobre las filas
  # del universo- y el tablero 0,270 de completitud -sobre la tabla entera-.
  set.seed(3)
  datos <- data.frame(
    tiene_auto = c(rep("Si", 300L), rep("No", 700L)),
    marca_auto = c(
      sample(c("Ford", "Fiat", "VW"), 270L, replace = TRUE),
      rep(NA, 30L), rep(NA, 700L)
    ),
    stringsAsFactors = FALSE
  )
  completitud <- function(analisis) {
    medicion <- analisis$medicion
    fila <- medicion[
      grepl("marca_auto", as.character(medicion$objeto_medible)), ,
      drop = FALSE
    ]
    fila$resultado[[1L]]
  }

  con <- analizar(
    datos, proteger_datos_personales = FALSE,
    argumentos_perfil = list(
      aplicabilidad = list(marca_auto = ~ tiene_auto == "Si")
    )
  )
  sin <- analizar(datos, proteger_datos_personales = FALSE)

  expect_equal(completitud(con), 0.9)
  expect_equal(completitud(sin), 0.27)

  # Y el perfil del mismo objeto cuenta lo mismo: 30 de 300 faltantes.
  fila <- con$perfil$columnas[con$perfil$columnas$columna == "marca_auto", ]
  expect_equal(fila$prop_faltantes, 0.1)
  expect_equal(fila$n_aplicables, 300L)
})

test_that("la regla se resuelve por tabla y no globalmente", {
  # Una regla puede nombrar una columna que existe en varias tablas del modelo.
  # El universo se evalua en cada una con sus propias filas: recortar una tabla
  # con la mascara de otra seria peor que no recortar.
  set.seed(4)
  personas <- data.frame(
    activo = c(rep("Si", 50L), rep("No", 50L)),
    correo = c(sample(c("a@b.c", "d@e.f"), 45L, replace = TRUE),
               rep(NA, 5L), rep(NA, 50L)),
    stringsAsFactors = FALSE
  )
  tramites <- data.frame(
    activo = rep("Si", 30L),
    correo = c(sample("x@y.z", 28L, replace = TRUE), rep(NA, 2L)),
    stringsAsFactors = FALSE
  )
  marco <- marco_calidad("m", list(Completitud = "Densidad"))
  modelo_medicion <- modelo(list(
    instanciar(especializar(metricas_nucleo()$NoNulo), "personas", "correo"),
    instanciar(especializar(metricas_nucleo()$NoNulo), "tramites", "correo")
  ), marco = marco)

  medido <- medir(
    modelo_medicion, list(personas = personas, tramites = tramites),
    id_medicion = "x", aplicabilidad = list(correo = ~ activo == "Si")
  )
  filas_de <- function(tabla) {
    sum(grepl(tabla, as.character(medido$objeto_medible), fixed = TRUE))
  }
  # `personas` tiene 50 activos de 100; `tramites` los 30.
  expect_equal(filas_de("personas"), 50L)
  expect_equal(filas_de("tramites"), 30L)
})
