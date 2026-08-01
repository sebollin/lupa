test_that("se validan las declaraciones y especializaciones", {
  expect_error(
    metrica("", "semántica", "atributo", "real"), "nombre"
  )
  expect_error(
    metrica("M", "", "atributo", "real"), "semantica"
  )
  expect_error(
    metrica("M", "semántica", "desconocida", "real"), "Granularidad"
  )
  expect_error(
    metrica("M", "semántica", "atributo", "entero"), "tipo_resultado"
  )
  expect_error(
    metrica(
      "M", "semántica", "atributo", "real", propiedades = c("x", "x")
    ),
    "únicos"
  )
  expect_error(
    metrica(
      "M", "semántica", "atributo", "real", propiedades = "nombre con espacio"
    ),
    "sintácticos"
  )
  expect_error(
    metrica("M", "semántica", "atributo", "real", metodo = 1),
    "metodo"
  )
  expect_error(
    metrica(
      "M", "semántica", "atributo", "real", validar_propiedades = 1
    ),
    "validar_propiedades"
  )

  generica <- metrica(
    "M", "semántica", "instanciaAtributo", "booleano",
    propiedades = "umbral"
  )
  expect_error(
    do.call(
      especializar,
      c(list(metrica = generica, nombre_especifico = "Especifica"), list(1))
    ),
    "tener nombre"
  )
  expect_error(especializar(generica, nombre_especifico = "", umbral = 1),
               "nombre_especifico")
  invalida <- metrica(
    "I", "semántica", "atributo", "real",
    validar_propiedades = function(x) 1
  )
  expect_error(especializar(invalida), "devolver una lista")
})

test_that("las closures también permiten la aplicación parcial directa", {
  metodo <- function(tablas, instancia) {
    x <- tablas[[instancia$entidad]][[instancia$atributos]]
    data.frame(
      resultado = x > instancia$configuracion$umbral,
      entidad = instancia$entidad,
      atributo = instancia$atributos,
      fila = seq_along(x),
      objeto = paste0("t$x[", seq_along(x), "]")
    )
  }
  generica <- metrica(
    "Mayor", "Supera un umbral.", "instanciaAtributo", "booleano",
    propiedades = "umbral", metodo = metodo
  )
  especifica <- generica(umbral = 2)
  instancia <- especifica(entidad = "t", atributos = "x")

  expect_equal(
    medir(modelo(list(instancia)), data.frame(x = 1:3))$resultado,
    c(0, 0, 1)
  )
})

test_that("se validan instancias, modelos y vínculos", {
  nucleo <- metricas_nucleo()
  no_nulo <- especializar(nucleo$NoNulo)

  expect_error(instanciar(no_nulo, character(), "x"), "entidad")
  expect_error(instanciar(no_nulo, "t", NA_character_), "atributos")
  expect_error(instanciar(no_nulo, "t", "x", nombre_instancia = ""),
               "nombre_instancia")
  sin_metodo <- especializar(metrica(
    "SinMetodo", "Sin método.", "atributo", "real"
  ))
  expect_error(instanciar(sin_metodo, "t", "x"), "requiere un `metodo`")
  futura <- especializar(metrica(
    "Futura", "Fuera del alcance.", "coleccion", "real",
    metodo = function(tablas, instancia) data.frame()
  ))
  expect_error(instanciar(futura, "bd"), "declarada")
  expect_error(modelo(), "una o más")
  expect_error(modelo(no_nulo), "instanciadas")

  instancia <- instanciar(no_nulo, "t", "x")
  expect_error(medir(instancia, data.frame(x = 1)), "modelo")
  expect_error(medir(modelo(instancia), data.frame(x = 1), fecha = NA), "fecha")
  expect_error(
    medir(modelo(instancia), data.frame(x = 1), id_medicion = ""),
    "id_medicion"
  )
  expect_error(
    medir(modelo(instancia), list(t = 1:3)), "lista con nombre"
  )
  expect_error(
    medir(modelo(instancia), data.frame(y = 1)), "atributo ligado"
  )
  expect_error(
    medir(
      modelo(instanciar(no_nulo, c("t", "otra"), "x")),
      list(t = data.frame(x = 1), otra = data.frame(x = 1))
    ),
    "requiere 1 entidad"
  )
})

test_that("se validan las configuraciones de las métricas del núcleo", {
  nucleo <- metricas_nucleo()
  expect_error(
    especializar(nucleo$Formato, expresion_regular = 1),
    "expresion_regular"
  )
  expect_error(
    especializar(nucleo$Formato, diccionario = list("a")), "atómico"
  )
  expect_error(
    especializar(nucleo$Formato, validador = 1), "validador"
  )
  expect_error(
    especializar(nucleo$Formato, desconocida = 1), "exactamente una"
  )
  expect_error(
    especializar(nucleo$ValoresPosiblesPorExtension, valores = list("a")),
    "atómico"
  )
  expect_error(
    especializar(nucleo$ReglaIntegridadIntraEntidad, regla = 1), "función"
  )
  expect_error(
    especializar(nucleo$ReglaIntegridadInterEntidad, otra = 1),
    "sólo acepta"
  )
  expect_error(
    especializar(nucleo$ReglaIntegridadInterEntidad, muestra = 0), "positivo"
  )
})

test_that("se validan los contratos de los métodos de medición", {
  nucleo <- metricas_nucleo()
  intra <- especializar(
    nucleo$ReglaIntegridadIntraEntidad,
    regla = function(x) rep(TRUE, nrow(x))
  )
  expect_error(
    medir(modelo(instanciar(intra, "t")), data.frame(x = 1:2)),
    "al menos un atributo"
  )
  expect_error(
    medir(modelo(instanciar(intra, "t", "y")), data.frame(x = 1:2)),
    "No se encontraron"
  )
  intra_mala <- especializar(
    nucleo$ReglaIntegridadIntraEntidad,
    regla = function(x) rep(NA, nrow(x))
  )
  expect_error(
    medir(modelo(instanciar(intra_mala, "t", "x")), data.frame(x = 1:2)),
    "lógico sin NA"
  )

  error <- instanciar(especializar(nucleo$ErrorEstandar), "t", "x")
  expect_error(medir(modelo(error), data.frame(x = letters[1:3])), "numérico")
  expect_error(medir(modelo(error), data.frame(x = c(1, NA))), "al menos dos")
  constante <- medir(modelo(error), data.frame(x = c(2, 2)))
  expect_equal(constante$resultado, 0)

  mala_salida <- function(tablas, instancia) data.frame(resultado = 0.5)
  generica <- metrica(
    "Personalizada", "Prueba contrato.", "atributo", "real"
  )
  instancia_mala <- instanciar(
    especializar(generica), "t", "x", metodo = mala_salida
  )
  expect_error(medir(modelo(instancia_mala), data.frame(x = 1:2)),
               "debe devolver un data frame")

  salida_real_invalida <- function(tablas, instancia) data.frame(
    resultado = 2, entidad = "t", atributo = "x", fila = NA_integer_,
    objeto = "t$x"
  )
  expect_error(
    medir(modelo(instanciar(
      especializar(generica), "t", "x", metodo = salida_real_invalida
    )), data.frame(x = 1:2)),
    "finitos en"
  )
})

test_that("se validan entradas y parámetros de agregación", {
  nucleo <- metricas_nucleo()
  no_nulo <- especializar(nucleo$NoNulo, nombre_especifico = "NoNulo")
  instancia <- instanciar(no_nulo, "t", "x")
  medidas <- medir(modelo(instancia), data.frame(x = c(1, NA)))

  expect_error(agregar(data.frame(), "atributo", "ratio"), "no vacío")
  fuera_rango <- medidas
  fuera_rango$resultado[[1L]] <- 2
  expect_error(agregar(fuera_rango, "atributo", "ratio"), "resultados")
  varias <- rbind(medidas, medidas)
  varias$id_medicion[3:4] <- "otra"
  expect_error(agregar(varias, "atributo", "ratio"), "id_medicion")
  expect_error(agregar(medidas, "inexistente", "ratio"), "Granularidad")

  reales <- agregar(medidas, "atributo", "ratio")
  expect_error(
    agregar(reales, "entidad", "ratio_umbral"), "umbral"
  )
  expect_error(
    agregar(reales, "entidad", "ratio_umbral", umbral = 2), "umbral"
  )
  expect_error(
    agregar(reales, "entidad", "promedio_ponderado", pesos = 1:2), "pesos"
  )
})

test_that("se agrega de entidad a conjunto de entidades", {
  nucleo <- metricas_nucleo()
  no_nulo <- especializar(nucleo$NoNulo, nombre_especifico = "NoNuloCompartido")
  a <- instanciar(no_nulo, "a", "x")
  b <- instanciar(no_nulo, "b", "x")
  medidas <- medir(
    modelo(a, b),
    list(a = data.frame(x = c(1, NA)), b = data.frame(x = c(1, 2)))
  )
  atributos <- agregar(medidas, "atributo", "ratio")
  entidades <- agregar(atributos, "entidad", "promedio")
  conjunto <- agregar(entidades, "conjuntoEntidades", "promedio")

  expect_equal(entidades$resultado, c(0.5, 1))
  expect_equal(conjunto$resultado, 0.75)
  expect_equal(conjunto$granularidad, "conjuntoEntidades")
  expect_match(conjunto$objeto_medible, "a, b", fixed = TRUE)
})

test_that("se validan reglas, perfiles y evaluaciones", {
  expect_error(regla_evaluacion("", identity), "nombre")
  expect_error(regla_evaluacion("r", identity, metricas = ""), "metricas")
  regla <- regla_evaluacion("r", function(x) x >= 0)
  expect_error(perfil_evaluacion("", regla), "nombre")
  expect_error(perfil_evaluacion("p", 1), "una o más reglas")
  expect_error(perfil_evaluacion("p", regla, regla), "deben ser únicos")
  expect_s3_class(perfil_evaluacion("p", list(regla)), "perfil_evaluacion")

  nucleo <- metricas_nucleo()
  instancia <- instanciar(especializar(nucleo$NoNulo), "t", "x")
  medidas <- medir(modelo(instancia), data.frame(x = c(1, NA)))
  perfil <- perfil_evaluacion("p", regla)
  expect_error(evaluar(data.frame(), perfil), "no vacío")
  expect_error(evaluar(medidas, regla), "perfil")
  mala <- perfil_evaluacion(
    "mala", regla_evaluacion("mala", function(x) rep(NA, length(x)))
  )
  expect_error(evaluar(medidas, mala), "lógicos sin NA")

  evaluada <- evaluar(medidas, perfil)
  expect_error(comparar_evaluaciones(evaluada, perfil), "evaluar")
  historica <- evaluar(rbind(medidas, transform(
    medidas, id_medicion = "segunda",
    id_medida = paste0("segunda-", seq_len(nrow(medidas)))
  )), perfil)
  expect_equal(nrow(historica$perfiles), 2L)
  expect_error(comparar_evaluaciones(historica, evaluada), "una sola corrida")
})
