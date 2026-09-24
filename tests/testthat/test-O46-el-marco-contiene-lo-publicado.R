# La cobertura del tablero se cuenta sobre los factores DEL MARCO. Una medida
# cuyo par dimension-factor el marco no declara no entra en ninguna casilla, asi
# que el objeto publicaba una fila con valor y, al lado, "se midieron 0 factores
# de este marco": las dos cosas ciertas, juntas leidas como contradiccion.
# Negarse no corresponde -declarar el marco propio y medir con las metricas del
# paquete es un flujo documentado-, asi que lo que faltaba era contar lo que
# queda afuera.

metrica_o46 <- function(dimension = "DimA", factor = "FactorA") {
  metodo <- function(tablas, instancia) data.frame(
    resultado = 1, entidad = "tabla", atributo = "x", fila = NA_integer_,
    objeto = "x"
  )
  metrica(
    "MideO46", "a", "atributo", "real", dimension = dimension,
    factor = factor, metodo = metodo
  )
}

medicion_o46 <- function(marco = NULL, dimension = "DimA",
                         factor = "FactorA") {
  instancia <- instanciar(especializar(metrica_o46(dimension, factor)),
                          "tabla", "x")
  medir(
    modelo(instancia, marco = marco), data.frame(x = 1),
    id_medicion = "o46", fecha = as.POSIXct("2026-09-24 12:00:00", tz = "UTC")
  )
}

test_that("el tablero cuenta y nombra lo que el marco no declara", {
  ajeno <- marco_calidad("Marco ajeno", list(DimB = "FactorB"))
  tablero <- tablero_calidad(medicion_o46(), marco = ajeno)
  alcance <- attr(tablero, "alcance", exact = TRUE)

  expect_equal(nrow(tablero), 1L)
  expect_equal(alcance$factores_medidos, 0L)
  expect_equal(alcance$medidos_fuera_del_marco, 1L)
  expect_identical(
    attr(tablero, "pares_fuera_del_marco", exact = TRUE), "DimA|FactorA"
  )
})

test_that("lo que queda afuera no se cuenta como casilla del marco", {
  # El numero nuevo no puede entrar en la suma de la cobertura: el marco tiene
  # los factores que tiene, y lo medido afuera no es uno de ellos.
  ajeno <- marco_calidad("Marco ajeno", list(
    DimB = "FactorB", DimC = "FactorC"
  ))
  alcance <- attr(
    tablero_calidad(medicion_o46(), marco = ajeno), "alcance", exact = TRUE
  )

  expect_equal(alcance$factores_marco, 2L)
  expect_equal(
    alcance$factores_medidos + alcance$sin_metrica_declarada +
      alcance$no_aplican + alcance$fuera_de_alcance,
    alcance$factores_marco
  )
  expect_equal(alcance$medidos_fuera_del_marco, 1L)
})

test_that("el marco que declara el par no cuenta nada afuera", {
  # El control tiene que poder fallar: si el conteo se disparara siempre, no
  # distinguiria el marco propio del ajeno.
  propio <- marco_calidad("Marco propio", list(
    DimA = "FactorA", DimB = "FactorB"
  ))
  tablero <- tablero_calidad(medicion_o46(), marco = propio)
  alcance <- attr(tablero, "alcance", exact = TRUE)

  expect_equal(alcance$factores_medidos, 1L)
  expect_equal(alcance$medidos_fuera_del_marco, 0L)
  expect_identical(
    attr(tablero, "pares_fuera_del_marco", exact = TRUE), character()
  )
})

test_that("los tres caminos por omision contienen lo que publican", {
  # Sin marco declarado, el tablero usa el de la medicion, el de AGESIC cuando
  # corresponde, o arma uno con los pares medidos: ninguno puede dejar una
  # medida afuera.
  propio <- marco_calidad("Marco propio", list(DimA = "FactorA"))
  for (medidas in list(medicion_o46(), medicion_o46(marco = propio))) {
    alcance <- attr(tablero_calidad(medidas), "alcance", exact = TRUE)
    expect_equal(alcance$medidos_fuera_del_marco, 0L)
    expect_equal(alcance$factores_medidos, 1L)
  }
})

test_that("la impresion nombra los pares que quedan afuera", {
  ajeno <- marco_calidad("Marco ajeno", list(DimB = "FactorB"))
  tablero <- tablero_calidad(medicion_o46(), marco = ajeno)
  salida <- NULL
  invisible(capture.output(salida <- cli::cli_fmt(print(tablero))))

  expect_true(any(grepl("no est", salida)))
  expect_true(any(grepl("DimA|FactorA", salida, fixed = TRUE)))
})

test_that("informar contra el marco propio sigue siendo posible", {
  # Es el flujo de la vinieta de inicio: el organismo declara SU marco, cuya
  # cobertura se satisface por `perfil_mide`, y mide con las metricas del
  # paquete, que traen su propia taxonomia. Rechazar ese cruce romperia el uso
  # documentado; lo que corresponde es declarar lo que queda afuera.
  factores <- data.frame(
    dimension = c("Estructura", "Trazabilidad"),
    factor = c("Ausencias observadas", "Origen documentado"),
    perfil_mide = c(TRUE, FALSE),
    como_resolverlo = c(
      "Revisar ausencias detectadas por el perfil.",
      "Declarar una metrica sobre el sistema de origen."
    ),
    stringsAsFactors = FALSE
  )
  propio <- marco_calidad("Marco operativo de la prueba", factores)
  analisis <- analizar(
    data.frame(
      codigo = c("A", "B", "B", NA), monto = c(1, 2, 3, 4),
      stringsAsFactors = FALSE
    ),
    marco = propio, fecha = as.POSIXct("2026-09-24 12:00:00", tz = "UTC")
  )
  alcance <- attr(analisis$tablero, "alcance", exact = TRUE)

  expect_gt(nrow(analisis$tablero), 0L)
  expect_equal(alcance$factores_marco, 2L)
  expect_gt(alcance$medidos_fuera_del_marco, 0L)
  expect_true(all(
    attr(analisis$tablero, "pares_fuera_del_marco", exact = TRUE) %in%
      paste(analisis$tablero$dimension, analisis$tablero$factor, sep = "|")
  ))
})
