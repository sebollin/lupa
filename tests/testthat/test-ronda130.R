# Benford sobre una clave primaria afirma un problema de calidad en una columna
# que no tiene distribucion que analizar. Se intento adivinar cual columna era
# clave por la forma de sus valores y ese camino se retiro -dependia de cuantas
# filas se cargaron y callaba magnitudes reales-. Declarada, no hay nada que
# adivinar.

test_that("una clave declarada excluye Benford, y queda declarado", {
  set.seed(3)
  clave <- sort(sample.int(600000L, 10000L))
  datos <- data.frame(MEsId = clave)

  sin_declarar <- perfilar(
    datos, analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE
  )
  # Sin declarar no se adivina nada: el comportamiento no cambia.
  expect_true(
    "desviacion_benford" %in% as.character(sin_declarar$hallazgos$tipo_hallazgo)
  )

  declarada <- perfilar(
    datos, clave = "MEsId", analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  expect_false(
    "desviacion_benford" %in% as.character(declarada$hallazgos$tipo_hallazgo)
  )
  # No se apaga: se declara. Sin esta fila, quien lea los hallazgos veria una
  # columna sin problemas en vez de una columna sobre la que no se corrio.
  expect_true(
    "ley_benford" %in% as.character(declarada$cobertura_diagnosticos$diagnostico)
  )
})

test_that("el motivo dice que fue declarada, no que lo parece", {
  # «Parece un identificador» es una inferencia del paquete; «se declaro» es un
  # hecho que trajo el usuario. Publicar la primera cuando corresponde la segunda
  # le atribuye al paquete una deduccion que no hizo.
  set.seed(3)
  datos <- data.frame(MEsId = sort(sample.int(600000L, 10000L)))
  perfil <- perfilar(
    datos, clave = "MEsId", analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  cobertura <- perfil$cobertura_diagnosticos
  motivo <- as.character(
    cobertura$motivo[as.character(cobertura$diagnostico) == "ley_benford"]
  )[[1L]]
  expect_match(motivo, "la clave fue declarada", fixed = TRUE)
  expect_false(grepl("parece un identificador", motivo, fixed = TRUE))
})

test_that("un monto no declarado conserva Benford, que es lo que hay que cuidar", {
  # El costo del arreglo retirado era este: callaba magnitudes reales. Aqui no se
  # calla nada, porque no se infiere nada.
  set.seed(8)
  montos <- data.frame(monto = round(stats::rlnorm(3000L, 9, 1.2)))
  perfil <- perfilar(
    montos, analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE
  )
  declinado <- "ley_benford" %in%
    as.character(perfil$cobertura_diagnosticos$diagnostico)
  expect_false(declinado)
})

test_that("declarar una clave compuesta excluye las dos columnas", {
  set.seed(5)
  datos <- data.frame(
    anio = rep(2020:2024, each = 400L),
    cod = rep(sort(sample.int(90000L, 400L)), times = 5L)
  )
  perfil <- perfilar(
    datos, clave = c("anio", "cod"), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  hallazgos <- perfil$hallazgos
  benford <- hallazgos[
    as.character(hallazgos$tipo_hallazgo) == "desviacion_benford", ,
    drop = FALSE
  ]
  expect_equal(nrow(benford), 0L)
})
