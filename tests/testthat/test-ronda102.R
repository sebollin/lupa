generar_benford_r102 <- function(n = 9000L) {
  10 ^ seq(0, 6, length.out = n)
}

perfil_benford_r102 <- function(x, nombre = "monto") {
  perfilar(
    setNames(data.frame(x), nombre),
    analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
}

test_that("los controles negativos no producen desviaciones de Benford", {
  controles <- list(
    edades = rep(18:95, 10L),
    notas = rep(1:12, 100L),
    porcentajes = rep(0:100, 10L),
    identificadores = 1000:9999,
    no_positivos = c(generar_benford_r102(998L), 0, -10),
    muestra_50 = generar_benford_r102(50L),
    constante = rep(100, 200L)
  )

  medicion <- lapply(names(controles), function(nombre) {
    perfil <- perfil_benford_r102(controles[[nombre]], nombre)
    cobertura <- perfil$cobertura_diagnosticos[
      perfil$cobertura_diagnosticos$diagnostico == "ley_benford",
      , drop = FALSE
    ]
    data.frame(
      caso = nombre,
      hallazgo = "desviacion_benford" %in% perfil$hallazgos$tipo_hallazgo,
      cobertura = nrow(cobertura) == 1L,
      motivo = if (nrow(cobertura)) cobertura$motivo[[1L]] else "",
      stringsAsFactors = FALSE
    )
  })
  medicion <- do.call(rbind, medicion)

  expect_false(
    any(medicion$hallazgo),
    info = paste(capture.output(medicion), collapse = "\n")
  )
  expect_true(all(medicion$cobertura),
              info = paste(capture.output(medicion), collapse = "\n"))
  expect_match(
    medicion$motivo[medicion$caso == "edades"], "ordenes de magnitud"
  )
  expect_match(
    medicion$motivo[medicion$caso == "notas"], "ordenes de magnitud"
  )
  expect_match(
    medicion$motivo[medicion$caso == "porcentajes"], "proporcion de positivos"
  )
  expect_match(
    medicion$motivo[medicion$caso == "identificadores"],
    "parece un identificador"
  )
  expect_match(
    medicion$motivo[medicion$caso == "no_positivos"],
    "proporcion de positivos"
  )
  expect_match(
    medicion$motivo[medicion$caso == "muestra_50"],
    "observaciones positivas"
  )
  expect_match(medicion$motivo[medicion$caso == "constante"], "sin variacion")
})

test_that("un control Benford pasa y una desviacion construida se describe", {
  control <- perfil_benford_r102(generar_benford_r102())
  resultado_control <- control$meta$benford$resultados$monto
  expect_true(resultado_control$aplica)
  expect_false(resultado_control$desviacion)
  expect_false("desviacion_benford" %in% control$hallazgos$tipo_hallazgo)
  expect_equal(nrow(resultado_control$distribucion), 9L)
  expect_named(
    resultado_control$distribucion,
    c("digito", "n_observado", "proporcion_observada",
      "proporcion_esperada", "n_esperado")
  )
  expect_true(is.finite(resultado_control$estadistico))

  alterado <- c(generar_benford_r102(), rep(900:999, 20L))
  perfil_alterado <- perfil_benford_r102(alterado)
  resultado_alterado <- perfil_alterado$meta$benford$resultados$monto
  hallazgo <- perfil_alterado$hallazgos[
    perfil_alterado$hallazgos$tipo_hallazgo == "desviacion_benford",
    , drop = FALSE
  ]
  expect_true(resultado_alterado$aplica)
  expect_true(resultado_alterado$desviacion)
  expect_equal(nrow(hallazgo), 1L)
  expect_match(hallazgo$evidencia, "X2=")
  expect_match(hallazgo$evidencia, "observado/esperado")
  expect_match(hallazgo$descripcion, "no evidencia de fraude")
  expect_match(hallazgo$sugerencia, "topes administrativos")
})

test_that("los umbrales de aplicabilidad quedan legibles en meta", {
  perfil <- perfil_benford_r102(rep(18:95, 10L), "edad")
  umbrales <- perfil$meta$benford$umbrales
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "ley_benford",
    , drop = FALSE
  ]

  expect_identical(
    names(umbrales),
    c("minimo_ordenes_magnitud", "minima_proporcion_positivos",
      "minimo_observaciones_utilizables", "nivel_significacion")
  )
  expect_equal(unname(unlist(umbrales)), c(3, 1, 100, 0.01))
  expect_equal(nrow(cobertura), 1L)
  expect_equal(cobertura$columna, "edad")
  expect_match(cobertura$motivo, "log10\\(max/min\\)")
  expect_false("desviacion_benford" %in% perfil$hallazgos$tipo_hallazgo)
})

test_that("sin columnas numericas candidatas el perfil es identico", {
  datos <- data.frame(
    texto = rep(c("a", "b"), 40L),
    fecha = as.Date("2024-01-01") + seq_len(80L),
    stringsAsFactors = FALSE
  )
  fecha <- as.POSIXct("2026-08-16 12:00:00", tz = "UTC")
  actual <- perfilar(datos, fecha = fecha, analizar_dependencias = FALSE)
  local_mocked_bindings(
    # El doble tiene que aceptar la misma firma que la funcion real, o el
    # simulacro falla por un motivo que no es el que el test mira. `...` lo
    # deja a salvo de que la firma crezca otra vez.
    .diagnosticar_benford = function(datos, columnas, hallazgos, ...) {
      list(
        hallazgos = list(),
        cobertura = lupa:::.cobertura_diagnosticos_vacia(),
        meta = NULL
      )
    },
    .package = "lupa"
  )
  sin_detector <- perfilar(datos, fecha = fecha, analizar_dependencias = FALSE)
  expect_identical(actual, sin_detector)
})

test_that("al quitar precondiciones reaparecen los falsos positivos", {
  edad <- rep(18:95, 10L)
  protegido <- perfil_benford_r102(edad, "edad")
  expect_false("desviacion_benford" %in% protegido$hallazgos$tipo_hallazgo)

  local_mocked_bindings(
    .precondiciones_benford = function(variacion, es_identificador,
                                       n_positivos, proporcion_positivos,
                                       ordenes, umbrales) character(),
    .package = "lupa"
  )
  roto <- perfil_benford_r102(edad, "edad")
  expect_true("desviacion_benford" %in% roto$hallazgos$tipo_hallazgo)
})
