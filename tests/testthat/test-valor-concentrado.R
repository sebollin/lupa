.perfil_valor_concentrado <- function(x) {
  perfilar(
    data.frame(id = seq_along(x), valor = x),
    analizar_dependencias = FALSE,
    ausencia_estructural = FALSE,
    proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
}

.tiene_valor_concentrado <- function(perfil) {
  any(as.character(perfil$hallazgos$tipo_hallazgo) == "valor_concentrado")
}

test_that("tres bancos reales elegibles no disparan valor_concentrado", {
  bancos <- list(
    airquality = datasets::airquality,
    faithful = datasets::faithful,
    iris = datasets::iris
  )

  perfiles <- lapply(
    bancos,
    function(datos) perfilar(
      datos,
      analizar_dependencias = FALSE,
      ausencia_estructural = FALSE,
      proteger_datos_personales = FALSE,
      casi_duplicados_vocabulario = FALSE
    )
  )
  for (nombre in names(perfiles)) {
    expect_false(
      .tiene_valor_concentrado(perfiles[[nombre]]),
      info = nombre
    )
  }
})

test_that("el relleno al 25 y 50 por ciento dispara y el 10 por ciento no", {
  base <- seq_len(100L)
  perfiles <- lapply(c(0.10, 0.25, 0.50), function(fraccion) {
    x <- base
    x[seq_len(round(fraccion * length(x)))] <- 1000
    .perfil_valor_concentrado(x)
  })

  expect_false(.tiene_valor_concentrado(perfiles[[1L]]))
  expect_true(.tiene_valor_concentrado(perfiles[[2L]]))
  expect_true(.tiene_valor_concentrado(perfiles[[3L]]))

  hallazgo <- perfiles[[2L]]$hallazgos[
    perfiles[[2L]]$hallazgos$tipo_hallazgo == "valor_concentrado", ,
    drop = FALSE
  ]
  expect_equal(hallazgo$severidad, factor(
    "sospechoso", levels = c("ok", "sospechoso", "error"), ordered = TRUE
  ))
  expect_equal(hallazgo$n_evaluados, 100)
  expect_equal(hallazgo$n_afectados, 25)
  expect_equal(hallazgo$unidad_conteo, "fila")
  expect_match(hallazgo$evidencia, "Valor modal: 1000", fixed = TRUE)
  expect_match(hallazgo$evidencia, "frecuencia de la moda: 25", fixed = TRUE)
  expect_match(hallazgo$evidencia, "frecuencia del segundo valor: 1", fixed = TRUE)
  expect_match(hallazgo$evidencia, "cociente moda/segundo: 25.000", fixed = TRUE)
  expect_match(hallazgo$evidencia, "fraccion de la moda sobre validos: 0.250", fixed = TRUE)
  expect_equal(hallazgo$trazabilidad[[1L]]$total, 25)
  expect_identical(hallazgo$trazabilidad[[1L]]$indices_fila, seq_len(25L))
})

test_that("la elegibilidad exige 20 validos y 10 distintos, sin cobertura ruidosa", {
  diecinueve_validos <- c(rep(1000, 10L), seq_len(9L), NA_real_)
  nueve_distintos <- c(rep(1000, 12L), seq_len(8L))
  veinte_y_diez <- c(rep(1000, 12L), seq_len(9L))

  expect_false(.tiene_valor_concentrado(
    .perfil_valor_concentrado(diecinueve_validos)
  ))
  expect_false(.tiene_valor_concentrado(
    .perfil_valor_concentrado(nueve_distintos)
  ))
  expect_true(.tiene_valor_concentrado(
    .perfil_valor_concentrado(veinte_y_diez)
  ))

  categorica <- .perfil_valor_concentrado(rep(1:5, each = 4L))
  expect_false(.tiene_valor_concentrado(categorica))
  expect_false(any(
    categorica$cobertura_diagnosticos$diagnostico == "valor_concentrado"
  ))
})

test_that("las dos puertas usan sus bordes inclusivos", {
  ratio_4_99 <- c(rep(1000, 499L), rep(1, 100L), 2:9)
  ratio_5 <- c(rep(1000, 500L), rep(1, 100L), 2:9)
  fraccion_0_149 <- c(rep(1000, 149L), seq_len(851L))
  fraccion_0_15 <- c(rep(1000, 150L), seq_len(850L))

  expect_false(.tiene_valor_concentrado(.perfil_valor_concentrado(ratio_4_99)))
  expect_true(.tiene_valor_concentrado(.perfil_valor_concentrado(ratio_5)))
  expect_false(.tiene_valor_concentrado(
    .perfil_valor_concentrado(fraccion_0_149)
  ))
  expect_true(.tiene_valor_concentrado(
    .perfil_valor_concentrado(fraccion_0_15)
  ))
})
