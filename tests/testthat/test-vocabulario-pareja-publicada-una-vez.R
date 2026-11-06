# La via equifrecuente declara las parejas cercanas que el camino principal no
# forma: las de frecuencias parejas, que caen bajo el piso de asimetria
# (`min_asimetria_vocabulario`). Su tope es otro numero
# (`max_asimetria_equifrecuente_vocabulario`), y los dos se tocan: con los
# valores por omision, `Montevideo` (50) contra `Montevido` (25) da 2, que
# llega al piso -el camino principal la publica- y no pasa el tope -la via
# equifrecuente tambien la contaba-. La salida por omision se contradecia: el
# hallazgo con la pareja y, en la misma columna, la fila de cobertura "1 grupo
# de formas cercanas no se formo". Con el detector encendido, la misma pareja
# salia dos veces. Con el tope por encima del piso pasaba en todo el tramo.
#
# La propiedad que se fija: una pareja sale una sola vez. Si el camino
# principal la publico, la cobertura no la cuenta como no formada; si el piso
# la dejo afuera, la cuenta, como siempre.

# Fuera de CRAN: el archivo arma doce perfiles; tarda 3,8 s si es lo primero
# que corre y 0,4 s con el paquete ya cargado (medido el 2026-11-06), por
# encima de los 0,2 s que se admiten en modo CRAN. Corre en la suite completa
# y en el CI, con NOT_CRAN=true.
testthat::skip_on_cran()

.una_vez_publicar <- function(x, equi, ...) {
  p <- suppressWarnings(suppressMessages(perfilar(
    data.frame(x = x, stringsAsFactors = FALSE),
    fecha = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    variantes_equifrecuentes_vocabulario = equi, ...
  )))
  h <- suppressWarnings(suppressMessages(hallazgos(p)))
  cb <- cobertura(p)
  list(
    casi = h[h$tipo_hallazgo == "casi_duplicados_vocabulario" &
               h$severidad == "sospechoso", , drop = FALSE],
    equi = h[h$tipo_hallazgo == "variantes_equifrecuentes_vocabulario", ,
             drop = FALSE],
    motivos = cb$motivo[cb$diagnostico == "proximidad_vocabulario"]
  )
}

.una_vez_no_formada <- function(motivos) {
  any(grepl("de formas cercanas no se form", motivos, fixed = TRUE,
            useBytes = TRUE))
}

.una_vez_pareja <- function(comun, rara) {
  c(rep("Montevideo", comun), rep("Montevido", rara), rep("Salto", 30L))
}

.una_vez_comprobar_publicada <- function(comun, rara, ...) {
  etiqueta <- paste0(comun, "/", rara)
  grupo <- paste0("[Montevideo (", comun, ") / Montevido (", rara, ")]")
  for (equi in c(FALSE, TRUE)) {
    salida <- .una_vez_publicar(.una_vez_pareja(comun, rara), equi, ...)
    expect_identical(nrow(salida$casi), 1L, label = etiqueta)
    expect_true(grepl(grupo, salida$casi$evidencia, fixed = TRUE,
                      useBytes = TRUE), label = etiqueta)
    expect_identical(nrow(salida$equi), 0L, label = etiqueta)
    expect_false(.una_vez_no_formada(salida$motivos), label = etiqueta)
  }
}

.una_vez_comprobar_contada <- function(comun, rara, ...) {
  etiqueta <- paste0(comun, "/", rara)
  grupo <- paste0("[Montevideo (", comun, ") / Montevido (", rara, ")]")
  apagado <- .una_vez_publicar(.una_vez_pareja(comun, rara), FALSE, ...)
  encendido <- .una_vez_publicar(.una_vez_pareja(comun, rara), TRUE, ...)
  expect_identical(nrow(apagado$casi), 0L, label = etiqueta)
  expect_true(any(grepl("de valores cercanos no se inform", apagado$motivos,
                        fixed = TRUE, useBytes = TRUE)), label = etiqueta)
  expect_true(.una_vez_no_formada(apagado$motivos), label = etiqueta)
  expect_identical(nrow(encendido$casi), 0L, label = etiqueta)
  expect_identical(nrow(encendido$equi), 1L, label = etiqueta)
  expect_true(grepl(grupo, encendido$equi$evidencia, fixed = TRUE,
                    useBytes = TRUE), label = etiqueta)
  expect_true(.una_vez_no_formada(encendido$motivos), label = etiqueta)
}

test_that("en el borde de asimetria 2 la pareja publicada no se cuenta otra vez", {
  skip_if_not_installed("stringdist")
  .una_vez_comprobar_publicada(50L, 25L)
  # El detector interno dice lo mismo: el candidato no se cuenta, en ninguno
  # de los dos modos.
  for (equi in c(FALSE, TRUE)) {
    r <- lupa:::.grupos_casi_duplicados_vocabulario(
      .una_vez_pareja(50L, 25L), lupa:::.resolver_normalizacion(TRUE), "x",
      detectar_variantes_equifrecuentes = equi
    )
    expect_length(r$grupos, 1L)
    expect_identical(r$grupos[[1L]]$origen, "distancia")
    expect_identical(r$alcance$n_grupos_sin_variante_rara, 0L)
    expect_length(r$grupos_equifrecuentes, 0L)
  }
})

test_that("con el tope por encima del piso, todo el tramo sale una sola vez", {
  skip_if_not_installed("stringdist")
  # 2,5, 4 y el tope mismo, 5.
  .una_vez_comprobar_publicada(
    50L, 20L, max_asimetria_equifrecuente_vocabulario = 5
  )
  .una_vez_comprobar_publicada(
    48L, 12L, max_asimetria_equifrecuente_vocabulario = 5
  )
  .una_vez_comprobar_publicada(
    50L, 10L, max_asimetria_equifrecuente_vocabulario = 5
  )
})

test_that("lo que el piso deja afuera se sigue contando", {
  skip_if_not_installed("stringdist")
  # Control con los valores por omision: 50 contra 26 queda bajo el piso.
  .una_vez_comprobar_contada(50L, 26L)
  # Y con el piso subido por encima de la asimetria: 48 contra 12 da 4, que no
  # llega a 5 y entra en el tope.
  .una_vez_comprobar_contada(
    48L, 12L, min_asimetria_vocabulario = 5,
    max_asimetria_equifrecuente_vocabulario = 5
  )
})
