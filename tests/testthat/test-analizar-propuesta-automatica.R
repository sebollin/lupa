# La medicion automatica no convierte en requisito una regularidad aprendida de
# la misma entrega. Esas propuestas quedan disponibles para confirmacion, pero
# permanecen fuera del tablero mientras `incluir` sea FALSE.

testthat::skip_on_cran()

test_that("analizar() mide solo propuestas automaticas activas", {
  data(datos_operativos, package = "lupa")
  resultado <- suppressWarnings(analizar(
    datos_operativos,
    argumentos_perfil = list(analizar_dependencias = FALSE)
  ))

  propuesta <- resultado$propuesta_modelo
  activas <- propuesta$estado == "lista" & propuesta$incluir
  inactivas <- propuesta$id_sugerencia[
    propuesta$estado == "lista" & !propuesta$incluir
  ]
  medidas <- as.character(resultado$medicion$metrica_instanciada)

  expect_true(is.data.frame(resultado$medicion))
  expect_equal(length(medidas), sum(activas))
  expect_false(any(medidas %in% inactivas))
  expect_false(any(medidas %in% c(
    "Formato:contacto", "ValoresPosiblesPorExtension:zona"
  )))
})
