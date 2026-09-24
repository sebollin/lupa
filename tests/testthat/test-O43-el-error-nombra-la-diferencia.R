# O43: acumular una corrida con un `id_medicion` ya presente y otra fecha se
# rechazaba diciendo "una configuracion diferente", y no habia ninguna
# configuracion distinta: el modelo, el marco, la aplicabilidad y el perfil
# eran los mismos. El mensaje mandaba a revisar donde no estaba el problema.

.evaluacion_de_entrega <- function(fecha) {
  nucleo <- metricas_nucleo()
  instancia <- instanciar(especializar(nucleo$NoNulo), "personas", "edad")
  medicion <- medir(
    modelo(instancia), data.frame(edad = c(20, 30, NA)),
    id_medicion = "entrega", fecha = as.POSIXct(fecha, tz = "UTC")
  )
  evaluar(
    medicion,
    perfil_evaluacion("Operativo",
                      regla_evaluacion("Presente", function(x) x > 0.5))
  )
}

test_that("el rechazo por fecha nombra la fecha y no la configuracion", {
  enero <- .evaluacion_de_entrega("2026-01-31")
  febrero <- .evaluacion_de_entrega("2026-02-28")
  historico <- historico_calidad(enero, detalle = "completo")

  error <- tryCatch(
    acumular_historico(historico, febrero, detalle = "completo"),
    error = function(e) conditionMessage(e)
  )

  expect_true(is.character(error))
  expect_match(error, "Difiere en: fecha", fixed = TRUE)
  expect_match(error, "id_medicion", fixed = TRUE)
  # Sin acentos en codigo: la guarda del paquete los prohibe fuera de los
  # comentarios. "configuraci" alcanza, porque el mensaje nuevo no la nombra.
  expect_false(grepl("configuraci", error, fixed = TRUE))
})

test_that("la misma corrida dos veces sigue acumulandose sin ruido", {
  # Control: lo que cambia es el mensaje del rechazo, no que se rechace de mas.
  enero <- .evaluacion_de_entrega("2026-01-31")
  historico <- historico_calidad(enero, detalle = "completo")

  otra_vez <- acumular_historico(historico, enero, detalle = "completo")

  expect_true(inherits(otra_vez, "historico_calidad"))
  expect_equal(nrow(otra_vez), nrow(historico))
})
