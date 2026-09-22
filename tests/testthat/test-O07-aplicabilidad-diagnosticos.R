# O07. Los diagnosticos de texto respetan el universo aplicable.
#
# `perfilar(..., aplicabilidad = list(col = ~cond))` declara que una columna
# solo corresponde a ciertas filas. El analisis y la identidad ya enmascaraban
# las demas como ausentes, pero los diagnosticos de texto recibian la columna
# CRUDA -mientras su vocabulario venia armado sobre la enmascarada-. Con eso
# `espacios_sobrantes` contaba la columna entera y trazaba solo lo aplicable, y
# el paquete disparaba su propia alarma: "cuenta 6 y traza 4".

.o07_datos <- function() {
  data.frame(
    activo = c("si", "si", "si", "si", "no", "no"),
    txt = c(" a ", " b ", " c ", " d ", " e ", " f "),
    stringsAsFactors = FALSE
  )
}

test_that("el paquete no dispara su alarma de trazabilidad con aplicabilidad declarada", {
  alarmas <- character()
  perfil <- withCallingHandlers(
    perfilar(.o07_datos(), aplicabilidad = list(txt = ~activo == "si")),
    warning = function(w) {
      if (grepl("trazabilidad inconsistente", conditionMessage(w), fixed = TRUE)) {
        alarmas <<- c(alarmas, conditionMessage(w))
      }
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(alarmas, character())
  hallazgo <- perfil$hallazgos[perfil$hallazgos$tipo_hallazgo == "espacios_sobrantes", ]
  expect_equal(nrow(hallazgo), 1L)
  # Cuenta solo el universo aplicable, y lo que cuenta es lo que traza.
  expect_equal(hallazgo$n_afectados, 4L)
  expect_setequal(hallazgo$trazabilidad[[1L]]$indices_fila, 1:4)
})

test_that("sin aplicabilidad declarada, el diagnostico sigue mirando la columna entera", {
  perfil <- perfilar(.o07_datos())
  hallazgo <- perfil$hallazgos[perfil$hallazgos$tipo_hallazgo == "espacios_sobrantes", ]
  expect_equal(hallazgo$n_afectados, 6L)
})
