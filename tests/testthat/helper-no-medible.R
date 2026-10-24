# `medir()` ya no aborta cuando el metodo de una metrica falla: la metrica queda
# en `cobertura_metricas` con estado `no_medible`, el motivo conserva el mensaje
# del metodo, y un aviso lo dice. Las pruebas que antes esperaban el error ahora
# esperan las tres cosas: sin el aviso la falla quedaria muda, y sin el motivo no
# se sabria que fallo.
.expect_no_medible <- function(expr, patron) {
  avisos <- character()
  medicion <- withCallingHandlers(expr, warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  cobertura <- attr(medicion, "cobertura_metricas", exact = TRUE)
  testthat::expect_s3_class(cobertura, "data.frame")
  fallidas <- cobertura[cobertura$estado == "no_medible", , drop = FALSE]
  testthat::expect_gte(nrow(fallidas), 1L)
  testthat::expect_true(any(grepl(patron, fallidas$motivo)), info = patron)
  testthat::expect_true(
    any(grepl("no se midieron porque su m", avisos, fixed = TRUE)),
    info = patron
  )
  invisible(medicion)
}
