# Un motivo con y sin punto final es el mismo motivo: el aviso de las metricas
# no medidas lo da una vez. El punto se agregaba despues de quitar los
# repetidos, y `x` y `x.` salian los dos.

test_that("el mismo motivo con y sin punto final sale una vez", {
  skip_on_cran()
  expect_identical(.frases_no_medibles(c("mismo motivo", "mismo motivo.")),
                   "mismo motivo.")
  cobertura <- data.frame(
    metrica_instanciada = c("A@t.x", "B@t.x"), estado = "no_medible",
    motivo = c("El referencial `r` vive en otra parte",
               "El referencial `r` vive en otra parte."),
    stringsAsFactors = FALSE
  )
  avisos <- character()
  withCallingHandlers(
    .avisar_no_medibles(cobertura, 3L),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(avisos, 1L)
  ocurrencias <- gregexpr("vive en otra parte", avisos[[1L]], fixed = TRUE,
                          useBytes = TRUE)[[1L]]
  expect_identical(sum(ocurrencias > 0L), 1L)
})

test_that("el control: dos motivos distintos salen los dos", {
  skip_on_cran()
  expect_identical(.frases_no_medibles(c("uno", "otro.")), c("uno.", "otro."))
})
