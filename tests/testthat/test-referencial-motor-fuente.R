# Un referencial del motor se resuelve en la conexion de la medicion: el mismo
# nombre en otra base es otra tabla, y el sello lo dice.

testthat::skip_on_cran()

test_that("la misma tabla en otra base cambia la huella de la metrica referencial", {
  skip_if_not_installed("RSQLite")
  archivo_a <- tempfile(fileext = ".sqlite")
  archivo_b <- tempfile(fileext = ".sqlite")
  ca <- DBI::dbConnect(RSQLite::SQLite(), archivo_a)
  cb <- DBI::dbConnect(RSQLite::SQLite(), archivo_b)
  on.exit({
    DBI::dbDisconnect(ca)
    DBI::dbDisconnect(cb)
    unlink(c(archivo_a, archivo_b))
  }, add = TRUE)
  for (con in list(ca, cb)) DBI::dbWriteTable(con, "ent", data.frame(k = "a"))
  DBI::dbWriteTable(ca, "ref", data.frame(k = "a"))
  DBI::dbWriteTable(cb, "ref", data.frame(k = "b"))
  ref <- referencial("ref", "k", completo = TRUE, alcance = "p")
  mod <- modelo(
    instanciar(especializar(metricas_referencial()$CorrectitudSemFuerte, "F"),
               "ent", "k", referencial = ref),
    instanciar(especializar(metricas_nucleo()$NoNulo, "N"), "ent", "k")
  )
  medir_en <- function(con, id) {
    medir_dbi(mod, con, fecha = as.POSIXct("2026-01-01", tz = "UTC"), id_medicion = id)
  }
  a <- medir_en(ca, "a")
  b <- medir_en(cb, "b")
  a2 <- medir_en(ca, "a2")
  huella <- function(m) {
    s <- attr(m, "sello_metricas")
    stats::setNames(s$huella_definicion, s$metrica_instanciada)
  }
  expect_false(huella(a)[["F@ent.k"]] == huella(b)[["F@ent.k"]])
  expect_identical(huella(a)[["N@ent.k"]], huella(b)[["N@ent.k"]])
  expect_identical(huella(a), huella(a2))
  avisos <- character()
  withCallingHandlers(rbind(a, b), warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_true(any(grepl("cambi\u00f3 de definici\u00f3n", avisos, fixed = TRUE)))
  expect_true(any(grepl("F@ent.k", avisos, fixed = TRUE)))
})
