# `conformidad_dbi()` comprueba en el servidor el pliegue que el referencial del
# motor escribe con `LOWER()`, y `medir_dbi()` deja de medir en SQL lo que no
# conforma.

testthat::skip_on_cran()

test_that("la bateria comprueba el pliegue del referencial del motor", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "dep", data.frame(codigo = c("NC", "SD")))
  DBI::dbWriteTable(con, "personas", data.frame(dep = c("NC", "nc", "SD", "ZZ")))
  ref <- referencial(DBI::Id(table = "dep"), "codigo", completo = TRUE, alcance = "d")
  mod <- modelo(instanciar(
    especializar(metricas_referencial()$CorrectitudSemFuerte, "F"),
    "personas", "dep", referencial = ref
  ))
  cf <- conformidad_dbi(con, mod)
  expect_identical(cf$regla, "referencial")
  expect_identical(cf$conformidad, "conforme")
  expect_identical(cf$n_motor, cf$n_r)
  # Con una guarda que no aparta nada, el motor y R difieren en los pares que
  # la guarda existe para apartar: no conforma, y la medicion no va por SQL.
  testthat::local_mocked_bindings(
    .sql_guarda_pliegue_dbi = function(expr, motor, perfil) "(1 = 0)"
  )
  roto <- conformidad_dbi(con, mod)
  expect_identical(roto$conformidad, "no_conforme")
  medicion <- suppressWarnings(medir_dbi(mod, con, conformidad = roto))
  expect_identical(nrow(medicion), 0L)
  expect_identical(attr(medicion, "cobertura_metricas")$estado, "no_medible")
  con_bloques <- suppressWarnings(medir_dbi(
    mod, con, conformidad = roto, estrategia_no_sql = "bloques"
  ))
  expect_identical(
    unname(attr(con_bloques, "ejecucion_dbi")$vias), "distintos_r"
  )
  expect_identical(con_bloques$numerador, 3)
})
