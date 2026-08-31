# `muestra_motor` ya no usa `Inf` como señal implícita de tabla completa: la API
# lo rechaza antes de consultar para no dejar un plan ambiguo.

test_that("muestra_motor infinito se rechaza antes de sondear", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "tabla166", data.frame(
    id = seq_len(200L),
    numero = seq_len(200L),
    indicador = rep(c(0, 1), 100L),
    vacia = rep(NA_real_, 200L)
  ))

  expect_error(
    plan_perfilado_dbi(
      conexion, "tabla166", universo = "muestra_motor", muestra_motor = Inf
    ),
    class = "lupa_error_argumento_dbi"
  )
  expect_error(
    perfilar_dbi(
      conexion, "tabla166", universo = "muestra_motor", muestra_motor = Inf,
      instrumentar = FALSE, analizar_dependencias = FALSE,
      casi_duplicados_vocabulario = FALSE
    ),
    class = "lupa_error_argumento_dbi"
  )
})
