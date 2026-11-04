test_that("un motor sin semilla conserva su muestra y no separa el desvio", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "ventas", data.frame(
    id = seq_len(1000L), monto = (seq_len(1000L) %% 37) + 0.25
  ))
  avisos <- character()
  r <- withCallingHandlers(suppressMessages(perfilar_dbi(
    con, "ventas", universo = "muestra_motor", muestra_motor = 150,
    proteger_datos_personales = FALSE
  )), warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  # SQLite no tiene una forma de fijar `RANDOM()`: la muestra se pide como
  # siempre, sin `ctid` ni `MD5()`, que no existen ahi.
  expect_equal(r$resumen_tabla$meta$muestreo$filas_obtenidas, 150)
  expect_length(avisos, 0L)
  sql <- r$resumen_tabla$sql$sql
  expect_false(any(grepl("ctid|MD5\\(", sql, perl = TRUE), na.rm = TRUE))
  expect_false(isTRUE(r$resumen_tabla$meta$muestreo$repetible))
})

test_that("la forma repetible se reconoce por motor y por objeto", {
  candidato_random <- list(tipo = "aleatorio", funciones = list(list(nombre = "random")))
  candidato_rand <- list(tipo = "aleatorio", funciones = list(list(nombre = "rand")))
  candidato_newid <- list(tipo = "aleatorio", funciones = list(list(nombre = "newid")))
  candidato_ts <- list(tipo = "tablesample", nombre = "tablesample_system")
  con <- structure(list(), class = "lupa_conexion_falsa")
  local_mocked_bindings(.senas_conexion_dbi = function(conexion) "sqlite")
  expect_false(.muestreo_repetible_dbi(candidato_random, con))
  expect_false(.muestreo_repetible_dbi(candidato_newid, con))
  expect_true(.muestreo_repetible_dbi(candidato_ts, con))
  local_mocked_bindings(.senas_conexion_dbi = function(conexion) "postgresql pqconnection")
  expect_true(.muestreo_repetible_dbi(candidato_random, con, "r"))
  expect_false(.muestreo_repetible_dbi(candidato_random, con, "view"))
  local_mocked_bindings(.senas_conexion_dbi = function(conexion) "mysql")
  expect_true(.muestreo_repetible_dbi(candidato_rand, con))
})
