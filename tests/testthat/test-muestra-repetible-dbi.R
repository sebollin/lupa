test_that("el muestreo no fija semilla ni separa el desvio", {
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
    metricas = c("basicos", "desvio"),
    bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )), warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  # La muestra se pide como siempre, sin semilla SQL, `ctid` ni `MD5()`.
  expect_equal(r$resumen_tabla$meta$muestreo$filas_obtenidas, 150)
  expect_length(avisos, 0L)
  fila <- r$resumen_tabla$columnas[
    r$resumen_tabla$columnas$columna == "monto", , drop = FALSE
  ]
  expect_true(is.finite(as.numeric(fila$media)))
  expect_true(is.finite(as.numeric(fila$desvio)))
  sql <- r$resumen_tabla$sql$sql
  expect_true(any(grepl("OVER \\(\\)", sql, perl = TRUE), na.rm = TRUE))
  expect_true(any(grepl("lupa_desvio_ventana", sql, fixed = TRUE), na.rm = TRUE))
  expect_false(any(grepl("ctid|MD5\\(|REPEATABLE|RAND\\(", sql,
                         perl = TRUE), na.rm = TRUE))
  expect_false(any(grepl("semilla|repetible", names(
    r$resumen_tabla$meta$muestreo
  ), fixed = TRUE)))
})

test_that("las formas de muestreo no reciben una semilla", {
  candidato_random <- list(tipo = "aleatorio", funciones = list(list(nombre = "random")))
  candidato_rand <- list(tipo = "aleatorio", funciones = list(list(nombre = "rand")))
  candidato_ts <- list(
    tipo = "tablesample", nombre = "tablesample_system",
    constructor = function(tabla, porcentaje) {
      paste0(tabla, " TABLESAMPLE SYSTEM (", porcentaje, ")")
    }
  )
  dialecto <- list(limitar = function(sql, n, salto = 0) {
    paste0(sql, " LIMIT ", n)
  })
  for (candidato in list(candidato_random, candidato_rand, candidato_ts)) {
    forma <- .forma_muestreo_dbi(
      candidato, "`avn_t`", "`x`", "10", 10L, dialecto
    )
    expect_false(grepl(
      "REPEATABLE|RAND\\([0-9]|MD5\\(|ctid", forma$sql,
      perl = TRUE
    ))
  }
})

test_that("la forma de desvio por ventana se declara por motor", {
  con <- structure(list(), class = "lupa_conexion_falsa")
  local_mocked_bindings(.senas_conexion_dbi = function(conexion) "sqlite")
  expect_true(.soporta_ventana_desvio_dbi(con))
  expect_identical(.forma_desvio_ventana_dbi(con), 3L)
  local_mocked_bindings(.senas_conexion_dbi = function(conexion) "sql server")
  expect_identical(.forma_desvio_ventana_dbi(con), 2L)
  local_mocked_bindings(.senas_conexion_dbi = function(conexion) "oracle")
  expect_false(.soporta_ventana_desvio_dbi(con))
})
