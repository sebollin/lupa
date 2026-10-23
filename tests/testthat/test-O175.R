# Prueba de regresion: corre en la suite completa, en el CI y en R-hub, que usan
# NOT_CRAN=true; en CRAN se saltea para que el check entre en sus tiempos.
testthat::skip_on_cran()

testthat::test_that("float8 de PostgreSQL conserva la aritmetica binaria", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RPostgres")
  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = 55432,
      user = "postgres", password = "lupa", dbname = "lupa"
    ),
    error = function(e) NULL
  )
  if (is.null(con)) testthat::skip("PostgreSQL real no disponible en 55432.")
  tabla <- "r38d_dev_o175_float8"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
  DBI::dbExecute(con, "SET extra_float_digits = 0")
  valores <- data.frame(
    a = rep(1000000000000005, 2),
    b = rep(1 / 3, 2),
    c = c(1e16 + 2, -1e16),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, tabla, valores)
  salida <- suppressMessages(lupa::perfilar_dbi(
    con, tabla, bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  ))
  columnas <- salida$resumen_tabla$columnas
  esperados <- c(a = 1000000000000005, b = 1 / 3, c = 1)
  for (nombre in names(valores)) {
    fila <- columnas[columnas$columna == nombre, , drop = FALSE]
    testthat::expect_equal(fila$media, esperados[[nombre]], tolerance = 0)
  }
})

testthat::test_that("la mediana exacta conserva vecinos de un ulp en PG93", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RPostgres")
  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = 55493,
      user = "postgres", password = "lupa", dbname = "lupa"
    ),
    error = function(e) NULL
  )
  if (is.null(con)) testthat::skip("PostgreSQL 9.3 no disponible en 55493.")
  tabla <- "r38d_dev_o175_mediana"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  base <- c(0.1, 1 / 3, 2 / 3, 1e5 / 7, 123.456)
  vecinos <- unlist(lapply(base, function(x) {
    x + (0:3) * 2^(floor(log2(x)) - 52)
  }))
  valores <- c(rep(vecinos, times = c(5, rep(1, length(vecinos) - 1))),
               vecinos[3], vecinos[3])
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla,
                             " (id integer, v double precision)"))
  literales <- paste0(
    "(", seq_along(valores), ", CAST('", sprintf("%.17g", valores),
    "' AS DOUBLE PRECISION))", collapse = ","
  )
  DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES ", literales))
  esperado <- lupa::perfilar(data.frame(v = valores))$columnas$mediana
  vias <- list(
    defecto = list(),
    bloques = list(bloque_filas = 7L),
    muestra_motor = list(universo = "muestra_motor", muestra_motor = 1000L)
  )
  for (via in vias) {
    salida <- suppressMessages(do.call(
      lupa::perfilar_dbi,
      c(list(con, tabla, proteger_datos_personales = FALSE,
             analizar_dependencias = FALSE), via)
    ))
    fila <- salida$resumen_tabla$columnas
    fila <- fila[fila$columna == "v", , drop = FALSE]
    testthat::expect_equal(fila$mediana, esperado, tolerance = 0)
  }
})

testthat::test_that("incluir_valores FALSE cierra todos los canales de valores", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RPostgres")
  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = 55432,
      user = "postgres", password = "lupa", dbname = "lupa"
    ),
    error = function(e) NULL
  )
  if (is.null(con)) testthat::skip("PostgreSQL real no disponible en 55432.")
  tabla <- "r38d_dev_o175_privacidad"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (id integer, d numeric)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla,
    " VALUES (1, 9007199254740993), (2, 9007199254740995),",
    " (3, 9007199254740997)"
  ))
  salida <- suppressMessages(lupa::perfilar_dbi(
    con, tabla, universo = "muestra_motor", muestra_motor = 3L,
    incluir_valores = FALSE, proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE
  ))
  fila <- salida$resumen_tabla$columnas
  fila <- fila[fila$columna == "d", , drop = FALSE]
  testthat::expect_false(isTRUE(salida$resumen_tabla$meta$incluir_valores))
  testthat::expect_true(all(is.na(fila[, c("moda", "minimo", "maximo",
                                           "mediana")])))
  if (is.data.frame(salida$perfil_muestra$columnas)) {
    muestra <- salida$perfil_muestra$columnas
    muestra <- muestra[muestra$columna == "d", , drop = FALSE]
    testthat::expect_true(all(is.na(muestra[, c("moda", "minimo", "maximo",
                                                "mediana")])))
  }
  serializado <- paste(capture.output(dput(salida)), collapse = "\n")
  testthat::expect_false(grepl("900719925474099", serializado, fixed = TRUE))
})

testthat::test_that("comparar_perfiles no convierte dos muestras DBI en deriva", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RPostgres")
  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = 55432,
      user = "postgres", password = "lupa", dbname = "lupa"
    ),
    error = function(e) NULL
  )
  if (is.null(con)) testthat::skip("PostgreSQL real no disponible en 55432.")
  tabla <- "r38d_dev_o175_deriva"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
  DBI::dbWriteTable(con, tabla, data.frame(
    id = 1:2000, x = seq(10, 20000, by = 10),
    stringsAsFactors = FALSE
  ))
  llamada <- function() suppressMessages(lupa::perfilar_dbi(
    con, tabla, muestra = 100L,
    orden_muestra = "id", proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE
  ))
  anterior <- llamada()
  actual <- llamada()
  deriva <- suppressMessages(lupa::comparar_perfiles(
    anterior$perfil_muestra, actual$perfil_muestra
  ))
  testthat::expect_true(any(deriva$aspecto == "muestra" &
                              deriva$cambio == "no_comparable"))
  no_afirmado <- c("rango", "cardinalidad", "hallazgo", "patron",
                   "alcance_resumen")
  testthat::expect_false(any(deriva$aspecto %in% no_afirmado))
})

testthat::test_that("un nombre con coma no resuelve falsamente su par", {
  set.seed(3811L)
  n <- 400L
  datos <- data.frame(
    id = seq_len(n), a = round(runif(n) * 1000, 2),
    z = rnorm(n), stringsAsFactors = FALSE, check.names = FALSE
  )
  datos[["monto, usd"]] <- datos$z * 3
  datos$total <- datos[["monto, usd"]]
  anterior <- suppressMessages(lupa::perfilar(
    datos, fecha = as.POSIXct("2026-01-01", tz = "UTC")
  ))
  actual <- suppressMessages(lupa::perfilar(
    datos[, setdiff(names(datos), "monto, usd"), drop = FALSE],
    fecha = as.POSIXct("2026-02-01", tz = "UTC")
  ))
  deriva <- suppressMessages(lupa::comparar_perfiles(anterior, actual))
  pares <- deriva[deriva$aspecto == "hallazgo" &
                    grepl("monto, usd", deriva$columna, fixed = TRUE), ,
                  drop = FALSE]
  testthat::expect_false(any(pares$cambio == "resuelto"))
})

testthat::test_that("analizar no deja escapar avisos de stats cor", {
  datos <- data.frame(
    a = c(1, 2, 3, 4, 5, NA_real_),
    b = c(7, 7, 7, 7, 7, 8)
  )
  testthat::expect_no_warning(lupa::analizar(
    datos, fecha = as.POSIXct("2026-03-01", tz = "UTC")
  ))
})
