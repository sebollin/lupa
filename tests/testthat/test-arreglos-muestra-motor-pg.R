testthat::skip_on_cran()

test_that("TABLESAMPLE corto reintenta en PostgreSQL", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  testthat::skip_if(
    !nzchar(Sys.getenv("LUPA_PG_HOST", unset = "")),
    "se necesita LUPA_PG_HOST para la prueba del motor"
  )
  con <- DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST"),
    port = as.integer(Sys.getenv("LUPA_PG_PORT", unset = "55432")),
    dbname = Sys.getenv("LUPA_PG_NAME", unset = "lupa"),
    user = Sys.getenv("LUPA_PG_USER", unset = "postgres"),
    password = Sys.getenv("LUPA_PG_PASS", unset = "lupa")
  )
  DBI::dbExecute(con, "DROP TABLE IF EXISTS arrm_reintento")
  on.exit({
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS arrm_reintento"),
        silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbExecute(
    con,
    "CREATE TABLE arrm_reintento AS SELECT g AS id FROM generate_series(1, 70000) g"
  )
  DBI::dbExecute(con, "ANALYZE arrm_reintento")
  # El reintento conserva la mejor de las dos muestras: si la de BERNOULLI no
  # mejora a la primera, el metodo publicado sigue siendo `tablesample_system`
  # (una de cada diez corridas, medido). Avisa solo la muestra final corta: un
  # reintento que recupera lo pedido deja la escasez en `meta$muestreo`.
  resultado <- NULL
  desajustes <- 0L
  for (i in seq_len(100L)) {
    aviso <- NULL
    candidato <- withCallingHandlers(
      perfilar_dbi(
        con, "arrm_reintento", universo = "muestra_motor", muestra_motor = 1L,
        muestra = 1L, metricas = "validos", proteger_datos_personales = FALSE
      ), warning = function(w) {
        if (inherits(w, "lupa_aviso_muestra_motor")) aviso <<- w
        invokeRestart("muffleWarning")
      }
    )
    muestreo <- candidato$resumen_tabla$meta$muestreo
    corta <- isTRUE(muestreo$filas_obtenidas < muestreo$filas_pedidas / 2)
    if (identical(muestreo$reintentos, 1L) && !identical(corta, !is.null(aviso))) {
      desajustes <- desajustes + 1L
    }
    if (is.null(resultado) &&
        identical(muestreo$metodo_inicial, "tablesample_system") &&
        identical(muestreo$reintentos, 1L) &&
        identical(muestreo$metodo, "tablesample_bernoulli") && !corta) {
      resultado <- candidato
      aviso_reintento <- aviso
    }
    if (!is.null(resultado) && i >= 20L) break
  }
  expect_identical(desajustes, 0L)
  if (is.null(resultado)) {
    testthat::skip("no aparecio un reintento ganador en 100 intentos")
  }
  expect_equal(resultado$resumen_tabla$meta$muestreo$reintentos, 1L)
  expect_equal(
    resultado$resumen_tabla$meta$muestreo$metodo,
    "tablesample_bernoulli"
  )
  expect_null(aviso_reintento)
  expect_true(is.data.frame(resultado$resumen_tabla$meta$consultas$fallas))
})

test_that("el spool limita bytes antes de traer el bloque", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  testthat::skip_if(
    !nzchar(Sys.getenv("LUPA_PG_HOST", unset = "")),
    "se necesita LUPA_PG_HOST para la prueba del motor"
  )
  con <- DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST"),
    port = as.integer(Sys.getenv("LUPA_PG_PORT", unset = "55432")),
    dbname = Sys.getenv("LUPA_PG_NAME", unset = "lupa"),
    user = Sys.getenv("LUPA_PG_USER", unset = "postgres"),
    password = Sys.getenv("LUPA_PG_PASS", unset = "lupa")
  )
  DBI::dbExecute(con, "DROP TABLE IF EXISTS arrm_bytes")
  on.exit({
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS arrm_bytes"),
        silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE arrm_bytes AS",
    "SELECT g AS id, string_agg(md5(g::text || ':' || s::text), '') AS texto",
    "FROM generate_series(1, 200) AS a(g)",
    "CROSS JOIN generate_series(1, 6250) AS b(s) GROUP BY g"
  ))
  DBI::dbExecute(con, "ANALYZE arrm_bytes")
  limite <- 64 * 1024^2
  aviso <- NULL
  resultado <- withCallingHandlers(
    perfilar_dbi(
      con, "arrm_bytes", universo = "muestra_motor", muestra_motor = 200L,
      muestra = 200L, metricas = "validos", max_bytes_muestra = limite,
      max_bytes_materializacion = limite, max_bytes_procesamiento = limite,
      proteger_datos_personales = FALSE
    ), warning = function(w) {
      aviso <<- w
      invokeRestart("muffleWarning")
    }
  )
  materializacion <- resultado$resumen_tabla$meta$materializacion
  expect_true(is.null(aviso))
  expect_true(isTRUE(materializacion$estimacion_bytes$disponible))
  expect_equal(materializacion$n_filas, 200)
  expect_identical(materializacion$estado, "cerrada_verificada")
  expect_lte(materializacion$bytes, limite)
  expect_false(is.null(resultado$perfil_muestra))
})

test_that("las binarias se perfilan con largo y resumen", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  testthat::skip_if(
    !nzchar(Sys.getenv("LUPA_PG_HOST", unset = "")),
    "se necesita LUPA_PG_HOST para la prueba del motor"
  )
  con <- DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST"),
    port = as.integer(Sys.getenv("LUPA_PG_PORT", unset = "55432")),
    dbname = Sys.getenv("LUPA_PG_NAME", unset = "lupa"),
    user = Sys.getenv("LUPA_PG_USER", unset = "postgres"),
    password = Sys.getenv("LUPA_PG_PASS", unset = "lupa")
  )
  DBI::dbExecute(con, "DROP TABLE IF EXISTS arrm_bin")
  on.exit({
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS arrm_bin"), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE arrm_bin (id integer, dato bytea)")
  DBI::dbExecute(con, paste(
    "INSERT INTO arrm_bin VALUES",
    "(1, decode('00', 'hex')), (2, decode('0001', 'hex')) ,",
    "(3, decode('0001', 'hex')), (4, NULL)"
  ))
  resultado <- suppressWarnings(perfilar_dbi(
    con, "arrm_bin", universo = "muestra_motor", muestra_motor = 4L,
    muestra = 4L, metricas = "validos", proteger_datos_personales = FALSE
  ))
  binaria <- resultado$perfil_muestra$columnas[
    resultado$perfil_muestra$columnas$columna == "dato", , drop = FALSE
  ]
  expect_equal(binaria$n_faltantes, 1)
  expect_equal(binaria$n_distintos, 2)
  expect_equal(binaria$longitud_minima, 1)
  expect_equal(binaria$longitud_maxima, 2)
  expect_true(grepl(
    "octet_length|md5", resultado$perfil_muestra$meta$origen_dbi$muestreo$sql,
    perl = TRUE
  ))
})
