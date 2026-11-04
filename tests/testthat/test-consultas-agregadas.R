.arrc_datos <- function() {
  data.frame(
    n1 = c(1, 1, 2, 3, NA), n2 = c(10, 10, 20, 30, 40),
    n3 = c(-1, 0, 2, 2, 3), n4 = c(5, 6, 7, 8, 9),
    texto = c("a", "a", "b", "c", NA), stringsAsFactors = FALSE
  )
}

test_that("SQL Server calcula el desvio centrado por lotes", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD")
  if (!nzchar(clave)) testthat::skip("Sin LUPA_MSSQL_PWD")
  conexion <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS", Server = "127.0.0.1", Port = 1433,
    UID = "sa", PWD = clave, Database = "lupa_pruebas",
    TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
  ), error = function(e) NULL)
  if (is.null(conexion)) testthat::skip("No se pudo abrir SQL Server")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  tabla <- "arrc_sqlserver"
  try(DBI::dbExecute(conexion, paste0("DROP TABLE ", tabla)), silent = TRUE)
  on.exit(try(DBI::dbExecute(conexion, paste0("DROP TABLE ", tabla)),
             silent = TRUE), add = TRUE)
  DBI::dbWriteTable(conexion, tabla, .arrc_datos())

  resultado <- suppressMessages(perfilar_dbi(
    conexion, tabla, metricas = c("basicos", "desvio"),
    universo = "tabla_completa", bloque_muestra = "solo_agregados",
    tamano_lote_planos = 2L, proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE, duplicados_aproximados = FALSE
  ))
  sql <- resultado$resumen_tabla$sql
  expect_equal(length(unique(sql$consulta_id[sql$etapa == "basicos"])), 2L)
  expect_equal(length(unique(sql$consulta_id[sql$etapa == "desvio"])), 2L)
  expect_equal(
    resultado$resumen_tabla$columnas$desvio[
      resultado$resumen_tabla$columnas$columna == "n1"
    ],
    sd(c(1, 1, 2, 3))
  )
})

test_that("PostgreSQL calcula el desvio exacto por lotes", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RPostgres")
  clave <- Sys.getenv("LUPA_PG_PASSWORD")
  if (!nzchar(clave)) testthat::skip("Sin LUPA_PG_PASSWORD")
  conexion <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("LUPA_PG_PORT", "55432")),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"),
    user = Sys.getenv("LUPA_PG_USER", "postgres"), password = clave
  ), error = function(e) NULL)
  if (is.null(conexion)) testthat::skip("No se pudo abrir PostgreSQL")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  tabla <- "arrc_postgres"
  try(DBI::dbExecute(conexion, paste0("DROP TABLE ", tabla)), silent = TRUE)
  on.exit(try(DBI::dbExecute(conexion, paste0("DROP TABLE ", tabla)),
             silent = TRUE), add = TRUE)
  DBI::dbWriteTable(conexion, tabla, .arrc_datos())

  resultado <- suppressMessages(perfilar_dbi(
    conexion, tabla, metricas = c("basicos", "desvio"),
    universo = "tabla_completa", bloque_muestra = "solo_agregados",
    tamano_lote_planos = 2L, proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE, duplicados_aproximados = FALSE
  ))
  sql <- resultado$resumen_tabla$sql
  expect_equal(length(unique(sql$consulta_id[sql$etapa == "basicos"])), 2L)
  expect_equal(length(unique(sql$consulta_id[sql$etapa == "desvio"])), 2L)
  expect_equal(
    resultado$resumen_tabla$columnas$desvio[
      resultado$resumen_tabla$columnas$columna == "n1"
    ],
    sd(c(1, 1, 2, 3))
  )
})

test_that("PostgreSQL conserva el desvio de dobles extremos", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RPostgres")
  clave <- Sys.getenv("LUPA_PG_PASSWORD")
  if (!nzchar(clave)) testthat::skip("Sin LUPA_PG_PASSWORD")
  puertos <- c(55432L, 55493L)
  probadas <- 0L
  for (puerto in puertos) {
    conexion <- tryCatch(DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = puerto,
      dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"), user = "postgres",
      password = clave
    ), error = function(e) NULL)
    if (is.null(conexion)) next
    probadas <- probadas + 1L
    tabla <- paste0("integ_extremos_desvio_", puerto)
    tryCatch({
      try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbExecute(conexion, paste0(
        "CREATE TABLE ", tabla, " (id integer, f8 double precision)"
      ))
      DBI::dbExecute(conexion, paste0(
        "INSERT INTO ", tabla, " VALUES (1, ",
        sprintf("%.17g", .Machine$double.xmax), "), (2, ",
        sprintf("%.17g", -.Machine$double.xmax), "), (3, 0), (4, 1)"
      ))
      avisos <- character()
      resultado <- withCallingHandlers(
        perfilar_dbi(
          conexion, tabla, metricas = c("basicos", "desvio"),
          bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE,
          analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
          ausencia_estructural = FALSE, duplicados_aproximados = FALSE
        ),
        warning = function(x) {
          avisos <<- c(avisos, conditionMessage(x))
          invokeRestart("muffleWarning")
        }
      )
      fila <- resultado$resumen_tabla$columnas[
        resultado$resumen_tabla$columnas$columna == "f8", , drop = FALSE
      ]
      esperado <- .Machine$double.xmax * sqrt(2 / 3)
      expect_true(is.finite(fila$desvio))
      expect_equal(fila$desvio, esperado, tolerance = 1e-12)
      expect_equal(nrow(resultado$resumen_tabla$meta$consultas$fallas), 0L)
      expect_length(avisos, 0L)
    }, finally = {
      try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbDisconnect(conexion)
    })
  }
  if (probadas == 0L) testthat::skip("No hay PostgreSQL disponible")
})

test_that("SQL Server conserva el desvio de dobles extremos", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD")
  if (!nzchar(clave)) testthat::skip("Sin LUPA_MSSQL_PWD")
  conexion <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS", Server = "127.0.0.1", Port = 1433,
    UID = "sa", PWD = clave, Database = "lupa_pruebas",
    TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
  ), error = function(e) NULL)
  if (is.null(conexion)) testthat::skip("No se pudo abrir SQL Server")
  tabla <- "integ_extremos_desvio_sqlserver"
  tryCatch({
    try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbExecute(conexion, paste0(
      "CREATE TABLE ", tabla, " (id int, f8 float)"
    ))
    DBI::dbExecute(conexion, paste0(
      "INSERT INTO ", tabla, " VALUES (1, ",
      sprintf("%.17g", .Machine$double.xmax), "), (2, ",
      sprintf("%.17g", -.Machine$double.xmax), "), (3, 0), (4, 1)"
    ))
    avisos <- character()
    resultado <- withCallingHandlers(
      perfilar_dbi(
        conexion, tabla, metricas = c("basicos", "desvio"),
        universo = "tabla_completa", bloque_muestra = "solo_agregados",
        proteger_datos_personales = FALSE, analizar_dependencias = FALSE,
        casi_duplicados_vocabulario = FALSE, ausencia_estructural = FALSE,
        duplicados_aproximados = FALSE
      ),
      warning = function(x) {
        avisos <<- c(avisos, conditionMessage(x))
        invokeRestart("muffleWarning")
      }
    )
    fila <- resultado$resumen_tabla$columnas[
      resultado$resumen_tabla$columnas$columna == "f8", , drop = FALSE
    ]
    esperado <- .Machine$double.xmax * sqrt(2 / 3)
    expect_true(is.finite(fila$desvio))
    expect_equal(fila$desvio, esperado, tolerance = 1e-12)
    expect_equal(nrow(resultado$resumen_tabla$meta$consultas$fallas), 0L)
    expect_length(avisos, 0L)
  }, finally = {
    try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(conexion)
  })
})

.arrc2_datos <- function(n = 5000L, sqlserver = FALSE) {
  set.seed(6218873L)
  salida <- data.frame(
    id = seq_len(n),
    n_int = sample(c(NA_integer_, 0L:20L), n, replace = TRUE),
    f_dec = round(stats::rnorm(n, 10), 3),
    f_grande = 1e9 + stats::runif(n),
    f_chico = stats::rnorm(n) * 1e-8,
    f_neg = -abs(stats::rnorm(n, 3)),
    f_const = rep(7, n),
    f_cero = rep(0, n),
    f_cero_na = c(rep(0, n - 50L), rep(NA_real_, 50L)),
    d_dec = round(stats::rnorm(n, 10), 3),
    stringsAsFactors = FALSE
  )
  if (isTRUE(sqlserver)) salida$d_money <- round(stats::rnorm(n, 10), 3)
  salida
}

.arrc2_crear_tabla <- function(conexion, tabla, datos, sqlserver = FALSE) {
  try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
      silent = TRUE)
  tipos <- if (isTRUE(sqlserver)) {
    paste(
      "id int, n_int int, f_dec float, f_grande float, f_chico float,",
      "f_neg float, f_const float, f_cero float, f_cero_na float,",
      "d_dec decimal(12,3), d_money money"
    )
  } else {
    paste(
      "id integer, n_int integer, f_dec double precision,",
      "f_grande double precision, f_chico double precision,",
      "f_neg double precision, f_const double precision,",
      "f_cero double precision, f_cero_na double precision,",
      "d_dec decimal(12,3)"
    )
  }
  DBI::dbExecute(conexion, paste0("CREATE TABLE ", tabla, " (", tipos, ")"))
  DBI::dbAppendTable(conexion, tabla, datos)
  invisible(NULL)
}

.arrc2_conectar_pg <- function(puerto) {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RPostgres")
  clave <- Sys.getenv("LUPA_PG_PASSWORD", "lupa")
  tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
      port = puerto, dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"),
      user = Sys.getenv("LUPA_PG_USER", "postgres"), password = clave
    ),
    error = function(e) NULL
  )
}

.arrc2_conectar_mssql <- function() {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  if (!nzchar(clave)) return(NULL)
  tryCatch(
    DBI::dbConnect(
      odbc::odbc(), Driver = "FreeTDS",
      Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
      Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
      UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
      Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
      TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
    ),
    error = function(e) NULL
  )
}

.arrc2_comprobar_desvios <- function(conexion, tabla, datos, columnas) {
  argumentos <- list(
    metricas = c("basicos", "desvio"), universo = "tabla_completa",
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE, duplicados_aproximados = FALSE,
    max_consultas = Inf
  )
  por_lotes <- suppressMessages(do.call(
    perfilar_dbi,
    c(list(conexion, tabla, tamano_lote_planos = 3L), argumentos)
  ))
  por_columna <- suppressMessages(do.call(
    perfilar_dbi,
    c(list(conexion, tabla, tamano_lote_planos = 1L), argumentos)
  ))
  extraer <- function(resultado) {
    filas <- resultado$resumen_tabla$columnas
    stats::setNames(
      as.numeric(filas$desvio[match(columnas, filas$columna)]), columnas
    )
  }
  desvios_lote <- extraer(por_lotes)
  desvios_columna <- extraer(por_columna)
  testthat::expect_identical(desvios_lote, desvios_columna)
  sql_lote <- as.character(por_lotes$resumen_tabla$sql$sql)
  if (inherits(conexion, "PqConnection")) {
    testthat::expect_true(any(grepl(
      "STDDEV_SAMP\\(CAST\\(\\(CAST\\(\\\"f_dec\\\" AS DOUBLE PRECISION\\) - ",
      sql_lote, perl = TRUE
    ), na.rm = TRUE))
  } else {
    testthat::expect_true(any(grepl(
      "STDEV\\(CAST\\(\\(\\\"f_dec\\\" - [0-9.]+e[+-][0-9]+\\) AS FLOAT\\)",
      sql_lote, perl = TRUE
    ), na.rm = TRUE))
  }
  esperados <- vapply(
    columnas, function(campo) stats::sd(datos[[campo]], na.rm = TRUE),
    numeric(1L)
  )
  testthat::expect_equal(desvios_lote, esperados, tolerance = 1e-12)
  invisible(NULL)
}

test_that("PostgreSQL 16 conserva los desvios al agrupar", {
  testthat::skip_on_cran()
  conexion <- .arrc2_conectar_pg(55432L)
  if (is.null(conexion)) testthat::skip("No PostgreSQL 16 available")
  tabla <- "integ2_desvio_pg16"
  datos <- .arrc2_datos()
  columnas <- c(
    "n_int", "f_dec", "f_grande", "f_chico", "f_neg", "f_const",
    "f_cero", "f_cero_na", "d_dec"
  )
  tryCatch({
    .arrc2_crear_tabla(conexion, tabla, datos)
    .arrc2_comprobar_desvios(conexion, tabla, datos, columnas)
  }, finally = {
    try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(conexion)
  })
})

test_that("PostgreSQL 9.3 conserva los desvios al agrupar", {
  testthat::skip_on_cran()
  conexion <- .arrc2_conectar_pg(55493L)
  if (is.null(conexion)) testthat::skip("No PostgreSQL 9.3 available")
  tabla <- "integ2_desvio_pg93"
  datos <- .arrc2_datos()
  columnas <- c(
    "n_int", "f_dec", "f_grande", "f_chico", "f_neg", "f_const",
    "f_cero", "f_cero_na", "d_dec"
  )
  tryCatch({
    .arrc2_crear_tabla(conexion, tabla, datos)
    .arrc2_comprobar_desvios(conexion, tabla, datos, columnas)
  }, finally = {
    try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(conexion)
  })
})

test_that("SQL Server conserva los desvios al agrupar", {
  testthat::skip_on_cran()
  conexion <- .arrc2_conectar_mssql()
  if (is.null(conexion)) testthat::skip("No SQL Server available")
  tabla <- "integ2_desvio_sqlserver"
  datos <- .arrc2_datos(sqlserver = TRUE)
  columnas <- c(
    "n_int", "f_dec", "f_grande", "f_chico", "f_neg", "f_const",
    "f_cero", "f_cero_na", "d_dec", "d_money"
  )
  tryCatch({
    .arrc2_crear_tabla(conexion, tabla, datos, sqlserver = TRUE)
    .arrc2_comprobar_desvios(conexion, tabla, datos, columnas)
  }, finally = {
    try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(conexion)
  })
})
