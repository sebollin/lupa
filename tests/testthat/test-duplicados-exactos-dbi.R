testthat::skip_on_cran()
skip_if_not_installed("DBI")

.de_conectar_pg <- function(puerto) {
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(puerto),
    user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en puerto ", puerto, "."))
  con
}

.de_conectar_mysql <- function(maria = FALSE) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  variable <- if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD"
  clave <- Sys.getenv(variable, unset = "")
  skip_if(!nzchar(clave), paste0("Sin ", variable, "."))
  puerto_def <- if (maria) "33306" else "33307"
  puerto_var <- if (maria) "LUPA_MARIADB_PORT" else "LUPA_MYSQL_PORT"
  puerto <- as.integer(Sys.getenv(puerto_var, Sys.getenv("LUPA_DB_PORT", puerto_def)))
  con <- tryCatch({
    if (maria) {
      DBI::dbConnect(
        RMariaDB::MariaDB(),
        host = Sys.getenv("LUPA_MARIADB_HOST", Sys.getenv("LUPA_DB_HOST", "127.0.0.1")),
        port = puerto,
        dbname = Sys.getenv("LUPA_MARIADB_DB", Sys.getenv("LUPA_DB_NAME", "lupa")),
        username = Sys.getenv("LUPA_MARIADB_USER", Sys.getenv("LUPA_DB_USER", "root")),
        password = clave
      )
    } else {
      DBI::dbConnect(
        RMySQL::MySQL(),
        host = Sys.getenv("LUPA_MYSQL_HOST", Sys.getenv("LUPA_DB_HOST", "127.0.0.1")),
        port = puerto,
        dbname = Sys.getenv("LUPA_MYSQL_DB", Sys.getenv("LUPA_DB_NAME", "lupa")),
        username = Sys.getenv("LUPA_MYSQL_USER", Sys.getenv("LUPA_DB_USER", "root")),
        password = clave
      )
    }
  }, error = function(e) NULL)
  skip_if(is.null(con), paste0(if (maria) "MariaDB" else "MySQL", " no disponible."))
  con
}

.de_conectar_mssql <- function() {
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(),
    Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"),
    PWD = clave,
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0",
    TrustServerCertificate = "yes",
    timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  con
}

test_that("SQLite aplica la regla canonica de duplicados exactos y coincide con R", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "CREATE TABLE lupa_garantias_de_sq (c CHAR(5), v VARCHAR(10) COLLATE NOCASE, f REAL)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_garantias_de_sq VALUES ",
    "('a', 'A', -0.0), ('a  ', 'A', 0.0), ('a', 'a', 0.0), ",
    "('p', 'x', 1.0), ('p', 'x  ', 1.0), ",
    "('z', NULL, NULL), ('z', NULL, NULL)"
  ))

  plan <- plan_perfilado_dbi(con, "lupa_garantias_de_sq", muestra = 100L)
  perf <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_de_sq", muestra = 100L, proteger_datos_personales = FALSE
  )))

  dup <- perf$resumen_tabla$meta$duplicados_exactos
  expect_identical(dup$estado, "calculado")
  expect_equal(dup$filas_duplicadas, 2)
  expect_equal(perf$perfil_muestra$general$filas_duplicadas, 2)
  expect_true(nzchar(dup$criterio))
  expect_identical(
    dup$regla_columnas$regla[dup$regla_columnas$columna == "c"],
    "char_sin_espacios_finales_binario"
  )
  expect_identical(
    dup$regla_columnas$regla[dup$regla_columnas$columna == "v"],
    "texto_binario"
  )
  expect_equal(perf$resumen_tabla$meta$consultas$emitidas, attr(plan, "total"))
})

test_that("DuckDB reconcilia decimales anchos, NOCASE, float y TIMESTAMP_NS", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_garantias_de_dk (",
    "v VARCHAR COLLATE NOCASE, d DECIMAL(25,20), f DOUBLE)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_garantias_de_dk VALUES ",
    "('A', 1.0, -0.0), ('A', 1.00, 0.0), ('a', 1.0, 0.0), ",
    "('x', 2.0, 1.0), ('x  ', 2.0, 1.0), ",
    "('D', 1.00000000000000000001, 2.0), ('D', 1.00000000000000000002, 2.0), ",
    "(NULL, NULL, 'NaN'::DOUBLE), (NULL, NULL, 'NaN'::DOUBLE)"
  ))

  perf <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_de_dk", muestra = 100L, proteger_datos_personales = FALSE
  )))
  expect_equal(perf$resumen_tabla$meta$duplicados_exactos$filas_duplicadas, 2)
  expect_equal(perf$perfil_muestra$general$filas_duplicadas, 2)

  # Sub-microsegundo TIMESTAMP_NS: SQL exacto = 1; muestra completa reconcilia a 1;
  # submuestra parcial no inventa un conteo redondeado (NA + cobertura)
  DBI::dbExecute(con, "CREATE TABLE lupa_garantias_de_ns (ts TIMESTAMP_NS)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_garantias_de_ns VALUES ",
    "('2026-01-01 12:00:00.000000001'), ",
    "('2026-01-01 12:00:00.000000002'), ",
    "('2026-01-01 12:00:00.000000001')"
  ))
  perf_full <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_de_ns", muestra = 100L, proteger_datos_personales = FALSE
  )))
  expect_equal(perf_full$resumen_tabla$meta$duplicados_exactos$filas_duplicadas, 1)
  expect_equal(perf_full$perfil_muestra$general$filas_duplicadas, 1)

  perf_sub <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_de_ns", muestra = 2L, proteger_datos_personales = FALSE
  )))
  expect_equal(perf_sub$resumen_tabla$meta$duplicados_exactos$filas_duplicadas, 1)
  expect_true(is.na(perf_sub$perfil_muestra$general$filas_duplicadas))
  expect_true(any(
    perf_sub$perfil_muestra$cobertura_diagnosticos$diagnostico == "filas_duplicadas"
  ))
})

.de_probar_pg <- function(puerto) {
  con <- .de_conectar_pg(puerto)
  limpiar <- function() {
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_garantias_de_pg"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_garantias_de_xml"), silent = TRUE)
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_garantias_de_pg (",
    "c CHAR(5), v VARCHAR(10), d NUMERIC(25,20), f DOUBLE PRECISION, ",
    "ts TIMESTAMP(6), tz TIMESTAMPTZ)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_garantias_de_pg VALUES ",
    "('a', 'A', 1.0, -0.0, '2026-01-01 12:00:00.000001', '2026-01-01 12:00:00+00'), ",
    "('a  ', 'A', 1.00, 0.0, '2026-01-01 12:00:00.000001', '2026-01-01 09:00:00-03'), ",
    "('a', 'a', 1.0, 0.0, '2026-01-01 12:00:00.000001', '2026-01-01 12:00:00+00'), ",
    "('p', 'x', 2.0, 1.0, '2026-01-01 12:00:00.000001', '2026-01-01 12:00:00+00'), ",
    "('p', 'x  ', 2.0, 1.0, '2026-01-01 12:00:00.000001', '2026-01-01 12:00:00+00'), ",
    "('d', 'D', 1.00000000000000000001, 2.0, '2026-01-01 12:00:00.000001', '2026-01-01 12:00:00+00'), ",
    "('d', 'D', 1.00000000000000000002, 2.0, '2026-01-01 12:00:00.000001', '2026-01-01 12:00:00+00'), ",
    "('h', 'H', 3.0, 3.0, '2026-01-01 12:00:00.000001', '2026-01-01 12:00:00+00'), ",
    "('h', 'H', 3.0, 3.0, '2026-01-01 12:00:00.000002', '2026-01-01 12:00:00+00'), ",
    "(NULL, NULL, NULL, 'NaN'::double precision, NULL, NULL), ",
    "(NULL, NULL, NULL, 'NaN'::double precision, NULL, NULL)"
  ))

  plan <- plan_perfilado_dbi(con, "lupa_garantias_de_pg", muestra = 100L)
  perf <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_de_pg", muestra = 100L, proteger_datos_personales = FALSE
  )))
  expect_equal(perf$resumen_tabla$meta$duplicados_exactos$filas_duplicadas, 2)
  expect_equal(perf$perfil_muestra$general$filas_duplicadas, 2)
  expect_gte(perf$resumen_tabla$meta$consultas$emitidas, attr(plan, "total"))
  expect_lte(perf$resumen_tabla$meta$consultas$emitidas, attr(plan, "total_maximo"))

  # Tipo no comparable (xml) -> NA con motivo explicito
  DBI::dbExecute(con, "CREATE TABLE lupa_garantias_de_xml (id INTEGER, doc XML)")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_de_xml VALUES (1, '<a/>'), (1, '<a/>')")
  perf_xml <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_de_xml", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))
  expect_true(is.na(perf_xml$resumen_tabla$meta$duplicados_exactos$filas_duplicadas))
  expect_identical(perf_xml$resumen_tabla$meta$duplicados_exactos$estado, "no_disponible")
  expect_match(perf_xml$resumen_tabla$meta$duplicados_exactos$motivo, "xml", fixed = TRUE)
}

test_that("PostgreSQL actual cuenta duplicados exactos igual que R y declara XML como NA", {
  .de_probar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
})

test_that("PostgreSQL 9.3 cuenta duplicados exactos igual que R y declara XML como NA", {
  .de_probar_pg(Sys.getenv("LUPA_PG93_PORT", "55493"))
})

.de_probar_mysql_maria <- function(maria = FALSE) {
  con <- .de_conectar_mysql(maria = maria)
  limpiar <- function() {
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_garantias_de_my"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_garantias_de_pt"), silent = TRUE)
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_garantias_de_my (",
    "c CHAR(5), v VARCHAR(10), d DECIMAL(25,20), ts DATETIME(6), f DOUBLE",
    ") CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_garantias_de_my VALUES ",
    "('a', 'A', 1.0, '2026-01-01 12:00:00.000001', -0.0), ",
    "('a  ', 'A', 1.00, '2026-01-01 12:00:00.000001', 0.0), ",
    "('a', 'a', 1.0, '2026-01-01 12:00:00.000001', 0.0), ",
    "('p', 'x', 2.0, '2026-01-01 12:00:00.000001', 1.0), ",
    "('p', 'x  ', 2.0, '2026-01-01 12:00:00.000001', 1.0), ",
    "('d', 'D', 1.00000000000000000001, '2026-01-01 12:00:00.000001', 2.0), ",
    "('d', 'D', 1.00000000000000000002, '2026-01-01 12:00:00.000001', 2.0), ",
    "('h', 'H', 3.0, '2026-01-01 12:00:00.000001', 3.0), ",
    "('h', 'H', 3.0, '2026-01-01 12:00:00.000002', 3.0), ",
    "(NULL, NULL, NULL, NULL, NULL), ",
    "(NULL, NULL, NULL, NULL, NULL)"
  ))

  plan <- plan_perfilado_dbi(con, "lupa_garantias_de_my", muestra = 100L)
  perf <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_de_my", muestra = 100L, proteger_datos_personales = FALSE
  )))
  expect_equal(perf$resumen_tabla$meta$duplicados_exactos$filas_duplicadas, 2)
  expect_equal(perf$perfil_muestra$general$filas_duplicadas, 2)
  expect_gte(perf$resumen_tabla$meta$consultas$emitidas, attr(plan, "total"))
  expect_lte(perf$resumen_tabla$meta$consultas$emitidas, attr(plan, "total_maximo"))

  DBI::dbExecute(con, "CREATE TABLE lupa_garantias_de_pt (id INT, g POINT)")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_de_pt VALUES (1, ST_GeomFromText('POINT(1 1)')), (1, ST_GeomFromText('POINT(1 1)'))")
  perf_pt <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_de_pt", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))
  expect_true(is.na(perf_pt$resumen_tabla$meta$duplicados_exactos$filas_duplicadas))
  expect_identical(perf_pt$resumen_tabla$meta$duplicados_exactos$estado, "no_disponible")
}

test_that("MySQL vence _ci y PAD SPACE y coincide con R en duplicados exactos", {
  .de_probar_mysql_maria(maria = FALSE)
})

test_that("MariaDB vence _ci y PAD SPACE y coincide con R en duplicados exactos", {
  .de_probar_mysql_maria(maria = TRUE)
})

test_that("SQL Server vence _CI_ y PAD SPACE, reconcilia DATETIME2(7) y declara XML NA", {
  con <- .de_conectar_mssql()
  limpiar <- function() {
    try(DBI::dbExecute(con, "IF OBJECT_ID(N'lupa_garantias_de_ms', N'U') IS NOT NULL DROP TABLE lupa_garantias_de_ms"), silent = TRUE)
    try(DBI::dbExecute(con, "IF OBJECT_ID(N'lupa_garantias_de_dt2', N'U') IS NOT NULL DROP TABLE lupa_garantias_de_dt2"), silent = TRUE)
    try(DBI::dbExecute(con, "IF OBJECT_ID(N'lupa_garantias_de_xml', N'U') IS NOT NULL DROP TABLE lupa_garantias_de_xml"), silent = TRUE)
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_garantias_de_ms (",
    "c CHAR(5), v VARCHAR(10), d DECIMAL(25,20), tz DATETIMEOFFSET(3))"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_garantias_de_ms VALUES ",
    "('a', 'A', 1.0, '2026-01-01 12:00:00.123+00:00'), ",
    "('a  ', 'A', 1.00, '2026-01-01 09:00:00.123-03:00'), ",
    "('a', 'a', 1.0, '2026-01-01 12:00:00.123+00:00'), ",
    "('p', 'x', 2.0, '2026-01-01 12:00:00.123+00:00'), ",
    "('p', 'x  ', 2.0, '2026-01-01 12:00:00.123+00:00'), ",
    "('d', 'D', 1.00000000000000000001, '2026-01-01 12:00:00.123+00:00'), ",
    "('d', 'D', 1.00000000000000000002, '2026-01-01 12:00:00.123+00:00'), ",
    "(NULL, NULL, NULL, NULL), ",
    "(NULL, NULL, NULL, NULL)"
  ))

  plan <- plan_perfilado_dbi(con, "lupa_garantias_de_ms", muestra = 100L)
  perf <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_de_ms", muestra = 100L, proteger_datos_personales = FALSE
  )))
  expect_equal(perf$resumen_tabla$meta$duplicados_exactos$filas_duplicadas, 2)
  expect_equal(perf$perfil_muestra$general$filas_duplicadas, 2)
  expect_equal(perf$resumen_tabla$meta$consultas$emitidas, attr(plan, "total"))

  DBI::dbExecute(con, "CREATE TABLE lupa_garantias_de_dt2 (ts DATETIME2(7))")
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_garantias_de_dt2 VALUES ",
    "('2026-01-01 12:00:00.0000001'), ",
    "('2026-01-01 12:00:00.0000002'), ",
    "('2026-01-01 12:00:00.0000001')"
  ))
  perf_full <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_de_dt2", muestra = 100L, proteger_datos_personales = FALSE
  )))
  expect_equal(perf_full$resumen_tabla$meta$duplicados_exactos$filas_duplicadas, 1)
  expect_equal(perf_full$perfil_muestra$general$filas_duplicadas, 1)

  perf_sub <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_de_dt2", muestra = 2L, proteger_datos_personales = FALSE
  )))
  expect_equal(perf_sub$resumen_tabla$meta$duplicados_exactos$filas_duplicadas, 1)
  expect_true(is.na(perf_sub$perfil_muestra$general$filas_duplicadas))

  DBI::dbExecute(con, "CREATE TABLE lupa_garantias_de_xml (id INT, doc XML)")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_de_xml VALUES (1, '<a/>'), (1, '<a/>')")
  perf_xml <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_de_xml", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))
  expect_true(is.na(perf_xml$resumen_tabla$meta$duplicados_exactos$filas_duplicadas))
  expect_identical(perf_xml$resumen_tabla$meta$duplicados_exactos$estado, "no_disponible")
})
