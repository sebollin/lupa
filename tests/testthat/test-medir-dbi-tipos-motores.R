testthat::skip_on_cran()
skip_if_not_installed("DBI")

test_that("SQLite y DuckDB reproducen tipos habilitados y conservan la guarda Unicode", {
  skip_if_not_installed("RSQLite")
  con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con_sqlite), add = TRUE)
  nuc <- metricas_nucleo()
  mod_float <- modelo(.mdt_inst(
    nuc$ValoresPosiblesPorExtension, "FLOAT4_sqlite", "d", "x", valores = 0.3
  ))
  .mdt_comparar(
    con_sqlite, "lupa_medir_sqlite_f", "x REAL", "(0.3), (0.1), (NULL)",
    mod_float
  )
  acento <- intToUtf8(0x00E1L)
  enie <- intToUtf8(0x00F1L)
  may_acento <- intToUtf8(0x00C1L)
  may_enie <- intToUtf8(0x00D1L)
  mod_texto <- modelo(.mdt_inst(
    nuc$NoNulo, "TEXTO_sqlite", "d", "x",
    valores_nulos = c(acento, enie)
  ))
  valores <- paste0(
    "('", may_acento, "'), ('", acento, "'), ('", may_enie,
    "'), ('", enie, "'), ('dato'), (NULL)"
  )
  .mdt_comparar(
    con_sqlite, "lupa_medir_sqlite_t", "x TEXT COLLATE BINARY", valores,
    mod_texto
  )
  kelvin <- intToUtf8(0x212AL)
  mod_kelvin <- modelo(.mdt_inst(
    nuc$NoNulo, "TEXTO_kelvin", "d", "x", valores_nulos = "k"
  ))
  .mdt_no_medible(
    con_sqlite, "lupa_medir_sqlite_k", "x TEXT COLLATE BINARY",
    paste0("('", kelvin, "'), ('k'), (NULL)"), mod_kelvin
  )

  skip_if_not_installed("duckdb")
  con_duck <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con_duck, shutdown = TRUE), add = TRUE)
  .mdt_comparar(
    con_duck, "lupa_medir_duck_t", "x TEXT", valores, mod_texto
  )
})

.mdt_pg <- function(puerto) {
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(puerto), user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"), bigint = "integer64"
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en puerto ", puerto, "."))
  con
}

test_that("PostgreSQL reproduce float4, float8 bajo y timestamptz por epoca", {
  con <- .mdt_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  on.exit({
    try(DBI::dbExecute(con, "SET TIME ZONE 'UTC'"), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  nuc <- metricas_nucleo()
  mod_f4 <- modelo(.mdt_inst(
    nuc$ValoresPosiblesPorExtension, "FLOAT4_pg", "d", "x", valores = 0.3
  ))
  .mdt_comparar(
    con, "lupa_medir_pg_f", "x REAL", "(0.3), (0.1), (NULL)", mod_f4
  )
  DBI::dbExecute(con, "SET extra_float_digits = 0")
  mod_f8 <- modelo(.mdt_inst(
    nuc$ValoresPosiblesPorExtension, "FLOAT8_pg", "d", "x",
    valores = 1.2345678901234567
  ))
  .mdt_comparar(
    con, "lupa_medir_pg_8", "x DOUBLE PRECISION",
    "('1.2345678901234567'), ('1.234567890123456'), (NULL)", mod_f8
  )
  mod_tz <- modelo(.mdt_inst(
    nuc$AtributoDuplicado, "TSTZ_pg", "d", "x"
  ))
  .mdt_comparar(
    con, "lupa_medir_pg_tz", "x TIMESTAMPTZ",
    "('2040-01-01 00:00:00-03'), ('2040-01-01 03:00:00+00'), (NULL)",
    mod_tz, sesion = "SET TIME ZONE 'America/Montevideo'"
  )
  acento <- intToUtf8(0x00E1L)
  enie <- intToUtf8(0x00F1L)
  may_acento <- intToUtf8(0x00C1L)
  may_enie <- intToUtf8(0x00D1L)
  mod_texto <- modelo(.mdt_inst(
    nuc$NoNulo, "TEXTO_pg", "d", "x", valores_nulos = c(acento, enie)
  ))
  valores <- paste0(
    "('", may_acento, "'), ('", acento, "'), ('", may_enie,
    "'), ('", enie, "'), ('dato'), (NULL)"
  )
  .mdt_comparar(
    con, "lupa_medir_pg_t", "x TEXT COLLATE \"C\"", valores, mod_texto
  )
})

.mdt_mssql <- function() {
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  con
}

test_that("SQL Server reproduce DATETIMEOFFSET por epoca y texto Unicode", {
  con <- .mdt_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  nuc <- metricas_nucleo()
  mod_dto <- modelo(.mdt_inst(
    nuc$AtributoDuplicado, "DTO_mssql", "d", "x"
  ))
  .mdt_comparar(
    con, "lupa_medir_mssql_d", "x DATETIMEOFFSET",
    "('2040-01-01 00:00:00 -03:00'), ('2040-01-01 03:00:00 +00:00'), (NULL)",
    mod_dto
  )
  acento <- intToUtf8(0x00E1L)
  enie <- intToUtf8(0x00F1L)
  may_acento <- intToUtf8(0x00C1L)
  may_enie <- intToUtf8(0x00D1L)
  mod_texto <- modelo(.mdt_inst(
    nuc$NoNulo, "TEXTO_mssql", "d", "x", valores_nulos = c(acento, enie)
  ))
  valores <- paste0(
    "(N'", may_acento, "'), (N'", acento, "'), (N'", may_enie,
    "'), (N'", enie, "'), (N'dato'), (NULL)"
  )
  .mdt_comparar(
    con, "lupa_medir_mssql_t", "x NVARCHAR(100) COLLATE Latin1_General_100_BIN2",
    valores, mod_texto
  )
})
