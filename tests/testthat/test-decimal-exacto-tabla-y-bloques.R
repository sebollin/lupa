# La media y el desvio de un decimal son los dobles mas cercanos de los
# exactos, por la tabla completa, por bloques y por el spool de la muestra.
#
# Lo que pasaba: por `bloque_filas` el desvio de 500000000000006.60 y vecinos se
# pedia al motor centrado en la media redondeada, con las diferencias como
# dobles, y salia a un ulp (`0x1.b534070e9620dp+0`); por la tabla completa,
# MySQL, MariaDB y SQL Server promediaban el doble de cada valor y con valores
# que se cancelan la media salia 0,00333 por 0,005; y el desvio de la tabla
# completa, y la media con escala 30, quedaban a un ulp en los cinco motores
# con decimales.
#
# Las esperadas son exactas: fracciones sobre el texto de cada valor,
# redondeadas al doble mas cercano fuera de R y escritas en hexadecimal.
#
# El desvio se suma en el motor centrado en un literal que sale de los datos:
# con `incluir_valores = FALSE` el SQL publicado no lo lleva.
testthat::skip_on_cran()

test_that("DuckDB: la media y el desvio de un decimal, por la tabla completa y el spool", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  .dext_probar(con, "dext", "DECIMAL(38,%d)", bloques = FALSE)
})

test_that("PostgreSQL: la media y el desvio de un numeric, por las tres vias", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("LUPA_PG_PORT", "55432")),
    user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(con), "PostgreSQL no disponible.")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dext_probar(con, "lupa_dext_pg", "NUMERIC(38,%d)")
})

test_that("SQL Server: la media y el desvio de un decimal y de money, por las tres vias", {
  skip_if_not_installed("DBI")
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dext_probar(con, "lupa_dext_mssql", "DECIMAL(38,%d)")
  # `money`, que odbc no lee al doble mas cercano.
  caso <- list(
    valores = c("500000000000006.6000", "500000000000009.6000",
                "500000000000005.6000", "500000000000007.6000"),
    media = 0x1.c6bf526340076p+48, desvio = 0x1.b534070e9620cp+0
  )
  try(DBI::dbExecute(con, "DROP TABLE lupa_dext_mssql_m"), silent = TRUE)
  DBI::dbExecute(con, "CREATE TABLE lupa_dext_mssql_m (x money)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_dext_mssql_m VALUES ",
    paste0("(", caso$valores, ")", collapse = ", ")
  ))
  .dext_comprobar(con, "lupa_dext_mssql_m", caso, "money")
  .dext_comprobar(con, "lupa_dext_mssql_m", caso, "money", bloque_filas = 2L,
                  bloque_muestra = "solo_agregados")
  try(DBI::dbExecute(con, "DROP TABLE lupa_dext_mssql_m"), silent = TRUE)
})
