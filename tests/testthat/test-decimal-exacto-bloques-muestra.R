# Por `bloque_filas` y por el spool de la muestra del motor, R recibe el doble
# de cada decimal. La media y la mediana de esos dobles no son las del decimal
# -la de 0,0001 y 0,0002 salia 0x1.3a92a30553262p-13, y la exacta es
# 0x1.3a92a30553261p-13-, y el desvio de un decimal de 5e14 con centavos, de
# los dobles, se aparta una parte en 300: se negaba. Ahora la media y la
# mediana son el doble mas cercano de las del decimal, y el desvio se resta en
# el decimal: por bloques lo calcula el motor, y por el spool sale del texto de
# cada valor.
#
# Las esperadas son exactas: fracciones sobre el texto de cada valor,
# redondeadas al doble mas cercano fuera de R y escritas en hexadecimal; el
# desvio, la raiz de la varianza exacta.
testthat::skip_on_cran()


test_that("DuckDB: la media, la mediana y el desvio de un decimal por el spool son los del decimal", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  # DuckDB no tiene lectura por bloques.
  .dexa_probar(con, "dexa", bloques = FALSE)
})

test_that("PostgreSQL: la media, la mediana y el desvio de un numeric por bloques y por el spool", {
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
  .dexa_probar(con, "lupa_dexa_pg", decimal = "NUMERIC")
})

test_that("SQL Server: la media, la mediana y el desvio de un decimal por bloques y por el spool", {
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
  .dexa_probar(con, "lupa_dexa_mssql", decimal = "decimal")
  # `money`: odbc no lee el valor al doble mas cercano, y la central de una
  # cantidad impar tambien se pide al motor como texto. La suma de tres
  # valores cerca del tope del tipo, como `decimal(38,4)`.
  try(DBI::dbExecute(con, "DROP TABLE lupa_dexa_mssql_m"), silent = TRUE)
  DBI::dbExecute(con, "CREATE TABLE lupa_dexa_mssql_m (m money)")
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_dexa_mssql_m VALUES (500000000000009.1534),",
    "(500000000000006.1987), (500000000000005.5911)"
  ))
  on.exit(try(DBI::dbExecute(con, "DROP TABLE lupa_dexa_mssql_m"),
              silent = TRUE), add = TRUE, after = FALSE)
  caso <- list(filas = 3L, columnas = list(m = list(
    media = 0x1.c6bf526340070p+48, mediana = 0x1.c6bf526340063p+48
  )))
  for (via in list(
    list(bloque_filas = 2L, bloque_muestra = "solo_agregados"),
    list(universo = "muestra_motor", muestra_motor = 3L)
  )) {
    do.call(.dexa_comprobar, c(list(con, "lupa_dexa_mssql_m", caso), via))
  }
})


test_that("el plan cuenta la prueba de la CTE que la muestra corre por un decimal", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  # Lo que agrega un decimal frente a un doble, en el plan y en la corrida: su
  # mediana deja la consolidada (una consulta) y la muestra prueba la CTE de
  # ventanas (la prueba y su control negativo), que el plan no contaba.
  contar <- function(tipo) {
    try(DBI::dbExecute(con, "DROP TABLE dexa_plan"), silent = TRUE)
    DBI::dbExecute(con, paste0("CREATE TABLE dexa_plan (d ", tipo, ", f DOUBLE)"))
    DBI::dbExecute(con, paste0(
      "INSERT INTO dexa_plan SELECT CAST(CAST(i AS DECIMAL(18,2)) / 100 AS ",
      tipo, "), i * 0.5 FROM range(1, 4001) AS t(i)"
    ))
    argumentos <- list(
      con, "dexa_plan", universo = "muestra_motor", muestra_motor = 100L,
      bloque_muestra = "solo_agregados"
    )
    plan <- suppressWarnings(suppressMessages(do.call(
      plan_perfilado_dbi, argumentos
    )))
    perfil <- suppressWarnings(suppressMessages(do.call(perfilar_dbi, c(
      argumentos, list(proteger_datos_personales = FALSE)
    ))))
    c(plan = as.numeric(attr(plan, "total")),
      corrida = as.numeric(perfil$resumen_tabla$meta$consultas$emitidas))
  }
  decimal <- contar("DECIMAL(18,2)")
  doble <- contar("DOUBLE")
  expect_identical(
    decimal[["plan"]] - doble[["plan"]],
    decimal[["corrida"]] - doble[["corrida"]]
  )
  expect_identical(decimal[["corrida"]] - doble[["corrida"]], 3)
})
