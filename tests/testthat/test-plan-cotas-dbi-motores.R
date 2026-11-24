testthat::skip_on_cran()
testthat::skip_if_not_installed("DBI")

.cotas_pg <- function(puerto) {
  testthat::skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(puerto), user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"), bigint = "integer64"
  ), error = function(e) NULL)
  testthat::skip_if(is.null(con), "PostgreSQL no disponible.")
  con
}

.cotas_mssql <- function() {
  testthat::skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  testthat::skip_if(!nzchar(clave), "Sin clave de SQL Server.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
  ), error = function(e) NULL)
  testthat::skip_if(is.null(con), "SQL Server no disponible.")
  con
}

.cotas_motor_comprobar <- function(con, tipo, infinito = FALSE) {
  tabla <- "lupa_plan_cotas_motor"
  quitar <- function() suppressWarnings(try(
    DBI::dbExecute(con, paste("DROP TABLE", tabla)), silent = TRUE
  ))
  quitar()
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla,
    " (id INTEGER, x ", tipo, ", c BIGINT, s VARCHAR(20))"
  ))
  filas <- paste0(
    "(", 1:12, ", ", 1:12 / 4, ", ",
    sprintf("%.0f", 9007199254740992 + 2 * 1:12), ", 'v", 1:12 %% 3L, "')"
  )
  DBI::dbExecute(con, paste(
    "INSERT INTO", tabla, "VALUES", paste(filas, collapse = ", ")
  ))
  if (isTRUE(infinito)) DBI::dbExecute(con, paste(
    "INSERT INTO", tabla, "VALUES (13, CAST('Infinity' AS DOUBLE PRECISION), 13, 'v1')"
  ))
  on.exit(quitar(), add = TRUE)
  configuraciones <- list(
    completa = list(bloque_muestra = "con_muestra"),
    completa_solo = list(bloque_muestra = "solo_agregados"),
    bloques = list(bloque_muestra = "solo_agregados", bloque_filas = 4L),
    muestra = list(
      universo = "muestra_motor", muestra_motor = 6L,
      bloque_muestra = "solo_agregados"
    )
  )
  comprobaciones <- 0L
  for (configuracion in configuraciones) {
    argumentos <- c(list(
      conexion = con, tabla = tabla, metricas = "mediana",
      fecha = as.POSIXct("2024-01-01", tz = "UTC")
    ), configuracion)
    argumentos_plan <- argumentos[setdiff(names(argumentos), "fecha")]
    plan <- suppressWarnings(suppressMessages(do.call(
      plan_perfilado_dbi, argumentos_plan
    )))
    perfil <- suppressWarnings(suppressMessages(do.call(
      perfilar_dbi, argumentos
    )))
    emitidas <- as.numeric(perfil$resumen_tabla$meta$consultas$emitidas)
    testthat::expect_gte(
      emitidas, as.numeric(attr(plan, "total_minimo")),
      label = paste(names(configuracion), "minimo")
    )
    testthat::expect_lte(
      emitidas, as.numeric(attr(plan, "total_maximo")),
      label = paste(names(configuracion), "maximo")
    )
    comprobaciones <- comprobaciones + 2L
  }
  cat("comprobaciones de cotas: ", comprobaciones, "\n", sep = "")
}

testthat::test_that("PostgreSQL mantiene el emitido dentro de las cotas", {
  motor <- getOption("lupa_plan_cotas_motor")
  if (!is.null(motor)) testthat::skip_if(
    !motor %in% c("pg", "pg93"), "Prueba reservada para PostgreSQL."
  )
  con <- .cotas_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .cotas_motor_comprobar(con, "DOUBLE PRECISION", infinito = TRUE)
})

testthat::test_that("SQL Server mantiene el emitido dentro de las cotas", {
  motor <- getOption("lupa_plan_cotas_motor")
  if (!is.null(motor)) testthat::skip_if(
    !identical(motor, "mssql"), "Prueba reservada para SQL Server."
  )
  con <- .cotas_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .cotas_motor_comprobar(con, "FLOAT")
})
