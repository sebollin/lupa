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
