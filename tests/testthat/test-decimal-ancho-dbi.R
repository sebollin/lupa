.adec_con_pg <- function(puerto) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = puerto, user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en ", puerto, "."))
  con
}

.adec_con_mssql <- function() {
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
  con
}

.adec_probar <- function(con, tabla) {
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  on.exit(try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE),
          add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla,
    " (id int, dec38 decimal(38,10), d20 decimal(20,0), chico decimal(10,2))"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ",
    "(1,9999999999999999999999999999.1234567890,12345678901234567890,1.25),",
    "(2,9999999999999999999999999999.1234567890,12345678901234567891,2.50),",
    "(3,NULL,NULL,NULL),(4,NULL,NULL,4.00)"
  ))
  for (metricas in list(NULL, c("validos", "distintos", "moda", "basicos", "desvio"))) {
    argumentos <- list(
      con, tabla, universo = "tabla_completa",
      bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
    )
    if (!is.null(metricas)) argumentos$metricas <- metricas
    resultado <- suppressWarnings(suppressMessages(
      do.call(perfilar_dbi, argumentos)
    ))
    columnas <- resultado$resumen_tabla$columnas
    sql <- resultado$resumen_tabla$sql
    for (columna in c("dec38", "d20")) {
      fila <- columnas[columnas$columna == columna, , drop = FALSE]
      expect_true(all(is.na(c(fila$minimo, fila$maximo, fila$media,
                              fila$desvio))))
      magnitud <- sql[sql$columna == columna &
                        sql$metrica %in% c("minimo", "maximo", "media",
                                           "desvio"), , drop = FALSE]
      expect_true(nrow(magnitud) >= 4L)
      expect_true(all(magnitud$estado == "no_disponible"))
      expect_true(all(grepl("2^53", magnitud$motivo, fixed = TRUE)))
      expect_false(any(grepl("no se pidio", magnitud$motivo, fixed = TRUE)))
    }
    if (!is.null(metricas)) {
      mediana <- sql[sql$columna == "dec38" & sql$metrica == "mediana", ,
                     drop = FALSE]
      expect_true(all(mediana$estado == "no_solicitado"))
    }
    chico <- columnas[columnas$columna == "chico", , drop = FALSE]
    expect_equal(as.numeric(chico$minimo), 1.25)
    expect_equal(as.numeric(chico$maximo), 4)
    expect_equal(as.numeric(columnas$n_distintos[columnas$columna == "d20"]), 2)
  }
}

test_that("un decimal de 2^53 o mas se declara por precision, con o sin metricas", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .adec_con_pg(puerto)
    .adec_probar(con, paste0("adec_pg_", puerto))
    DBI::dbDisconnect(con)
  }
  con <- .adec_con_mssql()
  .adec_probar(con, "adec_mssql")
  DBI::dbDisconnect(con)
})
