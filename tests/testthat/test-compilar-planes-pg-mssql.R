# Pruebas de compilacion para PostgreSQL y SQL Server cuando el servicio existe.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.compilar_prueba_conectar_pg <- function(puerto) {
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(puerto), user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"), bigint = "integer64"
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en puerto ", puerto, "."))
  con
}

.compilar_prueba_conectar_mssql <- function() {
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin clave de SQL Server.")
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

.compilar_prueba_probar_motor <- function(con, motor, tabla) {
  tipo_doble <- if (identical(motor, "sqlserver")) "FLOAT" else "DOUBLE PRECISION"
  borrar <- if (identical(motor, "sqlserver")) {
    paste0(
      "IF OBJECT_ID('dbo.", tabla,
      "', 'U') IS NOT NULL DROP TABLE ", tabla
    )
  } else paste0("DROP TABLE IF EXISTS ", tabla)
  on.exit({
    try(DBI::dbExecute(con, borrar), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(con, borrar)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (id INTEGER NOT NULL, x ", tipo_doble,
    " NOT NULL, txt VARCHAR(32))"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste0(
      "(", seq_len(20L), ", ", seq_len(20L), ", 'v",
      seq_len(20L) %% 4L, "')", collapse = ", "
    )
  ))
  mod <- modelo(instanciar(
    especializar(metricas_nucleo()$NoNulo, "compilar_NN"), tabla, "x"
  ))

  perfil <- plan_perfilado_dbi(
    con, tabla, universo = "tabla_completa", metricas = "validos",
    bloque_muestra = "solo_agregados", incluir_valores = FALSE,
    compilar = TRUE
  )
  perfil_compilacion <- attr(perfil, "compilacion", exact = TRUE)
  expect_true(any(perfil_compilacion$estado == "compilada"))
  expect_false(any(perfil_compilacion$estado == "no_compilada"))
  expect_false(any(grepl(
    "no se pudo construir todas", perfil_compilacion$motivo,
    fixed = TRUE, useBytes = TRUE
  ), na.rm = TRUE))

  medicion <- plan_medicion_dbi(
    mod, con, tablas = stats::setNames(tabla, tabla), compilar = TRUE
  )
  medicion_compilacion <- attr(medicion, "compilacion", exact = TRUE)
  expect_true(any(medicion_compilacion$estado == "compilada"))
  expect_false(any(medicion_compilacion$estado == "no_compilada"))

  casos <- list(
    funcion = paste0("SELECT lupa_funcion_inexistente(x) FROM ", tabla),
    columna = paste0("SELECT lupa_columna_inexistente FROM ", tabla)
  )
  for (nombre in names(casos)) {
    compilado <- lupa:::.with_compilador_dbi(con, motor, {
      lupa:::.compilar_sql_dbi(con, casos[[nombre]], nombre, motor)
    })
    registro <- compilado$compilacion
    expect_identical(registro$estado[[1L]], "no_compilada")
    expect_true(nzchar(registro$motivo[[1L]]))
  }
}

test_that("PostgreSQL compila planes y conserva motivos de fallos", {
  con <- .compilar_prueba_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  .compilar_prueba_probar_motor(con, "postgresql", "lupa_compilar_pg")
})

test_that("PostgreSQL 9.3 compila planes y conserva motivos de fallos", {
  con <- .compilar_prueba_conectar_pg(Sys.getenv("LUPA_PG93_PORT", "55493"))
  .compilar_prueba_probar_motor(con, "postgresql", "lupa_compilar_pg93")
})

test_that("SQL Server compila con NOEXEC sin ejecutar", {
  con <- .compilar_prueba_conectar_mssql()
  .compilar_prueba_probar_motor(con, "sqlserver", "lupa_compilar_mssql")
  con_sonda <- .compilar_prueba_conectar_mssql()
  on.exit(try(DBI::dbDisconnect(con_sonda), silent = TRUE), add = TRUE)
  tiempo <- system.time({
    compilado <- lupa:::.with_compilador_dbi(con_sonda, "sqlserver", {
      lupa:::.compilar_sql_dbi(
        con_sonda, "WAITFOR DELAY '00:00:01'", "no_ejecuta", "sqlserver"
      )
    })
  })[["elapsed"]]
  registro <- compilado$compilacion
  expect_identical(registro$estado[[1L]], "compilada")
  expect_identical(registro$metodo[[1L]], "SET NOEXEC ON")
  expect_lt(tiempo, 0.8)
})
