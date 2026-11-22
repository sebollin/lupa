# La conexion despues de compilar un plan en PostgreSQL y SQL Server: la sesion
# y la transaccion del usuario quedan como estaban. Se saltean si el motor no
# esta disponible en el entorno local.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.cses_conectar_pg <- function(puerto) {
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

.cses_conectar_mssql <- function() {
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

# Corre `codigo` y devuelve su valor con los avisos que dio, sin dejarlos
# pasar: la prueba afirma sobre ellos.
.cses_con_avisos <- function(codigo) {
  avisos <- character()
  valor <- withCallingHandlers(codigo, warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(valor = valor, avisos = avisos)
}

# ---- PostgreSQL ------------------------------------------------------------

.cses_pg_ajustes <- function(con) {
  DBI::dbGetQuery(con, "SELECT name, setting, source FROM pg_settings ORDER BY name")
}

.cses_probar_pg <- function(puerto, sufijo) {
  con <- .cses_conectar_pg(puerto)
  otra <- .cses_conectar_pg(puerto)
  tabla <- paste0("lupa_compilar_sesion_", sufijo)
  vista <- paste0("lupa_compilar_lenta_", sufijo)
  funcion <- paste0("lupa_compilar_dormir_", sufijo)
  borrar <- function() {
    for (s in c(paste0("DROP VIEW IF EXISTS ", vista),
                paste0("DROP FUNCTION IF EXISTS ", funcion, "(integer)"),
                paste0("DROP TABLE IF EXISTS ", tabla))) {
      try(suppressMessages(DBI::dbExecute(otra, s)), silent = TRUE)
    }
  }
  on.exit({
    borrar()
    try(DBI::dbDisconnect(con), silent = TRUE)
    try(DBI::dbDisconnect(otra), silent = TRUE)
  }, add = TRUE)
  borrar()
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (id INTEGER, x DOUBLE PRECISION, t VARCHAR(8))"))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " SELECT g, g / 4.0, CASE WHEN g % 3 = 0 THEN NULL ELSE 'v' || (g % 5) END",
    " FROM generate_series(1, 40) g"
  ))
  # Una funcion mal declarada IMMUTABLE: el planificador la ejecuta al plegar
  # constantes, y duerme mas que el tope de la compilacion.
  DBI::dbExecute(con, paste0(
    "CREATE FUNCTION ", funcion, "(integer) RETURNS integer IMMUTABLE LANGUAGE plpgsql AS ",
    "$$ BEGIN PERFORM pg_sleep(3); RETURN $1; END $$"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE VIEW ", vista, " AS SELECT id, x, ", funcion, "(1) AS c FROM ", tabla
  ))

  # La sesion queda igual: todo `pg_settings`, valor y origen.
  antes <- .cses_pg_ajustes(con)
  plan <- plan_perfilado_dbi(con, tabla, compilar = TRUE)
  expect_identical(.cses_pg_ajustes(con), antes)
  expect_identical(sum(attr(plan, "compilacion")$estado == "no_compilada"), 0L)

  # Una sentencia que el planificador ejecuta se corta en el tope y queda sin
  # verificar, no compilada ni colgada.
  lenta <- paste0("SELECT MAX(c) AS m FROM ", vista)
  tiempo <- system.time(compilado <- .with_compilador_dbi(con, "postgresql", {
    .compilar_sql_dbi(con, lenta, "x", "postgresql")
  }))[["elapsed"]]
  expect_identical(compilado$compilacion$estado, "no_verificada")
  expect_true(grepl("2000 ms", compilado$compilacion$motivo, fixed = TRUE))
  expect_lt(tiempo, 2.9)
  expect_identical(.cses_pg_ajustes(con), antes)

  # Con un tope propio menor en la sesion, manda el de la sesion, y despues
  # sigue puesto.
  DBI::dbExecute(con, "SET statement_timeout = 500")
  propio <- .cses_pg_ajustes(con)
  tiempo <- system.time(compilado <- .with_compilador_dbi(con, "postgresql", {
    .compilar_sql_dbi(con, lenta, "x", "postgresql")
  }))[["elapsed"]]
  expect_identical(compilado$compilacion$estado, "no_verificada")
  expect_true(grepl("500 ms", compilado$compilacion$motivo, fixed = TRUE))
  expect_lt(tiempo, 1.5)
  expect_identical(.cses_pg_ajustes(con), propio)
  DBI::dbExecute(con, "RESET statement_timeout")

  # Dentro de una transaccion del usuario: su fila pendiente sigue, la sesion
  # queda igual y la transaccion sigue abierta hasta su `COMMIT`.
  DBI::dbBegin(con)
  DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES (1000, 1, 'tx')"))
  antes <- .cses_pg_ajustes(con)
  plan <- plan_perfilado_dbi(con, tabla, compilar = TRUE)
  compilado <- .with_compilador_dbi(con, "postgresql", {
    .compilar_sql_dbi(con, lenta, "x", "postgresql")
  })
  expect_identical(compilado$compilacion$estado, "no_verificada")
  expect_identical(.cses_pg_ajustes(con), antes)
  expect_equal(as.numeric(DBI::dbGetQuery(
    con, paste0("SELECT COUNT(*) AS n FROM ", tabla, " WHERE id = 1000")
  )$n), 1)
  expect_equal(as.numeric(DBI::dbGetQuery(
    otra, paste0("SELECT COUNT(*) AS n FROM ", tabla, " WHERE id = 1000")
  )$n), 0)
  DBI::dbCommit(con)
  expect_equal(as.numeric(DBI::dbGetQuery(
    otra, paste0("SELECT COUNT(*) AS n FROM ", tabla, " WHERE id = 1000")
  )$n), 1)
}

test_that("PostgreSQL: compilar corta en el tope lo que el planificador ejecuta y deja la sesion como estaba", {
  .cses_probar_pg(Sys.getenv("LUPA_PG_PORT", "55432"), "pg")
})

test_that("PostgreSQL 9.3: compilar corta en el tope lo que el planificador ejecuta y deja la sesion como estaba", {
  .cses_probar_pg(Sys.getenv("LUPA_PG93_PORT", "55493"), "pg93")
})

# EXPLAIN y no PREPARE: PREPARE no mira los permisos, que PostgreSQL comprueba
# al iniciar el ejecutor; una columna sin permiso tiene que quedar sin compilar.
test_that("PostgreSQL: una columna sin permiso no compila", {
  con <- .cses_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  tabla <- "lupa_compilar_permiso_pg"
  rol <- "lupa_compilar_rol_pg"
  limpiar <- function() {
    try(DBI::dbExecute(con, "RESET ROLE"), silent = TRUE)
    try(suppressMessages(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))), silent = TRUE)
    try(suppressMessages(DBI::dbExecute(con, paste0("DROP ROLE IF EXISTS ", rol))), silent = TRUE)
  }
  on.exit({
    limpiar()
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  limpiar()
  creado <- tryCatch({
    DBI::dbExecute(con, paste0("CREATE ROLE ", rol))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!creado, "La credencial no puede crear roles.")
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (id INTEGER, secreto INTEGER)"))
  DBI::dbExecute(con, paste0("GRANT SELECT (id) ON ", tabla, " TO ", rol))
  DBI::dbExecute(con, paste0("SET ROLE ", rol))
  compilado <- .with_compilador_dbi(con, "postgresql", {
    .compilar_sql_dbi(con, paste0("SELECT id FROM ", tabla), "permitida", "postgresql")
    .compilar_sql_dbi(con, paste0("SELECT secreto FROM ", tabla), "sin_permiso", "postgresql")
  })
  expect_identical(compilado$compilacion$estado, c("compilada", "no_compilada"))
})

# ---- SQL Server ------------------------------------------------------------

.cses_mssql_tabla <- function(con, tabla) {
  borrar <- paste0("IF OBJECT_ID('dbo.", tabla, "', 'U') IS NOT NULL DROP TABLE ", tabla)
  DBI::dbExecute(con, borrar)
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (id INT, x FLOAT, t VARCHAR(8))"))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES (1, 0.5, 'a'), (2, 1.5, NULL), (3, 2.5, 'b')"
  ))
  borrar
}

# La conexion ejecuta: un `SELECT 1` devuelve su fila y un `INSERT` inserta,
# contado desde otra conexion.
.cses_mssql_ejecuta <- function(con, otra, tabla) {
  expect_identical(nrow(DBI::dbGetQuery(con, "SELECT 1 AS uno")), 1L)
  contar <- function() as.numeric(DBI::dbGetQuery(
    otra, paste0("SELECT COUNT(*) AS n FROM ", tabla)
  )$n)
  antes <- contar()
  DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES (99, 9.5, 'z')"))
  expect_equal(contar(), antes + 1)
  DBI::dbExecute(con, paste0("DELETE FROM ", tabla, " WHERE id = 99"))
}

test_that("SQL Server: despues de compilar una sentencia la conexion ejecuta", {
  con <- .cses_conectar_mssql()
  otra <- .cses_conectar_mssql()
  tabla <- "lupa_compilar_noexec_uno"
  borrar <- .cses_mssql_tabla(con, tabla)
  on.exit({
    try(DBI::dbExecute(otra, borrar), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
    try(DBI::dbDisconnect(otra), silent = TRUE)
  }, add = TRUE)
  for (caso in list(
    c(sql = paste0("SELECT COUNT(*) AS n FROM ", tabla), estado = "compilada"),
    c(sql = paste0("SELECT columna_que_no_existe FROM ", tabla), estado = "no_compilada")
  )) {
    r <- .cses_con_avisos(.with_compilador_dbi(con, "sqlserver", {
      .compilar_sql_dbi(con, caso[["sql"]], "x", "sqlserver")
    }))
    expect_identical(r$valor$compilacion$estado, caso[["estado"]])
    expect_identical(r$avisos, character())
    .cses_mssql_ejecuta(con, otra, tabla)
  }
})

test_that("SQL Server: despues de compilar los planes la conexion ejecuta y perfila igual", {
  con <- .cses_conectar_mssql()
  otra <- .cses_conectar_mssql()
  tabla <- "lupa_compilar_noexec_planes"
  borrar <- .cses_mssql_tabla(con, tabla)
  on.exit({
    try(DBI::dbExecute(otra, borrar), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
    try(DBI::dbDisconnect(otra), silent = TRUE)
  }, add = TRUE)
  r <- .cses_con_avisos(plan_perfilado_dbi(con, tabla, compilar = TRUE))
  expect_identical(r$avisos, character())
  expect_identical(sum(attr(r$valor, "compilacion")$estado == "no_compilada"), 0L)
  .cses_mssql_ejecuta(con, otra, tabla)
  # Dos veces seguidas en la misma conexion.
  r <- .cses_con_avisos(plan_perfilado_dbi(con, tabla, compilar = TRUE))
  expect_identical(r$avisos, character())
  .cses_mssql_ejecuta(con, otra, tabla)
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "compilar_NN"), tabla, "x"))
  r <- .cses_con_avisos(plan_medicion_dbi(mod, con, compilar = TRUE))
  expect_identical(r$avisos, character())
  .cses_mssql_ejecuta(con, otra, tabla)
  # El perfil por la misma conexion es el de una conexion nueva: 3 filas, `t`
  # con un nulo.
  perfil <- perfilar_dbi(con, tabla)
  columnas <- perfil$resumen_tabla$columnas
  expect_equal(columnas$n_validos[columnas$columna == "t"], 2)
  expect_equal(columnas$n_validos[columnas$columna == "id"], 3)
})

test_that("SQL Server: compilar dentro de una transaccion abierta la deja abierta, tambien con XACT_ABORT", {
  con <- .cses_conectar_mssql()
  otra <- .cses_conectar_mssql()
  tabla <- "lupa_compilar_noexec_tx"
  borrar <- .cses_mssql_tabla(con, tabla)
  on.exit({
    try(DBI::dbExecute(con, "SET XACT_ABORT OFF"), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
    try(DBI::dbExecute(otra, borrar), silent = TRUE)
    try(DBI::dbDisconnect(otra), silent = TRUE)
  }, add = TRUE)
  estado_tx <- function() DBI::dbGetQuery(con, paste(
    "SELECT @@TRANCOUNT AS trancount,",
    "CASE WHEN (@@OPTIONS & 16384) = 16384 THEN 1 ELSE 0 END AS xact_abort"
  ))
  for (xact_abort in c("OFF", "ON")) {
    DBI::dbExecute(con, paste("SET XACT_ABORT", xact_abort))
    DBI::dbBegin(con)
    DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES (10, 1, 'tx')"))
    r <- .cses_con_avisos(plan_perfilado_dbi(con, tabla, compilar = TRUE))
    expect_identical(r$avisos, character())
    # Una columna que no existe: con XACT_ABORT ON, ejecutada o bajo NOEXEC,
    # deshace la transaccion del usuario.
    compilado <- .with_compilador_dbi(con, "sqlserver", {
      .compilar_sql_dbi(con, paste0("SELECT columna_que_no_existe FROM ", tabla), "x", "sqlserver")
    })
    expect_identical(compilado$compilacion$estado, "no_compilada")
    tx <- estado_tx()
    expect_equal(as.numeric(tx$trancount), 1, info = xact_abort)
    expect_equal(as.numeric(tx$xact_abort), as.numeric(xact_abort == "ON"), info = xact_abort)
    expect_equal(as.numeric(DBI::dbGetQuery(
      con, paste0("SELECT COUNT(*) AS n FROM ", tabla, " WHERE id = 10")
    )$n), 1, info = xact_abort)
    DBI::dbCommit(con)
    expect_equal(as.numeric(DBI::dbGetQuery(
      otra, paste0("SELECT COUNT(*) AS n FROM ", tabla, " WHERE id = 10")
    )$n), 1, info = xact_abort)
    DBI::dbExecute(con, paste0("DELETE FROM ", tabla, " WHERE id = 10"))
  }
})

test_that("SQL Server: si NOEXEC no se apaga, compilar falla diciendo que la conexion quedo inutilizable", {
  con <- .cses_conectar_mssql()
  tabla <- "lupa_compilar_noexec_falla"
  borrar <- .cses_mssql_tabla(con, tabla)
  on.exit({
    try(DBI::dbExecute(con, "SET NOEXEC OFF"), silent = TRUE)
    try(DBI::dbExecute(con, borrar), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  # El `SET NOEXEC OFF` no llega al motor: lo que pasaba con el resultado
  # abierto.
  local_mocked_bindings(.orden_transaccion_dbi = function(conexion, orden, traer = FALSE) {
    if (identical(orden, "SET NOEXEC OFF")) return(0L)
    if (isTRUE(traer)) return(DBI::dbGetQuery(conexion, orden))
    DBI::dbExecute(conexion, orden)
  })
  expect_error(
    plan_perfilado_dbi(con, tabla, compilar = TRUE),
    class = "lupa_error_conexion_dbi", regexp = "inutilizable"
  )
  # Era cierto: la conexion no ejecutaba.
  expect_identical(nrow(DBI::dbGetQuery(con, "SELECT 1 AS uno")), 0L)
})
