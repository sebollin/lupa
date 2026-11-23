# El reconocedor de errores de la via DBI lee cada error donde el controlador lo
# escribe: el codigo estructurado si existe; si no, el codigo o la frase del
# encabezado, con el SQL y los literales del usuario quitados. Cada condicion
# de este archivo esta armada con la forma y el texto que el controlador
# entrego de verdad (RPostgres 1.4.10 con PostgreSQL 16 y 9.3; RMariaDB 1.3.5;
# RMySQL 0.11.3; odbc 1.7.0 con FreeTDS; duckdb 2.0.0; RSQLite), y las que
# dicen TRAMPA llevan el codigo o la frase en un dato, un nombre de columna o
# el SQL pegado: tienen que salir `rechazada`, y nunca conexion perdida.
testthat::skip_on_cran()

.RC_O <- intToUtf8(243L)

.rc_simple <- function(m) simpleError(m)
.rc_odbc <- function(x, sql, estado = "00000") structure(class = c("odbc_database_error", "rlang_error", "error", "condition"),
  list(message = c(`!` = paste0("ODBC failed with error ", estado, " from [FreeTDS][SQL Server].")), trace = NULL, parent = NULL,
       body = c(x = x, `*` = paste0("<SQL> '", sql, "'"), i = "From 'nanodbc/nanodbc.cpp:1802'."), call = NULL))
.rc_nano <- function(linea, texto, estado = "00000") structure(class = c("nanodbc::database_error", "C++Error", "error", "condition"),
  list(message = paste0("nanodbc/nanodbc.cpp:", linea, ": ", estado, "\n[FreeTDS][SQL Server]", texto, " "), call = NULL, cppstack = NULL))
.rc_duck <- function(tipo, crudo) structure(class = c("duckdb_error", "rlang_error", "error", "condition"),
  list(message = paste0(crudo, "\ni Context: rapi_prepare\ni Error type: ", tipo), context = "rapi_prepare", error_type = tipo, raw_message = crudo, call = NULL))
.RC_PG_COLA <- "\nserver closed the connection unexpectedly\n\tThis probably means the server terminated abnormally\n\tbefore or while processing the request.\n"
.RC_SQL_LARGO <- "SELECT COUNT(*) AS \"n_total_consulta\", COUNT(CASE WHEN \"connection reset\" IS NULL THEN NULL ELSE 1 END) FROM \"lupa_vcod\""
.rc_casos <- list(
  # ---------------- RPostgres: conexion perdida
  list("RPostgres", "sesion terminada, ociosa", .rc_simple(paste0("Failed to prepare query : FATAL:  terminating connection due to administrator command", .RC_PG_COLA)), "conexion"),
  list("RPostgres", "tope de ocio del servidor", .rc_simple(paste0("Failed to prepare query : FATAL:  terminating connection due to idle-session timeout", .RC_PG_COLA)), "conexion"),
  list("RPostgres", "sesion terminada, en vuelo", .rc_simple("Failed to fetch row : FATAL:  terminating connection due to administrator command\n"), "conexion"),
  list("RPostgres", "red cortada, ociosa", .rc_simple(paste0("Failed to prepare query : ", substring(.RC_PG_COLA, 2))), "conexion"),
  list("RPostgres", "red cortada, en vuelo o a mitad de resultado", .rc_simple(paste0("Failed to fetch row : ", substring(.RC_PG_COLA, 2))), "conexion"),
  # RPostgres: lo demas
  list("RPostgres", "statement_timeout", .rc_simple("Failed to fetch row : ERROR:  canceling statement due to statement timeout\n"), "cancelada"),
  list("RPostgres", "pg_cancel_backend", .rc_simple("Failed to fetch row : ERROR:  canceling statement due to user request\n"), "cancelada"),
  list("RPostgres", "lock_timeout (trae LINE 1 con el SQL)", .rc_simple("Failed to prepare query : ERROR:  canceling statement due to lock timeout\nLINE 1: SELECT COUNT(*) FROM lupa_t\n                             ^\n"), "cancelada"),
  list("RPostgres", "cancelacion con CONTEXT", .rc_simple("Failed to fetch row : ERROR:  canceling statement due to statement timeout\nCONTEXT:  SQL statement \"SELECT pg_sleep(0.01)\"\nPL/pgSQL function lupa_lento(integer) line 1 at PERFORM\n"), "cancelada"),
  list("RPostgres", "cancelacion, servidor en castellano", .rc_simple(paste0("Failed to fetch row : ERROR:  cancelando la sentencia debido a que se agot", .RC_O, " el tiempo de espera de sentencias\n")), "cancelada"),
  list("RPostgres", "permiso negado, 16", .rc_simple("Failed to fetch row : ERROR:  permission denied for table lupa_perm\n"), "permiso"),
  list("RPostgres", "permiso negado, 9.3", .rc_simple("Failed to fetch row : ERROR:  permission denied for relation lupa_perm\n"), "permiso"),
  list("RPostgres", "permiso sobre la funcion de la prueba de capacidad", .rc_simple("Failed to fetch row : ERROR:  permission denied for function stddev_samp\nCONTEXT:  PL/pgSQL function stddev_samp(numeric) line 1 at RAISE\n"), "permiso"),
  list("RPostgres", "interbloqueo en la prueba de capacidad", .rc_simple("Failed to fetch row : ERROR:  deadlock detected\nCONTEXT:  PL/pgSQL function stddev_samp(numeric) line 1 at RAISE\n"), "transitoria"),
  list("RPostgres", "capacidad ausente: STDEV", .rc_simple("Failed to prepare query : ERROR:  function stdev(numeric) does not exist\nLINE 1: SELECT STDEV(1.0) AS desvio\n               ^\nHINT:  No function matches the given name and argument types. You might need to add explicit type casts.\n"), "capacidad"),
  list("RPostgres", "capacidad ausente: PERCENTILE_CONT en 9.3", .rc_simple("Failed to prepare query : ERROR:  syntax error at or near \"(\"\nLINE 1: SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY x) AS p F...\n                                          ^\n"), "capacidad"),
  list("RPostgres", "transaccion abortada", .rc_simple("Failed to prepare query : ERROR:  current transaction is aborted, commands ignored until end of transaction block\n"), "transaccion"),
  # RPostgres: trampas
  list("RPostgres", "TRAMPA dato 'Lost connection to MySQL server during query'", .rc_simple("Failed to fetch row : ERROR:  invalid input syntax for type integer: \"Lost connection to MySQL server during query\"\n"), "rechazada"),
  list("RPostgres", "TRAMPA dato 'connection reset by peer'", .rc_simple("Failed to fetch row : ERROR:  invalid input syntax for type integer: \"connection reset by peer\"\n"), "rechazada"),
  list("RPostgres", "TRAMPA permiso sobre la tabla \"lupa_lost connection\"", .rc_simple("Failed to fetch row : ERROR:  permission denied for table lupa_lost connection\n"), "permiso"),
  list("RPostgres", "TRAMPA tabla inexistente \"lupa_closed connection\"", .rc_simple("Failed to prepare query : ERROR:  relation \"lupa_closed connection\" does not exist\nLINE 1: SELECT 1 FROM \"lupa_closed connection\" WHERE 1 = 0\n                      ^\n"), "rechazada"),
  list("RPostgres", "TRAMPA dato 'ERR 57014 x'", .rc_simple("Failed to fetch row : ERROR:  invalid input syntax for type integer: \"ERR 57014 x\"\n"), "rechazada"),
  list("RPostgres", "TRAMPA dato 'calle 1969 apto 2'", .rc_simple("Failed to prepare query : ERROR:  invalid input syntax for type integer: \"calle 1969 apto 2\"\nLINE 1: SELECT CAST('calle 1969 apto 2' AS integer) AS n\n                    ^\n"), "rechazada"),
  list("RPostgres", "TRAMPA dato 'canceling statement due to x'", .rc_simple("Failed to fetch row : ERROR:  invalid input syntax for type integer: \"canceling statement due to x\"\n"), "rechazada"),
  list("RPostgres", "TRAMPA columna \"canceling statement due to x\"", .rc_simple("Failed to prepare query : ERROR:  column \"canceling statement due to x\" does not exist\nLINE 1: SELECT \"canceling statement due to x\" FROM t\n               ^\n"), "rechazada"),
  list("RPostgres", "TRAMPA columna \"FATAL\" y dato FATAL:", .rc_simple("Failed to fetch row : ERROR:  invalid input syntax for type integer: \"FATAL:  terminating connection\"\n"), "rechazada"),
  list("RPostgres", "TRAMPA el SQL de LINE 1 dice 3024 y HYT00", .rc_simple("Failed to prepare query : ERROR:  column \"nope\" does not exist\nLINE 1: SELECT nope FROM t WHERE x >= 3024 AND c = 'HYT00 server closed the connection unexpectedly'\n               ^\n"), "rechazada"),
  # RPostgres: un error que el servidor informo con el texto de un corte. La
  # sesion sigue: una funcion con RAISE EXCEPTION, y una tabla foranea de
  # postgres_fdw con el remoto cortado en medio de la consulta (9.3 y 16).
  list("RPostgres", "TRAMPA RAISE con el texto de la red cortada", .rc_simple("Failed to fetch row : ERROR:  server closed the connection unexpectedly\nCONTEXT:  PL/pgSQL function lupa_f(text) line 1 at RAISE\n"), "rechazada"),
  list("RPostgres", "TRAMPA tabla foranea con el remoto cortado, 9.3", .rc_simple(paste0("Failed to fetch row : ERROR:  ", substring(.RC_PG_COLA, 2))), "rechazada"),
  list("RPostgres", "TRAMPA tabla foranea con el remoto cortado, 16", .rc_simple(paste0("Failed to fetch row : ERROR:  FATAL:  terminating connection due to administrator command", .RC_PG_COLA)), "rechazada"),
  list("RPostgres", "TRAMPA RAISE: no connection to the server", .rc_simple("Failed to fetch row : ERROR:  no connection to the server\n"), "rechazada"),
  list("RPostgres", "TRAMPA RAISE: could not send data", .rc_simple("Failed to fetch row : ERROR:  could not send data to server: Broken pipe\n"), "rechazada"),
  list("RPostgres", "TRAMPA RAISE: could not receive data", .rc_simple("Failed to fetch row : ERROR:  could not receive data from server: Connection reset by peer\n"), "rechazada"),
  list("RPostgres", "TRAMPA RAISE: SSL connection has been closed", .rc_simple("Failed to fetch row : ERROR:  SSL connection has been closed unexpectedly\n"), "rechazada"),
  list("RPostgres", "TRAMPA RAISE: connection to server was lost", .rc_simple("Failed to fetch row : ERROR:  connection to server was lost\n"), "rechazada"),
  list("RPostgres", "TRAMPA RAISE: Lost connection to database", .rc_simple("Failed to fetch row : ERROR:  Lost connection to database\n"), "rechazada"),
  # Lo mismo sin gravedad lo escribio el cliente, y es un corte.
  list("RPostgres", "no connection to the server, del cliente", .rc_simple("Failed to prepare query : no connection to the server\n"), "conexion"),
  list("RPostgres", "could not receive data, del cliente", .rc_simple("Failed to fetch row : could not receive data from server: Connection reset by peer\n"), "conexion"),
  # ---------------- RMariaDB
  list("RMariaDB", "conexion perdida (todas las formas)", .rc_simple("Lost connection to MySQL server during query [2013]"), "conexion"),
  list("RMariaDB", "max_statement_time", .rc_simple("Query execution was interrupted (max_statement_time exceeded) [1969]"), "cancelada"),
  list("RMariaDB", "KILL QUERY", .rc_simple("Query execution was interrupted [1317]"), "cancelada"),
  list("RMariaDB", "permiso de tabla", .rc_simple("SELECT command denied to user 'lupa_np'@'10.0.0.7' for table `lupa`.`lupa_perm` [1142]"), "permiso"),
  list("RMariaDB", "permiso de columna", .rc_simple("SELECT command denied to user 'lupa_np'@'10.0.0.7' for column 'secreto' in table 'lupa_perm_col' [1143]"), "permiso"),
  list("RMariaDB", "capacidad ausente: STDEV", .rc_simple("FUNCTION lupa.STDEV does not exist [1305]"), "capacidad"),
  list("RMariaDB", "capacidad ausente: PERCENTILE_CONT", .rc_simple("You have an error in your SQL syntax; check the manual that corresponds to your MariaDB server version for the right syntax to use near 'AS p FROM lupa_t' at line 1 [1064]"), "capacidad"),
  list("RMariaDB", "TRAMPA columna 'x [2013]'", .rc_simple("Unknown column 'x [2013]' in 'SELECT' [1054]"), "rechazada"),
  list("RMariaDB", "TRAMPA columna 'query execution was interrupted [1317]'", .rc_simple("Unknown column 'query execution was interrupted [1317]' in 'SELECT' [1054]"), "rechazada"),
  list("RMariaDB", "TRAMPA tabla que termina en [1969]", .rc_simple("Table 'lupa.ventas [1969]' doesn't exist [1146]"), "rechazada"),
  # ---------------- RMySQL (el codigo viene de dbGetException)
  list("RMySQL", "conexion perdida", .rc_simple("could not run statement: Lost connection to MySQL server during query"), "conexion", list(errorNum = 2013L, errorMsg = "Lost connection to MySQL server during query")),
  list("RMySQL", "tope de ocio", .rc_simple("could not run statement: The client was disconnected by the server because of inactivity. See wait_timeout and interactive_timeout for configuring this behavior."), "conexion", list(errorNum = 4031L, errorMsg = "The client was disconnected by the server because of inactivity. See wait_timeout and interactive_timeout for configuring this behavior.")),
  list("RMySQL", "permiso de columna", .rc_simple("could not run statement: SELECT command denied to user 'lupa_np'@'10.0.0.7' for column 'secreto' in table 'lupa_perm_col'"), "permiso", list(errorNum = 1143L, errorMsg = "SELECT command denied to user 'lupa_np'@'10.0.0.7' for column 'secreto' in table 'lupa_perm_col'")),
  list("RMySQL", "tope de tiempo: R no recibe error, solo el aviso", .rc_simple("error while fetching rows"), "cancelada", list(errorNum = 3024L, errorMsg = "Query execution was interrupted, maximum statement execution time exceeded"), TRUE),
  list("RMySQL", "KILL QUERY: R no recibe error, solo el aviso", .rc_simple("error while fetching rows"), "cancelada", list(errorNum = 1317L, errorMsg = "Query execution was interrupted"), TRUE),
  list("RMySQL", "capacidad ausente", .rc_simple("could not run statement: FUNCTION lupa.STDEV does not exist"), "capacidad", list(errorNum = 1305L, errorMsg = "FUNCTION lupa.STDEV does not exist")),
  list("RMySQL", "TRAMPA columna 'x [2013]'", .rc_simple("could not run statement: Unknown column 'x [2013]' in 'field list'"), "rechazada", list(errorNum = 1054L, errorMsg = "Unknown column 'x [2013]' in 'field list'")),
  list("RMySQL", "TRAMPA columna 'Lost connection to MySQL server', sin excepcion", .rc_simple("could not run statement: Unknown column 'Lost connection to MySQL server' in 'field list'"), "rechazada"),
  list("RMySQL", "conexion perdida sin excepcion (por el texto)", .rc_simple("could not run statement: Lost connection to MySQL server during query"), "conexion"),
  # ---------------- odbc + FreeTDS
  list("odbc", "sesion terminada o red cortada, primera sentencia", .rc_nano(1591, "Unknown error"), "conexion"),
  list("odbc", "sesion terminada, primera llamada de catalogo", .rc_nano(5833, "Unexpected EOF from the server"), "conexion"),
  list("odbc", "sesion terminada, en vuelo", .rc_odbc("Cannot continue the execution because the session is in the kill state.", "WAITFOR DELAY '00:00:20'"), "conexion"),
  list("odbc", "la sentencia siguiente a un EOF", .rc_odbc("Communication link failure", "SELECT 1 AS uno"), "conexion"),
  list("odbc", "tiempo de espera del controlador (estado HYT00)", .rc_odbc("Query timeout expired", "SELECT 1", "HYT00"), "cancelada"),
  list("odbc", "permiso de tabla", .rc_odbc("The SELECT permission was denied on the object 'lupa_perm', database 'lupa_pruebas', schema 'dbo'.", "SELECT * FROM lupa_perm"), "permiso"),
  list("odbc", "permiso de columna", .rc_odbc("The SELECT permission was denied on the column 'secreto' of the object 'lupa_perm_col', database 'lupa_pruebas', schema 'dbo'.", "SELECT secreto FROM lupa_perm_col"), "permiso"),
  list("odbc", "permiso, servidor en castellano", .rc_odbc(paste0("Se deneg", .RC_O, " el permiso SELECT en el objeto 'lupa_perm', base de datos 'lupa_pruebas', esquema 'dbo'."), "SELECT 1 FROM \"lupa_perm\" WHERE 1 = 0"), "permiso"),
  list("odbc", "tiempo de bloqueo", .rc_odbc("Lock request time out period exceeded.", "SELECT COUNT(*) AS n FROM lupa_unk"), "transitoria"),
  list("odbc", "capacidad ausente: STDDEV_SAMP", .rc_odbc("'STDDEV_SAMP' is not a recognized built-in function name.", "SELECT STDDEV_SAMP(1.0) AS desvio"), "capacidad"),
  list("odbc", "capacidad ausente: PERCENTILE_CONT", .rc_odbc("The function 'PERCENTILE_CONT' must have an OVER clause.", "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY x) AS p FROM lupa_t"), "capacidad"),
  list("odbc", "TRAMPA columna \"connection reset\" en el SQL pegado", .rc_odbc("Conversion failed when converting the varchar value 'HYT00' to data type int.", .RC_SQL_LARGO), "rechazada"),
  list("odbc", "TRAMPA dato 'ERR 57014 x'", .rc_odbc("Conversion failed when converting the varchar value 'ERR 57014 x' to data type int.", .RC_SQL_LARGO), "rechazada"),
  list("odbc", "TRAMPA dato 'calle 1969 apto 2'", .rc_odbc("Conversion failed when converting the varchar value 'calle 1969 apto 2' to data type int.", "SELECT CAST('calle 1969 apto 2' AS INTEGER) AS n"), "rechazada"),
  list("odbc", "TRAMPA columna 'Unknown error'", .rc_odbc("Invalid column name 'Unknown error'.", "SELECT \"Unknown error\" FROM t"), "rechazada"),
  list("odbc", "TRAMPA columna 'Communication link failure'", .rc_odbc("Invalid column name 'Communication link failure'.", "SELECT \"Communication link failure\" FROM t"), "rechazada"),
  list("odbc", "TRAMPA el SQL pegado dice HYT00 y TOP 3024", .rc_odbc("Invalid column name 'nope'.", "SELECT TOP 3024 nope, 'HYT00' FROM t"), "rechazada"),
  # ---------------- duckdb y RSQLite
  list("duckdb", "conexion cerrada", structure(class = c("duckdb_error", "rlang_error", "error", "condition"), list(message = "Invalid connection\ni Context: rapi_prepare", context = "rapi_prepare", call = NULL)), "conexion"),
  list("duckdb", "capacidad ausente: STDEV", .rc_duck("CATALOG", "Scalar Function with name stdev does not exist!\nDid you mean \"stddev\"?\n\nLINE 1: SELECT STDEV(1.0) AS desvio\n               ^^^^^^^^^^"), "capacidad"),
  list("duckdb", "transaccion abortada", .rc_duck("TRANSACTION", "Current transaction is aborted (please ROLLBACK)"), "transaccion"),
  list("duckdb", "TRAMPA dato 'Invalid connection'", .rc_duck("CONVERSION", "Could not convert string 'Invalid connection' to INT32 when casting from source column s\n\nLINE 1: SELECT CAST(s AS INTEGER) AS n FROM lupa_t"), "rechazada"),
  list("RSQLite", "conexion cerrada", .rc_simple("Invalid or closed connection"), "conexion"),
  list("RSQLite", "TRAMPA un resultado ya cerrado", .rc_simple("Invalid result set"), "rechazada"),
  list("RSQLite", "capacidad ausente", .rc_simple("no such function: STDDEV_SAMP"), "capacidad"),
  list("RSQLite", "TRAMPA columna que se llama como el error", .rc_simple("no such column: Invalid or closed connection"), "rechazada")
)

# Formas que un controlador puede dar y que no estaban en lo medido.
.rc_odbc_cuerpo <- function(cuerpo, estado = "00000", padre = NULL) {
  structure(
    class = c("odbc_database_error", "rlang_error", "error", "condition"),
    list(message = c(`!` = paste0(
      "ODBC failed with error ", estado, " from [FreeTDS][SQL Server]."
    )), trace = NULL, parent = padre, body = cuerpo, call = NULL)
  )
}

.rc_duck_real <- function(tipo, crudo) .rc_duck(tipo, crudo)

.rc_formas <- list(
  list("odbc", "el mensaje entero entre corchetes",
       .rc_odbc_cuerpo(c(x = "[FreeTDS][SQL Server][Communication link failure]")),
       "conexion"),
  list("odbc", "TRAMPA un nombre entre corchetes con la frase",
       .rc_odbc_cuerpo(c(x = "Invalid column name [Communication link failure].")),
       "rechazada"),
  list("odbc", "TRAMPA un estado escrito en un literal",
       .rc_odbc_cuerpo(c(x = "Invalid column name 'HYT00'.")), "rechazada"),
  list("odbc", "sin la linea del motor y sin estado",
       .rc_odbc_cuerpo(c(`*` = "<SQL> 'SELECT 1'")), "rechazada"),
  list("odbc", "sin la linea del motor, con el estado en el encabezado",
       .rc_odbc_cuerpo(c(`*` = "<SQL> 'SELECT 1'"), estado = "08S01"),
       "conexion"),
  list("odbc", "sin la linea del motor, con el error crudo como causa",
       .rc_odbc_cuerpo(c(`*` = "<SQL> 'SELECT 1'"),
                       padre = .rc_nano(1591L, "Unknown error")),
       "conexion"),
  list("RPostgres", "el mensaje entero entre comillas",
       .rc_simple("Failed to fetch row : \"server closed the connection unexpectedly\"\n"),
       "conexion"),
  # Con gravedad lo informo el servidor: la sesion sigue, citado o no.
  list("RPostgres", "TRAMPA el mensaje entero entre comillas, informado por el servidor",
       .rc_simple("Failed to fetch row : ERROR:  \"server closed the connection unexpectedly\"\n"),
       "rechazada"),
  list("RPostgres", "TRAMPA literal con comillas de dolar",
       .rc_simple("Failed to fetch row : ERROR:  invalid input syntax for type integer: $rrd$connection reset by peer$rrd$\n"),
       "rechazada"),
  list("RPostgres", "TRAMPA literal nacional",
       .rc_simple("Failed to fetch row : ERROR:  invalid input syntax for type integer: N'canceling statement due to x'\n"),
       "rechazada"),
  list("duckdb", "funcion que no existe",
       .rc_duck_real("CATALOG", "Scalar Function with name stddev_sampx does not exist!\nDid you mean \"stddev_samp\"?"),
       "capacidad"),
  list("duckdb", "firma que no casa",
       .rc_duck_real("BINDER", "No function matches the given name and argument types 'abs(VARCHAR, INTEGER)'."),
       "capacidad"),
  list("duckdb", "sintaxis que el motor no entiende",
       .rc_duck_real("PARSER", "syntax error at or near \"1\""), "capacidad"),
  list("duckdb", "una columna que no existe no es una capacidad ausente",
       .rc_duck_real("BINDER", "Referenced column \"nope\" not found in FROM clause!\nCandidate bindings: \"a\""),
       "rechazada"),
  list("duckdb", "una tabla que no existe tampoco",
       .rc_duck_real("CATALOG", "Table with name lupa_nope does not exist!\nDid you mean \"lupa_t\"?"),
       "rechazada"),
  list("odbc", "TRAMPA una funcion del usuario que se llama como el error",
       .rc_odbc_cuerpo(c(x = "'Communication link failure' is not a recognized built-in function name.")),
       "capacidad"),
  list("RPostgres", "TRAMPA un objeto del usuario al principio del mensaje",
       .rc_simple("Failed to prepare query : ERROR:  \"server closed the connection unexpectedly\" is not a view\n"),
       "rechazada"),
  list("RSQLite", "un resultado ya cerrado no es la conexion perdida",
       .rc_simple("Invalid result set"), "rechazada"),
  list("RSQLite", "cantidad de argumentos",
       .rc_simple("wrong number of arguments to function ABS()"), "capacidad"),
  list("RSQLite", "una tabla que no existe",
       .rc_simple("no such table: lupa_nope"), "rechazada")
)

.rc_reconocer <- function(caso) {
  .reconocer_error_dbi(
    caso[[1L]], caso[[3L]], if (length(caso) >= 5L) caso[[5L]] else NULL
  )
}

test_that("cada error medido se reconoce por lo que el controlador escribio", {
  expect_gte(length(.rc_casos), 71L)
  for (caso in c(.rc_casos, .rc_formas)) {
    expect_identical(
      .rc_reconocer(caso), caso[[4L]],
      info = paste(caso[[1L]], caso[[2L]], sep = ": ")
    )
  }
})

test_that("un codigo o una frase en un dato, un nombre o el SQL no decide nada", {
  trampas <- Filter(
    function(caso) startsWith(caso[[2L]], "TRAMPA"), c(.rc_casos, .rc_formas)
  )
  expect_gte(length(trampas), 23L)
  for (caso in trampas) {
    dicho <- .rc_reconocer(caso)
    expect_false(dicho %in% c("conexion", "cancelada"),
                 info = paste(caso[[1L]], caso[[2L]], sep = ": "))
  }
})

test_that("un estado SQLSTATE estructurado vale en cualquier controlador", {
  con_estado <- function(estado) {
    structure(
      class = c("simpleError", "error", "condition"),
      list(message = "fallo", call = NULL, sqlstate = estado)
    )
  }
  expect_identical(.reconocer_error_dbi("", con_estado("57014")), "cancelada")
  expect_identical(.reconocer_error_dbi("", con_estado("08006")), "conexion")
  expect_identical(.reconocer_error_dbi("", con_estado("42501")), "permiso")
  expect_identical(.reconocer_error_dbi("", con_estado("40P01")), "transitoria")
  # Sin estado y de un controlador que no se midio, el texto no decide.
  expect_identical(
    .reconocer_error_dbi("", .rc_simple(
      "canceling statement due to statement timeout"
    )),
    "rechazada"
  )
})

test_that("la falla de una consulta se clasifica por el reconocedor y no por palabras", {
  # Antes bastaba con que el mensaje dijera "cancel", "timeout" o `57014` en
  # cualquier parte, tambien en un nombre de columna o en un literal.
  expect_identical(
    .clasificar_falla_consulta_dbi(
      NA_character_, "no such column: timeout_cancelado_57014"
    ),
    "rechazada"
  )
  expect_identical(
    .clasificar_falla_consulta_dbi(NA_character_, "lo que sea", "cancelada"),
    "cancelada"
  )
  expect_identical(
    .clasificar_falla_consulta_dbi("57014", "lo que sea"), "cancelada"
  )
  for (otra in c("permiso", "capacidad", "transitoria", "rechazada")) {
    expect_identical(
      .clasificar_falla_consulta_dbi(NA_character_, "cancel timeout", otra),
      "rechazada"
    )
  }
  dato <- .rc_simple(
    "Failed to fetch row : ERROR:  invalid input syntax for type integer: \"ERR 57014 x\"\n"
  )
  expect_identical(.sqlstate_dbi(dato), NA_character_)
})
