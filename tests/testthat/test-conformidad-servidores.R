# conformidad_dbi() en PostgreSQL (actual y 9.3) y SQL Server, cuando el
# servicio existe: la bateria cuenta en el servidor, con el tipo y la
# intercalacion de cada columna real, lo mismo que R. Antes todo era
# `no_verificado` y la bateria no decia nada en un servidor.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.csrv_conectar_pg <- function(puerto) {
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

.csrv_conectar_mssql <- function() {
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

.csrv_fecha <- as.Date("2026-01-01")

.csrv_borrar <- function(con, motor, tabla) {
  sql <- if (identical(motor, "sqlserver")) {
    paste0("IF OBJECT_ID('dbo.", tabla, "', 'U') IS NOT NULL DROP TABLE ", tabla)
  } else {
    paste0("DROP TABLE IF EXISTS ", tabla)
  }
  DBI::dbExecute(con, sql)
}

.csrv_medir <- function(mod, con, ...) {
  suppressWarnings(medir_dbi(mod, con, id_medicion = "c", fecha = .csrv_fecha,
                             instrumentar = FALSE, ...))
}

# Una tabla con un numero, una fecha, un texto con la intercalacion por omision
# de la base y otro con la binaria; la bateria se arma con su esquema.
.csrv_probar <- function(con, motor, tabla, sql_intercalacion, binaria, partes) {
  on.exit({
    try(.csrv_borrar(con, motor, tabla), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  .csrv_borrar(con, motor, tabla)
  doble <- if (identical(motor, "sqlserver")) "FLOAT" else "DOUBLE PRECISION"
  texto <- if (identical(motor, "sqlserver")) "NVARCHAR(32)" else "VARCHAR(32)"
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (n ", doble, ", f DATE, t ", texto, ", tb ",
    texto, " COLLATE ", binaria, ")"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES (1, '2026-01-01', 'A', 'A'), ",
    "(5, '2026-02-01', 'a', 'B'), (NULL, NULL, NULL, NULL), ",
    "(2.5, '2026-03-01', 'x', 'x')"
  ))
  nuc <- metricas_nucleo()
  inst <- function(metrica, nombre, columna, ...) {
    instanciar(especializar(metrica, nombre, ...), tabla, columna,
               nombre_instancia = nombre)
  }
  mod <- modelo(
    inst(nuc$NoNulo, "NN", "n"),
    inst(nuc$ValoresPosiblesPorComprension, "R", "n", minimo = 0, maximo = 3),
    inst(nuc$ReglaIntegridadIntraEntidad, "P", "n", regla = ~ !is.na(n) & n > 1),
    inst(nuc$NoNulo, "NF", "f"),
    inst(nuc$NoNulo, "NB", "tb"),
    inst(nuc$ValoresPosiblesPorExtension, "Eb", "tb", valores = c("A", "B")),
    inst(nuc$ValoresPosiblesPorExtension, "Et", "t", valores = c("A", "B"))
  )
  bateria <- conformidad_dbi(con, mod)
  expect_s3_class(bateria, "conformidad_dbi")
  expect_identical(attr(bateria, "motor", exact = TRUE), motor)
  expect_false(any(bateria$conformidad == "no_verificado"))
  expect_false(any(bateria$conformidad == "no_conforme"))
  expect_setequal(unique(bateria$metrica_instanciada), c("NN", "R", "P", "NF", "NB", "Eb", "Et"))
  expect_length(attr(bateria, "motores_no_verificados", exact = TRUE), 0L)
  # La intercalacion por omision es la que el motor dice de su base.
  esperada <- as.character(DBI::dbGetQuery(con, sql_intercalacion)[[1L]][[1L]])
  expect_identical(attr(bateria, "intercalacion", exact = TRUE), esperada)
  expect_identical(attr(bateria, "intercalacion_motivo", exact = TRUE), NA_character_)
  expect_false(is.na(attr(bateria, "version", exact = TRUE)))
  # Las reglas numericas, de fecha y del texto binario se cuentan con el tipo
  # de la columna real y el motor da lo mismo que R.
  for (nombre in c("NN", "R", "P", "NF", "NB")) {
    filas <- bateria[bateria$metrica_instanciada == nombre, , drop = FALSE]
    expect_true(all(filas$conformidad == "conforme"), info = paste(motor, nombre))
    expect_true(all(filas$esquema == "tabla"), info = paste(motor, nombre))
    expect_identical(filas$n_motor, filas$n_r, info = paste(motor, nombre))
    expect_identical(filas$denominador_motor, filas$denominador_r,
                     info = paste(motor, nombre))
    expect_true(all(filas$denominador_motor > 0), info = paste(motor, nombre))
  }
  # La intercalacion binaria de la columna entra en los literales: el traductor
  # la reconoce. SQL Server ignora los espacios finales al comparar, y con los
  # valores inventados con espacio decide su guarda.
  eb <- bateria[bateria$metrica_instanciada == "Eb", , drop = FALSE]
  expect_false(any(grepl("no compara por bytes", eb$motivo, fixed = TRUE, useBytes = TRUE)), info = motor)
  if (identical(motor, "sqlserver")) {
    expect_true(all(eb$conformidad == "abstencion"))
    expect_true(all(grepl("espacios finales", eb$motivo, fixed = TRUE, useBytes = TRUE)))
  } else {
    expect_true(any(eb$conformidad == "conforme"), info = motor)
  }
  # El texto con la intercalacion de la base: PostgreSQL compara por bytes al
  # buscar igualdad; la de SQL Server no distingue mayusculas, y medir_dbi()
  # no cuenta esa regla en el motor.
  et <- bateria[bateria$metrica_instanciada == "Et" & bateria$caso == "limites_texto", ,
                drop = FALSE]
  if (identical(motor, "sqlserver")) {
    expect_true(all(et$conformidad == "abstencion"))
    expect_true(all(grepl("no compara por bytes", et$motivo, fixed = TRUE, useBytes = TRUE)))
  } else {
    expect_true(all(et$conformidad == "conforme"))
  }
  # Sin no conformidades, la bateria no le cambia nada a medir_dbi().
  sin <- .csrv_medir(mod, con)
  expect_gt(nrow(sin), 0L)
  expect_identical(.csrv_medir(mod, con, conformidad = bateria), sin)
  # La huella del servidor sale de lo que el motor dice de si mismo.
  lugar <- lupa:::.lugar_del_motor_dbi(con)
  expect_identical(names(lugar$servidor), partes)
  expect_false(anyNA(lugar$servidor))
  expect_identical(attr(bateria, "identidad", exact = TRUE)$servidor,
                   lupa:::.lugar_conexion_dbi(con))
  # La bateria no crea tablas: sus relaciones son consultas.
  expect_false(any(startsWith(DBI::dbListTables(con), "lupa_conf_")))
}

.CSRV_INTERCALACION_PG <- paste(
  "SELECT datcollate FROM pg_catalog.pg_database",
  "WHERE datname = current_database()"
)

test_that("PostgreSQL: la bateria cuenta en el servidor lo mismo que R", {
  con <- .csrv_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  .csrv_probar(con, "postgresql", "lupa_confsrv_t_pg", .CSRV_INTERCALACION_PG,
               "\"C\"", c("sistema", "base"))
})

test_that("PostgreSQL 9.3: la bateria cuenta en el servidor lo mismo que R", {
  con <- .csrv_conectar_pg(Sys.getenv("LUPA_PG93_PORT", "55493"))
  .csrv_probar(con, "postgresql", "lupa_confsrv_t_pg93", .CSRV_INTERCALACION_PG,
               "\"C\"", c("direccion", "puerto", "base"))
})

test_that("SQL Server: la bateria cuenta en el servidor lo mismo que R", {
  con <- .csrv_conectar_mssql()
  .csrv_probar(
    con, "sqlserver", "lupa_confsrv_t_mssql",
    "SELECT CAST(DATABASEPROPERTYEX(DB_NAME(), 'Collation') AS NVARCHAR(128))",
    "Latin1_General_BIN2", c("servidor", "maquina", "base")
  )
})

test_that("PostgreSQL: la bateria no deja abortada la transaccion del usuario", {
  con <- .csrv_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  tabla <- "lupa_confsrv_t_pg_tx"
  on.exit({
    try(DBI::dbRollback(con), silent = TRUE)
    try(.csrv_borrar(con, "postgresql", tabla), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  .csrv_borrar(con, "postgresql", tabla)
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (n NUMERIC(10,2))"))
  DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES (1), (2.5), (5), (NULL)"))
  mod <- modelo(instanciar(
    especializar(metricas_nucleo()$ValoresPosiblesPorComprension, "R",
                 minimo = 0, maximo = 3),
    tabla, "n", nombre_instancia = "R"
  ))
  DBI::dbBegin(con)
  # 1e12 no cabe en NUMERIC(10,2): el motor rechaza ese literal y la bateria
  # cuenta con lo demas, sin abortar la transaccion.
  bateria <- conformidad_dbi(con, mod)
  expect_true(all(bateria$conformidad == "conforme"))
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS uno")$uno, 1L)
  DBI::dbRollback(con)
})
