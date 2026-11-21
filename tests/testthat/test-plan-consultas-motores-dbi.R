# Promesa de consultas entre `plan_perfilado_dbi()` y `perfilar_dbi()` en los
# seis motores, cubriendo los bordes donde el desvio sobre columnas con menos
# de dos valores no nulos (0 o 1) provocaba reintentos fuera del plan en MySQL,
# MariaDB, SQL Server y `float8` de PostgreSQL.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.pcm_conectar_pg <- function(puerto) {
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(puerto),
    user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en puerto ", puerto, "."))
  con
}

.pcm_conectar_mysql <- function(maria = FALSE) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  variable <- if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD"
  clave <- Sys.getenv(variable, unset = "")
  skip_if(!nzchar(clave), paste0("Sin ", variable, "."))
  puerto_def <- if (maria) "33306" else "33307"
  puerto_var <- if (maria) "LUPA_MARIADB_PORT" else "LUPA_MYSQL_PORT"
  puerto <- as.integer(Sys.getenv(puerto_var, Sys.getenv("LUPA_DB_PORT", puerto_def)))
  con <- tryCatch({
    if (maria) {
      DBI::dbConnect(
        RMariaDB::MariaDB(),
        host = Sys.getenv("LUPA_MARIADB_HOST", Sys.getenv("LUPA_DB_HOST", "127.0.0.1")),
        port = puerto,
        dbname = Sys.getenv("LUPA_MARIADB_DB", Sys.getenv("LUPA_DB_NAME", "lupa")),
        username = Sys.getenv("LUPA_MARIADB_USER", Sys.getenv("LUPA_DB_USER", "root")),
        password = clave
      )
    } else {
      DBI::dbConnect(
        RMySQL::MySQL(),
        host = Sys.getenv("LUPA_MYSQL_HOST", Sys.getenv("LUPA_DB_HOST", "127.0.0.1")),
        port = puerto,
        dbname = Sys.getenv("LUPA_MYSQL_DB", Sys.getenv("LUPA_DB_NAME", "lupa")),
        username = Sys.getenv("LUPA_MYSQL_USER", Sys.getenv("LUPA_DB_USER", "root")),
        password = clave
      )
    }
  }, error = function(e) NULL)
  skip_if(is.null(con), paste0(if (maria) "MariaDB" else "MySQL", " no disponible."))
  con
}

.pcm_conectar_mssql <- function() {
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(),
    Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"),
    PWD = clave,
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0",
    TrustServerCertificate = "yes",
    timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  con
}

.pcm_verificar <- function(con, tabla, exacto = TRUE, metricas = NULL, info = tabla) {
  args_plan <- list(con, tabla, bloque_muestra = "solo_agregados")
  if (!is.null(metricas)) args_plan$metricas <- metricas
  plan <- suppressWarnings(suppressMessages(do.call(plan_perfilado_dbi, args_plan)))
  total_plan <- as.numeric(attr(plan, "total"))

  args_perf <- c(args_plan, list(proteger_datos_personales = FALSE))
  perf <- suppressWarnings(suppressMessages(do.call(perfilar_dbi, args_perf)))
  emitidas <- as.numeric(perf$resumen_tabla$meta$consultas$emitidas)

  if (isTRUE(exacto)) {
    expect_identical(emitidas, total_plan, info = info)
  } else {
    expect_lte(emitidas, total_plan, label = info)
  }
  perf_justo <- suppressWarnings(suppressMessages(do.call(
    perfilar_dbi, c(args_perf, list(max_consultas = total_plan))
  )))
  expect_false(isTRUE(perf_justo$resumen_tabla$meta$consultas$agotado), info = info)
  invisible(list(plan = plan, perfil = perf))
}

test_that("SQLite cumple el plan en tablas de 1 fila, vacias y toda NULL", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "CREATE TABLE lupa_plan_sq_una (id INTEGER NOT NULL, pid INTEGER, v TEXT)")
  DBI::dbExecute(con, "CREATE UNIQUE INDEX lupa_plan_sq_una_u ON lupa_plan_sq_una(id)")
  DBI::dbExecute(con, "INSERT INTO lupa_plan_sq_una VALUES (1, 999, 'x')")
  .pcm_verificar(con, "lupa_plan_sq_una", exacto = TRUE, info = "sqlite-1fila")

  DBI::dbExecute(con, "CREATE TABLE lupa_plan_sq_vacia (pid INTEGER)")
  .pcm_verificar(con, "lupa_plan_sq_vacia", exacto = FALSE, info = "sqlite-vacia")

  DBI::dbExecute(con, "CREATE TABLE lupa_plan_sq_null (u INTEGER, v TEXT)")
  DBI::dbExecute(con, "INSERT INTO lupa_plan_sq_null VALUES (NULL, 'a'), (NULL, 'a')")
  .pcm_verificar(con, "lupa_plan_sq_null", exacto = FALSE, info = "sqlite-all-null")

  DBI::dbExecute(con, "CREATE TABLE lupa_plan_sq_mix (u INTEGER, w INTEGER, v TEXT)")
  DBI::dbExecute(con, "INSERT INTO lupa_plan_sq_mix VALUES (NULL, 10, 'a'), (NULL, 20, 'b')")
  .pcm_verificar(con, "lupa_plan_sq_mix", exacto = FALSE, info = "sqlite-mix")
})

test_that("DuckDB cumple el plan en tablas de 1 fila, vacias y toda NULL", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "CREATE TABLE lupa_plan_dk_una (id INTEGER NOT NULL, pid INTEGER, v VARCHAR)")
  DBI::dbExecute(con, "CREATE UNIQUE INDEX lupa_plan_dk_una_u ON lupa_plan_dk_una(id)")
  DBI::dbExecute(con, "INSERT INTO lupa_plan_dk_una VALUES (1, 999, 'x')")
  .pcm_verificar(con, "lupa_plan_dk_una", exacto = TRUE, info = "duckdb-1fila")

  DBI::dbExecute(con, "CREATE TABLE lupa_plan_dk_vacia (pid INTEGER)")
  .pcm_verificar(con, "lupa_plan_dk_vacia", exacto = TRUE, info = "duckdb-vacia")

  DBI::dbExecute(con, "CREATE TABLE lupa_plan_dk_null (u INTEGER, v VARCHAR)")
  DBI::dbExecute(con, "INSERT INTO lupa_plan_dk_null VALUES (NULL, 'a'), (NULL, 'a')")
  .pcm_verificar(con, "lupa_plan_dk_null", exacto = TRUE, info = "duckdb-all-null")

  DBI::dbExecute(con, "CREATE TABLE lupa_plan_dk_mix (u INTEGER, w INTEGER, v VARCHAR)")
  DBI::dbExecute(con, "INSERT INTO lupa_plan_dk_mix VALUES (NULL, 10, 'a'), (NULL, 20, 'b')")
  .pcm_verificar(con, "lupa_plan_dk_mix", exacto = TRUE, info = "duckdb-mix")
})

.pcm_probar_pg <- function(puerto, etiqueta) {
  con <- .pcm_conectar_pg(puerto)
  tablas <- c("lupa_plan_pg_f8null", "lupa_plan_pg_f8mix", "lupa_plan_pg_una")
  limpiar <- function() {
    for (tb in rev(tablas)) {
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tb)), silent = TRUE)
    }
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  # float8 activa separar_desvio = TRUE en PostgreSQL: toda NULL no debe reintentar desvio
  DBI::dbExecute(con, "CREATE TABLE lupa_plan_pg_f8null (x DOUBLE PRECISION, v TEXT)")
  DBI::dbExecute(con, "INSERT INTO lupa_plan_pg_f8null VALUES (NULL, 'a'), (NULL, 'a')")
  .pcm_verificar(con, "lupa_plan_pg_f8null", exacto = FALSE, info = paste0(etiqueta, "-f8null"))

  DBI::dbExecute(con, "CREATE TABLE lupa_plan_pg_f8mix (x DOUBLE PRECISION, y DOUBLE PRECISION, v TEXT)")
  DBI::dbExecute(con, "INSERT INTO lupa_plan_pg_f8mix VALUES (NULL, 1.5, 'a'), (NULL, 2.5, 'b')")
  .pcm_verificar(con, "lupa_plan_pg_f8mix", exacto = FALSE, info = paste0(etiqueta, "-f8mix"))

  # basicos + mediana sin validos sobre float8 reutiliza cuenta del lote plano
  DBI::dbExecute(con, "CREATE TABLE lupa_plan_pg_una (id INTEGER NOT NULL, x DOUBLE PRECISION)")
  DBI::dbExecute(con, "CREATE UNIQUE INDEX lupa_plan_pg_una_u ON lupa_plan_pg_una(id)")
  DBI::dbExecute(con, "INSERT INTO lupa_plan_pg_una VALUES (1, 10.5), (2, 20.5)")
  .pcm_verificar(con, "lupa_plan_pg_una", exacto = TRUE,
                 metricas = c("basicos", "mediana"),
                 info = paste0(etiqueta, "-basicos-mediana"))
}

test_that("PostgreSQL actual cumple el plan en float8 toda NULL y basicos+mediana", {
  .pcm_probar_pg(Sys.getenv("LUPA_PG_PORT", "55432"), "pg16")
})

test_that("PostgreSQL 9.3 cumple el plan en float8 toda NULL y basicos+mediana", {
  .pcm_probar_pg(Sys.getenv("LUPA_PG93_PORT", "55493"), "pg93")
})

.pcm_probar_mysql_maria <- function(maria = FALSE) {
  con <- .pcm_conectar_mysql(maria = maria)
  etiqueta <- if (maria) "mariadb" else "mysql"
  limpiar <- function() {
    try(DBI::dbExecute(con, "SET FOREIGN_KEY_CHECKS = 1"), silent = TRUE)
    for (tb in c("lupa_plan_fkoff", "lupa_plan_ref", "lupa_plan_myisam", "lupa_plan_desvio_null")) {
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tb)), silent = TRUE)
    }
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  # Caso H1a: MyISAM con 1 fila (2 columnas numericas con n_validos = 1)
  DBI::dbExecute(con, "CREATE TABLE lupa_plan_ref (id INT PRIMARY KEY) ENGINE=MyISAM")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_plan_myisam (id INT NOT NULL, pid INT, v VARCHAR(20), ",
    "UNIQUE KEY lupa_plan_myisam_u (id), FOREIGN KEY (pid) REFERENCES lupa_plan_ref(id)) ",
    "ENGINE=MyISAM"
  ))
  DBI::dbExecute(con, "INSERT INTO lupa_plan_myisam VALUES (1, 999, 'x')")
  res_my <- .pcm_verificar(con, "lupa_plan_myisam", exacto = TRUE,
                           info = paste0(etiqueta, "-myisam-1fila"))
  # La sonda de mediana escalar ya no se emite ni falla por sintaxis en MySQL/MariaDB
  sondas <- res_my$perfil$resumen_tabla$meta$consultas$sondas_fallidas
  expect_false(any(as.character(sondas$etapa) == "sonda_mediana_escalar"),
               info = paste0(etiqueta, "-sin-sonda-mediana-escalar"))

  # Caso H1b: FOREIGN_KEY_CHECKS = 0 en tabla vacia (0 filas) y con 1 fila
  DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_plan_ref")
  DBI::dbExecute(con, "CREATE TABLE lupa_plan_ref (id INT PRIMARY KEY) ENGINE=InnoDB")
  DBI::dbExecute(con, "CREATE TABLE lupa_plan_fkoff (pid INT) ENGINE=InnoDB")
  DBI::dbExecute(con, "ALTER TABLE lupa_plan_fkoff ADD CONSTRAINT lupa_plan_fkoff_fk FOREIGN KEY (pid) REFERENCES lupa_plan_ref(id)")
  DBI::dbExecute(con, "SET FOREIGN_KEY_CHECKS = 0")
  .pcm_verificar(con, "lupa_plan_fkoff", exacto = FALSE,
                 info = paste0(etiqueta, "-fkoff-vacia"))
  DBI::dbExecute(con, "INSERT INTO lupa_plan_fkoff VALUES (999)")
  .pcm_verificar(con, "lupa_plan_fkoff", exacto = TRUE,
                 info = paste0(etiqueta, "-fkoff-1fila"))
  DBI::dbExecute(con, "SET FOREIGN_KEY_CHECKS = 1")

  # Caso H1c: metricas = "desvio" sin "basicos" (separar_desvio = TRUE) sobre toda NULL
  DBI::dbExecute(con, "CREATE TABLE lupa_plan_desvio_null (x INT NULL, y INT NULL)")
  DBI::dbExecute(con, "INSERT INTO lupa_plan_desvio_null VALUES (NULL, 10), (NULL, 20)")
  .pcm_verificar(con, "lupa_plan_desvio_null", exacto = TRUE, metricas = "desvio",
                 info = paste0(etiqueta, "-desvio-sin-basicos-mix"))
}

test_that("MySQL cumple el plan en MyISAM 1 fila, FOREIGN_KEY_CHECKS=0 y desvio toda NULL", {
  .pcm_probar_mysql_maria(maria = FALSE)
})

test_that("MariaDB cumple el plan en MyISAM 1 fila, FOREIGN_KEY_CHECKS=0 y desvio toda NULL", {
  .pcm_probar_mysql_maria(maria = TRUE)
})

test_that("SQL Server cumple el plan en columnas toda NULL (H2: filtro, vista, CHECK NOCHECK y mixta)", {
  con <- .pcm_conectar_mssql()
  limpiar <- function() {
    try(DBI::dbExecute(con, "IF OBJECT_ID(N'lupa_plan_view', N'V') IS NOT NULL DROP VIEW lupa_plan_view"), silent = TRUE)
    for (tb in c("lupa_plan_filter", "lupa_plan_nncheck", "lupa_plan_ms_mix")) {
      try(DBI::dbExecute(con, paste0("IF OBJECT_ID(N'", tb, "', N'U') IS NOT NULL DROP TABLE ", tb)), silent = TRUE)
    }
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  # Caso H2a: indice filtrado con columna numerica toda NULL
  DBI::dbExecute(con, "CREATE TABLE lupa_plan_filter (u INT NULL, v VARCHAR(20))")
  DBI::dbExecute(con, "CREATE UNIQUE INDEX lupa_plan_filter_u ON lupa_plan_filter(u) WHERE u IS NOT NULL")
  DBI::dbExecute(con, "INSERT INTO lupa_plan_filter VALUES (NULL, 'a'), (NULL, 'a')")
  .pcm_verificar(con, "lupa_plan_filter", exacto = FALSE, info = "mssql-filter-all-null")

  # Caso H2b: vista sobre la misma tabla
  DBI::dbExecute(con, "CREATE VIEW lupa_plan_view AS SELECT u, v FROM lupa_plan_filter")
  .pcm_verificar(con, "lupa_plan_view", exacto = FALSE, info = "mssql-view-all-null")

  # Caso H2c: CHECK WITH NOCHECK con columna numerica toda NULL
  DBI::dbExecute(con, "CREATE TABLE lupa_plan_nncheck (v INT NULL)")
  DBI::dbExecute(con, "INSERT INTO lupa_plan_nncheck VALUES (NULL)")
  DBI::dbExecute(con, "ALTER TABLE lupa_plan_nncheck WITH NOCHECK ADD CONSTRAINT lupa_plan_nncheck_ck CHECK (v IS NOT NULL)")
  .pcm_verificar(con, "lupa_plan_nncheck", exacto = FALSE, info = "mssql-nncheck-all-null")

  # Caso H2d: columna toda NULL junto a columna numerica con datos -> exacto == plan_total
  DBI::dbExecute(con, "CREATE TABLE lupa_plan_ms_mix (u INT NULL, w INT NULL, v VARCHAR(20))")
  DBI::dbExecute(con, "INSERT INTO lupa_plan_ms_mix VALUES (NULL, 10, 'a'), (NULL, 20, 'b')")
  .pcm_verificar(con, "lupa_plan_ms_mix", exacto = TRUE, info = "mssql-mix-exacto")
})
