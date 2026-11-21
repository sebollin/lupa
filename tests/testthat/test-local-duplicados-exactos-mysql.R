# Las pruebas de MySQL y MariaDB necesitan RMySQL y RMariaDB, que el paquete no
# declara: viven en un archivo local (`test-local-*`, fuera del tarball), como las
# demas pruebas de esos motores, y se saltean solas sin el motor.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.de_conectar_mysql <- function(maria = FALSE) {
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

.de_probar_mysql_maria <- function(maria = FALSE) {
  con <- .de_conectar_mysql(maria = maria)
  limpiar <- function() {
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_garantias_de_my"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_garantias_de_pt"), silent = TRUE)
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_garantias_de_my (",
    "c CHAR(5), v VARCHAR(10), d DECIMAL(25,20), ts DATETIME(6), f DOUBLE",
    ") CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_garantias_de_my VALUES ",
    "('a', 'A', 1.0, '2026-01-01 12:00:00.000001', -0.0), ",
    "('a  ', 'A', 1.00, '2026-01-01 12:00:00.000001', 0.0), ",
    "('a', 'a', 1.0, '2026-01-01 12:00:00.000001', 0.0), ",
    "('p', 'x', 2.0, '2026-01-01 12:00:00.000001', 1.0), ",
    "('p', 'x  ', 2.0, '2026-01-01 12:00:00.000001', 1.0), ",
    "('d', 'D', 1.00000000000000000001, '2026-01-01 12:00:00.000001', 2.0), ",
    "('d', 'D', 1.00000000000000000002, '2026-01-01 12:00:00.000001', 2.0), ",
    "('h', 'H', 3.0, '2026-01-01 12:00:00.000001', 3.0), ",
    "('h', 'H', 3.0, '2026-01-01 12:00:00.000002', 3.0), ",
    "(NULL, NULL, NULL, NULL, NULL), ",
    "(NULL, NULL, NULL, NULL, NULL)"
  ))

  plan <- plan_perfilado_dbi(con, "lupa_garantias_de_my", muestra = 100L)
  perf <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_de_my", muestra = 100L, proteger_datos_personales = FALSE
  )))
  expect_equal(perf$resumen_tabla$meta$duplicados_exactos$filas_duplicadas, 2)
  expect_equal(perf$perfil_muestra$general$filas_duplicadas, 2)
  expect_gte(perf$resumen_tabla$meta$consultas$emitidas, attr(plan, "total"))
  expect_lte(perf$resumen_tabla$meta$consultas$emitidas, attr(plan, "total_maximo"))

  DBI::dbExecute(con, "CREATE TABLE lupa_garantias_de_pt (id INT, g POINT)")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_de_pt VALUES (1, ST_GeomFromText('POINT(1 1)')), (1, ST_GeomFromText('POINT(1 1)'))")
  perf_pt <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_de_pt", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))
  expect_true(is.na(perf_pt$resumen_tabla$meta$duplicados_exactos$filas_duplicadas))
  expect_identical(perf_pt$resumen_tabla$meta$duplicados_exactos$estado, "no_disponible")
}

test_that("MySQL vence _ci y PAD SPACE y coincide con R en duplicados exactos", {
  .de_probar_mysql_maria(maria = FALSE)
})

test_that("MariaDB vence _ci y PAD SPACE y coincide con R en duplicados exactos", {
  .de_probar_mysql_maria(maria = TRUE)
})

