# Las pruebas de MySQL y MariaDB necesitan RMySQL y RMariaDB, que el paquete no
# declara: viven en un archivo local (`test-local-*`, fuera del tarball), como las
# demas pruebas de esos motores, y se saltean solas sin el motor.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.cg_conectar_mysql <- function(maria = FALSE) {
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

.cg_probar_mysql_maria <- function(maria = FALSE) {
  con <- .cg_conectar_mysql(maria = maria)
  limpiar <- function() {
    try(DBI::dbExecute(con, "SET SESSION foreign_key_checks = 1"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_garantias_cg_myisam"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_garantias_cg_hijo"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_garantias_cg_padre"), silent = TRUE)
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  DBI::dbExecute(con, "CREATE TABLE lupa_garantias_cg_padre (pid INT PRIMARY KEY, cod VARCHAR(20) NOT NULL) ENGINE=InnoDB")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_cg_padre VALUES (1, 'A'), (2, 'B')")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_garantias_cg_hijo (",
    "hid INT NOT NULL, pid INT, txt VARCHAR(30) NOT NULL, ",
    "CONSTRAINT fk_lupa_garantias_cg_pid FOREIGN KEY (pid) REFERENCES lupa_garantias_cg_padre(pid)",
    ") ENGINE=InnoDB"
  ))
  DBI::dbExecute(con, "CREATE UNIQUE INDEX idx_lupa_garantias_cg_hid ON lupa_garantias_cg_hijo(hid)")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_cg_hijo VALUES (10, 1, ''), (20, 2, '   ')")

  plan <- plan_perfilado_dbi(con, "lupa_garantias_cg_hijo", bloque_muestra = "solo_agregados")
  perf <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_cg_hijo", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))
  cat_meta <- perf$resumen_tabla$meta$catalogo
  expect_identical(cat_meta$identificacion$grado, "total")
  expect_true(cat_meta$identificacion$tipo %in% c("indice_unico", "restriccion_unica"))
  expect_identical(cat_meta$claves_foraneas$grado, "total")
  expect_true(isTRUE(cat_meta$claves_foraneas$validada))
  expect_true(isTRUE(cat_meta$not_null$not_null_con_blancos[cat_meta$not_null$columna == "txt"]))
  expect_equal(perf$resumen_tabla$meta$consultas$emitidas, attr(plan, "total"))

  # Con foreign_key_checks = 0 la FK baja a grado ninguna
  DBI::dbExecute(con, "SET SESSION foreign_key_checks = 0")
  perf_off <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_cg_hijo", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))
  expect_identical(perf_off$resumen_tabla$meta$catalogo$claves_foraneas$grado, "ninguna")
  DBI::dbExecute(con, "SET SESSION foreign_key_checks = 1")

  # Tabla MyISAM informa que el motor no valida claves foraneas
  DBI::dbExecute(con, "CREATE TABLE lupa_garantias_cg_myisam (id INT PRIMARY KEY, pid INT) ENGINE=MyISAM")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_cg_myisam VALUES (1, 99)")
  perf_m <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_cg_myisam", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))
  expect_match(
    perf_m$resumen_tabla$meta$catalogo$estado_lectura$claves_foraneas$motivo,
    "MyISAM", fixed = TRUE
  )
}

test_that("MySQL lee garantias del catalogo (InnoDB, MyISAM, foreign_key_checks, plan exacto)", {
  .cg_probar_mysql_maria(maria = FALSE)
})

test_that("MariaDB lee garantias del catalogo (InnoDB, MyISAM, foreign_key_checks, plan exacto)", {
  .cg_probar_mysql_maria(maria = TRUE)
})

