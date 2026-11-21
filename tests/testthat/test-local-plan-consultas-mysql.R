# Las pruebas de MySQL y MariaDB necesitan RMySQL y RMariaDB, que el paquete no
# declara: viven en un archivo local (`test-local-*`, fuera del tarball), como las
# demas pruebas de esos motores, y se saltean solas sin el motor.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

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

