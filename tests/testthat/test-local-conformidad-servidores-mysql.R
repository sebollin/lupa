# conformidad_dbi() en MySQL (RMySQL) y MariaDB (RMariaDB), cuando el servicio
# existe: la bateria cuenta en el servidor con el tipo y la intercalacion de
# cada columna real. Una columna BOOLEAN es TINYINT(1): R la lee como entero, y
# entre un logico y un numero TRUE es 1 en R como en el motor; la bateria es
# conforme y medir_dbi() cuenta esa regla en el motor.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.csrv_conectar_mysql <- function(maria = FALSE) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  prefijo <- if (maria) "LUPA_MARIA" else "LUPA_MYSQL"
  clave <- Sys.getenv(if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD", unset = "")
  skip_if(!nzchar(clave), paste0("Sin clave para ", prefijo, "."))
  con <- tryCatch({
    if (maria) {
      DBI::dbConnect(
        RMariaDB::MariaDB(), host = Sys.getenv("LUPA_MARIA_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_MARIA_PORT", "33306")),
        dbname = Sys.getenv("LUPA_MARIA_NAME", "lupa"),
        username = Sys.getenv("LUPA_MARIA_USER", "root"), password = clave
      )
    } else {
      DBI::dbConnect(
        RMySQL::MySQL(), host = Sys.getenv("LUPA_MYSQL_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_MYSQL_PORT", "33307")),
        dbname = Sys.getenv("LUPA_MYSQL_NAME", "lupa"),
        username = Sys.getenv("LUPA_MYSQL_USER", "root"), password = clave
      )
    }
  }, error = function(e) NULL)
  skip_if(is.null(con), paste0(prefijo, " no disponible."))
  con
}

.csrv_fecha_mysql <- as.Date("2026-01-01")

.csrv_medir_mysql <- function(mod, con, ...) {
  suppressWarnings(medir_dbi(mod, con, id_medicion = "c", fecha = .csrv_fecha_mysql,
                             instrumentar = FALSE, ...))
}

.csrv_probar_mysql <- function(maria) {
  con <- .csrv_conectar_mysql(maria)
  motor <- if (maria) "mariadb" else "mysql"
  tabla <- paste0("lupa_confsrv_t_", motor)
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
  # La binaria NO PAD: con PAD SPACE la guarda de espacios finales se abstiene.
  binaria <- if (maria) "utf8mb4_nopad_bin" else "utf8mb4_0900_bin"
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (n DOUBLE, t VARCHAR(32), tb VARCHAR(32) ",
    "COLLATE ", binaria, ", b BOOLEAN)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES (1, 'A', 'A', 1), (5, 'a', 'B', 0), ",
    "(NULL, NULL, NULL, NULL), (2.5, 'x', 'x', 1)"
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
    inst(nuc$ValoresPosiblesPorExtension, "Eb", "tb", valores = c("A", "B")),
    inst(nuc$ValoresPosiblesPorExtension, "Et", "t", valores = c("A", "B"))
  )
  bateria <- conformidad_dbi(con, mod)
  expect_identical(attr(bateria, "motor", exact = TRUE), motor)
  expect_false(any(bateria$conformidad %in% c("no_verificado", "no_conforme")))
  esperada <- as.character(DBI::dbGetQuery(con, "SELECT @@collation_database AS c")$c)
  expect_identical(attr(bateria, "intercalacion", exact = TRUE), esperada)
  expect_false(is.na(attr(bateria, "version", exact = TRUE)))
  for (nombre in c("NN", "R", "P", "Eb")) {
    filas <- bateria[bateria$metrica_instanciada == nombre &
                       bateria$caso != "limites_texto_no_ascii", , drop = FALSE]
    expect_gt(nrow(filas), 0L)
    expect_true(all(filas$conformidad == "conforme"), info = paste(motor, nombre))
    expect_true(all(filas$esquema == "tabla"), info = paste(motor, nombre))
    expect_identical(filas$n_motor, filas$n_r, info = paste(motor, nombre))
    expect_identical(filas$denominador_motor, filas$denominador_r,
                     info = paste(motor, nombre))
  }
  # La intercalacion de la base no distingue mayusculas: medir_dbi() no cuenta
  # esa regla en el motor, y la bateria se abstiene.
  et <- bateria[bateria$metrica_instanciada == "Et", , drop = FALSE]
  expect_true(all(et$conformidad == "abstencion"))
  expect_true(all(grepl("no compara por bytes", et$motivo, fixed = TRUE, useBytes = TRUE)))
  sin <- .csrv_medir_mysql(mod, con)
  expect_gt(nrow(sin), 0L)
  expect_identical(.csrv_medir_mysql(mod, con, conformidad = bateria), sin)
  lugar <- lupa:::.lugar_del_motor_dbi(con)
  expect_identical(names(lugar$servidor), c("anfitrion", "puerto", "base"))
  expect_false(anyNA(lugar$servidor))

  # TINYINT(1) contra TRUE: el motor y R cuentan 1 = TRUE.
  logica <- modelo(inst(nuc$ValoresPosiblesPorExtension, "El", "b", valores = TRUE))
  bl <- conformidad_dbi(con, logica)
  # Con la regla logico-numero (TRUE es 1, como el motor) la bateria es conforme.
  expect_true(all(bl$conformidad == "conforme"))
  en_r <- suppressWarnings(medir(
    logica, stats::setNames(list(DBI::dbReadTable(con, tabla)), tabla),
    id_medicion = "c", fecha = .csrv_fecha_mysql, detalle = "resumen"
  ))
  por_bloques <- .csrv_medir_mysql(logica, con, conformidad = bl,
                                   estrategia_no_sql = "bloques")
  expect_identical(c(por_bloques$numerador, por_bloques$denominador),
                   c(en_r$numerador, en_r$denominador))
  sin_bloques <- .csrv_medir_mysql(logica, con, conformidad = bl)
  expect_identical(nrow(sin_bloques), 1L)
  expect_identical(c(sin_bloques$numerador, sin_bloques$denominador),
                   c(en_r$numerador, en_r$denominador))
}

test_that("MySQL: la bateria cuenta en el servidor lo mismo que R", {
  .csrv_probar_mysql(FALSE)
})

test_that("MariaDB: la bateria cuenta en el servidor lo mismo que R", {
  .csrv_probar_mysql(TRUE)
})
