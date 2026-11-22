# Pruebas locales de la compilacion en MySQL y MariaDB: toda sentencia compila
# con PREPARE, que no ejecuta ninguna subconsulta y rechaza lo mismo que EXPLAIN.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.clsm_parametros <- function(maria) {
  list(
    paquete = if (maria) "RMariaDB" else "RMySQL",
    host = Sys.getenv(if (maria) "LUPA_MARIA_HOST" else "LUPA_MYSQL_HOST", "127.0.0.1"),
    puerto = as.integer(Sys.getenv(if (maria) "LUPA_MARIA_PORT" else "LUPA_MYSQL_PORT",
                                   if (maria) "33306" else "33307")),
    base = Sys.getenv(if (maria) "LUPA_MARIA_NAME" else "LUPA_MYSQL_NAME", "lupa"),
    usuario = Sys.getenv(if (maria) "LUPA_MARIA_USER" else "LUPA_MYSQL_USER", "root"),
    clave = Sys.getenv(if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD", unset = "")
  )
}

.clsm_conectar <- function(maria, usuario = NULL, clave = NULL) {
  p <- .clsm_parametros(maria)
  skip_if_not_installed(p$paquete)
  skip_if(!nzchar(p$clave), paste0("Sin clave para ", if (maria) "MariaDB" else "MySQL", "."))
  usuario <- usuario %||% p$usuario
  clave <- clave %||% p$clave
  con <- tryCatch({
    if (maria) {
      DBI::dbConnect(RMariaDB::MariaDB(), host = p$host, port = p$puerto,
                     dbname = p$base, username = usuario, password = clave)
    } else {
      DBI::dbConnect(RMySQL::MySQL(), host = p$host, port = p$puerto,
                     dbname = p$base, username = usuario, password = clave)
    }
  }, error = function(e) NULL)
  skip_if(is.null(con), paste0(if (maria) "MariaDB" else "MySQL", " no disponible."))
  con
}

.clsm_probar_prepare <- function(maria) {
  con <- .clsm_conectar(maria)
  motor <- if (maria) "mariadb" else "mysql"
  tabla <- paste0("lupa_compilar_sesion_", motor)
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (id INTEGER, x DOUBLE, t VARCHAR(8))"))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES (1, 0.5, 'a'), (2, 1.5, NULL), (3, 2.5, 'b')"
  ))
  # `EXPLAIN` ejecuta estas cuatro formas para planificar el resto (1 s cada
  # una con `SLEEP(1)`); `PREPARE`, ninguna.
  formas <- c(
    from = "SELECT * FROM (SELECT SLEEP(1) AS s) AS q",
    join = paste0("SELECT id FROM ", tabla, " JOIN (SELECT SLEEP(1) AS s) AS q ON 1 = 1"),
    coma = paste0("SELECT id FROM ", tabla, ", (SELECT SLEEP(1) AS s) AS q"),
    with = "WITH q AS (SELECT SLEEP(1) AS s) SELECT * FROM q"
  )
  tiempo <- system.time(compilado <- .with_compilador_dbi(con, motor, {
    for (sql in formas) .compilar_sql_dbi(con, sql, "subconsulta", motor)
  }))[["elapsed"]]
  expect_identical(compilado$compilacion$estado, rep("compilada", 4L))
  expect_identical(compilado$compilacion$metodo, rep("PREPARE", 4L))
  expect_lt(tiempo, 1.5)

  # El plan entero va por PREPARE; la forma candidata del desvio que el motor
  # no conoce (`STDEV`) es una candidata rechazada, y el plan sano no tiene
  # ninguna consulta que no compile.
  plan <- plan_perfilado_dbi(con, tabla, compilar = TRUE)
  compilacion <- attr(plan, "compilacion", exact = TRUE)
  enviadas <- !is.na(compilacion$metodo)
  expect_true(any(enviadas))
  expect_true(all(compilacion$metodo[enviadas] == "PREPARE"))
  expect_identical(sum(compilacion$estado == "no_compilada"), 0L)
  rechazadas <- compilacion[compilacion$estado == "candidata_rechazada", ]
  expect_true(nrow(rechazadas) >= 1L)
  expect_true(all(grepl("^SELECT (STDDEV_SAMP|STDEV)\\(", rechazadas$sql)))
  expect_true(all(nzchar(rechazadas$motivo)))
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "compilar_NN"), tabla, "x"))
  medicion <- attr(plan_medicion_dbi(mod, con, compilar = TRUE), "compilacion", exact = TRUE)
  expect_true(all(medicion$metodo[!is.na(medicion$metodo)] == "PREPARE"))
  expect_identical(sum(medicion$estado == "no_compilada"), 0L)
  # La sentencia preparada se libera: no queda nada que ejecutar con su nombre.
  expect_error(DBI::dbExecute(con, "EXECUTE lupa_compilar_sentencia"))
  # Y la conexion sigue ejecutando.
  expect_equal(as.numeric(DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", tabla))$n), 3)
}

test_that("MySQL compila toda sentencia con PREPARE sin ejecutar subconsultas", {
  .clsm_probar_prepare(FALSE)
})

test_that("MariaDB compila toda sentencia con PREPARE sin ejecutar subconsultas", {
  .clsm_probar_prepare(TRUE)
})

# PREPARE rechaza lo que la credencial no puede leer, como EXPLAIN; y acepta
# una vista que la credencial lee sin ver sus tablas de base, que EXPLAIN
# rechazaba aunque la lectura corre.
.clsm_probar_permisos <- function(maria) {
  adm <- .clsm_conectar(maria)
  motor <- if (maria) "mariadb" else "mysql"
  tabla <- paste0("lupa_compilar_permiso_", motor)
  vista <- paste0("lupa_compilar_permiso_v_", motor)
  usuario <- paste0("lupa_compilar_", motor)
  clave <- paste(sample(c(letters, LETTERS, 0:9), 24L, replace = TRUE), collapse = "")
  limpiar <- function() {
    for (s in c(paste0("DROP USER IF EXISTS '", usuario, "'@'%'"),
                paste0("DROP VIEW IF EXISTS ", vista),
                paste0("DROP TABLE IF EXISTS ", tabla))) {
      try(DBI::dbExecute(adm, s), silent = TRUE)
    }
  }
  on.exit({
    limpiar()
    try(DBI::dbDisconnect(adm), silent = TRUE)
  }, add = TRUE)
  limpiar()
  creado <- tryCatch({
    DBI::dbExecute(adm, paste0("CREATE USER '", usuario, "'@'%' IDENTIFIED BY '", clave, "'"))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!creado, "La credencial no puede crear usuarios.")
  DBI::dbExecute(adm, paste0("CREATE TABLE ", tabla, " (id INTEGER, secreto INTEGER)"))
  DBI::dbExecute(adm, paste0("INSERT INTO ", tabla, " VALUES (1, 10), (2, 20)"))
  DBI::dbExecute(adm, paste0("CREATE VIEW ", vista, " AS SELECT id, secreto FROM ", tabla))
  DBI::dbExecute(adm, paste0("GRANT SELECT (id) ON ", tabla, " TO '", usuario, "'@'%'"))
  DBI::dbExecute(adm, paste0("GRANT SELECT ON ", vista, " TO '", usuario, "'@'%'"))
  con <- .clsm_conectar(maria, usuario, clave)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE, after = FALSE)
  # La vista se lee: es la promesa contra la que se compila.
  expect_equal(nrow(DBI::dbGetQuery(con, paste0("SELECT secreto FROM ", vista))), 2L)
  compilado <- .with_compilador_dbi(con, motor, {
    .compilar_sql_dbi(con, paste0("SELECT id FROM ", tabla), "permitida", motor)
    .compilar_sql_dbi(con, paste0("SELECT secreto FROM ", tabla), "sin_permiso", motor)
    .compilar_sql_dbi(con, paste0("SELECT MAX(secreto) AS m FROM ", vista), "vista", motor)
  })
  expect_identical(
    compilado$compilacion$estado, c("compilada", "no_compilada", "compilada")
  )
  expect_true(nzchar(compilado$compilacion$motivo[[2L]]))
}

test_that("MySQL: PREPARE rechaza la columna sin permiso y acepta la vista legible", {
  .clsm_probar_permisos(FALSE)
})

test_that("MariaDB: PREPARE rechaza la columna sin permiso y acepta la vista legible", {
  .clsm_probar_permisos(TRUE)
})
