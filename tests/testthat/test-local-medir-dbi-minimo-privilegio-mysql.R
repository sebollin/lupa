# MySQL (RMySQL) y MariaDB (RMariaDB) con una credencial de minimo privilegio:
# solo `SELECT` sobre las tablas de la prueba. El contador de modificaciones
# del motor (`performance_schema` en MySQL, `INNODB_SYS_TABLESTATS` en MariaDB,
# que pide `PROCESS`) iba dentro de la consulta que mide, y negado la tumbaba:
# esa credencial no media ninguna metrica. Ahora se lee aparte y, negado, solo
# baja el control de lecturas, con el motivo. Los controladores no son
# dependencias declaradas: archivo local, que se saltea sin el motor.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.minmy_conectar <- function(maria, usuario = NULL, clave = NULL) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  variable <- if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD"
  clave_duenio <- Sys.getenv(variable, unset = "")
  skip_if(!nzchar(clave_duenio), paste0("Sin ", variable, "."))
  puerto <- as.integer(Sys.getenv(
    if (maria) "LUPA_MARIADB_PORT" else "LUPA_MYSQL_PORT",
    Sys.getenv("LUPA_DB_PORT", if (maria) "33306" else "33307")
  ))
  anfitrion <- Sys.getenv(if (maria) "LUPA_MARIADB_HOST" else "LUPA_MYSQL_HOST",
                          Sys.getenv("LUPA_DB_HOST", "127.0.0.1"))
  base <- Sys.getenv(if (maria) "LUPA_MARIADB_DB" else "LUPA_MYSQL_DB",
                     Sys.getenv("LUPA_DB_NAME", "lupa"))
  usuario_duenio <- Sys.getenv(if (maria) "LUPA_MARIADB_USER" else "LUPA_MYSQL_USER",
                               Sys.getenv("LUPA_DB_USER", "root"))
  tryCatch(DBI::dbConnect(
    if (maria) RMariaDB::MariaDB() else RMySQL::MySQL(),
    host = anfitrion, port = puerto, dbname = base,
    username = usuario %||% usuario_duenio, password = clave %||% clave_duenio
  ), error = function(e) NULL)
}

.minmy_probar <- function(maria) {
  motor <- if (maria) "mariadb" else "mysql"
  duenio <- .minmy_conectar(maria)
  skip_if(is.null(duenio), paste(motor, "no disponible."))
  on.exit(DBI::dbDisconnect(duenio), add = TRUE)
  usuario <- "lupa_mdi_minimo"
  clave <- .minp_clave()
  base <- as.character(DBI::dbGetQuery(duenio, "SELECT DATABASE() AS b")$b)
  limpiar <- function() {
    .minp_borrar_tablas(duenio)
    try(DBI::dbExecute(duenio, paste0("DROP USER IF EXISTS '", usuario, "'@'%'")),
        silent = TRUE)
  }
  limpiar()
  # El error de un CREATE USER repite la sentencia, con la clave: no se muestra.
  creado <- tryCatch({
    DBI::dbExecute(duenio, paste0("CREATE USER '", usuario, "'@'%' IDENTIFIED BY '",
                                  clave, "'"))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!creado, "La credencial no puede crear un usuario.")
  on.exit(limpiar(), add = TRUE, after = FALSE)
  .minp_crear_tablas(duenio)
  for (t in .minp_tablas) {
    DBI::dbExecute(duenio, paste0("GRANT SELECT ON `", base, "`.`", t, "` TO '",
                                  usuario, "'@'%'"))
  }
  minimo <- .minmy_conectar(maria, usuario, clave)
  skip_if(is.null(minimo), "El usuario nuevo no puede conectarse.")
  on.exit(DBI::dbDisconnect(minimo), add = TRUE, after = FALSE)
  m <- .minp_comparar(duenio, minimo, motor)
  cm_m <- attr(m$minimo, "ejecucion_dbi", exact = TRUE)$contador_motor
  cm_d <- attr(m$duenio, "ejecucion_dbi", exact = TRUE)$contador_motor
  expect_true(is.data.frame(cm_m) && nrow(cm_m) == 2L, info = motor)
  expect_false(any(cm_m$leido), info = motor)
  expect_true(all(nzchar(cm_m$motivo)), info = motor)
  expect_true(all(cm_d$leido), info = motor)
  expect_true(all(attr(m$minimo, "lecturas", exact = TRUE)$control ==
                    "solo_filas_agregadas_o_quitadas"), info = motor)
}

test_that("MySQL: una credencial de minimo privilegio mide lo mismo que el dueno", {
  .minmy_probar(FALSE)
})

test_that("MariaDB: una credencial de minimo privilegio mide lo mismo que el dueno", {
  .minmy_probar(TRUE)
})
