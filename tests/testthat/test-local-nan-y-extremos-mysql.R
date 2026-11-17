# NaN y magnitudes extremas contra MySQL y MariaDB en motores locales.
#
# Usan controladores que el paquete no declara (RMySQL, RMariaDB) y motores que
# solo existen en la maquina de desarrollo, asi que no viajan en el paquete:
# `.Rbuildignore` excluye `tests/testthat/test-local-*.R`. La suite de
# desarrollo (`devtools::test()`) las corre igual, desde el arbol fuente.

testthat::skip_on_cran()

.nf_con_rmariadb <- function(puerto, mariadb = FALSE, mysql = NULL) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RMariaDB")
  clave <- Sys.getenv(if (mariadb) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD",
                      unset = "")
  if (!nzchar(clave)) return(NULL)
  argumentos <- list(
    host = Sys.getenv(if (mariadb) "LUPA_MARIADB_HOST" else "LUPA_MYSQL_HOST",
                      "127.0.0.1"), port = puerto, user = "root",
    password = clave, dbname = "lupa"
  )
  if (!is.null(mysql)) argumentos$mysql <- mysql
  tryCatch(
    do.call(DBI::dbConnect, c(list(RMariaDB::MariaDB()), argumentos)),
    error = function(e) NULL
  )
}

.nf_con_rmysql <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RMySQL")
  clave <- Sys.getenv("LUPA_MYSQL_PWD", unset = "")
  if (!nzchar(clave)) return(NULL)
  tryCatch(DBI::dbConnect(
    RMySQL::MySQL(), host = Sys.getenv("LUPA_MYSQL_HOST", "127.0.0.1"),
    port = 33307L, user = "root", password = clave, dbname = "lupa"
  ), error = function(e) NULL)
}

test_that("el desvio extremo queda calculado y finito en MySQL y MariaDB", {
  motores <- list()
  mysql <- .nf_con_rmysql()
  if (!is.null(mysql)) motores$mysql_rmysql <- mysql
  mysql_rmaria <- .nf_con_rmariadb(33307L, mysql = FALSE)
  if (!is.null(mysql_rmaria)) motores$mysql_rmariadb <- mysql_rmaria
  mariadb <- .nf_con_rmariadb(33306L, mariadb = TRUE)
  if (!is.null(mariadb)) motores$mariadb <- mariadb
  if (!length(motores)) skip("MySQL y MariaDB no disponibles.")
  .nf_comprobar_extremos(motores)
})
