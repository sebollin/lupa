# El desvio de la muestra del motor con `solo_agregados` sale de la sentencia
# que vio la muestra, en MySQL y MariaDB (ver
# `test-muestra-misma-sentencia-dbi.R`). Aparte porque sus controladores no
# estan en Suggests.
testthat::skip_on_cran()

.dms_mysql <- function(maria) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  clave <- Sys.getenv(if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD")
  if (!nzchar(clave)) skip("falta la clave del motor")
  tryCatch({
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
  }, error = function(e) skip("motor local no disponible"))
}

test_that("MySQL y MariaDB: el desvio de la muestra no depende de otra sentencia", {
  for (maria in c(FALSE, TRUE)) {
    con <- .dms_mysql(maria)
    .dms_probar(con, if (maria) "lupa_dms_maria" else "lupa_dms_mysql",
                c(id = "INT", x = "DOUBLE"))
    DBI::dbDisconnect(con)
  }
})
