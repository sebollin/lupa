# La media y el desvio de un DECIMAL de MySQL y MariaDB son los dobles mas
# cercanos de los exactos, por las tres vias. Los casos y las comprobaciones
# estan en `helper-decimal-exacto-tabla.R`; esta prueba vive aparte porque
# los controladores de MySQL y MariaDB no estan en Suggests.
testthat::skip_on_cran()

.dext_mysql <- function(maria) {
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

test_that("MySQL y MariaDB: la media y el desvio de un DECIMAL, por las tres vias", {
  for (maria in c(FALSE, TRUE)) {
    con <- .dext_mysql(maria)
    .dext_probar(con, if (maria) "lupa_dext_maria" else "lupa_dext_mysql",
                 "DECIMAL(65,%d)")
    DBI::dbDisconnect(con)
  }
})
