# Los valores ausentes que decide la cuenta del lote de agregados, y la tabla
# sin filas por el spool, en MySQL y MariaDB (ver
# `test-cuenta-sin-valores-dbi.R`). Aparte porque sus controladores no estan
# en Suggests.
testthat::skip_on_cran()

.dcv_mysql <- function(maria) {
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

test_that("MySQL y MariaDB: sin valores o con uno solo, sin validos, decide la cuenta", {
  for (maria in c(FALSE, TRUE)) {
    con <- .dcv_mysql(maria)
    prefijo <- if (maria) "lupa_dcv_maria" else "lupa_dcv_mysql"
    tipos <- c(id = "INT", x = "DOUBLE", e = "INT")
    .dcv_probar(con, prefijo, tipos)
    .dcv_spool_vacia(con, prefijo, tipos)
    DBI::dbDisconnect(con)
  }
})
