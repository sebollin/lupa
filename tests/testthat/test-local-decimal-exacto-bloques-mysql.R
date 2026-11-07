# Por `bloque_filas` y por el spool de la muestra del motor, R recibe el doble
# de cada decimal. La media y la mediana de esos dobles no son las del decimal
# -la de 0,0001 y 0,0002 salia 0x1.3a92a30553262p-13, y la exacta es
# 0x1.3a92a30553261p-13-, y el desvio de un decimal de 5e14 con centavos, de
# los dobles, se aparta una parte en 300: se negaba. Ahora la media y la
# mediana son el doble mas cercano de las del decimal, y el desvio se resta en
# el decimal: por bloques lo calcula el motor, y por el spool sale del texto de
# cada valor.
#
# Las esperadas son exactas: fracciones sobre el texto de cada valor,
# redondeadas al doble mas cercano fuera de R y escritas en hexadecimal; el
# desvio, la raiz de la varianza exacta.
testthat::skip_on_cran()

.dexa_mysql <- function(maria) {
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

test_that("MySQL y MariaDB: la media, la mediana y el desvio de un DECIMAL por bloques y por el spool", {
  for (maria in c(FALSE, TRUE)) {
    con <- .dexa_mysql(maria)
    .dexa_probar(con, if (maria) "lupa_dexa_maria" else "lupa_dexa_mysql")
    DBI::dbDisconnect(con)
  }
})
