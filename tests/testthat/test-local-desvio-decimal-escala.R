# El catalogo de MySQL y MariaDB publica la escala de un decimal
# (`COLUMN_TYPE`): con escala 0 y valores por debajo de 2^53, los dobles son los
# valores y el desvio por bloques y por la muestra del motor se publica. Con
# decimales, por bloques y por la muestra lo dan las sumas exactas en el motor
# (ver `test-desvio-decimal-escala-dbi.R`).
#
# Usa RMySQL y RMariaDB, que el paquete no declara: ver `test-local-mysql.R`.

.lde_con <- function(maria) {
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

.lde_probar <- function(maria) {
  con <- .lde_con(maria)
  tabla <- if (maria) "lupa_lde_maria" else "lupa_lde_mysql"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  k <- c(0, 3, 7, 1, 9, 4, 2, 8, 5)
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (entero DECIMAL(38,0), centavos DECIMAL(18,2))"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ",
    paste0("(", sprintf("%.0f", 5e14 + k), ", ", sprintf("%.0f", 5e14 + k),
           ".35)", collapse = ", ")
  ))
  fecha <- as.POSIXct("2026-11-06 12:00:00", tz = "UTC")
  for (via in list(
    list(bloque_filas = 2L),
    list(universo = "muestra_motor", muestra_motor = length(k))
  )) {
    p <- suppressWarnings(suppressMessages(do.call(perfilar_dbi, c(list(
      con, tabla, proteger_datos_personales = FALSE, fecha = fecha,
      bloque_muestra = "solo_agregados"
    ), via))))
    sql <- p$resumen_tabla$sql
    columnas <- p$resumen_tabla$columnas
    info <- paste(names(via), collapse = ",")
    estado <- function(col) {
      e <- sql$estado[sql$columna == col & sql$metrica == "desvio"]
      e[[length(e)]]
    }
    expect_true(estado("entero") %in% c("calculado", "estimado"), info = info)
    desvio <- columnas$desvio[columnas$columna == "entero"]
    expect_lt(abs(desvio - stats::sd(k)) / stats::sd(k), 1e-12)
    # 500000000000000.35 no es un doble. Por bloques y por la muestra del motor
    # el desvio sale de las sumas exactas en el motor -la muestra las pide en la
    # misma sentencia que los agregados- y es el doble mas cercano del exacto:
    # la varianza es la de k, 10.
    expect_true(estado("centavos") %in% c("calculado", "estimado"),
                info = info)
    expect_identical(
      as.numeric(columnas$desvio[columnas$columna == "centavos"]), sqrt(10),
      info = info
    )
  }
}

test_that("MySQL publica el desvio de un decimal de escala 0 por bloques y por la muestra", {
  skip_on_cran()
  .lde_probar(maria = FALSE)
})

test_that("MariaDB publica el desvio de un decimal de escala 0 por bloques y por la muestra", {
  skip_on_cran()
  .lde_probar(maria = TRUE)
})
