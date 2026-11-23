# El plan de las consultas de fechas contra la corrida en MariaDB y MySQL: la
# misma muestra de la grilla que corre en PostgreSQL y SQL Server (ver
# `test-fechas-plan-consultas-motores.R` y `helper-fechas-plan-consultas.R`).
# Aparte porque sus controladores no estan en Suggests. MariaDB cuenta las
# fechas por literales; con RMySQL se declaran o se leen por bloques.
testthat::skip_on_cran()

.fpcl_conectar <- function(maria) {
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

.fpcl_grilla <- function(con, motor, fechas_como_texto = FALSE) {
  muestra <- .fpc_muestra_grilla(
    con, motor, paste0("lupa_fpl_", motor), fechas_como_texto = fechas_como_texto
  )
  expect_identical(muestra$recorridas, 40L)
  # La tabla sucia activo consultas condicionales: si no, la muestra no prueba
  # que una condicional concilie en este motor.
  expect_gt(muestra$condicionales, 3L)
}

test_that("MariaDB: el plan cuenta las consultas de fechas que la corrida emite", {
  con <- .fpcl_conectar(TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .fpcl_grilla(con, "mariadb")
})

test_that("MySQL: el plan cuenta las consultas de fechas que la corrida emite", {
  con <- .fpcl_conectar(FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # RMySQL entrega una columna `DATE` como texto: que es una fecha lo dice la
  # muestra, y su lectura por bloques es condicional.
  expect_true(is.character(
    DBI::dbGetQuery(con, "SELECT CAST('2020-01-01' AS DATE) AS lupa_fecha")$lupa_fecha
  ))
  .fpcl_grilla(con, "mysql", fechas_como_texto = TRUE)
})
