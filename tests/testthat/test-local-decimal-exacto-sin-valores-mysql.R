# Una media y un desvio de decimal sin valores o con uno solo, en MySQL y
# MariaDB (ver `test-decimal-exacto-sin-valores.R`). Aparte porque sus
# controladores no estan en Suggests.
testthat::skip_on_cran()

.dsv_mysql <- function(maria) {
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

test_that("MySQL y MariaDB: sin valores y con uno solo, el motivo es el verdadero", {
  for (maria in c(FALSE, TRUE)) {
    con <- .dsv_mysql(maria)
    .dsv_probar(con, if (maria) "lupa_dsv_maria" else "lupa_dsv_mysql",
                "DECIMAL(18,2)")
    DBI::dbDisconnect(con)
  }
})

# Una tabla sin filas por la muestra del motor, con `metricas = "desvio"`: el
# desvio nulo de la muestra se repite en una consulta sola, y el denominador
# del lote viaja con el, para que la fila sepa que la muestra no tuvo filas.
# Antes publicaba `no_aplica` ("requiere al menos dos valores"), como si la
# muestra hubiera tenido uno.
test_that("MySQL y MariaDB: la muestra vacia del desvio solo dice que esta vacia", {
  for (maria in c(FALSE, TRUE)) {
    con <- .dsv_mysql(maria)
    tabla <- if (maria) "lupa_dsv_maria_mv" else "lupa_dsv_mysql_mv"
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla,
                               " (id integer, d DECIMAL(18,2), x DOUBLE)"))
    perfil <- .dlot_perfil(con, tabla, metricas = "desvio",
                           universo = "muestra_motor", muestra_motor = 10L,
                           bloque_muestra = "solo_agregados")
    for (columna in c("d", "x")) {
      leido <- .dsv_estado(perfil, columna, "desvio")
      info <- paste(tabla, columna)
      expect_identical(leido$estado, "no_disponible", info = info)
      expect_true(startsWith(leido$motivo, "muestra_vacia:"), info = info)
      expect_true(is.na(leido$valor), info = info)
    }
    expect_identical(
      as.numeric(perfil$resumen_tabla$meta$filas_obtenidas_muestra), 0,
      info = tabla
    )
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbDisconnect(con)
  }
})
