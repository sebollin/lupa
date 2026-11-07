# Las sumas exactas de un DECIMAL de MySQL y MariaDB con cualquier `metricas`,
# sin valores, con el presupuesto justo y en una tabla `TEMPORARY`, que no esta
# en `information_schema`: sin catalogo el decimal llegaba sin escala y su
# desvio quedaba a un ulp. Aparte porque sus controladores no estan en Suggests;
# lo compartido esta en `helper-decimal-exacto-lotes.R`.
testthat::skip_on_cran()

.dlot_mysql <- function(maria) {
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

test_that("MySQL y MariaDB: cualquier metricas, el presupuesto justo y una tabla TEMPORARY", {
  centavos <- c("500000000000006.60", "500000000000009.60",
                "500000000000005.60", "500000000000007.60")
  for (maria in c(FALSE, TRUE)) {
    con <- .dlot_mysql(maria)
    tabla <- if (maria) "lupa_dlot_met_maria" else "lupa_dlot_met_mysql"
    .dlot_tabla(con, tabla, c(x = "DECIMAL(38,30)"), list(x = .dlot_treinta))
    .dlot_probar_metricas(con, tabla, tabla)
    .dlot_probar_presupuesto(con, tabla, tabla)
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbExecute(con, paste(
      "CREATE TEMPORARY TABLE lupa_dlot_temporal (x DECIMAL(18,2))"
    ))
    DBI::dbExecute(con, paste0(
      "INSERT INTO lupa_dlot_temporal VALUES ",
      paste0("(", centavos, ")", collapse = ", ")
    ))
    for (via in list(list(), list(bloque_filas = 2L))) {
      perfil <- do.call(.dlot_perfil, c(list(con, "lupa_dlot_temporal"), via))
      .dlot_esperar(perfil, "x", 0x1.c6bf526340076p+48, 0x1.b534070e9620cp+0,
                    info = paste(tabla, "temporal", names(via)))
    }
    DBI::dbDisconnect(con)
  }
})
