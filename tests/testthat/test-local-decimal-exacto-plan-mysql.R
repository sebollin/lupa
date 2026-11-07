# La muestra del motor con un decimal de 2^53, el plan contra lo emitido y el
# presupuesto justo, en MySQL y MariaDB (ver
# `test-decimal-exacto-plan-muestra.R`).
# Aparte porque sus controladores no estan en Suggests.
testthat::skip_on_cran()

.dplan_mysql <- function(maria) {
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

.dplan_probar_mysql <- function(con, prefijo, tipo) {
  # La muestra del motor, con un decimal de 2^53 o mas.
  tabla <- paste0(prefijo, "_ancho")
  .dlot_tabla(con, tabla, c(x = sprintf(tipo, 38L, 0L)),
              list(x = c("9007199254740993", "9007199254740995",
                         "9007199254740997")))
  for (metricas in list(c("basicos", "desvio"), "desvio")) {
    perfil <- .dlot_perfil(con, tabla, metricas = metricas,
                           universo = "muestra_motor", muestra_motor = 3L,
                           bloque_muestra = "solo_agregados")
    .dlot_esperar(
      perfil, "x",
      media = if ("basicos" %in% metricas) 0x1.0000000000002p+53,
      desvio = 0x1p+1, info = paste(prefijo, paste(metricas, collapse = "+"))
    )
  }
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  # El plan contra lo emitido, sin `validos`.
  tabla <- paste0(prefijo, "_plan")
  .dlot_tabla(con, tabla, c(a = sprintf(tipo, 38L, 30L),
                            b = sprintf(tipo, 38L, 30L), c = "integer"),
              list(a = c("1.10", "2.20", "3.30", "4.40"),
                   b = c("1.10", "2.20", "3.30", "4.40"),
                   c = c("1", "1", "1", "1")))
  for (metricas in list(c("basicos", "desvio"), "desvio", NULL)) {
    argumentos <- c(list(con, tabla),
                    if (!is.null(metricas)) list(metricas = metricas))
    plan <- do.call(plan_perfilado_dbi, argumentos)
    perfil <- do.call(.dlot_perfil, argumentos)
    expect_identical(
      as.numeric(perfil$resumen_tabla$meta$consultas$emitidas),
      as.numeric(attr(plan, "total")),
      info = paste(prefijo, "plan", paste(metricas, collapse = "+"))
    )
  }
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  # Con `max_consultas` en el total del plan, el desvio exacto sale.
  tabla <- paste0(prefijo, "_justo")
  .dlot_tabla(con, tabla, c(x = sprintf(tipo, 18L, 2L)),
              list(x = c("500000000000006.60", "500000000000009.60",
                         "500000000000003.60")))
  plan <- plan_perfilado_dbi(con, tabla, metricas = c("basicos", "desvio"))
  perfil <- .dlot_perfil(con, tabla, metricas = c("basicos", "desvio"),
                         max_consultas = attr(plan, "total"))
  .dlot_esperar(perfil, "x", 0x1.c6bf52634006ap+48, 0x1.8p+1,
                info = paste(prefijo, "max_consultas"))
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
}

test_that("MySQL y MariaDB: la muestra del motor, el plan y el presupuesto justo", {
  for (maria in c(FALSE, TRUE)) {
    con <- .dplan_mysql(maria)
    prefijo <- if (maria) "lupa_dplan_maria" else "lupa_dplan_mysql"
    .dplan_probar_mysql(con, prefijo, "DECIMAL(%d,%d)")
    DBI::dbDisconnect(con)
  }
})
