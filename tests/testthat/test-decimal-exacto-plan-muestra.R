# Las sumas exactas de los decimales van en el lote de agregados, sin una
# consulta de mas, y el plan cuenta lo mismo que la corrida.
#
# Lo que pasaba:
# * bajo la muestra del motor con `solo_agregados`, la media y el desvio de un
#   `numeric(38,0)` de 9007199254740993, ...995 y ...997 quedaban
#   `no_disponible`: la guarda del decimal de 2^53 tapaba las sumas exactas;
# * con `metricas = c("basicos", "desvio")` el plan no contaba el `COUNT(*)`
#   que la corrida pedia suelto (13 contra 14 en PostgreSQL);
# * con `max_consultas = 12` -el minimo del plan de antes- el desvio de un
#   `numeric(18,2)` de 5e14 quedaba `no_disponible`: la consulta de las sumas
#   no entraba, y tampoco con el total del plan, que se llevaba el `COUNT(*)`.
#
# Las esperadas son exactas: fracciones sobre el texto de cada valor,
# redondeadas al doble mas cercano fuera de R y escritas en hexadecimal.
testthat::skip_on_cran()

.dplan_probar <- function(con, prefijo, tipo) {
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

test_that("DuckDB: la muestra del motor, el plan y el presupuesto justo", {
  con <- .dlot_duck()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  .dplan_probar(con, "dplan", "DECIMAL(%d,%d)")
})

test_that("PostgreSQL: la muestra del motor, el plan y max_consultas = 12", {
  con <- .dlot_pg()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dplan_probar(con, "lupa_dplan_pg", "NUMERIC(%d,%d)")
  # El minimo del plan de antes alcanza: las sumas no cuestan una consulta.
  tabla <- "lupa_dplan_pg_doce"
  .dlot_tabla(con, tabla, c(x = "numeric(18,2)"),
              list(x = c("500000000000006.60", "500000000000009.60",
                         "500000000000003.60")))
  on.exit(try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)),
              silent = TRUE), add = TRUE, after = FALSE)
  perfil <- .dlot_perfil(con, tabla, metricas = c("basicos", "desvio"),
                         max_consultas = 12L)
  .dlot_esperar(perfil, "x", 0x1.c6bf52634006ap+48, 0x1.8p+1,
                info = "max_consultas = 12")
})

test_that("SQL Server: la muestra del motor, el plan y el presupuesto justo", {
  con <- .dlot_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dplan_probar(con, "lupa_dplan_mssql", "DECIMAL(%d,%d)")
})
