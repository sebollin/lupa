# Lo compartido por las pruebas de una media y un desvio de decimal sin valores
# o con uno solo (`test-decimal-exacto-sin-valores.R` y la de MySQL y MariaDB,
# aparte porque sus controladores no estan en Suggests).

.dsv_sin_valores <- "La columna no contiene valores no nulos."
.dsv_uno <- "El desv\u00edo muestral requiere al menos dos valores no nulos."

.dsv_estado <- function(perfil, columna, metrica) {
  sql <- perfil$resumen_tabla$sql
  fila <- sql$columna == columna & sql$metrica == metrica
  list(estado = sql$estado[fila], motivo = sql$motivo[fila],
       valor = perfil$resumen_tabla$columnas[[metrica]][
         perfil$resumen_tabla$columnas$columna == columna
       ])
}

.dsv_esperar <- function(perfil, metrica, estado, motivo, info) {
  leido <- .dsv_estado(perfil, "d", metrica)
  expect_identical(leido$estado, estado, info = paste(info, metrica))
  expect_identical(leido$motivo, motivo, info = paste(info, metrica))
  expect_true(is.na(leido$valor), info = paste(info, metrica))
}

.dsv_probar <- function(con, prefijo, tipo) {
  casos <- list(
    vacia = character(),
    nula = c("(1, NULL)", "(2, NULL)"),
    una = c("(1, 1.10)", "(2, NULL)")
  )
  for (caso in names(casos)) {
    tabla <- paste0(prefijo, "_", caso)
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (id integer, d ",
                               tipo, ")"))
    if (length(casos[[caso]])) {
      DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES ",
                                 paste(casos[[caso]], collapse = ", ")))
    }
    for (via in list(list(), list(bloque_muestra = "solo_agregados"))) {
      perfil <- do.call(.dlot_perfil, c(
        list(con, tabla, metricas = c("basicos", "desvio")), via
      ))
      info <- paste(prefijo, caso, names(via))
      if (caso == "una") {
        leido <- .dsv_estado(perfil, "d", "media")
        expect_identical(leido$estado, "calculado", info = info)
        expect_identical(as.numeric(leido$valor), 0x1.199999999999ap+0,
                         info = info)
        .dsv_esperar(perfil, "desvio", "no_aplica", .dsv_uno, info)
      } else {
        .dsv_esperar(perfil, "media", "sin_valores", .dsv_sin_valores, info)
        .dsv_esperar(perfil, "desvio", "sin_valores", .dsv_sin_valores, info)
      }
    }
    # La muestra del motor sin valores validos, con `solo_agregados`.
    if (caso == "nula") {
      perfil <- .dlot_perfil(con, tabla, metricas = c("basicos", "desvio"),
                             universo = "muestra_motor", muestra_motor = 10L,
                             bloque_muestra = "solo_agregados")
      for (metrica in c("media", "desvio")) {
        .dsv_esperar(perfil, metrica, "no_disponible",
                     "muestra_inestable:sin_valores_validos",
                     paste(prefijo, "muestra"))
      }
    }
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  }
}
