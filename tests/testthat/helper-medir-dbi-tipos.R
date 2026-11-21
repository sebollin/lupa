.mdt_fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")

.mdt_inst <- function(met, nombre, entidad, atributo, ...) {
  instanciar(especializar(met, nombre, ...), entidad, atributo,
             nombre_instancia = nombre)
}

.mdt_qid <- function(con, nombre) {
  as.character(DBI::dbQuoteIdentifier(con, nombre))
}

.mdt_drop <- function(con, tabla) {
  try(DBI::dbExecute(con, paste0("DROP TABLE ", .mdt_qid(con, tabla))),
      silent = TRUE)
}

.mdt_comparar <- function(con, tabla, ddl, valores, modelo, sesion = character()) {
  .mdt_drop(con, tabla)
  on.exit(.mdt_drop(con, tabla), add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", .mdt_qid(con, tabla), " (", ddl, ")"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", .mdt_qid(con, tabla), " VALUES ", valores
  ))
  for (sql in sesion) DBI::dbExecute(con, sql)
  datos <- list(d = DBI::dbReadTable(con, tabla))
  esperado <- suppressWarnings(medir(
    modelo, datos, id_medicion = "tipos-test", fecha = .mdt_fecha,
    detalle = "resumen"
  ))
  obtenido <- suppressWarnings(medir_dbi(
    modelo, con, tablas = c(d = tabla), id_medicion = "mab-test",
    fecha = .mdt_fecha
  ))
  expect_gt(nrow(esperado), 0L)
  expect_identical(nrow(obtenido), nrow(esperado))
  campos <- c("metrica_instanciada", "numerador", "denominador", "resultado")
  for (campo in campos) {
    expect_equal(obtenido[[campo]], esperado[[campo]], info = campo)
  }
  invisible(list(datos = datos, esperado = esperado, obtenido = obtenido))
}

.mdt_no_medible <- function(con, tabla, ddl, valores, modelo) {
  .mdt_drop(con, tabla)
  on.exit(.mdt_drop(con, tabla), add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", .mdt_qid(con, tabla), " (", ddl, ")"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", .mdt_qid(con, tabla), " VALUES ", valores
  ))
  resultado <- suppressWarnings(medir_dbi(
    modelo, con, tablas = c(d = tabla), id_medicion = "mab-test",
    fecha = .mdt_fecha
  ))
  expect_length(resultado$metrica_instanciada, 0L)
  cobertura <- attr(resultado, "cobertura_metricas", exact = TRUE)
  expect_true(any(cobertura$estado == "no_medible"))
  invisible(cobertura)
}
