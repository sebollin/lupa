# En MySQL el controlador entrega `TIMESTAMP`, `DATETIME` y `DATE` como texto.
# El motivo con el que el resumen de fechas declara esas columnas tiene que
# nombrar el tipo del motor, que es de fecha, y no decir que es texto; una
# columna que de verdad es texto sigue diciendolo.
testthat::skip_on_cran()

.mft_conectar <- function() {
  testthat::skip_if_not_installed("RMySQL")
  clave <- Sys.getenv("LUPA_MYSQL_PWD")
  if (!nzchar(clave)) testthat::skip("falta la clave del motor")
  tryCatch(DBI::dbConnect(
    RMySQL::MySQL(), host = Sys.getenv("LUPA_MYSQL_HOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("LUPA_MYSQL_PORT", "33307")),
    dbname = Sys.getenv("LUPA_MYSQL_NAME", "lupa"),
    username = Sys.getenv("LUPA_MYSQL_USER", "root"), password = clave
  ), error = function(e) testthat::skip("motor local no disponible"))
}

testthat::test_that("MySQL: el motivo de una fecha leida como texto nombra el tipo del motor", {
  con <- .mft_conectar()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabla <- "lupa_pg93c_motivo_fecha"
  borrar <- function() {
    try(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", tabla)), silent = TRUE)
  }
  borrar()
  on.exit(borrar(), add = TRUE, after = FALSE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (marca TIMESTAMP NULL, alta DATETIME NULL, ",
    "dia DATE NULL, texto VARCHAR(20) NULL)"
  ))
  for (i in 1:4) {
    DBI::dbExecute(con, sprintf(
      "INSERT INTO %s VALUES ('2024-01-0%d 10:00:00', '2024-01-0%d 10:00:00', '2024-01-0%d', '2024-01-0%d')",
      tabla, i, i, i, i
    ))
  }
  r <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, resumen_fechas = TRUE, proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE
  )))
  s <- r$resumen_tabla$sql
  motivo_de <- function(columna) {
    m <- unique(s$motivo[s$columna == columna & s$estado == "no_disponible"])
    m[!is.na(m)]
  }
  for (columna in c("marca", "alta", "dia")) {
    m <- motivo_de(columna)
    testthat::expect_identical(length(m), 1L, info = columna)
    testthat::expect_false(grepl("es texto en el motor", m, fixed = TRUE),
                           info = columna)
    testthat::expect_match(m, "el controlador la entrega como texto", fixed = TRUE,
                           info = columna)
  }
  m <- motivo_de("texto")
  testthat::expect_true(length(m) <= 1L)
  if (length(m)) testthat::expect_match(m, "es texto en el motor", fixed = TRUE)
})
