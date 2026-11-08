# Lo compartido por las pruebas de la muestra del motor con `solo_agregados`:
# el desvio sale de la sentencia que vio la muestra publicada, no de otra que
# sortea de nuevo (`test-muestra-misma-sentencia-dbi.R` y la de MySQL y
# MariaDB, aparte porque sus controladores no estan en Suggests).
#
# El sorteo de otra sentencia es aleatorio; para que la prueba no dependa del
# azar, la tabla tiene 40 filas y la muestra pide 100 -el lote ve la tabla
# entera y su desvio es `sd(x)`-, y a la sonda de magnitud y al repliegue
# escalado, cuando son sentencias propias, se les responde lo que responde una
# muestra vacia: la consulta corre y sus valores llegan nulos, y la cuenta en
# cero.

.dms_valores <- (1:40) * 1.1

.dms_tabla <- function(con, tabla, tipos) {
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (", paste(names(tipos), tipos, collapse = ", "),
    ")"
  ))
  filas <- sprintf("(%d, %.1f)", 1:40, .dms_valores)
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ", ")
  ))
  invisible(tabla)
}

# Una sentencia propia que sortea de nuevo y le toca una muestra vacia.
.dms_vaciar <- function(sonda = TRUE, repliegue = TRUE) {
  real <- lupa:::.consultar_dbi
  function(conexion, sql, presupuesto = NULL, filas = -1L,
           etapa = "consulta") {
    r <- real(conexion, sql, presupuesto, filas, etapa)
    vaciar <- (sonda && identical(etapa, "sonda_magnitud")) ||
      (repliegue && grepl("lupa_escala", sql, fixed = TRUE, useBytes = TRUE))
    if (vaciar && isTRUE(r$ok) && is.data.frame(r$datos)) {
      for (nombre in names(r$datos)) {
        r$datos[[nombre]] <- if (endsWith(nombre, "n_validos")) 0L else NA
      }
    }
    r
  }
}

.dms_esperar <- function(perfil, info) {
  sql <- perfil$resumen_tabla$sql
  columnas <- perfil$resumen_tabla$columnas
  fila <- sql$columna == "x" & sql$metrica == "desvio"
  expect_identical(sql$estado[fila], "estimado", info = info)
  expect_equal(columnas$desvio[columnas$columna == "x"], sd(.dms_valores),
               tolerance = 1e-12, info = info)
}

# La sonda de magnitud que sorteaba aparte (todos los motores) y el repliegue
# escalado de un doble en SQL Server, que la muestra con
# `metricas = "desvio"` forzaba sin necesidad.
.dms_probar <- function(con, tabla, tipos, repliegue = FALSE) {
  .dms_tabla(con, tabla, tipos)
  argumentos <- list(
    con, tabla, metricas = "desvio", universo = "muestra_motor",
    muestra_motor = 100L, bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE,
    fecha = as.POSIXct("2026-11-08 12:00:00", tz = "UTC")
  )
  escenarios <- list(sonda = c(TRUE, FALSE))
  if (repliegue) escenarios$repliegue <- c(FALSE, TRUE)
  for (nombre in names(escenarios)) {
    e <- escenarios[[nombre]]
    testthat::local_mocked_bindings(
      .consultar_dbi = .dms_vaciar(sonda = e[[1]], repliegue = e[[2]]),
      .package = "lupa"
    )
    perfil <- suppressWarnings(suppressMessages(do.call(
      perfilar_dbi, argumentos
    )))
    .dms_esperar(perfil, paste(tabla, nombre))
  }
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
}
