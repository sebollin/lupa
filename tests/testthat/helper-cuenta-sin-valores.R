# Lo compartido por las pruebas de los valores ausentes que decide la cuenta
# del lote de agregados (`test-cuenta-sin-valores-dbi.R` y la de MySQL y
# MariaDB, aparte porque sus controladores no estan en Suggests): un doble y un
# entero sin valores o con uno solo, sin `validos`, y la tabla sin filas por el
# spool de la muestra del motor.

.dcv_sin_valores <- "La columna no contiene valores no nulos."
.dcv_uno <- "El desv\u00edo muestral requiere al menos dos valores no nulos."
.dcv_muestra <- "muestra_inestable:sin_valores_validos"

.dcv_crear <- function(con, tabla, tipos, filas) {
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (", paste(names(tipos), tipos, collapse = ", "),
    ")"
  ))
  if (length(filas)) {
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ", ")
    ))
  }
  invisible(tabla)
}

.dcv_perfil <- function(con, tabla, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, proteger_datos_personales = FALSE,
    fecha = as.POSIXct("2026-11-07 12:00:00", tz = "UTC"), ...
  )))
}

.dcv_leer <- function(perfil, columna, metrica) {
  sql <- perfil$resumen_tabla$sql
  columnas <- perfil$resumen_tabla$columnas
  fila <- sql$columna == columna & sql$metrica == metrica
  list(
    estado = sql$estado[fila], motivo = sql$motivo[fila],
    valor = if (metrica %in% names(columnas)) {
      columnas[[metrica]][columnas$columna == columna]
    } else NA
  )
}

# El estado y el motivo de `metrica` en `columna`; `motivo` que termina en `:`
# se compara como prefijo.
.dcv_esperar <- function(perfil, columna, metrica, estado, motivo, info) {
  leido <- .dcv_leer(perfil, columna, metrica)
  info <- paste(info, columna, metrica)
  expect_identical(leido$estado, estado, info = info)
  if (endsWith(motivo, ":")) {
    expect_true(isTRUE(startsWith(leido$motivo, motivo)), info = info)
  } else {
    expect_identical(leido$motivo, motivo, info = info)
  }
  expect_true(all(is.na(leido$valor)), info = info)
}

# Sin `validos`, por la tabla completa, la tabla completa con
# `solo_agregados` y la muestra del motor con `solo_agregados`: una columna toda
# `NULL` y una tabla sin filas, `sin_valores` (en la muestra,
# `muestra_inestable:sin_valores_validos`, o `muestra_vacia:` si la muestra no
# tiene filas) en los extremos, la media y el desvio; con un solo valor, la
# media publicada y el desvio `no_aplica`. `tipos`: `id`, un doble `x` y un
# entero `e`.
.dcv_probar <- function(con, prefijo, tipos) {
  casos <- list(
    nula = c("(1, NULL, NULL)", "(2, NULL, NULL)"),
    una = c("(1, 1.5, 3)", "(2, NULL, NULL)"),
    vacia = character()
  )
  vias <- list(
    tabla = list(),
    tabla_sa = list(bloque_muestra = "solo_agregados"),
    motor_sa = list(universo = "muestra_motor", muestra_motor = 100L,
                    bloque_muestra = "solo_agregados")
  )
  juegos <- list(c("basicos", "desvio"), "desvio")
  for (caso in names(casos)) {
    tabla <- paste0(prefijo, "_", caso)
    .dcv_crear(con, tabla, tipos, casos[[caso]])
    for (via in names(vias)) for (metricas in juegos) {
      perfil <- do.call(.dcv_perfil, c(
        list(con, tabla, metricas = metricas), vias[[via]]
      ))
      muestra <- identical(via, "motor_sa")
      info <- paste(prefijo, caso, via, paste(metricas, collapse = "+"))
      pedidas <- c(
        if ("basicos" %in% metricas) c("minimo", "maximo", "media"),
        "desvio"
      )
      for (columna in c("x", "e")) for (metrica in pedidas) {
        if (identical(caso, "una")) {
          if (identical(metrica, "desvio")) {
            .dcv_esperar(perfil, columna, metrica, "no_aplica", .dcv_uno, info)
          } else {
            leido <- .dcv_leer(perfil, columna, metrica)
            expect_identical(leido$estado,
                             if (muestra) "estimado" else "calculado",
                             info = paste(info, columna, metrica))
            expect_identical(as.numeric(leido$valor),
                             if (columna == "x") 1.5 else 3,
                             info = paste(info, columna, metrica))
          }
        } else if (muestra && identical(caso, "vacia")) {
          .dcv_esperar(perfil, columna, metrica, "no_disponible",
                       "muestra_vacia:", info)
        } else if (muestra) {
          .dcv_esperar(perfil, columna, metrica, "no_disponible",
                       .dcv_muestra, info)
        } else {
          .dcv_esperar(perfil, columna, metrica, "sin_valores",
                       .dcv_sin_valores, info)
        }
      }
    }
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  }
}

# Una tabla sin filas por el spool (`con_muestra`): la muestra no tiene filas y
# el resumen lo dice como la via de solo agregados, `muestra_vacia:<metodo>`,
# sin ceros ni estados observados; el perfil de la muestra se arma igual, con
# las columnas y cero filas.
.dcv_spool_vacia <- function(con, prefijo, tipos) {
  tabla <- paste0(prefijo, "_spool")
  .dcv_crear(con, tabla, tipos, character())
  perfil <- .dcv_perfil(con, tabla, universo = "muestra_motor",
                        muestra_motor = 10L)
  sql <- perfil$resumen_tabla$sql
  columnas <- perfil$resumen_tabla$columnas
  alcance <- sql[sql$metrica != "n", , drop = FALSE]
  expect_gt(nrow(alcance), 0L)
  expect_true(all(alcance$estado == "no_disponible"), info = prefijo)
  expect_true(all(startsWith(alcance$motivo, "muestra_vacia:")), info = prefijo)
  expect_equal(as.numeric(columnas$n), rep(0, nrow(columnas)), info = prefijo)
  for (nombre in setdiff(names(columnas), c("columna", "n"))) {
    expect_true(all(is.na(columnas[[nombre]])), info = paste(prefijo, nombre))
  }
  expect_false(is.null(perfil$perfil_muestra), info = prefijo)
  expect_equal(perfil$perfil_muestra$meta$filas_analizadas, 0, info = prefijo)
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
}
