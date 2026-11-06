# Con `incluir_valores = FALSE` la salida no publica valores de celda, y eso
# incluye los motivos. El mensaje de un error del motor es texto ajeno que cita
# la celda que lo provoco -la conversion fallida de una vista, el desborde de
# un decimal con el valor entero- y sin proteccion se publicaba como llegaba en
# `resumen_tabla$sql$motivo`, `meta$consultas$fallas`, la muestra y el spool.
# El control: con `incluir_valores = TRUE` y sin proteccion el mensaje sigue
# entero, que es el diagnostico.
testthat::skip_on_cran()
skip_if_not_installed("DBI")
skip_if_not_installed("duckdb")

.msv_textos <- function(x) {
  salida <- character()
  visitar <- function(y) {
    if (is.environment(y) || is.function(y)) return(invisible())
    if (is.list(y)) {
      for (e in y) visitar(e)
    } else if (is.character(y)) {
      salida <<- c(salida, y[!is.na(y)])
    } else if (is.factor(y)) {
      salida <<- c(salida, levels(y))
    }
    a <- attributes(y)
    for (n in setdiff(names(a), c("class", "row.names"))) visitar(a[[n]])
    invisible()
  }
  visitar(x)
  salida
}

.msv_correr <- function(expr) {
  avisos <- character()
  valor <- withCallingHandlers(
    suppressMessages(expr),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  c(.msv_textos(unclass(valor)), avisos)
}

.msv_conexion <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  DBI::dbExecute(con, "CREATE TABLE msv_base (id INTEGER, t VARCHAR)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO msv_base VALUES (1, '11'), (2, 'ZQXKSECRETO'), (3, '33')"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE VIEW msv_vista AS SELECT id, CAST(t AS BIGINT) AS x FROM msv_base"
  ))
  # `AVG(v * 1.0)` desborda y el motor cita el valor.
  DBI::dbExecute(con, "CREATE TABLE msv_decimal (v DECIMAL(38,30))")
  DBI::dbExecute(con, paste0(
    "INSERT INTO msv_decimal VALUES ",
    "(13579246.975310000000000000000000000001), ",
    "(13579246.975310000000000000000000000002), ",
    "(13579246.975310000000000000000000000003)"
  ))
  con
}

test_that("con incluir_valores = FALSE el mensaje del motor no cita la celda", {
  con <- .msv_conexion()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  vias <- list(
    list(),
    list(bloque_muestra = "solo_agregados"),
    list(universo = "muestra_motor", muestra_motor = 3L,
         bloque_muestra = "solo_agregados"),
    list(universo = "muestra_motor", muestra_motor = 3L)
  )
  for (objeto in c("msv_vista", "msv_decimal")) {
    marca <- if (objeto == "msv_vista") "ZQXKSECRETO" else "13579246"
    for (via in vias) {
      sin_valores <- .msv_correr(do.call(perfilar_dbi, c(
        list(con, objeto, incluir_valores = FALSE,
             proteger_datos_personales = FALSE), via
      )))
      expect_false(any(grepl(marca, sin_valores, fixed = TRUE, useBytes = TRUE)),
                   info = paste(objeto, paste(names(via), collapse = ",")))
    }
    # El control: con valores, el mensaje del motor llega entero.
    con_valores <- .msv_correr(perfilar_dbi(
      con, objeto, incluir_valores = TRUE, proteger_datos_personales = FALSE
    ))
    expect_true(any(grepl(marca, con_valores, fixed = TRUE, useBytes = TRUE)), info = objeto)
  }
})

test_that("la coleccion con incluir_valores = FALSE tampoco cita la celda", {
  con <- .msv_conexion()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  col <- coleccion(con, c("msv_vista", "msv_decimal"), nombre = "msv")
  for (conservar in c(FALSE, TRUE)) {
    textos <- .msv_correr(perfilar_coleccion(
      col, incluir_valores = FALSE, conservar_perfiles = conservar,
      proteger_datos_personales = FALSE
    ))
    expect_false(any(grepl("ZQXKSECRETO", textos, fixed = TRUE, useBytes = TRUE)))
    expect_false(any(grepl("13579246", textos, fixed = TRUE, useBytes = TRUE)))
  }
  con_valores <- .msv_correr(perfilar_coleccion(
    col, conservar_perfiles = TRUE, proteger_datos_personales = FALSE
  ))
  expect_true(any(grepl("ZQXKSECRETO", con_valores, fixed = TRUE, useBytes = TRUE)))
})
