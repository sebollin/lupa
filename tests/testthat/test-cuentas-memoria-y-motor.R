# Una corrida con una parte medida en memoria y otra en el motor sobre la misma
# entidad. La lectura en memoria se identifica por el nombre de la entidad y la
# del motor por la tabla que resolvio: no hay una identidad que comparar, y se
# comparan por el nombre, con su numero de filas, como cuando las lecturas no
# traian la tabla. Si la tabla cambio entre las dos partes, las cifras no
# cierran; si no, cierran y el control de lecturas es `desconocido`. Lo mismo
# con una parte cuyas lecturas no traen la tabla, como las de un resumen medido
# antes de que se registrara.

.mym_fecha <- as.POSIXct("2026-06-30", tz = "UTC")
.mym_cortes <- list(Completitud = c(0.5, 0.9), Unicidad = c(0.5, 0.9))

.mym_ventas <- function() data.frame(id = 1:6, monto = c(1, NA, 3, 4, 5, 6))

.mym_con <- function(motor) {
  con <- if (identical(motor, "duckdb")) {
    DBI::dbConnect(duckdb::duckdb())
  } else {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  }
  DBI::dbWriteTable(con, "ventas", .mym_ventas())
  con
}

.mym_cerrar <- function(con, motor) {
  if (identical(motor, "duckdb")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
}

.mym_motores <- function() {
  c("sqlite", if (requireNamespace("duckdb", quietly = TRUE)) "duckdb")
}

.mym_memoria <- function() {
  medir(
    modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "Monto"),
                      "ventas", "monto")),
    list(ventas = .mym_ventas()), id_medicion = "j", fecha = .mym_fecha,
    detalle = "resumen"
  )
}

.mym_motor <- function(con, nombre = "Unico") {
  suppressWarnings(medir_dbi(
    modelo(instanciar(especializar(metricas_nucleo()$AtributoDuplicado, nombre),
                      "ventas", "id")),
    con, id_medicion = "j", fecha = .mym_fecha
  ))
}

# Las lecturas de un resumen medido antes de que registraran su tabla.
.mym_sin_tabla <- function(parte) {
  l <- attr(parte, "lecturas", exact = TRUE)
  l$tabla <- NULL
  attr(parte, "lecturas") <- l
  parte
}

.mym_mensajes <- function(x) {
  mensajes <- character()
  withCallingHandlers(
    utils::capture.output(print(x)),
    message = function(m) {
      mensajes <<- c(mensajes, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  paste(mensajes, collapse = " ")
}

test_that("si la tabla cambia entre la parte en memoria y la del motor, las cifras no cierran", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  for (motor in .mym_motores()) {
    con <- .mym_con(motor)
    memoria <- .mym_memoria()
    DBI::dbExecute(con, "INSERT INTO ventas VALUES (7, 9)")
    del_motor <- .mym_motor(con)
    .mym_cerrar(con, motor)
    expect_warning(u <- rbind(memoria, del_motor), "no cierran")
    expect_identical(u$n_filas_leidas, c(6, 7), label = motor)
    expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"), label = motor)
    expect_identical(u$motivo_cuentas, rep("tabla_cambio_entre_lecturas", 2L),
                     label = motor)
    expect_identical(u$diferencia_cuentas, c(1, 1), label = motor)
    # Ninguna se pinta.
    s <- semaforo_calidad(u, cortes = .mym_cortes)
    expect_true(all(is.na(s$nivel)), label = motor)
    # En el otro orden, lo mismo.
    al_reves <- suppressWarnings(rbind(del_motor, memoria))
    expect_identical(al_reves$estado_cuentas, c("no_cierran", "no_cierran"),
                     label = motor)
  }
})

test_that("con las mismas filas en memoria y en el motor cierran, y el control es desconocido", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  for (motor in .mym_motores()) {
    con <- .mym_con(motor)
    memoria <- .mym_memoria()
    del_motor <- .mym_motor(con)
    .mym_cerrar(con, motor)
    u <- rbind(memoria, del_motor)
    expect_identical(u$estado_cuentas, c("cierran", "cierran"), label = motor)
    s <- semaforo_calidad(u, cortes = .mym_cortes)
    expect_identical(s$control_lecturas, c("desconocido", "desconocido"),
                     label = motor)
    expect_false(anyNA(s$nivel), label = motor)
    # La impresion nombra esta causa.
    expect_true(grepl("una parte en memoria y otra en el motor", .mym_mensajes(s),
                      fixed = TRUE, useBytes = TRUE), label = motor)
  }
})

test_that("una parte cuyas lecturas no traen la tabla se compara por el nombre", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  for (motor in .mym_motores()) {
    con <- .mym_con(motor)
    guardada <- .mym_sin_tabla(.mym_motor(con, "Antes"))
    igual <- .mym_motor(con, "Igual")
    DBI::dbExecute(con, "INSERT INTO ventas VALUES (7, 9)")
    nueva <- .mym_motor(con, "Despues")
    .mym_cerrar(con, motor)
    u <- suppressWarnings(rbind(guardada, nueva))
    expect_identical(u$n_filas_leidas, c(6, 7), label = motor)
    expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"), label = motor)
    expect_identical(u$motivo_cuentas, rep("tabla_cambio_entre_lecturas", 2L),
                     label = motor)
    # Sin cambio cierran, y no se sabe si es la misma tabla.
    u <- rbind(guardada, igual)
    expect_identical(u$estado_cuentas, c("cierran", "cierran"), label = motor)
    s <- semaforo_calidad(u, cortes = .mym_cortes)
    expect_identical(s$control_lecturas, c("desconocido", "desconocido"),
                     label = motor)
  }
})

test_that("el control: dos partes del motor sobre la tabla se comparan por la tabla", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  for (motor in .mym_motores()) {
    con <- .mym_con(motor)
    antes <- .mym_motor(con, "Antes")
    igual <- .mym_motor(con, "Igual")
    DBI::dbExecute(con, "INSERT INTO ventas VALUES (7, 9)")
    despues <- .mym_motor(con, "Despues")
    .mym_cerrar(con, motor)
    u <- rbind(antes, igual)
    expect_identical(u$estado_cuentas, c("cierran", "cierran"), label = motor)
    s <- semaforo_calidad(u, cortes = .mym_cortes)
    expect_identical(s$control_lecturas, c("limitado", "limitado"), label = motor)
    u <- suppressWarnings(rbind(antes, despues))
    expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"), label = motor)
  }
})

test_that("el control: dos partes en memoria sobre la tabla se comparan por la huella", {
  skip_on_cran()
  una <- .mym_memoria()
  otra <- medir(
    modelo(instanciar(especializar(metricas_nucleo()$AtributoDuplicado, "Unico"),
                      "ventas", "id")),
    list(ventas = .mym_ventas()), id_medicion = "j", fecha = .mym_fecha,
    detalle = "resumen"
  )
  u <- rbind(una, otra)
  expect_identical(u$estado_cuentas, c("cierran", "cierran"))
  s <- semaforo_calidad(u, cortes = .mym_cortes)
  expect_identical(s$control_lecturas, c("completo", "completo"))
})

test_that("una parte sin lecturas entra en la comparacion con el conteo de su fila", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- .mym_con("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  memoria <- .mym_memoria()
  del_motor <- .mym_motor(con, "Antes")
  DBI::dbExecute(con, "INSERT INTO ventas VALUES (7, 9)")
  sin_lecturas <- .mym_motor(con, "Despues")
  attr(sin_lecturas, "lecturas") <- NULL
  # Las lecturas que quedan coinciden: lo unico que dice que la tabla cambio
  # son las siete filas que leyo la parte sin lecturas.
  u <- suppressWarnings(rbind(memoria, del_motor, sin_lecturas))
  expect_identical(u$n_filas_leidas, c(6, 6, 7))
  expect_identical(u$estado_cuentas, rep("no_cierran", 3L))
  expect_identical(u$diferencia_cuentas, rep(1, 3L))
  # Lo mismo entre dos partes del motor, donde el rotulo da una sola tabla.
  u <- suppressWarnings(rbind(del_motor, sin_lecturas))
  expect_identical(u$n_filas_leidas, c(6, 7))
  expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"))
})

test_that("con prioridad, una parte cuyas lecturas no traen la tabla se rechaza con su motivo", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- .mym_con("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  guardada <- .mym_sin_tabla(.mym_motor(con))
  nueva <- .mym_motor(con)
  expect_error(
    rbind(guardada = guardada, nueva = nueva, prioridad = c("guardada", "nueva")),
    "no hay c\u00f3mo probar", class = "lupa_consolidacion_no_admitida"
  )
})
