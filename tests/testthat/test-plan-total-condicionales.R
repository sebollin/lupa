testthat::skip_on_cran()

.pmt_conectar <- function(motor) {
  if (identical(motor, "sqlite")) {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  } else {
    drv_args <- list()
    if ("shared_home" %in% names(formals(duckdb::duckdb))) {
      drv_args$shared_home <- FALSE
    }
    drv <- do.call(duckdb::duckdb, drv_args)
    DBI::dbConnect(drv, dbdir = ":memory:")
  }
}

.pmt_centinela <- function(con) {
  DBI::dbWriteTable(con, "cen1", data.frame(
    id = 1:30,
    y = c(rep(-9, 5), seq_len(25) / 10),
    z = seq_len(30) / 7,
    stringsAsFactors = FALSE
  ))
}

.pmt_desconectar <- function(con, motor) {
  try(DBI::dbDisconnect(
    con, shutdown = identical(motor, "duckdb")
  ), silent = TRUE)
}

test_that("el total escalar acota una condicional desconocida", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  for (motor in c("sqlite", "duckdb")) {
    con <- .pmt_conectar(motor)
    .pmt_centinela(con)

    plan <- suppressWarnings(suppressMessages(plan_perfilado_dbi(
      con, "cen1", muestra = 10L
    )))
    fila <- plan[plan$clase_consulta ==
                   "conteos de faltantes disfrazados", , drop = FALSE]
    expect_equal(nrow(fila), 1L)
    expect_true(is.na(fila$n_consultas))
    expect_equal(fila$n_consultas_max, 3)
    expect_equal(attr(plan, "total_minimo"),
                 sum(plan$n_consultas, na.rm = TRUE))
    expect_equal(attr(plan, "total"), attr(plan, "total_minimo"))

    perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, "cen1", muestra = 10L,
      proteger_datos_personales = FALSE
    )))
    consultas <- perfil$resumen_tabla$meta$consultas
    conciliacion <- consultas$conciliacion$plan_corrida
    detalle <- conciliacion$detalle[
      conciliacion$detalle$clase_consulta == fila$clase_consulta,
      , drop = FALSE
    ]
    expect_identical(conciliacion$estado, "conciliado")
    expect_lte(as.numeric(consultas$emitidas),
               as.numeric(attr(plan, "total_maximo")))
    expect_equal(as.numeric(conciliacion$diferencia),
                 sum(as.numeric(conciliacion$detalle$diferencia)))
    expect_equal(nrow(detalle), 1L)
    expect_identical(detalle$direccion, "condicional")
    expect_match(detalle$motivo, "caso esperado.*peor caso")
    .pmt_desconectar(con, motor)
  }
})

test_that("la tabla vacia concilia contra el plan publicado de la corrida", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  for (motor in c("sqlite", "duckdb")) {
    con <- .pmt_conectar(motor)
    DBI::dbWriteTable(con, "vacia", data.frame(
      id = integer(), x = double(), g = character(),
      stringsAsFactors = FALSE
    ))

    perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, "vacia", proteger_datos_personales = FALSE
    )))
    plan_corrida <- perfil$resumen_tabla$meta$plan
    consultas <- perfil$resumen_tabla$meta$consultas
    conciliacion_historica <- consultas$conciliacion
    conciliacion <- conciliacion_historica$plan_corrida
    esperado <- sum(as.numeric(plan_corrida$n_consultas))
    expect_identical(
      conciliacion$estado,
      if (as.numeric(conciliacion$diferencia) == 0) {
        "conciliado"
      } else {
        "desfasado"
      }
    )
    expect_equal(as.numeric(conciliacion$anunciadas), esperado)
    expect_equal(as.numeric(conciliacion$emitidas),
                 as.numeric(consultas$emitidas))
    expect_equal(as.numeric(conciliacion$diferencia),
                 as.numeric(consultas$emitidas) - esperado)
    expect_equal(
      sum(as.numeric(conciliacion$detalle$diferencia)),
      as.numeric(conciliacion$diferencia)
    )
    expect_true(is.list(conciliacion_historica$plan_previo))
    expect_gt(as.numeric(conciliacion_historica$plan_previo$total), esperado)
    expect_match(conciliacion_historica$plan_previo$motivo, "vac.a\\.")
    .pmt_desconectar(con, motor)
  }
})

test_that("el print avisa cuando una consulta depende del dato", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .pmt_centinela(con)
  plan <- suppressWarnings(suppressMessages(plan_perfilado_dbi(
    con, "cen1", muestra = 10L
  )))
  mensajes <- character()
  salida <- withCallingHandlers(
    utils::capture.output(print(plan)),
    message = function(m) {
      mensajes <<- c(mensajes, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  salida <- c(salida, mensajes)
  expect_true(any(grepl("depende del dato", salida, fixed = TRUE)))
})
