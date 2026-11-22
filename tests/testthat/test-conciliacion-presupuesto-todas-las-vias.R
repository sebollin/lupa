testthat::skip_on_cran()

.conciliacion_todas_las_vias_conectar <- function(motor) {
  if (identical(motor, "sqlite")) {
    return(DBI::dbConnect(RSQLite::SQLite(), ":memory:"))
  }
  argumentos <- list()
  if ("shared_home" %in% names(formals(duckdb::duckdb))) {
    argumentos$shared_home <- FALSE
  }
  DBI::dbConnect(do.call(duckdb::duckdb, argumentos), dbdir = ":memory:")
}

.conciliacion_todas_las_vias_desconectar <- function(conexion, motor) {
  if (!DBI::dbIsValid(conexion)) return(invisible(NULL))
  try(DBI::dbDisconnect(
    conexion, shutdown = identical(motor, "duckdb")
  ), silent = TRUE)
}

.conciliacion_todas_las_vias_datos <- function(conexion) {
  set.seed(9)
  DBI::dbWriteTable(conexion, "t", data.frame(
    id = 1:60,
    x = round(rnorm(60), 2),
    g = sample(letters[1:4], 60, TRUE),
    stringsAsFactors = FALSE
  ))
}

.conciliacion_todas_las_vias_perfil <- function(conexion, argumentos) {
  suppressWarnings(suppressMessages(do.call(
    perfilar_dbi, c(list(conexion, "t"), argumentos)
  )))
}

testthat::test_that(
  "el presupuesto agotado queda desfasado en las vias SQL spool y bloques", {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("RSQLite")
    testthat::skip_if_not_installed("duckdb")

    vias <- list(
      sql = list(),
      spool = list(universo = "muestra_motor", muestra_motor = 30L),
      bloques = list(bloque_filas = 20L)
    )
    for (motor in c("sqlite", "duckdb")) {
      conexion <- .conciliacion_todas_las_vias_conectar(motor)
      .conciliacion_todas_las_vias_datos(conexion)
      for (via in names(vias)) {
        perfil <- .conciliacion_todas_las_vias_perfil(
          conexion, c(vias[[via]], list(max_consultas = 1L))
        )
        consultas <- perfil$resumen_tabla$meta$consultas
        conciliacion <- consultas$conciliacion

        testthat::expect_true(isTRUE(consultas$agotado), info = via)
        testthat::expect_identical(conciliacion$estado, "desfasado")
        testthat::expect_equal(
          conciliacion$diferencia,
          conciliacion$emitidas - conciliacion$anunciadas,
          info = paste(motor, via)
        )
        testthat::expect_equal(
          sum(conciliacion$detalle$diferencia), conciliacion$diferencia,
          info = paste(motor, via)
        )
      }
      .conciliacion_todas_las_vias_desconectar(conexion, motor)
    }
  }
)
testthat::test_that(
  "el barrido de topes de bloques nunca concilia un presupuesto agotado", {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("RSQLite")
    testthat::skip_if_not_installed("duckdb")

    for (motor in c("sqlite", "duckdb")) {
      conexion <- .conciliacion_todas_las_vias_conectar(motor)
      .conciliacion_todas_las_vias_datos(conexion)
      completo <- .conciliacion_todas_las_vias_perfil(
        conexion, list(bloque_filas = 20L)
      )
      emitidas_sin_tope <- completo$resumen_tabla$meta$consultas$emitidas

      for (maximo in seq_len(emitidas_sin_tope + 4L)) {
        perfil <- .conciliacion_todas_las_vias_perfil(
          conexion, list(bloque_filas = 20L, max_consultas = maximo)
        )
        consultas <- perfil$resumen_tabla$meta$consultas
        conciliacion <- consultas$conciliacion
        agotado <- isTRUE(consultas$agotado)

        if (agotado) {
          testthat::expect_identical(
            conciliacion$estado, "desfasado",
            info = paste(motor, "max_consultas", maximo)
          )
        }
        testthat::expect_equal(
          conciliacion$diferencia,
          conciliacion$emitidas - conciliacion$anunciadas,
          info = paste(motor, "max_consultas", maximo)
        )
        testthat::expect_equal(
          sum(conciliacion$detalle$diferencia), conciliacion$diferencia,
          info = paste(motor, "max_consultas", maximo)
        )
      }
      .conciliacion_todas_las_vias_desconectar(conexion, motor)
    }
  }
)
testthat::test_that(
  "la conciliacion de tablas sin valores usa el plan de corrida en ambos motores",
  {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("RSQLite")
    testthat::skip_if_not_installed("duckdb")

    for (motor in c("sqlite", "duckdb")) {
      conexion <- .conciliacion_todas_las_vias_conectar(motor)
      DBI::dbWriteTable(conexion, "vacia", data.frame(
        id = integer(), x = double(), g = character(),
        stringsAsFactors = FALSE
      ))
      DBI::dbWriteTable(conexion, "nulos", data.frame(
        a = rep(NA_real_, 10), b = rep(NA_character_, 10),
        stringsAsFactors = FALSE
      ))

      for (tabla in c("vacia", "nulos")) {
        perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
          conexion, tabla, fecha = as.POSIXct(
            "2026-11-19 12:00:00", tz = "UTC"
          )
        )))
        meta <- perfil$resumen_tabla$meta
        consultas <- meta$consultas
        conciliacion <- consultas$conciliacion
        corrida <- conciliacion$plan_corrida
        previo <- conciliacion$plan_previo
        esperado <- sum(as.numeric(meta$plan$n_consultas))

        testthat::expect_equal(corrida$anunciadas, esperado)
        testthat::expect_equal(corrida$emitidas, consultas$emitidas)
        testthat::expect_equal(
          corrida$diferencia, consultas$emitidas - esperado
        )
        testthat::expect_equal(
          sum(corrida$detalle$diferencia), corrida$diferencia
        )
        testthat::expect_true(is.list(previo))
        if (identical(tabla, "vacia")) {
          testthat::expect_match(previo$motivo, "vacia")
        } else {
          testthat::expect_match(previo$motivo, "valores no nulos")
        }
      }
      .conciliacion_todas_las_vias_desconectar(conexion, motor)
    }
  }
)
