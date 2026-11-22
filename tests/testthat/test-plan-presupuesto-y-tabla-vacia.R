suppressPackageStartupMessages(library(lupa))
testthat::skip_on_cran()

.pms7_cenitela <- function(conexion) {
  DBI::dbWriteTable(conexion, "pms7_cenitela", data.frame(
    id = 1:30,
    y = c(rep(-9, 5), seq_len(25) / 10),
    z = seq_len(30) / 7,
    stringsAsFactors = FALSE
  ))
}

.pms7_perfil <- function(conexion, tabla, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, tabla, fecha = as.POSIXct(
      "2026-11-19 12:00:00", tz = "UTC"
    ), proteger_datos_personales = FALSE, ...
  )))
}

testthat::test_that(
  "un recorte por presupuesto nunca queda conciliado", {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("RSQLite")
    for (motor in c("sqlite", "duckdb")) {
      if (identical(motor, "duckdb")) {
        testthat::skip_if_not_installed("duckdb")
        conexion <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
      } else {
        conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      }
      .pms7_cenitela(conexion)
      limite <- if (identical(motor, "sqlite")) 21L else 20L
      perfil <- .pms7_perfil(
        conexion, "pms7_cenitela", muestra = 10L,
        max_consultas = limite
      )
      conciliacion <- perfil$resumen_tabla$meta$consultas$conciliacion

      testthat::expect_true(
        isTRUE(perfil$resumen_tabla$meta$consultas$agotado)
      )
      testthat::expect_identical(conciliacion$estado, "desfasado")
      testthat::expect_equal(
        conciliacion$diferencia,
        conciliacion$emitidas - conciliacion$anunciadas
      )
      testthat::expect_equal(
        sum(conciliacion$detalle$diferencia), conciliacion$diferencia
      )
      testthat::expect_true(any(grepl(
        "presupuesto", conciliacion$detalle$motivo,
        fixed = TRUE
      )))
      testthat::expect_true(any(
        conciliacion$detalle$clase_consulta ==
          "conteos de faltantes disfrazados"
      ))
      try(DBI::dbDisconnect(
        conexion, shutdown = identical(motor, "duckdb")
      ), silent = TRUE)
    }
  }
)

testthat::test_that(
  "DuckDB vacio cuenta la mediana emitida y conserva la diferencia", {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("duckdb")
    conexion <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(try(DBI::dbDisconnect(conexion, shutdown = TRUE), silent = TRUE),
            add = TRUE)
    DBI::dbWriteTable(conexion, "pms7_vacio", data.frame(
      id = integer(), x = double(), g = character(),
      stringsAsFactors = FALSE
    ))

    perfil <- .pms7_perfil(conexion, "pms7_vacio")
    meta <- perfil$resumen_tabla$meta
    consultas <- meta$consultas
    plan <- meta$plan
    corrida <- consultas$conciliacion$plan_corrida

    testthat::expect_equal(sum(plan$n_consultas), consultas$emitidas)
    testthat::expect_equal(corrida$anunciadas, sum(plan$n_consultas))
    testthat::expect_equal(
      corrida$diferencia, corrida$emitidas - corrida$anunciadas
    )
    testthat::expect_equal(
      sum(corrida$detalle$diferencia), corrida$diferencia
    )
  }
)

testthat::test_that(
  "los planes de conciliacion exponen sus atributos total y etapa historica", {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("RSQLite")
    conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(conexion), add = TRUE)
    DBI::dbWriteTable(conexion, "pms7_vacio", data.frame(
      id = integer(), x = double(), g = character(),
      stringsAsFactors = FALSE
    ))

    perfil <- .pms7_perfil(conexion, "pms7_vacio")
    conciliacion <- perfil$resumen_tabla$meta$consultas$conciliacion

    testthat::expect_false("guardas_faltantes" %in% lupa:::.ETAPAS_INTERNAS_DBI)
    testthat::expect_false(is.null(attr(
      conciliacion$plan_previo, "total", exact = TRUE
    )))
    testthat::expect_false(is.null(attr(
      conciliacion$plan_corrida, "total", exact = TRUE
    )))
  }
)
