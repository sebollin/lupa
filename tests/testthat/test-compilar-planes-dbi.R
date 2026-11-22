testthat::skip_on_cran()
skip_if_not_installed("DBI")

.cpd_crear_sqlite <- function(n = 20L) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE cpd_datos (id INTEGER, x REAL, t TEXT)")
  if (n > 0L) {
    DBI::dbExecute(con, paste0(
      "INSERT INTO cpd_datos VALUES ", paste0(
        "(", seq_len(n), ", ", seq_len(n), ", 'v", seq_len(n) %% 5L, "')",
        collapse = ", "
      )
    ))
  }
  con
}

.cpd_modelo <- function() {
  modelo(instanciar(especializar(
    metricas_nucleo()$NoNulo, "NN"
  ), "cpd_datos", "x"))
}

test_that("el plan de perfilado compila sin traer filas y conserva los fallos", {
  skip_if_not_installed("RSQLite")
  con <- .cpd_crear_sqlite()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  plan <- plan_perfilado_dbi(
    con, "cpd_datos", bloque_muestra = "solo_agregados", compilar = TRUE
  )
  compilacion <- attr(plan, "compilacion", exact = TRUE)
  expect_true(is.data.frame(compilacion))
  expect_equal(nrow(compilacion), sum(plan$n_consultas_max))
  expect_true(any(compilacion$estado == "compilada"))
  # SQLite no tiene `STDDEV_SAMP`: esa forma candidata del desvio se rechaza,
  # la corrida usa la otra, y el plan sano no tiene ninguna consulta que no
  # compile.
  expect_false(any(compilacion$estado == "no_compilada"))
  rechazadas <- compilacion$estado == "candidata_rechazada"
  expect_true(any(grepl("STDDEV_SAMP", compilacion$sql[rechazadas], fixed = TRUE)))
  expect_true(all(nzchar(compilacion$motivo[rechazadas])))
  expect_true(all(compilacion$metodo[
    compilacion$estado == "compilada"
  ] %in% c("EXPLAIN QUERY PLAN", "dbSendQuery")))
})

test_that("una tabla grande no cambia la via de compilacion", {
  skip_if_not_installed("RSQLite")
  con <- .cpd_crear_sqlite(50000L)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  registro <- new.env(parent = emptyenv())
  registro$enviados <- character()
  suppressMessages(trace(
    "dbSendQuery", where = asNamespace("DBI"), print = FALSE,
    tracer = bquote(if (exists("statement", inherits = FALSE)) {
      assign(
        "enviados",
        c(get("enviados", envir = .(registro)), paste(statement, collapse = " ")),
        envir = .(registro)
      )
    })
  ))
  on.exit(suppressMessages(untrace("dbSendQuery", where = asNamespace("DBI"))),
          add = TRUE)
  tiempo <- system.time(invisible(plan_perfilado_dbi(
    con, "cpd_datos", bloque_muestra = "solo_agregados", compilar = TRUE
  )))[["elapsed"]]
  expect_true(is.finite(tiempo))
  expect_lt(tiempo, 10)
  sobre_tabla <- registro$enviados[
    grepl("cpd_datos", registro$enviados, fixed = TRUE)
  ]
  expect_true(length(sobre_tabla) > 0L)
  expect_true(all(
    grepl("EXPLAIN", sobre_tabla, fixed = TRUE) |
      grepl("WHERE 1 = 0", sobre_tabla, fixed = TRUE) |
      grepl("LIMIT 0", sobre_tabla, fixed = TRUE) |
      grepl("pragma_", sobre_tabla, fixed = TRUE) |
      grepl("sqlite_master", sobre_tabla, fixed = TRUE)
  ))
})

test_that("el plan de medicion compila sus consultas sin cambiar la corrida", {
  skip_if_not_installed("RSQLite")
  con <- .cpd_crear_sqlite()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  mod <- .cpd_modelo()
  plan <- plan_medicion_dbi(mod, con, compilar = TRUE)
  compilacion <- attr(plan, "compilacion", exact = TRUE)
  expect_equal(
    nrow(compilacion), sum(unname(attr(plan, "consultas", exact = TRUE)))
  )
  expect_true(all(compilacion$estado == "compilada"))
  expect_true(all(compilacion$metodo %in% c("EXPLAIN QUERY PLAN", "dbSendQuery")))

  resultado_compilado <- medir_dbi(mod, con, instrumentar = TRUE)
  con2 <- .cpd_crear_sqlite()
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  plan_sin <- plan_medicion_dbi(mod, con2)
  expect_false("compilacion" %in% names(attributes(plan_sin)))
  resultado <- medir_dbi(mod, con2, instrumentar = TRUE)
  expect_equal(
    attr(resultado, "ejecucion_dbi", exact = TRUE)$consultas,
    sum(unname(attr(plan_sin, "consultas", exact = TRUE)))
  )
  expect_identical(
    attr(resultado_compilado, "ejecucion_dbi", exact = TRUE)$consultas,
    attr(resultado, "ejecucion_dbi", exact = TRUE)$consultas
  )
  expect_identical(
    attr(resultado_compilado, "ejecucion_dbi", exact = TRUE)$sql,
    attr(resultado, "ejecucion_dbi", exact = TRUE)$sql
  )
})

test_that("el plan de medicion DuckDB usa EXPLAIN", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "cpd_datos", data.frame(x = 1:3))
  plan <- plan_medicion_dbi(.cpd_modelo(), con, compilar = TRUE)
  compilacion <- attr(plan, "compilacion", exact = TRUE)
  expect_true(nrow(compilacion) > 0L)
  expect_true(all(compilacion$estado == "compilada"))
  expect_true(all(compilacion$metodo == "EXPLAIN"))
})
