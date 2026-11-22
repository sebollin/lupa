# Compilar un plan nunca toca la transaccion del usuario, y una interrupcion no
# se publica como una sentencia que no compila. Sin servidor: SQLite y DuckDB.
# Los motores con servidor tienen sus propios archivos.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.ctr_tabla <- function(con) {
  DBI::dbExecute(con, "CREATE TABLE ctr_datos (id INTEGER, x DOUBLE, cod VARCHAR)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO ctr_datos VALUES ", paste0(
      "(", 1:30, ", ", (1:30) / 4, ", '", c("AB1", "CD2", "zz"), "')", collapse = ", "
    )
  ))
}

.ctr_contar <- function(con) {
  as.numeric(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM ctr_datos")$n)
}

.ctr_modelo <- function() {
  modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "NN"), "ctr_datos", "x"))
}

# Lo que se manda por la conexion mientras corre `codigo`.
.ctr_enviadas <- function(codigo) {
  enviadas <- new.env(parent = emptyenv())
  enviadas$sql <- character()
  anotar <- bquote(assign(
    "sql", c(get("sql", envir = .(enviadas)), paste(statement, collapse = " ")),
    envir = .(enviadas)
  ))
  genericas <- c("dbSendQuery", "dbGetQuery", "dbExecute", "dbSendStatement")
  for (generica in genericas) {
    suppressMessages(trace(generica, tracer = anotar, where = asNamespace("DBI"), print = FALSE))
  }
  on.exit(for (generica in genericas) {
    suppressMessages(untrace(generica, where = asNamespace("DBI")))
  }, add = TRUE)
  valor <- tryCatch(codigo, error = function(e) e)
  list(valor = valor, sql = enviadas$sql)
}

# La condicion que levanta R al vencer el tope de `setTimeLimit()`: un error
# comun, sin clase propia, con el mensaje traducido. Se levanta en el paso
# elegido, sin depender de cuanto tarde cada sentencia: con un tope de tiempo
# de verdad, la maquina cargada movia la interrupcion de paso.
.ctr_interrumpir <- function() {
  stop(simpleError(gettext("reached elapsed time limit", domain = "R")))
}

# Envuelve una funcion de envio para que la interrupcion llegue en la primera
# sentencia de la compilacion que casa con `patron`, antes o despues de
# mandarla.
.ctr_con_interrupcion <- function(original, patron, cuando) {
  disparada <- FALSE
  function(conexion, sql, ...) {
    dispara <- is.environment(.CONTEXTO_MOTOR$compilador) && !disparada &&
      grepl(patron, sql, perl = TRUE)
    if (dispara && identical(cuando, "antes")) {
      disparada <<- TRUE
      .ctr_interrumpir()
    }
    valor <- original(conexion, sql, ...)
    if (dispara && identical(cuando, "despues")) {
      disparada <<- TRUE
      if (inherits(valor, "DBIResult")) .limpiar_consulta_dbi(valor, conexion)
      .ctr_interrumpir()
    }
    valor
  }
}

test_that("DuckDB: con una transaccion abierta no se compila nada y la transaccion sigue", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  .ctr_tabla(con)
  for (abrir in c("dbBegin", "BEGIN TRANSACTION")) {
    if (identical(abrir, "dbBegin")) DBI::dbBegin(con) else DBI::dbExecute(con, abrir)
    DBI::dbExecute(con, "INSERT INTO ctr_datos VALUES (1000, 1, 'tx')")
    for (llamada in list(
      quote(plan_perfilado_dbi(con, "ctr_datos", compilar = TRUE)),
      quote(plan_medicion_dbi(.ctr_modelo(), con, compilar = TRUE))
    )) {
      r <- .ctr_enviadas(eval(llamada))
      expect_s3_class(r$valor, "lupa_error_transaccion_dbi")
      expect_match(conditionMessage(r$valor), "DuckDB", fixed = TRUE, info = abrir)
      # Antes de mandar nada: solo la lectura de `now()` que lo averigua.
      expect_true(all(grepl("^SELECT now\\(\\) AS lupa_ahora$", r$sql)), info = abrir)
    }
    # La transaccion sigue viva, con la fila del usuario, y el commit la guarda.
    expect_equal(.ctr_contar(con), 31, info = abrir)
    if (identical(abrir, "dbBegin")) DBI::dbCommit(con) else DBI::dbExecute(con, "COMMIT")
    expect_equal(.ctr_contar(con), 31, info = abrir)
    DBI::dbExecute(con, "DELETE FROM ctr_datos WHERE id = 1000")
  }
  # Fuera de una transaccion compila.
  plan <- plan_perfilado_dbi(con, "ctr_datos", compilar = TRUE)
  expect_true(any(attr(plan, "compilacion")$estado == "compilada"))
})

test_that("las formas candidatas del desvio se mandan como en la corrida, sin compilarlas", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  .ctr_tabla(con)
  r <- .ctr_enviadas(plan_perfilado_dbi(con, "ctr_datos", compilar = TRUE))
  compilacion <- attr(r$valor, "compilacion", exact = TRUE)
  candidatas <- compilacion[grepl("^SELECT (STDDEV_SAMP|STDEV)\\(", compilacion$sql), ]
  # DuckDB conoce `STDDEV_SAMP` y no `STDEV`.
  rechazada <- candidatas[candidatas$estado == "candidata_rechazada", ]
  expect_identical(nrow(rechazada), 1L)
  expect_match(rechazada$sql, "^SELECT STDEV\\(")
  expect_true(is.na(rechazada$metodo))
  # La rechazada se mando tal cual y nunca como `EXPLAIN`; la aceptada, despues,
  # se compila como cualquier sentencia.
  expect_true(any(grepl("^SELECT STDEV\\(", r$sql)))
  expect_false(any(grepl("^EXPLAIN SELECT STDEV\\(", r$sql)))
  expect_identical(
    candidatas$metodo[candidatas$estado == "compilada"], "EXPLAIN"
  )
})

test_that("SQLite: compilar dentro de una transaccion abierta la deja como estaba", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ctr_tabla(con)
  DBI::dbBegin(con)
  DBI::dbExecute(con, "INSERT INTO ctr_datos VALUES (1000, 1, 'tx')")
  plan <- plan_perfilado_dbi(con, "ctr_datos", compilar = TRUE)
  medicion <- plan_medicion_dbi(.ctr_modelo(), con, compilar = TRUE)
  compilado <- .with_compilador_dbi(con, "sqlite", {
    .compilar_sql_dbi(con, "SELECT columna_que_no_existe FROM ctr_datos", "x", "sqlite")
  })
  expect_identical(compilado$compilacion$estado, "no_compilada")
  expect_true(RSQLite::sqliteIsTransacting(con))
  expect_equal(.ctr_contar(con), 31)
  DBI::dbCommit(con)
  expect_equal(.ctr_contar(con), 31)
})

test_that("una interrupcion durante la compilacion sale como interrupcion y no como una sentencia que no compila", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  original_envio <- .enviar_consulta_dbi
  original_traer <- .traer_consulta_dbi
  tope <- gettext("reached elapsed time limit", domain = "R")
  for (motor in c("sqlite", "duckdb")) {
    for (caso in list(
      list(patron = "^EXPLAIN", donde = "envio", plan = "perfilado"),
      list(patron = "^EXPLAIN", donde = "envio", plan = "medicion"),
      list(patron = "^SELECT (STDDEV_SAMP|STDEV)\\(", donde = "traer", plan = "perfilado")
    )) {
      for (cuando in c("antes", "despues")) {
        con <- if (identical(motor, "sqlite")) {
          DBI::dbConnect(RSQLite::SQLite(), ":memory:")
        } else DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
        .ctr_tabla(con)
        envio <- if (identical(caso$donde, "envio")) {
          .ctr_con_interrupcion(original_envio, caso$patron, cuando)
        } else original_envio
        traer <- if (identical(caso$donde, "traer")) {
          .ctr_con_interrupcion(original_traer, caso$patron, cuando)
        } else original_traer
        info <- paste(motor, caso$plan, caso$patron, cuando)
        valor <- tryCatch(
          with_mocked_bindings(
            if (identical(caso$plan, "perfilado")) {
              plan_perfilado_dbi(con, "ctr_datos", compilar = TRUE)
            } else plan_medicion_dbi(.ctr_modelo(), con, compilar = TRUE),
            .enviar_consulta_dbi = envio, .traer_consulta_dbi = traer
          ),
          error = function(e) e
        )
        # La candidata rechazada no tiene un "despues": falla al mandarse.
        if (identical(caso$donde, "traer") && identical(cuando, "despues") &&
            !inherits(valor, "error")) {
          DBI::dbDisconnect(con)
          next
        }
        expect_true(inherits(valor, "error"), info = info)
        if (inherits(valor, "error")) {
          expect_identical(conditionMessage(valor), tope, info = info)
        } else {
          expect_false(any(grepl(tope, attr(valor, "compilacion")$motivo, fixed = TRUE)), info = info)
        }
        # La conexion sigue sirviendo.
        expect_equal(.ctr_contar(con), 30, info = info)
        if (identical(motor, "duckdb")) {
          DBI::dbDisconnect(con, shutdown = TRUE)
        } else DBI::dbDisconnect(con)
      }
    }
  }
})

test_that("el plan de medicion publica solo sentencias: ni la consulta de la sesion ni una fila por cada metrica por bloques", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ctr_tabla(con)
  nuc <- metricas_nucleo()
  mod <- modelo(
    instanciar(especializar(nuc$NoNulo, "NN"), "ctr_datos", "x"),
    instanciar(especializar(nuc$Formato, "Rx1", expresion_regular = "^[A-Z]{2}[0-9]$"), "ctr_datos", "cod"),
    instanciar(especializar(nuc$Formato, "Rx2", expresion_regular = "^[A-Z]"), "ctr_datos", "cod")
  )
  plan <- plan_medicion_dbi(mod, con, estrategia_no_sql = "bloques", compilar = TRUE)
  expect_identical(sum(plan$via == "bloques_r"), 2L)
  expect_identical(attr(plan, "consultas", exact = TRUE)[["bloques"]], 2L)
  compilacion <- attr(plan, "compilacion", exact = TRUE)
  # Cada fila es una sentencia; las dos metricas por bloques se calculan en R
  # sobre una sola lectura de la entidad.
  expect_false(anyNA(compilacion$sql))
  expect_false(any(compilacion$etapa == "plan"))
  expect_identical(sum(compilacion$etapa == "bloques"), 1L)
  expect_true(all(compilacion$estado == "compilada"))
})
