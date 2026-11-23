# Interrupciones en cada paso de la compilacion en PostgreSQL y SQL Server: la
# transaccion del usuario y la sesion quedan como estaban, y la interrupcion
# sale como tal. Se saltean si el motor no esta disponible en el entorno local.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.cint_conectar_pg <- function(puerto) {
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(puerto), user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"), bigint = "integer64"
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en puerto ", puerto, "."))
  con
}

.cint_conectar_mssql <- function() {
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin clave de SQL Server.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  con
}

# La condicion que levanta R al vencer el tope de `setTimeLimit()`: un error
# comun, sin clase propia, con el mensaje traducido. Se levanta en el paso
# elegido, sin depender de cuanto tarde cada sentencia: con un tope de tiempo
# de verdad, la maquina cargada movia la interrupcion de paso.
.cint_interrumpir <- function() {
  stop(simpleError(gettext("reached elapsed time limit", domain = "R")))
}

# Envolturas de los envios: la interrupcion llega en la primera sentencia de la
# compilacion -no de la preparacion del plan, que con la tabla ya analizada
# tambien manda un `EXPLAIN (FORMAT JSON)`- que casa con `patron`, antes de
# mandarla o despues de que el motor la ejecuto, y solo despues de ver la que
# casa con `armar`, si se pide.
.cint_envolturas <- function(patron, cuando, armar = NULL) {
  estado <- new.env(parent = emptyenv())
  estado$disparada <- FALSE
  estado$armada <- is.null(armar)
  envolver <- function(original) {
    # Se fuerza ya: evaluada dentro de la imitacion, la promesa daria la
    # imitacion misma.
    force(original)
    function(conexion, sql, ...) {
      compilando <- is.environment(.CONTEXTO_MOTOR$compilador)
      if (compilando && !estado$armada && grepl(armar, sql, perl = TRUE)) {
        estado$armada <- TRUE
      }
      dispara <- compilando && estado$armada && !estado$disparada &&
        grepl(patron, sql, perl = TRUE)
      if (dispara && identical(cuando, "antes")) {
        estado$disparada <- TRUE
        .cint_interrumpir()
      }
      valor <- original(conexion, sql, ...)
      if (dispara && identical(cuando, "despues")) {
        estado$disparada <- TRUE
        if (inherits(valor, "DBIResult")) .limpiar_consulta_dbi(valor, conexion)
        .cint_interrumpir()
      }
      valor
    }
  }
  list(
    estado = estado,
    .orden_transaccion_dbi = envolver(.orden_transaccion_dbi),
    .enviar_consulta_dbi = envolver(.enviar_consulta_dbi),
    .traer_consulta_dbi = envolver(.traer_consulta_dbi)
  )
}

.cint_correr <- function(codigo, envolturas) {
  valor <- tryCatch(
    with_mocked_bindings(
      codigo,
      .orden_transaccion_dbi = envolturas$.orden_transaccion_dbi,
      .enviar_consulta_dbi = envolturas$.enviar_consulta_dbi,
      .traer_consulta_dbi = envolturas$.traer_consulta_dbi
    ),
    error = function(e) e
  )
  valor
}

.cint_tope <- function() gettext("reached elapsed time limit", domain = "R")

# ---- PostgreSQL ------------------------------------------------------------

.cint_probar_pg <- function(puerto, sufijo) {
  adm <- .cint_conectar_pg(puerto)
  tabla <- paste0("lupa_compilar_interrupcion_", sufijo)
  on.exit({
    try(suppressMessages(DBI::dbExecute(adm, paste0("DROP TABLE IF EXISTS ", tabla))), silent = TRUE)
    try(DBI::dbDisconnect(adm), silent = TRUE)
  }, add = TRUE)
  suppressMessages(DBI::dbExecute(adm, paste0("DROP TABLE IF EXISTS ", tabla)))
  DBI::dbExecute(adm, paste0("CREATE TABLE ", tabla, " (id INTEGER, x DOUBLE PRECISION, t VARCHAR(8))"))
  DBI::dbExecute(adm, paste0(
    "INSERT INTO ", tabla, " SELECT g, g / 3.0, 'v' || (g % 4) FROM generate_series(1, 30) g"
  ))
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "compilar_NN"), tabla, "x"))
  ajuste <- function(con) DBI::dbGetQuery(
    con, "SELECT setting, source FROM pg_settings WHERE name = 'statement_timeout'"
  )
  casos <- list(
    list(tx = TRUE, patron = "^SAVEPOINT lupa_compilar"),
    list(tx = TRUE, patron = "^SET LOCAL statement_timeout"),
    list(tx = TRUE, patron = "^EXPLAIN "),
    list(tx = TRUE, patron = "^ROLLBACK TO SAVEPOINT lupa_compilar"),
    list(tx = TRUE, patron = "^RELEASE SAVEPOINT lupa_compilar"),
    list(tx = TRUE, patron = "^SAVEPOINT lupa_guarda", armar = "^SET LOCAL statement_timeout"),
    list(tx = TRUE, patron = "^SELECT STDEV\\("),
    list(tx = FALSE, patron = "^BEGIN$"),
    list(tx = FALSE, patron = "^SET LOCAL statement_timeout"),
    list(tx = FALSE, patron = "^EXPLAIN "),
    list(tx = FALSE, patron = "^ROLLBACK$")
  )
  for (caso in casos) {
    for (cuando in c("antes", "despues")) {
      # La candidata rechazada no tiene un "despues": falla al mandarse.
      if (grepl("STDEV", caso$patron, fixed = TRUE) && identical(cuando, "despues")) next
      for (plan in c("perfilado", "medicion")) {
        if (grepl("STDEV|lupa_guarda", caso$patron) && identical(plan, "medicion")) next
        info <- paste(sufijo, plan, caso$patron, cuando, if (caso$tx) "en transaccion" else "sin transaccion")
        con <- .cint_conectar_pg(puerto)
        if (isTRUE(caso$tx)) {
          DBI::dbBegin(con)
          DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES (1000, 1, 'tx')"))
        }
        antes <- ajuste(con)
        envolturas <- .cint_envolturas(caso$patron, cuando, caso$armar)
        valor <- .cint_correr(
          if (identical(plan, "perfilado")) {
            plan_perfilado_dbi(con, tabla, compilar = TRUE)
          } else plan_medicion_dbi(mod, con, compilar = TRUE),
          envolturas
        )
        expect_true(envolturas$estado$disparada, info = info)
        expect_true(inherits(valor, "error"), info = info)
        if (inherits(valor, "error")) {
          expect_identical(conditionMessage(valor), .cint_tope(), info = info)
        }
        expect_identical(ajuste(con), antes, info = info)
        if (isTRUE(caso$tx)) {
          expect_true(RPostgres::postgresIsTransacting(con), info = info)
          expect_equal(as.numeric(DBI::dbGetQuery(
            con, paste0("SELECT COUNT(*) AS n FROM ", tabla, " WHERE id = 1000")
          )$n), 1, info = info)
          DBI::dbCommit(con)
          expect_equal(as.numeric(DBI::dbGetQuery(
            adm, paste0("SELECT COUNT(*) AS n FROM ", tabla, " WHERE id = 1000")
          )$n), 1, info = info)
          DBI::dbExecute(adm, paste0("DELETE FROM ", tabla, " WHERE id = 1000"))
        } else {
          # Ninguna transaccion propia quedo abierta: `now()` cambia entre dos
          # sentencias.
          a1 <- DBI::dbGetQuery(con, "SELECT now() AS a")$a
          a2 <- DBI::dbGetQuery(con, "SELECT now() AS a")$a
          expect_false(identical(a1, a2), info = info)
        }
        DBI::dbDisconnect(con)
      }
    }
  }
}

test_that("PostgreSQL: una interrupcion en cualquier paso de la compilacion sale como tal y no toca la transaccion del usuario", {
  .cint_probar_pg(Sys.getenv("LUPA_PG_PORT", "55432"), "pg")
})

test_that("PostgreSQL 9.3: una interrupcion en cualquier paso de la compilacion sale como tal y no toca la transaccion del usuario", {
  .cint_probar_pg(Sys.getenv("LUPA_PG93_PORT", "55493"), "pg93")
})

test_that("PostgreSQL: con la transaccion de quien llama abortada, no se compila y se dice por que", {
  con <- .cint_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  tabla <- "lupa_compilar_abortada_pg"
  on.exit({
    try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    try(suppressMessages(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  suppressMessages(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)))
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (id INTEGER)"))
  # Un `BEGIN` a mano, que RPostgres no marca, y un error que la aborta.
  DBI::dbExecute(con, "BEGIN")
  try(DBI::dbGetQuery(con, "SELECT columna_que_no_existe FROM pg_class"), silent = TRUE)
  # Antes decia que no se pudo saber si habia una transaccion abierta, porque
  # la consulta que lo averigua fallaba. Lo primero que el motor contesta es
  # que la transaccion esta abortada, y eso es lo que se dice: como deshacerla,
  # y que la conexion sigue sirviendo.
  error <- tryCatch(
    plan_perfilado_dbi(con, tabla, compilar = TRUE), error = function(e) e
  )
  expect_s3_class(error, "lupa_error_transaccion_dbi")
  expect_true(isTRUE(error$datos$abortada))
  expect_match(conditionMessage(error), "est.{1,2} abortada")
  expect_match(conditionMessage(error), "`DBI::dbRollback()`", fixed = TRUE)
  DBI::dbExecute(con, "ROLLBACK")
  expect_s3_class(
    suppressWarnings(plan_perfilado_dbi(con, tabla, compilar = TRUE)),
    "plan_perfilado_dbi"
  )
})

# La compilacion arma los agregados sobre la misma fuente que la corrida: sobre
# la subconsulta que proyecta la lectura convertida -el doble leido exacto en
# 9.3, `real`- y con su identificador citado como nombre. Antes los armaba sobre
# la expresion de lectura y, en la ventana del desvio de la muestra del motor,
# la usaba de alias (`... AS CASE WHEN ...`): siete sentencias sanas que no
# compilaban y que la corrida nunca manda.
.cint_probar_muestra_motor <- function(puerto, sufijo) {
  con <- .cint_conectar_pg(puerto)
  tabla <- paste0("lupa_compilar_lectura_", sufijo)
  on.exit({
    try(suppressMessages(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  suppressMessages(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)))
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (id INTEGER, x DOUBLE PRECISION, r REAL)"))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " SELECT g, g / 3.0, g / 7.0 FROM generate_series(1, 300) g"
  ))
  for (opciones in list(
    list(universo = "muestra_motor", muestra_motor = 120L),
    list(universo = "muestra_motor", muestra_motor = 120L, bloque_muestra = "solo_agregados"),
    list()
  )) {
    info <- paste(sufijo, paste(names(opciones), collapse = ","))
    plan <- suppressWarnings(do.call(
      plan_perfilado_dbi, c(list(con, tabla, compilar = TRUE), opciones)
    ))
    compilacion <- attr(plan, "compilacion", exact = TRUE)
    expect_identical(sum(compilacion$estado == "no_compilada"), 0L, info = info)
    expect_false(any(grepl(" AS CASE WHEN", compilacion$sql, fixed = TRUE)), info = info)
    corrida <- suppressWarnings(do.call(perfilar_dbi, c(list(con, tabla), opciones)))
    expect_identical(nrow(corrida$resumen_tabla$meta$consultas$fallas), 0L, info = info)
  }
}

test_that("PostgreSQL 9.3: la lectura exacta de los dobles compila sin sentencias mal formadas", {
  .cint_probar_muestra_motor(Sys.getenv("LUPA_PG93_PORT", "55493"), "pg93")
})

test_that("PostgreSQL: una columna real compila sobre la misma fuente que la corrida", {
  .cint_probar_muestra_motor(Sys.getenv("LUPA_PG_PORT", "55432"), "pg")
})

# ---- SQL Server ------------------------------------------------------------

test_that("SQL Server: una interrupcion en cualquier paso de la compilacion sale como tal y deja la sesion como estaba", {
  adm <- .cint_conectar_mssql()
  tabla <- "lupa_compilar_interrupcion_mssql"
  borrar <- paste0("IF OBJECT_ID('dbo.", tabla, "', 'U') IS NOT NULL DROP TABLE ", tabla)
  on.exit({
    try(DBI::dbExecute(adm, borrar), silent = TRUE)
    try(DBI::dbDisconnect(adm), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(adm, borrar)
  DBI::dbExecute(adm, paste0("CREATE TABLE ", tabla, " (id INT, x FLOAT, t VARCHAR(8))"))
  DBI::dbExecute(adm, paste0(
    "INSERT INTO ", tabla, " VALUES (1, 0.5, 'a'), (2, 1.5, NULL), (3, 2.5, 'b')"
  ))
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "compilar_NN"), tabla, "x"))
  sesion <- function(con) DBI::dbGetQuery(con, paste(
    "SELECT @@TRANCOUNT AS trancount,",
    "CASE WHEN (@@OPTIONS & 16384) = 16384 THEN 1 ELSE 0 END AS xact_abort,",
    "CASE WHEN (@@OPTIONS & 2) = 2 THEN 1 ELSE 0 END AS implicitas"
  ))
  patrones <- c(
    "^SET XACT_ABORT OFF", "^SET NOEXEC ON", "^SELECT COUNT", "^SET NOEXEC OFF",
    "^SELECT 1 AS lupa_noexec", "^SET XACT_ABORT ON", "^SELECT STDDEV_SAMP\\("
  )
  for (patron in patrones) {
    for (cuando in c("antes", "despues")) {
      if (grepl("STDDEV", patron, fixed = TRUE) && identical(cuando, "despues")) next
      for (plan in c("perfilado", "medicion")) {
        if (grepl("STDDEV", patron, fixed = TRUE) && identical(plan, "medicion")) next
        info <- paste(plan, patron, cuando)
        con <- .cint_conectar_mssql()
        DBI::dbExecute(con, "SET XACT_ABORT ON")
        DBI::dbBegin(con)
        DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES (1000, 1, 'tx')"))
        antes <- sesion(con)
        envolturas <- .cint_envolturas(patron, cuando)
        valor <- suppressWarnings(.cint_correr(
          if (identical(plan, "perfilado")) {
            plan_perfilado_dbi(con, tabla, compilar = TRUE)
          } else plan_medicion_dbi(mod, con, compilar = TRUE),
          envolturas
        ))
        expect_true(envolturas$estado$disparada, info = info)
        expect_true(inherits(valor, "error"), info = info)
        if (inherits(valor, "error")) {
          expect_identical(conditionMessage(valor), .cint_tope(), info = info)
        }
        expect_identical(nrow(DBI::dbGetQuery(con, "SELECT 1 AS uno")), 1L, info = info)
        expect_identical(sesion(con), antes, info = info)
        DBI::dbCommit(con)
        expect_equal(as.numeric(DBI::dbGetQuery(
          adm, paste0("SELECT COUNT(*) AS n FROM ", tabla, " WHERE id = 1000")
        )$n), 1, info = info)
        DBI::dbExecute(adm, paste0("DELETE FROM ", tabla, " WHERE id = 1000"))
        DBI::dbDisconnect(con)
      }
    }
  }
})
