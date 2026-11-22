# Pruebas locales de la compilacion en MySQL y MariaDB: las sentencias que el
# usuario tiene preparadas no se tocan, y una interrupcion entre `PREPARE` y
# `DEALLOCATE` no deja nada preparado ni se publica como un rechazo.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.clim_conectar <- function(maria) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  clave <- Sys.getenv(if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD", unset = "")
  skip_if(!nzchar(clave), paste0("Sin clave para ", if (maria) "MariaDB" else "MySQL", "."))
  con <- tryCatch({
    if (maria) {
      DBI::dbConnect(
        RMariaDB::MariaDB(), host = Sys.getenv("LUPA_MARIA_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_MARIA_PORT", "33306")),
        dbname = Sys.getenv("LUPA_MARIA_NAME", "lupa"),
        username = Sys.getenv("LUPA_MARIA_USER", "root"), password = clave
      )
    } else {
      DBI::dbConnect(
        RMySQL::MySQL(), host = Sys.getenv("LUPA_MYSQL_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_MYSQL_PORT", "33307")),
        dbname = Sys.getenv("LUPA_MYSQL_NAME", "lupa"),
        username = Sys.getenv("LUPA_MYSQL_USER", "root"), password = clave
      )
    }
  }, error = function(e) NULL)
  skip_if(is.null(con), paste0(if (maria) "MariaDB" else "MySQL", " no disponible."))
  con
}

# La condicion que levanta R al vencer el tope de `setTimeLimit()`: un error
# comun, sin clase propia, con el mensaje traducido. Se levanta en el paso
# elegido, sin depender de cuanto tarde cada sentencia: con un tope de tiempo
# de verdad, la maquina cargada movia la interrupcion de paso.
.clim_interrumpir <- function() {
  stop(simpleError(gettext("reached elapsed time limit", domain = "R")))
}

.clim_preparadas <- function(con) {
  as.numeric(DBI::dbGetQuery(con, "SHOW SESSION STATUS LIKE 'Prepared_stmt_count'")[[2L]])
}

.clim_probar <- function(maria) {
  con <- .clim_conectar(maria)
  motor <- if (maria) "mariadb" else "mysql"
  tabla <- paste0("lupa_compilar_preparadas_", motor)
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (id INTEGER, x DOUBLE, t VARCHAR(8))"))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES (1, 0.5, 'a'), (2, 1.5, NULL), (3, 2.5, 'b')"
  ))
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "compilar_NN"), tabla, "x"))

  # Las sentencias preparadas del usuario, tambien con el nombre que la
  # compilacion usaba antes, siguen preparadas y responden lo suyo.
  # Se ejecutan con `dbExecute()` y se leen por variables de sesion: RMariaDB
  # no devuelve filas de un `EXECUTE` por `dbGetQuery()`.
  DBI::dbExecute(con, "PREPARE lupa_compilar_sentencia FROM 'SET @lupa_compilar_w = 41 + 1'")
  DBI::dbExecute(con, "PREPARE mi_sentencia FROM 'SET @lupa_compilar_v = 7'")
  on.exit(try(DBI::dbExecute(con, "DEALLOCATE PREPARE lupa_compilar_sentencia"), silent = TRUE),
          add = TRUE, after = FALSE)
  on.exit(try(DBI::dbExecute(con, "DEALLOCATE PREPARE mi_sentencia"), silent = TRUE),
          add = TRUE, after = FALSE)
  antes <- .clim_preparadas(con)
  plan <- plan_perfilado_dbi(con, tabla, compilar = TRUE)
  medicion <- plan_medicion_dbi(mod, con, compilar = TRUE)
  expect_true(any(attr(plan, "compilacion")$metodo == "PREPARE", na.rm = TRUE))
  DBI::dbExecute(con, "SET @lupa_compilar_w = NULL, @lupa_compilar_v = NULL")
  DBI::dbExecute(con, "EXECUTE lupa_compilar_sentencia")
  DBI::dbExecute(con, "EXECUTE mi_sentencia")
  variables <- DBI::dbGetQuery(con, "SELECT @lupa_compilar_w AS w, @lupa_compilar_v AS v")
  expect_equal(as.numeric(variables$w), 42)
  expect_equal(as.numeric(variables$v), 7)
  expect_identical(.clim_preparadas(con), antes)

  # Una interrupcion al preparar o al liberar, antes o despues de que el motor
  # la ejecute, dentro de una transaccion del usuario.
  original <- .orden_transaccion_dbi
  for (patron in c("^PREPARE lupa_compilar", "^DEALLOCATE PREPARE lupa_compilar")) {
    for (cuando in c("antes", "despues")) {
      for (llamada in c("perfilado", "medicion")) {
        info <- paste(motor, llamada, patron, cuando)
        disparada <- FALSE
        envoltura <- function(conexion, orden, traer = FALSE) {
          dispara <- is.environment(.CONTEXTO_MOTOR$compilador) && !disparada &&
            grepl(patron, orden, perl = TRUE)
          if (dispara && identical(cuando, "antes")) {
            disparada <<- TRUE
            .clim_interrumpir()
          }
          valor <- original(conexion, orden, traer)
          if (dispara && identical(cuando, "despues")) {
            disparada <<- TRUE
            .clim_interrumpir()
          }
          valor
        }
        DBI::dbBegin(con)
        DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES (1000, 1, 'tx')"))
        valor <- tryCatch(
          with_mocked_bindings(
            if (identical(llamada, "perfilado")) {
              plan_perfilado_dbi(con, tabla, compilar = TRUE)
            } else plan_medicion_dbi(mod, con, compilar = TRUE),
            .orden_transaccion_dbi = envoltura
          ),
          error = function(e) e
        )
        expect_true(disparada, info = info)
        expect_true(inherits(valor, "error"), info = info)
        if (inherits(valor, "error")) {
          expect_identical(
            conditionMessage(valor), gettext("reached elapsed time limit", domain = "R"),
            info = info
          )
        }
        expect_identical(.clim_preparadas(con), antes, info = info)
        expect_equal(as.numeric(DBI::dbGetQuery(
          con, paste0("SELECT COUNT(*) AS n FROM ", tabla, " WHERE id = 1000")
        )$n), 1, info = info)
        DBI::dbCommit(con)
        expect_equal(as.numeric(DBI::dbGetQuery(
          con, paste0("SELECT COUNT(*) AS n FROM ", tabla, " WHERE id = 1000")
        )$n), 1, info = info)
        DBI::dbExecute(con, paste0("DELETE FROM ", tabla, " WHERE id = 1000"))
      }
    }
  }
}

test_that("MySQL: compilar no toca las sentencias preparadas del usuario y una interrupcion no deja ninguna", {
  .clim_probar(FALSE)
})

test_that("MariaDB: compilar no toca las sentencias preparadas del usuario y una interrupcion no deja ninguna", {
  .clim_probar(TRUE)
})
