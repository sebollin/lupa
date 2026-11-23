# Herramientas de las pruebas de la conexion perdida en la via DBI.
#
# `.cp_imitar_dbi()` pone una envoltura sobre el punto unico: cuenta cada
# llamada del paquete al controlador, deja ver la sentencia, y puede cerrar la
# conexion o levantar una condicion fabricada justo antes de una de ellas. Lo
# que mide: cuantas llamadas LLEGAN al controlador despues del corte. Como la
# llamada viaja sin evaluar, solo se cuenta la que el punto unico evalua.

.cp_estado <- function() {
  est <- new.env(parent = emptyenv())
  est$k <- 0L
  est$ops <- character()
  est$sql <- character()
  est$cortada <- FALSE
  est$fallo_tras_corte <- FALSE
  est$tras <- 0L
  est$lanzar <- NULL
  est$llegaron <- 0L
  est
}

.cp_sql_de_la_llamada <- function(marco) {
  for (nombre in c("sql", "orden")) {
    valor <- get0(nombre, envir = marco, inherits = TRUE)
    if (is.character(valor) && length(valor) == 1L) return(valor)
  }
  NA_character_
}

# El manejador se arma aparte, sin la conexion en su entorno: los controladores
# dejan retenido el manejador del ultimo error suyo que se atrapo.
.cp_al_fallar <- function(est) {
  function(e) {
    if (est$cortada) est$fallo_tras_corte <- TRUE
    stop(e)
  }
}

.cp_imitar_dbi <- function(est, gancho = NULL, .env = parent.frame()) {
  original <- .dbi
  testthat::local_mocked_bindings(
    .dbi = function(operacion, conexion, llamada) {
      est$k <- est$k + 1L
      est$ops <- c(est$ops, operacion)
      sql <- .cp_sql_de_la_llamada(parent.frame())
      est$sql <- c(est$sql, sql)
      if (!is.null(gancho)) gancho(est, operacion, conexion, sql)
      lanzar <- est$lanzar
      est$lanzar <- NULL
      original(operacion, conexion, {
        est$llegaron <- est$llegaron + 1L
        if (est$fallo_tras_corte) est$tras <- est$tras + 1L
        if (!is.null(lanzar)) {
          est$fallo_tras_corte <- TRUE
          stop(lanzar)
        }
        tryCatch(llamada, error = .cp_al_fallar(est))
      })
    },
    .env = .env
  )
  invisible(est)
}

# Recolecta lo que quedo suelto. El controlador avisa, desde su finalizador,
# que la conexion recolectada no se habia cerrado; ese aviso no pasa por los
# manejadores de quien llama -sale mas tarde, suelto, al final de la corrida-
# y `suppressWarnings()` no lo alcanza. Se recolecta con los avisos apagados.
.cp_recolectar <- function() {
  anterior <- options(warn = -1)
  on.exit(options(anterior), add = TRUE)
  for (i in 1:3) gc(full = TRUE)
  invisible(NULL)
}

# Cierra la conexion justo antes de la llamada `objetivo` al controlador.
.cp_gancho_cierre <- function(objetivo) {
  function(est, operacion, conexion, sql) {
    if (!est$cortada && est$k == objetivo) {
      est$cortada <- TRUE
      suppressWarnings(try(DBI::dbDisconnect(conexion), silent = TRUE))
    }
  }
}

# Corre `codigo(con)` con la envoltura puesta y devuelve el valor o el error, y
# lo que se conto.
.cp_correr <- function(abrir, codigo, gancho = NULL) {
  con <- abrir()
  on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)),
          add = TRUE)
  est <- .cp_estado()
  .cp_imitar_dbi(est, gancho)
  valor <- tryCatch(
    suppressWarnings(suppressMessages(codigo(con))),
    error = function(e) e
  )
  list(
    valor = valor, k = est$k, tras = est$tras, ops = est$ops, sql = est$sql,
    llegaron = est$llegaron, pila_vacia = is.null(.CORRIDAS_DBI$actual)
  )
}

.cp_tabla <- function(n = 24L) {
  data.frame(
    id = seq_len(n), x = ifelse(seq_len(n) %% 5L == 0L, NA, seq_len(n) / 4),
    s = paste0("v", seq_len(n) %% 4L), stringsAsFactors = FALSE
  )
}

.cp_abrir_sqlite <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "lupa_t", .cp_tabla())
  DBI::dbWriteTable(con, "lupa_u", data.frame(
    id = seq_len(12L) * 2L, nota = paste0("n", seq_len(12L)),
    stringsAsFactors = FALSE
  ))
  con
}

.cp_abrir_duckdb <- function(...) {
  con <- suppressMessages(DBI::dbConnect(.duckdb_prueba(), ...))
  DBI::dbWriteTable(con, "lupa_t", .cp_tabla())
  DBI::dbWriteTable(con, "lupa_u", data.frame(
    id = seq_len(12L) * 2L, nota = paste0("n", seq_len(12L)),
    stringsAsFactors = FALSE
  ))
  con
}

.CP_FECHA <- as.POSIXct("2026-03-04 12:00:00", tz = "UTC")

.cp_modelo <- function() {
  nucleo <- metricas_nucleo()
  modelo(
    instanciar(especializar(nucleo$NoNulo, "NN_x"), "lupa_t", "x"),
    instanciar(especializar(nucleo$AtributoDuplicado, "D_id"), "lupa_t", "id")
  )
}

# Las letras con tilde, armadas: los archivos de prueba van en ASCII.
.CP_O <- intToUtf8(243L)
.CP_I <- intToUtf8(237L)
.CP_E <- intToUtf8(233L)
.CP_A <- intToUtf8(225L)
