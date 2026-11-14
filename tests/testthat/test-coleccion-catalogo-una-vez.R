# Con `columnas_candidatas`, `relaciones_coleccion()` validaba las candidatas
# contra las columnas de cada tabla y despues las volvia a pedir para leerla:
# dos consultas de catalogo por tabla (en SQL Server, `sys.columns` y
# `sys.masked_columns`), cuatro de mas por par, con la misma respuesta. La
# validacion y el lector comparten ahora lo consultado, sin estado global: otra
# llamada vuelve a preguntar. SQL Server se finge sobre RSQLite. ASCII.
testthat::skip_on_cran()

.emular_sqlserver_cuv <- function(catalogo, env = parent.frame()) {
  ns <- asNamespace("lupa")
  traer <- get(".traer_consulta_dbi", envir = ns)
  enviar <- get(".enviar_consulta_dbi", envir = ns)
  registro <- new.env()
  registro$sql <- character()
  nombra <- function(sql, tabla) {
    grepl(paste0("[`\"]", tabla, "[`\"]"), sql, perl = TRUE)
  }
  motor <- function(conexion, sql, original) {
    registro$sql <- c(registro$sql, sql)
    if (grepl("sys.masked_columns", sql, fixed = TRUE, useBytes = TRUE)) {
      return(data.frame(columna = character(), mascara = character(),
                        sin_mascara_columna = integer(),
                        sin_mascara_base = integer()))
    }
    if (grepl("sys.columns", sql, fixed = TRUE, useBytes = TRUE)) {
      for (t in names(catalogo)) if (nombra(sql, t)) return(catalogo[[t]])
      return(data.frame(posicion = integer(), columna = character(),
                        legible = integer()))
    }
    original(conexion, sql)
  }
  do.call(testthat::local_mocked_bindings, c(list(
    .traer_consulta_dbi = function(conexion, sql) motor(conexion, sql, traer),
    .enviar_consulta_dbi = function(conexion, sql) motor(conexion, sql, enviar),
    .senas_conexion_dbi = function(conexion) "microsoft sql server odbc"
  ), list(.package = "lupa", .env = env)))
  registro
}

.consultas_catalogo_cuv <- function(registro, tabla) {
  sql <- registro$sql[grepl("sys.columns", registro$sql, fixed = TRUE, useBytes = TRUE)]
  sum(grepl(paste0("[`\"]", tabla, "[`\"]"), sql, perl = TRUE))
}

test_that("con columnas_candidatas el catalogo de cada tabla se consulta una vez", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "cliente", data.frame(id = 1:40, nombre = paste0("n", 1:40)))
  DBI::dbWriteTable(con, "contacto", data.frame(
    nro = 901:960, cliente_id = rep(1:40, length.out = 60)
  ))
  registro <- .emular_sqlserver_cuv(list(
    cliente = data.frame(posicion = 1:2, columna = c("id", "nombre"), legible = 1L),
    contacto = data.frame(posicion = 1:2, columna = c("nro", "cliente_id"), legible = 1L)
  ))
  col <- suppressMessages(coleccion(con, c("cliente", "contacto")))
  relaciones <- function() suppressWarnings(suppressMessages(relaciones_coleccion(
    col, pares = data.frame(tabla_1 = "contacto", tabla_2 = "cliente"),
    columnas_candidatas = list(cliente = "id", contacto = "cliente_id")
  )))
  registro$sql <- character()
  r <- relaciones()
  expect_equal(.consultas_catalogo_cuv(registro, "cliente"), 1L)
  expect_equal(.consultas_catalogo_cuv(registro, "contacto"), 1L)
  # Lo que se relaciona no cambia: los 40 valores de `cliente_id` estan en `id`.
  rel <- r$relaciones
  expect_equal(nrow(rel), 1L)
  expect_identical(rel$columna_tabla1, "cliente_id")
  expect_identical(rel$columna_tabla2, "id")
  expect_equal(rel$cobertura_tabla1_en_tabla2, 1)
  # Sin estado global: la llamada siguiente vuelve a preguntar, una vez.
  relaciones()
  expect_equal(.consultas_catalogo_cuv(registro, "cliente"), 2L)
  expect_equal(.consultas_catalogo_cuv(registro, "contacto"), 2L)
})
