# Un nombre de tabla que la guarda no deja citar no llega a ninguna sentencia:
# la lectura se declara fallida. Antes la cita quedaba en `NA` y el motor
# recibia `SELECT ... FROM NA`, y con una tabla llamada `NA` en la base las
# relaciones se calculaban sobre esa otra tabla.
test_that("relaciones_coleccion no lee otra tabla cuando no puede citar el nombre", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE lupa_rel_a (id INTEGER, x DOUBLE)")
  DBI::dbExecute(con, "INSERT INTO lupa_rel_a VALUES (1, 1.5), (2, 2.5), (3, 3.5)")
  DBI::dbExecute(con, "CREATE TABLE \"NA\" (id INTEGER, x DOUBLE)")
  DBI::dbExecute(con, "INSERT INTO \"NA\" VALUES (7, 70), (8, 80)")
  # Bytes que no son UTF-8 valido y no declaran su codificacion.
  bytes <- rawToChar(as.raw(c(0x6f, 0x31, 0x36, 0x33, 0x5f, 0x61, 0xf1, 0x6f)))
  enviadas <- character()
  local_mocked_bindings(
    .traer_consulta_dbi = function(conexion, sql, ...) {
      enviadas <<- c(enviadas, sql)
      DBI::dbGetQuery(conexion, sql)
    },
    .enviar_consulta_dbi = function(conexion, sql, ...) {
      enviadas <<- c(enviadas, sql)
      DBI::dbSendQuery(conexion, sql)
    }
  )
  r <- suppressWarnings(suppressMessages(relaciones_coleccion(
    coleccion(con, c(bytes, "lupa_rel_a"), nombre = "lupa_bytes"),
    pares = data.frame(tabla_1 = bytes, tabla_2 = "lupa_rel_a",
                       stringsAsFactors = FALSE),
    columnas_candidatas = stats::setNames(list("id"), bytes)
  )))
  expect_false(any(grepl("FROM NA\\b", enviadas)))
  lecturas <- attr(r, "lecturas", exact = TRUE)
  if (is.null(lecturas)) lecturas <- r$meta$lecturas
  fila <- lecturas[lecturas$tabla != "lupa_rel_a", , drop = FALSE]
  expect_identical(nrow(fila), 1L)
  expect_false(isTRUE(fila$ok))
})
