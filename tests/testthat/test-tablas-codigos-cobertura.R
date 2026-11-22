# `candidata` exige que otra tabla use la tabla de codigos con la cobertura
# pedida; la forma sola, o una clave foranea con huerfanos, la deja `posible`.

testthat::skip_on_cran()

test_that("la cobertura de la columna que la usa decide si una tabla es candidata", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  x <- function(s) invisible(DBI::dbExecute(con, s))
  x("CREATE TABLE limpia (c INTEGER PRIMARY KEY, nombre TEXT)")
  x("INSERT INTO limpia VALUES (1, 'Alta'), (9, 'Sin dato')")
  x("CREATE TABLE usa_limpia (id INTEGER, c INTEGER REFERENCES limpia(c))")
  x("INSERT INTO usa_limpia VALUES (1, 1), (2, 9), (3, 9), (4, 1)")
  x("CREATE TABLE sucia (c INTEGER PRIMARY KEY, nombre TEXT)")
  x("INSERT INTO sucia VALUES (1, 'Alta'), (9, 'Sin dato')")
  x("CREATE TABLE usa_sucia (id INTEGER, c INTEGER REFERENCES sucia(c))")
  x("INSERT INTO usa_sucia VALUES (1, 1), (2, 5), (3, 6), (4, 9)")
  x("CREATE TABLE suelta (c INTEGER PRIMARY KEY, nombre TEXT)")
  x("INSERT INTO suelta VALUES (1, 'Alta'), (9, 'Sin dato')")
  col <- coleccion(con, c("limpia", "usa_limpia", "sucia", "usa_sucia", "suelta"))
  pc <- suppressWarnings(perfilar_coleccion(col))
  tc <- detectar_tablas_codigos(col, pc)
  estado <- function(tabla) tc$tablas$estado[tc$tablas$tabla == tabla & tc$tablas$columna_codigo %in% "c"]
  expect_identical(estado("limpia"), "candidata")
  # Dos de cuatro filas son huerfanas: cobertura 0,5.
  expect_identical(estado("sucia"), "posible")
  expect_identical(tc$tablas$cobertura_maxima[tc$tablas$tabla == "sucia"], 0.5)
  expect_match(tc$tablas$motivo[tc$tablas$tabla == "sucia"], "cobertura", fixed = TRUE)
  expect_identical(estado("suelta"), "posible")
  # La cobertura de una clave foranea declarada se mide, no se supone.
  expect_identical(
    tc$usos$cobertura[tc$usos$tabla_codigos == "sucia"], 0.5
  )
  # Sin propuesta para la que no es candidata.
  expect_false("sucia" %in% tc$propuesta$tabla_codigos)
  expect_true("limpia" %in% tc$propuesta$tabla_codigos)
  # Con un umbral que la admite, la sucia vuelve a ser candidata.
  laxa <- detectar_tablas_codigos(col, pc, umbral_cobertura = 0.5)
  expect_identical(laxa$tablas$estado[laxa$tablas$tabla == "sucia"], "candidata")
  expect_error(detectar_tablas_codigos(col, pc, umbral_cobertura = 2),
               class = "lupa_error_argumento_dbi")
})
