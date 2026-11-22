# Los bordes del referencial del motor y del detector de tablas de codigos: la
# cobertura en la direccion de la metrica, un cotejamiento declarado que no es
# binario, clases que el motor no escribe igual, `valor` de varias columnas, una
# clave toda nula y una tabla de codigos con dos etiquetas.

testthat::skip_on_cran()
skip_if_not_installed("RSQLite")

medir_ref_bordes <- function(con, ent, atributos, ref, clave, valor = character(),
                             metrica = "CorrectitudSemFuerte") {
  m <- metricas_referencial()
  r <- referencial(DBI::Id(table = ref), clave, valor, completo = TRUE, alcance = "p")
  suppressWarnings(medir_dbi(modelo(instanciar(
    especializar(m[[metrica]], "X"), ent, atributos, referencial = r
  )), con))
}

test_that("RatioCobertura cuenta las claves del referencial que aparecen, no las filas que casan", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "ref", data.frame(k = c("a", "b")))
  DBI::dbWriteTable(con, "ent", data.frame(k = c("a", "a", "z")))
  r <- medir_ref_bordes(con, "ent", "k", "ref", "k", metrica = "RatioCobertura")
  # Una de las dos claves aparece: 0,5, no las 2 de 3 filas.
  expect_identical(r$resultado, 0.5)
  expect_identical(c(r$numerador, r$denominador), c(1, 2))
})

test_that("un cotejamiento declarado que no es binario no se compara en el motor", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE ref (k TEXT COLLATE NOCASE)")
  DBI::dbExecute(con, "INSERT INTO ref VALUES ('a'), ('b')")
  DBI::dbWriteTable(con, "ent", data.frame(k = c("A", "a")))
  r <- medir_ref_bordes(con, "ent", "k", "ref", "k")
  expect_identical(nrow(r), 0L)
  expect_match(attr(r, "cobertura_metricas")$motivo, "no compara por bytes", fixed = TRUE)
})

test_that("un doble contra un texto queda declarado, con el motivo de la clase", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "ref", data.frame(k = c("1.5", "2")))
  DBI::dbWriteTable(con, "ent", data.frame(k = c(1.5, 2)))
  r <- medir_ref_bordes(con, "ent", "k", "ref", "k")
  expect_identical(nrow(r), 0L)
  expect_match(attr(r, "cobertura_metricas")$motivo, "texto can\u00f3nico", fixed = TRUE)
})

test_that("CorrectitudSemDebil compara todos los pares de un valor de varias columnas", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "ref", data.frame(k = 1:3, v1 = c("x", "y", "z"), v2 = c("p", "q", "r")))
  ent <- data.frame(k = c(1L, 2L, 3L, 1L), v1 = c("x", "y", "z", "x"), v2 = c("p", "Q", "s", NA))
  DBI::dbWriteTable(con, "ent", ent)
  r <- medir_ref_bordes(con, "ent", c("k", "v1", "v2"), "ref", "k", c("v1", "v2"),
                        metrica = "CorrectitudSemDebil")
  en_r <- suppressWarnings(medir(modelo(instanciar(
    especializar(metricas_referencial()$CorrectitudSemDebil, "X"), "ent",
    c("k", "v1", "v2"),
    referencial = referencial(DBI::dbReadTable(con, "ref"), "k", c("v1", "v2"),
                              completo = TRUE, alcance = "p")
  )), list(ent = ent), detalle = "resumen"))
  # (1, x, p) y (2, y, Q) -por el pliegue de mayusculas- casan; (3, z, s) no; la
  # fila con un NA no se evalua.
  expect_identical(c(r$numerador, r$denominador), c(2, 3))
  expect_identical(c(r$numerador, r$denominador, r$n_universo),
                   c(en_r$numerador, en_r$denominador, en_r$n_universo))
})

test_that("una clave toda nula se rechaza con la frase de referencial()", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "ref", data.frame(k = c(NA_character_, NA)))
  DBI::dbWriteTable(con, "ent", data.frame(k = "a"))
  r <- medir_ref_bordes(con, "ent", "k", "ref", "k")
  expect_match(attr(r, "cobertura_metricas")$motivo,
               "no puede contener valores ausentes", fixed = TRUE)
})

test_that("una tabla de codigos con dos etiquetas da una fila por cada una que casa", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(con, "CREATE TABLE cod (c INTEGER PRIMARY KEY, nombre TEXT, nota TEXT)")
  DBI::dbExecute(con, "INSERT INTO cod VALUES (1, 'Alta', 'x'), (9, 'Ignorado', 'Sin dato')")
  DBI::dbExecute(con, "CREATE TABLE hechos (id INTEGER, c INTEGER REFERENCES cod(c))")
  DBI::dbExecute(con, "INSERT INTO hechos VALUES (1, 1), (2, 9), (3, 9), (4, 1)")
  col <- coleccion(con, c("cod", "hechos"))
  tc <- detectar_tablas_codigos(col, suppressWarnings(perfilar_coleccion(col)))
  pr <- as.data.frame(tc$propuesta)
  expect_setequal(pr$columna_etiqueta[pr$codigo == "9"], c("nombre", "nota"))
  expect_identical(unique(pr$n_filas[pr$codigo == "9"]), 2)
  pr$incluir <- TRUE
  # Las dos filas son el mismo codigo en la misma columna: una sola entrada.
  expect_identical(ausencias_desde_propuesta(pr, tabla = "hechos"),
                   list(sentinelas_numericos = list(c = 9)))
})
