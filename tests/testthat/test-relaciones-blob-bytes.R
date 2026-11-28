# Dos columnas BLOB se comparan por sus bytes: un BLOB solo aparea con otro
# BLOB de los mismos bytes, nunca con un texto o un numero, y los conteos de la
# relacion son los del motor.
testthat::skip_on_cran()

.rbb_blob <- function(...) lapply(list(...), as.raw)

.rbb_fila <- function(r, c1, c2) {
  r[r$columna_tabla1 == c1 & r$columna_tabla2 == c2, , drop = FALSE]
}

test_that("en memoria, BLOB sin bytes comunes no es una relacion", {
  a <- data.frame(id = 1:3)
  a$k <- .rbb_blob(1, 2, 3)
  b <- data.frame(n = 1:3)
  b$fk <- .rbb_blob(7, 8, 9)
  r <- suppressWarnings(detectar_relaciones(a, b))
  f <- .rbb_fila(r, "k", "fk")
  expect_identical(f$cardinalidad, "sin_coincidencias")
  expect_identical(f$n_valores_comunes, 0L)
  expect_equal(f$cobertura_tabla2_en_tabla1, 0)

  b$fk <- .rbb_blob(1, 2, 9)
  r <- suppressWarnings(detectar_relaciones(a, b))
  f <- .rbb_fila(r, "k", "fk")
  expect_identical(f$n_valores_comunes, 2L)
  expect_equal(f$cobertura_tabla2_en_tabla1, 2 / 3)
})

test_that("los BLOB de varios bytes, como un identificador de 16, se comparan por sus bytes", {
  uuid <- function(i) as.raw(c(rep(0L, 15L), i))
  a <- data.frame(id = 1:3)
  a$k <- lapply(1:3, uuid)
  b <- data.frame(n = 1:3)
  b$fk <- lapply(c(2L, 3L, 9L), uuid)
  f <- .rbb_fila(suppressWarnings(detectar_relaciones(a, b)), "k", "fk")
  expect_identical(f$n_valores_comunes, 2L)
  expect_equal(f$cobertura_tabla2_en_tabla1, 2 / 3)
  b$fk <- lapply(7:9, uuid)
  f <- .rbb_fila(suppressWarnings(detectar_relaciones(a, b)), "k", "fk")
  expect_identical(f$cardinalidad, "sin_coincidencias")
})

test_that("un BLOB no aparea con texto ni numero que se escriban como sus bytes", {
  a <- data.frame(id = 1:3)
  a$k <- .rbb_blob(1, 2, 3)
  b <- data.frame(t = c("b:01", "b:02", "b:03"), u = c("01", "02", "03"),
                  v = c(1, 2, 3), stringsAsFactors = FALSE)
  r <- suppressWarnings(detectar_relaciones(a, b))
  for (columna in c("t", "u", "v")) {
    f <- .rbb_fila(r, "k", columna)
    expect_identical(f$cardinalidad, "sin_coincidencias")
    expect_identical(f$n_valores_comunes, 0L)
  }
  # El mismo par con el BLOB en la otra tabla.
  r2 <- suppressWarnings(detectar_relaciones(b, a))
  expect_identical(.rbb_fila(r2, "t", "k")$n_valores_comunes, 0L)
})

test_that("el BLOB vacio es un valor y NULL es ausencia", {
  a <- data.frame(id = 1:4)
  a$k <- list(raw(0), as.raw(1), NULL, as.raw(2))
  b <- data.frame(n = 1:3)
  b$fk <- list(raw(0), NULL, as.raw(1))
  f <- .rbb_fila(suppressWarnings(detectar_relaciones(a, b)), "k", "fk")
  expect_identical(f$n_valores_comunes, 2L)
  # Dos valores no ausentes en `fk`, los dos en `k`; de los tres de `k`, dos en `fk`.
  expect_equal(f$cobertura_tabla2_en_tabla1, 1)
  expect_equal(f$cobertura_tabla1_en_tabla2, 2 / 3)
})

test_that("la traza de huerfanos de un BLOB contra un texto nombra sus filas", {
  a <- data.frame(id = 1:3)
  a$k <- .rbb_blob(1, 2, 3)
  b <- data.frame(t = c("x", "y", "z"), stringsAsFactors = FALSE)
  f <- .rbb_fila(suppressWarnings(detectar_relaciones(a, b)), "k", "t")
  expect_identical(f$n_filas_huerfanas_tabla1, 3L)
  expect_identical(f$n_filas_huerfanas_tabla2, 3L)
})

.rbb_con <- function(motor) {
  if (identical(motor, "duckdb")) {
    testthat::skip_if_not_installed("duckdb")
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    tipo <- "BLOB"
    lit <- function(h) paste0("'\\x", h, "'::BLOB")
  } else {
    testthat::skip_if_not_installed("RSQLite")
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    tipo <- "BLOB"
    lit <- function(h) paste0("X'", h, "'")
  }
  DBI::dbExecute(con, paste0("CREATE TABLE ta (k ", tipo, ")"))
  DBI::dbExecute(con, paste0("CREATE TABLE tb (fk ", tipo, ", t VARCHAR)"))
  for (h in c("01", "02", "03")) {
    DBI::dbExecute(con, paste0("INSERT INTO ta VALUES (", lit(h), ")"))
  }
  for (par in list(c("07", "b:01"), c("08", "b:02"), c("09", "b:03"))) {
    DBI::dbExecute(con, paste0(
      "INSERT INTO tb VALUES (", lit(par[[1L]]), ", '", par[[2L]], "')"
    ))
  }
  con
}

.rbb_coleccion <- function(con) {
  suppressWarnings(relaciones_coleccion(
    coleccion(con, c("ta", "tb")),
    data.frame(tabla_1 = "ta", tabla_2 = "tb"), umbral_cobertura = 0
  ))$relaciones
}

test_that("en la coleccion un BLOB solo aparea con los mismos bytes (SQLite y DuckDB)", {
  for (motor in c("sqlite", "duckdb")) {
    con <- .rbb_con(motor)
    motor_cuenta <- DBI::dbGetQuery(
      con, "SELECT COUNT(*) AS n FROM tb WHERE fk IN (SELECT k FROM ta)"
    )$n
    expect_equal(as.numeric(motor_cuenta), 0)
    r <- .rbb_coleccion(con)
    f <- .rbb_fila(r, "k", "fk")
    expect_identical(f$cardinalidad, "sin_coincidencias")
    expect_equal(f$cobertura_tabla2_en_tabla1, 0)
    # Ni con el texto que se escribe como su clave.
    f <- .rbb_fila(r, "k", "t")
    expect_identical(f$cardinalidad, "sin_coincidencias")
    expect_equal(f$cobertura_tabla2_en_tabla1, 0)
    DBI::dbDisconnect(con)
  }
})

test_that("en la coleccion dos BLOB con bytes comunes tienen la cobertura del motor", {
  con <- .rbb_con("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE tb SET fk = X'01' WHERE t = 'b:01'")
  DBI::dbExecute(con, "UPDATE tb SET fk = X'02' WHERE t = 'b:02'")
  f <- .rbb_fila(.rbb_coleccion(con), "k", "fk")
  motor <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM tb WHERE fk IN (SELECT k FROM ta)"
  )$n
  expect_equal(f$cobertura_tabla2_en_tabla1, motor / 3)
  expect_equal(f$cobertura_tabla2_en_tabla1, 2 / 3)
})

test_that("los BLOB que entrega el controlador (clase blob) tambien se comparan por sus bytes", {
  testthat::skip_if_not_installed("RSQLite")
  con <- .rbb_con("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ta <- DBI::dbGetQuery(con, "SELECT k FROM ta")
  tb <- DBI::dbGetQuery(con, "SELECT fk, t FROM tb")
  r <- suppressWarnings(detectar_relaciones(ta, tb))
  for (columna in c("fk", "t")) {
    f <- .rbb_fila(r, "k", columna)
    expect_identical(f$cardinalidad, "sin_coincidencias")
    expect_identical(f$n_valores_comunes, 0L)
  }
  DBI::dbExecute(con, "UPDATE tb SET fk = X'01' WHERE t = 'b:01'")
  tb <- DBI::dbGetQuery(con, "SELECT fk, t FROM tb")
  f <- .rbb_fila(suppressWarnings(detectar_relaciones(ta, tb)), "k", "fk")
  expect_identical(f$n_valores_comunes, 1L)
})
