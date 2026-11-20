# En PostgreSQL con `extra_float_digits <= 0` (y antes de la 12) un double se
# lee como NUMERIC exacto por sus ocho bytes. Esa lectura daba `-99` como
# `-99.0000000000000000179...`: la cuenta de faltantes disfrazados comparaba
# contra `-99` y publicaba 0 con dos `-99` en la tabla. Ademas,
# `SUM((condicion)::integer)` daba NULL cuando la condicion era NULL en todas
# las filas (texto sin ningun numero) y la cuenta del texto quedaba sin
# publicar. La tabla lleva sus disfrazados fuera de la muestra de 5 filas.

testthat::skip_on_cran()

.fdpe_conectar <- function(puerto, efd = NULL) {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = puerto, user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  testthat::skip_if(is.null(con), paste0("PostgreSQL no disponible en ", puerto, "."))
  if (!is.null(efd)) {
    DBI::dbExecute(con, paste0("SET extra_float_digits = ", efd))
  }
  con
}

.fdpe_datos <- function() {
  n <- 30L
  data.frame(
    id = seq_len(n),
    frac = c(seq(1.5, by = 1.1, length.out = n - 3L), rep(-9.9, 3L)),
    txt = c(rep("ok", 25L), rep("N/D", 5L)),
    mezcla = c(as.character(1:20), rep("sin dato", 6L), rep("-99", 4L)),
    nulo = NA_real_,
    entero = c(seq_len(n - 2L), rep(-99, 2L))
  )
}

.fdpe_comprobar <- function(con) {
  tabla <- "fdpe_bordes"
  datos <- .fdpe_datos()
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  DBI::dbWriteTable(con, tabla, datos)
  on.exit(try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
              silent = TRUE), add = TRUE)
  sentinelas <- c(-99, -9.9)
  p <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, muestra = 5L, orden_muestra = "id",
    sentinelas_numericos = sentinelas
  )))
  m <- suppressWarnings(suppressMessages(perfilar(
    datos, sentinelas_numericos = sentinelas, fecha = "2026-01-15"
  )))
  sql <- p$resumen_tabla$columnas
  memoria <- m$columnas
  for (columna in c("frac", "txt", "mezcla", "nulo", "entero")) {
    testthat::expect_identical(
      as.numeric(sql$n_faltantes_disfrazados[sql$columna == columna]),
      as.numeric(memoria$n_faltantes_disfrazados[memoria$columna == columna]),
      info = columna
    )
  }
  # Control: la muestra de 5 filas no ve ningun disfrazado.
  muestra <- p$perfil_muestra$columnas
  testthat::expect_true(all(muestra$n_faltantes_disfrazados[
    muestra$columna %in% c("frac", "txt", "entero")
  ] == 0))
}

testthat::test_that("con extra_float_digits = 0 se cuentan los disfrazados de la tabla entera", {
  con <- .fdpe_conectar(as.integer(Sys.getenv("LUPA_PG_PORT", "55432")), efd = 0)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .fdpe_comprobar(con)
})

testthat::test_that("con la lectura de doble de siempre se cuentan igual", {
  con <- .fdpe_conectar(as.integer(Sys.getenv("LUPA_PG_PORT", "55432")))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .fdpe_comprobar(con)
})

testthat::test_that("la lectura exacta de un double vuelve exacta en SQL", {
  con <- .fdpe_conectar(as.integer(Sys.getenv("LUPA_PG_PORT", "55432")))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  valores <- c(-99, 17, 0.1, -0.5, 2^53 + 2, 1e-300, 5e-324, 1.7976931348623157e308)
  tabla <- "fdpe_exacta"
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  DBI::dbWriteTable(con, tabla, data.frame(id = seq_along(valores), v = valores))
  on.exit(try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
              silent = TRUE), add = TRUE)
  expresion <- lupa:::.lectura_double_postgresql_dbi('"v"')
  r <- DBI::dbGetQuery(con, paste0(
    "SELECT id, (", expresion, ") AS exacta, ((", expresion,
    ") = CAST(-99 AS NUMERIC)) AS es99, ((", expresion,
    ") = CAST(17 AS NUMERIC)) AS es17 FROM ", tabla, " ORDER BY id"
  ))
  testthat::expect_identical(as.numeric(r$exacta), valores)
  testthat::expect_identical(r$es99, c(TRUE, rep(FALSE, length(valores) - 1L)))
  testthat::expect_identical(r$es17, c(FALSE, TRUE, rep(FALSE, length(valores) - 2L)))
})

testthat::test_that("en PostgreSQL 9.3 se cuentan los disfrazados de la tabla entera", {
  con <- .fdpe_conectar(55493L)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .fdpe_comprobar(con)
})
