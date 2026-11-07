# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.con_o28 <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}

test_that("la fila de coleccion declara el universo de sus metricas", {
  con <- .con_o28()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(
    id = seq_len(100), valor = c(rep(1, 99), NA_real_)
  ))
  col <- coleccion(con, "t", nombre = "o28")

  completa <- perfilar_coleccion(col, muestra = 100)
  fila_completa <- completa$resumen_coleccion
  expect_equal(fila_completa$n_filas, 100)
  expect_equal(fila_completa$prop_faltantes_maxima, 0.01)
  expect_equal(fila_completa$n_columnas_sin_faltantes, 1)
  expect_equal(fila_completa$universo, "tabla_completa")

  muestra <- perfilar_coleccion(
    col, muestra = 10, universo = "muestra_motor", muestra_motor = 10
  )
  fila_muestra <- muestra$resumen_coleccion
  expect_equal(fila_muestra$n_filas, 100)
  expect_equal(fila_muestra$universo, "muestra_motor")
  expect_true(
    is.na(fila_muestra$prop_faltantes_maxima) ||
      fila_muestra$prop_faltantes_maxima >= 0
  )
})

# Una conexion SQLite que hace al motor calcular mal la media (un uno por
# ciento de mas): una divergencia de verdad, del lado del motor. Los metodos
# van en el nivel del archivo y solo si RSQLite esta: sin el no hay
# `SQLiteConnection` de la que heredar, y la prueba se saltea en `.con_o28()`.
if (requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE) &&
    !methods::isClass("conexion_sqlite_media_falsa_o28")) {
  # Con DBI adjunto y el generico por su nombre: con `DBI::dbSendQuery` como
  # objeto, `callNextMethod()` no encuentra el metodo siguiente.
  suppressPackageStartupMessages(library(DBI))
  methods::setClass("conexion_sqlite_media_falsa_o28",
                    contains = "SQLiteConnection")
  # `dbGetQuery()` pasa por `dbSendQuery()`: la reescritura no se repite.
  .o28_falsear <- function(statement) {
    gsub("(?<!1\\.01 \\* )AVG\\(", "1.01 * AVG(", statement, perl = TRUE)
  }
  methods::setMethod(
    "dbSendQuery", c("conexion_sqlite_media_falsa_o28", "character"),
    function(conn, statement, ...) {
      methods::callNextMethod(conn, .o28_falsear(statement), ...)
    }
  )
  methods::setMethod(
    "dbGetQuery", c("conexion_sqlite_media_falsa_o28", "character"),
    function(conn, statement, ...) {
      methods::callNextMethod(conn, .o28_falsear(statement), ...)
    }
  )
}

.o28_conexion_media_falsa <- function(cruda) {
  con <- methods::new("conexion_sqlite_media_falsa_o28")
  for (ranura in methods::slotNames(cruda)) {
    methods::slot(con, ranura) <- methods::slot(cruda, ranura)
  }
  con
}

test_that("la coleccion conserva las divergencias del perfil interno", {
  cruda <- .con_o28()
  on.exit(DBI::dbDisconnect(cruda), add = TRUE)
  # Una divergencia de verdad: el motor da la media un uno por ciento de mas.
  # Antes la daba la media de {1e16, 1, -1e16}, pero la que se equivocaba era
  # la de R (0,556 por 1/3), y desde que la media de `perfilar()` es la mas
  # cercana a la exacta las dos coinciden. Una columna `COLLATE RTRIM` no
  # sirve: esa diferencia es la que `criterio_comparacion` declara esperable, y
  # va a `no_corroborada`, no a `divergencia`.
  DBI::dbExecute(cruda, "CREATE TABLE t (v REAL)")
  DBI::dbExecute(cruda, "INSERT INTO t VALUES (1), (2), (3.5)")
  con <- .o28_conexion_media_falsa(cruda)
  col <- coleccion(con, "t", nombre = "o28")

  interno <- perfilar_dbi(con, "t", muestra = 10)
  divergencias_internas <- interno$resumen_tabla$cobertura[
    interno$resumen_tabla$cobertura$estado == "divergencia", , drop = FALSE
  ]
  expect_gt(nrow(divergencias_internas), 0L)

  liviano <- perfilar_coleccion(col, muestra = 10)
  divergencias <- liviano$cobertura_metricas[
    liviano$cobertura_metricas$estado == "divergencia", , drop = FALSE
  ]
  expect_equal(nrow(divergencias), nrow(divergencias_internas))
  expect_true(any(liviano$cobertura_coleccion$alcance == "divergencias"))
  expect_equal(liviano$meta$n_divergencias, nrow(divergencias))
})

test_that("sql_perfil conserva las lecturas sin retener perfiles completos", {
  con <- .con_o28()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "a", data.frame(id = seq_len(10)))
  DBI::dbWriteTable(con, "b", data.frame(id = seq_len(10), w = 2))
  col <- coleccion(con, c("a", "b"), nombre = "o28")

  liviano <- perfilar_coleccion(col)
  lecturas <- sql_perfil(liviano)
  expect_s3_class(lecturas, "data.frame")
  expect_gt(nrow(lecturas), 0L)
  expect_setequal(unique(lecturas$tabla), c("a", "b"))
  expect_null(liviano$perfiles)

  conservado <- perfilar_coleccion(col, conservar_perfiles = TRUE)
  expect_gt(nrow(sql_perfil(conservado)), 0L)
  expect_gt(nrow(conservado$perfiles[["a"]]$resumen_tabla$sql), 0L)
})

test_that("n_filas devuelve una fila por tabla en una coleccion", {
  con <- .con_o28()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "a", data.frame(id = seq_len(100)))
  DBI::dbWriteTable(con, "b", data.frame(id = seq_len(10)))
  col <- coleccion(con, c("a", "b"), nombre = "o28")

  perfil <- perfilar_coleccion(col)
  expect_equal(n_filas(perfil), c(a = 100, b = 10))

  comun <- perfilar_dbi(con, "a")
  expect_equal(as.numeric(n_filas(comun)), 100)
})
