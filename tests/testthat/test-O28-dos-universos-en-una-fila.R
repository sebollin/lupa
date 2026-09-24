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

test_that("la coleccion conserva las divergencias del perfil interno", {
  con <- .con_o28()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (v TEXT COLLATE RTRIM)")
  DBI::dbExecute(con, "INSERT INTO t VALUES ('x '), ('x '), ('x'), ('x')")
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
  expect_equal(n_filas(perfil), c(100, 10))

  comun <- perfilar_dbi(con, "a")
  expect_equal(as.numeric(n_filas(comun)), 100)
})
