skip_if_not_installed("RSQLite")

crear_tabla_o13 <- function(con, nombre, tipo, valores) {
  DBI::dbExecute(con, paste0("CREATE TABLE ", nombre, " (x ", tipo, ")"))
  DBI::dbExecute(
    con,
    paste0("INSERT INTO ", nombre, " (x) VALUES ", paste(valores, collapse = ", "))
  )
}

test_that("un BIGINT grande entregado como doble no se publica como medido", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", bigint = "numeric")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  crear_tabla_o13(
    con, "t_big", "BIGINT",
    c("(9007199254740993)", "(4503599627370497)", "(42)", "(NULL)")
  )

  perfil <- perfilar_dbi(con, "t_big")
  sql <- perfil$resumen_tabla$sql
  maximo_sql <- sql[sql$columna == "x" & sql$metrica == "maximo", , drop = FALSE]
  fila_sql <- perfil$resumen_tabla$columnas[1, , drop = FALSE]
  fila_muestra <- perfil$perfil_muestra$columnas[1, , drop = FALSE]
  cobertura <- perfil$perfil_muestra$cobertura_diagnosticos
  cobertura <- cobertura[cobertura$diagnostico == "entero64_como_doble", , drop = FALSE]

  expect_equal(maximo_sql$estado, "no_disponible")
  expect_true(is.na(fila_sql$maximo))
  expect_match(maximo_sql$motivo, "driver entrego como doble")
  expect_identical(fila_muestra$estado_resumen_cuantitativo, "omitidos_precision")
  expect_true(is.na(fila_muestra$minimo))
  expect_true(is.na(fila_muestra$maximo))
  expect_true(is.na(fila_muestra$media))
  expect_equal(fila_muestra$n_distintos, 3)
  expect_false(any(grepl(
    "entero64_como_doble", sql[sql$columna == "x", ]$motivo, fixed = TRUE
  )))
  expect_equal(nrow(cobertura), 0L)
  expect_match(
    perfil$perfil_muestra$meta$origen_dbi$muestreo$sql_muestra,
    "CAST(`x` AS VARCHAR) AS `x`", fixed = TRUE
  )
})

test_that("diez BIGINT consecutivos conservan sus distintos en la muestra", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", bigint = "numeric")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t_ids (id BIGINT, v INTEGER)")
  for (k in 0:9) DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO t_ids VALUES (1152921504606846976 + ", k, ", ", k, ")"
    )
  )

  perfil <- perfilar_dbi(con, "t_ids")
  fila <- perfil$perfil_muestra$columnas[
    perfil$perfil_muestra$columnas$columna == "id", , drop = FALSE
  ]
  hallazgos <- perfil$perfil_muestra$hallazgos

  expect_equal(fila$n_distintos, 10)
  expect_false(any(
    hallazgos$columna == "id" & hallazgos$tipo_hallazgo == "constante"
  ))
  expect_false("filas_duplicadas" %in% hallazgos$tipo_hallazgo)
  expect_match(
    perfil$perfil_muestra$meta$origen_dbi$muestreo$sql_muestra,
    "CAST(`id` AS VARCHAR) AS `id`", fixed = TRUE
  )
})

test_that("un BIGINT chico conserva el calculo", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", bigint = "numeric")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  crear_tabla_o13(con, "t_small", "BIGINT", c("(-2)", "(0)", "(42)", "(NULL)"))

  perfil <- perfilar_dbi(con, "t_small")
  maximo_sql <- perfil$resumen_tabla$sql[
    perfil$resumen_tabla$sql$columna == "x" &
      perfil$resumen_tabla$sql$metrica == "maximo", , drop = FALSE
  ]
  fila_sql <- perfil$resumen_tabla$columnas[1, , drop = FALSE]
  fila_muestra <- perfil$perfil_muestra$columnas[1, , drop = FALSE]

  expect_equal(maximo_sql$estado, "calculado")
  expect_equal(fila_sql$maximo, 42)
  expect_equal(fila_muestra$maximo, 42)
  expect_false(identical(fila_muestra$tipo_declarado, "texto"))
  expect_identical(fila_muestra$estado_resumen_cuantitativo, "calculados")
  expect_false(nrow(perfil$perfil_muestra$cobertura_diagnosticos) > 0L)
})

test_that("sin bit64 el BIGINT de la muestra queda como texto y se declara", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", bigint = "numeric")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  crear_tabla_o13(
    con, "t_sin_bit64", "BIGINT",
    c("(1152921504606846976)", "(1152921504606846977)")
  )

  testthat::local_mocked_bindings(
    .bit64_disponible = function() FALSE, .package = "lupa"
  )
  perfil <- perfilar_dbi(con, "t_sin_bit64")
  fila <- perfil$perfil_muestra$columnas[1, , drop = FALSE]
  cobertura <- perfil$perfil_muestra$cobertura_diagnosticos[
    perfil$perfil_muestra$cobertura_diagnosticos$diagnostico ==
      "entero64_como_texto", , drop = FALSE
  ]

  expect_identical(fila$tipo_declarado, "texto")
  expect_equal(fila$n_distintos, 2)
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$como_resolverlo, "bit64")
  expect_match(cobertura$como_resolverlo, "integer64")
})

test_that("un REAL grande sigue siendo un doble valido", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", bigint = "numeric")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  crear_tabla_o13(
    con, "t_real", "REAL",
    c("(9007199254740992.0)", "(4503599627370496.0)", "(42.0)", "(NULL)")
  )

  perfil <- perfilar_dbi(con, "t_real")
  maximo_sql <- perfil$resumen_tabla$sql[
    perfil$resumen_tabla$sql$columna == "x" &
      perfil$resumen_tabla$sql$metrica == "maximo", , drop = FALSE
  ]
  fila_muestra <- perfil$perfil_muestra$columnas[1, , drop = FALSE]

  expect_equal(maximo_sql$estado, "calculado")
  expect_equal(fila_muestra$maximo, 9007199254740992)
  expect_identical(fila_muestra$estado_resumen_cuantitativo, "calculados")
})
