test_that("la corroboracion declara NaN e infinitos en todos los campos divergentes", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(
    con, "datos_cruce", data.frame(
      x = c(1:20, NaN, NaN, NA_real_, Inf, -Inf)
    )
  )

  resultado <- suppressWarnings(lupa::perfilar_dbi(
    con, "datos_cruce", muestra = Inf,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  ))
  cobertura <- resultado$resumen_tabla$cobertura
  divergencias <- cobertura[cobertura$bloque == "corroboracion", , drop = FALSE]

  expect_setequal(
    divergencias$elemento,
    paste0("x::", lupa:::.METRICAS_CORROBORACION_DBI)
  )
  expect_true(all(divergencias$estado == "divergencia"))
  expect_true(all(grepl("resumen_tabla", divergencias$motivo, fixed = TRUE)))
  expect_true(all(grepl("perfil_muestra", divergencias$motivo, fixed = TRUE)))
  expect_true(all(grepl("ambos valores quedan publicados", divergencias$motivo,
                        fixed = TRUE)))
  expect_equal(
    resultado$resumen_tabla$meta$corroboracion_bloques$estado,
    "comparacion_completa"
  )
})

test_that("la corroboracion declara una media fuera de tolerancia", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "datos_cruce", data.frame(x = c(1e16, 1, -1e16)))

  resultado <- suppressWarnings(lupa::perfilar_dbi(
    con, "datos_cruce", muestra = Inf,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  ))
  divergencias <- resultado$resumen_tabla$cobertura
  divergencias <- divergencias[divergencias$bloque == "corroboracion", , drop = FALSE]

  expect_equal(divergencias$elemento, "x::media")
  expect_match(divergencias$motivo, "0e+00", fixed = TRUE)
  expect_match(divergencias$motivo, "3.3365885416666669e-01", fixed = TRUE)
})

test_that("datos limpios no inventan una divergencia", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "datos_cruce", data.frame(x = c(0.5, 0.5, 1.5, 2.5)))

  resultado <- suppressWarnings(lupa::perfilar_dbi(
    con, "datos_cruce", muestra = Inf,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  ))
  divergencias <- resultado$resumen_tabla$cobertura
  divergencias <- divergencias[divergencias$bloque == "corroboracion", , drop = FALSE]

  expect_equal(nrow(divergencias), 0L)
  expect_equal(
    resultado$resumen_tabla$meta$corroboracion_bloques$divergencias,
    0L
  )
})

test_that("una submuestra no convierte sus diferencias esperables en fallos", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "datos_cruce", data.frame(x = seq_len(20L)))

  resultado <- suppressWarnings(lupa::perfilar_dbi(
    con, "datos_cruce", muestra = 5L, orden_muestra = "x",
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  ))
  divergencias <- resultado$resumen_tabla$cobertura
  divergencias <- divergencias[divergencias$bloque == "corroboracion", , drop = FALSE]

  expect_equal(nrow(divergencias), 0L)
  expect_equal(
    resultado$resumen_tabla$meta$corroboracion_bloques$estado,
    "muestra_parcial"
  )
  expect_equal(
    resultado$resumen_tabla$meta$corroboracion_bloques$fraccion,
    0.25
  )
})

test_that("la escala decimal del caso MariaDB se declara sin elegir un ganador", {
  resumen <- list(
    columnas = data.frame(
      columna = "x", tipo_inferido = "doble", mediana = 1e8,
      stringsAsFactors = FALSE
    ),
    sql = data.frame(
      columna = "x", metrica = "mediana", estado = "calculado",
      sql = "SELECT mediana", stringsAsFactors = FALSE
    ),
    meta = list(universo = "tabla_completa", filas = 3)
  )
  perfil <- list(
    columnas = data.frame(
      columna = "x", tipo_inferido = "doble", mediana =
        1.2345678901234568e19, stringsAsFactors = FALSE
    ),
    meta = list(filas_analizadas = 3)
  )

  resultado <- lupa:::.cobertura_corroboracion_bloques_dbi(
    resumen, perfil
  )
  expect_equal(resultado$cobertura$elemento, "x::mediana")
  expect_equal(resultado$cobertura$estado, "divergencia")
  expect_match(resultado$cobertura$motivo, "No se elige un ganador",
               fixed = TRUE)
})
