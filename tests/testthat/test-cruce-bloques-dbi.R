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

test_that("la corroboracion sigue el contrato cuando los dos caminos difieren", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  # `{1e16, 1, -1e16}` tiene media 1/3 y el motor la calcula como `0`: acumula en
  # double y `1e16 + 1` redondea de vuelta a `1e16`.
  #
  # Cuanto da el lado de R DEPENDE DE LA PLATAFORMA, y eso costo dos corridas de
  # CI aprenderlo. `mean()` aplica una correccion de segunda pasada en
  # `long double`; en x86, que tiene long double de 80 bits, recupera
  # 0.33365885416666669 y hay divergencia. En macOS ARM64 no hay long double
  # extendido, `mean()` tambien da `0`, los dos caminos coinciden y **no hay nada
  # que declarar**.
  #
  # Las dos conductas son correctas. Lo que la prueba tiene que fijar no es el
  # numero ni la existencia de la divergencia, sino el CONTRATO, que vale en las
  # dos plataformas: si difieren se declara, y si coinciden no se inventa nada.
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "datos_cruce", data.frame(x = c(1e16, 1, -1e16)))

  resultado <- suppressWarnings(lupa::perfilar_dbi(
    con, "datos_cruce", muestra = Inf,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  ))
  divergencias <- resultado$resumen_tabla$cobertura
  divergencias <- divergencias[divergencias$bloque == "corroboracion", , drop = FALSE]

  media_motor <- resultado$resumen_tabla$columnas$media[
    resultado$resumen_tabla$columnas$columna == "x"
  ]
  media_muestra <- resultado$perfil_muestra$columnas$media[
    resultado$perfil_muestra$columnas$columna == "x"
  ]
  expect_equal(media_motor, 0)

  if (isTRUE(all.equal(media_motor, media_muestra))) {
    # Plataforma sin long double extendido: coinciden, y no se declara nada.
    expect_equal(nrow(divergencias), 0L)
  } else {
    expect_equal(divergencias$elemento, "x::media")
    # Texto fijo del paquete, no la representacion de un numero calculado.
    expect_match(divergencias$motivo, "resumen_tabla", fixed = TRUE)
    expect_match(divergencias$motivo, "perfil_muestra", fixed = TRUE)
    expect_match(divergencias$motivo, "no coinciden", fixed = TRUE)
    expect_match(divergencias$motivo, "3 de 3 filas", fixed = TRUE)
  }
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
