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

  # El motivo tiene que nombrar los DOS valores, y se toman de la misma corrida
  # en vez de escribirlos a mano.
  #
  # La primera version fijaba el literal `3.3365885416666669e-01`, que es lo que
  # da `mean(c(1e16, 1, -1e16))` en x86. Fallo en macOS ARM64 y con razon: `mean()`
  # aplica una correccion de segunda pasada en `long double`, y ARM64 no tiene
  # long double de 80 bits, asi que ese digito cambia por construccion. Fijar la
  # expansion decimal de una suma con cancelacion catastrofica es fijar
  # justamente el numero que no es portable.
  # El motivo nombra los dos bloques y dice que no coinciden; eso es texto fijo
  # del paquete y si es portable.
  expect_match(divergencias$motivo, "resumen_tabla", fixed = TRUE)
  expect_match(divergencias$motivo, "perfil_muestra", fixed = TRUE)
  expect_match(divergencias$motivo, "no coinciden", fixed = TRUE)
  expect_match(divergencias$motivo, "3 de 3 filas", fixed = TRUE)

  # Y lo que la prueba de verdad quiere decir: los dos caminos NO coinciden, el
  # del motor colapso a cero por la cancelacion y el de la muestra quedo cerca
  # de la verdad, que es 1/3.
  media_motor <- resultado$resumen_tabla$columnas$media[
    resultado$resumen_tabla$columnas$columna == "x"
  ]
  media_muestra <- resultado$perfil_muestra$columnas$media[
    resultado$perfil_muestra$columnas$columna == "x"
  ]
  expect_false(isTRUE(all.equal(media_motor, media_muestra)))
  expect_equal(media_motor, 0)
  expect_equal(media_muestra, 1 / 3, tolerance = 1e-2)
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
