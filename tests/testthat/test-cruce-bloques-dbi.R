# Fuera de CRAN: el check en Windows llego a 13 min (CRAN pide menos de 10) y
# esta prueba tarda mas de 0,2 s en modo CRAN (medicion del 2026-10-25). Corre en
# la suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

test_that("con NaN e infinitos los dos caminos coinciden", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  valores <- c(1:20, NaN, NaN, NA_real_, Inf, -Inf)
  DBI::dbWriteTable(con, "datos_cruce", data.frame(x = valores))

  resultado <- suppressWarnings(lupa::perfilar_dbi(
    con, "datos_cruce", muestra = Inf,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  ))
  cobertura <- resultado$resumen_tabla$cobertura
  divergencias <- cobertura[cobertura$bloque == "corroboracion", , drop = FALSE]

  # `NaN` y `NA` son tres faltantes; `Inf` y `-Inf` son presentes para los
  # conteos y quedan fuera de los agregados de magnitud. Los dos caminos ven
  # exactamente esas mismas reglas, asi que no se declara ninguna divergencia,
  # tampoco de `frecuencia_moda`.
  expect_equal(nrow(divergencias), 0L)
  expect_equal(
    resultado$resumen_tabla$meta$corroboracion_bloques$estado,
    "comparacion_completa"
  )
  resumen <- resultado$resumen_tabla$columnas
  resumen <- resumen[resumen$columna == "x", , drop = FALSE]
  muestra <- resultado$perfil_muestra$columnas
  muestra <- muestra[muestra$columna == "x", , drop = FALSE]
  memoria <- perfilar(data.frame(x = valores))$columnas
  memoria <- memoria[memoria$columna == "x", , drop = FALSE]
  expect_equal(resumen$n_faltantes, 3)
  expect_equal(resumen$n_validos, resumen$n - resumen$n_faltantes)
  expect_equal(muestra$n - muestra$n_faltantes, resumen$n_validos)
  expect_equal(memoria$n - memoria$n_faltantes, resumen$n_validos)
  for (campo in c(
    "n_distintos", "minimo", "maximo", "media", "mediana", "desvio",
    "n_ceros", "n_negativos", "frecuencia_moda"
  )) {
    expect_equal(resumen[[campo]], muestra[[campo]], tolerance = 1e-12,
                 info = campo)
    expect_equal(resumen[[campo]], memoria[[campo]], tolerance = 1e-12,
                 info = campo)
  }
  expect_true(is.na(resultado$resumen_tabla$columnas$moda[[1L]]))
  expect_true(is.na(resultado$perfil_muestra$columnas$moda[[1L]]))
  expect_true(is.na(memoria$moda[[1L]]))
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
  corroboracion <- resultado$resumen_tabla$cobertura
  corroboracion <- corroboracion[corroboracion$bloque == "corroboracion", ,
                                 drop = FALSE]
  divergencias <- corroboracion[corroboracion$estado == "divergencia", ,
                                drop = FALSE]

  # Ninguna diferencia esperable se convierte en divergencia...
  expect_equal(nrow(divergencias), 0L)
  # ...y tampoco se callan: la fila declara cuantas no se pudieron corroborar.
  declaradas <- corroboracion[corroboracion$estado == "no_corroborada", ,
                              drop = FALSE]
  expect_equal(nrow(declaradas), 1L)
  expect_true(grepl("no se declaran divergencia", declaradas$motivo[[1L]],
                    fixed = TRUE))
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
