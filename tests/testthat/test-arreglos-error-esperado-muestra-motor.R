testthat::skip_on_cran()

skip_if_not_installed("DBI")
skip_if_not_installed("duckdb")

test_that("error_esperado y estado se clasifican por metrica en las dos vias", {
  conexion <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conexion, shutdown = TRUE), add = TRUE)
  set.seed(1)
  DBI::dbWriteTable(
    conexion, "a2520_error_esperado",
    data.frame(
      x = rnorm(20000), y = sample(1:50, 20000, TRUE),
      stringsAsFactors = FALSE
    )
  )
  on.exit(
    try(DBI::dbRemoveTable(conexion, "a2520_error_esperado"), silent = TRUE),
    add = TRUE
  )

  esperado <- data.frame(
    metrica = c(
      "n", "n_validos", "n_faltantes", "prop_faltantes", "n_distintos",
      "tasa_distintos", "moda", "frecuencia_moda", "minimo", "maximo",
      "media", "n_ceros", "n_negativos", "mediana", "desvio"
    ),
    estado = c(
      "calculado", rep("observado_muestra", 5L), "estimado",
      "observado_muestra", rep("estimado", 3L),
      rep("observado_muestra", 2L), "estimado", "estimado"
    ),
    error_esperado = c(
      "no_aplica", rep("no_estimado", 3L), rep("no_estimable", 4L),
      rep("no_estimado", 5L), "no_estimable", "no_estimado"
    ),
    stringsAsFactors = FALSE
  )

  perfiles <- setNames(lapply(c("solo_agregados", "con_muestra"), function(via) {
    suppressWarnings(lupa::perfilar_dbi(
      conexion, "a2520_error_esperado", universo = "muestra_motor",
      muestra_motor = 2000L, bloque_muestra = via
    ))
  }), c("solo_agregados", "con_muestra"))
  registros <- lapply(perfiles, function(perfil) {
    sql <- perfil$resumen_tabla$sql
    salida <- sql[sql$columna == "x", c(
      "metrica", "estado", "error_esperado", "motivo"
    )]
    rownames(salida) <- NULL
    salida
  })

  for (i in seq_along(registros)) {
    actual <- registros[[i]]
    expect_equal(actual[, names(esperado)], esperado, info = names(perfiles)[i])
    expect_true(all(grepl(
      "conteo exacto en la muestra",
      actual$motivo[actual$metrica %in% c(
        "n_validos", "n_faltantes", "prop_faltantes"
      )], fixed = TRUE
    )), info = "motivo de completitud")
    expect_true(all(grepl(
      "cardinalidad observada",
      actual$motivo[actual$metrica %in% c("n_distintos", "tasa_distintos")],
      fixed = TRUE
    )), info = "motivo de cardinalidad")
    expect_true(grepl(
      "mediana de una muestra", actual$motivo[actual$metrica == "mediana"],
      fixed = TRUE
    ), info = "motivo de mediana")
  }
  expect_equal(
    registros[[1L]][, c("metrica", "estado", "error_esperado")],
    registros[[2L]][, c("metrica", "estado", "error_esperado")]
  )
})
