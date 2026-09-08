test_that("las politicas sample-only quedan explicitas y cubiertas en DBI", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(
    flag = c("Si", "No", "Si", "No", "Si", "No"),
    centinela = c(10, 9999, 20, 99, NA, 30),
    aplicable = c(10, 100, 20, 99, NA, 30),
    opcional = c(1, NA, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  ))

  expect_true(all(c(
    "sentinelas_numericos", "aplicabilidad", "columnas_opcionales"
  ) %in% names(formals(perfilar_dbi))))

  resultado <- suppressWarnings(perfilar_dbi(
    con, "t", sentinelas_numericos = 9999,
    aplicabilidad = list(aplicable = ~ flag == "Si"),
    columnas_opcionales = "opcional",
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  ))
  cobertura <- resultado$resumen_tabla$cobertura
  for (argumento in c(
    "sentinelas_numericos", "aplicabilidad", "columnas_opcionales"
  )) {
    fila <- cobertura[
      cobertura$bloque == "resumen_tabla" &
        cobertura$elemento == argumento,
      , drop = FALSE
    ]
    expect_equal(nrow(fila), 1L, info = argumento)
    expect_equal(fila$estado, "degradado", info = argumento)
    expect_match(fila$motivo, paste0("`", argumento, "`"), info = argumento)
    expect_match(fila$como_resolverlo, "perfil_muestra", info = argumento)
  }

  sql <- resultado$resumen_tabla$columnas
  muestra <- resultado$perfil_muestra$columnas
  sql_centinela <- sql[sql$columna == "centinela", "maximo"]
  muestra_centinela <- muestra[
    muestra$columna == "centinela", "maximo"
  ]
  expect_false(identical(sql_centinela, muestra_centinela))
  sql_aplicable <- sql[sql$columna == "aplicable", "maximo"]
  muestra_aplicable <- muestra[
    muestra$columna == "aplicable", "maximo"
  ]
  expect_false(identical(sql_aplicable, muestra_aplicable))
})

test_that("las formas equivalentes de la lista por omision dan lo mismo en DBI", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  x <- c(1:1500, rep(1501:1505, each = 200L), rep(-9L, 200L))
  DBI::dbWriteTable(con, "t", data.frame(x = x))
  d <- c(-9, -99, -999, -9999, 999)
  formas <- list(
    d,
    rev(d),
    as.integer(d),
    c(d, d[[1L]])
  )
  resultados <- lapply(formas, function(sentinelas) {
    suppressWarnings(perfilar_dbi(
      con, "t", universo = "muestra_motor", muestra_motor = 4000L,
      incluir_valores = TRUE, sentinelas_numericos = sentinelas,
      analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    ))
  })
  omitido <- suppressWarnings(perfilar_dbi(
    con, "t", universo = "muestra_motor", muestra_motor = 4000L,
    incluir_valores = TRUE, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  ))
  todos <- c(resultados, list(omitido))
  conteos <- vapply(todos, function(perfil) {
    as.numeric(perfil$perfil_muestra$columnas$n_faltantes_disfrazados[[1L]])
  }, numeric(1L))

  expect_equal(unname(conteos), rep(200, 5L))
  expect_equal(
    unname(lapply(todos, function(perfil) {
      perfil$perfil_muestra$meta$sentinelas_numericos
    })),
    rep(list(sort(d)), 5L)
  )
})
