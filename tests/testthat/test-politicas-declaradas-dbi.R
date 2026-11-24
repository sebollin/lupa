# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

# El resumen de tabla completa aplica los centinelas declarados, las columnas
# opcionales y la regla de aplicabilidad. Antes afirmaba lo contrario: una fila
# `degradado` por argumento, para toda la tabla, y el maximo del resumen
# distinto del de la muestra.
test_that("las politicas de ausencia se aplican en el resumen de tabla completa", {
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
  degradadas <- cobertura[
    cobertura$bloque == "resumen_tabla" & cobertura$estado == "degradado",
    , drop = FALSE
  ]
  # Las tres se aplicaron: no queda nada que declarar.
  expect_identical(nrow(degradadas), 0L)

  sql <- resultado$resumen_tabla$columnas
  muestra <- resultado$perfil_muestra$columnas
  de <- function(tabla, columna, campo) tabla[tabla$columna == columna, campo]
  # A mano: sin el 9999, `centinela` es 10, 20, 99, 30.
  expect_equal(de(sql, "centinela", "maximo"), 99)
  expect_equal(de(sql, "centinela", "media"), (10 + 20 + 99 + 30) / 4)
  expect_equal(de(sql, "centinela", "n_validos"), 5)
  expect_equal(de(sql, "centinela", "n_valores_excluidos_resumen"), 1)
  expect_equal(de(sql, "centinela", "maximo"),
               de(muestra, "centinela", "maximo"))
  # `opcional` tiene un `NULL` y no es un faltante.
  expect_equal(de(sql, "opcional", "n_faltantes"), 0)
  expect_equal(de(sql, "opcional", "n_aplicables"), 5)
  expect_equal(de(muestra, "opcional", "n_faltantes"), 0)
  # La regla deja las tres filas de `flag == "Si"`: 10, 20 y un `NULL`.
  expect_equal(de(sql, "aplicable", "maximo"), 20)
  expect_equal(de(sql, "aplicable", "n_aplicables"), 3)
  expect_equal(de(sql, "aplicable", "n_faltantes"), 1)
  expect_equal(de(muestra, "aplicable", "maximo"), 20)
  politica <- resultado$resumen_tabla$meta$politica_ausencia
  expect_identical(politica$columnas_opcionales, "opcional")
  expect_identical(politica$aplicabilidad, "aplicable")
  expect_identical(nrow(politica$sin_aplicar), 0L)
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
