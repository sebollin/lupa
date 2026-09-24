# O33: el README promete que la corroboracion entre los dos bloques del perfil
# DBI "no calla que no coinciden". Con una muestra parcial las diferencias
# ordinarias se atribuyen al muestreo -bien- y la cobertura salia vacia: el
# lector no tenia forma de saber que los dos bloques diferian.

test_that("una muestra parcial declara las comparaciones que no corrobora", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(
    id = seq_len(100L),
    v = c(rep("a", 90L), rep("b", 10L)),
    stringsAsFactors = FALSE
  ))

  perfilado <- suppressWarnings(perfilar_dbi(
    con, "t", muestra = 10L, orden_muestra = "id",
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  ))
  cobertura <- perfilado$resumen_tabla$cobertura
  cobertura <- cobertura[cobertura$bloque == "corroboracion", , drop = FALSE]
  meta <- perfilado$resumen_tabla$meta$corroboracion_bloques

  # La regla se conserva: una diferencia esperable no es una divergencia.
  expect_equal(sum(cobertura$estado == "divergencia"), 0L)
  expect_equal(meta$divergencias, 0L)
  expect_equal(meta$estado, "muestra_parcial")

  # Y no se calla: se declara cuantas no se pudieron corroborar y sobre que
  # fraccion de filas.
  declarada <- cobertura[cobertura$estado == "no_corroborada", , drop = FALSE]
  expect_equal(nrow(declarada), 1L)
  expect_true(meta$no_corroboradas > 0L)
  expect_match(declarada$motivo[[1L]], "no se declaran divergencia",
               fixed = TRUE)
  expect_match(declarada$motivo[[1L]], "10 de 100", fixed = TRUE)
  expect_match(declarada$como_resolverlo[[1L]], "muestra = Inf", fixed = TRUE)
})

test_that("con la muestra completa nada cambia", {
  # Control: cubriendo el 100% se corrobora de verdad, y ahi una diferencia
  # si es una divergencia. Sin filas que no coincidan, no hay declaracion.
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(
    id = seq_len(30L), v = as.numeric(seq_len(30L))
  ))

  perfilado <- perfilar_dbi(
    con, "t", muestra = Inf,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  )
  cobertura <- perfilado$resumen_tabla$cobertura
  cobertura <- cobertura[cobertura$bloque == "corroboracion", , drop = FALSE]
  meta <- perfilado$resumen_tabla$meta$corroboracion_bloques

  expect_equal(meta$estado, "comparacion_completa")
  expect_equal(sum(cobertura$estado == "no_corroborada"), 0L)
})
