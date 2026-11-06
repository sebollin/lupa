# Un percentil exacto del motor se usa solo si devuelve todas sus cifras.
# MariaDB 11.8 acepta `PERCENTILE_CONT ... OVER ()` y tipa el resultado como
# `double(18,10)`: 1e12 vuelve como 99999999.9999999999 y 1e-12 como 0. La
# consulta de prueba corria sobre `1.0` y nada miraba su valor. Las pruebas
# contra el motor estan en `test-local-mysql.R`; aca, la decision, sin motor.

testthat::skip_on_cran()

test_that("la prueba del percentil rechaza el resultado que pierde cifras", {
  esperado <- lupa:::.CIFRAS_PRUEBA_PERCENTIL_DBI
  exacto <- data.frame(
    lupa_sonda = esperado[["lupa_sonda"]],
    LUPA_PERCENTIL_FINO = esperado[["lupa_percentil_fino"]]
  )
  # Los alias se leen sin distinguir caja, como los pliegan algunos motores.
  expect_true(is.na(lupa:::.cifras_perdidas_percentil_dbi(exacto)))

  # Lo que devuelve MariaDB: saturado en 1e8 y redondeado a diez decimales.
  truncado <- data.frame(
    lupa_sonda = 99999999.9999999999, lupa_percentil_fino = 0
  )
  motivo <- lupa:::.cifras_perdidas_percentil_dbi(truncado)
  expect_match(motivo, "pierde cifras", fixed = TRUE)
  # Un solo valor redondeado alcanza para rechazarlo.
  redondeado <- exacto
  redondeado$LUPA_PERCENTIL_FINO <- signif(
    esperado[["lupa_percentil_fino"]], 10
  )
  expect_false(is.na(lupa:::.cifras_perdidas_percentil_dbi(redondeado)))
  # Y una respuesta sin el segundo campo no se toma como exacta.
  expect_false(is.na(lupa:::.cifras_perdidas_percentil_dbi(
    exacto["lupa_sonda"]
  )))
})

test_that("la prueba lee una fila con un valor grande y uno chico", {
  consulta <- lupa:::.prueba_percentil_dbi('"lupa_sonda"', ventana = TRUE)
  expect_match(consulta, "ORDER BY lupa_valor) OVER ()", fixed = TRUE)
  expect_match(consulta, "ORDER BY lupa_fino) OVER ()", fixed = TRUE)
  # Los literales vuelven a ser exactamente las cifras que se exigen.
  literales <- regmatches(
    consulta, gregexpr("-?[0-9.]+e[-+][0-9]+", consulta)
  )[[1L]]
  expect_identical(
    as.numeric(literales), unname(lupa:::.CIFRAS_PRUEBA_PERCENTIL_DBI)
  )
  expect_false(grepl("UNION", consulta, fixed = TRUE))
})

test_that("la consolidada no se elige si la prueba vuelve truncada", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  candidato <- list(
    nombre = "PERCENTILE_CONT_OVER", comprobar_cifras = TRUE,
    sonda = function(alias) lupa:::.prueba_percentil_dbi(alias, ventana = TRUE)
  )
  responder <- function(datos) {
    testthat::local_mocked_bindings(
      .candidatos_mediana_consolidada_dbi = function(conexion) list(candidato),
      .consultar_dbi = function(conexion, sql, presupuesto = NULL,
                                filas = -1L, etapa = "consulta") {
        list(ok = TRUE, datos = datos, motivo = NA_character_)
      },
      .package = "lupa", .env = environment()
    )
    lupa:::.sondar_mediana_consolidada_dbi(con, NULL)
  }
  truncada <- responder(
    data.frame(lupa_sonda = 99999999.9999999999, lupa_percentil_fino = 0)
  )
  expect_false(truncada$disponible)
  expect_match(truncada$motivo, "sin todas sus cifras", fixed = TRUE)
  expect_match(truncada$motivo, "pierde cifras", fixed = TRUE)

  exacta <- responder(as.data.frame(as.list(
    lupa:::.CIFRAS_PRUEBA_PERCENTIL_DBI
  )))
  expect_true(exacta$disponible)
  expect_identical(exacta$candidato$nombre, "PERCENTILE_CONT_OVER")
})
