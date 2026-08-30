skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

test_that("la mediana proyecta filas con la referencia local", {
  presupuesto <- lupa:::.presupuesto_dbi(Inf, instrumentar = TRUE)
  presupuesto$referencias_mediana <- list(
    primera = list(ms_por_fila = 0.0001),
    segunda = list(ms_por_fila = 0.0002)
  )

  proyeccion <- lupa:::.proyectar_costo_mediana_dbi(
    presupuesto, n_filas = 1000000, n_medianas = 3L
  )

  expect_true(proyeccion$disponible)
  expect_equal(proyeccion$duracion_estimada_ms, 450)
  expect_equal(proyeccion$ms_por_fila, 0.00015)
  expect_equal(proyeccion$n_filas, 1000000)
  expect_equal(proyeccion$n_medianas, 3L)
  expect_equal(proyeccion$n_referencias, 2L)
  expect_false(proyeccion$referencia_declarada)
  expect_match(proyeccion$fuente, "esta corrida")
})

test_that("la mediana usa el banco declarado cuando no hay referencia local", {
  presupuesto <- lupa:::.presupuesto_dbi(Inf, instrumentar = FALSE)

  proyeccion <- lupa:::.proyectar_costo_mediana_dbi(
    presupuesto, n_filas = 1000000, n_medianas = 2L,
    usar_referencia_banco = TRUE
  )

  expect_true(proyeccion$disponible)
  expect_equal(proyeccion$duracion_estimada_ms, 136)
  expect_equal(proyeccion$ms_por_fila, 68 / 1e6)
  expect_equal(proyeccion$n_referencias, 0L)
  expect_true(proyeccion$referencia_declarada)
  expect_match(proyeccion$fuente, "otra corrida")
  expect_match(proyeccion$motivo, "no como una medicion local")
})

test_that("la cota observada de lectura no se confunde con la referencia bancaria", {
  presupuesto <- lupa:::.presupuesto_dbi(Inf, instrumentar = TRUE)
  presupuesto$duracion_lectura_mediana <- 40000

  proyeccion <- lupa:::.proyectar_costo_mediana_dbi(
    presupuesto, n_filas = 1000000, n_medianas = 1L,
    usar_referencia_banco = TRUE
  )

  expect_equal(proyeccion$duracion_estimada_ms, 40000)
  expect_equal(proyeccion$cota_lectura_ms, 40000)
  expect_match(proyeccion$fuente, "cota de lectura observada")
  expect_match(proyeccion$motivo, "no es una medicion de mediana")
})

test_that("la moda usa cardinalidades disponibles y declara las que faltan", {
  presupuesto <- lupa:::.presupuesto_dbi(Inf, instrumentar = TRUE)
  presupuesto$referencias_moda <- list(
    primera = list(ms_por_distinto = 2),
    segunda = list(ms_por_distinto = 4)
  )
  cardinalidades <- list(
    a = list(n_distintos = 100),
    b = list(n_distintos = NA_real_),
    c = list(n_distintos = 300)
  )

  proyeccion <- lupa:::.proyectar_costo_moda_dbi(
    presupuesto, cardinalidades
  )

  expect_true(proyeccion$disponible)
  expect_equal(proyeccion$duracion_estimada_ms, 1200)
  expect_equal(proyeccion$n_distintos_proyectados, 400)
  expect_equal(proyeccion$n_columnas, 2L)
  expect_equal(proyeccion$n_referencias, 2L)
  expect_identical(proyeccion$columnas_sin_cardinalidad, "b")
  expect_identical(proyeccion$fuentes_cardinalidad, "fuente no declarada")
  expect_match(proyeccion$motivo, "parcial")
  expect_match(proyeccion$motivo, "Fuentes de cardinalidad")
})

test_that("la moda no inventa una proyeccion sin cardinalidad", {
  presupuesto <- lupa:::.presupuesto_dbi(Inf, instrumentar = TRUE)
  presupuesto$referencias_moda <- list(
    primera = list(ms_por_distinto = 2)
  )

  proyeccion <- lupa:::.proyectar_costo_moda_dbi(
    presupuesto, list(a = list(n_distintos = NA_real_))
  )

  expect_false(proyeccion$disponible)
  expect_true(is.na(proyeccion$duracion_estimada_ms))
  expect_identical(proyeccion$columnas_sin_cardinalidad, "a")
  expect_match(proyeccion$motivo, "falta la cardinalidad")
})

test_that("cada aviso tiene interruptor y umbral en el borde", {
  moda <- list(
    disponible = TRUE, duracion_estimada_ms = 30000, n_columnas = 1L,
    fuente = "referencia local", columnas_sin_cardinalidad = character()
  )
  mediana <- list(
    disponible = TRUE, duracion_estimada_ms = 30000, n_filas = 1000000,
    n_medianas = 1L, fuente = "referencia local",
    motivo = "proyeccion local"
  )

  expect_message(
    lupa:::.avisar_costo_moda_dbi(moda, TRUE, 30), "Costo estimado.*30,0 s"
  )
  expect_silent(lupa:::.avisar_costo_moda_dbi(moda, TRUE, 30.001))
  expect_silent(lupa:::.avisar_costo_moda_dbi(moda, FALSE, 0))
  expect_message(
    lupa:::.avisar_costo_mediana_dbi(mediana, TRUE, 30),
    "Costo estimado.*30,0 s"
  )
  expect_silent(lupa:::.avisar_costo_mediana_dbi(mediana, TRUE, 30.001))
  expect_silent(lupa:::.avisar_costo_mediana_dbi(mediana, FALSE, 0))
  expect_error(
    lupa:::.avisar_costo_mediana_dbi(mediana, TRUE, NA_real_),
    "umbral_segundos"
  )
})

test_that("la metadata separa ambos canales y el aviso llega antes de pagar", {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "tabla_aviso_caras", data.frame(
    a = rep(1:100, 100), b = rep(1:100, 100)
  ))

  eventos <- character()
  original_moda <- lupa:::.moda_columna_dbi
  original_escalar <- lupa:::.escalar_dbi
  testthat::local_mocked_bindings(
    .avisar_costo_moda_dbi = function(...) {
      eventos <<- c(eventos, "aviso_moda")
      invisible(TRUE)
    },
    .avisar_costo_mediana_dbi = function(...) {
      eventos <<- c(eventos, "aviso_mediana")
      invisible(TRUE)
    },
    .moda_columna_dbi = function(...) {
      eventos <<- c(eventos, "paga_moda")
      resultado <- original_moda(...)
      resultado$duracion_ms <- 1000
      resultado
    },
    .escalar_dbi = function(conexion, sql, alias, presupuesto,
                            etapa = "escalar") {
      if (identical(etapa, "mediana")) eventos <<- c(eventos, "paga_mediana")
      original_escalar(conexion, sql, alias, presupuesto, etapa = etapa)
    },
    .package = "lupa"
  )

  resultado <- suppressMessages(perfilar_dbi(
    conexion, "tabla_aviso_caras",
    metricas = c("validos", "distintos", "moda", "mediana"),
    bloque_muestra = "solo_agregados", instrumentar = TRUE,
    avisar_costo_distintos = FALSE, avisar_derrame_estimado = FALSE,
    avisar_costo_moda = TRUE, umbral_segundos_aviso_moda = 0,
    avisar_costo_mediana = TRUE, umbral_segundos_aviso_mediana = 0,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE, ausencia_estructural = FALSE,
    duplicados_aproximados = FALSE
  ))

  expect_lt(match("aviso_moda", eventos), match("paga_moda", eventos))
  expect_lt(
    match("aviso_mediana", eventos),
    max(which(eventos == "paga_mediana"))
  )
  expect_true(is.list(resultado$resumen_tabla$meta$costo_moda))
  expect_true(is.list(resultado$resumen_tabla$meta$costo_mediana))
  expect_true(is.null(resultado$resumen_tabla$meta$costo_metricas_caras))
})

test_that("la estrategia omitida deja declarada la falta de cardinalidad de moda", {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "sin_cardinalidad", data.frame(valor = 1:10))

  resultado <- suppressMessages(perfilar_dbi(
    conexion, "sin_cardinalidad",
    metricas = c("validos", "moda"), estrategia_distintos = "omitida",
    bloque_muestra = "solo_agregados", instrumentar = TRUE,
    avisar_costo_distintos = FALSE, avisar_derrame_estimado = FALSE,
    avisar_costo_moda = FALSE,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE, ausencia_estructural = FALSE,
    duplicados_aproximados = FALSE
  ))

  costo <- resultado$resumen_tabla$meta$costo_moda
  expect_false(costo$disponible)
  expect_match(costo$motivo, "cardinalidad")
  expect_match(costo$motivo, "[Nn]o se puede proyectar")
})

test_that("perfilar_dbi valida los cuatro controles nuevos", {
  expect_error(perfilar_dbi(NULL, "tabla", avisar_costo_moda = NA),
               "avisar_costo_moda")
  expect_error(perfilar_dbi(NULL, "tabla", umbral_segundos_aviso_moda = NA),
               "umbral_segundos_aviso_moda")
  expect_error(perfilar_dbi(NULL, "tabla", avisar_costo_mediana = NA),
               "avisar_costo_mediana")
  expect_error(perfilar_dbi(NULL, "tabla", umbral_segundos_aviso_mediana = NA),
               "umbral_segundos_aviso_mediana")
})
