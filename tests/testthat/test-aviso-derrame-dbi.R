test_that("la proyeccion de distintos usa mediciones planas de esta corrida", {
  presupuesto <- lupa:::.presupuesto_dbi(Inf, instrumentar = TRUE)
  presupuesto$referencias_planas <- list(
    primera = list(duracion_ms = 15000),
    segunda = list(duracion_ms = 17000)
  )

  proyeccion <- lupa:::.proyectar_costo_distintos_dbi(presupuesto, 2L)

  expect_true(proyeccion$disponible)
  expect_identical(proyeccion$duracion_referencia_ms, 16000)
  expect_identical(proyeccion$duracion_estimada_ms, 32000)
  expect_identical(proyeccion$n_referencias, 2L)
  expect_match(proyeccion$fuente, "medidas en esta corrida")
  expect_match(proyeccion$motivo, "estimacion")

  presupuesto$referencias_planas <- list()
  sin_referencia <- lupa:::.proyectar_costo_distintos_dbi(presupuesto, 2L)
  expect_false(sin_referencia$disponible)
  expect_true(is.na(sin_referencia$duracion_estimada_ms))
  expect_match(sin_referencia$motivo, "No hay una duracion medida")
})

test_that("el aviso de costo incluye el valor y su fuente", {
  proyeccion <- list(
    disponible = TRUE, duracion_estimada_ms = 30000, n_lotes = 2L,
    fuente = "mediana de 2 consultas planas medidas en esta corrida"
  )

  expect_message(
    lupa:::.avisar_costo_distintos_dbi(proyeccion),
    "Costo estimado.*30,0 s.*Fuente:.*medidas en esta corrida"
  )
})

test_that("el aviso llega antes de ejecutar el lote de distintos", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "tabla_aviso", data.frame(
    codigo = c("a", "b", "a"), valor = c(1, 2, 3),
    stringsAsFactors = FALSE
  ))

  eventos <- character()
  original_distintos <- lupa:::.conteos_distintos_lote_dbi
  testthat::local_mocked_bindings(
    .registrar_referencia_plana_dbi = function(presupuesto, consulta) {
      presupuesto$referencias_planas <- list(
        referencia = list(duracion_ms = 30000)
      )
      invisible(NULL)
    },
    .avisar_costo_distintos_dbi = function(proyeccion) {
      eventos <<- c(eventos, "aviso")
      expect_true(proyeccion$disponible)
      expect_identical(proyeccion$duracion_estimada_ms, 30000)
    },
    .conteos_distintos_lote_dbi = function(...) {
      eventos <<- c(eventos, "distintos")
      original_distintos(...)
    },
    .package = "lupa"
  )

  perfilar_dbi(
    conexion, "tabla_aviso", metricas = c("validos", "distintos"),
    bloque_muestra = "solo_agregados", instrumentar = TRUE,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE, ausencia_estructural = FALSE,
    duplicados_aproximados = FALSE
  )

  expect_lt(match("aviso", eventos), match("distintos", eventos))
})

.estadisticas_derrame_prueba <- function(escrito = 7, llamadas = 11) {
  data.frame(
    query = "SELECT COUNT(DISTINCT x) AS n FROM tabla",
    calls = llamadas, temp_blks_read = 2, temp_blks_written = escrito,
    query_normalizada = "SELECT COUNT(DISTINCT x) AS n FROM tabla",
    stringsAsFactors = FALSE
  )
}

test_that("el informe publica bloques cuando la llamada es atribuible", {
  lectura <- 0L
  antes <- .estadisticas_derrame_prueba(escrito = 0, llamadas = 10)
  despues <- .estadisticas_derrame_prueba(escrito = 7, llamadas = 11)
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "PqConnection",
    .estadisticas_derrame_postgresql_dbi = function(conexion) {
      lectura <<- lectura + 1L
      if (lectura == 1L) antes else despues
    },
    .package = "lupa"
  )
  conexion <- structure(list(), class = "PqConnection")
  presupuesto <- lupa:::.presupuesto_dbi(Inf, instrumentar = TRUE)

  inicio <- lupa:::.iniciar_instrumentacion_derrame_dbi(
    conexion, presupuesto, exacto = TRUE
  )
  expect_identical(inicio$estado, "observando")
  lupa:::.finalizar_instrumentacion_derrame_dbi(conexion, presupuesto)
  publicado <- lupa:::.publicar_derrame_dbi(presupuesto)

  expect_true(publicado$disponible)
  expect_identical(publicado$consultas_observadas, 1L)
  expect_identical(publicado$consultas_con_derrame, 1L)
  expect_identical(publicado$bloques_temporales_leidos, 0)
  expect_identical(publicado$bloques_temporales_escritos, 7)
  expect_identical(publicado$fuente, "pg_stat_statements")

  sql <- lupa:::.registro_sql_dbi(
    "x", "distintos", "medido", NA_character_, antes$query[[1L]]
  )
  sql <- lupa:::.adjuntar_derrame_sql_dbi(sql, presupuesto$derrame)
  expect_true(sql$derrame[[1L]])
  expect_identical(sql$bloques_temporales_escritos[[1L]], 7)
  expect_identical(sql$fuente_derrame[[1L]], "pg_stat_statements")
})

test_that("una llamada concurrente no se presenta como derrame propio", {
  lectura <- 0L
  antes <- .estadisticas_derrame_prueba(escrito = 0, llamadas = 10)
  despues <- .estadisticas_derrame_prueba(escrito = 7, llamadas = 12)
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "PqConnection",
    .estadisticas_derrame_postgresql_dbi = function(conexion) {
      lectura <<- lectura + 1L
      if (lectura == 1L) antes else despues
    },
    .package = "lupa"
  )
  conexion <- structure(list(), class = "PqConnection")
  presupuesto <- lupa:::.presupuesto_dbi(Inf, instrumentar = TRUE)
  lupa:::.iniciar_instrumentacion_derrame_dbi(
    conexion, presupuesto, exacto = TRUE
  )
  lupa:::.finalizar_instrumentacion_derrame_dbi(conexion, presupuesto)
  publicado <- lupa:::.publicar_derrame_dbi(presupuesto)

  expect_false(publicado$disponible)
  expect_identical(publicado$estado, "no_disponible")
  expect_identical(publicado$consultas_observadas, 0L)
  expect_match(publicado$motivo, "llamada exacta")
})
