test_that("la proyeccion de distintos usa el primer lote de esta corrida", {
  presupuesto <- lupa:::.presupuesto_dbi(Inf, instrumentar = TRUE)
  presupuesto$referencias_distintos <- list(
    primera = list(duracion_ms = 15000),
    segunda = list(duracion_ms = 17000)
  )

  proyeccion <- lupa:::.proyectar_costo_distintos_dbi(presupuesto, 2L)

  expect_true(proyeccion$disponible)
  expect_identical(proyeccion$duracion_referencia_ms, 16000)
  expect_identical(proyeccion$duracion_estimada_ms, 32000)
  expect_identical(proyeccion$n_referencias, 2L)
  expect_match(proyeccion$fuente, "primer lote de distintos")
  expect_false(grepl("agregados planos", proyeccion$fuente, fixed = TRUE))
  expect_match(proyeccion$motivo, "estimacion")

  presupuesto$referencias_distintos <- list()
  sin_referencia <- lupa:::.proyectar_costo_distintos_dbi(presupuesto, 2L)
  expect_false(sin_referencia$disponible)
  expect_true(is.na(sin_referencia$duracion_estimada_ms))
  expect_match(sin_referencia$motivo, "primer lote")
})

test_that("un solo lote no se presenta como una proyeccion", {
  presupuesto <- lupa:::.presupuesto_dbi(Inf, instrumentar = TRUE)
  presupuesto$referencias_distintos <- list(
    primera = list(duracion_ms = 15000)
  )

  proyeccion <- lupa:::.proyectar_costo_distintos_dbi(presupuesto, 1L)

  expect_false(proyeccion$disponible)
  expect_true(is.na(proyeccion$duracion_estimada_ms))
  expect_match(proyeccion$motivo, "un solo lote")
})

test_that("el aviso de costo incluye el valor y su fuente", {
  proyeccion <- list(
    disponible = TRUE, duracion_estimada_ms = 30000, n_lotes = 2L,
    fuente = "mediana de 1 consulta del primer lote de distintos medida en esta corrida"
  )

  expect_message(
    lupa:::.avisar_costo_distintos_dbi(proyeccion),
    "Costo estimado.*30,0 s.*Fuente:.*primer lote de distintos"
  )
})

test_that("el aviso de costo tiene interruptor y umbral en segundos", {
  proyeccion <- list(
    disponible = TRUE, duracion_estimada_ms = 30000, n_lotes = 2L,
    fuente = "mediana de consultas planas"
  )

  expect_message(
    lupa:::.avisar_costo_distintos_dbi(
      proyeccion, habilitado = TRUE, umbral_segundos = 0
    ),
    "Costo estimado"
  )
  expect_silent(lupa:::.avisar_costo_distintos_dbi(
    proyeccion, habilitado = FALSE, umbral_segundos = 0
  ))
  expect_silent(lupa:::.avisar_costo_distintos_dbi(
    proyeccion, habilitado = TRUE, umbral_segundos = Inf
  ))
  expect_silent(lupa:::.avisar_costo_distintos_dbi(
    proyeccion, habilitado = TRUE, umbral_segundos = 30.1
  ))
  expect_error(
    lupa:::.avisar_costo_distintos_dbi(
      proyeccion, umbral_segundos = NA_real_
    ),
    "umbral_segundos"
  )
})

test_that("perfilar_dbi rechaza umbrales NA antes de iniciar la corrida", {
  expect_error(
    perfilar_dbi(
      NULL, "tabla", umbral_segundos_aviso_distintos = NA_real_
    ),
    "umbral_segundos_aviso_distintos"
  )
  expect_error(
    perfilar_dbi(
      NULL, "tabla", umbral_bytes_aviso_derrame_estimado = NA_real_
    ),
    "umbral_bytes_aviso_derrame_estimado"
  )
})

test_that("el aviso llega entre el primer y el segundo lote de distintos", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "tabla_aviso", data.frame(
    codigo = c("a", "b", "a"), valor = c(1, 2, 3), tercero = c(3, 4, 5),
    stringsAsFactors = FALSE
  ))

  eventos <- character()
  numero_distintos <- 0L
  original_distintos <- lupa:::.conteos_distintos_lote_dbi
  testthat::local_mocked_bindings(
    .avisar_costo_distintos_dbi = function(
        proyeccion, habilitado, umbral_segundos) {
      eventos <<- c(eventos, "aviso")
      expect_true(proyeccion$disponible)
      expect_identical(proyeccion$duracion_estimada_ms, 60000)
      expect_match(proyeccion$fuente, "primer lote de distintos")
      expect_false(habilitado)
      expect_identical(umbral_segundos, 0)
    },
    .estimar_derrame_postgresql_dbi = function(...) {
      list(
        estado = "estimado", disponible = TRUE, es_estimacion = TRUE,
        supera_memoria = TRUE, lotes_sobre_memoria = 1L,
        lotes = data.frame(
          lote = 1L, columnas = "codigo", n_distintos_estimados = 1000,
          tamano_estimado_bytes = 10000000, supera_memoria = TRUE,
          stringsAsFactors = FALSE
        ), work_mem = "1MB", memoria_efectiva = "2MB",
        fuente = "pg_stats", motivo = "estimacion"
      )
    },
    .avisar_derrame_estimado_postgresql_dbi = function(
        estimacion, habilitado, umbral_bytes) {
      eventos <<- c(eventos, "aviso_memoria")
      expect_false(habilitado)
      expect_identical(umbral_bytes, 0)
    },
    .conteos_distintos_lote_dbi = function(...) {
      numero_distintos <<- numero_distintos + 1L
      eventos <<- c(eventos, paste0("distintos_", numero_distintos))
      resultado <- original_distintos(...)
      for (campo in names(resultado$resultados)) {
        resultado$resultados[[campo]]$distintos$duracion_ms <- 30000
      }
      resultado
    },
    .package = "lupa"
  )

  resultado <- perfilar_dbi(
    conexion, "tabla_aviso", metricas = c("validos", "distintos"),
    bloque_muestra = "solo_agregados", instrumentar = TRUE,
    avisar_costo_distintos = FALSE, umbral_segundos_aviso_distintos = 0,
    avisar_derrame_estimado = FALSE, umbral_bytes_aviso_derrame_estimado = 0,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE, ausencia_estructural = FALSE,
    duplicados_aproximados = FALSE
  )

  expect_lt(match("aviso_memoria", eventos), match("distintos_1", eventos))
  expect_lt(match("distintos_1", eventos), match("aviso", eventos))
  expect_lt(match("aviso", eventos), match("distintos_2", eventos))
  expect_true(is.list(resultado$resumen_tabla$meta$costo_distintos))
  expect_true(is.list(resultado$resumen_tabla$meta$derrame))
  expect_true(is.list(resultado$resumen_tabla$meta$estimacion_derrame))
  expect_identical(resultado$resumen_tabla$columnas$n_distintos, c(2, 3, 3))
})

test_that("metricas solo distintos tambien avisa despues del primer lote", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "solo_distintos", data.frame(
    a = c("a", "b", "a"), b = c("b", "c", "b"), c = c("c", "d", "c"),
    stringsAsFactors = FALSE
  ))

  original_distintos <- lupa:::.conteos_distintos_lote_dbi
  numero_distintos <- 0L
  testthat::local_mocked_bindings(
    .conteos_distintos_lote_dbi = function(...) {
      numero_distintos <<- numero_distintos + 1L
      resultado <- original_distintos(...)
      for (campo in names(resultado$resultados)) {
        resultado$resultados[[campo]]$distintos$duracion_ms <- 30000
      }
      resultado
    },
    .package = "lupa"
  )

  expect_message(
    resultado <- perfilar_dbi(
      conexion, "solo_distintos", metricas = "distintos",
      tamano_lote_distintos = 2L, umbral_segundos_aviso_distintos = 0,
      bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE,
      analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
      ausencia_estructural = FALSE, duplicados_aproximados = FALSE
    ),
    "Costo estimado"
  )

  expect_identical(numero_distintos, 2L)
  proyeccion <- resultado$resumen_tabla$meta$costo_distintos
  expect_true(proyeccion$disponible)
  expect_identical(proyeccion$duracion_estimada_ms, 60000)
  expect_match(proyeccion$fuente, "primer lote de distintos")
  sql <- resultado$resumen_tabla$sql
  expect_false(any(sql$estado != "no_solicitado" & sql$metrica %in%
    c("n_validos", "n_faltantes", "prop_faltantes")))
})

test_that("un perfil con un solo lote declara que no proyecta", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "un_lote", data.frame(
    a = c("a", "b", "a"), b = c("b", "c", "b"), stringsAsFactors = FALSE
  ))

  resultado <- expect_silent(perfilar_dbi(
    conexion, "un_lote", metricas = "distintos",
    tamano_lote_distintos = 2L, umbral_segundos_aviso_distintos = 0,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE, duplicados_aproximados = FALSE
  ))

  proyeccion <- resultado$resumen_tabla$meta$costo_distintos
  expect_false(proyeccion$disponible)
  expect_match(proyeccion$motivo, "un solo lote")
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

test_that("las llamadas concurrentes se publican como agregado de la ventana", {
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

  expect_true(publicado$disponible)
  expect_identical(publicado$estado, "medido")
  expect_identical(publicado$consultas_observadas, 1L)
  expect_identical(publicado$llamadas_en_ventana, 2)
  expect_match(publicado$motivo, "agregado")
})
