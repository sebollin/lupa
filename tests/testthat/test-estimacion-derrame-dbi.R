# La memoria del hash se estima antes de `COUNT(DISTINCT)`, pero nunca se
# publica como derrame medido. Las pruebas de aqui cubren las respuestas que
# PostgreSQL da cuando faltan estadisticas, permisos o una capacidad nueva.

.conexion_pq_falsa <- function() {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}

.estadisticas_hash_falsas <- function(
    columnas = "x", n_distinct = -1, avg_width = 10, reltuples = 1000) {
  data.frame(
    relacion_oid = "1", schemaname = "public", tablename = "t",
    reltuples = reltuples, es_raiz = TRUE, hoja = TRUE,
    attname = columnas, n_distinct = n_distinct, avg_width = avg_width,
    stringsAsFactors = FALSE
  )
}

.estimar_hash_falso <- function(stats, hash = 2, work = "1MB") {
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "PqConnection",
    .escalar_dbi = function(conexion, sql, campo, presupuesto = NULL,
                            etapa = "consulta") {
      if (grepl("work_mem", sql, fixed = TRUE) &&
          !grepl("hash_mem_multiplier", sql, fixed = TRUE)) {
        return(list(ok = TRUE, valor = work))
      }
      if (is.null(hash)) {
        return(list(ok = FALSE, valor = NULL,
                    motivo = "unrecognized configuration parameter"))
      }
      list(ok = TRUE, valor = hash)
    },
    .consultar_dbi = function(conexion, sql, presupuesto = NULL, filas = -1L,
                              etapa = "consulta") {
      list(ok = TRUE, datos = stats, motivo = NA_character_)
    },
    .package = "lupa"
  )
  conexion <- .conexion_pq_falsa()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  lupa:::.estimar_derrame_postgresql_dbi(
    conexion, "public.t", "x",
    lupa:::.presupuesto_dbi(Inf, instrumentar = FALSE), TRUE, "exacto", 1L
  )
}

test_that("el hash convierte n_distinct negativo con reltuples", {
  estimacion <- .estimar_hash_falso(.estadisticas_hash_falsas())

  expect_identical(estimacion$estado, "estimado")
  expect_identical(estimacion$columnas$n_distintos_estimados, 1000)
  expect_identical(estimacion$columnas$tamano_estimado_bytes, 74000)
  expect_identical(estimacion$hash_mem_multiplier, 2)
  expect_identical(estimacion$memoria_efectiva_bytes, 2 * 1024^2)
  expect_false(isTRUE(estimacion$supera_memoria))
  expect_true(estimacion$es_estimacion)
  expect_match(estimacion$motivo, "estimacion, no una medicion")
  expect_match(estimacion$motivo, "puede quedar corto")
})

test_that("reltuples negativo no se convierte en cero filas", {
  estimacion <- .estimar_hash_falso(
    .estadisticas_hash_falsas(n_distinct = -1, reltuples = -1)
  )

  expect_identical(estimacion$estado, "no_disponible")
  expect_equal(nrow(estimacion$columnas), 0L)
  expect_match(estimacion$motivo, "reltuples = -1")
  expect_false(grepl("dentro del limite", estimacion$motivo, fixed = TRUE))
})

test_that("una pg_stats sin fila no dice que no haya derrame", {
  sin_fila <- .estadisticas_hash_falsas()
  sin_fila$attname <- NA_character_
  sin_fila$n_distinct <- NA_real_
  sin_fila$avg_width <- NA_real_
  estimacion <- .estimar_hash_falso(sin_fila)

  expect_identical(estimacion$estado, "no_disponible")
  expect_true(grepl("No se pudo estimar", estimacion$motivo, fixed = TRUE))
  expect_false(grepl("no derrama", estimacion$motivo, fixed = TRUE))
  expect_false(isTRUE(estimacion$supera_memoria))
})

test_that("las estadisticas de varias hijas se suman para el lote", {
  stats <- rbind(
    .estadisticas_hash_falsas(n_distinct = 1000, avg_width = 8, reltuples = 1000),
    .estadisticas_hash_falsas(n_distinct = 2000, avg_width = 12, reltuples = 2000)
  )
  stats$relacion_oid <- c("10", "11")
  stats$tablename <- c("t_2025", "t_2026")
  stats$es_raiz <- FALSE
  estimacion <- .estimar_hash_falso(stats, work = "64KB")

  expect_identical(estimacion$estado, "estimado")
  expect_identical(estimacion$columnas$n_distintos_estimados, 3000)
  expect_identical(estimacion$columnas$n_relaciones, 2L)
  expect_identical(estimacion$lotes$n_distintos_estimados, 3000)
  expect_true(isTRUE(estimacion$lotes$supera_memoria))
  expect_true(isTRUE(estimacion$supera_memoria))
})

test_that("una particion o columna sin estadistica deja la estimacion parcial", {
  stats <- .estadisticas_hash_falsas()
  stats$relacion_oid <- "10"
  stats$es_raiz <- FALSE
  stats$tablename <- "t_2025"
  stats2 <- stats
  stats2$relacion_oid <- "11"
  stats2$tablename <- "t_2026"
  stats2$attname <- NA_character_
  stats2$n_distinct <- NA_real_
  stats2$avg_width <- NA_real_
  estimacion <- .estimar_hash_falso(rbind(stats, stats2))

  expect_identical(estimacion$estado, "no_disponible")
  expect_match(estimacion$motivo, "No se pudo estimar")
  expect_match(estimacion$motivo, "todas las relaciones")
})

test_that("la version sin hash_mem_multiplier usa work_mem sin romperse", {
  estimacion <- .estimar_hash_falso(
    .estadisticas_hash_falsas(n_distinct = 100), hash = NULL, work = "1MB"
  )

  expect_identical(estimacion$estado, "estimado")
  expect_identical(estimacion$hash_mem_multiplier, 1)
  expect_false(estimacion$hash_mem_multiplier_disponible)
  expect_identical(estimacion$memoria_efectiva_bytes, 1024^2)
  expect_match(estimacion$fuente, "pg_class.reltuples")
  expect_false(grepl("hash_mem_multiplier", estimacion$fuente, fixed = TRUE))
})

test_that("un motor que no es PostgreSQL queda sin estimacion", {
  llamada <- FALSE
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "SQLiteConnection",
    .escalar_dbi = function(...) {
      llamada <<- TRUE
      stop("no deberia consultar SHOW", call. = FALSE)
    },
    .package = "lupa"
  )
  estimacion <- lupa:::.estimar_derrame_postgresql_dbi(
    structure(list(), class = "SQLiteConnection"), "t", "x",
    lupa:::.presupuesto_dbi(Inf), TRUE, "exacto", 1L
  )

  expect_identical(estimacion$estado, "no_disponible")
  expect_match(estimacion$motivo, "no fue reconocida como PostgreSQL")
  expect_false(llamada)
})

test_that("el aviso identifica estimacion, memoria vigente y accion de sesion", {
  estimacion <- .estimar_hash_falso(
    .estadisticas_hash_falsas(n_distinct = 100000), work = "1MB"
  )

  expect_message(
    lupa:::.avisar_derrame_estimado_postgresql_dbi(estimacion),
    "estimacion.*no una medicion.*1MB.*sesion.*pg_stat_statements"
  )
})

test_that("SQLite publica que no pudo estimar, no que no derrama", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "sin_pg_stats", data.frame(x = 1:10))

  resultado <- perfilar_dbi(
    con, "sin_pg_stats", metricas = c("validos", "distintos"),
    bloque_muestra = "solo_agregados", instrumentar = FALSE,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE, ausencia_estructural = FALSE,
    duplicados_aproximados = FALSE
  )
  estimacion <- resultado$resumen_tabla$meta$estimacion_derrame

  expect_identical(estimacion$estado, "no_disponible")
  expect_match(estimacion$motivo, "no fue reconocida como PostgreSQL")
  expect_false(grepl("no derrama", estimacion$motivo, fixed = TRUE))
  expect_false(isTRUE(resultado$resumen_tabla$meta$derrame$disponible))
})

test_that("la medicion posterior prevalece sobre una estimacion optimista", {
  perfil <- list(
    resumen_tabla = list(
      meta = list(
        tabla = "tabla",
        alcance = "tabla_completa",
        filas = 100L,
        clave = NULL,
        consultas = list(emitidas = 1L, dialecto = list(nombre = "PostgreSQL")),
        estimacion_derrame = list(
          estado = "estimado", supera_memoria = FALSE,
          memoria_efectiva = "8,0 MB",
          fuente = "pg_stats; SHOW work_mem",
          es_estimacion = TRUE
        ),
        derrame = list(
          estado = "medido", disponible = TRUE,
          consultas_con_derrame = 1L,
          bloques_temporales_leidos = 2L,
          bloques_temporales_escritos = 3L
        )
      ),
      columnas = data.frame(),
      sql = data.frame(estado = "medido", stringsAsFactors = FALSE),
      cobertura = data.frame()
    ),
    perfil_muestra = NULL
  )
  class(perfil) <- "perfil_dbi"

  salida <- c(
    capture.output(print(perfil)),
    capture.output(print(perfil), type = "message")
  )
  texto <- paste(salida, collapse = " ")
  expect_match(texto, "Derrame estimado \\(no medido\\)")
  expect_match(texto, "Derrame medido")
  expect_lt(
    regexpr("Derrame estimado \\(no medido\\)", texto),
    regexpr("Derrame medido", texto)
  )
})
