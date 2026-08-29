skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

library(DBI)

test_that("cero CPU medido no se confunde con una medicion ausente", {
  expect_identical(lupa:::.duracion_cpu_ms_dbi(1, 1), 0)
  expect_true(is.na(lupa:::.duracion_cpu_ms_dbi(NULL, 1)))
})

.ronda154_datos <- function() {
  data.frame(
    id = 1:12,
    grupo = rep(c("a", "b"), 6L),
    valor = rep(c(1, 2), 6L),
    stringsAsFactors = FALSE
  )
}

.ronda154_conexion <- function() {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(conexion, "tabla154", .ronda154_datos())
  conexion
}

.ronda154_argumentos <- function(instrumentar = FALSE,
                                 politica_costo = "todas",
                                 umbral_cardinalidad = 0.95) {
  list(
    modo = "exacto", muestra = Inf, bloque_muestra = "solo_agregados",
    instrumentar = instrumentar, politica_costo = politica_costo,
    umbral_cardinalidad = umbral_cardinalidad,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE, ausencia_estructural = FALSE,
    duplicados_aproximados = FALSE,
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
}

.ronda154_sin_instrumentacion <- function(x) {
  campos <- c(
    "sql", "consulta_id", "etapa", "nivel", "duracion_ms", "cpu_ms",
    "n_filas_resultado", "bytes_resultado_r", "derrame",
    "bloques_temporales_leidos", "bloques_temporales_escritos",
    "fuente_derrame"
  )
  x$resumen_tabla$sql <- x$resumen_tabla$sql[
    , setdiff(names(x$resumen_tabla$sql), campos), drop = FALSE
  ]
  x$resumen_tabla$tiempos <- NULL
  x$resumen_tabla$meta$instrumentacion <- NULL
  x$resumen_tabla$meta$derrame <- NULL
  x$resumen_tabla$meta$costo_distintos <- NULL
  x
}

test_that("CPU queda medido por consulta y por etapa, y se puede apagar", {
  conexion <- .ronda154_conexion()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  argumentos <- .ronda154_argumentos()
  argumentos <- modifyList(argumentos, list(bloque_muestra = "con_muestra"))

  medido <- do.call(
    perfilar_dbi, c(list(conexion, "tabla154", metricas = "validos"),
                    modifyList(argumentos, list(instrumentar = TRUE)))
  )
  sql_medido <- medido$resumen_tabla$sql
  emitidas <- sql_medido[!is.na(sql_medido$consulta_id), , drop = FALSE]
  tiempos_medido <- medido$resumen_tabla$tiempos
  etapas_medido <- tiempos_medido[tiempos_medido$estado == "medido", , drop = FALSE]

  expect_true(nrow(emitidas) > 0L)
  expect_true(all(!is.na(emitidas$cpu_ms)))
  expect_true(all(emitidas$cpu_ms >= 0))
  expect_true(nrow(etapas_medido) > 0L)
  expect_true(all(!is.na(etapas_medido$cpu_ms)))
  expect_true(all(etapas_medido$cpu_ms >= 0))
  expect_identical(
    medido$resumen_tabla$meta$instrumentacion$cpu,
    "proc.time()[['user.self']] + proc.time()[['sys.self']]"
  )

  sin_medir <- do.call(
    perfilar_dbi, c(list(conexion, "tabla154", metricas = "validos"), argumentos)
  )
  expect_true(all(is.na(sin_medir$resumen_tabla$sql$cpu_ms)))
  expect_true(all(is.na(sin_medir$resumen_tabla$tiempos$cpu_ms)))
})

test_that("pedir todas conserva el objeto completo al quitar mediciones", {
  conexion <- .ronda154_conexion()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  argumentos <- .ronda154_argumentos(politica_costo = "todas")
  medido <- do.call(
    perfilar_dbi, c(list(conexion, "tabla154"),
                    modifyList(argumentos, list(instrumentar = TRUE)))
  )
  sin_medir <- do.call(
    perfilar_dbi, c(list(conexion, "tabla154"), argumentos)
  )

  expect_identical(
    .ronda154_sin_instrumentacion(medido),
    .ronda154_sin_instrumentacion(sin_medir)
  )
})

test_that("la politica de cardinalidad declara omisiones y ahorra consultas", {
  conexion <- .ronda154_conexion()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  argumentos <- .ronda154_argumentos()
  todas <- do.call(
    perfilar_dbi, c(list(conexion, "tabla154"), argumentos)
  )
  selectiva <- do.call(
    perfilar_dbi, c(
      list(conexion, "tabla154"),
      modifyList(argumentos, list(politica_costo = "por_cardinalidad"))
    )
  )

  expect_equal(
    todas$resumen_tabla$meta$consultas$emitidas -
      selectiva$resumen_tabla$meta$consultas$emitidas,
    2
  )
  expect_true(is.na(selectiva$resumen_tabla$columnas$moda[1L]))
  expect_true(is.na(selectiva$resumen_tabla$columnas$mediana[1L]))
  expect_equal(selectiva$resumen_tabla$columnas$moda[2L], "a")
  expect_equal(selectiva$resumen_tabla$columnas$mediana[3L], 1.5)

  omitidas <- selectiva$resumen_tabla$sql[
    selectiva$resumen_tabla$sql$columna == "id" &
      selectiva$resumen_tabla$sql$estado == "omitido_por_costo", , drop = FALSE
  ]
  expect_setequal(omitidas$metrica, c("moda", "frecuencia_moda", "mediana"))
  expect_true(all(!is.na(omitidas$motivo)))
  expect_true(all(grepl("politica_costo", omitidas$motivo, fixed = TRUE)))
  expect_true(all(is.na(omitidas$sql)))
  expect_true(all(is.na(omitidas$consulta_id)))
  expect_true(all(is.na(omitidas$cpu_ms)))

  decisiones <- selectiva$resumen_tabla$meta$decisiones_costo
  expect_false(decisiones$id$moda)
  expect_false(decisiones$id$mediana)
  expect_true(decisiones$grupo$moda)
  expect_equal(decisiones$id$detalle$proporcion_distintos, 1)

  plan <- plan_perfilado_dbi(
    conexion, "tabla154", modo = "exacto",
    bloque_muestra = "solo_agregados", instrumentar = FALSE,
    politica_costo = "por_cardinalidad"
  )
  expect_lt(attr(plan, "total"),
            selectiva$resumen_tabla$meta$consultas$emitidas)
  expect_lte(selectiva$resumen_tabla$meta$consultas$emitidas,
             attr(plan, "total_maximo"))
  fila_moda <- plan$clase_consulta == "moda (GROUP BY + orden + limite)"
  fila_mediana <- plan$clase_consulta == "mediana (orden total + limite/salto)"
  expect_equal(plan$n_consultas[fila_moda], 0)
  expect_equal(plan$n_consultas_max[fila_moda], 3)
  expect_equal(plan$n_consultas[fila_mediana], 0)
  expect_equal(plan$n_consultas_max[fila_mediana], 2)
  expect_true(all(c("validos", "distintos") %in%
                  attr(plan, "metricas_ejecucion")))
})

test_that("el umbral es una politica explicita y se puede mover", {
  conexion <- .ronda154_conexion()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  argumentos <- .ronda154_argumentos(
    politica_costo = "por_cardinalidad", umbral_cardinalidad = 0.1
  )
  resultado <- do.call(
    perfilar_dbi, c(list(conexion, "tabla154"), argumentos)
  )
  estados_id <- resultado$resumen_tabla$sql$estado[
    resultado$resumen_tabla$sql$columna == "id" &
      resultado$resumen_tabla$sql$metrica %in% c("moda", "mediana")
  ]
  estados_grupo <- resultado$resumen_tabla$sql$estado[
    resultado$resumen_tabla$sql$columna == "grupo" &
      resultado$resumen_tabla$sql$metrica == "moda"
  ]
  expect_true(all(estados_id == "omitido_por_costo"))
  expect_true(all(estados_grupo == "omitido_por_costo"))
  expect_match(
    resultado$resumen_tabla$meta$decisiones_costo$grupo$detalle$motivo,
    "umbral_cardinalidad"
  )
})

test_that("la capacidad consolidada usa un SELECT de PERCENTILE_CONT y SQLite cae atras", {
  if (!methods::isClass("ConexionRonda154Postgres")) {
    methods::setClass(
      "ConexionRonda154Postgres", contains = "SQLiteConnection"
    )
  }
  methods::setMethod(
    "dbGetInfo", "ConexionRonda154Postgres",
    function(dbObj, ...) list(dbms.name = "PostgreSQL")
  )
  methods::setMethod(
      "dbSendQuery", c("ConexionRonda154Postgres", "character"),
      function(conn, statement, ...) {
        # SQLite no implementa el percentil, pero puede ejecutar el mismo
        # contrato si el agregado se reemplaza por AVG para esta prueba.
        statement <- gsub(
          "PERCENTILE_CONT\\(0.5\\) WITHIN GROUP \\(ORDER BY ([^\\)]+)\\)",
          "AVG(\\1)", statement, perl = TRUE
        )
        callNextMethod(conn, statement, ...)
      }
  )
  cruda <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(cruda), add = TRUE)
  conexion <- methods::new("ConexionRonda154Postgres")
  for (ranura in methods::slotNames(cruda)) {
    methods::slot(conexion, ranura) <- methods::slot(cruda, ranura)
  }
  candidatos <- lupa:::.candidatos_mediana_consolidada_dbi(conexion)
  expect_length(candidatos, 1L)
  candidato <- candidatos[[1L]]
  sql <- candidato$construir_multiple(
    c(
      candidato$expresion('"a"', '"mediana_a"'),
      candidato$expresion('"b"', '"mediana_b"')
    ),
    '"tabla154"'
  )
  expect_equal(length(strsplit(sql, "PERCENTILE_CONT", fixed = TRUE)[[1L]]) - 1L, 2L)

  sqlite <- .ronda154_conexion()
  on.exit(DBI::dbDisconnect(sqlite), add = TRUE)
  expect_length(lupa:::.candidatos_mediana_consolidada_dbi(sqlite), 0L)

  DBI::dbWriteTable(conexion, "tabla154", .ronda154_datos())
  resultado <- perfilar_dbi(
    conexion, "tabla154", modo = "exacto", metricas = "mediana",
    bloque_muestra = "solo_agregados", instrumentar = FALSE,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE, ausencia_estructural = FALSE,
    duplicados_aproximados = FALSE
  )
  filas <- resultado$resumen_tabla$sql[
    resultado$resumen_tabla$sql$etapa == "medianas_consolidadas", ,
    drop = FALSE
  ]
  expect_equal(length(unique(filas$consulta_id)), 1L)
  expect_equal(resultado$resumen_tabla$columnas$mediana[c(1L, 3L)], c(6.5, 1.5))
  expect_equal(length(strsplit(filas$sql[[1L]], "PERCENTILE_CONT", fixed = TRUE)[[1L]]) - 1L, 2L)
})
