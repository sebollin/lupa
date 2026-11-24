# El plan de `plan_perfilado_dbi()` cuenta las consultas del resumen de fechas
# y de los periodos que la corrida emite, y la corrida las concilia con las
# demas en `resumen_tabla$meta$consultas`:
#
# - lo que no depende de los datos va en el total, y la corrida emite eso;
# - lo que los datos activan -la reserva de una fecha centinela, la grilla de
#   una columna con fechas futuras, la lectura de un texto que la muestra lee
#   como fecha- va entre el total y el maximo, y la conciliacion lo declara
#   condicional sin desfasarse;
# - `meta$plan` trae las mismas filas de fechas que el plan.
#
# Se recorre la grilla entera de `periodos`, `resumen_fechas`, `aplicabilidad`,
# `bloque_filas` y `estrategia_no_sql`, en SQLite y en DuckDB. Una muestra de la
# grilla corre en los motores de servidor en
# `test-fechas-plan-consultas-motores.R` y en
# `test-local-fechas-plan-consultas-mysql.R`. Las tablas y las comprobaciones
# compartidas estan en `helper-fechas-plan-consultas.R`.

.fpc_saltar <- function(motor) {
  skip_on_cran()
  skip_if_not_installed("DBI")
  if (identical(motor, "sqlite")) skip_if_not_installed("RSQLite")
  if (identical(motor, "duckdb")) skip_if_not_installed("duckdb")
}

.fpc_conectar <- function(motor, ...) {
  if (identical(motor, "sqlite")) {
    return(DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE))
  }
  DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:", ...)
}

.fpc_cerrar <- function(con, motor) {
  if (identical(motor, "duckdb")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
}

.fpc_grilla <- function(motor) {
  con <- .fpc_conectar(motor)
  on.exit(.fpc_cerrar(con, motor), add = TRUE)
  tablas <- list(lupa_plan_limpia = .fpc_limpia(), lupa_plan_sucia = .fpc_sucia())
  recorridas <- 0L
  condicionales <- 0L
  for (tabla in names(tablas)) {
    DBI::dbWriteTable(con, tabla, tablas[[tabla]])
    for (hay_periodo in c(FALSE, TRUE)) for (hay_resumen in c(FALSE, TRUE)) {
      for (regla in names(.fpc_reglas)) for (bloque in list(NULL, 2L)) {
        for (estrategia in c("no_medible", "bloques")) {
          argumentos <- list(metricas = "basicos", estrategia_no_sql = estrategia)
          if (hay_periodo) {
            argumentos$periodos <- c(inicio = "lupa_desde", fin = "lupa_hasta")
          }
          if (hay_resumen) argumentos$resumen_fechas <- TRUE
          if (!is.null(.fpc_reglas[[regla]])) {
            argumentos$aplicabilidad <- .fpc_reglas[[regla]]
          }
          if (!is.null(bloque)) argumentos$bloque_filas <- bloque
          par <- .fpc_par(con, tabla, argumentos)
          caso <- paste0(
            motor, ", ", tabla, ", periodos ", hay_periodo, ", resumen ", hay_resumen,
            ", regla ", regla, ", bloque_filas ", if (is.null(bloque)) "sin" else bloque,
            ", ", estrategia
          )
          expect_identical(
            .fpc_desacuerdos(par, exacto = identical(tabla, "lupa_plan_limpia")),
            NULL, label = caso
          )
          recorridas <- recorridas + 1L
          if (par$corrida$resumen_tabla$meta$consultas$emitidas > attr(par$plan, "total")) {
            condicionales <- condicionales + 1L
          }
        }
      }
    }
  }
  # La grilla entera, y la tabla sucia activo de verdad consultas condicionales:
  # si no, la mitad de la grilla no prueba nada.
  expect_identical(recorridas, 128L)
  expect_gt(condicionales, 10L)
}

test_that("SQLite: el plan cuenta las consultas de fechas que la corrida emite, en toda la grilla", {
  .fpc_saltar("sqlite")
  .fpc_grilla("sqlite")
})

test_that("DuckDB: el plan cuenta las consultas de fechas que la corrida emite, en toda la grilla", {
  .fpc_saltar("duckdb")
  .fpc_grilla("duckdb")
})

# ---- Cada parte del plan, en una tabla donde se sabe que consulta se emite ----

test_that("el maximo del resumen de fechas es el peor caso: la reserva y la grilla de cada columna", {
  .fpc_saltar("sqlite")
  con <- .fpc_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Dos columnas, cada una con una centinela repetida y con fechas futuras.
  DBI::dbWriteTable(con, "lupa_plan_peor", data.frame(
    lupa_id = 1:40,
    lupa_a = c(as.Date("2020-01-01") + 0:29, rep(as.Date("9999-12-31"), 5),
               as.Date("2030-01-01") + (0:4) * 7),
    lupa_b = c(as.Date("2021-01-01") + 0:29, rep(as.Date("1900-01-01"), 5),
               as.Date("2031-01-01") + (0:4) * 7)
  ))
  for (estrategia in c("no_medible", "bloques")) {
    par <- .fpc_par(con, "lupa_plan_peor", list(
      metricas = "basicos", resumen_fechas = TRUE, estrategia_no_sql = estrategia
    ))
    expect_identical(.fpc_desacuerdos(par, exacto = FALSE), NULL, label = estrategia)
    resumen <- .fpc_clase(par, "resumen")
    expect_identical(resumen$seguras, 1, label = estrategia)
    expect_identical(resumen$maximas, 4, label = estrategia)
    expect_identical(resumen$emitidas, 4, label = estrategia)
    expect_identical(resumen$direccion, "condicional", label = estrategia)
  }
})

test_that("la lectura de un texto que la muestra lee como fecha es condicional, y solo con bloques", {
  .fpc_saltar("sqlite")
  con <- .fpc_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_plan_texto", data.frame(
    lupa_id = 1:8, lupa_nota = format(as.Date("2021-01-01") + 0:7),
    stringsAsFactors = FALSE
  ))
  con_bloques <- .fpc_par(con, "lupa_plan_texto", list(
    metricas = "basicos", resumen_fechas = TRUE, estrategia_no_sql = "bloques"
  ))
  expect_identical(.fpc_desacuerdos(con_bloques, exacto = FALSE), NULL)
  resumen <- .fpc_clase(con_bloques, "resumen")
  expect_identical(c(resumen$seguras, resumen$maximas, resumen$emitidas), c(0, 1, 1))
  sin_bloques <- .fpc_par(con, "lupa_plan_texto", list(
    metricas = "basicos", resumen_fechas = TRUE
  ))
  expect_identical(.fpc_desacuerdos(sin_bloques, exacto = TRUE), NULL)
  resumen <- .fpc_clase(sin_bloques, "resumen")
  expect_identical(c(resumen$seguras, resumen$maximas, resumen$emitidas), c(0, 0, 0))
})

test_that("SQLite: una fecha guardada como texto manda a leer la columna y el periodo, dentro del maximo", {
  .fpc_saltar("sqlite")
  con <- .fpc_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE lupa_plan_guardado",
    "(lupa_id INTEGER, lupa_desde DATE, lupa_hasta DATE)"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_plan_guardado VALUES (1, 18262, 18300), (2, 18293, 18280),",
    "(3, 18322, '2020-06-01'), (4, 18350, 18400)"
  ))
  argumentos <- list(
    metricas = "basicos", resumen_fechas = TRUE,
    periodos = c(inicio = "lupa_desde", fin = "lupa_hasta")
  )
  con_bloques <- .fpc_par(con, "lupa_plan_guardado",
                          c(argumentos, list(estrategia_no_sql = "bloques")))
  expect_identical(.fpc_desacuerdos(con_bloques, exacto = FALSE), NULL)
  periodos <- .fpc_clase(con_bloques, "periodos")
  expect_identical(c(periodos$seguras, periodos$maximas, periodos$emitidas), c(1, 2, 2))
  resumen <- .fpc_clase(con_bloques, "resumen")
  expect_identical(c(resumen$seguras, resumen$emitidas), c(1, 2))
  # Sin bloques se declara, y no se lee nada de mas.
  sin_bloques <- .fpc_par(con, "lupa_plan_guardado", argumentos)
  expect_identical(.fpc_desacuerdos(sin_bloques, exacto = TRUE), NULL)
  periodos <- .fpc_clase(sin_bloques, "periodos")
  expect_identical(c(periodos$seguras, periodos$maximas, periodos$emitidas), c(1, 1, 1))
})

test_that("DuckDB: con el huso forzado y bloques las lecturas son seguras, y el plan las cuenta", {
  .fpc_saltar("duckdb")
  con <- .fpc_conectar(
    "duckdb", timezone_out = "America/Montevideo", tz_out_convert = "force"
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE lupa_plan_forzada",
    "(lupa_id INTEGER, lupa_desde TIMESTAMP, lupa_hasta TIMESTAMP)"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_plan_forzada VALUES",
    "(1, '2020-01-01 00:00:00', '2020-06-01 00:00:00'),",
    "(2, '2020-02-01 00:00:00', '2020-01-15 00:00:00'),",
    "(3, '2020-03-01 00:00:00', NULL)"
  ))
  argumentos <- list(
    metricas = "basicos", resumen_fechas = TRUE,
    periodos = c(inicio = "lupa_desde", fin = "lupa_hasta")
  )
  con_bloques <- .fpc_par(con, "lupa_plan_forzada",
                          c(argumentos, list(estrategia_no_sql = "bloques")))
  expect_identical(.fpc_desacuerdos(con_bloques, exacto = TRUE), NULL)
  resumen <- .fpc_clase(con_bloques, "resumen")
  expect_identical(c(resumen$seguras, resumen$maximas, resumen$emitidas), c(2, 2, 2))
  periodos <- .fpc_clase(con_bloques, "periodos")
  expect_identical(c(periodos$seguras, periodos$maximas, periodos$emitidas), c(1, 1, 1))
  sin_bloques <- .fpc_par(con, "lupa_plan_forzada", argumentos)
  expect_identical(.fpc_desacuerdos(sin_bloques, exacto = TRUE), NULL)
  expect_identical(.fpc_clase(sin_bloques, "resumen")$maximas, 0)
  expect_identical(.fpc_clase(sin_bloques, "periodos")$maximas, 0)
})

test_that("DuckDB: un infinito entre tipos distintos manda a leer el periodo, dentro del maximo", {
  .fpc_saltar("duckdb")
  con <- .fpc_conectar("duckdb")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE lupa_plan_infinito",
    "(lupa_id INTEGER, lupa_desde DATE, lupa_hasta TIMESTAMP)"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_plan_infinito VALUES (1, '2020-01-01', '2020-06-01'),",
    "(2, 'infinity', 'infinity'), (3, '2020-03-01', '2020-02-01')"
  ))
  argumentos <- list(
    metricas = "basicos", periodos = c(inicio = "lupa_desde", fin = "lupa_hasta"),
    estrategia_no_sql = "bloques"
  )
  con_infinito <- .fpc_par(con, "lupa_plan_infinito", argumentos)
  expect_identical(.fpc_desacuerdos(con_infinito, exacto = FALSE), NULL)
  periodos <- .fpc_clase(con_infinito, "periodos")
  expect_identical(c(periodos$seguras, periodos$maximas, periodos$emitidas), c(1, 2, 2))
  DBI::dbExecute(con, "DELETE FROM lupa_plan_infinito WHERE lupa_id = 2")
  sin_infinito <- .fpc_par(con, "lupa_plan_infinito", argumentos)
  expect_identical(.fpc_desacuerdos(sin_infinito, exacto = TRUE), NULL)
  periodos <- .fpc_clase(sin_infinito, "periodos")
  expect_identical(c(periodos$seguras, periodos$maximas, periodos$emitidas), c(1, 2, 1))
})

test_that("con muestra_motor las fechas no consultan y el plan no las anuncia", {
  .fpc_saltar("sqlite")
  con <- .fpc_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_plan_muestra", .fpc_sucia())
  par <- .fpc_par(con, "lupa_plan_muestra", list(
    metricas = "basicos", resumen_fechas = TRUE,
    periodos = c(inicio = "lupa_desde", fin = "lupa_hasta"),
    universo = "muestra_motor", muestra_motor = 4, estrategia_no_sql = "bloques"
  ))
  expect_identical(.fpc_desacuerdos(par, exacto = TRUE), NULL)
  expect_identical(nrow(.fpc_filas_fechas(par$plan)), 0L)
})

# El control de la conciliacion: tiene que acusar a unas fechas que emiten lo
# que el plan no anuncia. Se le da a la corrida un plan de fechas que anuncia
# de menos, y la conciliacion tiene que quedar desfasada y decir por que clase.
test_that("si las fechas emiten lo que el plan no anuncia, la conciliacion se desfasa", {
  .fpc_saltar("sqlite")
  con <- .fpc_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_plan_desfase", .fpc_limpia())
  argumentos <- list(
    con, "lupa_plan_desfase", fecha = .fpc_fecha, metricas = "basicos",
    resumen_fechas = TRUE, periodos = c(inicio = "lupa_desde", fin = "lupa_hasta")
  )
  sana <- suppressWarnings(do.call(perfilar_dbi, argumentos))
  expect_identical(sana$resumen_tabla$meta$consultas$conciliacion$estado, "conciliado")
  original <- .filas_plan_fechas_dbi
  testthat::local_mocked_bindings(
    .filas_plan_fechas_dbi = function(...) {
      filas <- original(...)
      filas$n_consultas <- rep(0, nrow(filas))
      filas$n_consultas_max <- rep(0, nrow(filas))
      filas
    },
    .package = "lupa"
  )
  corrida <- suppressWarnings(do.call(perfilar_dbi, argumentos))
  consultas <- corrida$resumen_tabla$meta$consultas
  expect_identical(consultas$conciliacion$estado, "desfasado")
  expect_identical(consultas$conciliacion$emitidas, consultas$emitidas)
  detalle <- consultas$conciliacion$detalle
  detalle <- detalle[detalle$clase_consulta %in% .fpc_clases, , drop = FALSE]
  expect_identical(nrow(detalle), 2L)
  expect_true(all(detalle$direccion == "no_anunciada"))
  expect_identical(detalle$n_emitidas, c(1, 1))
})

# Una capacidad que el motor rechaza al prepararse queda anotada entre las
# fallas de la corrida: PostgreSQL 9.3 no tiene la columna que consulta la de
# collation, y SQL Server no acepta la de la fuente por bloques. No tiene que
# ver con una lectura de fechas que activaron los datos, y la conciliacion no
# la toma por una via de respaldo; tampoco a una consulta de otra etapa que
# fallo. Una consulta de fechas que fallo si cuenta. Aca se anota cada falla en
# una corrida de SQLite; los motores de verdad estan en
# `test-fechas-plan-consultas-motores.R`.
.fpc_con_falla <- function(con, tabla, lista, etapa) {
  original <- .conciliar_fechas_dbi
  testthat::local_mocked_bindings(
    .conciliar_fechas_dbi = function(meta, filas_plan, emitidas, preparacion) {
      preparacion$presupuesto[[lista]] <- c(
        preparacion$presupuesto[[lista]],
        list(list(etapa = etapa, motivo = "lupa: falla anotada por la prueba",
                  tipo = "rechazada"))
      )
      original(meta, filas_plan, emitidas, preparacion)
    },
    .package = "lupa"
  )
  .fpc_par(con, tabla, list(
    metricas = "basicos", resumen_fechas = TRUE, estrategia_no_sql = "bloques"
  ))
}

test_that("una falla ajena a las fechas no desfasa una consulta de fechas que activaron los datos", {
  .fpc_saltar("sqlite")
  con <- .fpc_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_plan_rechazo", .fpc_sucia())
  ajenas <- list(
    capacidad = c("fallas_sondas", "sonda_collation_dbi"),
    otra_etapa = c("fallas_consultas", "basicos")
  )
  for (caso in names(ajenas)) {
    par <- .fpc_con_falla(con, "lupa_plan_rechazo", ajenas[[caso]][[1L]], ajenas[[caso]][[2L]])
    expect_identical(.fpc_desacuerdos(par, exacto = FALSE), NULL, label = caso)
    resumen <- .fpc_clase(par, "resumen")
    expect_gt(resumen$emitidas, resumen$seguras, label = caso)
    expect_identical(resumen$direccion, "condicional", label = caso)
  }
})

test_that("una consulta de fechas que fallo si desfasa la consulta de fechas de mas", {
  .fpc_saltar("sqlite")
  con <- .fpc_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_plan_falla", .fpc_sucia())
  par <- .fpc_con_falla(con, "lupa_plan_falla", "fallas_consultas", "fechas")
  conciliacion <- par$corrida$resumen_tabla$meta$consultas$conciliacion
  expect_identical(conciliacion$estado, "desfasado")
  resumen <- .fpc_clase(par, "resumen")
  expect_gt(resumen$emitidas, resumen$seguras)
  expect_identical(resumen$direccion, "no_anunciada")
})

test_that("una falla de fechas o de periodos es de su clase, no de los portones", {
  clases <- c(
    "portones (esquema y sondas)", "agregados planos",
    .fpc_clases[["resumen"]], .fpc_clases[["periodos"]]
  )
  expect_identical(.clase_consulta_por_etapa_dbi("fechas", clases),
                   .fpc_clases[["resumen"]])
  expect_identical(.clase_consulta_por_etapa_dbi("periodos", clases),
                   .fpc_clases[["periodos"]])
  # Sin su clase en el plan, se cobra donde se cobra todo lo que no tiene clase.
  sin_fechas <- clases[1:2]
  expect_identical(.clase_consulta_por_etapa_dbi("fechas", sin_fechas),
                   "portones (esquema y sondas)")
  expect_identical(.clase_consulta_por_etapa_dbi("periodos", sin_fechas),
                   "portones (esquema y sondas)")
})
