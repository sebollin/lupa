skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

library(DBI)

.capacidad_dbi_prueba <- new.env(parent = emptyenv())
.capacidad_dbi_prueba$modo <- "normal"
.capacidad_dbi_prueba$sql <- character()
.capacidad_dbi_prueba$dentro <- FALSE
.capacidad_dbi_prueba$muestra_vacia <- FALSE

if (!methods::isClass("ConexionCapacidadLupa")) {
  setClass("ConexionCapacidadLupa", contains = "SQLiteConnection")
}
if (!methods::isClass("ConexionAproximadaLupa")) {
  setClass("ConexionAproximadaLupa", contains = "SQLiteConnection")
}
if (!methods::isClass("ConexionMuestraCortaLupa")) {
  setClass("ConexionMuestraCortaLupa", contains = "SQLiteConnection")
}

.envolver_conexion_capacidad <- function(con, clase) {
  salida <- methods::new(clase)
  for (ranura in methods::slotNames(con)) {
    methods::slot(salida, ranura) <- methods::slot(con, ranura)
  }
  salida
}

.reiniciar_capacidad_dbi <- function(modo) {
  .capacidad_dbi_prueba$modo <- modo
  .capacidad_dbi_prueba$sql <- character()
  .capacidad_dbi_prueba$dentro <- FALSE
  invisible(NULL)
}

.traducir_capacidad_dbi <- function(statement) {
  salida <- statement
  salida <- gsub(
    " TABLESAMPLE SYSTEM \\(.*?\\)", "", salida, perl = TRUE,
    ignore.case = TRUE
  )
  salida <- gsub(
    " TABLESAMPLE \\(.*? PERCENT\\)", "", salida, perl = TRUE,
    ignore.case = TRUE
  )
  if (identical(.capacidad_dbi_prueba$modo, "aprox_distintos")) {
    salida <- gsub(
      "APPROX_COUNT_DISTINCT\\(([^)]*)\\)", "COUNT(DISTINCT \\1)",
      salida, perl = TRUE, ignore.case = TRUE
    )
  }
  if (identical(.capacidad_dbi_prueba$modo, "aprox_quantile")) {
    salida <- gsub(
      "approx_quantile\\(([^,]+),[[:space:]]*0.5\\)", "AVG(\\1)",
      salida, perl = TRUE, ignore.case = TRUE
    )
  }
  salida
}

.rechazar_capacidad_dbi <- function(statement) {
  modo <- .capacidad_dbi_prueba$modo
  if (identical(modo, "sin_muestreo") &&
      grepl("TABLESAMPLE|RANDOM\\(\\)|RAND\\(\\)|NEWID", statement,
            ignore.case = TRUE, perl = TRUE)) {
    stop("El motor no ofrece una forma de muestreo.", call. = FALSE)
  }
  if (identical(modo, "sin_tablesample") &&
      grepl("TABLESAMPLE", statement, ignore.case = TRUE)) {
    stop("TABLESAMPLE no disponible.", call. = FALSE)
  }
  # Reproduce lo que hace DuckDB: con un filtro trivialmente falso el parser no
  # llega a validar el metodo de muestreo, asi que una sonda que use
  # `WHERE 1 = 0` pasa y la consulta real falla. Un motor asi convierte una
  # sonda descuidada en una promesa falsa.
  if (identical(modo, "tablesample_mentiroso") &&
      grepl("TABLESAMPLE SYSTEM", statement, ignore.case = TRUE) &&
      !grepl("WHERE 1 = 0", statement, fixed = TRUE)) {
    stop(
      "Sample method System cannot be used with a discrete sample count.",
      call. = FALSE
    )
  }
  if (!identical(modo, "aprox_distintos") &&
      !identical(modo, "aprox_quantile") &&
      grepl("APPROX_COUNT_DISTINCT|approx_count_distinct", statement,
            ignore.case = TRUE)) {
    stop("Funcion aproximada no disponible.", call. = FALSE)
  }
  if (!identical(modo, "aprox_quantile") &&
      grepl("approx_quantile", statement, ignore.case = TRUE)) {
    stop("Cuantil aproximado no disponible.", call. = FALSE)
  }
  if (identical(modo, "aprox_quantile") &&
      grepl("lupa_mediana_sonda", statement, fixed = TRUE)) {
    stop("Mediana exacta por subconsulta no disponible.", call. = FALSE)
  }
  invisible(NULL)
}

setMethod(
  "dbGetQuery", c("ConexionCapacidadLupa", "character"),
  function(conn, statement, ...) {
    if (!.capacidad_dbi_prueba$dentro) {
      .capacidad_dbi_prueba$sql <- c(.capacidad_dbi_prueba$sql, statement)
      .rechazar_capacidad_dbi(statement)
    }
    previo <- .capacidad_dbi_prueba$dentro
    .capacidad_dbi_prueba$dentro <- TRUE
    on.exit(.capacidad_dbi_prueba$dentro <- previo, add = TRUE)
    callNextMethod(conn, .traducir_capacidad_dbi(statement), ...)
  }
)
setMethod(
  "dbSendQuery", c("ConexionCapacidadLupa", "character"),
  function(conn, statement, ...) {
    if (!.capacidad_dbi_prueba$dentro) {
      .capacidad_dbi_prueba$sql <- c(.capacidad_dbi_prueba$sql, statement)
      .rechazar_capacidad_dbi(statement)
    }
    previo <- .capacidad_dbi_prueba$dentro
    .capacidad_dbi_prueba$dentro <- TRUE
    on.exit(.capacidad_dbi_prueba$dentro <- previo, add = TRUE)
    callNextMethod(conn, .traducir_capacidad_dbi(statement), ...)
  }
)
setMethod(
  "dbGetQuery", c("ConexionAproximadaLupa", "character"),
  function(conn, statement, ...) {
    if (!.capacidad_dbi_prueba$dentro) {
      .capacidad_dbi_prueba$sql <- c(.capacidad_dbi_prueba$sql, statement)
      .rechazar_capacidad_dbi(statement)
    }
    previo <- .capacidad_dbi_prueba$dentro
    .capacidad_dbi_prueba$dentro <- TRUE
    on.exit(.capacidad_dbi_prueba$dentro <- previo, add = TRUE)
    callNextMethod(conn, .traducir_capacidad_dbi(statement), ...)
  }
)
setMethod(
  "dbSendQuery", c("ConexionAproximadaLupa", "character"),
  function(conn, statement, ...) {
    if (!.capacidad_dbi_prueba$dentro) {
      .capacidad_dbi_prueba$sql <- c(.capacidad_dbi_prueba$sql, statement)
      .rechazar_capacidad_dbi(statement)
    }
    previo <- .capacidad_dbi_prueba$dentro
    .capacidad_dbi_prueba$dentro <- TRUE
    on.exit(.capacidad_dbi_prueba$dentro <- previo, add = TRUE)
    callNextMethod(conn, .traducir_capacidad_dbi(statement), ...)
  }
)
setMethod(
  "dbGetQuery", c("ConexionMuestraCortaLupa", "character"),
  function(conn, statement, ...) {
    es_muestra <- grepl(
      "FROM `datos_corta` ORDER BY RANDOM\\(\\) LIMIT 5$", statement
    )
    es_consulta_muestra <- grepl(
      "ORDER BY RANDOM\\(\\) LIMIT 5", statement, ignore.case = TRUE
    )
    statement_ejecutable <- if (
      isTRUE(.capacidad_dbi_prueba$muestra_vacia) && es_consulta_muestra
    ) {
      gsub(
        " ORDER BY RANDOM\\(\\) LIMIT 5", " WHERE 1 = 0", statement,
        ignore.case = TRUE, perl = TRUE
      )
    } else {
      statement
    }
    salida <- callNextMethod(conn, statement_ejecutable, ...)
    if (!isTRUE(.capacidad_dbi_prueba$muestra_vacia) &&
        es_muestra && nrow(salida) > 0L) {
      salida <- salida[seq_len(nrow(salida) - 1L), , drop = FALSE]
    }
    salida
  }
)
setMethod(
  "dbSendQuery", c("ConexionMuestraCortaLupa", "character"),
  function(conn, statement, ...) {
    es_consulta_muestra <- grepl(
      "ORDER BY RANDOM\\(\\) LIMIT 5", statement, ignore.case = TRUE
    )
    statement_ejecutable <- if (
      isTRUE(.capacidad_dbi_prueba$muestra_vacia) && es_consulta_muestra
    ) {
      gsub(
        " ORDER BY RANDOM\\(\\) LIMIT 5", " WHERE 1 = 0", statement,
        ignore.case = TRUE, perl = TRUE
      )
    } else if (es_consulta_muestra) {
      sub("LIMIT 5$", "LIMIT 4", statement, ignore.case = TRUE)
    } else statement
    callNextMethod(conn, statement_ejecutable, ...)
  }
)

.conexion_capacidad_dbi <- function(clase, tabla = "datos") {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, tabla, data.frame(
    id = 1:20,
    monto = c(rep(10, 10), 20:29),
    texto = rep(c("a", "b"), 10),
    stringsAsFactors = FALSE
  ))
  .envolver_conexion_capacidad(con, clase)
}

.perfil_liviano_dbi_muestreado <- function(con, caso, ...) {
  argumentos <- modifyList(
    list(muestra = 5L, estrategia_mediana = "exacta",
         proteger_datos_personales = FALSE),
    .argumentos_caso_dbi(caso, muestra = 5L)
  )
  do.call(
    perfilar_dbi,
    c(
      list(conexion = con, tabla = "datos"), argumentos,
      list(...)
    )
  )
}

test_that("muestreado materializa una seleccion y marca su cobertura", {
  con <- .conexion_capacidad_dbi("ConexionCapacidadLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .reiniciar_capacidad_dbi("normal")

  resultado <- .perfil_liviano_dbi_muestreado(con, "muestreado")
  registros <- resultado$resumen_tabla$sql
  metricas <- registros[registros$metrica != "n", , drop = FALSE]

  expect_true(any(grepl("TABLESAMPLE", .capacidad_dbi_prueba$sql)))
  expect_true(all(metricas$alcance == "muestra"))
  expect_true(all(metricas$universo == 20))
  expect_true(all(metricas$tamano_muestra == 5))
  expect_true(all(metricas$fraccion == 0.25))
  expect_true(all(metricas$metodo == "spool_sesion_cliente"))
  expect_true(all(metricas$estado == "observado_muestra"))
  expect_true(all(metricas$error_esperado == "no_estimable"))
  expect_true(all(metricas$id_consulta == metricas$consulta_id))
  materializacion <- resultado$resumen_tabla$meta$materializacion
  expect_true(isTRUE(materializacion$externo))
  expect_true(isTRUE(materializacion$validado_relectura))
  expect_identical(materializacion$estado, "cerrada_verificada")
  expect_identical(materializacion$muestra_id,
                   resultado$resumen_tabla$meta$alcance$muestra_id)
  expect_identical(resultado$resumen_tabla$meta$alcance$universo_id,
                   "muestra_motor")
  expect_identical(resultado$resumen_tabla$meta$alcance_texto,
                   "tabla_muestreada")
  expect_false(any(names(registros) == "muestra_id"))
})

test_that("el metadato publico distingue pedido y obtenido", {
  con <- .conexion_capacidad_dbi("ConexionCapacidadLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .reiniciar_capacidad_dbi("normal")

  resultado <- .perfil_liviano_dbi_muestreado(con, "muestreado")
  muestreo <- resultado$resumen_tabla$meta$muestreo

  expect_equal(muestreo$tamano_muestra, 5)
  expect_equal(muestreo$filas_solicitadas, 5)
  expect_equal(muestreo$filas_pedidas, 5)
  expect_equal(muestreo$filas_obtenidas, 5)
  expect_true(muestreo$metodo %in% c("random_limit", "tablesample_system"))
  expect_equal(muestreo$fraccion, 0.25)
})

test_that("una muestra vacia no publica ceros ni estados de muestra observada", {
  cruda <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(
    cruda, "datos_corta",
    data.frame(valor = seq_len(20L))
  )
  con <- .envolver_conexion_capacidad(cruda, "ConexionMuestraCortaLupa")
  on.exit({
    .capacidad_dbi_prueba$muestra_vacia <- FALSE
    DBI::dbDisconnect(con)
  }, add = TRUE)
  .capacidad_dbi_prueba$muestra_vacia <- TRUE

  resultado <- perfilar_dbi(
    con, "datos_corta", universo = "muestra_motor", muestra_motor = 5L,
    muestra = 5L, estrategia_mediana = "exacta",
    proteger_datos_personales = FALSE
  )
  columnas <- resultado$resumen_tabla$columnas
  registros <- resultado$resumen_tabla$sql
  alcance <- registros[registros$metrica != "n", , drop = FALSE]

  expect_equal(columnas$n, 20)
  campos_sin_n <- setdiff(names(columnas), c("columna", "n"))
  expect_true(all(is.na(columnas[campos_sin_n])))
  expect_true(all(alcance$estado == "no_disponible"))
  expect_true(all(is.na(alcance$motivo) == FALSE))
  expect_true(all(
    alcance$motivo == "muestra_vacia:tablesample_system_sin_filas"
  ))
  expect_equal(resultado$resumen_tabla$meta$muestreo$filas_obtenidas, 0)
  expect_true(any(
    resultado$resumen_tabla$cobertura$bloque == "perfil_muestra" &
      resultado$resumen_tabla$cobertura$estado == "no_disponible"
  ))
})

test_that("una muestra no vacia con todos los valores nulos conserva sus estados", {
  cruda <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(
    cruda, "datos_corta",
    data.frame(valor = rep(NA_integer_, 20L))
  )
  con <- .envolver_conexion_capacidad(cruda, "ConexionMuestraCortaLupa")
  on.exit({
    .capacidad_dbi_prueba$muestra_vacia <- FALSE
    DBI::dbDisconnect(con)
  }, add = TRUE)

  resultado <- perfilar_dbi(
    con, "datos_corta", universo = "muestra_motor", muestra_motor = 5L,
    muestra = 5L, estrategia_mediana = "exacta",
    proteger_datos_personales = FALSE
  )
  registros <- resultado$resumen_tabla$sql
  alcance <- registros[
    registros$columna == "valor" & registros$metrica != "n", , drop = FALSE
  ]

  expect_equal(resultado$perfil_muestra$meta$origen_dbi$muestreo$filas_obtenidas, 4)
  expect_equal(resultado$resumen_tabla$columnas$n_validos, 0)
  expect_equal(resultado$resumen_tabla$columnas$n_faltantes, 4)
  expect_true(all(
    alcance$estado[alcance$metrica %in% c("n_validos", "n_faltantes")] ==
      "observado_muestra"
  ))
  expect_true(all(alcance$estado == "observado_muestra"))
  expect_true(isTRUE(resultado$resumen_tabla$meta$materializacion$validado_relectura))
})

test_that("el metadato publico conserva una muestra menor que el pedido", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "datos", data.frame(valor = seq_len(75L)))

  resultado <- perfilar_dbi(
    con, "datos", universo = "muestra_motor", muestra_motor = 100L,
    muestra = 100L, estrategia_mediana = "exacta",
    orden_muestra = "valor", proteger_datos_personales = FALSE
  )
  muestreo <- resultado$resumen_tabla$meta$muestreo

  expect_equal(muestreo$tamano_muestra, 75)
  expect_equal(muestreo$filas_solicitadas, 100)
  expect_equal(muestreo$filas_obtenidas, 75)
  expect_equal(muestreo$universo, 75)
})

test_that("la discrepancia de filas conserva su cobertura", {
  cruda <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(cruda, "datos_corta", data.frame(valor = seq_len(20L)))
  con <- .envolver_conexion_capacidad(cruda, "ConexionMuestraCortaLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  resultado <- perfilar_dbi(
    con, "datos_corta", universo = "muestra_motor", muestra_motor = 5L,
    muestra = 5L, estrategia_mediana = "exacta",
    orden_muestra = "valor", proteger_datos_personales = FALSE
  )
  muestreo <- resultado$perfil_muestra$meta$origen_dbi$muestreo
  cobertura <- resultado$resumen_tabla$cobertura
  discrepancia <- cobertura[cobertura$estado == "alcance_distinto", , drop = FALSE]

  expect_equal(muestreo$filas_obtenidas, 4)
  expect_equal(resultado$resumen_tabla$meta$muestreo$filas_obtenidas, 4)
  expect_false("coincide_con_lo_pedido" %in% names(muestreo))
  expect_equal(resultado$resumen_tabla$meta$materializacion$n_filas, 4)
  expect_true(isTRUE(resultado$resumen_tabla$meta$materializacion$validado_relectura))
})

test_that("sin muestreo el error esperado no aplica", {
  con <- .conexion_capacidad_dbi("ConexionCapacidadLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .reiniciar_capacidad_dbi("normal")

  resultado <- .perfil_liviano_dbi_muestreado(
    con, "exacto", metricas = c("validos", "moda", "mediana")
  )
  registros <- resultado$resumen_tabla$sql
  expect_true(all(registros$error_esperado == "no_aplica"))
})

test_that("sin capacidad de muestreo se degrada con diagnostico y no usa la tabla completa", {
  con <- .conexion_capacidad_dbi("ConexionCapacidadLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .reiniciar_capacidad_dbi("sin_muestreo")

  resultado <- .perfil_liviano_dbi_muestreado(con, "muestreado")
  registros <- resultado$resumen_tabla$sql
  metricas <- registros[registros$metrica != "n", , drop = FALSE]

  expect_true(all(metricas$estado %in% c("no_disponible", "no_aplica")))
  expect_true(all(is.na(resultado$resumen_tabla$columnas$n_validos)))
  expect_true(any(
    resultado$resumen_tabla$cobertura$bloque == "perfil_muestra" &
      resultado$resumen_tabla$cobertura$estado == "no_disponible"
  ))
  expect_true(isTRUE(resultado$resumen_tabla$meta$materializacion$externo))
  expect_false(isTRUE(resultado$resumen_tabla$meta$materializacion$validado_relectura))
})

test_that("sin TABLESAMPLE usa limite sobre un orden pseudoaleatorio", {
  con <- .conexion_capacidad_dbi("ConexionCapacidadLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .reiniciar_capacidad_dbi("sin_tablesample")

  resultado <- .perfil_liviano_dbi_muestreado(con, "muestreado")
  registros <- resultado$resumen_tabla$sql
  metricas <- registros[
    registros$metrica == "media" & registros$columna %in% c("id", "monto"),
    , drop = FALSE
  ]

  expect_true(any(grepl("ORDER BY RANDOM", .capacidad_dbi_prueba$sql)))
  expect_true(all(metricas$metodo == "spool_sesion_cliente"))
  expect_true(all(metricas$estado == "observado_muestra"))
})

test_that("aproximada_motor usa APPROX_COUNT_DISTINCT cuando la sonda responde", {
  con <- .conexion_capacidad_dbi("ConexionAproximadaLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .reiniciar_capacidad_dbi("aprox_distintos")

  resultado <- .perfil_liviano_dbi_muestreado(
    con, "exacto", metricas = c("validos", "distintos"),
    estrategia_distintos = "aproximada_motor"
  )
  registros <- resultado$resumen_tabla$sql
  distintos <- registros[registros$metrica == "n_distintos", , drop = FALSE]

  expect_true(any(grepl("APPROX_COUNT_DISTINCT", .capacidad_dbi_prueba$sql)))
  expect_true(all(distintos$estado == "estimado_motor"))
  expect_true(all(distintos$metodo == "APPROX_COUNT_DISTINCT"))
  expect_true(all(grepl("APPROX_COUNT_DISTINCT", distintos$sql)))
  expect_false(any(grepl("COUNT(DISTINCT", distintos$sql, fixed = TRUE)))
  expect_true(all(distintos$universo == 20))
  expect_true(all(distintos$error_esperado == "desconocido"))
  expect_true(all(distintos$estrategia_solicitada == "aproximada_motor"))
  expect_true(all(distintos$estrategia_resuelta == "APPROX_COUNT_DISTINCT"))
  expect_true(all(distintos$estado_estrategia == "estimado_motor"))
})

test_that("el plan declara la aproximada_motor sin escanear la tabla", {
  con <- .conexion_capacidad_dbi("ConexionAproximadaLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .reiniciar_capacidad_dbi("aprox_distintos")

  plan <- lupa:::plan_perfilado_dbi(
    con, "datos", bloque_muestra = "solo_agregados",
    metricas = c("validos", "distintos"),
    estrategia_distintos = "aproximada_motor"
  )
  estrategia <- attr(plan, "estrategia_distintos", exact = TRUE)
  expect_identical(estrategia$estrategia_solicitada, "aproximada_motor")
  expect_identical(estrategia$estrategia_resuelta, "APPROX_COUNT_DISTINCT")
  expect_identical(estrategia$estado, "estimado_motor")
  expect_true(any(grepl("APPROX_COUNT_DISTINCT", .capacidad_dbi_prueba$sql)))
  datos <- grepl("FROM `datos`", .capacidad_dbi_prueba$sql, fixed = TRUE) &
    !grepl("WHERE[[:space:]]+1[[:space:]]*=[[:space:]]*0",
           .capacidad_dbi_prueba$sql, ignore.case = TRUE) &
    !grepl("LIMIT[[:space:]]+0[[:space:]]*$", .capacidad_dbi_prueba$sql,
           ignore.case = TRUE)
  expect_false(any(datos))
  expect_false(any(grepl("COUNT(DISTINCT", .capacidad_dbi_prueba$sql,
                         fixed = TRUE)))
})

test_that("aproximada_motor no repliega a exacto si falta la funcion nativa", {
  con <- .conexion_capacidad_dbi("ConexionCapacidadLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .reiniciar_capacidad_dbi("normal")

  resultado <- .perfil_liviano_dbi_muestreado(
    con, "exacto", metricas = c("validos", "distintos"),
    estrategia_distintos = "aproximada_motor"
  )
  registros <- resultado$resumen_tabla$sql
  distintos <- registros[registros$metrica == "n_distintos", , drop = FALSE]

  expect_false(any(grepl("APPROX_COUNT_DISTINCT", distintos$sql)))
  expect_false(any(grepl("COUNT(DISTINCT", distintos$sql, fixed = TRUE)))
  expect_true(all(distintos$estado == "no_disponible"))
  expect_true(all(is.na(distintos$sql)))
  expect_true(all(distintos$estrategia_solicitada == "aproximada_motor"))
  expect_true(all(is.na(distintos$estrategia_resuelta)))
  expect_true(all(distintos$estado_estrategia == "no_disponible"))
  expect_identical(
    resultado$resumen_tabla$meta$estrategia_distintos$estado,
    "no_disponible"
  )
})

test_that("aproximada_motor usa la funcion nativa de cuantiles cuando responde", {
  con <- .conexion_capacidad_dbi("ConexionAproximadaLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .reiniciar_capacidad_dbi("aprox_quantile")

  resultado <- .perfil_liviano_dbi_muestreado(
    con, "aproximado", metricas = c("validos", "mediana")
  )
  registros <- resultado$resumen_tabla$sql
  mediana <- registros[
    registros$metrica == "mediana" & registros$columna %in% c("id", "monto"),
    , drop = FALSE
  ]

  expect_true(any(grepl("approx_quantile", .capacidad_dbi_prueba$sql)))
  expect_true(all(mediana$estado == "estimado"))
  expect_true(all(mediana$metodo == "approx_quantile"))
  expect_true(all(mediana$error_esperado == "desconocido"))
})

test_that("el plan acota las consultas en los cinco modos", {
  con <- .conexion_capacidad_dbi("ConexionCapacidadLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  for (caso in c("exacto", "seguro", "conteos", "muestreado", "aproximado")) {
    .reiniciar_capacidad_dbi("normal")
    argumentos <- modifyList(
      list(muestra = 5L, orden_muestra = "id"),
      .argumentos_caso_dbi(caso, muestra = 5L)
    )
    plan <- do.call(lupa:::plan_perfilado_dbi,
                    c(list(con, "datos"), argumentos))
    .reiniciar_capacidad_dbi("normal")
    resultado <- .perfil_liviano_dbi_muestreado(
      con, caso, orden_muestra = "id"
    )
    emitidas <- length(.capacidad_dbi_prueba$sql)
    if (identical(caso, "muestreado")) {
      spool_plan <- attr(plan, "materializacion", exact = TRUE)
      expect_true(is.list(spool_plan))
      expect_identical(spool_plan$seleccion_unica, 1L)
      expect_identical(spool_plan$pasadas$valor, "spool")
      expect_identical(spool_plan$pasadas$indice, "spool")
      expect_identical(spool_plan$pasadas$lsh, "spool")
      expect_gte(emitidas, 1L)
    } else {
      expect_lte(attr(plan, "total"), emitidas, label = paste("caso", caso))
      expect_lte(
        emitidas, attr(plan, "total_maximo"), label = paste("caso", caso)
      )
      expect_equal(
        resultado$resumen_tabla$meta$consultas$emitidas, emitidas,
        info = paste("caso", caso)
      )
    }
  }
})

test_that("los conteos textuales conservan integer64 cuando bit64 esta disponible", {
  skip_if_not_installed("bit64")
  valor <- lupa:::.conteo_dbi("9007199254740993")
  expect_s3_class(valor, "integer64")
  expect_identical(as.character(valor), "9007199254740993")
})


test_that("la sonda de muestreo ejercita la forma que despues se emite", {
  con <- .conexion_capacidad_dbi("ConexionCapacidadLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .reiniciar_capacidad_dbi("tablesample_mentiroso")

  resultado <- .perfil_liviano_dbi_muestreado(con, "muestreado")
  sondas <- grep("TABLESAMPLE", .capacidad_dbi_prueba$sql, value = TRUE)
  expect_true(length(sondas) > 0L)
  # Ninguna sonda de TABLESAMPLE puede llevar el filtro trivialmente falso: es
  # justo lo que le impide al motor validar la clausula.
  expect_false(any(grepl("WHERE 1 = 0", sondas, fixed = TRUE)))

  # Y el resultado tiene que ser util: el motor rechaza `SYSTEM`, la sonda lo
  # detecta, y el perfilado cae en otra forma en vez de publicar metricas que
  # no se pudieron calcular.
  registros <- resultado$resumen_tabla$sql
  metricas <- registros[registros$metrica != "n", , drop = FALSE]
  expect_false(any(metricas$estado == "no_disponible"))
  expect_true(any(metricas$estado %in% c("estimado", "observado_muestra")))
  expect_false(any(grepl("TABLESAMPLE SYSTEM", metricas$sql, ignore.case = TRUE)))
})

test_that("la sonda CTE de ventanas es barata y prueba la construccion impresa", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  candidato <- lupa:::.candidatos_mediana_cte_ventana_dbi(con)[[1L]]
  sql <- unname(candidato$sonda('"mediana"'))

  expect_true(startsWith(sql, "WITH lupa_mediana_datos AS"))
  expect_match(sql, "COUNT(*) OVER ()", fixed = TRUE)
  expect_match(sql, "ROW_NUMBER() OVER", fixed = TRUE)
  expect_match(sql, "VALUES (1.0), (2.0), (3.0), (4.0)", fixed = TRUE)
  expect_match(sql, "AVG\\([^)]* \\* 1\\.0\\)")
  expect_false(grepl("FROM datos", sql, fixed = TRUE))

  control_negativo <- sub(
    "AVG\\(", "FUNCION_INVALIDA_LUPA(", sql, fixed = FALSE
  )
  expect_match(control_negativo, "FUNCION_INVALIDA_LUPA", fixed = TRUE)
  expect_false(grepl("AVG\\([^)]* \\* 1\\.0\\)", control_negativo))
})

test_that("la guarda de costo de NEWID usa el universo y publica la decision", {
  rechazado <- lupa:::.evaluar_guardia_newid_dbi(10000000, 20000, 1L)
  expect_false(rechazado$aceptado)
  expect_identical(
    rechazado$motivo,
    "capacidad_no_aceptada:newid_costo_excede_presupuesto"
  )
  expect_equal(rechazado$proyeccion_newid_ms, 6500)
  expect_equal(rechazado$n_total, 10000000)
  expect_equal(rechazado$umbral_n_total, 100000)

  aceptado <- lupa:::.evaluar_guardia_newid_dbi(100000, 20000, 1L)
  expect_true(aceptado$aceptado)
  expect_identical(
    aceptado$motivo, "sesgo_muestreo:random_limit_newid_por_fila"
  )

  publicado <- lupa:::.publicar_muestreo_dbi(
    list(
      disponible = TRUE, candidato = list(nombre = "random_limit"),
      sondas = character(), motivo = NA_character_
    ),
    forma = list(
      metodo = "random_limit", funcion = "newid", descripcion = "NEWID",
      fraccion = 0.2, filas_solicitadas = 20000, filas_pedidas = 20000,
      sql = "SELECT TOP (20000) ..."
    ),
    n_total = 100000
  )
  expect_identical(publicado$metodo_muestreo, "random_limit")
  expect_identical(publicado$funcion_muestreo, "newid")
  expect_identical(publicado$sesgo_muestreo, "por_fila")
  expect_identical(publicado$motivo, "sesgo_muestreo:random_limit_newid_por_fila")
  expect_identical(publicado$motivo_exito,
                   "sesgo_muestreo:random_limit_newid_por_fila")
})
