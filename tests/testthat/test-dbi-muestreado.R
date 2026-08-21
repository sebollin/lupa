skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

library(DBI)

.capacidad_dbi_prueba <- new.env(parent = emptyenv())
.capacidad_dbi_prueba$modo <- "normal"
.capacidad_dbi_prueba$sql <- character()
.capacidad_dbi_prueba$dentro <- FALSE

if (!methods::isClass("ConexionCapacidadLupa")) {
  setClass("ConexionCapacidadLupa", contains = "SQLiteConnection")
}
if (!methods::isClass("ConexionAproximadaLupa")) {
  setClass("ConexionAproximadaLupa", contains = "SQLiteConnection")
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

.perfil_liviano_dbi_muestreado <- function(con, modo, ...) {
  do.call(
    perfilar_dbi,
    c(
      list(
        conexion = con, tabla = "datos", muestra = 5L, modo = modo,
        proteger_datos_personales = FALSE
      ),
      list(...)
    )
  )
}

test_that("muestreado usa una forma declarada por el motor y marca cada estimacion", {
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
  expect_true(all(metricas$metodo == "random_limit" | metricas$metodo == "tablesample_system"))
  expect_true(all(metricas$estado %in% c("estimado", "observado_muestra", "no_aplica")))
  distintos <- metricas[metricas$metrica == "n_distintos", , drop = FALSE]
  expect_true(all(distintos$estado == "observado_muestra"))
  expect_true(all(is.na(distintos$motivo)))
  expect_identical(resultado$resumen_tabla$meta$alcance, "tabla_muestreada")
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
    resultado$resumen_tabla$cobertura$bloque == "resumen_tabla" &
      grepl("muestreo", resultado$resumen_tabla$cobertura$motivo)
  ))
  expect_true(any(grepl("LIMIT", .capacidad_dbi_prueba$sql, fixed = TRUE)))
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
  expect_true(all(metricas$metodo == "random_limit"))
  expect_true(all(metricas$estado == "estimado"))
})

test_that("aproximado usa APPROX_COUNT_DISTINCT cuando la sonda responde", {
  con <- .conexion_capacidad_dbi("ConexionAproximadaLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .reiniciar_capacidad_dbi("aprox_distintos")

  resultado <- .perfil_liviano_dbi_muestreado(
    con, "aproximado", metricas = c("validos", "distintos")
  )
  registros <- resultado$resumen_tabla$sql
  distintos <- registros[registros$metrica == "n_distintos", , drop = FALSE]

  expect_true(any(grepl("APPROX_COUNT_DISTINCT", .capacidad_dbi_prueba$sql)))
  expect_true(all(distintos$estado == "estimado"))
  expect_true(all(distintos$metodo == "APPROX_COUNT_DISTINCT"))
  expect_true(all(distintos$universo == 20))
  expect_true(all(distintos$error_esperado == "desconocido"))
})

test_that("aproximado declara el respaldo COUNT DISTINCT si no hay funcion nativa", {
  con <- .conexion_capacidad_dbi("ConexionCapacidadLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .reiniciar_capacidad_dbi("normal")

  resultado <- .perfil_liviano_dbi_muestreado(
    con, "aproximado", metricas = c("validos", "distintos")
  )
  registros <- resultado$resumen_tabla$sql
  distintos <- registros[registros$metrica == "n_distintos", , drop = FALSE]

  expect_false(any(grepl("APPROX_COUNT_DISTINCT", distintos$sql)))
  expect_true(all(distintos$estado == "calculado"))
  expect_true(all(distintos$metodo == "COUNT(DISTINCT)"))
})

test_that("aproximado usa la funcion nativa de cuantiles cuando responde", {
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

test_that("el plan predice exactamente las consultas en los cinco modos", {
  con <- .conexion_capacidad_dbi("ConexionCapacidadLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  for (modo in c("exacto", "seguro", "conteos", "muestreado", "aproximado")) {
    .reiniciar_capacidad_dbi("normal")
    plan <- lupa:::plan_perfilado_dbi(
      con, "datos", muestra = 5L, orden_muestra = "id", modo = modo
    )
    .reiniciar_capacidad_dbi("normal")
    resultado <- .perfil_liviano_dbi_muestreado(
      con, modo, orden_muestra = "id"
    )
    expect_equal(
      length(.capacidad_dbi_prueba$sql), attr(plan, "total"),
      info = paste("modo", modo)
    )
    expect_equal(
      resultado$resumen_tabla$meta$consultas$emitidas, attr(plan, "total"),
      info = paste("modo", modo)
    )
  }
})

test_that("los conteos textuales conservan integer64 cuando bit64 esta disponible", {
  skip_if_not_installed("bit64")
  valor <- lupa:::.conteo_dbi("9007199254740993")
  expect_s3_class(valor, "integer64")
  expect_identical(as.character(valor), "9007199254740993")
})
