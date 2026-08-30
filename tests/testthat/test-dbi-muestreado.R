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
  metricas_sin_distintos <- metricas[
    !metricas$metrica %in% c("n_distintos", "tasa_distintos"), , drop = FALSE
  ]
  expect_true(all(
    metricas_sin_distintos$metodo == "random_limit" |
      metricas_sin_distintos$metodo == "tablesample_system" |
      (metricas_sin_distintos$metrica %in% c("moda", "frecuencia_moda") &
         metricas_sin_distintos$metodo == "ventana_agregado") |
      (metricas_sin_distintos$metrica == "mediana" &
         metricas_sin_distintos$metodo == "subconsulta_escalar")
  ))
  expect_true(all(metricas$estado %in% c("estimado", "observado_muestra", "no_aplica")))
  distintos <- metricas[metricas$metrica == "n_distintos", , drop = FALSE]
  expect_true(all(distintos$estado == "observado_muestra"))
  expect_true(all(grepl("cardinalidad observada", distintos$motivo, fixed = TRUE)))
  expect_true(all(distintos$error_esperado == "no_aplica"))
  expect_true(all(distintos$metodo == "COUNT(DISTINCT)"))
  expect_true(all(distintos$estrategia_solicitada == "exacta"))
  expect_true(all(distintos$estrategia_resuelta == "COUNT(DISTINCT)"))
  expect_true(all(distintos$estado_estrategia == "calculado"))
  no_estimados <- metricas[
    metricas$columna %in% c("id", "monto") &
    metricas$metrica %in% c(
      "n_validos", "n_faltantes", "prop_faltantes", "minimo", "maximo",
      "media", "n_ceros", "n_negativos", "desvio"
    ), , drop = FALSE
  ]
  expect_true(all(no_estimados$error_esperado == "no_estimado"))
  expect_true(all(grepl("no se calculo", no_estimados$motivo, fixed = TRUE)))
  no_estimables <- metricas[
    metricas$columna %in% c("id", "monto") &
    metricas$metrica %in% c("moda", "frecuencia_moda", "mediana"),
    , drop = FALSE
  ]
  expect_true(all(no_estimables$error_esperado == "no_estimable"))
  expect_true(all(grepl("cota simple", no_estimables$motivo, fixed = TRUE)))
  expect_identical(resultado$resumen_tabla$meta$alcance, "tabla_muestreada")
})

test_that("el metadato publico distingue pedido y obtenido", {
  con <- .conexion_capacidad_dbi("ConexionCapacidadLupa")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .reiniciar_capacidad_dbi("normal")

  resultado <- .perfil_liviano_dbi_muestreado(con, "muestreado")
  muestreo <- resultado$resumen_tabla$meta$muestreo

  expect_equal(muestreo$tamano_muestra, 5)
  expect_equal(muestreo$filas_solicitadas, 5)
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
    con, "datos_corta", muestra = 5L, modo = "muestreado",
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
  expect_true(all(grepl("muestra vacia", alcance$motivo, fixed = TRUE)))
  expect_equal(resultado$resumen_tabla$meta$muestreo$filas_obtenidas, 0)
  expect_true(any(grepl(
    "consulta de muestra devolvio 0 filas",
    resultado$resumen_tabla$cobertura$motivo,
    fixed = TRUE
  )))
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
    con, "datos_corta", muestra = 5L, modo = "muestreado",
    proteger_datos_personales = FALSE
  )
  registros <- resultado$resumen_tabla$sql
  alcance <- registros[
    registros$columna == "valor" & registros$metrica != "n", , drop = FALSE
  ]

  expect_equal(resultado$perfil_muestra$meta$origen_dbi$muestreo$filas_obtenidas, 4)
  expect_equal(resultado$resumen_tabla$columnas$n_validos, 0)
  expect_equal(resultado$resumen_tabla$columnas$n_faltantes, 20)
  expect_true(all(
    alcance$estado[alcance$metrica %in% c("n_validos", "n_faltantes")] ==
      "estimado"
  ))
  expect_false(any(alcance$estado == "no_disponible"))
})

test_that("el metadato publico conserva una muestra menor que el pedido", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "datos", data.frame(valor = seq_len(75L)))

  resultado <- perfilar_dbi(
    con, "datos", muestra = 100L, modo = "muestreado",
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
    con, "datos_corta", muestra = 5L, modo = "muestreado",
    orden_muestra = "valor", proteger_datos_personales = FALSE
  )
  muestreo <- resultado$perfil_muestra$meta$origen_dbi$muestreo
  cobertura <- resultado$resumen_tabla$cobertura
  discrepancia <- cobertura[cobertura$estado == "alcance_distinto", , drop = FALSE]

  expect_equal(muestreo$filas_obtenidas, 4)
  expect_equal(resultado$resumen_tabla$meta$muestreo$filas_obtenidas, 4)
  expect_false(muestreo$coincide_con_lo_pedido)
  expect_equal(nrow(discrepancia), 1L)
  expect_match(discrepancia$motivo, "devolvio 4 filas", fixed = TRUE)
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

test_that("el plan acota las consultas en los cinco modos", {
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
    emitidas <- length(.capacidad_dbi_prueba$sql)
    expect_lte(attr(plan, "total"), emitidas, label = paste("modo", modo))
    expect_lte(
      emitidas, attr(plan, "total_maximo"), label = paste("modo", modo)
    )
    expect_equal(
      resultado$resumen_tabla$meta$consultas$emitidas, emitidas,
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
