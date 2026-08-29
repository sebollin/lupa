skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

library(DBI)

.consolidacion_dbi <- new.env(parent = emptyenv())
.consolidacion_dbi$modo <- "normal"
.consolidacion_dbi$sql <- character()
.consolidacion_dbi$rechazos <- character()

if (!methods::isClass("ConexionConsolidacionLupa")) {
  setClass("ConexionConsolidacionLupa", contains = "SQLiteConnection")
}

.reiniciar_consolidacion_dbi <- function(modo = "normal") {
  .consolidacion_dbi$modo <- modo
  .consolidacion_dbi$sql <- character()
  .consolidacion_dbi$rechazos <- character()
  invisible(NULL)
}

.numero_expresiones_dbi <- function(texto, patron) {
  coincidencias <- gregexpr(patron, texto, ignore.case = TRUE, perl = TRUE)[[1L]]
  if (identical(coincidencias, -1L)) 0L else length(coincidencias)
}

.rechazar_consolidacion_dbi <- function(statement) {
  modo <- .consolidacion_dbi$modo
  if (identical(modo, "muchas_expresiones")) {
    if (.numero_expresiones_dbi(statement, "COUNT\\(") > 2L ||
        .numero_expresiones_dbi(statement, "SUM\\(CASE") > 2L ||
        .numero_expresiones_dbi(statement, "SQRT\\(") > 1L) {
      .consolidacion_dbi$rechazos <- c(
        .consolidacion_dbi$rechazos, "muchas expresiones"
      )
      stop("El motor rechazo demasiadas expresiones en el SELECT.", call. = FALSE)
    }
  }
  if (identical(modo, "tipo_malo") && grepl("mala", statement, fixed = TRUE) &&
      grepl("COUNT\\(|AVG\\(|MIN\\(|MAX\\(|SUM\\(|SQRT\\(|STDEV|STDDEV",
            statement, ignore.case = TRUE, perl = TRUE)) {
    es_lote <- .numero_expresiones_dbi(statement, "COUNT\\(") > 2L ||
      .numero_expresiones_dbi(statement, "SUM\\(CASE") > 2L ||
      .numero_expresiones_dbi(statement, "SQRT\\(") > 1L
    .consolidacion_dbi$rechazos <- c(
      .consolidacion_dbi$rechazos, if (es_lote) "tipo malo lote" else "tipo malo"
    )
    stop("El tipo de `mala` no admite el agregado solicitado.", call. = FALSE)
  }
  invisible(NULL)
}

setMethod(
  "dbSendQuery", c("ConexionConsolidacionLupa", "character"),
  function(conn, statement, ...) {
    .consolidacion_dbi$sql <- c(.consolidacion_dbi$sql, statement)
    .rechazar_consolidacion_dbi(statement)
    callNextMethod(conn, statement, ...)
  }
)

.envolver_consolidacion_dbi <- function(conexion) {
  salida <- methods::new("ConexionConsolidacionLupa")
  for (ranura in methods::slotNames(conexion)) {
    methods::slot(salida, ranura) <- methods::slot(conexion, ranura)
  }
  salida
}

.datos_consolidacion_dbi <- function() {
  data.frame(
    a = c(0, 1:9), b = c(-1, 2:10), c = c(NA, 3:11), d = 4:13,
    stringsAsFactors = FALSE
  )
}

.conexion_consolidacion_dbi <- function(modo = "normal", datos = .datos_consolidacion_dbi()) {
  cruda <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(cruda, "datos", datos)
  .reiniciar_consolidacion_dbi(modo)
  list(cruda = cruda, conexion = .envolver_consolidacion_dbi(cruda))
}

.perfilar_consolidacion_dbi <- function(conexion, ...) {
  perfilar_dbi(
    conexion, "datos", proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE, ...
  )
}

test_that("el plan predice exactamente los cinco modos contando solo dbSendQuery", {
  bases <- .conexion_consolidacion_dbi()
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)
  modos <- c("exacto", "seguro", "conteos", "muestreado", "aproximado")

  for (modo in modos) {
    .reiniciar_consolidacion_dbi("normal")
    plan <- lupa:::plan_perfilado_dbi(
      bases$conexion, "datos", modo = modo, muestra = 5L,
      tamano_lote = 2L
    )
    .reiniciar_consolidacion_dbi("normal")
    resultado <- .perfilar_consolidacion_dbi(
      bases$conexion, modo = modo, muestra = 5L, tamano_lote = 2L
    )
    expect_equal(
      length(.consolidacion_dbi$sql), attr(plan, "total"),
      info = paste("modo", modo)
    )
    expect_equal(
      resultado$resumen_tabla$meta$consultas$emitidas, attr(plan, "total"),
      info = paste("modo", modo)
    )
    expect_true(any(grepl("por lotes", plan$clase_consulta, fixed = TRUE)))
  }
})

test_that("los valores consolidados coinciden con los de lotes unitarios", {
  lote <- .conexion_consolidacion_dbi()
  unitario <- .conexion_consolidacion_dbi()
  on.exit(DBI::dbDisconnect(lote$cruda), add = TRUE)
  on.exit(DBI::dbDisconnect(unitario$cruda), add = TRUE)

  consolidado <- .perfilar_consolidacion_dbi(
    lote$conexion, modo = "exacto", muestra = 5L, orden_muestra = "a",
    tamano_lote = 2L
  )
  sin_compartir <- .perfilar_consolidacion_dbi(
    unitario$conexion, modo = "exacto", muestra = 5L, orden_muestra = "a",
    tamano_lote = 1L
  )
  expect_equal(
    consolidado$resumen_tabla$columnas,
    sin_compartir$resumen_tabla$columnas
  )
  expect_true(any(
    consolidado$resumen_tabla$sql$columnas_compartidas > 1L,
    na.rm = TRUE
  ))
})

test_that("el SQL mantiene una fila por campo y publica el lote compartido", {
  bases <- .conexion_consolidacion_dbi()
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)
  resultado <- .perfilar_consolidacion_dbi(
    bases$conexion, modo = "exacto", muestra = 5L
  )
  registros <- resultado$resumen_tabla$sql
  campos_antiguos <- c(
    "columna", "metrica", "estado", "motivo", "sql", "alcance", "universo",
    "tamano_muestra", "fraccion", "metodo", "error_esperado"
  )
  expect_true(all(campos_antiguos %in% names(registros)))
  expect_true(all(c("lote", "columnas_compartidas") %in% names(registros)))
  expect_equal(nrow(registros), 4L * 15L)
  expect_equal(
    as.vector(table(registros$columna)[c("a", "b", "c", "d")]), rep(15L, 4L)
  )

  compartidas <- registros[
    registros$metrica %in% c("n_validos", "media", "desvio") &
      !is.na(registros$lote), , drop = FALSE
  ]
  expect_true(all(compartidas$columnas_compartidas %in% c(1L, 4L)))
  for (metrica in unique(compartidas$metrica)) {
    for (lote in unique(compartidas$lote[compartidas$metrica == metrica])) {
      filas <- compartidas[compartidas$metrica == metrica &
        compartidas$lote == lote, , drop = FALSE]
      expect_equal(length(unique(filas$sql)), 1L)
    }
  }
})

test_that("un lote rechazado degrada a columnas sin perder las sanas", {
  bases <- .conexion_consolidacion_dbi("muchas_expresiones")
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)
  resultado <- .perfilar_consolidacion_dbi(
    bases$conexion, modo = "exacto", muestra = 5L
  )
  registros <- resultado$resumen_tabla$sql
  medidos <- registros[registros$columna %in% c("a", "b", "c", "d") &
    registros$metrica != "n", , drop = FALSE]
  expect_true(all(medidos$estado == "calculado"))
  expect_true(any(.consolidacion_dbi$rechazos == "muchas expresiones"))
  expect_true(any(registros$columnas_compartidas == 1L, na.rm = TRUE))
  reintentos <- medidos[!is.na(medidos$consulta_id), , drop = FALSE]
  expect_true(nrow(reintentos) > 0L)
  expect_true(all(reintentos$etapa %in% c(
    "conteos", "basicos", "desvio", "moda", "mediana"
  )))
  expect_true(all(reintentos$n_filas_resultado == 1))
})

test_that("un agregado rechazado para una columna conserva las demas del lote", {
  datos <- data.frame(
    a = 1:10, b = 11:20, mala = 21:30, stringsAsFactors = FALSE
  )
  bases <- .conexion_consolidacion_dbi("tipo_malo", datos)
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)
  resultado <- .perfilar_consolidacion_dbi(
    bases$conexion, modo = "exacto", muestra = 5L
  )
  registros <- resultado$resumen_tabla$sql
  sanas <- registros[registros$columna %in% c("a", "b") &
    registros$metrica != "n", , drop = FALSE]
  mala <- registros[registros$columna == "mala" & registros$metrica != "n", ,
                    drop = FALSE]
  expect_true(all(sanas$estado == "calculado"))
  expect_true(all(mala$estado == "no_disponible"))
  expect_true(any(.consolidacion_dbi$rechazos == "tipo malo lote"))
  expect_true(any(grepl("mala|agregado|tipo", mala$motivo, ignore.case = TRUE)))
  fallas_mala <- mala[!is.na(mala$consulta_id), , drop = FALSE]
  expect_true(nrow(fallas_mala) > 0L)
  expect_true(all(is.na(fallas_mala$n_filas_resultado)))
  expect_true(all(is.na(fallas_mala$bytes_resultado_r)))
  expect_true(all(is.na(
    resultado$resumen_tabla$columnas$media[
      resultado$resumen_tabla$columnas$columna == "mala"
    ]
  )))
})

test_that("la biseccion de agregados conserva las columnas culpables", {
  casos <- list(
    `3 una` = list(n = 3L, malas = 2L),
    `8 dos` = list(n = 8L, malas = c(2L, 7L)),
    `20 cinco` = list(n = 20L, malas = c(1L, 6L, 11L, 16L, 20L))
  )
  for (caso in casos) {
    sondear <- function(indices) !any(indices %in% caso$malas)
    obtenido <- lupa:::.aislar_ilegibles_dbi(
      sondear, function() TRUE, caso$n, 2L * caso$n
    )
    expect_setequal(obtenido$culpables, caso$malas)
    expect_false(obtenido$agotado)
  }

  # El tope no convierte una hoja que no se pudo probar en culpable.
  parcial <- lupa:::.aislar_ilegibles_dbi(
    function(indices) !any(indices == 1L), function() TRUE, 8L, 3L,
    conservar_legibles = TRUE
  )
  expect_true(parcial$agotado)
  expect_false(1L %in% parcial$culpables)
  expect_true(length(parcial$pendientes) > 0L)
})

test_that("la biseccion reduce el descarte de agregados y publica su lote", {
  base <- as.data.frame(setNames(
    replicate(20L, seq_len(100L), simplify = FALSE),
    paste0("c", seq_len(20L))
  ))
  # Cada perfil consulta ademas el catalogo de la clave primaria, aunque la
  # politica no necesite usarlo para decidir cardinalidad.
  esperadas <- c(`1` = 20L, `2` = 23L, `5` = 36L)
  for (cantidad in c(1L, 2L, 5L)) {
    datos <- base
    nombres <- c(
      paste0("mala", seq_len(cantidad)),
      paste0("c", seq_len(20L - cantidad))
    )
    names(datos) <- nombres
    bases <- .conexion_consolidacion_dbi("tipo_malo", datos)
    resultado <- .perfilar_consolidacion_dbi(
      bases$conexion, modo = "seguro", muestra = 5L,
      bloque_muestra = "solo_agregados", tamano_lote = 20L,
      instrumentar = FALSE
    )
    registros <- resultado$resumen_tabla$sql
    no_disponibles <- unique(registros$columna[
      registros$metrica != "n" & registros$estado == "no_disponible"
    ])
    expect_setequal(no_disponibles, nombres[seq_len(cantidad)])
    expect_identical(
      as.integer(resultado$resumen_tabla$meta$consultas$emitidas),
      unname(esperadas[as.character(cantidad)])
    )
    expect_identical(resultado$resumen_tabla$meta$tamano_lote_funciono, 10L)
    DBI::dbDisconnect(bases$cruda)
  }
})

test_that("el plan predice la fusion plana y separa COUNT DISTINCT", {
  bases <- .conexion_consolidacion_dbi()
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)
  .reiniciar_consolidacion_dbi("normal")
  plan <- lupa:::plan_perfilado_dbi(
    bases$conexion, "datos", modo = "exacto", muestra = 5L,
    bloque_muestra = "solo_agregados", tamano_lote = 2L
  )
  .reiniciar_consolidacion_dbi("normal")
  resultado <- .perfilar_consolidacion_dbi(
    bases$conexion, modo = "exacto", muestra = 5L,
    bloque_muestra = "solo_agregados", tamano_lote = 2L
  )
  expect_identical(
    as.integer(resultado$resumen_tabla$meta$consultas$emitidas),
    as.integer(attr(plan, "total"))
  )
  registros <- resultado$resumen_tabla$sql
  planos <- registros[
    registros$metrica %in% c(
      "n_validos", "n_faltantes", "prop_faltantes", "minimo", "maximo",
      "media", "n_ceros", "n_negativos", "desvio"
    ) & !is.na(registros$sql), , drop = FALSE]
  expect_equal(length(unique(planos$sql)), 2L)
  distintos <- registros[registros$metrica == "n_distintos", , drop = FALSE]
  expect_true(all(grepl("COUNT(DISTINCT", distintos$sql, fixed = TRUE)))
  expect_true(all(!grepl(
    "COUNT(DISTINCT", planos$sql, fixed = TRUE
  )))
  expect_true(any(grepl("agregados planos", plan$clase_consulta, fixed = TRUE)))
  expect_true(any(grepl("clase separada", plan$clase_consulta, fixed = TRUE)))
})

test_that("el rango del plan incluye el costo de una biseccion rechazada", {
  bases <- .conexion_consolidacion_dbi("muchas_expresiones")
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)
  .reiniciar_consolidacion_dbi("muchas_expresiones")
  plan <- lupa:::plan_perfilado_dbi(
    bases$conexion, "datos", modo = "exacto", muestra = 5L,
    bloque_muestra = "solo_agregados", tamano_lote = 4L
  )
  .reiniciar_consolidacion_dbi("muchas_expresiones")
  resultado <- .perfilar_consolidacion_dbi(
    bases$conexion, modo = "exacto", muestra = 5L,
    bloque_muestra = "solo_agregados", tamano_lote = 4L
  )
  emitidas <- as.integer(resultado$resumen_tabla$meta$consultas$emitidas)
  expect_gte(emitidas, as.integer(attr(plan, "total")))
  expect_lte(
    emitidas, as.integer(attr(plan, "total_lotes_rechazados"))
  )
})
