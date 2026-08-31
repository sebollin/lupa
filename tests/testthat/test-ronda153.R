skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

library(DBI)

.ronda153_estado <- new.env(parent = emptyenv())
.ronda153_estado$modo <- "normal"
.ronda153_estado$sql <- character()

if (!methods::isClass("ConexionRonda153Lupa")) {
  setClass("ConexionRonda153Lupa", contains = "SQLiteConnection")
}

setMethod(
  "dbSendQuery", c("ConexionRonda153Lupa", "character"),
  function(conn, statement, ...) {
    .ronda153_estado$sql <- c(.ronda153_estado$sql, statement)
    if (identical(.ronda153_estado$modo, "falla_lote") &&
        grepl("mala", statement, fixed = TRUE) &&
        grepl("lupa_l", statement, fixed = TRUE)) {
      stop("El motor rechazo una expresion del lote.", call. = FALSE)
    }
    callNextMethod(conn, statement, ...)
  }
)

.envolver_ronda153 <- function(conexion) {
  salida <- methods::new("ConexionRonda153Lupa")
  for (ranura in methods::slotNames(conexion)) {
    methods::slot(salida, ranura) <- methods::slot(conexion, ranura)
  }
  salida
}

.conexion_ronda153 <- function(datos, modo = "normal") {
  cruda <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(cruda, "tabla_prueba", datos)
  .ronda153_estado$modo <- modo
  .ronda153_estado$sql <- character()
  list(cruda = cruda, conexion = .envolver_ronda153(cruda))
}

.cerrar_ronda153 <- function(x) {
  suppressWarnings(try(DBI::dbDisconnect(x$conexion), silent = TRUE))
  suppressWarnings(try(DBI::dbDisconnect(x$cruda), silent = TRUE))
}

.ronda153_sin_instrumentacion <- function(x) {
  campos <- c(
    "sql", "lote", "columnas_compartidas", "id_muestra", "consulta_id",
    "etapa", "nivel", "duracion_ms", "n_filas_resultado", "bytes_resultado_r",
    "cpu_ms", "derrame", "bloques_temporales_leidos",
    "bloques_temporales_escritos", "fuente_derrame"
  )
  x$resumen_tabla$sql <- x$resumen_tabla$sql[
    , setdiff(names(x$resumen_tabla$sql), campos), drop = FALSE
  ]
  x$resumen_tabla$tiempos <- NULL
  x$resumen_tabla$meta$plan <- NULL
  x$resumen_tabla$meta$consultas <- NULL
  x$resumen_tabla$meta$tamano_lote <- NULL
  x$resumen_tabla$meta$tamano_lote_funciono <- NULL
  x$resumen_tabla$meta$tamano_lote_planos <- NULL
  x$resumen_tabla$meta$tamano_lote_distintos <- NULL
  x$resumen_tabla$meta$tamano_lote_planos_funciono <- NULL
  x$resumen_tabla$meta$tamano_lote_distintos_funciono <- NULL
  x$resumen_tabla$meta$instrumentacion <- NULL
  x$resumen_tabla$meta$derrame <- NULL
  x$resumen_tabla$meta$costo_distintos <- NULL
  x$resumen_tabla$meta$costo_moda <- NULL
  x$resumen_tabla$meta$costo_mediana <- NULL
  x
}

.ronda153_perfil <- function(conexion, tamano_lote, caso, ...) {
  argumentos <- modifyList(
    list(
      muestra = 12L, bloque_muestra = "solo_agregados",
      instrumentar = FALSE, proteger_datos_personales = FALSE,
      analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
      ausencia_estructural = FALSE, duplicados_aproximados = FALSE
    ),
    .argumentos_caso_dbi(caso, muestra = 12L)
  )
  do.call(
    perfilar_dbi,
    c(list(conexion, "tabla_prueba", tamano_lote = tamano_lote), argumentos,
      list(...))
  )
}

test_that("la fusion conserva la salida en los cinco casos con y sin ausentes", {
  casos <- list(
    con_ausentes = data.frame(
      id = c(0, 1, NA, 1), valor = c(0, NA, 0, 1),
      texto = c("a", "b", "c", NA), stringsAsFactors = FALSE
    ),
    sin_ausentes = data.frame(
      id = rep(c(0, 1), 2L), valor = rep(c(0, 1), 2L),
      texto = c("a", "b", "c", "d"), stringsAsFactors = FALSE
    )
  )
  casos_api <- c("exacto", "seguro", "conteos", "muestreado", "aproximado")
  conexiones <- list()
  on.exit(lapply(conexiones, .cerrar_ronda153), add = TRUE)
  for (nombre in names(casos)) {
    fusionada <- .conexion_ronda153(casos[[nombre]])
    unitaria <- .conexion_ronda153(casos[[nombre]])
    conexiones <- c(conexiones, list(fusionada, unitaria))
    for (caso in casos_api) {
      nuevo <- .ronda153_perfil(fusionada$conexion, 4L, caso)
      referencia <- .ronda153_perfil(unitaria$conexion, 1L, caso)
      expect_identical(
        .ronda153_sin_instrumentacion(nuevo),
        .ronda153_sin_instrumentacion(referencia),
        info = paste(nombre, caso)
      )
    }
  }
})

test_that("la cuenta fusionada ahorra la consulta y declara sus recorridos", {
  datos <- data.frame(
    id = 1:12, valor = seq_len(12), texto = rep(c("a", "b"), 6L),
    stringsAsFactors = FALSE
  )
  conexion <- .conexion_ronda153(datos)
  on.exit(.cerrar_ronda153(conexion), add = TRUE)
  casos_api <- c("exacto", "seguro", "conteos", "muestreado", "aproximado")
  mediciones <- list()
  for (caso in casos_api) {
    argumentos <- modifyList(
      list(muestra = 12L, bloque_muestra = "solo_agregados", tamano_lote = 4L),
      .argumentos_caso_dbi(caso, muestra = 12L)
    )
    .ronda153_estado$sql <- character()
    plan <- do.call(plan_perfilado_dbi,
                    c(list(conexion$conexion, "tabla_prueba"), argumentos))
    datos_plan <- grepl("FROM `tabla_prueba`", .ronda153_estado$sql,
                        fixed = TRUE) &
      !grepl("WHERE[[:space:]]+1[[:space:]]*=[[:space:]]*0",
             .ronda153_estado$sql, ignore.case = TRUE) &
      !grepl("LIMIT[[:space:]]+0[[:space:]]*$", .ronda153_estado$sql,
             ignore.case = TRUE)
      expect_false(any(datos_plan), info = paste("datos en el plan", caso))
    expect_false(any(grepl("COUNT(DISTINCT", .ronda153_estado$sql,
                           fixed = TRUE)), info = paste("distinct en el plan", caso))
    .ronda153_estado$modo <- "normal"
    .ronda153_estado$sql <- character()
    resultado <- .ronda153_perfil(conexion$conexion, 4L, caso)
    sql <- .ronda153_estado$sql
    cantidad_count <- vapply(
      gregexpr("COUNT\\(", sql, perl = TRUE),
      function(x) if (identical(x, -1L)) 0L else length(x), integer(1L)
    )
    solo_conteos <- grepl(
      "SELECT COUNT\\(\\*\\) AS.*lupa_n_total.*FROM `tabla_prueba`$",
      sql, perl = TRUE
    ) & cantidad_count == 1L
    fused <- grepl("n_total_consulta", sql, fixed = TRUE)
    expect_equal(
      resultado$resumen_tabla$meta$consultas$emitidas,
      attr(plan, "total"), info = paste("plan", caso)
    )
    expect_equal(sum(solo_conteos), 0L, info = paste("conteo separado", caso))
    expect_equal(sum(fused), 1L, info = paste("consulta fusionada", caso))
    # Los denominadores locales viajan en las consultas que ya necesitaban los
    # agregados; agregar la expresion no abre una consulta nueva.
    expect_equal(
      as.integer(attr(plan, "total")) + 1L - length(sql),
      1L, info = paste("ahorro", caso)
    )
    recorridos_fuente <- sum(vapply(
      gregexpr("FROM `tabla_prueba`", sql, fixed = TRUE),
      function(x) if (identical(x, -1L)) 0L else length(x), integer(1L)
    ))
    # En la tabla completa, el denominador local comparte el recorrido. El
    # muestreo aleatorio conserva un conteo exacto como subconsulta sobre la
    # tabla original: no abre una consulta, pero si agrega ese recorrido.
    ahorro_recorridos <- if (identical(caso, "muestreado")) 0L else 1L
    recorridos_esperados <- if (identical(caso, "muestreado")) {
      length(sql) + 1L
    } else {
      length(sql) + 2L * sum(
        resultado$resumen_tabla$sql$metodo == "subconsulta_escalar",
        na.rm = TRUE
      )
    }
    expect_equal(
      recorridos_fuente, recorridos_esperados,
      info = paste("recorridos en SQL", caso)
    )
    recorridos_antes <- if (identical(caso, "muestreado")) {
      recorridos_fuente
    } else {
      recorridos_fuente + 1L
    }
    expect_equal(
      recorridos_antes - recorridos_fuente, ahorro_recorridos,
      info = paste("recorridos comparables", caso)
    )
    if (identical(caso, "muestreado")) {
      expect_true(any(grepl("(SELECT COUNT(*) FROM", sql, fixed = TRUE)))
    } else {
      expect_true(any(grepl(
        "COUNT(*) AS `n_total_consulta`", sql, fixed = TRUE
      )))
    }
    mediciones[[caso]] <- c(
      plan = as.integer(attr(plan, "total")),
      actual = length(sql),
      expresiones_count = sum(cantidad_count),
      consultas_fusionadas = sum(fused),
      recorridos_fuente = recorridos_fuente,
      ahorro_recorridos = ahorro_recorridos
    )
  }
  if (identical(Sys.getenv("LUPA_RONDA153_MEDIR"), "1")) {
    for (caso in casos_api) {
      cat(
        "Ronda153", caso,
        paste(names(mediciones[[caso]]), mediciones[[caso]], sep = "=", collapse = " "),
        "\n"
      )
    }
  }
})

test_that("el repliegue conserva un denominador exacto si falla el lote", {
  datos <- data.frame(
    bueno = c(10, NA, 30, 40),
    mala = c(1, 2, NA, 4),
    stringsAsFactors = FALSE
  )
  conexion <- .conexion_ronda153(datos, modo = "falla_lote")
  on.exit(.cerrar_ronda153(conexion), add = TRUE)
  resultado <- perfilar_dbi(
    conexion$conexion, "tabla_prueba", universo = "tabla_completa",
    metricas = "validos", estrategia_mediana = "exacta", muestra = 4L,
    bloque_muestra = "solo_agregados", tamano_lote = 2L,
    instrumentar = FALSE, proteger_datos_personales = FALSE
  )
  filas <- resultado$resumen_tabla$columnas
  bueno <- filas[filas$columna == "bueno", , drop = FALSE]
  expect_equal(as.numeric(resultado$resumen_tabla$meta$filas), 4)
  expect_equal(bueno$n_validos, 3)
  expect_equal(bueno$n_faltantes, 1)
  expect_equal(bueno$prop_faltantes, 0.25)
  solo_conteos <- grepl(
    "SELECT COUNT\\(\\*\\) AS.*lupa_n_total.*FROM `tabla_prueba`$",
    .ronda153_estado$sql, perl = TRUE
  ) & vapply(
    gregexpr("COUNT\\(", .ronda153_estado$sql, perl = TRUE),
    function(x) if (identical(x, -1L)) 0L else length(x), integer(1L)
  ) == 1L
  expect_equal(sum(solo_conteos), 1L)
  expect_true(any(grepl("lupa_l", .ronda153_estado$sql, fixed = TRUE)))
})

test_that("el identificador sólo reúne métricas de una misma consulta", {
  datos <- data.frame(
    id = 1:12, valor = seq_len(12), extra = seq_len(12) * 2,
    texto = rep(c("a", "b"), 6L),
    stringsAsFactors = FALSE
  )
  conexion <- .conexion_ronda153(datos)
  on.exit(.cerrar_ronda153(conexion), add = TRUE)
  resultado <- perfilar_dbi(
    conexion$conexion, "tabla_prueba", universo = "tabla_completa",
    estrategia_mediana = "exacta", muestra = 12L,
    bloque_muestra = "solo_agregados", tamano_lote = 2L,
    instrumentar = FALSE, proteger_datos_personales = FALSE
  )
  sql <- resultado$resumen_tabla$sql
  planos <- sql[sql$metrica %in% c(
    "n_validos", "n_faltantes", "prop_faltantes", "minimo", "maximo",
    "media", "n_ceros", "n_negativos", "desvio"
  ) & sql$estado == "calculado", , drop = FALSE]
  expect_true(all(!is.na(planos$id_muestra)))
  expect_equal(length(unique(planos$id_muestra)), 2L)
  expect_true(all(!is.na(sql$id_muestra[sql$metrica == "n_distintos"])))
  expect_true(all(is.na(sql$id_muestra[sql$metrica %in% c(
    "moda", "frecuencia_moda", "mediana"
  )])))
  expect_true(all(is.na(sql$id_muestra[sql$estado == "no_disponible"])))
  expect_true(all(is.na(sql$id_muestra[is.na(sql$sql)])))
  con_id <- split(sql$sql[!is.na(sql$id_muestra)], sql$id_muestra[!is.na(sql$id_muestra)])
  expect_true(all(vapply(con_id, function(x) length(unique(x)) == 1L, logical(1L))))
  for (id in unique(planos$id_muestra)) {
    filas <- planos[planos$id_muestra == id, , drop = FALSE]
    expect_gt(length(unique(filas$columna)), 1L)
  }
})

# El identificador NO es una medicion: es un hecho estructural sobre que
# consulta produjo cada metrica. Apagar el cronometro no puede llevarse puesta
# la garantia de comparabilidad, y la documentacion lo listaba entre los campos
# que agrega `instrumentar`, que era falso.

test_that("el identificador de muestra no depende del cronometro", {
  skip_if_not_installed("RSQLite")
  set.seed(153L)
  datos <- data.frame(
    a = stats::rnorm(200L), b = sample(seq_len(20L), 200L, TRUE),
    c = sample(letters[1:5], 200L, TRUE), stringsAsFactors = FALSE
  )
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)

  con_reloj <- perfilar_dbi(
    conexion, "t", universo = "tabla_completa", estrategia_mediana = "exacta",
    tamano_lote = 2L,
    bloque_muestra = "solo_agregados", instrumentar = TRUE
  )$resumen_tabla$sql
  sin_reloj <- perfilar_dbi(
    conexion, "t", universo = "tabla_completa", estrategia_mediana = "exacta",
    tamano_lote = 2L,
    bloque_muestra = "solo_agregados", instrumentar = FALSE
  )$resumen_tabla$sql

  expect_true("id_muestra" %in% names(sin_reloj))
  expect_identical(
    sum(!is.na(sin_reloj$id_muestra)), sum(!is.na(con_reloj$id_muestra))
  )
  expect_gt(sum(!is.na(sin_reloj$id_muestra)), 0L)

  # Y el contrato sigue valiendo sin cronometro: mismo identificador, misma
  # consulta, en los dos sentidos.
  con_id <- sin_reloj[!is.na(sin_reloj$id_muestra) & !is.na(sin_reloj$sql), , drop = FALSE]
  por_id <- split(con_id$sql, con_id$id_muestra)
  expect_true(all(vapply(por_id, function(g) length(unique(g)) == 1L, logical(1L))))
  por_sql <- split(con_id$id_muestra, con_id$sql)
  expect_true(all(vapply(por_sql, function(g) length(unique(g)) == 1L, logical(1L))))
})
