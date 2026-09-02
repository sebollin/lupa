skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

.memoria_trabajo_args <- function() {
  list(
    bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE,
    duplicados_aproximados = FALSE,
    instrumentar = FALSE
  )
}

.memoria_trabajo_perfil <- function(conexion, tabla = "datos", ...) {
  do.call(
    perfilar_dbi,
    c(list(conexion, tabla), .memoria_trabajo_args(), list(...))
  )
}

.memoria_trabajo_datos <- function(n = 100000L) {
  n_distintos <- min(90000L, n)
  repetidos <- n - n_distintos
  x <- c(
    seq_len(n_distintos),
    if (repetidos > 0L) {
      rep(seq_len(min(10000L, n_distintos)), length.out = repetidos)
    }
  )
  data.frame(id = seq_len(n), x = x, stringsAsFactors = FALSE)
}

.memoria_trabajo_conexion <- function(datos = .memoria_trabajo_datos()) {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(conexion, "datos", datos)
  conexion
}

test_that("COUNT DISTINCT publica memoria creciente sobre la tabla completa", {
  conexion <- .memoria_trabajo_conexion()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)

  resultado <- .memoria_trabajo_perfil(
    conexion, metricas = "distintos", universo = "tabla_completa"
  )
  registros <- resultado$resumen_tabla$sql
  distintos <- registros[registros$metrica %in% c(
    "n_distintos", "tasa_distintos"
  ), , drop = FALSE]
  fila_x <- distintos[distintos$columna == "x", , drop = FALSE]

  expect_true(nrow(fila_x) == 2L)
  expect_true(all(fila_x$estado == "calculado"))
  expect_true(all(fila_x$alcance == "tabla_completa"))
  expect_true(all(fila_x$metodo == "COUNT(DISTINCT)"))
  expect_true(all(fila_x$memoria_trabajo == "creciente"))
  expect_equal(
    resultado$resumen_tabla$columnas$n_distintos[
      resultado$resumen_tabla$columnas$columna == "x"
    ],
    90000
  )
})

test_that("el agregado plano y el conteo de filas son acotados", {
  conexion <- .memoria_trabajo_conexion(.memoria_trabajo_datos(20L))
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)

  resultado <- .memoria_trabajo_perfil(
    conexion,
    metricas = c("validos", "distintos", "basicos"),
    universo = "tabla_completa"
  )
  registros <- resultado$resumen_tabla$sql
  planos <- registros[registros$metrica %in% c(
    "n_validos", "n_faltantes", "prop_faltantes", "minimo", "maximo",
    "media", "n_ceros", "n_negativos"
  ), , drop = FALSE]
  filas <- registros[registros$metrica == "n", , drop = FALSE]

  expect_true(nrow(planos) > 0L)
  expect_true(all(planos$metodo == "tabla_completa"))
  expect_true(all(planos$estado == "calculado"))
  expect_true(all(planos$memoria_trabajo == "acotado"))
  expect_true(all(filas$metodo == "conteo_universo"))
  expect_true(all(filas$memoria_trabajo == "acotado"))
})

test_that("una muestra real acota todas sus filas medidas, incluso distintos", {
  conexion <- .memoria_trabajo_conexion()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)

  resultado <- .memoria_trabajo_perfil(
    conexion,
    universo = "muestra_motor", muestra_motor = 3000L,
    metricas = c("validos", "distintos", "basicos", "moda", "mediana", "desvio")
  )
  registros <- resultado$resumen_tabla$sql
  estados_sin_medicion <- c(
    "no_solicitado", "omitida", "omitido_por_costo",
    "omitido_por_privacidad", "no_disponible", "no_aplica",
    "sin_valores", "no_medido"
  )
  medidos <- !registros$estado %in% estados_sin_medicion
  distintos <- registros[registros$metrica == "n_distintos", , drop = FALSE]

  expect_true(all(distintos$estado == "observado_muestra"))
  expect_true(all(distintos$alcance == "muestra"))
  expect_true(all(distintos$tamano_muestra == 3000))
  expect_true(all(distintos$fraccion < 1))
  expect_true(all(registros$memoria_trabajo[medidos] == "acotado"))
  expect_false(any(is.na(registros$memoria_trabajo[medidos])))
})

test_that("una muestra saturada clasifica el spool como trabajo creciente", {
  conexion <- .memoria_trabajo_conexion(.memoria_trabajo_datos(20L))
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)

  resultado <- .memoria_trabajo_perfil(
    conexion,
    universo = "muestra_motor", muestra_motor = 1000L,
    metricas = c("validos", "distintos", "basicos", "moda", "mediana", "desvio")
  )
  registros <- resultado$resumen_tabla$sql
  estados_sin_medicion <- c(
    "no_solicitado", "omitida", "omitido_por_costo",
    "omitido_por_privacidad", "no_disponible", "no_aplica",
    "sin_valores", "no_medido"
  )
  medidos <- !registros$estado %in% estados_sin_medicion
  saturados <- registros[
    medidos & registros$alcance == "muestra" & registros$fraccion == 1,
    , drop = FALSE
  ]
  spools <- saturados[saturados$metodo == "spool_sesion_cliente", , drop = FALSE]

  expect_true(nrow(saturados) > 0L)
  expect_false(any(is.na(saturados$memoria_trabajo)))
  expect_true(nrow(spools) > 0L)
  expect_true(all(spools$memoria_trabajo == "creciente"))
})

test_that("la misma metrica cambia de clase segun el metodo de DuckDB", {
  skip_if_not_installed("duckdb")
  conexion <- DBI::dbConnect(
    duckdb::duckdb(), dbdir = ":memory:"
  )
  on.exit(try(duckdb::duckdb_shutdown(conexion), silent = TRUE), add = TRUE)
  DBI::dbWriteTable(conexion, "datos", data.frame(
    x = rep(seq_len(5000L), 2L), stringsAsFactors = FALSE
  ))

  exacta <- .memoria_trabajo_perfil(
    conexion, metricas = "distintos", estrategia_distintos = "exacta"
  )
  aproximada <- .memoria_trabajo_perfil(
    conexion, metricas = "distintos",
    estrategia_distintos = "aproximada_motor"
  )
  fila_exacta <- exacta$resumen_tabla$sql[
    exacta$resumen_tabla$sql$metrica == "n_distintos", , drop = FALSE
  ]
  fila_aproximada <- aproximada$resumen_tabla$sql[
    aproximada$resumen_tabla$sql$metrica == "n_distintos", , drop = FALSE
  ]

  expect_true(all(fila_exacta$metodo == "COUNT(DISTINCT)"))
  expect_true(all(fila_exacta$estado == "calculado"))
  expect_true(all(fila_exacta$memoria_trabajo == "creciente"))
  expect_true(all(fila_aproximada$metodo == "APPROX_COUNT_DISTINCT"))
  expect_true(all(fila_aproximada$estado == "estimado_motor"))
  expect_true(all(fila_aproximada$memoria_trabajo == "acotado"))
})

test_that("el registro coincide con los metodos publicados, sin clasificar por metrica", {
  esperado <- c(
    "COUNT(DISTINCT)" = "creciente",
    "APPROX_COUNT_DISTINCT" = "acotado",
    "approx_count_distinct" = "acotado",
    "approx_count_distinct_generico" = "acotado",
    "ventana_agregado" = "creciente",
    "consulta_actual_sin_guardian" = "creciente",
    "subconsulta_escalar" = "creciente",
    "cte_ventana" = "creciente",
    "cte_ventana_tablesample_system" = "creciente",
    "cte_ventana_newid" = "creciente",
    "dos_consultas" = "creciente",
    "PERCENTILE_CONT" = "creciente",
    "PERCENTILE_CONT_OVER" = "creciente",
    "approx_percentile" = "acotado",
    "approx_quantile" = "acotado",
    "percentile_approx" = "acotado",
    "quantile" = "acotado",
    "spool_sesion_cliente" = "creciente",
    "dbfetch_bloques" = "acotado",
    "tabla_completa" = "acotado",
    "conteo_universo" = "acotado",
    "pg_stats.n_distinct" = "acotado"
  )
  expect_identical(lupa:::.REGISTRO_MEMORIA_TRABAJO_DBI, esperado)

  for (metodo in names(esperado)) {
    estado <- if (identical(metodo, "pg_stats.n_distinct")) {
      "estimado_catalogo"
    } else {
      "calculado"
    }
    registro <- lupa:::.registro_sql_dbi(
      "x", "metrica", estado, NA_character_, NA_character_,
      metadatos = lupa:::.metadatos_sql_dbi(
        alcance = "tabla_completa", fraccion = 1, metodo = metodo
      )
    )
    expect_identical(registro$memoria_trabajo, unname(esperado[[metodo]]))
  }

  muestra <- lupa:::.registro_sql_dbi(
    "x", "metrica", "observado_muestra", NA_character_, NA_character_,
    metadatos = lupa:::.metadatos_sql_dbi(
      alcance = "muestra", fraccion = 0.03, metodo = "COUNT(DISTINCT)"
    )
  )
  saturada <- lupa:::.registro_sql_dbi(
    "x", "metrica", "observado_muestra", NA_character_, NA_character_,
    metadatos = lupa:::.metadatos_sql_dbi(
      alcance = "muestra", fraccion = 1, metodo = "COUNT(DISTINCT)"
    )
  )
  muestra_spool <- lupa:::.registro_sql_dbi(
    "x", "metrica", "observado_muestra", NA_character_, NA_character_,
    metadatos = lupa:::.metadatos_sql_dbi(
      alcance = "muestra", fraccion = 0.2, metodo = "spool_sesion_cliente"
    )
  )
  expect_identical(muestra$memoria_trabajo, "acotado")
  expect_identical(muestra_spool$memoria_trabajo, "acotado")
  expect_identical(saturada$memoria_trabajo, "creciente")

  for (metodo in c(
    "cte_ventana", "cte_ventana_tablesample_system", "cte_ventana_newid"
  )) {
    muestra_cte <- lupa:::.registro_sql_dbi(
      "x", "metrica", "calculado", NA_character_, NA_character_,
      metadatos = lupa:::.metadatos_sql_dbi(
        alcance = "muestra", fraccion = 0.2, metodo = metodo
      )
    )
    saturada_cte <- lupa:::.registro_sql_dbi(
      "x", "metrica", "calculado", NA_character_, NA_character_,
      metadatos = lupa:::.metadatos_sql_dbi(
        alcance = "muestra", fraccion = 1, metodo = metodo
      )
    )
    expect_identical(muestra_cte$memoria_trabajo, "acotado")
    expect_identical(saturada_cte$memoria_trabajo, "creciente")
  }
})

test_that("la columna nueva es unica, queda al final y no entra al plan", {
  conexion <- .memoria_trabajo_conexion(data.frame(x = seq_len(20L)))
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)

  resultado <- .memoria_trabajo_perfil(conexion, metricas = "validos")
  sql <- resultado$resumen_tabla$sql
  nombres_anteriores <- c(
    "columna", "metrica", "estado", "motivo", "sql", "alcance", "universo",
    "tamano_muestra", "fraccion", "metodo", "error_esperado", "lote",
    "columnas_compartidas", "id_consulta", "estrategia_solicitada",
    "estrategia_resuelta", "estado_estrategia", "duracion_ms",
    "n_filas_resultado", "bytes_resultado_r", "cpu_ms", "consulta_id",
    "etapa", "derrame", "bloques_temporales_leidos",
    "bloques_temporales_escritos", "fuente_derrame", "llamadas_en_ventana", "nivel"
  )
  plan <- plan_perfilado_dbi(
    conexion, "datos", metricas = "validos", bloque_muestra = "solo_agregados"
  )

  expect_identical(names(sql), c(nombres_anteriores, "memoria_trabajo"))
  expect_equal(ncol(sql), length(nombres_anteriores) + 1L)
  # El resumen conserva una fila para cada metrica del vocabulario, incluso
  # las no solicitadas; el campo nuevo no agrega filas.
  expect_equal(nrow(sql), 15L)
  expect_false("memoria_trabajo" %in% names(plan))
  expect_identical(sql_perfil(resultado), sql)
})

test_that("los estados sin medicion y los metodos desconocidos quedan en NA", {
  estados <- c(
    "no_solicitado", "omitida", "omitido_por_costo",
    "omitido_por_privacidad", "no_disponible", "no_aplica",
    "sin_valores", "no_medido"
  )
  registros <- lapply(estados, function(estado) {
    lupa:::.registro_sql_dbi(
      "x", "metrica", estado, NA_character_, NA_character_,
      metadatos = lupa:::.metadatos_sql_dbi(
        alcance = "muestra", fraccion = 0.03, metodo = "COUNT(DISTINCT)"
      )
    )
  })
  futuro <- lupa:::.registro_sql_dbi(
    "x", "metrica", "calculado", NA_character_, NA_character_,
    metadatos = lupa:::.metadatos_sql_dbi(
      alcance = "tabla_completa", fraccion = 1, metodo = "metodo_del_futuro"
    )
  )
  salida <- do.call(rbind, registros)

  expect_true(all(is.na(salida$memoria_trabajo)))
  expect_true(is.na(futuro$memoria_trabajo))
})

test_that("una muestra saturada conserva el limite y declara el spool", {
  conexion <- .memoria_trabajo_conexion()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)

  resultado <- .memoria_trabajo_perfil(
    conexion,
    universo = "muestra_motor", muestra_motor = 200000L,
    metricas = c("validos", "distintos", "basicos")
  )
  registros <- resultado$resumen_tabla$sql
  planos <- registros[registros$metrica %in% c(
    "n_validos", "n_faltantes", "prop_faltantes", "minimo", "maximo",
    "media", "n_ceros", "n_negativos"
  ), , drop = FALSE]
  distintos <- registros[
    registros$metrica == "n_distintos" & registros$columna == "x",
    , drop = FALSE
  ]
  muestras <- registros[registros$metodo == "spool_sesion_cliente", , drop = FALSE]

  expect_true(nrow(planos) > 0L)
  expect_true(all(planos$alcance == "muestra"))
  expect_true(nrow(muestras) > 0L)
  expect_true(all(muestras$tamano_muestra == 100000))
  expect_true(all(muestras$fraccion == 1))
  expect_true(all(muestras$estado == "observado_muestra"))
  expect_true(all(muestras$memoria_trabajo == "creciente"))
  expect_true(isTRUE(resultado$resumen_tabla$meta$materializacion$validado_relectura))
  expect_identical(distintos$metodo[[1L]], "spool_sesion_cliente")
  expect_equal(
    resultado$resumen_tabla$columnas$n_distintos[
      resultado$resumen_tabla$columnas$columna == "x"
    ],
    90000
  )
})
