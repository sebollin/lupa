# Defectos de `perfilar_dbi()` fuera de la llamada por omision, cada uno con el
# caso que lo mostraba. Lo que tienen en comun: un argumento que casi nadie pasa
# y una condicion de falla (una columna ilegible, un recorrido que se corta, un
# reintento de la muestra), que es justo donde ninguna prueba miraba.
#
# Los atributos de cada columna viajaban en vectores paralelos por posicion: al
# recortar uno y no los otros, la columna de la derecha se juzgaba con el tipo de
# la omitida. El repliegue de la lectura por bloques rehacia la llamada a mano y
# perdia lo que la persona habia declarado.

# Comprueba que el tipo no se desplace cuando una columna intermedia falla.
test_that("la muestra conserva el tipo de cada columna junto a una ilegible", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE tipos_dbi (rota BLOB, derecha DECIMAL(18,2), texto VARCHAR)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO tipos_dbi VALUES ",
    "(decode('01','hex'), 100000000000000.01, 'a'),",
    "(decode('02','hex'), 100000000000000.02, 'b'),",
    "(decode('03','hex'), 100000000000000.03, 'a')"
  ))
  traer <- get(".traer_consulta_dbi", envir = asNamespace("lupa"))
  enviar <- get(".enviar_consulta_dbi", envir = asNamespace("lupa"))
  rechazar <- function(sql) {
    if (grepl("tipos_dbi", sql, fixed = TRUE) &&
        grepl("rota", sql, fixed = TRUE) &&
        !grepl("WHERE 1 = 0", sql, fixed = TRUE)) {
      stop("bad sample column", call. = FALSE)
    }
  }
  testthat::local_mocked_bindings(
    .traer_consulta_dbi = function(conexion, sql) {
      rechazar(sql)
      traer(conexion, sql)
    },
    .enviar_consulta_dbi = function(conexion, sql) {
      rechazar(sql)
      enviar(conexion, sql)
    },
    .package = "lupa"
  )
  r <- suppressWarnings(suppressMessages(lupa::perfilar_dbi(
    con, "tipos_dbi", muestra = 3,
    metricas = c("validos", "basicos", "desvio"), incluir_valores = TRUE,
    proteger_datos_personales = FALSE, instrumentar = FALSE
  )))
  fila <- r$perfil_muestra$columnas
  fila <- fila[fila$columna == "derecha", , drop = FALSE]
  expect_identical(as.character(fila$tipo_declarado), "doble")
  fila_resumen <- r$resumen_tabla$columnas
  fila_resumen <- fila_resumen[
    fila_resumen$columna == "derecha", , drop = FALSE
  ]
  expect_equal(as.numeric(fila_resumen$desvio), 0.01, tolerance = 1e-12)
})

# Comprueba que una columna rota no contamine las cuentas de las columnas sanas.
test_that("el perfil de una columna legible no cambia por tener una ilegible a su izquierda", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE pares_dbi (rota BLOB, entero INTEGER, doble DOUBLE, ",
    "decimal DECIMAL(18,2), texto VARCHAR)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO pares_dbi VALUES ",
    "(decode('01','hex'), 1, 1.25, 100.01, 'a'),",
    "(decode('02','hex'), 2, NULL, 100.02, 'b'),",
    "(decode('03','hex'), 3, 3.25, 100.03, 'a')"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE pares_dbi_ref (entero INTEGER, doble DOUBLE, ",
    "decimal DECIMAL(18,2), texto VARCHAR)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO pares_dbi_ref VALUES ",
    "(1, 1.25, 100.01, 'a'), (2, NULL, 100.02, 'b'),",
    "(3, 3.25, 100.03, 'a')"
  ))
  traer <- get(".traer_consulta_dbi", envir = asNamespace("lupa"))
  enviar <- get(".enviar_consulta_dbi", envir = asNamespace("lupa"))
  rechazar <- function(sql) {
    if (grepl("pares_dbi", sql, fixed = TRUE) &&
        grepl("rota", sql, fixed = TRUE) &&
        !grepl("WHERE 1 = 0", sql, fixed = TRUE)) {
      stop("bad pair column", call. = FALSE)
    }
  }
  testthat::local_mocked_bindings(
    .traer_consulta_dbi = function(conexion, sql) {
      rechazar(sql)
      traer(conexion, sql)
    },
    .enviar_consulta_dbi = function(conexion, sql) {
      rechazar(sql)
      enviar(conexion, sql)
    },
    .package = "lupa"
  )
  perfil <- function(tabla) suppressWarnings(suppressMessages(
    lupa::perfilar_dbi(
      con, tabla, muestra = 3,
      metricas = c("validos", "basicos", "desvio"),
      incluir_valores = TRUE, proteger_datos_personales = FALSE,
      instrumentar = FALSE
    )
  ))
  con_rota <- perfil("pares_dbi")
  sin_rota <- perfil("pares_dbi_ref")
  columnas <- c("entero", "doble", "decimal", "texto")
  campos <- intersect(c(
    "tipo_declarado", "tipo_inferido", "n_validos", "n_faltantes",
    "n_distintos", "minimo", "maximo", "media", "desvio"
  ), names(con_rota$perfil_muestra$columnas))
  fila <- function(r, columna) {
    x <- r$perfil_muestra$columnas
    unname(x[x$columna == columna, campos, drop = FALSE])
  }
  expect_equal(
    lapply(columnas, function(columna) fila(con_rota, columna)),
    lapply(columnas, function(columna) fila(sin_rota, columna)),
    tolerance = 1e-12
  )
})

# Comprueba que una magnitud no finita termine en un estado publicable.
test_that("una magnitud infinita sin `validos` no aborta la corrida", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "inf_dbi", data.frame(v = c(1, 2, 3)))
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(...) "mysql",
    .sonda_magnitud_columna_dbi = function(...) list(
      ok = TRUE, valor = Inf, minimo = -Inf, maximo_abs = Inf,
      n_validos = NA_real_, motivo = NA_character_
    ),
    .magnitudes_finitas_dbi = function(...) NULL,
    .package = "lupa"
  )
  r <- tryCatch(suppressWarnings(suppressMessages(lupa::perfilar_dbi(
    con, "inf_dbi", metricas = "desvio", incluir_valores = TRUE,
    proteger_datos_personales = FALSE, instrumentar = FALSE
  ))), error = identity)
  expect_false(inherits(r, "condition"))
  sql <- r$resumen_tabla$sql
  fila <- sql[sql$columna == "v" & sql$metrica == "desvio", , drop = FALSE]
  expect_identical(as.character(fila$estado), "no_disponible")
})

# Comprueba que la segunda lectura conserve los valores de la llamada original.
test_that("el repliegue de bloques reusa los argumentos de la llamada, una sola vez", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "fallback_dbi", data.frame(id = 1:6, x = 1:6))
  estado <- new.env(parent = emptyenv())
  estado$llamadas <- list()
  assign("estado_dbi_confirmados", estado, envir = .GlobalEnv)
  on.exit(rm("estado_dbi_confirmados", envir = .GlobalEnv), add = TRUE)
  testthat::local_mocked_bindings(
    .recorrer_fuente_bloques_dbi = function(...) list(
      acumuladores = list(), sobres = list(),
      bloques = list(solicitados = 1L, recorridos = 0L, filas_vistas = 0,
        fetches = 0L, consultas_sql = 1L, primer_ordinal = NA_real_,
        ultimo_ordinal = NA_real_, sin_solapamiento = TRUE),
      bytes = list(max_bloque = 0, entrada = 0, texto = 0, retenidos = 0,
        rss_maximo = NA_real_), locators = list(),
      error = "forced block failure"
    ),
    .package = "lupa"
  )
  trace("perfilar_dbi", tracer = quote({
    estado_dbi_confirmados$llamadas[[length(estado_dbi_confirmados$llamadas) + 1L]] <- list(
      max_consultas = max_consultas, dialecto = dialecto,
      max_celdas_muestra = max_celdas_muestra,
      max_bytes_muestra = max_bytes_muestra,
      avisar_costo_moda = avisar_costo_moda,
      max_bytes_procesamiento = max_bytes_procesamiento
    )
  }), where = asNamespace("lupa"), print = FALSE)
  on.exit(untrace("perfilar_dbi", where = asNamespace("lupa")), add = TRUE)
  r <- suppressWarnings(suppressMessages(lupa::perfilar_dbi(
    con, "fallback_dbi", bloque_filas = 2L, max_consultas = 100,
    dialecto = "portable", max_celdas_muestra = 2, max_bytes_muestra = 1234,
    avisar_costo_moda = FALSE, max_bytes_procesamiento = 4321,
    metricas = c("validos", "moda"), columnas_opcionales = "x",
    proteger_datos_personales = FALSE, instrumentar = FALSE
  )))
  expect_true(length(estado$llamadas) >= 2L)
  expect_true(all(vapply(estado$llamadas, function(x) {
    identical(x$max_consultas, 100) && identical(x$dialecto, "portable") &&
      identical(x$max_celdas_muestra, 2) &&
      identical(x$max_bytes_muestra, 1234) &&
      identical(x$avisar_costo_moda, FALSE) &&
      identical(x$max_bytes_procesamiento, 4321)
  }, logical(1L))))
  cobertura <- r$resumen_tabla$cobertura
  expect_length(cobertura$elemento[cobertura$elemento == "columnas_opcionales"], 1L)
})

# Comprueba que una llamada indirecta no pierda los argumentos declarados.
test_that("el repliegue de bloques conserva todo argumento declarado, tambien con do.call", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "fallback_formales_dbi", data.frame(id = 1:6, x = 1:6))
  estado <- new.env(parent = emptyenv())
  estado$llamadas <- list()
  assign("estado_dbi_formales", estado, envir = .GlobalEnv)
  on.exit(rm("estado_dbi_formales", envir = .GlobalEnv), add = TRUE)
  testthat::local_mocked_bindings(
    .recorrer_fuente_bloques_dbi = function(...) list(
      acumuladores = list(), sobres = list(),
      bloques = list(solicitados = 1L, recorridos = 0L, filas_vistas = 0,
        fetches = 0L, consultas_sql = 1L, primer_ordinal = NA_real_,
        ultimo_ordinal = NA_real_, sin_solapamiento = TRUE),
      bytes = list(max_bloque = 0, entrada = 0, texto = 0, retenidos = 0,
        rss_maximo = NA_real_), locators = list(),
      error = "forced block failure"
    ),
    .package = "lupa"
  )
  trace("perfilar_dbi", tracer = quote({
    estado_dbi_formales <- get("estado_dbi_formales", envir = .GlobalEnv)
    estado_dbi_formales$llamadas[[length(estado_dbi_formales$llamadas) + 1L]] <-
      list(
        formales = mget(
          setdiff(names(formals(perfilar_dbi)), "..."),
          envir = environment(), inherits = FALSE
        ), puntos = list(...)
      )
  }), where = asNamespace("lupa"), print = FALSE)
  on.exit(untrace("perfilar_dbi", where = asNamespace("lupa")), add = TRUE)
  argumentos <- list(
    conexion = con, tabla = "fallback_formales_dbi",
    universo = "tabla_completa", muestra = 4L, orden_muestra = "id",
    metricas = c("validos", "basicos"), estrategia_distintos = "exacta",
    estrategia_mediana = "exacta", politica_costo = "todas",
    bloque_muestra = "con_muestra", max_consultas = 101,
    dialecto = "portable", incluir_valores = TRUE, tamano_lote = 7L,
    tamano_lote_planos = 8L, tamano_lote_distintos = 9L,
    instrumentar = FALSE, umbral_cardinalidad = 0.75,
    avisar_costo_distintos = FALSE, umbral_segundos_aviso_distintos = 11,
    avisar_costo_moda = FALSE, umbral_segundos_aviso_moda = 12,
    avisar_costo_mediana = FALSE, umbral_segundos_aviso_mediana = 13,
    avisar_derrame_estimado = FALSE,
    umbral_bytes_aviso_derrame_estimado = 14,
    max_celdas_muestra = 10000, max_bytes_muestra = 1e6,
    bloque_filas = 2L, max_bytes_procesamiento = 1e6,
    max_bytes_materializacion = 1e6, columnas_opcionales = "x",
    sentinelas_numericos = c(-1, 999), fecha = "2026-01-15",
    proteger_datos_personales = FALSE
  )
  r <- suppressWarnings(suppressMessages(do.call(lupa::perfilar_dbi, argumentos)))
  expect_true(length(estado$llamadas) >= 2L)
  primero <- estado$llamadas[[1L]]
  segundo <- estado$llamadas[[2L]]
  formales_provistos <- setdiff(
    intersect(names(argumentos), names(formals(lupa::perfilar_dbi))),
    c("bloque_filas")
  )
  for (nombre in formales_provistos) {
    expect_identical(segundo$formales[[nombre]], primero$formales[[nombre]])
  }
  puntos_provistos <- setdiff(
    names(argumentos), c(names(formals(lupa::perfilar_dbi)), "bloque_filas")
  )
  expect_identical(segundo$puntos[puntos_provistos], primero$puntos[puntos_provistos])
  expect_null(segundo$formales$bloque_filas)
  expect_true(!is.null(r$resumen_tabla$meta$fuente_bloques_fallback))
})

# Comprueba que una expresion de entrada se evalue una sola vez.
test_that("el repliegue de bloques no vuelve a evaluar un argumento con efecto, llamado desde una funcion", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "fallback_effect_dbi", data.frame(id = 1:6, x = 1:6))
  testthat::local_mocked_bindings(
    .recorrer_fuente_bloques_dbi = function(...) list(
      acumuladores = list(), sobres = list(),
      bloques = list(solicitados = 1L, recorridos = 0L, filas_vistas = 0,
        fetches = 0L, consultas_sql = 1L, primer_ordinal = NA_real_,
        ultimo_ordinal = NA_real_, sin_solapamiento = TRUE),
      bytes = list(max_bloque = 0, entrada = 0, texto = 0, retenidos = 0,
        rss_maximo = NA_real_), locators = list(),
      error = "forced block failure"
    ),
    .package = "lupa"
  )
  veces <- 0L
  perfil_local <- function(mi_conexion, nombre, n, t) {
    lupa::perfilar_dbi(
      mi_conexion, nombre, bloque_filas = 2L, max_consultas = n,
      metricas = t, muestra = {
        veces <<- veces + 1L
        4L
      }, incluir_valores = TRUE, proteger_datos_personales = FALSE,
      instrumentar = FALSE
    )
  }
  r <- suppressWarnings(suppressMessages(
    perfil_local(con, "fallback_effect_dbi", 100, c("validos", "basicos"))
  ))
  expect_identical(veces, 1L)
  expect_true(!is.null(r$resumen_tabla$meta$fuente_bloques_fallback))
})

# Comprueba que el motivo del fallo de bloques llegue a la cobertura.
test_that("un repliegue de bloques que falla deja su mensaje en la cobertura", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "fallback_error_dbi", data.frame(id = 1:6, x = 1:6))
  testthat::local_mocked_bindings(
    .recorrer_fuente_bloques_dbi = function(...) list(
      acumuladores = list(), sobres = list(),
      bloques = list(solicitados = 1L, recorridos = 0L, filas_vistas = 0,
        fetches = 0L, consultas_sql = 1L, primer_ordinal = NA_real_,
        ultimo_ordinal = NA_real_, sin_solapamiento = TRUE),
      bytes = list(max_bloque = 0, entrada = 0, texto = 0, retenidos = 0,
        rss_maximo = NA_real_), locators = list(),
      error = "forced block failure"
    ),
    .resumen_tabla_dbi = function(...) stop(
      "original fallback failure", call. = FALSE
    ),
    .package = "lupa"
  )
  r <- suppressWarnings(suppressMessages(lupa::perfilar_dbi(
    con, "fallback_error_dbi", bloque_filas = 2L,
    metricas = c("validos", "basicos"), incluir_valores = TRUE,
    proteger_datos_personales = FALSE, instrumentar = FALSE
  )))
  fila <- r$resumen_tabla$cobertura[
    r$resumen_tabla$cobertura$bloque == "fuente_bloques", , drop = FALSE
  ]
  expect_true(any(grepl(
    "original fallback failure", fila$motivo, fixed = TRUE, useBytes = TRUE
  )))
})

# Comprueba que la via de colecciones use el mismo repliegue de bloques.
test_that("perfilar_coleccion llega al repliegue de bloques", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "collection_fallback_dbi", data.frame(
    id = 1:6, x = 1:6
  ))
  testthat::local_mocked_bindings(
    .recorrer_fuente_bloques_dbi = function(...) list(
      acumuladores = list(), sobres = list(),
      bloques = list(solicitados = 1L, recorridos = 0L, filas_vistas = 0,
        fetches = 0L, consultas_sql = 1L, primer_ordinal = NA_real_,
        ultimo_ordinal = NA_real_, sin_solapamiento = TRUE),
      bytes = list(max_bloque = 0, entrada = 0, texto = 0, retenidos = 0,
        rss_maximo = NA_real_), locators = list(),
      error = "collection forced block failure"
    ),
    .package = "lupa"
  )
  r <- suppressWarnings(suppressMessages(lupa::perfilar_coleccion(
    lupa::coleccion(con, "collection_fallback_dbi"),
    bloque_filas = 2L, bloque_muestra = "con_muestra",
    metricas = c("validos", "basicos"), conservar_perfiles = TRUE,
    instrumentar = FALSE
  )))
  cobertura <- r$perfiles[[1L]]$resumen_tabla$cobertura
  fila <- cobertura[cobertura$bloque == "fuente_bloques", , drop = FALSE]
  expect_true(nrow(fila) == 1L)
  expect_true(grepl(
    "collection forced block failure", fila$motivo,
    fixed = TRUE, useBytes = TRUE
  ))
})

# Comprueba que un perfil parcial no se publique como materializacion completa.
test_that("una materializacion completa que falla no publica el tamano del recorte", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE spool_dbi (v INTEGER, sano INTEGER)")
  DBI::dbExecute(con, "INSERT INTO spool_dbi VALUES (1,10),(2,20),(3,30),(4,40),(5,50)")
  estado <- new.env(parent = emptyenv())
  estado$tamanos <- integer()
  assign("estado_dbi_tamanos", estado, envir = .GlobalEnv)
  on.exit(rm("estado_dbi_tamanos", envir = .GlobalEnv), add = TRUE)
  trace("perfilar", tracer = quote({
    estado_dbi_tamanos$tamanos <- c(estado_dbi_tamanos$tamanos, nrow(datos))
    if (nrow(datos) > 2L) stop("full materialization failed", call. = FALSE)
  }), where = asNamespace("lupa"), print = FALSE)
  on.exit(untrace("perfilar", where = asNamespace("lupa")), add = TRUE)
  r <- suppressWarnings(suppressMessages(lupa::perfilar_dbi(
    con, "spool_dbi", universo = "muestra_motor", muestra_motor = 5,
    muestra = 2, bloque_muestra = "con_muestra", metricas = "validos",
    proteger_datos_personales = FALSE, instrumentar = FALSE
  )))
  fila <- r$resumen_tabla$columnas
  fila <- fila[fila$columna == "v", , drop = FALSE]
  expect_true(all(c(2L, 5L) %in% estado$tamanos))
  expect_true(is.na(fila$n[[1L]]))
})

# Comprueba que la fraccion pedida no desaparezca al cambiar de muestra.
test_that("la fraccion solicitada de la muestra sobrevive a un reintento", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE retry_dbi (id INTEGER, v INTEGER)")
  DBI::dbExecute(con, "INSERT INTO retry_dbi SELECT i, i FROM range(1, 21) t(i)")
  fuentes <- 0L
  candidato <- lupa:::.candidatos_muestreo_dbi(
    con, lupa:::.dialectos_dbi()$limit
  )[["tablesample_reservoir"]]
  testthat::local_mocked_bindings(
    .sondar_muestreo_dbi = function(...) list(
      disponible = TRUE, candidato = candidato, sondas = character(),
      motivo = NA_character_
    ),
    .fuente_muestreada_dbi = function(tabla_sql, campos_sql, muestra,
                                      n_total, dialecto, resolucion) {
      fuentes <<- fuentes + 1L
      limite <- if (fuentes == 1L) 1L else 5L
      list(sql = paste0("SELECT ", paste(campos_sql, collapse = ", "),
        " FROM retry_dbi LIMIT ", limite), filas = -1L,
        metodo = "tablesample_reservoir",
        descripcion = "TABLESAMPLE RESERVOIR (n ROWS)",
        fraccion = as.numeric(muestra) / 20,
        filas_solicitadas = min(as.numeric(muestra), 20),
        filas_pedidas = as.numeric(muestra))
    },
    .package = "lupa"
  )
  r <- suppressWarnings(suppressMessages(lupa::perfilar_dbi(
    con, "retry_dbi", universo = "muestra_motor", muestra_motor = 10,
    muestra = 10, bloque_muestra = "solo_agregados", metricas = "validos",
    incluir_valores = TRUE, proteger_datos_personales = FALSE,
    instrumentar = FALSE
  )))
  expect_equal(as.numeric(r$resumen_tabla$meta$muestreo$fraccion_solicitada), 0.5)
})

# Comprueba que el plan declare las mismas consultas que emite la corrida.
test_that("el replan por cardinalidad cuenta las mismas consultas que la corrida", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE plan_dbi (id INTEGER, v DECIMAL(18,2))")
  DBI::dbExecute(con, "INSERT INTO plan_dbi SELECT i, i FROM range(1, 101) t(i)")
  args <- list(conexion = con, tabla = "plan_dbi",
    metricas = c("validos", "desvio"), incluir_valores = TRUE,
    politica_costo = "por_cardinalidad", proteger_datos_personales = FALSE,
    instrumentar = FALSE)
  r <- suppressWarnings(suppressMessages(do.call(lupa::perfilar_dbi, args)))
  p <- suppressWarnings(suppressMessages(do.call(
    lupa::plan_perfilado_dbi, args[!names(args) %in% "proteger_datos_personales"]
  )))
  expect_identical(
    as.numeric(r$resumen_tabla$meta$plan$n_consultas_max),
    as.numeric(p$n_consultas_max)
  )

  # El banco mezcla texto y columnas con cardinalidad baja y alta para cubrir
  # la eleccion de moda y mediana desde una hasta treinta columnas.
  skip_if_not_installed("RSQLite")
  for (motor in c("sqlite", "duckdb")) {
    conexion_banco <- if (identical(motor, "sqlite")) {
      DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    } else {
      DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
    }
    for (con_texto in c(FALSE, TRUE)) {
      for (cantidad in seq_len(30L)) {
        nombre <- paste0("banco_", motor, "_", con_texto, "_", cantidad)
        datos <- lapply(seq_len(cantidad), function(i) {
          if (isTRUE(con_texto) && i == 1L) {
            paste0("texto_", seq_len(24L))
          } else if (i %% 2L) {
            seq_len(24L) + i / 100
          } else {
            rep(seq_len(3L), length.out = 24L)
          }
        })
        datos <- as.data.frame(stats::setNames(datos, paste0("c", seq_len(cantidad))))
        DBI::dbWriteTable(conexion_banco, nombre, datos, overwrite = TRUE)
        for (politica in c("todas", "por_cardinalidad")) {
          argumentos <- list(
            conexion = conexion_banco, tabla = nombre,
            metricas = c("validos", "distintos", "moda", "mediana"),
            incluir_valores = TRUE, politica_costo = politica,
            instrumentar = FALSE
          )
          corrida_banco <- suppressWarnings(suppressMessages(do.call(
            lupa::perfilar_dbi, c(
              argumentos, list(proteger_datos_personales = FALSE)
            )
          )))
          expect_identical(
            as.numeric(sum(corrida_banco$resumen_tabla$meta$plan$n_consultas)),
            as.numeric(corrida_banco$resumen_tabla$meta$consultas$emitidas)
          )
        }
      }
    }
    if (identical(motor, "sqlite")) {
      DBI::dbDisconnect(conexion_banco)
    } else {
      DBI::dbDisconnect(conexion_banco, shutdown = TRUE)
    }
  }
})

# Comprueba que una lista de formas aleatorias reciba una sola decision de costo.
test_that("la guardia de costo de una muestra aleatoria decide una sola vez para cualquier lista de candidatas", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "newid_dbi", data.frame(
    id = seq_len(200000L), value = seq_len(200000L)
  ))
  run <- function(multiples) {
    candidato <- list(
      nombre = "random_limit", descripcion = "NEWID", patron = NA_character_,
      tipo = "aleatorio", funciones = if (multiples) list(
        list(nombre = "newid", patron = "sql server", sql = "RANDOM()"),
        list(nombre = "random", patron = NA_character_, sql = "RANDOM()")
      ) else list(
        list(nombre = "newid", patron = "sql server", sql = "RANDOM()")
      )
    )
    testthat::local_mocked_bindings(
      .senas_conexion_dbi = function(...) "microsoft sql server",
      .sondar_muestreo_dbi = function(...) list(
        disponible = TRUE, candidato = candidato, sondas = character(),
        motivo = NA_character_
      ),
      .fuente_muestreada_dbi = function(tabla_sql, campos_sql, muestra,
                                        n_total, dialecto, resolucion) list(
        sql = paste0("SELECT ", paste(campos_sql, collapse = ", "),
          " FROM newid_dbi ORDER BY RANDOM() LIMIT 5"), filas = -1L,
        metodo = "random_limit", descripcion = "NEWID", funcion = "newid",
        fraccion = 5 / 200000, filas_solicitadas = 5, filas_pedidas = 5
      ),
      .package = "lupa"
    )
    suppressWarnings(suppressMessages(lupa::perfilar_dbi(
      con, "newid_dbi", universo = "muestra_motor", muestra_motor = 5,
      muestra = 5, bloque_muestra = "solo_agregados", dialecto = "portable",
      metricas = c("validos", "mediana"), incluir_valores = TRUE,
      proteger_datos_personales = FALSE, instrumentar = FALSE
    )))
  }
  uno <- run(FALSE)
  dos <- run(TRUE)
  estado_uno <- uno$resumen_tabla$meta$muestreo$disponible
  estado_dos <- dos$resumen_tabla$meta$muestreo$disponible
  expect_false(isTRUE(estado_uno))
  expect_false(isTRUE(estado_dos))
  sql_uno <- uno$resumen_tabla$sql[uno$resumen_tabla$sql$columna == "value" &
    uno$resumen_tabla$sql$metrica %in% c("n_validos", "mediana"),
    c("metrica", "estado"), drop = FALSE]
  sql_dos <- dos$resumen_tabla$sql[dos$resumen_tabla$sql$columna == "value" &
    dos$resumen_tabla$sql$metrica %in% c("n_validos", "mediana"),
    c("metrica", "estado"), drop = FALSE]
  expect_identical(sql_uno$estado, sql_dos$estado)
})

# Comprueba que un error de fuente no se confunda con una columna ilegible.
test_that("un error de la fuente conserva su origen y la columna ilegible conserva sus filas SQL", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE source_dbi (v INTEGER, sano INTEGER)")
  DBI::dbExecute(con, "INSERT INTO source_dbi VALUES (1,10),(2,20),(3,30)")
  testthat::local_mocked_bindings(
    .fuente_muestreada_dbi = function(...) stop(
      "deliberate source failure", call. = FALSE
    ),
    .package = "lupa"
  )
  r <- suppressWarnings(suppressMessages(lupa::perfilar_dbi(
    con, "source_dbi", universo = "muestra_motor", muestra_motor = 2,
    muestra = 2, bloque_muestra = "con_muestra", metricas = "validos",
    proteger_datos_personales = FALSE, instrumentar = FALSE
  )))
  expect_true(grepl(
    "error_fuente_muestreada:deliberate source failure",
    r$resumen_tabla$meta$muestreo$motivo_filas_obtenidas,
    fixed = TRUE
  ))
  r_solo <- suppressWarnings(suppressMessages(lupa::perfilar_dbi(
    con, "source_dbi", universo = "muestra_motor", muestra_motor = 2,
    muestra = 2, bloque_muestra = "solo_agregados", metricas = "validos",
    proteger_datos_personales = FALSE, instrumentar = FALSE
  )))
  expect_true(grepl(
    "error_fuente_muestreada:deliberate source failure",
    r_solo$resumen_tabla$meta$muestreo$motivo_filas_obtenidas,
    fixed = TRUE
  ))
  con43 <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con43, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con43, "CREATE TABLE illegible_dbi (rota INTEGER, sana INTEGER)")
  DBI::dbExecute(con43, "INSERT INTO illegible_dbi VALUES (1,10),(2,20)")
  catalogo <- list(
    campos = c("rota", "sana"), ilegibles = "rota", enmascaradas = character(),
    mascaras = stats::setNames(character(), character()), proyectar = TRUE,
    catalogo = TRUE, sql_catalogo = NA_character_,
    tipos_motor = stats::setNames(c("INTEGER", "INTEGER"), c("rota", "sana")),
    tipo_objeto = NA_character_, error = NULL
  )
  r43 <- local({
    testthat::local_mocked_bindings(
      .columnas_tabla_dbi = function(...) catalogo,
      .sondar_muestreo_dbi = function(...) list(
        disponible = FALSE, candidato = NULL, sondas = character(),
        motivo = "capacidad_no_aceptada:test"
      ),
      .package = "lupa"
    )
    suppressWarnings(suppressMessages(lupa::perfilar_dbi(
      con43, "illegible_dbi", universo = "muestra_motor", muestra_motor = 2,
      muestra = 2, metricas = c("validos", "basicos"),
      bloque_muestra = "con_muestra", proteger_datos_personales = FALSE,
      instrumentar = FALSE
    )))
  })
  sql_rota <- r43$resumen_tabla$sql[
    r43$resumen_tabla$sql$columna == "rota", , drop = FALSE
  ]
  expect_gt(nrow(sql_rota), 0L)
})

# Comprueba que el motivo de precision se conserve solo dentro del spool.
test_that("el motivo del decimal ancho queda dentro de la via del spool", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE decimal_reason_dbi (v DECIMAL(38,0))")
  DBI::dbExecute(con, paste0(
    "INSERT INTO decimal_reason_dbi SELECT CAST(12345678901234567890000 + i ",
    "AS DECIMAL(38,0)) FROM range(0, 30) t(i)"
  ))
  spool <- suppressWarnings(suppressMessages(lupa::perfilar_dbi(
    con, "decimal_reason_dbi", fecha = "2026-01-15",
    universo = "muestra_motor", muestra_motor = 1000,
    bloque_muestra = "con_muestra", proteger_datos_personales = FALSE,
    instrumentar = FALSE
  )))
  filas <- spool$resumen_tabla$sql[
    spool$resumen_tabla$sql$metrica %in%
      c("minimo", "media", "mediana", "desvio"), , drop = FALSE
  ]
  motivo_decimal <- paste(
    "La muestra conserva los valores exactos de un decimal ancho, pero",
    "sus m\u00e9tricas cuantitativas no caben en un doble sin perder cifras;",
    "no se publica la m\u00e9trica para no redondearla."
  )
  expect_true(all(as.character(filas$estado) == "no_disponible"))
  expect_true(all(as.character(filas$motivo) == motivo_decimal))
  fila_maximo <- spool$resumen_tabla$sql[
    spool$resumen_tabla$sql$metrica == "maximo", , drop = FALSE
  ]
  expect_identical(as.character(fila_maximo$estado), "no_disponible")
  expect_identical(
    as.character(fila_maximo$motivo),
    paste(
      "La consulta termin\u00f3 sin un valor para la m\u00e9trica; no se",
      "publica como calculada."
    )
  )

  DBI::dbExecute(con, "CREATE TABLE empty_text_reason_dbi (texto VARCHAR)")
  DBI::dbExecute(con, "INSERT INTO empty_text_reason_dbi VALUES ('')")
  normal <- suppressWarnings(suppressMessages(lupa::perfilar_dbi(
    con, "empty_text_reason_dbi", fecha = "2026-01-15",
    proteger_datos_personales = FALSE, instrumentar = FALSE
  )))
  fila <- normal$resumen_tabla$sql[
    normal$resumen_tabla$sql$columna == "texto" &
      normal$resumen_tabla$sql$metrica == "moda", , drop = FALSE
  ]
  expect_identical(as.character(fila$estado), "no_disponible")
  expect_identical(
    as.character(fila$motivo),
    paste(
      "La consulta termin\u00f3 sin un valor para la m\u00e9trica; no se",
      "publica como calculada."
    )
  )
})

# El desvio que el motor calcula en una pasada desborda con valores de mas de
# 1e150: la suma de cuadrados no entra en un doble. SQLite y DuckDB publicaban
# `Inf` como `calculado` en la llamada por omision, y `0` sobre valores distintos
# con magnitudes de 1e-200, porque el repliegue escalado estaba atado a una lista
# de motores que no los nombraba. La ayuda promete que no se publica ni un `Inf`
# ni un `0` sobre valores distintos, en ningun motor.
.adc_desvio_publicado <- function(conexion, x, ...) {
  DBI::dbWriteTable(conexion, "extremos", data.frame(x = x), overwrite = TRUE)
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, "extremos", fecha = "2026-01-15", ...
  )))
  filas <- perfil$resumen_tabla$sql
  fila <- filas[filas$columna == "x" & filas$metrica == "desvio", , drop = FALSE]
  list(valor = perfil$resumen_tabla$columnas$desvio[[1L]],
       estado = fila$estado[[1L]])
}

.adc_afirmar_desvio_sano <- function(publicado, esperado) {
  afirma <- publicado$estado %in% c("calculado", "estimado")
  if (afirma) {
    expect_true(is.finite(publicado$valor))
    expect_gt(publicado$valor, 0)
    expect_lt(abs(publicado$valor - esperado) / esperado, 1e-9)
  } else {
    # Negarlo es correcto; lo que no puede pasar es afirmar un valor falso.
    expect_true(is.na(publicado$valor))
  }
}

# Comprueba que los extremos no se publiquen como un numero falso.
test_that("el desvio de magnitudes extremas no sale Inf ni 0 en SQLite ni en DuckDB", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  base <- 1 + (1:40) / 100
  for (motor in c("sqlite", "duckdb")) {
    for (magnitud in c(1e200, 1e-200)) {
      x <- magnitud * base
      esperado <- stats::sd(base) * magnitud
      for (metricas in list(NULL, "desvio")) {
        conexion <- if (identical(motor, "sqlite")) {
          DBI::dbConnect(RSQLite::SQLite(), ":memory:")
        } else {
          DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
        }
        publicado <- if (is.null(metricas)) {
          .adc_desvio_publicado(conexion, x)
        } else {
          .adc_desvio_publicado(conexion, x, metricas = metricas)
        }
        if (identical(motor, "duckdb")) {
          DBI::dbDisconnect(conexion, shutdown = TRUE)
        } else {
          DBI::dbDisconnect(conexion)
        }
        .adc_afirmar_desvio_sano(publicado, esperado)
      }
    }
  }
})

# El desvio de magnitudes extremas tiene dos formas de fallar, y cual toca
# depende del motor. SQL Server y PostgreSQL cortan la consulta ("Arithmetic
# overflow error", "value out of range: overflow"); SQLite y DuckDB la
# responden con `Inf`. Por eso una consulta de desvio fallida quiere decir dos
# cosas: en los primeros, que la suma de cuadrados no entro en un doble, y la
# respuesta es la forma escalada; en los segundos, que el motor rechazo la
# columna, y repetirla escalada es otra sentencia que falla. Con una sola regla
# para todos, o SQL Server publicaba dos consultas fallidas y un aviso sobre un
# desvio que si calculaba, o una vista de DuckDB con columnas ilegibles mandaba
# una sentencia de mas por cada una. Sin un motor real no se ve: aca se ejerce
# la decision con las respuestas de cada motor fingidas.
.adc_desvio_ante <- function(senas, respuesta_normal) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sentencias <- character()
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(...) senas,
    .escalar_dbi = function(conexion, sql, campo, presupuesto = NULL,
                            etapa = "consulta") {
      sentencias <<- c(sentencias, sql)
      # La forma escalada se reconoce por la relacion derivada que arma.
      if (grepl("lupa_centrada", sql, fixed = TRUE)) {
        return(list(ok = TRUE, valor = 5))
      }
      respuesta_normal
    },
    .package = "lupa"
  )
  resultado <- .desvio_columna_dbi(
    con, "t", "x", function(nombre) nombre, 2L, centro = 1
  )
  list(sentencias = length(sentencias), resultado = resultado)
}

# Comprueba que el repliegue responda a lo que cada motor hace con el desborde.
test_that("una consulta de desvio fallida se responde escalada solo donde el desborde es un error", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  fallida <- list(ok = FALSE, valor = NULL, motivo = "desborde aritmetico")
  for (senas in c("microsoft sql server", "postgres")) {
    r <- .adc_desvio_ante(senas, fallida)
    expect_identical(r$sentencias, 2L, info = senas)
    expect_true(isTRUE(r$resultado$ok), info = senas)
    expect_identical(r$resultado$valor, 5, info = senas)
  }
  for (senas in c("sqlite", "duckdb")) {
    r <- .adc_desvio_ante(senas, fallida)
    expect_identical(r$sentencias, 1L, info = senas)
    expect_false(isTRUE(r$resultado$ok), info = senas)
    expect_identical(r$resultado$motivo, "desborde aritmetico", info = senas)
    # Lo que si se recalcula ahi es la respuesta que llego y no es finita.
    r <- .adc_desvio_ante(senas, list(ok = TRUE, valor = Inf))
    expect_identical(r$sentencias, 2L, info = senas)
    expect_identical(r$resultado$valor, 5, info = senas)
  }
  # Control: una respuesta sana no se repite en ningun motor.
  for (senas in c("microsoft sql server", "postgres", "sqlite", "duckdb")) {
    r <- .adc_desvio_ante(senas, list(ok = TRUE, valor = 3))
    expect_identical(r$sentencias, 1L, info = senas)
    expect_identical(r$resultado$valor, 3, info = senas)
  }
})

.adc_conexion_sqlserver <- function() {
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  con
}

# Lo mismo contra el motor, por las dos llamadas que las pruebas de la escala
# no ejercian con dobles extremos: la llamada por omision y `desvio` solo. Se
# saltea sin un SQL Server declarado, como sus hermanas.
test_that("SQL Server publica el desvio de dobles extremos sin consultas fallidas, tambien por omision", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  con <- .adc_conexion_sqlserver()
  tabla <- "adc_extremos_sqlserver"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
      silent = TRUE)
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (id int, x float)"))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES (1, 1e300), (2, 0), (3, 0), (4, 0)"
  ))
  # A mano: la media es 2,5e299 y los desvios al cuadrado suman 7,5e599, que
  # entre 3 da 2,5e599; su raiz es 5e299.
  esperado <- 5e299
  for (metricas in list(NULL, "desvio")) {
    argumentos <- list(
      con, tabla, universo = "tabla_completa",
      proteger_datos_personales = FALSE, analizar_dependencias = FALSE
    )
    if (!is.null(metricas)) argumentos$metricas <- metricas
    avisos <- character()
    perfil <- withCallingHandlers(
      suppressMessages(do.call(perfilar_dbi, argumentos)),
      warning = function(w) {
        avisos <<- c(avisos, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    info <- if (is.null(metricas)) "por omision" else "desvio solo"
    filas <- perfil$resumen_tabla$sql
    fila <- filas[filas$columna == "x" & filas$metrica == "desvio", ,
                  drop = FALSE]
    publicado <- perfil$resumen_tabla$columnas$desvio[
      perfil$resumen_tabla$columnas$columna == "x"
    ]
    expect_identical(as.character(fila$estado), "calculado", info = info)
    expect_lt(abs(publicado / esperado - 1), 1e-12, label = info)
    expect_identical(
      nrow(perfil$resumen_tabla$meta$consultas$fallas), 0L, info = info
    )
    expect_length(avisos, 0L)
  }
})

# Una columna que el motor no puede leer hace fallar la lectura de la muestra
# del motor entera. Lo que queda es el perfil aislado: otra lectura, sin esa
# columna. La ayuda dice que hace el resumen con el: las metricas quedan
# `no_disponible`, con `tamano_muestra` y `fraccion` en `NA`, porque no es la
# materializacion que se pidio. Pero una columna rota no les saca sus cuentas a
# las sanas: `resumen_tabla$columnas` las conserva. Las dos cosas valen a la
# vez, y se rompieron de a una: las cuentas del perfil aislado salian junto al
# total de la tabla (los validos de dos filas al lado de `n = 5`), y al
# corregirlo las metricas pasaron a darse por observadas en la muestra.
.adc_perfil_con_columna_rota <- function(muestra) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "adc_aislada", data.frame(
    id = 1:5, rota = 11:15, nombre = paste0("n", 1:5),
    stringsAsFactors = FALSE
  ))
  enviar <- get(".enviar_consulta_dbi", envir = asNamespace("lupa"))
  # El motor rechaza toda sentencia que lea `rota`; el esquema se deja pasar.
  rechazar <- function(sql) {
    if (grepl("adc_aislada", sql, fixed = TRUE) &&
        grepl("rota", sql, fixed = TRUE) &&
        !grepl("WHERE 1 = 0", sql, fixed = TRUE)) {
      stop("conversion invalida en rota", call. = FALSE)
    }
  }
  testthat::local_mocked_bindings(
    .traer_consulta_dbi = function(conexion, sql) {
      rechazar(sql)
      DBI::dbGetQuery(conexion, sql)
    },
    .enviar_consulta_dbi = function(conexion, sql) {
      rechazar(sql)
      enviar(conexion, sql)
    },
    .package = "lupa"
  )
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, "adc_aislada", universo = "muestra_motor", muestra_motor = 3,
    muestra = muestra, metricas = "validos", incluir_valores = FALSE,
    instrumentar = FALSE, proteger_datos_personales = FALSE
  )))
}

# Comprueba que el perfil aislado no pase por resumen ni pierda sus cuentas.
test_that("el perfil aislado de la muestra del motor conserva sus cuentas y no se publica como resumen", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # Con `muestra` igual a `muestra_motor` lo aislado trae las tres filas
  # pedidas; con `muestra` menor, dos. La regla es la misma en los dos casos.
  for (muestra in c(3, 2)) {
    perfil <- .adc_perfil_con_columna_rota(muestra)
    columnas <- perfil$resumen_tabla$columnas
    filas <- perfil$resumen_tabla$sql
    for (columna in c("id", "nombre")) {
      info <- paste(columna, "con muestra =", muestra)
      fila <- columnas[columnas$columna == columna, , drop = FALSE]
      # Las cuentas son las de las filas que se perfilaron, y `n` tambien.
      expect_equal(fila$n_validos, muestra, info = info)
      expect_equal(fila$n, muestra, info = info)
      registro <- filas[
        filas$columna == columna & filas$metrica == "n_validos", ,
        drop = FALSE
      ]
      expect_identical(as.character(registro$estado), "no_disponible",
                       info = info)
      expect_true(is.na(registro$tamano_muestra), info = info)
      expect_true(is.na(registro$fraccion), info = info)
    }
    expect_true(is.na(columnas$n_validos[columnas$columna == "rota"]))
    cobertura <- perfil$resumen_tabla$cobertura
    expect_identical(
      as.character(cobertura$elemento[
        cobertura$bloque == "resumen_tabla" &
          cobertura$estado == "no_disponible"
      ]),
      "id, rota, nombre"
    )
    expect_setequal(
      as.character(perfil$perfil_muestra$columnas$columna), c("id", "nombre")
    )
  }
})

# Construir el objeto no es publicarlo: con el perfil aislado no hay un conteo
# de las filas que devolvio la muestra, y la linea que lo anuncia abortaba la
# impresion entera porque no hay cantidad que poner en plural.
test_that("el perfil aislado de la muestra del motor se imprime", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  for (muestra in c(3, 2)) {
    perfil <- .adc_perfil_con_columna_rota(muestra)
    consola <- paste(cli::cli_fmt(print(perfil)), collapse = " ")
    expect_match(consola, "Perfil de muestra: sin conteo de filas",
                 fixed = TRUE, info = paste("muestra =", muestra))
  }
})

# Bajo la muestra del motor, `n` es el total de la tabla -lo dice la ayuda- y
# las demas cuentas son de la muestra. Al corregir el caso en que eso confunde
# (un spool recortado por el presupuesto publicaba los validos de tres filas
# junto a `n = 12`), `n` paso a ser el tamano de la muestra en toda corrida por
# el spool, tambien en las sanas: dejo de coincidir con la ayuda y con la via
# de solo agregados sobre la misma tabla. La regla tiene las dos mitades.
.adc_n_bajo_muestra <- function(motor, ...) {
  con <- if (identical(motor, "sqlite")) {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  } else {
    DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  }
  on.exit(if (identical(motor, "duckdb")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(
    id = 1:12, x = c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10, 11, 12), y = (1:12) * 10
  ))
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "t", universo = "muestra_motor", proteger_datos_personales = FALSE,
    ...
  )))
  list(
    n = perfil$resumen_tabla$columnas$n,
    validos = perfil$resumen_tabla$columnas$n_validos,
    materializacion = perfil$resumen_tabla$meta$materializacion
  )
}

# Comprueba que `n` sea el total de la tabla salvo que lo perfilado sea menos.
test_that("bajo la muestra del motor `n` es el total de la tabla, salvo que el spool quede recortado", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  for (motor in c("sqlite", "duckdb")) {
    # Seis filas de doce, sin recortes: el total sigue siendo doce, por el
    # spool y por la via de solo agregados.
    sana <- .adc_n_bajo_muestra(motor, muestra_motor = 6)
    expect_identical(as.numeric(sana$materializacion$n_filas), 6, info = motor)
    expect_false(isTRUE(sana$materializacion$recortada), info = motor)
    expect_equal(sana$n, rep(12, 3), info = motor)
    expect_true(all(sana$validos <= 6), info = motor)
    agregados <- .adc_n_bajo_muestra(
      motor, muestra_motor = 6, bloque_muestra = "solo_agregados"
    )
    expect_equal(agregados$n, rep(12, 3), info = motor)
  }
  # El presupuesto de bytes corta el spool antes de las doce filas pedidas:
  # lo perfilado son las que entraron, y `n` las cuenta a ellas.
  recortada <- .adc_n_bajo_muestra(
    "sqlite", muestra_motor = 12, max_bytes_materializacion = 6500
  )
  entraron <- as.numeric(recortada$materializacion$n_filas)
  expect_true(isTRUE(recortada$materializacion$recortada))
  expect_lt(entraron, 12)
  expect_equal(recortada$n, rep(entraron, 3))
  expect_true(all(recortada$validos <= entraron))
})

# Un desvio 0 sobre valores distintos es imposible y no se publica. Donde el
# motor no escalaba (SQLite, DuckDB) antes de negarlo se vuelve a pedir
# escalado, que es como se recupera el de magnitudes diminutas. Esa segunda
# sentencia no es gratis: el plan no la cuenta. En los motores que ya escalaban
# el 0 se negaba sin ella, y asi tiene que seguir.
.adc_pedidos_de_desvio <- function(senas = NULL) {
  set.seed(9)
  # Media enorme frente al ancho: el caso en que una pasada responde 0.
  datos <- data.frame(
    id = 1:61, ms = 1760000000000 + round(stats::runif(61, 0, 1000))
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", datos)
  pedidos <- logical()
  testthat::local_mocked_bindings(
    # El motor responde 0 a toda consulta de desvio por columna.
    .desvio_columna_dbi = function(conexion, tabla_sql, columna_sql, ...,
                                   forzar_escalado = FALSE) {
      if (grepl("ms", columna_sql, fixed = TRUE)) {
        pedidos <<- c(pedidos, isTRUE(forzar_escalado))
      }
      list(ok = TRUE, valor = 0, sql = "SELECT 0")
    },
    .package = "lupa"
  )
  if (!is.null(senas)) {
    testthat::local_mocked_bindings(
      .senas_conexion_dbi = function(...) senas, .package = "lupa"
    )
  }
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "t", metricas = c("basicos", "desvio"), universo = "tabla_completa",
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE
  )))
  filas <- perfil$resumen_tabla$sql
  list(
    pedidos = pedidos,
    estado = as.character(filas$estado[
      filas$columna == "ms" & filas$metrica == "desvio"
    ]),
    desvio = perfil$resumen_tabla$columnas$desvio[
      perfil$resumen_tabla$columnas$columna == "ms"
    ]
  )
}

# Comprueba que negar el 0 imposible no cueste una sentencia donde no costaba.
test_that("un desvio 0 sobre valores distintos se vuelve a pedir escalado solo en los motores que no escalaban", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  for (senas in c("mysql", "mariadb")) {
    r <- .adc_pedidos_de_desvio(senas)
    expect_identical(r$pedidos, FALSE, info = senas)
    expect_identical(r$estado, "no_disponible", info = senas)
    expect_true(is.na(r$desvio), info = senas)
  }
  # SQLite, con sus propias senas: el segundo pedido es el escalado. Y como el
  # motor fingido vuelve a responder 0, tampoco ahi se publica.
  r <- .adc_pedidos_de_desvio()
  expect_identical(r$pedidos, c(FALSE, TRUE))
  expect_identical(r$estado, "no_disponible")
  expect_true(is.na(r$desvio))
})
