skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

.conexion_i1_postgres <- function() {
  skip_if_not_installed("RPostgres")
  conexion <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = 55432,
      dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"),
      user = "postgres", password = Sys.getenv("LUPA_PG_PASSWORD", "lupa")
    ),
    error = function(e) NULL
  )
  if (is.null(conexion)) skip("PostgreSQL real no disponible en el puerto 55432.")
  conexion
}

test_that("I1 conserva la identidad de los acumuladores con uno o muchos bloques", {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  datos <- data.frame(
    id = 1:8,
    numero = c(NA, NaN, -Inf, Inf, 2, 2, -1, 0),
    texto = c("a", "a", NA, "b", "c", "b", "", "a"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conexion, "fuente_i1", datos)
  referencia <- lupa::perfilar(
    DBI::dbReadTable(conexion, "fuente_i1"),
    proteger_datos_personales = FALSE
  )$columnas
  referencia$n_validos <- referencia$n - referencia$n_faltantes
  columnas <- c(
    "columna", "n", "n_validos", "n_faltantes", "prop_faltantes",
    "n_distintos", "tasa_distintos", "moda", "frecuencia_moda", "minimo",
    "maximo", "media", "mediana", "desvio", "n_ceros", "n_negativos"
  )
  una <- lupa::perfilar_dbi(
    conexion, "fuente_i1", bloque_filas = 100L,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
  )
  muchas <- lupa::perfilar_dbi(
    conexion, "fuente_i1", bloque_filas = 2L,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
  )
  expect_identical(una$resumen_tabla$columnas[, columnas],
                   muchas$resumen_tabla$columnas[, columnas])
  expect_identical(muchas$resumen_tabla$columnas[, columnas],
                   referencia[, columnas])
  numero <- muchas$resumen_tabla$columnas[
    match("numero", muchas$resumen_tabla$columnas$columna), , drop = FALSE
  ]
  texto <- muchas$resumen_tabla$columnas[
    match("texto", muchas$resumen_tabla$columnas$columna), , drop = FALSE
  ]
  expect_identical(as.numeric(numero$n_nan), as.numeric(
    referencia$n_nan[match("numero", referencia$columna)]
  ))
  expect_identical(as.numeric(numero$n_infinito_positivo), as.numeric(
    referencia$n_infinito_positivo[match("numero", referencia$columna)]
  ))
  expect_identical(as.numeric(numero$n_infinito_negativo), as.numeric(
    referencia$n_infinito_negativo[match("numero", referencia$columna)]
  ))
  expect_identical(as.numeric(texto$longitud_media), as.numeric(
    referencia$longitud_media[match("texto", referencia$columna)]
  ))
  expect_identical(muchas$resumen_tabla$meta$bloques$recorridos, 4L)
  expect_identical(muchas$resumen_tabla$meta$bloques$filas_vistas, 8)
  expect_true(muchas$resumen_tabla$meta$bloques$fetches >= 4L)
  expect_true(muchas$resumen_tabla$meta$bytes$retenidos > 0)
  expect_true(nrow(muchas$resumen_tabla$meta$eventos) >= 4L)
  expect_match(
    muchas$resumen_tabla$meta$nota_inf,
    "representacion del controlador",
    fixed = TRUE
  )
})

test_that("I1 inicia el mapa para moda y mediana sin publicar n_distintos", {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE valores (id INTEGER PRIMARY KEY, v REAL)")
  DBI::dbExecute(conexion,
                 "INSERT INTO valores VALUES (1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6)")

  mediana <- lupa::perfilar_dbi(
    conexion, "valores", metricas = "mediana", bloque_filas = 2L,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE,
    instrumentar = FALSE
  )$resumen_tabla
  moda <- lupa::perfilar_dbi(
    conexion, "valores", metricas = "moda", bloque_filas = 2L,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE,
    instrumentar = FALSE
  )$resumen_tabla
  fila_mediana <- mediana$columnas[mediana$columnas$columna == "v", , drop = FALSE]
  fila_moda <- moda$columnas[moda$columnas$columna == "v", , drop = FALSE]
  distinto_mediana <- mediana$sql[
    mediana$sql$columna == "v" & mediana$sql$metrica == "n_distintos", , drop = FALSE
  ]
  distinto_moda <- moda$sql[
    moda$sql$columna == "v" & moda$sql$metrica == "n_distintos", , drop = FALSE
  ]

  expect_equal(fila_mediana$mediana, 3.5)
  expect_equal(fila_moda$moda, "1")
  expect_identical(distinto_mediana$estado, "no_solicitado")
  expect_identical(distinto_moda$estado, "no_solicitado")
  expect_identical(distinto_mediana$motivo, "La metrica no fue solicitada.")
  expect_identical(distinto_moda$motivo, "La metrica no fue solicitada.")
})

test_that("I1 distingue mapa truncado del tope de reconstruccion de mediana", {
  mapa <- data.frame(
    representante = seq_len(10L), frecuencia = rep(120000, 10L)
  )
  sobre <- list(estado = "calculado", resultado = mapa, motivo = NA_character_)
  acumuladores <- list()
  acumuladores[[paste("v", "distintos", sep = "\u001f")]] <- sobre
  fuente <- list(consulta = "SELECT v FROM valores", campos = "v")
  resultado <- lupa:::.fila_y_registros_bloques_dbi(
    "v", 1200000, "mediana", acumuladores, list(1), "REAL", TRUE,
    fuente
  )
  registro <- resultado$sql[resultado$sql$metrica == "mediana", , drop = FALSE]

  expect_identical(registro$estado, "no_disponible")
  expect_identical(
    registro$motivo,
    "familia_sin_acumulador:mediana_bloques_supera_tope_reconstruccion:1000000"
  )
  expect_false(grepl("mapa_distintos_truncado", registro$motivo, fixed = TRUE))
})

test_that("I1 aplica la politica de costo a la moda del mapa", {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "cardinal", data.frame(v = seq_len(1000L)))
  resultado <- lupa::perfilar_dbi(
    conexion, "cardinal", metricas = c("validos", "distintos", "moda"),
    bloque_filas = 100L, bloque_muestra = "solo_agregados",
    politica_costo = "por_cardinalidad", umbral_cardinalidad = 0.01,
    proteger_datos_personales = FALSE, instrumentar = FALSE
  )$resumen_tabla
  moda <- resultado$sql[resultado$sql$metrica == "moda", , drop = FALSE]

  expect_true(all(moda$estado == "omitido_por_costo"))
  expect_true(all(grepl("politica optativa", moda$motivo, fixed = TRUE)))
  expect_true(is.list(resultado$meta$decisiones_costo))
  expect_true(all(vapply(
    resultado$meta$decisiones_costo, function(x) identical(x$moda, FALSE),
    logical(1L)
  )))
})

test_that("I1 registra n, memoria acotada y las dos pasadas del localizador", {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "sin_clave", data.frame(valor = seq_len(7L)))
  resultado <- lupa::perfilar_dbi(
    conexion, "sin_clave", metricas = "validos", bloque_filas = 2L,
    orden_muestra = "valor", bloque_muestra = "con_muestra", muestra = 3L,
    proteger_datos_personales = FALSE, instrumentar = FALSE
  )$resumen_tabla
  sql <- resultado$sql
  medidos <- sql[sql$estado == "calculado", , drop = FALSE]

  expect_true("n" %in% sql$metrica)
  expect_true(nrow(medidos) > 0L)
  expect_true(all(!is.na(medidos$memoria_trabajo)))
  expect_true(all(medidos$memoria_trabajo == "acotado"))
  expect_identical(resultado$meta$fuente_bloques$metodo_orden, "row_locator")
  expect_false(resultado$meta$orden_muestra$aplicado)
  expect_match(resultado$meta$orden_muestra$motivo, "no_gobierna_fuente_bloques")
  expect_true(resultado$meta$consultas$emitidas >= 2L)
  expect_true(resultado$meta$consultas$fetches > resultado$meta$bloques$recorridos)
  expect_equal(resultado$meta$plan$n_consultas, resultado$meta$consultas$emitidas)
})

test_that("el preflight prefiere una clave primaria y publica orden completo", {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE ordenada (id INTEGER PRIMARY KEY, valor TEXT)")
  DBI::dbExecute(conexion, "INSERT INTO ordenada VALUES (2, 'b'), (1, 'a')")
  fuente <- lupa:::.fuente_bloques_dbi(
    conexion, "ordenada", bloque_filas = 1L,
    campos = c("id", "valor"),
    campos_sql = c("`id`", "`valor`"),
    clave = lupa:::.clave_primaria_dbi(conexion, "ordenada")
  )
  expect_true(fuente$disponible)
  expect_identical(fuente$metodo_orden, "pk")
  expect_true(fuente$estable)
  expect_match(fuente$orden_id, "collation=", fixed = TRUE)
  expect_match(fuente$orden_id, "deterministic=TRUE", fixed = TRUE)
  expect_match(fuente$consulta, "ORDER BY `id`", fixed = TRUE)
})

test_that("un localizador publica la perdida de identidad ordinal despues de VACUUM", {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "sin_pk_i1", data.frame(valor = 1:12))
  resultado <- lupa::perfilar_dbi(
    conexion, "sin_pk_i1", bloque_filas = 5L,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE,
    antes_segunda_pasada = function() {
      DBI::dbExecute(conexion, "DELETE FROM sin_pk_i1 WHERE valor = 5")
      DBI::dbExecute(conexion, "VACUUM")
    }
  )
  expect_identical(
    resultado$resumen_tabla$meta$segunda_pasada$estado,
    "resultset_no_reproducible"
  )
  expect_true(any(
    resultado$resumen_tabla$cobertura$estado == "degradado" &
      grepl("ordinal_fila_cambio", resultado$resumen_tabla$cobertura$motivo,
            fixed = TRUE)
  ))
})

test_that("DuckDB decide la ausencia de dbFetch incremental en el preflight", {
  skip_if_not_installed("duckdb")
  conexion <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(conexion, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE duck_i1 AS SELECT * FROM range(4)")
  resultado <- lupa::perfilar_dbi(
    conexion, "duck_i1", bloque_filas = 2L,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
  )
  expect_false(resultado$resumen_tabla$meta$fuente_bloques$disponible)
  expect_identical(
    resultado$resumen_tabla$meta$fuente_bloques$motivo,
    "no_disponible:dbfetch_no_incremental"
  )
  expect_identical(resultado$resumen_tabla$meta$bloques$fetches, 0L)
  expect_true(any(grepl(
    "no_disponible:dbfetch_no_incremental",
    resultado$resumen_tabla$cobertura$motivo, fixed = TRUE
  )))
})

test_that("bloque_filas valida la API minima de I1", {
  expect_error(
    lupa::perfilar_dbi(NULL, "x", bloque_filas = 0),
    class = "lupa_error_argumento_dbi"
  )
})

test_that("I1 conserva la identidad contra PostgreSQL real", {
  conexion <- .conexion_i1_postgres()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  tabla <- paste0("lupa_i1_pg_", Sys.getpid())
  on.exit(try(DBI::dbExecute(
    conexion, paste0("DROP TABLE IF EXISTS ", tabla)
  ), silent = TRUE), add = TRUE)
  datos <- data.frame(
    id = 1:8,
    numero = c(NA, NaN, -Inf, Inf, 2, 2, -1, 0),
    texto = c("a", "a", NA, "b", "c", "b", "", "a")
  )
  DBI::dbWriteTable(conexion, tabla, datos)
  referencia <- lupa::perfilar(
    DBI::dbReadTable(conexion, tabla), proteger_datos_personales = FALSE
  )$columnas
  corrida <- lupa::perfilar_dbi(
    conexion, tabla, bloque_filas = 2L,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
  )$resumen_tabla
  indice_numero <- match("numero", corrida$columnas$columna)
  indice_referencia <- match("numero", referencia$columna)
  expect_identical(corrida$columnas$n, referencia$n)
  expect_identical(corrida$columnas$n_validos,
                   referencia$n - referencia$n_faltantes)
  expect_identical(corrida$columnas$n_faltantes, referencia$n_faltantes)
  expect_identical(as.numeric(corrida$columnas$n_nan[[indice_numero]]),
                   as.numeric(referencia$n_nan[[indice_referencia]]))
  expect_identical(as.numeric(corrida$columnas$n_infinito_positivo[[indice_numero]]),
                   as.numeric(referencia$n_infinito_positivo[[indice_referencia]]))
  expect_identical(as.numeric(corrida$columnas$n_infinito_negativo[[indice_numero]]),
                   as.numeric(referencia$n_infinito_negativo[[indice_referencia]]))
  expect_identical(corrida$meta$bloques$recorridos, 4L)
})

test_that("I1 no publica estable una PK textual con collation no determinista", {
  conexion <- .conexion_i1_postgres()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  tabla <- paste0("lupa_i1_collation_", Sys.getpid())
  colacion <- paste0("lupa_i1_nondet_", Sys.getpid())
  on.exit(try(DBI::dbExecute(
    conexion, paste0("DROP TABLE IF EXISTS ", tabla, " CASCADE")
  ), silent = TRUE), add = TRUE)
  on.exit(try(DBI::dbExecute(
    conexion, paste0("DROP COLLATION IF EXISTS ", colacion, " CASCADE")
  ), silent = TRUE), add = TRUE)
  creada <- tryCatch({
    DBI::dbExecute(conexion, paste0(
      "CREATE COLLATION ", colacion,
      " (provider = icu, locale = 'und-u-ks-level1', deterministic = false)"
    ))
    DBI::dbExecute(conexion, paste0(
      "CREATE TABLE ", tabla, " (id text COLLATE ", colacion,
      " PRIMARY KEY, valor integer)"
    ))
    DBI::dbExecute(conexion, paste0(
      "INSERT INTO ", tabla, " VALUES ('a', 1), ('b', 2)"
    ))
    TRUE
  }, error = function(e) {
    skip(paste("PostgreSQL no permite la fixture ICU:", conditionMessage(e)))
    FALSE
  })
  if (!creada) return(invisible(NULL))
  resumen <- lupa::perfilar_dbi(
    conexion, tabla, bloque_filas = 1L,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
  )$resumen_tabla
  expect_identical(
    resumen$meta$fuente_bloques$metodo_orden,
    "resultset_no_reproducible"
  )
  expect_false(resumen$meta$fuente_bloques$estable)
  expect_match(
    resumen$meta$alcance$orden, "pk_deterministic=FALSE", fixed = TRUE
  )
})
