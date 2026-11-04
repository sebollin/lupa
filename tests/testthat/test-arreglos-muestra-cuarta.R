test_that("muestra_motor no confunde una tabla chica con un recorte", {
  testthat::skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "arrm_chica", data.frame(
    id = 1:3, texto = c("a", "b", "c")
  ))
  avisos <- character()
  resultado <- withCallingHandlers(
    perfilar_dbi(
      con, "arrm_chica", universo = "muestra_motor", muestra_motor = 10L,
      metricas = "validos", proteger_datos_personales = FALSE
    ),
    warning = function(x) {
      avisos <<- c(avisos, conditionMessage(x))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(avisos, 0L)
  expect_equal(resultado$perfil_muestra$meta$filas_analizadas, 3L)
  expect_true(is.na(resultado$perfil_muestra$meta$tope_que_mando))
  expect_equal(
    resultado$perfil_muestra$meta$materializacion$tope_que_mando,
    NA_character_
  )
})

test_that("muestra_motor conserva las filas que entran en el tope de bytes", {
  testthat::skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  texto <- paste(rep("x", 500L), collapse = "")
  DBI::dbWriteTable(con, "arrm_bytes", data.frame(
    id = seq_len(50L), texto = rep(texto, 50L)
  ))
  limite <- 10000L
  avisos <- character()
  resultado <- withCallingHandlers(
    perfilar_dbi(
      con, "arrm_bytes", universo = "muestra_motor", muestra_motor = 50L,
      metricas = "validos", max_bytes_muestra = limite,
      max_bytes_materializacion = limite, proteger_datos_personales = FALSE
    ),
    warning = function(x) {
      avisos <<- c(avisos, conditionMessage(x))
      invokeRestart("muffleWarning")
    }
  )
  materializacion <- resultado$resumen_tabla$meta$materializacion
  expect_true(length(avisos) >= 1L)
  expect_true(resultado$perfil_muestra$meta$filas_analizadas > 0L)
  expect_true(resultado$perfil_muestra$meta$filas_analizadas < 50L)
  expect_true(materializacion$bytes <= limite)
  expect_true(isTRUE(materializacion$recortada))
  expect_identical(resultado$perfil_muestra$meta$tope_que_mando, "bytes")
  expect_identical(materializacion$tope_que_mando, "bytes")
  expect_true(is.finite(materializacion$fraccion_materializada))
  expect_equal(
    resultado$perfil_muestra$meta$bytes_muestra,
    materializacion$bytes
  )
  expect_true(is.data.frame(resultado$resumen_tabla$tiempos))
})

test_that("muestra_motor aplica max_celdas_muestra antes de leer", {
  testthat::skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "arrm_celdas", data.frame(
    a = seq_len(100L), b = seq_len(100L), c = paste0("x", seq_len(100L))
  ))
  resultado <- suppressWarnings(perfilar_dbi(
    con, "arrm_celdas", universo = "muestra_motor", muestra_motor = 100L,
    metricas = "validos", max_celdas_muestra = 10L,
    max_bytes_muestra = Inf, proteger_datos_personales = FALSE
  ))
  expect_equal(resultado$perfil_muestra$meta$filas_analizadas, 3L)
  expect_equal(resultado$perfil_muestra$meta$max_celdas_muestra, 10L)
  expect_identical(resultado$perfil_muestra$meta$tope_que_mando, "celdas")
  expect_equal(resultado$resumen_tabla$meta$muestreo$filas_pedidas, 3L)
  expect_equal(resultado$resumen_tabla$meta$muestreo$filas_solicitadas, 100L)
})

test_that("una tabla vacia con muestra_motor se perfila con cero filas", {
  testthat::skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE arrm_vacia (id INTEGER, texto TEXT)")
  resultado <- tryCatch(
    perfilar_dbi(
      con, "arrm_vacia", universo = "muestra_motor", muestra_motor = 10L,
      metricas = "validos", proteger_datos_personales = FALSE
    ),
    error = identity
  )
  expect_false(inherits(resultado, "error"))
  expect_equal(resultado$perfil_muestra$meta$filas_analizadas, 0L)
  expect_equal(nrow(resultado$perfil_muestra$columnas), 2L)
})

test_that("las fallas solicitadas agregan degradado a la cobertura", {
  testthat::skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "arrm_falla", data.frame(a = 1:5, b = 6:10))
  avisos <- character()
  resultado <- withCallingHandlers(
    perfilar_dbi(
      con, "arrm_falla", universo = "tabla_completa",
      metricas = c("validos", "basicos"), max_consultas = 3L,
      proteger_datos_personales = FALSE
    ),
    warning = function(x) {
      avisos <<- c(avisos, conditionMessage(x))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(nrow(resultado$resumen_tabla$meta$consultas$fallas) > 0L)
  degradado <- resultado$resumen_tabla$cobertura[
    resultado$resumen_tabla$cobertura$estado == "degradado", , drop = FALSE
  ]
  expect_equal(nrow(degradado), 1L)
  expect_true(length(avisos) >= 1L)
})

.arrm_con_pg <- function() {
  skip_if_not_installed("RPostgres")
  pass <- Sys.getenv("LUPA_PG_PASS", Sys.getenv("LUPA_PG_PASSWORD"))
  if (!nzchar(pass)) skip("falta la clave de PostgreSQL")
  tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(),
      host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
      port = as.integer(Sys.getenv("LUPA_PG_PORT", "55432")),
      user = Sys.getenv("LUPA_PG_USER", "postgres"),
      password = pass,
      dbname = Sys.getenv("LUPA_PG_NAME", Sys.getenv("LUPA_PG_DBNAME", "lupa"))
    ),
    error = function(e) skip("PostgreSQL local no disponible")
  )
}

test_that("el catalogo solo propone el primer bloque y la medicion manda", {
  testthat::skip_on_cran()
  con <- .arrm_con_pg()
  tablas <- c("arrm_reltuples", "arrm_binaria", "arrm_toast")
  on.exit({
    for (tabla in tablas) try(DBI::dbExecute(
      con, paste0("DROP TABLE IF EXISTS ", tabla)
    ), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)

  DBI::dbExecute(con, "CREATE TABLE arrm_reltuples (id INTEGER, texto TEXT)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO arrm_reltuples SELECT g::integer, repeat(chr(97 + ",
    "(g::integer % 20)), 1000) FROM generate_series(1, 100) g"
  ))
  DBI::dbExecute(con, "ANALYZE arrm_reltuples")
  DBI::dbExecute(con, paste0(
    "INSERT INTO arrm_reltuples SELECT g::integer, repeat(chr(97 + ",
    "(g::integer % 20)), 1000) FROM generate_series(101, 30100) g"
  ))
  resultado <- suppressWarnings(perfilar_dbi(
    con, "arrm_reltuples", universo = "muestra_motor", muestra_motor = 1000L,
    metricas = "validos", max_bytes_muestra = 2097152L,
    max_bytes_materializacion = 2097152L, proteger_datos_personales = FALSE
  ))
  # La cota de fila usa el maximo seguro de la proyeccion, no el promedio
  # comprimido del catalogo: puede recortar antes aunque las filas observadas
  # sean cortas.
  expect_gt(resultado$perfil_muestra$meta$filas_analizadas, 0L)
  expect_equal(resultado$perfil_muestra$meta$materializacion$fraccion_materializada,
               resultado$resumen_tabla$meta$muestreo$fraccion)
  expect_true(is.finite(
    resultado$perfil_muestra$meta$materializacion$estimacion_bytes$bytes_por_fila
  ))
  expect_equal(resultado$perfil_muestra$meta$columnas_texto_largo, "texto")

  DBI::dbExecute(con, "CREATE TABLE arrm_binaria (id INTEGER, bin BYTEA)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO arrm_binaria SELECT g::integer, decode(repeat(chr(100)||",
    "chr(101)||chr(97)||chr(100)||chr(98)||chr(101)||chr(101)||chr(102),",
    "262144), chr(104)||chr(101)||chr(120)) FROM generate_series(1, 4) g"
  ))
  DBI::dbExecute(con, "ANALYZE arrm_binaria")
  binaria <- suppressWarnings(perfilar_dbi(
    con, "arrm_binaria", universo = "muestra_motor", muestra_motor = 4L,
    metricas = "validos", max_bytes_muestra = 1572864L,
    max_bytes_materializacion = 1572864L, proteger_datos_personales = FALSE
  ))
  expect_equal(binaria$perfil_muestra$meta$filas_analizadas, 4L)
  expect_identical(binaria$perfil_muestra$meta$columnas_binarias, "bin")
  expect_true(binaria$perfil_muestra$meta$materializacion$bytes < 10000L)

  DBI::dbExecute(con, "CREATE TABLE arrm_toast (id INTEGER, texto TEXT)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO arrm_toast SELECT g::integer, repeat(chr(97), 500000)",
    " FROM generate_series(1, 20) g"
  ))
  DBI::dbExecute(con, "ANALYZE arrm_toast")
  toast <- suppressWarnings(perfilar_dbi(
    con, "arrm_toast", universo = "muestra_motor", muestra_motor = 20L,
    metricas = "validos", max_bytes_muestra = 5242880L,
    max_bytes_materializacion = 5242880L, proteger_datos_personales = FALSE
  ))
  expect_equal(toast$perfil_muestra$meta$filas_analizadas, 20L)
  expect_true(toast$perfil_muestra$meta$materializacion$bytes <= 5242880L)
  expect_false(isTRUE(toast$perfil_muestra$meta$materializacion$recortada))
  expect_equal(toast$perfil_muestra$meta$columnas_texto_largo, "texto")
})

test_that("solo_agregados reintenta un TABLESAMPLE vacio", {
  testthat::skip_on_cran()
  con <- .arrm_con_pg()
  on.exit({
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS arrm_solo"), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE arrm_solo (id INTEGER, texto TEXT)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO arrm_solo SELECT g::integer, chr(118) || (g::integer % 7)",
    " FROM generate_series(1, 200) g"
  ))
  DBI::dbExecute(con, "ANALYZE arrm_solo")
  encontrado <- FALSE
  for (i in seq_len(100L)) {
    avisos <- character()
    resultado <- withCallingHandlers(
      perfilar_dbi(
        con, "arrm_solo", universo = "muestra_motor", muestra_motor = 100L,
        bloque_muestra = "solo_agregados", metricas = "validos",
        proteger_datos_personales = FALSE
      ),
      warning = function(x) {
        avisos <<- c(avisos, conditionMessage(x))
        invokeRestart("muffleWarning")
      }
    )
    muestreo <- resultado$resumen_tabla$meta$muestreo
    if (isTRUE(muestreo$reintentos >= 1L) ||
        (is.finite(muestreo$filas_obtenidas) &&
         muestreo$filas_obtenidas < 50L)) {
      encontrado <- TRUE
      if (muestreo$filas_obtenidas < 50L) expect_gte(muestreo$reintentos, 1L)
      if (muestreo$filas_obtenidas >= 50L) {
        expect_length(avisos, 0L)
      } else {
        expect_true(length(avisos) >= 1L)
      }
      break
    }
  }
  if (!encontrado) skip("TABLESAMPLE no produjo una muestra corta en 100 intentos")
})
