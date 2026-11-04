test_that("la proyeccion de texto largo tiene una cota de fila", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "postgresql",
    .package = "lupa"
  )
  dialecto <- .dialectos_dbi()$limit
  proyeccion <- .campos_sql_muestra_dbi(
    con, "dato", '"dato"', list(character()), "text", dialecto,
    tipos_motor = "text", max_largo_valor_vocabulario = 10000
  )
  expect_true(grepl("lupa_texto_largo", proyeccion$campos_sql, fixed = TRUE))
  expect_true(grepl("MD5", proyeccion$campos_sql, fixed = TRUE))
  expect_equal(proyeccion$columnas_texto_largo, "dato")
  expect_true(is.finite(proyeccion$bytes_por_fila_max))
  expect_gt(proyeccion$bytes_por_fila_max, 40000)
})

test_that("los arreglos no se confunden con el tipo binario base", {
  skip_on_cran()
  expect_false(.es_tipo_binario_muestra_dbi("bytea[]", "postgresql"))
  expect_true(.es_tipo_binario_muestra_dbi("bytea", "postgresql"))
  expect_false(.es_tipo_binario_muestra_dbi("varbinary_alias", "sql server"))
  expect_true(.es_tipo_binario_muestra_dbi("varbinary(max)", "sql server"))
})

test_that("el catalogo de SQL Server separa tipo de sistema y alias", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sql_recibido <- character()
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "microsoft sql server odbc",
    .traer_consulta_dbi = function(conexion, sql) {
      sql_recibido <<- c(sql_recibido, sql)
      data.frame(
        posicion = 1L, columna = "dato", legible = TRUE,
        tipo_motor = "varbinary", tipo_declarado_motor = "alias_binario",
        tipo_objeto = "U", stringsAsFactors = FALSE
      )
    },
    .package = "lupa"
  )
  permisos <- .permisos_columnas_dbi(con, "arrm_alias", .presupuesto_dbi())
  expect_true(length(sql_recibido) >= 1L)
  expect_true(grepl("TYPE_NAME(c.system_type_id)", sql_recibido[[1L]],
                    fixed = TRUE))
  expect_true(grepl("TYPE_NAME(c.user_type_id)", sql_recibido[[1L]],
                    fixed = TRUE))
  expect_equal(permisos$tipo_motor[[1L]], "varbinary")
  expect_equal(permisos$tipo_declarado_motor[[1L]], "alias_binario")
})

test_that("el perfil de texto largo conserva longitudes y cobertura", {
  skip_on_cran()
  datos <- data.frame(
    dato = c(
      "abc",
      "lupa_texto_largo:12000:24000:0123456789abcdef0123456789abcdef",
      "lupa_texto_largo:12000:24000:0123456789abcdef0123456789abcdef",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, proteger_datos_personales = FALSE)
  perfil <- .ajustar_textos_largos_dbi(
    perfil, datos, "dato", max_largo = 10000
  )
  fila <- perfil$columnas[perfil$columnas$columna == "dato", , drop = FALSE]
  expect_equal(fila$n_faltantes, 1L)
  expect_equal(fila$n_distintos, 2L)
  expect_equal(fila$longitud_minima, 3)
  expect_equal(fila$longitud_maxima, 12000)
  expect_equal(fila$n_longitudes_resumidas, 2L)
  cobertura <- perfil$cobertura_diagnosticos
  expect_true(any(cobertura$diagnostico == "texto_largo_no_evaluado"))
})

test_that("el tope de procesamiento avisa y publica el descarte", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "arrm_procesamiento", data.frame(
    id = seq_len(1000L), texto = rep(strrep("x", 2000L), 1000L),
    stringsAsFactors = FALSE
  ))
  avisos <- list()
  resultado <- withCallingHandlers(
    perfilar_dbi(
      con, "arrm_procesamiento", universo = "muestra_motor",
      muestra_motor = 1000L, muestra = 1000L,
      max_bytes_procesamiento = 1000, metricas = "validos",
      proteger_datos_personales = FALSE
    ),
    warning = function(w) {
      avisos[[length(avisos) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(vapply(avisos, inherits,
                         logical(1L), "lupa_aviso_muestra_motor")))
  expect_null(resultado$perfil_muestra)
  expect_equal(resultado$resumen_tabla$meta$tope_que_mando, "procesamiento")
  expect_equal(
    resultado$resumen_tabla$meta$materializacion$tope_que_mando,
    "procesamiento"
  )
  expect_false(isTRUE(resultado$resumen_tabla$meta$materializacion$recortada))
})

test_that("entre tope de celdas y bytes se publica un solo ganador", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "arrm_dos_topes", data.frame(
    id = seq_len(10L), texto = rep(strrep("x", 2000L), 10L),
    stringsAsFactors = FALSE
  ))
  avisos <- list()
  resultado <- withCallingHandlers(
    perfilar_dbi(
      con, "arrm_dos_topes", universo = "muestra_motor",
      muestra_motor = 10L, muestra = 10L, max_celdas_muestra = 20L,
      max_bytes_muestra = 9000, max_bytes_materializacion = 9000,
      metricas = "validos", proteger_datos_personales = FALSE
    ),
    warning = function(w) {
      avisos[[length(avisos) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(vapply(avisos, inherits,
                         logical(1L), "lupa_aviso_muestra_motor")))
  expect_equal(resultado$resumen_tabla$meta$tope_que_mando, "bytes")
  expect_equal(resultado$perfil_muestra$meta$tope_que_mando, "bytes")
  expect_equal(
    resultado$resumen_tabla$meta$materializacion$tope_que_mando, "bytes"
  )
})

test_that("PostgreSQL limita antes de leer filas de texto extenso", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  testthat::skip_if(
    !nzchar(Sys.getenv("LUPA_PG_HOST", unset = "")),
    "se necesita LUPA_PG_HOST para la prueba del motor"
  )
  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST"),
      port = as.integer(Sys.getenv("LUPA_PG_PORT", unset = "55432")),
      dbname = Sys.getenv("LUPA_PG_NAME", unset = "lupa"),
      user = Sys.getenv("LUPA_PG_USER", unset = "postgres"),
      password = Sys.getenv("LUPA_PG_PASS", unset = "lupa")
    ), error = function(e) NULL
  )
  testthat::skip_if(is.null(con), "PostgreSQL no disponible")
  tabla <- "arrm_texto_largo"
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE", tabla, "AS",
    "SELECT g AS id, repeat(md5(g::text), 2048) AS texto",
    "FROM generate_series(1, 800) AS s(g)"
  ))
  DBI::dbExecute(con, paste("ANALYZE", tabla))
  limite <- 32 * 1024^2
  resultado <- suppressWarnings(perfilar_dbi(
    con, tabla, universo = "muestra_motor", muestra_motor = 800L,
    muestra = 800L, metricas = "validos",
    max_bytes_muestra = limite, max_bytes_materializacion = limite,
    max_bytes_procesamiento = limite,
    proteger_datos_personales = FALSE
  ))
  materializacion <- resultado$resumen_tabla$meta$materializacion
  expect_true(is.finite(materializacion$bytes_por_fila_max))
  expect_true(materializacion$n_filas > 0)
  expect_lte(materializacion$bytes, limite)
  expect_true(materializacion$n_filas <= 800)
  expect_false(is.null(resultado$perfil_muestra))
  expect_identical(
    resultado$perfil_muestra$meta$columnas_texto_largo, "texto"
  )
  expect_true(any(
    resultado$perfil_muestra$cobertura_diagnosticos$diagnostico ==
      "texto_largo_no_evaluado"
  ))
})

test_that("SQL Server reconoce el tipo base de un alias varbinary", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  testthat::skip_if(!nzchar(clave), "se necesita LUPA_MSSQL_PWD")
  con <- tryCatch(
    DBI::dbConnect(
      odbc::odbc(), Driver = "FreeTDS",
      Server = Sys.getenv("LUPA_MSSQL_HOST", unset = "127.0.0.1"),
      Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", unset = "1433")),
      UID = Sys.getenv("LUPA_MSSQL_USER", unset = "sa"), PWD = clave,
      Database = Sys.getenv("LUPA_MSSQL_NAME", unset = "lupa_pruebas"),
      TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
    ), error = function(e) NULL
  )
  testthat::skip_if(is.null(con), "SQL Server no disponible")
  tabla <- "dbo.arrm_alias_varbinary"
  tipo <- "dbo.arrm_tipo_varbinary"
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  try(DBI::dbExecute(con, paste0("DROP TYPE IF EXISTS ", tipo)), silent = TRUE)
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    try(DBI::dbExecute(con, paste0("DROP TYPE IF EXISTS ", tipo)),
        silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbExecute(con, paste0("CREATE TYPE ", tipo, " FROM varbinary(max)"))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (id int, dato ", tipo, ")"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla,
    " VALUES (1, CONVERT(varbinary(max), REPLICATE(CAST('a' AS varchar(max)), 1000))),",
    "(2, NULL)"
  ))
  resultado <- suppressWarnings(perfilar_dbi(
    con, tabla, universo = "muestra_motor", muestra_motor = 2L,
    muestra = 2L, metricas = "validos", proteger_datos_personales = FALSE
  ))
  binaria <- resultado$perfil_muestra$columnas[
    resultado$perfil_muestra$columnas$columna == "dato", , drop = FALSE
  ]
  expect_equal(binaria$n_faltantes, 1)
  expect_equal(binaria$longitud_maxima, 1000)
  expect_equal(resultado$perfil_muestra$meta$columnas_binarias, "dato")
})
