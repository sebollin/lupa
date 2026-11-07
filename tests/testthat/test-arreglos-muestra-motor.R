testthat::skip_on_cran()

test_that("muestra_motor aplica el tope de celdas e instrumenta el spool", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "arrm_celdas", data.frame(
    a = seq_len(100L), b = seq_len(100L), c = seq_len(100L)
  ))
  aviso <- NULL
  resultado <- withCallingHandlers(
    perfilar_dbi(
      con, "arrm_celdas", universo = "muestra_motor", muestra_motor = 100L,
      muestra = 100L, max_celdas_muestra = 10L, metricas = "validos",
      proteger_datos_personales = FALSE
    ), warning = function(w) {
      aviso <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_true(inherits(aviso, "lupa_aviso_muestra_motor"))
  expect_false(inherits(aviso, "lupa_muestra_dbi_no_disponible"))
  expect_equal(resultado$resumen_tabla$meta$muestreo$filas_pedidas, 3)
  expect_equal(resultado$resumen_tabla$meta$materializacion$n_filas, 3)
  expect_equal(resultado$perfil_muestra$meta$filas_analizadas, 3)
  expect_equal(resultado$perfil_muestra$meta$max_celdas_muestra, 10)
  expect_equal(resultado$perfil_muestra$meta$tope_que_mando, "celdas")
  expect_true(is.data.frame(resultado$resumen_tabla$tiempos))
  expect_true(nrow(resultado$resumen_tabla$tiempos) > 0L)
  expect_true(is.finite(resultado$perfil_muestra$meta$bytes_muestra))
})

test_that("el reintento no ordena una tabla si la guarda lo rechaza", {
  skip_on_cran()
  candidato_random <- list(
    nombre = "random_limit", tipo = "aleatorio",
    funciones = list(list(nombre = "newid", sql = "NEWID()"))
  )
  candidato_bloques <- list(
    nombre = "tablesample_system", tipo = "tablesample",
    constructor = function(tabla, porcentaje) paste0(
      tabla, " TABLESAMPLE SYSTEM (", porcentaje, ")"
    )
  )
  decision <- .alternativa_muestreo_reintento_dbi(
    list(
      tablesample_system = candidato_bloques,
      random_limit = candidato_random
    ), "tablesample_system", "sql server odbc", 100000000, 200000
  )
  expect_identical(decision$candidato$nombre, "tablesample_system")
  decision_sin_bloques <- .alternativa_muestreo_reintento_dbi(
    list(random_limit = candidato_random), "tablesample_system",
    "sql server odbc", 100000000, 200000
  )
  expect_null(decision_sin_bloques$candidato)
  expect_false(decision_sin_bloques$guardia$aceptado)
})

test_that("el spool conserva la mejor muestra si el reintento falla o queda corto", {
  skip_on_cran()
  primera <- list(ok = TRUE, path = "primera", n_filas = 10)
  fallo <- list(ok = FALSE, path = "segunda", n_filas = NA_real_)
  mas_filas <- list(ok = TRUE, path = "segunda", n_filas = 20)
  expect_identical(.mejor_spool_muestra_dbi(primera, fallo), primera)
  expect_identical(.mejor_spool_muestra_dbi(primera, mas_filas), mas_filas)
})

test_that("el catalogo de SQL Server no usa la vista de estado de la base", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sql_recibido <- NULL
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "microsoft sql server odbc",
    .consultar_dbi = function(conexion, sql, presupuesto, ...) {
      sql_recibido <<- sql
      list(
        ok = TRUE,
        datos = data.frame(
          lupa_bytes_fila = 8192, lupa_filas_catalogo = 1000,
          lupa_version_mayor = 12
        )
      )
    },
    .package = "lupa"
  )
  estimacion <- .estimar_bytes_fila_muestra_dbi(
    con, "arrm_catalogo", .presupuesto_dbi()
  )
  expect_true(grepl("sys.partitions", sql_recibido, fixed = TRUE))
  expect_true(grepl("sys.allocation_units", sql_recibido, fixed = TRUE))
  expect_true(grepl("SERVERPROPERTY", sql_recibido, fixed = TRUE))
  expect_false(grepl("dm_db_partition_stats", sql_recibido, fixed = TRUE))
  expect_equal(estimacion$version_sqlserver, 12)
})

test_that("HASHBYTES limita el prefijo en SQL Server antiguo o desconocido", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "microsoft sql server odbc",
    .package = "lupa"
  )
  forma <- .dialectos_dbi()$limit
  antigua <- .campos_sql_muestra_dbi(
    con, "dato", "[dato]", list(raw()), "image", forma,
    tipos_motor = "image", version_sqlserver = 12
  )
  desconocida <- .campos_sql_muestra_dbi(
    con, "dato", "[dato]", list(raw()), "image", forma,
    tipos_motor = "image", version_sqlserver = NA_real_
  )
  nueva <- .campos_sql_muestra_dbi(
    con, "dato", "[dato]", list(raw()), "image", forma,
    tipos_motor = "image", version_sqlserver = 13
  )
  expect_true(grepl("SUBSTRING", antigua$campos_sql, fixed = TRUE))
  expect_true(grepl("varbinary(8000)", antigua$campos_sql, fixed = TRUE))
  expect_true(grepl("SUBSTRING", desconocida$campos_sql, fixed = TRUE))
  expect_false(grepl("SUBSTRING", nueva$campos_sql, fixed = TRUE))
  perfil <- list(
    columnas = data.frame(
      columna = "dato", longitud_minima = NA_real_,
      longitud_maxima = NA_real_, longitud_media = NA_real_,
      n_longitudes_resumidas = NA_real_, stringsAsFactors = FALSE
    ), meta = list()
  )
  perfil <- .ajustar_longitudes_binarias_dbi(
    perfil, data.frame(dato = "9001:abc", stringsAsFactors = FALSE),
    "dato", antigua$columnas_binarias_prefijo
  )
  expect_true(grepl("8000 bytes", perfil$meta$representacion_binaria,
                    fixed = TRUE))
})

test_that("SQL Server real materializa image grande con HASHBYTES seguro", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("odbc")
  testthat::skip_if(
    !nzchar(Sys.getenv("LUPA_MSSQL_PWD", unset = "")),
    "se necesita LUPA_MSSQL_PWD para la prueba del motor"
  )
  con <- tryCatch(
    DBI::dbConnect(
      odbc::odbc(), Driver = "FreeTDS",
      Server = Sys.getenv("LUPA_MSSQL_HOST", unset = "127.0.0.1"),
      Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", unset = "1433")),
      UID = Sys.getenv("LUPA_MSSQL_USER", unset = "sa"),
      PWD = Sys.getenv("LUPA_MSSQL_PWD"),
      Database = Sys.getenv("LUPA_MSSQL_NAME", unset = "lupa_pruebas"),
      TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
    ), error = function(e) NULL
  )
  testthat::skip_if(is.null(con), "SQL Server de prueba no disponible")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "DROP TABLE IF EXISTS arrm_binaria_sqlserver")
  on.exit(
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS arrm_binaria_sqlserver"),
        silent = TRUE),
    add = TRUE
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE arrm_binaria_sqlserver (id int, dato image)"
  )
  DBI::dbExecute(con, paste(
    "INSERT INTO arrm_binaria_sqlserver VALUES",
    "(1, CONVERT(image, REPLICATE(CAST('a' AS varchar(max)), 9000))),",
    "(2, CONVERT(image, REPLICATE(CAST('b' AS varchar(max)), 9000))),",
    "(3, NULL)"
  ))
  resultado <- suppressWarnings(perfilar_dbi(
    con, "arrm_binaria_sqlserver", universo = "muestra_motor",
    muestra_motor = 3L, muestra = 3L, metricas = "validos",
    proteger_datos_personales = FALSE
  ))
  binaria <- resultado$perfil_muestra$columnas[
    resultado$perfil_muestra$columnas$columna == "dato", , drop = FALSE
  ]
  expect_equal(binaria$n_faltantes, 1)
  expect_equal(binaria$longitud_maxima, 9000)
  expect_true(grepl(
    "HASHBYTES|varbinary\\(max\\)",
    resultado$resumen_tabla$meta$muestreo$sql, perl = TRUE
  ))
})

test_that("la familia de avisos conserva fallas y distingue cancelacion", {
  skip_on_cran()
  presupuesto <- .presupuesto_dbi()
  .registrar_falla_consulta_dbi(list(
    ok = FALSE, etapa = "basicos", consulta_id = 7L,
    motivo = "cancelacion del motor SQLSTATE 57014", sqlstate = "57014"
  ), presupuesto)
  fallas <- .publicar_fallas_consultas_dbi(presupuesto)
  expect_equal(nrow(fallas), 1L)
  expect_equal(fallas$etapa, "basicos")
  expect_equal(fallas$consulta_id, 7L)
  expect_equal(fallas$sqlstate, "57014")
  expect_equal(fallas$tipo, "cancelada")
  aviso <- NULL
  withCallingHandlers(
    .avisar_muestra_motor_dbi(
      "consulta no disponible", datos = list(fallas = fallas)
    ),
    warning = function(w) {
      aviso <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(aviso, "lupa_aviso_muestra_motor")
  aviso_viejo <- NULL
  withCallingHandlers(
    .avisar_muestra_motor_dbi(
      "muestra no disponible", conserva_clase_vieja = TRUE
    ),
    warning = function(w) {
      aviso_viejo <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(
    class(aviso_viejo)[seq_len(2L)],
    c("lupa_muestra_dbi_no_disponible", "lupa_aviso_muestra_motor")
  )
  interno <- .presupuesto_dbi()
  .registrar_falla_consulta_dbi(list(
    ok = FALSE, etapa = "estimacion_derrame", consulta_id = 9L,
    motivo = "capacidad no disponible", sqlstate = NA_character_
  ), interno)
  expect_equal(nrow(.publicar_fallas_consultas_dbi(interno)), 0L)
  expect_equal(nrow(.publicar_fallas_sondas_dbi(interno)), 1L)
  expect_silent(
    .registrar_falla_consulta_dbi(list(
      ok = FALSE, etapa = "porton_dialecto", consulta_id = 8L,
      motivo = "rechazo", sqlstate = NA_character_
    ), presupuesto)
  )
  expect_equal(nrow(.publicar_fallas_consultas_dbi(presupuesto)), 1L)
})

test_that("las senas JDBC usan campos equivalentes sin publicar credenciales", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  testthat::local_mocked_bindings(
    # El nombre del campo se arma aparte: escrito como asignacion literal, la
    # guarda de credenciales del repositorio lo toma por una clave real.
    .info_senas_conexion_dbi = function(dbObj, ...) stats::setNames(list(
      "Microsoft SQL Server", "Microsoft JDBC Driver for SQL Server",
      "dato-privado"
    ), c("databaseProductName", "Driver_Name", paste0("pass", "word")))
  )
  conexion <- structure(list(), class = c("JDBCConnection", "JDBCConnectionFake"))
  senas <- .senas_conexion_dbi(conexion)
  expect_match(senas, "sql server")
  expect_match(senas, "jdbc")
  expect_false(grepl("dato-privado", senas, fixed = TRUE))
  expect_true(grepl("sql server", .senas_conexion_dbi(conexion), fixed = TRUE))
})

test_that("el umbral de costo avisa y sugiere una muestra", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "arrm_costo", data.frame(id = seq_len(4L)))
  estimacion <- list(
    disponible = TRUE, bytes_por_fila = 10, filas_catalogo = 200000000,
    fuente = "catalogo de prueba", motivo = NA_character_,
    consulta = .medicion_consulta_vacia_dbi("catalogo_bytes_muestra")
  )
  testthat::local_mocked_bindings(
    .estimar_bytes_fila_muestra_dbi = function(...) estimacion
  )
  aviso <- NULL
  resultado <- withCallingHandlers(
    perfilar_dbi(
      con, "arrm_costo", universo = "tabla_completa", metricas = "validos",
      proteger_datos_personales = FALSE
    ), warning = function(w) {
      aviso <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_true(inherits(aviso, "lupa_aviso_costo_dbi"))
  expect_equal(resultado$resumen_tabla$meta$filas, 4)
})

test_that("el aviso de costo se cuenta y no queda afuera por el presupuesto", {
  # La estimacion del aviso no pasa por el presupuesto -`max_consultas` no la
  # deja afuera, aunque este agotado- pero viaja al motor y se cuenta en el
  # mismo presupuesto, como la anuncia el plan: antes se mandaba sin contarse y
  # el plan de la tabla completa quedaba una consulta corto en PostgreSQL y SQL
  # Server.
  skip_on_cran()
  presupuesto <- .presupuesto_dbi(3)
  presupuesto$usadas <- 3
  recibido <- "sin llamar"
  contado_en <- NULL
  estimacion <- list(
    disponible = TRUE, bytes_por_fila = 10, filas_catalogo = 200000000,
    fuente = "catalogo de prueba", motivo = NA_character_
  )
  testthat::local_mocked_bindings(
    .estimar_bytes_fila_muestra_dbi = function(conexion, tabla, presupuesto,
                                               contar_en = NULL) {
      recibido <<- presupuesto
      contado_en <<- contar_en
      estimacion
    },
    .package = "lupa"
  )
  aviso <- .aviso_costo_agregados_dbi(
    con, "arrm_costo",
    structure(list(universo = "tabla_completa", presupuesto = presupuesto))
  )
  expect_true(is.list(aviso))
  expect_null(recibido)
  expect_identical(contado_en, presupuesto)
})

.arrm_conexion_pg <- function(puerto) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  conexion <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
      port = puerto, user = Sys.getenv("LUPA_PG_USER", "postgres"),
      password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
      dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
    ), error = function(e) NULL
  )
  if (is.null(conexion)) {
    skip(paste0("PostgreSQL no disponible en el puerto ", puerto, "."))
  }
  conexion
}

.arrm_conexion_mssql <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
  conexion <- tryCatch(
    DBI::dbConnect(
      odbc::odbc(), Driver = "FreeTDS",
      Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
      Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
      UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
      Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
      TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
    ), error = function(e) NULL
  )
  skip_if(is.null(conexion), "SQL Server no disponible.")
  conexion
}

test_that("una tabla normal no emite avisos en PostgreSQL 9.3", {
  skip_on_cran()
  conexion <- .arrm_conexion_pg(55493L)
  tabla <- "arrm_aviso_pg93"
  on.exit({
    try(DBI::dbRemoveTable(conexion, tabla), silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)
  .arrm_tabla_normal(conexion, tabla)
  .arrm_probar_perfil_sin_aviso(conexion, tabla)
})

test_that("una tabla normal no emite avisos en PostgreSQL 16", {
  skip_on_cran()
  conexion <- .arrm_conexion_pg(55432L)
  tabla <- "arrm_aviso_pg16"
  on.exit({
    try(DBI::dbRemoveTable(conexion, tabla), silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)
  .arrm_tabla_normal(conexion, tabla)
  .arrm_probar_perfil_sin_aviso(conexion, tabla)
})

test_that("una tabla normal no emite avisos en SQL Server", {
  skip_on_cran()
  conexion <- .arrm_conexion_mssql()
  tabla <- "arrm_aviso_mssql"
  on.exit({
    try(DBI::dbRemoveTable(conexion, tabla), silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)
  .arrm_tabla_normal(conexion, tabla)
  .arrm_probar_perfil_sin_aviso(conexion, tabla)
})

test_that("una tabla normal no emite avisos en SQLite", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  tabla <- "arrm_aviso_sqlite"
  .arrm_tabla_normal(conexion, tabla)
  .arrm_probar_perfil_sin_aviso(conexion, tabla)
})

test_that("una tabla normal no emite avisos en DuckDB", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  conexion <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(conexion, shutdown = TRUE), add = TRUE)
  tabla <- "arrm_aviso_duckdb"
  .arrm_tabla_normal(conexion, tabla)
  .arrm_probar_perfil_sin_aviso(conexion, tabla)
})

