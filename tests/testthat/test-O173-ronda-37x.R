testthat::skip_if_not_installed("DBI")

.r37x_pg <- function() {
  testthat::skip_if_not_installed("RPostgres")
  conexion <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = 55432,
      user = "postgres", password = "lupa", dbname = "lupa"
    ),
    error = function(e) NULL
  )
  if (is.null(conexion)) {
    testthat::skip("PostgreSQL real no disponible en el puerto 55432.")
  }
  conexion
}

.r37x_mssql <- function() {
  testthat::skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD")
  if (!nzchar(clave)) {
    testthat::skip("Sin LUPA_MSSQL_PWD: no hay un SQL Server de prueba declarado.")
  }
  conexion <- tryCatch(
    DBI::dbConnect(
      odbc::odbc(), Driver = "FreeTDS", Server = "127.0.0.1", Port = 1433,
      UID = "sa", PWD = clave, Database = "lupa_refutacion",
      TDS_Version = "8.0", encoding = "UTF-8"
    ),
    error = function(e) NULL
  )
  if (is.null(conexion)) {
    testthat::skip("SQL Server real no disponible en el puerto 1433.")
  }
  conexion
}

.r37x_drop_pg <- function(conexion) {
  try(DBI::dbExecute(conexion, "DROP VIEW IF EXISTS r37x_dev_test_v"), silent = TRUE)
  try(DBI::dbExecute(conexion, "DROP TABLE IF EXISTS r37x_dev_test_ref"), silent = TRUE)
  try(DBI::dbExecute(conexion, "DROP TABLE IF EXISTS r37x_dev_test_src"), silent = TRUE)
  try(DBI::dbExecute(conexion, "DROP TABLE IF EXISTS r37x_dev_test_sample"), silent = TRUE)
}

testthat::test_that("la muestra con orden conserva las columnas sanas de una vista rota", {
  conexion <- .r37x_pg()
  on.exit({
    try(.r37x_drop_pg(conexion), silent = TRUE)
    try(DBI::dbDisconnect(conexion), silent = TRUE)
  }, add = TRUE)
  .r37x_drop_pg(conexion)
  DBI::dbExecute(conexion, "CREATE TABLE r37x_dev_test_src (id integer, nombre text)")
  DBI::dbWriteTable(
    conexion, "r37x_dev_test_src",
    data.frame(id = 1:300, nombre = rep(c("a", "b"), 150L)), append = TRUE
  )
  DBI::dbExecute(conexion, paste(
    "CREATE VIEW r37x_dev_test_v AS SELECT id,",
    "CAST(CASE WHEN id = 250 THEN 'bad' ELSE CAST(id AS text) END AS integer) AS cod,",
    "nombre FROM r37x_dev_test_src"
  ))

  for (orden in list("id", "cod", c("nombre", "cod"), c("id", "nombre"))) {
    resultado <- lupa::perfilar_dbi(
      conexion, "r37x_dev_test_v", muestra = 100L, orden_muestra = orden,
      metricas = "validos", proteger_datos_personales = FALSE
    )
    expect_equal(nrow(resultado$perfil_muestra$columnas), 2L)
    expect_setequal(resultado$perfil_muestra$columnas$columna, c("id", "nombre"))
    cobertura <- resultado$resumen_tabla$cobertura
    fila <- cobertura[cobertura$bloque == "perfil_muestra" &
                        cobertura$estado == "alcance_distinto", , drop = FALSE]
    expect_equal(nrow(fila), 1L)
    expect_false(grepl("ninguna fallo por si sola", fila$motivo[[1L]], fixed = TRUE))
    if (any(orden %in% c("cod"))) {
      expect_true(grepl("orden_muestra", fila$motivo[[1L]], fixed = TRUE))
    } else {
      expect_true(grepl("cod", fila$motivo[[1L]], fixed = TRUE))
    }
  }
})

testthat::test_that("un orden roto se informa y no fabrica un motivo de aislamiento", {
  conexion <- .r37x_pg()
  on.exit({
    try(.r37x_drop_pg(conexion), silent = TRUE)
    try(DBI::dbDisconnect(conexion), silent = TRUE)
  }, add = TRUE)
  .r37x_drop_pg(conexion)
  DBI::dbExecute(conexion, "CREATE TABLE r37x_dev_test_src (id integer, nombre text)")
  DBI::dbWriteTable(
    conexion, "r37x_dev_test_src",
    data.frame(id = 1:300, nombre = rep(c("a", "b"), 150L)), append = TRUE
  )
  DBI::dbExecute(conexion, paste(
    "CREATE VIEW r37x_dev_test_v AS SELECT id, nombre,",
    "CAST(CASE WHEN id = 250 THEN 'bad' ELSE CAST(id AS text) END AS integer) AS cod",
    "FROM r37x_dev_test_src"
  ))

  for (orden in list("cod", c("nombre", "cod"))) {
    resultado <- lupa::perfilar_dbi(
      conexion, "r37x_dev_test_v", muestra = 100L,
      orden_muestra = orden, metricas = "validos",
      proteger_datos_personales = FALSE
    )
    expect_equal(nrow(resultado$perfil_muestra$columnas), 2L)
    expect_setequal(resultado$perfil_muestra$columnas$columna, c("id", "nombre"))
    cobertura <- resultado$resumen_tabla$cobertura
    fila <- cobertura[cobertura$bloque == "perfil_muestra" &
                        cobertura$estado == "alcance_distinto", , drop = FALSE]
    expect_equal(nrow(fila), 1L)
    expect_false(grepl("ninguna fallo por si sola", fila$motivo[[1L]], fixed = TRUE))
    expect_true(grepl("orden_muestra", fila$motivo[[1L]], fixed = TRUE))
  }
})

testthat::test_that("bloque_filas conserva magnitudes sanas cuando una columna falla", {
  conexion <- .r37x_pg()
  on.exit({
    try(.r37x_drop_pg(conexion), silent = TRUE)
    try(DBI::dbDisconnect(conexion), silent = TRUE)
  }, add = TRUE)
  .r37x_drop_pg(conexion)
  DBI::dbExecute(conexion, "CREATE TABLE r37x_dev_test_src (id integer, nombre text)")
  DBI::dbWriteTable(
    conexion, "r37x_dev_test_src",
    data.frame(id = 1:20, nombre = rep(c("a", "b"), 10L)), append = TRUE
  )
  DBI::dbExecute(conexion, paste(
    "CREATE VIEW r37x_dev_test_v AS SELECT id,",
    "CAST(CASE WHEN id = 5 THEN id / (id - 5) ELSE id END AS integer) AS malo,",
    "CAST(id AS double precision) AS valor, nombre FROM r37x_dev_test_src"
  ))

  resultado <- lupa::perfilar_dbi(
    conexion, "r37x_dev_test_v", muestra = 5L, bloque_filas = 5L,
    proteger_datos_personales = FALSE
  )
  fila <- resultado$resumen_tabla$columnas[
    resultado$resumen_tabla$columnas$columna == "valor", , drop = FALSE
  ]
  expect_equal(nrow(resultado$perfil_muestra$columnas), 3L)
  expect_true(is.finite(fila$minimo) && is.finite(fila$maximo))
  cobertura <- resultado$resumen_tabla$cobertura
  expect_true(any(cobertura$bloque == "perfil_muestra" &
                    cobertura$estado == "alcance_distinto"))
  expect_false(any(cobertura$bloque == "perfil_muestra" &
                     cobertura$estado == "no_solicitado"))
})

testthat::test_that("muestra_motor resume toda la materializacion y recorta solo el diagnostico", {
  conexion <- .r37x_pg()
  on.exit({
    try(.r37x_drop_pg(conexion), silent = TRUE)
    try(DBI::dbDisconnect(conexion), silent = TRUE)
  }, add = TRUE)
  .r37x_drop_pg(conexion)
  datos <- data.frame(id = seq_len(20000L), valor = seq_len(20000L))
  DBI::dbWriteTable(conexion, "r37x_dev_test_sample", datos, overwrite = TRUE)

  resultado <- lupa::perfilar_dbi(
    conexion, "r37x_dev_test_sample", universo = "muestra_motor",
    muestra_motor = 500L, muestra = 100L, metricas = "validos",
    proteger_datos_personales = FALSE
  )
  filas_motor <- as.numeric(resultado$resumen_tabla$meta$materializacion$n_filas)
  if (!is.finite(filas_motor) || filas_motor <= 100) {
    testthat::skip("TABLESAMPLE no materializo suficientes filas para la cifra estable.")
  }
  fila <- resultado$resumen_tabla$columnas[
    resultado$resumen_tabla$columnas$columna == "valor", , drop = FALSE
  ]
  expect_equal(as.numeric(fila$n), 20000)
  expect_equal(as.numeric(fila$n_validos + fila$n_faltantes), filas_motor)
  filas_diagnostico <- resultado$perfil_muestra$general$filas
  expect_equal(filas_diagnostico, 100)
  expect_true(filas_motor > filas_diagnostico)
  motivos <- resultado$resumen_tabla$sql$motivo
  expect_false(any(grepl(
    "materializacion completa fallo", motivos[!is.na(motivos)], fixed = TRUE
  )))
})

testthat::test_that("relaciones conserva las columnas sanas de una vista rota", {
  conexion <- .r37x_pg()
  on.exit({
    try(.r37x_drop_pg(conexion), silent = TRUE)
    try(DBI::dbDisconnect(conexion), silent = TRUE)
  }, add = TRUE)
  .r37x_drop_pg(conexion)
  DBI::dbExecute(conexion, "CREATE TABLE r37x_dev_test_ref (id integer, nombre text)")
  DBI::dbWriteTable(
    conexion, "r37x_dev_test_ref",
    data.frame(id = 1:20, nombre = rep(c("a", "b"), 10L)), append = TRUE
  )
  DBI::dbExecute(conexion, paste(
    "CREATE VIEW r37x_dev_test_v AS SELECT id,",
    "CAST(CASE WHEN id = 5 THEN 'bad' ELSE CAST(id AS text) END AS integer) AS malo,",
    "nombre FROM r37x_dev_test_ref"
  ))

  resultado <- lupa::relaciones_coleccion(
    lupa::coleccion(conexion, c("r37x_dev_test_ref", "r37x_dev_test_v")),
    pares = data.frame(
      tabla_1 = "r37x_dev_test_ref", tabla_2 = "r37x_dev_test_v",
      stringsAsFactors = FALSE
    )
  )
  expect_true(any(resultado$relaciones$columna_tabla1 == "id" &
                   resultado$relaciones$columna_tabla2 == "id"))
  expect_true(any(resultado$relaciones$columna_tabla1 == "nombre" &
                   resultado$relaciones$columna_tabla2 == "nombre"))
})

testthat::test_that("char y varchar se comparan por su valor logico en PostgreSQL", {
  conexion <- .r37x_pg()
  on.exit({
    try(DBI::dbExecute(conexion, "DROP TABLE IF EXISTS r37x_dev_test_char"), silent = TRUE)
    try(DBI::dbExecute(conexion, "DROP TABLE IF EXISTS r37x_dev_test_varchar"), silent = TRUE)
    try(DBI::dbDisconnect(conexion), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(conexion, "DROP TABLE IF EXISTS r37x_dev_test_char")
  DBI::dbExecute(conexion, "DROP TABLE IF EXISTS r37x_dev_test_varchar")
  DBI::dbExecute(conexion, "CREATE TABLE r37x_dev_test_char (cod char(10))")
  DBI::dbExecute(conexion, "CREATE TABLE r37x_dev_test_varchar (cod varchar(10))")
  DBI::dbExecute(conexion, "INSERT INTO r37x_dev_test_char VALUES ('A1'), ('A2')")
  DBI::dbExecute(conexion, "INSERT INTO r37x_dev_test_varchar VALUES ('A1'), ('A2')")

  resultado <- lupa::relaciones_coleccion(
    lupa::coleccion(conexion, c("r37x_dev_test_char", "r37x_dev_test_varchar")),
    pares = data.frame(
      tabla_1 = "r37x_dev_test_char", tabla_2 = "r37x_dev_test_varchar",
      stringsAsFactors = FALSE
    )
  )
  expect_true(any(resultado$relaciones$columna_tabla1 == "cod" &
                   resultado$relaciones$columna_tabla2 == "cod"))
})

testthat::test_that("SQL Server recupera la muestra si falla una columna del orden", {
  conexion <- .r37x_mssql()
  on.exit({
    try(DBI::dbExecute(conexion, "IF OBJECT_ID('r37x_dev_test_order_v') IS NOT NULL DROP VIEW r37x_dev_test_order_v", immediate = TRUE), silent = TRUE)
    try(DBI::dbExecute(conexion, "IF OBJECT_ID('r37x_dev_test_order_src') IS NOT NULL DROP TABLE r37x_dev_test_order_src", immediate = TRUE), silent = TRUE)
    try(DBI::dbDisconnect(conexion), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(conexion, "IF OBJECT_ID('r37x_dev_test_order_v') IS NOT NULL DROP VIEW r37x_dev_test_order_v", immediate = TRUE)
  DBI::dbExecute(conexion, "IF OBJECT_ID('r37x_dev_test_order_src') IS NOT NULL DROP TABLE r37x_dev_test_order_src", immediate = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE r37x_dev_test_order_src (id int, nombre varchar(20))", immediate = TRUE)
  DBI::dbExecute(conexion, paste(
    "INSERT INTO r37x_dev_test_order_src SELECT TOP 300",
    "ROW_NUMBER() OVER (ORDER BY (SELECT NULL)),",
    "CASE WHEN ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) % 2 = 0",
    "THEN 'a' ELSE 'b' END FROM sys.all_objects a CROSS JOIN sys.all_objects b"
  ), immediate = TRUE)
  DBI::dbExecute(conexion, paste(
    "CREATE VIEW r37x_dev_test_order_v AS SELECT id, nombre,",
    "SUM(CAST(CASE WHEN id = 250 THEN 'bad' ELSE CAST(id AS varchar(20)) END AS int)) OVER () AS cod",
    "FROM r37x_dev_test_order_src"
  ), immediate = TRUE)

  for (orden in list("id", "cod", c("nombre", "cod"), c("id", "nombre"))) {
    resultado <- lupa::perfilar_dbi(
      conexion, "r37x_dev_test_order_v", muestra = 100L,
      orden_muestra = orden, metricas = "validos",
      proteger_datos_personales = FALSE
    )
    expect_equal(nrow(resultado$perfil_muestra$columnas), 2L)
    expect_setequal(resultado$perfil_muestra$columnas$columna, c("id", "nombre"))
    cobertura <- resultado$resumen_tabla$cobertura
    fila <- cobertura[cobertura$bloque == "perfil_muestra" &
                        cobertura$estado == "alcance_distinto", , drop = FALSE]
    expect_equal(nrow(fila), 1L)
    expect_false(grepl("ninguna fallo por si sola", fila$motivo[[1L]], fixed = TRUE))
    if (any(orden %in% c("cod"))) {
      expect_true(grepl("orden_muestra", fila$motivo[[1L]], fixed = TRUE))
    } else {
      expect_true(grepl("cod", fila$motivo[[1L]], fixed = TRUE))
    }
  }
})

testthat::test_that("SQL Server no pierde la transaccion del usuario al leer una vista rota", {
  conexion <- .r37x_mssql()
  otra <- .r37x_mssql()
  on.exit({
    try(DBI::dbRollback(conexion), silent = TRUE)
    try(DBI::dbExecute(conexion, "DROP VIEW r37x_dev_test_tx_v", immediate = TRUE), silent = TRUE)
    try(DBI::dbExecute(conexion, "DROP TABLE r37x_dev_test_tx", immediate = TRUE), silent = TRUE)
    try(DBI::dbExecute(conexion, "DROP TABLE r37x_dev_test_tx_user", immediate = TRUE), silent = TRUE)
    try(DBI::dbDisconnect(otra), silent = TRUE)
    try(DBI::dbDisconnect(conexion), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(conexion, "IF OBJECT_ID('r37x_dev_test_tx_v') IS NOT NULL DROP VIEW r37x_dev_test_tx_v", immediate = TRUE)
  DBI::dbExecute(conexion, "IF OBJECT_ID('r37x_dev_test_tx') IS NOT NULL DROP TABLE r37x_dev_test_tx", immediate = TRUE)
  DBI::dbExecute(conexion, "IF OBJECT_ID('r37x_dev_test_tx_user') IS NOT NULL DROP TABLE r37x_dev_test_tx_user", immediate = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE r37x_dev_test_tx (id int)", immediate = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE r37x_dev_test_tx_user (id int)", immediate = TRUE)
  DBI::dbExecute(conexion, "INSERT INTO r37x_dev_test_tx VALUES (1), (2), (3), (4), (5)", immediate = TRUE)
  DBI::dbExecute(conexion, paste(
    "CREATE VIEW r37x_dev_test_tx_v AS SELECT id,",
    "CAST(CASE WHEN id = 5 THEN 'bad' ELSE CAST(id AS varchar(20)) END AS int) AS malo",
    "FROM r37x_dev_test_tx"
  ), immediate = TRUE)

  DBI::dbBegin(conexion)
  DBI::dbExecute(conexion, "INSERT INTO r37x_dev_test_tx_user VALUES (1)", immediate = TRUE)
  capturado <- tryCatch(
    lupa::perfilar_dbi(
      conexion, "r37x_dev_test_tx_v", bloque_filas = 2L, muestra = 5L,
      metricas = "validos", proteger_datos_personales = FALSE
    ),
    error = function(e) e
  )
  expect_s3_class(capturado, "lupa_error_transaccion_dbi")
  estado <- DBI::dbGetQuery(
    conexion, "SELECT @@TRANCOUNT AS n", immediate = TRUE
  )$n[[1L]]
  expect_equal(as.numeric(estado), 0)
  DBI::dbRollback(conexion)
  DBI::dbExecute(conexion, "INSERT INTO r37x_dev_test_tx_user VALUES (2)", immediate = TRUE)
  expect_equal(
    DBI::dbGetQuery(otra, "SELECT COUNT(*) AS n FROM r37x_dev_test_tx_user", immediate = TRUE)$n[[1L]],
    1
  )
})

testthat::test_that("el error de materializacion cierra el archivo del spool", {
  conexion <- .r37x_mssql()
  on.exit({
    try(DBI::dbRollback(conexion), silent = TRUE)
    try(DBI::dbExecute(conexion, "IF OBJECT_ID('r37x_dev_test_spool') IS NOT NULL DROP TABLE r37x_dev_test_spool", immediate = TRUE), silent = TRUE)
    try(DBI::dbExecute(conexion, "IF OBJECT_ID('r37x_dev_test_spool_user') IS NOT NULL DROP TABLE r37x_dev_test_spool_user", immediate = TRUE), silent = TRUE)
    try(DBI::dbDisconnect(conexion), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(conexion, "IF OBJECT_ID('r37x_dev_test_spool') IS NOT NULL DROP TABLE r37x_dev_test_spool", immediate = TRUE)
  DBI::dbExecute(conexion, "IF OBJECT_ID('r37x_dev_test_spool_user') IS NOT NULL DROP TABLE r37x_dev_test_spool_user", immediate = TRUE)
  DBI::dbExecute(conexion, paste(
    "CREATE TABLE r37x_dev_test_spool (id int, codigo varchar(20),",
    "relleno char(300), cod AS CAST(codigo AS int))"
  ), immediate = TRUE)
  DBI::dbExecute(conexion, paste(
    "INSERT INTO r37x_dev_test_spool (id, codigo, relleno)",
    "SELECT TOP 2000 ROW_NUMBER() OVER (ORDER BY (SELECT NULL)),",
    "CASE WHEN ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) % 50 = 0",
    "THEN 'x' + CAST(ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS varchar(20))",
    "ELSE CAST(ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS varchar(20)) END,",
    "'r' FROM sys.all_objects a CROSS JOIN sys.all_objects b"
  ), immediate = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE r37x_dev_test_spool_user (id int)", immediate = TRUE)
  antes <- list.files(
    tempdir(), pattern = "^lupa-spool-muestra-.*\\.bin$", full.names = TRUE
  )
  DBI::dbBegin(conexion)
  capturado <- tryCatch(
    lupa::perfilar_dbi(
      conexion, "r37x_dev_test_spool", universo = "muestra_motor",
      muestra_motor = 2000L, metricas = "validos",
      proteger_datos_personales = FALSE
    ),
    error = function(e) e
  )
  expect_s3_class(capturado, "lupa_error_transaccion_dbi")
  expect_match(
    conditionMessage(capturado),
    "durante `db(SendQuery|Fetch|ClearResult)`"
  )
  DBI::dbRollback(conexion)
  gc()
  despues <- list.files(
    tempdir(), pattern = "^lupa-spool-muestra-.*\\.bin$", full.names = TRUE
  )
  expect_length(setdiff(despues, antes), 0L)
})

testthat::test_that("char y varchar se comparan por su valor logico en SQL Server", {
  conexion <- .r37x_mssql()
  on.exit({
    try(DBI::dbExecute(conexion, "DROP TABLE r37x_dev_test_char", immediate = TRUE), silent = TRUE)
    try(DBI::dbExecute(conexion, "DROP TABLE r37x_dev_test_varchar", immediate = TRUE), silent = TRUE)
    try(DBI::dbDisconnect(conexion), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(conexion, "IF OBJECT_ID('r37x_dev_test_char') IS NOT NULL DROP TABLE r37x_dev_test_char", immediate = TRUE)
  DBI::dbExecute(conexion, "IF OBJECT_ID('r37x_dev_test_varchar') IS NOT NULL DROP TABLE r37x_dev_test_varchar", immediate = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE r37x_dev_test_char (cod char(10))", immediate = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE r37x_dev_test_varchar (cod varchar(10))", immediate = TRUE)
  DBI::dbExecute(conexion, "INSERT INTO r37x_dev_test_char VALUES ('A1'), ('A2')", immediate = TRUE)
  DBI::dbExecute(conexion, "INSERT INTO r37x_dev_test_varchar VALUES ('A1'), ('A2')", immediate = TRUE)

  resultado <- lupa::relaciones_coleccion(
    lupa::coleccion(conexion, c("r37x_dev_test_char", "r37x_dev_test_varchar")),
    pares = data.frame(
      tabla_1 = "r37x_dev_test_char", tabla_2 = "r37x_dev_test_varchar",
      stringsAsFactors = FALSE
    )
  )
  expect_true(any(resultado$relaciones$columna_tabla1 == "cod" &
                   resultado$relaciones$columna_tabla2 == "cod"))
})
