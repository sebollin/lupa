# Prueba de regresion: corre en la suite completa, en el CI y en R-hub, que usan
# NOT_CRAN=true; en CRAN se saltea para que el check entre en sus tiempos.
testthat::skip_on_cran()

testthat::skip_if_not_installed("DBI")

.o179_pg <- function(puerto) {
  testthat::skip_if_not_installed("RPostgres")
  conexion <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = puerto,
      user = "postgres", password = "lupa", dbname = "lupa"
    ),
    error = function(e) NULL
  )
  if (is.null(conexion)) {
    testthat::skip(paste0("PostgreSQL no disponible en el puerto ", puerto, "."))
  }
  conexion
}

.o179_mssql <- function() {
  testthat::skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD")
  if (!nzchar(clave)) {
    testthat::skip("Sin LUPA_MSSQL_PWD: no hay un SQL Server de prueba declarado.")
  }
  conexion <- tryCatch(
    DBI::dbConnect(
      odbc::odbc(), Driver = "FreeTDS", Server = "127.0.0.1", Port = 1433,
      UID = "sa", PWD = clave, Database = "lupa_pruebas",
      TDS_Version = "8.0", encoding = "UTF-8"
    ),
    error = function(e) NULL
  )
  if (is.null(conexion)) {
    testthat::skip("SQL Server no disponible en el puerto 1433.")
  }
  conexion
}

.o179_drop_pg <- function(conexion) {
  try(DBI::dbExecute(conexion, "DROP TABLE IF EXISTS r39x_dev_o179_pg_text"),
      silent = TRUE)
  try(DBI::dbExecute(conexion, "DROP TABLE IF EXISTS r39x_dev_o179_pg_varchar"),
      silent = TRUE)
  try(DBI::dbExecute(conexion, "DROP TABLE IF EXISTS r39x_dev_o179_pg_char"),
      silent = TRUE)
}

.o179_drop_mssql <- function(conexion) {
  try(DBI::dbExecute(conexion, "IF @@TRANCOUNT > 0 ROLLBACK"), silent = TRUE)
  try(DBI::dbExecute(conexion,
                     "IF OBJECT_ID('r39x_dev_o179_view', 'V') IS NOT NULL DROP VIEW r39x_dev_o179_view"),
      silent = TRUE)
  try(DBI::dbExecute(conexion,
                     "IF OBJECT_ID('r39x_dev_o179_syn', 'SN') IS NOT NULL DROP SYNONYM r39x_dev_o179_syn"),
      silent = TRUE)
  for (tabla in c("r39x_dev_o179_src", "r39x_dev_o179_syn_src", "r39x_dev_o179_lob",
                  "r39x_dev_o179_user")) {
    try(DBI::dbExecute(conexion,
                       paste0("IF OBJECT_ID('", tabla,
                              "', 'U') IS NOT NULL DROP TABLE ", tabla)),
        silent = TRUE)
  }
}

testthat::test_that("PostgreSQL no relaciona char con text relleno", {
  for (puerto in c(55432L, 55493L)) {
    conexion <- .o179_pg(puerto)
    on.exit({
      .o179_drop_pg(conexion)
      try(DBI::dbDisconnect(conexion), silent = TRUE)
    }, add = TRUE)
    .o179_drop_pg(conexion)
    DBI::dbExecute(conexion,
                   "CREATE TABLE r39x_dev_o179_pg_char (c char(6))")
    DBI::dbExecute(conexion,
                   "CREATE TABLE r39x_dev_o179_pg_text (id int, t text)")
    DBI::dbExecute(conexion,
                   "CREATE TABLE r39x_dev_o179_pg_varchar (id int, v varchar(10))")
    DBI::dbExecute(conexion,
                   "INSERT INTO r39x_dev_o179_pg_char SELECT 'A' || g FROM generate_series(1, 4) g")
    DBI::dbExecute(conexion,
                   "INSERT INTO r39x_dev_o179_pg_text SELECT g, 'A' || g || ' ' FROM generate_series(1, 4) g")
    DBI::dbExecute(conexion,
                   "INSERT INTO r39x_dev_o179_pg_varchar SELECT g, 'A' || g || ' ' FROM generate_series(1, 4) g")

    conteos <- DBI::dbGetQuery(conexion, paste(
      "SELECT (SELECT COUNT(*) FROM r39x_dev_o179_pg_text",
      "WHERE t IN (SELECT c FROM r39x_dev_o179_pg_char)) AS n_text,",
      "(SELECT COUNT(*) FROM r39x_dev_o179_pg_varchar",
      "WHERE v IN (SELECT c FROM r39x_dev_o179_pg_char)) AS n_varchar"
    ))
    expect_equal(as.numeric(conteos$n_text), 0)
    expect_equal(as.numeric(conteos$n_varchar), 4)

    resultado <- lupa::relaciones_coleccion(
      lupa::coleccion(conexion, c(
        "r39x_dev_o179_pg_char", "r39x_dev_o179_pg_text",
        "r39x_dev_o179_pg_varchar"
      )),
      pares = data.frame(
        tabla_1 = c("r39x_dev_o179_pg_char", "r39x_dev_o179_pg_char"),
        tabla_2 = c("r39x_dev_o179_pg_text", "r39x_dev_o179_pg_varchar"),
        stringsAsFactors = FALSE
      )
    )
    relaciones <- as.data.frame(resultado$relaciones)
    expect_false(any(relaciones$tabla_2 == "r39x_dev_o179_pg_text"))
    fila_varchar <- relaciones[
      relaciones$tabla_2 == "r39x_dev_o179_pg_varchar", , drop = FALSE
    ]
    expect_equal(nrow(fila_varchar), 1L)
    expect_equal(as.numeric(fila_varchar$n_valores_comunes), 4)
    .o179_drop_pg(conexion)
    DBI::dbDisconnect(conexion)
    conexion <- NULL
  }
})

testthat::test_that("SQL Server trata un sinonimo no clasificado como tabla", {
  conexion <- .o179_mssql()
  on.exit({
    .o179_drop_mssql(conexion)
    try(DBI::dbDisconnect(conexion), silent = TRUE)
  }, add = TRUE)
  .o179_drop_mssql(conexion)
  DBI::dbExecute(conexion,
                 "CREATE TABLE r39x_dev_o179_syn_src (id int, valor int)")
  DBI::dbExecute(conexion, paste(
    "INSERT INTO r39x_dev_o179_syn_src SELECT TOP 200",
    "ROW_NUMBER() OVER (ORDER BY (SELECT 1)),",
    "ROW_NUMBER() OVER (ORDER BY (SELECT 1))",
    "FROM sys.all_objects a CROSS JOIN sys.all_objects b"
  ))
  DBI::dbExecute(conexion,
                 "CREATE SYNONYM r39x_dev_o179_syn FOR r39x_dev_o179_syn_src")
  DBI::dbExecute(conexion, "CREATE TABLE r39x_dev_o179_user (k int)")
  DBI::dbExecute(conexion, "SET XACT_ABORT ON")
  DBI::dbBegin(conexion)
  DBI::dbExecute(conexion, "INSERT INTO r39x_dev_o179_user VALUES (1)")

  sin_orden <- tryCatch(
    lupa::perfilar_dbi(
      conexion, "r39x_dev_o179_syn", universo = "muestra_motor",
      muestra_motor = 50L, metricas = "validos",
      proteger_datos_personales = FALSE
    ),
    error = function(e) e
  )
  con_orden <- tryCatch(
    lupa::perfilar_dbi(
      conexion, "r39x_dev_o179_syn", universo = "muestra_motor",
      muestra_motor = 50L, muestra = 50L, orden_muestra = "id",
      metricas = "validos", proteger_datos_personales = FALSE
    ),
    error = function(e) e
  )
  estado <- DBI::dbGetQuery(conexion, paste(
    "SELECT @@TRANCOUNT AS trancount, XACT_STATE() AS xact_state,",
    "(SELECT COUNT(*) FROM r39x_dev_o179_user) AS fila"
  ))
  expect_s3_class(sin_orden, "perfil_dbi")
  expect_s3_class(con_orden, "perfil_dbi")
  expect_equal(as.numeric(estado$trancount), 1)
  expect_equal(as.numeric(estado$xact_state), 1)
  expect_equal(as.numeric(estado$fila), 1)
})

testthat::test_that("la lectura DBI recorta char en PostgreSQL", {
  for (puerto in c(55432L, 55493L)) {
    conexion <- .o179_pg(puerto)
    on.exit({
      .o179_drop_pg(conexion)
      try(DBI::dbDisconnect(conexion), silent = TRUE)
    }, add = TRUE)
    .o179_drop_pg(conexion)
    DBI::dbExecute(conexion,
                   "CREATE TABLE r39x_dev_o179_pg_char (id int, cod char(10))")
    DBI::dbExecute(conexion,
                   "INSERT INTO r39x_dev_o179_pg_char SELECT g, 'A' || g FROM generate_series(1, 20) g")

    resultado <- lupa::perfilar_dbi(
      conexion, "r39x_dev_o179_pg_char", muestra = 20L,
      bloque_filas = 8L, metricas = c("validos", "distintos"),
      proteger_datos_personales = FALSE
    )
    hallazgos <- resultado$perfil_muestra$hallazgos
    expect_false(any(
      hallazgos$columna == "cod" &
        hallazgos$tipo_hallazgo == "espacios_sobrantes"
    ))
    lectura <- as.data.frame(resultado$resumen_tabla$meta$lectura_convertida)
    cod <- lectura[lectura$columna == "cod", , drop = FALSE]
    expect_equal(nrow(cod), 1L)
    expect_match(cod$lectura, "RTRIM", ignore.case = TRUE)
    .o179_drop_pg(conexion)
    DBI::dbDisconnect(conexion)
    conexion <- NULL
  }
})

testthat::test_that("SQL Server declara no disponibles los agregados de tipos no comparables", {
  conexion <- .o179_mssql()
  on.exit({
    .o179_drop_mssql(conexion)
    try(DBI::dbDisconnect(conexion), silent = TRUE)
  }, add = TRUE)
  .o179_drop_mssql(conexion)
  DBI::dbExecute(conexion, paste(
    "CREATE TABLE r39x_dev_o179_lob (id int, tx text, nx ntext,",
    "im image, xm xml, ge geography)"
  ))
  DBI::dbExecute(conexion, paste(
    "INSERT INTO r39x_dev_o179_lob VALUES (1, CAST('a' AS text),",
    "CAST(N'a' AS ntext), CAST(0x01 AS image), CAST('<a/>' AS xml),",
    "geography::Point(-34.9, -56.1, 4326))"
  ))
  DBI::dbExecute(conexion, "CREATE TABLE r39x_dev_o179_user (k int)")
  DBI::dbExecute(conexion, "SET XACT_ABORT ON")
  DBI::dbBegin(conexion)
  DBI::dbExecute(conexion, "INSERT INTO r39x_dev_o179_user VALUES (1)")

  resultado <- lupa::perfilar_dbi(
    conexion, "r39x_dev_o179_lob",
    metricas = c("validos", "distintos", "basicos", "moda", "mediana", "desvio"),
    proteger_datos_personales = FALSE
  )
  estado <- DBI::dbGetQuery(conexion, paste(
    "SELECT @@TRANCOUNT AS trancount, XACT_STATE() AS xact_state,",
    "(SELECT COUNT(*) FROM r39x_dev_o179_user) AS fila"
  ))
  expect_s3_class(resultado, "perfil_dbi")
  expect_equal(as.numeric(estado$trancount), 1)
  expect_equal(as.numeric(estado$xact_state), 1)
  expect_equal(as.numeric(estado$fila), 1)

  manual <- DBI::dbGetQuery(conexion, paste(
    "SELECT COUNT(*) AS n,",
    "COUNT(CASE WHEN tx IS NULL THEN NULL ELSE 1 END) AS tx,",
    "COUNT(CASE WHEN nx IS NULL THEN NULL ELSE 1 END) AS nx,",
    "COUNT(CASE WHEN im IS NULL THEN NULL ELSE 1 END) AS im,",
    "COUNT(CASE WHEN xm IS NULL THEN NULL ELSE 1 END) AS xm,",
    "COUNT(CASE WHEN ge IS NULL THEN NULL ELSE 1 END) AS ge",
    "FROM r39x_dev_o179_lob"
  ))
  expect_equal(as.numeric(manual$n), 1)
  expect_equal(as.numeric(manual[c("tx", "nx", "im", "xm", "ge")]),
               rep(1, 5))

  sql <- as.data.frame(resultado$resumen_tabla$sql)
  lob <- c("tx", "nx", "im", "xm", "ge")
  incompatibles <- sql[
    sql$columna %in% lob & sql$metrica %in% c("n_distintos", "moda"),
    , drop = FALSE
  ]
  expect_equal(nrow(incompatibles), 10L)
  expect_true(all(incompatibles$estado == "no_disponible"))
  expect_true(all(is.na(incompatibles$sql)))
  filas_lob <- resultado$resumen_tabla$columnas[
    resultado$resumen_tabla$columnas$columna %in% lob, , drop = FALSE
  ]
  expect_equal(as.numeric(filas_lob$n_validos), rep(1, 5))
})

testthat::test_that("SQL Server muestrea una vista sin emitir TABLESAMPLE", {
  conexion <- .o179_mssql()
  on.exit({
    .o179_drop_mssql(conexion)
    try(DBI::dbDisconnect(conexion), silent = TRUE)
  }, add = TRUE)
  .o179_drop_mssql(conexion)
  DBI::dbExecute(conexion,
                 "CREATE TABLE r39x_dev_o179_src (id int, valor int)")
  DBI::dbExecute(conexion, paste(
    "INSERT INTO r39x_dev_o179_src SELECT TOP 200",
    "ROW_NUMBER() OVER (ORDER BY (SELECT 1)),",
    "ROW_NUMBER() OVER (ORDER BY (SELECT 1))",
    "FROM sys.all_objects a CROSS JOIN sys.all_objects b"
  ))
  DBI::dbExecute(conexion,
                 "CREATE VIEW r39x_dev_o179_view AS SELECT id, valor FROM r39x_dev_o179_src")
  DBI::dbExecute(conexion, "CREATE TABLE r39x_dev_o179_user (k int)")
  DBI::dbExecute(conexion, "SET XACT_ABORT ON")
  DBI::dbBegin(conexion)
  DBI::dbExecute(conexion, "INSERT INTO r39x_dev_o179_user VALUES (1)")

  resultado <- lupa::perfilar_dbi(
    conexion, "r39x_dev_o179_view", universo = "muestra_motor",
    muestra_motor = 50L, metricas = "validos",
    proteger_datos_personales = FALSE
  )
  estado <- DBI::dbGetQuery(conexion, paste(
    "SELECT @@TRANCOUNT AS trancount, XACT_STATE() AS xact_state,",
    "(SELECT COUNT(*) FROM r39x_dev_o179_user) AS fila"
  ))
  expect_s3_class(resultado, "perfil_dbi")
  expect_equal(as.numeric(estado$trancount), 1)
  expect_equal(as.numeric(estado$xact_state), 1)
  expect_equal(as.numeric(estado$fila), 1)
  expect_identical(resultado$resumen_tabla$meta$muestreo$metodo, "random_limit")
  sql <- as.data.frame(resultado$resumen_tabla$sql)
  expect_false(any(grepl("TABLESAMPLE", sql$sql, ignore.case = TRUE), na.rm = TRUE))
  fila <- resultado$resumen_tabla$columnas[
    resultado$resumen_tabla$columnas$columna == "id", , drop = FALSE
  ]
  expect_true(as.numeric(fila$n_validos) > 0)
  expect_true(as.numeric(fila$n_validos) <= 200)
})

testthat::test_that("la coleccion tambien muestrea una vista de SQL Server", {
  conexion <- .o179_mssql()
  on.exit({
    .o179_drop_mssql(conexion)
    try(DBI::dbDisconnect(conexion), silent = TRUE)
  }, add = TRUE)
  .o179_drop_mssql(conexion)
  DBI::dbExecute(conexion,
                 "CREATE TABLE r39x_dev_o179_src (id int, valor int)")
  DBI::dbExecute(conexion, paste(
    "INSERT INTO r39x_dev_o179_src SELECT TOP 200",
    "ROW_NUMBER() OVER (ORDER BY (SELECT 1)),",
    "ROW_NUMBER() OVER (ORDER BY (SELECT 1))",
    "FROM sys.all_objects a CROSS JOIN sys.all_objects b"
  ))
  DBI::dbExecute(conexion,
                 "CREATE VIEW r39x_dev_o179_view AS SELECT id, valor FROM r39x_dev_o179_src")
  DBI::dbExecute(conexion, "CREATE TABLE r39x_dev_o179_user (k int)")
  DBI::dbExecute(conexion, "SET XACT_ABORT ON")
  DBI::dbBegin(conexion)
  DBI::dbExecute(conexion, "INSERT INTO r39x_dev_o179_user VALUES (1)")

  resultado <- lupa::perfilar_coleccion(
    lupa::coleccion(conexion, "r39x_dev_o179_view"),
    universo = "muestra_motor", muestra_motor = 50L,
    proteger_datos_personales = FALSE
  )
  estado <- DBI::dbGetQuery(conexion, paste(
    "SELECT @@TRANCOUNT AS trancount, XACT_STATE() AS xact_state,",
    "(SELECT COUNT(*) FROM r39x_dev_o179_user) AS fila"
  ))
  expect_s3_class(resultado, "perfil_coleccion")
  expect_equal(as.numeric(estado$trancount), 1)
  expect_equal(as.numeric(estado$xact_state), 1)
  expect_equal(as.numeric(estado$fila), 1)
})
