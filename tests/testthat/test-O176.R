# Prueba de regresion: corre en la suite completa, en el CI y en R-hub, que usan
# NOT_CRAN=true; en CRAN se saltea para que el check entre en sus tiempos.
testthat::skip_on_cran()

skip_if_not_installed("DBI")

.o176_pg <- function() {
  skip_if_not_installed("RPostgres")
  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = 55432,
      user = "postgres", password = "lupa", dbname = "lupa"
    ),
    error = function(e) NULL
  )
  if (is.null(con)) skip("PostgreSQL real no disponible en 55432.")
  con
}

.o176_ms <- function() {
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD")
  if (!nzchar(clave)) {
    skip("Sin LUPA_MSSQL_PWD: no hay un SQL Server de prueba declarado.")
  }
  con <- tryCatch(
    DBI::dbConnect(
      odbc::odbc(), Driver = "FreeTDS", Server = "127.0.0.1",
      Port = 1433, UID = "sa", PWD = clave,
      Database = "lupa_pruebas", TDS_Version = "8.0", encoding = "UTF-8"
    ),
    error = function(e) NULL
  )
  if (is.null(con)) skip("SQL Server real no disponible en 1433.")
  con
}

.o176_exec <- function(con, sql) {
  DBI::dbExecute(con, sql, immediate = inherits(con, "OdbcConnection"))
}

test_that("detectar_relaciones no recorta varchar por regla global", {
  resultado <- lupa::detectar_relaciones(
    data.frame(k = c("A1 ", "A2"), stringsAsFactors = FALSE),
    data.frame(k = c("A1", "A2"), stringsAsFactors = FALSE),
    muestra = Inf
  )
  expect_identical(resultado$n_valores_comunes, 1L)
})

test_that("PostgreSQL declara la columna ilegible y compara char con varchar", {
  con <- .o176_pg()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tablas <- c("r38x_dev_o176_pg_ref", "r38x_dev_o176_pg_child")
  vista <- "r38x_dev_o176_pg_bad"
  on.exit({
    for (objeto in c(vista, tablas)) {
      try(DBI::dbExecute(con, paste0(
        if (identical(objeto, vista)) "DROP VIEW IF EXISTS " else
          "DROP TABLE IF EXISTS ", DBI::dbQuoteIdentifier(con, objeto)
      )), silent = TRUE)
    }
  }, add = TRUE)
  try(.o176_exec(con, paste0("DROP VIEW IF EXISTS ",
    DBI::dbQuoteIdentifier(con, vista))), silent = TRUE)
  for (tabla_vieja in tablas) {
    try(.o176_exec(con, paste0("DROP TABLE IF EXISTS ",
      DBI::dbQuoteIdentifier(con, tabla_vieja))), silent = TRUE)
  }
  .o176_exec(con, paste0(
    "CREATE TABLE ", DBI::dbQuoteIdentifier(con, tablas[[1L]]),
    " (k char(4), id integer)"
  ))
  .o176_exec(con, paste0(
    "CREATE TABLE ", DBI::dbQuoteIdentifier(con, tablas[[2L]]),
    " (k varchar(4), id integer)"
  ))
  .o176_exec(con, paste0(
    "INSERT INTO ", DBI::dbQuoteIdentifier(con, tablas[[1L]]),
    " VALUES ('A1', 1), ('A2', 2)"
  ))
  .o176_exec(con, paste0(
    "INSERT INTO ", DBI::dbQuoteIdentifier(con, tablas[[2L]]),
    " VALUES ('A1 ', 1), ('A2 ', 2)"
  ))
  .o176_exec(con, paste0(
    "CREATE VIEW ", DBI::dbQuoteIdentifier(con, vista),
    " AS SELECT id, k, CAST(k AS integer) AS bad FROM ",
    DBI::dbQuoteIdentifier(con, tablas[[2L]])
  ))
  # Se prueba el camino que aisla una lectura rota sin perder la relacion sana.
  salida <- lupa::relaciones_coleccion(
    lupa::coleccion(con, c(tablas[[1L]], vista)),
    data.frame(tabla_1 = tablas[[1L]], tabla_2 = vista), muestra = Inf,
    columnas_candidatas = stats::setNames(
      list("id", c("id", "bad")), c(tablas[[1L]], vista)
    )
  )
  expect_true(any(
    salida$cobertura_podas$motivo == "columna_lectura_fallida" &
      salida$cobertura_podas$columna_tabla2 == "bad"
  ))
  expect_true(any(
    salida$relaciones$columna_tabla1 == "id" &
      salida$relaciones$columna_tabla2 == "id"
  ))

  # El par char-varchar se compara por la igualdad del motor, no por el valor
  # con relleno que el controlador entrega para varchar.
  salida_relleno <- lupa::relaciones_coleccion(
    lupa::coleccion(con, tablas),
    data.frame(tabla_1 = tablas[[1L]], tabla_2 = tablas[[2L]]),
    muestra = Inf, columnas_candidatas = stats::setNames(
      list(c("k"), c("k")), tablas
    )
  )
  fila <- salida_relleno$relaciones[1L, , drop = FALSE]
  expect_identical(fila$n_valores_comunes, 2L)
})

test_that("SQL Server ordena una vista por la columna cruda y conserva el par", {
  con <- .o176_ms()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabla <- "r38x_dev_o176_ms_order"
  vista <- "r38x_dev_o176_ms_order_v"
  on.exit({
    try(.o176_exec(con, paste0("DROP VIEW IF EXISTS ", vista)), silent = TRUE)
    try(.o176_exec(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  }, add = TRUE)
  try(.o176_exec(con, paste0("DROP VIEW IF EXISTS ", vista)), silent = TRUE)
  try(.o176_exec(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  .o176_exec(con, paste0(
    "CREATE TABLE ", tabla,
    " (id int, c char(6), codigo varchar(10), k int)"
  ))
  .o176_exec(con, paste0(
    "INSERT INTO ", tabla,
    " VALUES (1, '0001', '1', 1), (2, '0002', '2', 2),",
    " (3, '0003', 'q', 3), (4, '0004', '4', 4)"
  ))
  .o176_exec(con, paste0(
    "CREATE VIEW ", vista, " AS SELECT id, c, CAST(codigo AS int) AS bad, k FROM ", tabla
  ))
  perfil <- lupa::perfilar_dbi(
    con, vista, muestra = 2, orden_muestra = "c",
    proteger_datos_personales = FALSE
  )
  expect_false(is.null(perfil$perfil_muestra))
  expect_equal(perfil$perfil_muestra$meta$filas_analizadas, 2)
  expect_false(any(grepl("orden_muestra.*no se pudo", perfil$resumen_tabla$cobertura$motivo)))
})

test_that("muestra_motor no publica el perfil aislado como resumen", {
  con <- .o176_ms()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabla <- "r38x_dev_o176_ms_spool"
  vista <- "r38x_dev_o176_ms_spool_v"
  on.exit({
    try(.o176_exec(con, paste0("DROP VIEW IF EXISTS ", vista)), silent = TRUE)
    try(.o176_exec(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  }, add = TRUE)
  try(.o176_exec(con, paste0("DROP VIEW IF EXISTS ", vista)), silent = TRUE)
  try(.o176_exec(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  .o176_exec(con, paste0(
    "CREATE TABLE ", tabla, " (id int, codigo varchar(10), c varchar(4))"
  ))
  .o176_exec(con, paste0(
    "INSERT INTO ", tabla,
    " VALUES (1, '1', 'a'), (2, '2', 'b'), (3, 'q', 'c'), (4, '4', 'd')"
  ))
  .o176_exec(con, paste0(
    "CREATE VIEW ", vista, " AS SELECT id, c, CAST(codigo AS int) AS bad FROM ", tabla
  ))
  perfil <- lupa::perfilar_dbi(
    con, vista, universo = "muestra_motor", muestra_motor = 4,
    muestra = 2, proteger_datos_personales = FALSE
  )
  fila <- perfil$resumen_tabla$sql[
    perfil$resumen_tabla$sql$columna == "id" &
      perfil$resumen_tabla$sql$metrica == "n_validos", , drop = FALSE
  ]
  expect_identical(fila$estado, "no_disponible")
  expect_true(is.na(fila$tamano_muestra))
  expect_true(is.na(fila$fraccion))
  expect_true(any(perfil$resumen_tabla$cobertura$bloque == "resumen_tabla" &
    perfil$resumen_tabla$cobertura$estado == "no_disponible"))
})

test_that("nombres DBI con bytes se detienen antes del controlador", {
  con <- .o176_pg()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  bytes <- rawToChar(as.raw(0xf3))
  Encoding(bytes) <- "bytes"
  resultado <- tryCatch(
    lupa::perfilar_dbi(con, bytes), error = function(e) e
  )
  expect_s3_class(resultado, "lupa_error_codificacion_dbi")
  plan <- tryCatch(
    lupa::plan_perfilado_dbi(con, bytes), error = function(e) e
  )
  expect_s3_class(plan, "lupa_error_codificacion_dbi")
})

test_that("la etapa publicada distingue dbSendQuery y transaccion implicita", {
  con <- .o176_ms()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabla <- "r38x_dev_o176_ms_tx"
  vista <- "r38x_dev_o176_ms_tx_v"
  on.exit({
    try(.o176_exec(con, "IF @@TRANCOUNT > 0 ROLLBACK"), silent = TRUE)
    try(.o176_exec(con, paste0("DROP VIEW IF EXISTS ", vista)), silent = TRUE)
    try(.o176_exec(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  }, add = TRUE)
  try(.o176_exec(con, paste0("DROP VIEW IF EXISTS ", vista)), silent = TRUE)
  try(.o176_exec(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  .o176_exec(con, paste0("CREATE TABLE ", tabla, " (id int, codigo varchar(10))"))
  .o176_exec(con, paste0(
    "INSERT INTO ", tabla, " VALUES (1, 'q'), (2, '2')"
  ))
  .o176_exec(con, paste0(
    "CREATE VIEW ", vista, " AS SELECT id, CAST(codigo AS int) AS bad FROM ", tabla
  ))
  .o176_exec(con, "SET IMPLICIT_TRANSACTIONS ON")
  resultado <- tryCatch(lupa::perfilar_dbi(
    con, vista, proteger_datos_personales = FALSE
  ), error = function(e) e)
  expect_s3_class(resultado, "lupa_error_transaccion_dbi")
  expect_match(conditionMessage(resultado), "dbSendQuery", fixed = TRUE)
  expect_match(conditionMessage(resultado), "transaccion implicita", fixed = TRUE)
})

test_that("columnas_opcionales con bytes tiene clase propia", {
  con <- .o176_pg()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  bytes <- rawToChar(as.raw(0xf3))
  Encoding(bytes) <- "bytes"
  resultado <- tryCatch(
    lupa::perfilar_dbi(con, "pg_catalog.pg_class", columnas_opcionales = bytes),
    error = function(e) e
  )
  expect_s3_class(resultado, "lupa_error_codificacion_dbi")
})
