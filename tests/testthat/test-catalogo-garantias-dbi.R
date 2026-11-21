testthat::skip_on_cran()
skip_if_not_installed("DBI")

.cg_conectar_pg <- function(puerto) {
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(puerto),
    user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en puerto ", puerto, "."))
  con
}

.cg_conectar_mssql <- function() {
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(),
    Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"),
    PWD = clave,
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0",
    TrustServerCertificate = "yes",
    timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  con
}

test_that("SQLite lee indices unicos (totales y parciales), FKs y NOT NULL con blancos", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")

  DBI::dbExecute(con, "CREATE TABLE lupa_garantias_padre (pid INTEGER PRIMARY KEY, cod TEXT NOT NULL)")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_padre VALUES (1, 'A'), (2, 'B')")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_garantias_hijo (",
    "hid INTEGER NOT NULL, ",
    "pid INTEGER REFERENCES lupa_garantias_padre(pid), ",
    "txt TEXT NOT NULL, ",
    "nota TEXT, ",
    "parcial_c INTEGER)"
  ))
  DBI::dbExecute(con, "CREATE UNIQUE INDEX idx_lupa_garantias_hid ON lupa_garantias_hijo(hid)")
  DBI::dbExecute(con, "CREATE UNIQUE INDEX idx_lupa_garantias_parcial ON lupa_garantias_hijo(parcial_c) WHERE parcial_c > 0")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_hijo VALUES (10, 1, '', 'ok', 1), (20, 2, '   ', NULL, 2), (30, 1, 'N/A', 'x', NULL)")

  plan <- plan_perfilado_dbi(con, "lupa_garantias_hijo", bloque_muestra = "solo_agregados")
  perf <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_hijo", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))

  cat_meta <- perf$resumen_tabla$meta$catalogo
  expect_true(is.list(cat_meta))
  expect_identical(cat_meta$identificacion$grado, "total")
  expect_identical(cat_meta$identificacion$tipo, "indice_unico")
  expect_identical(cat_meta$identificacion$columnas, "hid")

  uk <- cat_meta$claves_unicas
  expect_equal(nrow(uk), 2L)
  fila_hid <- uk[uk$nombre == "idx_lupa_garantias_hid", , drop = FALSE]
  fila_par <- uk[uk$nombre == "idx_lupa_garantias_parcial", , drop = FALSE]
  expect_identical(fila_hid$grado, "total")
  expect_false(isTRUE(fila_hid$es_parcial))
  expect_identical(fila_hid$semantica_null, "nulls_distinct")
  expect_identical(fila_par$grado, "parcial")
  expect_true(isTRUE(fila_par$es_parcial))
  expect_match(fila_par$filtro, "parcial_c")

  fk <- cat_meta$claves_foraneas
  expect_equal(nrow(fk), 1L)
  expect_true(isTRUE(fk$validada))
  expect_identical(fk$grado, "parcial")

  nn <- cat_meta$not_null
  fila_txt <- nn[nn$columna == "txt", , drop = FALSE]
  expect_true(isTRUE(fila_txt$not_null))
  expect_identical(fila_txt$grado, "total")
  expect_equal(fila_txt$n_blancos, 2)
  expect_true(isTRUE(fila_txt$not_null_con_blancos))

  # Cobertura avisa NOT NULL con blancos
  cob <- perf$resumen_tabla$cobertura
  expect_true(any(grepl("NOT NULL con blancos", cob$motivo, fixed = TRUE)))

  # El resumen impreso no describe la tabla como "no declara clave"
  impreso <- paste(utils::capture.output(print(perf), type = "message"), collapse = "\n")
  expect_false(grepl("no declara clave", impreso, fixed = TRUE))
  expect_true(grepl("NOT NULL con blancos", impreso, fixed = TRUE))

  # Plan exacto y ahorro de consultas (duplicados garantizados, sin verificacion de orden)
  expect_identical(perf$resumen_tabla$meta$duplicados_exactos$estado, "garantizado")
  expect_identical(as.integer(perf$resumen_tabla$meta$duplicados_exactos$filas_duplicadas), 0L)
  expect_equal(perf$resumen_tabla$meta$consultas$emitidas, attr(plan, "total"))

  # Con muestra finita se siguen contando faltantes disfrazados sobre columnas NOT NULL
  plan_ord <- plan_perfilado_dbi(con, "lupa_garantias_hijo", orden_muestra = "hid", muestra = 10L)
  perf_ord <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_hijo", orden_muestra = "hid", muestra = 10L,
    proteger_datos_personales = FALSE
  )))
  col_txt <- perf_ord$resumen_tabla$columnas[perf_ord$resumen_tabla$columnas$columna == "txt", , drop = FALSE]
  expect_equal(col_txt$n_faltantes_disfrazados, 3)
  expect_false(any(perf_ord$resumen_tabla$sql$etapa == "orden_muestra"))
  expect_gte(perf_ord$resumen_tabla$meta$consultas$emitidas, attr(plan_ord, "total"))
  expect_lte(perf_ord$resumen_tabla$meta$consultas$emitidas, attr(plan_ord, "total_maximo"))

  # Tabla con solo columnas INT NOT NULL y metricas = "validos" ahorra COUNT(col)
  DBI::dbExecute(con, "CREATE TABLE lupa_garantias_solo_nn (a INTEGER NOT NULL, b INTEGER NOT NULL)")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_solo_nn VALUES (1, 10), (2, 20)")
  plan_nn <- plan_perfilado_dbi(con, "lupa_garantias_solo_nn", metricas = "validos", bloque_muestra = "solo_agregados")
  perf_nn <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_solo_nn", metricas = "validos", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))
  expect_equal(perf_nn$resumen_tabla$columnas$n_validos, c(2, 2))
  expect_equal(perf_nn$resumen_tabla$columnas$n_faltantes, c(0, 0))
  expect_equal(perf_nn$resumen_tabla$meta$consultas$emitidas, attr(plan_nn, "total"))
  # Sin PK ni indice unico sigue diciendo "no declara clave"
  impreso_nn <- paste(utils::capture.output(print(perf_nn), type = "message"), collapse = "\n")
  expect_true(grepl("no declara clave", impreso_nn, fixed = TRUE))

  # Con PRAGMA foreign_keys = OFF la FK queda grado ninguna
  DBI::dbExecute(con, "PRAGMA foreign_keys = OFF")
  perf_off <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_hijo", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))
  expect_identical(perf_off$resumen_tabla$meta$catalogo$claves_foraneas$grado, "ninguna")
  expect_false(isTRUE(perf_off$resumen_tabla$meta$catalogo$claves_foraneas$validada))
})

test_that("DuckDB lee indice unico, FK validada, NOT NULL con blancos y plan exacto", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "CREATE TABLE lupa_garantias_padre (pid INTEGER PRIMARY KEY, cod VARCHAR NOT NULL)")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_padre VALUES (1, 'A'), (2, 'B')")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_garantias_hijo (",
    "hid INTEGER NOT NULL, ",
    "pid INTEGER REFERENCES lupa_garantias_padre(pid), ",
    "txt VARCHAR NOT NULL, ",
    "nota VARCHAR)"
  ))
  DBI::dbExecute(con, "CREATE UNIQUE INDEX idx_lupa_garantias_hid ON lupa_garantias_hijo(hid)")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_hijo VALUES (10, 1, '', 'ok'), (20, 2, '   ', NULL), (30, 1, 'hola', 'x')")

  plan <- plan_perfilado_dbi(con, "lupa_garantias_hijo", bloque_muestra = "solo_agregados")
  perf <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_hijo", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))

  cat_meta <- perf$resumen_tabla$meta$catalogo
  expect_identical(cat_meta$identificacion$grado, "total")
  expect_true(cat_meta$identificacion$tipo %in% c("indice_unico", "restriccion_unica"))
  expect_identical(cat_meta$claves_foraneas$grado, "total")
  expect_true(isTRUE(cat_meta$claves_foraneas$validada))
  fila_txt <- cat_meta$not_null[cat_meta$not_null$columna == "txt", , drop = FALSE]
  expect_true(isTRUE(fila_txt$not_null_con_blancos))
  expect_equal(fila_txt$n_blancos, 2)
  expect_identical(perf$resumen_tabla$meta$duplicados_exactos$estado, "garantizado")
  expect_equal(perf$resumen_tabla$meta$consultas$emitidas, attr(plan, "total"))
})

.cg_probar_pg <- function(puerto, probar_nnd = FALSE) {
  con <- .cg_conectar_pg(puerto)
  limpiar <- function() {
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_garantias_cg_hijo"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_garantias_cg_padre"), silent = TRUE)
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  DBI::dbExecute(con, "CREATE TABLE lupa_garantias_cg_padre (pid INTEGER PRIMARY KEY, cod VARCHAR(20) NOT NULL)")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_cg_padre VALUES (1, 'A'), (2, 'B')")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_garantias_cg_hijo (",
    "hid INTEGER NOT NULL, ",
    "pid INTEGER REFERENCES lupa_garantias_cg_padre(pid), ",
    "pid_nv INTEGER, ",
    "txt VARCHAR(30) NOT NULL, ",
    "parcial_c INTEGER)"
  ))
  DBI::dbExecute(con, "CREATE UNIQUE INDEX idx_lupa_garantias_cg_hid ON lupa_garantias_cg_hijo(hid)")
  DBI::dbExecute(con, "CREATE UNIQUE INDEX idx_lupa_garantias_cg_par ON lupa_garantias_cg_hijo(parcial_c) WHERE parcial_c > 0")
  DBI::dbExecute(con, "ALTER TABLE lupa_garantias_cg_hijo ADD CONSTRAINT fk_lupa_garantias_cg_nv FOREIGN KEY (pid_nv) REFERENCES lupa_garantias_cg_padre(pid) NOT VALID")
  if (isTRUE(probar_nnd)) {
    DBI::dbExecute(con, "ALTER TABLE lupa_garantias_cg_hijo ADD COLUMN u_nnd INTEGER")
    DBI::dbExecute(con, "CREATE UNIQUE INDEX idx_lupa_garantias_cg_nnd ON lupa_garantias_cg_hijo(u_nnd) NULLS NOT DISTINCT")
    DBI::dbExecute(con, "INSERT INTO lupa_garantias_cg_hijo VALUES (10, 1, 1, '', 1, 100), (20, 2, 2, '   ', 2, NULL)")
  } else {
    DBI::dbExecute(con, "INSERT INTO lupa_garantias_cg_hijo VALUES (10, 1, 1, '', 1), (20, 2, 2, '   ', 2)")
  }

  plan <- plan_perfilado_dbi(con, "lupa_garantias_cg_hijo", bloque_muestra = "solo_agregados")
  perf <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_cg_hijo", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))
  cat_meta <- perf$resumen_tabla$meta$catalogo
  expect_identical(cat_meta$identificacion$grado, "total")
  expect_identical(cat_meta$identificacion$tipo, "indice_unico")

  uk <- cat_meta$claves_unicas
  expect_identical(uk$grado[uk$nombre == "idx_lupa_garantias_cg_hid"], "total")
  expect_identical(uk$grado[uk$nombre == "idx_lupa_garantias_cg_par"], "parcial")
  if (isTRUE(probar_nnd)) {
    expect_identical(uk$semantica_null[uk$nombre == "idx_lupa_garantias_cg_nnd"], "nulls_not_distinct")
    expect_identical(uk$grado[uk$nombre == "idx_lupa_garantias_cg_nnd"], "total")
  }

  fk <- cat_meta$claves_foraneas
  fk_val <- fk[fk$nombre != "fk_lupa_garantias_cg_nv", , drop = FALSE]
  fk_nv  <- fk[fk$nombre == "fk_lupa_garantias_cg_nv", , drop = FALSE]
  expect_true(isTRUE(fk_val$validada))
  expect_identical(fk_val$grado, "total")
  expect_false(isTRUE(fk_nv$validada))
  expect_identical(fk_nv$grado, "parcial")

  fila_txt <- cat_meta$not_null[cat_meta$not_null$columna == "txt", , drop = FALSE]
  expect_true(isTRUE(fila_txt$not_null_con_blancos))
  expect_equal(fila_txt$n_blancos, 2)
  expect_identical(perf$resumen_tabla$meta$duplicados_exactos$estado, "garantizado")
  expect_equal(perf$resumen_tabla$meta$consultas$emitidas, attr(plan, "total"))
}

test_that("PostgreSQL actual lee garantias completas del catalogo y plan exacto", {
  .cg_probar_pg(Sys.getenv("LUPA_PG_PORT", "55432"), probar_nnd = TRUE)
})

test_that("PostgreSQL 9.3 lee garantias del catalogo (parciales, NOT VALID, NOT NULL)", {
  .cg_probar_pg(Sys.getenv("LUPA_PG93_PORT", "55493"), probar_nnd = FALSE)
})

test_that("SQL Server lee indices unicos filtrados, FK is_not_trusted y NOT NULL con blancos", {
  con <- .cg_conectar_mssql()
  limpiar <- function() {
    try(DBI::dbExecute(con, "IF OBJECT_ID(N'lupa_garantias_cg_hijo', N'U') IS NOT NULL DROP TABLE lupa_garantias_cg_hijo"), silent = TRUE)
    try(DBI::dbExecute(con, "IF OBJECT_ID(N'lupa_garantias_cg_padre', N'U') IS NOT NULL DROP TABLE lupa_garantias_cg_padre"), silent = TRUE)
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  DBI::dbExecute(con, "CREATE TABLE lupa_garantias_cg_padre (pid INT PRIMARY KEY, cod VARCHAR(20) NOT NULL)")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_cg_padre VALUES (1, 'A'), (2, 'B')")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_garantias_cg_hijo (",
    "hid INT NOT NULL, ",
    "pid INT REFERENCES lupa_garantias_cg_padre(pid), ",
    "pid_nt INT, ",
    "txt VARCHAR(30) NOT NULL, ",
    "parcial_c INT)"
  ))
  DBI::dbExecute(con, "CREATE UNIQUE INDEX idx_lupa_garantias_cg_hid ON lupa_garantias_cg_hijo(hid)")
  DBI::dbExecute(con, "CREATE UNIQUE INDEX idx_lupa_garantias_cg_par ON lupa_garantias_cg_hijo(parcial_c) WHERE parcial_c IS NOT NULL")
  DBI::dbExecute(con, "ALTER TABLE lupa_garantias_cg_hijo WITH NOCHECK ADD CONSTRAINT fk_lupa_garantias_cg_nt FOREIGN KEY (pid_nt) REFERENCES lupa_garantias_cg_padre(pid)")
  DBI::dbExecute(con, "INSERT INTO lupa_garantias_cg_hijo VALUES (10, 1, 1, '', 1), (20, 2, 2, '   ', 2)")

  plan <- plan_perfilado_dbi(con, "lupa_garantias_cg_hijo", bloque_muestra = "solo_agregados")
  perf <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "lupa_garantias_cg_hijo", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))
  cat_meta <- perf$resumen_tabla$meta$catalogo
  expect_identical(cat_meta$identificacion$grado, "total")
  expect_identical(cat_meta$identificacion$tipo, "indice_unico")

  uk <- cat_meta$claves_unicas
  expect_identical(uk$semantica_null[uk$nombre == "idx_lupa_garantias_cg_hid"], "nulls_not_distinct")
  expect_identical(uk$grado[uk$nombre == "idx_lupa_garantias_cg_hid"], "total")
  expect_identical(uk$grado[uk$nombre == "idx_lupa_garantias_cg_par"], "parcial")

  fk <- cat_meta$claves_foraneas
  fk_val <- fk[fk$nombre != "fk_lupa_garantias_cg_nt", , drop = FALSE]
  fk_nt  <- fk[fk$nombre == "fk_lupa_garantias_cg_nt", , drop = FALSE]
  expect_true(isTRUE(fk_val$validada))
  expect_identical(fk_val$grado, "total")
  expect_false(isTRUE(fk_nt$validada))
  expect_identical(fk_nt$grado, "parcial")

  expect_true(isTRUE(cat_meta$not_null$not_null_con_blancos[cat_meta$not_null$columna == "txt"]))
  expect_equal(perf$resumen_tabla$meta$consultas$emitidas, attr(plan, "total"))
})
