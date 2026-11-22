# Pruebas de muestra reproducible por huella en el motor (con codificacion
# canonica UTF-8 y alcance declarado) y marca de estabilidad con contadores
# del motor en SQLite, DuckDB, PostgreSQL (actual y 9.3) y SQL Server.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.mre_fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")

.mre_conectar_pg <- function(puerto) {
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

.mre_conectar_mssql <- function() {
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

.mre_args_base <- function(...) {
  c(list(
    proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    fecha = .mre_fecha
  ), list(...))
}

.mre_perfilar <- function(con, tabla, ...) {
  suppressWarnings(suppressMessages(do.call(
    perfilar_dbi, c(list(con, tabla), .mre_args_base(...))
  )))
}

.mre_plan <- function(con, tabla, ...) {
  suppressWarnings(suppressMessages(plan_perfilado_dbi(con, tabla, ...)))
}

.mre_verificar_par_orden_invertido <- function(con, tb_a, tb_b, muestra = 30L,
                                               clave = NULL,
                                               origen_esperado = "clave_primaria",
                                               con_contador = TRUE,
                                               alcance_esperado = "entre_motores_md5",
                                               info = tb_a) {
  p_plan <- if (is.null(clave)) {
    .mre_plan(con, tb_a, muestra = muestra)
  } else {
    .mre_plan(con, tb_a, muestra = muestra, clave = clave)
  }
  r1 <- if (is.null(clave)) {
    .mre_perfilar(con, tb_a, muestra = muestra)
  } else {
    .mre_perfilar(con, tb_a, muestra = muestra, clave = clave)
  }
  r2 <- if (is.null(clave)) {
    .mre_perfilar(con, tb_a, muestra = muestra)
  } else {
    .mre_perfilar(con, tb_a, muestra = muestra, clave = clave)
  }
  rb <- if (is.null(clave)) {
    .mre_perfilar(con, tb_b, muestra = muestra)
  } else {
    .mre_perfilar(con, tb_b, muestra = muestra, clave = clave)
  }

  m1 <- r1$perfil_muestra$meta$origen_dbi$muestreo
  m2 <- r2$perfil_muestra$meta$origen_dbi$muestreo
  mb <- rb$perfil_muestra$meta$origen_dbi$muestreo

  expect_true(isTRUE(m1$reproducible), info = info)
  expect_identical(m1$metodo, "muestra_huella_motor", info = info)
  expect_identical(m1$origen_clave_muestra, origen_esperado, info = info)
  expect_identical(m1$alcance_reproducibilidad, alcance_esperado, info = info)
  expect_equal(m1$filas_obtenidas, muestra, info = info)
  expect_true(is.list(r1$perfil_muestra$meta$muestra_huella), info = info)
  expect_identical(
    r1$perfil_muestra$meta$muestra_huella$metodo, "muestra_huella_motor",
    info = info
  )
  expect_identical(
    r1$perfil_muestra$meta$muestra_huella$alcance_reproducibilidad,
    alcance_esperado, info = info
  )

  # Dos corridas sobre la misma tabla producen exactamente el mismo perfil de muestra.
  expect_identical(r1$perfil_muestra$columnas, r2$perfil_muestra$columnas, info = info)
  expect_identical(r1$perfil_muestra$general, r2$perfil_muestra$general, info = info)
  expect_identical(m1$sql_muestra, m2$sql_muestra, info = info)

  # Incluso con las mismas filas insertadas en orden fisico invertido, la
  # muestra seleccionada es identica.
  expect_identical(r1$perfil_muestra$columnas, rb$perfil_muestra$columnas, info = paste(info, "invertida"))
  expect_identical(r1$perfil_muestra$general, rb$perfil_muestra$general, info = paste(info, "invertida"))
  expect_true(isTRUE(mb$reproducible), info = paste(info, "invertida"))

  # Plan exacto: las consultas emitidas coinciden con `attr(plan, "total")`.
  expect_identical(
    as.numeric(r1$resumen_tabla$meta$consultas$emitidas),
    as.numeric(attr(p_plan, "total")),
    info = paste(info, "plan")
  )
  # Estabilidad en tabla sin cambios: solo `sin_cambio_observado` + FALSE cuando
  # existe contador sincrono del motor; sin contador, `sin_cambio_en_los_controles`
  # + NA con la lista explicita de controles verificados.
  est <- r1$resumen_tabla$meta$estabilidad
  expect_true(is.list(est), info = paste(info, "estabilidad"))
  expect_length(est$diferencias, 0L)
  expect_identical(est$tabla, tb_a, info = paste(info, "estabilidad-tabla"))
  expect_true(length(est$controles) >= 1L, info = paste(info, "controles"))
  if (isTRUE(con_contador)) {
    expect_identical(est$estado, "sin_cambio_observado", info = paste(info, "estabilidad"))
    expect_identical(est$cambio_detectado, FALSE, info = paste(info, "estabilidad"))
    expect_identical(est$alcance_verificacion, "tabla_completa", info = paste(info, "estabilidad"))
    expect_true(!is.na(est$contador_motor) && nzchar(est$contador_motor), info = paste(info, "contador"))
  } else {
    expect_identical(est$estado, "sin_cambio_en_los_controles", info = paste(info, "estabilidad"))
    expect_true(is.na(est$cambio_detectado), info = paste(info, "estabilidad-na"))
    expect_identical(est$alcance_verificacion, "controles_parciales", info = paste(info, "estabilidad"))
  }
  invisible(list(plan = p_plan, r1 = r1, rb = rb))
}

.mre_poblar_par <- function(con, tb_a, tb_b, ddl_cols, n = 150L) {
  for (tb in c(tb_a, tb_b)) {
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tb)), silent = TRUE)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tb, " (", ddl_cols, ")"))
  }
  filas <- sprintf(
    "(%d, 'K-%04d', 'G%d', %d)",
    seq_len(n), seq_len(n), seq_len(n) %% 7L, seq_len(n) * 10L
  )
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tb_a, " (id, cod, grupo, monto) VALUES ",
    paste(filas, collapse = ", ")
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tb_b, " (id, cod, grupo, monto) VALUES ",
    paste(rev(filas), collapse = ", ")
  ))
}

# Fixture canonico compartido para verificar igualdad exacta entre motores MD5
# con clave compuesta de texto Unicode, DECIMAL(20,4), DATE, BIGINT > 2^53 y UUID.
.mre_poblar_cruce <- function(con, tabla, motor) {
  try(DBI::dbExecute(con, paste0(
    if (identical(motor, "mssql")) paste0("IF OBJECT_ID(N'", tabla, "', N'U') IS NOT NULL DROP TABLE ", tabla)
    else paste0("DROP TABLE IF EXISTS ", tabla)
  )), silent = TRUE)
  ddl <- switch(
    motor,
    pg = "id INTEGER NOT NULL PRIMARY KEY, clave TEXT NOT NULL, monto NUMERIC(20,4) NOT NULL, fecha DATE NOT NULL, grande BIGINT NOT NULL, uid UUID NOT NULL",
    pg93 = "id INTEGER NOT NULL PRIMARY KEY, clave TEXT NOT NULL, monto NUMERIC(20,4) NOT NULL, fecha DATE NOT NULL, grande BIGINT NOT NULL, uid UUID NOT NULL",
    mssql = "id INT NOT NULL PRIMARY KEY, clave NVARCHAR(200) NOT NULL, monto DECIMAL(20,4) NOT NULL, fecha DATE NOT NULL, grande BIGINT NOT NULL, uid UNIQUEIDENTIFIER NOT NULL",
    duckdb = "id INTEGER NOT NULL, clave VARCHAR NOT NULL, monto DECIMAL(20,4) NOT NULL, fecha DATE NOT NULL, grande BIGINT NOT NULL, uid UUID NOT NULL"
  )
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (", ddl, ")"))
  texto_base <- c(
    "a", "A", "a ", "\u00e1", "\u00c1", "e\u0301", "\u00e9",
    "\u00df", "\u0130", "\U0001f600"
  )
  n <- 240L
  claves_txt <- paste0(rep(texto_base, length.out = n), "-", sprintf("%03d", seq_len(n)))
  montos_txt <- sprintf("%d.%04d", seq_len(n) %% 1000L, seq_len(n) %% 10000L)
  montos_txt[c(1L, 2L, 3L, 4L)] <- c("0.0000", "10.0000", "-0.5000", "100.0100")
  fechas_txt <- format(as.Date("2020-01-01") + seq_len(n) - 1L, "%Y-%m-%d")
  grandes_txt <- sprintf("900719925474%04d", seq_len(n) + 1L)
  uids_txt <- sprintf("a1b2c3d4-e5f6-0000-0000-%012x", seq_len(n))
  pref_str <- if (identical(motor, "mssql")) "N'" else "'"
  filas <- vapply(seq_len(n), function(i) {
    sprintf(
      "(%d, %s%s', %s, '%s', %s, '%s')",
      i, pref_str, gsub("'", "''", claves_txt[[i]], fixed = TRUE),
      montos_txt[[i]], fechas_txt[[i]], grandes_txt[[i]], uids_txt[[i]]
    )
  }, character(1L))
  for (ini in seq(1L, n, by = 60L)) {
    fin <- min(n, ini + 59L)
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla, " (id, clave, monto, fecha, grande, uid) VALUES ",
      paste(filas[ini:fin], collapse = ", ")
    ))
  }
  if (identical(motor, "duckdb")) {
    DBI::dbExecute(con, paste0("CREATE UNIQUE INDEX ", tabla, "_u ON ", tabla, "(id)"))
  }
  invisible(tabla)
}

.mre_ids_cruce <- function(con, tabla) {
  res <- .mre_perfilar(
    con, tabla, muestra = 35L,
    clave = c("clave", "monto", "fecha", "grande", "uid"),
    metricas = "basicos"
  )
  mu <- res$perfil_muestra$meta$origen_dbi$muestreo
  expect_true(isTRUE(mu$reproducible))
  expect_identical(mu$alcance_reproducibilidad, "entre_motores_md5")
  expect_identical(mu$codificacion_huella, "utf8_canonica_md5")
  sel <- DBI::dbGetQuery(con, mu$sql_muestra)
  as.integer(sel$id)
}

test_that("SQLite: muestra reproducible en este motor y estabilidad honesta sin contador", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # 1. Clave primaria compuesta
  .mre_poblar_par(
    con, "lupa_muestra_sq_pk_a", "lupa_muestra_sq_pk_b",
    "id INTEGER NOT NULL, cod TEXT NOT NULL, grupo TEXT, monto INTEGER, PRIMARY KEY (id, cod)"
  )
  res_pk <- .mre_verificar_par_orden_invertido(
    con, "lupa_muestra_sq_pk_a", "lupa_muestra_sq_pk_b",
    muestra = 25L, origen_esperado = "clave_primaria",
    con_contador = FALSE, alcance_esperado = "motor", info = "sqlite-pk"
  )
  expect_identical(
    res_pk$r1$perfil_muestra$meta$origen_dbi$muestreo$codificacion_huella,
    "huella_sql_sqlite"
  )
  expect_match(
    res_pk$r1$perfil_muestra$meta$origen_dbi$muestreo$motivo_reproducibilidad,
    "este motor"
  )

  # 2. Indice unico de grado total (sin PK)
  .mre_poblar_par(
    con, "lupa_muestra_sq_uq_a", "lupa_muestra_sq_uq_b",
    "id INTEGER NOT NULL, cod TEXT NOT NULL, grupo TEXT, monto INTEGER"
  )
  DBI::dbExecute(con, "CREATE UNIQUE INDEX lupa_muestra_sq_uq_ia ON lupa_muestra_sq_uq_a(cod)")
  DBI::dbExecute(con, "CREATE UNIQUE INDEX lupa_muestra_sq_uq_ib ON lupa_muestra_sq_uq_b(cod)")
  .mre_verificar_par_orden_invertido(
    con, "lupa_muestra_sq_uq_a", "lupa_muestra_sq_uq_b",
    muestra = 25L, origen_esperado = "indice_unico",
    con_contador = FALSE, alcance_esperado = "motor", info = "sqlite-uq"
  )

  # 3. Sin PK ni indice unico, pero con `clave =` declarada (unica y no unica)
  .mre_poblar_par(
    con, "lupa_muestra_sq_cl_a", "lupa_muestra_sq_cl_b",
    "id INTEGER, cod TEXT, grupo TEXT, monto INTEGER"
  )
  res_cl <- .mre_verificar_par_orden_invertido(
    con, "lupa_muestra_sq_cl_a", "lupa_muestra_sq_cl_b",
    muestra = 25L, clave = "cod", origen_esperado = "clave_declarada",
    con_contador = FALSE, alcance_esperado = "motor", info = "sqlite-clave"
  )
  expect_true(any(
    res_cl$plan$clase_consulta == "verificacion de unicidad de la clave de muestra"
  ))
  justo <- .mre_perfilar(
    con, "lupa_muestra_sq_cl_a", muestra = 25L, clave = "cod",
    max_consultas = attr(res_cl$plan, "total")
  )
  expect_false(isTRUE(justo$resumen_tabla$meta$consultas$agotado))
  expect_true(isTRUE(justo$perfil_muestra$meta$origen_dbi$muestreo$reproducible))

  plan_nu <- .mre_plan(con, "lupa_muestra_sq_cl_a", muestra = 25L, clave = "grupo")
  r_nu <- .mre_perfilar(con, "lupa_muestra_sq_cl_a", muestra = 25L, clave = "grupo")
  expect_false(isTRUE(r_nu$perfil_muestra$meta$origen_dbi$muestreo$reproducible))
  expect_identical(
    as.numeric(r_nu$resumen_tabla$meta$consultas$emitidas),
    as.numeric(attr(plan_nu, "total"))
  )

  # 4. Sin clave alguna: mantiene metodo sin orden garantizado y declara el motivo
  r_sin <- .mre_perfilar(con, "lupa_muestra_sq_cl_a", muestra = 25L)
  msin <- r_sin$perfil_muestra$meta$origen_dbi$muestreo
  expect_false(isTRUE(msin$reproducible))
  expect_identical(msin$metodo, "primeras_filas_sin_orden_garantizado")
  expect_match(msin$motivo_reproducibilidad, "clave")
  expect_match(msin$motivo_reproducibilidad, "no garantiza")

  # 5. Alcance de SQL Server < 15 se restringe a este motor (UTF-16)
  fn_h <- getFromNamespace(".expresion_huella_motor_dbi", "lupa")
  h14 <- fn_h("id", "sqlserver", version_sqlserver = 14, tipos_canonicos = "entero")
  h16 <- fn_h("id", "sqlserver", version_sqlserver = 16, tipos_canonicos = "entero")
  expect_identical(h14$alcance_reproducibilidad, "motor")
  expect_identical(h14$codificacion_huella, "utf16_mssql_md5")
  expect_identical(h16$alcance_reproducibilidad, "entre_motores_md5")
  expect_identical(h16$codificacion_huella, "utf8_canonica_md5")
})

test_that("DuckDB: muestra reproducible, prefiltro 12k, clave no canonica y estabilidad sin contador", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # 1. Indice unico de grado total en DuckDB (columna NOT NULL)
  .mre_poblar_par(
    con, "lupa_muestra_dk_uq_a", "lupa_muestra_dk_uq_b",
    "id INTEGER NOT NULL, cod VARCHAR NOT NULL, grupo VARCHAR, monto INTEGER"
  )
  DBI::dbExecute(con, "CREATE UNIQUE INDEX lupa_muestra_dk_uq_ia ON lupa_muestra_dk_uq_a(id)")
  DBI::dbExecute(con, "CREATE UNIQUE INDEX lupa_muestra_dk_uq_ib ON lupa_muestra_dk_uq_b(id)")
  .mre_verificar_par_orden_invertido(
    con, "lupa_muestra_dk_uq_a", "lupa_muestra_dk_uq_b",
    muestra = 30L, origen_esperado = "indice_unico",
    con_contador = FALSE, alcance_esperado = "entre_motores_md5", info = "duckdb-uq"
  )

  # 2. Restriccion UNIQUE sobre NOT NULL (grado total en catalogo)
  .mre_poblar_par(
    con, "lupa_muestra_dk_un_a", "lupa_muestra_dk_un_b",
    "id INTEGER NOT NULL UNIQUE, cod VARCHAR NOT NULL, grupo VARCHAR, monto INTEGER"
  )
  .mre_verificar_par_orden_invertido(
    con, "lupa_muestra_dk_un_a", "lupa_muestra_dk_un_b",
    muestra = 30L, origen_esperado = "restriccion_unica",
    con_contador = FALSE, alcance_esperado = "entre_motores_md5", info = "duckdb-unique"
  )

  # 3. Clave declarada sin indice unico
  .mre_poblar_par(
    con, "lupa_muestra_dk_cl_a", "lupa_muestra_dk_cl_b",
    "id INTEGER, cod VARCHAR, grupo VARCHAR, monto INTEGER"
  )
  .mre_verificar_par_orden_invertido(
    con, "lupa_muestra_dk_cl_a", "lupa_muestra_dk_cl_b",
    muestra = 30L, clave = "cod", origen_esperado = "clave_declarada",
    con_contador = FALSE, alcance_esperado = "entre_motores_md5", info = "duckdb-clave"
  )

  # 4. Clave con tipo no canonico (DOUBLE): reproducible solo en este motor
  DBI::dbExecute(con, "CREATE TABLE lupa_muestra_dk_dbl (k DOUBLE NOT NULL, v INTEGER)")
  DBI::dbExecute(con, "INSERT INTO lupa_muestra_dk_dbl SELECT (i * 0.25)::DOUBLE, i FROM range(1, 81) AS r(i)")
  r_dbl <- .mre_perfilar(con, "lupa_muestra_dk_dbl", muestra = 20L, clave = "k")
  m_dbl <- r_dbl$perfil_muestra$meta$origen_dbi$muestreo
  expect_true(isTRUE(m_dbl$reproducible))
  expect_identical(m_dbl$alcance_reproducibilidad, "motor")

  # 5. Tabla de 12.000 filas: activa prefiltro por umbral C1 (`filtro_umbral = TRUE`)
  DBI::dbExecute(con, paste(
    "CREATE TABLE lupa_muestra_dk_12k (id INTEGER NOT NULL, cod VARCHAR NOT NULL, monto INTEGER)"
  ))
  DBI::dbExecute(con, "CREATE UNIQUE INDEX lupa_muestra_dk_12k_u ON lupa_muestra_dk_12k(id)")
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_muestra_dk_12k",
    "SELECT i::INTEGER, 'C-' || i::VARCHAR, (i * 3)::INTEGER FROM range(1, 12001) AS r(i)"
  ))
  p12 <- .mre_plan(con, "lupa_muestra_dk_12k", muestra = 200L, sentinelas_numericos = c(-999))
  r12_a <- .mre_perfilar(con, "lupa_muestra_dk_12k", muestra = 200L, sentinelas_numericos = c(-999))
  r12_b <- .mre_perfilar(con, "lupa_muestra_dk_12k", muestra = 200L, sentinelas_numericos = c(-999))
  m12 <- r12_a$perfil_muestra$meta$origen_dbi$muestreo
  expect_true(isTRUE(m12$reproducible))
  expect_true(isTRUE(m12$filtro_umbral))
  expect_identical(m12$funcion_hash, "md5")
  expect_equal(m12$filas_obtenidas, 200)
  expect_identical(r12_a$perfil_muestra$columnas, r12_b$perfil_muestra$columnas)
  expect_identical(
    as.numeric(r12_a$resumen_tabla$meta$consultas$emitidas),
    as.numeric(attr(p12, "total"))
  )

  # 6. Error simulado al leer filas de la muestra con huella: cae a respaldo
  # sin perder `perfil_muestra`, con `reproducible = FALSE` y motivo explicito.
  orig_traer_dk <- getFromNamespace(".traer_consulta_dbi", "lupa")
  r_dk_fallo <- testthat::with_mocked_bindings(
    .mre_perfilar(con, "lupa_muestra_dk_cl_a", muestra = 20L, clave = "cod", max_bytes_muestra = Inf),
    .traer_consulta_dbi = function(conexion, sql) {
      if (grepl("md5(", sql, fixed = TRUE, useBytes = TRUE)) {
        stop("error simulado al leer filas de huella en duckdb", call. = FALSE)
      }
      orig_traer_dk(conexion, sql)
    },
    .package = "lupa"
  )
  expect_false(is.null(r_dk_fallo$perfil_muestra))
  mdk_f <- r_dk_fallo$perfil_muestra$meta$origen_dbi$muestreo
  expect_false(isTRUE(mdk_f$reproducible))
  expect_identical(mdk_f$metodo, "primeras_filas_sin_orden_garantizado")
  expect_equal(mdk_f$filas_obtenidas, 20)
  expect_match(mdk_f$motivo_reproducibilidad, "huella")
  expect_true(any(
    r_dk_fallo$resumen_tabla$cobertura$bloque == "perfil_muestra" &
      r_dk_fallo$resumen_tabla$cobertura$estado == "degradado"
  ))
})

test_that("Estabilidad sin contador (SQLite): detecta cambios en controles y nunca miente FALSE", {
  skip_if_not_installed("RSQLite")
  cruda <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(cruda), add = TRUE)
  DBI::dbExecute(cruda, "CREATE TABLE lupa_muestra_est (id INTEGER PRIMARY KEY, monto INTEGER, txt TEXT)")
  DBI::dbExecute(cruda, paste0(
    "INSERT INTO lupa_muestra_est VALUES ",
    paste0("(", 1:40, ", ", (1:40) * 10L, ", 'v", (1:40) %% 5L, "')", collapse = ", ")
  ))

  env_orig <- new.env(parent = emptyenv())
  env_orig$fn <- getFromNamespace(".consultar_cierre_estabilidad_dbi", "lupa")

  # Caso A: INSERT durante la lectura (cambia n_filas y max(id)/max(monto))
  hecho_ins <- FALSE
  r_ins <- testthat::with_mocked_bindings(
    .mre_perfilar(cruda, "lupa_muestra_est", muestra = 20L),
    .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
      if (!hecho_ins) {
        hecho_ins <<- TRUE
        DBI::dbExecute(cruda, "INSERT INTO lupa_muestra_est VALUES (999, 99999, 'nuevo')")
      }
      env_orig$fn(conexion, tabla_sql, antes, presupuesto)
    },
    .package = "lupa"
  )
  est_ins <- r_ins$resumen_tabla$meta$estabilidad
  expect_identical(est_ins$estado, "cambio_detectado")
  expect_true(isTRUE(est_ins$cambio_detectado))
  expect_true(any(grepl("n_filas: 40 -> 41", est_ins$diferencias, fixed = TRUE, useBytes = TRUE)))
  expect_true(any(r_ins$resumen_tabla$cobertura$bloque == "consistencia"))

  # Caso B: sin contador el cierre solo consulta COUNT(*) y MAX(clave indexada),
  # nunca columnas no indexadas (`monto`). Un UPDATE de la clave indexada se
  # detecta; un UPDATE de `monto` cae honestamente en `sin_cambio_en_los_controles`.
  DBI::dbExecute(cruda, "DELETE FROM lupa_muestra_est WHERE id = 999")
  hecho_upd <- FALSE
  r_upd <- testthat::with_mocked_bindings(
    .mre_perfilar(cruda, "lupa_muestra_est", muestra = 20L),
    .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
      if (!hecho_upd) {
        hecho_upd <<- TRUE
        DBI::dbExecute(cruda, "UPDATE lupa_muestra_est SET id = 999 WHERE id = 40")
      }
      env_orig$fn(conexion, tabla_sql, antes, presupuesto)
    },
    .package = "lupa"
  )
  est_upd <- r_upd$resumen_tabla$meta$estabilidad
  expect_identical(est_upd$estado, "cambio_detectado")
  expect_true(isTRUE(est_upd$cambio_detectado))
  expect_identical(est_upd$antes$n_filas, est_upd$despues$n_filas)
  expect_true(any(grepl("maximo\\(id\\)", est_upd$diferencias, perl = TRUE)))
  expect_false(grepl("monto", est_upd$sql, fixed = TRUE, useBytes = TRUE))

  DBI::dbExecute(cruda, "UPDATE lupa_muestra_est SET id = 40 WHERE id = 999")
  hecho_monto <- FALSE
  r_monto <- testthat::with_mocked_bindings(
    .mre_perfilar(cruda, "lupa_muestra_est", muestra = 20L),
    .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
      if (!hecho_monto) {
        hecho_monto <<- TRUE
        DBI::dbExecute(cruda, "UPDATE lupa_muestra_est SET monto = 88888 WHERE id = 1")
      }
      env_orig$fn(conexion, tabla_sql, antes, presupuesto)
    },
    .package = "lupa"
  )
  est_monto <- r_monto$resumen_tabla$meta$estabilidad
  expect_identical(est_monto$estado, "sin_cambio_en_los_controles")
  expect_true(is.na(est_monto$cambio_detectado))
  expect_false(any(grepl("monto", est_monto$controles, fixed = TRUE, useBytes = TRUE)))
  expect_false(grepl("monto", est_monto$sql, fixed = TRUE, useBytes = TRUE))

  # Tabla sin indice en SQLite: el cierre emite solo COUNT(*), sin MAX de columnas
  DBI::dbExecute(cruda, "CREATE TABLE lupa_muestra_est_sin_idx (a INTEGER, monto INTEGER)")
  DBI::dbExecute(cruda, "INSERT INTO lupa_muestra_est_sin_idx VALUES (1, 10), (2, 20), (3, 30)")
  r_si <- .mre_perfilar(cruda, "lupa_muestra_est_sin_idx", muestra = 2L)
  est_si <- r_si$resumen_tabla$meta$estabilidad
  expect_identical(est_si$estado, "sin_cambio_en_los_controles")
  expect_true(is.na(est_si$cambio_detectado))
  expect_false(grepl("MAX\\(", est_si$sql, perl = TRUE))

  # Si falla la consulta con huella en el motor (con o sin sonda de bytes),
  # nunca se pierde `perfil_muestra`: cae a la muestra sin orden garantizado
  # con `reproducible = FALSE`, motivo explicito y aviso en `cobertura`.
  for (mb in c(50 * 1024^2, Inf)) {
    r_fallo <- testthat::with_mocked_bindings(
      .mre_perfilar(
        cruda, "lupa_muestra_est", muestra = 20L,
        clave = "id", max_bytes_muestra = mb
      ),
      .expresion_huella_motor_dbi = function(...) {
        list(
          expr = "funcion_inexistente_lupa(id)",
          funcion = "huella_sql_sqlite",
          soporta_umbral_hex = FALSE,
          alcance_reproducibilidad = "motor",
          codificacion_huella = "huella_sql_sqlite"
        )
      },
      .package = "lupa"
    )
    expect_false(is.null(r_fallo$perfil_muestra))
    mf <- r_fallo$perfil_muestra$meta$origen_dbi$muestreo
    expect_false(isTRUE(mf$reproducible))
    expect_identical(mf$metodo, "primeras_filas_sin_orden_garantizado")
    expect_equal(mf$filas_obtenidas, 20)
    expect_match(mf$motivo_reproducibilidad, "huella")
    expect_true(any(
      r_fallo$resumen_tabla$cobertura$bloque == "perfil_muestra" &
        r_fallo$resumen_tabla$cobertura$estado == "degradado"
    ))
  }

  # Error al leer filas (`dbFetch`) o al cerrar el cursor (`dbClearResult`)
  # durante la lectura con huella: tampoco se pierde `perfil_muestra`.
  orig_env <- getFromNamespace(".enviar_consulta_dbi", "lupa")
  orig_fetch <- getFromNamespace(".traer_filas_dbi", "lupa")
  orig_limp <- getFromNamespace(".limpiar_consulta_dbi", "lupa")
  for (etapa_fallo in c("leer", "cerrar")) {
    env_est <- new.env(parent = emptyenv())
    env_est$es_huella <- FALSE
    r_et <- testthat::with_mocked_bindings(
      .mre_perfilar(
        cruda, "lupa_muestra_est", muestra = 20L,
        clave = "id", dialecto = "portable", max_bytes_muestra = Inf
      ),
      .enviar_consulta_dbi = function(conexion, sql) {
        env_est$es_huella <- grepl("ORDER BY", sql, fixed = TRUE, useBytes = TRUE)
        orig_env(conexion, sql)
      },
      .traer_filas_dbi = function(resultado, n = -1L, conexion) {
        if (identical(etapa_fallo, "leer") && isTRUE(env_est$es_huella)) {
          env_est$es_huella <- FALSE
          stop("error simulado al leer filas de huella", call. = FALSE)
        }
        orig_fetch(resultado, n = n, conexion = conexion)
      },
      .limpiar_consulta_dbi = function(resultado, conexion) {
        marcado <- identical(etapa_fallo, "cerrar") && isTRUE(env_est$es_huella)
        if (marcado) env_est$es_huella <- FALSE
        ok_l <- orig_limp(resultado, conexion)
        if (marcado) {
          stop("error simulado al cerrar resultado de huella", call. = FALSE)
        }
        ok_l
      },
      .package = "lupa"
    )
    expect_false(is.null(r_et$perfil_muestra), info = etapa_fallo)
    m_et <- r_et$perfil_muestra$meta$origen_dbi$muestreo
    expect_false(isTRUE(m_et$reproducible), info = etapa_fallo)
    expect_identical(m_et$metodo, "primeras_filas_sin_orden_garantizado", info = etapa_fallo)
    expect_equal(m_et$filas_obtenidas, 20, info = etapa_fallo)
    expect_match(m_et$motivo_reproducibilidad, "huella", info = etapa_fallo)
    expect_true(any(
      r_et$resumen_tabla$cobertura$bloque == "perfil_muestra" &
        r_et$resumen_tabla$cobertura$estado == "degradado"
    ), info = etapa_fallo)
  }

  # Caso C: presupuesto agotado -> estado `no_verificada`, cambio_detectado = NA
  r_ag <- .mre_perfilar(cruda, "lupa_muestra_est", muestra = 20L, max_consultas = 6)
  est_ag <- r_ag$resumen_tabla$meta$estabilidad
  expect_identical(est_ag$estado, "no_verificada")
  expect_true(is.na(est_ag$cambio_detectado))
  expect_true(nzchar(est_ag$motivo))

  # Caso D: universo = "muestra_motor" -> estado `no_verificada` con motivo explicito
  r_mm <- .mre_perfilar(cruda, "lupa_muestra_est", universo = "muestra_motor", muestra_motor = 20L)
  est_mm <- r_mm$resumen_tabla$meta$estabilidad
  expect_identical(est_mm$estado, "no_verificada")
  expect_true(is.na(est_mm$cambio_detectado))
  expect_match(est_mm$motivo, "muestra_motor")

  # Caso E: incluir_valores = FALSE sin contador -> `sin_cambio_en_los_controles`,
  # cambio_detectado = NA y sin exponer extremos ni valores al imprimir
  r_nv <- .mre_perfilar(cruda, "lupa_muestra_est", muestra = 20L, incluir_valores = FALSE)
  est_nv <- r_nv$resumen_tabla$meta$estabilidad
  expect_identical(est_nv$estado, "sin_cambio_en_los_controles")
  expect_true(is.na(est_nv$cambio_detectado))
  expect_true(any(grepl("COUNT\\(\\*\\)", est_nv$controles, perl = TRUE)))
  expect_match(est_nv$motivo, "fuera de esos controles")
  if (is.list(est_nv$antes$agregados)) {
    for (ag in est_nv$antes$agregados) {
      expect_true(is.na(ag$minimo))
      expect_true(is.na(ag$maximo))
    }
  }
  salida_print <- paste(c(
    utils::capture.output(print(r_nv), type = "output"),
    utils::capture.output(print(r_nv), type = "message")
  ), collapse = "\n")
  expect_match(salida_print, "controles verificados")

  # Caso F: consulta unica (`solo_agregados` + `metricas = "validos"`): 0 consultas extra
  p_u <- .mre_plan(cruda, "lupa_muestra_est", metricas = "validos", bloque_muestra = "solo_agregados")
  r_u <- .mre_perfilar(cruda, "lupa_muestra_est", metricas = "validos", bloque_muestra = "solo_agregados")
  expect_identical(r_u$resumen_tabla$meta$estabilidad$estado, "sin_cambio_observado")
  expect_identical(r_u$resumen_tabla$meta$estabilidad$cambio_detectado, FALSE)
  expect_identical(r_u$resumen_tabla$meta$estabilidad$metodo, "consulta_unica")
  expect_identical(
    as.numeric(r_u$resumen_tabla$meta$consultas$emitidas),
    as.numeric(attr(p_u, "total"))
  )
})

.mre_probar_pg <- function(puerto, etiqueta) {
  con <- .mre_conectar_pg(puerto)
  con2 <- .mre_conectar_pg(puerto)
  tbs <- paste0(
    "lupa_muestra_", etiqueta,
    c("_pk_a", "_pk_b", "_uq_a", "_uq_b", "_cl_a", "_cl_b", "_mut", "_txt", "_md5_txt", "_md5_vc")
  )
  vista <- paste0("lupa_muestra_", etiqueta, "_vista")
  rol_sin_md5 <- paste0("lupa_muestra_sin_md5_", etiqueta)
  limpiar <- function() {
    try(DBI::dbExecute(con, "RESET ROLE"), silent = TRUE)
    try(DBI::dbExecute(con, "GRANT EXECUTE ON FUNCTION md5(text) TO PUBLIC"), silent = TRUE)
    try(DBI::dbExecute(con, paste0("DROP VIEW IF EXISTS ", vista)), silent = TRUE)
    for (tb in tbs) try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tb)), silent = TRUE)
    try(DBI::dbExecute(con, paste0("DROP OWNED BY ", rol_sin_md5)), silent = TRUE)
    try(DBI::dbExecute(con, paste0("DROP ROLE IF EXISTS ", rol_sin_md5)), silent = TRUE)
  }
  on.exit({
    limpiar()
    try(DBI::dbDisconnect(con2), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  limpiar()

  # PK compuesta
  .mre_poblar_par(
    con, tbs[[1L]], tbs[[2L]],
    "id INTEGER NOT NULL, cod TEXT NOT NULL, grupo TEXT, monto INTEGER, PRIMARY KEY (id, cod)"
  )
  .mre_verificar_par_orden_invertido(
    con, tbs[[1L]], tbs[[2L]], muestra = 30L,
    origen_esperado = "clave_primaria", con_contador = TRUE,
    alcance_esperado = "entre_motores_md5", info = paste0(etiqueta, "-pk")
  )

  # Indice unico total sin PK
  .mre_poblar_par(
    con, tbs[[3L]], tbs[[4L]],
    "id INTEGER NOT NULL, cod TEXT NOT NULL, grupo TEXT, monto INTEGER"
  )
  DBI::dbExecute(con, paste0("CREATE UNIQUE INDEX ", tbs[[3L]], "_u ON ", tbs[[3L]], "(cod)"))
  DBI::dbExecute(con, paste0("CREATE UNIQUE INDEX ", tbs[[4L]], "_u ON ", tbs[[4L]], "(cod)"))
  .mre_verificar_par_orden_invertido(
    con, tbs[[3L]], tbs[[4L]], muestra = 30L,
    origen_esperado = "indice_unico", con_contador = TRUE,
    alcance_esperado = "entre_motores_md5", info = paste0(etiqueta, "-uq")
  )

  # Clave declarada sin PK/indice
  .mre_poblar_par(
    con, tbs[[5L]], tbs[[6L]],
    "id INTEGER, cod TEXT, grupo TEXT, monto INTEGER"
  )
  .mre_verificar_par_orden_invertido(
    con, tbs[[5L]], tbs[[6L]], muestra = 30L, clave = "cod",
    origen_esperado = "clave_declarada", con_contador = TRUE,
    alcance_esperado = "entre_motores_md5", info = paste0(etiqueta, "-clave")
  )

  # Estabilidad ante INSERT desde segunda conexion
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tbs[[7L]], " (id INTEGER PRIMARY KEY, monto INTEGER, txt TEXT)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tbs[[7L]], " SELECT i, i * 10, 'v' || (i % 5) FROM generate_series(1, 50) AS i"
  ))
  mutado <- FALSE
  env_orig <- new.env(parent = emptyenv())
  env_orig$fn <- getFromNamespace(".consultar_cierre_estabilidad_dbi", "lupa")
  r_pg_mut <- testthat::with_mocked_bindings(
    .mre_perfilar(con, tbs[[7L]], muestra = 20L),
    .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
      if (!mutado) {
        mutado <<- TRUE
        DBI::dbExecute(con2, paste0("INSERT INTO ", tbs[[7L]], " VALUES (999, 99999, 'ext')"))
      }
      env_orig$fn(conexion, tabla_sql, antes, presupuesto)
    },
    .package = "lupa"
  )
  expect_identical(r_pg_mut$resumen_tabla$meta$estabilidad$estado, "cambio_detectado", info = etiqueta)
  expect_true(isTRUE(r_pg_mut$resumen_tabla$meta$estabilidad$cambio_detectado), info = etiqueta)

  # Estabilidad ante UPDATE puramente textual (mismo COUNT, id y monto) a 0 ms
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tbs[[8L]], " (id INTEGER PRIMARY KEY, monto INTEGER, txt TEXT NOT NULL)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tbs[[8L]], " SELECT i, i * 10, 'texto-' || i FROM generate_series(1, 60) AS i"
  ))
  mutado_txt <- FALSE
  r_pg_txt <- testthat::with_mocked_bindings(
    .mre_perfilar(con, tbs[[8L]], muestra = 20L, metricas = "basicos"),
    .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
      if (!mutado_txt) {
        mutado_txt <<- TRUE
        DBI::dbExecute(con2, paste0("UPDATE ", tbs[[8L]], " SET txt = 'texto-cambiado' WHERE id = 1"))
      }
      env_orig$fn(conexion, tabla_sql, antes, presupuesto)
    },
    .package = "lupa"
  )
  est_txt <- r_pg_txt$resumen_tabla$meta$estabilidad
  expect_identical(est_txt$estado, "cambio_detectado", info = paste(etiqueta, "upd-txt"))
  expect_true(isTRUE(est_txt$cambio_detectado), info = paste(etiqueta, "upd-txt"))
  expect_identical(est_txt$contador_motor, "xmin_mvcc", info = paste(etiqueta, "upd-txt"))
  expect_true(any(grepl("^xmin\\(", est_txt$diferencias, perl = TRUE)), info = paste(etiqueta, "upd-txt"))
  expect_false(grepl("monto", est_txt$sql, fixed = TRUE, useBytes = TRUE), info = paste(etiqueta, "cierre-sin-monto"))

  # Vista en PostgreSQL (sin xmin): estado honesto `sin_cambio_en_los_controles` y NA
  DBI::dbExecute(con, paste0("CREATE VIEW ", vista, " AS SELECT id, monto, txt FROM ", tbs[[8L]]))
  r_v <- .mre_perfilar(con, vista, muestra = 20L, metricas = "basicos")
  est_v <- r_v$resumen_tabla$meta$estabilidad
  expect_identical(est_v$estado, "sin_cambio_en_los_controles", info = paste(etiqueta, "vista"))
  expect_true(is.na(est_v$cambio_detectado), info = paste(etiqueta, "vista"))
  expect_true(length(est_v$controles) >= 1L, info = paste(etiqueta, "vista"))

  # Rol sin permiso EXECUTE sobre md5(text): el error aparece al leer filas
  # (no al preparar). Nunca se pierde `perfil_muestra`: cae a la lectura sin
  # orden garantizado (y al descarte sin huella cuando `k TEXT` tambien usa md5).
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tbs[[9L]], " (id INT PRIMARY KEY, k TEXT NOT NULL, v INT)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tbs[[9L]], " SELECT i, 'k' || i, i FROM generate_series(1, 120) AS i"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tbs[[10L]], " (id INT PRIMARY KEY, k INT NOT NULL, v INT)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tbs[[10L]], " SELECT i, i * 10, i FROM generate_series(1, 120) AS i"
  ))
  DBI::dbExecute(con, paste0("CREATE ROLE ", rol_sin_md5, " NOLOGIN"))
  DBI::dbExecute(con, paste0("GRANT USAGE ON SCHEMA public TO ", rol_sin_md5))
  DBI::dbExecute(con, paste0("GRANT SELECT ON ", tbs[[9L]], ", ", tbs[[10L]], " TO ", rol_sin_md5))
  DBI::dbExecute(con, "REVOKE EXECUTE ON FUNCTION md5(text) FROM PUBLIC")
  DBI::dbExecute(con, paste0("SET ROLE ", rol_sin_md5))

  r_md5_txt <- .mre_perfilar(con, tbs[[9L]], muestra = 20L, metricas = "basicos")
  expect_false(is.null(r_md5_txt$perfil_muestra), info = paste(etiqueta, "md5-revocado-text"))
  m_txt <- r_md5_txt$perfil_muestra$meta$origen_dbi$muestreo
  expect_false(isTRUE(m_txt$reproducible), info = paste(etiqueta, "md5-revocado-text"))
  expect_identical(m_txt$metodo, "primeras_filas_sin_orden_garantizado", info = paste(etiqueta, "md5-revocado-text"))
  expect_equal(m_txt$filas_obtenidas, 20, info = paste(etiqueta, "md5-revocado-text"))
  expect_true(all(c("id", "v") %in% m_txt$columnas_leidas), info = paste(etiqueta, "md5-revocado-text"))
  expect_match(m_txt$motivo_reproducibilidad, "md5|huella", ignore.case = TRUE, info = paste(etiqueta, "md5-revocado-text"))
  expect_true(any(
    r_md5_txt$resumen_tabla$cobertura$bloque == "perfil_muestra" &
      r_md5_txt$resumen_tabla$cobertura$estado == "degradado"
  ), info = paste(etiqueta, "md5-revocado-text"))

  r_md5_vc <- .mre_perfilar(con, tbs[[10L]], muestra = 20L, clave = "k", metricas = "basicos")
  expect_false(is.null(r_md5_vc$perfil_muestra), info = paste(etiqueta, "md5-revocado-varchar"))
  m_vc <- r_md5_vc$perfil_muestra$meta$origen_dbi$muestreo
  expect_false(isTRUE(m_vc$reproducible), info = paste(etiqueta, "md5-revocado-varchar"))
  expect_identical(m_vc$metodo, "primeras_filas_sin_orden_garantizado", info = paste(etiqueta, "md5-revocado-varchar"))
  expect_equal(m_vc$filas_obtenidas, 20, info = paste(etiqueta, "md5-revocado-varchar"))
  expect_true(all(c("id", "k", "v") %in% m_vc$columnas_leidas), info = paste(etiqueta, "md5-revocado-varchar"))
  expect_match(m_vc$motivo_reproducibilidad, "md5|huella", ignore.case = TRUE, info = paste(etiqueta, "md5-revocado-varchar"))
  expect_true(any(
    r_md5_vc$resumen_tabla$cobertura$bloque == "perfil_muestra" &
      r_md5_vc$resumen_tabla$cobertura$estado == "degradado"
  ), info = paste(etiqueta, "md5-revocado-varchar"))

  DBI::dbExecute(con, "RESET ROLE")
  DBI::dbExecute(con, "GRANT EXECUTE ON FUNCTION md5(text) TO PUBLIC")
}

test_that("PostgreSQL actual: muestra reproducible por huella, plan exacto y estabilidad (xmin + vista)", {
  .mre_probar_pg(Sys.getenv("LUPA_PG_PORT", "55432"), "pg")
})

test_that("PostgreSQL 9.3: muestra reproducible por huella, plan exacto y estabilidad (xmin + vista)", {
  .mre_probar_pg(Sys.getenv("LUPA_PG93_PORT", "55493"), "pg93")
})

test_that("SQL Server: muestra reproducible (UTF-8 MD5), plan exacto y estabilidad (DMV, rowversion y vista)", {
  con <- .mre_conectar_mssql()
  con2 <- .mre_conectar_mssql()
  tbs <- paste0(
    "lupa_muestra_mssql",
    c("_pk_a", "_pk_b", "_uq_a", "_uq_b", "_cl_a", "_cl_b", "_txt_pk", "_txt_heap", "_rv", "_col")
  )
  vista <- "lupa_muestra_mssql_vista"
  limpiar <- function() {
    try(DBI::dbExecute(con, paste0(
      "IF OBJECT_ID(N'", vista, "', N'V') IS NOT NULL DROP VIEW ", vista
    )), silent = TRUE)
    for (tb in tbs) {
      try(DBI::dbExecute(con, paste0(
        "IF OBJECT_ID(N'", tb, "', N'U') IS NOT NULL DROP TABLE ", tb
      )), silent = TRUE)
    }
  }
  on.exit({
    limpiar()
    try(DBI::dbDisconnect(con2), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  limpiar()

  .mre_poblar_par(
    con, tbs[[1L]], tbs[[2L]],
    "id INT NOT NULL, cod NVARCHAR(40) NOT NULL, grupo NVARCHAR(20), monto INT, PRIMARY KEY (id, cod)"
  )
  res_ms <- .mre_verificar_par_orden_invertido(
    con, tbs[[1L]], tbs[[2L]], muestra = 30L,
    origen_esperado = "clave_primaria", con_contador = TRUE,
    alcance_esperado = "entre_motores_md5", info = "mssql-pk"
  )
  expect_identical(
    res_ms$r1$perfil_muestra$meta$origen_dbi$muestreo$funcion_hash,
    "hashbytes_md5"
  )

  .mre_poblar_par(
    con, tbs[[3L]], tbs[[4L]],
    "id INT NOT NULL, cod NVARCHAR(40) NOT NULL, grupo NVARCHAR(20), monto INT"
  )
  DBI::dbExecute(con, paste0("CREATE UNIQUE INDEX ", tbs[[3L]], "_u ON ", tbs[[3L]], "(cod)"))
  DBI::dbExecute(con, paste0("CREATE UNIQUE INDEX ", tbs[[4L]], "_u ON ", tbs[[4L]], "(cod)"))
  .mre_verificar_par_orden_invertido(
    con, tbs[[3L]], tbs[[4L]], muestra = 30L,
    origen_esperado = "indice_unico", con_contador = TRUE,
    alcance_esperado = "entre_motores_md5", info = "mssql-uq"
  )

  .mre_poblar_par(
    con, tbs[[5L]], tbs[[6L]],
    "id INT, cod NVARCHAR(40), grupo NVARCHAR(20), monto INT"
  )
  .mre_verificar_par_orden_invertido(
    con, tbs[[5L]], tbs[[6L]], muestra = 30L, clave = "cod",
    origen_esperado = "clave_declarada", con_contador = TRUE,
    alcance_esperado = "entre_motores_md5", info = "mssql-clave"
  )

  # UPDATE puramente textual en tabla con PK y en heap (sys.dm_db_index_operational_stats)
  env_orig <- new.env(parent = emptyenv())
  env_orig$fn <- getFromNamespace(".consultar_cierre_estabilidad_dbi", "lupa")
  for (par in list(
    list(tb = tbs[[7L]], ddl = "id INT PRIMARY KEY, monto INT, txt NVARCHAR(40)", tag = "mssql-pk-txt"),
    list(tb = tbs[[8L]], ddl = "id INT, monto INT, txt NVARCHAR(40)", tag = "mssql-heap-txt")
  )) {
    DBI::dbExecute(con, paste0("CREATE TABLE ", par$tb, " (", par$ddl, ")"))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", par$tb, " VALUES ",
      paste0("(", 1:30, ", ", (1:30) * 10L, ", N'v", 1:30, "')", collapse = ", ")
    ))
    mutado <- FALSE
    r_mut <- testthat::with_mocked_bindings(
      .mre_perfilar(con, par$tb, muestra = 15L, metricas = "basicos"),
      .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
        if (!mutado) {
          mutado <<- TRUE
          DBI::dbExecute(con2, paste0("UPDATE ", par$tb, " SET txt = N'texto-cambiado' WHERE id = 1"))
        }
        env_orig$fn(conexion, tabla_sql, antes, presupuesto)
      },
      .package = "lupa"
    )
    est_m <- r_mut$resumen_tabla$meta$estabilidad
    expect_identical(est_m$estado, "cambio_detectado", info = par$tag)
    expect_true(isTRUE(est_m$cambio_detectado), info = par$tag)
    expect_identical(est_m$contador_motor, "dm_db_index_operational_stats", info = par$tag)
    expect_true(any(grepl("contador_dml_motor", est_m$diferencias, fixed = TRUE, useBytes = TRUE)), info = par$tag)
  }

  # Tabla con columna ROWVERSION: detecta UPDATE textual usando rowversion
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tbs[[9L]], " (id INT PRIMARY KEY, monto INT, txt NVARCHAR(40), rv ROWVERSION)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tbs[[9L]], " (id, monto, txt) VALUES ",
    paste0("(", 1:30, ", ", (1:30) * 10L, ", N'v", 1:30, "')", collapse = ", ")
  ))
  r_rv_ok <- .mre_perfilar(con, tbs[[9L]], muestra = 15L, metricas = "basicos")
  expect_identical(r_rv_ok$resumen_tabla$meta$estabilidad$estado, "sin_cambio_observado")
  expect_identical(r_rv_ok$resumen_tabla$meta$estabilidad$contador_motor, "rowversion")
  mutado_rv <- FALSE
  r_rv_mut <- testthat::with_mocked_bindings(
    .mre_perfilar(con, tbs[[9L]], muestra = 15L, metricas = "basicos"),
    .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
      if (!mutado_rv) {
        mutado_rv <<- TRUE
        DBI::dbExecute(con2, paste0("UPDATE ", tbs[[9L]], " SET txt = N'cambiado-rv' WHERE id = 1"))
      }
      env_orig$fn(conexion, tabla_sql, antes, presupuesto)
    },
    .package = "lupa"
  )
  expect_identical(r_rv_mut$resumen_tabla$meta$estabilidad$estado, "cambio_detectado")
  expect_true(any(grepl("rowversion", r_rv_mut$resumen_tabla$meta$estabilidad$diferencias, fixed = TRUE, useBytes = TRUE)))

  # Vista en SQL Server: sin contador -> `sin_cambio_en_los_controles` y NA
  DBI::dbExecute(con, paste0("CREATE VIEW ", vista, " AS SELECT id, monto, txt FROM ", tbs[[7L]]))
  r_v <- .mre_perfilar(con, vista, muestra = 15L, metricas = "basicos")
  expect_identical(r_v$resumen_tabla$meta$estabilidad$estado, "sin_cambio_en_los_controles")
  expect_true(is.na(r_v$resumen_tabla$meta$estabilidad$cambio_detectado))

  # El cierre con DMV en SQL Server lee solo el contador, sin barrer la tabla
  est_ms_pk <- res_ms$r1$resumen_tabla$meta$estabilidad
  expect_false(grepl(tbs[[1L]], est_ms_pk$sql, fixed = TRUE, useBytes = TRUE) &&
               grepl(paste0("FROM\\s+\"?", tbs[[1L]]), est_ms_pk$sql, perl = TRUE))
  expect_true(any(res_ms$plan$alcance == "lee el contador del motor"))

  # Colaciones en SQL Server (incluyendo Latin1_General_100_BIN2 en NVARCHAR y
  # VARCHAR, PK por omision y `clave =`): nunca falla ni pierde `perfil_muestra`
  tb_col <- "lupa_muestra_mssql_col"
  casos_col <- list(
    list(col = "Latin1_General_100_BIN2", tipo = "NVARCHAR(60)", pk = TRUE),
    list(col = "Latin1_General_100_BIN2", tipo = "VARCHAR(60)", pk = FALSE),
    list(col = "Latin1_General_BIN2", tipo = "NVARCHAR(60)", pk = TRUE),
    list(col = "Latin1_General_BIN", tipo = "VARCHAR(60)", pk = FALSE),
    list(col = "Latin1_General_100_CS_AS", tipo = "NVARCHAR(60)", pk = FALSE),
    list(col = "Latin1_General_100_CI_AS_SC", tipo = "NVARCHAR(60)", pk = FALSE),
    list(col = "SQL_Latin1_General_CP1_CS_AS", tipo = "VARCHAR(60)", pk = FALSE),
    list(col = "Latin1_General_100_CI_AS_SC_UTF8", tipo = "VARCHAR(60)", pk = FALSE),
    list(col = "Latin1_General_100_BIN2_UTF8", tipo = "VARCHAR(60)", pk = FALSE)
  )
  ids_bin2 <- NULL
  ids_cs <- NULL
  for (caso in casos_col) {
    try(DBI::dbExecute(con, paste0(
      "IF OBJECT_ID(N'", tb_col, "', N'U') IS NOT NULL DROP TABLE ", tb_col
    )), silent = TRUE)
    if (isTRUE(caso$pk)) {
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", tb_col, " (k1 ", caso$tipo, " COLLATE ", caso$col,
        " NOT NULL PRIMARY KEY, monto INT NOT NULL)"
      ))
      DBI::dbExecute(con, paste0(
        "INSERT INTO ", tb_col, " VALUES ",
        paste0("(N'k", 1:80, "', ", 1:80, ")", collapse = ", ")
      ))
      r_c <- .mre_perfilar(con, tb_col, muestra = 20L, metricas = "basicos")
    } else {
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", tb_col, " (id INT NOT NULL PRIMARY KEY, k1 ",
        caso$tipo, " COLLATE ", caso$col, " NOT NULL, monto INT NOT NULL)"
      ))
      DBI::dbExecute(con, paste0(
        "INSERT INTO ", tb_col, " VALUES ",
        paste0("(", 1:80, ", N'k", 1:80, "', ", 1:80, ")", collapse = ", ")
      ))
      r_c <- .mre_perfilar(con, tb_col, muestra = 20L, clave = c("k1", "monto"), metricas = "basicos")
    }
    tag_c <- paste(caso$col, caso$tipo, if (isTRUE(caso$pk)) "pk" else "clave")
    expect_false(is.null(r_c$perfil_muestra), info = tag_c)
    mc <- r_c$perfil_muestra$meta$origen_dbi$muestreo
    expect_true(isTRUE(mc$reproducible), info = tag_c)
    expect_identical(mc$metodo, "muestra_huella_motor", info = tag_c)
    expect_identical(mc$alcance_reproducibilidad, "entre_motores_md5", info = tag_c)
    if (identical(caso$col, "Latin1_General_100_BIN2") && identical(caso$tipo, "VARCHAR(60)")) {
      ids_bin2 <- as.integer(DBI::dbGetQuery(con, mc$sql_muestra)$id)
    }
    if (identical(caso$col, "Latin1_General_100_CS_AS")) {
      ids_cs <- as.integer(DBI::dbGetQuery(con, mc$sql_muestra)$id)
    }
  }
  expect_identical(ids_bin2, ids_cs, info = "BIN2 y CS_AS eligen las mismas filas")

  # Error simulado al leer filas de la muestra con huella en SQL Server:
  # nunca se pierde `perfil_muestra`, cae a respaldo con `reproducible = FALSE`.
  orig_env_ms <- getFromNamespace(".enviar_consulta_dbi", "lupa")
  orig_fetch_ms <- getFromNamespace(".traer_filas_dbi", "lupa")
  env_ms_f <- new.env(parent = emptyenv())
  env_ms_f$es_huella <- FALSE
  r_ms_f <- testthat::with_mocked_bindings(
    .mre_perfilar(con, tbs[[1L]], muestra = 20L, metricas = "basicos", max_bytes_muestra = Inf),
    .enviar_consulta_dbi = function(conexion, sql) {
      env_ms_f$es_huella <- grepl("HASHBYTES", sql, fixed = TRUE, useBytes = TRUE)
      orig_env_ms(conexion, sql)
    },
    .traer_filas_dbi = function(resultado, n = -1L, conexion) {
      if (isTRUE(env_ms_f$es_huella)) {
        env_ms_f$es_huella <- FALSE
        stop("error simulado al leer filas de huella en mssql", call. = FALSE)
      }
      orig_fetch_ms(resultado, n = n, conexion = conexion)
    },
    .package = "lupa"
  )
  expect_false(is.null(r_ms_f$perfil_muestra), info = "mssql-fallo-lectura")
  mms_f <- r_ms_f$perfil_muestra$meta$origen_dbi$muestreo
  expect_false(isTRUE(mms_f$reproducible), info = "mssql-fallo-lectura")
  expect_identical(mms_f$metodo, "primeras_filas_sin_orden_garantizado", info = "mssql-fallo-lectura")
  expect_equal(mms_f$filas_obtenidas, 20, info = "mssql-fallo-lectura")
  expect_match(mms_f$motivo_reproducibilidad, "huella", info = "mssql-fallo-lectura")
  expect_true(any(
    r_ms_f$resumen_tabla$cobertura$bloque == "perfil_muestra" &
      r_ms_f$resumen_tabla$cobertura$estado == "degradado"
  ), info = "mssql-fallo-lectura")
})

test_that("Cruce entre motores MD5: misma muestra con clave compuesta Unicode, DECIMAL, DATE, BIGINT > 2^53 y UUID", {
  skip_if_not_installed("duckdb")
  con_dk <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con_dk, shutdown = TRUE), silent = TRUE), add = TRUE)
  .mre_poblar_cruce(con_dk, "lupa_muestra_cruce_dk", "duckdb")
  ids_dk <- .mre_ids_cruce(con_dk, "lupa_muestra_cruce_dk")
  expect_length(ids_dk, 35L)

  # PostgreSQL actual
  con_pg <- tryCatch(.mre_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432")), skip = function(e) NULL)
  if (!is.null(con_pg)) {
    on.exit({
      try(DBI::dbExecute(con_pg, "DROP TABLE IF EXISTS lupa_muestra_cruce_pg"), silent = TRUE)
      try(DBI::dbDisconnect(con_pg), silent = TRUE)
    }, add = TRUE)
    .mre_poblar_cruce(con_pg, "lupa_muestra_cruce_pg", "pg")
    ids_pg <- .mre_ids_cruce(con_pg, "lupa_muestra_cruce_pg")
    expect_identical(ids_pg, ids_dk, info = "pg vs duckdb")
  }

  # PostgreSQL 9.3
  con_pg93 <- tryCatch(.mre_conectar_pg(Sys.getenv("LUPA_PG93_PORT", "55493")), skip = function(e) NULL)
  if (!is.null(con_pg93)) {
    on.exit({
      try(DBI::dbExecute(con_pg93, "DROP TABLE IF EXISTS lupa_muestra_cruce_pg93"), silent = TRUE)
      try(DBI::dbDisconnect(con_pg93), silent = TRUE)
    }, add = TRUE)
    .mre_poblar_cruce(con_pg93, "lupa_muestra_cruce_pg93", "pg93")
    ids_pg93 <- .mre_ids_cruce(con_pg93, "lupa_muestra_cruce_pg93")
    expect_identical(ids_pg93, ids_dk, info = "pg93 vs duckdb")
  }

  # SQL Server 2019+ (UTF-8)
  con_ms <- tryCatch(.mre_conectar_mssql(), skip = function(e) NULL)
  if (!is.null(con_ms)) {
    on.exit({
      try(DBI::dbExecute(con_ms, "IF OBJECT_ID(N'lupa_muestra_cruce_ms', N'U') IS NOT NULL DROP TABLE lupa_muestra_cruce_ms"), silent = TRUE)
      try(DBI::dbDisconnect(con_ms), silent = TRUE)
    }, add = TRUE)
    .mre_poblar_cruce(con_ms, "lupa_muestra_cruce_ms", "mssql")
    ids_ms <- .mre_ids_cruce(con_ms, "lupa_muestra_cruce_ms")
    expect_identical(ids_ms, ids_dk, info = "mssql vs duckdb")
  }
})
