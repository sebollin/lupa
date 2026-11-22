# Pruebas locales (MySQL y MariaDB) de muestra reproducible por huella MD5
# canonica (incluyendo cruce con DuckDB sobre clave compuesta Unicode/tipos
# mixtos) y marca de estabilidad con contadores del motor. Se ubican en
# `test-local-*` porque el paquete no declara RMySQL ni RMariaDB en DESCRIPTION.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.mrm_fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")

.mrm_conectar <- function(maria = FALSE) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  variable <- if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD"
  clave <- Sys.getenv(variable, unset = "")
  skip_if(!nzchar(clave), paste0("Sin ", variable, "."))
  puerto_def <- if (maria) "33306" else "33307"
  puerto_var <- if (maria) "LUPA_MARIADB_PORT" else "LUPA_MYSQL_PORT"
  puerto <- as.integer(Sys.getenv(puerto_var, Sys.getenv("LUPA_DB_PORT", puerto_def)))
  con <- tryCatch({
    if (maria) {
      DBI::dbConnect(
        RMariaDB::MariaDB(),
        host = Sys.getenv("LUPA_MARIADB_HOST", Sys.getenv("LUPA_DB_HOST", "127.0.0.1")),
        port = puerto,
        dbname = Sys.getenv("LUPA_MARIADB_DB", Sys.getenv("LUPA_DB_NAME", "lupa")),
        username = Sys.getenv("LUPA_MARIADB_USER", Sys.getenv("LUPA_DB_USER", "root")),
        password = clave
      )
    } else {
      DBI::dbConnect(
        RMySQL::MySQL(),
        host = Sys.getenv("LUPA_MYSQL_HOST", Sys.getenv("LUPA_DB_HOST", "127.0.0.1")),
        port = puerto,
        dbname = Sys.getenv("LUPA_MYSQL_DB", Sys.getenv("LUPA_DB_NAME", "lupa")),
        username = Sys.getenv("LUPA_MYSQL_USER", Sys.getenv("LUPA_DB_USER", "root")),
        password = clave
      )
    }
  }, error = function(e) NULL)
  skip_if(is.null(con), paste0(if (maria) "MariaDB" else "MySQL", " no disponible."))
  con
}

.mrm_poblar_par <- function(con, tb_a, tb_b, ddl_cols, n = 150L) {
  for (tb in c(tb_a, tb_b)) {
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tb)), silent = TRUE)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tb, " (", ddl_cols, ") ENGINE=InnoDB"))
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

.mrm_poblar_cruce <- function(con, tabla, es_duckdb = FALSE) {
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  if (isTRUE(es_duckdb)) {
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla, " (",
      "id INTEGER NOT NULL, clave VARCHAR NOT NULL, monto DECIMAL(20,4) NOT NULL, ",
      "fecha DATE NOT NULL, grande BIGINT NOT NULL, uid UUID NOT NULL)"
    ))
  } else {
    DBI::dbExecute(con, "SET NAMES utf8mb4")
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla, " (",
      "id INT NOT NULL PRIMARY KEY, ",
      "clave VARCHAR(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL, ",
      "monto DECIMAL(20,4) NOT NULL, fecha DATE NOT NULL, grande BIGINT NOT NULL, ",
      "uid CHAR(36) NOT NULL) ENGINE=InnoDB"
    ))
  }
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
  filas <- vapply(seq_len(n), function(i) {
    sprintf(
      "(%d, '%s', %s, '%s', %s, '%s')",
      i, gsub("'", "''", claves_txt[[i]], fixed = TRUE),
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
  if (isTRUE(es_duckdb)) {
    DBI::dbExecute(con, paste0("CREATE UNIQUE INDEX ", tabla, "_u ON ", tabla, "(id)"))
  }
}

.mrm_probar <- function(maria = FALSE) {
  etiqueta <- if (maria) "mariadb" else "mysql"
  con <- .mrm_conectar(maria = maria)
  con2 <- .mrm_conectar(maria = maria)
  tbs <- paste0(
    "lupa_muestra_", etiqueta,
    c("_pk_a", "_pk_b", "_uq_a", "_uq_b", "_cl_a", "_cl_b", "_mut", "_txt", "_cruce")
  )
  vista <- paste0("lupa_muestra_", etiqueta, "_vista")
  limpiar <- function() {
    try(DBI::dbExecute(con, paste0("DROP VIEW IF EXISTS ", vista)), silent = TRUE)
    for (tb in tbs) try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tb)), silent = TRUE)
  }
  on.exit({
    limpiar()
    try(DBI::dbDisconnect(con2), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  limpiar()

  perfilar_loc <- function(tb, ...) {
    suppressWarnings(suppressMessages(perfilar_dbi(
      con, tb, proteger_datos_personales = FALSE,
      analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
      fecha = .mrm_fecha, ...
    )))
  }

  contador_esperado <- if (isTRUE(maria)) "innodb_modified_counter" else "performance_schema"

  verificar_par <- function(tb_a, tb_b, clave = NULL, origen = "clave_primaria", tag = "") {
    p <- if (is.null(clave)) {
      suppressWarnings(suppressMessages(plan_perfilado_dbi(con, tb_a, muestra = 30L)))
    } else {
      suppressWarnings(suppressMessages(plan_perfilado_dbi(con, tb_a, muestra = 30L, clave = clave)))
    }
    r1 <- if (is.null(clave)) perfilar_loc(tb_a, muestra = 30L) else perfilar_loc(tb_a, muestra = 30L, clave = clave)
    r2 <- if (is.null(clave)) perfilar_loc(tb_a, muestra = 30L) else perfilar_loc(tb_a, muestra = 30L, clave = clave)
    rb <- if (is.null(clave)) perfilar_loc(tb_b, muestra = 30L) else perfilar_loc(tb_b, muestra = 30L, clave = clave)
    m1 <- r1$perfil_muestra$meta$origen_dbi$muestreo
    expect_true(isTRUE(m1$reproducible), info = paste(etiqueta, tag))
    expect_identical(m1$metodo, "muestra_huella_motor", info = paste(etiqueta, tag))
    expect_identical(m1$alcance_reproducibilidad, "entre_motores_md5", info = paste(etiqueta, tag))
    expect_true(m1$origen_clave_muestra %in% origen, info = paste(etiqueta, tag))
    expect_identical(m1$funcion_hash, "md5", info = paste(etiqueta, tag))
    expect_identical(r1$perfil_muestra$columnas, r2$perfil_muestra$columnas, info = paste(etiqueta, tag))
    expect_identical(r1$perfil_muestra$columnas, rb$perfil_muestra$columnas, info = paste(etiqueta, tag, "inv"))
    expect_identical(
      as.numeric(r1$resumen_tabla$meta$consultas$emitidas),
      as.numeric(attr(p, "total")),
      info = paste(etiqueta, tag, "plan")
    )
    est <- r1$resumen_tabla$meta$estabilidad
    expect_identical(est$estado, "sin_cambio_observado", info = paste(etiqueta, tag))
    expect_identical(est$cambio_detectado, FALSE, info = paste(etiqueta, tag))
    expect_identical(est$contador_motor, contador_esperado, info = paste(etiqueta, tag))
  }

  # 1. PK compuesta (en MySQL tiene grado total del catalogo; en MariaDB
  # `show_index` publica garantia desconocida y se prueba con indice unico
  # compuesto de grado total y con `clave =`).
  if (!isTRUE(maria)) {
    .mrm_poblar_par(
      con, tbs[[1L]], tbs[[2L]],
      "id INT NOT NULL, cod VARCHAR(40) NOT NULL, grupo VARCHAR(20), monto INT, PRIMARY KEY (id, cod)"
    )
    verificar_par(tbs[[1L]], tbs[[2L]], origen = "clave_primaria", tag = "pk")
  } else {
    .mrm_poblar_par(
      con, tbs[[1L]], tbs[[2L]],
      "id INT NOT NULL, cod VARCHAR(40) NOT NULL, grupo VARCHAR(20), monto INT, UNIQUE KEY uq_comp (id, cod)"
    )
    verificar_par(
      tbs[[1L]], tbs[[2L]], origen = c("restriccion_unica", "indice_unico"),
      tag = "uq-comp"
    )
  }

  # 2. Indice unico total sin PK
  .mrm_poblar_par(
    con, tbs[[3L]], tbs[[4L]],
    "id INT NOT NULL, cod VARCHAR(40) NOT NULL, grupo VARCHAR(20), monto INT, UNIQUE KEY uq_cod (cod)"
  )
  verificar_par(tbs[[3L]], tbs[[4L]], origen = c("restriccion_unica", "indice_unico"), tag = "uq")

  # 3. Clave declarada sin PK/indice
  .mrm_poblar_par(
    con, tbs[[5L]], tbs[[6L]],
    "id INT, cod VARCHAR(40), grupo VARCHAR(20), monto INT"
  )
  verificar_par(tbs[[5L]], tbs[[6L]], clave = "cod", origen = "clave_declarada", tag = "clave")

  # 4. Sin clave -> no reproducible con motivo explicito
  r_sin <- perfilar_loc(tbs[[5L]], muestra = 30L)
  expect_false(isTRUE(r_sin$perfil_muestra$meta$origen_dbi$muestreo$reproducible), info = etiqueta)

  # 5. Estabilidad: INSERT real desde segunda conexion
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tbs[[7L]], " (id INT PRIMARY KEY, monto INT, txt VARCHAR(20)) ENGINE=InnoDB"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tbs[[7L]], " VALUES ",
    paste0("(", 1:40, ", ", (1:40) * 10L, ", 'v", (1:40) %% 5L, "')", collapse = ", ")
  ))
  env_orig <- new.env(parent = emptyenv())
  env_orig$fn <- getFromNamespace(".consultar_cierre_estabilidad_dbi", "lupa")
  mutado <- FALSE
  r_mut <- testthat::with_mocked_bindings(
    perfilar_loc(tbs[[7L]], muestra = 20L),
    .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
      if (!mutado) {
        mutado <<- TRUE
        DBI::dbExecute(con2, paste0("INSERT INTO ", tbs[[7L]], " VALUES (999, 99999, 'ext')"))
      }
      env_orig$fn(conexion, tabla_sql, antes, presupuesto)
    },
    .package = "lupa"
  )
  expect_identical(r_mut$resumen_tabla$meta$estabilidad$estado, "cambio_detectado", info = etiqueta)
  expect_true(isTRUE(r_mut$resumen_tabla$meta$estabilidad$cambio_detectado), info = etiqueta)

  # 6. Estabilidad: UPDATE puramente textual (mismo segundo, 0 ms) desde con2
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tbs[[8L]], " (id INT PRIMARY KEY, monto INT, txt VARCHAR(40) NOT NULL) ENGINE=InnoDB"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tbs[[8L]], " VALUES ",
    paste0("(", 1:40, ", ", (1:40) * 10L, ", 'texto-", 1:40, "')", collapse = ", ")
  ))
  mutado_txt <- FALSE
  r_txt <- testthat::with_mocked_bindings(
    perfilar_loc(tbs[[8L]], muestra = 20L, metricas = "basicos"),
    .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
      if (!mutado_txt) {
        mutado_txt <<- TRUE
        DBI::dbExecute(con2, paste0("UPDATE ", tbs[[8L]], " SET txt = 'texto-cambiado' WHERE id = 1"))
      }
      env_orig$fn(conexion, tabla_sql, antes, presupuesto)
    },
    .package = "lupa"
  )
  est_txt <- r_txt$resumen_tabla$meta$estabilidad
  expect_identical(est_txt$estado, "cambio_detectado", info = paste(etiqueta, "upd-txt"))
  expect_true(isTRUE(est_txt$cambio_detectado), info = paste(etiqueta, "upd-txt"))
  expect_identical(est_txt$contador_motor, contador_esperado, info = paste(etiqueta, "upd-txt"))
  expect_false(grepl("monto", est_txt$sql, fixed = TRUE, useBytes = TRUE), info = paste(etiqueta, "cierre-sin-monto"))

  # 6b. En MySQL con `performance_schema` parcial (instrumento, consumidor o
  # setup_objects apagados), un UPDATE textual nunca afirma `sin_cambio_observado`:
  # cae a `sin_cambio_en_los_controles` con `NA` y motivo explicito.
  if (!isTRUE(maria)) {
    restaurar_ps <- function() {
      try(DBI::dbExecute(con2, "UPDATE performance_schema.setup_instruments SET ENABLED='YES', TIMED='YES' WHERE NAME = 'wait/io/table/sql/handler'"), silent = TRUE)
      try(DBI::dbExecute(con2, "UPDATE performance_schema.setup_consumers SET ENABLED='YES' WHERE NAME IN ('global_instrumentation', 'thread_instrumentation')"), silent = TRUE)
      try(DBI::dbExecute(con2, paste0("DELETE FROM performance_schema.setup_objects WHERE OBJECT_TYPE='TABLE' AND OBJECT_NAME='", tbs[[8L]], "'")), silent = TRUE)
      try(DBI::dbExecute(con2, "DELETE FROM performance_schema.setup_actors"), silent = TRUE)
      try(DBI::dbExecute(con2, "INSERT INTO performance_schema.setup_actors (HOST, USER, ROLE, ENABLED, HISTORY) VALUES ('%', '%', '%', 'YES', 'YES')"), silent = TRUE)
    }
    on.exit(restaurar_ps(), add = TRUE, after = FALSE)

    # Instrumento de E/S de tablas apagado
    DBI::dbExecute(con2, "UPDATE performance_schema.setup_instruments SET ENABLED='NO', TIMED='NO' WHERE NAME = 'wait/io/table/sql/handler'")
    m_io <- FALSE
    r_io <- testthat::with_mocked_bindings(
      perfilar_loc(tbs[[8L]], muestra = 20L, metricas = "basicos"),
      .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
        if (!m_io) {
          m_io <<- TRUE
          DBI::dbExecute(con2, paste0("UPDATE ", tbs[[8L]], " SET txt = 'io-off-upd' WHERE id = 2"))
        }
        env_orig$fn(conexion, tabla_sql, antes, presupuesto)
      },
      .package = "lupa"
    )
    restaurar_ps()
    e_io <- r_io$resumen_tabla$meta$estabilidad
    expect_identical(e_io$estado, "sin_cambio_en_los_controles", info = "mysql-ps-io-off")
    expect_true(is.na(e_io$cambio_detectado), info = "mysql-ps-io-off")
    expect_match(e_io$motivo, "performance_schema", info = "mysql-ps-io-off")

    # Consumidor global_instrumentation apagado
    DBI::dbExecute(con2, "UPDATE performance_schema.setup_consumers SET ENABLED='NO' WHERE NAME = 'global_instrumentation'")
    m_gl <- FALSE
    r_gl <- testthat::with_mocked_bindings(
      perfilar_loc(tbs[[8L]], muestra = 20L, metricas = "basicos"),
      .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
        if (!m_gl) {
          m_gl <<- TRUE
          DBI::dbExecute(con2, paste0("UPDATE ", tbs[[8L]], " SET txt = 'glob-off-upd' WHERE id = 3"))
        }
        env_orig$fn(conexion, tabla_sql, antes, presupuesto)
      },
      .package = "lupa"
    )
    restaurar_ps()
    e_gl <- r_gl$resumen_tabla$meta$estabilidad
    expect_identical(e_gl$estado, "sin_cambio_en_los_controles", info = "mysql-ps-glob-off")
    expect_true(is.na(e_gl$cambio_detectado), info = "mysql-ps-glob-off")

    # Objeto excluido en setup_objects
    DBI::dbExecute(con2, paste0(
      "INSERT INTO performance_schema.setup_objects (OBJECT_TYPE, OBJECT_SCHEMA, OBJECT_NAME, ENABLED, TIMED) ",
      "VALUES ('TABLE', DATABASE(), '", tbs[[8L]], "', 'NO', 'NO')"
    ))
    m_ob <- FALSE
    r_ob <- testthat::with_mocked_bindings(
      perfilar_loc(tbs[[8L]], muestra = 20L, metricas = "basicos"),
      .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
        if (!m_ob) {
          m_ob <<- TRUE
          DBI::dbExecute(con2, paste0("UPDATE ", tbs[[8L]], " SET txt = 'obj-off-upd' WHERE id = 4"))
        }
        env_orig$fn(conexion, tabla_sql, antes, presupuesto)
      },
      .package = "lupa"
    )
    restaurar_ps()
    e_ob <- r_ob$resumen_tabla$meta$estabilidad
    expect_identical(e_ob$estado, "sin_cambio_en_los_controles", info = "mysql-ps-obj-off")
    expect_true(is.na(e_ob$cambio_detectado), info = "mysql-ps-obj-off")

    # 6c. Hilos/actores no instrumentados (`setup_actors` y `threads`):
    # (1) comodin de `setup_actors` apagado con conexion escritora nueva;
    # (2) conexion escritora abierta mientras estaba apagado y mantenida viva
    #     despues de volver `setup_actors` a YES (`threads.INSTRUMENTED = 'NO'`);
    # (3) comodin en YES mas regla de exclusion especifica `ENABLED = 'NO'`.
    DBI::dbExecute(con2, "UPDATE performance_schema.setup_actors SET ENABLED='NO' WHERE HOST='%' AND USER='%' AND ROLE='%'")
    wr_no <- .mrm_conectar(maria = FALSE)
    on.exit(try(DBI::dbDisconnect(wr_no), silent = TRUE), add = TRUE, after = FALSE)
    m_ac <- FALSE
    r_ac <- testthat::with_mocked_bindings(
      perfilar_loc(tbs[[8L]], muestra = 20L, metricas = "basicos"),
      .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
        if (!m_ac) {
          m_ac <<- TRUE
          DBI::dbExecute(wr_no, paste0("UPDATE ", tbs[[8L]], " SET txt = 'act-off-upd' WHERE id = 5"))
        }
        env_orig$fn(conexion, tabla_sql, antes, presupuesto)
      },
      .package = "lupa"
    )
    e_ac <- r_ac$resumen_tabla$meta$estabilidad
    expect_identical(e_ac$estado, "sin_cambio_en_los_controles", info = "mysql-ps-actors-off")
    expect_true(is.na(e_ac$cambio_detectado), info = "mysql-ps-actors-off")

    # Restaurar `setup_actors` a YES dejando `wr_no` abierto (`threads.INSTRUMENTED = 'NO'`)
    DBI::dbExecute(con2, "UPDATE performance_schema.setup_actors SET ENABLED='YES', HISTORY='YES' WHERE HOST='%' AND USER='%' AND ROLE='%'")
    m_th <- FALSE
    r_th <- testthat::with_mocked_bindings(
      perfilar_loc(tbs[[8L]], muestra = 20L, metricas = "basicos"),
      .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
        if (!m_th) {
          m_th <<- TRUE
          DBI::dbExecute(wr_no, paste0("UPDATE ", tbs[[8L]], " SET txt = 'thr-no-upd' WHERE id = 6"))
        }
        env_orig$fn(conexion, tabla_sql, antes, presupuesto)
      },
      .package = "lupa"
    )
    try(DBI::dbDisconnect(wr_no), silent = TRUE)
    e_th <- r_th$resumen_tabla$meta$estabilidad
    expect_identical(e_th$estado, "sin_cambio_en_los_controles", info = "mysql-ps-thread-no")
    expect_true(is.na(e_th$cambio_detectado), info = "mysql-ps-thread-no")

    # Regla especifica de exclusion con comodin en YES
    DBI::dbExecute(con2, "INSERT INTO performance_schema.setup_actors (HOST, USER, ROLE, ENABLED, HISTORY) VALUES ('%', 'root', '%', 'NO', 'NO')")
    m_ex <- FALSE
    r_ex <- testthat::with_mocked_bindings(
      perfilar_loc(tbs[[8L]], muestra = 20L, metricas = "basicos"),
      .consultar_cierre_estabilidad_dbi = function(conexion, tabla_sql, antes, presupuesto = NULL) {
        if (!m_ex) {
          m_ex <<- TRUE
          DBI::dbExecute(con2, paste0("UPDATE ", tbs[[8L]], " SET txt = 'excl-off-upd' WHERE id = 7"))
        }
        env_orig$fn(conexion, tabla_sql, antes, presupuesto)
      },
      .package = "lupa"
    )
    restaurar_ps()
    e_ex <- r_ex$resumen_tabla$meta$estabilidad
    expect_identical(e_ex$estado, "sin_cambio_en_los_controles", info = "mysql-ps-actor-excl")
    expect_true(is.na(e_ex$cambio_detectado), info = "mysql-ps-actor-excl")

    # Una vez restaurado `setup_actors` y sin hilos no instrumentados, vuelve a confiar en el contador
    r_ok_ps <- perfilar_loc(tbs[[8L]], muestra = 20L, metricas = "basicos")
    e_ok_ps <- r_ok_ps$resumen_tabla$meta$estabilidad
    expect_identical(
      e_ok_ps$estado, "sin_cambio_observado",
      info = paste("mysql-ps-restaurado:", e_ok_ps$motivo %||% "-")
    )
    expect_identical(
      e_ok_ps$cambio_detectado, FALSE,
      info = paste("mysql-ps-restaurado:", e_ok_ps$motivo %||% "-")
    )
  }

  # 6d. Error simulado al leer filas de la muestra con huella: nunca se pierde
  # `perfil_muestra`, cae a respaldo con `reproducible = FALSE` y motivo explicito.
  orig_traer_loc <- getFromNamespace(".traer_consulta_dbi", "lupa")
  r_loc_f <- testthat::with_mocked_bindings(
    perfilar_loc(tbs[[1L]], muestra = 20L, metricas = "basicos", max_bytes_muestra = Inf),
    .traer_consulta_dbi = function(conexion, sql) {
      if (grepl("MD5(", sql, fixed = TRUE, useBytes = TRUE)) {
        stop("error simulado al leer filas de huella", call. = FALSE)
      }
      orig_traer_loc(conexion, sql)
    },
    .package = "lupa"
  )
  expect_false(is.null(r_loc_f$perfil_muestra), info = paste(etiqueta, "fallo-lectura"))
  mloc_f <- r_loc_f$perfil_muestra$meta$origen_dbi$muestreo
  expect_false(isTRUE(mloc_f$reproducible), info = paste(etiqueta, "fallo-lectura"))
  expect_identical(mloc_f$metodo, "primeras_filas_sin_orden_garantizado", info = paste(etiqueta, "fallo-lectura"))
  expect_equal(mloc_f$filas_obtenidas, 20, info = paste(etiqueta, "fallo-lectura"))
  expect_match(mloc_f$motivo_reproducibilidad, "huella", info = paste(etiqueta, "fallo-lectura"))
  expect_true(any(
    r_loc_f$resumen_tabla$cobertura$bloque == "perfil_muestra" &
      r_loc_f$resumen_tabla$cobertura$estado == "degradado"
  ), info = paste(etiqueta, "fallo-lectura"))

  # 7. Vista (sin contador del motor): `sin_cambio_en_los_controles` y NA
  DBI::dbExecute(con, paste0("CREATE VIEW ", vista, " AS SELECT id, monto, txt FROM ", tbs[[8L]]))
  r_v <- perfilar_loc(vista, muestra = 20L, metricas = "basicos")
  expect_identical(r_v$resumen_tabla$meta$estabilidad$estado, "sin_cambio_en_los_controles", info = paste(etiqueta, "vista"))
  expect_true(is.na(r_v$resumen_tabla$meta$estabilidad$cambio_detectado), info = paste(etiqueta, "vista"))

  # 8. Cruce entre motores MD5 contra DuckDB con clave compuesta Unicode/DECIMAL/DATE/BIGINT/UUID
  if (requireNamespace("duckdb", quietly = TRUE)) {
    con_dk <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
    on.exit(try(DBI::dbDisconnect(con_dk, shutdown = TRUE), silent = TRUE), add = TRUE)
    .mrm_poblar_cruce(con_dk, "lupa_muestra_cruce_dk_loc", es_duckdb = TRUE)
    r_dk <- suppressWarnings(suppressMessages(perfilar_dbi(
      con_dk, "lupa_muestra_cruce_dk_loc", muestra = 35L,
      clave = c("clave", "monto", "fecha", "grande", "uid"),
      metricas = "basicos", proteger_datos_personales = FALSE,
      analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
      fecha = .mrm_fecha
    )))
    ids_dk <- as.integer(DBI::dbGetQuery(con_dk, r_dk$perfil_muestra$meta$origen_dbi$muestreo$sql_muestra)$id)

    .mrm_poblar_cruce(con, tbs[[9L]], es_duckdb = FALSE)
    r_my <- perfilar_loc(
      tbs[[9L]], muestra = 35L,
      clave = c("clave", "monto", "fecha", "grande", "uid"),
      metricas = "basicos"
    )
    mu_my <- r_my$perfil_muestra$meta$origen_dbi$muestreo
    expect_true(isTRUE(mu_my$reproducible), info = paste(etiqueta, "cruce"))
    expect_identical(mu_my$alcance_reproducibilidad, "entre_motores_md5", info = paste(etiqueta, "cruce"))
    ids_my <- as.integer(DBI::dbGetQuery(con, mu_my$sql_muestra)$id)
    expect_identical(ids_my, ids_dk, info = paste(etiqueta, "cruce vs duckdb"))
  }
}

test_that("MySQL: muestra reproducible por huella MD5 canonica, plan exacto y estabilidad", {
  .mrm_probar(maria = FALSE)
})

test_that("MariaDB: muestra reproducible por huella MD5 canonica, plan exacto y estabilidad", {
  .mrm_probar(maria = TRUE)
})
