# Pruebas de equivalencia y guardas de `medir_dbi()` / `plan_medicion_dbi()`
# en PostgreSQL (actual y 9.3) y SQL Server. Se saltean si el motor no esta
# disponible en el entorno local.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.lmd_conectar_pg <- function(puerto, bigint = "integer64") {
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(puerto),
    user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"),
    bigint = bigint
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en puerto ", puerto, "."))
  con
}

.lmd_conectar_mssql <- function() {
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

.lmd_comparar_resumen <- function(esp, obt, info = "") {
  cols <- c(
    "metrica_instanciada", "entidad", "atributo", "objeto",
    "resultado", "agregacion", "tipo_resultado",
    "numerador", "denominador", "n_universo", "n_no_evaluados",
    "motivo_no_evaluados"
  )
  expect_gt(nrow(obt), 0L)
  for (i in seq_len(nrow(obt))) {
    m <- obt$metrica_instanciada[[i]]
    j <- match(m, esp$metrica_instanciada)
    expect_false(is.na(j), info = paste(info, m))
    for (cc in cols) {
      expect_identical(esp[[cc]][[j]], obt[[cc]][[i]],
                       info = paste(info, m, cc))
    }
  }
}

.lmd_inst <- function(met, nom, ent, atr, ...) {
  instanciar(especializar(met, nom, ...), ent, atr, nombre_instancia = nom)
}

test_that("PostgreSQL actual reproduce memoria en todas las metricas traducibles", {
  con <- .lmd_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  limpiar <- function() {
    try(DBI::dbExecute(con, "SET TIME ZONE 'UTC'"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_medir_pg_d"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_medir_pg_r"), silent = TRUE)
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_medir_pg_d (",
    "ni INTEGER, nd DOUBLE PRECISION, dc NUMERIC(20,2), ",
    "tx TEXT, lg BOOLEAN, fe DATE, fe2 DATE, ts TIMESTAMP, tz TIMESTAMPTZ)"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_medir_pg_r (",
    "ki INTEGER, kd NUMERIC(20,2), kt TEXT, kf DATE)"
  ))
  q_idot <- as.character(DBI::dbQuoteString(con, intToUtf8(0x0130L)))
  q_kelv <- as.character(DBI::dbQuoteString(con, intToUtf8(0x212AL)))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_medir_pg_d VALUES ",
    "(1, 0.3, 0.30, 'a', TRUE, '2026-01-01', '2026-06-01', '2026-01-01 12:00:00', '2026-01-01 12:00:00+00'), ",
    "(2, 0.30000000000000004, 9007199254740992, 'a ', FALSE, '1960-06-15', '2025-05-10', '1960-06-15 00:00:00', '1960-06-15 00:00:00+00'), ",
    "(2, -0.0, 9007199254740993, E' \\tNA\\r\\n ', TRUE, '2026-01-01', '2026-01-01', '2026-01-01 12:00:00.500', '2026-01-01 12:00:00+00'), ",
    "(-99, 0.0, 0.10, 'sin\tdato', FALSE, '2040-03-15', '2026-06-01', '2040-03-15 08:30:00', '2040-03-15 08:30:00+00'), ",
    "(5, 'NaN'::double precision, 'NaN'::numeric, ", q_idot, ", TRUE, '2025-05-10', '2025-05-10', '2025-05-10 00:00:00', '2025-05-10 00:00:00+00'), ",
    "(1, 'NaN'::double precision, 1.25, ", q_kelv, ", NULL, '2026-06-01', NULL, NULL, NULL), ",
    "(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_medir_pg_r VALUES ",
    "(1, 0.30, 'a', '2026-01-01'), ",
    "(2, 9007199254740992, '1', '1960-06-15'), ",
    "(NULL, NULL, NULL, NULL)"
  ))

  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  mod <- modelo(list(
    .lmd_inst(nuc$NoNulo, "NN_int", "d", "ni", valores_nulos = -99L),
    .lmd_inst(nuc$NoNulo, "NN_dbl", "d", "nd"),
    .lmd_inst(nuc$NoNulo, "NN_txt_cent", "d", "tx",
              valores_nulos = c("NA", "sin dato", "i")),
    .lmd_inst(nuc$NoNulo, "NN_apl", "d", "ni", aplicable = ~ ni > 0L | is.na(ni)),
    .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_dec", "d", "dc",
              valores = c(0.3, 9007199254740992)),
    .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_txt", "d", "tx",
              valores = c("a", "a ")),
    .lmd_inst(nuc$Formato, "FMT_dic", "d", "lg", diccionario = c(TRUE)),
    .lmd_inst(nuc$ValoresPosiblesPorComprension, "RC_fe", "d", "fe",
              minimo = as.Date("2020-01-01"), maximo = as.Date("2030-01-01")),
    .lmd_inst(nuc$OportunidadAtributoPorFecha, "OP_ts", "d", "ts",
              fecha_limite = as.POSIXct("2026-01-01 12:00:00", tz = "UTC")),
    .lmd_inst(nuc$OportunidadAtributoPorFecha, "OP_tz", "d", "tz",
              fecha_limite = as.POSIXct("2026-06-01 00:00:00", tz = "UTC")),
    .lmd_inst(nuc$OportunidadEntPorIntervalo, "OPE_fe", "d", character(),
              vigencia = vigencia(columna_actualizacion = "fe",
                                  inicio_intervalo = as.Date("1960-01-01"),
                                  fin_intervalo = as.Date("2026-06-01"))),
    .lmd_inst(nuc$AtributoDuplicado, "DUP_dbl", "d", "nd"),
    .lmd_inst(nuc$AtributoDuplicado, "DUP_txt", "d", "tx"),
    .lmd_inst(nuc$ConjuntoAtributosDuplicado, "CDUP_nd", "d", c("nd", "dc")),
    .lmd_inst(nuc$ReglaIntegridadIntraEntidad, "F_ord", "d", c("fe", "fe2"),
              regla = ~ is.na(fe) | is.na(fe2) | fe <= fe2),
    .lmd_inst(nuc$ReglaIntegridadIntraEntidad, "F_log", "d", c("lg", "ni"),
              regla = ~ is.na(lg) | lg | (ni %in% c(2L, -99L))),
    .lmd_inst(nuc$ReglaIntegridadInterEntidad, "REF_dec", c("r", "d"), c("kd", "dc")),
    .lmd_inst(nuc$ReglaIntegridadInterEntidad, "REF_txt", c("r", "d"), c("kt", "tx")),
    .lmd_inst(nuc$ReglaIntegridadInterEntidad, "REF_ti", c("r", "d"), c("kt", "ni"))
  ))
  tablas_id <- list(
    d = DBI::Id(schema = "public", table = "lupa_medir_pg_d"),
    r = DBI::Id(schema = "public", table = "lupa_medir_pg_r")
  )
  tablas_r <- list(
    d = DBI::dbReadTable(con, tablas_id$d),
    r = DBI::dbReadTable(con, tablas_id$r)
  )
  p <- plan_medicion_dbi(mod, con, tablas = tablas_id)
  expect_identical(attr(p, "motor", exact = TRUE), "postgresql")
  expect_true(all(p$via %in% c("sql_consolidado", "sql_dedicado", "sql_dedicado_referencial")))

  for (apli in list(NULL, list(ni = ~ !is.na(tx) | ni > 0L, tx = ~ !(ni %in% c(-99L, NA))))) {
    esp <- suppressWarnings(medir(
      mod, tablas_r, id_medicion = "pg", fecha = fecha,
      aplicabilidad = apli, detalle = "resumen"
    ))
    obt <- suppressWarnings(medir_dbi(
      mod, con, tablas = tablas_id, id_medicion = "pg", fecha = fecha,
      aplicabilidad = apli
    ))
    expect_identical(nrow(obt), length(mod$metricas))
    .lmd_comparar_resumen(esp, obt, info = if (is.null(apli)) "pg_sin_apl" else "pg_con_apl")
  }
})

test_that("PostgreSQL aplica guardas de bpchar, float4, BIGINT, colacion no determinista, huso y bloques", {
  con <- .lmd_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  limpiar <- function() {
    try(DBI::dbExecute(con, "SET TIME ZONE 'UTC'"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_medir_pg_g"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_medir_pg_nd"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP COLLATION IF EXISTS lupa_medir_ci_nd"), silent = TRUE)
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_medir_pg_g (",
    "ch CHAR(5), fl4 REAL, big BIGINT, ts TIMESTAMP, tz TIMESTAMPTZ, tx TEXT)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_medir_pg_g VALUES ",
    "('a', 0.3, 1, '2026-01-01 12:00:00', '2026-01-01 12:00:00+00', 'abc'), ",
    "('a ', 0.1, 2, '2026-06-01 00:00:00', '2026-06-01 00:00:00+00', '123'), ",
    "(NULL, NULL, NULL, NULL, NULL, NULL)"
  ))
  nuc <- metricas_nucleo()
  mod <- modelo(list(
    .lmd_inst(nuc$NoNulo, "NN_ch", "g", "ch"),
    .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_ch", "g", "ch", valores = c("a", "a    ")),
    .lmd_inst(nuc$NoNulo, "NN_fl4", "g", "fl4"),
    .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_fl4", "g", "fl4", valores = c(0.3, 0.1)),
    .lmd_inst(nuc$NoNulo, "NN_big", "g", "big"),
    .lmd_inst(nuc$OportunidadAtributoPorFecha, "OP_tz", "g", "tz",
              fecha_limite = as.POSIXct("2026-03-01 00:00:00", tz = "UTC")),
    .lmd_inst(nuc$OportunidadAtributoPorFecha, "OP_ts", "g", "ts",
              fecha_limite = as.POSIXct("2026-03-01 00:00:00", tz = "UTC")),
    .lmd_inst(nuc$Formato, "FMT_rx", "g", "tx", expresion_regular = "^[a-z]+$")
  ))
  p_utc <- plan_medicion_dbi(mod, con, tablas = c(g = "lupa_medir_pg_g"))
  vias_utc <- stats::setNames(p_utc$via, p_utc$metrica_instanciada)
  expect_identical(vias_utc[["NN_ch"]], "sql_consolidado")
  expect_identical(vias_utc[["VE_ch"]], "no_medible")
  expect_identical(vias_utc[["NN_fl4"]], "sql_consolidado")
  local({
    datos <- list(g = DBI::dbReadTable(con, "lupa_medir_pg_g"))
    esperado <- suppressWarnings(medir(
      mod, datos, id_medicion = "pg_guardas", fecha = as.POSIXct(
        "2026-11-21 12:00:00", tz = "UTC"
      ), detalle = "resumen"
    ))
    obtenido <- suppressWarnings(medir_dbi(
      mod, con, tablas = c(g = "lupa_medir_pg_g"),
      id_medicion = "pg_guardas", fecha = as.POSIXct(
        "2026-11-21 12:00:00", tz = "UTC"
      )
    ))
    expect_identical(
      attr(obtenido, "ejecucion_dbi", exact = TRUE)$vias[["VE_fl4"]],
      "sql_consolidado"
    )
    .lmd_comparar_resumen(
      esperado[esperado$metrica_instanciada == "VE_fl4", , drop = FALSE],
      obtenido[obtenido$metrica_instanciada == "VE_fl4", , drop = FALSE],
      info = "pg_ve_fl4"
    )
  })
  expect_identical(vias_utc[["NN_big"]], "no_medible")
  expect_identical(vias_utc[["OP_tz"]], "sql_consolidado")
  expect_identical(vias_utc[["OP_ts"]], "sql_consolidado")

  # Huso no UTC: timestamptz pasa a no_medible y timestamp sigue en SQL
  DBI::dbExecute(con, "SET TIME ZONE 'America/Montevideo'")
  p_mvd <- plan_medicion_dbi(mod, con, tablas = c(g = "lupa_medir_pg_g"))
  vias_mvd <- stats::setNames(p_mvd$via, p_mvd$metrica_instanciada)
  local({
    datos <- list(g = DBI::dbReadTable(con, "lupa_medir_pg_g"))
    esperado <- suppressWarnings(medir(
      mod, datos, id_medicion = "pg_huso", fecha = as.POSIXct(
        "2026-11-21 12:00:00", tz = "UTC"
      ), detalle = "resumen"
    ))
    obtenido <- suppressWarnings(medir_dbi(
      mod, con, tablas = c(g = "lupa_medir_pg_g"),
      id_medicion = "pg_huso", fecha = as.POSIXct(
        "2026-11-21 12:00:00", tz = "UTC"
      )
    ))
    expect_identical(
      attr(obtenido, "ejecucion_dbi", exact = TRUE)$vias[["OP_tz"]],
      "sql_consolidado"
    )
    .lmd_comparar_resumen(
      esperado[esperado$metrica_instanciada == "OP_tz", , drop = FALSE],
      obtenido[obtenido$metrica_instanciada == "OP_tz", , drop = FALSE],
      info = "pg_op_tz"
    )
  })
  expect_identical(vias_mvd[["OP_ts"]], "sql_consolidado")
  DBI::dbExecute(con, "SET TIME ZONE 'UTC'")

  # Lectura por bloques sobre metrica separable no SQL
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  df_g <- list(g = DBI::dbReadTable(con, "lupa_medir_pg_g"))
  mod_b <- modelo(.lmd_inst(nuc$Formato, "FMT_rx", "g", "tx", expresion_regular = "^[a-z]+$"))
  esp_b <- suppressWarnings(medir(mod_b, df_g, id_medicion = "b", fecha = fecha, detalle = "resumen"))
  obt_b <- suppressWarnings(medir_dbi(mod_b, con, tablas = c(g = "lupa_medir_pg_g"),
                                      id_medicion = "b", fecha = fecha,
                                      estrategia_no_sql = "bloques"))
  expect_identical(attr(obt_b, "ejecucion_dbi")$vias[["FMT_rx"]], "bloques_r")
  .lmd_comparar_resumen(esp_b, obt_b, info = "pg_bloques")

  # Colacion no determinista ICU (PG >= 12)
  creo_nd <- tryCatch({
    DBI::dbExecute(con, "CREATE COLLATION lupa_medir_ci_nd (provider = icu, locale = 'und-u-ks-level2', deterministic = false)")
    TRUE
  }, error = function(e) FALSE)
  if (creo_nd) {
    DBI::dbExecute(con, "CREATE TABLE lupa_medir_pg_nd (t TEXT COLLATE lupa_medir_ci_nd)")
    DBI::dbExecute(con, "INSERT INTO lupa_medir_pg_nd VALUES ('a'), ('A')")
    mod_nd <- modelo(list(
      .lmd_inst(nuc$NoNulo, "NN_nd", "d", "t"),
      .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_nd", "d", "t", valores = "a"),
      .lmd_inst(nuc$AtributoDuplicado, "DUP_nd", "d", "t")
    ))
    p_nd <- plan_medicion_dbi(mod_nd, con, tablas = c(d = "lupa_medir_pg_nd"))
    vias_nd <- stats::setNames(p_nd$via, p_nd$metrica_instanciada)
    expect_identical(vias_nd[["NN_nd"]], "sql_consolidado")
    expect_identical(vias_nd[["VE_nd"]], "no_medible")
    expect_identical(vias_nd[["DUP_nd"]], "no_medible")
  }
})

test_that("PostgreSQL 9.3 traduce sin to_regclass ni collisdeterministic y guarda extra_float_digits", {
  con <- .lmd_conectar_pg(Sys.getenv("LUPA_PG93_PORT", "55493"))
  limpiar <- function() {
    try(DBI::dbExecute(con, "SET TIME ZONE 'UTC'"), silent = TRUE)
    try(DBI::dbExecute(con, "RESET extra_float_digits"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_medir_pg93_d"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_medir_pg93_r"), silent = TRUE)
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_medir_pg93_d (",
    "ni INTEGER, nd DOUBLE PRECISION, dc NUMERIC(20,2), tx TEXT, fe DATE, ts TIMESTAMP, tz TIMESTAMPTZ)"
  ))
  DBI::dbExecute(con, "CREATE TABLE lupa_medir_pg93_r (ki INTEGER, kt TEXT)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_medir_pg93_d VALUES ",
    "(1, 0.3, 0.30, 'a', '2026-01-01', '2026-01-01 12:00:00', '2026-01-01 12:00:00+00'), ",
    "(2, 0.30000000000000004, 9007199254740992, 'a ', '1960-06-15', '1960-06-15 00:00:00', '1960-06-15 00:00:00+00'), ",
    "(2, 'NaN'::double precision, 9007199254740993, E' \\tNA\\r\\n', '2026-01-01', '2026-01-01 12:00:00', '2026-01-01 12:00:00+00'), ",
    "(NULL, NULL, NULL, NULL, NULL, NULL, NULL)"
  ))
  DBI::dbExecute(con, "INSERT INTO lupa_medir_pg93_r VALUES (1, 'a'), (2, '1')")

  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  mod <- modelo(list(
    .lmd_inst(nuc$NoNulo, "NN_txt", "d", "tx", valores_nulos = "NA"),
    .lmd_inst(nuc$NoNulo, "NN_nd", "d", "nd"),
    .lmd_inst(nuc$AtributoDuplicado, "DUP_nd", "d", "nd"),
    .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_dc", "d", "dc",
              valores = c(0.3, 9007199254740992)),
    .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_txt", "d", "tx", valores = c("a", "a ")),
    .lmd_inst(nuc$OportunidadAtributoPorFecha, "OP_fe", "d", "fe",
              fecha_limite = as.Date("2025-01-01")),
    .lmd_inst(nuc$OportunidadAtributoPorFecha, "OP_tz", "d", "tz",
              fecha_limite = as.POSIXct("2025-01-01 00:00:00", tz = "UTC")),
    .lmd_inst(nuc$ReglaIntegridadInterEntidad, "REF_ti", c("r", "d"), c("kt", "ni"))
  ))
  tablas <- c(d = "lupa_medir_pg93_d", r = "lupa_medir_pg93_r")
  p0 <- plan_medicion_dbi(mod, con, tablas = tablas)
  vias0 <- stats::setNames(p0$via, p0$metrica_instanciada)
  expect_identical(vias0[["NN_nd"]], "sql_consolidado")
  expect_identical(vias0[["DUP_nd"]], "sql_dedicado")
  expect_identical(vias0[["NN_txt"]], "sql_consolidado")
  expect_identical(vias0[["VE_dc"]], "sql_consolidado")
  expect_identical(vias0[["VE_txt"]], "sql_consolidado")
  expect_identical(vias0[["OP_fe"]], "sql_consolidado")
  expect_identical(vias0[["REF_ti"]], "sql_dedicado_referencial")

  tablas_r0 <- list(d = DBI::dbReadTable(con, "lupa_medir_pg93_d"),
                    r = DBI::dbReadTable(con, "lupa_medir_pg93_r"))
  esp0 <- suppressWarnings(medir(mod, tablas_r0, id_medicion = "p93", fecha = fecha, detalle = "resumen"))
  obt0 <- suppressWarnings(medir_dbi(mod, con, tablas = tablas, id_medicion = "p93", fecha = fecha))
  .lmd_comparar_resumen(esp0, obt0, info = "pg93_efd0")

  # Con extra_float_digits = 3, float8 si se traduce y coincide con memoria
  DBI::dbExecute(con, "SET extra_float_digits = 3")
  p3 <- plan_medicion_dbi(mod, con, tablas = tablas)
  expect_identical(p3$via[p3$metrica_instanciada == "DUP_nd"], "sql_dedicado")
  tablas_r3 <- list(d = DBI::dbReadTable(con, "lupa_medir_pg93_d"),
                    r = DBI::dbReadTable(con, "lupa_medir_pg93_r"))
  esp3 <- suppressWarnings(medir(mod, tablas_r3, id_medicion = "p93b", fecha = fecha, detalle = "resumen"))
  obt3 <- suppressWarnings(medir_dbi(mod, con, tablas = tablas, id_medicion = "p93b", fecha = fecha))
  expect_identical(nrow(obt3), length(mod$metricas))
  .lmd_comparar_resumen(esp3, obt3, info = "pg93_efd3")
})

test_that("SQL Server reproduce memoria en metricas traducibles, logica de tres valores y referencial", {
  con <- .lmd_conectar_mssql()
  limpiar <- function() {
    try(DBI::dbExecute(con, "DROP TABLE lupa_medir_ss_d"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE lupa_medir_ss_r"), silent = TRUE)
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_medir_ss_d (",
    "ni INT, nd FLOAT, rl REAL, dc DECIMAL(20,2), ",
    "tx NVARCHAR(40) COLLATE Latin1_General_100_BIN2, ",
    "vc VARCHAR(40) COLLATE Latin1_General_BIN2, ",
    "lg BIT, fe DATE, fe2 DATE, dt DATETIME, dt2 DATETIME2(3))"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_medir_ss_r (",
    "ki INT, kd DECIMAL(20,2), ",
    "kt NVARCHAR(40) COLLATE Latin1_General_100_BIN2, ",
    "kv VARCHAR(40) COLLATE Latin1_General_BIN2, kf DATE)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_medir_ss_d VALUES ",
    "(1, 0.3, 0.3, 0.30, N'a', 'a', 1, '2026-01-01', '2026-06-01', '2026-01-01 12:00:00', '2026-01-01 12:00:00.000'), ",
    "(2, 0.1, 0.1, 9007199254740992, N'A', 'A', 0, '1960-06-15', '2025-05-10', '1960-06-15 00:00:00', '2026-01-01 12:00:00.500'), ",
    "(2, 0.3, 0.5, 9007199254740993, N'\u00e1', '1', 1, '2026-01-01', '2026-01-01', '2026-01-01 12:00:00', '1960-06-15 00:00:00'), ",
    "(-99, 1.5, 0.3, 0.10, N'a' + NCHAR(9), 'b', NULL, '2040-03-15', '2026-06-01', '2040-03-15 08:30:00', '2040-03-15 08:30:00'), ",
    "(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_medir_ss_r VALUES ",
    "(1, 0.30, N'a', 'a', '2026-01-01'), ",
    "(2, 9007199254740992, N'1', '1', '1960-06-15'), ",
    "(NULL, NULL, NULL, NULL, NULL)"
  ))

  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  mod <- modelo(list(
    .lmd_inst(nuc$NoNulo, "NN_int", "d", "ni", valores_nulos = -99L),
    .lmd_inst(nuc$NoNulo, "NN_apl", "d", "ni", aplicable = ~ ni > 0L | is.na(ni)),
    .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_dec", "d", "dc",
              valores = c(0.3, 9007199254740992)),
    .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_rl", "d", "rl", valores = c(0.3, 0.5)),
    .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_tx", "d", "tx", valores = c("a", "\u00e1")),
    .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_vc", "d", "vc", valores = c("a", "b")),
    .lmd_inst(nuc$Formato, "FMT_log", "d", "lg", diccionario = c(TRUE)),
    .lmd_inst(nuc$ValoresPosiblesPorComprension, "RC_fe", "d", "fe",
              minimo = as.Date("2020-01-01"), maximo = as.Date("2030-01-01")),
    .lmd_inst(nuc$OportunidadAtributoPorFecha, "OP_dt", "d", "dt",
              fecha_limite = as.POSIXct("2026-01-01 12:00:00", tz = "UTC")),
    .lmd_inst(nuc$OportunidadAtributoPorFecha, "OP_dt2", "d", "dt2",
              fecha_limite = as.POSIXct("2026-01-01 12:00:00", tz = "UTC")),
    .lmd_inst(nuc$AtributoDuplicado, "DUP_tx", "d", "tx"),
    .lmd_inst(nuc$ConjuntoAtributosDuplicado, "CDUP_nt", "d", c("ni", "tx")),
    .lmd_inst(nuc$ReglaIntegridadIntraEntidad, "F_3v", "d", c("ni", "lg"),
              regla = ~ is.na(ni > 1L) | is.na(lg) | ((ni > 1L) == lg)),
    .lmd_inst(nuc$ReglaIntegridadIntraEntidad, "F_ord", "d", c("fe", "fe2"),
              regla = ~ is.na(fe) | is.na(fe2) | fe <= fe2),
    .lmd_inst(nuc$ReglaIntegridadInterEntidad, "REF_dec", c("r", "d"), c("kd", "dc")),
    .lmd_inst(nuc$ReglaIntegridadInterEntidad, "REF_tx", c("r", "d"), c("kt", "tx")),
    .lmd_inst(nuc$ReglaIntegridadInterEntidad, "REF_ti", c("r", "d"), c("kt", "ni")),
    .lmd_inst(nuc$ReglaIntegridadInterEntidad, "REF_vi", c("r", "d"), c("kv", "ni")),
    .lmd_inst(nuc$ReglaIntegridadInterEntidad, "REF_iv", c("r", "d"), c("ki", "vc"))
  ))
  tablas_id <- list(
    d = DBI::Id(schema = "dbo", table = "lupa_medir_ss_d"),
    r = DBI::Id(schema = "dbo", table = "lupa_medir_ss_r")
  )
  tablas_r <- list(
    d = DBI::dbReadTable(con, tablas_id$d),
    r = DBI::dbReadTable(con, tablas_id$r)
  )
  p <- plan_medicion_dbi(mod, con, tablas = tablas_id)
  expect_identical(attr(p, "motor", exact = TRUE), "sqlserver")
  expect_true(all(p$via %in% c("sql_consolidado", "sql_dedicado", "sql_dedicado_referencial")))

  for (apli in list(NULL, list(ni = ~ !is.na(tx) | ni > 0L, tx = ~ !(ni %in% c(-99L, NA))))) {
    esp <- suppressWarnings(medir(
      mod, tablas_r, id_medicion = "ss", fecha = fecha,
      aplicabilidad = apli, detalle = "resumen"
    ))
    obt <- suppressWarnings(medir_dbi(
      mod, con, tablas = tablas_id, id_medicion = "ss", fecha = fecha,
      aplicabilidad = apli
    ))
    expect_identical(nrow(obt), length(mod$metricas))
    .lmd_comparar_resumen(esp, obt, info = if (is.null(apli)) "ss_sin_apl" else "ss_con_apl")
  }
})

test_that("SQL Server aplica guardas de espacios finales, _BIN2, CHAR, DATETIMEOFFSET y centinela texto", {
  con <- .lmd_conectar_mssql()
  limpiar <- function() {
    try(DBI::dbExecute(con, "DROP TABLE lupa_medir_ss_g"), silent = TRUE)
    try(DBI::dbExecute(con, "DROP TABLE lupa_medir_ss_gr"), silent = TRUE)
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_medir_ss_g (",
    "ch CHAR(5) COLLATE Latin1_General_BIN2, ",
    "v_ci VARCHAR(20) COLLATE SQL_Latin1_General_CP1_CI_AS, ",
    "v_sp NVARCHAR(20) COLLATE Latin1_General_100_BIN2, ",
    "v_ok NVARCHAR(20) COLLATE Latin1_General_100_BIN2, ",
    "big BIGINT, dto DATETIMEOFFSET)"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_medir_ss_gr (k NVARCHAR(20) COLLATE Latin1_General_100_BIN2)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_medir_ss_g VALUES ",
    "('a', 'a', N'a', N'a', 1, '2026-01-01 12:00:00 +00:00'), ",
    "('a ', 'A', N'a ', N'b', 2, '2026-01-01 12:00:00 -03:00'), ",
    "(NULL, NULL, NULL, NULL, NULL, NULL)"
  ))
  DBI::dbExecute(con, "INSERT INTO lupa_medir_ss_gr VALUES (N'a'), (N'b ')")

  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  mod <- modelo(list(
    .lmd_inst(nuc$NoNulo, "NN_ch", "g", "ch"),
    .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_ch", "g", "ch", valores = c("a", "a    ")),
    .lmd_inst(nuc$NoNulo, "NN_ci", "g", "v_ci"),
    .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_ci", "g", "v_ci", valores = "a"),
    .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_sp_data", "g", "v_sp", valores = c("a", "b")),
    .lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_sp_dom", "g", "v_ok", valores = c("a ", "b")),
    .lmd_inst(nuc$AtributoDuplicado, "DUP_sp", "g", "v_sp"),
    .lmd_inst(nuc$NoNulo, "NN_cent_txt", "g", "v_ok", valores_nulos = "NA"),
    .lmd_inst(nuc$NoNulo, "NN_big", "g", "big"),
    .lmd_inst(nuc$NoNulo, "NN_dto", "g", "dto"),
    .lmd_inst(nuc$OportunidadAtributoPorFecha, "OP_dto", "g", "dto",
              fecha_limite = as.POSIXct("2026-01-01 13:00:00", tz = "UTC")),
    .lmd_inst(nuc$ReglaIntegridadInterEntidad, "REF_pk_sp", c("r", "g"), c("k", "v_ok"))
  ))
  tablas <- c(g = "lupa_medir_ss_g", r = "lupa_medir_ss_gr")
  r <- suppressWarnings(medir_dbi(mod, con, tablas = tablas, id_medicion = "ssg", fecha = fecha))
  cob <- attr(r, "cobertura_metricas", exact = TRUE)
  expect_identical(
    attr(r, "ejecucion_dbi", exact = TRUE)$vias[c("NN_cent_txt", "OP_dto")],
    c(NN_cent_txt = "sql_consolidado", OP_dto = "sql_consolidado")
  )
  .lmd_comparar_resumen(
    suppressWarnings(medir(
      mod,
      list(
        g = DBI::dbReadTable(con, "lupa_medir_ss_g"),
        r = DBI::dbReadTable(con, "lupa_medir_ss_gr")
      ),
      id_medicion = "ssg", fecha = fecha, detalle = "resumen"
    )),
    r,
    info = "ssg"
  )
  expect_true(all(cob$estado == "no_medible"))

  # Con estrategia_no_sql = "bloques", VE_sp_data se resuelve por bloques con 0 diferencias
  mod_b <- modelo(.lmd_inst(nuc$ValoresPosiblesPorExtension, "VE_sp_data", "g", "v_sp", valores = c("a", "b")))
  df_g <- list(g = DBI::dbReadTable(con, "lupa_medir_ss_g"))
  esp_b <- suppressWarnings(medir(mod_b, df_g, id_medicion = "ssb", fecha = fecha, detalle = "resumen"))
  obt_b <- suppressWarnings(medir_dbi(mod_b, con, tablas = c(g = "lupa_medir_ss_g"),
                                      id_medicion = "ssb", fecha = fecha,
                                      estrategia_no_sql = "bloques"))
  expect_identical(attr(obt_b, "ejecucion_dbi")$vias[["VE_sp_data"]], "bloques_r")
  .lmd_comparar_resumen(esp_b, obt_b, info = "ss_bloques_sp")
})
