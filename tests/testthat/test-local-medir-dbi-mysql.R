# Las pruebas de MySQL y MariaDB necesitan RMySQL y RMariaDB, que el paquete no
# declara: viven en un archivo local (`test-local-*`, fuera del tarball), como las
# demas pruebas de esos motores, y se saltean solas sin el motor.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.lmd_conectar_mysql <- function(maria = FALSE) {
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

.lmd_comparar_my <- function(esp, obt, info = "") {
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

.lmd_inst_my <- function(met, nom, ent, atr, ...) {
  instanciar(especializar(met, nom, ...), ent, atr, nombre_instancia = nom)
}

test_that("MySQL reproduce memoria con utf8mb4_0900_bin NO PAD y aplica guardas PAD SPACE y RMySQL", {
  con <- .lmd_conectar_mysql(maria = FALSE)
  limpiar <- function() {
    try(DBI::dbExecute(con, "SET time_zone = '+00:00'"), silent = TRUE)
    for (tb in c("lupa_medir_my_d", "lupa_medir_my_r", "lupa_medir_my_g", "lupa_medir_my_gr")) {
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tb)), silent = TRUE)
    }
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_medir_my_d (",
    "ni INT, big BIGINT, nd DOUBLE, dc DECIMAL(20,2), ",
    "tx VARCHAR(40) COLLATE utf8mb4_0900_bin, fe DATE",
    ") CHARACTER SET utf8mb4"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_medir_my_r (",
    "ki INT, kd DECIMAL(20,2), kt VARCHAR(40) COLLATE utf8mb4_0900_bin",
    ") CHARACTER SET utf8mb4"
  ))
  q_idot <- as.character(DBI::dbQuoteString(con, intToUtf8(0x0130L)))
  q_kelv <- as.character(DBI::dbQuoteString(con, intToUtf8(0x212AL)))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_medir_my_d VALUES ",
    "(1, 9007199254740992, 0.3, 0.30, 'a', '2026-01-01'), ",
    "(2, 9007199254740993, 0.1, 9007199254740992, 'a ', '1960-06-15'), ",
    "(2, 1, 0.3, 9007199254740993, ' \tNA\r\n ', '2026-01-01'), ",
    "(-99, 2, 1.5, 0.10, 'sin\tdato', '2040-03-15'), ",
    "(5, 3, 2.0, 1.25, ", q_idot, ", '2025-05-10'), ",
    "(1, 4, 2.0, 1.25, ", q_kelv, ", NULL), ",
    "(NULL, NULL, NULL, NULL, NULL, NULL)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_medir_my_r VALUES ",
    "(1, 0.30, 'a'), (2, 9007199254740992, '1'), (NULL, NULL, NULL)"
  ))

  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  mod <- modelo(list(
    .lmd_inst_my(nuc$NoNulo, "NN_int", "d", "ni", valores_nulos = -99L),
    .lmd_inst_my(nuc$NoNulo, "NN_txt_cent", "d", "tx",
                 valores_nulos = c("NA", "sin dato", "i")),
    .lmd_inst_my(nuc$NoNulo, "NN_fe", "d", "fe"),
    .lmd_inst_my(nuc$ValoresPosiblesPorExtension, "VE_big", "d", "big",
                 valores = 9007199254740992),
    .lmd_inst_my(nuc$ValoresPosiblesPorExtension, "VE_dec", "d", "dc",
                 valores = c(0.3, 9007199254740992)),
    .lmd_inst_my(nuc$ValoresPosiblesPorExtension, "VE_txt", "d", "tx",
                 valores = c("a", "a ")),
    .lmd_inst_my(nuc$ValoresPosiblesPorComprension, "RC_int", "d", "ni",
                 minimo = 1L, maximo = 5L),
    .lmd_inst_my(nuc$AtributoDuplicado, "DUP_txt", "d", "tx"),
    .lmd_inst_my(nuc$ConjuntoAtributosDuplicado, "CDUP_it", "d", c("ni", "tx")),
    .lmd_inst_my(nuc$ReglaIntegridadIntraEntidad, "F_in", "d", c("ni", "tx"),
                 regla = ~ (ni %in% c(1L, 2L, NA)) | !(tx %in% c("a ", NA))),
    .lmd_inst_my(nuc$ReglaIntegridadInterEntidad, "REF_dec", c("r", "d"), c("kd", "dc")),
    .lmd_inst_my(nuc$ReglaIntegridadInterEntidad, "REF_txt", c("r", "d"), c("kt", "tx")),
    .lmd_inst_my(nuc$ReglaIntegridadInterEntidad, "REF_ti", c("r", "d"), c("kt", "ni"))
  ))
  tablas <- c(d = "lupa_medir_my_d", r = "lupa_medir_my_r")
  tablas_r <- list(
    d = suppressWarnings(DBI::dbReadTable(con, "lupa_medir_my_d")),
    r = suppressWarnings(DBI::dbReadTable(con, "lupa_medir_my_r"))
  )
  p <- plan_medicion_dbi(mod, con, tablas = tablas)
  expect_identical(attr(p, "motor", exact = TRUE), "mysql")
  expect_true(all(p$via %in% c("sql_consolidado", "sql_dedicado", "sql_dedicado_referencial")))

  for (apli in list(NULL, list(ni = ~ !is.na(tx) | ni > 0L, tx = ~ !(ni %in% c(-99L, NA))))) {
    esp <- suppressWarnings(medir(
      mod, tablas_r, id_medicion = "my", fecha = fecha,
      aplicabilidad = apli, detalle = "resumen"
    ))
    obt <- suppressWarnings(medir_dbi(
      mod, con, tablas = tablas, id_medicion = "my", fecha = fecha,
      aplicabilidad = apli
    ))
    expect_identical(nrow(obt), length(mod$metricas))
    .lmd_comparar_my(esp, obt, info = if (is.null(apli)) "my_sin_apl" else "my_con_apl")
  }

  # Guardas de PAD SPACE, CI, FLOAT/BIT bajo RMySQL, conversion implicita y bloques
  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_medir_my_g (",
    "n INT, v_ci VARCHAR(20) COLLATE utf8mb4_0900_ai_ci, ",
    "v_pad VARCHAR(20) COLLATE utf8mb4_bin, ",
    "v_pad_ok VARCHAR(20) COLLATE utf8mb4_bin, ",
    "fl FLOAT, b BIT(1)) CHARACTER SET utf8mb4"
  ))
  DBI::dbExecute(con, "CREATE TABLE lupa_medir_my_gr (k VARCHAR(20) COLLATE utf8mb4_bin) CHARACTER SET utf8mb4")
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_medir_my_g VALUES ",
    "(1, 'a', 'a', 'a', 0.3, b'1'), ",
    "(2, 'A', 'a ', 'b', 0.1, b'0'), ",
    "(NULL, NULL, NULL, NULL, NULL, NULL)"
  ))
  DBI::dbExecute(con, "INSERT INTO lupa_medir_my_gr VALUES ('a'), ('b ')")

  mod_g <- modelo(list(
    .lmd_inst_my(nuc$NoNulo, "NN_ci", "g", "v_ci"),
    .lmd_inst_my(nuc$ValoresPosiblesPorExtension, "VE_ci", "g", "v_ci", valores = "a"),
    .lmd_inst_my(nuc$ValoresPosiblesPorExtension, "VE_pad_ok", "g", "v_pad_ok", valores = c("a", "b")),
    .lmd_inst_my(nuc$ValoresPosiblesPorExtension, "VE_pad_sp", "g", "v_pad", valores = c("a", "b")),
    .lmd_inst_my(nuc$ValoresPosiblesPorExtension, "VE_pad_dom", "g", "v_pad_ok", valores = c("a ", "b")),
    .lmd_inst_my(nuc$NoNulo, "NN_pad_cent", "g", "v_pad_ok", valores_nulos = "NA"),
    .lmd_inst_my(nuc$ValoresPosiblesPorExtension, "VE_fl", "g", "fl", valores = c(0.3, 0.1)),
    .lmd_inst_my(nuc$NoNulo, "NN_bit", "g", "b"),
    .lmd_inst_my(nuc$ValoresPosiblesPorExtension, "VE_imp", "g", "n", valores = c("1", "2")),
    .lmd_inst_my(nuc$ReglaIntegridadInterEntidad, "REF_pk_sp", c("r", "g"), c("k", "v_pad_ok"))
  ))
  rg <- suppressWarnings(medir_dbi(
    mod_g, con, tablas = c(g = "lupa_medir_my_g", r = "lupa_medir_my_gr"),
    id_medicion = "myg", fecha = fecha
  ))
  cob_g <- attr(rg, "cobertura_metricas", exact = TRUE)
  expect_setequal(rg$metrica_instanciada, c("NN_ci", "VE_pad_ok"))
  expect_true(all(c("VE_ci", "VE_pad_sp", "VE_pad_dom", "NN_pad_cent",
                    "VE_fl", "NN_bit", "VE_imp", "REF_pk_sp") %in% cob_g$metrica_instanciada))
  expect_true(all(cob_g$estado == "no_medible"))

  mod_b <- modelo(.lmd_inst_my(nuc$ValoresPosiblesPorExtension, "VE_pad_sp", "g", "v_pad", valores = c("a", "b")))
  df_g <- list(g = suppressWarnings(DBI::dbReadTable(con, "lupa_medir_my_g")))
  esp_b <- suppressWarnings(medir(mod_b, df_g, id_medicion = "myb", fecha = fecha, detalle = "resumen"))
  obt_b <- suppressWarnings(medir_dbi(mod_b, con, tablas = c(g = "lupa_medir_my_g"),
                                      id_medicion = "myb", fecha = fecha,
                                      estrategia_no_sql = "bloques"))
  expect_identical(attr(obt_b, "ejecucion_dbi")$vias[["VE_pad_sp"]], "bloques_r")
  .lmd_comparar_my(esp_b, obt_b, info = "my_bloques")
})

test_that("MariaDB reproduce memoria con utf8mb4_nopad_bin, fechas, FLOAT, BIT y guarda de huso", {
  con <- .lmd_conectar_mysql(maria = TRUE)
  limpiar <- function() {
    try(DBI::dbExecute(con, "SET time_zone = '+00:00'"), silent = TRUE)
    for (tb in c("lupa_medir_ma_d", "lupa_medir_ma_r", "lupa_medir_ma_g")) {
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tb)), silent = TRUE)
    }
  }
  on.exit({ limpiar(); try(DBI::dbDisconnect(con), silent = TRUE) }, add = TRUE)
  limpiar()

  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_medir_ma_d (",
    "ni INT, nd DOUBLE, fl FLOAT, dc DECIMAL(20,2), ",
    "tx VARCHAR(40) COLLATE utf8mb4_nopad_bin, lg BIT(1), ",
    "fe DATE, fe2 DATE, dt DATETIME(3), ts TIMESTAMP NULL",
    ") CHARACTER SET utf8mb4"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_medir_ma_r (",
    "ki INT, kd DECIMAL(20,2), kt VARCHAR(40) COLLATE utf8mb4_nopad_bin, kf DATE",
    ") CHARACTER SET utf8mb4"
  ))
  q_idot <- as.character(DBI::dbQuoteString(con, intToUtf8(0x0130L)))
  q_kelv <- as.character(DBI::dbQuoteString(con, intToUtf8(0x212AL)))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_medir_ma_d VALUES ",
    "(1, 0.3, 0.3, 0.30, 'a', b'1', '2026-01-01', '2026-06-01', '2026-01-01 12:00:00.000', '2026-01-01 12:00:00'), ",
    "(2, 0.1, 0.1, 9007199254740992, 'a ', b'0', '1960-06-15', '2025-05-10', '2026-01-01 12:00:00.500', '2020-06-15 00:00:00'), ",
    "(2, 0.3, 0.5, 9007199254740993, ' \tNA\r\n ', b'1', '2026-01-01', '2026-01-01', '1960-06-15 00:00:00.000', '2026-01-01 12:00:00'), ",
    "(-99, 1.5, 0.3, 0.10, 'sin\tdato', NULL, '2040-03-15', '2026-06-01', '2040-03-15 08:30:00.000', '2030-03-15 08:30:00'), ",
    "(5, 2.0, 0.5, 1.25, ", q_idot, ", b'1', '2025-05-10', '2025-05-10', '2025-05-10 00:00:00.000', '2025-05-10 00:00:00'), ",
    "(1, 2.0, 0.5, 1.25, ", q_kelv, ", b'0', NULL, NULL, NULL, NULL), ",
    "(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_medir_ma_r VALUES ",
    "(1, 0.30, 'a', '2026-01-01'), (2, 9007199254740992, '1', '1960-06-15'), (NULL, NULL, NULL, NULL)"
  ))

  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  mod <- modelo(list(
    .lmd_inst_my(nuc$NoNulo, "NN_int", "d", "ni", valores_nulos = -99L),
    .lmd_inst_my(nuc$NoNulo, "NN_txt_cent", "d", "tx",
                 valores_nulos = c("NA", "sin dato", "i")),
    .lmd_inst_my(nuc$ValoresPosiblesPorExtension, "VE_dec", "d", "dc",
                 valores = c(0.3, 9007199254740992)),
    .lmd_inst_my(nuc$ValoresPosiblesPorExtension, "VE_fl", "d", "fl", valores = c(0.3, 0.5)),
    .lmd_inst_my(nuc$ValoresPosiblesPorExtension, "VE_txt", "d", "tx", valores = c("a", "a ")),
    .lmd_inst_my(nuc$Formato, "FMT_log", "d", "lg", diccionario = c(TRUE)),
    .lmd_inst_my(nuc$ValoresPosiblesPorComprension, "RC_fe", "d", "fe",
                 minimo = as.Date("2020-01-01"), maximo = as.Date("2030-01-01")),
    .lmd_inst_my(nuc$OportunidadAtributoPorFecha, "OP_dt", "d", "dt",
                 fecha_limite = as.POSIXct("2026-01-01 12:00:00", tz = "UTC")),
    .lmd_inst_my(nuc$OportunidadAtributoPorFecha, "OP_ts", "d", "ts",
                 fecha_limite = as.POSIXct("2026-01-01 12:00:00", tz = "UTC")),
    .lmd_inst_my(nuc$OportunidadEntPorIntervalo, "OPE_fe", "d", character(),
                 vigencia = vigencia(columna_actualizacion = "fe",
                                     inicio_intervalo = as.Date("1960-01-01"),
                                     fin_intervalo = as.Date("2026-06-01"))),
    .lmd_inst_my(nuc$AtributoDuplicado, "DUP_txt", "d", "tx"),
    .lmd_inst_my(nuc$ConjuntoAtributosDuplicado, "CDUP_it", "d", c("ni", "tx")),
    .lmd_inst_my(nuc$ReglaIntegridadIntraEntidad, "F_ord", "d", c("fe", "fe2"),
                 regla = ~ is.na(fe) | is.na(fe2) | fe <= fe2),
    .lmd_inst_my(nuc$ReglaIntegridadIntraEntidad, "F_log", "d", c("lg", "ni"),
                 regla = ~ is.na(lg) | lg | (ni %in% c(2L, -99L))),
    .lmd_inst_my(nuc$ReglaIntegridadInterEntidad, "REF_dec", c("r", "d"), c("kd", "dc")),
    .lmd_inst_my(nuc$ReglaIntegridadInterEntidad, "REF_txt", c("r", "d"), c("kt", "tx")),
    .lmd_inst_my(nuc$ReglaIntegridadInterEntidad, "REF_ti", c("r", "d"), c("kt", "ni"))
  ))
  tablas <- c(d = "lupa_medir_ma_d", r = "lupa_medir_ma_r")
  tablas_r <- list(
    d = DBI::dbReadTable(con, "lupa_medir_ma_d"),
    r = DBI::dbReadTable(con, "lupa_medir_ma_r")
  )
  p <- plan_medicion_dbi(mod, con, tablas = tablas)
  expect_identical(attr(p, "motor", exact = TRUE), "mariadb")
  expect_true(all(p$via %in% c("sql_consolidado", "sql_dedicado", "sql_dedicado_referencial")))

  for (apli in list(NULL, list(ni = ~ !is.na(tx) | ni > 0L, tx = ~ !(ni %in% c(-99L, NA))))) {
    esp <- suppressWarnings(medir(
      mod, tablas_r, id_medicion = "ma", fecha = fecha,
      aplicabilidad = apli, detalle = "resumen"
    ))
    obt <- suppressWarnings(medir_dbi(
      mod, con, tablas = tablas, id_medicion = "ma", fecha = fecha,
      aplicabilidad = apli
    ))
    expect_identical(nrow(obt), length(mod$metricas))
    .lmd_comparar_my(esp, obt, info = if (is.null(apli)) "ma_sin_apl" else "ma_con_apl")
  }

  # Huso no UTC: TIMESTAMP pasa a no_medible y DATETIME sigue en SQL
  DBI::dbExecute(con, "SET time_zone = '-03:00'")
  p_m3 <- plan_medicion_dbi(mod, con, tablas = tablas)
  vias_m3 <- stats::setNames(p_m3$via, p_m3$metrica_instanciada)
  expect_identical(vias_m3[["OP_ts"]], "no_medible")
  expect_identical(vias_m3[["OP_dt"]], "sql_consolidado")
  DBI::dbExecute(con, "SET time_zone = '+00:00'")

  # Guardas BIGINT (integer64) y PAD SPACE (utf8mb4_bin)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_medir_ma_g (big BIGINT, v_pad VARCHAR(20) COLLATE utf8mb4_bin) CHARACTER SET utf8mb4"
  ))
  DBI::dbExecute(con, "INSERT INTO lupa_medir_ma_g VALUES (1, 'a'), (2, 'a ')")
  mod_g <- modelo(list(
    .lmd_inst_my(nuc$NoNulo, "NN_big", "g", "big"),
    .lmd_inst_my(nuc$ValoresPosiblesPorExtension, "VE_pad", "g", "v_pad", valores = c("a", "b"))
  ))
  rg <- suppressWarnings(medir_dbi(mod_g, con, tablas = c(g = "lupa_medir_ma_g"),
                                   id_medicion = "mag", fecha = fecha))
  expect_identical(nrow(rg), 0L)
  expect_true(all(attr(rg, "cobertura_metricas")$estado == "no_medible"))
})

test_that("MariaDB no compara un entero sin signo que R lee desbordado, y cuenta sus presentes", {
  con <- .lmd_conectar_mysql(maria = TRUE)
  on.exit({
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_medir_unsigned"), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_medir_unsigned")
  DBI::dbExecute(con, "CREATE TABLE lupa_medir_unsigned (x INT UNSIGNED)")
  DBI::dbExecute(con, "INSERT INTO lupa_medir_unsigned VALUES (4294967295), (1), (NULL)")
  nuc <- metricas_nucleo()
  mod <- modelo(
    instanciar(especializar(nuc$ValoresPosiblesPorExtension, "VE_u",
                            valores = c(1, 4294967295)),
               "d", "x", nombre_instancia = "VE_u"),
    instanciar(especializar(nuc$NoNulo, "NN_u"), "d", "x",
               nombre_instancia = "NN_u")
  )
  r <- suppressWarnings(medir_dbi(mod, con, tablas = c(d = "lupa_medir_unsigned")))
  cob <- attr(r, "cobertura_metricas")
  fila_ve <- cob[cob$metrica_instanciada == "VE_u", , drop = FALSE]
  expect_equal(nrow(fila_ve), 1L)
  expect_identical(as.character(fila_ve$estado), "no_medible")
  expect_true(grepl("sin signo", fila_ve$motivo, fixed = TRUE))
  nn <- r[r$metrica_instanciada == "NN_u", , drop = FALSE]
  expect_equal(nrow(nn), 1L)
  expect_equal(as.numeric(nn$numerador), 2)
  expect_equal(as.numeric(nn$denominador), 3)
})
