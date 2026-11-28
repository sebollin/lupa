# SQL Server: una columna con enmascaramiento dinamico (`MASKED WITH`) para una
# credencial sin `UNMASK` no se perfila ni se mide como si su valor fuera el
# dato, este la tabla en la base actual o en otra (`DBI::Id(catalog = ...)`).
# Cada prueba se saltea sola sin SQL Server o sin permiso para crear bases.
testthat::skip_on_cran()

# `preparar(c)` recibe la lista con las conexiones del duenio (`dueno`, base
# actual; `otra`) y el rol, y arma lo que cada prueba necesita ademas de las dos
# tablas comunes; `limpieza` son las sentencias que lo deshacen.
.mascara_montar <- function(preparar = NULL, limpieza = character()) {
  con <- .dlot_mssql()
  base_actual <- as.character(DBI::dbGetQuery(con, "SELECT DB_NAME() AS d")$d)
  otra <- "lupa_mascara_otra"
  rol <- "lupa_mascara_lector"
  clave <- paste0("Lr3_", paste(sample(c(letters, LETTERS, 0:9), 16L, TRUE), collapse = ""))
  ejecutar <- function(c, sql) try(DBI::dbExecute(c, sql), silent = TRUE)
  tablas <- c("lupa_mascara_t1", "lupa_mascara_t2")
  limpiar <- function() {
    for (sql in limpieza) ejecutar(con, sql)
    for (t in tablas) {
      ejecutar(con, paste0("IF OBJECT_ID(N'dbo.", t, "', N'U') IS NOT NULL DROP TABLE dbo.", t))
    }
    ejecutar(con, paste0("IF DB_ID(N'", otra, "') IS NOT NULL BEGIN ALTER DATABASE ", otra,
                         " SET SINGLE_USER WITH ROLLBACK IMMEDIATE; DROP DATABASE ", otra, " END"))
    ejecutar(con, paste0("IF EXISTS (SELECT 1 FROM sys.database_principals WHERE name = N'",
                         rol, "') DROP USER ", rol))
    ejecutar(con, paste0("IF EXISTS (SELECT 1 FROM sys.server_principals WHERE name = N'",
                         rol, "') DROP LOGIN ", rol))
  }
  limpiar()
  armado <- tryCatch({
    otra_con <- NULL
    crear <- function(c, esquema_prefijo) for (t in tablas) {
      DBI::dbExecute(c, paste0(
        "CREATE TABLE dbo.", t, " (k INT, b VARCHAR(10) COLLATE Latin1_General_BIN2 ",
        "MASKED WITH (FUNCTION = 'default()'), c VARCHAR(10))"
      ))
      DBI::dbExecute(c, paste0(
        "INSERT INTO dbo.", t, " VALUES (1,'sec1','p'), (2,'sec2','p'), ",
        "(3,'sec3','q'), (4,'sec1','q')"
      ))
    }
    crear(con)
    DBI::dbExecute(con, paste0("CREATE DATABASE ", otra))
    otra_con <- DBI::dbConnect(
      odbc::odbc(), Driver = "FreeTDS",
      Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
      Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
      UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = Sys.getenv("LUPA_MSSQL_PWD"),
      Database = otra, TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
    )
    crear(otra_con)
    DBI::dbExecute(con, paste0("CREATE LOGIN ", rol, " WITH PASSWORD = N",
                               DBI::dbQuoteString(con, clave)))
    for (c in list(con, otra_con)) {
      DBI::dbExecute(c, paste0("CREATE USER ", rol, " FOR LOGIN ", rol))
      DBI::dbExecute(c, paste0("GRANT VIEW DEFINITION TO ", rol))
      for (t in tablas) DBI::dbExecute(c, paste0("GRANT SELECT ON dbo.", t, " TO ", rol))
    }
    if (!is.null(preparar)) {
      preparar(list(dueno = con, otra = otra_con, rol = rol, base = otra))
    }
    DBI::dbDisconnect(otra_con)
    lector <- DBI::dbConnect(
      odbc::odbc(), Driver = "FreeTDS",
      Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
      Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
      UID = rol, PWD = clave, Database = base_actual,
      TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
    )
    lector
  }, error = function(e) e)
  if (inherits(armado, "condition")) {
    limpiar()
    DBI::dbDisconnect(con)
    testthat::skip(paste("No se pudo armar la base de la prueba:", conditionMessage(armado)))
  }
  list(
    dueno = con, lector = armado, base = otra, tablas = tablas,
    id = function(base, t) if (base == "actual") t else
      DBI::Id(catalog = otra, schema = "dbo", table = t),
    limpiar = function() {
      try(DBI::dbDisconnect(armado), silent = TRUE)
      limpiar()
      try(DBI::dbDisconnect(con), silent = TRUE)
    }
  )
}

.mascara_perfil <- function(con, tabla) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, bloque_muestra = "solo_agregados", analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )))
}

test_that("perfilar_dbi: la columna enmascarada no publica la mascara, en la base actual y en otra", {
  m <- .mascara_montar()
  on.exit(m$limpiar(), add = TRUE)
  for (donde in c("actual", "otra")) {
    tabla <- m$id(donde, m$tablas[[1L]])
    p <- .mascara_perfil(m$lector, tabla)
    col <- p$resumen_tabla$columnas
    b <- col[col$columna == "b", , drop = FALSE]
    expect_true(is.na(b$n_distintos), info = donde)
    expect_true(is.na(b$moda), info = donde)
    cob <- p$resumen_tabla$cobertura
    expect_true(any(cob$elemento == "b" & cob$estado == "no_disponible"), info = donde)
    expect_true(grepl("enmascarada", cob$motivo[cob$elemento == "b"][[1L]], useBytes = TRUE),
                info = donde)
    # La columna sin mascara se perfila entera: igual que con el duenio.
    pd <- .mascara_perfil(m$dueno, tabla)
    cd <- pd$resumen_tabla$columnas
    columnas <- c("columna", "n", "n_faltantes", "n_distintos", "moda", "frecuencia_moda")
    expect_identical(col[col$columna == "c", columnas], cd[cd$columna == "c", columnas],
                     info = donde)
    # Y con permiso para ver el dato, la columna b se mide.
    expect_equal(cd$n_distintos[cd$columna == "b"], 3, info = donde)
    expect_identical(cd$moda[cd$columna == "b"], "sec1", info = donde)
  }
})

test_that("medir_dbi: una regla por bloques no mide la mascara de una columna enmascarada", {
  m <- .mascara_montar()
  on.exit(m$limpiar(), add = TRUE)
  nuc <- metricas_nucleo()
  mod <- modelo(
    instanciar(especializar(nuc$Formato, "Rf", expresion_regular = "^sec[0-9]$"), "E", "b"),
    instanciar(especializar(nuc$Formato, "Rc", expresion_regular = "^[pq]$"), "E", "c")
  )
  medir <- function(con, tabla) suppressWarnings(suppressMessages(medir_dbi(
    mod, con, tablas = list(E = tabla), id_medicion = "m",
    fecha = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    estrategia_no_sql = "bloques"
  )))
  for (donde in c("actual", "otra")) {
    tabla <- m$id(donde, m$tablas[[1L]])
    r_dueno <- medir(m$dueno, tabla)
    r_lector <- medir(m$lector, tabla)
    # El duenio lee el dato: las dos reglas dan 1.
    expect_identical(sort(r_dueno$metrica_instanciada), c("Rc@E.c", "Rf@E.b"), info = donde)
    expect_equal(r_dueno$resultado, c(1, 1), info = donde)
    # El lector mide la columna sin mascara, igual; la enmascarada no se mide
    # (con la mascara daria 0) y lo dice.
    expect_identical(r_lector$metrica_instanciada, "Rc@E.c", info = donde)
    expect_equal(r_lector$resultado, 1, info = donde)
    cob <- attr(r_lector, "cobertura_metricas", exact = TRUE)
    fila <- cob[cob$metrica_instanciada == "Rf@E.b", , drop = FALSE]
    expect_identical(fila$estado, "no_medible", info = donde)
    expect_true(grepl("enmascarada", fila$motivo, useBytes = TRUE), info = donde)
  }
})

test_that("medir_dbi: una regla que cuenta el motor sigue midiendose sobre una enmascarada", {
  m <- .mascara_montar()
  on.exit(m$limpiar(), add = TRUE)
  nuc <- metricas_nucleo()
  mod <- modelo(instanciar(especializar(nuc$NoNulo, "Rn"), "E", "b"))
  for (donde in c("actual", "otra")) {
    tabla <- m$id(donde, m$tablas[[1L]])
    medir <- function(con) suppressWarnings(suppressMessages(medir_dbi(
      mod, con, tablas = list(E = tabla), id_medicion = "m",
      fecha = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
    )))
    expect_identical(medir(m$lector)$resultado, medir(m$dueno)$resultado, info = donde)
  }
})

test_that("relaciones_coleccion: una columna enmascarada no se relaciona por su mascara", {
  m <- .mascara_montar()
  on.exit(m$limpiar(), add = TRUE)
  for (donde in c("actual", "otra")) {
    tablas <- list(m$id(donde, m$tablas[[1L]]), m$id(donde, m$tablas[[2L]]))
    nombres <- if (donde == "actual") m$tablas else
      paste0(m$base, ".dbo.", m$tablas)
    rel <- function(con) {
      r <- suppressWarnings(suppressMessages(relaciones_coleccion(
        coleccion(con, if (donde == "actual") unlist(tablas) else tablas),
        pares = data.frame(tabla_1 = nombres[[1L]], tabla_2 = nombres[[2L]])
      )))
      r$relaciones$columna_tabla1
    }
    expect_true("b" %in% rel(m$dueno), info = donde)
    expect_false("b" %in% rel(m$lector), info = donde)
    expect_true("c" %in% rel(m$lector), info = donde)
  }
})

# ---- El permiso efectivo, las vistas y el catalogo que no se ve ---------------

.mascara_datos <- function(tabla) paste0(
  "INSERT INTO ", tabla, " VALUES (1,'sec1','otro1'), (2,'sec2','otro2'), ",
  "(3,'sec3','otro3'), (4,'sec1','otro1')"
)

.mascara_medir_formato <- function(con, tabla, columna, expresion) {
  nuc <- metricas_nucleo()
  mod <- modelo(instanciar(
    especializar(nuc$Formato, "Rf", expresion_regular = expresion), "E", columna
  ))
  suppressWarnings(suppressMessages(medir_dbi(
    mod, con, tablas = list(E = tabla), id_medicion = "m",
    fecha = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    estrategia_no_sql = "bloques"
  )))
}

.mascara_declarada <- function(p, columna) {
  col <- p$resumen_tabla$columnas
  cob <- p$resumen_tabla$cobertura
  is.na(col$n_distintos[col$columna == columna]) &&
    any(cob$elemento == columna & cob$estado == "no_disponible")
}

test_that("un GRANT UNMASK de base no tapa un DENY UNMASK de columna ni de esquema", {
  preparar <- function(x) {
    for (c in list(x$dueno, x$otra)) {
      DBI::dbExecute(c, paste0(
        "CREATE TABLE dbo.lupa_mascara_d (k INT, b1 VARCHAR(10) COLLATE Latin1_General_BIN2 ",
        "MASKED WITH (FUNCTION = 'default()'), b2 VARCHAR(10) COLLATE Latin1_General_BIN2 ",
        "MASKED WITH (FUNCTION = 'default()'))"
      ))
      DBI::dbExecute(c, .mascara_datos("dbo.lupa_mascara_d"))
      DBI::dbExecute(c, paste0("GRANT SELECT ON dbo.lupa_mascara_d TO ", x$rol))
      DBI::dbExecute(c, paste0("GRANT UNMASK TO ", x$rol))
      DBI::dbExecute(c, paste0("DENY UNMASK ON OBJECT::dbo.lupa_mascara_d (b1) TO ", x$rol))
    }
    DBI::dbExecute(x$dueno, "CREATE SCHEMA lupa_mascara_sch")
    DBI::dbExecute(x$dueno, paste0(
      "CREATE TABLE lupa_mascara_sch.d (k INT, b VARCHAR(10) COLLATE Latin1_General_BIN2 ",
      "MASKED WITH (FUNCTION = 'default()'))"
    ))
    DBI::dbExecute(x$dueno, "INSERT INTO lupa_mascara_sch.d VALUES (1,'sec1'), (2,'sec2')")
    DBI::dbExecute(x$dueno, paste0("GRANT SELECT ON lupa_mascara_sch.d TO ", x$rol))
    DBI::dbExecute(x$dueno, paste0("GRANT UNMASK TO ", x$rol))
    DBI::dbExecute(x$dueno, paste0("DENY UNMASK ON SCHEMA::lupa_mascara_sch TO ", x$rol))
  }
  m <- .mascara_montar(preparar, c(
    "DROP TABLE lupa_mascara_sch.d", "DROP SCHEMA lupa_mascara_sch",
    "DROP TABLE dbo.lupa_mascara_d"
  ))
  on.exit(m$limpiar(), add = TRUE)
  for (donde in c("actual", "otra")) {
    tabla <- m$id(donde, "lupa_mascara_d")
    p <- .mascara_perfil(m$lector, tabla)
    # b1 tiene el DENY: llega la mascara y se declara; b2 se lee entera.
    expect_true(.mascara_declarada(p, "b1"), info = donde)
    col <- p$resumen_tabla$columnas
    expect_equal(col$n_distintos[col$columna == "b2"], 3, info = donde)
    r1 <- .mascara_medir_formato(m$lector, tabla, "b1", "^sec[0-9]$")
    expect_identical(nrow(r1), 0L, info = donde)
    r2 <- .mascara_medir_formato(m$lector, tabla, "b2", "^otro[0-9]$")
    expect_equal(r2$resultado, 1, info = donde)
  }
  esquema <- DBI::Id(schema = "lupa_mascara_sch", table = "d")
  expect_true(.mascara_declarada(.mascara_perfil(m$lector, esquema), "b"))
  expect_identical(nrow(.mascara_medir_formato(m$lector, esquema, "b", "^sec[0-9]$")), 0L)
})

test_that("una vista sobre una tabla enmascarada no publica la mascara como dato", {
  preparar <- function(x) {
    for (c in list(x$dueno, x$otra)) {
      DBI::dbExecute(c, paste0(
        "CREATE VIEW dbo.lupa_mascara_v AS SELECT k, b, c FROM dbo.lupa_mascara_t1"
      ))
      DBI::dbExecute(c, paste0("GRANT SELECT ON dbo.lupa_mascara_v TO ", x$rol))
    }
  }
  m <- .mascara_montar(preparar, "DROP VIEW dbo.lupa_mascara_v")
  on.exit(m$limpiar(), add = TRUE)
  for (donde in c("actual", "otra")) {
    vista <- m$id(donde, "lupa_mascara_v")
    p <- .mascara_perfil(m$lector, vista)
    expect_true(.mascara_declarada(p, "b"), info = donde)
    cob <- p$resumen_tabla$cobertura
    expect_true(grepl("enmascarada", cob$motivo[cob$elemento == "b"][[1L]], useBytes = TRUE),
                info = donde)
    # La columna de texto con origen visible y sin mascara se perfila entera.
    col <- p$resumen_tabla$columnas
    expect_equal(col$n_distintos[col$columna == "c"], 2, info = donde)
    r <- .mascara_medir_formato(m$lector, vista, "b", "^sec[0-9]$")
    expect_identical(nrow(r), 0L, info = donde)
    # El duenio lee el dato: la misma vista se perfila y se mide.
    expect_equal(.mascara_perfil(m$dueno, vista)$resumen_tabla$columnas$n_distintos[2L], 3,
                 info = donde)
    expect_equal(.mascara_medir_formato(m$dueno, vista, "b", "^sec[0-9]$")$resultado, 1,
                 info = donde)
  }
})

.mascara_declara_origen <- function(p) {
  cob <- p$resumen_tabla$cobertura
  fila <- cob[cob$estado == "degradado" &
                grepl("No se pudo comprobar", cob$motivo, useBytes = TRUE), , drop = FALSE]
  nrow(fila) == 1L && identical(p$resumen_tabla$meta$permisos_sin_consultar, "origen")
}

test_that("una vista cuyo origen no se ve se publica como siempre y declara que no se pudo comprobar", {
  preparar <- function(x) {
    DBI::dbExecute(x$dueno, "CREATE VIEW dbo.lupa_mascara_v AS SELECT k, b, c FROM dbo.lupa_mascara_t2")
    DBI::dbExecute(x$dueno, paste0("GRANT SELECT ON dbo.lupa_mascara_v TO ", x$rol))
    DBI::dbExecute(x$dueno, paste0("REVOKE SELECT ON dbo.lupa_mascara_t2 FROM ", x$rol))
    DBI::dbExecute(x$dueno, paste0("REVOKE VIEW DEFINITION FROM ", x$rol))
  }
  m <- .mascara_montar(preparar, "DROP VIEW dbo.lupa_mascara_v")
  on.exit(m$limpiar(), add = TRUE)
  vista <- "lupa_mascara_v"
  p <- .mascara_perfil(m$lector, vista)
  # No se sabe si el origen esta enmascarado: las cifras son las de siempre y
  # una sola declaracion sobre la tabla lo dice.
  expect_true(.mascara_declara_origen(p))
  col <- p$resumen_tabla$columnas
  expect_equal(col$n_distintos[col$columna == "k"], 4)
  expect_false(.mascara_declarada(p, "b"))
  r <- .mascara_medir_formato(m$lector, vista, "c", "^[pq]$")
  expect_equal(r$resultado, 1)
  declaracion <- attr(r, "mascara_sin_comprobar", exact = TRUE)
  expect_identical(declaracion$entidad, "E")
  expect_true(grepl("pueden ser las de la m", declaracion$motivo, useBytes = TRUE))
})

test_that("una vista sobre tablas sin mascara con un lector restringido da las cifras del duenio", {
  preparar <- function(x) {
    DBI::dbExecute(x$dueno, "CREATE TABLE dbo.lupa_mascara_n (k INT, n INT, c VARCHAR(10) COLLATE Latin1_General_BIN2)")
    DBI::dbExecute(x$dueno, "INSERT INTO dbo.lupa_mascara_n VALUES (1,10,'sec1'),(2,20,'sec2'),(3,30,'sec3'),(4,40,'sec1')")
    DBI::dbExecute(x$dueno, "CREATE VIEW dbo.lupa_mascara_vn AS SELECT k, n, c FROM dbo.lupa_mascara_n")
    DBI::dbExecute(x$dueno, paste0("GRANT SELECT ON dbo.lupa_mascara_vn TO ", x$rol))
    DBI::dbExecute(x$dueno, paste0("REVOKE VIEW DEFINITION FROM ", x$rol))
  }
  m <- .mascara_montar(preparar, c("DROP VIEW dbo.lupa_mascara_vn", "DROP TABLE dbo.lupa_mascara_n"))
  on.exit(m$limpiar(), add = TRUE)
  vista <- "lupa_mascara_vn"
  pl <- .mascara_perfil(m$lector, vista)
  pd <- .mascara_perfil(m$dueno, vista)
  columnas <- c("columna", "n", "n_faltantes", "n_distintos", "moda", "frecuencia_moda",
                "minimo", "maximo", "media")
  expect_identical(pl$resumen_tabla$columnas[, columnas], pd$resumen_tabla$columnas[, columnas])
  expect_true(.mascara_declara_origen(pl))
  expect_false(.mascara_declara_origen(pd))
  rl <- .mascara_medir_formato(m$lector, vista, "c", "^sec[0-9]$")
  rd <- .mascara_medir_formato(m$dueno, vista, "c", "^sec[0-9]$")
  expect_identical(rl$resultado, rd$resultado)
  expect_identical(nrow(rl), 1L)
  expect_identical(attr(rl, "mascara_sin_comprobar", exact = TRUE)$entidad, "E")
  expect_null(attr(rd, "mascara_sin_comprobar", exact = TRUE))
})

test_that("un sinonimo o un catalogo que no se ve se mide como siempre y se declara", {
  preparar <- function(x) {
    DBI::dbExecute(x$dueno, "CREATE SYNONYM dbo.lupa_mascara_s FOR dbo.lupa_mascara_t1")
    DBI::dbExecute(x$dueno, paste0("GRANT SELECT ON dbo.lupa_mascara_s TO ", x$rol))
    DBI::dbExecute(x$dueno, paste0("DENY VIEW DEFINITION ON dbo.lupa_mascara_t2 TO ", x$rol))
  }
  m <- .mascara_montar(preparar, "DROP SYNONYM dbo.lupa_mascara_s")
  on.exit(m$limpiar(), add = TRUE)
  for (tabla in c("lupa_mascara_s", "lupa_mascara_t2")) {
    r <- .mascara_medir_formato(m$lector, tabla, "c", "^[pq]$")
    expect_identical(nrow(r), 1L, info = tabla)
    expect_equal(r$resultado, 1, info = tabla)
    expect_identical(attr(r, "mascara_sin_comprobar", exact = TRUE)$entidad, "E", info = tabla)
    p <- .mascara_perfil(m$lector, tabla)
    expect_true(any(p$resumen_tabla$cobertura$estado == "degradado"), info = tabla)
    expect_false(is.na(p$resumen_tabla$meta$permisos_sin_consultar), info = tabla)
  }
})

test_that("con UNMASK efectivo en todas las columnas no hay nada que declarar aunque el catalogo no se vea", {
  preparar <- function(x) {
    DBI::dbExecute(x$dueno, paste0("GRANT UNMASK TO ", x$rol))
    DBI::dbExecute(x$dueno, paste0("DENY VIEW DEFINITION ON dbo.lupa_mascara_t2 TO ", x$rol))
  }
  m <- .mascara_montar(preparar)
  on.exit(m$limpiar(), add = TRUE)
  r <- .mascara_medir_formato(m$lector, "lupa_mascara_t2", "b", "^sec[0-9]$")
  expect_equal(r$resultado, 1)
  expect_null(attr(r, "mascara_sin_comprobar", exact = TRUE))
  p <- .mascara_perfil(m$lector, "lupa_mascara_t2")
  expect_null(p$resumen_tabla$meta$permisos_sin_consultar)
  col <- p$resumen_tabla$columnas
  expect_equal(col$n_distintos[col$columna == "b"], 3)
})

test_that("el atajo del UNMASK efectivo cita el nombre de la columna: un corchete no lo desactiva", {
  preparar <- function(x) {
    for (t in c("lupa_mascara_x", "lupa_mascara_y")) {
      DBI::dbExecute(x$dueno, paste0(
        "CREATE TABLE dbo.", t, " (z VARCHAR(10) COLLATE Latin1_General_BIN2, ",
        "[c]]d] VARCHAR(10) COLLATE Latin1_General_BIN2 MASKED WITH (FUNCTION = 'default()'))"
      ))
      DBI::dbExecute(x$dueno, paste0("INSERT INTO dbo.", t, " VALUES ('sec1','a'), ('sec2','b')"))
      DBI::dbExecute(x$dueno, paste0("GRANT SELECT ON dbo.", t, " TO ", x$rol))
      DBI::dbExecute(x$dueno, paste0("DENY VIEW DEFINITION ON dbo.", t, " TO ", x$rol))
    }
    DBI::dbExecute(x$dueno, paste0("GRANT UNMASK TO ", x$rol))
    # Con el DENY sobre la columna, el dato llega enmascarado aunque el UNMASK
    # de la base este concedido; sin el DENY (tabla y), llega entero.
    DBI::dbExecute(x$dueno, paste0(
      "DENY UNMASK ON OBJECT::dbo.lupa_mascara_x ([c]]d]) TO ", x$rol
    ))
  }
  m <- .mascara_montar(preparar, c("DROP TABLE dbo.lupa_mascara_x", "DROP TABLE dbo.lupa_mascara_y"))
  on.exit(m$limpiar(), add = TRUE)
  leido <- DBI::dbGetQuery(m$lector, "SELECT [c]]d] AS v FROM dbo.lupa_mascara_x")$v
  expect_true(all(leido == "xxxx"))
  p <- .mascara_perfil(m$lector, "lupa_mascara_x")
  expect_false(is.null(p$resumen_tabla$meta$permisos_sin_consultar))
  r <- .mascara_medir_formato(m$lector, "lupa_mascara_x", "z", "^sec[0-9]$")
  expect_identical(attr(r, "mascara_sin_comprobar", exact = TRUE)$entidad, "E")
  # Sin el DENY, el UNMASK efectivo de todas las columnas es 1: nada que declarar.
  p_y <- .mascara_perfil(m$lector, "lupa_mascara_y")
  expect_null(p_y$resumen_tabla$meta$permisos_sin_consultar)
  r_y <- .mascara_medir_formato(m$lector, "lupa_mascara_y", "z", "^sec[0-9]$")
  expect_null(attr(r_y, "mascara_sin_comprobar", exact = TRUE))
})
