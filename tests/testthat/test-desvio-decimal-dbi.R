# El desvio de un decimal no se publica `calculado` lejos del exacto. Tres
# construcciones lo hacian: en SQL Server la resta centrada en `decimal(38, s)`
# redondeaba el empate hacia lados distintos; por bloques y por la muestra del
# motor el desvio salia de los dobles de valores con mas cifras de las que un
# doble guarda; y en SQLite la muestra sumaba los cuadrados alrededor de una
# media redondeada. Las referencias son exactas: raiz de la varianza de los
# valores escritos, no `sd()` sobre dobles.

.ddd_mssql <- function(base = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas")) {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
    Database = base, TDS_Version = "8.0", TrustServerCertificate = "yes",
    timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  con
}

.ddd_pg <- function() {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("LUPA_PG_PORT", "55432")),
    user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(con), "PostgreSQL no disponible.")
  con
}

.ddd_desvio <- function(perfil, columna) {
  sql <- perfil$resumen_tabla$sql
  fila <- sql[sql$columna == columna & sql$metrica == "desvio", , drop = FALSE]
  columnas <- perfil$resumen_tabla$columnas
  list(
    estado = unique(as.character(fila$estado)),
    motivo = unique(as.character(fila$motivo)),
    valor = as.numeric(columnas$desvio[columnas$columna == columna])
  )
}

.ddd_perfil <- function(con, tabla, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE, ...
  )))
}

test_that("SQL Server no redondea hacia lados distintos la resta centrada de un decimal(38, s)", {
  con <- .ddd_mssql()
  on.exit({
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS ddd_empate"), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  try(DBI::dbExecute(con, "DROP TABLE IF EXISTS ddd_empate"), silent = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE ddd_empate (a DECIMAL(38,0), b DECIMAL(38,2),",
    "c DECIMAL(38,30), d DECIMAL(18,2))"
  ))
  # Medias 2.5, 0.025 y 2.5e-30: el centro termina en 5 y la resta la recorta.
  DBI::dbExecute(con, paste(
    "INSERT INTO ddd_empate VALUES",
    "(1, 0.01, 0.000000000000000000000000000001, 1.25),",
    "(2, 0.02, 0.000000000000000000000000000002, 2.50),",
    "(3, 0.03, 0.000000000000000000000000000003, 3.75),",
    "(4, 0.04, 0.000000000000000000000000000004, 4.00)"
  ))
  perfil <- .ddd_perfil(con, "ddd_empate")
  # sd(1, 2, 3, 4) = sqrt(5 / 3); las otras son la misma por su unidad.
  exacto <- sqrt(5 / 3)
  esperado <- c(a = exacto, b = exacto / 100, c = exacto * 1e-30,
                # 1.25, 2.50, 3.75, 4.00: suma de cuadrados 4.8125.
                d = sqrt(4.8125 / 3))
  for (columna in names(esperado)) {
    desvio <- .ddd_desvio(perfil, columna)
    expect_identical(desvio$estado, "calculado", info = columna)
    expect_equal(desvio$valor, esperado[[columna]], tolerance = 1e-12,
                 info = columna)
  }
})

test_that("el desvio de un decimal que el doble no guarda no sale calculado de los dobles", {
  con <- .ddd_pg()
  on.exit({
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS ddd_ancho"), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  try(DBI::dbExecute(con, "DROP TABLE IF EXISTS ddd_ancho"), silent = TRUE)
  DBI::dbExecute(con, "CREATE TABLE ddd_ancho (v NUMERIC(18,2))")
  textos <- c("500000000000006.60", "500000000000002.90", "500000000000004.22",
              "500000000000007.00", "500000000000004.58", "500000000000000.09",
              "500000000000001.31", "500000000000008.77", "500000000000003.43")
  DBI::dbExecute(con, paste0(
    "INSERT INTO ddd_ancho VALUES ", paste0("(", textos, ")", collapse = ", ")
  ))
  # El desvio exacto, en centavos enteros para no pasar por dobles.
  centavos <- as.numeric(sub(".", "", sub("^500000000000", "", textos),
                             fixed = TRUE))
  exacto <- stats::sd(centavos) / 100
  # Por la tabla completa el motor resta la media en `numeric`: sale bien.
  completa <- .ddd_desvio(.ddd_perfil(con, "ddd_ancho"), "v")
  expect_identical(completa$estado, "calculado")
  expect_equal(completa$valor, exacto, tolerance = 1e-12)
  # Por bloques, de los dobles, daria 3,1115 contra 3,1047: no sale de ahi. El
  # motor resta a cada valor un centro en `numeric` y suma las diferencias.
  bloques <- .ddd_desvio(.ddd_perfil(con, "ddd_ancho", bloque_filas = 3L), "v")
  expect_identical(bloques$estado, "calculado")
  expect_equal(bloques$valor, exacto, tolerance = 1e-12)
  # Control: un decimal chico por bloques se sigue publicando.
  DBI::dbExecute(con, "DELETE FROM ddd_ancho")
  DBI::dbExecute(con, "INSERT INTO ddd_ancho VALUES (1.25), (2.50), (3.75), (4.00)")
  chico <- .ddd_desvio(.ddd_perfil(con, "ddd_ancho", bloque_filas = 3L), "v")
  expect_identical(chico$estado, "calculado")
  expect_equal(chico$valor, sqrt(4.8125 / 3), tolerance = 1e-12)
})

test_that("SQLite por la muestra del motor centra el desvio sin perder las cifras", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE ddd_muestra (a INTEGER, b INTEGER)")
  # a: 2^53 - 2 y 2^53 - 1, desvio 0,7071 (salia 1). b: 5e14 + k, con
  # cancelacion (salia a 1e-5 relativo).
  DBI::dbExecute(con, paste(
    "INSERT INTO ddd_muestra VALUES",
    "(9007199254740990, 500000000000001), (9007199254740991, 500000000000007),",
    "(NULL, 500000000000003), (NULL, 500000000000009)"
  ))
  perfil <- .ddd_perfil(con, "ddd_muestra", universo = "muestra_motor",
                        muestra_motor = 100L)
  a <- .ddd_desvio(perfil, "a")
  b <- .ddd_desvio(perfil, "b")
  expect_true(a$estado %in% c("calculado", "estimado"))
  expect_equal(a$valor, sqrt(0.5), tolerance = 1e-12)
  expect_true(b$estado %in% c("calculado", "estimado"))
  expect_equal(b$valor, stats::sd(c(1, 7, 3, 9)), tolerance = 1e-12)
})

test_that("la muestra del motor de SQL Server publica el desvio exacto, no el que centro en doble", {
  con <- .ddd_mssql()
  on.exit({
    try(DBI::dbExecute(con, "DROP TABLE IF EXISTS ddd_muestra_ancho"),
        silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  try(DBI::dbExecute(con, "DROP TABLE IF EXISTS ddd_muestra_ancho"), silent = TRUE)
  DBI::dbExecute(con, "CREATE TABLE ddd_muestra_ancho (v DECIMAL(18,2))")
  DBI::dbExecute(con, paste(
    "INSERT INTO ddd_muestra_ancho VALUES (500000000000006.60),",
    "(500000000000002.90), (500000000000004.22), (500000000000007.00),",
    "(500000000000004.58), (500000000000000.09)"
  ))
  muestra <- .ddd_desvio(.ddd_perfil(
    con, "ddd_muestra_ancho", universo = "muestra_motor", muestra_motor = 100L
  ), "v")
  # El que centraba en doble no se publicaba; el de las sumas exactas, pedidas
  # en la misma sentencia que los agregados de la muestra, si: el exacto.
  expect_identical(muestra$estado, "estimado")
  expect_identical(muestra$valor, 0x1.456fce117a4e8p+1)
  # Por la tabla completa la resta se hace en `decimal` y se publica.
  centavos <- c(660, 290, 422, 700, 458, 9)
  completa <- .ddd_desvio(.ddd_perfil(con, "ddd_muestra_ancho"), "v")
  expect_identical(completa$estado, "calculado")
  expect_equal(completa$valor, stats::sd(centavos) / 100, tolerance = 1e-12)
})
