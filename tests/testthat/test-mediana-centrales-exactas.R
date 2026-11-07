# La mediana de una columna de enteros con algun valor desde 2^53 depende solo
# de sus centrales. `perfilar_dbi()` la decide por su valor desde que la regla
# es una (`.causa_mediana_inexacta_dbi()`), y `perfilar()` sobre `integer64`
# -que es tambien `perfil_muestra` y el spool de `muestra_motor`- la negaba por
# la columna: la de `{1, 2, 2^63 - 1}` es 2, y no se publicaba. En la via por
# omision el resumen SQL publicaba 2 y la muestra `NA`, y la corroboracion lo
# contaba como divergencia. La mediana exacta se calcula aca con `bit64` (la
# suma de las dos centrales, exacta en 64 bits), nunca con `median()` sobre los
# dobles.

.mce_exacta_doble <- function(texto) {
  x <- bit64::as.integer64(texto)
  x <- x[order(bit64::rank.integer64(x))]
  n <- length(x)
  centrales <- x[unique(c((n + 1L) %/% 2L, (n + 2L) %/% 2L))]
  # El doble de la mediana exacta, como entero de 64 bits.
  if (length(centrales) == 1L) centrales * 2L else centrales[[1L]] + centrales[[2L]]
}

.mce_es_exacta <- function(valor, texto) {
  is.finite(valor) && (valor * 2) < 2^62 && (valor * 2) > -2^62 &&
    identical(as.character(bit64::as.integer64(valor * 2)),
              as.character(.mce_exacta_doble(texto)))
}

.mce_perfil <- function(datos, ...) {
  suppressWarnings(perfilar(
    datos, proteger_datos_personales = FALSE,
    fecha = as.POSIXct("2026-11-06 12:00:00", tz = "UTC"), ...
  ))$columnas
}

test_that("perfilar() publica la mediana de un integer64 que pasa de 2^53 cuando sus centrales caben en un doble", {
  skip_if_not_installed("bit64")
  publicadas <- list(
    A = c("1", "2", "9223372036854775807"),
    E = c("-4611686018427387904", "5", "4611686018427387904"),
    F = c("-4611686018427387904", "4", "7", "4611686018427387904"),
    L = c("-9007199254740991", "1", "2", "9007199254740995"),
    N = c("-9223372036854775807", "5", "9223372036854775807"),
    T = c("-4611686018427387904", "4503599627370497", "4503599627370499",
          "4611686018427387904")
  )
  for (nombre in names(publicadas)) {
    texto <- publicadas[[nombre]]
    fila <- .mce_perfil(data.frame(v = bit64::as.integer64(texto)))
    expect_identical(fila$estado_resumen_cuantitativo, "omitidos_precision",
                     info = nombre)
    expect_true(.mce_es_exacta(fila$mediana, texto), info = nombre)
    # Lo que depende de toda la columna sigue sin publicarse.
    expect_true(is.na(fila$media) && is.na(fila$desvio) &&
                  is.na(fila$minimo) && is.na(fila$maximo), info = nombre)
  }
  # Una central desde 2^53 -tambien 2^53 justo- no se publica.
  negadas <- list(
    B = c("1", "9007199254740993", "9007199254740995", "9007199254740997"),
    D = c("-9007199254740991", "9007199254740995"),
    K = c("1", "2", "4611686018427387904", "4611686018427387905"),
    U = c("1", "9007199254740992", "4611686018427387904")
  )
  for (nombre in names(negadas)) {
    fila <- .mce_perfil(data.frame(v = bit64::as.integer64(negadas[[nombre]])))
    expect_true(is.na(fila$mediana), info = nombre)
  }
})

test_that("el mismo entero escrito como texto recibe la misma mediana que el integer64", {
  skip_if_not_installed("bit64")
  conjuntos <- list(
    c("1", "2", "9223372036854775807"),
    c("-4611686018427387904", "4", "7", "4611686018427387904"),
    c("-9007199254740991", "-0", "2", "9007199254740995", "+5"),
    c("1", "9007199254740993", "9007199254740995", "9007199254740997"),
    # Mas alla de 64 bits: solo el texto lo guarda.
    c("1", "2", "170141183460469231731687303715884105727")
  )
  for (i in seq_along(conjuntos)) {
    texto <- conjuntos[[i]]
    fila_texto <- .mce_perfil(data.frame(v = texto, stringsAsFactors = FALSE))
    expect_identical(fila_texto$estado_resumen_cuantitativo, "omitidos_precision",
                     info = i)
    if (i < 5L) {
      fila_64 <- .mce_perfil(data.frame(v = bit64::as.integer64(texto)))
      expect_identical(fila_texto$mediana, fila_64$mediana, info = i)
    } else {
      expect_identical(fila_texto$mediana, 2)
    }
  }
  # "-0" es el cero: la central de -(2^53 - 1), -0, 2, 5, 2^53 + 3 es 2.
  expect_identical(.mce_perfil(data.frame(
    v = conjuntos[[3L]], stringsAsFactors = FALSE
  ))$mediana, 2)
  # Los centinelas declarados salen antes de las centrales, como en
  # `integer64`. Sin el 1 quedan 2 y 2^63 - 1, que no cabe: `NA`. Sin el 4,
  # la central es 7.
  fila <- .mce_perfil(data.frame(v = conjuntos[[1L]], stringsAsFactors = FALSE),
                      sentinelas_numericos = 1)
  expect_true(is.na(fila$mediana))
  fila <- .mce_perfil(data.frame(v = conjuntos[[2L]], stringsAsFactors = FALSE),
                      sentinelas_numericos = 4)
  expect_identical(fila$mediana, 7)
  expect_identical(fila$mediana, .mce_perfil(
    data.frame(v = bit64::as.integer64(conjuntos[[2L]])),
    sentinelas_numericos = 4
  )$mediana)
})

test_that("una columna de codigo que pasa de 2^53 sigue sin publicar la mediana", {
  skip_if_not_installed("bit64")
  datos <- data.frame(v = bit64::as.integer64(c("1", "2", "9223372036854775807")))
  fila <- .mce_perfil(datos, columnas_codigo = "v")
  expect_true(is.na(fila$mediana))
  expect_identical(fila$estado_resumen_cuantitativo, "omitidos_precision")
  # Control: sin la declaracion, si.
  expect_identical(.mce_perfil(datos)$mediana, 2)
})

test_that("el spool de la muestra del motor y la corroboracion publican la mediana de un BIGINT que pasa de 2^53", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("bit64")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (v INTEGER)")
  DBI::dbExecute(con, paste(
    "INSERT INTO t VALUES (-4611686018427387904), (4), (7),",
    "(4611686018427387904)"
  ))
  fecha <- as.POSIXct("2026-11-06 12:00:00", tz = "UTC")
  spool <- suppressWarnings(perfilar_dbi(
    con, "t", universo = "muestra_motor", muestra_motor = 100L,
    proteger_datos_personales = FALSE, fecha = fecha
  ))
  sql <- spool$resumen_tabla$sql
  expect_identical(spool$resumen_tabla$columnas$mediana, 5.5)
  expect_true(sql$estado[sql$columna == "v" & sql$metrica == "mediana"] %in%
                c("estimado", "observado_muestra"))
  # La tabla completa: el resumen SQL y `perfil_muestra` dicen lo mismo.
  completa <- suppressWarnings(perfilar_dbi(
    con, "t", proteger_datos_personales = FALSE, fecha = fecha
  ))
  expect_identical(completa$resumen_tabla$columnas$mediana, 5.5)
  expect_identical(completa$perfil_muestra$columnas$mediana, 5.5)
  expect_identical(completa$resumen_tabla$meta$corroboracion_bloques$divergencias, 0L)
})

test_that("un decimal entero que la muestra convirtio a integer64 no se tapa como si fuera un doble", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (v DECIMAL(38,0))")
  DBI::dbExecute(con, paste(
    "INSERT INTO t VALUES (-4611686018427387904), (0), (5),",
    "(4611686018427387904)"
  ))
  p <- suppressWarnings(perfilar_dbi(
    con, "t", universo = "muestra_motor", muestra_motor = 100L,
    proteger_datos_personales = FALSE,
    fecha = as.POSIXct("2026-11-06 12:00:00", tz = "UTC")
  ))
  columnas <- p$resumen_tabla$columnas
  sql <- p$resumen_tabla$sql
  estado <- function(m) sql$estado[sql$columna == "v" & sql$metrica == m]
  expect_identical(columnas$mediana, 2.5)
  expect_identical(as.numeric(columnas$n_ceros), 1)
  expect_identical(as.numeric(columnas$n_negativos), 1)
  expect_identical(estado("n_ceros"), "observado_muestra")
  # Lo que depende de toda la columna sigue retenido.
  expect_true(is.na(columnas$media) && is.na(columnas$minimo))
  expect_false(any(grepl("decimal entregado como doble", sql$motivo[sql$columna == "v"],
                         fixed = TRUE)))
})

test_that("la muestra de SQL Server lee un decimal(38, 0) de 38 cifras", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("odbc")
  skip_if_not_installed("bit64")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  tabla <- "lupa_mce_decimal38"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (v DECIMAL(38,0), w INT)"))
  # En T-SQL `CAST(... AS VARCHAR)` es `VARCHAR(30)`, y estas 38 cifras no
  # entraban: el motor abortaba la lectura y la muestra entera -tambien `w`-
  # quedaba `no_disponible`.
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES (1, 10), (2, 20), ",
    "(99999999999999999999999999999999999999, 30)"
  ))
  p <- suppressWarnings(perfilar_dbi(
    con, tabla, universo = "muestra_motor", muestra_motor = 100L,
    proteger_datos_personales = FALSE,
    fecha = as.POSIXct("2026-11-06 12:00:00", tz = "UTC")
  ))
  sql <- p$resumen_tabla$sql
  expect_false(any(grepl("error_lectura_spool", sql$motivo, fixed = TRUE)))
  columnas <- p$resumen_tabla$columnas
  expect_identical(columnas$mediana[columnas$columna == "v"], 2)
  expect_identical(columnas$mediana[columnas$columna == "w"], 20)
})
