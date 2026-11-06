# El desvio de un decimal que llega como doble se niega por bloques y por la
# muestra del motor cuando el redondeo de cada valor puede moverlo
# (`.desvio_mal_condicionado_dbi()`). Tres cosas que esa regla dejaba:
#
# - Negaba un decimal de escala 0 con valores por debajo de 2^53, que el doble
#   si representa: 5e14 + k en `numeric(18,0)` tiene el desvio exacto. Cuando el
#   tipo declara la escala, la guarda la mira.
# - El spool de la muestra del motor -el camino por omision de
#   `universo = "muestra_motor"`- calcula el desvio en R sobre esos dobles y no
#   tenia la guarda: publicaba un desvio a una parte en 300 del exacto.
# - El spool de SQL Server leia `money` con dos decimales.

.dde_fecha <- as.POSIXct("2026-11-06 12:00:00", tz = "UTC")

.dde_desvio <- function(perfil, columna) {
  sql <- perfil$resumen_tabla$sql
  fila <- sql[sql$columna == columna & sql$metrica == "desvio", , drop = FALSE]
  list(
    estado = fila$estado[[nrow(fila)]], motivo = fila$motivo[[nrow(fila)]],
    valor = perfil$resumen_tabla$columnas$desvio[
      perfil$resumen_tabla$columnas$columna == columna
    ]
  )
}

test_that("por bloques, un decimal de escala 0 por debajo de 2^53 publica su desvio", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  k <- c(0, 3, 7, 1, 9, 4, 2, 8, 5)
  DBI::dbWriteTable(con, "t", data.frame(id = seq_along(k), d = 5e14 + k))
  # SQLite no tiene decimales: el tipo que publicaria el catalogo de
  # PostgreSQL, MySQL o MariaDB se declara a mano, y los valores llegan como
  # los dobles que entregaria el controlador.
  tipo_d <- NULL
  testthat::local_mocked_bindings(
    .tipos_declarados_con_motor_dbi = function(tipos, tipos_motor) {
      c("integer", tipo_d)
    },
    .package = "lupa"
  )
  perfil <- function(tipo) {
    tipo_d <<- tipo
    suppressWarnings(perfilar_dbi(
      con, "t", bloque_filas = 2L, bloque_muestra = "solo_agregados",
      proteger_datos_personales = FALSE, fecha = .dde_fecha
    ))
  }
  # El desvio no cambia al restar 5e14: el de `k` es el exacto.
  for (tipo in c("numeric(18,0)", "decimal(10)")) {
    d <- .dde_desvio(perfil(tipo), "d")
    expect_identical(d$estado, "calculado", info = tipo)
    expect_lt(abs(d$valor - stats::sd(k)) / stats::sd(k), 1e-12)
  }
  # Sin escala, o con decimales, los dobles no dicen que sean los valores.
  for (tipo in c("numeric", "numeric(18,2)")) {
    d <- .dde_desvio(perfil(tipo), "d")
    expect_identical(d$estado, "no_disponible", info = tipo)
    expect_identical(d$motivo, lupa:::.MOTIVO_DESVIO_DECIMAL_DOBLE_DBI, info = tipo)
  }
})

test_that("el spool de la muestra del motor no publica el desvio que el doble de un decimal movio", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  centavos <- c("0.60", "3.15", "7.20", "1.05", "9.90", "4.45", "2.30", "8.75", "5.10")
  k <- as.numeric(centavos)
  DBI::dbExecute(con, paste(
    "CREATE TABLE t (ancho DECIMAL(18,2), chico DECIMAL(18,2),",
    "entero DECIMAL(18,0))"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO t VALUES ", paste0(
      "(5000000000000", sub("^([0-9])", "0\\1", centavos), ", ", centavos, ", ",
      sprintf("%.0f", 5e14 + seq_along(k)), ")", collapse = ", "
    )
  ))
  # El primer valor de `ancho` es 500000000000000.60: lo que el doble guarda es
  # 500000000000000.625. Medido sin esta guarda: el spool publicaba 3,3097 contra
  # 3,3205 (una parte en 300).
  p <- suppressWarnings(perfilar_dbi(
    con, "t", universo = "muestra_motor", muestra_motor = length(k),
    proteger_datos_personales = FALSE, fecha = .dde_fecha
  ))
  ancho <- .dde_desvio(p, "ancho")
  expect_identical(ancho$estado, "no_disponible")
  expect_identical(ancho$motivo, lupa:::.MOTIVO_DESVIO_DECIMAL_DOBLE_DBI)
  expect_true(is.na(ancho$valor))
  pm <- p$perfil_muestra$columnas
  expect_true(is.na(pm$desvio[pm$columna == "ancho"]))
  # Control: los mismos centavos, chicos, y los enteros que la muestra convirtio
  # exactos a `integer64`.
  chico <- .dde_desvio(p, "chico")
  expect_true(chico$estado %in% c("estimado", "observado_muestra"))
  expect_lt(abs(chico$valor - stats::sd(k)) / stats::sd(k), 1e-12)
  entero <- .dde_desvio(p, "entero")
  expect_true(entero$estado %in% c("estimado", "observado_muestra"))
  expect_lt(abs(entero$valor - stats::sd(seq_along(k))) / stats::sd(seq_along(k)), 1e-12)
})

test_that("el spool de SQL Server lee money con sus cuatro decimales", {
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
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  tabla <- "lupa_dde_money"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (m money, s smallmoney)"))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES (0.0001, 0.0001), (0.0002, 0.0002), ",
    "(0.0003, 0.0003), (0.0004, 0.0004)"
  ))
  p <- suppressWarnings(perfilar_dbi(
    con, tabla, universo = "muestra_motor", muestra_motor = 4L,
    proteger_datos_personales = FALSE, fecha = .dde_fecha
  ))
  columnas <- p$resumen_tabla$columnas
  for (columna in c("m", "s")) {
    fila <- columnas[columnas$columna == columna, , drop = FALSE]
    # `CAST(money AS VARCHAR)` escribia "0.00": minimo, media y desvio 0.
    expect_equal(fila$minimo, 1e-4, info = columna)
    expect_equal(fila$media, 2.5e-4, info = columna)
    expect_lt(abs(fila$desvio - stats::sd(1:4) * 1e-4) / (stats::sd(1:4) * 1e-4),
              1e-12)
  }
})
