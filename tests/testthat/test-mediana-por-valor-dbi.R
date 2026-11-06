# La mediana de una columna entera o decimal se publica `calculado` cuando su
# valor es exacto, y eso se decide por el valor y no por la columna (ver
# `.causa_mediana_inexacta_dbi()`). Habia dos reglas: la de los basicos negaba
# la mediana de toda columna con algun valor de al menos 2^53 -la de
# `{1, 2, 2^63 - 1}` en BIGINT, que es 2- y la de la mediana misma la publicaba
# cuando quedaba por debajo de 2^53, que con una cantidad par miente.
#
# Los valores esperados son exactos: se escriben como texto y se comparan con
# `identical()`, no con `median()` sobre dobles.

.mpv_perfil <- function(conexion, tabla, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, tabla, bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE, ...
  )))
}

.mpv_mediana <- function(perfil, columna = "v") {
  sql <- perfil$resumen_tabla$sql
  fila <- sql[sql$columna == columna & sql$metrica == "mediana", , drop = FALSE]
  columnas <- perfil$resumen_tabla$columnas
  list(
    estado = unique(as.character(fila$estado)),
    motivo = unique(as.character(fila$motivo)),
    valor = as.numeric(columnas$mediana[columnas$columna == columna])
  )
}

.mpv_tabla <- function(conexion, tabla, tipo, valores) {
  try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
      silent = TRUE)
  DBI::dbExecute(conexion, paste0("CREATE TABLE ", tabla, " (v ", tipo, ")"))
  DBI::dbExecute(conexion, paste0(
    "INSERT INTO ", tabla, " VALUES ",
    paste0("(", valores, ")", collapse = ", ")
  ))
}

.mpv_duckdb <- function() {
  skip_on_cran()
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
}

test_that("la mediana impar de un BIGINT que pasa de 2^53 se publica si su central es exacta", {
  conexion <- .mpv_duckdb()
  on.exit(try(duckdb::duckdb_shutdown(conexion), silent = TRUE), add = TRUE)
  casos <- list(
    # La central es 2 y el maximo del tipo no la toca.
    list(valores = c("1", "2", "9223372036854775807"), esperada = 2),
    list(valores = c("-4611686018427387904", "5", "4611686018427387904"),
         esperada = 5)
  )
  for (tipo in c("BIGINT", "HUGEINT")) {
    for (caso in casos) {
      .mpv_tabla(conexion, "mpv_impar", tipo, caso$valores)
      perfil <- .mpv_perfil(conexion, "mpv_impar")
      mediana <- .mpv_mediana(perfil)
      info <- paste(tipo, paste(caso$valores, collapse = ","))
      expect_identical(mediana$estado, "calculado", info = info)
      expect_identical(mediana$valor, caso$esperada, info = info)
      # Las otras seis dependen de toda la columna y siguen sin publicarse.
      sql <- perfil$resumen_tabla$sql
      otras <- sql[sql$columna == "v" & sql$metrica %in%
                     setdiff(lupa:::.METRICAS_DE_MAGNITUD_DBI, "mediana"), ]
      expect_true(nrow(otras) == 6L && all(otras$estado == "no_disponible"),
                  info = info)
    }
  }
  # Control: la central de al menos 2^53 sigue sin publicarse.
  .mpv_tabla(conexion, "mpv_impar", "BIGINT",
             c("1", "9007199254740993", "4611686018427387904"))
  mediana <- .mpv_mediana(.mpv_perfil(conexion, "mpv_impar"))
  expect_identical(mediana$estado, "no_disponible")
  expect_true(is.na(mediana$valor))
})

test_that("la mediana par no se publica si una central pudo pasar de 2^53", {
  conexion <- .mpv_duckdb()
  on.exit(try(duckdb::duckdb_shutdown(conexion), silent = TRUE), add = TRUE)
  maximo <- "170141183460469231731687303715884105727"
  # La exacta es 2. El doble de 9007199254740995 es 9007199254740996, y el
  # promedio de los dobles, 2,5: por debajo de 2^53 pero falso. Con el maximo de
  # HUGEINT los basicos desbordan, y la mediana se publicaba 2,5.
  .mpv_tabla(conexion, "mpv_par", "HUGEINT", c(
    paste0("-", maximo), "-9007199254740991", "9007199254740995", maximo
  ))
  mediana <- .mpv_mediana(.mpv_perfil(conexion, "mpv_par"))
  expect_identical(mediana$estado, "no_disponible")
  expect_true(is.na(mediana$valor))
  expect_match(mediana$motivo, "cantidad de valores es par", fixed = TRUE)
  # Lo mismo en BIGINT, sin desborde.
  .mpv_tabla(conexion, "mpv_par", "BIGINT",
             c("-9007199254740991", "9007199254740995"))
  mediana <- .mpv_mediana(.mpv_perfil(conexion, "mpv_par"))
  expect_identical(mediana$estado, "no_disponible")
  # Control: con una cantidad par y una columna que no pasa de 2^53, se publica.
  .mpv_tabla(conexion, "mpv_par", "BIGINT", c("1", "2", "3", "4"))
  mediana <- .mpv_mediana(.mpv_perfil(conexion, "mpv_par"))
  expect_identical(mediana$estado, "calculado")
  expect_identical(mediana$valor, 2.5)
})

test_that("DuckDB no trunca la mediana de un DECIMAL a su escala", {
  conexion <- .mpv_duckdb()
  on.exit(try(duckdb::duckdb_shutdown(conexion), silent = TRUE), add = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE mpv_decimal (d DECIMAL(10,2), e DECIMAL(38,0))")
  DBI::dbExecute(conexion, paste(
    "INSERT INTO mpv_decimal VALUES (1.00, 1), (1.01, 2), (1.01, 3), (1.00, 4)"
  ))
  perfil <- .mpv_perfil(conexion, "mpv_decimal")
  leidos <- DBI::dbGetQuery(conexion, "SELECT d, e FROM mpv_decimal")
  # La mediana de R sobre lo que lee el controlador, como en T-SQL.
  for (columna in c("d", "e")) {
    mediana <- .mpv_mediana(perfil, columna)
    expect_identical(mediana$estado, "calculado", info = columna)
    expect_identical(mediana$valor, stats::median(as.numeric(leidos[[columna]])),
                     info = columna)
  }
  expect_identical(.mpv_mediana(perfil, "e")$valor, 2.5)
})

test_that("PostgreSQL no publica la mediana que su interpolacion redondea", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  conexion <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("LUPA_PG_PORT", "55432")),
    user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(conexion), "PostgreSQL no disponible.")
  on.exit({
    try(DBI::dbExecute(conexion, "DROP TABLE IF EXISTS mpv_pg"), silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)
  # Ninguno pasa de 2^53. La exacta es -0,5; `PERCENTILE_CONT` daba -1.
  .mpv_tabla(conexion, "mpv_pg", "bigint",
             c("-9007199254740991", "9007199254740990"))
  perfil <- .mpv_perfil(conexion, "mpv_pg")
  mediana <- .mpv_mediana(perfil)
  sql <- perfil$resumen_tabla$sql
  metodo <- sql$metodo[sql$columna == "v" & sql$metrica == "mediana"]
  if (identical(mediana$estado, "calculado")) {
    # Otro camino -la subconsulta escalar de 9.3- promedia en NUMERIC y acierta.
    expect_false(any(grepl("^PERCENTILE_CONT", metodo)))
    expect_identical(mediana$valor, -0.5)
  } else {
    expect_identical(mediana$estado, "no_disponible")
    expect_match(mediana$motivo, "PERCENTILE_CONT", fixed = TRUE)
  }
  # Control: la impar de una columna que pasa de 2^53 se publica.
  .mpv_tabla(conexion, "mpv_pg", "bigint",
             c("1", "2", "9223372036854775807"))
  mediana <- .mpv_mediana(.mpv_perfil(conexion, "mpv_pg"))
  expect_identical(mediana$estado, "calculado")
  expect_identical(mediana$valor, 2)
})

# Con una cantidad par y una columna que pasa de 2^53, la mediana se publica si
# las dos centrales estan en (-2^52, 2^52): una consulta cuenta cuantos valores
# quedan de cada lado, y entonces cualquier promedio de las dos es exacto.
test_that("la mediana par de una columna que pasa de 2^53 se publica si sus centrales son chicas", {
  conexion <- .mpv_duckdb()
  on.exit(try(duckdb::duckdb_shutdown(conexion), silent = TRUE), add = TRUE)
  casos <- list(
    list(valores = c("-4611686018427387904", "4", "7", "4611686018427387904"),
         esperada = 5.5),
    list(valores = c("-9007199254740991", "1", "2", "9007199254740995"),
         esperada = 1.5)
  )
  for (tipo in c("BIGINT", "HUGEINT")) {
    for (caso in casos) {
      .mpv_tabla(conexion, "mpv_centrales", tipo, caso$valores)
      mediana <- .mpv_mediana(.mpv_perfil(conexion, "mpv_centrales"))
      info <- paste(tipo, paste(caso$valores, collapse = ","))
      expect_identical(mediana$estado, "calculado", info = info)
      expect_identical(mediana$valor, caso$esperada, info = info)
    }
  }
  # Control: la central alta pasa de 2^52 y no se publica.
  .mpv_tabla(conexion, "mpv_centrales", "BIGINT",
             c("1", "2", "4611686018427387904", "4611686018427387905"))
  mediana <- .mpv_mediana(.mpv_perfil(conexion, "mpv_centrales"))
  expect_identical(mediana$estado, "no_disponible")
})
