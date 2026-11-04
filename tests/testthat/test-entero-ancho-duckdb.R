test_that("DuckDB publica enteros anchos entregados como doble", {
  skip_on_cran()
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  conexion <- DBI::dbConnect(
    duckdb::duckdb(shared_home = TRUE), dbdir = ":memory:"
  )
  on.exit(try(duckdb::duckdb_shutdown(conexion), silent = TRUE), add = TRUE)
  DBI::dbExecute(conexion, paste0(
    "CREATE TABLE a2561_entero_duck (",
    "big BIGINT, ubig UBIGINT, hug HUGEINT, uhug UHUGEINT, chico BIGINT)"
  ))
  DBI::dbExecute(conexion, paste0(
    "INSERT INTO a2561_entero_duck ",
    "SELECT v, v, v, v, CASE WHEN i <= 4 THEN 2 ELSE i END ",
    "FROM (SELECT CASE WHEN i <= 5 THEN ",
    "1234567890123456789 ELSE 1234567890123400000 + i END AS v, i ",
    "FROM range(1, 41) r(i))"
  ))
  on.exit(try(DBI::dbExecute(
    conexion, "DROP TABLE a2561_entero_duck"
  ), silent = TRUE), add = TRUE)
  esperado <- "1234567890123456789"
  magnitudes <- c("minimo", "maximo", "media", "mediana", "desvio")
  for (modo in c("solo_agregados", "con_muestra")) {
    perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
      conexion, "a2561_entero_duck", bloque_muestra = modo,
      proteger_datos_personales = FALSE
    )))
    columnas <- perfil$resumen_tabla$columnas
    for (campo in c("big", "ubig", "hug", "uhug")) {
      fila <- columnas[columnas$columna == campo, , drop = FALSE]
      expect_identical(as.character(fila$moda), esperado, info = paste(modo, campo))
      expect_equal(as.numeric(fila$frecuencia_moda), 5,
                   info = paste(modo, campo))
      sql <- perfil$resumen_tabla$sql
      moda <- sql[sql$columna == campo & sql$metrica == "moda", , drop = FALSE]
      expect_identical(as.character(moda$estado), "calculado")
      afectados <- sql[sql$columna == campo & sql$metrica %in% magnitudes,
                       , drop = FALSE]
      expect_true(all(as.character(afectados$estado) == "no_disponible"),
                  info = paste(modo, campo))
    }
    fila_chico <- columnas[columnas$columna == "chico", , drop = FALSE]
    expect_identical(as.character(fila_chico$moda), "2")
    sql_chico <- perfil$resumen_tabla$sql
    sql_chico <- sql_chico[sql_chico$columna == "chico" &
                             sql_chico$metrica == "moda", , drop = FALSE]
    expect_false(grepl("CAST", as.character(sql_chico$sql), fixed = TRUE))
  }
  perfil_bloques <- suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, "a2561_entero_duck", bloque_muestra = "solo_agregados",
    bloque_filas = 15L, proteger_datos_personales = FALSE
  )))
  columnas <- perfil_bloques$resumen_tabla$columnas
  for (campo in c("big", "ubig", "hug", "uhug")) {
    fila <- columnas[columnas$columna == campo, , drop = FALSE]
    sql <- perfil_bloques$resumen_tabla$sql
    moda <- sql[sql$columna == campo & sql$metrica == "moda", , drop = FALSE]
    if (identical(as.character(moda$estado), "no_disponible")) {
      expect_true(is.na(fila$moda), info = paste("bloques", campo))
      expect_match(as.character(moda$motivo), "dbfetch_no_incremental")
    } else {
      expect_identical(as.character(fila$moda), esperado,
                       info = paste("bloques", campo))
      expect_equal(as.numeric(fila$frecuencia_moda), 5,
                   info = paste("bloques", campo))
    }
  }
})

test_that("DuckDB no desborda HUGEINT extremo antes de su guarda", {
  skip_on_cran()
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  conexion <- DBI::dbConnect(
    duckdb::duckdb(shared_home = TRUE), dbdir = ":memory:"
  )
  on.exit(try(duckdb::duckdb_shutdown(conexion), silent = TRUE), add = TRUE)
  DBI::dbExecute(conexion, paste0(
    "CREATE TABLE a2561_hugeint_max (hug HUGEINT)"
  ))
  DBI::dbExecute(conexion, paste0(
    "INSERT INTO a2561_hugeint_max VALUES (",
    "170141183460469231731687303715884105726), (",
    "170141183460469231731687303715884105727)"
  ))
  on.exit(try(DBI::dbExecute(
    conexion, "DROP TABLE a2561_hugeint_max"
  ), silent = TRUE), add = TRUE)
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, "a2561_hugeint_max", bloque_muestra = "solo_agregados",
    metricas = c("moda", "basicos", "mediana", "desvio"),
    proteger_datos_personales = FALSE
  )))
  fila <- perfil$resumen_tabla$columnas
  fila <- fila[fila$columna == "hug", , drop = FALSE]
  expect_identical(as.character(fila$moda),
                   "170141183460469231731687303715884105726")
  expect_true(all(is.na(fila[, c("minimo", "maximo", "media", "mediana", "desvio")])))
  sql <- perfil$resumen_tabla$sql
  afectados <- sql[sql$columna == "hug" & sql$metrica %in%
                     c("minimo", "maximo", "media", "mediana", "desvio"),
                   , drop = FALSE]
  expect_true(all(as.character(afectados$estado) == "no_disponible"))
  # La media puede caer por el desborde del motor (`HUGEINT * 1.0`) o por la
  # guarda: las dos dejan `no_disponible`. La mediana viene de otra consulta y
  # la tapa su propio valor, de al menos 2^53.
  mediana <- afectados[afectados$metrica == "mediana", , drop = FALSE]
  expect_true(any(grepl("2^53", mediana$motivo, fixed = TRUE)))
})

test_that("DuckDB da exacta la media de un BIGINT grande por debajo de 2^53", {
  skip_on_cran()
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  # Sumar en doble -`AVG(CAST(x AS DOUBLE))`- erraba esta media en 1.723
  # unidades; `AVG(x * 1.0)` suma en decimal exacto. La exacta es
  # 112589990685500014847/25000 = 4503599627420000.59..., y el doble mas
  # cercano es 4503599627420001 (entre 2^52 y 2^53 los dobles van de a 1).
  conexion <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(duckdb::duckdb_shutdown(conexion), silent = TRUE), add = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE a2561_media_big (b BIGINT)")
  DBI::dbExecute(conexion, paste0(
    "INSERT INTO a2561_media_big SELECT 4503599627370000 + ",
    "(i * 7919) % 100003 FROM range(1, 200001) r(i)"
  ))
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, "a2561_media_big", bloque_muestra = "solo_agregados",
    metricas = "basicos", proteger_datos_personales = FALSE
  )))
  fila <- perfil$resumen_tabla$columnas
  expect_identical(fila$media[fila$columna == "b"], 4503599627420001)
})
