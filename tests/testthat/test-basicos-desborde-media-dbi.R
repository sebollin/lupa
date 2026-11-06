# Los basicos de `perfilar_dbi()` son una sola consulta -minimo, maximo, media y
# conteos-, y en DuckDB `AVG(v * 1.0)` desborda con un `HUGEINT` desde 10^37 (es
# un `DECIMAL(38, 1)`) y con un `UHUGEINT` desde 10^38 (no entra en
# `DECIMAL(38, 0)`). El motor rechazaba la consulta entera: el minimo, el
# maximo y los conteos caian con la media, con el error del motor por motivo, y
# ese error cita el valor de la celda aun con `incluir_valores = FALSE`. Ahora
# los basicos se repiten sin el promedio, y la columna se juzga como cualquier
# otra que pasa de 2^53: la misma respuesta que un BIGINT con un valor asi.

.bdm_estados <- function(perfil, columna) {
  sql <- perfil$resumen_tabla$sql
  m <- c("minimo", "maximo", "media", "mediana", "desvio", "n_ceros",
         "n_negativos")
  vapply(m, function(x) {
    e <- sql$estado[sql$columna == columna & sql$metrica == x]
    if (length(e)) e[[length(e)]] else NA_character_
  }, character(1L))
}

test_that("el desborde de la media de un HUGEINT no se lleva los demas basicos ni cita la celda", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  tope_h <- "170141183460469231731687303715884105727"
  tope_u <- "340282366920938463463374607431768211455"
  DBI::dbExecute(con, "CREATE TABLE h (v HUGEINT, u UHUGEINT, b BIGINT, w INTEGER)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO h VALUES (1, 1, 1, 1), (2, 2, 2, 2), (",
    tope_h, ", ", tope_u, ", 9223372036854775807, 3)"
  ))
  for (incluir in c(TRUE, FALSE)) {
    p <- suppressWarnings(perfilar_dbi(
      con, "h", proteger_datos_personales = FALSE, incluir_valores = incluir,
      bloque_muestra = "solo_agregados"
    ))
    sql <- p$resumen_tabla$sql
    referencia <- .bdm_estados(p, "b")
    # La misma respuesta que la columna BIGINT con un valor desde 2^53.
    expect_identical(.bdm_estados(p, "v"), referencia, info = paste("incluir_valores =", incluir))
    expect_identical(.bdm_estados(p, "u"), referencia, info = paste("incluir_valores =", incluir))
    # Ningun motivo es el error crudo del motor, ni cita los valores.
    propios <- sql$motivo[sql$columna %in% c("v", "u")]
    expect_false(any(grepl("Out of Range|Could not cast", propios)), info = paste("incluir_valores =", incluir))
    expect_false(any(grepl(tope_h, sql$motivo, fixed = TRUE)), info = paste("incluir_valores =", incluir))
    expect_false(any(grepl(tope_u, sql$motivo, fixed = TRUE)), info = paste("incluir_valores =", incluir))
    # La columna chica de al lado no se entera.
    expect_identical(p$resumen_tabla$columnas$media[p$resumen_tabla$columnas$columna == "w"], 2)
  }
  # Con valores, el maximo nombra la causa: es un entero ancho.
  p <- suppressWarnings(perfilar_dbi(
    con, "h", proteger_datos_personales = FALSE, bloque_muestra = "solo_agregados"
  ))
  sql <- p$resumen_tabla$sql
  expect_match(sql$motivo[sql$columna == "v" & sql$metrica == "maximo"],
               "entero ancho", fixed = TRUE)
  # La mediana sigue juzgada por su valor: la central de 1, 2, tope es 2.
  expect_identical(p$resumen_tabla$columnas$mediana[p$resumen_tabla$columnas$columna == "v"], 2)
})

test_that("si el promedio desborda y los extremos caben en un doble, solo la media queda afuera", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (v INTEGER)")
  DBI::dbExecute(con, "INSERT INTO t VALUES (-3), (0), (0), (5)")
  # Un motor cuyo promedio desborda con valores chicos no existe entre los que
  # se prueban: se simula el rechazo de la consulta con el promedio.
  original <- lupa:::.consultar_dbi
  testthat::local_mocked_bindings(
    .consultar_dbi = function(conexion, sql, ...) {
      if (grepl("AVG(", sql, fixed = TRUE) && grepl("MIN(", sql, fixed = TRUE)) {
        return(list(ok = FALSE, motivo = "Out of Range Error: Overflow in AVG",
                    datos = NULL))
      }
      original(conexion, sql, ...)
    },
    .package = "lupa"
  )
  p <- suppressWarnings(perfilar_dbi(
    con, "t", proteger_datos_personales = FALSE, bloque_muestra = "solo_agregados"
  ))
  estados <- .bdm_estados(p, "v")
  expect_identical(unname(estados[c("minimo", "maximo", "n_ceros", "n_negativos")]),
                   rep("calculado", 4L))
  expect_identical(unname(estados[["media"]]), "no_disponible")
  sql <- p$resumen_tabla$sql
  expect_match(sql$motivo[sql$columna == "v" & sql$metrica == "media"],
               "desbord", fixed = TRUE)
  columnas <- p$resumen_tabla$columnas
  expect_identical(c(columnas$minimo, columnas$maximo), c(-3, 5))
  expect_identical(as.numeric(c(columnas$n_ceros, columnas$n_negativos)), c(2, 1))
  expect_true(is.na(columnas$media))
})
