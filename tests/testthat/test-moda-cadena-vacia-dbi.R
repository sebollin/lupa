# Una columna de texto cuya moda es la cadena vacia: `perfilar()` publica
# `moda = ""`, y `perfilar_dbi()` publicaba el mismo valor con el estado
# `no_disponible` y el motivo "la consulta termino sin un valor". El motor
# devolvio el valor; la via lo confundia con el `NULL` de SQL. Las dos vias
# tienen que decir lo mismo.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.conexiones_moda_vacia <- function() {
  conexiones <- list()
  if (requireNamespace("duckdb", quietly = TRUE)) {
    conexiones$duckdb <- function() DBI::dbConnect(.duckdb_prueba())
  }
  if (requireNamespace("RSQLite", quietly = TRUE)) {
    conexiones$sqlite <- function() DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  }
  conexiones
}

.datos_moda_vacia <- function() {
  data.frame(
    # 60 vacias y 40 `a`: la moda es "" con frecuencia 60.
    t = c(rep("", 60L), rep("a", 40L)),
    # Solo vacias: en una muestra del motor la moda sigue siendo "".
    s = rep("", 100L),
    # Lo que tiene que seguir igual: una moda comun, una columna sin valores y
    # una sin repetidos.
    x = c(rep("x", 60L), rep("a", 40L)),
    v = rep(NA_character_, 100L),
    w = as.character(1:100),
    stringsAsFactors = FALSE
  )
}

.estado_moda_dbi <- function(perfil, columna) {
  sql <- perfil$resumen_tabla$sql
  as.character(sql$estado[sql$metrica == "moda" & sql$columna == columna])
}

.moda_dbi <- function(perfil, columna) {
  columnas <- perfil$resumen_tabla$columnas
  columnas$moda[columnas$columna == columna]
}

test_that("una moda que es la cadena vacia sale calculada en perfilar_dbi(), como en perfilar()", {
  conexiones <- .conexiones_moda_vacia()
  if (!length(conexiones)) skip("Sin duckdb ni RSQLite.")
  datos <- .datos_moda_vacia()
  fecha <- as.POSIXct("2026-01-15", tz = "UTC")
  en_memoria <- perfilar(datos, fecha = fecha)
  expect_identical(en_memoria$columnas$moda[en_memoria$columnas$columna == "t"], "")
  for (motor in names(conexiones)) {
    con <- conexiones[[motor]]()
    DBI::dbWriteTable(con, "t", datos)
    for (bloque in c("con_muestra", "solo_agregados")) {
      perfil <- perfilar_dbi(con, "t", fecha = fecha, bloque_muestra = bloque)
      info <- paste(motor, bloque)
      expect_identical(.moda_dbi(perfil, "t"), "", info = info)
      columnas <- perfil$resumen_tabla$columnas
      expect_equal(columnas$frecuencia_moda[columnas$columna == "t"], 60, info = info)
      expect_identical(.estado_moda_dbi(perfil, "t"), "calculado", info = info)
      expect_identical(.estado_moda_dbi(perfil, "s"), "calculado", info = info)
    }
    # Sobre una muestra del motor la cifra es una estimacion, no una ausencia.
    muestra <- perfilar_dbi(con, "t", fecha = fecha, universo = "muestra_motor",
                            muestra_motor = 30)
    expect_identical(.moda_dbi(muestra, "s"), "", info = motor)
    expect_identical(.estado_moda_dbi(muestra, "s"), "estimado", info = motor)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("la moda que falta de verdad sigue declarandose como falta", {
  conexiones <- .conexiones_moda_vacia()
  if (!length(conexiones)) skip("Sin duckdb ni RSQLite.")
  datos <- .datos_moda_vacia()
  fecha <- as.POSIXct("2026-01-15", tz = "UTC")
  for (motor in names(conexiones)) {
    con <- conexiones[[motor]]()
    DBI::dbWriteTable(con, "t", datos)
    perfil <- perfilar_dbi(con, "t", fecha = fecha)
    expect_identical(.moda_dbi(perfil, "x"), "x", info = motor)
    expect_identical(.estado_moda_dbi(perfil, "x"), "calculado", info = motor)
    # Sin valores no hay moda: `NA` y su estado propio.
    expect_identical(.moda_dbi(perfil, "v"), NA_character_, info = motor)
    expect_identical(.estado_moda_dbi(perfil, "v"), "sin_valores", info = motor)
    # Ningun valor se repite: tampoco hay moda.
    expect_identical(.moda_dbi(perfil, "w"), NA_character_, info = motor)
    expect_identical(.estado_moda_dbi(perfil, "w"), "no_aplica", info = motor)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})
