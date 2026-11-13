# La muestra de una tabla de base de datos se perfila con el mismo lector de
# numeros escritos como texto que una tabla en memoria.
testthat::skip_on_cran()

.comprobar_numero_texto_dbi <- function(con) {
  DBI::dbWriteTable(
    con, "texto_numerico",
    data.frame(
      precio = c("10", "12", "9", "15", "9,5"),
      capacidad = c("1.5TB", "500GB", "250MB", "2TB", "750GB"),
      norma = c("LEY 18331", "LEY 19355", "LEY 17930", "LEY 16000", "LEY 15000"),
      peso = c("12 kg", "9 kg", "11 kg", "7 kg", "8 kg"),
      stringsAsFactors = FALSE
    )
  )
  perfil <- perfilar_dbi(
    con, "texto_numerico", muestra = Inf, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )$perfil_muestra
  fila <- function(columna) {
    perfil$columnas[perfil$columnas$columna == columna, , drop = FALSE]
  }
  hallazgos_de <- function(columna) {
    as.character(perfil$hallazgos$tipo_hallazgo[perfil$hallazgos$columna %in% columna])
  }
  # Un decimal entre enteros: doble, y los cinco valores entran al resumen.
  testthat::expect_identical(as.character(fila("precio")$tipo_inferido), "doble")
  testthat::expect_identical(as.numeric(fila("precio")$proporcion_tipo_inferido), 1)
  testthat::expect_identical(
    as.integer(fila("precio")$n_valores_excluidos_resumen), 0L
  )
  testthat::expect_identical(as.numeric(fila("precio")$maximo), 15)
  # Un sufijo en mayusculas no es una unidad, y `LEY` no es una moneda.
  for (columna in c("capacidad", "norma")) {
    testthat::expect_identical(
      as.integer(fila(columna)$n_numeros_texto), 0L, info = columna
    )
    testthat::expect_false(
      "numero_como_texto" %in% hallazgos_de(columna), info = columna
    )
  }
  testthat::expect_identical(as.character(fila("norma")$numero_texto_moneda), "")
  # Control: la unidad en minusculas se sigue contando.
  testthat::expect_identical(as.integer(fila("peso")$n_numeros_texto), 5L)
  testthat::expect_identical(as.character(fila("peso")$numero_texto_unidad), "kg")
  testthat::expect_true("numero_como_texto" %in% hallazgos_de("peso"))
}

testthat::test_that("DuckDB perfila los numeros como texto con el lector comun", {
  testthat::skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .comprobar_numero_texto_dbi(con)
})

testthat::test_that("SQLite perfila los numeros como texto con el lector comun", {
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .comprobar_numero_texto_dbi(con)
})
