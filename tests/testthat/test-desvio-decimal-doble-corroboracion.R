# El desvio de un decimal que la muestra lee como doble, por la via por omision.
#
# `resumen_tabla` calcula el desvio en el motor, restando en el tipo de la
# columna: es el exacto. `perfil_muestra` lo calcula en R sobre lo que entrego
# el controlador, y un `DECIMAL(18,2)` de 5e14 llega como el doble de su texto:
# 3,30971 contra 3,32050. La corroboracion lo contaba como divergencia, y
# `perfilar_coleccion()` subia la tabla a `alcance = "divergencias"`, en
# PostgreSQL, MySQL, MariaDB, DuckDB y SQL Server. La muestra no publica ese
# desvio, lo declara, y la corroboracion no lo compara (causa `decimal_doble`).
# El control: un desvio que el motor calcula mal sigue siendo divergencia.
testthat::skip_on_cran()
skip_if_not_installed("DBI")
skip_if_not_installed("duckdb")

.ddc_centavos <- c("0.60", "3.15", "7.20", "1.05", "9.90", "4.45", "2.30",
                   "8.75", "5.10")

.ddc_crear <- function(con) {
  # 5e14 mas los centavos: 500000000000000.60, 500000000000003.15, ...
  anchos <- paste0("5000000000000", sprintf("%05.2f", as.numeric(.ddc_centavos)))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ddc_t (ancho DECIMAL(18,2), chico DECIMAL(18,2))"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ddc_t VALUES ",
    paste0("(", anchos, ", ", .ddc_centavos, ")", collapse = ", ")
  ))
  invisible(anchos)
}

.ddc_desvio <- function(perfil, bloque, columna) {
  columnas <- perfil[[bloque]]$columnas
  as.numeric(columnas$desvio[columnas$columna == columna])
}

test_that("el desvio de un decimal leido como doble no se cuenta como divergencia", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  .ddc_crear(con)
  exacto <- stats::sd(as.numeric(.ddc_centavos))
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "ddc_t", proteger_datos_personales = FALSE
  )))
  # El resumen sigue publicando el exacto.
  expect_equal(.ddc_desvio(perfil, "resumen_tabla", "ancho"), exacto,
               tolerance = 1e-12)
  # La muestra no publica el de los dobles, y lo declara.
  expect_true(is.na(.ddc_desvio(perfil, "perfil_muestra", "ancho")))
  expect_identical(perfil$perfil_muestra$meta$desvio_decimal_doble, "ancho")
  # La columna chica no se toca: su desvio sale bien de los dobles.
  expect_equal(.ddc_desvio(perfil, "perfil_muestra", "chico"), exacto,
               tolerance = 1e-12)
  corroboracion <- perfil$resumen_tabla$meta$corroboracion_bloques
  expect_identical(corroboracion$estado, "comparacion_completa")
  expect_identical(as.integer(corroboracion$divergencias), 0L)
  nc <- corroboracion$no_comparadas
  expect_identical(
    nc$causa[nc$columna == "ancho" & nc$metrica == "desvio"], "decimal_doble"
  )
  expect_false(any(nc$columna == "chico" & nc$metrica == "desvio"))
  expect_false(any(perfil$resumen_tabla$cobertura$estado == "divergencia"))

  coleccion_perfil <- suppressWarnings(suppressMessages(perfilar_coleccion(
    coleccion(con, "ddc_t", nombre = "ddc"), proteger_datos_personales = FALSE
  )))
  expect_false(any(coleccion_perfil$cobertura_coleccion$alcance == "divergencias"))
})

# Una conexion que hace al motor calcular mal el desvio de `chico` (un uno por
# ciento de mas), y la del decimal ancho tambien. El desvio de un decimal sale
# de la suma exacta de los cuadrados de las diferencias con un centro; la
# conexion la multiplica por 1,0201, y el desvio queda a una parte en cien
# millones de 1,01 veces el exacto (la suma de las diferencias no se toca).
# Tambien falsea `STDDEV_SAMP()`, por si la via vuelve a pedirselo al motor.
library(DBI)
if (!methods::isClass("conexion_duckdb_desvio_falso")) {
  setClass("conexion_duckdb_desvio_falso", contains = "duckdb_connection")
}
# `dbGetQuery()` pasa por `dbSendQuery()`: la reescritura no se repite.
.ddc_falsear <- function(statement) {
  statement <- gsub(
    "SUM\\((?!1\\.0201 \\* )CAST\\(\\((chico|ancho) - ",
    "SUM(1.0201 * CAST((\\1 - ", statement, perl = TRUE
  )
  gsub("(?<!1\\.01 \\* )STDDEV_SAMP\\((chico|ancho)", "1.01 * STDDEV_SAMP(\\1",
       statement, perl = TRUE)
}
setMethod(
  "dbSendQuery", c("conexion_duckdb_desvio_falso", "character"),
  function(conn, statement, ...) callNextMethod(conn, .ddc_falsear(statement), ...)
)
setMethod(
  "dbGetQuery", c("conexion_duckdb_desvio_falso", "character"),
  function(conn, statement, ...) callNextMethod(conn, .ddc_falsear(statement), ...)
)

test_that("un desvio que el motor calcula mal sigue siendo divergencia", {
  cruda <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(cruda, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  .ddc_crear(cruda)
  con <- methods::new("conexion_duckdb_desvio_falso")
  for (ranura in methods::slotNames(cruda)) {
    methods::slot(con, ranura) <- methods::slot(cruda, ranura)
  }
  exacto <- stats::sd(as.numeric(.ddc_centavos))
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "ddc_t", proteger_datos_personales = FALSE
  )))
  # La conexion falseo de verdad el desvio del motor.
  expect_equal(.ddc_desvio(perfil, "resumen_tabla", "chico"), 1.01 * exacto,
               tolerance = 1e-7)
  corroboracion <- perfil$resumen_tabla$meta$corroboracion_bloques
  expect_identical(as.integer(corroboracion$divergencias), 1L)
  cobertura <- perfil$resumen_tabla$cobertura
  divergencias <- cobertura[cobertura$estado == "divergencia", , drop = FALSE]
  expect_equal(nrow(divergencias), 1L)
  expect_true(grepl("chico", divergencias$elemento, fixed = TRUE, useBytes = TRUE))
  expect_true(grepl("desvio", divergencias$motivo, fixed = TRUE, useBytes = TRUE))
  # El del decimal ancho no se puede corroborar contra la muestra, y se dice.
  nc <- corroboracion$no_comparadas
  expect_identical(
    nc$causa[nc$columna == "ancho" & nc$metrica == "desvio"], "decimal_doble"
  )
})
