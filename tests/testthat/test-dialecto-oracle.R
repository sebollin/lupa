skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

library(DBI)

.oracle_dbi_prueba <- new.env(parent = emptyenv())
.oracle_dbi_prueba$sql <- character()

if (!methods::isClass("ConexionOracleLupa")) {
  setClass("ConexionOracleLupa", contains = "SQLiteConnection")
}

.envolver_oracle_lupa <- function(conexion) {
  salida <- methods::new("ConexionOracleLupa")
  for (ranura in methods::slotNames(conexion)) {
    methods::slot(salida, ranura) <- methods::slot(conexion, ranura)
  }
  salida
}

.reiniciar_oracle_lupa <- function() {
  .oracle_dbi_prueba$sql <- character()
  invisible(NULL)
}

.traducir_oracle_lupa <- function(statement) {
  salida <- statement
  salida <- gsub(
    '"SYSTEM"."TABLA"', '"TABLA"', salida, fixed = TRUE
  )
  salida <- gsub(
    "`SYSTEM`.`TABLA`", "`TABLA`", salida, fixed = TRUE
  )
  salida <- gsub(
    " FETCH FIRST ([0-9]+) ROWS ONLY", " LIMIT \\1", salida,
    ignore.case = TRUE, perl = TRUE
  )
  salida <- gsub(
    " SAMPLE \\([^)]*\\)", "", salida, ignore.case = TRUE, perl = TRUE
  )
  salida <- gsub(
    " FROM DUAL\\b", "", salida, ignore.case = TRUE, perl = TRUE
  )
  salida
}

setMethod(
  "dbIsValid", "ConexionOracleLupa",
  function(dbObj, ...) stop("El controlador simulado no expone dbIsValid.")
)
setMethod(
  "dbExistsTable", c("ConexionOracleLupa", "character"),
  function(conn, name, ...) FALSE
)
setMethod(
  "dbExistsTable", c("ConexionOracleLupa", "Id"),
  function(conn, name, ...) FALSE
)
setMethod(
  "dbListFields", c("ConexionOracleLupa", "character"),
  function(conn, name, ...) c("ID", "VALOR")
)
setMethod(
  "dbListFields", c("ConexionOracleLupa", "Id"),
  function(conn, name, ...) c("ID", "VALOR")
)
setMethod(
  "dbGetQuery", c("ConexionOracleLupa", "character"),
  function(conn, statement, ...) {
    .oracle_dbi_prueba$sql <- c(.oracle_dbi_prueba$sql, statement)
    if (grepl("STDDEV_SAMP\\(1\\.0\\)", statement, perl = TRUE)) {
      if (!grepl("FROM DUAL", statement, fixed = TRUE)) {
        stop("Oracle exige FROM DUAL para una consulta sin tabla.")
      }
      return(data.frame(DESVIO = 1))
    }
    if (grepl("TABLESAMPLE", statement, ignore.case = TRUE)) {
      stop("TABLESAMPLE no es una sintaxis Oracle.")
    }
    callNextMethod(conn, .traducir_oracle_lupa(statement), ...)
  }
)
setMethod(
  "dbSendQuery", c("ConexionOracleLupa", "character"),
  function(conn, statement, ...) {
    .oracle_dbi_prueba$sql <- c(.oracle_dbi_prueba$sql, statement)
    if (grepl("TABLESAMPLE", statement, ignore.case = TRUE)) {
      stop("TABLESAMPLE no es una sintaxis Oracle.")
    }
    callNextMethod(conn, .traducir_oracle_lupa(statement), ...)
  }
)

.conexion_oracle_lupa <- function() {
  cruda <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(cruda, "TABLA", data.frame(ID = 1:20, VALOR = 21:40))
  list(cruda = cruda, envuelta = .envolver_oracle_lupa(cruda))
}

test_that("la sonda de desvio agrega DUAL en Oracle", {
  bases <- .conexion_oracle_lupa()
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)
  .reiniciar_oracle_lupa()

  presupuesto <- lupa:::.presupuesto_dbi()
  alias <- function(nombre) as.character(DBI::dbQuoteIdentifier(
    bases$envuelta, nombre
  ))
  lupa:::.sondar_forma_desvio(bases$envuelta, presupuesto, alias)

  expect_identical(presupuesto$forma_desvio, 1L)
  expect_true(any(grepl("FROM DUAL", .oracle_dbi_prueba$sql, fixed = TRUE)))
  expect_false(any(grepl("WHERE 1 = 0", .oracle_dbi_prueba$sql, fixed = TRUE)))
})

test_that("Oracle sondea y emite SAMPLE con su sintaxis propia", {
  bases <- .conexion_oracle_lupa()
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)
  .reiniciar_oracle_lupa()

  dialecto <- lupa:::.dialectos_dbi()$fetch_first
  presupuesto <- lupa:::.presupuesto_dbi()
  resolucion <- lupa:::.sondar_muestreo_dbi(
    bases$envuelta, '"TABLA"', dialecto, presupuesto
  )
  expect_true(resolucion$disponible)
  expect_identical(resolucion$candidato$nombre, "oracle_sample")
  expect_true(any(grepl("SAMPLE (1)", resolucion$sondas, fixed = TRUE)))
  expect_false(any(grepl("SAMPLE.*WHERE 1 = 0", resolucion$sondas)))

  forma <- lupa:::.fuente_muestreada_dbi(
    '"TABLA"', c('"ID"', '"VALOR"'), 10, 20, dialecto,
    resolucion
  )
  expect_match(forma$sql, "SAMPLE \\([[:space:]]*50\\)")
  expect_match(forma$sql, "FETCH FIRST 10 ROWS ONLY", fixed = TRUE)
})

test_that("un nombre calificado funciona con texto e Id aunque el driver no lo enumere", {
  bases <- .conexion_oracle_lupa()
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  texto <- perfilar_dbi(
    bases$envuelta, "SYSTEM.TABLA", universo = "tabla_completa",
    metricas = "validos", estrategia_mediana = "exacta", muestra = 2,
    proteger_datos_personales = FALSE
  )
  por_id <- perfilar_dbi(
    bases$envuelta, DBI::Id(schema = "SYSTEM", table = "TABLA"),
    universo = "tabla_completa", metricas = "validos",
    estrategia_mediana = "exacta", muestra = 2,
  )

  expect_equal(texto$resumen_tabla$meta$filas, 20)
  expect_equal(por_id$resumen_tabla$meta$filas, 20)
  expect_equal(
    texto$resumen_tabla$columnas$columna,
    por_id$resumen_tabla$columnas$columna
  )
})
