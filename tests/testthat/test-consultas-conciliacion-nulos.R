testthat::skip_on_cran()

test_that("una tabla sin valores explica las consultas que no hacen falta", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  suppressPackageStartupMessages(library(DBI))
  clase <- "LupaNulosFalla"
  if (!methods::isClass(clase)) {
    methods::setClass(clase, contains = "SQLiteConnection")
    rechazar <- function(sql) {
      patrones <- getOption("lupa.nulos.patrones")
      if (length(patrones)) {
        for (patron in patrones) {
          if (grepl(patron, sql, fixed = TRUE, useBytes = TRUE)) {
            stop("rechazada por prueba: ", patron, call. = FALSE)
          }
        }
      }
    }
    for (nombre in c(
      "dbGetQuery", "dbSendQuery", "dbExecute", "dbSendStatement"
    )) {
      methods::setMethod(nombre, signature(clase, "character"),
        function(conn, statement, ...) {
          rechazar(statement)
          callNextMethod()
        })
    }
  }
  conexion_base <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion_base), add = TRUE)
  DBI::dbWriteTable(conexion_base, "nulos", data.frame(
    a = rep(NA_real_, 10), b = rep(NA_character_, 10),
    c = rep(NA_integer_, 10)
  ))
  conexion <- methods::new(clase, conexion_base)
  opciones <- options(lupa.nulos.patrones = c(
    "information_schema.table_constraints", "STDDEV_SAMP"
  ))
  on.exit(options(opciones), add = TRUE)

  configuraciones <- list(
    list(), list(bloque_muestra = "solo_agregados")
  )
  for (argumentos in configuraciones) {
    perfil <- suppressWarnings(suppressMessages(do.call(
      perfilar_dbi, c(list(conexion, "nulos"), argumentos)
    )))
    conciliacion <- perfil$resumen_tabla$meta$consultas$conciliacion
    motivos <- paste(as.character(conciliacion$detalle$motivo), collapse = " ")
    expect_identical(conciliacion$estado, "desfasado")
    expect_equal(as.numeric(conciliacion$diferencia), -2)
    expect_true(grepl("no hay valores no nulos", motivos,
                      fixed = TRUE, useBytes = TRUE))
    patron_interno <- paste0("clave_primaria|", paste0("son", "da_desvio"))
    expect_false(grepl(patron_interno, motivos,
                       perl = TRUE, useBytes = TRUE))
    expect_true(grepl("no hacen falta", motivos,
                      fixed = TRUE, useBytes = TRUE))
    expect_equal(anyDuplicated(conciliacion$detalle$clase_consulta), 0L)
  }
})

test_that("una clase repetida queda una sola vez en el detalle", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  suppressPackageStartupMessages(library(DBI))
  clase <- "LupaNulosFalla"
  if (!methods::isClass(clase)) {
    methods::setClass(clase, contains = "SQLiteConnection")
  }
  conexion_base <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion_base), add = TRUE)
  DBI::dbWriteTable(conexion_base, "t", data.frame(
    id = 1:60, x = seq_len(60) / 10,
    g = rep(letters[1:4], length.out = 60),
    stringsAsFactors = FALSE
  ))
  conexion <- methods::new(clase, conexion_base)
  opciones <- options(lupa.nulos.patrones = c("COUNT(DISTINCT", "MAX("))
  on.exit(options(opciones), add = TRUE)

  for (maximo in list(NULL, 12L)) {
    argumentos <- list(conexion, "t")
    if (!is.null(maximo)) argumentos$max_consultas <- maximo
    perfil <- suppressWarnings(suppressMessages(do.call(
      perfilar_dbi, argumentos
    )))
    consultas <- perfil$resumen_tabla$meta$consultas
    detalle <- consultas$conciliacion$detalle
    expect_equal(anyDuplicated(detalle$clase_consulta), 0L)
    expect_equal(
      sum(as.numeric(detalle$diferencia)),
      as.numeric(consultas$conciliacion$diferencia)
    )
    if (!is.null(maximo)) {
      agregados <- detalle[grepl("agregados planos", detalle$clase_consulta,
                                 fixed = TRUE, useBytes = TRUE), , drop = FALSE]
      expect_equal(nrow(agregados), 1L)
      expect_identical(agregados$direccion, "mixta")
      expect_true(grepl("presupuesto", agregados$motivo,
                        fixed = TRUE, useBytes = TRUE))
      expect_true(grepl("respaldo", agregados$motivo,
                        fixed = TRUE, useBytes = TRUE))
    }
  }
})
