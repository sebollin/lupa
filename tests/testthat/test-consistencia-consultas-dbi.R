skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

library(DBI)

.consistencia_dbi <- new.env(parent = emptyenv())
.consistencia_dbi$accion <- "ninguna"
.consistencia_dbi$mutada <- FALSE
.consistencia_dbi$cruda <- NULL

if (!methods::isClass("ConexionConsistenciaLupa")) {
  setClass("ConexionConsistenciaLupa", contains = "SQLiteConnection")
}

setMethod(
  "dbGetQuery", c("ConexionConsistenciaLupa", "character"),
  function(conn, statement, ...) {
    resultado <- callNextMethod(conn, statement, ...)
    cambia <- grepl("n_total_consulta", statement, fixed = TRUE) &&
      !isTRUE(.consistencia_dbi$mutada)
    if (cambia && identical(.consistencia_dbi$accion, "borrar")) {
      DBI::dbExecute(
        .consistencia_dbi$cruda, "DELETE FROM datos WHERE rowid > 50"
      )
      .consistencia_dbi$mutada <- TRUE
    }
    if (cambia && identical(.consistencia_dbi$accion, "insertar")) {
      DBI::dbExecute(
        .consistencia_dbi$cruda,
        "INSERT INTO datos (z) VALUES (2001), (2002), (2003), (2004), (2005), (2006), (2007), (2008), (2009), (2010), (2011)"
      )
      .consistencia_dbi$mutada <- TRUE
    }
    resultado
  }
)

.envolver_consistencia_dbi <- function(conexion) {
  salida <- methods::new("ConexionConsistenciaLupa")
  for (ranura in methods::slotNames(conexion)) {
    methods::slot(salida, ranura) <- methods::slot(conexion, ranura)
  }
  salida
}

.conexion_consistencia_dbi <- function(datos, accion) {
  cruda <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(cruda, "datos", datos)
  .consistencia_dbi$accion <- accion
  .consistencia_dbi$mutada <- FALSE
  .consistencia_dbi$cruda <- cruda
  list(cruda = cruda, conexion = .envolver_consistencia_dbi(cruda))
}

.cerrar_consistencia_dbi <- function(conexion) {
  suppressWarnings(try(DBI::dbDisconnect(conexion$conexion), silent = TRUE))
  suppressWarnings(try(DBI::dbDisconnect(conexion$cruda), silent = TRUE))
}

test_that("la completitud usa el total de la consulta de cada lote", {
  bases <- .conexion_consistencia_dbi(
    data.frame(a = rep(1, 100), b = rep(1, 100)), "borrar"
  )
  on.exit(.cerrar_consistencia_dbi(bases), add = TRUE)

  resultado <- perfilar_dbi(
    bases$conexion, "datos", metricas = "validos", tamano_lote_planos = 1L,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
  )
  filas <- resultado$resumen_tabla$columnas
  fila_b <- filas[filas$columna == "b", , drop = FALSE]
  registros <- resultado$resumen_tabla$sql
  consultas_locales <- registros[
    registros$metrica == "n_validos" & !is.na(registros$sql), , drop = FALSE
  ]

  expect_equal(fila_b$n_validos, 50)
  expect_equal(fila_b$n_faltantes, 0)
  expect_equal(fila_b$prop_faltantes, 0)
  expect_equal(length(unique(consultas_locales$consulta_id)), 2L)
  expect_true(all(grepl("COUNT(*) AS `n_total_consulta`", consultas_locales$sql,
                        fixed = TRUE)))
  expect_false(any(grepl(
    "SELECT COUNT(*) AS `lupa_n_total`", registros$sql, fixed = TRUE
  )))
})

test_that("la cota de distintos usa el guardian de su misma consulta", {
  bases <- .conexion_consistencia_dbi(
    data.frame(z = c(rep(1, 10), rep(NA, 90))), "insertar"
  )
  on.exit(.cerrar_consistencia_dbi(bases), add = TRUE)

  resultado <- perfilar_dbi(
    bases$conexion, "datos", metricas = c("validos", "distintos"),
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
  )
  fila <- resultado$resumen_tabla$columnas
  distinto <- resultado$resumen_tabla$sql[
    resultado$resumen_tabla$sql$metrica == "n_distintos", , drop = FALSE
  ]

  expect_equal(fila$n_validos, 10)
  expect_equal(fila$n_distintos, 12)
  expect_equal(fila$tasa_distintos, 12 / 21)
  expect_true(all(distinto$estado == "calculado"))
  expect_false(any(grepl("El motor informo", distinto$motivo, fixed = TRUE)))
  expect_true(all(grepl("n_validos_guard", distinto$sql, fixed = TRUE)))
  expect_equal(length(unique(distinto$consulta_id)), 1L)
})
