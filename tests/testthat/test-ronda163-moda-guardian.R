skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

library(DBI)

.conexion_moda_sin_ventana <- function() {
  if (!methods::isClass("ConexionModaSinVentanaLupa")) {
    methods::setClass(
      "ConexionModaSinVentanaLupa", contains = "SQLiteConnection"
    )
  }
  methods::setMethod(
    "dbSendQuery", c("ConexionModaSinVentanaLupa", "character"),
    function(conn, statement, ...) {
      if (grepl("SUM\\(COUNT\\(\\*\\)\\) OVER", statement)) {
        stop("la ventana no esta disponible", call. = FALSE)
      }
      callNextMethod()
    }
  )
  cruda <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(cruda, "datos", data.frame(
    valor = c(rep("a", 3), rep("b", 2)),
    stringsAsFactors = FALSE
  ))
  conexion <- methods::new("ConexionModaSinVentanaLupa")
  for (ranura in methods::slotNames(cruda)) {
    methods::slot(conexion, ranura) <- methods::slot(cruda, ranura)
  }
  list(cruda = cruda, conexion = conexion)
}

.cerrar_moda_sin_ventana <- function(bases) {
  suppressWarnings(try(DBI::dbDisconnect(bases$conexion), silent = TRUE))
  suppressWarnings(try(DBI::dbDisconnect(bases$cruda), silent = TRUE))
}

.argumentos_moda_guardian <- list(
  bloque_muestra = "solo_agregados", instrumentar = FALSE,
  proteger_datos_personales = FALSE, analizar_dependencias = FALSE,
  casi_duplicados_vocabulario = FALSE, ausencia_estructural = FALSE,
  duplicados_aproximados = FALSE
)

test_that("la moda trae su guardian en la misma sentencia", {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "datos", data.frame(
    valor = c(rep("a", 3), rep("b", 2), NA_character_),
    stringsAsFactors = FALSE
  ))

  resultado <- do.call(
    perfilar_dbi,
    c(list(conexion, "datos", metricas = c("validos", "moda")),
      .argumentos_moda_guardian)
  )
  fila <- resultado$resumen_tabla$columnas
  moda <- resultado$resumen_tabla$sql[
    resultado$resumen_tabla$sql$metrica == "moda", , drop = FALSE
  ]
  guardian <- resultado$resumen_tabla$meta$moda_guardian

  expect_equal(fila$moda, "a")
  expect_equal(fila$frecuencia_moda, 3)
  expect_true(guardian$disponible)
  expect_identical(guardian$metodo, "ventana_agregado")
  expect_true(grepl("SUM(COUNT(*) ) OVER", moda$sql, fixed = TRUE) ||
              grepl("SUM(COUNT(*)) OVER", moda$sql, fixed = TRUE))
  expect_true(grepl("n_validos_guard", moda$sql, fixed = TRUE))
  expect_match(moda$motivo, "se comprobo", ignore.case = TRUE)
  expect_false(grepl("No se pudo comprobar", moda$motivo, fixed = TRUE))
  expect_equal(length(unique(moda$consulta_id)), 1L)
})

test_that("una sonda rechazada conserva la consulta de moda anterior", {
  bases <- .conexion_moda_sin_ventana()
  on.exit(.cerrar_moda_sin_ventana(bases), add = TRUE)

  resultado <- do.call(
    perfilar_dbi,
    c(list(bases$conexion, "datos", metricas = c("validos", "moda")),
      .argumentos_moda_guardian)
  )
  moda <- resultado$resumen_tabla$sql[
    resultado$resumen_tabla$sql$metrica == "moda", , drop = FALSE
  ]
  guardian <- resultado$resumen_tabla$meta$moda_guardian

  expect_false(guardian$disponible)
  expect_match(guardian$motivo, "rechazo", ignore.case = TRUE)
  expect_false(grepl("n_validos_guard", moda$sql, fixed = TRUE))
  expect_match(moda$metodo, "sin_guardian", fixed = TRUE)
  expect_match(moda$motivo, "No se pudo comprobar", fixed = TRUE)
})

test_that("una frecuencia imposible se descarta antes de registrarse", {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  moda <- list(
    ok = TRUE,
    datos = data.frame(valor = "a", frecuencia = 4, n_validos_guard = 3),
    sql = "SELECT moda",
    consulta_id = 1L,
    metadatos = list(
      n_validos_guard = 3, consulta_id_guard = 1L,
      cota_comprobable = TRUE
    )
  )
  resultado <- .resumen_columna_dbi(
    conexion, "datos", "valor", character(), 3,
    metricas = "moda", moda_precalculada = moda
  )
  registro <- resultado$sql[resultado$sql$metrica == "moda", , drop = FALSE]

  expect_true(is.na(resultado$fila$moda))
  expect_true(is.na(resultado$fila$frecuencia_moda))
  expect_identical(registro$estado, "no_disponible")
  expect_match(registro$motivo, "supera los 3", fixed = TRUE)
})
