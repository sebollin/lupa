skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

library(DBI)

.conexion_invariante_dbi <- function() {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(conexion, "t", data.frame(x = 1:4))
  conexion
}

.aproximacion_solo_consulta_dbi <- function() {
  list(
    nombre = "aprox_solo_consulta",
    error_esperado = "desconocido",
    construir = function(expr, tabla, alias) paste0(
      "SELECT COUNT(", expr, ") AS ", alias, " FROM ", tabla
    )
  )
}

.aproximacion_con_expresion_dbi <- function() {
  list(
    nombre = "aprox_con_expresion",
    error_esperado = "desconocido",
    construir = function(expr, tabla, alias) paste0(
      "SELECT COUNT(", expr, ") AS ", alias, " FROM ", tabla
    ),
    expresion = function(expr, alias) paste0(
      "COUNT(", expr, ") AS ", alias
    )
  )
}

test_that("un resultado fallido nunca se publica como medido", {
  estados_invalidos <- c("calculado", "observado", "observado_muestra", "estimado")
  for (estado in estados_invalidos) {
    resultado <- list(
      ok = FALSE, valor = NULL, estado = estado,
      motivo = "la consulta no se emitio", sql = NA_character_
    )
    registro <- .registrar_resultado_dbi(
      list(), "x", "n_distintos", resultado
    )[[1L]]
    expect_identical(registro$estado, "no_disponible", info = estado)
  }
})

test_that("las omisiones deliberadas conservan su estado propio", {
  registro <- .metricas_omitidas_dbi(
    list(), "x", "mediana", "no_solicitado", "no se pidio"
  )[[1L]]
  expect_identical(registro$estado, "no_solicitado")
})

test_that("sin expresion aproximada no se consolida el conteo", {
  conexion <- .conexion_invariante_dbi()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  aproximacion <- .aproximacion_solo_consulta_dbi()

  resultado <- .conteos_columna_dbi(
    conexion, "t", "x", identity, TRUE, TRUE,
    aproximacion_distintos = aproximacion
  )

  expect_false(resultado$consolidada)
  expect_match(resultado$validos$sql, "COUNT(x)", fixed = TRUE)
  expect_match(resultado$distintos$sql, "COUNT(x)", fixed = TRUE)
  expect_false(grepl("COUNT(DISTINCT", resultado$distintos$sql, fixed = TRUE))
  expect_identical(resultado$distintos$estado, "estimado")
  expect_identical(
    resultado$distintos$metadatos$metodo,
    "aprox_solo_consulta"
  )

  lote <- .conteos_distintos_lote_dbi(
    conexion, "t", "x", c(x = "x"), identity, 1L, NULL,
    aproximacion_distintos = aproximacion
  )
  resultado_lote <- lote$resultados$x$distintos
  expect_false(lote$resultados$x$consolidada)
  expect_match(resultado_lote$sql, "COUNT(x)", fixed = TRUE)
  expect_false(grepl("COUNT(DISTINCT", resultado_lote$sql, fixed = TRUE))
  expect_identical(resultado_lote$estado, "estimado")
})

test_that("la expresion aproximada determina el SQL consolidado", {
  conexion <- .conexion_invariante_dbi()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  resultado <- .conteos_columna_dbi(
    conexion, "t", "x", identity, TRUE, TRUE,
    aproximacion_distintos = .aproximacion_con_expresion_dbi()
  )

  expect_true(resultado$consolidada)
  expect_true(grepl("COUNT(x)", resultado$distintos$sql, fixed = TRUE))
  expect_false(grepl("COUNT(DISTINCT", resultado$distintos$sql, fixed = TRUE))
  expect_identical(resultado$distintos$estado, "estimado")
})

test_that("un lote no emitido no se convierte en estimacion", {
  conexion <- .conexion_invariante_dbi()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  presupuesto <- .presupuesto_dbi(0)
  lote <- .conteos_distintos_lote_dbi(
    conexion, "t", "x", c(x = "x"), identity, 1L, presupuesto,
    aproximacion_distintos = .aproximacion_con_expresion_dbi()
  )
  resultado <- lote$resultados$x$distintos

  expect_false(resultado$ok)
  expect_null(resultado$estado)
  registro <- .registrar_resultado_dbi(
    list(), "x", "n_distintos", resultado,
    metadatos = .metadatos_sql_dbi()
  )[[1L]]
  expect_identical(registro$estado, "no_disponible")
})

test_that("la cota de distintos se rechaza antes del unico registro", {
  conexion <- .conexion_invariante_dbi()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  validos <- list(ok = TRUE, valor = 3, sql = "SELECT COUNT(x)")
  distintos <- list(
    ok = TRUE, valor = 4, estado = "estimado",
    sql = "SELECT APPROX_COUNT_DISTINCT(x)",
    consulta_id = 2L,
    metadatos = list(
      metodo = "aprox_con_expresion", error_esperado = "desconocido",
      n_validos_guard = 3, consulta_id_guard = 2L,
      cota_comprobable = TRUE
    )
  )
  agregados <- list(
    conteos = list(x = list(validos = validos, distintos = distintos)),
    basicos = list(), desvio = list()
  )

  resultado <- .resumen_columna_dbi(
    conexion, "t", "x", 1, 3,
    metricas = c("validos", "distintos"), agregados = agregados
  )
  registros <- resultado$sql[
    resultado$sql$metrica %in% c("n_distintos", "tasa_distintos"), ,
    drop = FALSE
  ]

  expect_equal(nrow(registros), 2L)
  expect_true(all(registros$estado == "no_disponible"))
  expect_true(all(grepl("4 valores distintos sobre 3", registros$motivo,
                        fixed = TRUE)))
  expect_true(is.na(resultado$fila$n_distintos))
})

test_that("una mediana aproximada fallida tampoco se declara estimada", {
  conexion <- .conexion_invariante_dbi()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  aproximacion <- list(
    nombre = "aprox_mediana_fallida",
    error_esperado = "desconocido",
    construir = function(expr, tabla, alias) paste0(
      "SELECT funcion_inexistente(", expr, ") AS ", alias,
      " FROM ", tabla
    )
  )
  resultado <- .resumen_columna_dbi(
    conexion, "t", "x", 1, 4, metricas = "mediana",
    aproximacion_mediana = aproximacion
  )
  registro <- resultado$sql[resultado$sql$metrica == "mediana", , drop = FALSE]

  expect_equal(nrow(registro), 1L)
  expect_identical(registro$estado, "no_disponible")
})

test_that("una mediana consolidada conserva el metodo que se ejecuto", {
  conexion <- .conexion_invariante_dbi()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  aproximacion <- list(
    nombre = "aprox_mediana_consolidada",
    error_esperado = "desconocido",
    expresion = function(expr, alias) paste0("AVG(", expr, ") AS ", alias),
    construir_multiple = function(expresiones, tabla) paste0(
      "SELECT ", paste(expresiones, collapse = ", "), " FROM ", tabla
    )
  )
  resultado <- .medianas_lote_consolidadas_dbi(
    conexion, "t", "x", c(x = "x"), identity, 1L, aproximacion,
    NULL, estado = "estimado"
  )$resultados$x

  expect_true(resultado$ok)
  expect_identical(resultado$estado, "estimado")
  expect_identical(
    resultado$metadatos$metodo,
    "aprox_mediana_consolidada"
  )
  expect_identical(resultado$metadatos$error_esperado, "desconocido")
})
