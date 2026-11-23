testthat::skip_on_cran()

.capacidad_plan_conexion <- function(motor) {
  if (identical(motor, "sqlite")) {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  } else {
    DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  }
}

test_that("las capacidades y las guardas numericas tienen clases separadas", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  for (motor in c("sqlite", "duckdb")) {
    conexion <- .capacidad_plan_conexion(motor)
    DBI::dbWriteTable(conexion, "cen1", data.frame(
      id = 1:30, y = c(rep(-9, 5), seq_len(25) / 10),
      z = seq_len(30) / 7
    ))
    plan <- suppressWarnings(suppressMessages(
      lupa::plan_perfilado_dbi(conexion, "cen1", muestra = 10)
    ))
    fila_faltantes <- plan[
      plan$clase_consulta == "conteos de faltantes disfrazados", ,
      drop = FALSE
    ]
    fila_desvio <- plan[grepl(
      "^capacidad de desvio", plan$clase_consulta,
      perl = TRUE, useBytes = TRUE
    ), , drop = FALSE]
    expect_equal(nrow(fila_faltantes), 1L)
    expect_true(is.na(fila_faltantes$n_consultas))
    expect_equal(fila_faltantes$n_consultas_max, 3)
    expect_equal(nrow(fila_desvio), 1L)
    expect_equal(fila_desvio$n_consultas, 2)
    expect_equal(fila_desvio$n_consultas_max, 2)

    perfil <- suppressWarnings(suppressMessages(
      lupa::perfilar_dbi(conexion, "cen1", muestra = 10,
                         proteger_datos_personales = FALSE)
    ))
    publicado <- perfil$resumen_tabla$meta$plan
    conciliacion <- perfil$resumen_tabla$meta$consultas$conciliacion
    guardas_publicadas <- publicado[
      publicado$clase_consulta == "conteos de faltantes disfrazados", ,
      drop = FALSE
    ]
    expect_equal(guardas_publicadas$n_consultas, 1)
    expect_identical(conciliacion$estado, "conciliado")
    expect_false(any(grepl(
      paste0("son", "da_desvio"), conciliacion$detalle$motivo,
      fixed = TRUE, useBytes = TRUE
    )))
    try(DBI::dbDisconnect(
      conexion, shutdown = identical(motor, "duckdb")
    ), silent = TRUE)
  }
})

test_that("una tabla vacia publica el conocimiento observado del plan", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  for (motor in c("sqlite", "duckdb")) {
    conexion <- .capacidad_plan_conexion(motor)
    DBI::dbWriteTable(conexion, "vacia", data.frame(
      id = integer(), x = double(), g = character(),
      stringsAsFactors = FALSE
    ))
    perfil <- suppressWarnings(suppressMessages(
      lupa::perfilar_dbi(conexion, "vacia",
                         proteger_datos_personales = FALSE)
    ))
    publicado <- perfil$resumen_tabla$meta$plan
    medianas <- publicado[grepl(
      "^mediana \\(", publicado$clase_consulta,
      perl = TRUE, useBytes = TRUE
    ), , drop = FALSE]
    capacidades <- publicado[grepl(
      "^capacidad de ", publicado$clase_consulta,
      perl = TRUE, useBytes = TRUE
    ), , drop = FALSE]
    detalle <- perfil$resumen_tabla$meta$consultas$conciliacion$detalle
    detalle_con_diferencia <- detalle[detalle$diferencia != 0, , drop = FALSE]
    previo <- perfil$resumen_tabla$meta$consultas$conciliacion$plan_previo
    expect_equal(nrow(medianas), 1L)
    expect_equal(medianas$n_consultas, 0)
    expect_gte(medianas$n_consultas_max, 1)
    expect_true(any(grepl(
      "^capacidad de desvio", capacidades$clase_consulta,
      perl = TRUE, useBytes = TRUE
    )))
    expect_identical(
      perfil$resumen_tabla$meta$consultas$conciliacion$estado,
      "conciliado"
    )
    expect_equal(nrow(detalle_con_diferencia), 0L)
    expect_true(is.list(previo))
    previo_con_diferencia <- previo$detalle[previo$detalle$diferencia != 0, , drop = FALSE]
    expect_true(nrow(previo_con_diferencia) >= 1L)
    expect_true(any(grepl(
      "^mediana \\(", previo_con_diferencia$clase_consulta,
      perl = TRUE, useBytes = TRUE
    )))
    expect_false(any(grepl(
      "^portones \\(", detalle_con_diferencia$clase_consulta,
      perl = TRUE, useBytes = TRUE
    )))
    try(DBI::dbDisconnect(
      conexion, shutdown = identical(motor, "duckdb")
    ), silent = TRUE)
  }
})
