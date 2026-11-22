# Una sonda de muestreo rechazada no debe borrar la razon de las consultas que
# el plan habia contado. El caso usa una sola candidata para que la falla deje
# la muestra no disponible de forma determinista.
testthat::skip_on_cran()

test_that("la cuenta de consultas explica las clases omitidas por una muestra fallida", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  conexion <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(conexion, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  DBI::dbExecute(conexion,
                 "CREATE TABLE conc (d DECIMAL(18,2), f DOUBLE)")
  DBI::dbExecute(conexion, paste(
    "INSERT INTO conc SELECT CAST(CAST(i AS DECIMAL(18,2)) / 100 AS",
    "DECIMAL(18,2)), i * 0.5 FROM range(1, 4001) AS q(i)"
  ))

  reales <- asNamespace("lupa")
  consultar_real <- get(".consultar_dbi", envir = reales)
  candidatos_real <- get(".candidatos_muestreo_dbi", envir = reales)
  primera_sonda <- TRUE
  testthat::local_mocked_bindings(
    .candidatos_muestreo_dbi = function(...) candidatos_real(...)[1L],
    .consultar_dbi = function(conexion, sql, presupuesto = NULL, filas = -1L,
                              etapa = "consulta") {
      if (primera_sonda && identical(etapa, "sonda_muestreo")) {
        primera_sonda <<- FALSE
        return(consultar_real(
          conexion, "SELECT FUNCION_INVALIDA_LUPA()", presupuesto,
          filas = filas, etapa = etapa
        ))
      }
      consultar_real(conexion, sql, presupuesto, filas, etapa)
    },
    .package = "lupa"
  )

  argumentos <- list(
    conexion, "conc", universo = "muestra_motor", muestra_motor = 100L,
    bloque_muestra = "solo_agregados"
  )
  plan <- suppressWarnings(suppressMessages(do.call(
    plan_perfilado_dbi, argumentos
  )))
  perfil <- suppressWarnings(suppressMessages(do.call(
    perfilar_dbi, c(argumentos, list(proteger_datos_personales = FALSE))
  )))
  consultas <- perfil$resumen_tabla$meta$consultas
  conciliacion <- consultas$conciliacion

  expect_gt(as.numeric(attr(plan, "total")), consultas$emitidas)
  expect_identical(conciliacion$estado, "desfasado")
  expect_identical(as.numeric(conciliacion$anunciadas),
                   as.numeric(attr(plan, "total")))
  expect_identical(as.numeric(conciliacion$emitidas),
                   as.numeric(consultas$emitidas))
  expect_true(any(conciliacion$detalle$direccion == "no_emitida"))
  expect_true(any(grepl(
    "muestra del motor no quedo disponible",
    conciliacion$detalle$motivo, fixed = TRUE, useBytes = TRUE
  )))
  expect_true(any(grepl(
    "sonda fue rechazada", conciliacion$detalle$motivo,
    fixed = TRUE, useBytes = TRUE
  )))
  expect_true(any(
    conciliacion$detalle$clase_consulta == "mediana (orden total + limite/salto)"
  ))
  expect_equal(nrow(consultas$sondas_fallidas), 1L)
})
