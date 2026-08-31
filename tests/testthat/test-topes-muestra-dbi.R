skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

library(DBI)

.tabla_topes_dbi <- function(n = 40L) {
  data.frame(
    id = seq_len(n),
    texto = paste0("fila-", sprintf("%03d", seq_len(n)),
                   paste(rep("x", 80L), collapse = "")),
    stringsAsFactors = FALSE
  )
}

.con_topes_dbi <- function(n = 40L) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "topes", .tabla_topes_dbi(n))
  con
}

.argumentos_perfil_topes_dbi <- list(
  modo = "conteos", analizar_dependencias = FALSE,
  ausencia_estructural = FALSE, casi_duplicados_vocabulario = FALSE,
  proteger_datos_personales = FALSE
)

test_that("el tope de celdas limita la lectura DBI y no los agregados SQL", {
  con <- .con_topes_dbi()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sin_topes <- do.call(
    perfilar_dbi,
    c(list(conexion = con, tabla = "topes", muestra = 20L,
           max_celdas_muestra = Inf, max_bytes_muestra = Inf),
      .argumentos_perfil_topes_dbi)
  )
  con_tope <- do.call(
    perfilar_dbi,
    c(list(conexion = con, tabla = "topes", muestra = 20L,
           max_celdas_muestra = 10L, max_bytes_muestra = Inf),
      .argumentos_perfil_topes_dbi)
  )

  expect_equal(nrow(con_tope$perfil_muestra$columnas), 2L)
  expect_true(all(con_tope$perfil_muestra$columnas$n == 5L))
  alcance <- con_tope$perfil_muestra$meta$origen_dbi$muestreo
  expect_match(alcance$sql_muestra, "LIMIT 5")
  expect_equal(con_tope$resumen_tabla$columnas,
               sin_topes$resumen_tabla$columnas)
  expect_equal(con_tope$resumen_tabla$columnas$n, rep(40, 2))

  cobertura <- con_tope$perfil_muestra$cobertura_diagnosticos
  cobertura <- cobertura[cobertura$diagnostico == "muestra_perfilado", ,
                         drop = FALSE]
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$motivo, "celdas observadas: 40")
  expect_match(cobertura$motivo, "manda el tope de celdas")
  resumen_cobertura <- con_tope$resumen_tabla$cobertura
  resumen_cobertura <- resumen_cobertura[
    resumen_cobertura$bloque == "perfil_muestra" &
      resumen_cobertura$estado == "degradado", , drop = FALSE
  ]
  expect_equal(nrow(resumen_cobertura), 1L)
  expect_identical(resumen_cobertura$motivo[[1L]], cobertura$motivo[[1L]])
  expect_identical(con_tope$perfil_muestra$meta$tope_que_mando, "celdas")
  expect_identical(
    con_tope$perfil_muestra$meta$tope_que_mando_texto,
    cobertura$motivo[[1L]]
  )
})

test_that("el tope de bytes sondea antes y limita la consulta final", {
  con <- .con_topes_dbi()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  limite <- as.numeric(object.size(DBI::dbGetQuery(
    con, "SELECT `id`, `texto` FROM `topes` LIMIT 5"
  )))

  resultado <- do.call(
    perfilar_dbi,
    c(list(conexion = con, tabla = "topes", muestra = 40L,
           max_celdas_muestra = Inf, max_bytes_muestra = limite),
      .argumentos_perfil_topes_dbi)
  )
  meta <- resultado$perfil_muestra$meta
  alcance <- resultado$perfil_muestra$meta$origen_dbi$muestreo

  expect_lt(meta$muestra_efectiva, 40)
  expect_lte(meta$bytes_muestra, limite)
  expect_gt(meta$bytes_sonda, meta$bytes_muestra)
  expect_match(alcance$sql_muestra, "LIMIT")
  expect_false(grepl("LIMIT 40$", alcance$sql_muestra))
  cobertura <- resultado$perfil_muestra$cobertura_diagnosticos
  cobertura <- cobertura[cobertura$diagnostico == "muestra_perfilado", ,
                         drop = FALSE]
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$motivo, "bytes observados")
  expect_match(cobertura$motivo, "manda el tope de bytes")
  resumen_cobertura <- resultado$resumen_tabla$cobertura
  resumen_cobertura <- resumen_cobertura[
    resumen_cobertura$bloque == "perfil_muestra" &
      resumen_cobertura$estado == "degradado", , drop = FALSE
  ]
  expect_equal(nrow(resumen_cobertura), 1L)
  expect_identical(resumen_cobertura$motivo[[1L]], cobertura$motivo[[1L]])
  expect_identical(resultado$perfil_muestra$meta$tope_que_mando, "bytes")
})

test_that("sin topes no se declara un recorte y solo_agregados no los aplica", {
  con <- .con_topes_dbi(20L)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  resultado <- do.call(
    perfilar_dbi,
    c(list(conexion = con, tabla = "topes", muestra = 5L,
           max_celdas_muestra = Inf, max_bytes_muestra = Inf),
      .argumentos_perfil_topes_dbi)
  )
  expect_equal(nrow(resultado$perfil_muestra$cobertura_diagnosticos[
    resultado$perfil_muestra$cobertura_diagnosticos$diagnostico ==
      "muestra_perfilado", , drop = FALSE
  ]), 0L)
  expect_equal(nrow(resultado$resumen_tabla$cobertura[
    resultado$resumen_tabla$cobertura$bloque == "perfil_muestra" &
      resultado$resumen_tabla$cobertura$estado == "degradado", ,
    drop = FALSE
  ]), 0L)
  expect_identical(resultado$perfil_muestra$meta$tope_que_mando, "muestra")

  solo <- do.call(
    perfilar_dbi,
    c(list(conexion = con, tabla = "topes", muestra = 5L,
           max_celdas_muestra = 1L, max_bytes_muestra = 1L,
           bloque_muestra = "solo_agregados"),
      .argumentos_perfil_topes_dbi)
  )
  expect_null(solo$perfil_muestra)
  expect_equal(solo$resumen_tabla$columnas,
               resultado$resumen_tabla$columnas)
})

test_that("el plan declara el recorte de celdas y la sonda de bytes", {
  con <- .con_topes_dbi()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  plan_celdas <- plan_perfilado_dbi(
    con, "topes", muestra = 20L, modo = "conteos",
    max_celdas_muestra = 10L, max_bytes_muestra = Inf
  )
  tope <- attr(plan_celdas, "tope_muestra", exact = TRUE)
  expect_true(tope$recortada_por_celdas)
  expect_equal(tope$filas_maximas, 5)
  expect_equal(attr(plan_celdas, "max_celdas_muestra", exact = TRUE), 10)
  expect_equal(
    plan_celdas$n_consultas[plan_celdas$clase_consulta == "muestra"], 1
  )
  salida <- capture.output(print(plan_celdas), type = "message")
  expect_true(any(grepl("tope de celdas", salida, fixed = TRUE)))

  plan_bytes <- plan_perfilado_dbi(
    con, "topes", muestra = 20L, modo = "conteos",
    max_celdas_muestra = Inf, max_bytes_muestra = 1000L
  )
  tope_bytes <- attr(plan_bytes, "tope_muestra", exact = TRUE)
  expect_true(tope_bytes$requiere_sonda_bytes)
  expect_equal(
    plan_bytes$n_consultas[plan_bytes$clase_consulta == "muestra"], 2
  )
  expect_equal(attr(plan_bytes, "max_bytes_muestra", exact = TRUE), 1000)
})

test_that("los dos lugares conservan las mismas cifras por omision", {
  expect_identical(
    formals(perfilar)$max_celdas_muestra,
    formals(perfilar_dbi)$max_celdas_muestra
  )
  expect_identical(
    formals(perfilar)$max_bytes_muestra,
    formals(perfilar_dbi)$max_bytes_muestra
  )
  expect_identical(
    formals(perfilar_dbi)$max_celdas_muestra,
    formals(plan_perfilado_dbi)$max_celdas_muestra
  )
  expect_identical(
    formals(perfilar_dbi)$max_bytes_muestra,
    formals(plan_perfilado_dbi)$max_bytes_muestra
  )
})
