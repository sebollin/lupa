# La muestra es un bloque optativo. La decision no se mezcla con `modo`, que
# sigue seleccionando solo las metricas SQL.

.tabla_ronda145 <- function(n = 40L) {
  data.frame(
    id = seq_len(n),
    texto = rep(c("a", "b", "c"), length.out = n),
    valor = seq_len(n) / 10,
    stringsAsFactors = FALSE
  )
}

test_that("solo_agregados no lee ni cobra el bloque de muestra", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "tabla145", .tabla_ronda145())

  plan_con <- plan_perfilado_dbi(
    con, "tabla145", modo = "conteos", muestra = 10L
  )
  plan_solo <- plan_perfilado_dbi(
    con, "tabla145", modo = "conteos", muestra = 10L,
    bloque_muestra = "solo_agregados"
  )
  expect_true(any(plan_con$clase_consulta == "muestra"))
  expect_false(any(plan_solo$clase_consulta == "muestra"))
  expect_true(is.na(attr(plan_solo, "muestra", exact = TRUE)))
  expect_equal(attr(plan_solo, "bloque_muestra", exact = TRUE), "solo_agregados")
  expect_true(is.na(attr(plan_solo, "filas_leidas", exact = TRUE)))
  expect_true(is.na(attr(plan_con, "filas_leidas", exact = TRUE)))
  expect_lt(attr(plan_solo, "total", exact = TRUE),
            attr(plan_solo, "total_maximo", exact = TRUE))

  resultado <- perfilar_dbi(
    con, "tabla145", modo = "conteos", muestra = 10L,
    bloque_muestra = "solo_agregados"
  )
  expect_null(resultado$perfil_muestra)
  expect_equal(
    resultado$resumen_tabla$meta$consultas$emitidas,
    attr(plan_solo, "total", exact = TRUE)
  )
  cobertura <- resultado$resumen_tabla$cobertura
  fila <- cobertura[cobertura$bloque == "perfil_muestra", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(fila$estado, "no_solicitado")
  expect_match(fila$motivo, "No se solicito")
  expect_false(grepl("No se pudo", fila$motivo, fixed = TRUE))
  expect_warning(hallazgos(resultado), "no solicito la muestra")
  expect_equal(nrow(columnas(resultado)), 3L)
  expect_equal(as.numeric(n_filas(resultado)), 40)
})

test_that("el valor por omision conserva el perfil anterior", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "tabla145", .tabla_ronda145(12L))
  opcion <- options(lupa.progreso = FALSE)
  on.exit(options(opcion), add = TRUE)

  por_omision <- perfilar_dbi(
    con, "tabla145", modo = "conteos", muestra = 7L,
    proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
  explicito <- perfilar_dbi(
    con, "tabla145", modo = "conteos", muestra = 7L,
    bloque_muestra = "con_muestra", proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
  quitar_tiempos <- function(x) {
    campos_nuevos <- c(
      "duracion_ms", "n_filas_resultado", "bytes_resultado_r",
      "cpu_ms", "consulta_id", "etapa", "derrame",
      "bloques_temporales_leidos", "bloques_temporales_escritos",
      "fuente_derrame"
    )
    x$resumen_tabla$sql <- x$resumen_tabla$sql[
      , setdiff(names(x$resumen_tabla$sql), campos_nuevos), drop = FALSE
    ]
    x$resumen_tabla$tiempos <- NULL
    x$resumen_tabla$meta$instrumentacion <- NULL
    x$resumen_tabla$meta$derrame <- NULL
    x$resumen_tabla$meta$costo_distintos <- NULL
    attr(x$resumen_tabla$meta$plan, "costo_distintos") <- NULL
    x
  }
  expect_identical(quitar_tiempos(por_omision), quitar_tiempos(explicito))
})

test_that("perfilar_coleccion propaga solo_agregados y no rompe con muestra NULL", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "tabla145", .tabla_ronda145(8L))

  resultado <- perfilar_coleccion(
    coleccion(con, "tabla145"), bloque_muestra = "solo_agregados",
    conservar_perfiles = TRUE, proteger_datos_personales = FALSE
  )
  expect_equal(resultado$meta$n_perfiladas, 1L)
  expect_equal(resultado$meta$n_sin_perfilar, 0L)
  expect_true(is.na(resultado$resumen_coleccion$muestra_solicitada))
  expect_true(is.na(resultado$resumen_coleccion$muestra_analizada))
  expect_null(resultado$perfiles$tabla145$perfil_muestra)
  fila <- resultado$cobertura_coleccion[
    resultado$cobertura_coleccion$alcance == "muestra_no_solicitada", ,
    drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$motivo, "No se solicito")
})

# Con `bloque_muestra = "solo_agregados"` no se trae ninguna fila a R, asi que el
# detector de vocabulario no corre y el plan no puede cobrar sus pares. Lo hacia
# bien en cuatro modos y mal en `muestreado`, porque el conteo colgaba del
# conjunto `.ALCANCES_CON_MUESTRA_DBI`, que mete en la misma bolsa el muestreo
# DEL MOTOR -que en ese modo ocurre igual- y el bloque DEL CLIENTE, que es lo
# unico que trae filas. El plan impreso llegaba a contradecirse a dos lineas:
# "el plan incluye solo agregados SQL" y despues "mas 4.000.000 pares de formas
# a comparar en R".

test_that("solo_agregados no cobra trabajo de R en ningun modo", {
  skip_if_not_installed("RSQLite")
  set.seed(145L)
  n <- 3000L
  datos <- data.frame(
    t1 = sample(sprintf("v%02d", seq_len(80L)), n, TRUE),
    t2 = sample(sprintf("w%02d", seq_len(60L)), n, TRUE),
    x = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)

  for (modo in c("exacto", "seguro", "conteos", "aproximado", "muestreado")) {
    solo <- plan_perfilado_dbi(
      conexion, "t", modo = modo, muestra = 100L,
      bloque_muestra = "solo_agregados"
    )
    expect_identical(
      attr(solo, "pares_texto", exact = TRUE), 0,
      info = paste("solo_agregados en modo", modo)
    )
    # Y con el bloque pedido, el mismo modo si los cuenta: la prueba caeria
    # tambien si alguien apagara el conteo para todos los casos.
    con <- plan_perfilado_dbi(
      conexion, "t", modo = modo, muestra = 100L,
      bloque_muestra = "con_muestra"
    )
    expect_gt(attr(con, "pares_texto", exact = TRUE), 0)
  }
})
