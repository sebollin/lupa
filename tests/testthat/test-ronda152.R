skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

.ronda152_datos <- function() {
  data.frame(
    # Valores exactos en binario: la sonda aleatoria del modo muestreado
    # puede cambiar el orden de las filas, pero no el resultado medido.
    id = rep(c(0, 1), 6L),
    cantidad = rep(c(0, 2), 6L),
    codigo = rep(c("a", "b", "b", "c"), length.out = 12L),
    marca = rep(c(TRUE, FALSE, NA), length.out = 12L),
    stringsAsFactors = FALSE
  )
}

.ronda152_conexion <- function() {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(conexion, "tabla_prueba", .ronda152_datos())
  conexion
}

.ronda152_sin_auditoria <- function(x) {
  campos_auditoria <- c(
    "sql", "lote", "columnas_compartidas", "consulta_id", "etapa",
    "duracion_ms", "n_filas_resultado", "bytes_resultado_r", "cpu_ms",
    "id_muestra"
  )
  x$resumen_tabla$sql <- x$resumen_tabla$sql[
    , setdiff(names(x$resumen_tabla$sql), campos_auditoria), drop = FALSE
  ]
  x$resumen_tabla$tiempos <- NULL
  x$resumen_tabla$meta$plan <- NULL
  x$resumen_tabla$meta$consultas <- NULL
  x$resumen_tabla$meta$tamano_lote <- NULL
  x$resumen_tabla$meta$tamano_lote_funciono <- NULL
  x$resumen_tabla$meta$tamano_lote_planos <- NULL
  x$resumen_tabla$meta$tamano_lote_distintos <- NULL
  x$resumen_tabla$meta$tamano_lote_planos_funciono <- NULL
  x$resumen_tabla$meta$tamano_lote_distintos_funciono <- NULL
  x$resumen_tabla$meta$instrumentacion <- NULL
  x
}

test_that("la fusion plana conserva el objeto medido en los cinco modos", {
  modos <- c("exacto", "seguro", "conteos", "muestreado", "aproximado")
  fusionada <- .ronda152_conexion()
  unitaria <- .ronda152_conexion()
  on.exit(DBI::dbDisconnect(fusionada), add = TRUE)
  on.exit(DBI::dbDisconnect(unitaria), add = TRUE)

  for (modo in modos) {
    argumentos <- list(
      modo = modo, muestra = 12L, bloque_muestra = "solo_agregados",
      instrumentar = FALSE, proteger_datos_personales = FALSE,
      analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
      ausencia_estructural = FALSE, duplicados_aproximados = FALSE
    )
    nuevo <- do.call(
      perfilar_dbi, c(list(fusionada, "tabla_prueba", tamano_lote = 4L), argumentos)
    )
    referencia <- do.call(
      perfilar_dbi, c(list(unitaria, "tabla_prueba", tamano_lote = 1L), argumentos)
    )
    expect_identical(
      .ronda152_sin_auditoria(nuevo),
      .ronda152_sin_auditoria(referencia),
      info = paste("modo", modo)
    )
  }
})

test_that("la cuenta fusionada coincide con la predicha", {
  conexion <- .ronda152_conexion()
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  modos <- c("exacto", "seguro", "conteos", "muestreado", "aproximado")
  for (modo in modos) {
    plan <- plan_perfilado_dbi(
      conexion, "tabla_prueba", modo = modo, muestra = 12L,
      bloque_muestra = "solo_agregados", tamano_lote = 4L
    )
    resultado <- perfilar_dbi(
      conexion, "tabla_prueba", modo = modo, muestra = 12L,
      bloque_muestra = "solo_agregados", tamano_lote = 4L,
      instrumentar = FALSE, proteger_datos_personales = FALSE,
      analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
      ausencia_estructural = FALSE, duplicados_aproximados = FALSE
    )
    expect_identical(
      as.integer(resultado$resumen_tabla$meta$consultas$emitidas),
      as.integer(attr(plan, "total")),
      info = paste("modo", modo)
    )
  }
})
