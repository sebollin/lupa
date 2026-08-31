skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

library(DBI)

.datos_ronda148 <- function() {
  data.frame(
    id = 1:12,
    texto = rep(c("a", "b", "c"), 4),
    monto = seq_len(12) / 10,
    stringsAsFactors = FALSE
  )
}

.conexion_ronda148 <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "datos", .datos_ronda148())
  con
}

.sin_instrumentacion_ronda148 <- function(x) {
  campos_nuevos <- c(
    "duracion_ms", "n_filas_resultado", "bytes_resultado_r",
    "cpu_ms", "consulta_id", "etapa", "nivel", "derrame",
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
  x$resumen_tabla$meta$costo_moda <- NULL
  x$resumen_tabla$meta$costo_mediana <- NULL
  x
}

.perfil_base_ronda148 <- function(con, caso, instrumentar = TRUE,
                                  bloque_muestra = "con_muestra",
                                  metricas = "validos", ...) {
  argumentos <- .argumentos_caso_dbi(caso, muestra = 12L)
  argumentos$metricas <- metricas
  argumentos <- modifyList(list(muestra = 12L), argumentos)
  do.call(perfilar_dbi, c(list(
    con, "datos",
    orden_muestra = "id",
    instrumentar = instrumentar, proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE, duplicados_aproximados = FALSE,
    bloque_muestra = bloque_muestra,
    fecha = as.POSIXct("2026-01-01", tz = "UTC"), ...
  ), argumentos))
}

test_that("la instrumentacion no cambia el objeto en los cinco casos", {
  casos <- c("exacto", "seguro", "conteos", "aproximado")
  con <- .conexion_ronda148()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  for (caso in casos) {
    medido <- .perfil_base_ronda148(con, caso, instrumentar = TRUE)
    sin_medir <- .perfil_base_ronda148(con, caso, instrumentar = FALSE)
    expect_identical(
      .sin_instrumentacion_ronda148(medido),
      .sin_instrumentacion_ronda148(sin_medir),
      info = paste("caso", caso)
    )
  }
  # `muestra_motor` toma una muestra aleatoria nueva en cada corrida. Comparar
  # dos perfiles completos con `instrumentar` distinto exigiría fijar una
  # muestra materializada, que no es parte de este contrato; la invariancia de
  # estados y metadatos del motor muestreado queda cubierta en dbi-muestreado.
})

test_that("sql publica medicion real, identificador y etapa", {
  con <- .conexion_ronda148()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  resultado <- .perfil_base_ronda148(
    con, "conteos", bloque_muestra = "con_muestra"
  )
  sql <- resultado$resumen_tabla$sql

  expect_true(all(c(
    "duracion_ms", "n_filas_resultado", "bytes_resultado_r",
    "cpu_ms", "consulta_id", "etapa"
  ) %in% names(sql)))
  emitidas <- sql[!is.na(sql$consulta_id), , drop = FALSE]
  expect_true(nrow(emitidas) > 0L)
  expect_true(all(emitidas$n_filas_resultado == 1))
  expect_true(all(emitidas$bytes_resultado_r > 0))
  expect_true(all(emitidas$etapa %in% c("conteo_filas", "conteos")))
  expect_true(any(!is.na(emitidas$duracion_ms)))
  expect_true(any(!is.na(emitidas$cpu_ms)))
  expect_true(all(emitidas$cpu_ms >= 0))
})

test_that("sql marca una sola duracion sumable por consulta", {
  con <- .conexion_ronda148()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  resultado <- .perfil_base_ronda148(
    con, "conteos", metricas = "distintos", bloque_muestra = "solo_agregados"
  )
  sql <- resultado$resumen_tabla$sql
  emitidas <- sql[!is.na(sql$consulta_id), , drop = FALSE]

  expect_true("nivel" %in% names(sql))
  expect_true(is.integer(sql$nivel))
  expect_true(all(sql$nivel >= 1L))
  expect_true(any(emitidas$nivel == 2L))

  suma_ingenua <- sum(emitidas$duracion_ms)
  suma_por_consulta <- sum(emitidas$duracion_ms[emitidas$nivel == 1L])
  suma_por_id <- sum(vapply(
    split(emitidas$duracion_ms, emitidas$consulta_id),
    function(duraciones) duraciones[[1L]], numeric(1L)
  ))

  expect_gt(suma_ingenua, suma_por_consulta)
  expect_equal(suma_por_consulta, suma_por_id)
})

test_that("no solicitado y sin consulta no se publican como cero", {
  con <- .conexion_ronda148()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  resultado <- .perfil_base_ronda148(
    con, "conteos", metricas = "validos"
  )
  sql <- resultado$resumen_tabla$sql
  no_solicitado <- sql[sql$estado == "no_solicitado", , drop = FALSE]

  expect_true(nrow(no_solicitado) > 0L)
  expect_true(all(is.na(no_solicitado$duracion_ms)))
  expect_true(all(is.na(no_solicitado$n_filas_resultado)))
  expect_true(all(is.na(no_solicitado$bytes_resultado_r)))
  expect_true(all(is.na(no_solicitado$cpu_ms)))
  expect_true(all(is.na(no_solicitado$consulta_id)))
  expect_true(all(no_solicitado$etapa == "no_solicitado"))
})

test_that("un fallo de consulta conserva su etapa y duracion cuando se midio", {
  con <- .conexion_ronda148()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  presupuesto <- lupa:::.presupuesto_dbi(Inf, instrumentar = TRUE)
  resultado <- lupa:::.consultar_dbi(
    con, "SELECT * FROM tabla_que_no_existe", presupuesto,
    etapa = "prueba_fallo"
  )

  expect_false(resultado$ok)
  expect_true(is.integer(resultado$consulta_id))
  expect_identical(resultado$etapa, "prueba_fallo")
  expect_true(is.na(resultado$n_filas_resultado))
  expect_true(is.na(resultado$bytes_resultado_r))
  expect_true(!is.na(resultado$duracion_ms))
  expect_true(!is.na(resultado$cpu_ms))
  expect_true(resultado$cpu_ms >= 0)
})

test_that("el apagado deja explicito que no se midio", {
  con <- .conexion_ronda148()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  resultado <- .perfil_base_ronda148(con, "conteos", instrumentar = FALSE)
  tiempos <- resultado$resumen_tabla$tiempos

  expect_true(all(tiempos$estado %in% c("no_medido", "no_solicitado")))
  expect_true(all(is.na(tiempos$duracion_ms)))
  expect_true(all(is.na(tiempos$cpu_ms)))
  expect_false(any(is.na(resultado$resumen_tabla$sql$etapa)))
  expect_true(all(is.na(resultado$resumen_tabla$sql$duracion_ms)))
  expect_true(all(is.na(resultado$resumen_tabla$sql$cpu_ms)))
})

test_that("tiempos de R incluyen lectura, perfil y analisis opcional", {
  con <- .conexion_ronda148()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  resultado <- .perfil_base_ronda148(
    con, "conteos", bloque_muestra = "con_muestra"
  )
  tiempos <- resultado$resumen_tabla$tiempos

  expect_true(all(c(
    "lectura_muestra", "perfilado_muestra", "perfilado_columnas",
    "dependencias", "casi_duplicados_vocabulario", "duplicados_aproximados",
    "ausencia_estructural"
  ) %in% tiempos$etapa))
  expect_true(all(c(
    "etapa", "duracion_ms", "cpu_ms", "estado", "n_ejecuciones"
  ) %in% names(tiempos)))
  expect_true(any(tiempos$estado == "medido"))
  expect_true(all(tiempos$n_ejecuciones >= 1L))
  expect_true(all(is.na(tiempos$duracion_ms) |
                 tiempos$duracion_ms > 0))
})

test_that("el plan conserva sus campos como predicciones", {
  con <- .conexion_ronda148()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  plan <- plan_perfilado_dbi(
    con, "datos", universo = "tabla_completa", metricas = "validos",
    estrategia_mediana = "exacta", muestra = 12L
  )

  expect_false("duracion_ms" %in% names(plan))
  expect_false("n_filas_resultado" %in% names(plan))
  expect_false("bytes_resultado_r" %in% names(plan))
  expect_match(attr(plan, "supuesto_costo"), "trabajo", ignore.case = TRUE)
})

# La anidacion de las etapas estaba dicha en la viñeta y en el `Rd`, y no en el
# objeto. Quien mira `resumen_tabla$tiempos` en la consola ve siete filas sin
# nada que le advierta que sumarlas da mas que la corrida entera. El paquete ya
# habia decidido esta misma cuestion una vez -la dependencia del orden en LSH se
# movio de la documentacion al `alcance`- asi que aca se aplica el precedente:
# el trazador lleva la profundidad y el objeto publica `nivel`.

test_that("los tiempos declaran que nivel se puede sumar", {
  skip_if_not_installed("RSQLite")
  set.seed(148L)
  n <- 4000L
  datos <- data.frame(
    texto = sample(sprintf("v%03d", seq_len(60L)), n, TRUE),
    cat = sample(c("a", "b", NA), n, TRUE),
    num = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)

  tiempos <- perfilar_dbi(conexion, "t")$resumen_tabla$tiempos
  expect_true("nivel" %in% names(tiempos))
  expect_true(is.integer(tiempos$nivel))
  expect_true(all(tiempos$nivel >= 1L))

  # `perfilado_muestra` es de primer nivel y contiene a las demas, que quedan
  # por debajo. Si alguna subetapa apareciera en nivel 1, sumar las de nivel 1
  # dejaria de tener sentido.
  nivel_de <- function(e) tiempos$nivel[tiempos$etapa == e]
  expect_identical(nivel_de("perfilado_muestra"), 1L)
  expect_identical(nivel_de("lectura_muestra"), 1L)
  for (sub in c("perfilado_columnas", "dependencias")) {
    if (sub %in% tiempos$etapa) expect_gt(nivel_de(sub), 1L)
  }

  # Y la propiedad que justifica la columna: las contenidas no exceden a la que
  # las contiene, mientras que sumar todas si excede.
  medidas <- tiempos[tiempos$estado == "medido", , drop = FALSE]
  contenidas <- sum(medidas$duracion_ms[medidas$nivel > 1L])
  expect_lte(
    contenidas,
    sum(medidas$duracion_ms[medidas$etapa == "perfilado_muestra"]) * 1.05
  )
})

test_that("apagar la instrumentacion no inventa niveles", {
  skip_if_not_installed("RSQLite")
  datos <- data.frame(a = c("x", "y", "z"), b = 1:3, stringsAsFactors = FALSE)
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)
  tiempos <- perfilar_dbi(conexion, "t", instrumentar = FALSE)$resumen_tabla$tiempos
  expect_true("nivel" %in% names(tiempos))
  expect_true(all(is.na(tiempos$duracion_ms[tiempos$estado == "no_medido"])))
  expect_true(all(is.na(tiempos$cpu_ms[tiempos$estado == "no_medido"])))
})
