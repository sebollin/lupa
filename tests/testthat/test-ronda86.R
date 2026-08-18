test_that("la falta de stringdist se declara aunque no haya fusion exacta", {
  local_mocked_bindings(
    .stringdist_disponible = function() FALSE,
    .package = "lupa"
  )
  sin_fusion <- perfilar(
    data.frame(nombre = c("Montevido", "Canelone", "Rocha")),
    analizar_dependencias = FALSE, normalizar = FALSE,
    proteger_datos_personales = FALSE
  )
  cobertura_sin <- sin_fusion$cobertura_diagnosticos[
    sin_fusion$cobertura_diagnosticos$diagnostico == "proximidad_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(cobertura_sin), 1L)
  expect_match(cobertura_sin$motivo, "falta el paquete opcional 'stringdist'")
  expect_false("casi_duplicados_vocabulario" %in%
                 sin_fusion$hallazgos$tipo_hallazgo)

  con_fusion <- perfilar(
    data.frame(nombre = c("Montevideo", "MONTEVIDEO", "Montevido")),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  hallazgo_con <- con_fusion$hallazgos[
    con_fusion$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo_con), 1L)
  expect_match(hallazgo_con$evidencia, "origen=normalizacion")
  expect_match(hallazgo_con$evidencia, "falta el paquete opcional 'stringdist'")
  expect_equal(nrow(con_fusion$cobertura_diagnosticos), 1L)
})

test_that("la evidencia de variantes distingue token y edicion interna", {
  datos <- c(
    rep("CAMINO CARRASCO", 30L), rep("CAMINO AGRARIOS", 2L),
    rep("Montevideo", 30L), rep("Montevido", 2L)
  )
  grupos <- lupa:::.grupos_casi_duplicados_vocabulario(
    datos, lupa:::.resolver_normalizacion(FALSE), "nombre"
  )$grupos
  camino <- vapply(grupos, function(g) {
    all(c("CAMINO CARRASCO", "CAMINO AGRARIOS") %in% g$variantes)
  }, logical(1L))
  montevideo <- vapply(grupos, function(g) {
    all(c("Montevideo", "Montevido") %in% g$variantes)
  }, logical(1L))
  expect_true(any(camino))
  expect_true(any(montevideo))
  expect_equal(grupos[[which(camino)[[1L]]]]$clase_diferencia, "token_completo")
  expect_equal(grupos[[which(montevideo)[[1L]]]]$clase_diferencia,
               "token_unico")
})

test_that("la zona de origen y el cambio de fecha civil quedan declarados", {
  desplazada <- as.POSIXct(
    c("2026-01-01 23:59:00", "2026-01-01 12:00:00"),
    tz = "America/Montevideo"
  )
  estable <- as.POSIXct(
    c("2026-01-01 09:00:00", "2026-01-01 12:00:00"),
    tz = "America/Montevideo"
  )
  perfil <- perfilar(
    data.frame(desplazada = desplazada, estable = estable),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas[perfil$columnas$columna == "desplazada", , drop = FALSE]
  expect_equal(fila$zona_horaria_origen, "America/Montevideo")
  expect_equal(fila$n_filas_fecha_civil_distinta_utc, 1L)
  expect_true(fila$fecha_civil_distinta_utc)
  expect_true(any(
    perfil$hallazgos$columna == "desplazada" &
      perfil$hallazgos$tipo_hallazgo == "zona_horaria_fecha_hora"
  ))
  fila_estable <- perfil$columnas[perfil$columnas$columna == "estable", , drop = FALSE]
  expect_equal(fila_estable$n_filas_fecha_civil_distinta_utc, 0L)
  expect_false(any(
    perfil$hallazgos$columna == "estable" &
      perfil$hallazgos$tipo_hallazgo == "zona_horaria_fecha_hora"
  ))
})

test_that("la proporcion de tipo declara sus filas y su muestreo", {
  pequeno <- perfilar(
    data.frame(x = as.character(seq_len(20L))), muestra = 100L,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  fila_pequena <- pequeno$columnas[1L, , drop = FALSE]
  expect_equal(fila_pequena$n_filas_analizadas_tipo, 20L)
  expect_false(fila_pequena$muestreado_tipo_inferido)

  grande <- perfilar(
    data.frame(x = as.character(seq_len(100001L))), muestra = 100000L,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  fila_grande <- grande$columnas[1L, , drop = FALSE]
  expect_equal(fila_grande$n_filas_analizadas_tipo, 100000L)
  expect_true(fila_grande$muestreado_tipo_inferido)
  inferencia <- inferir_tipo(as.character(seq_len(100001L)), muestra = 100000L)
  expect_equal(inferencia$n_analizados, 100000L)
  expect_true(inferencia$muestreado)
  expect_lt(as.numeric(object.size(inferencia)), 1e6)
})
