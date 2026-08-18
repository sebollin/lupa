tipos_con_traza_por_fila <- c(
  "mayusculas_inconsistentes", "normalizacion_unicode"
)

test_that("las trazas por fila coinciden con fixtures de corrupcion conocidos", {
  set.seed(20260818)
  n <- 1000L
  indices_atipicos <- sample.int(n, 10L)
  indices_mojibake <- sample(setdiff(seq_len(n), indices_atipicos), 15L)

  edades <- as.character(sample(18:80, n, replace = TRUE))
  edades[indices_atipicos] <- "200"
  texto <- rep("texto", n)
  texto[indices_mojibake] <- "caf\u00c3\u00a9"
  texto[indices_mojibake[[1L]]] <- paste0("texto ", "\u00ef\u00bf\u00bd")
  datos <- data.frame(
    id = seq_len(n), edad = edades, texto = texto,
    stringsAsFactors = FALSE
  )

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  esperados <- list(
    edad = list(outliers = indices_atipicos),
    texto = list(
      patron_raro = indices_mojibake,
      codificacion_rota = indices_mojibake
    )
  )
  filas <- which(
    as.character(perfil$hallazgos$unidad_conteo) == "fila" |
      as.character(perfil$hallazgos$tipo_hallazgo) %in% tipos_con_traza_por_fila
  )
  expect_gt(length(filas), 0L)

  for (i in filas) {
    hallazgo <- perfil$hallazgos[i, , drop = FALSE]
    columna <- as.character(hallazgo$columna[[1L]])
    tipo <- as.character(hallazgo$tipo_hallazgo[[1L]])
    esperados_hallazgo <- esperados[[columna]][[tipo]]
    expect_false(
      is.null(esperados_hallazgo),
      info = paste(columna, tipo, "no tiene fixture declarado")
    )
    traza <- hallazgo$trazabilidad[[1L]]
    expect_equal(
      sort(traza$indices_fila), sort(esperados_hallazgo),
      info = paste(columna, tipo)
    )
    expect_length(setdiff(esperados_hallazgo, traza$indices_fila), 0L)
    expect_length(setdiff(traza$indices_fila, esperados_hallazgo), 0L)
    expect_equal(traza$total, length(esperados_hallazgo))
  }
})

test_that("patron_raro enumera solo los desvios de una secuencia densa", {
  set.seed(20260818)
  x <- c(as.character(3:1000), rep(c("-5", "-6", "-7"), each = 2L))
  perfil <- perfilar(
    data.frame(col = x, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "patron_raro", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(hallazgo$unidad_conteo, "fila")
  expect_match(hallazgo$evidencia, "Desv\u00edos: -9", fixed = TRUE)
  expect_equal(hallazgo$n_afectados, 6)
  traza <- hallazgo$trazabilidad[[1L]]
  expect_equal(traza$estado, "disponible")
  expect_equal(traza$alcance, "completo")
  expect_setequal(traza$indices_fila, 999:1004)
  expect_setequal(x[traza$indices_fila], c("-5", "-6", "-7"))
  expect_equal(length(unique(traza$indices_fila)), 6L)
  expect_equal(traza$total, 6)
})

test_that("los hallazgos numericos textuales comparten la vista inferida", {
  n <- 1000L
  indices_ceros <- c(101L, 202L, 303L)
  indices_negativos <- c(404L, 505L, 606L)
  indices_no_finitos <- c(707L, 808L)
  cantidad <- as.character(seq_len(n))
  cantidad[indices_ceros] <- "0"
  cantidad[indices_negativos] <- c("-1", "-2", "-3")
  cantidad[indices_no_finitos] <- c("Inf", "-Inf")
  datos <- data.frame(id = seq_len(n), cantidad = cantidad)

  perfil <- perfilar(
    datos,
    analizar_dependencias = FALSE,
    columnas_sin_ceros = "cantidad",
    columnas_no_negativas = "cantidad",
    sentinelas_numericos = numeric(),
    umbral_patron_raro = 0
  )
  esperados <- list(
    casi_clave = indices_ceros,
    valores_no_finitos = indices_no_finitos,
    ceros_no_permitidos = indices_ceros,
    negativos_no_permitidos = indices_negativos
  )
  hallazgos <- perfil$hallazgos[
    perfil$hallazgos$columna == "cantidad" &
      perfil$hallazgos$unidad_conteo == "fila" |
        perfil$hallazgos$tipo_hallazgo %in% tipos_con_traza_por_fila, ,
      drop = FALSE
  ]
  expect_setequal(as.character(hallazgos$tipo_hallazgo), names(esperados))
  for (i in seq_len(nrow(hallazgos))) {
    tipo <- as.character(hallazgos$tipo_hallazgo[[i]])
    traza <- hallazgos$trazabilidad[[i]]
    expect_equal(sort(traza$indices_fila), sort(esperados[[tipo]]))
    expect_length(setdiff(esperados[[tipo]], traza$indices_fila), 0L)
    expect_length(setdiff(traza$indices_fila, esperados[[tipo]]), 0L)
  }
})
