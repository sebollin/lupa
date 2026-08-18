.datos_casi_clave_r112 <- function(densa = TRUE) {
  valores <- if (densa) {
    seq_len(2693L)
  } else {
    set.seed(11201)
    sample(10000000L:99999999L, 2693L)
  }
  repetido <- valores[[1350L]]
  candidato <- c(valores, rep(repetido, 14L))
  data.frame(
    fila = seq_along(candidato), candidato = candidato,
    stringsAsFactors = FALSE
  )
}

test_that("una casi-clave densa declara la colision y el criterio", {
  datos <- .datos_casi_clave_r112(TRUE)
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$columna == "candidato" &
      perfil$hallazgos$tipo_hallazgo == "casi_clave", , drop = FALSE
  ]

  expect_equal(nrow(hallazgo), 1L)
  expect_equal(as.character(hallazgo$severidad), "sospechoso")
  expect_equal(hallazgo$n_evaluados, 2707)
  expect_equal(hallazgo$n_afectados, 15)
  expect_match(hallazgo$evidencia, "1350 (15)", fixed = TRUE)
  expect_match(
    hallazgo$evidencia,
    paste0(
      "criterio_casi_clave: n_filas>=100, tasa_distintos>=0.900 y ",
      "concentracion_colisiones>=0.500"
    ),
    fixed = TRUE
  )

  claves <- detectar_claves(datos, max_combinacion = 1L)
  casi <- claves[claves$columnas == "candidato", , drop = FALSE]
  expect_equal(nrow(casi), 1L)
  expect_true(casi$casi_clave)
  expect_false(casi$unicidad_exacta)
  expect_equal(casi$n_distintos_exactos, 2693L)
  expect_equal(casi$n_valores_colisionados, 1L)
  expect_equal(casi$n_filas_en_colision, 15L)
  expect_equal(casi$n_duplicados_excedentes, 14L)
  expect_equal(casi$concentracion_colisiones, 1)
  expect_equal(casi$colisiones, "1350 (15)")
})

test_that("una casi-clave de identificadores aleatorios llega a analizar", {
  datos <- .datos_casi_clave_r112(FALSE)
  repetido <- as.character(datos$candidato[[1350L]])
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$columna == "candidato" &
      perfil$hallazgos$tipo_hallazgo == "casi_clave", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(hallazgo$evidencia, paste0(repetido, " (15)"), fixed = TRUE)

  claves <- detectar_claves(datos, max_combinacion = 1L)
  casi <- claves[claves$columnas == "candidato", , drop = FALSE]
  expect_true(casi$casi_clave)
  expect_match(casi$colisiones, paste0(repetido, " (15)"), fixed = TRUE)

  integral <- analizar(
    datos, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE, medir_propuesta = FALSE
  )
  advertencia <- integral$advertencias[
    integral$advertencias$tipo == "casi_clave", , drop = FALSE
  ]
  expect_equal(nrow(advertencia), 1L)
  expect_match(advertencia$descripcion, "candidato", fixed = TRUE)
  expect_match(advertencia$descripcion, paste0(repetido, " (15)"), fixed = TRUE)
})

test_that("colisiones dispersas y baja unicidad no son casi-claves", {
  frecuencias_cerveza <- c(
    rep(1L, 2221L), rep(2L, 76L), rep(3L, 3L), 4L, 6L, 6L, 12L
  )
  cerveza <- rep(
    sprintf("cerveza_%04d", seq_along(frecuencias_cerveza)),
    frecuencias_cerveza
  )
  controles <- list(
    cerveza = cerveza,
    categorica = rep(LETTERS[1:5], 20L),
    mitad_distintos = rep(sprintf("v%02d", seq_len(50L)), each = 2L),
    constante = rep("unico", 100L)
  )

  for (nombre in names(controles)) {
    datos <- data.frame(valor = controles[[nombre]], stringsAsFactors = FALSE)
    perfil <- perfilar(
      datos, analizar_dependencias = FALSE,
      casi_duplicados_vocabulario = FALSE,
      proteger_datos_personales = FALSE
    )
    expect_false(
      "casi_clave" %in% perfil$hallazgos$tipo_hallazgo,
      info = nombre
    )
    claves <- detectar_claves(datos, max_combinacion = 1L)
    expect_false(any(claves$casi_clave), info = nombre)
  }

  resumen <- lupa:::.resumen_casi_clave(cerveza)
  expect_equal(resumen$n_filas, 2410L)
  expect_equal(resumen$n_distintos, 2304L)
  expect_equal(resumen$n_valores_colisionados, 83L)
  expect_equal(resumen$n_duplicados_excedentes, 106L)
  expect_equal(resumen$concentracion_colisiones, 11 / 106)
  expect_false(resumen$es_casi_clave)
})

test_that("faltantes disfrazados se retiran de variantes de vocabulario", {
  casos <- list(
    no_na = c(rep("NO", 950L), rep("NA", 50L)),
    si_sd = c(rep("SI", 950L), rep("SD", 50L))
  )
  for (nombre in names(casos)) {
    perfil <- perfilar(
      data.frame(valor = casos[[nombre]], stringsAsFactors = FALSE),
      analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    )
    tipos <- perfil$hallazgos$tipo_hallazgo
    expect_true("faltantes_disfrazados" %in% tipos, info = nombre)
    expect_false("casi_duplicados_vocabulario" %in% tipos, info = nombre)
  }
})

test_that("una errata que no es faltante sigue como casi-duplicado", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(valor = c(
    rep("Montevideo", 240L), rep("MONTEVIDEO", 25L), rep("Montevido", 8L)
  ))
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]

  expect_equal(nrow(hallazgo), 1L)
  expect_equal(as.character(hallazgo$severidad), "sospechoso")
  expect_match(hallazgo$evidencia, "Montevideo (240)", fixed = TRUE)
  expect_match(hallazgo$evidencia, "MONTEVIDEO (25)", fixed = TRUE)
  expect_match(hallazgo$evidencia, "Montevido (8)", fixed = TRUE)
  expect_match(
    hallazgo$evidencia,
    "valores_excluidos_faltantes_disfrazados=0", fixed = TRUE
  )
})
