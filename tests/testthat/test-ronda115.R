.tipos_casi_clave_r115 <- function(datos) {
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_clave", , drop = FALSE
  ]
}

.datos_casi_clave_r115 <- function(densa = TRUE) {
  valores <- if (densa) {
    seq_len(2693L)
  } else {
    set.seed(11201)
    sample(10000000L:99999999L, 2693L)
  }
  c(valores, rep(valores[[1350L]], 14L))
}

test_that("casi_clave exige evidencia suficiente por cantidad de filas", {
  tamanos <- c(5L, 10L, 13L, 20L, 100L, 1000L)
  resumenes <- lapply(tamanos, function(n) {
    lupa:::.resumen_casi_clave(c(seq_len(n - 1L), 1L))
  })

  expect_equal(
    vapply(resumenes, `[[`, logical(1L), "es_casi_clave"),
    c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )
  expect_equal(
    vapply(resumenes, `[[`, integer(1L), "n_distintos"),
    tamanos - 1L
  )
  expect_equal(
    vapply(resumenes, `[[`, numeric(1L), "tasa_distintos"),
    (tamanos - 1L) / tamanos
  )
  expect_true(all(vapply(
    resumenes, `[[`, integer(1L), "min_filas"
  ) == 100L))
})

test_that("datos_operativos no produce casi_claves", {
  hallazgos <- .tipos_casi_clave_r115(datos_operativos)
  expect_equal(nrow(hallazgos), 0L)
  expect_false(any(
    detectar_claves(datos_operativos, max_combinacion = 1L)$casi_clave
  ))

  variables <- clasificar_variables(
    datos_operativos,
    perfilar(datos_operativos, analizar_dependencias = FALSE)
  )
  observadas <- variables[match(
    c("id_evento", "fecha_evento", "monto", "contacto"),
    variables$columna
  ), c("columna", "rol", "confianza")]
  expect_equal(
    observadas$rol,
    c("identificador", "fecha", "medida", "categoria")
  )
  expect_equal(observadas$confianza, c(0.9, 0.85, 0.65, 0.55))
})

test_that("fechas y fecha-hora no son casi_claves", {
  dias <- as.Date("2024-01-01") + c(0:298, 150)
  instantes <- as.POSIXct(dias, tz = "UTC")
  fechas_texto <- format(dias, "%Y-%m-%d")
  datos <- data.frame(
    dias = dias, instantes = instantes, fechas_texto = fechas_texto
  )

  expect_equal(nrow(.tipos_casi_clave_r115(datos)), 0L)
  resumenes <- list(
    lupa:::.resumen_casi_clave(dias),
    lupa:::.resumen_casi_clave(instantes),
    lupa:::.resumen_casi_clave(fechas_texto)
  )
  expect_true(all(vapply(resumenes, `[[`, character(1L), "rol") == "fecha"))
  expect_false(any(vapply(
    resumenes, `[[`, logical(1L), "es_casi_clave"
  )))
  expect_false(any(detectar_claves(datos, max_combinacion = 1L)$casi_clave))
})

test_that("los cuatro positivos conservan casi_clave y declaran el minimo", {
  positivos <- list(
    densa_2707 = .datos_casi_clave_r115(TRUE),
    aleatoria_2707 = .datos_casi_clave_r115(FALSE),
    doble_296 = as.double(c(1:295, 150)),
    entero_296 = as.integer(c(1:295, 150))
  )

  for (nombre in names(positivos)) {
    hallazgo <- .tipos_casi_clave_r115(data.frame(valor = positivos[[nombre]]))
    expect_equal(nrow(hallazgo), 1L, info = nombre)
    expect_match(
      hallazgo$evidencia,
      "criterio_casi_clave: n_filas>=100",
      fixed = TRUE, info = nombre
    )
  }
})
