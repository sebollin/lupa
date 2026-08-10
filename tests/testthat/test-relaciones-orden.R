test_that("el perfil detecta fechas invertidas y conserva la trazabilidad", {
  datos <- data.frame(
    inicio = as.Date(c("2024-01-10", "2024-05-01", "2024-03-01")),
    fin = as.Date(c("2024-02-10", "2024-04-01", "2024-06-01"))
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "relacion_orden_columnas", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(hallazgo$columna, "inicio,fin")
  expect_equal(hallazgo$n_evaluados, 3)
  expect_equal(hallazgo$n_afectados, 1)
  expect_equal(hallazgo$unidad_conteo, "fila")
  expect_equal(hallazgo$severidad, ordered("sospechoso", levels = c(
    "ok", "sospechoso", "error"
  )))
  expect_match(hallazgo$sugerencia, "ReglaIntegridadIntraEntidad", fixed = TRUE)
  expect_equal(hallazgo$trazabilidad[[1L]]$indices_fila, 2L)
  expect_equal(hallazgo$trazabilidad[[1L]]$total, 1)
})

test_that("el perfil detecta una relación numérica invertida", {
  datos <- data.frame(
    monto_bruto = c(100, 200, 300),
    monto_neto = c(110, 190, 330)
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "relacion_orden_columnas", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(hallazgo$n_evaluados, 3)
  expect_equal(hallazgo$n_afectados, 1)
  expect_match(hallazgo$evidencia, "monto_bruto=200", fixed = TRUE)
  expect_match(hallazgo$evidencia, "monto_neto=190", fixed = TRUE)
})

test_that("no se agregan relaciones a una tabla sin orden dominante", {
  set.seed(6701)
  datos <- as.data.frame(replicate(8L, rnorm(200L)))
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  expect_false(any(
    perfil$hallazgos$tipo_hallazgo == "relacion_orden_columnas"
  ))
  expect_equal(perfil$meta$orden_columnas$pares_analizados, 28)
})

test_that("el alcance declara las columnas omitidas por el límite", {
  set.seed(6702)
  datos <- as.data.frame(replicate(21L, rnorm(30L)))
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, max_columnas_orden = 20L
  )
  alcance <- perfil$meta$orden_columnas
  expect_true(alcance$truncado)
  expect_equal(length(alcance$columnas_analizadas), 20L)
  expect_equal(alcance$columnas_omitidas, "V21")
  expect_equal(alcance$pares_analizados, choose(20, 2))
})

test_that("una relación completa no se informa y los conteos son exactos", {
  datos <- data.frame(
    inicio = as.Date("2024-01-01") + 0:99,
    fin = as.Date("2024-02-01") + 0:99,
    bruto = seq(100, 199),
    neto = seq(110, 209)
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  relaciones <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "relacion_orden_columnas", , drop = FALSE
  ]
  expect_equal(nrow(relaciones), 0L)
})

test_that("el perfil sin relaciones conserva sus hallazgos previos", {
  datos <- data.frame(
    estado = rep(c("activo", "inactivo"), 20L),
    importe = seq(1, 40),
    comentario = rep("ok", 40L),
    stringsAsFactors = FALSE
  )
  perfil_optimizado <- perfilar(datos, analizar_dependencias = FALSE)
  testthat::local_mocked_bindings(
    .detectar_orden_columnas = function(...) list(
      hallazgos = list(),
      alcance = list(
        filas_evaluadas = nrow(datos), columnas_comparables = character(),
        columnas_analizadas = character(), columnas_omitidas = character(),
        pares_comparables = 0, pares_analizados = 0, truncado = FALSE,
        max_columnas = 20L, umbral_cumplimiento = 0.95, minimo_filas = 3L
      )
    ),
    .package = "lupa"
  )
  perfil_sin_detector <- perfilar(datos, analizar_dependencias = FALSE)
  expect_identical(
    perfil_optimizado$hallazgos,
    perfil_sin_detector$hallazgos
  )
})
