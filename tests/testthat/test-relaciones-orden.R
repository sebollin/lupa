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

test_that("conserva los conteos de relaciones raras en padrones grandes", {
  n <- 2000L
  inicio <- as.Date("2024-01-01") + seq_len(n)
  fin <- inicio + 30L
  fin[seq_len(40L)] <- inicio[seq_len(40L)] - 1L
  bruto <- seq(1000, length.out = n)
  neto <- bruto + 100
  neto[seq_len(25L)] <- bruto[seq_len(25L)] - 1
  perfil <- perfilar(
    data.frame(inicio = inicio, fin = fin, bruto = bruto, neto = neto),
    analizar_dependencias = FALSE
  )
  relaciones <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "relacion_orden_columnas", , drop = FALSE
  ]
  expect_equal(relaciones$columna, c("inicio,fin", "bruto,neto"))
  expect_equal(relaciones$n_evaluados, c(n, n))
  expect_equal(relaciones$n_afectados, c(40, 25))
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

test_that("descarta relaciones entre magnitudes sin solapamiento", {
  set.seed(6801)
  datos <- data.frame(
    edad = sample(18:95, 3000L, replace = TRUE),
    monto = runif(3000L, 100, 90000),
    anio = sample(2000:2025, 3000L, replace = TRUE),
    cantidad = sample(1:10, 3000L, replace = TRUE),
    texto = c(" borde", rep("ok", 2999L))
  )
  perfil_predeterminado <- perfilar(datos, analizar_dependencias = FALSE)
  relaciones <- perfil_predeterminado$hallazgos[
    perfil_predeterminado$hallazgos$tipo_hallazgo ==
      "relacion_orden_columnas", , drop = FALSE
  ]
  expect_equal(relaciones$columna, "anio,monto")
  expect_equal(perfil_predeterminado$meta$orden_columnas$pares_descartados_magnitud, 0)
  expect_equal(perfil_predeterminado$meta$orden_columnas$umbral_solapamiento_iqr, 0)

  perfil_filtrado <- perfilar(
    datos, analizar_dependencias = FALSE, umbral_solapamiento_orden = 0.4
  )
  expect_gt(length(perfil_filtrado$hallazgos$tipo_hallazgo), 0L)
  expect_false(any(
    perfil_filtrado$hallazgos$tipo_hallazgo == "relacion_orden_columnas"
  ))
  alcance <- perfil_filtrado$meta$orden_columnas
  expect_equal(alcance$pares_descartados_magnitud, 6)
  expect_equal(alcance$pares_evaluados_orden, 0)
  expect_equal(alcance$umbral_solapamiento_iqr, 0.4)
})

test_that("el valor por omisión conserva relaciones reales de rango distinto", {
  n <- 100L

  nacimiento <- as.Date("1990-01-01") + seq_len(n)
  solicitud <- as.Date("2020-01-01") + seq_len(n)
  solicitud[[1L]] <- as.Date("1980-01-01")
  perfil_fechas <- perfilar(
    data.frame(nacimiento = nacimiento, solicitud = solicitud),
    analizar_dependencias = FALSE
  )
  hallazgo_fechas <- perfil_fechas$hallazgos[
    perfil_fechas$hallazgos$tipo_hallazgo == "relacion_orden_columnas", ,
    drop = FALSE
  ]
  expect_equal(hallazgo_fechas$columna, "nacimiento,solicitud")
  expect_equal(hallazgo_fechas$n_afectados, 1)

  descuento <- rep(100, n)
  bruto <- rep(10000, n)
  descuento[[1L]] <- 20000
  perfil_montos <- perfilar(
    data.frame(descuento = descuento, bruto = bruto),
    analizar_dependencias = FALSE
  )
  hallazgo_montos <- perfil_montos$hallazgos[
    perfil_montos$hallazgos$tipo_hallazgo == "relacion_orden_columnas", ,
    drop = FALSE
  ]
  expect_equal(hallazgo_montos$columna, "descuento,bruto")
  expect_equal(hallazgo_montos$n_afectados, 1)

  menor <- rep(10, n)
  titular <- rep(100, n)
  menor[[1L]] <- 200
  perfil_personas <- perfilar(
    data.frame(menor = menor, titular = titular),
    analizar_dependencias = FALSE
  )
  hallazgo_personas <- perfil_personas$hallazgos[
    perfil_personas$hallazgos$tipo_hallazgo == "relacion_orden_columnas", ,
    drop = FALSE
  ]
  expect_equal(hallazgo_personas$columna, "menor,titular")
  expect_equal(hallazgo_personas$n_afectados, 1)
})

test_that("el solapamiento funciona con fechas y POSIXct", {
  datos <- data.frame(
    inicio = as.POSIXct(c("2024-01-10", "2024-05-01", "2024-03-01"),
                        tz = "UTC"),
    fin = as.POSIXct(c("2024-02-10", "2024-04-01", "2024-06-01"),
                     tz = "UTC")
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  expect_equal(sum(perfil$hallazgos$tipo_hallazgo ==
                   "relacion_orden_columnas"), 1L)
  expect_equal(perfil$meta$orden_columnas$pares_evaluados_orden, 1)
})

test_that("los rangos intercuartiles de anchura cero no dividen por cero", {
  iguales <- data.frame(a = rep(5, 30L), b = rep(5, 30L))
  distintos <- data.frame(a = rep(5, 30L), b = rep(9, 30L))
  perfil_iguales <- perfilar(iguales, analizar_dependencias = FALSE)
  perfil_distintos <- perfilar(distintos, analizar_dependencias = FALSE)
  perfil_distintos_filtrado <- perfilar(
    distintos, analizar_dependencias = FALSE,
    umbral_solapamiento_orden = 0.4
  )
  expect_false(any(perfil_iguales$hallazgos$tipo_hallazgo ==
                   "relacion_orden_columnas"))
  expect_false(any(perfil_distintos$hallazgos$tipo_hallazgo ==
                   "relacion_orden_columnas"))
  expect_equal(perfil_iguales$meta$orden_columnas$pares_descartados_magnitud, 0)
  expect_equal(perfil_distintos$meta$orden_columnas$pares_descartados_magnitud, 0)
  expect_equal(
    perfil_distintos_filtrado$meta$orden_columnas$pares_descartados_magnitud,
    1
  )
})
