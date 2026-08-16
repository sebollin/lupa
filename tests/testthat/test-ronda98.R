test_that("detecta suma rota y proporcionalidad con constante descubierta", {
  n <- 200L
  neto <- seq_len(n) * 10
  iva <- neto * 0.22
  total <- neto + iva
  total[[10L]] <- total[[10L]] + 500

  perfil <- perfilar(
    data.frame(neto = neto, iva = iva, total = total),
    analizar_dependencias = FALSE
  )
  relaciones <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "relacion_aritmetica_columnas", ,
    drop = FALSE
  ]

  expect_equal(nrow(relaciones), 2L)
  suma <- relaciones[grepl("neto,iva,total", relaciones$columna, fixed = TRUE), ]
  proporcion <- relaciones[relaciones$columna == "neto,iva", ]
  expect_equal(nrow(suma), 1L)
  expect_equal(suma$n_evaluados, 200)
  expect_equal(suma$n_afectados, 1)
  expect_equal(as.character(suma$severidad), "sospechoso")
  expect_match(suma$descripcion, "Se observó", fixed = TRUE)
  expect_false(grepl("debe ser", suma$descripcion, fixed = TRUE))
  expect_match(suma$evidencia, "0.995 de cumplimiento", fixed = TRUE)
  expect_match(suma$evidencia, "Tolerancia declarada: 1e-08", fixed = TRUE)
  expect_match(suma$evidencia, "fila 10", fixed = TRUE)
  expect_equal(suma$trazabilidad[[1L]]$indices_fila, 10L)

  expect_equal(nrow(proporcion), 1L)
  expect_equal(as.character(proporcion$severidad), "ok")
  expect_equal(proporcion$n_afectados, 0)
  expect_match(proporcion$descripcion, "iva ~= neto * 0.22", fixed = TRUE)
  expect_match(proporcion$evidencia, "Constante observada k=0.22", fixed = TRUE)
  expect_match(proporcion$evidencia, "neto ~= iva / 0.22", fixed = TRUE)
  expect_equal(perfil$meta$aritmetica_columnas$umbral_cumplimiento, 0.995)
  expect_equal(perfil$meta$aritmetica_columnas$tolerancia, 1e-8)
})

test_that("detecta una resta como identidad aritmetica equivalente", {
  set.seed(9801)
  debe <- runif(200L, 1000, 5000)
  haber <- runif(200L, 10, 900)
  saldo <- debe - haber
  saldo[[17L]] <- saldo[[17L]] - 100
  perfil <- perfilar(
    data.frame(saldo = saldo, debe = debe, haber = haber),
    analizar_dependencias = FALSE
  )
  relaciones <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "relacion_aritmetica_columnas", ,
    drop = FALSE
  ]
  expect_equal(nrow(relaciones), 1L)
  expect_equal(relaciones$columna, "saldo,haber,debe")
  expect_match(
    relaciones$evidencia, "saldo ~= debe - haber", fixed = TRUE
  )
  expect_equal(relaciones$trazabilidad[[1L]]$indices_fila, 17L)
})

test_that("umbral y tolerancia aritmeticos cambian resultados visiblemente", {
  set.seed(9802)
  x <- runif(200L, 10, 20)
  y <- runif(200L, 30, 40)
  z <- x + y
  z[1:2] <- z[1:2] + 5
  predeterminado <- perfilar(
    data.frame(x, y, z), analizar_dependencias = FALSE
  )
  flexible <- perfilar(
    data.frame(x, y, z), analizar_dependencias = FALSE,
    umbral_aritmetica = 0.99
  )
  expect_false(any(predeterminado$hallazgos$tipo_hallazgo ==
                   "relacion_aritmetica_columnas"))
  expect_true(any(flexible$hallazgos$tipo_hallazgo ==
                  "relacion_aritmetica_columnas"))
  expect_match(
    flexible$hallazgos$evidencia[
      flexible$hallazgos$tipo_hallazgo == "relacion_aritmetica_columnas"
    ],
    "0.990 de cumplimiento", fixed = TRUE
  )

  casi <- data.frame(x = x, y = y, z = x + y + 1e-5)
  estricto <- perfilar(casi, analizar_dependencias = FALSE)
  tolerante <- perfilar(
    casi, analizar_dependencias = FALSE, tolerancia_aritmetica = 1e-4
  )
  expect_false(any(estricto$hallazgos$tipo_hallazgo ==
                   "relacion_aritmetica_columnas"))
  expect_true(any(tolerante$hallazgos$tipo_hallazgo ==
                  "relacion_aritmetica_columnas"))
  expect_match(
    tolerante$hallazgos$evidencia[
      tolerante$hallazgos$tipo_hallazgo == "relacion_aritmetica_columnas"
    ],
    "Tolerancia declarada: 1e-04", fixed = TRUE
  )
})

test_that("diez columnas numericas independientes no generan relaciones", {
  set.seed(9803)
  datos <- as.data.frame(replicate(10L, rnorm(200L)))
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  expect_equal(sum(
    perfil$hallazgos$tipo_hallazgo == "relacion_aritmetica_columnas"
  ), 0L)
  expect_null(perfil$meta$aritmetica_columnas)
})

test_that("los NA quedan fuera del universo y las fechas no participan", {
  n <- 205L
  neto <- seq_len(n) * 3
  iva <- neto * 0.22
  total <- neto + iva
  ausentes <- c(2L, 30L, 70L, 120L, 180L)
  neto[ausentes] <- NA_real_
  total[[10L]] <- total[[10L]] + 500
  datos <- data.frame(
    fecha = as.Date("2024-01-01") + seq_len(n),
    neto = neto, iva = iva, total = total
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  relaciones <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "relacion_aritmetica_columnas", ,
    drop = FALSE
  ]
  suma <- relaciones[
    lengths(strsplit(relaciones$columna, ",", fixed = TRUE)) == 3L, ,
    drop = FALSE
  ]
  expect_equal(nrow(suma), 1L)
  expect_equal(suma$n_evaluados[[1L]], 200)
  expect_match(suma$evidencia[[1L]], "universo: 200 de 205", fixed = TRUE)
  expect_equal(suma$trazabilidad[[1L]]$indices_fila, 10L)
  expect_false(any(grepl("fecha", relaciones$columna, fixed = TRUE)))
  expect_false("fecha" %in% perfil$meta$aritmetica_columnas$columnas_numericas)
  expect_true("Date" %in% perfil$meta$aritmetica_columnas$clases_excluidas)
})

test_that("el limite aritmetico actua y queda en cobertura", {
  set.seed(9804)
  datos <- as.data.frame(replicate(21L, rnorm(200L)))
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico ==
      "relacion_aritmetica_columnas", , drop = FALSE
  ]
  expect_equal(nrow(cobertura), 1L)
  expect_equal(cobertura$columna, "V21")
  expect_match(cobertura$motivo, "3420 de 3990", fixed = TRUE)
  expect_match(cobertura$motivo, "190 de 210", fixed = TRUE)
  expect_true(perfil$meta$aritmetica_columnas$truncado)
  expect_equal(
    length(perfil$meta$aritmetica_columnas$columnas_analizadas), 20L
  )
})

test_that("sin columnas numericas no agrega ruido", {
  datos <- data.frame(
    texto = rep(c("a", "b"), 20L),
    fecha = as.Date("2024-01-01") + seq_len(40L),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  expect_false(any(perfil$hallazgos$tipo_hallazgo ==
                   "relacion_aritmetica_columnas"))
  expect_false("relacion_aritmetica_columnas" %in%
               perfil$cobertura_diagnosticos$diagnostico)
  expect_null(perfil$meta$aritmetica_columnas)
})

test_that("un perfil sin relaciones es identico sin el detector", {
  set.seed(9805)
  datos <- as.data.frame(replicate(10L, rnorm(200L)))
  fecha <- as.POSIXct("2026-08-15 12:00:00", tz = "UTC")
  perfil_actual <- perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE
  )
  testthat::local_mocked_bindings(
    .detectar_aritmetica_columnas = function(datos, umbral, tolerancia,
                                              max_columnas) {
      list(
        hallazgos = list(),
        alcance = lupa:::.alcance_aritmetica_columnas(
          datos, seq_along(datos), seq_along(datos), umbral, tolerancia,
          max_columnas
        ),
        cobertura = lupa:::.cobertura_diagnosticos_vacia()
      )
    },
    .package = "lupa"
  )
  perfil_sin_detector <- perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE
  )
  expect_identical(perfil_actual, perfil_sin_detector)
})

test_that("valida los argumentos aritmeticos publicos", {
  datos <- data.frame(a = 1:3, b = 4:6)
  expect_error(perfilar(datos, umbral_aritmetica = 0), "umbral_aritmetica")
  expect_error(
    perfilar(datos, tolerancia_aritmetica = -1), "tolerancia_aritmetica"
  )
  expect_error(
    perfilar(datos, max_columnas_aritmetica = 1),
    "max_columnas_aritmetica"
  )
})
