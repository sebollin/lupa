test_that("la no-medicion vive fuera de hallazgos y se cuenta por diagnostico", {
  set.seed(9001)
  datos <- as.data.frame(setNames(
    replicate(40L, sample(c("alpha", "beta", "gamma"), 100L, TRUE),
             simplify = FALSE),
    paste0("v", seq_len(40L))
  ), stringsAsFactors = FALSE)
  local_mocked_bindings(
    .stringdist_disponible = function() FALSE,
    .package = "lupa"
  )
  sin_stringdist <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_equal(nrow(sin_stringdist$hallazgos), 0L)
  expect_equal(nrow(sin_stringdist$cobertura_diagnosticos), 40L)
  expect_true(all(
    sin_stringdist$cobertura_diagnosticos$diagnostico ==
      "proximidad_vocabulario"
  ))
  salida <- capture.output(print(sin_stringdist), type = "message")
  expect_true(any(grepl("0 hallazgos sospechosos", salida, fixed = TRUE)))
  expect_true(any(grepl("40 diagnosticos no evaluados", salida, fixed = TRUE)))
})

test_that("con stringdist una columna limpia no registra no-medicion", {
  skip_if_not_installed("stringdist")
  set.seed(9002)
  datos <- data.frame(
    a = sample(sprintf("valor-%03d", seq_len(100L))),
    b = sample(sprintf("otro-%03d", seq_len(100L))),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_equal(nrow(perfil$cobertura_diagnosticos), 0L)
  negativos <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(negativos$columna, c("a", "b"))
  expect_true(all(as.character(negativos$severidad) == "ok"))
  expect_true(all(negativos$n_afectados == 0))
})

test_that("una no-medicion no oculta un hallazgo real", {
  local_mocked_bindings(
    .stringdist_disponible = function() FALSE,
    .package = "lupa"
  )
  perfil <- perfilar(
    data.frame(nombre = c("Montevideo", "MONTEVIDEO", "Montevido")),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(as.character(hallazgo$severidad), "sospechoso")
  expect_equal(nrow(perfil$cobertura_diagnosticos), 1L)
})

test_that("unicode, integer64 y zona sin declarar se consolidan en la cobertura", {
  local_mocked_bindings(
    .stringi_disponible = function() FALSE,
    .bit64_disponible = function() FALSE,
    .package = "lupa"
  )
  # La columna se marca DESPUES de armar el data.frame, no antes. Armarlo con
  # una columna ya de clase `integer64` obliga a `as.data.frame()` a buscar el
  # metodo de bit64, asi que la prueba de "no esta bit64" exigia que bit64
  # estuviera instalado. Y este camino es ademas el realista: la columna llega
  # marcada dentro de una tabla que ya existe -de un RDS, de un driver- en una
  # maquina donde bit64 no esta.
  datos <- data.frame(
    texto = c("Jose", "Jos\u0301e"), codigo = c(1, 2),
    fecha = as.POSIXct(c("2026-01-01 12:00:00", "2026-01-01 13:00:00"))
  )
  class(datos$codigo) <- "integer64"
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE, proteger_datos_personales = FALSE
  )
  tipos <- perfil$cobertura_diagnosticos$diagnostico
  expect_true(all(c(
    "normalizacion_unicode", "integer64_sin_soporte",
    "zona_horaria_fecha_hora"
  ) %in% tipos))
  expect_false(any(perfil$hallazgos$tipo_hallazgo %in% c(
    "normalizacion_unicode", "integer64_sin_soporte",
    "zona_horaria_fecha_hora"
  )))
})

test_that("una zona declarada se mide y no entra en la cobertura", {
  fecha <- as.POSIXct(
    c("2026-01-01 23:59:00", "2026-01-01 12:00:00"),
    tz = "America/Montevideo"
  )
  perfil <- perfilar(
    data.frame(fecha = fecha), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE, proteger_datos_personales = FALSE
  )
  expect_equal(perfil$columnas$zona_horaria_origen, "America/Montevideo")
  expect_equal(perfil$columnas$n_filas_fecha_civil_distinta_utc, 1L)
  expect_false("zona_horaria_fecha_hora" %in%
                 perfil$cobertura_diagnosticos$diagnostico)
  expect_true(any(
    perfil$hallazgos$tipo_hallazgo == "zona_horaria_fecha_hora" &
      perfil$hallazgos$severidad == "sospechoso"
  ))
})
