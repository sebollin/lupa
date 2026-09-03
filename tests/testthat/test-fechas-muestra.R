test_that("el resumen de fechas declara lo que descarta la muestra", {
  valores <- c(
    rep("2024-01-01", 95L),
    "01/01/2023", "2023/12/31", "1-ene-2022", "2022-13-99", "hola"
  )
  perfil <- perfilar(
    data.frame(fecha = valores, stringsAsFactors = FALSE),
    muestra = 20L, analizar_dependencias = FALSE,
    ausencia_estructural = FALSE, proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas[perfil$columnas$columna == "fecha", , drop = FALSE]
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "resumen_cuantitativo" &
      perfil$cobertura_diagnosticos$columna == "fecha", , drop = FALSE
  ]

  expect_equal(fila$n_fechas_resumidas, 95L)
  expect_equal(fila$n_fechas_excluidas_granularidad, 5L)
  expect_equal(fila$estado_resumen_cuantitativo, "calculados_sobre_dias")
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$motivo, "5")
  expect_false("formatos_fecha_mixtos" %in%
                 as.character(perfil$hallazgos$tipo_hallazgo))
})

test_that("muestra Inf conserva el resumen de fechas completo", {
  valores <- c(
    rep("2024-01-01", 95L),
    "01/01/2023", "2023/12/31", "1-ene-2022", "2022-13-99", "hola"
  )
  perfil <- perfilar(
    data.frame(fecha = valores, stringsAsFactors = FALSE),
    muestra = Inf, analizar_dependencias = FALSE,
    ausencia_estructural = FALSE, proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas[perfil$columnas$columna == "fecha", , drop = FALSE]

  expect_equal(fila$minimo_fecha, "2022-01-01")
  expect_equal(fila$maximo_fecha, "2024-01-01")
  expect_equal(fila$n_fechas_resumidas, 97L)
  expect_equal(fila$n_fechas_excluidas_granularidad, 0L)
  expect_equal(fila$estado_resumen_cuantitativo, "calculados")
  expect_equal(nrow(perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "resumen_cuantitativo" &
      perfil$cobertura_diagnosticos$columna == "fecha", , drop = FALSE
  ]), 0L)
})

test_that("el resumen numerico declara conversiones descartadas de la muestra", {
  valores <- c(rep("10", 99L), "no es numero")
  perfil <- perfilar(
    data.frame(valor = valores, stringsAsFactors = FALSE),
    muestra = 20L, analizar_dependencias = FALSE,
    ausencia_estructural = FALSE, proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas[perfil$columnas$columna == "valor", , drop = FALSE]
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "resumen_cuantitativo" &
      perfil$cobertura_diagnosticos$columna == "valor", , drop = FALSE
  ]

  expect_equal(fila$minimo, 10)
  expect_equal(fila$maximo, 10)
  expect_equal(fila$n_valores_excluidos_resumen, 1L)
  expect_equal(fila$estado_resumen_cuantitativo, "calculados_sobre_valores")
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$motivo, "1")
})

test_that("el analisis temporal declara fechas que el formato muestreado no parsea", {
  valores <- c(
    rep("2024-01-01", 95L),
    "01/01/2023", "2023/12/31", "1-ene-2022", "2022-13-99", "hola"
  )
  datos <- data.frame(fecha = valores, stringsAsFactors = FALSE)
  perfil <- perfilar(
    datos, muestra = 20L, analizar_dependencias = FALSE,
    ausencia_estructural = FALSE, proteger_datos_personales = FALSE
  )
  resultado <- analizar_tiempo(
    datos, perfil = perfil, columnas = "fecha", frecuencia_dias = 1
  )

  expect_equal(resultado$resumen$n_fechas_excluidas_parseo, 5L)
  expect_equal(
    resultado$resumen$estado_resumen,
    "calculados_sobre_fechas_parseadas"
  )
  expect_equal(resultado$resumen$fecha_minima, as.Date("2024-01-01"))
})

test_that("la relacion de orden no compara columnas con conversion parcial", {
  valores <- c(
    rep("2024-01-01", 95L),
    "01/01/2023", "2023/12/31", "1-ene-2022", "2022-13-99", "hola"
  )
  perfil <- perfilar(
    data.frame(inicio = valores, fin = rep("2025-01-01", 100L)),
    muestra = 20L, analizar_dependencias = FALSE,
    ausencia_estructural = FALSE, proteger_datos_personales = FALSE
  )
  alcance <- perfil$meta$orden_columnas

  expect_equal(alcance$columnas_conversion_parcial, "inicio")
  expect_equal(alcance$n_valores_excluidos_conversion, 5)
  expect_equal(alcance$pares_descartados_conversion_parcial, 1)
  expect_false(any(perfil$hallazgos$tipo_hallazgo ==
                     "relacion_orden_columnas"))
})
