# Paso 3: `medir(detalle = "resumen")`, clase `medicion_resumen`, `no_resumible` y B1.

test_that("medir(detalle = 'resumen') devuelve medicion_resumen sin heredar de medicion y tablero identico", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(
    id = 1:8,
    n = c(1, 2, NA, 4, 2, 100, 3, NA),
    t = c("x", "y", "n/a", "x", "z", NA, "x", "y"),
    l = c(TRUE, FALSE, TRUE, TRUE, FALSE, NA, TRUE, TRUE),
    f = as.Date(c("2026-01-01", "2026-01-05", NA, "2026-01-12",
                  "2026-01-03", "2026-01-02", "2026-01-15", "2026-01-04")),
    f2 = as.Date(c("2026-01-02", "2026-01-04", "2026-01-06", "2026-01-12",
                   "2026-01-05", "2026-01-03", "2026-01-14", "2026-01-09")),
    stringsAsFactors = FALSE
  )
  ref <- data.frame(k = c(1L, 2L, 3L, 4L))
  contrato <- vigencia("f", fecha_acceso = as.Date("2026-02-15"),
                       fecha_limite = as.Date("2026-01-10"))
  mod <- modelo(
    instanciar(especializar(nuc$NoNulo, "NN_n"), "d", "n"),
    instanciar(especializar(nuc$NoNulo, "NN_cent", valores_nulos = c("n/a", "")), "d", "t"),
    instanciar(especializar(nuc$NoNulo, "NN_apl", aplicable = ~ l), "d", "n"),
    instanciar(especializar(nuc$ValoresPosiblesPorExtension, "Dom", valores = c("x", "y")), "d", "t"),
    instanciar(especializar(nuc$Formato, "Dic", diccionario = c("x", "y", "z")), "d", "t"),
    instanciar(especializar(nuc$Formato, "Rx", expresion_regular = "^[a-z]+$"), "d", "t"),
    instanciar(especializar(nuc$ValoresPosiblesPorComprension, "Rng", minimo = 0, maximo = 10), "d", "n"),
    instanciar(especializar(nuc$AtributoDuplicado, "Dup"), "d", "n"),
    instanciar(especializar(nuc$ConjuntoAtributosDuplicado, "DupC"), "d", c("n", "t")),
    instanciar(especializar(nuc$OportunidadAtributoPorFecha, "OpA", fecha_limite = as.Date("2026-01-10")), "d", "f"),
    instanciar(especializar(nuc$OportunidadEntPorFecha, "OpE", vigencia = contrato), "d"),
    instanciar(especializar(nuc$ReglaIntegridadIntraEntidad, "Ord",
                            regla = ~ is.na(f) | is.na(f2) | f <= f2),
               "d", c("f", "f2")),
    instanciar(especializar(nuc$ReglaIntegridadInterEntidad, "FK"), c("ref", "d"), c("k", "n")),
    instanciar(especializar(nuc$ErrorEstandar, "SE"), "d", "n"),
    instanciar(especializar(nuc$GradoOportunidadAtributoPorFecha, "Grado",
                            fecha_solicitud = as.Date("2025-12-01"),
                            fecha_fin_utilidad = as.Date("2026-02-28")), "d", "f"),
    instanciar(especializar(nuc$EntidadDuplicada, "ED"), "d")
  )
  datos <- list(ref = ref, d = d)
  mc <- suppressWarnings(medir(mod, datos, id_medicion = "m3", fecha = fecha,
                               proteger_datos_personales = TRUE))
  mr <- suppressWarnings(medir(mod, datos, id_medicion = "m3", fecha = fecha,
                               proteger_datos_personales = TRUE,
                               detalle = "resumen"))

  expect_s3_class(mr, "medicion_resumen")
  expect_false(inherits(mr, "medicion"))
  expect_identical(attr(mr, "detalle", exact = TRUE), "resumen")
  expect_identical(names(mr), lupa:::.COLUMNAS_RESUMEN)
  expect_true("unidad_conteo" %in% names(mr))
  expect_false("unidad" %in% names(mr))

  expect_identical(attr(mr, "cobertura_metricas"), attr(mc, "cobertura_metricas"))
  expect_identical(attr(mr, "alcance_medidas"), attr(mc, "alcance_medidas"))
  expect_identical(attr(mr, "configuracion_modelo"), attr(mc, "configuracion_modelo"))
  expect_identical(tablero_calidad(mr), tablero_calidad(mc))
})

test_that("una metrica duracion/entero por celda queda no_resumible en el resumen (M4)", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(f = as.Date(c("2026-01-01", "2026-01-05", NA)))
  contrato <- vigencia("f", fecha_acceso = as.Date("2026-02-15"),
                       fecha_ultimo_cambio = as.Date("2026-02-01"),
                       frecuencia_cambio = 10)
  mod <- modelo(
    instanciar(especializar(nuc$NoNulo, "NN"), "d", "f"),
    instanciar(especializar(nuc$DesactualizacionPorFecha, "DF", vigencia = contrato), "d"),
    instanciar(especializar(nuc$DesactualizacionPorCambios, "DC", vigencia = contrato), "d")
  )
  mr <- medir(mod, list(d = d), id_medicion = "m3m4", fecha = fecha,
              proteger_datos_personales = TRUE, detalle = "resumen")
  expect_identical(mr$metrica_instanciada, "NN@d.f")
  cob <- attr(mr, "cobertura_metricas", exact = TRUE)
  expect_true(all(c("DF@d", "DC@d") %in% cob$metrica_instanciada))
  expect_identical(cob$estado[cob$metrica_instanciada %in% c("DF@d", "DC@d")],
                   c("no_resumible", "no_resumible"))
})

test_that("un metodo que produce mas medidas que su universo marca medidas_repetidas (B1)", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  m_doble <- metrica(
    "MedidaDoble", "Produce dos medidas por fila.", "instanciaAtributo",
    "booleano", dimension = "Exactitud", factor = "Exactitud sint\u00e1ctica",
    metodo = function(tablas, instancia) {
      x <- tablas[[instancia$entidad[[1L]]]][[instancia$atributos[[1L]]]]
      filas <- rep(seq_along(x), each = 2L)
      data.frame(
        resultado = rep(TRUE, length(filas)),
        entidad = instancia$entidad[[1L]],
        atributo = instancia$atributos[[1L]],
        fila = filas,
        objeto = paste0(instancia$entidad[[1L]], "$", instancia$atributos[[1L]], "[", filas, "]#", seq_along(filas)),
        stringsAsFactors = FALSE
      )
    }
  )
  d <- data.frame(x = c(1, 2, 3))
  mod <- modelo(instanciar(especializar(m_doble, "MD"), "d", "x"))
  mr <- medir(mod, list(d = d), id_medicion = "m3b1", fecha = fecha,
              proteger_datos_personales = TRUE, detalle = "resumen")
  expect_identical(mr$n_universo, 3)
  expect_identical(mr$denominador, 6)
  expect_true(is.na(mr$n_no_evaluados))
  expect_identical(mr$motivo_no_evaluados, "medidas_repetidas")
})

test_that("detalle = 'completo' es la opcion por omision y mantiene clase medicion", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  d <- data.frame(x = c(1, NA, 3))
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "NN"), "d", "x"))
  m_def <- medir(mod, list(d = d), id_medicion = "m3c", fecha = fecha,
                 proteger_datos_personales = TRUE)
  m_exp <- medir(mod, list(d = d), id_medicion = "m3c", fecha = fecha,
                 proteger_datos_personales = TRUE, detalle = "completo")
  expect_s3_class(m_def, "medicion")
  expect_identical(m_def, m_exp)
})
