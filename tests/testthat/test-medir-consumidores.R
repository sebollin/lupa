# Paso 4: consumidores de `medicion_resumen`, F2 (`rbind` incompatible) y F3 (universo vacio).

test_that("tablero_calidad, indice_calidad y cobertura_analisis son identical() a la completa", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(
    i = c(rep(NA_integer_, 38L), 1L, 2L),
    t = rep(c("x", "y", NA, "z"), 10L),
    r = seq_len(40L),
    stringsAsFactors = FALSE
  )
  mod <- modelo(
    instanciar(especializar(nuc$NoNulo, "NN"), "d", "i", nombre_instancia = "NN_i"),
    instanciar(especializar(nuc$NoNulo, "NN"), "d", "t", nombre_instancia = "NN_t"),
    instanciar(especializar(nuc$ValoresPosiblesPorComprension, "Rng", minimo = 1, maximo = 20), "d", "r"),
    instanciar(especializar(nuc$AtributoDuplicado, "Dup"), "d", "t")
  )
  mc <- medir(mod, list(d = d), id_medicion = "c1", fecha = fecha,
              proteger_datos_personales = TRUE)
  mr <- medir(mod, list(d = d), id_medicion = "c1", fecha = fecha,
              proteger_datos_personales = TRUE, detalle = "resumen")

  tc <- tablero_calidad(mc)
  tr <- tablero_calidad(mr)
  expect_identical(tr, tc)

  pesos <- c(Completitud = 0.5, Consistencia = 0.25, Unicidad = 0.25)
  internos <- c("componente-0001" = 0.5, "componente-0002" = 0.5)
  expect_identical(
    indice_calidad(mr, pesos, pesos_internos = internos),
    indice_calidad(mc, pesos, pesos_internos = internos)
  )

  perfil <- suppressWarnings(perfilar(d, proteger_datos_personales = TRUE))
  expect_identical(cobertura_analisis(perfil, mr), cobertura_analisis(perfil, mc))
})

test_that("evaluar, agregar, historico_calidad y reportar rechazan el resumen con lupa_resumen_no_admitido", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(i = c(rep(NA_integer_, 18L), 1L, 2L), t = rep(c("x", NA), 10L),
                  stringsAsFactors = FALSE)
  mod <- modelo(
    instanciar(especializar(nuc$NoNulo, "NN"), "d", "i", nombre_instancia = "NN_i"),
    instanciar(especializar(nuc$NoNulo, "NN"), "d", "t", nombre_instancia = "NN_t")
  )
  mr <- medir(mod, list(d = d), id_medicion = "c2", fecha = fecha,
              proteger_datos_personales = TRUE, detalle = "resumen")
  p <- perfil_evaluacion("p", regla_evaluacion("celdas", function(x) x == 1,
                                               proporcion_minima = 0.95))

  expect_error(evaluar(mr, p), class = "lupa_resumen_no_admitido")
  expect_error(agregar(mr[1L, , drop = FALSE], "atributo", "ratio"),
               class = "lupa_resumen_no_admitido")
  expect_error(historico_calidad(mr), class = "lupa_resumen_no_admitido")
  expect_error(historico_calidad(list(mr)), class = "lupa_resumen_no_admitido")
  expect_error(reportar(mr), class = "lupa_resumen_no_admitido")
  expect_error(tablero_calidad(mr, agregaciones = c(NN_i = "promedio")),
               class = "lupa_resumen_no_admitido")
})

test_that("tablero_calidad rechaza un resumen que dejo metricas no_resumible", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(f = as.Date(c("2026-01-01", "2026-01-05", NA)))
  contrato <- vigencia("f", fecha_acceso = as.Date("2026-02-15"),
                       fecha_ultimo_cambio = as.Date("2026-02-01"))
  mod <- modelo(
    instanciar(especializar(nuc$NoNulo, "NN"), "d", "f"),
    instanciar(especializar(nuc$DesactualizacionPorFecha, "DF", vigencia = contrato), "d")
  )
  mr <- medir(mod, list(d = d), id_medicion = "c3", fecha = fecha,
              proteger_datos_personales = TRUE, detalle = "resumen")
  expect_error(tablero_calidad(mr), class = "lupa_resumen_no_admitido")
  expect_error(indice_calidad(mr, pesos = c(Completitud = 1)),
               class = "lupa_resumen_no_admitido")
})

test_that("print, `[` y rbind conservan clase y atributos; rbind rechaza contextos incompatibles (F2)", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(x = c(1, NA, 1, NA), y = c("a", "b", "a", "b"),
                  stringsAsFactors = FALSE)
  m1 <- modelo(instanciar(especializar(nuc$NoNulo, "M1"), "d", "x"))
  m2 <- modelo(instanciar(especializar(nuc$NoNulo, "M2"), "d", "y"))
  mj <- modelo(c(m1$metricas, m2$metricas))

  r1 <- medir(m1, list(d = d), id_medicion = "misma", fecha = fecha,
              proteger_datos_personales = TRUE, detalle = "resumen")
  r2 <- medir(m2, list(d = d), id_medicion = "misma", fecha = fecha,
              proteger_datos_personales = TRUE, detalle = "resumen")
  rj <- medir(mj, list(d = d), id_medicion = "misma", fecha = fecha,
              proteger_datos_personales = TRUE, detalle = "resumen")
  mc <- medir(m1, list(d = d), id_medicion = "misma", fecha = fecha,
              proteger_datos_personales = TRUE)

  # print devuelve el objeto identico
  out <- capture.output(ret <- print(rj))
  expect_identical(ret, rj)

  # `[` conserva clase y atributos
  sub <- rj[1L, , drop = FALSE]
  expect_s3_class(sub, "medicion_resumen")
  expect_identical(attr(sub, "configuracion_modelo"), attr(rj, "configuracion_modelo"))

  # rbind de metricas distintas en misma corrida y misma aplicabilidad
  rb <- rbind(r1, r2)
  expect_s3_class(rb, "medicion_resumen")
  expect_identical(as.data.frame(unclass(rb), stringsAsFactors = FALSE),
                   as.data.frame(unclass(rj), stringsAsFactors = FALSE))
  expect_identical(attr(rb, "configuracion_modelo"), attr(rj, "configuracion_modelo"))

  # rbind del mismo resumen o resumen + completa: rechazados
  expect_error(rbind(r1, r1), class = "lupa_resumen_no_admitido")
  expect_error(rbind(r1, mc), class = "lupa_resumen_no_admitido")
  expect_error(rbind(mc, r1), class = "lupa_resumen_no_admitido")

  # F2: rbind con aplicabilidades distintas debe rechazarse
  r2_app <- medir(m2, list(d = d), id_medicion = "misma", fecha = fecha,
                  aplicabilidad = list(y = ~ y == "a"),
                  proteger_datos_personales = TRUE, detalle = "resumen")
  expect_error(rbind(r1, r2_app), class = "lupa_resumen_no_admitido")

  # F2: rbind de corridas distintas con modelos distintos debe rechazarse
  r2_otra <- medir(m2, list(d = d), id_medicion = "otra", fecha = fecha,
                   proteger_datos_personales = TRUE, detalle = "resumen")
  expect_error(rbind(r1, r2_otra), class = "lupa_resumen_no_admitido")
})

test_that("tablero_calidad e indice_calidad ante universo vacio responden como la completa (F3)", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(x = c(1, NA, 1))
  mod <- modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "x"))
  vacio <- list(x = ~ x == 99)

  mc_v <- suppressWarnings(medir(mod, list(d = d), id_medicion = "v", fecha = fecha,
                                 aplicabilidad = vacio, proteger_datos_personales = TRUE))
  mr_v <- suppressWarnings(medir(mod, list(d = d), id_medicion = "v", fecha = fecha,
                                 aplicabilidad = vacio, proteger_datos_personales = TRUE,
                                 detalle = "resumen"))
  expect_identical(nrow(mr_v), 0L)
  expect_identical(attr(mr_v, "cobertura_metricas"), attr(mc_v, "cobertura_metricas"))

  err_tc <- tryCatch(tablero_calidad(mc_v), error = conditionMessage)
  err_tr <- tryCatch(tablero_calidad(mr_v), error = conditionMessage)
  expect_identical(err_tr, err_tc)
  expect_false(grepl("differing number of rows", err_tr, fixed = TRUE))

  err_ic <- tryCatch(indice_calidad(mc_v, pesos = c(Completitud = 1)),
                     error = conditionMessage)
  err_ir <- tryCatch(indice_calidad(mr_v, pesos = c(Completitud = 1)),
                     error = conditionMessage)
  expect_identical(err_ir, err_ic)
  expect_false(grepl("differing number of rows", err_ir, fixed = TRUE))
})

test_that("recorrido de superficie: ninguna funcion exportada falla abierto sobre medicion_resumen", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(x = c(1, NA, 1, 2), y = c("a", "b", "a", "b"),
                  stringsAsFactors = FALSE)
  mod <- modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "x"))
  mc <- medir(mod, list(d = d), id_medicion = "sup", fecha = fecha,
              proteger_datos_personales = TRUE)
  mr <- medir(mod, list(d = d), id_medicion = "sup", fecha = fecha,
              proteger_datos_personales = TRUE, detalle = "resumen")
  perfil <- suppressWarnings(perfilar(d, proteger_datos_personales = TRUE))
  ev_p <- perfil_evaluacion("p", regla_evaluacion("ok", function(x) x == 1))

  comprobar <- function(res_c, res_r, modo) {
    if (identical(modo, "identico")) {
      expect_identical(res_r, res_c)
    } else {
      expect_error(force(res_r), class = "lupa_resumen_no_admitido")
    }
  }
  comprobar(tablero_calidad(mc), tablero_calidad(mr), "identico")
  comprobar(indice_calidad(mc, pesos = c(Completitud = 1)),
            indice_calidad(mr, pesos = c(Completitud = 1)), "identico")
  comprobar(cobertura_analisis(perfil, mc), cobertura_analisis(perfil, mr), "identico")
  expect_error(evaluar(mr, ev_p), class = "lupa_resumen_no_admitido")
  expect_error(agregar(mr, "atributo", "ratio"), class = "lupa_resumen_no_admitido")
  expect_error(historico_calidad(mr), class = "lupa_resumen_no_admitido")
  expect_error(reportar(mr), class = "lupa_resumen_no_admitido")
})
