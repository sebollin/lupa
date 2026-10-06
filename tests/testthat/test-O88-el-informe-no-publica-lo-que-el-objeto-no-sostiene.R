# La evolucion del informe no publica el delta de un par que la deriva declara no
# comparable.
#
# La seccion "Evolucion de perfiles de madurez" calculaba el delta con un `diff()`
# propio: la misma regla que `detectar_deriva_calidad()` escrita dos veces, y la
# copia no sabia lo que decide la original. La deriva declara NO COMPARABLE un par
# con cambio de marco -"no se publica la comparacion del resultado"- y el informe
# publicaba el delta de ese par dos secciones mas abajo, sin marca. Ahora el delta
# sale de la deriva.

.o88_historico <- function(marcos) {
  nucleo <- metricas_nucleo()
  instancia <- instanciar(especializar(nucleo$NoNulo), "t", "a")
  factores <- marco_agesic()$factores
  perfil <- perfil_evaluacion(
    "Basico", regla_evaluacion("R1", function(x) x > 0.5)
  )
  datos <- list(c(1, 2, 3), c(1, NA, NA), c(1, 2, 3, 4))
  evaluaciones <- lapply(seq_along(marcos), function(i) {
    medicion <- medir(
      modelo(instancia, marco = marco_calidad(marcos[[i]], factores)),
      data.frame(a = datos[[i]]), id_medicion = paste0("r", i),
      fecha = as.POSIXct("2026-01-01", tz = "UTC") + i * 86400
    )
    evaluar(medicion, perfil)
  })
  do.call(historico_calidad, evaluaciones)
}

test_that("el par con cambio de marco no publica delta y dice por que", {
  evolucion <- lupa:::.evolucion_historico(
    .o88_historico(c("Marco A", "Marco B", "Marco B"))
  )
  r2 <- evolucion[evolucion$id_medicion == "r2", ]
  # Antes: -0.667, al lado de "no se publica la comparacion del resultado".
  expect_true(is.na(r2$delta))
  expect_identical(r2$comparado_con, "r1")
  expect_match(r2$comparacion, "marco", fixed = TRUE)
  # El par siguiente, sin cambio de marco, si se compara.
  r3 <- evolucion[evolucion$id_medicion == "r3", ]
  expect_equal(r3$delta, 1 - 1 / 3)
  expect_identical(r3$comparado_con, "r2")
})

test_that("el control: sin cambio de marco los deltas son las restas", {
  historico <- .o88_historico(c("Marco A", "Marco A", "Marco A"))
  evolucion <- lupa:::.evolucion_historico(historico)
  evolucion <- evolucion[order(evolucion$fecha), ]
  expect_true(is.na(evolucion$delta[[1L]]))
  expect_equal(evolucion$delta[-1L], diff(evolucion$resultado))
  # Y lo mismo que publica la deriva, fila por fila.
  deriva <- as.data.frame(detectar_deriva_calidad(historico))
  deriva <- deriva[deriva$aspecto == "resultado", ]
  expect_equal(
    evolucion$delta[match(deriva$id_medicion_actual, evolucion$id_medicion)],
    deriva$delta
  )
})

test_that("el informe entero dice lo mismo que su seccion de deriva", {
  historico <- .o88_historico(c("Marco A", "Marco B", "Marco B"))
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(historico, detectar_deriva_calidad(historico), archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "")
  # Con todas sus cifras: el informe ya no redondea a ocho. Y desde la ronda 26,
  # con las que vuelven al valor: 1 - 1/3 necesita dieciseis -con quince,
  # 0.666666666666667, no es el doble del objeto-.
  expect_false(grepl("-0.6666666666666667", html, fixed = TRUE))
  expect_true(grepl("0.6666666666666667", html, fixed = TRUE))
  expect_identical(as.numeric("0.6666666666666667"), 1 - 1 / 3)
})

test_that("una medicion sin `id_medicion` no publica cero corridas", {
  # La misma familia: una cifra del informe que el objeto no sostiene. Una
  # medicion recortada conserva su clase y publicaba "2 medidas en 0 corrida(s)".
  instancia <- instanciar(especializar(metricas_nucleo()$NoNulo), "t", "a")
  medicion <- medir(modelo(instancia), data.frame(a = c(1, NA)), id_medicion = "m")
  rota <- as.data.frame(medicion)
  rota$id_medicion <- NULL
  class(rota) <- class(medicion)
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(rota, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "")
  expect_false(grepl("0 corrida(s)", html, fixed = TRUE))
  expect_true(grepl("le falta la columna <code>id_medicion</code>", html,
                    fixed = TRUE))
  # El control: la medicion entera sigue contando su corrida.
  reportar(medicion, archivo = archivo, sobrescribir = TRUE)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "")
  expect_true(grepl("2 medidas en 1 corrida(s)", html, fixed = TRUE))
})
