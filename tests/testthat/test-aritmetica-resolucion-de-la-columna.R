# La proporcionalidad mide el redondeo de la respuesta con la resolucion que la
# columna muestra -multiplos de 0,05, 0,25, 0,5, 5 o 25- y no con la potencia
# de diez de sus decimales. Todo construido en enteros: la exactitud no depende
# de los dobles.

# Pesada para CRAN (mas de 0,2 s en la medicion sin red del 2026-11-10): corre en
# la suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

relaciones_res <- function(d) {
  p <- perfilar(d, analizar_dependencias = FALSE,
                fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  h <- p$hallazgos
  list(h = h[h$tipo_hallazgo == "relacion_aritmetica_columnas", , drop = FALSE],
       meta = p$meta$aritmetica_columnas)
}

# base = u / 20 y respuesta = redondeo de base * 11 / 50 a multiplos de 1 / 20.
# Con `desde = 2e8` la respuesta llega a 2,2e6 y la tolerancia relativa (0,022)
# queda apenas debajo del medio cuanto (0,025) en todas las filas.
caso_005 <- function(desde = 2e8) {
  u <- desde + 2 * ((seq_len(100L) * 7919) %% 100000000)
  respuesta_unidades <- floor(11 * u / 50 + 0.5)
  list(base = u / 20, respuesta = respuesta_unidades / 20,
       residuo_veinteavos = respuesta_unidades - 11 * u / 50)
}

test_that("la resolucion de una columna es la del redondeo que muestra", {
  r <- lupa:::.resolucion_columna_aritmetica
  set.seed(1)
  valores <- stats::runif(300, 1, 1e6)
  expect_equal(r(round(valores * 20) / 20), 0.05)
  expect_equal(r(round(valores * 4) / 4), 0.25)
  expect_equal(r(round(valores * 2) / 2), 0.5)
  expect_equal(r(5 * round(valores)), 5)
  expect_equal(r(25 * round(valores)), 25)
  expect_equal(r(round(valores, 2)), 0.01)
  # Unas filas fuera de la reticula no la hacen caer; muchas, si.
  roto <- as.numeric(sprintf("%.2f", round(valores * 20) / 20))
  roto[1:5] <- roto[1:5] + 0.01
  expect_equal(r(roto), 0.05)
  roto[1:40] <- roto[1:40] + 0.01
  expect_equal(r(roto), 0.01)
  # Tres centavos pares no hacen una columna de 0,02: se explica por azar.
  expect_equal(r(c(0.02, 0.04, 0.06)), 0.01)
  expect_true(is.na(r(stats::runif(100))))
})

test_that("una proporcionalidad sana redondeada a 0,05 se publica sana", {
  caso <- caso_005()
  # A mano: el residuo exacto es a lo sumo 12/25 de veinteavo, 0,024.
  expect_lte(max(abs(caso$residuo_veinteavos)), 1 / 2)
  r <- relaciones_res(data.frame(base = caso$base, respuesta = caso$respuesta))
  expect_equal(nrow(r$h), 1L)
  expect_equal(as.character(r$h$severidad), "ok")
  expect_equal(r$h$n_afectados, 0)
  expect_match(r$h$descripcion, "respuesta ~= base * 0.22", fixed = TRUE)
  expect_match(r$h$evidencia, "<= 0.025: la mitad del cuanto decimal de `respuesta` (0.05)",
               fixed = TRUE)
  expect_equal(r$meta$proporcionalidades_descartadas_cuanto, 0)
})

test_that("con resolucion 0,05 se cuentan las rotas por un cuanto y las que salen de la reticula", {
  # Sobre respuestas de 4,4e6, donde la relativa supera el medio cuanto. (Sobre
  # 2,2e6, cinco rotas dejan a la relativa debajo del criterio y la relacion no
  # se reconoce: la decision de siempre para un producto redondeado.)
  caso <- caso_005(desde = 4e8)
  respuesta <- caso$respuesta
  # Rotas por un cuanto donde el residuo no cae en la mitad.
  lejos <- which(abs(caso$residuo_veinteavos) < 0.4)
  cuanto <- sort(lejos[c(3L, 17L, 41L)])
  respuesta[cuanto] <- respuesta[cuanto] + c(0.05, -0.05, 0.05)
  # Y dos fuera de la reticula por un centavo.
  fuera <- sort(setdiff(lejos, cuanto)[c(5L, 30L)])
  respuesta[fuera] <- as.numeric(sprintf("%.2f", respuesta[fuera] + 0.01))
  r <- relaciones_res(data.frame(base = caso$base, respuesta = respuesta))
  expect_equal(nrow(r$h), 1L)
  expect_equal(sort(r$h$trazabilidad[[1L]]$indices_fila), sort(c(cuanto, fuera)))
})

test_that("con resolucion 0,25 sobre importes grandes se publica sana y cuenta las rotas", {
  # base = u / 4 y respuesta = redondeo de base * 3 / 10 a cuartos, sobre 1e8.
  u <- 4e8 + 2 * ((seq_len(200L) * 7919) %% 4e8)
  unidades <- floor(3 * u / 10 + 0.5)
  base <- u / 4
  respuesta <- unidades / 4
  r <- relaciones_res(data.frame(base = base, respuesta = respuesta))
  expect_equal(nrow(r$h), 1L)
  expect_equal(r$h$n_afectados, 0)
  residuo <- unidades - 3 * u / 10
  lejos <- which(abs(residuo) < 0.4)
  rotas <- sort(lejos[c(2L, 9L, 23L, 60L)])
  respuesta[rotas] <- respuesta[rotas] + c(0.25, -0.25, 0.25, -0.25)
  r <- relaciones_res(data.frame(base = base, respuesta = respuesta))
  expect_equal(sort(r$h$trazabilidad[[1L]]$indices_fila), rotas)
})

test_that("las identidades siguen con la potencia de diez de sus decimales", {
  set.seed(5)
  a <- 0.25 * round(stats::runif(300, 1e8, 1e9) * 4)
  b <- 0.25 * round(stats::runif(300, 1e8, 1e9) * 4)
  total <- a + b
  i <- sort(sample(300L, 5L))
  # Fuera de la reticula de 0,25 por un centavo: con medio cuanto de 0,25 no
  # se veria; con el de los decimales, si.
  total[i] <- as.numeric(sprintf("%.2f", total[i] + 0.01))
  r <- relaciones_res(data.frame(a = a, b = b, total = total))
  expect_equal(nrow(r$h), 1L)
  expect_equal(sort(r$h$trazabilidad[[1L]]$indices_fila), i)
  expect_match(r$h$evidencia, "min(1e-08", fixed = TRUE)
})
