# Los bordes de la cota del cuanto: hasta donde se reconoce un decimal guardado,
# que se publica cuando mas de la mitad de las filas estan rotas, la suma con
# cancelacion y el conteo de lo que la cota descarta.

# Pesada para CRAN (mas de 0,2 s en la medicion sin red del 2026-11-10): corre en
# la suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

suma_rota <- function(escala, cuanto, rotas, semilla = 42L) {
  set.seed(semilla)
  n <- 300L
  a <- round(stats::runif(n, escala / 10, escala) / cuanto) * cuanto
  b <- round(stats::runif(n, escala / 10, escala) / cuanto) * cuanto
  total <- a + b
  i <- sample(n, rotas)
  total[i] <- total[i] + cuanto
  list(datos = data.frame(a = a, b = b, total = total), rotas = sort(i))
}

relacion <- function(perfil) {
  h <- perfil$hallazgos
  h[h$tipo_hallazgo == "relacion_aritmetica_columnas", , drop = FALSE]
}

test_that("los centavos rotos se cuentan hasta importes de 1e12, y los enteros hasta 1e14", {
  casos <- list(c(7e9, 0.01), c(1e11, 0.01), c(4e11, 0.01), c(1e12, 1), c(4e13, 1))
  for (caso in casos) {
    s <- suma_rota(caso[[1L]], caso[[2L]], 30L)
    rel <- relacion(perfilar(s$datos, analizar_dependencias = FALSE))
    info <- paste(caso, collapse = " ")
    expect_equal(nrow(rel), 1L, info = info)
    expect_equal(rel$n_afectados, 30, info = info)
    expect_equal(sort(rel$trazabilidad[[1L]]$indices_fila), s$rotas, info = info)
    expect_match(rel$evidencia, "min(1e-08", fixed = TRUE, info = info)
  }
})

test_that("por encima del limite la evidencia dice que la tolerancia no se acota y cuanto vale", {
  s <- suma_rota(1e12, 0.01, 30L)
  rel <- relacion(perfilar(s$datos, analizar_dependencias = FALSE))
  expect_equal(nrow(rel), 1L)
  expect_match(rel$evidencia, "No se acota por un cuanto decimal", fixed = TRUE)
  expect_match(rel$evidencia, "15 cifras significativas", fixed = TRUE)
  # A mano: la tolerancia mas grande es 1e-8 por el valor mas grande.
  mayor <- max(abs(unlist(s$datos)))
  expect_match(rel$evidencia, paste0(
    "la tolerancia llega a ",
    format(signif(1e-8 * mayor, 3), scientific = FALSE, trim = TRUE)
  ), fixed = TRUE)
  # Debajo del limite, sin cota no hay texto de limite.
  a <- stats::runif(50)
  b <- stats::runif(50)
  chico <- relacion(perfilar(data.frame(a = a, b = b, total = a + b),
                             analizar_dependencias = FALSE))
  expect_false(grepl("No se acota", chico$evidencia, fixed = TRUE))
})

test_that("con mas de la mitad de las filas rotas por un centavo no se publica una regularidad sana", {
  for (escala in c(1e7, 1e9, 1e11)) {
    s <- suma_rota(escala, 0.01, 153L)
    perfil <- perfilar(s$datos, analizar_dependencias = FALSE)
    expect_equal(nrow(relacion(perfil)), 0L, info = escala)
    expect_equal(perfil$meta$aritmetica_columnas$identidades_descartadas_cuanto,
                 1, info = escala)
  }
})

test_that("lo que la cota descarta deja su conteo en el alcance", {
  s <- suma_rota(1e8, 0.01, 120L)
  perfil <- perfilar(s$datos, analizar_dependencias = FALSE)
  expect_equal(nrow(relacion(perfil)), 0L)
  alcance <- perfil$meta$aritmetica_columnas
  expect_false(is.null(alcance))
  expect_equal(alcance$identidades_descartadas_cuanto, 1)
  expect_equal(alcance$identidades_descartadas_escala, 0)
})

test_that("una suma con cancelacion verdadera no acusa filas", {
  set.seed(7)
  x <- stats::runif(300, -100, 100)
  y <- stats::runif(300, -100, 100)
  for (grande in c(3e8, 1e9, 3e9)) {
    a <- grande + x
    b <- -grande + y
    # A mano: la diferencia es el redondeo de los operandos.
    expect_lt(max(abs(a + b - (x + y))), 8 * .Machine$double.eps * grande)
    rel <- relacion(perfilar(data.frame(a = a, b = b, c = x + y),
                             analizar_dependencias = FALSE))
    expect_equal(nrow(rel), 1L, info = grande)
    expect_equal(as.character(rel$severidad), "ok", info = grande)
    expect_equal(rel$n_afectados, 0, info = grande)
    expect_match(rel$evidencia, "|sumandos|", fixed = TRUE, info = grande)
  }
})

test_that("las relaciones aproximadas por redondeo de cifras siguen publicandose", {
  set.seed(31)
  n <- 400L
  for (escala in c(1e8, 1e11, 4e11)) {
    u <- stats::runif(n, 0.5, 1) * escala
    v <- stats::runif(n, 0.01, 0.1) * escala
    for (cifras in c(9L, 11L, 15L)) {
      s <- function(w) signif(w, cifras)
      rel <- relacion(perfilar(data.frame(u = s(u), v = s(v), total = s(u + v)),
                               analizar_dependencias = FALSE))
      info <- paste(escala, cifras)
      expect_equal(nrow(rel), 1L, info = info)
      expect_equal(rel$n_afectados, 0, info = info)
    }
  }
})

test_that("el cuanto incluye los gruesos y no reconoce 15 cifras", {
  cuanto <- lupa:::.cuanto_decimal_aritmetica
  expect_equal(cuanto(c(100, 2500, 37000)), 100)
  expect_equal(cuanto(signif(stats::runif(50, 5e10, 1e11), 9)), 100)
  expect_equal(cuanto(c(1.5, 20, 300)), 0.1)
  expect_true(is.na(cuanto(signif(stats::runif(50, 1e5, 9e5), 15))))
  expect_equal(cuanto(round(stats::runif(50, 1e11, 9e11), 2)), 0.01)
  expect_true(is.na(cuanto(round(stats::runif(50, 1e12, 2e12), 2))))
})
