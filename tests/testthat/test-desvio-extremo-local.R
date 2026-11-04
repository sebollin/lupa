.adv_sd_referencia_local <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  escala <- 2^(floor(log2(max(abs(x)))) - 1)
  y <- x / escala
  m <- mean(y)
  d <- y - m
  sqrt(max(0, (sum(d^2) - sum(d)^2 / length(y)) / (length(y) - 1L))) * escala
}

.adv_media_referencia_local <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  escala <- 2^(floor(log2(max(abs(x)))) - 1)
  sum(x / escala) / length(x) * escala
}

.adv_tolerancia_local <- function(x) {
  if (length(x) == 1L && is.finite(x) && x != 0) 1e-12 else 1e-320
}

test_that("perfilar local escala momentos extremos", {
  set.seed(20261106)
  casos <- list(
    tiny30 = 1e-160 * (1 + runif(30L)),
    tiny_mix = c(1e-160 * runif(20L), -1e-161 * runif(20L)),
    sub_e200 = 1e-200 * (1 + runif(50L)),
    borde151 = 1e151 + runif(40L) * 1e140,
    alea200 = runif(200L, 1e307, 1.7e308),
    signado = c(.Machine$double.xmax, -.Machine$double.xmax, 0, 1),
    casi_xm = c(.Machine$double.xmax,
                .Machine$double.xmax * (1 - 2^-52),
                .Machine$double.xmax)
  )
  for (nombre in names(casos)) {
    valores <- casos[[nombre]]
    resultado <- perfilar(
      data.frame(x = valores), proteger_datos_personales = FALSE
    )
    fila <- resultado$columnas[resultado$columnas$columna == "x", , drop = FALSE]
    esperado <- .adv_sd_referencia_local(valores)
    esperado_media <- .adv_media_referencia_local(valores)
    expect_equal(as.numeric(fila$media), esperado_media,
                 tolerance = .adv_tolerancia_local(esperado_media),
                 info = nombre)
    expect_equal(as.numeric(fila$desvio), esperado,
                 tolerance = .adv_tolerancia_local(esperado),
                 info = nombre)
    expect_true(is.finite(fila$desvio), info = nombre)
  }
})

test_that("perfilar local conserva cero exacto para una constante extrema", {
  resultado <- perfilar(
    data.frame(x = rep(.Machine$double.xmax, 37L)),
    proteger_datos_personales = FALSE
  )
  fila <- resultado$columnas[resultado$columnas$columna == "x", , drop = FALSE]
  expect_identical(as.numeric(fila$desvio), 0)
})
