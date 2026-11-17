test_that("la muestra es el prefijo mas largo dentro del tope", {
  testthat::skip_on_cran()
  n <- 300L
  datos <- data.frame(
    valor = factor(sprintf("nivel-%04d", seq_len(n)),
                   levels = sprintf("nivel-%04d", seq_len(n)))
  )
  huella <- lupa:::.indices_huella_tabla(datos, n - 1L)$indices
  ks <- seq_len(n - 1L)
  bytes <- vapply(ks, function(k) as.numeric(object.size(
    lupa:::.muestra_tabla_datos(datos, k, indices_muestra = huella)
  )), numeric(1L))
  tope <- bytes[[120L]]
  valido <- which(bytes <= tope)
  esperado <- max(valido)
  expect_lte(bytes[[esperado]], tope)
  if (esperado < n - 1L) expect_gt(bytes[[esperado + 1L]], tope)

  alcance <- lupa:::.resolver_muestra_perfilado(
    datos, muestra = 200L, max_celdas_muestra = Inf,
    max_bytes_muestra = tope
  )
  expect_identical(as.integer(alcance$filas_efectivas), esperado)
  expect_lte(alcance$bytes_muestra, tope)
  expect_true(is.finite(alcance$bytes_sonda))
})

test_that("un factor de muchos niveles llega al tope de celdas", {
  testthat::skip_on_cran()
  set.seed(102)
  n <- 60000L
  datos <- as.data.frame(replicate(19L, rnorm(n), simplify = FALSE))
  names(datos) <- paste0("x", seq_len(19L))
  datos$f <- factor(sprintf("nivel_%06d", seq_len(50000L)))[
    rep(seq_len(50000L), length.out = n)
  ]
  alcance <- lupa:::.resolver_muestra_perfilado(
    datos, muestra = 1e5, max_celdas_muestra = 1e6,
    max_bytes_muestra = 512 * 1024^2
  )
  expect_identical(as.integer(alcance$filas_efectivas), 50000L)
  expect_lte(alcance$celdas_efectivas, 1e6)
  expect_lte(alcance$bytes_muestra, 512 * 1024^2)
  expect_true(is.na(alcance$bytes_sonda))
})

test_that("con mas filas que la muestra el candidato cabe sin estimacion", {
  testthat::skip_on_cran()
  n <- 1200L
  datos <- data.frame(
    valor = factor(sprintf("nivel-%04d", seq_len(n)),
                   levels = sprintf("nivel-%04d", seq_len(n)))
  )
  alcance <- lupa:::.resolver_muestra_perfilado(
    datos, muestra = 400L, max_celdas_muestra = Inf,
    max_bytes_muestra = 512 * 1024^2
  )
  expect_identical(as.integer(alcance$filas_efectivas), 400L)
  expect_true(is.na(alcance$bytes_sonda))
  expect_lte(alcance$bytes_muestra, 512 * 1024^2)
})

test_that("la busqueda no mide el total como prefijo", {
  testthat::skip_on_cran()
  n <- 120L
  datos <- data.frame(valor = factor(sprintf("nivel-%04d", seq_len(n))))
  indices <- lupa:::.indices_huella_tabla(datos, n - 1L)$indices
  tope <- as.numeric(object.size(lupa:::.muestra_tabla_datos(
    datos, 40L, indices_muestra = indices
  )))
  medidas <- integer()
  original <- lupa:::.muestra_tabla_datos
  testthat::local_mocked_bindings(
    .muestra_tabla_datos = function(datos, filas, indices_muestra = NULL) {
      medidas <<- c(medidas, as.integer(filas))
      original(datos, filas, indices_muestra = indices_muestra)
    },
    .package = "lupa"
  )
  alcance <- lupa:::.resolver_muestra_perfilado(
    datos, muestra = n, max_celdas_muestra = Inf,
    max_bytes_muestra = tope
  )
  stopifnot(sum(medidas == n) <= 1L)
  stopifnot(all(medidas[medidas != n] < n))
  expect_lt(alcance$filas_efectivas, n)
})
