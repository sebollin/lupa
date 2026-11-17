test_that("muestrear_vector directo usa la huella del vector", {
  valores <- rep(c("A-001", "B-002", "C-003", "raro-999"), length.out = 1200L)
  primero <- lupa:::.muestrear_vector(valores, 100L)
  segundo <- lupa:::.muestrear_vector(rev(valores), 100L)
  expect_true(primero$muestreado)
  expect_identical(primero$valores, segundo$valores)
})

test_that("descubrir_patrones directo es invariante al permutar el vector", {
  valores <- rep(c("AA-001", "BB-002", "raro-999"), length.out = 1200L)
  primero <- descubrir_patrones(valores, muestra = 100L, expandir = TRUE)
  segundo <- descubrir_patrones(rev(valores), muestra = 100L, expandir = TRUE)
  expect_identical(primero, segundo)
})

test_that("detectar_formatos_fecha directo es invariante al permutar el vector", {
  valores <- rep(c("2020-01-01", "01/02/2020", "texto"), length.out = 1200L)
  primero <- detectar_formatos_fecha(valores, muestra = 100L)
  segundo <- detectar_formatos_fecha(rev(valores), muestra = 100L)
  expect_identical(primero, segundo)
})

test_that("inferir_tipo directo es invariante al permutar el vector", {
  valores <- rep(c("1", "2", "3", "texto"), length.out = 1200L)
  primero <- inferir_tipo(valores, muestra = 100L)
  segundo <- inferir_tipo(rev(valores), muestra = 100L)
  expect_identical(primero, segundo)
})

test_that("clasificar_variables directo es invariante al permutar filas", {
  datos <- data.frame(
    nivel = rep(c("A", "B", "C"), length.out = 1200L),
    valor = rep(c(1, 2, 3, 4), length.out = 1200L),
    stringsAsFactors = FALSE
  )
  permutacion <- rev(seq_len(nrow(datos)))
  primero <- clasificar_variables(datos, muestra = 100L)
  segundo <- clasificar_variables(datos[permutacion, , drop = FALSE],
                                  muestra = 100L)
  expect_identical(primero, segundo)
})

test_that("distribucion_valores directo es invariante al permutar filas", {
  datos <- data.frame(
    nivel = rep(c("A", "B", "C"), length.out = 1200L),
    valor = rep(c(1, 2, 3, 4), length.out = 1200L),
    stringsAsFactors = FALSE
  )
  permutacion <- rev(seq_len(nrow(datos)))
  primero <- distribucion_valores(datos, muestra = 100L)
  segundo <- distribucion_valores(datos[permutacion, , drop = FALSE],
                                  muestra = 100L)
  expect_identical(primero, segundo)
})

test_that("detectar_asociaciones directo es invariante al permutar filas", {
  datos <- data.frame(
    a = rep(c(1, 2, 3, 4), length.out = 1200L),
    b = seq_len(1200L),
    grupo = rep(c("A", "B", "C"), length.out = 1200L),
    stringsAsFactors = FALSE
  )
  permutacion <- rev(seq_len(nrow(datos)))
  primero <- detectar_asociaciones(datos, muestra = 100L, umbral = 0)
  segundo <- detectar_asociaciones(
    datos[permutacion, , drop = FALSE], muestra = 100L, umbral = 0
  )
  expect_identical(primero, segundo)
})

test_that("analizar conserva sus diagnosticos muestreados al permutar filas", {
  skip_on_cran()
  set.seed(113L)
  n <- 800L
  datos <- data.frame(
    id = sample.int(n),
    texto = sample(c("ano", "B", NA_character_), n, replace = TRUE),
    valor = rnorm(n),
    fecha = as.Date("2020-01-01") + sample(0:10, n, replace = TRUE),
    categoria = sample(letters[1:4], n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  permutacion <- sample.int(n)
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  primero <- suppressWarnings(analizar(
    datos, nombre = "datos", fecha = fecha, muestra = 100L,
    muestra_asociacion = 40L, analizar_dependencias = FALSE
  ))
  segundo <- suppressWarnings(analizar(
    datos[permutacion, , drop = FALSE], nombre = "datos", fecha = fecha,
    muestra = 100L, muestra_asociacion = 40L, analizar_dependencias = FALSE
  ))
  publicados <- function(x) {
    hallazgos <- x$perfil$hallazgos
    hallazgos$trazabilidad <- NULL
    list(
      columnas = x$perfil$columnas,
      patrones = x$perfil$patrones,
      formatos_fecha = x$perfil$formatos_fecha,
      hallazgos = hallazgos,
      dependencias = x$perfil$dependencias,
      muestra_huella = x$perfil$meta$muestra_huella,
      distribuciones = x$distribuciones,
      variables = x$variables,
      asociaciones = x$asociaciones
    )
  }
  expect_identical(publicados(primero), publicados(segundo))
})
