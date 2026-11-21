# comparar_perfiles(): la media corrida por un redondeo distinto en cada lado no
# es deriva. La media observada queda a lo sumo a medio paso de la de los
# valores sin redondear, asi que un corrimiento menor que lo que explican los
# dos pasos no se publica; uno mayor, o con el mismo paso, si.

testthat::skip_on_cran()

.crm_perfil <- function(datos, fecha) {
  suppressWarnings(suppressMessages(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )))
}

.crm_media <- function(anterior, actual) {
  d <- as.data.frame(suppressWarnings(suppressMessages(comparar_perfiles(
    .crm_perfil(anterior, as.POSIXct("2026-01-01", tz = "UTC")),
    .crm_perfil(actual, as.POSIXct("2026-02-01", tz = "UTC"))
  ))))
  d[d$aspecto == "deriva_media", , drop = FALSE]
}

.crm_normal <- function(n, media, desvio, semilla) {
  set.seed(semilla)
  media + desvio * sample(stats::qnorm((seq_len(n) - 0.5) / n))
}

test_that("los mismos datos a 0 y a 1 decimal no corren la media", {
  # Con desvio 0,15 los enteros son casi todos 78: su media (78,005) queda a
  # 0,115 de la de los datos, menos de medio paso.
  for (media in c(78.12, 78.2)) {
    d <- .crm_media(
      data.frame(x = round(.crm_normal(1500L, media, 0.15, 1L), 0L)),
      data.frame(x = round(.crm_normal(1500L, media, 0.15, 2L), 1L))
    )
    expect_equal(nrow(d), 0L)
  }
})

test_that("un corrimiento mayor que el redondeo, o con el mismo paso, se publica", {
  d <- .crm_media(
    data.frame(x = round(.crm_normal(1500L, 78.2, 0.15, 1L), 0L)),
    data.frame(x = round(.crm_normal(1500L, 79.6, 0.15, 2L), 1L))
  )
  expect_equal(nrow(d), 1L)
  expect_identical(as.character(d$severidad), "error")
  d <- .crm_media(
    data.frame(x = round(.crm_normal(1500L, 78.2, 0.15, 1L), 1L)),
    data.frame(x = round(.crm_normal(1500L, 78.4, 0.15, 2L), 1L))
  )
  expect_equal(nrow(d), 1L)
})

test_that("una columna continua con pocos valores no descuenta un paso que no es una grilla", {
  # n = 30 contra 2.000 de valores todos distintos: el rango sobre n cambia
  # cuatro veces, pero no hay redondeo que descontar.
  d <- .crm_media(
    data.frame(x = .crm_normal(30L, 100, 15, 3L)),
    data.frame(x = .crm_normal(2000L, 115, 15, 4L))
  )
  expect_equal(nrow(d), 1L)
})
