# comparar_perfiles(): el paso de una columna entera no "cambia" porque un lado
# tenga dos valores distintos y el otro tres; y una cola (un centinela, un tope)
# que cae en una muestra chica y no en la otra no es un cambio de dispersion.

testthat::skip_on_cran()

.cpc_perfil <- function(datos, fecha) {
  suppressWarnings(suppressMessages(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )))
}

.cpc_deriva <- function(anterior, actual) {
  d <- as.data.frame(suppressWarnings(suppressMessages(comparar_perfiles(
    .cpc_perfil(anterior, as.POSIXct("2026-01-01", tz = "UTC")),
    .cpc_perfil(actual, as.POSIXct("2026-02-01", tz = "UTC"))
  ))))
  d[d$aspecto %in% c("deriva_media", "deriva_dispersion", "deriva_categorias"),
    , drop = FALSE]
}

.cpc_normal <- function(n, media, desvio, semilla) {
  set.seed(semilla)
  media + desvio * sample(stats::qnorm((seq_len(n) - 0.5) / n))
}

test_that("conteos 0/1 contra 0/1/2 conservan la deriva de la media y su severidad", {
  d <- .cpc_deriva(
    data.frame(x = rep(0:1, c(21L, 9L))),
    data.frame(x = rep(0:2, c(100L, 250L, 150L)))
  )
  fila <- d[d$aspecto == "deriva_media", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$severidad), "error")
  # El paso es 1 en las dos entregas: la fila no habla de pasos ni de redondeo.
  expect_false(grepl("paso medio", fila$evidencia, fixed = TRUE, useBytes = TRUE))
  # Uno 0/1 y otro 1/2/3 concentrado en 2: la severidad no baja por un paso que no cambio.
  d <- .cpc_deriva(
    data.frame(x = rep(0:1, c(18L, 12L))),
    data.frame(x = rep(1:3, c(40L, 720L, 40L)))
  )
  expect_identical(as.character(d$severidad[d$aspecto == "deriva_media"]), "error")
})

test_that("porcentajes 49/50 contra 49/50/51 conservan la deriva de la media", {
  d <- .cpc_deriva(
    data.frame(x = rep(49:50, c(15L, 15L))),
    data.frame(x = rep(49:51, c(200L, 200L, 200L)))
  )
  expect_equal(sum(d$aspecto == "deriva_media"), 1L)
})

test_that("un centinela que cae en una muestra chica y no en la otra no acusa dispersion", {
  con <- c(round(.cpc_normal(28L, 100, 5, 1L)), rep(999, 6L))
  sin <- round(.cpc_normal(43L, 100, 5, 2L))
  expect_false("deriva_dispersion" %in% .cpc_deriva(data.frame(x = con), data.frame(x = sin))$aspecto)
  expect_false("deriva_dispersion" %in% .cpc_deriva(data.frame(x = sin), data.frame(x = con))$aspecto)
})

test_that("una dispersion real sin redondeo no se explica con el redondeo", {
  d <- .cpc_deriva(
    data.frame(x = .cpc_normal(20L, 0, 1, 1L)),
    data.frame(x = .cpc_normal(20L, 0, 5, 2L))
  )
  fila <- d[d$aspecto == "deriva_dispersion", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_false(grepl("redondeo", paste(fila$descripcion, fila$evidencia),
                     fixed = TRUE, useBytes = TRUE))
})

test_that("cuando se descuenta el redondeo, la fila dice los pasos de las dos entregas", {
  d <- .cpc_deriva(
    data.frame(x = .cpc_normal(1500L, 100, 0.3, 3L)),
    data.frame(x = round(.cpc_normal(1500L, 102, 0.3, 4L), 0L))
  )
  fila <- d[d$aspecto == "deriva_media", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_true(grepl("actual, 1", fila$evidencia,
                    fixed = TRUE, useBytes = TRUE))
})

test_that("enteros dispersos en un rango ancho tienen la misma grilla que los juntos", {
  # 0, 1, 2 contra 0, 1, 2 y 20: el paso medio entre valores distintos es 1 y 6,7,
  # pero la grilla de los dos es 1 y no hay redondeo que descontar.
  d <- .cpc_deriva(
    data.frame(x = rep(0:2, c(8L, 15L, 7L))),
    data.frame(x = rep(c(0L, 1L, 2L, 20L), c(60L, 120L, 360L, 60L)))
  )
  fila <- d[d$aspecto == "deriva_media", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$severidad), "sospechoso")
  expect_false(grepl("Grilla", fila$evidencia, fixed = TRUE, useBytes = TRUE))
})
