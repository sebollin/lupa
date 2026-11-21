# comparar_perfiles(): la deriva de la dispersion con otra resolucion y con pocos
# valores, y la de las categorias cuando una entrega tiene pocas filas. Cada
# caso positivo va con su control: el paquete prefiere callar a afirmar un
# cambio que es ruido o redondeo.

testthat::skip_on_cran()

.cmc_enero <- as.POSIXct("2026-01-01", tz = "UTC")
.cmc_febrero <- as.POSIXct("2026-02-01", tz = "UTC")

.cmc_perfil <- function(datos, fecha = .cmc_enero) {
  suppressWarnings(suppressMessages(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )))
}

.cmc_deriva <- function(anterior, actual) {
  d <- as.data.frame(suppressWarnings(suppressMessages(comparar_perfiles(
    .cmc_perfil(anterior), .cmc_perfil(actual, .cmc_febrero)
  ))))
  d[d$aspecto %in% c("deriva_media", "deriva_dispersion", "deriva_categorias"),
    , drop = FALSE]
}

# Una normal determinista: los cuantiles de n puntos, mezclados con una semilla.
.cmc_normal <- function(n, media, desvio, semilla) {
  set.seed(semilla)
  media + desvio * sample(stats::qnorm((seq_len(n) - 0.5) / n))
}

test_that("una dispersion que cambia con la resolucion se publica diciendo que tambien cambio el paso", {
  d <- .cmc_deriva(
    data.frame(x = round(.cmc_normal(2000L, 0, 0.2, 1L), 2L)),
    data.frame(x = round(.cmc_normal(2000L, 0, 1, 2L), 0L))
  )
  fila <- d[d$aspecto == "deriva_dispersion", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$severidad), "error")
  expect_true(grepl("Grilla", fila$descripcion, fixed = TRUE, useBytes = TRUE))
  # Lo que se publica es la razon observada, y la evidencia dice cuanto
  # quedo tras descontar el redondeo, que no la deja bajo el umbral.
  expect_gt(fila$delta, 4)
  expect_true(grepl("descontado el redondeo", fila$evidencia, fixed = TRUE,
                    useBytes = TRUE))
})

test_that("el mismo desvio escrito con otra resolucion no acusa dispersion", {
  # Tres valores distintos de un lado: el paso se calcula, cambia 100 veces y el
  # desvio de los enteros incluye el error de redondeo.
  base <- .cmc_normal(2000L, 0, 0.4, 3L)
  d <- .cmc_deriva(data.frame(x = round(base, 2L)),
                   data.frame(x = round(base, 0L)))
  expect_false("deriva_dispersion" %in% d$aspecto)
  # Dos grupos que pasan de 99,4 y 100,6 a 99 y 101 (y algun valor en medio).
  set.seed(4L)
  base <- rep(c(99.4, 100.6), 1000L) + stats::rnorm(2000L, 0, 0.3)
  d <- .cmc_deriva(data.frame(x = round(base, 2L)),
                   data.frame(x = round(base, 0L)))
  expect_false("deriva_dispersion" %in% d$aspecto)
})

test_that("con 40 valores por lado un desvio que se triplica se publica", {
  d <- .cmc_deriva(
    data.frame(x = .cmc_normal(40L, 0, 1, 5L)),
    data.frame(x = .cmc_normal(40L, 0, 3, 6L))
  )
  fila <- d[d$aspecto == "deriva_dispersion", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$delta, 3, tolerance = 1e-6)
})

test_that("con pocos valores la dispersion se calla si cabe en el ruido o falta muestra", {
  # 40 valores y un desvio que sube 30 %: ni siquiera pasa el umbral.
  d <- .cmc_deriva(data.frame(x = .cmc_normal(40L, 0, 1, 5L)),
                   data.frame(x = .cmc_normal(40L, 0, 1.3, 6L)))
  expect_false("deriva_dispersion" %in% d$aspecto)
  # 15 valores: debajo del minimo, aunque el desvio se cuadruplique.
  d <- .cmc_deriva(data.frame(x = .cmc_normal(15L, 0, 1, 5L)),
                   data.frame(x = .cmc_normal(15L, 0, 4, 6L)))
  expect_false("deriva_dispersion" %in% d$aspecto)
  # Una distribucion de cola pesada (un valor lejano en cada lado) tampoco.
  cola <- function(desvio, semilla) c(.cmc_normal(39L, 0, desvio, semilla), 40 * desvio)
  d <- .cmc_deriva(data.frame(x = cola(1, 5L)), data.frame(x = cola(3, 6L)))
  expect_false("deriva_dispersion" %in% d$aspecto)
})

test_that("una categoria que casi no esta en la entrega chica pero pesa un tercio se publica", {
  # 14 de 41 filas contra 14 de 278: con tan pocas filas en un lado, la clase
  # esperaria menos de cinco y el PSI la juntaba con el resto y callaba.
  anterior <- data.frame(g = rep(c("x", "y"), c(27L, 14L)))
  actual <- data.frame(g = rep(c("x", "y"), c(264L, 14L)))
  d <- .cmc_deriva(anterior, actual)
  fila <- d[d$aspecto == "deriva_categorias", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_true(grepl("prueba exacta", fila$descripcion, fixed = TRUE, useBytes = TRUE))
  expect_true(grepl("Fisher", fila$evidencia, fixed = TRUE, useBytes = TRUE))
})

test_that("las mismas proporciones con pocas filas no acusan categorias", {
  # Mismo reparto con 41 y 278 filas, y con 15 y 20.
  d <- .cmc_deriva(data.frame(g = rep(c("x", "y"), c(27L, 14L))),
                   data.frame(g = rep(c("x", "y"), c(183L, 95L))))
  expect_false("deriva_categorias" %in% d$aspecto)
  d <- .cmc_deriva(data.frame(g = rep(c("x", "y", "z"), c(8L, 5L, 2L))),
                   data.frame(g = rep(c("x", "y", "z"), c(11L, 6L, 3L))))
  expect_false("deriva_categorias" %in% d$aspecto)
  # De 1 % a 10 % con 100 filas por lado: la prueba exacta no alcanza el nivel de
  # la columna.
  d <- .cmc_deriva(data.frame(g = rep(c("x", "y"), c(99L, 1L))),
                   data.frame(g = rep(c("x", "y"), c(90L, 10L))))
  expect_false("deriva_categorias" %in% d$aspecto)
})
