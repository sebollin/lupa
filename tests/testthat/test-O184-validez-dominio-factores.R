# Validez de dominio: el factor que se publica es verdad (todos los que explican
# la columna), el perdon por centinela es solo del valor repetido, los nombres de
# coordenada y de edad se reconocen, y las convenciones legitimas (longitud de 0
# a 360, 9999-12-31 como "sin fin") avisan de la columna.

testthat::skip_on_cran()

.F_O184g <- as.Date("2026-11-19")

.dom_O184g <- function(datos, ...) {
  perfil <- suppressWarnings(perfilar(
    datos, fecha = .F_O184g, analizar_dependencias = FALSE, ...
  ))
  h <- perfil$hallazgos
  h[h$tipo_hallazgo %in% c(
    "valor_fuera_dominio", "fecha_futura", "fecha_centinela"
  ), , drop = FALSE]
}

testthat::test_that("se nombran todos los factores que explican la columna", {
  h <- .dom_O184g(data.frame(edad = 0:3650))
  testthat::expect_true(grepl("semanas", h$evidencia, fixed = TRUE))
  testthat::expect_true(grepl("d\u00edas", h$evidencia, fixed = TRUE))
  testthat::expect_true(grepl("no distinguen", h$evidencia, fixed = TRUE))
  h <- .dom_O184g(data.frame(edad = 0:1000))
  for (u in c("meses", "semanas", "d\u00edas")) {
    testthat::expect_true(grepl(u, h$evidencia, fixed = TRUE), info = u)
  }
  # Una edad en dias hasta 24 anos no es "multiplicada por 100".
  h <- .dom_O184g(data.frame(edad = 0:9000))
  testthat::expect_true(grepl("d\u00edas", h$evidencia, fixed = TRUE))
  testthat::expect_false(grepl("multiplicada", h$evidencia, fixed = TRUE))
  # Una edad en meses de 0 a 300 nombra el 12.
  h <- .dom_O184g(data.frame(edad = 0:300))
  testthat::expect_true(grepl("meses", h$evidencia, fixed = TRUE))
})

testthat::test_that("el perdon por centinela es solo del valor repetido", {
  set.seed(1941)
  h <- .dom_O184g(data.frame(
    latitud = c(sample(300:600, 95L, TRUE), rep(999, 4L), 950)
  ))
  testthat::expect_equal(h$n_afectados[h$severidad == "sospechoso"], 1)
  ok <- h[h$severidad == "ok", , drop = FALSE]
  testthat::expect_true(grepl("999", ok$evidencia, fixed = TRUE))
  testthat::expect_true(grepl("centinela", ok$evidencia, fixed = TRUE))
  h <- .dom_O184g(data.frame(
    edad = c(sample(0:1200, 95L, TRUE), rep(-999, 3L), rep(-500, 2L))
  ))
  testthat::expect_equal(h$n_afectados[h$severidad == "sospechoso"], 2)
})

testthat::test_that("dos codigos repetidos que concentran lo de afuera no son una escala", {
  set.seed(1942)
  for (v in list(
    data.frame(pct = c(rep(500, 45L), rep(999, 45L), sample(0:100, 10L))),
    data.frame(edad = c(rep(500, 45L), rep(999, 45L), sample(0:90, 10L)))
  )) {
    h <- .dom_O184g(v)
    testthat::expect_false(any(grepl("multiplicada|expresada", h$evidencia)))
    testthat::expect_true(any(grepl("500 y 999", h$evidencia, fixed = TRUE)))
  }
})

testthat::test_that("lo imposible repetido no es una escala: se acusa por fila", {
  set.seed(1947)
  h <- .dom_O184g(data.frame(
    latitud = c(rep(500, 45L), rep(999, 45L), runif(10, -40, -30))
  ))
  testthat::expect_identical(as.character(h$severidad), "sospechoso")
  testthat::expect_false(any(grepl("multiplicada", h$evidencia)))
})

testthat::test_that("el texto habla de la parte de la columna que esta en otra escala", {
  set.seed(1943)
  h <- .dom_O184g(data.frame(
    latitud = c(sample(400:600, 60L, TRUE), runif(40L, -40, -30))
  ))
  testthat::expect_true(grepl("el 60% de la columna", h$evidencia, fixed = TRUE))
})

testthat::test_that("las coordenadas y la edad se reconocen por parejas y calificativos", {
  set.seed(1944)
  par_fuera <- function(a, b) {
    d <- data.frame(c(rep(95, 5L), runif(95L, -40, -30)),
                    c(rep(195, 5L), runif(95L, -60, -50)))
    names(d) <- c(a, b)
    .dom_O184g(d)
  }
  for (par in list(c("lat", "longitud"), c("lat", "longitude"),
                   c("coord_lat", "coord_lon"), c("lat_inicio", "lon_inicio"),
                   c("Lat", "Longitud"), c("lat", "lng"))) {
    h <- par_fuera(par[[1L]], par[[2L]])
    testthat::expect_true(all(c(par[[1L]], par[[2L]]) %in% h$columna),
                          info = paste(par, collapse = "+"))
  }
  h <- .dom_O184g(data.frame(
    lon = runif(100L, -60, -50),
    latitud = c(rep(95, 5L), runif(95L, -40, -30))
  ))
  testthat::expect_true("latitud" %in% h$columna)
  for (nombre in c("age_in_years", "age_years")) {
    v <- setNames(data.frame(c(rep(200, 5L), sample(1:80, 95L, TRUE))), nombre)
    h <- .dom_O184g(v)
    testthat::expect_equal(h$n_afectados, 5, info = nombre)
  }
  # Sin pareja, o con nombres que no son coordenadas: nada.
  for (nombre in c("lat", "lon", "latencia", "lona", "long_cable",
                   "latitud_rel")) {
    v <- setNames(data.frame(c(rep(500, 5L), runif(95L, 1, 10))), nombre)
    testthat::expect_equal(nrow(.dom_O184g(v)), 0L, info = nombre)
  }
})

testthat::test_that("las convenciones legitimas avisan de la columna", {
  set.seed(1945)
  h <- .dom_O184g(data.frame(
    longitud = runif(100L, 0, 360), latitud = runif(100L, -40, 40)
  ))
  h <- h[h$columna == "longitud", , drop = FALSE]
  testthat::expect_identical(as.character(h$severidad), "ok")
  testthat::expect_true(grepl("0 a 360", h$evidencia, fixed = TRUE))
  # Con negativos no es esa convencion: el 195 sigue siendo un error.
  h <- .dom_O184g(data.frame(
    longitud = c(rep(195, 5L), runif(95L, -60, -50)),
    latitud = runif(100L, -40, 40)
  ))
  testthat::expect_identical(as.character(h$severidad), "sospechoso")
  reales <- as.character(.F_O184g - sample(1:900, 30L))
  for (caso in list(
    c(nombre = "fecha_hasta", n = 1L),
    c(nombre = "vigencia_hasta", n = 20L),
    c(nombre = "fecha_fin", n = 3L)
  )) {
    f <- c(rep("9999-12-31", as.integer(caso[["n"]])), reales)
    h <- .dom_O184g(setNames(data.frame(f), caso[["nombre"]]))
    h <- h[h$tipo_hallazgo == "fecha_centinela", , drop = FALSE]
    testthat::expect_equal(nrow(h), 1L, info = caso[["nombre"]])
    testthat::expect_identical(as.character(h$severidad), "ok",
                               info = caso[["nombre"]])
    testthat::expect_true(grepl("sin fin", h$descripcion, fixed = TRUE),
                          info = caso[["nombre"]])
  }
  # 1900-01-01 en esos nombres sigue como antes.
  f <- c("1900-01-01", "1900-01-01", as.character(.F_O184g + sample(10:900, 18L)))
  h <- .dom_O184g(data.frame(fecha_vencimiento = f))
  testthat::expect_identical(as.character(h$severidad), "sospechoso")
})

testthat::test_that("perfilar_por pasa a la tabla los argumentos de perfilar", {
  set.seed(1946)
  t <- data.frame(
    g = rep(c("a", "b"), each = 50L),
    edad = c(sample(0:90, 90L, TRUE), rep(200, 10L))
  )
  por <- suppressWarnings(perfilar_por(
    t, por = "g", min_filas = 1L, fecha = .F_O184g, muestra = 1000,
    analizar_dependencias = FALSE
  ))
  d <- por[por$tipo_hallazgo == "valor_fuera_dominio", , drop = FALSE]
  testthat::expect_equal(sum(d$n_afectados), 10)
})

testthat::test_that("la convencion de 0 a 360 pide una parte sustancial arriba de 180", {
  set.seed(1951)
  # Europa con dos errores en 200: no es una convencion, son dos errores.
  h <- .dom_O184g(data.frame(
    longitud = c(runif(198L, 0, 40), 200, 250), latitud = runif(200L, 35, 60)
  ))
  h <- h[h$columna == "longitud", , drop = FALSE]
  testthat::expect_identical(as.character(h$severidad), "sospechoso")
  testthat::expect_equal(h$n_afectados, 2)
  # Americas en 0-360 (180:330): aviso de columna.
  h <- .dom_O184g(data.frame(
    longitud = runif(100L, 180, 330), latitud = runif(100L, -50, 50)
  ))
  h <- h[h$columna == "longitud", , drop = FALSE]
  testthat::expect_identical(as.character(h$severidad), "ok")
  # 9 de 100 arriba de 180 (menos del 10%): sigue siendo por fila.
  h <- .dom_O184g(data.frame(
    longitud = c(runif(91L, 0, 40), runif(9L, 200, 300)),
    latitud = runif(100L, 35, 60)
  ))
  h <- h[h$columna == "longitud", , drop = FALSE]
  testthat::expect_identical(as.character(h$severidad), "sospechoso")
})

testthat::test_that("los avisos afirman lo observado y dejan la causa como posibilidad", {
  set.seed(1952)
  h <- .dom_O184g(data.frame(latitud = runif(200L, -80, 80) * 5))
  testthat::expect_true(grepl("puede estar multiplicada por 10", h$evidencia,
                              fixed = TRUE))
  testthat::expect_true(grepl("u otra escala", h$evidencia, fixed = TRUE))
  testthat::expect_false(grepl("parece", h$evidencia, fixed = TRUE))
  h <- .dom_O184g(data.frame(edad = 0:3650))
  testthat::expect_true(grepl("puede estar expresada en", h$evidencia,
                              fixed = TRUE))
  h <- .dom_O184g(data.frame(
    longitud = runif(100L, 180, 330), latitud = runif(100L, -50, 50)
  ))
  h <- h[h$columna == "longitud", , drop = FALSE]
  testthat::expect_true(grepl("puede ser la convenci\u00f3n", h$evidencia,
                              fixed = TRUE))
})
