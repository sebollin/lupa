# Validez de dominio: un reescalado es un reescalado (un factor de una lista corta
# explica casi toda la columna), lo imposible no se arregla con un factor, lo que
# se publica es verdad, y `perfilar_por()` decide con la tabla entera.

testthat::skip_on_cran()

.F_O184f <- as.Date("2026-11-19")

.dom_O184f <- function(datos, ...) {
  perfil <- suppressWarnings(perfilar(
    datos, fecha = .F_O184f, analizar_dependencias = FALSE, ...
  ))
  h <- perfil$hallazgos
  h[h$tipo_hallazgo %in% c(
    "valor_fuera_dominio", "fecha_futura", "fecha_centinela"
  ), , drop = FALSE]
}

testthat::test_that("un negativo lejano en masa no es un reescalado", {
  set.seed(1931)
  casos <- list(
    edad_100 = data.frame(edad = rep(-500, 20L)),
    edad_95 = data.frame(edad = c(rep(-500, 95L), sample(0:90, 5L))),
    edad_90 = data.frame(edad = c(rep(-500, 90L), sample(0:90, 10L))),
    pct_100 = data.frame(pct = rep(-200, 20L)),
    pct_90 = data.frame(pct = c(rep(-200, 90L), sample(0:100, 10L)))
  )
  for (nombre in names(casos)) {
    h <- .dom_O184f(casos[[nombre]])
    testthat::expect_equal(nrow(h), 1L, info = nombre)
    testthat::expect_identical(as.character(h$severidad), "sospechoso",
                               info = nombre)
  }
})

testthat::test_that("la latitud por 10 se lee igual cruce o no el 90%", {
  set.seed(1932)
  for (n_dentro in c(18L, 21L)) {
    # 200 valores: n_dentro quedan en [-90, 90] y el resto, afuera, todos de la
    # latitud real multiplicada por 10.
    base <- c(runif(n_dentro, -8, 8), runif(200L - n_dentro, -80, -10))
    h <- .dom_O184f(data.frame(latitud = base * 10))
    testthat::expect_equal(nrow(h), 1L, info = n_dentro)
    testthat::expect_identical(as.character(h$severidad), "ok", info = n_dentro)
    testthat::expect_true(grepl("multiplicada por 10", h$evidencia, fixed = TRUE),
                          info = n_dentro)
  }
})

testthat::test_that("un 10:14 multiplicado por 10 no se confunde con un indice de base 100", {
  h <- .dom_O184f(data.frame(pct = 101:140))
  testthat::expect_equal(nrow(h), 1L)
  testthat::expect_false(grepl("multiplicada", h$evidencia, fixed = TRUE))
  h <- .dom_O184f(data.frame(latitud = 91:110))
  testthat::expect_identical(as.character(h$severidad), "sospechoso")
  h <- .dom_O184f(data.frame(edad = 240:1200))
  testthat::expect_true(grepl("expresada en meses", h$evidencia, fixed = TRUE))
  h <- .dom_O184f(data.frame(edad = sample(0:30000, 200L, TRUE)))
  testthat::expect_true(grepl("expresada en d\u00edas", h$evidencia, fixed = TRUE))
  # Un 999 repetido es un centinela, no meses.
  h <- .dom_O184f(data.frame(edad = c(rep(999, 51L), sample(0:90, 49L, TRUE))))
  testthat::expect_true(grepl("centinela", h$evidencia, fixed = TRUE))
})

testthat::test_that("un negativo que ningun factor arregla se acusa aunque el resto sea un reescalado", {
  set.seed(1933)
  h <- .dom_O184f(data.frame(edad = c(sample(240:1200, 96L, TRUE),
                                      -5, -6, -7, -8)))
  testthat::expect_equal(h$n_afectados[h$severidad == "sospechoso"], 4)
  testthat::expect_equal(h$n_afectados[h$severidad == "ok"], 96)
})

testthat::test_that("el aviso publica el rango de lo de afuera, no una cifra que cae adentro", {
  set.seed(1934)
  h <- .dom_O184f(data.frame(
    pct = c(150:194, 150:194, sample(0:100, 10L, TRUE))
  ))
  h <- h[h$severidad == "ok", , drop = FALSE]
  testthat::expect_equal(nrow(h), 1L)
  testthat::expect_true(grepl("van de 150 a 194", h$evidencia, fixed = TRUE))
  testthat::expect_false(grepl("mediana de lo de afuera es", h$evidencia,
                               fixed = TRUE))
})

testthat::test_that("print cuenta el aviso de columna aparte", {
  perfil <- suppressWarnings(perfilar(
    data.frame(pct = 101:140), fecha = .F_O184f, analizar_dependencias = FALSE
  ))
  salida <- paste(testthat::capture_messages(print(perfil)), collapse = "")
  testthat::expect_true(grepl("aviso de columna", salida, fixed = TRUE))
  ok <- sum(perfil$hallazgos$severidad == "ok")
  testthat::expect_true(grepl(
    paste0((ok - 1L), " hallazgo"), salida, fixed = TRUE
  ))
})

testthat::test_that("perfilar_por decide el reescalado y la inferencia con la tabla", {
  set.seed(1935)
  t <- data.frame(
    g = rep(c("a", "b"), c(30L, 70L)),
    edad = c(rep(-500, 30L), sample(0:90, 70L, TRUE))
  )
  por <- suppressWarnings(perfilar_por(
    t, por = "g", min_filas = 1L, fecha = .F_O184f,
    analizar_dependencias = FALSE
  ))
  d <- por[por$tipo_hallazgo == "valor_fuera_dominio", , drop = FALSE]
  testthat::expect_identical(as.character(d$severidad), "sospechoso")
  testthat::expect_equal(d$n_afectados, 30)
  # La columna par toda ausente en un grupo no le quita la regla a `lat`.
  n <- 100L
  coord <- data.frame(
    g = rep(c("a", "b"), each = n / 2L),
    lat = c(runif(n / 2L, -40, -30), rep(95, 5L), runif(n / 2L - 5L, -40, -30)),
    lng = c(runif(n / 2L, -60, -50), rep(NA_real_, n / 2L))
  )
  por <- suppressWarnings(perfilar_por(
    coord, por = "g", min_filas = 1L, fecha = .F_O184f,
    analizar_dependencias = FALSE
  ))
  d <- por[por$tipo_hallazgo == "valor_fuera_dominio", , drop = FALSE]
  testthat::expect_equal(sum(d$n_afectados), 5)
})

testthat::test_that("la tabla entera respeta aplicabilidad", {
  set.seed(1936)
  n <- 100L
  aplica <- c(rep(TRUE, 90L), rep(FALSE, 10L))
  edad <- c(sample(0:90, 85L, TRUE), rep(200, 5L), rep(200, 10L))
  t <- data.frame(g = rep(c("a", "b"), each = 50L), aplica = aplica,
                  edad = edad)
  directo <- suppressWarnings(perfilar(
    t[c("aplica", "edad")], fecha = .F_O184f, analizar_dependencias = FALSE,
    aplicabilidad = list(edad = ~ aplica)
  ))
  h <- directo$hallazgos[directo$hallazgos$tipo_hallazgo == "valor_fuera_dominio",
                         , drop = FALSE]
  testthat::expect_identical(as.character(h$severidad), "sospechoso")
  por <- suppressWarnings(perfilar_por(
    t, por = "g", min_filas = 1L, fecha = .F_O184f,
    analizar_dependencias = FALSE, aplicabilidad = list(edad = ~ aplica)
  ))
  d <- por[por$tipo_hallazgo == "valor_fuera_dominio", , drop = FALSE]
  testthat::expect_true(all(d$severidad == "sospechoso"))
})

testthat::test_that("el centinela mayoritario se mide con lo que se publica", {
  set.seed(1937)
  reales <- as.character(.F_O184f - sample(1:900, 29L))
  nac <- c(rep("1900-01-01", 29L), "1970-01-01", "1970-01-01", reales)
  h <- .dom_O184f(data.frame(fecha_desde = nac))
  h <- h[h$tipo_hallazgo == "fecha_centinela", , drop = FALSE]
  testthat::expect_equal(nrow(h), 1L)
  testthat::expect_identical(as.character(h$severidad), "sospechoso")
  testthat::expect_equal(h$n_afectados, 29)
})
