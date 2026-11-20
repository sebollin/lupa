# Validez de dominio: ningun silencio. Una columna en otra escala avisa y
# nombra la escala; lo imposible se acusa por fila aunque lo demas este en otra
# escala; `perfilar_por()` decide con la tabla entera; `reportar()` no cuenta el
# aviso de columna entre los correctos.

testthat::skip_on_cran()

.F_O184e <- as.Date("2026-11-19")

.dom_O184e <- function(datos, ...) {
  perfil <- suppressWarnings(perfilar(
    datos, fecha = .F_O184e, analizar_dependencias = FALSE, ...
  ))
  h <- perfil$hallazgos
  h[h$tipo_hallazgo %in% c(
    "valor_fuera_dominio", "fecha_futura", "fecha_centinela"
  ), , drop = FALSE]
}

testthat::test_that("una columna en otra escala avisa y nombra la escala", {
  set.seed(1921)
  casos <- list(
    meses = data.frame(edad = 240:1200),
    dias = data.frame(edad = 0:30000),
    grados_por_10 = data.frame(latitud = runif(40, -35, -30) * 10),
    mezcla = data.frame(edad = c(sample(0:90, 49L, TRUE), rep(131, 24L),
                                 rep(500, 27L))),
    centinela_51 = data.frame(edad = c(rep(999, 51L), sample(0:90, 49L, TRUE)))
  )
  for (nombre in names(casos)) {
    h <- .dom_O184e(casos[[nombre]])
    testthat::expect_true(nrow(h) >= 1L, info = nombre)
    testthat::expect_true(all(h$severidad[h$n_afectados > 0] != "error"),
                          info = nombre)
    testthat::expect_true(any(h$severidad == "ok"), info = nombre)
  }
  h <- .dom_O184e(casos$meses)
  testthat::expect_true(grepl("expresada en meses", h$evidencia, fixed = TRUE))
  h <- .dom_O184e(casos$centinela_51)
  testthat::expect_true(grepl("centinela", h$evidencia, fixed = TRUE))
  testthat::expect_true(grepl("999", h$evidencia, fixed = TRUE))
  # Los nombres que ya anuncian la unidad siguen sin inferir.
  excluidas <- data.frame(
    edad_paciente_dias = sample(0:30000, 50L, TRUE),
    longitud_cable = sample(200:5000, 50L, TRUE)
  )
  testthat::expect_equal(nrow(.dom_O184e(excluidas)), 0L)
})

testthat::test_that("lo imposible se acusa aunque lo demas este en otra escala", {
  set.seed(1922)
  h <- .dom_O184e(data.frame(edad = c(rep(-500, 36L), sample(0:90, 24L, TRUE))))
  testthat::expect_identical(as.character(h$severidad), "sospechoso")
  testthat::expect_equal(h$n_afectados, 36)
  h <- .dom_O184e(data.frame(
    latitud = c(runif(40, 400, 600), runif(60, -40, -30))
  ))
  testthat::expect_true(any(h$severidad == "sospechoso"))
  testthat::expect_equal(sum(h$n_afectados[h$severidad == "sospechoso"]), 40)
  # Un porcentaje negativo en un nombre de porcentaje es imposible.
  for (v in list(-20:-1, rep(-0.5, 20L))) {
    h <- .dom_O184e(data.frame(pct = v))
    testthat::expect_identical(as.character(h$severidad), "sospechoso")
    testthat::expect_equal(h$n_afectados, 20)
  }
  # Con imposibles, lo que depende de la escala no desaparece: va en el aviso.
  h <- .dom_O184e(data.frame(
    edad = c(rep(-1, 15L), rep(200, 15L), sample(0:90, 70L, TRUE))
  ))
  testthat::expect_equal(nrow(h), 2L)
  testthat::expect_equal(h$n_afectados[h$severidad == "sospechoso"], 15)
  testthat::expect_equal(h$n_afectados[h$severidad == "ok"], 15)
})

testthat::test_that("perfilar_por decide el dominio con el contexto de la tabla", {
  set.seed(1923)
  n <- 100L
  t <- data.frame(
    g = rep(c("a", "b"), each = n / 2),
    lat = c(rep(95, 5L), runif(n - 5L, -40, -30)),
    lng = runif(n, -60, -50)
  )
  global <- .dom_O184e(t[c("lat", "lng")])
  testthat::expect_equal(sum(global$n_afectados), 5)
  por <- suppressWarnings(perfilar_por(
    t, por = "g", min_filas = 1L, fecha = .F_O184e,
    analizar_dependencias = FALSE
  ))
  d <- por[por$tipo_hallazgo == "valor_fuera_dominio", , drop = FALSE]
  testthat::expect_equal(sum(d$n_afectados), 5)
  # El centinela mayoritario se decide con la tabla, no con cada grupo.
  vig <- as.character(.F_O184e - sample(1:900, 20L))
  c60 <- data.frame(
    g = rep(c("a", "b"), each = 30L),
    fecha_desde = c(rep("1900-01-01", 40L), vig)
  )
  c60 <- c60[sample(nrow(c60)), ]
  por <- suppressWarnings(perfilar_por(
    c60, por = "g", min_filas = 1L, fecha = .F_O184e,
    analizar_dependencias = FALSE
  ))
  d <- por[por$tipo_hallazgo == "fecha_centinela", , drop = FALSE]
  testthat::expect_true(nrow(d) >= 1L)
  testthat::expect_true(all(d$severidad == "ok"))
})

testthat::test_that("reportar no cuenta el aviso de columna entre los correctos", {
  perfil <- suppressWarnings(perfilar(
    data.frame(pct = 101:120), fecha = .F_O184e, analizar_dependencias = FALSE
  ))
  ruta <- tempfile(fileext = ".html")
  on.exit(unlink(ruta), add = TRUE)
  invisible(reportar(perfil, archivo = ruta))
  html <- paste(readLines(ruta, warn = FALSE), collapse = "")
  tarjeta <- function(etiqueta) {
    patron <- paste0("<span>", etiqueta, "</span><strong>")
    posicion <- regexpr(patron, html, fixed = TRUE)
    if (posicion < 0L) return(NA_real_)
    resto <- substr(html, posicion + attr(posicion, "match.length"),
                    posicion + attr(posicion, "match.length") + 20L)
    as.numeric(sub("</strong>.*$", "", resto))
  }
  testthat::expect_equal(tarjeta("Avisos de columna"), 1)
  ok <- sum(perfil$hallazgos$severidad == "ok")
  testthat::expect_equal(tarjeta("Correctos"), ok - 1)
})

testthat::test_that("comparar un empeoramiento de columna a columna no lo lee resuelto", {
  set.seed(1924)
  a <- suppressWarnings(perfilar(
    data.frame(pct = c(rep(150, 10L), sample(0:100, 10L))),
    fecha = .F_O184e, analizar_dependencias = FALSE
  ))
  b <- suppressWarnings(perfilar(
    data.frame(pct = c(rep(1500, 12L), sample(0:100, 8L))),
    fecha = .F_O184e, analizar_dependencias = FALSE
  ))
  cmp <- comparar_perfiles(a, b)
  testthat::expect_false(any(
    cmp$cambio == "resuelto" & cmp$valor_anterior == "valor_fuera_dominio"
  ))
})
