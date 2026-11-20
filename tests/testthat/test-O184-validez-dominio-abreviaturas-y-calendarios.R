# Validez de dominio: una abreviatura de coordenada sin pareja avisa si el
# contenido parece una coordenada, `lat` + `long` y los calificativos de
# sistema de referencia forman pareja, los calendarios y pronosticos no avisan
# de fechas futuras y se reconocen mas nombres de edad de personas.

testthat::skip_on_cran()

.F_O184i <- as.Date("2026-11-19")

.dom_O184i <- function(datos, ...) {
  perfil <- suppressWarnings(perfilar(
    datos, fecha = .F_O184i, analizar_dependencias = FALSE, ...
  ))
  h <- perfil$hallazgos
  h[h$tipo_hallazgo %in% c(
    "valor_fuera_dominio", "fecha_futura", "fecha_centinela"
  ), , drop = FALSE]
}

.lat_O184i <- function(fuera = 5L, n = 100L) {
  c(rep(95.5, fuera), round(runif(n - fuera, -60, -30), 5))
}
.lon_O184i <- function(fuera = 5L, n = 100L) {
  c(rep(195.5, fuera), round(runif(n - fuera, -70, -50), 5))
}

testthat::test_that("una abreviatura de coordenada sola avisa si el contenido lo parece", {
  set.seed(1981)
  casos <- list(
    lat = .lat_O184i(), lon = .lon_O184i(), lng = .lon_O184i(),
    lat_wgs84 = .lat_O184i(), geo_lat = .lat_O184i()
  )
  for (nombre in names(casos)) {
    h <- .dom_O184i(setNames(data.frame(casos[[nombre]]), nombre))
    testthat::expect_equal(nrow(h), 1L, info = nombre)
    testthat::expect_identical(h$tipo_hallazgo, "valor_fuera_dominio",
                               info = nombre)
    # Solo avisa: con una abreviatura no se acusan filas.
    testthat::expect_identical(as.character(h$severidad), "ok", info = nombre)
    testthat::expect_equal(h$n_afectados, 5, info = nombre)
    testthat::expect_false(grepl("minor", h$evidencia, fixed = TRUE),
                           info = nombre)
    testthat::expect_true(grepl("latencia", h$descripcion, fixed = TRUE),
                          info = nombre)
  }
})

testthat::test_that("la abreviatura sola no avisa si el contenido no es el de una coordenada", {
  set.seed(1982)
  casos <- list(
    # enteros (un indice), sin decimales de coordenada
    lat = c(rep(95, 5L), 1:95),
    # valores de un calculo (15 decimales)
    lat = c(rep(95.5, 5L), runif(95L, 0, 50)),
    # mas de la mitad fuera del rango
    lat = .lat_O184i(fuera = 60L),
    # pocos valores
    lat = c(95.5, round(runif(8L, -60, -30), 5)),
    # no es una abreviatura sin calificar
    latencia = .lat_O184i(), lona = .lon_O184i(), long_cable = .lon_O184i(),
    # `long` sola es el largo de algo
    long = .lon_O184i()
  )
  for (i in seq_along(casos)) {
    h <- .dom_O184i(setNames(data.frame(casos[[i]]), names(casos)[[i]]))
    testthat::expect_equal(nrow(h), 0L, info = paste(i, names(casos)[[i]]))
  }
})

testthat::test_that("la abreviatura sola avisa con una parte intermedia de imposibles, sin acantilado", {
  set.seed(1991)
  for (fuera in c(10L, 11L, 15L, 25L, 49L)) {
    h <- .dom_O184i(data.frame(lat = .lat_O184i(fuera = fuera)))
    testthat::expect_equal(nrow(h), 1L, info = fuera)
    testthat::expect_equal(h$n_afectados, fuera, info = fuera)
    testthat::expect_identical(as.character(h$severidad), "ok", info = fuera)
  }
})

testthat::test_that("`long` solo es pareja si su contenido es el de una coordenada", {
  set.seed(1992)
  n <- 100L
  largos <- list(
    round(runif(n, 0, 1000), 3), sample(0:255, n, TRUE),
    round(runif(n, 0, 5000), 3)
  )
  for (i in seq_along(largos)) {
    d <- data.frame(lat = round(runif(n, -40, -30), 5), long = largos[[i]])
    testthat::expect_equal(nrow(.dom_O184i(d)), 0L, info = i)
  }
  # Con latencias y largos, ni una ni otra se acusan.
  d <- data.frame(lat = round(runif(n, 0, 500), 3), long = round(runif(n, 0, 1000), 3))
  testthat::expect_equal(nrow(.dom_O184i(d)), 0L)
})

testthat::test_that("en un grupo sin la columna pareja, la pareja no baja de sospechoso", {
  set.seed(1993)
  t <- data.frame(
    g = rep(c("a", "b"), each = 50L),
    lat = c(rep(95.5, 5L), round(runif(45L, -40, -30), 5),
            rep(95.5, 5L), round(runif(45L, -40, -30), 5)),
    lng = c(round(runif(50L, -60, -50), 5), rep(NA_real_, 50L))
  )
  por <- suppressWarnings(perfilar_por(
    t, por = "g", min_filas = 1L, fecha = .F_O184i,
    analizar_dependencias = FALSE
  ))
  d <- por[por$tipo_hallazgo == "valor_fuera_dominio" & por$columna == "lat", ,
           drop = FALSE]
  testthat::expect_equal(nrow(d), 2L)
  testthat::expect_true(all(as.character(d$severidad) == "sospechoso"))
})

testthat::test_that("la abreviatura sola no se une a una columna de otra cosa", {
  set.seed(1983)
  # Con la latitud completa en la tabla, `lat` es otra cosa.
  h <- .dom_O184i(data.frame(
    lat = .lat_O184i(), latitud = runif(100L, -40, -30)
  ))
  testthat::expect_equal(nrow(h), 0L)
  # Una latitud sin sentido de coordenada (relativa) tampoco.
  h <- .dom_O184i(data.frame(lat_rel = .lat_O184i()))
  testthat::expect_equal(nrow(h), 0L)
})

testthat::test_that("la abreviatura sola avisa lo mismo con la tabla entera y por grupo", {
  set.seed(1984)
  t <- data.frame(
    g = rep(c("a", "b"), c(30L, 70L)),
    lat = .lat_O184i(fuera = 6L)[sample(100L)]
  )
  por <- suppressWarnings(perfilar_por(
    t, por = "g", min_filas = 1L, fecha = .F_O184i,
    analizar_dependencias = FALSE
  ))
  d <- por[por$tipo_hallazgo == "valor_fuera_dominio", , drop = FALSE]
  testthat::expect_true(nrow(d) >= 1L)
  testthat::expect_true(all(as.character(d$severidad) == "ok"))
})

testthat::test_that("`lat` + `long` y los calificativos de sistema de referencia forman pareja", {
  set.seed(1985)
  pares <- list(
    c("lat", "long"), c("lat_wgs84", "lon_wgs84"), c("lat_geo", "lon_geo"),
    c("lat_epsg4326", "lon_epsg4326"), c("lat", "lon_wgs84"),
    c("latitud", "longitud_wgs84")
  )
  for (par in pares) {
    d <- data.frame(.lat_O184i(), .lon_O184i())
    names(d) <- par
    h <- .dom_O184i(d)
    testthat::expect_setequal(h$columna, par)
    testthat::expect_true(all(as.character(h$severidad) == "sospechoso"),
                          info = paste(par, collapse = "+"))
    testthat::expect_true(all(h$n_afectados == 5),
                          info = paste(par, collapse = "+"))
  }
  # `long` con un largo de otra cosa no es una pareja.
  d <- data.frame(lat = .lat_O184i(), long_cable = .lon_O184i())
  testthat::expect_false("long_cable" %in% .dom_O184i(d)$columna)
})

testthat::test_that("un calendario, un pronostico o una lista de periodos avisan con la nota de calendario y sin acusar filas", {
  casos <- list(
    mensual = seq(as.Date("2020-01-01"), as.Date("2035-12-01"), by = "month"),
    diario = seq(as.Date("2020-01-01"), as.Date("2035-12-31"), by = "day"),
    repetido = rep(seq(.F_O184i - 100, .F_O184i + 100, by = "day"), each = 3L),
    pronostico = .F_O184i + 0:59,
    desde_manana = .F_O184i + 1:30,
    semanal = .F_O184i - 100 + 7 * (0:40),
    fin_de_mes = seq(as.Date("2025-02-01"), by = "month", length.out = 40) - 1,
    anual = as.Date(paste0(2015:2040, "-01-01")),
    habiles = {
      d <- seq(.F_O184i - 100, .F_O184i + 100, by = "day")
      d[!format(d, "%u") %in% c("6", "7")]
    }
  )
  # Se avisa de la columna, sin acusar filas, y el texto dice lo observado y la
  # posibilidad de que sea un calendario o un pronostico.
  for (nombre in names(casos)) {
    h <- .dom_O184i(data.frame(fecha = casos[[nombre]]))
    testthat::expect_equal(nrow(h), 1L, info = nombre)
    testthat::expect_identical(as.character(h$severidad), "ok", info = nombre)
    testthat::expect_true(grepl("calendario", h$descripcion, fixed = TRUE),
                          info = nombre)
    testthat::expect_true(grepl("grilla regular sin huecos", h$evidencia,
                                fixed = TRUE), info = nombre)
  }
  h <- .dom_O184i(data.frame(
    periodo = format(seq(as.Date("2026-01-01"), by = "month", length.out = 36L))
  ))
  testthat::expect_true(grepl("calendario", h$descripcion, fixed = TRUE))
  # Lo mismo por grupo: la tabla decide que es un calendario.
  t <- data.frame(
    g = rep(c("a", "b"), each = 30L), fecha = .F_O184i + 0:59
  )
  por <- suppressWarnings(perfilar_por(
    t, por = "g", min_filas = 1L, fecha = .F_O184i,
    analizar_dependencias = FALSE
  ))
  d <- por[por$tipo_hallazgo == "fecha_futura", , drop = FALSE]
  testthat::expect_true(all(as.character(d$severidad) == "ok"))
  testthat::expect_true(all(grepl("calendario", d$descripcion, fixed = TRUE)))
})

testthat::test_that("una marca de cuando se toco un registro no es un calendario", {
  f <- .F_O184i
  for (nombre in c("fecha_modificacion", "fecha_actualizacion", "fecha_captura",
                   "fecha_revision", "fecha_validacion")) {
    h <- .dom_O184i(setNames(data.frame(f + (-365):365), nombre))
    testthat::expect_equal(nrow(h), 1L, info = nombre)
    testthat::expect_false(grepl("calendario", h$descripcion, fixed = TRUE),
                           info = nombre)
  }
})

testthat::test_that("lo que no es una grilla sin huecos sigue avisando o acusando", {
  set.seed(1986)
  f <- .F_O184i
  casos <- list(
    # al azar
    list("fecha_alta", c(f - sample(1:900, 14L), f + sample(1:900, 36L)), "ok"),
    list("fecha", c(f - sample(1:1500, 192L), f + sample(30:900, 8L)),
         "sospechoso"),
    # una grilla con un valor suelto que la rompe
    list("fecha", c(seq(f - 200, f - 1, by = "day"), as.Date("2099-01-01")),
         "sospechoso"),
    list("fecha", c(seq(f - 200, f - 1, by = "day"), rep(as.Date("2030-01-01"), 3L)),
         "sospechoso"),
    # una grilla con huecos
    list("fecha", f + cumsum(sample(1:2, 80L, TRUE)), "ok"),
    # pocas fechas distintas
    list("fecha", f + 0:5, "ok"),
    # una tanda de fechas seguidas lejos del presente
    list("fecha_registro", as.Date("2030-01-01") + 0:19, "ok"),
    list("fecha", as.Date("2027-06-01") + 0:19, "ok"),
    # una fecha de registro (de algo que ya paso) no es un calendario
    list("fecha_alta", f + (-30):30, "ok"),
    # nadie nace en el futuro
    list("fecha_nacimiento", f + 0:59, "sospechoso")
  )
  for (i in seq_along(casos)) {
    caso <- casos[[i]]
    h <- .dom_O184i(setNames(data.frame(caso[[2L]]), caso[[1L]]))
    testthat::expect_equal(nrow(h), 1L, info = paste(i, caso[[1L]]))
    testthat::expect_identical(as.character(h$severidad), caso[[3L]],
                               info = paste(i, caso[[1L]]))
  }
  # Un calendario de feriados irregular sigue con su aviso.
  h <- .dom_O184i(data.frame(fecha = as.Date(c(
    "2024-01-01", "2024-05-01", "2024-08-25", "2024-12-25", "2025-01-01",
    "2025-05-01", "2025-08-25", "2025-12-25", "2026-01-01", "2026-05-01",
    "2026-08-25", "2026-12-25", "2027-01-01", "2027-05-01", "2027-08-25",
    "2027-12-25"
  ))))
  testthat::expect_equal(nrow(h), 1L)
})

testthat::test_that("se reconocen parientes y oficios como edad de persona", {
  set.seed(1987)
  nombres <- c("edad_conyuge", "edad_hermano", "edad_abuela", "edad_tutor",
               "edad_beneficiario", "edad_trabajador", "workers_age",
               "worker_age", "teacher_age", "baby_age", "guardian_age",
               "doctor_age", "wife_age", "age_of_husband", "driver_age")
  for (nombre in nombres) {
    v <- setNames(data.frame(c(rep(200, 5L), sample(1:80, 95L, TRUE))), nombre)
    h <- .dom_O184i(v)
    testthat::expect_equal(h$n_afectados, 5, info = nombre)
    v <- setNames(data.frame(c(rep(-1, 5L), sample(1:80, 95L, TRUE))), nombre)
    h <- .dom_O184i(v)
    testthat::expect_identical(as.character(h$severidad), "sospechoso",
                               info = nombre)
  }
  # Un objeto o una unidad de tiempo no es una edad de persona.
  for (nombre in c("edad_vehiculo", "edad_expediente", "edad_hermano_dias",
                   "worker_age_days", "edad_menor_meses", "edad_docente",
                   "worker_age_seconds", "worker_age_minutes", "worker_age_ms",
                   "teacher_age_minutes")) {
    v <- setNames(data.frame(c(rep(200, 5L), sample(1:80, 95L, TRUE))), nombre)
    testthat::expect_equal(nrow(.dom_O184i(v)), 0L, info = nombre)
  }
})
