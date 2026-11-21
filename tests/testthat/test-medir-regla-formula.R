# Paso 2: ReglaIntegridadIntraEntidad(regla = ~ ...) con evaluador AST cerrado.

test_that("ReglaIntegridadIntraEntidad acepta formula y coincide con su function(d) gemela", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(
    inicio = as.Date(c("2026-01-01", "2026-01-10", NA, "2026-01-05")),
    fin = as.Date(c("2026-01-05", "2026-01-08", "2026-01-09", NA)),
    a = c(1, 2, 3, 4),
    b = c(2, 1, 3, 5),
    t = c("x", "y", NA, "x"),
    stringsAsFactors = FALSE
  )
  lim <- 2
  m_formula <- modelo(
    instanciar(
      especializar(nuc$ReglaIntegridadIntraEntidad, "Orden",
                   regla = ~ is.na(inicio) | is.na(fin) | inicio <= fin),
      "d", c("inicio", "fin"), nombre_instancia = "Orden"
    ),
    instanciar(
      especializar(nuc$ReglaIntegridadIntraEntidad, "Arit",
                   regla = ~ a + b >= 3 & a <= 2 + 2),
      "d", c("a", "b"), nombre_instancia = "Arit"
    ),
    instanciar(
      especializar(nuc$ReglaIntegridadIntraEntidad, "NegIn",
                   regla = ~ !(t %in% c("x", NA))),
      "d", "t", nombre_instancia = "NegIn"
    )
  )
  m_funcion <- modelo(
    instanciar(
      especializar(nuc$ReglaIntegridadIntraEntidad, "Orden",
                   regla = function(x) is.na(x$inicio) | is.na(x$fin) | x$inicio <= x$fin),
      "d", c("inicio", "fin"), nombre_instancia = "Orden"
    ),
    instanciar(
      especializar(nuc$ReglaIntegridadIntraEntidad, "Arit",
                   regla = function(x) x$a + x$b >= 3 & x$a <= lim + 2),
      "d", c("a", "b"), nombre_instancia = "Arit"
    ),
    instanciar(
      especializar(nuc$ReglaIntegridadIntraEntidad, "NegIn",
                   regla = function(x) !(x$t %in% c("x", NA))),
      "d", "t", nombre_instancia = "NegIn"
    )
  )
  r_for <- medir(m_formula, list(d = d), id_medicion = "m2", fecha = fecha,
                 proteger_datos_personales = TRUE)
  r_fun <- medir(m_funcion, list(d = d), id_medicion = "m2", fecha = fecha,
                 proteger_datos_personales = TRUE)
  expect_identical(as.data.frame(unclass(r_for), stringsAsFactors = FALSE), as.data.frame(unclass(r_fun), stringsAsFactors = FALSE))
  expect_identical(
    r_for$resultado[r_for$metrica_instanciada == "Orden"],
    c(1, 0, 1, 1)
  )
  expect_identical(
    r_for$resultado[r_for$metrica_instanciada == "NegIn"],
    c(0, 1, 0, 0)
  )
})

test_that("formula con NA en el resultado da no_medible con el texto del validador", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(
    inicio = as.Date(c("2026-01-01", NA, "2026-01-05")),
    fin = as.Date(c("2026-01-05", "2026-01-08", "2026-01-06"))
  )
  mod <- modelo(instanciar(
    especializar(nuc$ReglaIntegridadIntraEntidad, "Orden",
                 regla = ~ inicio <= fin),
    "d", c("inicio", "fin")
  ))
  r <- suppressWarnings(medir(mod, list(d = d), id_medicion = "m2na",
                              fecha = fecha, proteger_datos_personales = TRUE))
  expect_identical(nrow(r), 0L)
  cob <- attr(r, "cobertura_metricas", exact = TRUE)
  expect_identical(cob$estado[[1L]], "no_medible")
  expect_match(
    cob$motivo[[1L]],
    "debe devolver un vector l\u00f3gico sin NA de longitud 3",
    fixed = TRUE
  )
})

test_that("la formula ve solo los atributos ligados y no columnas no ligadas", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(a = c(1, 2, 3), oculta = c(10, 20, 30))
  mod <- modelo(instanciar(
    especializar(nuc$ReglaIntegridadIntraEntidad, "NoLigada",
                 regla = ~ a < oculta),
    "d", "a"
  ))
  r <- suppressWarnings(medir(mod, list(d = d), id_medicion = "m2s",
                              fecha = fecha, proteger_datos_personales = TRUE))
  expect_identical(nrow(r), 0L)
  cob <- attr(r, "cobertura_metricas", exact = TRUE)
  expect_identical(cob$estado[[1L]], "no_medible")
})

test_that("la formula nunca ejecuta llamadas fuera de la lista cerrada (F1)", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(t = c("x", "y", "z"), n = c(1, 2, 3), stringsAsFactors = FALSE)

  marca <- 0L
  boom <- function() {
    marca <<- marca + 1L
    "x"
  }
  env_trampa <- new.env(parent = baseenv())
  makeActiveBinding("activo", function() {
    marca <<- marca + 1L
    1L
  }, env_trampa)

  reglas_prohibidas <- list(
    llamada = ~ t == boom(),
    bloque = ~ { boom(); t == "x" },
    cortocircuito = ~ FALSE & boom(),
    en_conjunto = ~ t %in% c("x", boom()),
    funcion_no_admitida = ~ nchar(t) > 0,
    enlace_activo = stats::as.formula("~ n == activo", env = env_trampa)
  )

  for (nm in names(reglas_prohibidas)) {
    marca <- 0L
    mod <- modelo(instanciar(
      especializar(nuc$ReglaIntegridadIntraEntidad, "Seg",
                   regla = reglas_prohibidas[[nm]]),
      "d", c("t", "n")
    ))
    r <- suppressWarnings(medir(mod, list(d = d), id_medicion = "m2f1",
                                fecha = fecha, proteger_datos_personales = TRUE))
    expect_identical(marca, 0L, info = nm)
    expect_identical(nrow(r), 0L, info = nm)
    cob <- attr(r, "cobertura_metricas", exact = TRUE)
    expect_identical(cob$estado[[1L]], "no_medible", info = nm)
  }
})
