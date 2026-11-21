# Paso 0: `ratio` y `ratio_umbral` se calculan como numerador / denominador.

test_that("tablero_calidad y agregar calculan ratio y ratio_umbral como numerador / denominador", {
  skip_on_cran()
  skip_if(.Machine$longdouble.digits <= 53L,
          "En plataformas sin long double extendido mean() ya coincide con k / n.")
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  # 115 presentes de 2051: en x86-64 (long double de 64 bits de mantisa),
  # mean(c(rep(1, 115), rep(0, 1936)) == 1) difiere de 115 / 2051 en 1 ulp.
  d <- data.frame(
    x = c(rep(1, 115L), rep(NA_real_, 2051L - 115L)),
    p = c(rep(0.8, 115L), rep(0.2, 2051L - 115L))
  )
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "NN"), "d", "x"))
  m <- medir(mod, list(d = d), id_medicion = "m0", fecha = fecha,
             proteger_datos_personales = TRUE)
  tab <- tablero_calidad(m)
  agr <- agregar(m, "atributo", "ratio")
  esperado <- 115 / 2051
  expect_identical(tab$valor[[1L]], esperado)
  expect_identical(agr$resultado[[1L]], esperado)

  m_real <- metrica(
    "PuntajeReal", "Puntaje en [0, 1].", "instanciaAtributo", "real",
    dimension = "Exactitud", factor = "Exactitud sint\u00e1ctica",
    metodo = function(tablas, instancia) {
      x <- tablas[[instancia$entidad[[1L]]]][[instancia$atributos[[1L]]]]
      data.frame(
        resultado = x,
        entidad = instancia$entidad[[1L]],
        atributo = instancia$atributos[[1L]],
        fila = seq_along(x),
        objeto = paste0(instancia$entidad[[1L]], "$",
                        instancia$atributos[[1L]], "[", seq_along(x), "]"),
        stringsAsFactors = FALSE
      )
    }
  )
  mod_r <- modelo(instanciar(especializar(m_real, "PR"), "d", "p"))
  mr <- medir(mod_r, list(d = d), id_medicion = "m0r", fecha = fecha,
              proteger_datos_personales = TRUE)
  agr_umbral <- agregar(mr, "atributo", "ratio_umbral", umbral = 0.5)
  expect_identical(agr_umbral$resultado[[1L]], esperado)
})
