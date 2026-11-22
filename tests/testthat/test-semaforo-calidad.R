# `semaforo_calidad()`: pinta solo lo que se gano el color, con cortes por
# dimension o por metrica, y nunca combina dimensiones.

.sc_fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")

.sc_resumen <- function(d = NULL) {
  nuc <- metricas_nucleo()
  if (is.null(d)) {
    d <- data.frame(n = c(1, 2, NA, 4, 2, 100, 0, NA, 5, 6),
                    t = c("x", "y", NA, "x", "z", NA, "x", "y", "x", "x"),
                    stringsAsFactors = FALSE)
  }
  medir(modelo(
    instanciar(especializar(nuc$NoNulo, "NN"), "d", "n"),
    instanciar(especializar(nuc$Formato, "Dic", diccionario = c("x", "y")), "d", "t"),
    instanciar(especializar(nuc$AtributoDuplicado, "Dup"), "d", "t"),
    instanciar(especializar(nuc$ValoresPosiblesPorComprension, "Rng", minimo = 0, maximo = 10),
               "d", "n")
  ), list(d = d), id_medicion = "s", fecha = .sc_fecha, detalle = "resumen")
}

test_that("las celdas son las del tablero y cada una cae en su banda", {
  skip_on_cran()
  mr <- .sc_resumen()
  s <- semaforo_calidad(mr, cortes = list(Completitud = c(0.7, 0.9), Exactitud = c(0.5, 0.95),
                                          Unicidad = c(0.5, 0.9), Consistencia = c(0.5, 0.8)))
  tb <- tablero_calidad(mr)
  expect_s3_class(s, "semaforo_calidad")
  expect_identical(s$componente, tb$componente)
  expect_identical(s$metrica_instanciada, tb$metrica_instanciada)
  expect_identical(s$valor, tb$valor)
  nivel <- stats::setNames(s$nivel, s$metrica_instanciada)
  expect_identical(unname(nivel["NN@d.n"]), "amarillo")       # 0.8
  expect_identical(unname(nivel["Dic@d.t"]), "amarillo")      # 0.875
  expect_identical(unname(nivel["Dup@d.t"]), "rojo")          # 1 - 0.875
  expect_identical(unname(nivel["Rng@d.n"]), "verde")         # 7/8 contra 0.8
  # Los vacios son de la completitud: la validez evaluo todo su universo.
  expect_identical(s$fraccion_evaluada, rep(1, 4L))
  dup <- s[s$metrica_instanciada == "Dup@d.t", ]
  expect_identical(dup$escala_comparada, "1 - valor")
  expect_equal(c(dup$desde, dup$hasta), c(0.5, 1))
  expect_false(any(c("total", "peor") %in% names(attr(s, "conteo_niveles"))))
  expect_false(any(grepl("total", attr(s, "conteo_niveles")$dimension)))
})

test_that("el borde del corte alcanza el nivel, tambien con la resta de las de defecto", {
  skip_on_cran()
  nuc <- metricas_nucleo()
  d <- data.frame(t = c(rep("x", 9L), "z"), u = c(rep("a", 9L), "b"), stringsAsFactors = FALSE)
  mr <- medir(modelo(instanciar(especializar(nuc$Formato, "Dic", diccionario = "x"), "d", "t"),
                     instanciar(especializar(nuc$AtributoDuplicado, "Dup"), "d", "u")),
              list(d = d), id_medicion = "s", fecha = .sc_fecha, detalle = "resumen")
  # Dic = 0.9; Dup = 0.9 de duplicadas, comparado 1 - 0.9 = 0.0999...
  s <- semaforo_calidad(mr, cortes = list(Exactitud = c(0.5, 0.9), Unicidad = c(0.1, 0.5)))
  expect_identical(s$nivel, c("verde", "amarillo"))
  abajo <- semaforo_calidad(mr, cortes = list(Exactitud = c(0.5, 0.9 + 1e-9),
                                              Unicidad = c(0.1 + 1e-9, 0.5)))
  expect_identical(abajo$nivel, c("amarillo", "rojo"))
})

test_that("sin cuentas que cierren no hay color, aunque el valor sea verde", {
  skip_on_cran()
  mitad <- metrica(
    "Mitad", "Mide la mitad.", "instanciaAtributo", "booleano",
    dimension = "Exactitud", factor = "Exactitud sintactica",
    metodo = function(tablas, instancia) {
      x <- tablas[[instancia$entidad[[1L]]]][[instancia$atributos[[1L]]]]
      f <- which(!is.na(x)); f <- f[f %% 2L == 0L]
      data.frame(resultado = rep(TRUE, length(f)), entidad = instancia$entidad[[1L]],
                 atributo = instancia$atributos[[1L]], fila = f,
                 objeto = paste0("d$x[", f, "]"), stringsAsFactors = FALSE)
    }
  )
  entidad <- metrica(
    "UnaCifra", "Una cifra por tabla.", "entidad", "real",
    dimension = "Exactitud", factor = "Exactitud sintactica",
    metodo = function(tablas, instancia) {
      data.frame(resultado = 0.99, entidad = instancia$entidad[[1L]], atributo = NA_character_,
                 fila = NA_integer_, objeto = instancia$entidad[[1L]], stringsAsFactors = FALSE)
    }
  )
  d <- data.frame(x = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10))
  mr <- suppressWarnings(medir(modelo(instanciar(especializar(mitad, "M"), "d", "x"),
                                      instanciar(especializar(entidad, "U"), "d")),
                               list(d = d), id_medicion = "s", fecha = .sc_fecha,
                               detalle = "resumen"))
  s <- semaforo_calidad(mr, cortes = list(Exactitud = c(0.5, 0.9)))
  expect_identical(s$nivel, c(NA_character_, NA_character_))
  expect_identical(s$motivo_sin_color, c("no_cierran", "sin_cuentas"))
})

test_that("un vocabulario truncado y una metrica no acotada quedan sin color", {
  skip_on_cran()
  skip_if_not_installed("stringdist")
  nuc <- metricas_nucleo()
  d <- data.frame(t = c("a", "b", "c", "d", "e", "f", NA, "a", "b", "c"), n = c(1:9, NA),
                  stringsAsFactors = FALSE)
  mr <- medir(modelo(
    instanciar(especializar(nuc$EntidadContradictoria, "EC", max_valores = 3L), "d", "t"),
    instanciar(especializar(nuc$Escala, "Esc", escala = escala(error = 0.5, tipo = "absoluto")), "d", "n")
  ), list(d = d), id_medicion = "s", fecha = .sc_fecha, detalle = "resumen")
  s <- semaforo_calidad(mr, cortes = list(Unicidad = c(0.5, 0.9), Exactitud = c(0.5, 0.9)))
  motivos <- stats::setNames(s$motivo_sin_color, s$metrica_instanciada)
  expect_identical(unname(motivos["EC@d.t"]), "parcial")
  expect_identical(unname(motivos["Esc@d.n"]), "no_acotada")
})

test_that("manda la regla mas especifica y `por` separa la instancia de la generica", {
  skip_on_cran()
  nuc <- metricas_nucleo()
  d <- data.frame(t = c("x", "y", "x", "z"), stringsAsFactors = FALSE)
  mr <- medir(modelo(
    instanciar(especializar(nuc$NoNulo, "NN"), "d", "t", nombre_instancia = "Formato"),
    instanciar(especializar(nuc$Formato, "Dic", diccionario = c("x", "y")), "d", "t"),
    instanciar(especializar(nuc$Formato, "Otro", diccionario = c("x")), "d", "t")
  ), list(d = d), id_medicion = "s", fecha = .sc_fecha, detalle = "resumen")
  cortes <- data.frame(
    por = c("dimension", "metrica", "metrica_especifica", "metrica_instanciada"),
    nombre = c("Exactitud", "Formato", "Otro", "Formato"),
    amarillo = c(0.1, 0.2, 0.3, 0.4), verde = c(0.6, 0.7, 0.8, 0.9),
    stringsAsFactors = FALSE
  )
  s <- semaforo_calidad(mr, cortes = cortes)
  regla <- stats::setNames(s$regla, s$metrica_instanciada)
  expect_identical(unname(regla["Formato"]), "metrica_instanciada:Formato")
  expect_identical(unname(regla["Dic@d.t"]), "metrica:Formato")
  expect_identical(unname(regla["Otro@d.t"]), "metrica_especifica:Otro")
})

test_that("una clave sin celdas se declara, una mal escrita se rechaza, y cortes se valida", {
  skip_on_cran()
  mr <- .sc_resumen()
  s <- semaforo_calidad(mr, cortes = list(Completitud = c(0.5, 0.9), Frescura = c(0.5, 0.9)))
  expect_identical(attr(s, "cortes_sin_celdas")$nombre, "Frescura")
  expect_error(semaforo_calidad(mr, cortes = list(completitud = c(0.5, 0.9))), "se parece")
  expect_error(semaforo_calidad(mr, cortes = list()), "al menos una regla")
  expect_error(semaforo_calidad(mr, cortes = list(Completitud = c(0.9, 0.5))), "crecientes")
  expect_error(semaforo_calidad(mr, cortes = list(Completitud = c(0.5, 1.2))), "crecientes")
  expect_error(semaforo_calidad(mr, cortes = list(Completitud = 0.5)), "uno por nivel")
  expect_error(semaforo_calidad(mr), "obligatorio")
  expect_error(semaforo_calidad(mr, cortes = list(Completitud = c(0.5, 0.9)),
                                niveles = c("rojo", "rojo", "verde")), "distintos")
  # `evaluado_minimo` no existe: la validez evalua su propio universo.
  expect_error(semaforo_calidad(mr, cortes = data.frame(
    por = "dimension", nombre = "Completitud", amarillo = 0.5, verde = 0.9,
    evaluado_minimo = 0.5)), "no son niveles")
})

test_that("una metrica sin dimension solo se pinta con una regla por metrica", {
  skip_on_cran()
  sin_dim <- metrica(
    "SinDim", "Sin dimension.", "instanciaAtributo", "booleano",
    metodo = function(tablas, instancia) {
      x <- tablas[[instancia$entidad]][[instancia$atributos]]
      data.frame(resultado = !is.na(x), entidad = instancia$entidad,
                 atributo = instancia$atributos, fila = seq_along(x),
                 objeto = paste0("o", seq_along(x)), stringsAsFactors = FALSE)
    }
  )
  mr <- medir(modelo(instanciar(especializar(sin_dim, "SD"), "d", "x")),
              list(d = data.frame(x = c(1, 2, 3))), id_medicion = "s", fecha = .sc_fecha,
              detalle = "resumen")
  s <- semaforo_calidad(mr, cortes = list(Completitud = c(0.5, 0.9)), marco = marco_agesic())
  expect_identical(s$motivo_sin_color, "sin_dimension")
  s2 <- semaforo_calidad(mr, cortes = data.frame(por = "metrica", nombre = "SinDim",
                                                 amarillo = 0.5, verde = 0.9),
                         marco = marco_agesic())
  expect_identical(s2$nivel, "verde")
  expect_identical(attr(s2, "conteo_niveles")$dimension, "(sin dimensi\u00f3n)")
})

test_that("las no_resumible quedan fuera del semaforo sin tumbarlo", {
  skip_on_cran()
  nuc <- metricas_nucleo()
  d <- data.frame(f = as.Date(c("2026-01-01", "2026-01-05", NA)))
  vig <- vigencia("f", fecha_acceso = as.Date("2026-02-15"),
                  fecha_ultimo_cambio = as.Date("2026-02-01"), frecuencia_cambio = 10)
  mr <- medir(modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "f"),
                     instanciar(especializar(nuc$DesactualizacionPorFecha, "DF", vigencia = vig), "d")),
              list(d = d), id_medicion = "s", fecha = .sc_fecha, detalle = "resumen")
  expect_error(tablero_calidad(mr), class = "lupa_resumen_no_admitido")
  s <- semaforo_calidad(mr, cortes = list(Completitud = c(0.5, 0.9)))
  expect_identical(s$metrica_instanciada, "NN@d.f")
  expect_identical(attr(s, "fuera_del_semaforo")$metrica_instanciada, "DF@d")
  expect_identical(attr(s, "conteo_niveles")$fuera_del_semaforo[
    attr(s, "conteo_niveles")$dimension == "Frescura"], 1L)
  solo <- medir(modelo(instanciar(especializar(nuc$DesactualizacionPorFecha, "DF", vigencia = vig), "d")),
                list(d = d), id_medicion = "s", fecha = .sc_fecha, detalle = "resumen")
  vacio <- semaforo_calidad(solo, cortes = list(Completitud = c(0.5, 0.9)))
  expect_identical(nrow(vacio), 0L)
  expect_identical(attr(vacio, "fuera_del_semaforo")$metrica_instanciada, "DF@d")
})

test_that("un resultado no finito queda sin_denominador y no aborta", {
  skip_on_cran()
  mr <- .sc_resumen()
  mr$resultado[[1L]] <- NaN
  mr$denominador[[1L]] <- 0
  s <- semaforo_calidad(mr, cortes = list(Completitud = c(0.5, 0.9)))
  expect_identical(s$motivo_sin_color[s$metrica_instanciada == "NN@d.n"], "sin_denominador")
  expect_identical(nrow(s), 4L)
})

test_that("lo que no es un resumen se rechaza con su clase", {
  skip_on_cran()
  nuc <- metricas_nucleo()
  mod <- modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "x"))
  mc <- medir(mod, list(d = data.frame(x = c(1, NA))), id_medicion = "s", fecha = .sc_fecha)
  expect_error(semaforo_calidad(mc, cortes = list(Completitud = c(0.5, 0.9))),
               class = "lupa_semaforo_sin_cuentas")
  expect_error(semaforo_calidad(tablero_calidad(mc), cortes = list(Completitud = c(0.5, 0.9))),
               class = "lupa_semaforo_sin_cuentas")
})

test_that("conteo_niveles cuenta por dimension las metricas sin medir", {
  skip_on_cran()
  nuc <- metricas_nucleo()
  d <- data.frame(t = c(NA_character_, NA, NA), x = c(1, 2, 3))
  mr <- medir(modelo(instanciar(especializar(nuc$Formato, "Rx", expresion_regular = "^a$"), "d", "t"),
                     instanciar(especializar(nuc$NoNulo, "NN"), "d", "x")),
              list(d = d), id_medicion = "s", fecha = .sc_fecha, detalle = "resumen")
  s <- semaforo_calidad(mr, cortes = list(Completitud = c(0.5, 0.9)))
  conteo <- attr(s, "conteo_niveles")
  expect_identical(conteo$sin_medir[conteo$dimension == "Exactitud"], 1L)
  expect_identical(conteo$verde[conteo$dimension == "Completitud"], 1L)
})

test_that("la impresion no evalua el texto de quien llama", {
  skip_on_cran()
  mr <- .sc_resumen()
  s <- semaforo_calidad(mr, cortes = list(Completitud = c(0.5, 0.9)),
                        niveles = c("{rojo}", "{amarillo}", "{verde}"))
  expect_no_error(suppressMessages(utils::capture.output(print(s))))
  expect_identical(s$nivel[s$metrica_instanciada == "NN@d.n"], "{amarillo}")
})
