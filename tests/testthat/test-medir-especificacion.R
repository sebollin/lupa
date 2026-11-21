# Paso 1: especificacion unica de metricas traducibles y puerta de catalogo.

test_that("toda metrica del catalogo tiene ruta declarada en .RUTAS_CATALOGO_METRICAS", {
  skip_on_cran()
  catalogo <- c(names(metricas_nucleo()), names(metricas_referencial()))
  rutas <- lupa:::.RUTAS_CATALOGO_METRICAS
  expect_true(is.data.frame(rutas))
  expect_identical(setdiff(catalogo, rutas$metrica), character())
  expect_identical(setdiff(rutas$metrica, catalogo), character())
  expect_false(anyDuplicated(rutas$metrica) > 0L)
  sin_spec <- rutas$especificada == "no"
  expect_false(anyNA(rutas$ruta_sin_especificacion[sin_spec]))
})

test_that("las metricas traducibles producen especificacion y respetan metodos propios", {
  skip_on_cran()
  nuc <- metricas_nucleo()
  inst_nn <- instanciar(especializar(nuc$NoNulo, "NN"), "d", "x")
  spec_nn <- lupa:::.especificacion_metrica(inst_nn)
  expect_identical(spec_nn$nodo, "no_ausente")

  # Un metodo propio no usa la especificacion del paquete.
  propio <- function(tablas, instancia) {
    lupa:::.salida_metodo(TRUE, "d", "x", 1L, "d$x[1]")
  }
  inst_prop <- instanciar(especializar(nuc$NoNulo, "NNp"), "d", "x",
                          metodo = propio)
  expect_null(lupa:::.especificacion_metrica(inst_prop))

  # Formato con diccionario tiene especificacion; con regex no.
  inst_dic <- instanciar(especializar(nuc$Formato, "FD", diccionario = c("a", "b")),
                         "d", "t")
  inst_rx <- instanciar(especializar(nuc$Formato, "FR", expresion_regular = "^a"),
                        "d", "t")
  expect_identical(lupa:::.especificacion_metrica(inst_dic)$nodo, "en_conjunto")
  expect_null(lupa:::.especificacion_metrica(inst_rx))

  # Rango tiene especificacion; predicado no.
  inst_rng <- instanciar(especializar(nuc$ValoresPosiblesPorComprension, "R",
                                      minimo = 0, maximo = 10), "d", "x")
  inst_pred <- instanciar(especializar(nuc$ValoresPosiblesPorComprension, "P",
                                       predicado = function(v) v > 0), "d", "x")
  expect_identical(lupa:::.especificacion_metrica(inst_rng)$nodo, "en_rango")
  expect_null(lupa:::.especificacion_metrica(inst_pred))
})

test_that("cobertura y alcance desde conteos coinciden con las tablas en memoria", {
  skip_on_cran()
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(x = c(1, NA, 3, NA), y = c("a", "b", "a", "b"),
                  stringsAsFactors = FALSE)
  inst <- instanciar(especializar(nuc$ValoresPosiblesPorComprension, "R",
                                  minimo = 0, maximo = 5), "d", "x")
  al_tabla <- lupa:::.alcance_medidas_instancia(list(d = d), inst, 2L)
  al_conteo <- lupa:::.alcance_medidas_desde_conteos(inst, 4, 2L, 2)
  expect_identical(al_tabla, al_conteo)

  cob_tabla <- lupa:::.cobertura_metrica_no_evaluada(
    list(d = d[0, , drop = FALSE]), inst, "m1", fecha,
    tablas_originales = list(d = d)
  )
  cob_conteo <- lupa:::.cobertura_metrica_desde_conteos(
    inst, "m1", fecha, list(
      entidad_dependiente_vacia = NULL,
      entidad_presente = TRUE,
      n_recortada = 0L,
      n_original = 4L,
      aplicable_vacio = FALSE,
      sin_valores_atributos = FALSE
    )
  )
  expect_identical(cob_tabla, cob_conteo)
})
