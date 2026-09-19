# El veredicto de una fila lo decidian las OTRAS filas de la tanda. La regla
# unica que arreglo las cuatro columnas del veredicto calculaba su tolerancia
# con `max(abs(delta), abs(corte))`, y `delta` es un VECTOR: `max()` colapsa el
# grupo entero. Consecuencias medidas, las dos publicadas:
#   - un solo `delta` NA -una medicion sin evaluar- dejaba `significativo`,
#     `direccion`, `severidad` y `descripcion` en NA para TODOS los pares del
#     grupo, incluido un deterioro de 0,9 a 0,7 que es cuatro veces el umbral;
#   - la tolerancia de una fila salia del delta mas grande del grupo, asi que la
#     misma comparacion cambiaba de veredicto segun con quien viajara.
#
# La prueba no mira la aritmetica: compara la fila publicada con la fila que la
# misma comparacion publica cuando viaja sola.

.n75_armar_corridas <- function() {
  nucleo <- metricas_nucleo()
  par_a <- especializar(nucleo$NoNulo, nombre_especifico = "NNA")
  par_b <- especializar(nucleo$NoNulo, nombre_especifico = "NNB")
  modelo_dos <- function() {
    modelo(list(
      instanciar(par_a, "t", "dato"), instanciar(par_b, "t", "extra")
    ))
  }
  perfil <- perfil_evaluacion(
    "Operativo", regla_evaluacion("C", function(x) x > 0.6)
  )
  buenos <- data.frame(
    dato = c(rep("x", 9L), NA), extra = c(rep("y", 9L), NA),
    stringsAsFactors = FALSE
  )
  malos <- data.frame(
    dato = c(rep("x", 5L), rep(NA, 5L)), extra = c(rep("y", 9L), NA),
    stringsAsFactors = FALSE
  )
  corrida <- function(id, fecha, datos, inaplicable = FALSE) {
    argumentos <- list(
      modelo_dos(), datos, id_medicion = id,
      fecha = as.POSIXct(fecha, tz = "UTC")
    )
    if (inaplicable) {
      # Una sola metrica sin universo aplicable deja el resultado del perfil
      # sin evaluar. Con las dos, `medir()` rechaza la medicion entera y no
      # hay `NA` que contamine: el fixture comodo no muestra el defecto.
      argumentos$aplicabilidad <- list(extra = ~ extra == "no_existe")
    }
    suppressWarnings(evaluar(do.call(medir, argumentos), perfil))
  }
  list(
    buena = corrida("m1", "2026-01-31", buenos),
    mala = corrida("m2", "2026-02-28", malos),
    sin_evaluar = corrida("m3", "2026-03-31", malos, inaplicable = TRUE),
    mala_final = corrida("m4", "2026-04-30", malos)
  )
}

.n75_fila <- function(historico, anterior, actual) {
  deriva <- suppressWarnings(detectar_deriva_calidad(historico, umbral = 0.05))
  fila <- deriva[
    deriva$aspecto == "resultado" &
      deriva$id_medicion_anterior == anterior &
      deriva$id_medicion_actual == actual, , drop = FALSE
  ]
  fila
}

test_that("un par sin evaluar no contamina el veredicto de los otros pares", {
  c3 <- .n75_armar_corridas()

  solo <- .n75_fila(historico_calidad(c3$buena, c3$mala), "m1", "m2")
  skip_if(!nrow(solo), "la deriva no produjo el par m1->m2")

  # El mismo par, ahora acompanado por dos pares cuyo delta es NA.
  acompanado <- .n75_fila(
    historico_calidad(c3$buena, c3$mala, c3$sin_evaluar, c3$mala_final),
    "m1", "m2"
  )
  expect_equal(nrow(acompanado), 1L)

  # El par cambio de verdad y se publica como tal, viaje solo o acompanado.
  expect_true(isTRUE(solo$significativo[[1L]]))
  expect_true(isTRUE(acompanado$significativo[[1L]]))
  expect_false(is.na(acompanado$direccion[[1L]]))
  expect_false(is.na(acompanado$severidad[[1L]]))
  expect_false(is.na(acompanado$descripcion[[1L]]))

  # Y las cuatro columnas del veredicto dicen exactamente lo mismo en los dos.
  for (columna in c("delta", "cambio_absoluto", "significativo", "direccion",
                    "severidad", "descripcion")) {
    expect_identical(
      acompanado[[columna]][[1L]], solo[[columna]][[1L]],
      info = paste("columna", columna)
    )
  }
})

test_that("la fila del par sin evaluar dice que no se puede comparar", {
  # El otro lado de la misma tanda: el par que SI tiene el NA no puede publicar
  # un veredicto, y tiene que decir de que lado falta el resultado.
  c3 <- .n75_armar_corridas()
  historico <- historico_calidad(
    c3$buena, c3$mala, c3$sin_evaluar, c3$mala_final
  )
  hacia <- .n75_fila(historico, "m2", "m3")
  desde <- .n75_fila(historico, "m3", "m4")
  skip_if(!nrow(hacia) || !nrow(desde), "la deriva no produjo los pares con NA")

  expect_false(isTRUE(hacia$significativo[[1L]]))
  expect_false(isTRUE(desde$significativo[[1L]]))
  expect_match(as.character(hacia$descripcion[[1L]]), "actual no se evalu")
  expect_match(as.character(desde$descripcion[[1L]]), "anterior no se evalu")
})
