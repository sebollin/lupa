# Dos defectos de la misma familia que el paquete ya declaro cerrada dos veces:
# una fila que se contradice a si misma, y un umbral que decide con la resta en
# coma flotante. Los encontro una vuelta corrida por otra familia de modelo
# sobre un terreno que la anterior habia dado por limpio.

test_that("la deriva nombra de que lado esta el resultado sin evaluar", {
  # La descripcion miraba SOLO si el delta era NA, no de que lado venia, asi
  # que culpaba siempre al resultado anterior. Con el NA del lado actual, la
  # fila publicaba `resultado_anterior = 0,8` junto a "el resultado anterior no
  # se evaluo". Paso los controles porque la deriva ordena por fecha y el
  # espejo del par si salia bien: hay que probar LOS DOS sentidos.
  nucleo <- metricas_nucleo()
  par_a <- especializar(nucleo$NoNulo, nombre_especifico = "NNA")
  par_b <- especializar(nucleo$NoNulo, nombre_especifico = "NNB")
  modelo_dos <- function() {
    modelo(list(instanciar(par_a, "t", "dato"), instanciar(par_b, "t", "extra")))
  }
  datos <- data.frame(
    dato = c(rep("x", 7L), rep(NA, 3L)),
    extra = c(rep("y", 9L), NA),
    stringsAsFactors = FALSE
  )
  perfil <- perfil_evaluacion(
    "Operativo", regla_evaluacion("C", function(x) x > 0.6)
  )

  # La corrida con universo aplicable vacio deja la regla sin evaluar.
  sin_evaluar <- suppressWarnings(evaluar(medir(
    modelo_dos(), datos, id_medicion = "sin_evaluar",
    fecha = as.POSIXct("2026-02-28", tz = "UTC"),
    aplicabilidad = list(extra = ~ extra == "no_existe")
  ), perfil))
  evaluada <- suppressWarnings(evaluar(medir(
    modelo_dos(), datos, id_medicion = "evaluada",
    fecha = as.POSIXct("2026-01-31", tz = "UTC")
  ), perfil))

  descripcion_de <- function(deriva) {
    fila <- deriva[deriva$aspecto == "resultado", , drop = FALSE]
    skip_if(!nrow(fila), "la deriva no produjo fila de resultado")
    as.character(fila$descripcion[[1L]])
  }

  # Anterior evaluada, actual sin evaluar: el NA es del lado ACTUAL.
  hacia_na <- suppressWarnings(detectar_deriva_calidad(
    historico_calidad(evaluada, sin_evaluar)
  ))
  expect_match(descripcion_de(hacia_na), "actual no se evalu")

  # Y el espejo, que es el unico que la version anterior acertaba.
  desde_na <- suppressWarnings(detectar_deriva_calidad(historico_calidad(
    suppressWarnings(evaluar(medir(
      modelo_dos(), datos, id_medicion = "sin_evaluar_antes",
      fecha = as.POSIXct("2026-01-31", tz = "UTC"),
      aplicabilidad = list(extra = ~ extra == "no_existe")
    ), perfil)),
    suppressWarnings(evaluar(medir(
      modelo_dos(), datos, id_medicion = "evaluada_despues",
      fecha = as.POSIXct("2026-02-28", tz = "UTC")
    ), perfil))
  )))
  expect_match(descripcion_de(desde_na), "anterior no se evalu")
})

test_that("el umbral de comparar_perfiles se compara con tolerancia", {
  # Sin tolerancia, la resta en coma flotante decide el veredicto: dos pares
  # que publican el mismo `delta = 0,05` recibian veredictos distintos.
  # `detectar_deriva_calidad()` ya lo comparaba con tolerancia, y su propio
  # comentario dice que hacerlo en un archivo y no en el otro no es una
  # decision sino una inconsistencia.
  #
  # La primera version de esta prueba cerraba buscando la palabra "tolerancia"
  # en el CUERPO de `comparar_perfiles()`. Eso afirma sobre la implementacion y
  # no sobre la conducta: al extraer la regla a un auxiliar -que es una mejora-
  # la prueba se puso roja sin que nada se hubiera roto. Se cierra midiendo.
  alcanza <- getFromNamespace(".alcanza_umbral_deriva", "lupa")
  umbral <- 0.05
  pares <- list(c(0.65, 0.70), c(0.70, 0.75), c(0.10, 0.15), c(0.20, 0.25))

  crudos <- vapply(pares, function(p) (p[[2L]] - p[[1L]]) >= umbral, logical(1L))
  # El caso existe: comparando a secas, los cuatro NO coinciden.
  expect_false(all(crudos) || !any(crudos))

  # Y por la via del paquete, los cuatro dan lo mismo: publican la misma
  # magnitud y reciben el mismo veredicto.
  del_paquete <- vapply(pares, function(p) {
    alcanza(p[[2L]] - p[[1L]], umbral)
  }, logical(1L))
  expect_true(all(del_paquete))
})
