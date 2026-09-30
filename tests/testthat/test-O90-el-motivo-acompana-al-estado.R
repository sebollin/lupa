# El motivo publicado no puede contradecir al estado que acompana.

test_that("un marco propio que el perfil no mide no recibe el motivo de medido", {
  # Los dos motivos que precisan que examino el perfil se asignaban por clave, sin
  # mirar el estado: un marco propio con `Densidad` y `perfil_mide = FALSE`
  # publicaba `no_declarada` al lado de "el perfil conto ausentes reales y
  # disfrazados". En `marco_agesic()` no se ve porque esos pares traen
  # `perfil_mide = TRUE`.
  factores <- marco_agesic()$factores
  factores <- factores[factores$factor %in% c("Densidad", "No-duplicaci\u00f3n"),
                       c("dimension", "factor")]
  expect_equal(nrow(factores), 2L)
  perfil <- suppressWarnings(perfilar(
    data.frame(a = c(1, NA, 3), b = c("x", "y", "x"))
  ))
  propia <- as.data.frame(cobertura_analisis(
    perfil, modelo = marco_calidad("Propio", factores)
  ))
  expect_true(all(propia$estado == "no_declarada"))
  expect_false(any(grepl("El perfil cont", propia$motivo, fixed = TRUE)))
  expect_false(any(grepl("El perfil examin", propia$motivo, fixed = TRUE)))

  # El control: con `perfil_mide = TRUE` los dos motivos especificos siguen.
  factores$perfil_mide <- TRUE
  medida <- as.data.frame(cobertura_analisis(
    perfil, modelo = marco_calidad("Propio", factores)
  ))
  expect_true(all(medida$estado == "medida"))
  expect_true(any(grepl("El perfil cont", medida$motivo, fixed = TRUE)))
  expect_true(any(grepl("El perfil examin", medida$motivo, fixed = TRUE)))
})

test_that("'dependiente' se dice solo de la segunda entidad de una metrica entre dos", {
  # `NoNulo` sobre una tabla vacia publicaba "la entidad dependiente `t`": la
  # palabra es de `ReglaIntegridadInterEntidad`, y se buscaba entre todas las
  # entidades ligadas, la principal incluida.
  nucleo <- metricas_nucleo()
  sola <- medir(modelo(instanciar(especializar(nucleo$NoNulo), "t", "x")),
                data.frame(x = numeric()))
  motivo <- attr(sola, "cobertura_metricas", exact = TRUE)$motivo
  expect_false(grepl("dependiente", motivo, fixed = TRUE))
  expect_match(motivo, "alcance vac", fixed = TRUE)

  # El control: con la referencia llena y la dependiente vacia, la palabra
  # corresponde y se conserva.
  inter <- instanciar(
    especializar(nucleo$ReglaIntegridadInterEntidad),
    c("ref", "dep"), c("id", "id_ref")
  )
  entre_dos <- medir(
    modelo(inter),
    list(ref = data.frame(id = 1:3), dep = data.frame(id_ref = integer()))
  )
  motivo <- attr(entre_dos, "cobertura_metricas", exact = TRUE)$motivo
  expect_match(motivo, "la entidad dependiente `dep`", fixed = TRUE)
})
