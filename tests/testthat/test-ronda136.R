# Una corrida contra una base PostGIS real emitio
# `coordenada_fuera_dominio en geom: traza no disponible`, y no se pudo reproducir
# con ocho fixtures distintos entre tres revisiones independientes. El dato que lo
# cerro no salio de adivinar: salio de preguntar cual de las seis formas del aviso
# habia sido, y sobre que tipo de hallazgo.
#
# La causa: la rama que devuelve los indices de un hallazgo de geometria estaba
# guardada por `inherits(x, "sfc")`. Por DBI las geometrias llegan como texto WKT
# o como blob WKB, no como `sfc`, asi que el `switch` no corria y la traza salia
# `no_disponible` -avisando de una incoherencia que no existia, porque los indices
# ya estaban calculados en `resultado$geometria`-.
#
# La condicion correcta es que el analisis haya dejado sus indices, no de que
# clase es la columna: son posiciones de fila y valen igual en las tres formas.

test_that("los indices de geometria se trazan venga como venga la columna", {
  skip_if_not_installed("sf")
  geometrias <- sf::st_sfc(
    list(sf::st_point(c(500, 200)), sf::st_point(c(-56, -34))), crs = 4326L
  )
  # La primera esta fuera del dominio; el analisis ya dejo su indice.
  resultado <- list(geometria = list(
    indices_fuera_de_dominio = 1L,
    indices_invalidas = integer(),
    indices_vacias = integer()
  ))

  formas <- list(
    sfc = geometrias,
    wkt = sf::st_as_text(geometrias),
    wkb = sf::st_as_binary(geometrias)
  )
  for (nombre in names(formas)) {
    indices <- lupa:::.indices_hallazgo_columna(
      "coordenada_fuera_dominio", formas[[nombre]], NA, resultado
    )
    expect_equal(indices, 1L, info = paste("forma", nombre))
  }
})

test_that("una columna sin analisis de geometria no inventa una traza", {
  # El control que hace valer la prueba de arriba: si la guarda nueva devolviera
  # indices para cualquier columna, habria cambiado un descarte por una invencion,
  # que es peor.
  expect_null(lupa:::.indices_hallazgo_columna(
    "coordenada_fuera_dominio", letters[1:3], NA, list()
  ))
  expect_null(lupa:::.indices_hallazgo_columna(
    "coordenada_fuera_dominio", 1:3, NA, list(geometria = NULL)
  ))
})

test_that("los otros hallazgos de geometria se trazan igual en las tres formas", {
  skip_if_not_installed("sf")
  geometrias <- sf::st_sfc(
    list(sf::st_point(c(0, 0)), sf::st_point(c(1, 1))), crs = 4326L
  )
  resultado <- list(geometria = list(
    indices_fuera_de_dominio = integer(),
    indices_invalidas = 2L,
    indices_vacias = 1L
  ))
  casos <- list(
    geometria_invalida = 2L,
    geometria_vacia = 1L
  )
  for (tipo in names(casos)) {
    for (columna in list(geometrias, sf::st_as_text(geometrias))) {
      expect_equal(
        lupa:::.indices_hallazgo_columna(tipo, columna, NA, resultado),
        casos[[tipo]],
        info = tipo
      )
    }
  }
})
