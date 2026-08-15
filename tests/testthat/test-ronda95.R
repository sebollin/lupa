perfilar_sf_ronda95 <- function(geometria) {
  perfilar(
    sf::st_sf(id = seq_along(geometria), geometria = geometria),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
}

fila_geometria_ronda95 <- function(perfil) {
  perfil$columnas[
    perfil$columnas$columna == "geometria", , drop = FALSE
  ]
}

test_that("una geometria vacia no esta fuera del dominio geografico", {
  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(
    sf::st_point(c(-56, -34)), sf::st_point(), crs = 4326
  )

  perfil <- perfilar_sf_ronda95(geometria)
  fila <- fila_geometria_ronda95(perfil)

  expect_equal(fila$n_geometrias_vacias, 1L)
  expect_equal(fila$n_fuera_de_dominio, 0L)
  expect_false("coordenada_fuera_dominio" %in%
                 perfil$hallazgos$tipo_hallazgo)
})

test_that("el alcance geografico excluye vacias y conserva violaciones reales", {
  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(
    sf::st_point(c(-56, -34)), sf::st_point(), sf::st_point(c(200, 999)),
    crs = 4326
  )

  perfil <- perfilar_sf_ronda95(geometria)
  fila <- fila_geometria_ronda95(perfil)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "coordenada_fuera_dominio",
    , drop = FALSE
  ]

  expect_equal(fila$n_geometrias_vacias, 1L)
  expect_equal(fila$n_fuera_de_dominio, 1L)
  expect_equal(hallazgo$n_evaluados, 2)
  expect_equal(hallazgo$n_afectados, 1)
  expect_equal(hallazgo$trazabilidad[[1L]]$indices_fila, 3L)
})

test_that("un dominio disponible sobre solo vacias tiene universo cero", {
  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(sf::st_point(), sf::st_point(), crs = 4326)

  metricas <- lupa:::.perfilar_geometria(geometria)
  perfil <- perfilar_sf_ronda95(geometria)
  fila <- fila_geometria_ronda95(perfil)

  expect_true(metricas$dominio_evaluado)
  expect_equal(metricas$n_dominio_evaluados, 0L)
  expect_equal(fila$n_geometrias_vacias, 2L)
  expect_equal(fila$n_geometrias_invalidas, 0L)
  expect_equal(fila$n_fuera_de_dominio, 0L)
  expect_false("coordenada_fuera_dominio" %in%
                 perfil$hallazgos$tipo_hallazgo)
  expect_true(all(is.na(unlist(fila[c(
    "bbox_xmin", "bbox_xmax", "bbox_ymin", "bbox_ymax"
  )]))))
})

test_that("el dominio proyectado usa el mismo universo no vacio", {
  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(
    sf::st_point(c(500000, 6200000)), sf::st_point(),
    sf::st_point(c(1e16, 1e16)), crs = 32721
  )

  perfil <- perfilar_sf_ronda95(geometria)
  fila <- fila_geometria_ronda95(perfil)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "coordenada_fuera_dominio",
    , drop = FALSE
  ]

  expect_equal(fila$n_geometrias_vacias, 1L)
  expect_equal(fila$n_fuera_de_dominio, 1L)
  expect_equal(hallazgo$n_evaluados, 2)
  expect_equal(hallazgo$n_afectados, 1)
  expect_equal(hallazgo$trazabilidad[[1L]]$indices_fila, 3L)

  solo_vacias <- sf::st_sfc(sf::st_point(), sf::st_point(), crs = 32721)
  metricas_vacias <- lupa:::.perfilar_geometria(solo_vacias)
  expect_true(metricas_vacias$dominio_evaluado)
  expect_equal(metricas_vacias$n_dominio_evaluados, 0L)
  expect_equal(metricas_vacias$n_fuera_de_dominio, 0L)
})

test_that("validez y bbox conservan la semantica de sf ante vacias", {
  skip_if_not_installed("sf")
  autointerseccion <- rbind(
    c(0, 0), c(1, 1), c(1, 0), c(0, 1), c(0, 0)
  )
  geometria <- sf::st_sfc(
    sf::st_polygon(list(autointerseccion)), sf::st_polygon(), crs = 4326
  )

  perfil <- perfilar_sf_ronda95(geometria)
  fila <- fila_geometria_ronda95(perfil)

  expect_equal(fila$n_geometrias_vacias, 1L)
  expect_equal(fila$n_geometrias_invalidas, 1L)
  expect_equal(
    unname(unlist(fila[c(
      "bbox_xmin", "bbox_xmax", "bbox_ymin", "bbox_ymax"
    )])),
    c(0, 1, 0, 1)
  )
  expect_equal(
    perfil$hallazgos$trazabilidad[[
      match("geometria_invalida", perfil$hallazgos$tipo_hallazgo)
    ]]$indices_fila,
    1L
  )
})
