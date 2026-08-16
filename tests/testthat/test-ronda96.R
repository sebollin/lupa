perfilar_sf_ronda96 <- function(geometria) {
  perfilar(
    sf::st_sf(id = seq_along(geometria), geometria = geometria),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
}

fila_geometria_ronda96 <- function(perfil) {
  perfil$columnas[
    perfil$columnas$columna == "geometria", , drop = FALSE
  ]
}

test_that("los tipos geometricos mixtos se comparan por familia", {
  skip_if_not_installed("sf")
  triangulo <- rbind(c(0, 0), c(1, 0), c(0, 1), c(0, 0))
  casos <- list(
    list(
      geometria = sf::st_sfc(
        sf::st_point(c(0, 0)),
        sf::st_multipoint(rbind(c(1, 1), c(2, 2))), crs = 4326
      ),
      tipos = "POINT, MULTIPOINT", hallazgo = FALSE
    ),
    list(
      geometria = sf::st_sfc(
        sf::st_polygon(list(triangulo)),
        sf::st_multipolygon(list(list(triangulo))), crs = 4326
      ),
      tipos = "POLYGON, MULTIPOLYGON", hallazgo = FALSE
    ),
    list(
      geometria = sf::st_sfc(
        sf::st_linestring(rbind(c(0, 0), c(1, 1))),
        sf::st_multilinestring(list(rbind(c(2, 2), c(3, 3)))), crs = 4326
      ),
      tipos = "LINESTRING, MULTILINESTRING", hallazgo = FALSE
    ),
    list(
      geometria = sf::st_sfc(
        sf::st_point(c(0, 0)), sf::st_polygon(list(triangulo)), crs = 4326
      ),
      tipos = "POINT, POLYGON", hallazgo = TRUE
    )
  )

  for (caso in casos) {
    perfil <- perfilar_sf_ronda96(caso$geometria)
    fila <- fila_geometria_ronda96(perfil)
    expect_equal(fila$tipo_geometria, caso$tipos)
    expect_identical(
      "tipos_geometria_mixtos" %in% perfil$hallazgos$tipo_hallazgo,
      caso$hallazgo
    )
  }

  coleccion <- sf::st_sfc(
    sf::st_geometrycollection(list(sf::st_point(c(0, 0)))), crs = 4326
  )
  perfil_coleccion <- perfilar_sf_ronda96(coleccion)
  expect_true("tipos_geometria_mixtos" %in%
                perfil_coleccion$hallazgos$tipo_hallazgo)
})

test_that("la validez geografica declara el criterio planar y no afirma error", {
  skip_if_not_installed("sf")
  anillo_polar <- sf::st_as_sfc(
    "POLYGON((-135 80, -45 80, 45 80, 135 80, -135 80))",
    crs = 4326
  )

  perfil <- perfilar_sf_ronda96(anillo_polar)
  fila <- fila_geometria_ronda96(perfil)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "geometria_invalida", , drop = FALSE
  ]

  expect_equal(fila$n_geometrias_invalidas, 1L)
  expect_equal(fila$validez_criterio, "planar")
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(as.character(hallazgo$severidad), "sospechoso")
  expect_match(hallazgo$descripcion, "planar")
  expect_match(hallazgo$evidencia, "esferica")

  autointerseccion <- rbind(
    c(500000, 6200000), c(500100, 6200100), c(500100, 6200000),
    c(500000, 6200100), c(500000, 6200000)
  )
  proyectada <- sf::st_sfc(
    sf::st_polygon(list(autointerseccion)), crs = 32721
  )
  hallazgo_proyectado <- perfilar_sf_ronda96(proyectada)$hallazgos
  hallazgo_proyectado <- hallazgo_proyectado[
    hallazgo_proyectado$tipo_hallazgo == "geometria_invalida", , drop = FALSE
  ]
  expect_equal(as.character(hallazgo_proyectado$severidad), "error")
})

test_that("el conteo de dominio publica su universo no vacio", {
  skip_if_not_installed("sf")
  geometria <- do.call(sf::st_sfc, c(
    rep(list(sf::st_point()), 99L),
    list(sf::st_point(c(-56.2, -34.9)), crs = 4326)
  ))

  fila <- fila_geometria_ronda96(perfilar_sf_ronda96(geometria))

  expect_equal(fila$n_geometrias_vacias, 99L)
  expect_equal(fila$n_fuera_de_dominio, 0L)
  expect_equal(fila$n_dominio_evaluados, 1L)
  expect_equal(fila$n_bbox_evaluados, 1L)
  expect_equal(
    match("n_dominio_evaluados", names(fila)),
    match("n_fuera_de_dominio", names(fila)) + 1L
  )
})

test_that("las dimensiones Z y M se declaran aunque no se evaluen", {
  skip_if_not_installed("sf")
  casos <- list(
    list(
      geometria = sf::st_sfc(
        sf::st_point(c(-56.2, -34.9, 1e300), dim = "XYZ"), crs = 4326
      ),
      dimension = "XYZ", omitida = "Z"
    ),
    list(
      geometria = sf::st_sfc(
        sf::st_point(c(-56.2, -34.9, 7), dim = "XYM"), crs = 4326
      ),
      dimension = "XYM", omitida = "M"
    )
  )

  for (caso in casos) {
    perfil <- perfilar_sf_ronda96(caso$geometria)
    fila <- fila_geometria_ronda96(perfil)
    cobertura <- perfil$cobertura_diagnosticos

    expect_equal(fila$dimension_geometria, caso$dimension)
    expect_equal(fila$dimensiones_no_evaluadas, caso$omitida)
    expect_true("dimensiones_geometria_no_evaluadas" %in%
                  cobertura$diagnostico)
    motivo <- cobertura$motivo[
      cobertura$diagnostico == "dimensiones_geometria_no_evaluadas"
    ]
    expect_match(motivo, caso$omitida)
  }
})

test_that("los nuevos campos geometricos son NA fuera de columnas sfc", {
  datos <- data.frame(id = 1:3, nombre = c("A", "B", "C"))
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  campos <- c(
    "dimension_geometria", "dimensiones_no_evaluadas",
    "validez_criterio", "n_dominio_evaluados", "n_bbox_evaluados"
  )

  expect_true(all(vapply(
    perfil$columnas[campos], function(x) all(is.na(x)), logical(1L)
  )))
})
