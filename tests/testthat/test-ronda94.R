test_that("las coordenadas imposibles se miden contra el CRS declarado", {
  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(
    sf::st_point(c(-56.2, -34.9)),
    sf::st_point(c(-56.1, -34.8)),
    sf::st_point(c(-56.1, -34.8)),
    sf::st_point(c(200, 999)),
    crs = 4326
  )
  perfil <- perfilar(
    sf::st_sf(id = seq_along(geometria), geometria = geometria),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas[perfil$columnas$columna == "geometria", , drop = FALSE]
  nuevos <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo %in% c(
      "crs_no_declarado", "geometria_invalida", "geometria_vacia",
      "coordenada_fuera_dominio", "tipos_geometria_mixtos"
    ), , drop = FALSE
  ]

  expect_equal(fila$crs_declarado, "4326")
  expect_equal(fila$tipo_geometria, "POINT")
  expect_equal(fila$n_geometrias_vacias, 0L)
  expect_equal(fila$n_geometrias_invalidas, 0L)
  expect_equal(fila$n_fuera_de_dominio, 1L)
  expect_equal(
    unname(unlist(fila[c("bbox_xmin", "bbox_xmax", "bbox_ymin", "bbox_ymax")])),
    c(-56.2, 200, -34.9, 999)
  )
  expect_equal(nuevos$tipo_hallazgo, "coordenada_fuera_dominio")
  expect_equal(as.character(nuevos$severidad), "error")
  expect_equal(nuevos$n_evaluados, 4)
  expect_equal(nuevos$n_afectados, 1)
  expect_equal(nuevos$unidad_conteo, "geometria")
  expect_equal(nuevos$trazabilidad[[1L]]$indices_fila, 4L)
  expect_equal(nrow(perfil$cobertura_diagnosticos), 0L)
})

test_that("sin CRS no se inventa un dominio", {
  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(
    sf::st_point(c(-56.2, -34.9)), sf::st_point(c(200, 999))
  )
  perfil <- perfilar(
    sf::st_sf(id = seq_along(geometria), geometria = geometria),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas[perfil$columnas$columna == "geometria", , drop = FALSE]

  expect_true(is.na(fila$crs_declarado))
  expect_true(is.na(fila$n_fuera_de_dominio))
  expect_equal(fila$n_geometrias_invalidas, 0L)
  expect_true("crs_no_declarado" %in% perfil$hallazgos$tipo_hallazgo)
  expect_false("coordenada_fuera_dominio" %in% perfil$hallazgos$tipo_hallazgo)
  expect_equal(nrow(perfil$cobertura_diagnosticos), 0L)
})

test_that("se declaran invalidez, vacios y tipos geometricos mixtos", {
  skip_if_not_installed("sf")
  autointerseccion <- rbind(
    c(0, 0), c(1, 1), c(1, 0), c(0, 1), c(0, 0)
  )
  invalida <- sf::st_sfc(sf::st_polygon(list(autointerseccion)), crs = 4326)
  perfil_invalida <- perfilar(
    sf::st_sf(id = 1L, geometria = invalida),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  expect_equal(perfil_invalida$columnas$n_geometrias_invalidas[[2L]], 1L)
  expect_true("geometria_invalida" %in%
                perfil_invalida$hallazgos$tipo_hallazgo)

  mixta <- sf::st_sfc(
    sf::st_point(c(0, 0)), sf::st_polygon(),
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(0, 1), c(0, 0)))),
    crs = 4326
  )
  perfil_mixta <- perfilar(
    sf::st_sf(id = seq_along(mixta), geometria = mixta),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  fila <- perfil_mixta$columnas[
    perfil_mixta$columnas$columna == "geometria", , drop = FALSE
  ]
  tipos <- perfil_mixta$hallazgos$tipo_hallazgo

  expect_equal(fila$tipo_geometria, "POINT, POLYGON")
  expect_equal(fila$n_geometrias_vacias, 1L)
  expect_true(all(c("geometria_vacia", "tipos_geometria_mixtos") %in% tipos))
  expect_equal(
    as.character(perfil_mixta$hallazgos$severidad[
      match(c("geometria_vacia", "tipos_geometria_mixtos"), tipos)
    ]),
    rep("sospechoso", 2L)
  )
})

test_that("sin sf la geometria queda en cobertura y no en hallazgos", {
  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(
    sf::st_point(c(-56.2, -34.9)), sf::st_point(c(-56.1, -34.8)),
    crs = 4326
  )
  local_mocked_bindings(
    .sf_disponible = function() FALSE,
    .package = "lupa"
  )
  perfil <- perfilar(
    sf::st_sf(id = seq_along(geometria), geometria = geometria),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas[perfil$columnas$columna == "geometria", , drop = FALSE]
  campos <- c(
    "crs_declarado", "tipo_geometria", "n_geometrias_vacias",
    "n_geometrias_invalidas", "n_fuera_de_dominio",
    "bbox_xmin", "bbox_xmax", "bbox_ymin", "bbox_ymax"
  )

  expect_true(all(vapply(fila[campos], function(x) is.na(x[[1L]]), logical(1L))))
  expect_equal(nrow(perfil$hallazgos), 0L)
  expect_equal(nrow(perfil$cobertura_diagnosticos), 1L)
  expect_equal(perfil$cobertura_diagnosticos$diagnostico, "perfil_geometria")
  expect_equal(perfil$cobertura_diagnosticos$dependencia, "sf")
})

test_that("una tabla sin geometria conserva NA sin ruido", {
  datos <- data.frame(id = 1:3, nombre = c("A", "B", "C"))
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  campos <- c(
    "crs_declarado", "tipo_geometria", "n_geometrias_vacias",
    "n_geometrias_invalidas", "n_fuera_de_dominio",
    "bbox_xmin", "bbox_xmax", "bbox_ymin", "bbox_ymax"
  )

  expect_true(all(vapply(perfil$columnas[campos], function(x) all(is.na(x)),
                         logical(1L))))
  expect_equal(nrow(perfil$cobertura_diagnosticos), 0L)
  expect_false(any(perfil$hallazgos$tipo_hallazgo %in% c(
    "crs_no_declarado", "geometria_invalida", "geometria_vacia",
    "coordenada_fuera_dominio", "tipos_geometria_mixtos"
  )))
})
