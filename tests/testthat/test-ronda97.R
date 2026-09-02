perfilar_sf_ronda97 <- function(geometria) {
  perfilar(
    sf::st_sf(id = seq_along(geometria), geometria = geometria),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
}

fila_geometria_ronda97 <- function(perfil) {
  perfil$columnas[
    perfil$columnas$columna == "geometria", , drop = FALSE
  ]
}

test_that("la BBOX se extrae por estructura y no desde texto citado", {
  wkt <- paste0(
    'PROJCRS["texto BBOX[-1,-1,1,1]",',
    "USAGE[AREA[\"prueba\"], BBOX ( -80, -60, 0, -54 )]]"
  )
  area <- .bbox_area_uso_crs(list(wkt = wkt))

  expect_true(area$evaluada)
  expect_equal(
    area$bbox,
    c(xmin = -60, ymin = -80, xmax = -54, ymax = 0)
  )
  expect_false(area$global)
})

test_that("validez, dominio y bbox publican sus alcances distintos", {
  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(
    sf::st_linestring(rbind(c(0, 0), c(1, 1))),
    sf::st_linestring(),
    sf::st_linestring(rbind(c(2, 2), c(2, 2))),
    sf::st_linestring(rbind(c(199, 0), c(200, 1))),
    crs = 4326
  )

  perfil <- perfilar_sf_ronda97(geometria)
  fila <- fila_geometria_ronda97(perfil)
  tipos <- c(
    "geometria_invalida", "coordenada_fuera_dominio", "geometria_vacia"
  )
  hallazgos <- perfil$hallazgos[
    match(tipos, perfil$hallazgos$tipo_hallazgo), , drop = FALSE
  ]

  expect_equal(hallazgos$tipo_hallazgo, tipos)
  expect_equal(hallazgos$n_afectados, rep(1, 3L))
  expect_equal(hallazgos$n_evaluados, c(4, 3, 4))
  expect_equal(fila$n_validez_evaluados, 4L)
  expect_equal(fila$n_dominio_evaluados, 3L)
  expect_equal(fila$n_bbox_evaluados, 3L)
  expect_equal(fila$bbox_xmax, 200)
  expect_equal(
    fila$bbox_alcance,
    "coordenadas_crudas_de_geometrias_no_vacias"
  )
})

test_that("XYM y XYZM calculan validez en XY sin evaluar la medida", {
  skip_if_not_installed("sf")
  casos <- list(
    XYM = list(
      primero = rbind(c(0, 0, 5), c(1, 1, 6)),
      segundo = rbind(c(2, 2, 7), c(3, 3, 8))
    ),
    XYZM = list(
      primero = rbind(c(0, 0, 10, 5), c(1, 1, 11, 6)),
      segundo = rbind(c(2, 2, 12, 7), c(3, 3, 13, 8))
    )
  )

  for (dimension in names(casos)) {
    caso <- casos[[dimension]]
    geometria <- sf::st_sfc(
      sf::st_linestring(caso$primero, dim = dimension),
      sf::st_linestring(caso$segundo, dim = dimension),
      crs = 4326
    )
    perfil <- perfilar_sf_ronda97(geometria)
    fila <- fila_geometria_ronda97(perfil)
    cobertura <- perfil$cobertura_diagnosticos
    dimensiones <- cobertura[
      cobertura$diagnostico == "dimensiones_geometria_no_evaluadas",
      , drop = FALSE
    ]

    expect_equal(fila$n_geometrias_invalidas, 0L, info = dimension)
    expect_equal(fila$n_validez_evaluados, 2L, info = dimension)
    expect_equal(fila$validez_preprocesamiento, "st_zm(x)", info = dimension)
    expect_equal(nrow(dimensiones), 1L, info = dimension)
    expect_match(dimensiones$motivo, "st_zm\\(\\)", info = dimension)
    expect_false("validez_geometria" %in% cobertura$diagnostico,
                 info = dimension)
  }

  invalida <- sf::st_sfc(
    sf::st_linestring(rbind(c(2, 2, 7), c(2, 2, 8)), dim = "XYM"),
    crs = 4326
  )
  expect_equal(
    fila_geometria_ronda97(perfilar_sf_ronda97(invalida))$n_geometrias_invalidas,
    1L
  )
})

test_that("las dimensiones adicionales no desordenan el dominio proyectado", {
  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(
    sf::st_point(c(500000, 6200000, 10), dim = "XYZ"),
    sf::st_point(c(500100, 6200100, 7), dim = "XYM"),
    sf::st_point(c(500200, 6200200, 11, 8), dim = "XYZM"),
    sf::st_linestring(rbind(
      c(500300, 6200300, 12, 9),
      c(500400, 6200400, 13, 10)
    ), dim = "XYZM"),
    crs = 31981
  )

  medido <- lupa:::.perfilar_geometria(geometria)

  expect_identical(medido$crs_declarado, "31981")
  expect_identical(medido$dimension_geometria, "XYZ, XYM, XYZM")
  expect_identical(medido$n_geometrias_invalidas, 0L)
  expect_identical(medido$n_dominio_evaluados, 4L)
  expect_identical(medido$n_fuera_de_dominio, 0L)
})

test_that("EWKB con SRID mixto conserva el dominio de cada geometria", {
  skip_if_not_installed("sf")
  utm <- sf::st_as_binary(
    sf::st_sfc(sf::st_point(c(500000, 6200000)), crs = 31981),
    EWKB = TRUE
  )
  geografica <- sf::st_as_binary(
    sf::st_sfc(sf::st_point(c(-56.2, -34.9)), crs = 4326),
    EWKB = TRUE
  )
  columna <- structure(c(utm, geografica), class = "WKB")

  medido <- lupa:::.perfilar_geometria(columna)

  expect_identical(medido$crs_declarado, "31981, 4326")
  expect_identical(medido$n_geometrias_invalidas, 0L)
  expect_identical(medido$n_dominio_evaluados, 2L)
  expect_identical(medido$n_fuera_de_dominio, 0L)
})

test_that("el area de uso detecta unidades incompatibles y declara su limite", {
  skip_if_not_installed("sf")
  punto <- function(x, y, crs) {
    sf::st_sfc(sf::st_point(c(x, y)), crs = crs)
  }
  correcta_21 <- sf::st_transform(punto(-56.088, -34, 4326), 32721)
  zona_21_como_20 <- suppressWarnings(sf::st_set_crs(correcta_21, 32720))
  correcta_23 <- sf::st_transform(punto(-46.636, -34, 4326), 32723)
  zona_23_como_21 <- suppressWarnings(sf::st_set_crs(correcta_23, 32721))
  europa <- sf::st_transform(punto(3, 50, 4326), 32631)
  europa_como_21 <- suppressWarnings(sf::st_set_crs(europa, 32721))
  casos <- list(
    grados_como_utm_21 = punto(-56, -34, 32721),
    utm_21_correcta = correcta_21,
    utm_21_como_20 = zona_21_como_20,
    utm_23_como_21 = zona_23_como_21,
    europa_como_21 = europa_como_21
  )
  esperados <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
  longitudes <- vapply(casos, function(geometria) {
    unname(sf::st_coordinates(sf::st_transform(geometria, 4326))[[1L]])
  }, numeric(1L))

  expect_equal(
    unname(round(longitudes, 3)),
    c(-147.237, -56.088, -62.088, -58.636, -57)
  )
  for (i in seq_along(casos)) {
    perfil <- perfilar_sf_ronda97(casos[[i]])
    encontrado <- "coordenada_fuera_dominio" %in%
      perfil$hallazgos$tipo_hallazgo
    expect_identical(encontrado, esperados[[i]], info = names(casos)[[i]])
  }

  hallazgo <- perfilar_sf_ronda97(casos[[1L]])$hallazgos
  hallazgo <- hallazgo[
    hallazgo$tipo_hallazgo == "coordenada_fuera_dominio", , drop = FALSE
  ]
  expect_match(hallazgo$descripcion, "unidades")
  expect_match(hallazgo$descripcion, "no detecta.*zona UTM")
})

test_that("un area de uso global es no-op evaluado", {
  skip_if_not_installed("sf")
  perfil <- perfilar_sf_ronda97(
    sf::st_sfc(sf::st_point(c(-56, -34)), crs = 4326)
  )

  expect_false("coordenada_fuera_dominio" %in%
                 perfil$hallazgos$tipo_hallazgo)
  expect_false("dominio_geometria" %in%
                 perfil$cobertura_diagnosticos$diagnostico)
  expect_equal(fila_geometria_ronda97(perfil)$n_dominio_evaluados, 1L)
})

test_that("un WKT sin BBOX queda declarado en cobertura", {
  skip_if_not_installed("sf")
  crs_sin_bbox <- sf::st_crs(
    "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"
  )
  expect_false(grepl("BBOX", crs_sin_bbox$wkt, fixed = TRUE))
  perfil <- perfilar_sf_ronda97(
    sf::st_sfc(sf::st_point(c(500000, 6200000)), crs = crs_sin_bbox)
  )
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "dominio_geometria",
    , drop = FALSE
  ]

  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$motivo, "BBOX")
  expect_true(is.na(fila_geometria_ronda97(perfil)$n_fuera_de_dominio))
})

test_that("los campos de ronda 97 son NA sin geometria", {
  datos <- data.frame(id = 1:3, nombre = c("A", "B", "C"))
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  campos <- c(
    "n_validez_evaluados", "validez_preprocesamiento", "bbox_alcance"
  )

  expect_true(all(vapply(
    perfil$columnas[campos], function(x) all(is.na(x)), logical(1L)
  )))
})
