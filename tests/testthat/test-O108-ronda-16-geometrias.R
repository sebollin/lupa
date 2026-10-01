# Ronda 16-B: geometrias con aplicabilidad, y geometria escrita como texto.

test_that("la aplicabilidad sobre una sfc no aborta ni cuenta las excluidas", {
  skip_if_not_installed("sf")
  x <- sf::st_sf(
    id = 1:4,
    geom = sf::st_sfc(
      sf::st_point(c(200, 0)), sf::st_point(c(2, 2)), sf::st_point(c(2, 2)),
      sf::st_point(c(3, 3)), crs = 4326
    ),
    zona = c("B", "A", "A", "A")
  )
  # Antes: la primera fila afuera abortaba ("object(s) should be of class
  # 'sfg'").
  p <- suppressWarnings(perfilar(x, aplicabilidad = list(geom = ~ zona == "A")))
  fila <- p$columnas[p$columnas$columna == "geom", ]
  expect_equal(fila$n_distintos, 2L)
  expect_equal(fila$tasa_distintos, 2 / 3)
  expect_equal(fila$n_fuera_de_dominio, 0L)
  # Antes: con filas excluidas que no son la primera, las excluidas se volvian
  # `POINT EMPTY` y contaban como un valor: tasa 1.5 sobre dos geometrias.
  y <- x
  y$zona <- c("A", "B", "B", "A")
  p <- suppressWarnings(perfilar(y, aplicabilidad = list(geom = ~ zona == "A")))
  fila <- p$columnas[p$columnas$columna == "geom", ]
  expect_equal(fila$n_distintos, 2L)
  expect_lte(fila$tasa_distintos, 1)
  expect_no_error(suppressWarnings(
    analizar(x, aplicabilidad = list(geom = ~ zona == "A"))
  ))
  # Control: sin regla, las cuatro.
  fila <- suppressWarnings(perfilar(x))$columnas
  fila <- fila[fila$columna == "geom", ]
  expect_equal(fila$n_distintos, 3L)
  expect_equal(fila$n_fuera_de_dominio, 1L)
})

test_that("una geometria escrita como WKT no recibe diagnosticos de palabras", {
  p <- suppressWarnings(perfilar(
    data.frame(wkt = c("POINT (0 0)", "POINT (1 1)", "POINT (200 0)"))
  ))
  # Antes: `casi_duplicados_vocabulario` sobre tres puntos.
  expect_false(any(p$hallazgos$tipo_hallazgo %in% c(
    "casi_duplicados_vocabulario", "posible_identificador", "patron_raro"
  )))
  # Control: un texto comun sigue recibiendo el diagnostico de vocabulario.
  texto <- suppressWarnings(perfilar(data.frame(
    zona = c(rep("Norte", 30), rep("norte", 2), rep("Sur", 30))
  )))
  expect_true("casi_duplicados_vocabulario" %in% texto$hallazgos$tipo_hallazgo)
})

test_that("el retiro de los diagnosticos de palabras se declara", {
  # Sin `stringdist` la proximidad no se evalua y se declara por esa causa; sin
  # `sf` no hay hallazgos geometricos. Lo documentado en los dos casos, asi que
  # esta mitad necesita los dos paquetes.
  skip_if_not_installed("sf")
  skip_if_not_installed("stringdist")
  p <- suppressWarnings(perfilar(
    data.frame(wkt = c("POINT (0 0)", "POINT (1 1)", "POINT (200 0)"))
  ))
  expect_true("crs_no_declarado" %in% p$hallazgos$tipo_hallazgo)
  cobertura <- p$cobertura_diagnosticos
  declarada <- cobertura[cobertura$columna == "wkt" &
                           cobertura$diagnostico == "proximidad_vocabulario", ]
  expect_equal(nrow(declarada), 1L)
  expect_match(declarada$motivo, "geometria escrita como texto", fixed = TRUE)
})

test_that("una columna WKT con un valor corrupto se reconoce y declara la perdida", {
  p <- suppressWarnings(perfilar(data.frame(
    wkt = c("POINT (0 0)", "POINT (1 1)", "POINT (200 0)", "esto no es wkt")
  )))
  # Antes: no se reconocia y `cobertura_analisis()` decia que no habia
  # geometria.
  expect_identical(p$columnas$representacion_geometria, "WKT")
  expect_false(is.na(p$columnas$motivo_representacion))
  cobertura <- suppressWarnings(cobertura_analisis(p))
  expect_false(any(grepl("No se identificaron columnas de geometr",
                         cobertura$motivo, fixed = TRUE)))
  # Control: sin mayoria de WKT, no es geometria.
  q <- suppressWarnings(perfilar(data.frame(wkt = c("POINT (0 0)", "hola"))))
  expect_true(is.na(q$columnas$representacion_geometria))
})
