# El conteo y la traza de un hallazgo tienen que describir el MISMO conjunto de filas.
#
# Con `aplicabilidad` declarada no lo hacian, y de las dos formas posibles: el conteo
# contaba fuera del universo mientras la traza recortaba -geometrias- y la traza
# nombraba filas que el conteo no contaba -mayusculas-. Las dos veces `perfilar()`
# emitia su propio aviso `lupa_trazabilidad_incoherente` sobre un perfil INTACTO,
# cuando hay una prueba que declara que un perfil intacto no lo emite.

avisos_de <- function(expr) {
  recogidos <- character()
  valor <- withCallingHandlers(
    expr,
    warning = function(w) {
      recogidos <<- c(recogidos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(valor = valor, avisos = unique(recogidos))
}

fila_de <- function(perfil, tipo) {
  todas <- hallazgos(perfil)
  todas[todas$tipo_hallazgo == tipo, , drop = FALSE]
}

filas_trazadas <- function(fila) {
  traza <- fila$trazabilidad[[1L]]
  if (is.list(traza) && !is.null(traza$indices_fila)) {
    as.integer(traza$indices_fila)
  } else {
    integer()
  }
}

test_that("con universo declarado, la traza de mayusculas no nombra de mas", {
  # `c` en una fila aplicable colisiona con `C` en una fila que NO lo es: el conteo
  # decia 2 valores distintos y citaba `"A"; "a"`, y la traza nombraba las filas 1, 2
  # y 3. Los grupos se formaban sobre la columna cruda y se recortaban despues.
  datos <- data.frame(
    texto = c("A", "a", "c", "c", "C"),
    regla = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  corrida <- avisos_de(perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(texto = ~ regla == TRUE)
  ))
  fila <- fila_de(corrida$valor, "mayusculas_inconsistentes")

  expect_identical(nrow(fila), 1L)
  expect_identical(as.numeric(fila$n_afectados), 2)
  expect_identical(filas_trazadas(fila), c(1L, 2L))
  # Y el paquete no se acusa a si mismo.
  expect_false(any(grepl("trazabilidad", corrida$avisos, fixed = TRUE)))
})

test_that("sin universo declarado, las mayusculas trazan como siempre", {
  # Mitad de control: la misma tabla sin `aplicabilidad`. Ahi `c` y `C` colisionan de
  # verdad y las cinco filas pertenecen al universo.
  datos <- data.frame(
    texto = c("A", "a", "c", "c", "C"), stringsAsFactors = FALSE
  )

  corrida <- avisos_de(perfilar(datos, analizar_dependencias = FALSE))
  fila <- fila_de(corrida$valor, "mayusculas_inconsistentes")

  expect_identical(as.numeric(fila$n_afectados), 4)
  expect_identical(filas_trazadas(fila), 1:5)
  expect_false(any(grepl("trazabilidad", corrida$avisos, fixed = TRUE)))
})

test_that("con universo declarado, la geometria no cuenta fuera de el", {
  skip_if_not_installed("sf")
  # Tres geometrias dentro del area de uso, una fuera y aplicable, dos fuera y NO
  # aplicables. Dentro del universo hay UNA fuera de dominio; el hallazgo contaba 3 y
  # la traza nombraba 1.
  puntos <- sf::st_sfc(
    sf::st_point(c(400000, 6000000)), sf::st_point(c(400000, 6000001)),
    sf::st_point(c(400000, 6000002)), sf::st_point(c(10, 10)),
    sf::st_point(c(10, 11)), sf::st_point(c(10, 12)),
    crs = 32721
  )
  datos <- data.frame(
    geom = puntos, regla = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  names(datos)[[1L]] <- "geom"

  corrida <- avisos_de(perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(geom = ~ regla == TRUE)
  ))
  fila <- fila_de(corrida$valor, "coordenada_fuera_dominio")

  expect_identical(nrow(fila), 1L)
  expect_identical(as.numeric(fila$n_afectados), 1)
  expect_identical(as.numeric(fila$n_evaluados), 4)
  expect_identical(filas_trazadas(fila), 4L)
  expect_false(any(grepl("trazabilidad", corrida$avisos, fixed = TRUE)))
})

test_that("los tipos mixtos cuentan el universo, no la columna entera", {
  skip_if_not_installed("sf")
  puntos <- sf::st_sfc(
    sf::st_point(c(1, 1)), sf::st_point(c(2, 2)), sf::st_point(c(3, 3)),
    sf::st_point(c(4, 4)), sf::st_point(c(5, 5)),
    sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(2, 2, 3, 3), ncol = 2, byrow = TRUE)),
    crs = 4326
  )
  datos <- data.frame(
    geom = puntos, regla = c(rep(TRUE, 5), FALSE, FALSE)
  )
  names(datos)[[1L]] <- "geom"

  corrida <- avisos_de(perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(geom = ~ regla == TRUE)
  ))
  fila <- fila_de(corrida$valor, "tipos_geometria_mixtos")

  expect_identical(as.numeric(fila$n_evaluados), 5)
  expect_identical(as.numeric(fila$n_afectados), 5)
  expect_identical(filas_trazadas(fila), 1:5)
  expect_false(any(grepl("trazabilidad", corrida$avisos, fixed = TRUE)))
})

test_that("sin universo declarado, la geometria cuenta la columna entera", {
  # Mitad de control de los dos anteriores: el recorte no puede aplicarse donde no
  # hay universo declarado.
  skip_if_not_installed("sf")
  puntos <- sf::st_sfc(
    sf::st_point(c(1, 1)), sf::st_point(c(2, 2)), sf::st_point(c(3, 3)),
    sf::st_point(c(4, 4)), sf::st_point(c(5, 5)),
    sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(2, 2, 3, 3), ncol = 2, byrow = TRUE)),
    crs = 4326
  )
  datos <- data.frame(geom = puntos)
  names(datos)[[1L]] <- "geom"

  fila <- fila_de(
    perfilar(datos, analizar_dependencias = FALSE), "tipos_geometria_mixtos"
  )

  expect_identical(as.numeric(fila$n_evaluados), 7)
  expect_identical(as.numeric(fila$n_afectados), 7)
  expect_identical(filas_trazadas(fila), 1:7)
})

test_that("la lista de campos de indices de geometria esta en un solo lugar", {
  # Los cinco campos los leen el remapeo y el recorte al universo. Si cada uno los
  # escribiera por su lado, agregar un diagnostico con indices dejaria a uno de los
  # dos sin enterarse.
  campos <- lupa:::.CAMPOS_INDICES_GEOMETRIA
  vacias <- lupa:::.metricas_geometria_vacias()

  expect_true(all(campos %in% names(vacias)))
  expect_identical(length(campos), 5L)
})
