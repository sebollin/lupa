## Arreglos de 2.43 y 2.51 sobre la geometria y sobre la puerta de entrada al
## analisis de texto. Cada bloque corresponde a un defecto medido.

# ---------------------------------------------------------------------------
# 1. La columna no atomica se declara no analizable en vez de deparsarse
# ---------------------------------------------------------------------------

test_that("una sfc se declara no analizable como texto y no se convierte", {
  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(
    sf::st_point(c(-56, -34)), sf::st_point(c(-55, -33)), crs = 4326
  )

  preparado <- lupa:::.texto_analizable(geometria)

  expect_false(preparado$analizable)
  expect_identical(preparado$valores, rep(NA_character_, 2L))
  expect_identical(preparado$valores_identidad, geometria)
  expect_match(preparado$motivo, "lista de objetos")
  ## El deparse producia 17.901 caracteres por poligono; ahora no hay texto.
  expect_true(all(is.na(preparado$valores)))
})

test_that("una lista de raw (WKB) tampoco pasa por la maquinaria de texto", {
  columna <- list(as.raw(c(1, 1, 0, 0, 0)), as.raw(c(1, 2, 0, 0, 0)))

  preparado <- lupa:::.texto_analizable(columna)

  expect_false(preparado$analizable)
  expect_identical(preparado$valores, rep(NA_character_, 2L))
  expect_identical(preparado$valores_identidad, columna)
})

test_that("una lista de escalares atomicos sigue siendo analizable", {
  ## El corte separa lo que se convierte de lo que se deparsa: `as.character()`
  ## sobre esta lista devuelve los valores, no codigo fuente.
  columna <- list("ana", "beto", "ana")

  preparado <- lupa:::.texto_analizable(columna)

  expect_true(preparado$analizable)
  expect_identical(preparado$valores, columna)
  expect_identical(preparado$posiciones, integer())
})

test_that("las clases que saben convertirse a texto no se declaran perdidas", {
  ## POSIXlt es una lista y define as.character(): la coercion da la fecha.
  columna <- as.POSIXlt(as.POSIXct(c("2026-01-01", "2026-02-01"), tz = "UTC"))

  preparado <- lupa:::.texto_analizable(columna)

  expect_true(preparado$analizable)
  expect_identical(preparado$valores, columna)
})

test_that("el texto, los factores y los atomicos conservan su contrato", {
  texto <- c("Ana ", NA, "beto")
  numeros <- c(1.5, NA, 3)
  factores <- factor(c("a", "b", "a"))

  preparado_texto <- lupa:::.texto_analizable(texto)
  preparado_numeros <- lupa:::.texto_analizable(numeros)
  preparado_factores <- lupa:::.texto_analizable(factores)

  expect_identical(preparado_texto$valores, texto)
  expect_true(preparado_texto$analizable)
  expect_identical(preparado_numeros$valores, numeros)
  expect_true(preparado_numeros$analizable)
  expect_identical(preparado_factores$valores, c("a", "b", "a"))
  expect_true(preparado_factores$analizable)
})

test_that("los bytes UTF-8 invalidos se siguen aislando y declarando", {
  mala <- rawToChar(as.raw(c(0x61, 0xff, 0x62)))
  valores <- c(mala, "sano", mala)

  preparado <- lupa:::.texto_analizable(valores)

  expect_identical(preparado$posiciones, c(1L, 3L))
  expect_identical(preparado$valores, c(NA, "sano", NA))
  expect_true(preparado$analizable)
})

test_that("el costo del perfilado deja de escalar con los vertices", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("sf")
  anillo <- function(n) {
    angulos <- seq(0, 2 * pi, length.out = n + 1L)
    puntos <- cbind(-56 + 0.05 * cos(angulos), -34 + 0.05 * sin(angulos))
    puntos[nrow(puntos), ] <- puntos[1L, ]
    sf::st_polygon(list(puntos))
  }
  capa <- function(vertices) {
    geometria <- sf::st_sfc(
      lapply(seq_len(10L), function(i) anillo(vertices)), crs = 4326
    )
    sf::st_sf(id = seq_len(10L), geometry = geometria)
  }
  invisible(perfilar(capa(10L), analizar_dependencias = FALSE))

  flaca <- system.time(
    perfilar(capa(300L), analizar_dependencias = FALSE)
  )[["elapsed"]]
  gorda <- system.time(
    perfilar(capa(30000L), analizar_dependencias = FALSE)
  )[["elapsed"]]

  ## Cien veces mas vertices costaban del orden de cien veces mas tiempo porque
  ## cada etapa de texto deparsaba la geometria entera. Ahora la unica parte que
  ## sigue leyendo vertices es el codigo geometrico.
  expect_lt(gorda, max(1, flaca * 25))
})

# ---------------------------------------------------------------------------
# 2. La transformacion de dominio deja de rearmar PROJ por geometria
# ---------------------------------------------------------------------------

test_that("el dominio proyectado coincide con la transformacion unica", {
  skip_if_not_installed("sf")
  angulos <- seq(0, 2 * pi, length.out = 21L)
  poligono <- function(cx) {
    puntos <- cbind(cx + 5000 * cos(angulos), 6200000 + 5000 * sin(angulos))
    puntos[nrow(puntos), ] <- puntos[1L, ]
    sf::st_polygon(list(puntos))
  }
  geometria <- sf::st_sfc(
    lapply(seq_len(50L), function(i) poligono(500000 + (i - 1L) * 5000)),
    crs = 32721
  )
  geometria <- c(geometria, sf::st_sfc(sf::st_point(), crs = 32721))
  vacias <- sf::st_is_empty(geometria)
  crs <- sf::st_crs(geometria)
  area <- lupa:::.bbox_area_uso_crs(crs)

  medido <- lupa:::.evaluar_dominio_geometria(geometria, crs, vacias)

  transformada <- suppressWarnings(sf::st_transform(geometria, 4326))
  coordenadas <- lapply(unclass(transformada), lupa:::.coordenadas_sfg)
  esperado <- logical(length(geometria))
  evaluables <- which(!vacias)
  esperado[evaluables] <- vapply(coordenadas[evaluables], function(y) {
    !nrow(y) || lupa:::.coordenadas_fuera_longlat(y) ||
      (!isTRUE(area$global) && lupa:::.coordenadas_fuera_bbox(y, area$bbox))
  }, logical(1L))

  expect_true(medido$evaluado)
  expect_identical(medido$fuera, esperado)
  expect_identical(medido$n_evaluados, length(evaluables))
  expect_identical(medido$n_no_evaluados, 0L)
})

test_that("una geometria imposible de proyectar no descarta a las demas", {
  skip_if_not_installed("sf")
  ## 2.51: antes, una sola geometria fuera de la guarda devolvia
  ## `evaluado = FALSE` y tiraba lo que ya se sabia de toda la columna.
  geometria <- sf::st_sfc(
    sf::st_point(c(500000, 6200000)), sf::st_point(c(1e16, 1e16)),
    sf::st_point(c(505000, 6200000)), sf::st_point(),
    crs = 32721
  )

  medido <- lupa:::.evaluar_dominio_geometria(
    geometria, sf::st_crs(geometria), sf::st_is_empty(geometria)
  )

  expect_true(medido$evaluado)
  expect_identical(medido$fuera, c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(medido$n_evaluados, 3L)
})

test_that("la rama geografica conserva su resultado", {
  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(
    sf::st_point(c(-56, -34)), sf::st_point(c(200, 999)), sf::st_point(),
    crs = 4326
  )

  medido <- lupa:::.evaluar_dominio_geometria(
    geometria, sf::st_crs(geometria), sf::st_is_empty(geometria)
  )

  expect_true(medido$evaluado)
  expect_identical(medido$fuera, c(FALSE, TRUE, FALSE))
  expect_identical(medido$n_evaluados, 2L)
})

# ---------------------------------------------------------------------------
# 3. El presupuesto de geometrias y de vertices, con el recorte declarado
# ---------------------------------------------------------------------------

geometria_de_prueba <- function(n, vertices = 20L) {
  angulos <- seq(0, 2 * pi, length.out = vertices + 1L)
  sf::st_sfc(lapply(seq_len(n), function(i) {
    puntos <- cbind(
      -56 + (i - 1L) * 0.001 + 0.0005 * cos(angulos),
      -34 + 0.0005 * sin(angulos)
    )
    puntos[nrow(puntos), ] <- puntos[1L, ]
    sf::st_polygon(list(puntos))
  }), crs = 4326)
}

test_that("sin recorte el presupuesto no altera ninguna metrica", {
  skip_if_not_installed("sf")
  geometria <- geometria_de_prueba(12L)

  metricas <- lupa:::.perfilar_geometria(geometria)

  expect_false(metricas$geometrias_recortadas)
  expect_identical(metricas$n_geometrias, 12L)
  expect_identical(metricas$n_geometrias_analizadas, 12L)
  expect_identical(metricas$n_validez_evaluados, 12L)
  expect_identical(metricas$n_dominio_evaluados, 12L)
  expect_true(is.na(metricas$motivo_recorte))
})

test_that("el tope de geometrias recorta y declara su alcance", {
  skip_if_not_installed("sf")
  geometria <- geometria_de_prueba(40L)

  metricas <- lupa:::.perfilar_geometria(geometria, max_geometrias = 8L)

  expect_true(metricas$geometrias_recortadas)
  expect_identical(metricas$n_geometrias, 40L)
  expect_identical(metricas$n_geometrias_analizadas, 8L)
  expect_identical(metricas$n_validez_evaluados, 8L)
  expect_identical(metricas$n_dominio_evaluados, 8L)
  expect_match(metricas$motivo_recorte, "8 de 40")
  ## Lo que no escala con vertices se sigue midiendo sobre la columna entera.
  expect_identical(metricas$n_geometrias_vacias, 0L)
  expect_identical(metricas$n_bbox_evaluados, 40L)
})

test_that("el tope de vertices recorta y declara cuantos analizo", {
  skip_if_not_installed("sf")
  geometria <- geometria_de_prueba(40L, vertices = 20L)

  metricas <- lupa:::.perfilar_geometria(geometria, max_vertices = 200)

  expect_true(metricas$geometrias_recortadas)
  expect_lt(metricas$n_geometrias_analizadas, 40L)
  expect_lte(metricas$n_vertices_analizados, 200)
  expect_match(metricas$motivo_recorte, "vertices")
})

test_that("los topes se pueden mover con opciones", {
  skip_if_not_installed("sf")
  geometria <- geometria_de_prueba(40L)
  anteriores <- options(lupa.max_geometrias = 5L)
  on.exit(options(anteriores), add = TRUE)

  metricas <- lupa:::.perfilar_geometria(geometria)

  expect_true(metricas$geometrias_recortadas)
  expect_identical(metricas$n_geometrias_analizadas, 5L)
})

test_that("los indices recortados apuntan a las filas de la columna", {
  skip_if_not_installed("sf")
  invalido <- sf::st_polygon(list(rbind(
    c(0, 0), c(1, 1), c(1, 0), c(0, 1), c(0, 0)
  )))
  valido <- sf::st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  )))
  geometria <- sf::st_sfc(
    valido, valido, invalido, valido, valido, invalido, crs = 4326
  )

  completo <- lupa:::.perfilar_geometria(geometria)
  recortado <- lupa:::.perfilar_geometria(geometria, max_geometrias = 3L)

  expect_identical(completo$indices_invalidas, c(3L, 6L))
  expect_true(all(recortado$indices_invalidas %in% seq_len(6L)))
  expect_identical(recortado$n_validez_evaluados, 3L)
})

# ---------------------------------------------------------------------------
# 4. La validez conserva lo medido y declara lo que no pudo evaluar
# ---------------------------------------------------------------------------

test_that("la validez declara cuantas evaluo y cuantas no", {
  skip_if_not_installed("sf")
  invalido <- sf::st_polygon(list(rbind(
    c(0, 0), c(1, 1), c(1, 0), c(0, 1), c(0, 0)
  )))
  valido <- sf::st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  )))
  geometria <- sf::st_sfc(valido, invalido, crs = 4326)

  metricas <- lupa:::.perfilar_geometria(geometria)

  expect_true(metricas$validez_evaluada)
  expect_identical(metricas$n_geometrias_invalidas, 1L)
  expect_identical(metricas$indices_invalidas, 2L)
  expect_identical(metricas$n_validez_evaluados, 2L)
  expect_identical(metricas$n_validez_no_evaluados, 0L)
  expect_identical(metricas$indices_validez_no_evaluados, integer())
})

test_that("un NA de st_is_valid no descarta el conteo de las demas", {
  skip_if_not_installed("sf")
  ## 2.51: antes, un unico NA devolvia `validez_evaluada = FALSE` y borraba el
  ## conteo y los indices de todas las geometrias.
  cuadrado <- function(desplazamiento) {
    sf::st_polygon(list(rbind(
      c(desplazamiento, 0), c(desplazamiento + 1, 0),
      c(desplazamiento + 1, 1), c(desplazamiento, 1), c(desplazamiento, 0)
    )))
  }
  invalido <- sf::st_polygon(list(rbind(
    c(0, 0), c(1, 1), c(1, 0), c(0, 1), c(0, 0)
  )))
  geometria <- sf::st_sfc(cuadrado(0), invalido, cuadrado(3), crs = 4326)
  local_mocked_bindings(
    st_is_valid = function(x, ...) c(TRUE, FALSE, NA), .package = "sf"
  )

  metricas <- lupa:::.perfilar_geometria(geometria)

  expect_true(metricas$validez_evaluada)
  expect_identical(metricas$n_validez_evaluados, 2L)
  expect_identical(metricas$n_geometrias_invalidas, 1L)
  expect_identical(metricas$indices_invalidas, 2L)
  expect_identical(metricas$n_validez_no_evaluados, 1L)
  expect_identical(metricas$indices_validez_no_evaluados, 3L)
  expect_match(metricas$motivo_validez, "1 de 3")
})

test_that("una transformacion que falla conserva lo que si se pudo medir", {
  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(
    sf::st_point(c(500000, 6200000)), sf::st_point(c(505000, 6200000)),
    sf::st_point(c(510000, 6200000)), crs = 32721
  )
  original <- sf::st_transform
  intentos <- 0L
  local_mocked_bindings(
    st_transform = function(x, ...) {
      intentos <<- intentos + 1L
      if (length(x) != 1L || intentos == 3L) {
        stop("PROJ no pudo armar el pipeline")
      }
      original(x, ...)
    },
    .package = "sf"
  )

  medido <- lupa:::.evaluar_dominio_geometria(
    geometria, sf::st_crs(geometria), sf::st_is_empty(geometria)
  )

  expect_true(medido$evaluado)
  expect_identical(medido$n_evaluados, 2L)
  expect_identical(medido$no_evaluados, 2L)
  expect_match(medido$motivo, "1 de 3")
})

test_that("si no se pudo evaluar ninguna, no se afirma haberlo hecho", {
  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(
    sf::st_point(c(500000, 6200000)), sf::st_point(c(505000, 6200000)),
    crs = 32721
  )
  local_mocked_bindings(
    st_transform = function(x, ...) stop("PROJ no pudo armar el pipeline"),
    .package = "sf"
  )

  medido <- lupa:::.evaluar_dominio_geometria(
    geometria, sf::st_crs(geometria), sf::st_is_empty(geometria)
  )

  expect_false(medido$evaluado)
  expect_identical(medido$n_evaluados, NA_integer_)
  expect_match(medido$motivo, "PROJ")
})

# ---------------------------------------------------------------------------
# 5. WKT, WKB y hexadecimal: detectar y convertir, o declarar la perdida
# ---------------------------------------------------------------------------

test_that("una columna WKT se reconoce, se convierte y se mide", {
  skip_if_not_installed("sf")
  geometria <- geometria_de_prueba(5L)
  columna <- sf::st_as_text(geometria)

  metricas <- lupa:::.perfilar_geometria(columna)

  expect_true(metricas$aplica)
  expect_identical(metricas$representacion_geometria, "WKT")
  expect_true(metricas$geometria_convertida)
  expect_identical(metricas$tipo_geometria, "POLYGON")
  expect_identical(metricas$n_geometrias, 5L)
  expect_identical(metricas$n_geometrias_invalidas, 0L)
  expect_false(is.na(metricas$bbox_xmin))
})

test_that("una columna WKB cruda y una hexadecimal se reconocen igual", {
  skip_if_not_installed("sf")
  geometria <- geometria_de_prueba(5L)
  crudo <- sf::st_as_binary(geometria)
  hexadecimal <- vapply(crudo, function(bytes) {
    paste0(sprintf("%02X", as.integer(bytes)), collapse = "")
  }, character(1L))

  metricas_crudo <- lupa:::.perfilar_geometria(crudo)
  metricas_hex <- lupa:::.perfilar_geometria(hexadecimal)

  expect_identical(metricas_crudo$representacion_geometria, "WKB")
  expect_true(metricas_crudo$geometria_convertida)
  expect_identical(metricas_crudo$tipo_geometria, "POLYGON")
  expect_identical(metricas_hex$representacion_geometria, "WKB hexadecimal")
  expect_true(metricas_hex$geometria_convertida)
  expect_identical(metricas_hex$tipo_geometria, "POLYGON")
})

test_that("los bytes raw que no son WKB nunca llegan a sf", {
  skip_if_not_installed("sf")
  llamadas <- 0L
  local_mocked_bindings(
    st_as_sfc = function(...) {
      llamadas <<- llamadas + 1L
      stop("st_as_sfc no debe recibir bytes raw basura")
    },
    .package = "sf"
  )
  columna <- list(
    as.raw(c(2, 3, 4, 5, 6)), as.raw(c(2, 9, 9, 9, 9))
  )

  metricas <- expect_no_error(lupa:::.perfilar_geometria(columna))

  expect_identical(llamadas, 0L)
  expect_false(metricas$aplica)
  expect_true(is.na(metricas$representacion_geometria))
})

test_that("la rama raw adivinada solo pasa WKB plausibles a sf", {
  skip_if_not_installed("sf")
  valido <- sf::st_as_binary(
    sf::st_sfc(sf::st_point(c(1, 2)))
  )[[1L]]
  invalido <- as.raw(c(2, 3, 4, 5, 6))
  recibidos <- list()
  original <- sf::st_as_sfc
  local_mocked_bindings(
    st_as_sfc = function(x, ...) {
      recibidos <<- c(recibidos, list(x))
      original(x, ...)
    },
    .package = "sf"
  )

  metricas <- lupa:::.perfilar_geometria(list(valido, invalido))

  expect_true(metricas$geometria_convertida)
  expect_length(recibidos, 1L)
  expect_length(recibidos[[1L]], 1L)
  expect_identical(recibidos[[1L]][[1L]], valido)
})

test_that("la plausibilidad WKB respeta los bordes del encabezado", {
  expect_false(lupa:::.wkb_plausible(as.raw(1:4)))
  ## La guarda decia que cinco bytes alcanzaban «aunque un punto completo
  ## necesite 21». Un GDAL de win-builder revento -segfault, no error- con
  ## exactamente ese trunco de encabezado plausible, y un tryCatch no atrapa
  ## un segfault en C: ahora la aritmetica exige tambien el piso de largo por
  ## tipo, y un punto de cinco bytes es implausible.
  expect_false(lupa:::.wkb_plausible(as.raw(c(1, 1, 0, 0, 0))))
  expect_false(lupa:::.wkb_plausible(as.raw(c(0, 0, 0, 0, 1))))
  punto_completo <- as.raw(c(1, 1, 0, 0, 0, rep(0, 16)))
  expect_true(lupa:::.wkb_plausible(punto_completo))
  ## Una linea vacia es legitima con solo su conteo en cero: piso de 4 bytes.
  linea_vacia <- as.raw(c(1, 2, 0, 0, 0, 0, 0, 0, 0))
  expect_true(lupa:::.wkb_plausible(linea_vacia))
  expect_false(lupa:::.wkb_plausible(as.raw(c(2, 1, 0, 0, 0))))
  expect_false(lupa:::.wkb_plausible(as.raw(c(1, 8, 0, 0, 0))))
  ## ISO 1001 y las banderas EWKB Z/M conservan el tipo base POINT; el piso
  ## del punto (16 bytes de cuerpo) rige igual.
  expect_false(lupa:::.wkb_plausible(as.raw(c(1, 233, 3, 0, 0))))
  expect_true(lupa:::.wkb_plausible(as.raw(c(1, 233, 3, 0, 0, rep(0, 16)))))
  expect_false(lupa:::.wkb_plausible(as.raw(c(1, 1, 0, 0, 128))))
  expect_false(lupa:::.wkb_plausible(as.raw(c(1, 1, 0, 0, 64))))
  ## EWKB SRID agrega cuatro bytes obligatorios al encabezado, y el punto
  ## sigue debiendo sus 16 de cuerpo: el total plausible minimo es 25.
  ewkb_srid_corto <- as.raw(c(1, 1, 0, 0, 32, 230, 16, 0))
  ewkb_srid_solo_encabezado <- c(ewkb_srid_corto, as.raw(0))
  ewkb_srid_completo <- c(ewkb_srid_solo_encabezado, as.raw(rep(0, 16)))
  expect_false(lupa:::.wkb_plausible(ewkb_srid_corto))
  expect_false(lupa:::.wkb_plausible(ewkb_srid_solo_encabezado))
  expect_true(lupa:::.wkb_plausible(ewkb_srid_completo))
})

test_that("un punto WKB real y un EWKB con SRID siguen parseandose", {
  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326)
  wkb <- sf::st_as_binary(geometria)[[1L]]
  ewkb <- sf::st_as_binary(geometria, EWKB = TRUE)[[1L]]

  expect_identical(length(wkb), 21L)
  expect_identical(length(ewkb), 25L)
  expect_identical(as.integer(wkb[[1L]]), 1L)
  expect_identical(as.integer(ewkb[[1L]]), 1L)
  expect_true(lupa:::.wkb_plausible(wkb))
  expect_true(lupa:::.wkb_plausible(ewkb))

  metricas_wkb <- lupa:::.perfilar_geometria(structure(list(wkb), class = "WKB"))
  metricas_ewkb <- lupa:::.perfilar_geometria(structure(list(ewkb), class = "WKB"))

  expect_true(metricas_wkb$geometria_convertida)
  expect_true(metricas_ewkb$geometria_convertida)
  expect_identical(metricas_wkb$tipo_geometria, "POINT")
  expect_identical(metricas_ewkb$tipo_geometria, "POINT")
  expect_identical(metricas_ewkb$crs_declarado, "4326")
})

test_that("un WKB declarado con un valor invalido declara la perdida sin sf", {
  skip_if_not_installed("sf")
  valido <- sf::st_as_binary(
    sf::st_sfc(sf::st_point(c(1, 2)))
  )[[1L]]
  invalido <- as.raw(c(2, 3, 4, 5, 6))
  columna <- structure(list(valido, invalido), class = "WKB")
  llamadas <- 0L
  local_mocked_bindings(
    st_as_sfc = function(...) {
      llamadas <<- llamadas + 1L
      stop("st_as_sfc no debe recibir un WKB estructuralmente invalido")
    },
    .package = "sf"
  )

  metricas <- lupa:::.perfilar_geometria(columna)

  expect_identical(llamadas, 0L)
  expect_true(metricas$aplica)
  expect_identical(metricas$representacion_geometria, "WKB")
  expect_true(metricas$sf_evaluado)
  expect_false(metricas$geometria_convertida)
  expect_false(metricas$validez_evaluada)
  expect_false(metricas$dominio_evaluado)
  expect_match(metricas$motivo_representacion, "estructuralmente invalidos")
  expect_match(metricas$motivo_validez, "estructuralmente invalidos")
  expect_match(metricas$motivo_dominio, "estructuralmente invalidos")
})

test_that("el perfil publica las metricas espaciales de una columna WKT", {
  skip_if_not_installed("sf")
  columna <- sf::st_as_text(geometria_de_prueba(5L))
  datos <- data.frame(id = seq_len(5L), geom = columna, stringsAsFactors = FALSE)

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  fila <- perfil$columnas[perfil$columnas$columna == "geom", ]

  ## Antes quedaban todas en NA y `cobertura_diagnosticos` no traia una sola
  ## fila sobre la geometria.
  expect_identical(fila$tipo_geometria, "POLYGON")
  expect_identical(fila$n_geometrias_invalidas, 0L)
  expect_false(is.na(fila$bbox_xmin))
  expect_true("crs_no_declarado" %in% perfil$hallazgos$tipo_hallazgo)
})

test_that("un WKT que no se puede convertir declara la perdida", {
  skip_if_not_installed("sf")
  columna <- c(
    "POLYGON ((0 0, 1 0, 1 1, 0 0))", "POLYGON ((no es una coordenada))"
  )

  ## La perdida se declara por aritmetica propia: el texto corrupto no puede
  ## llegar a sf, porque un GDAL real reventaba (segfault) al parsearlo y un
  ## tryCatch no atrapa un segfault. La guarda se prueba con el caso que debe
  ## disparar: si sf llegara a recibirlo, este mock corta la corrida.
  local_mocked_bindings(
    st_as_sfc = function(...) stop("st_as_sfc no debe recibir WKT corrupto"),
    .package = "sf"
  )
  metricas <- suppressMessages(lupa:::.perfilar_geometria(columna))

  ## Nunca "no aplica" sobre datos que si son geometricos.
  expect_true(metricas$aplica)
  expect_identical(metricas$representacion_geometria, "WKT")
  expect_false(metricas$geometria_convertida)
  expect_false(metricas$validez_evaluada)
  expect_false(metricas$dominio_evaluado)
  expect_match(metricas$motivo_representacion, "no se pudo convertir")
  expect_match(metricas$motivo_validez, "no se pudo convertir")
})

test_that("un WKT con SRID conserva el CRS declarado", {
  skip_if_not_installed("sf")
  columna <- paste0("SRID=4326;", sf::st_as_text(geometria_de_prueba(3L)))

  metricas <- lupa:::.perfilar_geometria(columna)

  expect_identical(metricas$crs_declarado, "4326")
  expect_identical(metricas$tipo_geometria, "POLYGON")
})

test_that("los valores ausentes no se cuentan como geometrias", {
  skip_if_not_installed("sf")
  columna <- c(sf::st_as_text(geometria_de_prueba(3L)), NA_character_)

  metricas <- lupa:::.perfilar_geometria(columna)

  expect_identical(metricas$n_geometrias, 4L)
  expect_identical(metricas$n_geometrias_analizadas, 3L)
  expect_identical(metricas$n_geometrias_vacias, 0L)
  ## Un ausente no es una geometria recortada: no hay recorte que declarar.
  expect_false(metricas$geometrias_recortadas)
})

test_that("el recorte de la conversion se declara como recorte", {
  skip_if_not_installed("sf")
  columna <- sf::st_as_text(geometria_de_prueba(20L))

  metricas <- lupa:::.perfilar_geometria(columna, max_geometrias = 5L)

  expect_identical(metricas$n_geometrias, 20L)
  expect_identical(metricas$n_geometrias_analizadas, 5L)
  expect_true(metricas$geometrias_recortadas)
  expect_match(metricas$motivo_recorte, "5 de 20")
})

test_that("las columnas que no son geometria no se declaran geometricas", {
  candidatas <- list(
    texto = c("Ana", "Beto", "Carla"),
    identificador_hexadecimal = c(
      "00ff00ff00ff00ff00", "01ab01ab01ab01ab01"
    ),
    numeros_como_texto = c("123456", "789012"),
    vacio = character(),
    ausentes = c(NA_character_, NA_character_),
    lista = list(1, 2, 3),
    raw_que_no_es_wkb = list(as.raw(c(2, 3, 4, 5, 6)), as.raw(c(2, 9, 9, 9, 9)))
  )

  for (nombre in names(candidatas)) {
    metricas <- suppressMessages(
      lupa:::.perfilar_geometria(candidatas[[nombre]])
    )
    expect_false(metricas$aplica, info = nombre)
    expect_true(is.na(metricas$representacion_geometria), info = nombre)
  }
})

test_that("el reconocimiento sobrevive a los bytes UTF-8 invalidos", {
  mala <- rawToChar(as.raw(c(0x61, 0xff, 0x62)))

  metricas <- expect_no_error(lupa:::.perfilar_geometria(c(mala, "sano")))

  expect_false(metricas$aplica)
})
