# comparar_perfiles(): la deriva de la distribucion (media, dispersion y reparto
# de las categorias) y el cambio de conteo de un hallazgo que sigue con la misma
# severidad. Cada medida se pide con su tamano de efecto y por encima del ruido
# de muestreo: las pruebas de falsos positivos son parte de la promesa.

# Prueba de regresion: corre en la suite completa, en el CI y en R-hub, que usan
# NOT_CRAN=true; en CRAN se saltea para que el check entre en sus tiempos.
testthat::skip_on_cran()

.cdd_enero <- as.POSIXct("2026-01-01", tz = "UTC")
.cdd_febrero <- as.POSIXct("2026-02-01", tz = "UTC")
.cdd_aspectos <- c(
  "deriva_media", "deriva_dispersion", "deriva_categorias", "conteo_hallazgo"
)

.cdd_perfil <- function(datos, fecha = .cdd_enero, ...) {
  suppressWarnings(suppressMessages(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, ...
  )))
}

.cdd_comparar <- function(anterior, actual, ...) {
  as.data.frame(suppressWarnings(suppressMessages(
    comparar_perfiles(anterior, actual, ...)
  )))
}

.cdd_deriva <- function(anterior, actual, ...) {
  d <- .cdd_comparar(
    .cdd_perfil(anterior), .cdd_perfil(actual, .cdd_febrero), ...
  )
  d[d$aspecto %in% .cdd_aspectos, , drop = FALSE]
}

# Una normal determinista: los cuantiles de n puntos, mezclados con una semilla.
.cdd_normal <- function(n, media, desvio, semilla) {
  set.seed(semilla)
  media + desvio * sample(stats::qnorm((seq_len(n) - 0.5) / n))
}

.cdd_categorias <- function(n, p, semilla, valores = c("a", "b")) {
  set.seed(semilla)
  sample(rep(valores, times = round(n * p)))
}

test_that("una media corrida 25 con desvio 15 se publica, y no sale callada", {
  d <- .cdd_deriva(
    data.frame(x = .cdd_normal(4000L, 100, 15, 1L)),
    data.frame(x = .cdd_normal(4000L, 125, 15, 2L))
  )
  fila <- d[d$aspecto == "deriva_media", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$severidad), "error")
  expect_equal(fila$delta, 25 / 15, tolerance = 1e-3)
  expect_true(grepl("umbrales", fila$evidencia, fixed = TRUE, useBytes = TRUE))
  expect_false("deriva_dispersion" %in% d$aspecto)
})

test_that("el cambio de proporciones 50/50 a 80/20 se publica", {
  anterior <- data.frame(g = c(rep("a", 1000L), rep("b", 1000L)))
  actual <- data.frame(g = c(rep("a", 1600L), rep("b", 400L)))
  d <- .cdd_deriva(anterior, actual)
  fila <- d[d$aspecto == "deriva_categorias", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$severidad), "error")
  expect_gt(fila$delta, 0.25)
  expect_match(fila$evidencia, "PSI", fixed = TRUE)
})

test_that("un cambio de varianza sin cambio de media se publica como dispersion", {
  d <- .cdd_deriva(
    data.frame(x = .cdd_normal(4000L, 100, 15, 4L)),
    data.frame(x = .cdd_normal(4000L, 100, 30, 5L))
  )
  expect_identical(d$aspecto, "deriva_dispersion")
  expect_identical(as.character(d$severidad), "error")
  expect_equal(d$delta, 2, tolerance = 1e-2)
})

test_that("un corrimiento de cola que mueve la media se publica", {
  base <- .cdd_normal(4000L, 100, 15, 6L)
  cola <- base
  arriba <- base >= stats::quantile(base, 0.9)
  cola[arriba] <- cola[arriba] + 45
  d <- .cdd_deriva(data.frame(x = base), data.frame(x = cola))
  expect_true("deriva_media" %in% d$aspecto)
})

test_that("una categoria nueva que se lleva el 30 % la dice el vocabulario, sin duplicarla", {
  anterior <- data.frame(g = c(rep("a", 1400L), rep("b", 600L)))
  actual <- data.frame(g = c(rep("a", 980L), rep("b", 420L), rep("c", 600L)))
  d <- .cdd_comparar(.cdd_perfil(anterior), .cdd_perfil(actual, .cdd_febrero))
  expect_true(any(d$aspecto == "vocabulario" & d$severidad == "error"))
  # El reparto entre a y b no cambio: no hay una segunda alarma.
  expect_false("deriva_categorias" %in% d$aspecto)
})

test_that("perfiles identicos y misma distribucion con n grande no dan deriva", {
  x <- data.frame(
    x = .cdd_normal(3000L, 100, 15, 7L),
    g = .cdd_categorias(3000L, c(0.6, 0.4), 8L)
  )
  expect_equal(nrow(.cdd_deriva(x, x)), 0L)
  y <- data.frame(
    x = .cdd_normal(3000L, 100, 15, 9L),
    g = .cdd_categorias(3000L, c(0.6, 0.4), 10L)
  )
  expect_equal(nrow(.cdd_deriva(x, y)), 0L)
  # Una media corrida 0,03 desvios: significativa con n enorme, trivial en
  # tamano. El tamano de efecto manda.
  z <- data.frame(x = .cdd_normal(3000L, 100.45, 15, 11L))
  expect_equal(nrow(.cdd_deriva(x["x"], z)), 0L)
})

test_that("dos muestras chicas de la misma poblacion no acusan deriva", {
  hits <- 0L
  for (semilla in 1:60) {
    set.seed(semilla)
    a <- data.frame(
      x = rnorm(30L, 100, 15), g = sample(c("a", "b", "c"), 30L, TRUE),
      stringsAsFactors = FALSE
    )
    b <- data.frame(
      x = rnorm(30L, 100, 15), g = sample(c("a", "b", "c"), 30L, TRUE),
      stringsAsFactors = FALSE
    )
    hits <- hits + nrow(.cdd_deriva(a, b))
  }
  expect_lte(hits, 1L)
})

test_that("las categorias raras y las columnas casi constantes no acusan ruido", {
  hits <- 0L
  for (semilla in 1:40) {
    set.seed(100L + semilla)
    prob <- c(0.3, 0.2, rep(0.5 / 18, 18L))
    a <- data.frame(
      g = sample(paste0("c", 1:20), 400L, TRUE, prob = prob),
      k = ifelse(stats::runif(400L) < 0.98, 5, stats::rnorm(400L, 5, 1)),
      stringsAsFactors = FALSE
    )
    b <- data.frame(
      g = sample(paste0("c", 1:20), 400L, TRUE, prob = prob),
      k = ifelse(stats::runif(400L) < 0.98, 5, stats::rnorm(400L, 5, 1)),
      stringsAsFactors = FALSE
    )
    hits <- hits + nrow(.cdd_deriva(a, b))
  }
  expect_lte(hits, 1L)
})

test_that("un cambio de tipo ya diagnosticado no se repite como deriva de la media", {
  anterior <- data.frame(x = rep(1:100, 20L))
  actual <- data.frame(x = rep(1:100, 20L) * 1000)
  d <- .cdd_comparar(.cdd_perfil(anterior), .cdd_perfil(actual, .cdd_febrero))
  expect_true("tipo_declarado" %in% d$aspecto)
  expect_false("deriva_media" %in% d$aspecto)
})

test_that("un lado que es una muestra de la base no afirma deriva", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  grande <- data.frame(x = .cdd_normal(2000L, 100, 15, 12L))
  corrida <- data.frame(x = .cdd_normal(2000L, 150, 15, 13L))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "enero", grande)
  DBI::dbWriteTable(con, "febrero", corrida)
  dbi <- function(tabla, fecha) {
    suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, fecha = fecha, muestra = 300L, orden_muestra = "x",
      proteger_datos_personales = FALSE, analizar_dependencias = FALSE
    )))
  }
  d <- .cdd_comparar(dbi("enero", .cdd_enero), dbi("febrero", .cdd_febrero))
  expect_true("muestra" %in% d$aspecto)
  expect_false(any(d$aspecto %in% .cdd_aspectos))
})

test_that("las columnas que aparecen o se van no dan deriva", {
  grande <- data.frame(x = .cdd_normal(2000L, 100, 15, 12L))
  corrida <- data.frame(x = .cdd_normal(2000L, 150, 15, 13L))
  d <- .cdd_comparar(
    .cdd_perfil(data.frame(x = grande$x, y = grande$x)),
    .cdd_perfil(data.frame(x = grande$x, z = corrida$x), .cdd_febrero)
  )
  expect_false(any(d$aspecto %in% .cdd_aspectos))
  expect_true(all(c("desaparecida", "aparecida") %in% d$cambio))
})

test_that("el conteo de un hallazgo con la misma severidad se publica si es material y no es ruido", {
  # Edades fuera de dominio: su conteo no depende de la fecha de la corrida.
  futuras <- function(p, n) {
    edad <- rep(30L, n)
    edad[seq_len(round(p * n))] <- 250L
    data.frame(edad = edad)
  }
  d <- .cdd_deriva(futuras(0.5, 2000L), futuras(0.6, 2000L))
  fila <- d[d$aspecto == "conteo_hallazgo", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(fila$cambio, "agravado")
  expect_equal(fila$delta, 0.1)
  expect_identical(fila$valor_anterior, "1000 de 2000")
  expect_identical(fila$valor_actual, "1200 de 2000")
  # Al reves: mejora, severidad `ok`.
  d <- .cdd_deriva(futuras(0.6, 2000L), futuras(0.5, 2000L))
  d <- d[d$aspecto == "conteo_hallazgo", , drop = FALSE]
  expect_identical(d$cambio, "atenuado")
  expect_identical(as.character(d$severidad), "ok")
  # Con 100 filas, 50 % -> 60 % cabe en el ruido: no hay fila.
  expect_equal(nrow(.cdd_deriva(futuras(0.5, 100L), futuras(0.6, 100L))), 0L)
  # Una fila de mas, de 1 en 100 a 2 en 100, tampoco.
  fuera <- function(k) {
    edad <- rep(30L, 100L)
    edad[seq_len(k)] <- 250L
    data.frame(edad = edad)
  }
  expect_equal(nrow(.cdd_deriva(fuera(1L), fuera(2L))), 0L)
  # Mismo conteo: ninguna fila.
  expect_equal(nrow(.cdd_deriva(futuras(0.5, 2000L), futuras(0.5, 2000L))), 0L)
})

test_that("un perfil guardado y vuelto a leer da la misma deriva", {
  a <- .cdd_perfil(data.frame(
    x = .cdd_normal(2000L, 100, 15, 16L),
    g = .cdd_categorias(2000L, c(0.5, 0.5), 17L)
  ))
  b <- .cdd_perfil(data.frame(
    x = .cdd_normal(2000L, 120, 15, 18L),
    g = .cdd_categorias(2000L, c(0.8, 0.2), 19L)
  ), .cdd_febrero)
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  saveRDS(a, archivo, version = 2)
  leido <- readRDS(archivo)
  directa <- .cdd_comparar(a, b)
  releida <- .cdd_comparar(leido, b)
  expect_identical(
    directa[directa$aspecto %in% .cdd_aspectos, ],
    releida[releida$aspecto %in% .cdd_aspectos, ]
  )
  expect_true(all(c("deriva_media", "deriva_categorias") %in% directa$aspecto))
  salida <- utils::capture.output(print(suppressMessages(comparar_perfiles(a, b))))
  expect_true(any(grepl("deriva_media", salida, fixed = TRUE, useBytes = TRUE)))
  expect_no_error(utils::capture.output(print(summary(
    suppressMessages(comparar_perfiles(a, b))
  ))))
})

test_that("los perfiles de la base dan la misma deriva que los de memoria", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  enero <- data.frame(
    x = .cdd_normal(3000L, 100, 15, 20L),
    g = .cdd_categorias(3000L, c(0.5, 0.5), 21L), stringsAsFactors = FALSE
  )
  febrero <- data.frame(
    x = .cdd_normal(3000L, 125, 15, 22L),
    g = .cdd_categorias(3000L, c(0.8, 0.2), 23L), stringsAsFactors = FALSE
  )
  clave <- function(d) {
    d <- d[d$aspecto %in% .cdd_aspectos, , drop = FALSE]
    d[.orden_por_bytes(d$columna, d$aspecto), c("columna", "aspecto", "severidad", "delta")]
  }
  memoria <- clave(.cdd_comparar(.cdd_perfil(enero), .cdd_perfil(febrero, .cdd_febrero)))
  expect_equal(nrow(memoria), 2L)
  motores <- list(
    sqlite = function() DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    duckdb = function() DBI::dbConnect(.duckdb_prueba(), ":memory:")
  )
  for (motor in names(motores)) {
    con <- motores[[motor]]()
    DBI::dbWriteTable(con, "enero", enero)
    DBI::dbWriteTable(con, "febrero", febrero)
    dbi <- function(tabla, fecha) {
      suppressWarnings(suppressMessages(perfilar_dbi(
        con, tabla, fecha = fecha, proteger_datos_personales = FALSE,
        analizar_dependencias = FALSE
      )))
    }
    desde_base <- clave(.cdd_comparar(dbi("enero", .cdd_enero), dbi("febrero", .cdd_febrero)))
    rownames(desde_base) <- NULL
    rownames(memoria) <- NULL
    expect_equal(desde_base$columna, memoria$columna, info = motor)
    expect_equal(desde_base$aspecto, memoria$aspecto, info = motor)
    expect_equal(as.character(desde_base$severidad),
                 as.character(memoria$severidad), info = motor)
    expect_equal(desde_base$delta, memoria$delta, tolerance = 1e-6, info = motor)
    if (identical(motor, "duckdb")) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    } else {
      DBI::dbDisconnect(con)
    }
  }
})

test_that("un cambio de unidad o de politica de centinelas no se repite como deriva", {
  x <- .cdd_normal(2000L, 100, 15, 24L)
  anterior <- .cdd_perfil(data.frame(x = x))
  actual <- .cdd_perfil(data.frame(x = x * 1000), .cdd_febrero)
  anterior$columnas$unidad <- "m"
  actual$columnas$unidad <- "km"
  d <- .cdd_comparar(anterior, actual)
  expect_true("unidad" %in% d$aspecto)
  expect_false(any(d$aspecto %in% .cdd_aspectos))
  # La misma columna con -999: con el centinela declarado en un lado y no en el
  # otro, la media cambia por la politica y no por los datos.
  y <- c(x[1:1900], rep(-999, 100L))
  d <- .cdd_comparar(
    .cdd_perfil(data.frame(x = y), sentinelas_numericos = -999),
    .cdd_perfil(data.frame(x = y), .cdd_febrero)
  )
  expect_true("configuracion_sentinelas_numericos" %in% d$aspecto)
  expect_false("deriva_media" %in% d$aspecto)
})

# Segunda tanda: la forma no es el valor, el NaN se cuenta una vez, la deriva se
# abstiene cuando la comparacion no es decidible y no contradice lo publicado.

test_that("el mismo reparto escrito con otra forma no es deriva de categorias", {
  anterior <- data.frame(g = c(rep("Si", 900L), rep("SI", 100L),
                               rep("No", 900L), rep("NO", 100L)))
  actual <- data.frame(g = c(rep("Si", 100L), rep("SI", 900L),
                             rep("No", 100L), rep("NO", 900L)))
  d <- .cdd_comparar(.cdd_perfil(anterior), .cdd_perfil(actual, .cdd_febrero))
  expect_false("deriva_categorias" %in% d$aspecto)
  # Espacios en los bordes: tampoco hay conteo de `espacios_sobrantes`.
  anterior <- data.frame(g = rep(c(" a", "b"), 500L))
  actual <- data.frame(g = rep(c("a", "b"), 500L))
  d <- .cdd_comparar(.cdd_perfil(anterior), .cdd_perfil(actual, .cdd_febrero))
  expect_false(any(d$aspecto %in% c("deriva_categorias", "conteo_hallazgo")))
  # Y un reparto que si cambio, con formas mezcladas, sigue saliendo.
  base <- data.frame(g = c(rep("Si", 500L), rep("SI", 500L), rep("No", 1000L)))
  otro <- data.frame(g = c(rep("Si", 1200L), rep("SI", 400L), rep("No", 400L)))
  d <- .cdd_comparar(.cdd_perfil(base), .cdd_perfil(otro, .cdd_febrero))
  expect_true("deriva_categorias" %in% d$aspecto)
})

test_that("un NaN se descuenta una vez del n: la deriva real con 50 % de NaN sale", {
  cuerpo <- .cdd_normal(1998L, 0, 2, 30L)
  a <- data.frame(x = c(-10, 10, cuerpo))
  b <- data.frame(x = c(-10, 10, cuerpo[1:998] + 3, rep(NaN, 1000L)))
  d <- .cdd_comparar(.cdd_perfil(a), .cdd_perfil(b, .cdd_febrero))
  fila <- d[d$aspecto == "deriva_media", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$valor_actual, "n 1000", fixed = TRUE)
  expect_match(fila$valor_anterior, "n 2000", fixed = TRUE)
})

test_that("sin unidad, sin politica de centinelas o con la universo redefinido no hay deriva", {
  x <- .cdd_normal(2000L, 100, 15, 31L)
  anterior <- .cdd_perfil(data.frame(x = x))
  actual <- .cdd_perfil(data.frame(x = x * 1000), .cdd_febrero)
  # Perfil viejo sin la columna `unidad`.
  viejo <- anterior
  viejo$columnas$unidad <- NULL
  actual$columnas$unidad <- "km"
  d <- .cdd_comparar(viejo, actual)
  expect_false(any(d$aspecto %in% .cdd_aspectos))
  # Perfil viejo sin la politica de centinelas, con faltantes numericos
  # disfrazados en la columna: no se sabe con que politica se midio.
  con_centinela <- c(x[1:1900], rep(-999, 100L))
  viejo <- .cdd_perfil(data.frame(x = con_centinela))
  viejo$meta$sentinelas_numericos <- NULL
  d <- .cdd_comparar(viejo, .cdd_perfil(data.frame(x = con_centinela + 50),
                                        .cdd_febrero))
  expect_false(any(d$aspecto %in% .cdd_aspectos))
  # Columnas opcionales declaradas en un lado: el universo cambia por la regla.
  d <- .cdd_comparar(
    .cdd_perfil(data.frame(x = x), columnas_opcionales = "x"),
    .cdd_perfil(data.frame(x = x + 40), .cdd_febrero)
  )
  expect_true("configuracion_columnas_opcionales" %in% d$aspecto)
  expect_false(any(d$aspecto %in% .cdd_aspectos))
  # Y sin esa configuracion, el mismo corrimiento sale.
  d <- .cdd_comparar(.cdd_perfil(data.frame(x = x)),
                     .cdd_perfil(data.frame(x = x + 40), .cdd_febrero))
  expect_true("deriva_media" %in% d$aspecto)
})

test_that("los textos dicen lo observado y no afirman lo que no se midio", {
  x <- .cdd_normal(2000L, 100, 15, 32L)
  d <- .cdd_comparar(.cdd_perfil(data.frame(x = x)),
                     .cdd_perfil(data.frame(x = x * 1000), .cdd_febrero))
  fila <- d[d$aspecto == "deriva_media", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_false(grepl("no es un cambio de unidad", fila$descripcion, fixed = TRUE,
                     useBytes = TRUE))
  expect_match(fila$descripcion, "cambio de unidad", fixed = TRUE)
  d <- .cdd_deriva(
    data.frame(g = c(rep("a", 1000L), rep("b", 1000L))),
    data.frame(g = c(rep("a", 1600L), rep("b", 400L)))
  )
  expect_false(grepl("muestreo explique", d$descripcion, fixed = TRUE,
                     useBytes = TRUE))
  expect_match(d$descripcion, "0,001", fixed = TRUE)
})

test_that("el conteo de fecha_futura no cambia porque paso el tiempo", {
  datos <- data.frame(f = as.Date(c(
    rep("2026-01-15", 700L), rep("2030-01-01", 1300L)
  )))
  d <- .cdd_comparar(.cdd_perfil(datos), .cdd_perfil(datos, .cdd_febrero))
  expect_false("conteo_hallazgo" %in% d$aspecto)
})

test_that("el conteo no se compara si cambio el universo por los faltantes", {
  uno <- function(n_na) {
    data.frame(x = c(rep(NA_integer_, n_na), seq_len(2000L - n_na)),
               y = c(rep(NA_integer_, n_na), seq_len(2000L - n_na)))
  }
  d <- .cdd_comparar(.cdd_perfil(uno(1900L)), .cdd_perfil(uno(1600L), .cdd_febrero))
  expect_false("conteo_hallazgo" %in% d$aspecto)
})

test_that("una entrega a 2 decimales y otra a 0 no acusa deriva de dispersion", {
  base <- .cdd_normal(1000L, 0, 0.2, 33L)
  d <- .cdd_comparar(.cdd_perfil(data.frame(x = round(base, 2L))),
                     .cdd_perfil(data.frame(x = round(base, 0L)), .cdd_febrero))
  expect_false(any(d$aspecto %in% .cdd_aspectos))
  # Con la misma resolucion el cambio de dispersion sigue saliendo.
  d <- .cdd_deriva(data.frame(x = round(.cdd_normal(1000L, 0, 0.2, 34L), 2L)),
                   data.frame(x = round(.cdd_normal(1000L, 0, 0.5, 35L), 2L)))
  expect_true("deriva_dispersion" %in% d$aspecto)
})

# Tercera tanda: el n no es deriva, la resolucion no decidible se abstiene, la
# abstencion es por columna y una constante que cambia de valor se publica.

test_that("mas filas de la misma distribucion no dan conteo de duplicados", {
  set.seed(40L)
  a <- data.frame(x = sample(1:1000, 1000L, replace = TRUE))
  b <- data.frame(x = sample(1:1000, 2000L, replace = TRUE))
  d <- .cdd_comparar(.cdd_perfil(a), .cdd_perfil(b, .cdd_febrero))
  expect_false("conteo_hallazgo" %in% d$aspecto)
})

test_that("un redondeo a dos valores distintos no acusa dispersion ni conteo", {
  set.seed(41L)
  base <- rnorm(200L, 0, 0.2)
  d <- .cdd_comparar(.cdd_perfil(data.frame(x = round(base, 2L))),
                     .cdd_perfil(data.frame(x = round(base, 0L)), .cdd_febrero))
  expect_false(any(d$aspecto %in% .cdd_aspectos))
  base <- rep(c(99.4, 100.6), 500L) + rnorm(1000L, 0, 0.01)
  d <- .cdd_comparar(.cdd_perfil(data.frame(x = round(base, 2L))),
                     .cdd_perfil(data.frame(x = round(base, 0L)), .cdd_febrero))
  expect_false("deriva_dispersion" %in% d$aspecto)
})

test_that("la abstencion es por columna: la deriva de la otra columna sale", {
  set.seed(42L)
  datos <- data.frame(x = rnorm(2000L), y = rnorm(2000L))
  b <- datos
  b$y <- b$y + 3
  solo_y <- function(d) d$columna[d$aspecto %in% .cdd_aspectos]
  # Perfil viejo sin `unidad`, y el otro la declara solo en x.
  anterior <- .cdd_perfil(datos)
  actual <- .cdd_perfil(b, .cdd_febrero)
  anterior$columnas$unidad <- NULL
  actual$columnas$unidad <- c("m", NA)
  d <- .cdd_comparar(anterior, actual)
  expect_identical(unique(solo_y(d)), "y")
  # Si ninguno declara unidad, se compara todo.
  actual$columnas$unidad <- NULL
  d <- .cdd_comparar(anterior, actual)
  expect_true("y" %in% solo_y(d))
  # Columnas opcionales declaradas en x: y se compara.
  d <- .cdd_comparar(.cdd_perfil(datos, columnas_opcionales = "x"),
                     .cdd_perfil(b, .cdd_febrero))
  expect_identical(unique(solo_y(d)), "y")
  # Politica de centinelas distinta: y no tiene faltantes numericos disfrazados.
  d <- .cdd_comparar(.cdd_perfil(datos, sentinelas_numericos = -999),
                     .cdd_perfil(b, .cdd_febrero, sentinelas_numericos = numeric(0)))
  expect_identical(unique(solo_y(d)), "y")
  # Aplicabilidad en x.
  datos$marca <- rep(c("Si", "No"), 1000L)
  b$marca <- datos$marca
  d <- .cdd_comparar(
    .cdd_perfil(datos, aplicabilidad = list(x = ~ marca == "Si")),
    .cdd_perfil(b, .cdd_febrero)
  )
  expect_identical(unique(solo_y(d)), "y")
})

test_that("una columna constante que pasa a otra constante se publica sin medida estandarizada", {
  d <- .cdd_deriva(data.frame(x = rep(10, 500L)), data.frame(x = rep(20, 500L)))
  expect_equal(nrow(d), 1L)
  expect_identical(d$aspecto, "deriva_media")
  expect_true(is.na(d$delta))
  expect_match(d$descripcion, "constante en 10 y ahora es constante en 20",
               fixed = TRUE)
  expect_equal(nrow(.cdd_deriva(data.frame(x = rep(10, 500L)),
                                data.frame(x = rep(10, 500L)))), 0L)
})
