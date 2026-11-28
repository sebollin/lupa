# Una proporcion (`proporcion`, `participacion`) cuya escala de 0 a 1 es
# plausible -la mayoria de sus valores que no son 0 ni 1 esta estrictamente
# entre 0 y 1- y que tiene valores entre 1 y 100. Por los valores solos la
# escala es ambigua (una proporcion con errores, un porcentaje de valores chicos
# y una razon dan los mismos numeros), asi que el paquete no decide: publica un
# hallazgo propio (`valor_fuera_dominio`, aviso `ok` de la columna) con lo
# medido y las dos lecturas, sin acusar filas, sin cambiar lo que los demas
# hallazgos publican y sin repetir sus filas.

testthat::skip_on_cran()

.fecha_unitaria <- as.Date("2026-11-19")

.perfil_unitaria <- function(datos, fecha = .fecha_unitaria, ...) {
  suppressWarnings(suppressMessages(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE, ...
  )))
}

.interiores_unitaria <- function(n) seq(0.01, 0.99, length.out = n)

.dominio_unitaria <- function(perfil, columna = "proporcion") {
  h <- as.data.frame(perfil$hallazgos)
  h[h$tipo_hallazgo == "valor_fuera_dominio" & !is.na(h$columna) &
      h$columna == columna, , drop = FALSE]
}

# El hallazgo de la lectura: su evidencia lo nombra.
.lectura_unitaria <- function(perfil, columna = "proporcion") {
  h <- .dominio_unitaria(perfil, columna)
  h[grepl("Lectura de la escala de 0 a 1", h$evidencia, fixed = TRUE), ,
    drop = FALSE]
}

.n_lectura_unitaria <- function(valores, nombre = "proporcion", ...) {
  datos <- data.frame(x = valores)
  names(datos) <- nombre
  nrow(.lectura_unitaria(.perfil_unitaria(datos, ...), nombre))
}

.filas_traza_unitaria <- function(h) {
  if (!nrow(h)) return(integer())
  sort(unlist(lapply(h$trazabilidad, function(t) as.integer(t$indices_fila))))
}

.dos_lecturas <- "Si la escala es de 0 a 1, los que pasan de 1 quedan fuera de su dominio; si es de 0 a 100, no."

test_that("una proporcion de 0 a 1 con un 1.5: lo medido y las dos lecturas, sin acusar", {
  datos <- data.frame(proporcion = c(.interiores_unitaria(99L), 1.5))
  p <- .perfil_unitaria(datos)
  h <- .dominio_unitaria(p)
  expect_equal(nrow(h), 1L)
  expect_identical(as.character(h$severidad), "ok")
  expect_equal(h$n_afectados, 1)
  expect_equal(h$n_evaluados, 100)
  expect_identical(h$unidad_conteo, "fila")
  expect_identical(.filas_traza_unitaria(h), 100L)
  expect_true(startsWith(
    h$evidencia, "Escala de 0 a 1 [minimo=0, maximo=1], le"
  ))
  expect_true(grepl(
    "de 100 valores distintos de 0 y de 1, 99 est", h$evidencia, fixed = TRUE
  ))
  expect_true(grepl("en (0, 1) y 1 en (1, 100] (1.5)", h$evidencia, fixed = TRUE))
  expect_true(grepl(.dos_lecturas, h$evidencia, fixed = TRUE))
  # No dice que sea un error ni que la columna mezcle escalas.
  textos <- paste(h$descripcion, h$evidencia, h$sugerencia)
  expect_false(grepl("error", textos, fixed = TRUE))
  expect_false(grepl("mezcl", textos, fixed = TRUE))
  # El dominio inferido que se publica sigue siendo el de [0, 100].
  inferido <- p$meta$dominios$inferidos$proporcion
  expect_identical(inferido$maximo, 100)
  expect_identical(inferido$tipo_dominio, "porcentaje")
  expect_null(inferido$contenido_unitario)
})

test_that("sin umbral de separacion: el 5 y el 10 % de valores sobre 1 tambien se dicen", {
  bajo <- .interiores_unitaria
  # Con el 5 % y el 10 % arriba de 1 la lectura sigue: no hay corte que
  # distinga un error de un valor legitimo.
  expect_equal(.n_lectura_unitaria(c(bajo(95L), rep(1.5, 5L))), 1L)
  expect_equal(.n_lectura_unitaria(c(bajo(90L), rep(1.5, 10L))), 1L)
  expect_equal(.n_lectura_unitaria(c(bajo(950L), rep(1.5, 50L))), 1L)
  h <- .lectura_unitaria(.perfil_unitaria(
    data.frame(proporcion = c(bajo(90L), seq(1.01, 1.5, length.out = 10L)))
  ))
  expect_equal(h$n_afectados, 10)
  expect_true(grepl("90 est", h$evidencia, fixed = TRUE))
  expect_true(grepl("y 10 en (1, 100] (de 1.01 a 1.5)", h$evidencia, fixed = TRUE))
  # Un porcentaje continuo de valores chicos (sin hueco en 1) recibe el mismo
  # aviso, con las mismas dos lecturas: no se lo acusa.
  set.seed(20261141)
  x <- exp(stats::rnorm(200L, 0, 1.5))
  pct <- pmin(x / stats::quantile(x, 0.96, names = FALSE, type = 7), 100)
  hp <- .lectura_unitaria(
    .perfil_unitaria(data.frame(participacion_mercado = pct)),
    "participacion_mercado"
  )
  expect_equal(nrow(hp), 1L)
  expect_identical(as.character(hp$severidad), "ok")
  expect_true(grepl(.dos_lecturas, hp$evidencia, fixed = TRUE))
})

test_that("la escala de 0 a 1 tiene que ser plausible: la mayoria de lo que no es 0 ni 1 en (0, 1)", {
  bajo <- .interiores_unitaria
  expect_equal(.n_lectura_unitaria(c(bajo(51L), rep(1.5, 49L))), 1L)
  expect_equal(.n_lectura_unitaria(c(bajo(50L), rep(1.5, 50L))), 0L)
  chico <- function(n) seq(0.01, 0.9, length.out = n)
  grande <- function(n, desde = 10) seq(desde, 100, length.out = n)
  casos <- list(
    c(chico(10L), grande(90L)), c(chico(20L), grande(80L)),
    c(chico(50L), grande(50L)), c(rep(0, 10L), grande(90L)),
    c(rep(1, 10L), grande(90L)),
    c(seq(0.05, 0.95, length.out = 50L), seq(1.6, 10, length.out = 50L)),
    c(bajo(50L), seq(2, 50, length.out = 50L))
  )
  for (valores in casos) {
    expect_equal(.n_lectura_unitaria(valores), 0L)
  }
  # Lo que cae fuera de [0, 100] tambien cuenta como contenido fuera de (0, 1).
  expect_equal(.n_lectura_unitaria(c(bajo(50L), 1.5, seq(150, 500, length.out = 49L))), 0L)
  # 0 a 1000 y 0 a 500: el aviso de siempre, solo.
  mil <- data.frame(proporcion = c(
    rep(0.5, 100L), seq(2, 100, length.out = 100L),
    seq(150, 1000, length.out = 300L), rep(0, 500L)
  ))
  h <- .dominio_unitaria(.perfil_unitaria(mil))
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_afectados, 300)
  quinientos <- data.frame(proporcion = c(
    rep(0.5, 10L), seq(10, 100, length.out = 9L),
    seq(150, 500, length.out = 39L), rep(0, 42L)
  ))
  h500 <- .dominio_unitaria(.perfil_unitaria(quinientos))
  expect_equal(nrow(h500), 1L)
  expect_equal(h500$n_afectados, 39)
  # La participacion 90/10 con un hueco: la mezcla la dice `escala_mixta` y el
  # aviso no repite sus filas.
  mercado <- data.frame(participacion_mercado = c(
    seq(0.01, 0.9, length.out = 90L), seq(10, 100, length.out = 10L)
  ))
  pm <- .perfil_unitaria(mercado)
  expect_true(any(pm$hallazgos$tipo_hallazgo == "escala_mixta"))
  expect_equal(nrow(.dominio_unitaria(pm, "participacion_mercado")), 0L)
})

test_that("un 0 o un 1 no cuentan: una binaria con pocos intermedios y un 1.5 recibe el aviso", {
  set.seed(20261152)
  binaria <- function(k) {
    v <- rep(c(0, 1), 500L)
    v[sample(1000L, k)] <- stats::runif(k, 0.01, 0.99)
    v[[which(v > 0 & v < 1)[[1L]]]] <- 1.5
    v
  }
  for (k in c(3L, 5L, 10L, 20L)) {
    h <- .lectura_unitaria(.perfil_unitaria(data.frame(proporcion = binaria(k))))
    expect_equal(nrow(h), 1L)
    expect_equal(h$n_afectados, 1)
    expect_true(grepl("iguales a 0 o a 1 valen en las dos escalas", h$evidencia,
                      fixed = TRUE))
  }
  # Un solo intermedio contra un 1.5 no hace plausible la escala de 0 a 1.
  expect_equal(.n_lectura_unitaria(binaria(2L)), 0L)
  # Un entero 0/1 no tiene nada estrictamente entre 0 y 1.
  expect_equal(.n_lectura_unitaria(rep(0:1, 50L)), 0L)
})

test_that("los centinelas que el perfil cuenta como faltantes disfrazados no entran en las cuentas", {
  bajo <- .interiores_unitaria
  uno <- data.frame(proporcion = c(bajo(95L), rep(1.5, 4L), -9))
  p1 <- .perfil_unitaria(uno)
  expect_equal(p1$columnas$n_faltantes_disfrazados, 1)
  h1 <- .lectura_unitaria(p1)
  expect_equal(h1$n_afectados, 4)
  expect_true(grepl("de 99 valores distintos de 0 y de 1, 95 est", h1$evidencia,
                    fixed = TRUE))
  expect_true(grepl("el faltante disfrazado no cuenta", h1$evidencia,
                    fixed = TRUE))
  veinte <- data.frame(proporcion = c(bajo(95L), rep(1.5, 4L), rep(-9, 20L)))
  h20 <- .dominio_unitaria(.perfil_unitaria(veinte))
  # El de [0, 100] sigue diciendo los -9, como siempre; el aviso, los 1.5.
  expect_equal(sort(h20$n_afectados), c(4, 20))
  expect_true(grepl("de 99 valores distintos de 0 y de 1",
                    h20$evidencia[h20$n_afectados == 4], fixed = TRUE))
})

test_that("el borde de 1 decide con el valor exacto y se escribe con las cifras que hagan falta", {
  bajo <- .interiores_unitaria
  lectura <- function(extra) {
    .lectura_unitaria(.perfil_unitaria(
      data.frame(proporcion = c(bajo(95L), rep(extra, 3L)))
    ))
  }
  # Igual a 1 con la tolerancia con que el paquete compara dobles.
  for (extra in c(1 + .Machine$double.eps, 1 + sqrt(.Machine$double.eps),
                  1 + 1e-12)) {
    expect_equal(nrow(lectura(extra)), 0L)
  }
  # Fuera de esa tolerancia ya no es 1, aunque con cuatro cifras se escriba 1.
  h7 <- lectura(1 + 1e-7)
  expect_equal(h7$n_afectados, 3)
  expect_true(grepl("3 en (1, 100] (1.0000001)", h7$evidencia, fixed = TRUE))
  expect_equal(lectura(1.0004)$n_afectados, 3)
  h <- lectura(1.001)
  expect_equal(nrow(h), 1L)
  expect_true(grepl("3 en (1, 100] (1.001)", h$evidencia, fixed = TRUE))
  a <- lectura(1.5)
  b <- lectura(1.5 + .Machine$double.eps)
  expect_identical(a$evidencia, b$evidencia)
  expect_identical(a$n_afectados, b$n_afectados)
})

test_that("el borde de 100 es el de [0, 100] y se escribe igual en los dos hallazgos", {
  bajo <- .interiores_unitaria
  # 99.996 queda adentro y no se escribe como 100.
  h <- .lectura_unitaria(.perfil_unitaria(
    data.frame(proporcion = c(bajo(96L), rep(1.5, 4L), 99.996))
  ))
  expect_equal(h$n_afectados, 5)
  expect_true(grepl("(de 1.5 a 99.996)", h$evidencia, fixed = TRUE))
  # 100.004 queda afuera, en el hallazgo de [0, 100] y no en el aviso.
  afuera <- .dominio_unitaria(.perfil_unitaria(
    data.frame(proporcion = c(bajo(96L), rep(1.5, 4L), 100.004))
  ))
  expect_equal(sort(afuera$n_afectados), c(1, 4))
  expect_identical(
    .filas_traza_unitaria(afuera[as.character(afuera$severidad) == "sospechoso", ]),
    101L
  )
  # El aviso de columna de [0, 100] tampoco escribe 100.004 como 100.
  columna <- data.frame(proporcion = c(
    rep(0.5, 10L), seq(10, 100, length.out = 9L), 100.004,
    seq(150, 500, length.out = 38L), rep(0, 42L)
  ))
  h500 <- .dominio_unitaria(.perfil_unitaria(columna))
  expect_equal(h500$n_afectados, 39)
  expect_true(grepl("Los valores de afuera van de 100.004 a 500", h500$evidencia,
                    fixed = TRUE))
  # Exactamente 100 esta adentro y se escribe 100.
  h100 <- .lectura_unitaria(.perfil_unitaria(
    data.frame(proporcion = c(bajo(96L), 1.5, 100))
  ))
  expect_true(grepl("(de 1.5 a 100)", h100$evidencia, fixed = TRUE))
})

test_that("un nombre que dice porcentaje no se lee en la escala de 0 a 1", {
  bajo <- .interiores_unitaria
  valores <- c(bajo(96L), rep(1.5, 4L))
  for (nombre in c("participacion_porcentual", "proporcion_porcentual",
                   "porcentual_participacion", "participacion_tanto_por_ciento",
                   "participacionPorcentual", "proporcion_porcentuales",
                   "proporcion_por_ciento", "proporcion_percentage",
                   "proporcion_pct", "participacion_pct", "porcentaje",
                   "pct_aprobados", "tasa_respuesta", "prop", "prop_aprobados")) {
    expect_equal(.n_lectura_unitaria(valores, nombre), 0L)
  }
  for (nombre in c("proporcion", "participacion", "ProporcionMercado",
                   "proporcion_de_la_venta", "PROPORCION")) {
    expect_equal(.n_lectura_unitaria(valores, nombre), 1L)
  }
  # Una regla declarada no se lee: la declaracion manda.
  expect_equal(.n_lectura_unitaria(
    valores, dominios = list(proporcion = list(minimo = 0, maximo = 100))
  ), 0L)
})

test_that("un hecho, una voz: los hallazgos de [0, 100] no cambian y nadie repite filas", {
  datos <- data.frame(proporcion = c(
    seq(0, 1, length.out = 96L), 1.5, 2, -0.2, 150
  ))
  h <- .dominio_unitaria(.perfil_unitaria(datos))
  expect_equal(nrow(h), 2L)
  sospechoso <- h[as.character(h$severidad) == "sospechoso", , drop = FALSE]
  expect_equal(sospechoso$n_afectados, 2)
  expect_identical(.filas_traza_unitaria(sospechoso), c(99L, 100L))
  expect_identical(
    sospechoso$evidencia,
    "Dominio inclusivo [minimo=0, maximo=100]; fuente: nombre_y_contenido; filas afectadas: 2."
  )
  nuevo <- .lectura_unitaria(.perfil_unitaria(datos))
  expect_identical(.filas_traza_unitaria(nuevo), c(97L, 98L))
  expect_true(grepl("2 quedan fuera de [0, 100]", nuevo$evidencia, fixed = TRUE))
})

test_that("no repite lo que escala_mixta nombra, con todas sus filas aunque su traza se corte", {
  datos <- data.frame(proporcion = c(
    .interiores_unitaria(39000L), seq(10, 90, length.out = 1100L), rep(1.2, 3L)
  ))
  p <- .perfil_unitaria(datos)
  mixta <- p$hallazgos[p$hallazgos$tipo_hallazgo == "escala_mixta", , drop = FALSE]
  expect_equal(mixta$n_afectados, 1100)
  expect_true(isTRUE(mixta$trazabilidad[[1L]]$truncado))
  h <- .lectura_unitaria(p)
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_afectados, 3)
  expect_identical(.filas_traza_unitaria(h), 40101:40103)
  expect_true(grepl("1100 de esos los nombra `escala_mixta`", h$evidencia,
                    fixed = TRUE))
  campos <- unique(unlist(lapply(p$hallazgos$trazabilidad, names)))
  expect_false("indices_completos" %in% campos)
  expect_false("posicion_columna" %in% campos)
  todo <- data.frame(proporcion = c(
    .interiores_unitaria(3900L), seq(10, 90, length.out = 100L)
  ))
  pt <- .perfil_unitaria(todo)
  expect_true(any(pt$hallazgos$tipo_hallazgo == "escala_mixta"))
  expect_equal(nrow(.dominio_unitaria(pt)), 0L)
})

test_that("la identidad es la posicion de la columna, no su nombre", {
  datos <- data.frame(
    proporcion = c(.interiores_unitaria(3900L), seq(10, 90, length.out = 100L),
                   rep(1.2, 3L)),
    proporcion = c(.interiores_unitaria(3900L), rep(1.2, 103L)),
    check.names = FALSE
  )
  h <- .lectura_unitaria(.perfil_unitaria(datos))
  expect_equal(nrow(h), 2L)
  expect_setequal(h$n_afectados, c(3, 103))
})

.cambios_unitaria <- function(a, b, ...) {
  d <- as.data.frame(suppressWarnings(suppressMessages(
    comparar_perfiles(a, b, ...)
  )))
  de_dominio <- (d$aspecto %in% "hallazgo" &
                   (d$valor_anterior %in% "valor_fuera_dominio" |
                      d$valor_actual %in% "valor_fuera_dominio")) |
    (d$aspecto %in% c("conteo_hallazgo", "severidad_hallazgo") &
       startsWith(d$evidencia, "valor_fuera_dominio:"))
  d[de_dominio, , drop = FALSE]
}

test_that("comparar_perfiles compara el aviso con su par y deja los de siempre como estaban", {
  fa <- as.Date("2026-11-18")
  fb <- as.Date("2026-11-19")
  entrega <- function(n_neg, n_alto, n_unitaria, fecha) {
    .perfil_unitaria(data.frame(proporcion = c(
      .interiores_unitaria(2000L - n_neg - n_alto - n_unitaria),
      rep(-0.2, n_neg), seq(200, 500, length.out = n_alto),
      rep(1.5, n_unitaria)
    )), fecha)
  }
  d <- .cambios_unitaria(entrega(1L, 1L, 10L, fa), entrega(1L, 1L, 60L, fb),
                         umbral_cambio = 0.01)
  expect_equal(nrow(d), 1L)
  expect_identical(d$aspecto, "conteo_hallazgo")
  expect_identical(d$cambio, "agravado")
  expect_identical(d$valor_anterior, "10 de 2000")
  expect_identical(d$valor_actual, "60 de 2000")
  d2 <- .cambios_unitaria(entrega(1L, 1L, 10L, fa), entrega(39L, 1L, 10L, fb),
                          umbral_cambio = 0.01)
  expect_equal(nrow(d2), 1L)
  expect_identical(d2$valor_anterior, "2 de 2000")
  expect_identical(d2$valor_actual, "40 de 2000")
  d3 <- .cambios_unitaria(entrega(1L, 1L, 0L, fa), entrega(1L, 1L, 10L, fb))
  expect_equal(nrow(d3), 1L)
  expect_identical(d3$cambio, "aparecido")
  expect_equal(nrow(.cambios_unitaria(entrega(1L, 1L, 10L, fa),
                                      entrega(1L, 1L, 10L, fb))), 0L)
})

test_that("mientras los valores de arriba sigan, la comparacion no los da por resueltos", {
  bajo <- .interiores_unitaria
  fa <- as.Date("2026-11-18")
  fb <- as.Date("2026-11-19")
  pf <- function(v, f) .perfil_unitaria(data.frame(proporcion = v), f)
  resuelto <- function(a, b) {
    d <- .cambios_unitaria(pf(a, fa), pf(b, fb))
    any(d$cambio %in% "resuelto")
  }
  base <- c(bajo(96L), rep(1.5, 4L))
  expect_false(resuelto(base, c(bajo(95L), rep(1.5, 5L))))
  expect_false(resuelto(base, c(bajo(90L), rep(1.5, 10L))))
  expect_false(resuelto(c(bajo(95L), rep(1.5, 4L)), c(bajo(95L), rep(1.5, 4L), -9)))
  expect_false(resuelto(c(bajo(95L), rep(1.5, 4L)), c(bajo(95L), rep(1.5, 4L), -9, -9, -9)))
  expect_false(resuelto(c(bajo(95L), rep(1.5, 4L)), c(bajo(95L), rep(1.5, 4L), 150)))
  expect_false(resuelto(c(base, 150), c(bajo(90L), rep(1.5, 10L), 150)))
  # Y cuando los valores de arriba se van, si.
  expect_true(resuelto(base, bajo(100L)))
})


test_that("se cuenta con el valor exacto: 1.00004 y 0.99996 no son 1, 1 + 1e-12 si", {
  bajo <- .interiores_unitaria
  lectura <- function(valores) {
    .lectura_unitaria(.perfil_unitaria(data.frame(proporcion = valores)))
  }
  h5 <- lectura(c(bajo(95L), rep(1.5, 4L), 1.00004))
  expect_equal(h5$n_afectados, 5)
  expect_true(grepl(
    "de 100 valores distintos de 0 y de 1, 95 est", h5$evidencia, fixed = TRUE
  ))
  expect_true(grepl("y 5 en (1, 100] (de 1.00004 a 1.5)", h5$evidencia,
                    fixed = TRUE))
  expect_false(grepl("iguales a 0 o a 1", h5$evidencia, fixed = TRUE))
  expect_false(grepl("el igual a 0 o a 1", h5$evidencia, fixed = TRUE))
  h_bajo <- lectura(c(bajo(95L), rep(1.5, 4L), 0.99996))
  expect_equal(h_bajo$n_afectados, 4)
  expect_true(grepl(
    "de 100 valores distintos de 0 y de 1, 96 est", h_bajo$evidencia, fixed = TRUE
  ))
  h10 <- lectura(c(bajo(95L), rep(1.5, 4L), rep(1.00004, 4L), rep(1.5, 2L)))
  expect_equal(h10$n_afectados, 10)
  expect_true(grepl("y 10 en (1, 100]", h10$evidencia, fixed = TRUE))
  veinte <- c(bajo(95L), rep(1.5, 4L), rep(1.00004, 20L))
  h24 <- lectura(veinte)
  expect_equal(h24$n_afectados, 24)
  expect_true(grepl("de 119 valores distintos de 0 y de 1, 95 est", h24$evidencia,
                    fixed = TRUE))
  # El plan revisa las 24, no 4.
  datos <- data.frame(proporcion = veinte)
  plan <- as.data.frame(suppressWarnings(suppressMessages(
    planificar_limpieza(.perfil_unitaria(datos), datos = datos)
  )))
  expect_equal(
    plan$n_afectadas[plan$hallazgo %in% "valor_fuera_dominio"], 24
  )
  # Solo valores apenas sobre 1: ya no callan.
  expect_equal(lectura(c(bajo(95L), rep(1.00004, 20L)))$n_afectados, 20)
  # 1 + 1e-12 es 1: no cuenta, y lo dice.
  h_uno <- lectura(c(bajo(95L), rep(1.5, 4L), 1 + 1e-12))
  expect_equal(h_uno$n_afectados, 4)
  expect_true(grepl("el igual a 0 o a 1 vale en las dos escalas", h_uno$evidencia,
                    fixed = TRUE))
})

test_that("resuelto solo si los valores en (1, 100] se fueron; si siguen, modificado", {
  bajo <- .interiores_unitaria
  fa <- as.Date("2026-11-18")
  fb <- as.Date("2026-11-19")
  pf <- function(v, f) .perfil_unitaria(data.frame(proporcion = v), f)
  a <- pf(c(bajo(96L), rep(1.5, 4L)), fa)
  expect_equal(nrow(.lectura_unitaria(a)), 1L)
  para <- function(v) .cambios_unitaria(a, pf(v, fb))
  # 4 -> 100 valores 1.5: la mayoria paso a (1, 100] y la escala de 0 a 1 dejo
  # de ser plausible; el aviso no esta, pero los valores siguen.
  b_cien <- pf(c(bajo(96L), rep(1.5, 100L)), fb)
  expect_equal(nrow(.lectura_unitaria(b_cien)), 0L)
  registro <- b_cien$meta$dominios$escala_unitaria
  expect_equal(registro$n_arriba, 100)
  expect_false(registro$plausible)
  for (b in list(c(bajo(96L), rep(1.5, 100L)),
                 c(bajo(96L), seq(1.01, 100, length.out = 100L)),
                 c(bajo(50L), rep(1.5, 50L)),
                 c(bajo(96L), rep(1.5, 4L), rep(150, 97L)))) {
    d <- para(b)
    expect_false(any(d$cambio %in% "resuelto"))
    fila <- d[d$cambio %in% "modificado", , drop = FALSE]
    expect_equal(nrow(fila), 1L)
    expect_identical(as.character(fila$severidad), "sospechoso")
    expect_true(grepl("los valores que pasan de 1 siguen", fila$descripcion,
                      fixed = TRUE))
  }
  expect_true(grepl("hay 100 en (1, 100] y 96 en (0, 1)",
                    para(c(bajo(96L), rep(1.5, 100L)))$descripcion[[1L]],
                    fixed = TRUE))
  # Si los valores en (1, 100] se van, si es resuelto.
  d_fin <- para(bajo(100L))
  expect_true(any(d_fin$cambio %in% "resuelto"))
  expect_false(any(d_fin$cambio %in% "modificado"))
  # Una proporcion sin valores sobre 1 no guarda cuentas: el meta es el de
  # siempre.
  expect_null(pf(bajo(100L), fb)$meta$dominios$escala_unitaria)
})

test_that("el id de un hallazgo de siempre no cambia cuando aparece el aviso", {
  a <- data.frame(proporcion = c(.interiores_unitaria(990L), rep(-0.2, 10L)))
  b <- data.frame(proporcion = c(.interiores_unitaria(960L), rep(-0.2, 30L),
                                 rep(1.5, 10L)))
  pa <- .perfil_unitaria(a)
  pb <- .perfil_unitaria(b)
  ids <- function(p) {
    h <- identificar_hallazgos(p, nombre_tabla = "t")
    h$id_hallazgo[h$tipo_hallazgo == "valor_fuera_dominio"]
  }
  expect_identical(ids(pa), "t||proporcion|valor_fuera_dominio")
  expect_setequal(ids(pb), c("t||proporcion|valor_fuera_dominio",
                             "t||proporcion|valor_fuera_dominio|escala_0_1"))
  dec <- suppressWarnings(suppressMessages(decidir_hallazgos(
    pa, "falso_positivo", motivo = "prueba", quien = "equipo",
    columna = "proporcion", tipo_hallazgo = "valor_fuera_dominio", datos = a,
    nombre_tabla = "t"
  )))
  pd <- suppressWarnings(suppressMessages(aplicar_decisiones(
    pb, dec, datos = b, nombre_tabla = "t"
  )))
  e <- as.data.frame(hallazgos(pd))
  e <- e[e$tipo_hallazgo == "valor_fuera_dominio", , drop = FALSE]
  expect_identical(
    e$estado_decision[as.character(e$severidad) == "sospechoso"], "revisar"
  )
  expect_identical(
    e$estado_decision[as.character(e$severidad) == "ok"], "pendiente"
  )
})

test_that("se decide en un lugar: los grupos de perfilar_por y los motores dicen lo mismo que una tabla con ese contenido", {
  datos <- data.frame(
    g = rep(c("A", "B"), c(100L, 100L)),
    proporcion = c(.interiores_unitaria(99L), 1.5, seq(2, 100, length.out = 100L))
  )
  tabla <- .perfil_unitaria(datos[, "proporcion", drop = FALSE])
  expect_equal(nrow(.lectura_unitaria(tabla)), 0L)
  solo_a <- .lectura_unitaria(.perfil_unitaria(
    datos[datos$g == "A", "proporcion", drop = FALSE]
  ))
  por <- as.data.frame(suppressWarnings(suppressMessages(perfilar_por(
    datos, "g", min_filas = 1L, fecha = .fecha_unitaria,
    analizar_dependencias = FALSE
  ))))
  por <- por[por$tipo_hallazgo == "valor_fuera_dominio" &
               grepl("Lectura de la escala de 0 a 1", por$evidencia, fixed = TRUE), ,
             drop = FALSE]
  expect_identical(as.character(por$grupo), "A")
  expect_identical(por$n_afectados, solo_a$n_afectados)
  expect_identical(por$evidencia, solo_a$evidencia)
  motor <- data.frame(id = 1:200, proporcion = c(.interiores_unitaria(198L), 1.5, 2))
  memoria <- .lectura_unitaria(.perfil_unitaria(motor))
  expect_equal(memoria$n_afectados, 2)
  conexiones <- list()
  if (requireNamespace("RSQLite", quietly = TRUE)) {
    conexiones$sqlite <- function() DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  }
  if (requireNamespace("duckdb", quietly = TRUE)) {
    conexiones$duckdb <- function() {
      DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
    }
  }
  skip_if(!length(conexiones), "sin RSQLite ni duckdb")
  for (nombre in names(conexiones)) {
    con <- conexiones[[nombre]]()
    DBI::dbWriteTable(con, "t", motor)
    p <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, "t", muestra = Inf, fecha = .fecha_unitaria,
      analizar_dependencias = FALSE, instrumentar = FALSE
    )))
    DBI::dbDisconnect(con, shutdown = identical(nombre, "duckdb"))
    h <- .lectura_unitaria(p$perfil_muestra)
    expect_identical(h$n_afectados, memoria$n_afectados)
    expect_identical(h$evidencia, memoria$evidencia)
    expect_identical(.filas_traza_unitaria(h), .filas_traza_unitaria(memoria))
  }
})
