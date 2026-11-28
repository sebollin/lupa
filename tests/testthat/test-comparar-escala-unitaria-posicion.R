# Cuando la lectura de la escala de 0 a 1 de una proporcion estaba en el perfil
# anterior y no en el nuevo, `comparar_perfiles()` decide con la columna
# apareada por su posicion (las homonimas, por su orden) y con las cuentas de
# esa columna, no con las de todas las que se llaman igual. Si el perfil nuevo
# no dejo constancia de haber evaluado la lectura en esa columna, no se sabe si
# los valores siguen: la fila es `no_comparable`, no `resuelto`.

testthat::skip_on_cran()

.fa_posicion <- as.Date("2026-11-18")
.fb_posicion <- as.Date("2026-11-19")

.perfil_posicion <- function(datos, fecha) {
  suppressWarnings(suppressMessages(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE
  )))
}

.bajo_posicion <- function(n) seq(0.01, 0.99, length.out = n)

# Las filas de la comparacion que hablan de un `valor_fuera_dominio`.
.dominio_comparado <- function(a, b) {
  d <- as.data.frame(suppressWarnings(suppressMessages(comparar_perfiles(a, b))))
  d[d$aspecto %in% "hallazgo" &
      (d$valor_anterior %in% "valor_fuera_dominio" |
         d$valor_actual %in% "valor_fuera_dominio"), , drop = FALSE]
}

.homonimas_posicion <- function(...) {
  columnas <- list(...)
  datos <- as.data.frame(columnas, check.names = FALSE)
  names(datos) <- rep("proporcion", length(columnas))
  datos
}

test_that("con homonimas se lee la columna apareada por posicion, no la suma de todas", {
  bajo <- .bajo_posicion
  con_aviso <- c(bajo(96L), rep(1.5, 4L))
  # A: una sola `proporcion`, con el aviso. B: dos; la primera, la apareada,
  # quedo limpia; la segunda tiene 50/50 y no publica aviso.
  a <- .perfil_posicion(data.frame(proporcion = con_aviso), .fa_posicion)
  b <- .perfil_posicion(.homonimas_posicion(bajo(100L), c(bajo(50L), rep(1.5, 50L))),
                        .fb_posicion)
  d <- .dominio_comparado(a, b)
  expect_false(any(d$cambio %in% "modificado"))
  expect_true(any(d$cambio %in% "resuelto"))
  # A y B con dos homonimas: el aviso era de la primera, que quedo limpia.
  a2 <- .perfil_posicion(.homonimas_posicion(con_aviso, bajo(100L)), .fa_posicion)
  b2 <- .perfil_posicion(.homonimas_posicion(bajo(100L), c(bajo(50L), rep(1.5, 50L))),
                         .fb_posicion)
  d2 <- .dominio_comparado(a2, b2)
  expect_false(any(d2$cambio %in% "modificado"))
  # Tres en B: la primera limpia, la segunda no plausible y la tercera con
  # `escala_mixta`. Nada de cuentas sumadas ni del motivo de otra columna.
  b3 <- .perfil_posicion(.homonimas_posicion(
    bajo(4000L), c(bajo(2000L), seq(1.01, 100, length.out = 2000L)),
    c(bajo(3900L), seq(10, 90, length.out = 100L))
  ), .fb_posicion)
  d3 <- .dominio_comparado(a2, b3)
  expect_false(any(d3$cambio %in% "modificado"))
  expect_false(any(grepl("8000", d3$descripcion, fixed = TRUE)))
  # Si la apareada sigue con valores en (1, 100], son sus cuentas, solas.
  b4 <- .perfil_posicion(.homonimas_posicion(
    c(bajo(96L), rep(1.5, 100L)), c(bajo(98L), rep(1.5, 98L))
  ), .fb_posicion)
  d4 <- .dominio_comparado(a2, b4)
  fila <- d4[d4$cambio %in% "modificado", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_true(grepl("hay 100 en (1, 100] y 96 en (0, 1), de 196 valores",
                    fila$descripcion, fixed = TRUE))
})

test_that("sin constancia de que el perfil nuevo evaluo la lectura, no es resuelto", {
  bajo <- .bajo_posicion
  datos <- data.frame(proporcion = c(bajo(96L), rep(1.5, 4L)))
  a <- .perfil_posicion(datos, .fa_posicion)
  b <- .perfil_posicion(datos, .fb_posicion)
  expect_identical(b$meta$dominios$lectura_escala_unitaria$evaluadas$posicion_columna, 1L)
  # Un perfil de una version que no guardaba la lectura: los mismos datos, sin
  # la constancia ni las cuentas.
  viejo <- b
  viejo$meta$dominios$lectura_escala_unitaria <- NULL
  viejo$meta$dominios$escala_unitaria <- NULL
  viejo$hallazgos <- viejo$hallazgos[
    !grepl("Lectura de la escala de 0 a 1", viejo$hallazgos$evidencia, fixed = TRUE),
    , drop = FALSE
  ]
  d <- .dominio_comparado(a, viejo)
  expect_false(any(d$cambio %in% "resuelto"))
  fila <- d[d$cambio %in% "no_comparable", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$severidad), "sospechoso")
  expect_true(grepl("no evalu", fila$descripcion, fixed = TRUE))
  # Un perfil nuevo sano, con la constancia y sin valores en (1, 100]: resuelto.
  sano <- .perfil_posicion(data.frame(proporcion = bajo(100L)), .fb_posicion)
  expect_identical(sano$meta$dominios$lectura_escala_unitaria$evaluadas$posicion_columna, 1L)
  d_sano <- .dominio_comparado(a, sano)
  expect_true(any(d_sano$cambio %in% "resuelto"))
  expect_false(any(d_sano$cambio %in% c("no_comparable", "modificado")))
  # La constancia no es una politica de dominios: no hay fila de configuracion.
  todo <- as.data.frame(suppressWarnings(suppressMessages(comparar_perfiles(a, sano))))
  expect_false(any(todo$aspecto %in% "configuracion_dominios"))
})

test_that("el modificado cierra sus cuentas: dice tambien los que quedan fuera de [0, 100]", {
  bajo <- .bajo_posicion
  a <- .perfil_posicion(data.frame(proporcion = c(bajo(96L), rep(1.5, 4L))),
                        .fa_posicion)
  b <- .perfil_posicion(
    data.frame(proporcion = c(bajo(96L), rep(1.5, 4L), rep(150, 97L))),
    .fb_posicion
  )
  fila <- .dominio_comparado(a, b)
  fila <- fila[fila$cambio %in% "modificado", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_true(grepl(
    "hay 4 en (1, 100], 96 en (0, 1) y 97 fuera de [0, 100], de 197 valores",
    fila$descripcion, fixed = TRUE
  ))
})
