# `sentinelas_numericos = numeric(0)` declara que no hay centinelas: es una
# politica, y la fila `configuracion_sentinelas_numericos` de
# `comparar_perfiles()` la publicaba como texto vacio, `modificado` sin decir a
# que. Las otras politicas que son listas -`cadenas_ausencia`, la gemela,
# `aplicabilidad` y `columnas_opcionales`- escriben la lista vacia "ninguna", y
# los centinelas tienen que decirlo igual. ASCII.
testthat::skip_on_cran()

.perfil_lvc <- function(d, fecha, ...) {
  suppressWarnings(suppressMessages(perfilar(
    d, fecha = as.POSIXct(fecha, tz = "UTC"), analizar_dependencias = FALSE, ...
  )))
}

.fila_lvc <- function(deriva, aspecto) {
  deriva[deriva$aspecto == aspecto, , drop = FALSE]
}

test_that("una lista de centinelas vacia se publica como la de cadenas de ausencia", {
  datos <- data.frame(
    id = 1:30, edad = c(rep(30, 28), 999, 40),
    ciudad = c(rep("Salto", 27), "N/A", "Melo", "Rivera"),
    stringsAsFactors = FALSE
  )
  con_centinelas <- .perfil_lvc(datos, "2026-01-01")
  sin_centinelas <- .perfil_lvc(datos, "2026-02-01", sentinelas_numericos = numeric(0))
  expect_identical(length(sin_centinelas$meta$sentinelas_numericos), 0L)

  directa <- .fila_lvc(
    comparar_perfiles(con_centinelas, sin_centinelas),
    "configuracion_sentinelas_numericos"
  )
  expect_equal(nrow(directa), 1L)
  expect_identical(as.character(directa$cambio), "modificado")
  expect_false(is.na(directa$valor_actual))
  expect_true(nzchar(directa$valor_actual))

  inversa <- .fila_lvc(
    comparar_perfiles(
      .perfil_lvc(datos, "2026-01-01", sentinelas_numericos = numeric(0)),
      .perfil_lvc(datos, "2026-02-01")
    ),
    "configuracion_sentinelas_numericos"
  )
  expect_equal(nrow(inversa), 1L)
  expect_identical(inversa$valor_anterior, directa$valor_actual)

  # La gemela: la lista de cadenas de ausencia retirada.
  cadenas <- .fila_lvc(
    comparar_perfiles(
      .perfil_lvc(datos, "2026-01-01", cadenas_ausencia = "N/A"),
      .perfil_lvc(datos, "2026-02-01")
    ),
    "configuracion_cadenas_ausencia"
  )
  expect_equal(nrow(cadenas), 1L)
  expect_identical(directa$valor_actual, cadenas$valor_actual)
  expect_identical(directa$valor_actual, "ninguna")
})

test_that("una lista de centinelas con valores se sigue publicando entera", {
  # Control: los valores, separados por `; `, como antes.
  datos <- data.frame(id = 1:30, edad = c(rep(30, 28), 999, 40))
  fila <- .fila_lvc(
    comparar_perfiles(
      .perfil_lvc(datos, "2026-01-01"),
      .perfil_lvc(datos, "2026-02-01", sentinelas_numericos = c(999, -9))
    ),
    "configuracion_sentinelas_numericos"
  )
  expect_equal(nrow(fila), 1L)
  expect_identical(fila$valor_anterior, "-9999; -999; -99; -9; 999")
  expect_identical(fila$valor_actual, "-9; 999")
})
