# Las acciones que quitan controles o normalizan espacios Unicode trabajan sobre
# las celdas que pueden cambiar, y el conteo de lo que se pierde arma grupos solo
# con los valores de las celdas que cambiaron. Sobre una columna grande con pocas
# celdas que tocar, las dos cosas recorrian la columna entera.

test_that("quitar un control decodifica solo las celdas que pueden traerlo", {
  skip_on_cran()
  x <- c(as.character(1:5000), paste0("12", intToUtf8(0x200BL), "3"))
  llamadas <- 0L
  original <- .codigos_decodificables
  local_mocked_bindings(.codigos_decodificables = function(texto) {
    llamadas <<- llamadas + 1L
    original(texto)
  })
  r <- .quitar_controles_invisibles(x)
  expect_identical(r$valor, c(as.character(1:5000), "123"))
  expect_identical(r$n, 1L)
  # Antes se decodificaba cada una de las 5.001 celdas.
  expect_lte(llamadas, 3L)
})

test_that("normalizar espacios Unicode no recorre las celdas ASCII", {
  skip_on_cran()
  x <- c(as.character(1:5000), paste0("12", intToUtf8(0xA0L), "3"))
  llamadas <- 0L
  original <- .codigos_decodificables
  local_mocked_bindings(.codigos_decodificables = function(texto) {
    llamadas <<- llamadas + 1L
    original(texto)
  })
  r <- .normalizar_espacios_invisibles(x)
  expect_identical(r$valor, c(as.character(1:5000), "12 3"))
  expect_identical(r$n, 1L)
  expect_lte(llamadas, 3L)
})

test_that("una celda latin1 con un espacio duro suelto sigue normalizandose", {
  skip_on_cran()
  celda <- rawToChar(as.raw(c(0x31, 0x32, 0xA0, 0x33)))
  Encoding(celda) <- "latin1"
  r <- .normalizar_espacios_invisibles(c("1", "2", celda))
  expect_identical(r$n, 1L)
  expect_identical(enc2utf8(r$valor[[3L]]), "12 3")
})

test_that("lo que se pierde se cuenta igual con grupos que no cambiaron", {
  skip_on_cran()
  zw <- intToUtf8(0x200BL)
  # La celda cambiada choca con una que no cambio: dos valores distintos
  # quedan en uno, y la que cambio es la que perdio el suyo.
  r <- .quitar_controles_invisibles(c("ana", paste0("an", zw, "a"), "beto", "beto"))
  expect_identical(r$n, 1L)
  expect_identical(r$n_no_reversibles, 1L)
  # Un grupo repetido sin celdas cambiadas no pierde nada.
  r <- .quitar_controles_invisibles(c("beto", "beto", paste0("x", zw)))
  expect_identical(r$n_no_reversibles, 0L)
})
