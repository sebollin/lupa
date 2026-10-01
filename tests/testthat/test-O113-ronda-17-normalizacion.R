# Ronda 17-A: la normalizacion de texto y lo que empareja.

.N_O113 <- function(x, ...) lupa:::.normalizacion_aplicar(x, normalizacion(...))

test_that("la enie y la dieresis se protegen tambien en mayuscula", {
  # La salida va descompuesta: se comparan salidas entre si. Antes "A\u00d1O"
  # daba "ano" y "a\u00f1o", "a\u00f1o".
  expect_identical(.N_O113("A\u00d1O"), .N_O113("a\u00f1o"))
  expect_false(identical(.N_O113("A\u00d1O"), .N_O113("ano")))
  expect_identical(.N_O113("G\u00dcIRALDES"), .N_O113("g\u00fciraldes"))
  # Sin bajar la caja, la mayuscula y la tilde se conservan.
  sin_bajar <- .N_O113("A\u00d1O", minusculas = FALSE)
  expect_true(startsWith(sin_bajar, "A"))
  expect_true(0x0303L %in% utf8ToInt(sin_bajar))
  claves <- detectar_claves(data.frame(x = c("PE\u00d1A", "Pe\u00f1a")), max_combinacion = 1)
  expect_false(claves$unicidad_normalizada)
  # Control: lo que no lleva enie sigue igualandose.
  claves <- detectar_claves(data.frame(x = c("PENA", "Pena")), max_combinacion = 1)
  expect_false(claves$unicidad_normalizada)
})

test_that("bajar la caja no depende del locale fuera del latin", {
  original <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", original)), add = TRUE)
  x <- c("\u041c\u041e\u0421\u041a\u0412\u0410", "\u043c\u043e\u0441\u043a\u0432\u0430", "\u039f\u039d\u039f\u039c\u0391", "\u03bf\u03bd\u03bf\u03bc\u03b1", "\uff21\uff22\uff23", "\uff41\uff42\uff43")
  resultados <- lapply(c("C", "es_UY.UTF-8", "en_US.UTF-8"), function(locale) {
    puesto <- suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
    if (!nzchar(puesto)) return(NULL)
    list(
      pliegue = lupa:::.normalizacion_minusculas_vector(x),
      clave = lupa:::.clave_sin_escritura(x)
    )
  })
  resultados <- Filter(Negate(is.null), resultados)
  expect_gte(length(resultados), 1L)
  for (r in resultados) {
    expect_identical(r$pliegue, c("\u043c\u043e\u0441\u043a\u0432\u0430", "\u043c\u043e\u0441\u043a\u0432\u0430", "\u03bf\u03bd\u03bf\u03bc\u03b1", "\u03bf\u03bd\u03bf\u03bc\u03b1", "\uff41\uff42\uff43", "\uff41\uff42\uff43"))
    expect_identical(r$clave[[1L]], r$clave[[2L]])
    expect_true(nzchar(r$clave[[3L]]))
  }
})

test_that("dos textos con bytes invalidos distintos no son el mismo valor", {
  inv <- c(rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9))),
           rawToChar(as.raw(c(0x6e, 0x69, 0xf1, 0x6f))))
  claves <- detectar_claves(data.frame(x = inv, stringsAsFactors = FALSE),
                            max_combinacion = 1)
  # Antes: un solo valor distinto, igual a "".
  expect_true(claves$unicidad_normalizada)
  expect_equal(claves$n_distintos_normalizados, 2L)
})

test_that("la ligadura de s larga y t es st, y las comillas no dependen del camino", {
  expect_identical(.N_O113("ca\ufb05a", ligaduras = TRUE), "casta")
  # Control: la otra ligadura de st.
  expect_identical(.N_O113("ca\ufb06a", ligaduras = TRUE), "casta")
  # El mismo valor da la misma clave con una enie al lado o sin ella: antes el
  # camino escalar y el vectorial respondian distinto.
  con <- .N_O113("abc\u2019\u201d \u00f1andu")
  sin <- .N_O113("abc\u2019\u201d nandu")
  expect_identical(sub(" .*$", "", con), sub(" .*$", "", sin))
})

test_that("un valor no finge el separador de una clave compuesta", {
  d <- data.frame(c1 = c("a\u001fb", "a", "a", "e"),
                  c2 = c("c", "b\u001fc", "d", "c"), stringsAsFactors = FALSE)
  par <- detectar_claves(d, max_combinacion = 2, normalizar = FALSE)
  par <- par[par$columnas == "c1 + c2", ]
  # Antes: cuatro filas distintas contadas como tres.
  expect_equal(par$n_distintos_exactos, 4L)
  expect_true(par$unicidad_normalizada)
})
