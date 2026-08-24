# La huella de mascaras desbordaba el entero y perdia su tercer numero justo en
# las tablas grandes, que es donde comparar cuesta.

test_that("la huella no desborda ni pierde su tercer numero en tablas grandes", {
  # `idx * idx` desborda el entero a partir de la fila 46.341. El aviso era lo
  # menos importante: el tercer componente salia `NA`, asi que la huella pasaba
  # de tres numeros a dos sin que nada lo dijera.
  mascara <- rep(FALSE, 60000L)
  mascara[c(10L, 50000L, 55000L)] <- TRUE
  expect_silent(huella <- .huella_mascara(mascara))
  expect_false(grepl("NA", huella, fixed = TRUE))
  expect_equal(length(strsplit(huella, "-", fixed = TRUE)[[1L]]), 3L)
})

test_that("dos mascaras distintas de filas altas no comparten huella", {
  # El caso medido: las dos daban `2-105000-NA` porque el unico componente que
  # las separaba era el que el desborde habia anulado.
  a <- rep(FALSE, 60000L); a[c(50000L, 55000L)] <- TRUE
  b <- rep(FALSE, 60000L); b[c(51000L, 54000L)] <- TRUE
  expect_false(identical(.huella_mascara(a), .huella_mascara(b)))
})

test_that("la huella sigue siendo la misma cuenta en el rango donde no desbordaba", {
  # La reduccion antes de multiplicar es `(a*a) mod p == ((a mod p)^2) mod p`,
  # asi que por debajo del desborde el resultado no cambia. Se comprueba contra
  # la formula vieja, calculada aqui en doble para que no desborde ella misma.
  mascara <- rep(FALSE, 1000L)
  mascara[c(3L, 17L, 400L, 999L)] <- TRUE
  idx <- which(mascara)
  viejo <- paste(
    length(idx), sum(idx %% 1000003),
    sum((as.numeric(idx) * as.numeric(idx)) %% 1000003), sep = "-"
  )
  expect_equal(.huella_mascara(mascara), viejo)
})
