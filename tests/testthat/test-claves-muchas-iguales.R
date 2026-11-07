# Muchas copias de un identificador: cada una es una clave simple, y ninguna
# combinacion que la contenga es minima. Armar esas combinaciones para
# descartarlas era cubico en el numero de copias.

copias_de_un_id <- function(k, n = 200L) {
  set.seed(k)
  id <- sample(n)
  d <- as.data.frame(stats::setNames(
    replicate(k, id, simplify = FALSE), sprintf("c%03d", seq_len(k))
  ))
  d$g1 <- rep(1:20, each = n / 20L)
  d$g2 <- rep(1:10, n / 10L)
  d$g3 <- rep(c("a", "b", "c", "d"), n / 4L)
  d
}

test_that("las copias de un identificador se publican como claves equivalentes", {
  k <- 12L
  r <- detectar_claves(copias_de_un_id(k))
  simples <- r[r$n_columnas == 1L, ]
  expect_equal(nrow(simples), k)
  expect_true(all(simples$redundante))
  # A mano: cada copia es equivalente a todas las demas, en el orden de la tabla.
  nombres <- sprintf("c%03d", seq_len(k))
  for (i in seq_len(k)) {
    esperado <- paste(nombres[c(seq_len(k)[seq_len(k) > i], seq_len(k)[seq_len(k) < i])],
                      collapse = ", ")
    expect_equal(simples$equivalente_a[[i]], esperado, info = i)
  }
  expect_equal(nrow(attr(r, "claves_redundantes")), k * (k - 1L) / 2L)
  # La compuesta de las columnas que no son clave sigue apareciendo.
  expect_true("g1 + g2" %in% r$columnas)
})

test_that("cientos de copias de un identificador no arman las combinaciones que contienen una", {
  skip_on_cran()
  d <- copias_de_un_id(150L)
  tiempo <- system.time(r <- detectar_claves(d))[["elapsed"]]
  expect_equal(sum(r$n_columnas == 1L), 150L)
  expect_equal(nrow(attr(r, "claves_redundantes")), 150L * 149L / 2L)
  # Antes, con 100 copias, 36 s; con 150, mas de dos minutos.
  expect_lt(tiempo, 30)
})
