# Un dependiente casi constante alcanza el umbral con cualquier determinante:
# el cumplimiento es la proporcion de su moda, agrupe lo que agrupe. Esos pares
# no describen ninguna regla y no se publican.

tabla_estado_casi_constante <- function() {
  set.seed(2582)
  n <- 5000L
  data.frame(
    depto = sample(sprintf("D%d", 1:6), n, TRUE),
    turno = sample(c("M", "T", "N"), n, TRUE),
    rubro = sample(sprintf("R%02d", 1:12), n, TRUE),
    lote = sample(sprintf("L%02d", 1:20), n, TRUE),
    estado = ifelse(seq_len(n) %in% sample(n, 17L), "BAJA", "ACTIVO"),
    stringsAsFactors = FALSE
  )
}

test_that("un determinante que no mejora la moda del dependiente no se publica", {
  d <- tabla_estado_casi_constante()
  # A mano: la moda sola ya acierta 4983 de 5000 filas, por encima de 0,995.
  expect_equal(max(table(d$estado)) / nrow(d), 0.9966)
  dep <- detectar_dependencias(d)
  expect_false(any(dep$dependiente == "estado"))
  expect_false(any(perfilar(d)$dependencias$dependiente == "estado"))
})

test_that("un determinante que si explica el estado casi constante se publica", {
  d <- tabla_estado_casi_constante()
  # Un motivo con veinte valores repartidos, y uno mas -el cierre- que lleva
  # 12 de las 17 bajas: acierta mas que la moda sola, sin ser exacto.
  d$motivo <- sample(sprintf("M%02d", 1:20), nrow(d), TRUE)
  d$motivo[which(d$estado == "BAJA")[1:12]] <- "CIERRE"
  dep <- detectar_dependencias(d)
  fila <- dep[dep$determinante == "motivo" & dep$dependiente == "estado", ]
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$n_violaciones, 5L)
  expect_false(fila$exacta)
  expect_false(any(dep$dependiente == "estado" & dep$determinante != "motivo"))
})
