# Un producto redondeado con respuestas chicas frente a su cuanto: la tolerancia
# relativa no llega al redondeo en ninguna fila y no reconoce la relacion. Se lee
# con la mitad del cuanto si hay al menos 20 bases y 20 respuestas distintas y
# la relacion explica mas filas que la moda de la respuesta; entre columnas
# independientes chicas, esa lectura publicaba relaciones espurias.

proporcionales_chicas <- function(d) {
  p <- perfilar(d, analizar_dependencias = FALSE,
                fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  h <- p$hallazgos
  h <- h[h$tipo_hallazgo == "relacion_aritmetica_columnas", , drop = FALSE]
  h[grepl("proporcional", h$descripcion, fixed = TRUE), , drop = FALSE]
}

test_that("un producto redondeado a 0,05 con respuestas de miles se publica y cuenta las rotas", {
  set.seed(21)
  n <- 300L
  u <- round(exp(stats::runif(n, log(20), log(1e5 / 0.05))))
  m <- round(u * 0.05 * 0.18 / 0.05)
  base <- as.numeric(sprintf("%.2f", u * 0.05))
  sana <- proporcionales_chicas(data.frame(base = base,
                                           resp = as.numeric(sprintf("%.2f", m * 0.05))))
  expect_equal(nrow(sana), 1L)
  expect_equal(as.character(sana$severidad), "ok")
  expect_equal(sana$n_afectados, 0)
  expect_match(sana$descripcion, "resp ~= base * 0.18", fixed = TRUE)
  # Rotas por un cuanto donde el residuo exacto no cae en la mitad.
  residuo <- m * 1000 - 180 * u
  lejos <- which(abs(residuo) * 2 < 1000 * 0.8)
  rotas <- sort(lejos[c(4L, 40L, 90L, 150L, 200L)])
  m[rotas] <- m[rotas] + c(1, -1, 1, -1, 1)
  rota <- proporcionales_chicas(data.frame(base = base,
                                           resp = as.numeric(sprintf("%.2f", m * 0.05))))
  expect_equal(nrow(rota), 1L)
  expect_equal(sort(rota$trazabilidad[[1L]]$indices_fila), rotas)
})

test_that("un iva redondeado a centavos sobre netos chicos se publica", {
  set.seed(22)
  neto <- round(stats::runif(300, 10, 1000), 2)
  iva <- round(neto * 0.22, 2)
  rel <- proporcionales_chicas(data.frame(neto = neto, iva = iva))
  expect_equal(nrow(rel), 1L)
  expect_equal(rel$n_afectados, 0)
  expect_match(rel$descripcion, "iva ~= neto * 0.22", fixed = TRUE)
})

test_that("entre columnas independientes chicas no aparece una proporcionalidad", {
  set.seed(23)
  # Una binaria "es" la mitad de una columna casi siempre 1.
  d1 <- data.frame(x = sample(1:2, 50, TRUE, prob = c(0.95, 0.05)),
                   y = sample(0:1, 50, TRUE))
  expect_equal(nrow(proporcionales_chicas(d1)), 0L)
  # Una base de rango estrecho "explica" con k = 0.05 la moda de la respuesta.
  d2 <- data.frame(x = round(stats::runif(300, 95, 105), 2),
                   y = ifelse(stats::runif(300) < 0.92, 5, sample(1:100, 300, TRUE)))
  expect_equal(nrow(proporcionales_chicas(d2)), 0L)
  # Y un producto real con pocos valores distintos tampoco: no alcanza la guarda.
  b <- sample(1:10, 300, TRUE)
  expect_equal(nrow(proporcionales_chicas(data.frame(b = b, r = round(b * 0.5)))), 0L)
})
