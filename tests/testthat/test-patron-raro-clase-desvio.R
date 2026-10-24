# `patron_raro` señala valores que no siguen el patrón dominante. Hay dos clases
# de desvío que se leen muy distinto y hasta ahora se veían iguales:
#
#   estructural        `SIN CODIGO` frente a `AB-12345`: otra forma
#   largo_de_corrida   `persona9@` frente a `persona300@`: la misma forma con
#                      un número de otro largo
#
# El segundo es casi siempre inofensivo en correos, URL o pesos, y casi siempre
# grave en una cédula. Están medidos y son indistinguibles por la forma, así que
# **la severidad no cambia**: lo que cambia es que el hallazgo lo declara y quien
# lo lee lo resuelve de un vistazo.

.evidencia_patron_raro <- function(valores) {
  hallazgos <- perfilar(
    data.frame(v = valores, stringsAsFactors = FALSE),
    proteger_datos_personales = FALSE
  )$hallazgos
  fila <- hallazgos[as.character(hallazgos$tipo_hallazgo) == "patron_raro", ]
  expect_equal(nrow(fila), 1L)
  fila
}

test_that("un desvío que sólo cambia el largo de una corrida se declara", {
  correos <- .evidencia_patron_raro(sprintf("persona%d@x.uy", 1:300))
  expect_true(grepl("clase_desvio=largo_de_corrida", correos$evidencia, fixed = TRUE))
  # La severidad no se toca: el caso legítimo y el sospechoso son
  # indistinguibles por la forma.
  expect_equal(as.character(correos$severidad), "sospechoso")
  expect_equal(correos$n_afectados, 9)

  urls <- .evidencia_patron_raro(sprintf("https://a.uy/r/%d", 1:300))
  expect_true(grepl("clase_desvio=largo_de_corrida", urls$evidencia, fixed = TRUE))
})

test_that("un desvío de otra estructura se declara como tal", {
  codigos <- .evidencia_patron_raro(
    c(sprintf("AB-%05d", 1:290), rep("SIN CODIGO", 10L))
  )
  expect_true(grepl("clase_desvio=estructural", codigos$evidencia, fixed = TRUE))
  expect_false(grepl("largo_de_corrida", codigos$evidencia, fixed = TRUE))
})

test_that("el discriminante separa clases de forma y no largos", {
  # Misma secuencia de clases, distinta multiplicidad: sólo largo.
  expect_true(lupa:::.desvio_solo_por_largo("a+9+@a.a+", "a+9@a.a+"))
  expect_true(lupa:::.desvio_solo_por_largo("A+-9+", "A+-9"))
  # Otra secuencia de clases: estructural.
  expect_false(lupa:::.desvio_solo_por_largo("A+-9+", "A+ A+"))
  expect_false(lupa:::.desvio_solo_por_largo("a+9+", "9+a+"))
  # Idénticos no son desvío.
  expect_false(lupa:::.desvio_solo_por_largo("a+9+", "a+9+"))
})
