
# `fuente_estado` explica de donde sale `estado_prueba`. A MariaDB le habia
# quedado pegado el texto de una fila "esperado" sobre un estado "probado",
# contradiciendo a su propia fila y a los dos README, que dicen "medido contra
# el motor real" con las cifras de la corrida.
test_that("la fuente declarada corresponde con el estado declarado", {
  requisitos <- requisitos_motor()
  probados <- requisitos[requisitos$estado_prueba == "probado", ]
  expect_true(nrow(probados) >= 6L)
  expect_true(
    all(grepl("benchmark/verificar_motor.R", probados$fuente_estado,
              fixed = TRUE)),
    info = paste(probados$motor[!grepl("benchmark/verificar_motor.R",
                                       probados$fuente_estado, fixed = TRUE)],
                 collapse = ", ")
  )
  expect_true(all(!grepl("README", probados$fuente_estado, fixed = TRUE)))
  expect_true(all(!grepl("esperado", probados$fuente_estado, fixed = TRUE)))

  # El control: las filas que SI son esperadas también nombran la corrida que
  # delimita lo que quedó fuera de la matriz actual.
  esperados <- requisitos[requisitos$estado_prueba == "esperado", ]
  expect_true(nrow(esperados) > 0L)
  expect_true(all(grepl("benchmark/verificar_motor.R", esperados$fuente_estado,
                        fixed = TRUE)))
  expect_true(all(!grepl("README", esperados$fuente_estado, fixed = TRUE)))

  bigquery <- requisitos[requisitos$id_motor == "bigquery", ]
  expect_equal(nrow(bigquery), 1L)
  expect_true(grepl("sin medici\u00f3n publicada", bigquery$fuente_estado,
                    fixed = TRUE))
  expect_false(grepl("README", bigquery$fuente_estado, fixed = TRUE))
})
