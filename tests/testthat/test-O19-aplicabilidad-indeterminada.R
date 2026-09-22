# O19. Lo indeterminado no es lo que no corresponde.
#
# La documentacion de `aplicabilidad` promete: "Las filas donde el predicado no
# se puede determinar se declaran aparte, sin contarse ni como aplicables ni
# como no aplicables". Se contaban dos veces: bien como
# `n_aplicabilidad_indeterminada`, y mal como
# `n_presentes_fuera_de_aplicabilidad`, que disparaba el hallazgo
# `valor_fuera_de_aplicabilidad` -"la regla declarada dice que no corresponde"-
# sobre filas donde la regla no dijo que no, dijo que no sabe. La mascara
# `!is.na(crudo) & crudo` mete el indeterminado en el mismo saco que el `FALSE`.
#
# Y el mismo hallazgo publicaba su universo al reves: con una regla que deja el
# universo vacio decia "1000 afectados de 0 evaluados", porque tomaba como
# denominador el universo aplicable cuando cuenta filas de AFUERA de el.

.o19_datos <- function(indeterminadas = 250L) {
  n <- 1000L
  grupos <- rep(c("Si", "No", NA, "Si"), each = n / 4L)
  data.frame(
    tiene_auto = grupos,
    gasto = c(
      seq_len(n / 4L), rep(NA_integer_, n / 4L),
      seq_len(n / 4L), seq_len(n / 4L)
    ),
    stringsAsFactors = FALSE
  )
}

test_that("una fila indeterminada no se publica como valor fuera del universo", {
  datos <- .o19_datos()
  perfil <- perfilar(datos, aplicabilidad = list(gasto = ~ tiene_auto == "Si"))
  fila <- perfil$columnas[perfil$columnas$columna == "gasto", , drop = FALSE]

  expect_equal(fila$n_aplicables, 500)
  expect_equal(fila$n_no_aplica, 250)
  expect_equal(fila$n_aplicabilidad_indeterminada, 250)
  # Las 250 filas indeterminadas tienen valor presente y NO son filas de fuera.
  expect_equal(fila$n_presentes_fuera_de_aplicabilidad, 0)
  expect_equal(fila$n_presentes_en_aplicabilidad_indeterminada, 250)

  hallazgo <- as.data.frame(hallazgos(perfil))
  expect_false(any(hallazgo$tipo_hallazgo == "valor_fuera_de_aplicabilidad"))

  # Lo que no se pudo decidir se declara, no se calla.
  cobertura <- perfil$cobertura_diagnosticos
  fila_cobertura <- cobertura[
    cobertura$diagnostico == "valor_fuera_de_aplicabilidad", , drop = FALSE
  ]
  expect_equal(nrow(fila_cobertura), 1L)
  expect_match(fila_cobertura$motivo, "no se pudo determinar", fixed = TRUE)
})

test_that("un valor donde la regla si dice que no corresponde sigue siendo hallazgo", {
  # El control: sin filas indeterminadas, el hallazgo tiene que aparecer igual
  # que antes, o este arreglo estaria tapando el error que el paquete persigue.
  datos <- data.frame(
    tiene_auto = rep(c("Si", "No"), each = 100L),
    gasto = c(seq_len(100L), seq_len(100L)),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, aplicabilidad = list(gasto = ~ tiene_auto == "Si"))
  fila <- perfil$columnas[perfil$columnas$columna == "gasto", , drop = FALSE]
  expect_equal(fila$n_presentes_fuera_de_aplicabilidad, 100)
  expect_equal(fila$n_aplicabilidad_indeterminada, 0)

  hallazgo <- as.data.frame(hallazgos(perfil))
  fila_hallazgo <- hallazgo[
    hallazgo$tipo_hallazgo == "valor_fuera_de_aplicabilidad", , drop = FALSE
  ]
  expect_equal(nrow(fila_hallazgo), 1L)
  expect_equal(fila_hallazgo$n_afectados, 100)
  # El denominador son las filas que pudieron producir el error, no el universo
  # aplicable: antes publicaba afectados sobre un universo que los excluia.
  expect_equal(fila_hallazgo$n_evaluados, 100)
  expect_false(any(perfil$cobertura_diagnosticos$diagnostico ==
                     "valor_fuera_de_aplicabilidad"))
})

test_that("con el universo vacio el hallazgo no dice mil de cero", {
  datos <- data.frame(
    tiene_auto = rep("No", 200L), gasto = seq_len(200L),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, aplicabilidad = list(gasto = ~ tiene_auto == "Si"))
  hallazgo <- as.data.frame(hallazgos(perfil))
  fila <- hallazgo[
    hallazgo$tipo_hallazgo == "valor_fuera_de_aplicabilidad", , drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$n_afectados, 200)
  expect_equal(fila$n_evaluados, 200)
})

test_that("sin aplicabilidad declarada no cambia nada", {
  datos <- data.frame(a = c(1, NA, 3), b = c("x", "y", NA), stringsAsFactors = FALSE)
  perfil <- perfilar(datos)
  fila <- perfil$columnas[perfil$columnas$columna == "a", , drop = FALSE]
  expect_equal(fila$n_presentes_fuera_de_aplicabilidad, 0)
  expect_equal(fila$n_presentes_en_aplicabilidad_indeterminada, 0)
  expect_false(any(perfil$cobertura_diagnosticos$diagnostico ==
                     "valor_fuera_de_aplicabilidad"))
})
