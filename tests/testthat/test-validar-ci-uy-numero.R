# Una cedula guardada como numero pierde sus ceros iniciales. Con uno perdido
# -`1234561`- la regla de los siete digitos la reconstruia; con dos o mas
# -`1236`, que es `00001236`- se rechazaba, mientras ese mismo documento como
# texto se aceptaba.

.dv_ci_prueba <- function(cuerpo) {
  d <- as.integer(strsplit(sprintf("%07d", cuerpo), "")[[1L]])
  (10L - (sum(d * c(2L, 9L, 8L, 7L, 6L, 3L, 4L)) %% 10L)) %% 10L
}

test_that("un numero valida como la cedula que escribe con sus ceros", {
  cuerpos <- c(1L, 12L, 123L, 1234L, 12345L, 123456L, 1234567L)
  textos <- sprintf("%07d%d", cuerpos,
                    vapply(cuerpos, .dv_ci_prueba, integer(1L)))
  # A mano: 0000123 da 6 con los pesos 2,9,8,7,6,3,4.
  expect_identical(textos[[3L]], "00001236")
  expect_true(all(validar_ci_uy(textos)))
  expect_identical(validar_ci_uy(as.numeric(textos)), rep(TRUE, 7L))
  expect_identical(validar_ci_uy(as.integer(textos)), rep(TRUE, 7L))
  # Y el mismo numero con el digito cambiado sigue sin validar.
  malos <- as.numeric(textos) + ifelse(as.numeric(textos) %% 10 == 9, -1, 1)
  expect_identical(validar_ci_uy(malos), rep(FALSE, 7L))
  skip_if_not_installed("bit64")
  expect_true(validar_ci_uy(bit64::as.integer64("1236")))
})

test_that("el texto corto sigue exigiendo siete u ocho digitos", {
  # El texto guarda los ceros: `"1236"` no es la escritura de una cedula.
  expect_false(validar_ci_uy("1236"))
  expect_false(validar_ci_uy(factor("1236")))
  expect_true(validar_ci_uy("00001236"))
})

test_that("el cuerpo de ceros no es una cedula, ni como numero ni como texto", {
  # Su digito cuadra -(10 - 0) mod 10 es 0-, pero la numeracion empieza en 1:
  # el `0` con que una planilla marca "sin cedula" no es un documento valido.
  expect_identical(validar_ci_uy(c(0, NA, 1236)), c(FALSE, NA, TRUE))
  expect_false(validar_ci_uy("00000000"))
  expect_false(validar_ci_uy("0000000"))
  expect_false(validar_ci_uy("0.000.000-0"))
  # El control: el cuerpo 1 si es una cedula.
  expect_true(validar_ci_uy("00000016"))
})
