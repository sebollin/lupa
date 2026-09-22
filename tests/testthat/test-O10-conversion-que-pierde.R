# O10. Una conversion que pierde algo lo dice con todas sus marcas.
#
#   * La marca `destructiva` se calculaba solo cuando la columna era segura.
#     Sobre `10%` junto a `0.1` -no lo es- el plan media cinco valores
#     irrecuperables, su propio motivo decia "Se declara destructiva", y la
#     marca quedaba en FALSE: activada a mano, el aviso de acciones
#     destructivas no aparecia y el registro repetia FALSE.
#
#   * La guarda de redondeo miraba solo enteros. `33.333333333333333333%`
#     tiene 20 cifras, el `double` guarda 17, y la conversion se recomendaba y
#     aplicaba sola como reversible. A la precision con que vino escrito, todo
#     decimal de hasta 15 cifras vuelve exacto: la guarda no acusa a una
#     columna comun con decimales.

.o10_plan <- function(valores) {
  datos <- data.frame(v = valores, stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(datos), datos = datos)
  fila <- plan$estrategia == "convertir_numero_regional"
  list(datos = datos, plan = plan, fila = fila)
}

test_that("una perdida medida marca la accion destructiva aunque la columna no sea segura", {
  caso <- .o10_plan(c("10%", "0.1", "10%", "5%", "0.05"))
  expect_equal(sum(caso$fila), 1L)
  plan <- caso$plan
  # La premisa: la columna no es segura y la perdida esta medida.
  expect_false(plan$recomendada[caso$fila])
  expect_equal(plan$parametros[caso$fila][[1L]]$n_no_reversibles, 5L)
  expect_false(plan$reversible[caso$fila])
  # Lo que se promete: lo que no es reversible se marca destructivo.
  expect_true(plan$destructiva[caso$fila])

  plan$aplicar <- caso$fila
  expect_message(print(plan), "destructivas activas")
  registro <- as.data.frame(aplicar(plan, caso$datos)$registro)
  expect_true(registro$destructiva)
  expect_equal(registro$n_no_reversibles, 5L)
})

test_that("una columna que solo espera configuracion no se declara destructiva", {
  caso <- .o10_plan(c("1.234", "2.345", "3.456", "4.567", "5.678", "6.789"))
  expect_equal(sum(caso$fila), 1L)
  expect_false(caso$plan$recomendada[caso$fila])
  expect_false(caso$plan$destructiva[caso$fila])
})

test_that("un decimal que no entra en un double no se declara reversible", {
  caso <- .o10_plan(c("33.333333333333333333%", "66.666666666666666666%",
                      "12.345678901234567890%"))
  expect_equal(sum(caso$fila), 1L)
  plan <- caso$plan
  expect_false(plan$reversible[caso$fila])
  expect_false(plan$recomendada[caso$fila])
  expect_false(plan$aplicar[caso$fila])
  expect_true(plan$destructiva[caso$fila])
  expect_equal(plan$parametros[caso$fila][[1L]]$n_no_reversibles, 3L)
  expect_match(plan$parametros[caso$fila][[1L]]$motivo_no_reversible,
               "se redondear", fixed = TRUE)
})

test_that("la guarda de decimales no acusa a una columna comun", {
  # El borde: 15 cifras significativas siempre vuelven exactas, y tambien las
  # de 16 que el double representa. Un control que solo se ve dar TRUE no
  # distingue una guarda que funciona de una que no mira.
  comunes <- list(
    c("1.234,56", "2,5", "3,14159", "0,75", "7"),
    c("10.5%", "20.25%", "30.125%", "40%"),
    c("0.123456789012345", "0.2", "0.3"),
    c("1234.56", "0.1", "0.2", "99.99")
  )
  for (valores in comunes) {
    caso <- .o10_plan(valores)
    expect_equal(sum(caso$fila), 1L, info = valores[[1L]])
    expect_true(caso$plan$reversible[caso$fila], info = valores[[1L]])
    expect_equal(caso$plan$parametros[caso$fila][[1L]]$n_no_reversibles, 0L,
                 info = valores[[1L]])
  }
})
