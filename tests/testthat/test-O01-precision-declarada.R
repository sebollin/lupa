# O01. Una conversión que redondea no puede declararse reversible.
#
# `convertir_numero_regional`, marcada `recomendada`, convertía
# `9007199254740993` en `9007199254740992` -`as.numeric()` no puede con un
# entero por encima de 2^53- y el plan publicaba `reversible = TRUE`,
# `destructiva = FALSE` y `n_no_reversibles = 0`. Ningún formato recupera el
# 3 perdido.
#
# La comprobacion de reversibilidad buscaba COLISIONES: dos originales
# distintos que terminan en el mismo valor. Acá no hay colisión -un solo valor
# se mueve y nadie más cae ahí-, así que no la veía.
#
# Es la misma regla que el perfil ya aplica, que por encima de 2^53 se niega a
# medir la secuencia entera en vez de publicar un número redondeado.

.o01_plan <- function(valores) {
  datos <- data.frame(v = valores, id = seq_along(valores), stringsAsFactors = FALSE)
  list(
    datos = datos,
    plan = planificar_limpieza(
      perfilar(datos, analizar_dependencias = FALSE), datos
    )
  )
}

.o01_fila <- function(plan) {
  which(as.character(plan$estrategia) == "convertir_numero_regional")
}

test_that("un entero que no entra en un double se declara no reversible", {
  armado <- .o01_plan(c("9007199254740993", "1.5", "2.5", "3.5"))
  fila <- .o01_fila(armado$plan)
  expect_true(length(fila) > 0L)

  expect_false(any(as.logical(armado$plan$reversible[fila])))
  expect_true(any(as.logical(armado$plan$destructiva[fila])))
  expect_false(any(as.logical(armado$plan$recomendada[fila])))

  # Y si el usuario la aplica igual, el registro tiene que contarlo.
  plan <- armado$plan
  plan$aplicar <- as.character(plan$estrategia) == "convertir_numero_regional"
  registro <- as.data.frame(aplicar(plan, armado$datos)$registro)
  expect_true(registro$n_no_reversibles[[1L]] >= 1L)
})

test_that("la guarda se prueba con el valor donde cambia, no con la magnitud", {
  # 2^53 = 9007199254740992 ES representable exacto, y 2^53+1 redondea
  # JUSTO a 2^53. Una comprobacion `> 2^53` sobre el valor convertido da FALSE
  # sobre el unico caso que hay que atrapar: por eso se comparan los digitos.
  exacto <- .o01_plan(c("9007199254740992", "1.5", "2.5", "3.5"))
  fila_exacto <- .o01_fila(exacto$plan)
  expect_true(length(fila_exacto) > 0L)
  expect_true(all(as.logical(exacto$plan$reversible[fila_exacto])))

  anterior <- .o01_plan(c("9007199254740991", "1.5", "2.5", "3.5"))
  fila_anterior <- .o01_fila(anterior$plan)
  expect_true(length(fila_anterior) > 0L)
  expect_true(all(as.logical(anterior$plan$reversible[fila_anterior])))
})

test_that("la guarda no molesta a las conversiones corrientes", {
  for (caso in list(
    c("1.5", "2.5", "3.5", "4.5"),
    c("1.234,5", "2.345,5", "3.456,5", "4.567,5"),
    c("-12", "34", "-56", "78,9")
  )) {
    armado <- .o01_plan(caso)
    fila <- .o01_fila(armado$plan)
    if (!length(fila)) next
    expect_true(all(as.logical(armado$plan$reversible[fila])),
                info = paste(caso, collapse = ","))
  }
})

test_that("declarar una accion destructiva obliga a decir por que", {
  armado <- .o01_plan(c("9007199254740993", "1.5", "2.5", "3.5"))
  fila <- .o01_fila(armado$plan)
  expect_true(length(fila) > 0L)
  justificacion <- as.character(armado$plan$justificacion[fila])[[1L]]
  # Sin el motivo, el texto salia con un punto suelto adelante -"`. Se declara
  # destructiva`"- y el usuario no tenia nada con que decidir.
  expect_false(grepl("^\\s*\\.", justificacion))
  expect_true(grepl("doble", justificacion, fixed = TRUE))
  expect_true(grepl("recuperar", justificacion, fixed = TRUE))
})

test_that("convertir ausencias textuales no afirma que no infiere el dominio", {
  datos <- data.frame(
    v = c("NA", "NA", "Namibia", "Nepal", "Peru", "NA"),
    id = seq_len(6L), stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(perfilar(datos, analizar_dependencias = FALSE), datos)
  fila <- grep("ausencias", as.character(plan$estrategia))
  expect_true(length(fila) > 0L)
  justificacion <- as.character(plan$justificacion[fila])[[1L]]
  # Decidir que `NA` es una ausencia y no el codigo de Namibia ES inferir el
  # dominio. El paquete puede reconocer el token, no su significado en esta
  # columna, y tiene que decir cual de las dos cosas hizo.
  expect_false(grepl("sin inferir el dominio", justificacion, fixed = TRUE))
  expect_true(grepl("no es reversible", justificacion, fixed = TRUE))
  expect_false(any(as.logical(plan$reversible[fila])))
})
