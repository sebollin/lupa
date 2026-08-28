# La unicidad de una clave declarada se evalua SOLO entre las filas con la clave
# completa. Eso abre un caso que no es ni verdadero ni no verificado: cuando
# ninguna fila tiene la clave completa, la unicidad entre claves completas es
# cierta sobre un conjunto vacio. Decirlo "verificada" seria cierto y enganoso.

.ronda161_clave <- function(datos, clave) {
  suppressWarnings(perfilar(datos, clave = clave))$meta$clave
}

test_that("sin ninguna clave completa la unicidad no se declara verificada", {
  datos <- data.frame(
    a = c(NA_character_, NA_character_, NA_character_),
    b = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )

  clave <- .ronda161_clave(datos, c("a", "b"))

  expect_identical(clave$unicidad$estado, "sin_casos_evaluables")
  expect_identical(clave$unicidad$filas_evaluadas, 0L)
  expect_identical(clave$unicidad$filas_totales, 3L)
  expect_identical(clave$ausencia_nulos$estado, "refutada")
})

test_that("el aviso dice por que no se pudo evaluar, y no que faltaran filas", {
  datos <- data.frame(
    a = c(NA_character_, NA_character_, NA_character_),
    b = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )
  mensaje <- NULL
  withCallingHandlers(
    perfilar(datos, clave = c("a", "b")),
    warning = function(w) {
      mensaje <<- c(mensaje, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  mensaje <- paste(mensaje, collapse = " ")

  expect_match(mensaje, "ninguna de las 3 filas tiene la clave completa")
  # Habia tres filas: decir que no habia es falso.
  expect_false(grepl("no habia filas", mensaje, fixed = TRUE))
  expect_false(grepl("fue verificada", mensaje, fixed = TRUE))
})

test_that("una repeticion entre filas incompletas no refuta la unicidad", {
  # En SQL dos NULL no son iguales: la repeticion de `(NA, "z")` no viola la
  # unicidad. Entre las claves completas -"p" y "q"- no hay ninguna.
  datos <- data.frame(
    a = c("p", "q", NA_character_, NA_character_),
    b = c("x", "y", "z", "z"),
    stringsAsFactors = FALSE
  )

  clave <- .ronda161_clave(datos, c("a", "b"))

  expect_identical(clave$unicidad$estado, "verificada")
  expect_identical(clave$unicidad$filas_evaluadas, 2L)
  expect_equal(clave$unicidad$filas_repetidas, 0)
  # La colision no se pierde: queda en el eje que le corresponde.
  expect_true(clave$trazabilidad$colisiona_con_ausentes)
  expect_equal(clave$trazabilidad$filas_colision_con_ausentes, 2)
})

test_that("una repeticion entre claves completas si la refuta", {
  datos <- data.frame(
    a = c("p", NA_character_, "p"),
    b = c("x", "y", "x"),
    stringsAsFactors = FALSE
  )

  clave <- .ronda161_clave(datos, c("a", "b"))

  expect_identical(clave$unicidad$estado, "refutada")
  expect_identical(clave$unicidad$filas_evaluadas, 2L)
  expect_equal(clave$unicidad$filas_repetidas, 1)
  expect_equal(clave$unicidad$filas_en_colision, 2)
})

test_that("la trazabilidad conserva la semantica de R y la unicidad no", {
  # Son dos preguntas distintas y no pueden compartir etiqueta: la unicidad
  # responde por el universo evaluable, la trazabilidad por como se localizan
  # las filas.
  datos <- data.frame(
    a = c("p", "q", NA_character_, NA_character_),
    b = c("x", "y", "z", "z"),
    stringsAsFactors = FALSE
  )

  clave <- .ronda161_clave(datos, c("a", "b"))

  expect_identical(clave$unicidad$semantica, "claves_completas")
  expect_identical(clave$trazabilidad$semantica, "R")
})
