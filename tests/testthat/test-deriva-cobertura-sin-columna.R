# Una fila de `cobertura_diagnosticos` sin columna -`columna = NA`- dice que el
# diagnostico no se evaluo en ninguna parte de la tabla, y entonces tampoco en
# cada columna. Una entrega sin filas declina `columnas_duplicadas` asi (dos
# columnas vacias coinciden sin que eso sea evidencia), y la comparacion leia
# esa fila como de la tabla: el grupo de columnas iguales del perfil anterior,
# que es un hallazgo de columnas, salia `resuelto` con severidad `ok`. Una
# entrega vacia no es rara en una serie mensual. ASCII.
testthat::skip_on_cran()

.perfil_csc <- function(d, fecha) {
  suppressWarnings(suppressMessages(perfilar(
    d, fecha = as.POSIXct(fecha, tz = "UTC"), analizar_dependencias = FALSE
  )))
}

.fila_grupo_csc <- function(deriva) {
  deriva[deriva$aspecto == "hallazgo" &
           deriva$valor_anterior %in% "columnas_duplicadas", , drop = FALSE]
}

test_that("un grupo de columnas iguales contra una entrega sin filas sale no_evaluado", {
  anterior <- .perfil_csc(
    data.frame(x = c(1, 2, 3), y = c(1, 2, 3), z = c(5, 6, 7)), "2026-01-01"
  )
  actual <- .perfil_csc(
    data.frame(x = numeric(), y = numeric(), z = numeric()), "2026-02-01"
  )
  expect_true("columnas_duplicadas" %in% anterior$hallazgos$tipo_hallazgo)
  cobertura <- actual$cobertura_diagnosticos
  sin_columna <- cobertura$diagnostico == "columnas_duplicadas"
  expect_equal(sum(sin_columna), 1L)
  expect_true(is.na(cobertura$columna[sin_columna]))

  fila <- .fila_grupo_csc(comparar_perfiles(anterior, actual))
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$columna), "x")
  expect_identical(as.character(fila$cambio), "no_evaluado")
  expect_identical(as.character(fila$severidad), "sospechoso")
})

test_that("el grupo que se separa de verdad, con filas, sigue saliendo resuelto", {
  # Control: la regla no declina lo que el perfil nuevo si comparo. Las mismas
  # columnas con filas, y `y` distinta de `x` en la ultima: el grupo se separo y
  # la cobertura no declina nada.
  anterior <- .perfil_csc(
    data.frame(x = c(1, 2, 3), y = c(1, 2, 3), z = c(5, 6, 7)), "2026-01-01"
  )
  actual <- .perfil_csc(
    data.frame(x = c(1, 2, 3), y = c(1, 2, 4), z = c(5, 6, 7)), "2026-02-01"
  )
  expect_false("columnas_duplicadas" %in% actual$cobertura_diagnosticos$diagnostico)
  fila <- .fila_grupo_csc(comparar_perfiles(anterior, actual))
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$cambio), "resuelto")
  expect_identical(as.character(fila$severidad), "ok")
})

test_that("la entrega sin filas antes de la que tiene el grupo no cambia de lectura", {
  # El sentido inverso no pasa por la cobertura del perfil nuevo: el grupo
  # aparece, como antes.
  anterior <- .perfil_csc(
    data.frame(x = numeric(), y = numeric(), z = numeric()), "2026-01-01"
  )
  actual <- .perfil_csc(
    data.frame(x = c(1, 2, 3), y = c(1, 2, 3), z = c(5, 6, 7)), "2026-02-01"
  )
  deriva <- comparar_perfiles(anterior, actual)
  fila <- deriva[deriva$aspecto == "hallazgo" &
                   deriva$valor_actual %in% "columnas_duplicadas", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$cambio), "aparecido")
})
