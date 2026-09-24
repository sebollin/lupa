# O42: `guardar_analisis()` vuelve a aplicar la proteccion antes de escribir, y
# el objeto releido quedaba contradiciendose: la moda salia
# `[valor protegido]` y `dato_personal_protegido` decia `FALSE`. Quien filtra
# por esa marca para saber que columnas traen evidencia enmascarada no
# encontraba ninguna, sobre un archivo que si las ocultaba.

.analisis_con_documentos <- function() {
  datos <- data.frame(
    cedula = c(12345678, 87654321, 87654321, 13579246, 24681357, 11223344)
  )
  analizar(datos, analizar_dependencias = FALSE,
           proteger_datos_personales = FALSE)
}

test_that("al releer, la marca acompania al valor enmascarado", {
  sin_proteger <- .analisis_con_documentos()
  destino <- tempfile(fileext = ".rds")
  on.exit(unlink(destino), add = TRUE)

  suppressMessages(guardar_analisis(sin_proteger, destino))
  releido <- leer_analisis(destino)
  fila <- releido$perfil$columnas[
    releido$perfil$columnas$columna == "cedula", ,
    drop = FALSE
  ]

  expect_equal(as.character(fila$moda[[1L]]), "[valor protegido]")
  expect_true(isTRUE(fila$dato_personal_protegido[[1L]]))
  expect_true(isTRUE(releido$meta$persistencia$evidencia_protegida))
})

test_that("el releido dice lo mismo que la proteccion nativa", {
  # Control: las dos puertas producen la misma declaracion sobre la columna.
  sin_proteger <- .analisis_con_documentos()
  destino <- tempfile(fileext = ".rds")
  on.exit(unlink(destino), add = TRUE)
  suppressMessages(guardar_analisis(sin_proteger, destino))
  releido <- leer_analisis(destino)

  nativo <- analizar(
    data.frame(
      cedula = c(12345678, 87654321, 87654321, 13579246, 24681357, 11223344)
    ),
    analizar_dependencias = FALSE
  )

  campo <- function(x) {
    fila <- x$perfil$columnas[x$perfil$columnas$columna == "cedula", ,
                              drop = FALSE]
    c(moda = as.character(fila$moda[[1L]]),
      protegido = as.character(fila$dato_personal_protegido[[1L]]))
  }

  expect_equal(campo(releido), campo(nativo))
})

test_that("un analisis sin columnas personales no se marca protegido", {
  # Control: la marca sigue al enmascarado, no al acto de guardar.
  datos <- data.frame(monto = c(10, 20, 30, 40, 50, 60))
  analisis <- analizar(datos, analizar_dependencias = FALSE,
                       proteger_datos_personales = FALSE)
  destino <- tempfile(fileext = ".rds")
  on.exit(unlink(destino), add = TRUE)

  suppressMessages(guardar_analisis(analisis, destino))
  releido <- leer_analisis(destino)
  fila <- releido$perfil$columnas[
    releido$perfil$columnas$columna == "monto", ,
    drop = FALSE
  ]

  expect_false(isTRUE(fila$dato_personal_protegido[[1L]]))
  expect_false(identical(as.character(fila$moda[[1L]]), "[valor protegido]"))
})
