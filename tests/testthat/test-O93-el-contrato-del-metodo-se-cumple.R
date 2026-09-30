# La parte semantica del contrato de `metodo` se hace cumplir.
#
# `?modelo_calidad` pide una observacion por objeto, la fila de cada medida por
# celda y la entidad a la que corresponde; `medir()` solo validaba la forma. Un
# metodo que devolvia dos filas por celda publicaba seis medidas para tres celdas
# y `agregar(, "ratio")` daba 0,5 donde la respuesta es 0,667.

.o93_medir <- function(metodo, granularidad = "atributo") {
  propia <- metrica("Propia", "Prueba del contrato.", granularidad, "booleano")
  medir(modelo(instanciar(especializar(propia), "t", "x", metodo = metodo)),
        data.frame(x = c(1, 2, 3)))
}

test_that("dos observaciones del mismo objeto se rechazan", {
  duplica <- function(tablas, instancia) {
    data.frame(resultado = c(TRUE, TRUE, FALSE, FALSE), entidad = "t",
               atributo = "x", fila = c(1L, 2L, 3L, 3L),
               objeto = paste0("t$x[", c(1, 2, 3, 3), "]"))
  }
  expect_error(.o93_medir(duplica, "instanciaAtributo"), "exactamente una")
})

test_that("una entidad no ligada se rechaza", {
  fantasma <- function(tablas, instancia) {
    data.frame(resultado = TRUE, entidad = "fantasma", atributo = "x",
               fila = NA_integer_, objeto = "fantasma$x")
  }
  expect_error(.o93_medir(fantasma), "no est\u00e1 ligada")
})

test_that("una medida por celda sin fila se rechaza", {
  sin_fila <- function(tablas, instancia) {
    data.frame(resultado = c(TRUE, FALSE), entidad = "t", atributo = "x",
               fila = NA_integer_, objeto = c("a", "b"))
  }
  expect_error(.o93_medir(sin_fila, "instanciaAtributo"), "sin `fila`",
               fixed = TRUE)
})

test_that("el control: el metodo que cumple el contrato mide", {
  cumple <- function(tablas, instancia) {
    data.frame(resultado = c(TRUE, TRUE, FALSE), entidad = "t", atributo = "x",
               fila = 1:3, objeto = paste0("t$x[", 1:3, "]"))
  }
  medicion <- .o93_medir(cumple, "instanciaAtributo")
  expect_equal(nrow(medicion), 3L)
  expect_equal(agregar(medicion, "atributo", "ratio")$resultado, 2 / 3)
  # Y una medida agregada por atributo si puede no tener fila.
  agregada <- function(tablas, instancia) {
    data.frame(resultado = TRUE, entidad = "t", atributo = "x",
               fila = NA_integer_, objeto = "t$x")
  }
  expect_equal(nrow(.o93_medir(agregada)), 1L)
})
