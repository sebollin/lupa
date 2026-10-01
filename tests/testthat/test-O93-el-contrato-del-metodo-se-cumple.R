# La parte semantica del contrato de `metodo` se hace cumplir.
#
# Una salida que lo viola es un metodo que fallo: la metrica queda `no_medible`
# con el mensaje del contrato, `medir()` avisa, y las demas metricas se miden. Al
# principio abortaba `medir()` entero, y eso se llevaba la medicion de las demas
# -lo mismo que la llamada envuelta ya evitaba-.
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

test_that("dos observaciones del mismo objeto dejan la metrica no medible", {
  duplica <- function(tablas, instancia) {
    data.frame(resultado = c(TRUE, TRUE, FALSE, FALSE), entidad = "t",
               atributo = "x", fila = c(1L, 2L, 3L, 3L),
               objeto = paste0("t$x[", c(1, 2, 3, 3), "]"))
  }
  .expect_no_medible(.o93_medir(duplica, "instanciaAtributo"), "exactamente una")
})

test_that("una entidad no ligada deja la metrica no medible", {
  fantasma <- function(tablas, instancia) {
    data.frame(resultado = TRUE, entidad = "fantasma", atributo = "x",
               fila = NA_integer_, objeto = "fantasma$x")
  }
  .expect_no_medible(.o93_medir(fantasma), "no est\u00e1 ligada")
})

test_that("una medida por celda sin fila deja la metrica no medible", {
  sin_fila <- function(tablas, instancia) {
    data.frame(resultado = c(TRUE, FALSE), entidad = "t", atributo = "x",
               fila = NA_integer_, objeto = c("a", "b"))
  }
  .expect_no_medible(.o93_medir(sin_fila, "instanciaAtributo"), "sin `fila`")
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
