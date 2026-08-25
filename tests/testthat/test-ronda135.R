# `.normalizar_tablas_modelo()` tiene dos entradas hermanas. La de un solo data
# frame exige que el modelo tenga una sola entidad. La de lista comprobaba la
# forma -nombrada, sin repetidos, todo data frames- pero no que estuvieran las
# entidades que el modelo necesita.
#
# Salio de barrer la CLASE de un defecto anterior: una condicion validada rama
# por rama, con una hermana sin ella. Buscar el patron y no la instancia es lo
# que lo encontro.

.modelo_de <- function(entidades) {
  do.call(modelo, lapply(entidades, function(e) {
    instanciar(especializar(metricas_nucleo()$NoNulo), e, "x")
  }))
}

test_that("faltar tablas se rechaza en la frontera y se nombran TODAS", {
  # Antes fallaba mucho despues, al ligar cada metrica, y nombraba una sola: con
  # tres ausentes el usuario agregaba la primera, volvia a correr y chocaba con
  # la siguiente. Tres vueltas para el mismo problema.
  modelo_cuatro <- .modelo_de(c("t_a", "t_b", "t_c", "t_d"))
  mensaje <- tryCatch(
    medir(modelo_cuatro, list(t_a = data.frame(x = 1))),
    error = function(e) conditionMessage(e)
  )
  expect_true(is.character(mensaje))
  for (faltante in c("t_b", "t_c", "t_d")) {
    expect_match(mensaje, faltante, fixed = TRUE)
  }
  # Y dice que se recibio, que es la mitad que permite ver el error de tipeo.
  expect_match(mensaje, "`datos` tiene: t_a", fixed = TRUE)
})

test_that("una tabla que el modelo no pide se sigue ignorando", {
  # El control que hace valer la guarda: convertirla en igualdad exacta habria
  # apagado un uso legitimo. Medido antes de escribirla.
  modelo_dos <- .modelo_de(c("t_a", "t_b"))
  medidas <- medir(modelo_dos, list(
    t_a = data.frame(x = 1), t_b = data.frame(x = 2),
    sobrante = data.frame(x = 3)
  ))
  expect_equal(nrow(medidas), 2L)
})

test_that("la rama de un solo data frame conserva su propia guarda", {
  # La hermana que si validaba no tiene que haber cambiado.
  modelo_dos <- .modelo_de(c("t_a", "t_b"))
  expect_error(
    medir(modelo_dos, data.frame(x = 1)),
    "lista con nombre", fixed = TRUE
  )
  modelo_uno <- .modelo_de("t_a")
  expect_equal(nrow(medir(modelo_uno, data.frame(x = 1))), 1L)
})
