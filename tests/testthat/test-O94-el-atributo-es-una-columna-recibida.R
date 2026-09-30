# La cuarta clausula del contrato de `metodo`: el atributo de cada medida es una
# columna de las tablas que el metodo recibio.
#
# No se compara contra los atributos LIGADOS: las metricas de vigencia publican
# con razon la columna del contrato, que no esta ligada, y `CorrectitudSemDebil`
# publica `dni+nombre`. La pregunta del contrato es "nombre de la columna", y un
# metodo no puede haber medido una columna que no le llego.

.o94_medir <- function(atributo) {
  propia <- metrica("Propia", "Prueba del contrato.", "atributo", "booleano")
  metodo <- function(tablas, instancia) {
    data.frame(resultado = TRUE, entidad = "t", atributo = atributo,
               fila = NA_integer_, objeto = paste0("t$", atributo))
  }
  medir(modelo(instanciar(especializar(propia), "t", "x", metodo = metodo)),
        data.frame(x = c(1, 2), y = c(3, 4), `a+b` = 1:2, check.names = FALSE))
}

test_that("un atributo que no es columna de lo recibido se rechaza", {
  expect_error(.o94_medir("columna_inexistente"), "no es una columna")
  expect_error(.o94_medir("x+fantasma"), "no es una columna")
})

test_that("el control: columnas recibidas, ligadas o no, y unidas por `+`", {
  # Una columna no ligada pero recibida: es la forma de las metricas de vigencia.
  expect_equal(nrow(.o94_medir("y")), 1L)
  # Dos columnas unidas, la forma de `CorrectitudSemDebil`.
  expect_equal(nrow(.o94_medir("x+y")), 1L)
  # Un nombre que LLEVA un `+` se reconoce entero antes de partirlo.
  expect_equal(nrow(.o94_medir("a+b")), 1L)
  # Y una metrica de vigencia del catalogo, que publica la columna del contrato.
  instancia <- instanciar(
    especializar(metricas_nucleo()$DesactualizacionPorFecha,
                 vigencia = vigencia("f", fecha_acceso = as.Date("2026-02-01"),
                                     frecuencia_cambio = 1)),
    "t", "x"
  )
  medicion <- medir(modelo(instancia), data.frame(
    f = as.Date(c("2026-01-30", "2026-02-01")), x = 1:2
  ))
  expect_equal(nrow(medicion), 2L)
})
