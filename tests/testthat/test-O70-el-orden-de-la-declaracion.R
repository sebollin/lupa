# El orden en que se DECLARA un modelo no es el modelo.
#
# Un modelo es un conjunto de instrumentos y un marco es un conjunto de pares
# dimension/factor: escribirlos en otro orden no cambia nada de lo que se mide, y
# la serie lo publicaba como `configuracion_modelo / no_comparable` con severidad
# error sobre resultados identicos -delta 0-. El paquete ya habia decidido esto
# dos veces, para las politicas de `normalizacion(proteger=)` y para los pesos de
# `agregar()`, asi que no era una decision nueva sino una inconsistencia.
#
# La mitad de control es la que da sentido a la otra: hay un eje donde el orden SI
# significa -los `atributos` de una instancia, que la regla recibe en ese orden- y
# ahi la descripcion tiene que seguir distinguiendo. La prueba lo verifica midiendo
# que el resultado cambia, no citando la documentacion.

datos_orden <- function() {
  data.frame(
    edad = c(20, NA, 35),
    peso = c(60, 70, NA),
    stringsAsFactors = FALSE
  )
}

medir_orden <- function(mod, id, fecha = "2026-01-31") {
  medir(mod, list(personas = datos_orden()), id_medicion = id,
        fecha = as.POSIXct(fecha, tz = "UTC"))
}

instancias_orden <- function() {
  nucleo <- metricas_nucleo()
  list(
    edad = instanciar(especializar(nucleo$NoNulo, "NoNuloEdad"), "personas", "edad"),
    peso = instanciar(especializar(nucleo$NoNulo, "NoNuloPeso"), "personas", "peso")
  )
}

resultados_orden <- function(medicion) {
  tabla <- as.data.frame(medicion)
  tabla <- tabla[order(tabla$metrica_instanciada, tabla$fila), , drop = FALSE]
  as.numeric(tabla$resultado)
}

test_that("declarar las mismas metricas en otro orden no cambia el modelo", {
  i <- instancias_orden()
  ab <- medir_orden(modelo(i$edad, i$peso), "ab")
  ba <- medir_orden(modelo(i$peso, i$edad), "ba")

  expect_identical(
    attr(ab, "configuracion_modelo", exact = TRUE),
    attr(ba, "configuracion_modelo", exact = TRUE)
  )
  # Y lo medido es lo mismo, que es la razon por la que el modelo es el mismo.
  expect_identical(sort(resultados_orden(ab)), sort(resultados_orden(ba)))
})

test_that("declarar los mismos pares del marco en otro orden no cambia el modelo", {
  i <- instancias_orden()
  ab <- medir_orden(
    modelo(i$edad, marco = marco_calidad(
      "Propio", list(Completitud = "Densidad", Exactitud = "Correctitud"))), "ab")
  ba <- medir_orden(
    modelo(i$edad, marco = marco_calidad(
      "Propio", list(Exactitud = "Correctitud", Completitud = "Densidad"))), "ba")

  expect_identical(
    attr(ab, "configuracion_modelo", exact = TRUE),
    attr(ba, "configuracion_modelo", exact = TRUE)
  )
})

test_that("la deriva no publica un cambio de modelo por el orden de la declaracion", {
  i <- instancias_orden()
  perfil <- perfil_evaluacion(
    "Basico", regla_evaluacion("Presente", function(x) x > 0)
  )
  historico <- historico_calidad(
    evaluar(medir_orden(modelo(i$edad, i$peso), "enero", "2026-01-31"), perfil),
    evaluar(medir_orden(modelo(i$peso, i$edad), "febrero", "2026-02-28"), perfil)
  )
  deriva <- detectar_deriva_calidad(historico, nivel = "perfil")

  expect_false(any(deriva$aspecto == "configuracion_modelo", na.rm = TRUE))
})

test_that("el orden de los atributos SI cambia el modelo, porque cambia la medida", {
  # La mitad de control. `.metodo_regla_intra()` entrega a la regla las columnas
  # en el orden de `atributos`, asi que este es un eje donde reordenar no es un
  # artefacto de escritura: es otra medicion. Si algun dia se "arregla" ordenando
  # todo, esta prueba lo frena.
  nucleo <- metricas_nucleo()
  regla <- especializar(
    nucleo$ReglaIntegridadIntraEntidad, "PrimeraMayor",
    regla = function(d) !is.na(d[[1L]]) & !is.na(d[[2L]]) & d[[1L]] > d[[2L]]
  )
  ep <- medir_orden(modelo(instanciar(regla, "personas", c("edad", "peso"))), "ep")
  pe <- medir_orden(modelo(instanciar(regla, "personas", c("peso", "edad"))), "pe")

  expect_false(identical(resultados_orden(ep), resultados_orden(pe)))
  expect_false(identical(
    attr(ep, "configuracion_modelo", exact = TRUE),
    attr(pe, "configuracion_modelo", exact = TRUE)
  ))
})

test_that("cambiar una metrica del modelo sigue siendo un cambio de modelo", {
  # El otro control: que la insensibilidad al orden no se haya comido la senal.
  nucleo <- metricas_nucleo()
  i <- instancias_orden()
  otra <- instanciar(
    especializar(nucleo$NoNulo, "NoNuloOtroNombre"), "personas", "peso"
  )
  expect_false(identical(
    attr(medir_orden(modelo(i$edad, i$peso), "a"), "configuracion_modelo", exact = TRUE),
    attr(medir_orden(modelo(i$edad, otra), "b"), "configuracion_modelo", exact = TRUE)
  ))
})
