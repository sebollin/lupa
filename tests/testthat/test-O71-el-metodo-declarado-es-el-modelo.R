# Medir con otro metodo es medir otra cosa, y la serie tiene que decirlo.
#
# `instanciar(metodo = )` deja que quien llama reemplace el metodo de medicion de
# una metrica. Ese metodo no entraba en la descripcion del modelo: dos corridas con
# metodos distintos daban `configuracion_modelo` identica y la deriva publicaba la
# diferencia como `aspecto = resultado` -un deterioro de los DATOS-. Un cambio del
# modelo que la serie nunca declaraba.
#
# El criterio, y es lo que estas pruebas fijan: entra en el modelo **lo que declaro
# quien llama**. El metodo por omision de la metrica no entra, porque es del
# paquete: serializar su texto haria que cada version nueva de lupa acusara un
# cambio de modelo en toda serie existente. Las dos mitades se prueban.

metodo_presente <- function(tablas, instancia) {
  x <- tablas[[instancia$entidad]][[instancia$atributos]]
  lupa:::.salida_metodo(
    !is.na(x), instancia$entidad, instancia$atributos, seq_along(x),
    paste0(instancia$entidad, "[", seq_along(x), ",]")
  )
}

metodo_mayor_de_21 <- function(tablas, instancia) {
  x <- tablas[[instancia$entidad]][[instancia$atributos]]
  lupa:::.salida_metodo(
    !is.na(x) & x > 21, instancia$entidad, instancia$atributos, seq_along(x),
    paste0(instancia$entidad, "[", seq_along(x), ",]")
  )
}

medir_metodo <- function(metodo, id, fecha = "2026-01-31") {
  especifica <- especializar(metricas_nucleo()$NoNulo, "NoNuloEdad")
  medir(
    modelo(instanciar(especifica, "personas", "edad", metodo = metodo)),
    list(personas = data.frame(edad = c(20, NA, 35))),
    id_medicion = id, fecha = as.POSIXct(fecha, tz = "UTC")
  )
}

configuracion <- function(medicion) {
  attr(medicion, "configuracion_modelo", exact = TRUE)
}

test_that("el metodo que declara quien llama es parte del modelo", {
  a <- medir_metodo(metodo_presente, "a")
  b <- medir_metodo(metodo_mayor_de_21, "b")

  # Primero que la premisa sea cierta: los dos metodos miden distinto.
  expect_false(identical(
    as.numeric(as.data.frame(a)$resultado),
    as.numeric(as.data.frame(b)$resultado)
  ))
  expect_false(identical(configuracion(a), configuracion(b)))
})

test_that("la deriva publica el cambio de metodo como cambio de modelo", {
  perfil <- perfil_evaluacion(
    "Basico", regla_evaluacion("Presente", function(x) x > 0)
  )
  deriva <- detectar_deriva_calidad(
    historico_calidad(
      evaluar(medir_metodo(metodo_presente, "enero", "2026-01-31"), perfil),
      evaluar(medir_metodo(metodo_mayor_de_21, "febrero", "2026-02-28"), perfil)
    ),
    nivel = "perfil"
  )

  expect_true(any(deriva$aspecto == "configuracion_modelo", na.rm = TRUE))
})

test_that("cambiar el metodo por omision del paquete NO cambia el modelo", {
  # La mitad que protege al usuario de lupa: si el metodo por omision entrara en la
  # descripcion, un refactor interno del paquete acusaria un cambio de modelo en
  # toda serie existente. Se cambia el metodo por omision de la metrica del nucleo
  # y la configuracion tiene que quedar igual.
  antes <- medir(
    modelo(instanciar(
      especializar(metricas_nucleo()$NoNulo, "NoNuloEdad"), "personas", "edad"
    )),
    list(personas = data.frame(edad = c(20, NA, 35))),
    id_medicion = "antes", fecha = as.POSIXct("2026-01-31", tz = "UTC")
  )
  local_mocked_bindings(.metodo_no_nulo = metodo_mayor_de_21, .package = "lupa")
  despues <- medir(
    modelo(instanciar(
      especializar(metricas_nucleo()$NoNulo, "NoNuloEdad"), "personas", "edad"
    )),
    list(personas = data.frame(edad = c(20, NA, 35))),
    id_medicion = "despues", fecha = as.POSIXct("2026-01-31", tz = "UTC")
  )

  # Que el reemplazo haya ocurrido: sin esto la prueba pasaria sin medir nada.
  expect_false(identical(
    as.numeric(as.data.frame(antes)$resultado),
    as.numeric(as.data.frame(despues)$resultado)
  ))
  expect_identical(configuracion(antes), configuracion(despues))
})
