# `analizar()` mide metrica por metrica y no arma el detalle entero.
#
# Medir todo el modelo junto materializaba una fila por celda y por metrica antes
# de agregar -21 filas de medida por fila de datos en una tabla de 19 columnas- y
# sobre 4,24 millones de filas eso mato el proceso con 103 GB en una evaluacion
# real. Sin `conservar_detalle_medicion`, ahora cada metrica se mide y se agrega
# antes de la siguiente. El resultado tiene que ser el MISMO: el camino de siempre
# sigue vivo con `conservar_detalle_medicion = TRUE`, y es el control.

.o96_las_dos <- function(datos, ...) {
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  correr <- function(detalle) suppressWarnings(analizar(
    datos, fecha = fecha, id_medicion = "o96",
    conservar_detalle_medicion = detalle, ...
  ))
  list(nuevo = correr(FALSE), viejo = correr(TRUE))
}

test_that("metrica por metrica da el mismo tablero, medicion y cobertura", {
  nucleo <- metricas_nucleo()
  mixta <- data.frame(
    id = 1:40, x = c(rep(NA, 10), 1:30), cat = rep(c("a", "b", NA, "c"), 10),
    f = as.Date("2026-01-01") + c(0:38, NA), stringsAsFactors = FALSE
  )
  bordes <- data.frame(edad = c(5, NA, NA, NA), cod = c("AB", NA, "c1", "ZZ"),
                       stringsAsFactors = FALSE)
  # Con una metrica `no_medible` y una con alcance parcial: los dos atributos que
  # se juntan al final y no por metrica.
  con_bordes <- modelo(
    instanciar(especializar(nucleo$NoNulo), "t", "edad"),
    instanciar(especializar(nucleo$ErrorEstandar), "t", "edad"),
    instanciar(especializar(nucleo$Formato, expresion_regular = "^[A-Z]+$"),
               "t", "cod")
  )
  casos <- list(
    list(datos = mixta),
    list(datos = mixta,
         argumentos_perfil = list(aplicabilidad = list(x = ~ id > 10))),
    list(datos = iris),
    list(datos = bordes, nombre = "t", modelo_confirmado = con_bordes)
  )
  for (i in seq_along(casos)) {
    r <- do.call(.o96_las_dos, casos[[i]])
    expect_gt(nrow(as.data.frame(r$nuevo$tablero)), 0L)
    expect_equal(r$nuevo$tablero, r$viejo$tablero, info = i)
    expect_equal(r$nuevo$medicion, r$viejo$medicion, info = i)
    expect_equal(r$nuevo$cobertura, r$viejo$cobertura, info = i)
    expect_null(r$nuevo$detalle_medicion)
    expect_s3_class(r$viejo$detalle_medicion, "medicion")
  }
})

test_that("ningun medir() recibe mas de una metrica ni arma mas de una columna", {
  # La propiedad que baja la memoria, medida en su causa y no en megabytes, que
  # dependen de la maquina: cada llamada a `medir()` lleva UNA instancia.
  datos <- data.frame(a = c(1, NA, 3, 4), b = c("x", "y", NA, "z"),
                      c = c(NA, 2, 2, 5), stringsAsFactors = FALSE)
  llamadas <- list()
  original <- medir
  local_mocked_bindings(medir = function(modelo, datos, ...) {
    resultado <- original(modelo, datos, ...)
    llamadas[[length(llamadas) + 1L]] <<- list(
      metricas = length(modelo$metricas), filas = nrow(resultado)
    )
    resultado
  })
  suppressWarnings(analizar(datos))
  expect_gt(length(llamadas), 1L)
  expect_true(all(vapply(llamadas, `[[`, integer(1L), "metricas") == 1L))
  expect_true(all(vapply(llamadas, `[[`, integer(1L), "filas") <= nrow(datos)))
})

test_that("si ninguna metrica produce medidas, analizar() declara en vez de abortar", {
  nucleo <- metricas_nucleo()
  datos <- data.frame(edad = c(5, NA, NA, NA))
  solo_falla <- modelo(instanciar(especializar(nucleo$ErrorEstandar), "t", "edad"))
  for (detalle in c(FALSE, TRUE)) {
    resultado <- suppressWarnings(analizar(
      datos, nombre = "t", modelo_confirmado = solo_falla,
      conservar_detalle_medicion = detalle
    ))
    # Antes: abortaba en el tablero y se perdian el perfil y el plan.
    expect_s3_class(resultado$perfil, "perfil")
    expect_equal(nrow(as.data.frame(resultado$tablero)), 0L)
    motivos <- attr(resultado$tablero, "cobertura_metricas", exact = TRUE)
    expect_identical(as.character(motivos$estado), "no_medible")
    expect_false(resultado$meta$modelo_medido)
  }
  # Y con una evaluacion pedida: avisa que no evaluo, no la inventa.
  avisos <- character()
  resultado <- withCallingHandlers(
    analizar(datos, nombre = "t", modelo_confirmado = solo_falla,
             perfil_evaluacion = perfil_evaluacion(
               "P", regla_evaluacion("R", function(x) x > 0)
             )),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_null(resultado$evaluacion)
  expect_true(any(grepl("No se evalu", avisos, fixed = TRUE)))
})
