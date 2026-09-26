# `senal_redundante(ventana = )` promete "tolerancia maxima admitida entre los
# valores". En `detectar_discordancias()` la ventana se aplica solo si TODAS las
# columnas son `is.numeric()`, y un `Date` no lo es: las columnas temporales caen
# en la rama de igualdad exacta y la ventana declarada NO se usa.
#
# Medido: dos columnas `Date` a un dia de distancia con `ventana = 1` dan
# `n_discordantes = 2`, y el control numerico con las mismas distancias da 0. La
# fila publicaba `ventana = 1` al lado de una comparacion que no la uso.
#
# El arreglo no cambia el numero -cambiarlo seria inventar una tolerancia sobre
# fechas que el paquete no define-: lo declara, que es lo que hace el resto del
# paquete cuando una declaracion no puede gobernar.

.o66_fechas <- function() {
  data.frame(a = as.Date(c("2026-01-30", "2026-01-31")),
             b = as.Date(c("2026-01-31", "2026-02-01")))
}

test_that("una ventana que no puede gobernar se declara", {
  senal <- senal_redundante(c("a", "b"), nombre = "s", ventana = 1)
  expect_warning(
    resultado <- as.data.frame(detectar_discordancias(.o66_fechas(), senal)),
    "ventana no se aplic"
  )
  # El numero NO cambia: las dos filas siguen siendo discordantes por igualdad
  # exacta, y la fila sigue publicando la ventana DECLARADA.
  expect_equal(resultado$n_discordantes, 2)
  expect_equal(resultado$ventana, 1)
  # Y la evidencia lo decia desde antes; el aviso es para quien lee la tabla.
  expect_true(grepl("comparacion textual exacta", resultado$evidencia,
                    fixed = TRUE))
})

test_that("sin ventana declarada no avisa nada", {
  # La mitad de control: una guarda que avisara siempre pasaria la prueba de
  # arriba sin distinguir nada.
  expect_silent(
    detectar_discordancias(.o66_fechas(),
                           senal_redundante(c("a", "b"), nombre = "s"))
  )
})

test_that("con columnas numericas la ventana gobierna y no avisa", {
  datos <- data.frame(a = c(1, 2), b = c(2, 3))
  senal <- senal_redundante(c("a", "b"), nombre = "s", ventana = 1)
  expect_silent(resultado <- detectar_discordancias(datos, senal))
  # Las mismas distancias que el caso de fechas: aca la tolerancia si actua.
  expect_equal(as.data.frame(resultado)$n_discordantes, 0)
})

test_that("el camino que el aviso recomienda funciona", {
  # El aviso dice transformar las fechas a un numero en la unidad de la ventana.
  # Si ese consejo no funcionara, el aviso seria peor que el silencio.
  senal <- senal_redundante(
    c("a", "b"), nombre = "s", ventana = 1,
    transformacion = list(a = as.numeric, b = as.numeric)
  )
  expect_silent(resultado <- detectar_discordancias(.o66_fechas(), senal))
  expect_equal(as.data.frame(resultado)$n_discordantes, 0)
})
