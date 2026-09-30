# `alcance_medidas` cuenta el universo de la METRICA, y afirma una causa solo si
# las cuentas la sostienen.
#
# `NoNulo` declara su universo con la propiedad `aplicable`, y el alcance contaba
# `nrow()` de la tabla: una metrica que midio sus tres filas aplicables publicaba
# "midio 3 de 4 en el universo aplicable: las que no tienen valor no producen
# medida", y la fila que faltaba no era una sin valor, era una fuera del universo.
# El mismo universo declarado por `aplicabilidad` en `medir()` no publicaba nada.
# Y un metodo propio que mide la mitad de las filas por decision suya recibia el
# mismo motivo sin tener un solo ausente.

.o89_datos <- function(marca = c("A", NA, "B", "C")) {
  data.frame(tiene_auto = c("Si", "No", "Si", "Si"), marca_auto = marca,
             stringsAsFactors = FALSE)
}

test_that("con `aplicable` propio, la metrica que mide su universo no publica alcance", {
  nucleo <- metricas_nucleo()
  por_propiedad <- especializar(
    nucleo$NoNulo, nombre_especifico = "NMarca", aplicable = ~ tiene_auto == "Si"
  )
  medicion <- medir(modelo(instanciar(por_propiedad, "t", "marca_auto")),
                    .o89_datos())
  expect_equal(nrow(medicion), 3L)
  # Antes: en_el_universo = 4, medidas = 3.
  expect_null(attr(medicion, "alcance_medidas", exact = TRUE))

  # Y lo mismo que el universo declarado en `medir()`: dos declaraciones del
  # mismo universo publican lo mismo.
  por_medir <- medir(
    modelo(instanciar(especializar(nucleo$NoNulo), "t", "marca_auto")),
    .o89_datos(), aplicabilidad = list(marca_auto = ~ tiene_auto == "Si")
  )
  expect_equal(sort(por_medir$resultado), sort(medicion$resultado))
  expect_null(attr(por_medir, "alcance_medidas", exact = TRUE))
})

test_that("el control: una metrica por celda sigue declarando lo que no midio", {
  formato <- instanciar(
    especializar(metricas_nucleo()$Formato, expresion_regular = "^[A-Z]$"),
    "t", "marca_auto"
  )
  medicion <- medir(modelo(formato), .o89_datos(c("A", NA, NA, "C")))
  alcance <- attr(medicion, "alcance_medidas", exact = TRUE)
  expect_equal(alcance$en_el_universo, 4)
  expect_equal(alcance$medidas, 2)
  # Las dos que faltan son las dos sin valor: la causa se puede afirmar.
  expect_match(alcance$motivo, "las que no tienen valor no producen medida",
               fixed = TRUE)
})

test_that("un metodo que mide una parte por decision propia no recibe esa causa", {
  propia <- metrica("MitadPropia", "Mide la mitad.", "instanciaEntidad", "booleano")
  metodo <- function(tablas, instancia) {
    data.frame(resultado = rep(TRUE, 5), entidad = "t",
               atributo = NA_character_, fila = 1:5,
               objeto = paste0("t[", 1:5, "]"))
  }
  medicion <- medir(
    modelo(instanciar(especializar(propia), "t", metodo = metodo)),
    data.frame(x = 1:10)
  )
  alcance <- attr(medicion, "alcance_medidas", exact = TRUE)
  expect_equal(alcance$medidas, 5)
  expect_equal(alcance$en_el_universo, 10)
  expect_false(grepl("no tienen valor", alcance$motivo, fixed = TRUE))
  expect_match(alcance$motivo, "las otras 5 no produjeron medida", fixed = TRUE)
})
