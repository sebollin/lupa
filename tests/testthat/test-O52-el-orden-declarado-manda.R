# La distancia se mide sobre los valores concatenados, asi que el orden en que
# se combinan las columnas cambia el resultado: sobre las mismas cinco filas,
# `c("nombre", "domicilio")` publica 6 pares y `c("domicilio", "nombre")`
# publica 10, con el mismo umbral. Esa dependencia es inherente a comparar
# texto concatenado y ahora esta documentada. Lo que esta prueba fija es la
# invarianza que si vale y que hace reproducible al objeto: declarar las
# columnas vuelve el resultado independiente de como esten ordenadas en el
# archivo, y el objeto publica el orden que uso.

datos_o52 <- function(orden = c("nombre", "domicilio")) {
  completo <- data.frame(
    nombre = c("Ana Perez", "Luis Diaz", "Juan Perez", "Ana Peres",
               "Juan Peres"),
    domicilio = c("Calle 1", "Calle 9", "Calle 1", "Calle 1", "Calle 2"),
    stringsAsFactors = FALSE
  )
  completo[, orden, drop = FALSE]
}

pares_o52 <- function(datos, columnas) {
  resultado <- detectar_duplicados_aproximados(
    datos, columnas = columnas, umbral = 0.2, normalizar = FALSE,
    proteger_datos_personales = FALSE
  )
  list(
    claves = paste(resultado$pares$fila_1, resultado$pares$fila_2),
    columnas = resultado$columnas,
    distancias = resultado$pares$distancia
  )
}

test_that("declarar las columnas independiza del orden del archivo", {
  skip_if_not_installed("stringdist")
  skip_if_not_installed("stringi")
  uno <- pares_o52(datos_o52(), c("nombre", "domicilio"))
  otro <- pares_o52(datos_o52(c("domicilio", "nombre")),
                    c("nombre", "domicilio"))

  expect_gt(length(uno$claves), 0L)
  expect_setequal(uno$claves, otro$claves)
  expect_equal(uno$distancias, otro$distancias)
})

test_that("el objeto publica el orden que uso para comparar", {
  skip_if_not_installed("stringdist")
  skip_if_not_installed("stringi")
  # Sin declararlas, manda el orden del archivo. El resultado sigue siendo
  # reproducible porque el objeto dice cual uso: es la diferencia entre
  # depender del orden y ocultarlo.
  sin_declarar <- pares_o52(datos_o52(), NULL)
  invertido <- pares_o52(datos_o52(c("domicilio", "nombre")), NULL)

  expect_identical(sin_declarar$columnas, c("nombre", "domicilio"))
  expect_identical(invertido$columnas, c("domicilio", "nombre"))

  # Y lo publicado alcanza para rehacer cada corrida: pedir explicitamente el
  # orden que el objeto declara devuelve exactamente sus pares.
  rehecho <- pares_o52(datos_o52(c("domicilio", "nombre")),
                       invertido$columnas)
  expect_setequal(rehecho$claves, invertido$claves)
  expect_equal(rehecho$distancias, invertido$distancias)
})

test_that("una fila sin texto comparable no se compara y se declara", {
  skip_if_not_installed("stringdist")
  skip_if_not_installed("stringi")
  datos <- data.frame(x = c("", NA, "abcde"), stringsAsFactors = FALSE)
  resultado <- detectar_duplicados_aproximados(
    datos, columnas = "x", umbral = 0.2, proteger_datos_personales = FALSE
  )
  alcance <- resultado$alcance

  expect_equal(alcance$n_filas_total, 3)
  expect_equal(alcance$n_filas_validas, 1)
  expect_equal(nrow(resultado$pares), 0L)
  expect_true(nzchar(alcance$razon))

  # El control: con texto en las tres filas, las mismas columnas si comparan.
  completo <- detectar_duplicados_aproximados(
    data.frame(x = c("abcde", "abcde", "zzzz"), stringsAsFactors = FALSE),
    columnas = "x", umbral = 0.2, proteger_datos_personales = FALSE
  )
  expect_equal(completo$alcance$n_filas_validas, 3)
  expect_gt(nrow(completo$pares), 0L)
})
