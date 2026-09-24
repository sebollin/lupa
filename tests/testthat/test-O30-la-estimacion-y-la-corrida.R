# O30: una estimacion es una promesa sobre la corrida. `estimar_costo()`
# afirmaba "exhaustiva_por_bloques" siempre, y con los mismos argumentos la
# corrida podia mirar 447 filas de 5000 y declararse muestreada.

test_that("la estimacion declara el modo y las filas que va a mirar", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = paste0("nom ", seq_len(5000L)),
                      stringsAsFactors = FALSE)

  estimacion <- estimar_costo(datos, columnas = "nombre",
                              estrategia = "teselas", max_pares = 100000)
  corrida <- detectar_duplicados_aproximados(
    datos, columnas = "nombre", estrategia = "teselas",
    max_pares = 100000, max_resultados = 10
  )

  expect_equal(estimacion$alcance$modo_comparacion,
               corrida$alcance$modo_comparacion)
  expect_equal(estimacion$alcance$modo_comparacion, "muestreada_por_bloques")
  expect_equal(estimacion$alcance$filas_previstas,
               corrida$alcance$muestra_efectiva)
  expect_equal(estimacion$alcance$filas_totales, nrow(datos))
  expect_equal(estimacion$alcance$estrategia_prevista,
               corrida$alcance$estrategia)
})

test_that("cuando la corrida mira todas las filas la estimacion lo dice", {
  # Control: sin limite que fuerce muestreo, las dos siguen diciendo
  # exhaustiva, que es lo que decian antes para todos los casos.
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = paste0("nom ", seq_len(60L)),
                      stringsAsFactors = FALSE)

  estimacion <- estimar_costo(datos, columnas = "nombre",
                              estrategia = "teselas")
  corrida <- detectar_duplicados_aproximados(
    datos, columnas = "nombre", estrategia = "teselas", max_resultados = 10
  )

  expect_equal(estimacion$alcance$modo_comparacion, "exhaustiva_por_bloques")
  expect_equal(corrida$alcance$modo_comparacion, "exhaustiva_por_bloques")
  expect_equal(estimacion$alcance$filas_previstas, nrow(datos))
  expect_equal(estimacion$alcance$filas_previstas,
               corrida$alcance$muestra_efectiva)
})
