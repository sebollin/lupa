# El tope de largo se evaluaba sobre el valor CRUDO y la comparacion corria
# sobre el NORMALIZADO. Con el perfil `amplio` del propio paquete -sin codigo
# ajeno- eso abria un desvio: la normalizacion expande ligaduras, "ffl" es un
# caracter que se convierte en tres, y una columna de 5.101 caracteres crudos
# pasaba el tope de 10.000, se comparaba con 15.303 y publicaba `largo_maximo`
# 5.101. El tope existe por lo que cuesta la comparacion y por como se degrada
# la distancia normalizada: las dos cosas dependen de la cadena que entra a
# `stringdist`, no de la que estaba guardada.

# U+FB04 en bytes, para que este archivo siga siendo ASCII.
.ligadura_ffl <- function() {
  x <- rawToChar(as.raw(c(0xef, 0xac, 0x84)))
  Encoding(x) <- "UTF-8"
  x
}

test_that("el tope mide el valor normalizado, que es el que se compara", {
  skip_if_not_installed("stringdist")
  base <- strrep(.ligadura_ffl(), 5100)   # 5.101 crudos, 15.301 normalizados
  datos <- data.frame(
    t = c(base, base, paste0(base, "x")), stringsAsFactors = FALSE
  )
  con_amplio <- suppressWarnings(detectar_duplicados_aproximados(
    datos, columnas = "t", normalizar = "amplio",
    proteger_datos_personales = FALSE, max_largo_valor = 10000L
  ))

  # Crudo por debajo del tope, normalizado por encima: se excluye.
  expect_lt(max(nchar(datos$t)), 10000L)
  expect_equal(con_amplio$alcance$n_columnas_excluidas_largo, 1L)
  expect_equal(con_amplio$alcance$n_pares_comparados, 0L)
  # Y el largo publicado es el que se iba a comparar, no el guardado.
  expect_gt(con_amplio$alcance$largo_maximo, 10000L)
})

test_that("sin normalizacion que expanda, el tope no cambia de conducta", {
  # La guarda tiene que cerrar el desvio SIN excluir de mas: una que se lleva
  # por delante el caso corriente arregla un falso positivo creando otro.
  skip_if_not_installed("stringdist")
  base <- strrep(.ligadura_ffl(), 5100)
  datos <- data.frame(
    t = c(base, base, paste0(base, "x")), stringsAsFactors = FALSE
  )
  sin_normalizar <- detectar_duplicados_aproximados(
    datos, columnas = "t", normalizar = FALSE,
    proteger_datos_personales = FALSE, max_largo_valor = 10000L
  )
  expect_equal(sin_normalizar$alcance$n_columnas_excluidas_largo, 0L)
  expect_equal(sin_normalizar$alcance$largo_maximo, 5101)
  expect_gt(sin_normalizar$alcance$n_pares_comparados, 0L)
})

# El motivo del tope de vocabulario contaba FILAS y decia "valores". La regla de
# proximidad trabaja sobre el vocabulario -los valores distintos-, asi que esa
# era la unidad prometida y no la medida.
#
# Sobrevivio porque el fixture que lo cubria tenia UNA fila por valor, donde las
# dos cuentas coinciden. Un caso comodo no puede separar dos unidades que solo
# se separan cuando hay repeticion.

test_that("el tope de vocabulario cuenta valores distintos y filas por separado", {
  skip_if_not_installed("stringdist")
  largo <- strrep("a", 12000L)
  datos <- data.frame(
    t = c(rep(largo, 100L), "corto"), stringsAsFactors = FALSE
  )
  alcance <- .grupos_casi_duplicados_vocabulario(
    datos$t, NULL, "t", max_valores = 100L, max_pares = Inf, max_trabajo = Inf
  )$alcance

  # Un valor distinto, cien filas. Las dos cuentas, cada una en su unidad.
  expect_equal(alcance$n_valores_largos, 1L)
  expect_equal(alcance$n_filas_largas, 100L)

  perfil <- perfilar(datos, proteger_datos_personales = FALSE)
  motivo <- perfil$cobertura_diagnosticos$motivo[[1L]]
  expect_match(motivo, "1 valor(es) distinto(s)", fixed = TRUE)
  expect_match(motivo, "100 fila(s)", fixed = TRUE)
})
