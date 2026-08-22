# El orden por bytes exige que la codificacion este DECLARADA. R rechaza con
# "Character encoding must be UTF-8, Latin-1 or bytes" una cadena marcada
# `unknown` que contenga bytes no ASCII, aunque sean UTF-8 perfectamente
# validos. Y asi llega cualquier CSV en espanol leido con `read.csv()`.
#
# Lo destapo un registro publico de sanciones del catalogo nacional: el valor
# "Combustibles liquidos" con tilde rompia el perfil entero. Ninguna de las
# cuatro auditorias lo encontro, porque todos sus fixtures eran ASCII.

# `rawToChar()` produce exactamente la marca que rompe: bytes UTF-8 validos con
# `Encoding()` en "unknown". Construirlo con literales del archivo no serviria,
# porque el parser de R los marca como UTF-8 y el caso no se ejercita.
.texto_sin_marca <- function(bytes) rawToChar(as.raw(bytes))

test_that("una cadena con acentos y sin codificacion declarada no rompe el orden", {
  liquidos <- .texto_sin_marca(c(
    0x43, 0x6f, 0x6d, 0x62, 0x75, 0x73, 0x74, 0x69, 0x62, 0x6c, 0x65, 0x73,
    0x20, 0x6c, 0xc3, 0xad, 0x71, 0x75, 0x69, 0x64, 0x6f, 0x73
  ))
  # La prueba no vale si el fixture no tiene la propiedad que rompe.
  expect_identical(Encoding(liquidos), "unknown")
  expect_true(validUTF8(liquidos))

  expect_identical(.ordenar_por_bytes(c("Zeta", liquidos, "Alfa"))[[1L]], "Alfa")

  datos <- data.frame(
    sector = c(rep(liquidos, 30L), rep("Comercio", 20L), "Combustible liquido"),
    stringsAsFactors = FALSE
  )
  expect_no_error(
    perfil <- perfilar(
      datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    )
  )
  expect_equal(nrow(perfil$columnas), 1L)
  expect_equal(perfil$columnas$n_distintos, 3L)
})

test_that("la moda tampoco rompe con acentos sin declarar", {
  # El mismo defecto estaba en el desempate de la moda, por la misma linea.
  cafe <- .texto_sin_marca(c(0x43, 0x61, 0x66, 0xc3, 0xa9))
  expect_identical(Encoding(cafe), "unknown")
  datos <- data.frame(x = c(rep(cafe, 5L), "Bar", "Bar"), stringsAsFactors = FALSE)
  expect_no_error(
    perfil <- perfilar(
      datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    )
  )
  expect_equal(perfil$columnas$frecuencia_moda, 5L)
})

test_that("un byte que no se puede decodificar se ordena igual y no rompe", {
  # Lo que queda invalido despues de marcar la codificacion se pasa por
  # `iconv(sub = "byte")`: no es bonito, pero es determinista y ordenable.
  roto <- .texto_sin_marca(c(0x41, 0xff, 0x42))
  expect_false(validUTF8(roto))
  expect_no_error(salida <- .ordenar_por_bytes(c("Zeta", roto, "Alfa")))
  expect_length(salida, 3L)
  # No se fija en que posicion cae el byte roto -`iconv` lo vuelve `<ff>`, y `<`
  # ordena antes que las letras-, sino lo que el orden tiene que garantizar: que
  # no rompa, que no pierda elementos y que sea el mismo dos veces seguidas.
  expect_identical(salida, .ordenar_por_bytes(c("Alfa", roto, "Zeta")))
  expect_identical(salida[[length(salida)]], "Zeta")
})
