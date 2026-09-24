# Una cifra publicada tiene que poder rehacerse desde lo que el objeto publica.
# Con `normalizar = FALSE` el lector cree comparar las cadenas tal como se
# guardaron, y la distancia publicada no es esa: la descomposicion canonica se
# aplica siempre -es lo que hace que `cafe` precompuesto y `cafe` con acento
# combinante sean el mismo texto- y un acento pasa a contar como un caracter
# aparte. Medido: `cafe` con tilde y `cafe` distan 0.04 descompuestas y 0.117
# tal cual, asi que el par entra o no en un umbral de 0.1 segun cual se mida.
# La conducta se conserva -es la correcta- y ahora esta escrita en la pagina de
# la funcion; esta prueba fija que lo escrito siga siendo cierto.

if (requireNamespace("stringdist", quietly = TRUE) &&
    requireNamespace("stringi", quietly = TRUE)) {

  test_that("la distancia publicada es la de las formas descompuestas", {
    valores <- c(
      rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xc3, 0xa9))), # cafe con tilde
      "cafe",
      rawToChar(as.raw(c(0x6e, 0x69, 0xc3, 0xb1, 0x61))), # nina con enie
      "nina"
    )
    Encoding(valores) <- "UTF-8"
    datos <- data.frame(nombre = valores, stringsAsFactors = FALSE)

    resultado <- detectar_duplicados_aproximados(
      datos, columnas = "nombre", umbral = 0.1, normalizar = FALSE,
      proteger_datos_personales = FALSE
    )
    pares <- resultado$pares
    expect_gt(nrow(pares), 0L)

    for (i in seq_len(nrow(pares))) {
      uno <- sub("^nombre=", "", pares$evidencia_1[[i]])
      otro <- sub("^nombre=", "", pares$evidencia_2[[i]])
      descompuesta <- stringdist::stringdist(
        stringi::stri_trans_nfd(uno), stringi::stri_trans_nfd(otro),
        method = resultado$metodo, p = resultado$p
      )
      cruda <- stringdist::stringdist(
        uno, otro, method = resultado$metodo, p = resultado$p
      )
      expect_equal(pares$distancia[[i]], descompuesta, tolerance = 1e-9,
                   info = paste(uno, otro))
      # Y la diferencia no es cosmetica: la cruda deja el par afuera del
      # umbral que el objeto declara.
      expect_gt(cruda, resultado$umbral)
    }
  })

}
