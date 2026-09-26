# `.reemplazar_valores_protegidos()` sustituye valor por valor. Cuando un valor
# protegido es PREFIJO de otro y aparece primero en la lista, el largo queda
# cortado y su cola sale publicada. El piso del propio paquete dice que seis
# caracteres identifican -`.MIN_LARGO_VALOR_IDENTIFICANTE`-, asi que la cola de
# `"Maria Nunez de Castro"` -nueve caracteres- era una fuga con la regla de casa.
#
# Los valores salen de las celdas de las columnas protegidas EN EL ORDEN DE LAS
# FILAS, asi que cual llega primero depende de como este ordenada la tabla: el
# defecto era silencioso y dependiente de los datos.

.o62_piso <- function() lupa:::.MIN_LARGO_VALOR_IDENTIFICANTE

test_that("la cola del valor largo no se publica, venga en cualquier orden", {
  casos <- list(
    list(v = c("Juan Perez", "Juan Perez Gomez"),
         x = "titular: Juan Perez Gomez"),
    list(v = c("Maria Nunez", "Maria Nunez de Castro"),
         x = "beneficiaria: Maria Nunez de Castro"),
    list(v = c("12345678", "123456789012"), x = "documento: 123456789012")
  )
  # Lo que se exige es que no sobreviva NINGUN trozo del valor protegido; la
  # etiqueta de la izquierda -"titular:"- no es dato protegido y tiene que quedar.
  esperado <- c("titular: [valor protegido]",
                "beneficiaria: [valor protegido]",
                "documento: [valor protegido]")
  for (i in seq_along(casos)) {
    for (orden in list(casos[[i]]$v, rev(casos[[i]]$v))) {
      salida <- lupa:::.reemplazar_valores_protegidos(casos[[i]]$x, orden)
      expect_equal(salida, esperado[[i]],
                   info = paste(casos[[i]]$x, "|", paste(orden, collapse = "/")))
      # Y la cola concreta que antes se publicaba, nombrada una por una.
      for (cola in c("Gomez", "de Castro", "9012")) {
        expect_false(grepl(cola, salida, fixed = TRUE),
                     info = paste(salida, "|", cola))
      }
    }
  }
  # El piso con el que el paquete decide que algo identifica: la cola mas larga
  # que se publicaba -"de Castro"- lo pasa, asi que era una fuga con la regla de
  # casa y no una molestia estetica.
  expect_gte(nchar("de Castro"), .o62_piso())
})

test_that("el resultado no depende del orden de la lista", {
  # El control de la propiedad, no de los tres casos: con la lista al reves, la
  # salida tiene que ser IDENTICA. Antes no lo era, y eso era el defecto.
  valores <- c("Ana Gomez", "Ana Gomez Diaz", "Pedro Ruiz", "Pedro Ruiz Mora")
  texto <- c("uno: Ana Gomez Diaz", "dos: Pedro Ruiz Mora", "tres: Ana Gomez",
             "cuatro: nada que ver")
  expect_identical(
    lupa:::.reemplazar_valores_protegidos(texto, valores),
    lupa:::.reemplazar_valores_protegidos(texto, rev(valores))
  )
})

test_that("un valor repetido no anida el marcador", {
  # Sustituir dos veces el mismo valor corrompe el marcador cuando el valor
  # aparece DENTRO de el. Medido con `prot`, que tiene entre tres y cinco
  # caracteres -por debajo del piso, asi que el enmascarado por variante no tapa
  # el elemento entero y la corrupcion se ve-:
  #
  #   una pasada:   dato [valor protegido]
  #   dos pasadas:  dato [valor [valor protegido]egido]   <- y "egido" sale afuera
  una <- lupa:::.reemplazar_valores_protegidos("dato prot", "prot")
  expect_equal(una, "dato [valor protegido]")
  expect_equal(
    lupa:::.reemplazar_valores_protegidos("dato prot", rep("prot", 2L)), una
  )
  # El control: asi se veia el defecto, aplicando el reemplazo dos veces a mano.
  dos_veces <- lupa:::.reemplazar_valores_protegidos(una, "prot")
  expect_true(grepl("[valor [valor", dos_veces, fixed = TRUE))
})

test_that("el enmascarado no depende de la codificacion del texto", {
  # Esta es la guarda que mas importa de este archivo, y no cubre el defecto que
  # se arreglo: cubre el ARREGLO QUE NO SE HIZO. Reemplazar el bucle por una
  # alternancia PCRE es la optimizacion obvia -el bucle cuesta 395 `gsub` por
  # llamada- y medida DEJABA DE ENMASCARAR el texto marcado `latin1`, porque
  # `paste()` traduce a UTF-8 al armar el patron y entonces la `e` acentuada tiene
  # dos bytes en el patron y uno en el sujeto. Sin esta prueba, esa regresion de
  # privacidad entra sin que nada chille.
  #
  # Los fixtures se construyen con `rawToChar` para que el archivo quede en ASCII.
  latin1 <- rawToChar(as.raw(c(0x4a, 0x6f, 0x73, 0xe9, 0x20, 0x50, 0x65,
                               0x72, 0x65, 0x7a)))
  Encoding(latin1) <- "latin1"
  expect_equal(
    lupa:::.reemplazar_valores_protegidos(latin1, latin1),
    "[valor protegido]"
  )

  # Bytes que no son UTF-8 valido: el paquete trabaja con codificaciones rotas.
  invalido <- rawToChar(as.raw(c(0x4a, 0x75, 0x61, 0x6e, 0x20, 0xff, 0x65,
                                 0x72, 0x65, 0x7a)))
  expect_equal(
    lupa:::.reemplazar_valores_protegidos(invalido, invalido),
    "[valor protegido]"
  )

  # Y el mismo texto en UTF-8, para que la prueba no pase por casualidad sobre un
  # solo camino de codificacion.
  utf8 <- intToUtf8(c(0x4a, 0x6f, 0x73, 0xe9, 0x20, 0x50, 0x65, 0x72, 0x65, 0x7a))
  expect_equal(
    lupa:::.reemplazar_valores_protegidos(utf8, utf8),
    "[valor protegido]"
  )
})

test_that("una tabla con un nombre que es prefijo de otro no filtra la cola", {
  # La prueba de arriba mide la primitiva; esta atraviesa la funcion publica, que
  # es donde la promesa se cumple o no.
  datos <- data.frame(
    documento = c("41234567", "51234567", "61234567"),
    nombre = c("Maria Nunez", "Maria Nunez de Castro", "Ana Lopez"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  texto <- paste(capture.output(print(perfil)), collapse = " ")
  expect_false(grepl("de Castro", texto, fixed = TRUE))
  expect_false(grepl("Maria Nunez", texto, fixed = TRUE))
})
