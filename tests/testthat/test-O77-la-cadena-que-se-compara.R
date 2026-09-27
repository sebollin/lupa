# TODAS las pruebas de este archivo saltean sin `stringdist`, y no por prolijidad:
# sin ese paquete la funcion declara que no puede comparar y devuelve un objeto vacio,
# asi que las aserciones de aca no medirian el arreglo sino la ausencia del paquete.
# La suite local lo tiene instalado y daba verde; el check con dependencias estrictas
# lo corre SIN el y dio `FAIL 7`. La suite verde no es el check.
#
# Cuatro cosas de `detectar_duplicados_aproximados()`: la cadena que realmente se
# compara no era la que el objeto declaraba.
#
# Las tres primeras producian FALSOS POSITIVOS -pares a distancia 0 entre filas que
# no son iguales- y la cuarta hace que un numero publicado no se pueda rehacer con la
# receta documentada.

test_that("dos columnas de texto con el mismo nombre no se comparan en silencio", {
  skip_if_not_installed("stringdist")
  # El camino explicito rechaza los nombres repetidos; el automatico tomaba
  # `names(datos)` sin deduplicar y `.seleccionar_columnas()` los resuelve con
  # `match()`, o sea la PRIMERA columna comparada contra si misma: publicaba
  # evidencia con valores que la segunda columna no tiene y `exacto` a distancia 0
  # donde la distancia de los valores reales -0,162- no llega al umbral.
  datos <- data.frame(
    txt = c("ana", "ana", "luis"),
    txt = c("b", "XXXXXXXXXXXX", "c"),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  resultado <- detectar_duplicados_aproximados(datos, max_resultados = Inf)

  expect_identical(nrow(resultado$pares), 0L)
  expect_identical(nrow(resultado$hallazgos), 0L)
  expect_match(resultado$razon, "mismo nombre")
  expect_match(resultado$razon, "No se compararon sus filas", fixed = TRUE)
})

test_that("sin nombres repetidos la comparacion corre como siempre", {
  skip_if_not_installed("stringdist")
  # Mitad de control: la guarda nueva no puede apagar el camino automatico.
  datos <- data.frame(
    txt = c("ana", "ana", "luis"),
    otro = c("b", "b", "c"),
    stringsAsFactors = FALSE
  )

  resultado <- detectar_duplicados_aproximados(datos, max_resultados = Inf)

  expect_gt(nrow(resultado$pares), 0L)
  expect_false(nzchar(resultado$razon))
})

test_that("un valor declarado bytes no se iguala al literal de su escape", {
  skip_if_not_installed("stringdist")
  # El rendido de publicacion convierte un valor `bytes` en `a\xc3\xb1o`, que es una
  # cadena que se puede teclear: el valor no textual y ese literal de diez caracteres
  # quedaban identicos y el par salia `exacto_normalizado` con
  # `igualo_normalizar = TRUE` incluso con `normalizar = FALSE`, donde no hay ningun
  # mecanismo declarado para igualar textos.
  crudo <- rawToChar(as.raw(c(0x61, 0xC3, 0xB1, 0x6F)))
  Encoding(crudo) <- "bytes"
  literal <- "a\\xc3\\xb1o"
  datos <- data.frame(txt = c(crudo, literal), stringsAsFactors = FALSE)

  resultado <- detectar_duplicados_aproximados(
    datos, columnas = "txt", normalizar = FALSE, max_resultados = Inf
  )

  expect_identical(nrow(resultado$pares), 0L)
})

test_that("el mismo texto en UTF-8 y en latin1 sigue siendo el mismo valor", {
  skip_if_not_installed("stringdist")
  # Mitad de control del anterior, y la equivalencia que el manual promete: la clave
  # de bytes distingue un `bytes` de un literal SIN romper esto.
  utf8 <- rawToChar(as.raw(c(0x61, 0xC3, 0xB1, 0x6F)))
  Encoding(utf8) <- "UTF-8"
  latin1 <- rawToChar(as.raw(c(0x61, 0xF1, 0x6F)))
  Encoding(latin1) <- "latin1"
  datos <- data.frame(txt = c(utf8, latin1), stringsAsFactors = FALSE)

  resultado <- detectar_duplicados_aproximados(
    datos, columnas = "txt", normalizar = FALSE, max_resultados = Inf
  )

  expect_identical(nrow(resultado$pares), 1L)
  expect_identical(as.character(resultado$pares$tipo_par), "exacto")
  expect_identical(resultado$pares$distancia, 0)
})

test_that("el separador de concatenacion no puede salir de un valor", {
  skip_if_not_installed("stringdist")
  # `c1 = "x | y", c2 = "z"` y `c1 = "x", c2 = "y | z"` daban los dos "x | y | z":
  # dos filas que no comparten NINGUN valor salian `exacto_normalizado` a distancia
  # 0, mientras el informe de fusiones del propio objeto declaraba que ningun paso de
  # normalizacion habia fundido nada.
  datos <- data.frame(
    c1 = c("x | y", "x"), c2 = c("z", "y | z"), stringsAsFactors = FALSE
  )

  resultado <- detectar_duplicados_aproximados(
    datos, umbral = 0.1, max_resultados = Inf
  )

  expect_false(any(resultado$pares$distancia == 0))
  expect_false(any(as.character(resultado$pares$tipo_par) == "exacto_normalizado"))
  expect_false(any(resultado$pares$igualo_normalizar))
})

test_that("la distancia de varias columnas se rehace con la receta documentada", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    c1 = c("x | y", "x"), c2 = c("z", "y | z"), stringsAsFactors = FALSE
  )

  resultado <- detectar_duplicados_aproximados(
    datos, umbral = 0.5, max_resultados = Inf
  )

  escapar <- function(v) gsub("|", "\\|", v, fixed = TRUE)
  fila <- function(i) {
    paste(escapar(datos$c1[[i]]), escapar(datos$c2[[i]]), sep = " | ")
  }
  rehecha <- stringdist::stringdist(fila(1), fila(2), method = "jw", p = 0.1)

  expect_equal(resultado$pares$distancia[[1L]], rehecha)
})

test_that("la distancia de una sola columna se rehace sin separador", {
  # El control de la receta: con una columna no hay concatenacion que deshacer.
  skip_if_not_installed("stringdist")
  datos <- data.frame(t = c("ana", "anna"), stringsAsFactors = FALSE)

  resultado <- detectar_duplicados_aproximados(
    datos, columnas = "t", umbral = 0.3, normalizar = FALSE
  )

  expect_equal(
    resultado$pares$distancia[[1L]],
    stringdist::stringdist("ana", "anna", method = "jw", p = 0.1)
  )
})

test_that("la descomposicion canonica llega hasta donde el manual dice", {
  # El roxygen prometia que la descomposicion canonica "se aplica siempre" y daba la
  # receta "para reproducir un numero publicado hay que descomponer primero". Las dos
  # cosas no pueden ser ciertas: la tabla cubre el subconjunto latino, asi que fuera
  # de el dos escrituras canonicamente equivalentes NO colapsan y la receta da otro
  # numero. Ahora el limite esta escrito, y esta prueba lo sostiene: si algun dia la
  # tabla se amplia, esto se pone rojo y obliga a actualizar el manual.
  skip_if_not_installed("stringdist")
  # Griego: el mismo nombre precompuesto y descompuesto.
  nfc <- intToUtf8(c(957, 953, 954, 972, 962))
  nfd <- intToUtf8(c(957, 953, 954, 959, 769, 962))
  fuera <- detectar_duplicados_aproximados(
    data.frame(txt = c(nfc, nfd), stringsAsFactors = FALSE),
    columnas = "txt", normalizar = normalizacion(acentos = FALSE),
    umbral = 0.5, max_resultados = Inf
  )
  # Latino: `cafe` precompuesto y descompuesto, que si colapsan.
  latino_nfc <- intToUtf8(c(99, 97, 102, 233))
  latino_nfd <- intToUtf8(c(99, 97, 102, 101, 769))
  dentro <- detectar_duplicados_aproximados(
    data.frame(txt = c(latino_nfc, latino_nfd), stringsAsFactors = FALSE),
    columnas = "txt", normalizar = normalizacion(acentos = FALSE),
    umbral = 0.5, max_resultados = Inf
  )

  expect_identical(as.character(dentro$pares$tipo_par), "exacto_normalizado")
  expect_identical(dentro$pares$distancia, 0)
  # Fuera del subconjunto latino no colapsan, y el numero se rehace SIN descomponer,
  # que es la receta que el manual da para ese caso.
  expect_identical(as.character(fuera$pares$tipo_par), "aproximado")
  expect_gt(fuera$pares$distancia[[1L]], 0)
  expect_equal(
    fuera$pares$distancia[[1L]],
    stringdist::stringdist(nfc, nfd, method = "jw", p = 0.1)
  )
})
