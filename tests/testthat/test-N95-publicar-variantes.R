# N95. La evidencia de vocabulario publica el valor, no la forma con que se lo
# analizo.
#
# Comparar vocabulario exige interpretar: `tolower()` y la normalizacion tienen
# que ver caracteres y no octetos sueltos, asi que un valor declarado `bytes`
# se marca UTF-8 antes de compararlo. Eso esta bien. Lo que estaba mal es que
# lo que se INFORMABA salia de esa forma interpretada, y entonces el mismo
# valor aparecia de dos maneras dentro del mismo objeto: la tabla del perfil lo
# publicaba con su marca intacta y la evidencia del hallazgo lo publicaba ya
# armado como caracter. Quien lee el informe no podia saber que eran el mismo
# valor, que es justo lo que el hallazgo le pide mirar antes de unificar.
#
# Los fixtures se construyen con `rawToChar(as.raw(...))` porque la fuente del
# paquete es ASCII y porque escribir el acento en el archivo no permitiria
# elegir la marca, que es la propiedad que se mide.

.n95_marcar <- function(bytes, codificacion) {
  s <- rawToChar(as.raw(bytes))
  Encoding(s) <- codificacion
  s
}

# "nino" y "Nino" con la enie, en UTF-8 y en latin1.
.n95_utf8_min <- function() .n95_marcar(c(0x6e, 0x69, 0xc3, 0xb1, 0x6f), "unknown")
.n95_utf8_may <- function() .n95_marcar(c(0x4e, 0x69, 0xc3, 0xb1, 0x6f), "unknown")
.n95_lat_min <- function() .n95_marcar(c(0x6e, 0x69, 0xf1, 0x6f), "latin1")
.n95_lat_may <- function() .n95_marcar(c(0x4e, 0x69, 0xf1, 0x6f), "latin1")

# Perfila una columna dada tal cual, sin armarla a partir de un par.
.n95_perfil_columna <- function(columna) {
  perfilar(
    data.frame(v = columna, stringsAsFactors = FALSE),
    casi_duplicados_vocabulario = TRUE
  )
}

.n95_perfil <- function(minuscula, mayuscula) {
  datos <- data.frame(
    v = c(rep(minuscula, 3L), mayuscula, "otro"),
    stringsAsFactors = FALSE
  )
  perfilar(datos, casi_duplicados_vocabulario = TRUE)
}

# El fragmento entre corchetes es el unico lugar donde se publican las
# variantes. El resto de la evidencia lleva el mensaje del paquete, que esta en
# espanol y tiene acentos propios: medir sobre la evidencia entera daria un
# falso positivo con el `n` de "espanol" y la prueba no mediria nada.
.n95_fragmento <- function(perfil) {
  h <- hallazgos(perfil)
  i <- which(as.character(h$tipo) == "casi_duplicados_vocabulario")
  if (!length(i)) return(NULL)
  sub("\\].*", "]", as.character(h$evidencia[[i[[1L]]]]))
}

test_that("un valor declarado bytes se publica igual en el perfil y en la evidencia", {
  v <- .n95_utf8_min()
  Encoding(v) <- "bytes"
  a <- .n95_utf8_may()
  Encoding(a) <- "bytes"
  perfil <- .n95_perfil(v, a)

  fragmento <- .n95_fragmento(perfil)
  expect_false(is.null(fragmento))

  # La forma de publicacion del paquete, la misma que usa la tabla del perfil.
  esperada <- lupa:::.texto_publicable(v)
  expect_true(grepl(esperada, fragmento, fixed = TRUE, useBytes = TRUE))

  # Y no puede traer los octetos crudos: eso seria haber interpretado una
  # cadena cuya declaracion dice que no se interprete.
  crudos <- rawToChar(as.raw(c(0xc3, 0xb1)))
  expect_false(grepl(crudos, fragmento, fixed = TRUE, useBytes = TRUE))
  expect_false(any(as.integer(charToRaw(fragmento)) > 127L))

  # La moda sale de otra capa; las dos tienen que decir lo mismo.
  moda <- as.character(perfil$columnas$moda)
  expect_true(grepl(lupa:::.texto_publicable(moda), fragmento,
                    fixed = TRUE, useBytes = TRUE))
})

test_that("una columna latin1 corriente no aborta y se publica como caracter", {
  # Este es el caso que rompio el primer intento de arreglo: publicar SIEMPRE
  # el original hacia que `utf8ToInt()` devolviera NA sobre latin1 y el render
  # abortara. `latin1` no es `bytes`: ahi R conoce la codificacion y la
  # convierte sin perder nada, asi que se publica el caracter.
  perfil <- .n95_perfil(.n95_lat_min(), .n95_lat_may())
  fragmento <- .n95_fragmento(perfil)
  expect_false(is.null(fragmento))
  expect_false(grepl("\\x", fragmento, fixed = TRUE))
  esperado <- rawToChar(as.raw(c(0xc3, 0xb1)))
  expect_true(grepl(esperado, fragmento, fixed = TRUE, useBytes = TRUE))
})

test_that("el texto sin marcar sigue publicandose intacto", {
  perfil <- .n95_perfil(.n95_utf8_min(), .n95_utf8_may())
  fragmento <- .n95_fragmento(perfil)
  expect_false(is.null(fragmento))
  expect_false(grepl("\\x", fragmento, fixed = TRUE))

  ascii <- .n95_perfil("nino", "Nino")
  expect_true(grepl("nino", .n95_fragmento(ascii), fixed = TRUE))
})

test_that("la traza sigue apareando por valor contra el texto analizado", {
  # `variantes` tiene que seguir siendo la forma interpretada: la traza la usa
  # para indexar por valor con `match()`, que ABORTA si un lado viene marcado
  # `bytes`. Publicar y aparear son dos usos del mismo dato y por eso viajan en
  # campos distintos. Sin esta comprobacion, un arreglo que unificara los dos
  # campos volveria a tumbar `perfilar()` entero.
  v <- .n95_utf8_min()
  Encoding(v) <- "bytes"
  a <- .n95_utf8_may()
  Encoding(a) <- "bytes"
  perfil <- .n95_perfil(v, a)
  h <- hallazgos(perfil)
  i <- which(as.character(h$tipo) == "casi_duplicados_vocabulario")
  expect_true(length(i) > 0L)
  traza <- h$trazabilidad[[i[[1L]]]]
  expect_equal(traza$n_filas_formas_variantes, 1L)
  expect_equal(traza$n_filas_formas_dominantes, 3L)
})

test_that("una forma que junta secuencias de bytes distintas no se publica como una sola", {
  # `ni<f1>o` declarado latin1 y `ni<c3><b1>o` declarado bytes son DOS
  # secuencias distintas que se interpretan como la misma palabra. Se cuentan
  # juntas, que es correcto porque el vocabulario se compara por caracteres.
  # Pero entonces la etiqueta no puede ser la de un original: decir
  # `ni\xc3\xb1o (3)` afirma que esa secuencia aparece tres veces cuando
  # aparece una. Y cual de las dos se publicaba dependia del ORDEN DE LLEGADA.
  lat <- .n95_lat_min()
  byt <- .n95_utf8_min()
  Encoding(byt) <- "bytes"
  may <- .n95_utf8_may()
  Encoding(may) <- "bytes"

  bytes_primero  <- .n95_fragmento(.n95_perfil_columna(c(byt, lat, lat, may, "otro")))
  latin1_primero <- .n95_fragmento(.n95_perfil_columna(c(lat, byt, byt, may, "otro")))
  expect_false(is.null(bytes_primero))

  # Las mismas filas en distinto orden no pueden dar etiquetas distintas.
  expect_identical(bytes_primero, latin1_primero)

  # Y la forma mezclada se publica en la unidad en que se conto: caracteres.
  escapada <- lupa:::.texto_publicable(byt)
  expect_false(grepl(escapada, bytes_primero, fixed = TRUE, useBytes = TRUE))

  # La forma homogenea si conserva la declaracion: ahi el conteo y la etiqueta
  # hablan de la misma secuencia.
  homogenea <- .n95_fragmento(.n95_perfil_columna(c(byt, byt, byt, may, "otro")))
  expect_true(grepl(escapada, homogenea, fixed = TRUE, useBytes = TRUE))
})
