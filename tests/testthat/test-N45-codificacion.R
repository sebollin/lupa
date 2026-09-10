test_that("N45: los nombres UTF-8 desconocidos no rompen bajo locale C", {
  nombre <- rawToChar(as.raw(c(0x4e, 0xc3, 0x89, 0x6d, 0x65, 0x72, 0x6f)))
  datos <- data.frame(1:3, c("x", "y", "z"), stringsAsFactors = FALSE)
  names(datos) <- c(nombre, "otro")
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", anterior), add = TRUE)
  Sys.setlocale("LC_CTYPE", "C")

  perfil <- expect_no_error(perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  expect_identical(
    charToRaw(perfil$columnas$columna[[1L]]), charToRaw(nombre)
  )
  expect_equal(perfil$columnas$columna[[2L]], "otro")
})

test_that("N45: bytes UTF-8 v\u00e1lidos se normalizan en las dos APIs", {
  cafe <- rawToChar(as.raw(c(0x43, 0x61, 0x66, 0xc3, 0xa9)))
  valores <- c(cafe, "Bar", "Bar")
  Encoding(valores) <- "bytes"
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", anterior), add = TRUE)
  Sys.setlocale("LC_CTYPE", "C")

  perfil <- expect_no_error(perfilar(
    data.frame(a = valores, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  inferencia <- expect_no_error(inferir_tipo(valores))
  expect_equal(perfil$columnas$n_codificacion_invalida, 0L)
  expect_equal(inferencia$tipo, "texto")

  ascii <- c("Bar", "Baz")
  Encoding(ascii) <- "bytes"
  expect_no_error(perfilar(
    data.frame(a = ascii, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))

  invalido <- rawToChar(as.raw(c(0x41, 0xff, 0x42)))
  Encoding(invalido) <- "bytes"
  perfil_invalido <- expect_no_error(perfilar(
    data.frame(a = invalido, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  expect_equal(perfil_invalido$columnas$n_codificacion_invalida, 1L)
})

test_that("N45: la identidad por bytes conserva moda y cardinalidad", {
  latin1 <- rawToChar(as.raw(c(0x4e, 0xfa, 0x6d, 0x65, 0x72, 0x6f)))
  perfil <- perfilar(
    data.frame(a = c(latin1, latin1, latin1, "Bar", "Bar"),
               stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas[1L, , drop = FALSE]
  expect_equal(fila$n_distintos, 2L)
  expect_equal(fila$frecuencia_moda, 3L)
  expect_equal(fila$tasa_distintos, 2 / 5)
  expect_identical(
    lupa:::.clave_bytes(fila$moda), lupa:::.clave_bytes(latin1)
  )
  expect_false("constante" %in% perfil$hallazgos$tipo_hallazgo)
  expect_equal(fila$n_codificacion_invalida, 3L)
})

test_that("N45: un universo textual inv\u00e1lido no publica ceros de reparaci\u00f3n", {
  latin1 <- rawToChar(as.raw(c(0x4e, 0xfa, 0x6d, 0x65, 0x72, 0x6f)))
  perfil <- perfilar(
    data.frame(a = rep(latin1, 3L), stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas[1L, , drop = FALSE]
  campos <- c(
    "n_codificacion_rota", "n_codificacion_reparable",
    "n_codificacion_reparable_parcialmente", "n_codificacion_irreparable",
    "n_codificacion_no_se_pudo"
  )
  expect_true(all(vapply(fila[campos], is.na, logical(1L))))
  expect_true(is.na(fila$estado_codificacion_reparacion))
  expect_false(fila$unicode_evaluado)
  expect_equal(fila$n_codificacion_invalida, 3L)
  expect_true(any(perfil$hallazgos$tipo_hallazgo == "codificacion_invalida"))
})

test_that("el marcado de la entrada no agrega un modo de falla propio", {
  # `.marcar_utf8_tabla()` corre ANTES de cualquier validacion. Cuando empezo a
  # mirar `names()`, una tabla sin nombres moria adentro del marcado, en
  # `Encoding(NULL)`. El marcado no valida ni rechaza: declara una codificacion.
  # Lo que no es texto no se marco, asi que pasa intacto y lo trata quien
  # corresponda.
  #
  # No se afirma NINGUN texto de mensaje: el que sale es de base R y viene
  # traducido segun el locale de quien corre la prueba, asi que afirmarlo seria
  # medir la maquina y no el paquete.
  sin_nombres <- data.frame(x = 1:3)
  names(sin_nombres) <- NULL
  expect_no_error(.marcar_utf8_tabla(sin_nombres))
  expect_identical(.marcar_utf8_tabla(sin_nombres), sin_nombres)
  expect_identical(.marcar_utf8_textos(NULL), NULL)
  expect_identical(.marcar_utf8_textos(character()), character())
  expect_identical(.marcar_utf8_textos(1:3), 1:3)
})

test_that("el nombre de columna queda marcado sea cual sea el locale", {
  # La condicion que decidia si asignar era `!identical(marcados, originales)`, y
  # `identical()` responde distinto segun el locale: con los mismos bytes, una
  # cadena `unknown` y otra `UTF-8` son identicas bajo un locale UTF-8 y
  # distintas bajo `C`. O sea que el marcado era un no-op justo en el locale
  # donde corre esta suite. Se decide por la marca, que es lo que cambia.
  nombre <- rawToChar(as.raw(c(0x4e, 0xc3, 0x89, 0x6d, 0x65, 0x72, 0x6f)))
  expect_identical(Encoding(nombre), "unknown")
  expect_true(validUTF8(nombre))
  tabla <- data.frame(1:3, c("x", "y", "z"), stringsAsFactors = FALSE)
  names(tabla) <- c(nombre, "otro")

  marcada <- .marcar_utf8_tabla(tabla)

  expect_identical(Encoding(names(marcada)[[1L]]), "UTF-8")
  # Marcar declara la codificacion; no cambia un byte.
  expect_identical(charToRaw(names(marcada)[[1L]]), charToRaw(nombre))
  # Y lo mismo para los niveles de un factor, que decidian con la misma condicion.
  con_factor <- data.frame(
    f = factor(c(nombre, "otro", nombre)), stringsAsFactors = FALSE
  )
  marcada_f <- .marcar_utf8_tabla(con_factor)
  niveles <- levels(marcada_f$f)
  # El nivel se busca por sus BYTES, no con `==`. Comparar cadenas traduce al
  # locale, asi que `levels(f) == nombre` encuentra el nivel bajo un locale UTF-8
  # y no lo encuentra bajo `C` -medido: `character(0)`-. Seleccionar asi haria
  # que la prueba midiera el locale de quien la corre, que es exactamente el
  # defecto que se retiro de `test-locale.R` esta misma jornada.
  coincide <- vapply(
    niveles, function(z) identical(charToRaw(z), charToRaw(nombre)), logical(1L)
  )
  expect_true(any(coincide))
  expect_identical(Encoding(niveles[coincide]), "UTF-8")
})
