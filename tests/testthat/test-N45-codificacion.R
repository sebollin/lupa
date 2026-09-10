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
