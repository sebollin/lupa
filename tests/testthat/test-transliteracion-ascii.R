# Codifica a mano solo servia para los puntos de dos bytes -hasta U+07FF- y se
# rompio con "embedded nul" al entrar U+1E9E, que ocupa tres. `intToUtf8()` es
# correcto para cualquier punto de codigo y no hay que probarlo.
.codigo_utf8 <- function(hexadecimal) {
  resultado <- intToUtf8(strtoi(hexadecimal, base = 16L))
  Encoding(resultado) <- "UTF-8"
  resultado
}

test_that("el mapa translitera cada caracter de forma determinista", {
  mapa <- lupa:::.MAPA_TRANSLITERACION_ASCII
  entrada <- vapply(names(mapa), .codigo_utf8, character(1L))
  codigos_esperados <- c(
    sprintf("%04X", c(
      0xAA, 0xB5, 0xBA, 0xC0:0xD6, 0xD8:0xF6, 0xF8:0xFF
    )),
    sprintf("%04X", 0x100:0x17F),
    # Agregados tras auditar el mapa carácter por carácter: la doble ese
    # mayuscula, que estaba en el mapa de pliegue y no en este, y un punado de
    # Latin Extended-B -las comas suscritas del rumano y la O con trazo y
    # agudo- que el consumidor borraba del nombre por no estar cubierto.
    "1E9E", "0218", "0219", "021A", "021B", "01FE", "01FF"
  )

  expect_length(mapa, 200L)
  expect_setequal(names(mapa), codigos_esperados)
  expect_equal(lupa:::.transliterar_ascii(entrada), unname(mapa))
  expect_equal(
    lupa:::.transliterar_ascii(paste0(entrada, collapse = "")),
    paste0(unname(mapa), collapse = "")
  )

  no_mapeado <- rawToChar(as.raw(c(0xCE, 0xA9)))
  Encoding(no_mapeado) <- "UTF-8"
  expect_identical(lupa:::.transliterar_ascii(no_mapeado), no_mapeado)
})

test_that("perfilar reconoce y protege una fecha de fallecimiento acentuada", {
  nombre <- paste0(
    "FECHA_DEFUNCI",
    .codigo_utf8("00D3"),
    "N"
  )
  expect_equal(lupa:::.transliterar_ascii(nombre), "FECHA_DEFUNCION")
  fechas <- as.Date(c("1980-01-01", "1981-01-01"))
  datos <- data.frame(fechas, check.names = FALSE)
  names(datos) <- nombre

  perfil <- perfilar(
    datos, fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    analizar_dependencias = FALSE
  )
  encontrado <- perfil$datos_personales[
    match(nombre, perfil$datos_personales$columna), , drop = FALSE
  ]
  columna <- perfil$columnas[
    perfil$columnas$columna == nombre, , drop = FALSE
  ]

  expect_equal(encontrado$tipo, "fecha_fallecimiento")
  expect_true(encontrado$proteger)
  expect_equal(nrow(columna), 1L)
  expect_equal(columna$tipo_dato_personal, "fecha_fallecimiento")
  expect_equal(columna$minimo_fecha, "[valor protegido]")
  expect_equal(columna$maximo_fecha, "[valor protegido]")
  expect_equal(columna$media_fecha, "[valor protegido]")
  expect_equal(columna$mediana_fecha, "[valor protegido]")
})

test_that("los cuatro consumidores igualan nombres con y sin acentos", {
  fecha_acentuada <- paste0("FECHA_DEFUNCI", .codigo_utf8("00D3"), "N")
  fecha_plana <- "FECHA_DEFUNCION"
  texto_acentuado <- paste0("ca", .codigo_utf8("00F1"), "on")
  texto_plano <- "canon"
  columna_acentuada <- paste0("N", .codigo_utf8("00DA"), "MERO_DOCUMENTO")
  columna_plana <- "NUMERO_DOCUMENTO"

  expect_equal(
    lupa:::.transliterar_ascii(c(
      fecha_acentuada, texto_acentuado, columna_acentuada
    )),
    c(fecha_plana, texto_plano, columna_plana)
  )
  resultados_acentuados <- c(
    lupa:::.normalizar_nombre_fecha(fecha_acentuada),
    lupa:::.clave_sin_escritura(texto_acentuado),
    lupa:::.nombres_snake(columna_acentuada),
    lupa:::.normalizar_nombre_columna(columna_acentuada)
  )
  resultados_planos <- c(
    lupa:::.normalizar_nombre_fecha(fecha_plana),
    lupa:::.clave_sin_escritura(texto_plano),
    lupa:::.nombres_snake(columna_plana),
    lupa:::.normalizar_nombre_columna(columna_plana)
  )

  expect_equal(resultados_acentuados, resultados_planos)
  expect_equal(
    resultados_acentuados,
    c("fecha_defuncion", "canon", "numero_documento", "numerodocumento")
  )
})

test_that("los dos mapas cubren lo mismo y la forma descompuesta colapsa", {
  # Los tres casos salen de una auditoria caracter por caracter de los mapas.
  codigo <- function(...) {
    x <- intToUtf8(c(...))
    Encoding(x) <- "UTF-8"
    x
  }

  # 1. La doble ese mayuscula estaba en el mapa de pliegue y faltaba en el de
  #    transliteracion: "<SS>eta" perdia la letra entera. Y como su destino son
  #    dos caracteres, tiene que estar en la lista de especiales: metida en el
  #    tramo uno a uno, `chartr` desalinea TODA la tabla.
  expect_identical(lupa:::.transliterar_ascii(codigo(0x1E9E, 0x65)), "SSe")
  expect_true("1E9E" %in% lupa:::.CODIGOS_ESPECIALES_TRANSLITERACION_ASCII)

  # 2. Un caracter del medio de la tabla, como control de que no hay desalineo.
  expect_identical(lupa:::.transliterar_ascii(codigo(0x015A)), "S")

  # 3. La forma descompuesta tiene que colapsar con la precompuesta: antes la
  #    marca combinante sobrevivia y el consumidor la volvia separador.
  expect_identical(
    lupa:::.transliterar_ascii(paste0("S", codigo(0x0301), "anchez")),
    lupa:::.transliterar_ascii(codigo(0x015A, 0x61, 0x6E, 0x63, 0x68, 0x65, 0x7A))
  )

  # 4. Las comas suscritas del rumano, que quedaban fuera del mapa y por lo
  #    tanto el consumidor las borraba del nombre.
  expect_identical(lupa:::.transliterar_ascii(codigo(0x0218, 0x0219)), "Ss")
})

test_that("el mapa de transliteracion es coherente consigo mismo", {
  # Una entrada mal puesta -destino de largo distinto en el tramo uno a uno-
  # desalinea la tabla entera y corrompe letras que no se tocaron. Se comprueba
  # que cada entrada se aplica como dice y que ningun destino sale de ASCII.
  mapa <- lupa:::.MAPA_TRANSLITERACION_ASCII
  caracteres <- vapply(strtoi(names(mapa), 16L), intToUtf8, character(1L))
  aplicado <- vapply(caracteres, lupa:::.transliterar_ascii, character(1L),
                     USE.NAMES = FALSE)
  expect_identical(aplicado, unname(mapa))
  expect_false(any(grepl("[^ -~]", unname(mapa))))
  expect_false(any(duplicated(names(mapa))))
})
