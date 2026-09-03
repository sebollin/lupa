.codigo_utf8 <- function(hexadecimal) {
  codigo <- strtoi(hexadecimal, base = 16L)
  bytes <- c(
    0xC0 + codigo %/% 0x40,
    0x80 + codigo %% 0x40
  )
  resultado <- rawToChar(as.raw(bytes))
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
    sprintf("%04X", 0x100:0x17F)
  )

  expect_length(mapa, 193L)
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
