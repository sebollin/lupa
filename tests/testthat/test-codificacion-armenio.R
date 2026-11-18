testthat::skip_on_cran()

# Los bytes UTF-8 de una palabra leidos como Windows-1252: los cinco bytes que
# esa tabla no define quedan como el punto C1 de igual numero. Se arma byte a
# byte con iconv(), sin las tablas del paquete.
.armenio_leido_1252 <- function(codigos) {
  bytes <- as.integer(charToRaw(enc2utf8(intToUtf8(codigos))))
  letras <- vapply(bytes, function(b) {
    x <- iconv(rawToChar(as.raw(b)), from = "windows-1252", to = "UTF-8")
    if (is.na(x)) intToUtf8(b) else x
  }, character(1L))
  paste(letras, collapse = "")
}

.armenio_palabras <- list(
  # la primera letra es U+056D: su segundo byte es un guion blando
  khach = c(0x56DL, 0x561L, 0x579L),
  yerevan = c(0x535L, 0x580L, 0x587L, 0x561L, 0x576L),
  mayusculas = c(0x53DL, 0x531L, 0x549L),
  # el guion blando queda al final de la celda
  akh = c(0x561L, 0x56DL),
  # U+0581 deja un byte que Windows-1252 no define
  dprots = c(0x564L, 0x57AL, 0x580L, 0x578L, 0x581L)
)

test_that("el armenio leido como Windows-1252 se detecta y se repara entero", {
  for (nombre in names(.armenio_palabras)) {
    palabra <- intToUtf8(.armenio_palabras[[nombre]])
    for (contexto in c("sola", "frase")) {
      rota <- .armenio_leido_1252(.armenio_palabras[[nombre]])
      celda <- if (contexto == "sola") rota else paste("Nombre:", rota, "fin")
      esperada <- if (contexto == "sola") palabra else paste("Nombre:", palabra, "fin")
      datos <- data.frame(t = c(celda, "otro", "tres", "cuatro"),
                          stringsAsFactors = FALSE)
      info <- paste(nombre, contexto)
      perfil <- suppressWarnings(perfilar(
        datos, fecha = "2026-01-15", analizar_dependencias = FALSE
      ))
      h <- hallazgos(perfil)
      expect_true(any(h$tipo_hallazgo == "codificacion_rota" & h$columna == "t"), info = info)
      plan <- suppressWarnings(planificar_limpieza(perfil, datos))
      limpio <- suppressWarnings(aplicar(plan, datos))$datos
      expect_identical(as.character(limpio$t[[1L]]), esperada, info = info)
    }
  }
})

test_that("una O seguida de un signo que no forma letra armenia es texto bien escrito", {
  # cada celda trae dos veces una O delante de un signo: el par daria un acento
  # hebreo, una letra del suplemento cirilico o una letra armenia sin uso
  celdas <- c(
    sueco_espacio_duro = paste0("SN", intToUtf8(c(0xD6L, 0xA0L, 0xD6L, 0xA0L)), "X"),
    apostrofos = intToUtf8(c(0xD6L, 0x2019L, 0xD6L, 0x2019L)),
    comillas = intToUtf8(c(0xD6L, 0xBBL, 0xD6L, 0xBBL)),
    raya = intToUtf8(c(0xD6L, 0x2014L, 0xD6L, 0x2014L)),
    guion_blando = paste0(intToUtf8(c(0xD6L, 0xADL, 0xD6L, 0xADL)), "BIK"),
    circunflejo = intToUtf8(c(0xD4L, 0x2019L, 0xD4L, 0x2019L))
  )
  for (nombre in names(celdas)) {
    celda <- celdas[[nombre]]
    datos <- data.frame(t = c(celda, "otro", "tres", "cuatro"),
                        stringsAsFactors = FALSE)
    perfil <- suppressWarnings(perfilar(
      datos, fecha = "2026-01-15", analizar_dependencias = FALSE
    ))
    h <- hallazgos(perfil)
    expect_false(any(h$tipo_hallazgo == "codificacion_rota" & h$columna == "t"),
                 info = nombre)
    plan <- suppressWarnings(planificar_limpieza(perfil, datos))
    limpio <- utf8ToInt(as.character(
      suppressWarnings(aplicar(plan, datos))$datos$t[[1L]]
    ))
    # las letras quedan; la limpieza solo puede tocar el espacio duro o el
    # guion blando
    letras <- utf8ToInt(celda)
    letras <- letras[!letras %in% c(0xA0L, 0xADL)]
    expect_identical(limpio[!limpio %in% c(0x20L, 0xA0L, 0xADL)], letras,
                     info = nombre)
  }
})
