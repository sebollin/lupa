# N52: los nombres publicados conservan bytes y marca para indexar la entrada.

suppressPackageStartupMessages(library(lupa))

.nombre_u8_N52 <- function() {
  rawToChar(as.raw(c(
    0x63, 0x61, 0x74, 0x65, 0x67, 0x6f, 0x72, 0xc3, 0xad, 0x61
  )))
}

.nombre_latin1_N52 <- function() {
  rawToChar(as.raw(c(
    0x63, 0x61, 0x74, 0x65, 0x67, 0x6f, 0x72, 0xed, 0x61
  )))
}

.tabla_nombre_N52 <- function(nombre) {
  salida <- data.frame(
    valor = seq_len(7L), auxiliar = seq_len(7L),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  names(salida) <- c(nombre, "valor")
  salida
}

test_that("N52: cada nombre publicado indexa la tabla original en ambos locales", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  for (locale in c("es_UY.UTF-8", "C")) {
    puesto <- try(Sys.setlocale("LC_CTYPE", locale), silent = TRUE)
    if (!identical(puesto, locale)) next
    for (nombre in list(.nombre_u8_N52(), .nombre_latin1_N52())) {
      datos <- .tabla_nombre_N52(nombre)
      n <- names(datos)[[1L]]
      perfil <- lupa::perfilar(
        datos, analizar_dependencias = FALSE,
        casi_duplicados_vocabulario = FALSE,
        proteger_datos_personales = FALSE
      )
      q <- perfil$columnas$columna[[1L]]

      expect_true(q %in% names(datos), info = locale)
      expect_false(is.null(datos[[q]]), info = locale)
      expect_equal(ncol(datos[, q, drop = FALSE]), 1L, info = locale)
      expect_identical(charToRaw(q), charToRaw(n), info = locale)
      expect_identical(Encoding(q), Encoding(n), info = locale)
    }
  }
})
