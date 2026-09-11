# N53: las salidas a disco no dependen del locale de la sesion.

suppressPackageStartupMessages(library(lupa))

.bytes_N53 <- function(...) {
  rawToChar(as.raw(c(...)))
}

.datos_N53 <- function() {
  nombre <- .bytes_N53(
    0x6e, 0xc3, 0xb3, 0x6d, 0x69, 0x6e, 0x61
  )
  datos <- data.frame(
    valor = c(
      .bytes_N53(0x52, 0x65, 0x70, 0xc3, 0xba, 0x62, 0x6c, 0x69, 0x63, 0x61),
      "normal", "otro"
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  names(datos) <- nombre
  datos
}

.perfil_N53 <- function() {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  .fijar_locale_N53(.locale_utf8_N53())
  perfilar(
    .datos_N53(),
    nombre = .bytes_N53(0x50, 0x65, 0x72, 0x66, 0x69, 0x6c, 0x20,
                        0xC3, 0xA1),
    fecha = as.POSIXct("2026-09-11 10:44:51", tz = "UTC"),
    analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
}

.bytes_archivo_N53 <- function(archivo) {
  tamano <- file.info(archivo)$size
  expect_true(is.finite(tamano) && tamano >= 0)
  leidos <- readBin(archivo, what = "raw", n = tamano)
  expect_equal(length(leidos), tamano)
  leidos
}

# Fijar un locale CONCRETO es medir la maquina. `es_UY.UTF-8` no existe en el
# contenedor del minimo declarado, ni en las maquinas de CRAN, ni en la mayoria
# de las instalaciones: la primera version de este archivo hacia `stop()` ahi y
# el check del contenedor fallo entero. Lo que la prueba necesita no es ESE
# locale sino UNO cualquiera que sea UTF-8, y `C`, que existe siempre.
.locale_utf8_N53 <- function() {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  candidatos <- c("es_UY.UTF-8", "es_ES.UTF-8", "en_US.UTF-8", "C.UTF-8",
                  "UTF-8", anterior)
  for (cand in candidatos) {
    puesto <- suppressWarnings(try(Sys.setlocale("LC_CTYPE", cand),
                                   silent = TRUE))
    if (identical(puesto, cand) &&
        grepl("utf-?8", cand, ignore.case = TRUE)) {
      return(cand)
    }
  }
  NULL
}

.fijar_locale_N53 <- function(locale) {
  if (is.null(locale)) skip("no hay ningun locale UTF-8 disponible")
  puesto <- suppressWarnings(try(Sys.setlocale("LC_CTYPE", locale),
                                 silent = TRUE))
  if (!identical(puesto, locale)) {
    skip(paste0("no se pudo fijar LC_CTYPE=", locale))
  }
}

test_that("N53: el HTML conserva tamano, bytes y cierre en ambos locales", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  perfil <- .perfil_N53()
  archivo <- file.path(tempdir(), "lupa-N53-reporte.html")
  on.exit(unlink(archivo), add = TRUE)
  esperado <- NULL

  for (locale in c(.locale_utf8_N53(), "C")) {
    .fijar_locale_N53(locale)
    reportar(
      perfil, archivo = archivo, sobrescribir = TRUE,
      titulo = .bytes_N53(0x52, 0x65, 0x70, 0x6f, 0x72, 0x74, 0x65, 0x20,
                          0xC3, 0xA9),
      fecha = as.POSIXct("2026-09-11 10:44:51", tz = "UTC")
    )
    bytes <- .bytes_archivo_N53(archivo)
    if (is.null(esperado)) esperado <- bytes

    expect_equal(
      file.info(archivo)$size, length(esperado), info = locale
    )
    expect_true(
      grepl("</html>\\s*$", rawToChar(bytes), perl = TRUE), info = locale
    )
    expect_identical(bytes, esperado, info = locale)
  }
})

.analisis_N53 <- function() {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  .fijar_locale_N53(.locale_utf8_N53())
  analizar(
    .datos_N53(), conservar_datos = TRUE, muestra = Inf,
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    fecha = as.POSIXct("2026-09-11 10:44:51", tz = "UTC")
  )
}

.historico_N53 <- function() {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  .fijar_locale_N53(.locale_utf8_N53())
  nucleo <- metricas_nucleo()
  instancia <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "NoNuloN53"),
    "tabla", "valor"
  )
  medidas <- medir(
    modelo(instancia), data.frame(
      valor = c(.bytes_N53(0x52, 0x65, 0x70, 0xc3, 0xba, 0x62, 0x6c, 0x69, 0x63, 0x61),
                "normal"),
      stringsAsFactors = FALSE
    ),
    id_medicion = .bytes_N53(0x6d, 0x65, 0x64, 0x69, 0x63, 0x69, 0xc3, 0xb3, 0x6e),
    fecha = as.POSIXct("2026-09-11 10:44:51", tz = "UTC")
  )
  historico_calidad(medidas)
}

# `saveRDS()` incluye la codificacion nativa de la sesion en la cabecera; por
# eso el archivo binario puede cambiar de tamano entre locales sin cambiar el
# contenido serializado. La garantia relevante aqui es la recuperacion byte a
# byte de los textos acentuados.
test_that("N53: los RDS conservan bytes acentuados en ambos locales", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  analisis <- .analisis_N53()
  historico <- .historico_N53()
  directorio <- tempdir()
  archivo_analisis <- file.path(directorio, "lupa-N53-analisis.rds")
  archivo_historico <- file.path(directorio, "lupa-N53-historico.rds")
  on.exit(unlink(c(archivo_analisis, archivo_historico)), add = TRUE)

  for (locale in c(.locale_utf8_N53(), "C")) {
    .fijar_locale_N53(locale)
    guardar_analisis(
      analisis, archivo_analisis, incluir_datos = TRUE,
      proteger_datos_personales = FALSE, sobrescribir = TRUE,
      comprimir = FALSE
    )
    guardar_historico(historico, archivo_historico, sobrescribir = TRUE)
    bytes_analisis <- .bytes_archivo_N53(archivo_analisis)
    bytes_historico <- .bytes_archivo_N53(archivo_historico)
    expect_true(length(bytes_analisis) > 0L, info = locale)
    expect_true(length(bytes_historico) > 0L, info = locale)
    recuperado_analisis <- leer_analisis(archivo_analisis)
    recuperado_historico <- leer_historico(archivo_historico)
    expect_identical(
      charToRaw(names(recuperado_analisis$datos)[[1L]]),
      charToRaw(names(analisis$datos)[[1L]]), info = locale
    )
    expect_identical(
      charToRaw(recuperado_historico$id_medicion[[1L]]),
      charToRaw(historico$id_medicion[[1L]]), info = locale
    )
  }
})
