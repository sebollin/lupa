# Ronda 29-T: la normalizacion y la codificacion del texto. Cada bloque es un
# hallazgo de la refutacion, con el caso que lo reproducia y la cifra rehecha a
# mano. Los datos se arman con `rawToChar()` o con escapes: el archivo es ASCII.

# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.locale_o144 <- function(locale) {
  puesto <- suppressWarnings(tryCatch(
    Sys.setlocale("LC_CTYPE", locale), error = function(e) NA_character_
  ))
  length(puesto) == 1L && !is.na(puesto) && nzchar(puesto)
}

.locales_o144 <- function() {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  utf8 <- NA_character_
  for (candidato in c("C.UTF-8", "C.utf8", "es_UY.UTF-8", "es_UY.utf8",
                      "en_US.UTF-8", "en_US.utf8")) {
    if (.locale_o144(candidato) &&
        isTRUE(l10n_info()[["UTF-8"]])) {
      utf8 <- candidato
      break
    }
  }
  c(if (.locale_o144("C")) "C", if (!is.na(utf8)) utf8)
}

.hex_o144 <- function(x) {
  vapply(x, function(s) paste(as.character(charToRaw(s)), collapse = ""),
         character(1L), USE.NAMES = FALSE)
}

.normalizados_o144 <- function(x, normalizar = normalizacion()) {
  datos <- data.frame(x = x, stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(datos, normalizar = normalizar))
  perfil$meta$normalizacion_fusiones$x$n_distintos_normalizados
}

test_that("H2: reparar_codificacion repara bajo LC_CTYPE=C el UTF-8 sin marca que el perfil midio", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  if (!.locale_o144("C")) skip("No se pudo fijar LC_CTYPE=C.")

  # `Ca` + A con tilde + signo de mas o menos + `ada`: el mojibake de `Canada` con
  # enie leido una vez como CP1252. Bytes UTF-8 validos, SIN marca, como los deja
  # `read.csv()` sin `encoding`.
  roto <- rawToChar(as.raw(c(0x43, 0x61, 0xc3, 0x83, 0xc2, 0xb1, 0x61, 0x64, 0x61)))
  sano <- rawToChar(as.raw(c(0x43, 0x61, 0xc3, 0xb1, 0x61, 0x64, 0x61)))
  expect_identical(Encoding(roto), "unknown")
  datos <- data.frame(v = c(roto, roto, "Montevideo"), stringsAsFactors = FALSE)

  plan <- suppressWarnings(planificar_limpieza(suppressWarnings(perfilar(datos)), datos))
  i <- plan$estrategia == "reparar_codificacion"
  expect_identical(sum(i), 1L)
  expect_identical(as.character(plan$estado[i]), "lista")
  expect_identical(as.numeric(plan$n_afectadas[i]), 2)

  plan$aplicar <- i
  r <- suppressWarnings(aplicar(plan, datos))
  # Antes: `fallida`, n_cambiadas 0 y el dato intacto bajo C; `ejecutada` y 2 bajo
  # un locale UTF-8.
  expect_identical(r$registro$estado, "ejecutada")
  expect_identical(as.numeric(r$registro$n_cambiadas), 2)
  # A mano: las dos celdas quedan con los bytes de `Canada` con enie, 43 61 c3 b1...
  expect_identical(.hex_o144(r$datos$v), c(rep(.hex_o144(sano), 2L), .hex_o144("Montevideo")))
})

test_that("H3: n_codificacion_normalizada cuenta recodificaciones, igual en todo locale", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  locales <- .locales_o144()
  if (!length(locales)) skip("No hay locales para comparar.")

  # " Jose " y "Jose" con e aguda, bytes UTF-8 sin marca; y la misma columna en
  # latin1. Solo en latin1 los bytes del contenido cambian al recortar: e9 pasa
  # a c3 a9 en la celda tocada (1); sin marca los bytes de salida son los mismos
  # que los de entrada sin los blancos (0). Un texto ASCII nunca se recodifica.
  jose_esp <- rawToChar(as.raw(c(0x20, 0x4a, 0x6f, 0x73, 0xc3, 0xa9, 0x20)))
  jose <- rawToChar(as.raw(c(0x4a, 0x6f, 0x73, 0xc3, 0xa9)))
  jose_esp_l <- rawToChar(as.raw(c(0x20, 0x4a, 0x6f, 0x73, 0xe9, 0x20)))
  jose_l <- rawToChar(as.raw(c(0x4a, 0x6f, 0x73, 0xe9)))
  Encoding(jose_esp_l) <- "latin1"
  Encoding(jose_l) <- "latin1"

  una <- function(x, estrategia) {
    datos <- data.frame(x = x, stringsAsFactors = FALSE)
    plan <- suppressWarnings(planificar_limpieza(suppressWarnings(perfilar(datos)), datos))
    plan$aplicar <- plan$estrategia == estrategia
    r <- suppressWarnings(aplicar(plan, datos))
    fila <- r$registro[r$registro$estrategia == estrategia, , drop = FALSE]
    list(n = as.numeric(fila$n_codificacion_normalizada), bytes = .hex_o144(r$datos$x))
  }
  resultados <- lapply(locales, function(locale) {
    expect_true(.locale_o144(locale))
    list(
      sin_marca = una(c(jose_esp, jose, "Ana "), "recortar_espacios"),
      latin1 = una(c(jose_esp_l, jose_l, "Ana "), "recortar_espacios"),
      ascii = una(c("Jos&eacute;", "Jos&eacute;", "Ana"), "decodificar_entidades_html")
    )
  })
  for (r in resultados) {
    expect_identical(r$sin_marca$n, 0)
    expect_identical(r$latin1$n, 1)
    expect_identical(r$ascii$n, 0)
    expect_identical(r$sin_marca$bytes, c("4a6f73c3a9", "4a6f73c3a9", "416e61"))
    expect_identical(r$latin1$bytes, c("4a6f73c3a9", "4a6f73e9", "416e61"))
  }
  # Y los mismos conteos y bytes en todos los locales disponibles.
  expect_true(all(vapply(resultados, identical, logical(1L), resultados[[1L]])))
})

# A mano, con las clases de Unicode: macron debajo (0331), anillo debajo (0325)
# y linea vertical debajo (0329) son de clase 220 y van ANTES que el agudo
# (0301, 230). La tabla vieja les daba 230 y conservaba el orden de llegada.
.pares_nfc_nfd_o144 <- function() {
  list(
    c(intToUtf8(c(0x53, 0xE1, 0x331, 0x6C, 0x61)),
      intToUtf8(c(0x53, 0x61, 0x331, 0x301, 0x6C, 0x61))),
    c(intToUtf8(c(0x70, 0xE9, 0x325)), intToUtf8(c(0x70, 0x65, 0x325, 0x301))),
    c(intToUtf8(c(0x6F, 0xF3, 0x329)), intToUtf8(c(0x6F, 0x6F, 0x329, 0x301)))
  )
}

test_that("H6: NFC y NFD del mismo texto se igualan, y dos ordenes canonicamente distintos no", {
  sin_acentos <- normalizacion(acentos = FALSE)
  for (par in .pares_nfc_nfd_o144()) {
    expect_identical(.normalizados_o144(par, sin_acentos), 1L)
  }
  # Dos marcas de la misma clase (0323 y 0331, las dos 220) en distinto orden son
  # textos canonicamente DISTINTOS: el orden entre iguales no se toca.
  distintos <- c(intToUtf8(c(0x61, 0x323, 0x331)), intToUtf8(c(0x61, 0x331, 0x323)))
  expect_identical(.normalizados_o144(distintos, sin_acentos), 2L)

  # Los signos Kelvin y Angstrom son, para Unicode, la K y la A con anillo.
  expect_identical(
    .normalizados_o144(c("K", intToUtf8(0x212A), intToUtf8(0xC5), intToUtf8(0x212B)),
                       normalizacion(acentos = FALSE, minusculas = FALSE)),
    2L
  )
})

test_that("H6: detectar_duplicados_aproximados iguala NFC y NFD del mismo texto", {
  skip_if_not_installed("stringdist")
  sin_acentos <- normalizacion(acentos = FALSE)
  for (par in .pares_nfc_nfd_o144()) {
    r <- detectar_duplicados_aproximados(
      data.frame(x = par, stringsAsFactors = FALSE), columnas = "x",
      normalizar = sin_acentos, proteger_datos_personales = FALSE
    )
    expect_identical(as.character(r$pares$tipo_par), "exacto_normalizado")
  }
})

test_that("H6: las tablas de la descomposicion canonica son las de Unicode (oraculo stringi)", {
  skip_if_not_installed("stringi")
  clases <- lupa:::.normalizacion_clases_combinantes
  marcas <- as.integer(names(clases))
  expect_identical(marcas, 0x0300:0x0332)
  oraculo <- vapply(marcas, function(m) {
    for (valor in c(1L, 202L, 214L, 216L, 218L, 220L, 222L, 224L, 226L, 228L,
                    230L, 232L, 233L, 234L, 240L)) {
      if (stringi::stri_detect_regex(intToUtf8(m), sprintf("^\\p{ccc=%d}$", valor))) {
        return(valor)
      }
    }
    NA_integer_
  }, integer(1L))
  expect_identical(unname(clases), oraculo)

  descomposiciones <- lupa:::.normalizacion_decomposiciones
  claves <- as.integer(names(descomposiciones))
  expect_true(all(c(0x212A, 0x212B) %in% claves))
  nfd <- stringi::stri_trans_nfd(intToUtf8(claves, multiple = TRUE))
  expect_identical(
    lapply(descomposiciones, as.integer),
    stats::setNames(lapply(nfd, utf8ToInt), names(descomposiciones))
  )
})

.blancos_o144 <- function() {
  q <- "\""
  c(
    paste0(q, "Juan", q, "\tPerez"), paste0(q, "Juan", q, " Perez"),
    paste0("Calle ", q, "Uno", q, "\r\nApto 2"), paste0("Calle ", q, "Uno", q, "\nApto 2"),
    paste0("Calle ", q, "Uno", q, " Apto 2"),
    paste0("Perez\t", q, "Juan", q), paste0("Perez ", q, "Juan", q)
  )
}

test_that("H1: una comilla junto a una tabulacion o un salto se quita como junto a un espacio", {
  x <- .blancos_o144()
  # A mano: colapsar los blancos ASCII y despues quitar las comillas que abren o
  # cierran una palabra deja tres valores.
  a_mano <- gsub("\"", "", tolower(trimws(gsub("[[:space:]]+", " ", x))), fixed = TRUE)
  expect_identical(length(unique(a_mano)), 3L)
  expect_identical(.normalizados_o144(x), 3L)
})

test_that("H1: detectar_duplicados_aproximados iguala la comilla junto a una tabulacion", {
  skip_if_not_installed("stringdist")
  r <- detectar_duplicados_aproximados(
    data.frame(x = .blancos_o144()[1:2], stringsAsFactors = FALSE), columnas = "x",
    proteger_datos_personales = FALSE
  )
  expect_identical(as.character(r$pares$tipo_par), "exacto_normalizado")
})

test_that("H5: los dos caminos de la normalizacion dan la misma clave para las comillas", {
  # Un valor con dos o mas marcas combinantes -dos enies- va por el camino
  # escalar; con una, por el vectorial. La clave no puede depender de eso.
  pena <- intToUtf8(c(0x50, 0x65, 0xF1, 0x61))
  nunoa <- intToUtf8(c(0xD1, 0x75, 0xF1, 0x6F, 0x61))
  abre_cierra <- intToUtf8(c(0x20, 0x201C, 0x201D))
  # A mano: quitar la comilla final, el salto y la apertura y cierre sueltos deja
  # `pena` con enie y `nunoa` con dos: dos valores.
  x <- c(paste0(pena, "\"\n"), paste0(pena, "\""), paste0(nunoa, "\"\n"),
         paste0(nunoa, "\""), paste0(pena, abre_cierra), pena,
         paste0(nunoa, abre_cierra), nunoa)
  expect_identical(.normalizados_o144(x), 2L)

  # Y comparando las dos salidas, sin pasar por el perfil.
  casos <- c(x, "abc'\"", "\"abc\" d", "d 'abc'", paste0(nunoa, "'\""),
             paste0("x ", intToUtf8(0x2018), nunoa, intToUtf8(0x2019)))
  for (perfil in list(normalizacion(), normalizacion(espacios = FALSE),
                      normalizacion(acentos = FALSE))) {
    vectorial <- lupa:::.normalizacion_aplicar(casos, perfil)
    escalar <- vapply(casos, lupa:::.normalizacion_uno, character(1L),
                      perfil = perfil, USE.NAMES = FALSE)
    expect_identical(vectorial, escalar)
  }
})

test_that("H7: la sigma final se pliega al comparar y no al escribir minusculas", {
  mayusculas <- intToUtf8(c(0x3A4, 0x3A1, 0x395, 0x399, 0x3A3))
  minusculas <- intToUtf8(c(0x3C4, 0x3C1, 0x3B5, 0x3B9, 0x3C2))
  # A mano: CaseFolding de Unicode lleva 03A3 y 03C2 a 03C3; los dos son `treis`.
  expect_identical(.normalizados_o144(c(mayusculas, minusculas)), 1L)
  claves <- as.data.frame(detectar_claves(
    data.frame(x = c(mayusculas, minusculas), stringsAsFactors = FALSE)
  ))
  expect_identical(as.integer(claves$n_distintos_normalizados), 1L)
  expect_false(claves$unicidad_normalizada)

  # Control: `convertir_minusculas` ESCRIBE, y una sigma final bien escrita no se
  # toca. La columna tiene `ATHINA`/`athina` para que el plan proponga la accion.
  logos <- intToUtf8(c(0x3BB, 0x3BF, 0x3B3, 0x3BF, 0x3C2))
  datos <- data.frame(g = c(intToUtf8(c(0x391, 0x398, 0x397, 0x39D, 0x391)),
                            intToUtf8(c(0x3B1, 0x3B8, 0x3B7, 0x3BD, 0x3B1)), logos),
                      stringsAsFactors = FALSE)
  plan <- suppressWarnings(planificar_limpieza(suppressWarnings(perfilar(datos)), datos))
  plan$aplicar <- plan$estrategia == "convertir_minusculas"
  expect_identical(sum(plan$aplicar), 1L)
  r <- suppressWarnings(aplicar(plan, datos))
  expect_identical(utf8ToInt(r$datos$g[[3L]]), utf8ToInt(logos))
})

test_that("H8: perfilar y detectar_claves cuentan igual los normalizados con la marca bytes", {
  con_tilde <- rawToChar(as.raw(c(0x43, 0x61, 0x66, 0xc3, 0xa9, 0x20, 0x43, 0x65,
                                  0x6e, 0x74, 0x72, 0x61, 0x6c)))
  Encoding(con_tilde) <- "bytes"
  x <- c("Cafe Central", con_tilde, "CAFE CENTRAL")
  # A mano: `bytes` declara que eso no se interpreta como texto, y entra con su
  # clave de bytes `Caf\xc3\xa9 Central`, que no se pliega a `cafe central`.
  # Quedan dos: `cafe central` y la de bytes.
  datos <- data.frame(x = x, stringsAsFactors = FALSE)
  claves <- as.data.frame(detectar_claves(datos))
  expect_identical(as.integer(claves$n_distintos_normalizados), 2L)
  expect_identical(.normalizados_o144(x), 2L)

  # Control: la misma tilde marcada UTF-8 es texto y se pliega con las otras dos.
  Encoding(con_tilde) <- "UTF-8"
  expect_identical(.normalizados_o144(c("Cafe Central", con_tilde, "CAFE CENTRAL")), 1L)
})

test_that("H4: una celda irreparable no bloquea la reparacion y la parcial se selecciona marcando aplicar", {
  # `Canada` con enie y `Senor` con enie leidos una vez como CP1252, y una celda
  # con el caracter de reemplazo que ninguna reparacion local recupera.
  canada <- "Ca\u00c3\u00b1ada"
  senor <- "Se\u00c3\u00b1or"
  datos <- data.frame(v = c(canada, canada, senor, "Montevideo", "Pe\ufffda"),
                      stringsAsFactors = FALSE)
  plan <- suppressWarnings(planificar_limpieza(suppressWarnings(perfilar(datos)), datos))
  i <- which(plan$estrategia == "reparar_codificacion")
  expect_identical(length(i), 1L)
  expect_identical(as.character(plan$estado[[i]]), "lista")
  expect_true(plan$aplicar[[i]])
  expect_identical(plan$estado_reparacion[[i]], "reparado")
  plan$aplicar[-i] <- FALSE
  r <- suppressWarnings(aplicar(plan, datos))
  # A mano: las tres celdas CP1252 vuelven a su original y la U+FFFD queda.
  expect_identical(as.numeric(r$registro$n_cambiadas), 3)
  expect_identical(r$datos$v, c("Ca\u00f1ada", "Ca\u00f1ada", "Se\u00f1or",
                                "Montevideo", "Pe\ufffda"))

  # Una reparacion de verdad parcial no se activa sola, pero se selecciona como
  # dice la ayuda: marcando `aplicar`, sin reescribir `estado`.
  parcial <- data.frame(t = c("Direcci\u00c3\u00b3n\ufffd", "Paysand\u00c3\u00ba", "ok"),
                        stringsAsFactors = FALSE)
  plan <- suppressWarnings(planificar_limpieza(suppressWarnings(perfilar(parcial)), parcial))
  i <- which(plan$estrategia == "reparar_codificacion")
  expect_identical(length(i), 1L)
  expect_identical(as.character(plan$estado[[i]]), "lista")
  expect_false(plan$aplicar[[i]])
  plan$aplicar[] <- FALSE
  plan$aplicar[[i]] <- TRUE
  r <- suppressWarnings(aplicar(plan, parcial))
  expect_identical(as.numeric(r$registro$n_cambiadas), 2)
  expect_identical(r$datos$t, c("Direcci\u00f3n\ufffd", "Paysand\u00fa", "ok"))
})

test_that("bajo LC_CTYPE=C el texto UTF-8 sin marca no hace avisar a perfilar, planificar ni aplicar", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  if (!.locale_o144("C")) skip("No se pudo fijar LC_CTYPE=C.")

  # La tabla de r29t-18: bytes UTF-8 validos sin marca, como los deja una lectura
  # sin `encoding`. Antes, la proteccion pasaba `trimws()` sobre los valores sin
  # declararlos y R avisaba dos veces "unable to translate ... to UTF-8"; con
  # `options(warn = 2)` el aviso se volvia error y un `tryCatch` interno dejaba
  # sin aplicar el filtro del catalogo de ausencias.
  hex <- list(c(0x43, 0x61, 0xc3, 0x83, 0xc2, 0xb1, 0x61, 0x64, 0x61),
              c(0x53, 0x65, 0xc3, 0x83, 0xc2, 0xb1, 0x6f, 0x72),
              c(0x4a, 0x6f, 0x73, 0xc3, 0xa9),
              c(0x4d, 0x6f, 0x6e, 0x74, 0x65, 0x76, 0x69, 0x64, 0x65, 0x6f),
              c(0x50, 0x65, 0xc3, 0xb1, 0x61))
  nombre <- vapply(hex, function(b) rawToChar(as.raw(b)), character(1L))
  expect_true(all(Encoding(nombre) == "unknown"))
  datos <- data.frame(id = seq_along(nombre), nombre = nombre, stringsAsFactors = FALSE)

  avisos <- character()
  r <- withCallingHandlers({
    perfil <- perfilar(datos)
    plan <- planificar_limpieza(perfil, datos)
    aplicar(plan, datos)
  }, warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_identical(avisos, character())
  # A mano: las dos celdas con mojibake vuelven a `Canada` y `Senor` con enie.
  fila <- r$registro[r$registro$estrategia == "reparar_codificacion", , drop = FALSE]
  expect_identical(as.character(fila$estado), "ejecutada")
  expect_identical(as.numeric(fila$n_cambiadas), 2)

  # Los validadores declaran lo mismo: una cedula escrita con un espacio duro
  # (c2 a0) entre los digitos y el guion es la misma que con el guion pegado, y
  # bajo C el espacio duro sin marca se leia como dos bytes que no eran espacio.
  cedula <- rawToChar(as.raw(c(0x31, 0x2e, 0x32, 0x33, 0x34, 0x2e, 0x35, 0x36,
                               0x37, 0xc2, 0xa0, 0x2d, 0x32)))
  expect_identical(validar_ci_uy(c(cedula, "1.234.567-2")), c(TRUE, TRUE))
})
