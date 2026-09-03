.codigo_utf8_locale <- function(hexadecimal) {
  codigo <- strtoi(hexadecimal, base = 16L)
  bytes <- if (codigo <= 0x7F) {
    codigo
  } else if (codigo <= 0x7FF) {
    c(0xC0 + codigo %/% 0x40, 0x80 + codigo %% 0x40)
  } else {
    c(
      0xE0 + codigo %/% 0x1000,
      0x80 + (codigo %/% 0x40) %% 0x40,
      0x80 + codigo %% 0x40
    )
  }
  resultado <- rawToChar(as.raw(bytes))
  Encoding(resultado) <- "UTF-8"
  resultado
}

test_that("el plegado escalar de caja conserva y baja acentos en C", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  puesto <- suppressWarnings(tryCatch(
    Sys.setlocale("LC_CTYPE", "C"),
    error = function(e) NA_character_
  ))
  if (is.na(puesto) || !identical(puesto, "C")) {
    skip("no se pudo fijar LC_CTYPE=C")
  }

  superiores <- c("00C1", "00C9", "00D1", "00D3", "00DA", "00DC")
  inferiores <- c("00E1", "00E9", "00F1", "00F3", "00FA", "00FC")
  mayusculas <- vapply(superiores, .codigo_utf8_locale, character(1L))
  minusculas <- vapply(
    inferiores, .codigo_utf8_locale, character(1L)
  )
  entrada <- paste0("I", paste0(mayusculas, collapse = ""))
  esperado <- paste0("i", paste0(minusculas, collapse = ""))

  expect_identical(lupa:::.normalizacion_minusculas(entrada), esperado)
})

test_that("el camino vectorial pliega caracteres fuera de la tabla de descomposicion", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  puesto <- suppressWarnings(tryCatch(
    Sys.setlocale("LC_CTYPE", "C"),
    error = function(e) NA_character_
  ))
  if (is.na(puesto) || !identical(puesto, "C")) {
    skip("no se pudo fijar LC_CTYPE=C")
  }

  mayuscula <- .codigo_utf8_locale("1E9E")
  minuscula <- .codigo_utf8_locale("00DF")
  perfil <- normalizacion(
    espacios = FALSE, acentos = FALSE, comillas = FALSE, minusculas = TRUE
  )
  entrada <- c(paste0("X", mayuscula), paste0("x", minuscula))
  esperado <- c(paste0("x", minuscula), paste0("x", minuscula))

  expect_identical(lupa:::.normalizacion_aplicar(entrada, perfil), esperado)
})

test_that("el mismo dato normalizado da lo mismo bajo dos LC_CTYPE", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  aguda <- .codigo_utf8_locale("00C9")
  aguda_baja <- .codigo_utf8_locale("00E9")
  eszett <- .codigo_utf8_locale("1E9E")
  eszett_baja <- .codigo_utf8_locale("00DF")
  entrada <- c(
    paste0("CAF", aguda), paste0("caf", aguda_baja), eszett, eszett_baja
  )
  perfil <- normalizacion(
    espacios = FALSE, acentos = FALSE, comillas = FALSE, minusculas = TRUE
  )
  resultados <- lapply(c("C", "es_UY.UTF-8"), function(locale) {
    puesto <- suppressWarnings(tryCatch(
      Sys.setlocale("LC_CTYPE", locale),
      error = function(e) NA_character_
    ))
    if (is.na(puesto) || !identical(puesto, locale)) {
      skip(paste0("no se pudo fijar LC_CTYPE=", locale))
    }
    lupa:::.normalizacion_aplicar(entrada, perfil)
  })

  expect_identical(resultados[[1L]], resultados[[2L]])
})

test_that("las variantes de mayusculas acentuadas se detectan bajo C", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  puesto <- suppressWarnings(tryCatch(
    Sys.setlocale("LC_CTYPE", "C"),
    error = function(e) NA_character_
  ))
  if (is.na(puesto) || !identical(puesto, "C")) {
    skip("no se pudo fijar LC_CTYPE=C")
  }

  aguda_mayuscula <- .codigo_utf8_locale("00C9")
  aguda_minuscula <- .codigo_utf8_locale("00E9")
  diagnostico <- lupa:::.diagnosticar_texto(c(
    paste0("CAF", aguda_mayuscula), paste0("caf", aguda_minuscula)
  ))

  expect_identical(diagnostico$n_variantes_mayusculas, 2L)
})

test_that("el rango de duplicados usa orden por bytes en cualquier LC_COLLATE", {
  anterior <- Sys.getlocale("LC_COLLATE")
  on.exit(try(Sys.setlocale("LC_COLLATE", anterior), silent = TRUE), add = TRUE)
  valores <- c("A", "a", .codigo_utf8_locale("00C1"), "z", "N")
  esperado <- c(1L, 3L, 5L, 4L, 2L)

  for (locale in c("C", "es_UY.UTF-8")) {
    puesto <- suppressWarnings(tryCatch(
      Sys.setlocale("LC_COLLATE", locale),
      error = function(e) NA_character_
    ))
    if (is.na(puesto) || !identical(puesto, locale)) {
      skip(paste0("no se pudo fijar LC_COLLATE=", locale))
    }
    rango <- lupa:::.rango_canonico_duplicados(valores, seq_along(valores))
    expect_identical(as.integer(rango), esperado, info = locale)
  }
})

test_that("el desempate de patrones por bloques no usa LC_COLLATE", {
  anterior <- Sys.getlocale("LC_COLLATE")
  on.exit(try(Sys.setlocale("LC_COLLATE", anterior), silent = TRUE), add = TRUE)
  entrada <- c("A1", "a1", "_A", "_a")

  for (locale in c("C", "es_UY.UTF-8")) {
    puesto <- suppressWarnings(tryCatch(
      Sys.setlocale("LC_COLLATE", locale),
      error = function(e) NA_character_
    ))
    if (is.na(puesto) || !identical(puesto, locale)) {
      skip(paste0("no se pudo fijar LC_COLLATE=", locale))
    }
    resultado <- lupa:::.descubrir_patrones_bloques(
      entrada, max_patrones = 1L, tamano = 1L
    )
    expect_identical(resultado$patron[[1L]], "A9", info = locale)
  }
})

test_that("los identificadores Oracle no usan toupper dependiente del locale", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  puesto <- suppressWarnings(tryCatch(
    Sys.setlocale("LC_CTYPE", "tr_TR.UTF-8"),
    error = function(e) NA_character_
  ))
  if (is.na(puesto) || !identical(puesto, "tr_TR.UTF-8")) {
    skip("no se pudo fijar LC_CTYPE=tr_TR.UTF-8")
  }

  consultas <- lupa:::.consultas_clave_primaria()
  indice <- which(vapply(consultas, function(x) {
    identical(x$nombre, "all_constraints")
  }, logical(1L)))
  sql <- consultas[[indice[[1L]]]]$sql("esquema", "mi_tabla")

  expect_true(grepl("t.table_name = 'MI_TABLA'", sql, fixed = TRUE))
  expect_true(grepl("t.owner = 'ESQUEMA'", sql, fixed = TRUE))
})

test_that("el texto de los cortes usa punto bajo LC_NUMERIC con coma", {
  anterior <- Sys.getlocale("LC_NUMERIC")
  on.exit(try(Sys.setlocale("LC_NUMERIC", anterior), silent = TRUE), add = TRUE)
  puesto <- suppressWarnings(tryCatch(
    Sys.setlocale("LC_NUMERIC", "de_DE.UTF-8"),
    error = function(e) NA_character_
  ))
  if (is.na(puesto) || !identical(puesto, "de_DE.UTF-8")) {
    skip("no se pudo fijar LC_NUMERIC=de_DE.UTF-8")
  }

  ajuste <- list(corte = 1.5, etiqueta_corte = NA_character_, clase_umbral = NULL)
  expect_identical(lupa:::.texto_corte_umbral(ajuste), "1.5")
})

test_that("la I mayuscula con punto se pliega igual en cualquier locale", {
  # U+0130 era la unica mayuscula latina sin entrada en el mapa, y es justo la
  # que el locale trata distinto: bajo un locale UTF-8 `tolower()` la baja a
  # `i`, y bajo `C` la deja intacta. Sin entrada propia, la clave normalizada de
  # "ISTANBUL" con I turca dependia de la maquina.
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  i_con_punto <- intToUtf8(0x0130)
  esperado <- 0x0069L

  expect_identical(
    utf8ToInt(lupa:::.normalizacion_minusculas(i_con_punto)), esperado
  )

  if (is.na(Sys.setlocale("LC_CTYPE", "C"))) {
    skip("no se pudo fijar LC_CTYPE=C en esta plataforma")
  }
  expect_identical(
    utf8ToInt(lupa:::.normalizacion_minusculas(i_con_punto)), esperado
  )
})
