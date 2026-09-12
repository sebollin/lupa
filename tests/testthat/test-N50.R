# N50: los nombres de columna se comparan por su contenido, no por el locale.

suppressPackageStartupMessages(library(lupa))

.nombre_u8_N50 <- function() {
  rawToChar(as.raw(c(
    0x63, 0x61, 0x74, 0x65, 0x67, 0x6f, 0x72, 0xc3, 0xad, 0x61
  )))
}

.nombre_u8_N50_marcado <- function() {
  nombre <- .nombre_u8_N50()
  Encoding(nombre) <- "UTF-8"
  nombre
}

.datos_nombre_N50 <- function(dos = FALSE) {
  nombres <- if (dos) {
    c(.nombre_u8_N50(), .nombre_u8_N50_marcado())
  } else {
    .nombre_u8_N50()
  }
  datos <- data.frame(
    lapply(seq_along(nombres), function(i) seq_len(100L)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(datos) <- nombres
  datos
}

.localidades_N50 <- function() c("es_UY.UTF-8", "C")

test_that("N50.1: un nombre UTF-8 valido no es problematico bajo C", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  for (locale in .localidades_N50()) {
    puesto <- suppressWarnings(
      # `Sys.setlocale()` AVISA cuando el locale no existe, y `try()` no
      # silencia avisos: en toda maquina sin ese locale -CI, el contenedor y
      # CRAN- estas pruebas ensuciaban la salida con `WARN 6` en las cinco
      # plataformas. El aviso no aporta nada: la decision se toma mirando el
      # valor devuelto.
      try(Sys.setlocale("LC_CTYPE", locale), silent = TRUE)
    )
    if (!identical(puesto, locale)) next
    datos <- .datos_nombre_N50()
    perfil <- lupa::perfilar(
      datos, analizar_dependencias = FALSE,
      casi_duplicados_vocabulario = FALSE,
      proteger_datos_personales = FALSE
    )
    tipos <- perfil$hallazgos$tipo_hallazgo
    expect_true("posible_identificador" %in% tipos, info = locale)
    expect_false("nombres_columnas_problematicos" %in% tipos, info = locale)
    expect_identical(
      lupa:::.nombres_para_operar(.nombre_u8_N50()),
      lupa:::.nombres_para_operar(.nombre_u8_N50_marcado()),
      info = locale
    )
    expect_identical(
      lupa:::.nombres_make_names(.nombre_u8_N50()),
      lupa:::.nombres_make_names(.nombre_u8_N50_marcado()),
      info = locale
    )
    expect_identical(
      charToRaw(perfil$columnas$columna[[1L]]),
      charToRaw(names(datos)[[1L]]), info = locale
    )
  }
})

test_that("N50.2: desambiguar no publica columnas inexistentes", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  for (locale in .localidades_N50()) {
    puesto <- suppressWarnings(
      # `Sys.setlocale()` AVISA cuando el locale no existe, y `try()` no
      # silencia avisos: en toda maquina sin ese locale -CI, el contenedor y
      # CRAN- estas pruebas ensuciaban la salida con `WARN 6` en las cinco
      # plataformas. El aviso no aporta nada: la decision se toma mirando el
      # valor devuelto.
      try(Sys.setlocale("LC_CTYPE", locale), silent = TRUE)
    )
    if (!identical(puesto, locale)) next
    datos <- .datos_nombre_N50(dos = TRUE)
    claves <- lupa::detectar_claves(datos, max_combinacion = 1L)
    expect_true(all(claves$columnas %in% names(datos)), info = locale)
    perfil <- lupa::perfilar(
      datos, analizar_dependencias = FALSE,
      casi_duplicados_vocabulario = FALSE,
      proteger_datos_personales = FALSE
    )
    expect_equal(nrow(perfil$columnas), 2L, info = locale)
    expect_true(all(vapply(seq_len(2L), function(i) {
      identical(charToRaw(perfil$columnas$columna[[i]]),
                charToRaw(names(datos)[[i]]))
    }, logical(1L))), info = locale)
    expect_true(all(lupa:::.nombres_para_operar(perfil$columnas$columna) ==
                      lupa:::.nombres_para_operar(names(datos))), info = locale)

    nombre_descompuesto <- paste0("categori", intToUtf8(769L), "a")
    distintos <- data.frame(seq_len(100L), seq_len(100L), check.names = FALSE)
    names(distintos) <- c(.nombre_u8_N50_marcado(), nombre_descompuesto)
    expect_false(identical(
      charToRaw(names(distintos)[[1L]]), charToRaw(names(distintos)[[2L]])
    ), info = locale)
    claves_distintos <- lupa::detectar_claves(
      distintos, max_combinacion = 1L
    )
    expect_equal(length(claves_distintos$columnas), 2L, info = locale)
    expect_true(all(claves_distintos$columnas %in% names(distintos)),
                info = locale)
    perfil_distintos <- lupa::perfilar(
      distintos, analizar_dependencias = FALSE,
      casi_duplicados_vocabulario = FALSE, proteger_datos_personales = FALSE
    )
    expect_true(all(vapply(seq_len(2L), function(i) {
      identical(charToRaw(perfil_distintos$columnas$columna[[i]]),
                charToRaw(names(distintos)[[i]]))
    }, logical(1L))), info = locale)
  }
})

test_that("N50.3: todos los consumidores aceptan el perfil de la misma tabla", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  for (locale in .localidades_N50()) {
    puesto <- suppressWarnings(
      # `Sys.setlocale()` AVISA cuando el locale no existe, y `try()` no
      # silencia avisos: en toda maquina sin ese locale -CI, el contenedor y
      # CRAN- estas pruebas ensuciaban la salida con `WARN 6` en las cinco
      # plataformas. El aviso no aporta nada: la decision se toma mirando el
      # valor devuelto.
      try(Sys.setlocale("LC_CTYPE", locale), silent = TRUE)
    )
    if (!identical(puesto, locale)) next
    datos <- .datos_nombre_N50()
    perfil <- lupa::perfilar(
      datos, analizar_dependencias = FALSE,
      casi_duplicados_vocabulario = FALSE,
      proteger_datos_personales = FALSE
    )
    expect_no_error(lupa::distribucion_valores(
      datos, perfil = perfil, proteger_datos_personales = FALSE
    ))
    expect_no_error(lupa::analizar(
      datos, proteger_datos_personales = FALSE,
      argumentos_perfil = list(analizar_dependencias = FALSE)
    ))
    expect_no_error(lupa::clasificar_variables(
      datos, perfil = perfil
    ))
    expect_no_error(lupa::analizar_tiempo(
      datos, perfil = perfil, columnas = names(datos)
    ))
    expect_no_error(lupa::detectar_claves(
      datos, perfil = perfil, max_combinacion = 1L
    ))
    expect_no_error(lupa::detectar_duplicados_aproximados(
      datos, perfil = perfil, columnas = names(datos),
      proteger_datos_personales = FALSE
    ))
    expect_no_error(lupa::proponer_modelo(perfil, datos))
    expect_no_error(lupa::planificar_limpieza(perfil, datos))
  }
})

test_that("N50.4: la declaracion del usuario resuelve el nombre publicado", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  for (locale in .localidades_N50()) {
    puesto <- suppressWarnings(
      # `Sys.setlocale()` AVISA cuando el locale no existe, y `try()` no
      # silencia avisos: en toda maquina sin ese locale -CI, el contenedor y
      # CRAN- estas pruebas ensuciaban la salida con `WARN 6` en las cinco
      # plataformas. El aviso no aporta nada: la decision se toma mirando el
      # valor devuelto.
      try(Sys.setlocale("LC_CTYPE", locale), silent = TRUE)
    )
    if (!identical(puesto, locale)) next
    datos <- .datos_nombre_N50()
    datos$valor <- seq_len(nrow(datos))
    datos$aplicable <- TRUE
    nombre <- names(datos)[[1L]]
    expect_no_error(lupa::perfilar(
      datos, clave = nombre, columnas_sin_ceros = nombre,
      columnas_no_negativas = nombre, columnas_opcionales = "valor",
      columnas_personales = nombre,
      aplicabilidad = list(aplicable = ~ TRUE),
      analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
      proteger_datos_personales = FALSE
    ))
    expect_no_error(lupa::perfilar_por(
      datos, por = nombre, clave = "valor", min_filas = 1L,
      analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
      proteger_datos_personales = FALSE
    ))
    expect_no_error(lupa::detectar_duplicados_aproximados(
      datos, columnas = nombre, bloquear_por = nombre,
      proteger_datos_personales = FALSE
    ))
    expect_no_error(lupa::detectar_relaciones(
      datos, datos, columnas_candidatas = list(
        tabla1 = nombre, tabla2 = "valor"
      ), muestra = 100L
    ))
    expect_no_error(lupa::referencial(
      datos, clave = nombre, valor = "valor"
    ))
    claves_distintas <- lupa:::.nombres_para_operar(c(
      .nombre_u8_N50(), paste0("categori", intToUtf8(769L), "a")
    ))
    expect_equal(length(unique(claves_distintas)), 2L)
    invalido <- rawToChar(as.raw(0xC3))
    literal_marca <- "<lupa-byte:C3>"
    expect_equal(
      length(unique(lupa:::.nombres_para_operar(c(invalido, literal_marca)))),
      2L, info = locale
    )
  }
})
