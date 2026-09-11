# N48: los nombres son datos del usuario y un vector raw es una columna atomica.

.nombre_latin1_N48 <- function() {
  rawToChar(as.raw(c(
    0x63, 0x61, 0x74, 0x65, 0x67, 0x6f, 0x72, 0xed, 0x61
  )))
}

.tabla_nombre_latin1_N48 <- function() {
  nombre <- .nombre_latin1_N48()
  valor_invalido <- rawToChar(as.raw(c(0x76, 0xed, 0x6c, 0x6f, 0x72)))
  datos <- data.frame(
    x = c(1:6, 7), y = c(2:7, 8), z = c(3:8, 9),
    stringsAsFactors = FALSE
  )
  names(datos)[[1L]] <- nombre
  datos$texto <- c(rep(valor_invalido, 6L), "ok")
  datos
}

test_that("N48: nombres Latin-1 siguen el camino declarado en ambos locales", {
  nombre <- .nombre_latin1_N48()
  datos <- .tabla_nombre_latin1_N48()
  problema <- lupa:::.nombres_columnas_problematicos(nombre)
  expect_true(problema$codificacion_invalida[[1L]])
  expect_identical(
    charToRaw(problema$original[[1L]]), charToRaw(nombre)
  )
  expect_true(validUTF8(problema$propuesto[[1L]]))
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)

  for (locale in c("es_UY.UTF-8", "C")) {
    puesto <- suppressWarnings(tryCatch(
      Sys.setlocale("LC_CTYPE", locale), error = function(e) NA_character_
    ))
    if (is.na(puesto) || !identical(puesto, locale)) next

    perfil <- expect_no_error(perfilar(
      datos, analizar_dependencias = FALSE,
      casi_duplicados_vocabulario = FALSE,
      proteger_datos_personales = FALSE
    ))
    expect_identical(
      charToRaw(perfil$columnas$columna[[1L]]), charToRaw(nombre),
      info = locale
    )
    nombre_hallazgo <- perfil$hallazgos[
      perfil$hallazgos$tipo_hallazgo == "nombres_columnas_problematicos",
      , drop = FALSE
    ]
    expect_equal(nrow(nombre_hallazgo), 1L, info = locale)
    expect_match(nombre_hallazgo$descripcion[[1L]], "codific", info = locale)
    expect_identical(
      perfil$columnas$n_codificacion_invalida[
        perfil$columnas$columna == "texto"
      ], 6L,
      info = locale
    )

    sugerencia <- expect_no_error(sugerir_clave(
      datos, maximo = 5L
    ))
    expect_true(any(vapply(sugerencia$columna, function(x) {
      identical(charToRaw(x), charToRaw(nombre))
    }, logical(1L))), info = locale)

    plan <- expect_no_error(planificar_limpieza(perfil, datos))
    expect_true("normalizar_nombres" %in% plan$estrategia, info = locale)
  }
})

test_that("N48: raw se convierte a texto en las tres puertas", {
  for (columna in list(as.raw(1:3), I(as.raw(1:3)))) {
    datos <- data.frame(x = 1:3)
    datos$r <- columna
    preparado <- lupa:::.texto_analizable(columna)
    expect_true(preparado$analizable)
    expect_identical(preparado$valores, c("01", "02", "03"))
    expect_identical(preparado$valores_identidad, columna)

    perfil <- expect_no_error(perfilar(
      datos, analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    ))
    expect_no_error(distribucion_valores(
      datos, perfil = perfil, proteger_datos_personales = FALSE
    ))
    distribucion <- distribucion_valores(
      datos, perfil = perfil, proteger_datos_personales = FALSE
    )
    expect_identical(
      distribucion$frecuencias$valor[
        distribucion$frecuencias$columna == "r"
      ], c("01", "02", "03")
    )

    expect_no_error(analizar(
      datos, proteger_datos_personales = FALSE,
      argumentos_perfil = list(analizar_dependencias = FALSE)
    ))
  }
})
