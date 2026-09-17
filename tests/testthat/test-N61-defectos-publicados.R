.fixture_n61_publicacion <- function() {
  set.seed(7)
  n <- 120L
  datos <- data.frame(
    entero = c(1:100, rep(999L, 20L)),
    decimal = c(round(rnorm(100L, 3.5, 0.2), 4), rep(NA_real_, 20L)),
    # La moda decimal repetida hace visible el renderizado de `columnas$moda`.
    moda_decimal = c(rep(3.5, 80L), rep(2.25, 40L)),
    chico = c(runif(100L, 1e-6, 1e-4), rep(NA_real_, 20L)),
    grande = c(runif(100L, 1e6, 1e9), rep(NA_real_, 20L)),
    texto = c(
      rep("comun", 90L), rep("Comun", 25L),
      "raro1", "raro2", "raro3", "raro4", "raro5"
    ),
    casi_clave = c(1:118, 118L, 118L),
    stringsAsFactors = FALSE
  )
  names(datos)[names(datos) == "decimal"] <- "a\u00f1o_medici\u00f3n"
  datos
}

.perfil_n61_publicacion <- function() {
  suppressWarnings(perfilar(
    .fixture_n61_publicacion(),
    fecha = as.POSIXct("2026-09-17 12:00:00", tz = "UTC"),
    muestra = Inf, analizar_dependencias = TRUE,
    sentinelas_numericos = c(999, -99)
  ))
}

test_that("las cinco hojas publicadas son inmunes a scipen", {
  opciones <- options(scipen = 0, OutDec = ".")
  on.exit(options(opciones), add = TRUE)

  base <- .perfil_n61_publicacion()
  options(scipen = -5)
  alternativo <- .perfil_n61_publicacion()

  expect_identical(
    as.character(base$columnas$moda),
    as.character(alternativo$columnas$moda)
  )
  hojas <- list(
    evidencia = cbind(
      base$hallazgos$evidencia,
      alternativo$hallazgos$evidencia
    ),
    motivo = cbind(
      base$cobertura_diagnosticos$motivo,
      alternativo$cobertura_diagnosticos$motivo
    ),
    como_resolverlo = cbind(
      base$cobertura_diagnosticos$como_resolverlo,
      alternativo$cobertura_diagnosticos$como_resolverlo
    )
  )
  for (nombre in names(hojas)) {
    expect_identical(hojas[[nombre]][, 1L], hojas[[nombre]][, 2L], info = nombre)
  }
  expect_identical(
    base$meta$costo_tabla_ancha$fuente,
    alternativo$meta$costo_tabla_ancha$fuente
  )
  fila_moda <- base$columnas[base$columnas$columna == "moda_decimal", , drop = FALSE]
  expect_equal(as.character(fila_moda$moda), "3.5")
  expect_true(any(nzchar(as.character(base$hallazgos$evidencia))))
  expect_true(any(nzchar(as.character(base$cobertura_diagnosticos$motivo))))
})

test_that("la clasificacion personal y los sentinelas son inmunes a scipen y digits", {
  datos <- data.frame(
    cod = c(rep(1e7, 60L), rep(1234567, 40L), rep(999, 20L)),
    chico = c(rep(0.000123456789, 60L), rep(0.000234567891, 40L),
              rep(NA_real_, 20L)),
    stringsAsFactors = FALSE
  )
  opciones <- options(scipen = 0, digits = 7, OutDec = ".")
  on.exit(options(opciones), add = TRUE)

  referencia <- NULL
  for (scipen in c(0, -5, 100)) {
    for (digits in c(3, 7, 15)) {
      options(scipen = scipen, digits = digits)
      perfil <- suppressWarnings(perfilar(
        datos, fecha = as.POSIXct("2026-09-17 12:00:00", tz = "UTC"),
        muestra = Inf, analizar_dependencias = FALSE,
        sentinelas_numericos = c(999, -99)
      ))
      estado <- list(
        datos_personales = perfil$datos_personales,
        sentinelas_numericos = perfil$meta$sentinelas_numericos
      )
      if (is.null(referencia)) referencia <- estado
      expect_identical(estado, referencia,
                       info = paste0("scipen=", scipen, ", digits=", digits))
    }
  }
})

.fijar_locale_n61 <- function(locale) {
  categorias <- c("LC_CTYPE", "LC_COLLATE")
  ok <- vapply(categorias, function(categoria) {
    suppressWarnings(Sys.setlocale(categoria, locale))
    identical(Sys.getlocale(categoria), locale)
  }, logical(1L))
  all(ok)
}

.perfil_n61_locale <- function() {
  datos <- .fixture_n61_publicacion()
  sin_marca <- function(x) rawToChar(charToRaw(x))
  nombre_acento <- intToUtf8(c(
    97L, 241L, 111L, 95L, 109L, 101L, 100L,
    105L, 99L, 105L, 243L, 110L
  ))
  names(datos)[names(datos) == nombre_acento] <- sin_marca(nombre_acento)
  perfilar(
    datos,
    fecha = as.POSIXct("2026-09-17 12:00:00", tz = "UTC"),
    muestra = Inf, analizar_dependencias = TRUE,
    sentinelas_numericos = c(999, -99),
    columnas_sin_ceros = names(datos)[1:3],
    columnas_no_negativas = names(datos)[4:5]
  )
}

.fixture_n61_ausencia <- function() {
  nombre_real <- rawToChar(charToRaw("categor\u00eda"))
  datos <- data.frame(
    entero = c(seq_len(100L), rep(NA_integer_, 20L)),
    categoria = c(rep("categor\u00eda", 100L), rep("otra", 20L)),
    stringsAsFactors = FALSE
  )
  names(datos)[[2L]] <- nombre_real
  datos
}

test_that("el consejo de ausencia estructural conserva el nombre real", {
  categorias <- c("LC_CTYPE", "LC_COLLATE")
  originales <- stats::setNames(
    vapply(categorias, Sys.getlocale, character(1L)), categorias
  )
  on.exit(
    for (categoria in categorias) {
      suppressWarnings(Sys.setlocale(categoria, originales[[categoria]]))
    },
    add = TRUE
  )
  locale_utf8 <- if (.fijar_locale_n61("es_UY.UTF-8")) {
    "es_UY.UTF-8"
  } else if (.fijar_locale_n61("es_UY.utf8")) {
    "es_UY.utf8"
  } else {
    NULL
  }
  if (is.null(locale_utf8)) {
    skip("no hay un locale es_UY UTF-8 disponible")
  }

  datos <- .fixture_n61_ausencia()
  nombre_real <- names(datos)[[2L]]
  perfiles <- lapply(c(locale_utf8, "C"), function(locale) {
    expect_true(.fijar_locale_n61(locale))
    suppressWarnings(perfilar(
      datos, fecha = as.POSIXct("2026-09-17 12:00:00", tz = "UTC"),
      muestra = Inf, analizar_dependencias = FALSE
    ))
  })
  indice <- vapply(
    perfiles,
    function(perfil) which(perfil$hallazgos$tipo_hallazgo ==
                             "posible_ausencia_estructural")[[1L]],
    integer(1L)
  )
  sugerencias <- Map(
    function(perfil, i) perfil$hallazgos$sugerencia[[i]], perfiles, indice
  )
  evidencias <- Map(
    function(perfil, i) perfil$hallazgos$evidencia[[i]], perfiles, indice
  )
  expect_identical(evidencias[[1L]], evidencias[[2L]])
  expect_identical(sugerencias[[1L]], sugerencias[[2L]])
  expect_true(grepl(
    .marcar_utf8_textos(nombre_real), sugerencias[[1L]], fixed = TRUE
  ))
  expect_false(grepl("<c3><ad>", sugerencias[[1L]], fixed = TRUE))
})

test_that("la deriva de configuracion cruza saveRDS sin depender del locale", {
  categorias <- c("LC_CTYPE", "LC_COLLATE")
  originales <- stats::setNames(
    vapply(categorias, Sys.getlocale, character(1L)), categorias
  )
  on.exit(
    for (categoria in categorias) {
      suppressWarnings(Sys.setlocale(categoria, originales[[categoria]]))
    },
    add = TRUE
  )
  locale_utf8 <- if (.fijar_locale_n61("es_UY.UTF-8")) {
    "es_UY.UTF-8"
  } else if (.fijar_locale_n61("es_UY.utf8")) {
    "es_UY.utf8"
  } else {
    NULL
  }
  if (is.null(locale_utf8)) {
    skip("no hay un locale es_UY UTF-8 disponible")
  }

  for (direccion in list(c(locale_utf8, "C"), c("C", locale_utf8))) {
    expect_true(.fijar_locale_n61(direccion[[1L]]))
    guardado <- suppressWarnings(.perfil_n61_locale())
    archivo <- tempfile(fileext = ".rds")
    on.exit(unlink(archivo), add = TRUE)
    saveRDS(guardado, archivo)
    expect_true(.fijar_locale_n61(direccion[[2L]]))
    deriva <- suppressWarnings(as.data.frame(comparar_perfiles(
      readRDS(archivo), .perfil_n61_locale()
    )))
    configuracion <- startsWith(as.character(deriva$aspecto), "configuracion_")
    expect_equal(
      sum(configuracion), 0L,
      info = paste0("guardado=", direccion[[1L]],
                    ", comparado=", direccion[[2L]])
    )
  }
})

test_that("el formateo fijo respeta OutDec", {
  opciones <- options(scipen = -5, OutDec = ",")
  on.exit(options(opciones), add = TRUE)
  expect_identical(.formatear_numero_publicado(3.5), "3,5")
  expect_identical(.formatear_numero_publicado(3), "3")
})

test_that("la secuencia integer64 sin bit64 se declara como no evaluada", {
  # No se desinstala `bit64` dentro de la suite: eso no es portable ni
  # reversible. Se inyecta la condicion en el predicado interno para probar
  # esta rama real; la reproduccion en procesos separados de instalado/no
  # cargado queda documentada en el informe de desarrollo.
  local_mocked_bindings(
    .bit64_disponible = function() FALSE,
    .package = "lupa"
  )
  datos <- data.frame(codigo = c(1, 2))
  class(datos$codigo) <- "integer64"
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas[1L, , drop = FALSE]
  campos <- c(
    "secuencia_entera_densa", "densidad_secuencia_entera",
    "n_posiciones_secuencia_entera", "n_huecos_secuencia_entera",
    "hueco_maximo_secuencia_entera"
  )
  expect_true(all(vapply(campos, function(campo) is.na(fila[[campo]]), logical(1L))))
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "secuencia_entera", ,
    drop = FALSE
  ]
  expect_equal(nrow(cobertura), 1L)
  expect_equal(as.character(cobertura$dependencia), "bit64")
  expect_match(cobertura$motivo, "no se pudo evaluar.*bit64", ignore.case = TRUE)
  expect_false("secuencia_entera" %in% as.character(perfil$hallazgos$tipo_hallazgo))
})

# El arreglo de la notacion cientifica reemplazo `as.character()` por
# `format(..., scientific = FALSE)`, y eso cerro `scipen` y **abrio** `digits`,
# que `as.character()` no tenia. Medido en su momento: la moda de
# `123456.789012345` se publicaba `123456.8` SIN tocar ninguna opcion, y
# `123457` con `digits = 3`. `perfilar.Rd` promete el valor "tal como llego,
# sin reinterpretarlo", asi que truncar a siete cifras significativas lo
# desmiente mas de lo que lo desmentia `3.5e+00`.
#
# Estas dos comprobaciones existen porque la comprobacion de cierre del ARREGLO
# no puede ser la misma que la del defecto: aquella preguntaba "cambia con
# `scipen`?" y la respuesta era no. La que faltaba era "cambia con algo MAS, que
# antes no cambiaba?".
.fixture_n61_precision <- function() {
  data.frame(
    # Mas de siete cifras significativas: con `3.5` este eje no se ve, porque
    # se imprime igual con cualquier `digits`.
    larga = c(rep(123456.789012345, 80L), rep(0.000123456789, 40L)),
    stringsAsFactors = FALSE
  )
}

.perfil_n61_precision <- function() {
  suppressWarnings(perfilar(
    .fixture_n61_precision(),
    fecha = as.POSIXct("2026-09-17 12:00:00", tz = "UTC"),
    muestra = Inf, analizar_dependencias = FALSE
  ))
}

test_that("la moda publicada conserva el valor completo, no lo trunca", {
  opciones <- options(scipen = 0, OutDec = ".", digits = 7)
  on.exit(options(opciones), add = TRUE)
  moda <- as.character(.perfil_n61_precision()$columnas$moda[[1L]])
  # La promesa es el valor tal como llego. Con `digits` por omision, `format()`
  # lo habria dejado en "123456.8".
  expect_identical(moda, "123456.789012345")
})

test_that("las hojas publicadas son inmunes a options(digits)", {
  opciones <- options(scipen = 0, OutDec = ".", digits = 7)
  on.exit(options(opciones), add = TRUE)

  base <- .perfil_n61_precision()
  options(digits = 3)
  pocos <- .perfil_n61_precision()
  options(digits = 15)
  muchos <- .perfil_n61_precision()

  for (otro in list(pocos, muchos)) {
    expect_identical(
      as.character(base$columnas$moda), as.character(otro$columnas$moda)
    )
    expect_identical(
      as.character(base$cobertura_diagnosticos$motivo),
      as.character(otro$cobertura_diagnosticos$motivo)
    )
    expect_identical(
      as.character(base$hallazgos$evidencia),
      as.character(otro$hallazgos$evidencia)
    )
  }
})

test_that("el p-valor publicado de Benford es inmune a scipen", {
  opciones <- options(scipen = 0, digits = 7, OutDec = ".")
  on.exit(options(opciones), add = TRUE)
  base <- .perfil_n61_precision()
  options(scipen = 100, digits = 3)
  alternativo <- .perfil_n61_precision()
  evidencia <- function(perfil) {
    perfil$hallazgos$evidencia[
      perfil$hallazgos$tipo_hallazgo == "desviacion_benford"
    ]
  }
  expect_identical(evidencia(base), evidencia(alternativo))
})
