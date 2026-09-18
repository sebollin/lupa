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

# Cualquier locale UTF-8 sirve para esta comprobacion: lo que se contrasta es
# UTF-8 contra `C`, no una region. Pedir SOLO el uruguayo hace que la prueba se
# SALTEE en toda maquina que no lo tenga generada -las de CRAN entre ellas-, y
# una guarda que se saltea donde importa no se distingue de una que no existe.
#
# Este proyecto ya tuvo este defecto: el 2026-09-12 las cinco plataformas de CI
# pasaron de `WARN 0` a `WARN 6` con el mismo aviso, "cannot be honored". Volvio
# en estas pruebas. La lista de candidatos es la que ya usan otras cinco pruebas
# del paquete; se toma la primera que QUEDE PUESTA, comprobandolo con
# `Sys.getlocale()`, porque `Sys.setlocale()` avisa en vez de fallar.
.primer_locale_utf8_n61 <- function() {
  candidatos <- c(
    "es_UY.UTF-8", "es_UY.utf8", "es_ES.UTF-8", "es_ES.utf8",
    "en_US.UTF-8", "en_US.utf8", "C.UTF-8", "C.utf8"
  )
  for (candidato in candidatos) {
    if (.fijar_locale_n61(candidato)) return(candidato)
  }
  NULL
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
  locale_utf8 <- .primer_locale_utf8_n61()
  if (is.null(locale_utf8)) {
    skip("no hay ningun locale UTF-8 disponible en esta maquina")
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
  locale_utf8 <- .primer_locale_utf8_n61()
  if (is.null(locale_utf8)) {
    skip("no hay ningun locale UTF-8 disponible en esta maquina")
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

test_that("la evidencia decimal usa una sola marca con OutDec", {
  # La deteccion se apoya en stringdist, que esta en Suggests: bajo
  # `_R_CHECK_DEPENDS_ONLY_=true` -que es como corre CRAN- no existe, no hay
  # hallazgos, y la prueba indexaba vacio. Aparecio en el check, nunca en la
  # suite local.
  skip_if_not_installed("stringdist")
  opciones <- options(OutDec = ".")
  on.exit(options(opciones), add = TRUE)
  datos <- data.frame(
    nombre = c(rep("Montevideo", 20L), rep("Montevido", 3L), "Otro"),
    medida = c(rep(3.5, 20L), rep(2.5, 4L)),
    stringsAsFactors = FALSE
  )
  evidencia <- function(marca) {
    options(OutDec = marca)
    perfil <- perfilar(
      datos, analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    )
    list(
      moda = as.character(perfil$columnas$moda[
        perfil$columnas$columna == "medida"
      ]),
      evidencia = perfil$hallazgos$evidencia[[which(
        perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario"
      )[[1L]]]]
    )
  }

  con_punto <- evidencia(".")
  con_coma <- evidencia(",")
  expect_identical(con_punto$moda, "3.5")
  expect_identical(con_coma$moda, "3,5")
  expect_match(con_punto$evidencia, "asimetria=6\\.7", fixed = FALSE)
  expect_match(con_coma$evidencia, "asimetria=6,7", fixed = TRUE)
  expect_false(grepl("asimetria=6\\.7", con_coma$evidencia))
  expect_false(grepl("asimetria=6,7", con_punto$evidencia, fixed = TRUE))
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

.sin_marca_n63 <- function(x) {
  vapply(x, function(valor) rawToChar(charToRaw(valor)), character(1L))
}

.fixture_n63_publicacion <- function() {
  set.seed(7)
  n <- 60L
  datos <- data.frame(
    id = seq_len(n),
    texto = c(
      rep(.sin_marca_n63("com\u00fan"), 30L), rep("comun", 25L),
      .sin_marca_n63(c(
        "ca\u00f1\u00f3n", "C\u00f1\u00f3n", "c\u00d1\u00f3n",
        "c\u00e1\u00f1on", "flexi\u00f3n"
      ))
    ),
    acentos = .sin_marca_n63(c(
      rep("categor\u00eda", 40L), rep("categoria", 10L),
      rep("d\u00fcr\u00fcm", 5L), rep("raz\u00f3n", 5L)
    )),
    orden_alf = .sin_marca_n63(rep(c(
      "entidad_\u00e1\u00e9", "entidad_b", "entidad_\u00f1",
      "entidad_z", "entidad_a"
    ), 12L)),
    fechas_txt = c(
      rep("2024-01-15", 30L), rep("31/12/2023", 15L),
      "2024-06-01", "01-ago-2024", rep("2024-12-31", 10L),
      "2023-01-01", "no-fecha", NA_character_
    ),
    correos = c(
      rep("usuario@ejemplo.uy", 50L), "sin-arroba", "otro@test.org",
      .sin_marca_n63("m\u00e1l_acento@ejemplo.uy"), rep("a@b.c", 7L)
    ),
    numerico = c(rnorm(50L, 100, 20), rep(NA_real_, 10L)),
    entero = c(seq_len(55L), rep(999L, 5L)),
    stringsAsFactors = FALSE
  )
  names(datos)[names(datos) == "acentos"] <- .sin_marca_n63("categor\u00eda")
  names(datos)[names(datos) == "orden_alf"] <- .sin_marca_n63("a\u00f1o_medici\u00f3n")
  datos
}

.capturar_n63_publicacion <- function(locale) {
  expect_true(.fijar_locale_n61(locale))
  datos <- .fixture_n63_publicacion()
  perfil <- suppressWarnings(perfilar(datos, muestra = Inf))
  propuesta <- proponer_modelo(
    perfil, datos = datos, max_valores_dominio = 5
  )
  avisos_print <- character()
  salida <- withCallingHandlers(
    capture.output(print(propuesta)),
    warning = function(condicion) {
      avisos_print <<- c(avisos_print, conditionMessage(condicion))
      invokeRestart("muffleWarning")
    }
  )
  sin_metodo <- tryCatch(
    capture.output(print.data.frame(propuesta)),
    error = function(condicion) NULL
  )
  avisos <- character()
  cambios <- withCallingHandlers(
    comparar_perfiles(perfil, perfil),
    warning = function(condicion) {
      avisos <<- c(avisos, conditionMessage(condicion))
      invokeRestart("muffleWarning")
    }
  )
  list(
    salida = salida, avisos_print = avisos_print, sin_metodo = sin_metodo,
    avisos = avisos, cambios = cambios
  )
}

test_that("print de propuesta_modelo no aborta bajo C y conserva la tabla", {
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
  locale_utf8 <- .primer_locale_utf8_n61()
  if (is.null(locale_utf8)) {
    skip("no hay ningun locale UTF-8 disponible en esta maquina")
  }

  utf8 <- .capturar_n63_publicacion(locale_utf8)
  bajo_c <- .capturar_n63_publicacion("C")

  # El defecto era que print() abortaba bajo C. La prueba de cierre es que
  # corra, sin avisos, y publicando la misma cantidad de filas que bajo UTF-8.
  expect_gt(length(utf8$salida), 1L)
  expect_identical(length(bajo_c$salida), length(utf8$salida))
  expect_identical(utf8$avisos_print, character())
  expect_identical(bajo_c$avisos_print, character())

  # Y el arreglo no le quita el formato a quien nunca tuvo el problema: bajo
  # UTF-8 la salida es exactamente la tabla alineada que produce R.
  expect_false(is.null(utf8$sin_metodo))
  expect_identical(utf8$salida, utf8$sin_metodo)

  # Sigue siendo una tabla, no un volcado separado por tabuladores.
  expect_false(any(grepl("\t", utf8$salida, fixed = TRUE)))
  expect_false(any(grepl("\t", bajo_c$salida, fixed = TRUE)))
})

test_that("comparar_perfiles no avisa con perfiles identicos bajo C", {
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
  locale_utf8 <- .primer_locale_utf8_n61()
  if (is.null(locale_utf8)) {
    skip("no hay ningun locale UTF-8 disponible en esta maquina")
  }

  for (locale in c(locale_utf8, "C")) {
    resultado <- .capturar_n63_publicacion(locale)
    expect_equal(length(resultado$avisos), 0L, info = locale)
    expect_equal(nrow(resultado$cambios), 0L, info = locale)
  }
})

test_that("el ultimo recurso del impresor no se traga errores ajenos", {
  imprimir <- getFromNamespace(".print_data_frame_bytes", "lupa")

  # El volcado por bytes existe para texto que no es UTF-8 valido. Acotado por
  # "fallo" en vez de por esa condicion, se tragaba el format() de una clase
  # del usuario y publicaba el valor crudo como si fuera el formateado.
  registerS3method(
    "format", "clase_rota_n63",
    function(x, ...) stop("FORMATO_USUARIO_N63", call. = FALSE),
    envir = environment()
  )
  con_formateador_roto <- data.frame(n = 1:2)
  con_formateador_roto$c <- structure(c(10, 20), class = "clase_rota_n63")

  expect_error(
    capture.output(print.data.frame(con_formateador_roto)), "FORMATO_USUARIO_N63"
  )
  expect_error(
    capture.output(imprimir(con_formateador_roto)), "FORMATO_USUARIO_N63"
  )
})

test_that("los bytes que no son UTF-8 validos se escapan y la tabla se conserva", {
  imprimir <- getFromNamespace(".print_data_frame_bytes", "lupa")

  # Estos bytes no son UTF-8 valido ni traen codificacion declarada, asi que se
  # escapan con el octal que usa el propio R -`\\377`-, que es ASCII: la copia
  # de exhibicion queda imprimible en cualquier locale y la tabla sigue siendo
  # una tabla alineada, no un volcado. El octal, y no `<ff>`, porque esa forma
  # la puede escribir el usuario y entonces dos valores distintos publicarian
  # lo mismo.
  crudo <- rawToChar(as.raw(c(0x41L, 0xffL, 0x42L)))
  expect_false(all(validUTF8(crudo)))
  con_bytes_invalidos <- data.frame(n = 1L)
  con_bytes_invalidos[["valor"]] <- I(list(rep(crudo, 3L)))

  expect_error(capture.output(print.data.frame(con_bytes_invalidos)))
  salida <- capture.output(imprimir(con_bytes_invalidos))
  expect_gt(length(salida), 1L)
  expect_true(any(grepl("377", salida, fixed = TRUE)))
  expect_false(any(grepl("A<ff>B", salida, fixed = TRUE)))
  expect_false(any(grepl("\t", salida, fixed = TRUE)))

  # Y la afirmacion que vale mas que la forma del escape: donde
  # `print.data.frame()` SI puede con los bytes tal cual, la salida es
  # exactamente la suya. Escapar de mas tambien es una diferencia, y este
  # expect es el que la ve.
  columna_simple <- data.frame(v = crudo, stringsAsFactors = FALSE)
  expect_identical(
    capture.output(imprimir(columna_simple)),
    capture.output(print.data.frame(columna_simple))
  )
})

test_that("el error ajeno llega aunque tambien haya bytes invalidos", {
  imprimir <- getFromNamespace(".print_data_frame_bytes", "lupa")

  # El caso que tumbo la version anterior del arreglo: la condicion que
  # habilitaba el camino alternativo -hay bytes invalidos- se cumplia, pero el
  # error que habia disparado el fallo era del usuario. Ahora no hay camino
  # alternativo, asi que no hay donde perderlo.
  registerS3method(
    "format", "ambos_n64",
    function(x, ...) stop("ERROR_AJENO_N64", call. = FALSE),
    envir = environment()
  )
  crudo <- rawToChar(as.raw(c(0x41L, 0xffL, 0x42L)))
  mezcla <- data.frame(n = 1:3, stringsAsFactors = FALSE)
  mezcla$invalido <- rep(crudo, 3L)
  mezcla$roto <- structure(c(1, 2, 3), class = "ambos_n64")

  expect_error(capture.output(imprimir(mezcla)), "ERROR_AJENO_N64")
})

test_that("imprimir no le cambia la marca a un objeto de referencia", {
  marcar <- getFromNamespace(".marcar_objeto_para_exhibir", "lupa")

  # Un environment no se copia: escribirle los atributos marcados le cambia el
  # objeto al usuario. Medido contra su control, print() de base lo deja como
  # estaba.
  sin_marca <- rawToChar(charToRaw("a\u00f1o_medici\u00f3n"))
  referencia <- new.env(parent = emptyenv())
  attr(referencia, "texto") <- sin_marca
  expect_identical(Encoding(attr(referencia, "texto")), "unknown")

  invisible(marcar(referencia))
  expect_identical(Encoding(attr(referencia, "texto")), "unknown")

  anidado <- list(hoja = referencia)
  invisible(marcar(anidado))
  expect_identical(Encoding(attr(referencia, "texto")), "unknown")
})

test_that("el error ajeno llega intacto aunque recorrer la columna tambien falle", {
  imprimir <- getFromNamespace(".print_data_frame_bytes", "lupa")

  # La condicion que habilita el ultimo recurso se evalua dentro del manejador
  # y recorre el marco con `[[`. Si esa clase tambien falla al indexarse, el
  # error del recorrido tapaba la causa real: el usuario recibia
  # INDEXACION_USUARIO en vez de FORMATO_USUARIO.
  registerS3method(
    "format", "explota_doble_n63",
    function(x, ...) stop("FORMATO_USUARIO_N63", call. = FALSE),
    envir = environment()
  )
  registerS3method(
    "[[", "explota_doble_n63",
    function(x, ...) stop("INDEXACION_USUARIO_N63", call. = FALSE),
    envir = environment()
  )
  doble <- data.frame(n = 1:3)
  doble$valor <- structure(
    list("uno", "dos", "tres"), class = c("explota_doble_n63", "list")
  )

  expect_error(capture.output(print.data.frame(doble)), "FORMATO_USUARIO_N63")
  expect_error(capture.output(imprimir(doble)), "FORMATO_USUARIO_N63")
})

test_that("la validez UTF-8 se decide con el criterio de R, no con el de iconv", {
  marcar <- getFromNamespace(".marcar_para_exhibir", "lupa")
  imprimir <- getFromNamespace(".print_data_frame_bytes", "lupa")

  # `iconv()` acepta secuencias fuera del rango de Unicode que `validUTF8()`
  # rechaza. Declarando UTF-8 con el criterio de iconv a secas, el paquete
  # publicaba `<U+00110000>`: un punto de codigo que no existe.
  fuera_de_rango <- rawToChar(as.raw(c(0xF4L, 0x90L, 0x80L, 0x80L)))
  expect_false(validUTF8(fuera_de_rango))
  expect_false(is.na(iconv(fuera_de_rango, "UTF-8", "UTF-8", sub = NA)))
  expect_identical(Encoding(marcar(fuera_de_rango)), "unknown")

  marco <- data.frame(v = fuera_de_rango, stringsAsFactors = FALSE)
  expect_identical(
    capture.output(imprimir(marco)),
    capture.output(print.data.frame(marco))
  )
})

test_that("el escape del reintento es reversible", {
  imprimir <- getFromNamespace(".print_data_frame_bytes", "lupa")

  # Dos valores distintos del usuario -el byte 0xff y el TEXTO literal que R
  # usa para representarlo- tienen que publicar celdas distintas. Con el escape
  # sin duplicar la barra publicaban la misma, que es el defecto que tenia la
  # forma `<ff>` mudado al camino del reintento.
  byte_crudo <- rawToChar(as.raw(0xffL))
  texto_literal <- "\\377"
  expect_false(identical(byte_crudo, texto_literal))

  marco <- data.frame(n = 1:2)
  marco$x <- I(list(byte_crudo, texto_literal))
  # El reintento sólo se dispara donde `print.data.frame()` no puede.
  expect_error(capture.output(print.data.frame(marco)))

  salida <- capture.output(imprimir(marco))
  celdas <- trimws(sub("^[0-9]+ +[0-9]+ +", "", salida[-1L]))
  expect_length(unique(celdas), 2L)
})
