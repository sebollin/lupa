# Ronda 30-L: el paquete entero bajo el locale C. La promesa es la misma
# respuesta en cualquier locale; cada bloque es un hallazgo de la refutacion,
# con el caso que lo reproducia y la cifra rehecha a mano. Cada prueba fija
# LC_CTYPE y LC_COLLATE, los dos, y los restaura: dentro de la suite testthat ya
# fija LC_COLLATE en C, asi que hay que nombrar los dos locales que se comparan.
# Los datos se arman con `rawToChar()`: el archivo es ASCII.

.hex_o148 <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) NA_character_ else paste(as.character(charToRaw(s)), collapse = "")
  }, character(1L), USE.NAMES = FALSE)
}

.guardar_o148 <- function() {
  c(LC_CTYPE = Sys.getlocale("LC_CTYPE"), LC_COLLATE = Sys.getlocale("LC_COLLATE"))
}

.restaurar_o148 <- function(anterior) {
  for (categoria in names(anterior)) {
    try(suppressWarnings(Sys.setlocale(categoria, anterior[[categoria]])),
        silent = TRUE)
  }
}

.fijar_o148 <- function(locale) {
  ok <- vapply(c("LC_CTYPE", "LC_COLLATE"), function(categoria) {
    puesto <- suppressWarnings(tryCatch(
      Sys.setlocale(categoria, locale), error = function(e) ""
    ))
    length(puesto) == 1L && !is.na(puesto) && nzchar(puesto)
  }, logical(1L))
  all(ok)
}

# `C` y un locale UTF-8, si la maquina los tiene. El UTF-8 se busca primero
# entre los que intercalan distinto que `C` (el espanol, el ingles).
.locales_o148 <- function() {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  utf8 <- NA_character_
  for (candidato in c("es_UY.UTF-8", "es_UY.utf8", "es_ES.UTF-8", "es_ES.utf8",
                      "en_US.UTF-8", "en_US.utf8", "C.UTF-8", "C.utf8")) {
    if (.fijar_o148(candidato) && isTRUE(l10n_info()[["UTF-8"]])) {
      utf8 <- candidato
      break
    }
  }
  c(if (.fijar_o148("C")) "C", if (!is.na(utf8)) utf8)
}

.F0_O148 <- as.POSIXct("2026-01-15", tz = "UTC")

test_that("H1: SI con tilde es logico y SIN INFORMACION con tilde es un faltante, bajo C y bajo UTF-8", {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  locales <- .locales_o148()
  if (!"C" %in% locales) skip("No se pudo fijar el locale C.")

  # S + I con tilde (c3 8d) y la O con tilde (c3 93, c3 b3): UTF-8 sin marca,
  # como los deja `read.csv()` sin `encoding`.
  si <- rawToChar(as.raw(c(0x53, 0xc3, 0x8d)))
  sin_info_may <- rawToChar(as.raw(c(0x53, 0x49, 0x4e, 0x20, 0x49, 0x4e, 0x46, 0x4f,
                                     0x52, 0x4d, 0x41, 0x43, 0x49, 0xc3, 0x93, 0x4e)))
  sin_info_min <- rawToChar(as.raw(c(0x53, 0x69, 0x6e, 0x20, 0x69, 0x6e, 0x66, 0x6f,
                                     0x72, 0x6d, 0x61, 0x63, 0x69, 0xc3, 0xb3, 0x6e)))
  for (locale in locales) {
    expect_true(.fijar_o148(locale))
    v <- rep(c(si, "NO"), 10)
    # A mano: los 20 valores son literales logicos (`si` con tilde y `no`).
    expect_identical(inferir_tipo(v)$tipo, "logico", info = locale)
    datos <- data.frame(id = 1:20, acepta = v, stringsAsFactors = FALSE)
    perfil <- perfilar(datos, fecha = .F0_O148, proteger_datos_personales = FALSE,
                       analizar_dependencias = FALSE)
    plan <- planificar_limpieza(perfil, datos)
    expect_identical(sum(plan$columna == "acepta"), 1L, info = locale)
    r <- aplicar(plan, datos)
    expect_identical(r$datos$acepta, rep(c(TRUE, FALSE), 10), info = locale)

    # A mano: 4 + 4 + 4 marcadores de ausencia.
    m <- c(rep(sin_info_may, 4), rep(sin_info_min, 4), rep("N/D", 4))
    datos <- data.frame(id = 1:24, marcador = c(m, paste0("valor", 1:12)),
                        stringsAsFactors = FALSE)
    perfil <- perfilar(datos, fecha = .F0_O148, proteger_datos_personales = FALSE,
                       analizar_dependencias = FALSE)
    cols <- columnas(perfil)
    expect_identical(
      as.numeric(cols$n_faltantes_disfrazados[cols$columna == "marcador"]), 12,
      info = locale
    )
    r <- aplicar(planificar_limpieza(perfil, datos), datos)
    expect_identical(sum(is.na(r$datos$marcador)), 12L, info = locale)
  }
})

test_that("H1: un plan armado en una sesion UTF-8 se ejecuta igual bajo C", {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  locales <- .locales_o148()
  if (length(locales) < 2L) skip("Hacen falta C y un locale UTF-8.")

  si <- rawToChar(as.raw(c(0x53, 0xc3, 0x8d)))
  datos <- data.frame(id = 1:20, acepta = rep(c(si, "NO"), 10),
                      stringsAsFactors = FALSE)
  expect_true(.fijar_o148(locales[[2L]]))
  plan <- planificar_limpieza(
    perfilar(datos, fecha = .F0_O148, proteger_datos_personales = FALSE,
             analizar_dependencias = FALSE),
    datos
  )
  expect_true(.fijar_o148("C"))
  r <- aplicar(plan, datos)
  # Antes: `fallida`, "Hay valores presentes que no pueden convertirse a logico."
  expect_identical(r$registro$estado, "ejecutada")
  expect_identical(r$datos$acepta, rep(c(TRUE, FALSE), 10))
})

test_that("H1: convertir_minusculas sobre una columna latin1 no escribe escapes bajo C", {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  locales <- .locales_o148()
  if (!"C" %in% locales) skip("No se pudo fijar el locale C.")

  # `A` + I con tilde + `B` y su minuscula, en latin1 (cd, ed).
  v <- c(rep(rawToChar(as.raw(c(0x41, 0xcd, 0x42))), 3),
         rep(rawToChar(as.raw(c(0x61, 0xed, 0x62))), 3), "otro", "OTRO")
  Encoding(v) <- "latin1"
  datos <- data.frame(x = v, stringsAsFactors = FALSE)
  for (locale in locales) {
    expect_true(.fijar_o148(locale))
    perfil <- perfilar(datos, fecha = .F0_O148, proteger_datos_personales = FALSE,
                       analizar_dependencias = FALSE)
    plan <- planificar_limpieza(perfil, datos)
    plan$aplicar <- plan$estrategia == "convertir_minusculas"
    expect_identical(sum(plan$aplicar), 1L)
    r <- aplicar(plan, datos)
    # A mano: las seis celdas quedan `a` + i con tilde (c3 ad) + `b`, y cambian
    # cuatro: las tres en mayuscula y `OTRO`. Antes, bajo C, el texto `a<cd>b`.
    expect_identical(.hex_o148(r$datos$x),
                     c(rep("61c3ad62", 6L), "6f74726f", "6f74726f"), info = locale)
    expect_identical(as.numeric(r$registro$n_cambiadas), 4, info = locale)
  }
})

test_that("H1: ninguna funcion del paquete pliega la caja con tolower, toupper o casefold", {
  # Por construccion: las tres consultan LC_CTYPE -bajo C no bajan la I con
  # tilde; bajo un locale turco la I baja a la i sin punto-. El texto del
  # usuario se pliega con el mapa del paquete y el vocabulario ASCII con
  # `chartr()`. Se recorre el espacio de nombres, que existe tambien instalado.
  ns <- asNamespace("lupa")
  nombres <- ls(ns, all.names = TRUE)
  usa <- vapply(nombres, function(nombre) {
    f <- get(nombre, envir = ns)
    is.function(f) && !is.primitive(f) &&
      any(c("tolower", "toupper", "casefold") %in% all.names(body(f)))
  }, logical(1L))
  expect_identical(names(usa)[usa], character())
})

test_that("H2: un importe con el euro sin marca se convierte igual bajo C y bajo UTF-8", {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  locales <- .locales_o148()
  if (!"C" %in% locales) skip("No se pudo fijar el locale C.")

  # El euro (e2 82 ac) + espacio + importe con coma decimal, UTF-8 sin marca.
  euro <- rawToChar(as.raw(c(0xe2, 0x82, 0xac)))
  importes <- c("10", "20,5", "30", "1.234,5")
  datos <- data.frame(id = 1:8, moneda = rep(paste(euro, importes), 2),
                      stringsAsFactors = FALSE)
  expect_identical(unique(Encoding(datos$moneda)), "unknown")
  for (locale in locales) {
    expect_true(.fijar_o148(locale))
    perfil <- perfilar(datos, fecha = .F0_O148, proteger_datos_personales = FALSE,
                       analizar_dependencias = FALSE)
    plan <- planificar_limpieza(perfil, datos)
    i <- plan$estrategia == "convertir_numero_regional"
    expect_identical(sum(i), 1L, info = locale)
    # Antes, bajo C: `aplicar = FALSE` y "no responden al formato numerico regional".
    expect_true(isTRUE(plan$aplicar[i]), info = locale)
    r <- aplicar(plan, datos)
    # A mano: 10, 20,5, 30 y 1.234,5 con coma decimal y punto de miles.
    expect_identical(r$datos$moneda, rep(c(10, 20.5, 30, 1234.5), 2), info = locale)
  }
})

test_that("H3: un nombre de columna con un byte latin1 suelto no tumba analizar() ni perfilar() en ningun locale", {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  opciones <- options(warn = 2)
  on.exit(options(opciones), add = TRUE)
  locales <- .locales_o148()
  if (length(locales) < 2L) skip("Hacen falta C y un locale UTF-8.")

  # `Tel` + e9 + `fono`: lo que deja `read.csv(check.names = FALSE)` sobre un
  # CSV Windows-1252 en una sesion UTF-8. No es UTF-8 valido.
  nombre <- rawToChar(as.raw(c(0x54, 0x65, 0x6c, 0xe9, 0x66, 0x6f, 0x6e, 0x6f)))
  datos <- data.frame(
    id = 1:30, nombre = rep(c("Ana Sosa", "Luis Gomez", "Rosa Diaz"), 10),
    x = rep(c("099123456", "098765432", "091111111"), 10),
    stringsAsFactors = FALSE
  )
  names(datos)[3] <- nombre
  expect_false(validUTF8(names(datos)[3]))
  medidas <- list()
  for (locale in locales) {
    expect_true(.fijar_o148(locale))
    # Antes, bajo UTF-8: "input string 1 is invalid in this locale" en el
    # `gsub(fixed = TRUE)` que escapa el nombre; y `perfilar()` con
    # `warn = 2` abortaba en el `strsplit()` de la proteccion.
    a <- analizar(datos, fecha = .F0_O148, id_medicion = "m1")
    medidas[[locale]] <- as.character(a$medicion$objeto_medible)
    p <- perfilar(datos, fecha = .F0_O148)
    expect_identical(nrow(p$columnas), 3L, info = locale)
  }
  # La misma medicion en los dos: un objeto medible por medida.
  expect_identical(.hex_o148(medidas[[1L]]), .hex_o148(medidas[[2L]]))
})

test_that("H4: el desempate de formatos y los id de la propuesta siguen los bytes, no la intercalacion", {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  locales <- .locales_o148()
  if (length(locales) < 2L) skip("Hacen falta C y un locale UTF-8.")
  expect_true(.fijar_o148(locales[[2L]]))
  # Si el locale UTF-8 intercala como C (C.UTF-8), la prueba no distingue nada.
  if (identical(sort(c("a", "B")), c("B", "a"))) {
    skip("El locale UTF-8 disponible intercala como C.")
  }

  fechas <- c(rep(c("2024-01-15", "2024-02-20"), 3),
              rep(c("15/03/2024", "20/04/2024"), 3))
  datos <- data.frame(
    id = 1:24,
    depto = rep(c("Paysandu", "Montevideo", "Salto", "Rivera", "Paysandu",
                  "Montevideo"), 4),
    `Fecha de Alta` = rep(c("2020-01-15", "2020-02-15"), 12),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  for (locale in locales) {
    expect_true(.fijar_o148(locale))
    # A mano: empate 6 a 6, y por bytes `%Y` (25 59) va antes que `%d` (25 64).
    expect_identical(detectar_formatos_fecha(fechas)$formato,
                     c("%Y-%m-%d", "%d/%m/%Y"), info = locale)
    propuesta <- proponer_modelo(
      perfilar(datos, fecha = .F0_O148, proteger_datos_personales = FALSE,
               analizar_dependencias = FALSE),
      datos
    )
    formato <- propuesta[propuesta$metrica == "Formato", , drop = FALSE]
    # A mano: `F` (46) va antes que `d` (64) en bytes; en la intercalacion
    # espanola, al reves. El id numera despues de ordenar.
    expect_identical(as.character(formato$atributos), c("Fecha de Alta", "depto"),
                     info = locale)
    expect_identical(formato$id_sugerencia, c("sugerencia-0001", "sugerencia-0002"),
                     info = locale)
  }
})

test_that("H4: ninguna funcion del paquete ordena con order() o sort() de base", {
  # Por construccion: todo pasa por `.orden_por_bytes()` y `.sort_por_bytes()`,
  # que ordenan el texto por sus bytes con `method = "radix"`. Solo esos
  # ayudantes y los que ya ordenaban por la clave de bytes llaman a base.
  ns <- asNamespace("lupa")
  permitidas <- c(".orden_por_bytes", ".orden_seguro", ".ordenar_por_bytes")
  nombres <- setdiff(ls(ns, all.names = TRUE), permitidas)
  usa <- vapply(nombres, function(nombre) {
    f <- get(nombre, envir = ns)
    is.function(f) && !is.primitive(f) &&
      any(c("order", "sort", "sort.int", "sort.list", "rank", "xtfrm") %in%
            all.names(body(f)))
  }, logical(1L))
  expect_identical(names(usa)[usa], character())
})

test_that("H5: una etiqueta que junta nombres latin1 sale UTF-8 en los dos locales, y un nombre solo, como llego", {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  locales <- .locales_o148()
  if (!"C" %in% locales) skip("No se pudo fijar el locale C.")

  # `descripcion` con la o con tilde en latin1 (f3).
  desc <- rawToChar(as.raw(c(0x64, 0x65, 0x73, 0x63, 0x72, 0x69, 0x70, 0x63,
                             0x69, 0xf3, 0x6e)))
  Encoding(desc) <- "latin1"
  claves <- data.frame(fecha = rep(c("a", "b"), 4),
                       x = rep(c("xx", "yy", "zz", "ww"), each = 2),
                       stringsAsFactors = FALSE)
  names(claves)[2] <- desc
  valores <- c("Montevideo", "Montevideo ", "Monte video", "Salto", "Salto",
               "Rivera", "Rivera", "Paysandu")
  dup <- data.frame(id = 1:8, x = valores, stringsAsFactors = FALSE)
  names(dup)[2] <- desc
  for (locale in locales) {
    expect_true(.fijar_o148(locale))
    k <- suppressWarnings(detectar_claves(claves))
    compuesta <- k$columnas[k$n_columnas == 2L]
    # A mano: `fecha + descripci` + c3 b3 (la o con tilde en UTF-8) + `n`.
    expect_identical(.hex_o148(compuesta),
                     "6665636861202b20646573637269706369c3b36e", info = locale)
    # Los duplicados aproximados necesitan `stringdist` (Suggests).
    if (!requireNamespace("stringdist", quietly = TRUE)) next
    d <- detectar_duplicados_aproximados(dup, columnas = desc,
                                         proteger_datos_personales = FALSE)
    etiqueta <- unique(d$hallazgos$columna)
    # Antes, bajo C: el texto `descripci<f3>n`, que no indexa la tabla.
    expect_identical(.hex_o148(etiqueta), "646573637269706369f36e", info = locale)
    expect_identical(dup[[etiqueta]], valores, info = locale)
  }
})

test_that("H6: clasificar_variables() no declara ausente un nivel que observo, bajo C", {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  locales <- .locales_o148()
  if (!"C" %in% locales) skip("No se pudo fijar el locale C.")

  # `Optimo` con la O con tilde (c3 93), sin marca, y `Alto`: los dos presentes.
  optimo <- rawToChar(as.raw(c(0xc3, 0x93, 0x70, 0x74, 0x69, 0x6d, 0x6f)))
  f <- factor(rep(c("Alto", optimo), 3), levels = c("Alto", optimo))
  for (locale in locales) {
    expect_true(.fijar_o148(locale))
    cv <- clasificar_variables(data.frame(nivel = f),
                               proteger_datos_personales = FALSE)
    # A mano: 2 niveles declarados, los 2 aparecen en los datos.
    expect_identical(as.integer(cv$n_niveles_declarados), 2L, info = locale)
    expect_identical(as.integer(cv$n_niveles_observados), 2L, info = locale)
    expect_identical(as.integer(cv$n_niveles_ausentes), 0L, info = locale)
  }
})

test_that("H7: la evidencia cita el ejemplo con los mismos bytes bajo C y bajo UTF-8", {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  locales <- .locales_o148()
  if (length(locales) < 2L) skip("Hacen falta C y un locale UTF-8.")

  jose <- rawToChar(as.raw(c(0x4a, 0x6f, 0x73, 0xc3, 0xa9, 0x20)))
  datos <- data.frame(id = 1:6, nombre = c(jose, "Ana", "Luis", "Rosa", "Ana", "Luis"),
                      stringsAsFactors = FALSE)
  evidencias <- list()
  for (locale in locales) {
    expect_true(.fijar_o148(locale))
    p <- perfilar(datos, fecha = .F0_O148, proteger_datos_personales = FALSE,
                  analizar_dependencias = FALSE)
    ev <- p$hallazgos$evidencia[p$hallazgos$tipo_hallazgo == "espacios_sobrantes"]
    expect_length(ev, 1L)
    evidencias[[locale]] <- .hex_o148(ev)
    # A mano: el ejemplo entre comillas lleva los bytes del valor, 4a 6f 73 c3 a9
    # 20 -antes, bajo C, el escape `Jos\u00e9`-, y asi casa con la tabla de patrones.
    expect_true(grepl("224a6f73c3a92022", evidencias[[locale]], fixed = TRUE),
                info = locale)
  }
  expect_identical(evidencias[[1L]], evidencias[[2L]])
})

test_that("H7: ninguna funcion del paquete cita con encodeString() fuera de su reemplazo", {
  ns <- asNamespace("lupa")
  nombres <- setdiff(ls(ns, all.names = TRUE), ".comillas_sin_locale_uno")
  usa <- vapply(nombres, function(nombre) {
    f <- get(nombre, envir = ns)
    is.function(f) && !is.primitive(f) && "encodeString" %in% all.names(body(f))
  }, logical(1L))
  expect_identical(names(usa)[usa], character())
})

test_that("H8: detectar_claves() con un nombre UTF-8 no avisa bajo C", {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  opciones <- options(warn = 2)
  on.exit(options(opciones), add = TRUE)
  locales <- .locales_o148()
  if (!"C" %in% locales) skip("No se pudo fijar el locale C.")

  datos <- data.frame(id = 1:4, x = c("a", "b", "a", "b"), y = c(1, 1, 2, 2))
  names(datos)[3] <- intToUtf8(c(0x61, 0xf1, 0x6f))
  for (locale in locales) {
    expect_true(.fijar_o148(locale))
    # Antes, bajo C: "(converted from warning) unable to translate 'a<U+00F1>o'
    # to native encoding" en `as.data.frame()` de `.es_clave()`.
    k <- detectar_claves(datos)
    # A mano: `id` sola, y `x` con la tercera -(a,1) (b,1) (a,2) (b,2)-.
    expect_identical(as.integer(k$n_columnas), c(1L, 2L), info = locale)
  }
})

# Lo que encontro el cierre por comparacion: el catalogo de funciones exportadas
# sobre las tablas de las sondas, en dos procesos, campo por campo. Cada bloque
# es de la familia de un hallazgo.

test_that("H4 (cierre): comparar_perfiles() publica sus filas en el mismo orden bajo C y bajo UTF-8", {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  locales <- .locales_o148()
  if (length(locales) < 2L) skip("Hacen falta C y un locale UTF-8.")
  expect_true(.fijar_o148(locales[[2L]]))
  if (identical(sort(c("a", "B")), c("B", "a"))) {
    skip("El locale UTF-8 disponible intercala como C.")
  }
  datos <- data.frame(id = 1:24, Jose = c(1:23, NA))
  repetidos <- datos[c(1:24, 1:24), ]
  ordenes <- list()
  for (locale in locales) {
    expect_true(.fijar_o148(locale))
    d <- comparar_perfiles(
      perfilar(datos, fecha = .F0_O148, proteger_datos_personales = FALSE,
               analizar_dependencias = FALSE),
      perfilar(repetidos, fecha = .F0_O148 + 86400,
               proteger_datos_personales = FALSE, analizar_dependencias = FALSE)
    )
    resueltos <- d$columna[d$aspecto == "hallazgo" & d$cambio == "resuelto"]
    # A mano: los dos hallazgos que se resuelven, por bytes `J` (4a) antes que
    # `i` (69); la intercalacion espanola pone `id` primero. El resumen agrupaba
    # con `split()`, que ordena los grupos como `sort()`.
    expect_identical(resueltos, c("Jose", "id"), info = locale)
    ordenes[[locale]] <- paste(d$columna, d$aspecto, d$cambio)
  }
  expect_identical(ordenes[[1L]], ordenes[[2L]])
})

test_that("H5 (cierre): medir() nombra cada celda con un nombre latin1 igual bajo C y bajo UTF-8", {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  locales <- .locales_o148()
  if (length(locales) < 2L) skip("Hacen falta C y un locale UTF-8.")
  desc <- rawToChar(as.raw(c(0x64, 0x65, 0x73, 0x63, 0xf3)))
  Encoding(desc) <- "latin1"
  datos <- data.frame(id = 1:6, x = rep(c("a", "b"), 3), stringsAsFactors = FALSE)
  names(datos)[2] <- desc
  objetos <- list()
  for (locale in locales) {
    expect_true(.fijar_o148(locale))
    perfil <- perfilar(datos, fecha = .F0_O148, proteger_datos_personales = FALSE,
                       analizar_dependencias = FALSE)
    propuesta <- proponer_modelo(perfil, datos)
    propuesta$incluir <- TRUE
    m <- medir(modelo_desde_propuesta(propuesta), datos, id_medicion = "m1",
               fecha = .F0_O148, proteger_datos_personales = FALSE)
    objetos[[locale]] <- .hex_o148(as.character(m$objeto_medible))
  }
  # A mano: la celda 1 de la columna se nombra `datos$desc` + o con tilde en
  # UTF-8 (c3 b3) + `[1]`; antes, bajo C, el texto `desc<f3>` (3c 66 33 3e).
  celda <- paste0(.hex_o148("datos$desc"), "c3b3", .hex_o148("[1]"))
  expect_true(celda %in% objetos[[1L]])
  expect_false(any(grepl("3c66333e", objetos[[1L]], fixed = TRUE)))
  expect_identical(objetos[[1L]], objetos[[2L]])
})

test_that("H3 (cierre): el reporte de una tabla con nombres latin1 y un byte suelto es el mismo bajo C y bajo UTF-8", {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  locales <- .locales_o148()
  if (length(locales) < 2L) skip("Hacen falta C y un locale UTF-8.")
  latin1 <- function(bytes) {
    x <- rawToChar(as.raw(bytes))
    Encoding(x) <- "latin1"
    x
  }
  datos <- data.frame(
    id = 1:6, a = c(" t\u00e9 ", "caf\u00e9", "x", "y", "x", "y"),
    b = rep(c("uno", "dos"), 3), stringsAsFactors = FALSE
  )
  datos$a <- iconv(datos$a, "UTF-8", "latin1")
  # `descripcion` en latin1 y `Tel` + e9 + `fono` sin marca (UTF-8 invalido).
  names(datos)[2] <- latin1(c(0x64, 0x65, 0x73, 0x63, 0xf3))
  names(datos)[3] <- rawToChar(as.raw(c(0x54, 0x65, 0x6c, 0xe9, 0x66, 0x6f, 0x6e, 0x6f)))
  archivos <- character()
  for (locale in locales) {
    expect_true(.fijar_o148(locale))
    # El mismo nombre de archivo en dos directorios: el reporte lo cita.
    carpeta <- tempfile()
    dir.create(carpeta)
    on.exit(unlink(carpeta, recursive = TRUE), add = TRUE)
    archivo <- file.path(carpeta, "reporte.html")
    reportar(perfilar(datos, fecha = .F0_O148, proteger_datos_personales = FALSE,
                      analizar_dependencias = FALSE),
             archivo = archivo, proteger_datos_personales = FALSE, fecha = .F0_O148)
    archivos[[locale]] <- archivo
  }
  html <- lapply(archivos, function(a) readBin(a, "raw", file.size(a)))
  # Antes, bajo C, `desc&lt;f3&gt;`, `t&lt;e9&gt;` y `Tel&lt;e9&gt;fono` donde
  # una sesion UTF-8 escribia la letra y `Tel\xe9fono`.
  expect_identical(html[[1L]], html[[2L]])
  texto <- rawToChar(html[[1L]])
  expect_false(grepl("&lt;e9&gt;", texto, fixed = TRUE, useBytes = TRUE))
  expect_false(grepl("&lt;f3&gt;", texto, fixed = TRUE, useBytes = TRUE))
})

test_that("H6 (cierre): print() de clasificar_variables() no aborta en ningun locale", {
  anterior <- .guardar_o148()
  on.exit(.restaurar_o148(anterior), add = TRUE)
  locales <- .locales_o148()
  if (!"C" %in% locales) skip("No se pudo fijar el locale C.")
  # Un nivel UTF-8 sin marca (`Optimo` con la O con tilde, c3 93) y otro con un
  # byte latin1 suelto (`Bajo` con la a con tilde en latin1, e1), que no es
  # UTF-8 valido.
  optimo <- rawToChar(as.raw(c(0xc3, 0x93, 0x70, 0x74, 0x69, 0x6d, 0x6f)))
  roto <- rawToChar(as.raw(c(0x42, 0xe1, 0x6a, 0x6f)))
  f <- factor(rep(c("Alto", optimo, roto), 2), levels = c("Alto", optimo, roto))
  impresos <- list()
  for (locale in locales) {
    expect_true(.fijar_o148(locale))
    cv <- clasificar_variables(data.frame(nivel = f), proteger_datos_personales = FALSE)
    # Antes: bajo C "invalid multibyte string" en `format.AsIs()` por el nivel
    # sin marca; bajo UTF-8, lo mismo por el byte suelto.
    expect_no_error(utils::capture.output(print(cv)), message = locale)
    # A mano: el nivel legible se publica con sus bytes; el roto, como lo
    # muestra la consola -`B\xe1jo`-, igual en los dos locales.
    impresos[[locale]] <- .hex_o148(cv$niveles_observados[[1L]])
    expect_identical(impresos[[locale]][1:2], c("416c746f", "c3937074696d6f"),
                     info = locale)
    expect_identical(impresos[[locale]][[3L]], .hex_o148("B\\xe1jo"), info = locale)
  }
  if (length(impresos) == 2L) expect_identical(impresos[[1L]], impresos[[2L]])
})

test_that("H3 (cierre): analizar() no propone un dominio que no puede leer", {
  opciones <- options(warn = 2)
  on.exit(options(opciones), add = TRUE)
  # Una columna de categorias en latin1 SIN marca: el perfil cuenta 3 distintas
  # por identidad y ninguna es UTF-8 valido. La propuesta armaba
  # `ValoresPosiblesPorExtension` con un dominio vacio y `analizar()` avisaba
  # -en cualquier locale- que su metodo no devolvio ninguna medida.
  categorias <- vapply(list(c(0x74, 0xe9), c(0x63, 0x61, 0x66, 0xe9), c(0x61, 0xf1, 0x6f)),
                       function(b) rawToChar(as.raw(b)), character(1L))
  datos <- data.frame(id = 1:12, cat = rep(categorias, 4), stringsAsFactors = FALSE)
  a <- analizar(datos, fecha = .F0_O148, id_medicion = "m1",
                proteger_datos_personales = FALSE, analizar_dependencias = FALSE)
  expect_false("ValoresPosiblesPorExtension" %in% a$propuesta_modelo$metrica)
})

test_that("H3: todo grepl() con fixed = TRUE y un patron ASCII busca por bytes", {
  # Por construccion: buscar un patron ASCII fijo por bytes es exacto en UTF-8 y
  # en latin1, y no avisa ni aborta en ningun locale; sin `useBytes`, bajo un
  # locale UTF-8 un nombre con un byte suelto avisaba "input string N is invalid
  # in this locale" -con `warn = 2`, abortaba-. Se recorren las llamadas de
  # todas las funciones del espacio de nombres.
  sin_bytes <- character()
  revisar <- function(e, funcion) {
    if (!is.call(e)) return(invisible())
    cabeza <- e[[1L]]
    if (is.symbol(cabeza) && as.character(cabeza) %in% c("grepl", "grep")) {
      argumentos <- as.list(e)[-1L]
      nombres <- names(argumentos)
      if (is.null(nombres)) nombres <- rep("", length(argumentos))
      fijo <- "fixed" %in% nombres && isTRUE(argumentos[["fixed"]])
      patron <- if ("pattern" %in% nombres) argumentos[["pattern"]] else
        argumentos[nombres == ""][[1L]]
      if (fijo && !"useBytes" %in% nombres && is.character(patron) &&
          !grepl("[^\\x01-\\x7f]", patron, perl = TRUE, useBytes = TRUE)) {
        sin_bytes <<- c(sin_bytes, funcion)
      }
    }
    for (i in seq_along(e)[-1L]) {
      if (!identical(e[[i]], quote(expr = )) && is.call(e[[i]])) {
        revisar(e[[i]], funcion)
      }
    }
    invisible()
  }
  ns <- asNamespace("lupa")
  for (nombre in ls(ns, all.names = TRUE)) {
    f <- get(nombre, envir = ns)
    if (is.function(f) && !is.primitive(f)) revisar(body(f), nombre)
  }
  expect_identical(unique(sin_bytes), character())
})
