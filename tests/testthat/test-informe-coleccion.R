# El informe de una coleccion: con cientos de tablas, el objeto de
# `perfilar_coleccion()` no alcanzaba para entregar. `reportar()` lo rechazaba
# entero y no habia una tabla plana que exportar: ni el estado de cada tabla, ni
# su ubicacion en el marco, ni lo que no se midio con su motivo en una sola
# tabla. `as.data.frame()` -y `summary()`- lo bajan a tablas planas, y
# `reportar()` publica las mismas.

# Pesada para CRAN (perfilar una coleccion por DBI tarda varios segundos): corre
# en la suite completa y en el CI, que usan NOT_CRAN=true.
testthat::skip_on_cran()

# Cinco tablas declaradas, una por estado posible salvo `medicion_incompleta`:
# una sana, una vacia, una vista cuya tabla base se borro (el motor falla al
# leerla), una que no existe y una declarada como vista, que no se perfila.
.coleccion_informe_prueba <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  pedidos <- data.frame(
    id = 1:25,
    importe = c(NA, 12.5, NA, seq(10, 100, length.out = 22)),
    canal = rep(c("web", "local", "telefono", "web", "web"), 5),
    alta = as.character(as.Date("2025-03-01") + 0:24),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "pedidos", pedidos)
  DBI::dbWriteTable(con, "bajas", data.frame(id = integer(), causa = character()))
  DBI::dbExecute(con, "CREATE TABLE base_borrada (a INTEGER)")
  DBI::dbExecute(con, "CREATE VIEW resumen_roto AS SELECT a FROM base_borrada")
  DBI::dbExecute(con, "DROP TABLE base_borrada")
  declaradas <- data.frame(
    tabla = c("pedidos", "bajas", "resumen_roto", "no_existe", "tablero"),
    tipo = c("tabla", "tabla", "tabla", "tabla", "vista"),
    stringsAsFactors = FALSE
  )
  list(con = con, coleccion = coleccion(con, declaradas, nombre = "comercial"))
}

.perfil_coleccion_informe <- function(conservar_perfiles = TRUE) {
  base <- .coleccion_informe_prueba()
  perfil <- suppressWarnings(perfilar_coleccion(
    base$coleccion, conservar_perfiles = conservar_perfiles
  ))
  DBI::dbDisconnect(base$con)
  perfil
}

test_that("el indice tiene una fila por tabla declarada, con su estado y su motivo", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  perfil <- .perfil_coleccion_informe()
  indice <- as.data.frame(perfil)

  expect_identical(nrow(indice), as.integer(perfil$meta$n_declaradas))
  expect_identical(
    indice$identificador,
    c("pedidos", "bajas", "resumen_roto", "no_existe", "tablero")
  )
  expect_identical(
    as.character(indice$estado),
    c("perfilada", "vacia", "con_error", "con_error", "no_medida")
  )
  expect_identical(indice$tipo, c("tabla", "tabla", "tabla", "tabla", "vista"))
  # Las identidades del objeto se conservan: lo perfilado y lo que no suman lo
  # declarado, por estado.
  estado <- as.character(indice$estado)
  expect_identical(
    sum(estado %in% c("perfilada", "vacia", "medicion_incompleta")),
    as.integer(perfil$meta$n_perfiladas)
  )
  expect_identical(
    sum(estado %in% c("con_error", "no_medida")),
    as.integer(perfil$meta$n_sin_perfilar)
  )
  expect_identical(sum(estado == "vacia"), as.integer(perfil$meta$n_vacias))

  # El motivo es el que registro la cobertura, no uno nuevo.
  cobertura <- perfil$cobertura_coleccion
  for (tabla in c("bajas", "resumen_roto", "no_existe", "tablero")) {
    esperado <- cobertura$motivo[cobertura$tabla == tabla]
    expect_identical(indice$motivo[indice$tabla == tabla], esperado)
    expect_identical(
      indice$como_resolverlo[indice$tabla == tabla],
      cobertura$como_resolverlo[cobertura$tabla == tabla]
    )
  }
  expect_true(is.na(indice$motivo[indice$tabla == "pedidos"]))
  expect_identical(indice$alcances_cobertura[indice$tabla == "bajas"], "tabla_vacia")

  # Los numeros son los de `resumen_coleccion` y NA donde no se perfilo.
  resumen <- perfil$resumen_coleccion
  expect_identical(indice$n_filas[1:2], resumen$n_filas)
  expect_identical(indice$prop_faltantes_maxima[1:2], resumen$prop_faltantes_maxima)
  expect_identical(indice$momento[1:2], resumen$momento)
  expect_true(all(is.na(indice$n_filas[3:5])))
  expect_true(all(is.na(indice$n_columnas[3:5])))

  # Los hallazgos se cuentan del perfil conservado; en la vacia no hay filas
  # que examinar y su cero se leeria como una tabla limpia.
  h <- hallazgos(perfil$perfiles$pedidos)
  expect_identical(
    indice$n_hallazgos_sospechoso[[1L]],
    as.numeric(sum(as.character(h$severidad) == "sospechoso"))
  )
  expect_identical(
    indice$n_hallazgos_error[[1L]],
    as.numeric(sum(as.character(h$severidad) == "error"))
  )
  expect_true(all(is.na(indice$n_hallazgos_error[2:5])))
  expect_true(all(is.na(indice$n_hallazgos_sospechoso[2:5])))

  expect_identical(summary(perfil), indice)
  expect_identical(summary(perfil, "no_medido"), as.data.frame(perfil, parte = "no_medido"))
})

test_that("la matriz es cobertura_analisis() de cada perfil, sin combinar celdas", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  perfil <- .perfil_coleccion_informe()
  marco <- marco_agesic()
  matriz <- as.data.frame(perfil, parte = "matriz")
  nombres_factores <- paste(marco$factores$dimension, marco$factores$factor, sep = " / ")

  expect_identical(nrow(matriz), 5L)
  expect_identical(
    names(matriz),
    c("tabla", "esquema", "identificador", "estado_tabla", nombres_factores)
  )
  celdas <- matriz[nombres_factores]
  # Ninguna cifra: cada celda es un estado del marco.
  expect_true(all(vapply(celdas, is.factor, logical(1L))))
  expect_true(all(vapply(celdas, function(x) {
    identical(levels(x), c("medida", "no_declarada", "no_aplica", "fuera_de_alcance"))
  }, logical(1L))))
  for (tabla in c("pedidos", "bajas")) {
    esperado <- cobertura_analisis(perfil$perfiles[[tabla]], modelo = marco)
    fila <- unlist(lapply(celdas[matriz$tabla == tabla, , drop = FALSE], as.character))
    expect_identical(unname(fila), as.character(esperado$estado))
  }
  # La tabla vacia no se lee como medida: lo que el perfil mide queda sin
  # nada que examinar.
  densidad <- nombres_factores[marco$factores$factor == "Densidad"]
  expect_identical(as.character(matriz[[densidad]]), c("medida", "no_aplica", NA, NA, NA))
  expect_true(all(is.na(as.matrix(celdas[3:5, ]))))

  dimensiones <- as.data.frame(perfil, parte = "dimensiones")
  expect_identical(nrow(dimensiones), 5L * nrow(marco$factores))
  esperado <- cobertura_analisis(perfil$perfiles$pedidos, modelo = marco)
  propias <- dimensiones[dimensiones$tabla == "pedidos", ]
  expect_identical(propias$motivo, esperado$motivo)
  expect_identical(propias$estado, esperado$estado)
  expect_identical(unique(propias$marco), marco$nombre)
  expect_true(all(is.na(dimensiones$estado[dimensiones$tabla == "no_existe"])))
  expect_true(all(grepl("no se perfil", dimensiones$motivo[dimensiones$tabla == "no_existe"])))

  # Sin marco, el de `cobertura_analisis()` por omision: la misma matriz.
  expect_identical(
    as.data.frame(perfil, parte = "matriz", marco = NULL), matriz
  )
  expect_identical(as.data.frame(perfil, parte = "matriz", marco = marco), matriz)
  # Otro marco, otras columnas: la matriz sigue a la taxonomia que se le pasa.
  iso <- marco_iso25012()
  otra <- as.data.frame(perfil, parte = "matriz", marco = iso)
  expect_identical(ncol(otra), 4L + nrow(iso$factores))
  expect_error(
    as.data.frame(perfil, parte = "matriz", marco = "iso"),
    "`marco` debe provenir de marco_calidad()", fixed = TRUE
  )
})

test_that("lo no medido junta las coberturas tal como el objeto las registra", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  perfil <- .perfil_coleccion_informe()
  no_medido <- as.data.frame(perfil, parte = "no_medido")

  de_coleccion <- no_medido[no_medido$origen == "cobertura_coleccion", ]
  cobertura <- perfil$cobertura_coleccion
  expect_identical(nrow(de_coleccion), nrow(cobertura))
  expect_setequal(de_coleccion$motivo, cobertura$motivo)
  expect_setequal(de_coleccion$alcance, cobertura$alcance)

  metricas <- perfil$cobertura_metricas
  metricas <- metricas[metricas$estado != "calculado", ]
  de_metricas <- no_medido[no_medido$origen == "cobertura_metricas", ]
  expect_identical(nrow(de_metricas), nrow(metricas))
  expect_setequal(
    paste(de_metricas$tabla, de_metricas$columna, de_metricas$metrica, de_metricas$estado),
    paste(metricas$tabla, metricas$columna, metricas$metrica, metricas$estado)
  )
  expect_setequal(de_metricas$motivo, metricas$motivo)

  diagnosticos <- sum(vapply(perfil$perfiles, function(p) {
    nrow(p$perfil_muestra$cobertura_diagnosticos)
  }, integer(1L)))
  expect_identical(sum(no_medido$origen == "cobertura_diagnosticos"), diagnosticos)

  # Agrupado por tabla, en el orden del indice.
  orden <- unique(no_medido$identificador)
  indice <- as.data.frame(perfil)$identificador
  expect_identical(orden, indice[indice %in% orden])
})

test_that("cada parte se escribe y se vuelve a leer como CSV", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  perfil <- .perfil_coleccion_informe()
  for (parte in c("tablas", "matriz", "dimensiones", "no_medido", "hallazgos")) {
    tabla <- as.data.frame(perfil, parte = parte)
    expect_s3_class(tabla, "data.frame")
    expect_false(any(vapply(tabla, is.list, logical(1L))), info = parte)
    texto <- utils::capture.output(utils::write.csv(tabla, row.names = FALSE))
    leida <- utils::read.csv(text = texto, check.names = FALSE)
    expect_identical(nrow(leida), nrow(tabla), info = parte)
    expect_identical(names(leida), names(tabla), info = parte)
  }
  h <- as.data.frame(perfil, parte = "hallazgos")
  completos <- hallazgos(perfil)
  expect_identical(nrow(h), nrow(completos))
  expect_identical(h$tipo_hallazgo, completos$tipo_hallazgo)
  expect_false("trazabilidad" %in% names(h))
})

test_that("sin perfiles conservados, lo que no se puede ubicar queda en NA y avisado", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  perfil <- .perfil_coleccion_informe(conservar_perfiles = FALSE)
  indice <- expect_silent(as.data.frame(perfil))
  expect_identical(
    as.character(indice$estado),
    c("perfilada", "vacia", "con_error", "con_error", "no_medida")
  )
  expect_true(all(is.na(indice$n_hallazgos_error)))
  expect_warning(
    matriz <- as.data.frame(perfil, parte = "matriz"),
    "conservar_perfiles = TRUE", fixed = TRUE
  )
  expect_true(all(is.na(as.matrix(matriz[-(1:4)]))))
  no_medido <- as.data.frame(perfil, parte = "no_medido")
  expect_identical(
    sum(no_medido$origen == "cobertura_coleccion"),
    nrow(perfil$cobertura_coleccion)
  )
  expect_warning(h <- as.data.frame(perfil, parte = "hallazgos"), "conservar_perfiles")
  expect_identical(nrow(h), 0L)
})

test_that("un argumento mal escrito se rechaza en vez de devolver el indice", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  perfil <- .perfil_coleccion_informe(conservar_perfiles = FALSE)
  expect_error(as.data.frame(perfil, partes = "matriz"), "`partes`", fixed = TRUE)
  expect_error(as.data.frame(perfil, parte = "matrices"), "`parte` debe ser", fixed = TRUE)
  expect_identical(
    as.data.frame(perfil, stringsAsFactors = FALSE), as.data.frame(perfil)
  )
})

test_that("reportar() publica la coleccion con su indice, su matriz y lo no medido", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  perfil <- .perfil_coleccion_informe()
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(perfil, archivo = archivo, fecha = as.POSIXct("2026-01-31", tz = "UTC"))
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_false(grepl("Secci\u00f3n no armada", html, fixed = TRUE))
  expect_true(grepl(
    "<h2>Perfil de colecci\u00f3n: comercial</h2>", html, fixed = TRUE
  ))
  tarjeta <- function(etiqueta, valor) {
    paste0("<span>", etiqueta, "</span><strong>", valor, "</strong>")
  }
  expect_true(grepl(tarjeta("Declaradas", 5), html, fixed = TRUE))
  expect_true(grepl(tarjeta("Perfiladas", 1), html, fixed = TRUE))
  expect_true(grepl(tarjeta("Con error", 2), html, fixed = TRUE))
  expect_true(grepl(tarjeta("No medidas", 1), html, fixed = TRUE))
  # Cada tabla que no se perfilo llega con su motivo, escapado como el resto.
  cobertura <- perfil$cobertura_coleccion
  for (motivo in cobertura$motivo) {
    expect_true(grepl(.html_texto(.resumir_valor_reporte(motivo)), html, fixed = TRUE))
  }
  # La matriz: un encabezado por factor del marco.
  marco <- marco_agesic()
  for (nombre in paste(marco$factores$dimension, marco$factores$factor, sep = " / ")) {
    expect_true(grepl(paste0("<th>", .html_texto(nombre), "</th>"), html, fixed = TRUE))
  }

  # Sin perfiles, la seccion se arma igual y dice que falta.
  sin_perfiles <- .perfil_coleccion_informe(conservar_perfiles = FALSE)
  otro <- tempfile(fileext = ".html")
  on.exit(unlink(otro), add = TRUE)
  expect_silent(reportar(sin_perfiles, archivo = otro))
  html <- paste(readLines(otro, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("Secci\u00f3n no armada", html, fixed = TRUE))
  expect_true(grepl("no conserv\u00f3 los perfiles", html, fixed = TRUE))
  expect_true(grepl(tarjeta("Con error", 2), html, fixed = TRUE))
})

test_that("con la proteccion pedida, el informe vuelve a proteger los hallazgos", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  correos <- paste0("socio", 1:40, "@dominio.test")
  correos[[3L]] <- "SOCIOTRES@dominio.test "
  DBI::dbWriteTable(con, "socios", data.frame(id = 1:40, correo = correos))
  perfil <- suppressWarnings(perfilar_coleccion(
    coleccion(con, "socios"), conservar_perfiles = TRUE
  ))
  # Primera mitad: sin proteccion el valor llega a la evidencia publicada; si
  # no llegara, la comprobacion de abajo no probaria nada.
  h <- as.data.frame(perfil, parte = "hallazgos")
  expect_true(any(grepl("SOCIOTRES", h$evidencia, fixed = TRUE)))
  abierto <- tempfile(fileext = ".html")
  protegido <- tempfile(fileext = ".html")
  on.exit(unlink(c(abierto, protegido)), add = TRUE)
  reportar(perfil, archivo = abierto)
  reportar(perfil, archivo = protegido, proteger_datos_personales = TRUE)
  leer <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("SOCIOTRES", leer(abierto), fixed = TRUE))
  expect_false(grepl("SOCIOTRES", leer(protegido), fixed = TRUE))
  expect_true(grepl("vuelve a proteger sus hallazgos", leer(protegido), fixed = TRUE))
})

test_that("en un motor con esquemas, la identidad del indice es la de la coleccion", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(.duckdb_prueba())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA ventas")
  DBI::dbExecute(con, "CREATE SCHEMA archivo")
  DBI::dbExecute(con, "CREATE TABLE ventas.items (id INTEGER, precio DOUBLE)")
  DBI::dbExecute(con, "INSERT INTO ventas.items VALUES (1, 10), (2, NULL), (3, 7.5)")
  DBI::dbExecute(con, "CREATE TABLE archivo.items (id INTEGER)")
  declaradas <- data.frame(
    esquema = c("ventas", "archivo", "ventas"),
    tabla = c("items", "items", "ausente"),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar_coleccion(
    coleccion(con, declaradas, nombre = "dos_esquemas"), conservar_perfiles = TRUE
  ))
  indice <- as.data.frame(perfil)
  # Dos tablas con el mismo nombre en esquemas distintos no se funden.
  expect_identical(indice$identificador, c("ventas.items", "archivo.items", "ventas.ausente"))
  expect_identical(as.character(indice$estado), c("perfilada", "vacia", "con_error"))
  expect_identical(indice$esquema, c("ventas", "archivo", "ventas"))
  matriz <- as.data.frame(perfil, parte = "matriz")
  expect_identical(matriz$identificador, indice$identificador)
  no_medido <- as.data.frame(perfil, parte = "no_medido")
  expect_true(all(no_medido$identificador %in% indice$identificador))
})

test_that("bajo el locale C la matriz conserva los nombres del marco y no avisa", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  perfil <- .perfil_coleccion_informe()
  en_utf8 <- names(as.data.frame(perfil, parte = "matriz"))
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(suppressWarnings(Sys.setlocale("LC_CTYPE", anterior)), silent = TRUE), add = TRUE)
  puesto <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if(!nzchar(puesto), "la maquina no deja fijar LC_CTYPE = C")
  # Los factores con tilde del marco se volvian argumentos de una llamada a
  # `data.frame()` y cada uno avisaba "unable to translate".
  bajo_c <- expect_silent(as.data.frame(perfil, parte = "matriz"))
  bytes <- function(x) {
    vapply(x, function(s) paste(as.character(charToRaw(s)), collapse = ""), "",
           USE.NAMES = FALSE)
  }
  expect_identical(bytes(names(bajo_c)), bytes(en_utf8))
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  expect_silent(reportar(perfil, archivo = archivo))
})

test_that("una tabla con tilde en el nombre conserva su identidad en el indice y el informe", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  anios <- paste0("a", intToUtf8(241), "os")
  ausente <- paste0("caf", intToUtf8(233))
  DBI::dbWriteTable(con, anios, data.frame(x = c(1, NA, 3)))
  perfil <- suppressWarnings(perfilar_coleccion(
    coleccion(con, c(anios, ausente)), conservar_perfiles = TRUE
  ))
  indice <- summary(perfil)
  expect_identical(indice$identificador, c(anios, ausente))
  expect_identical(as.character(indice$estado), c("perfilada", "con_error"))
  no_medido <- as.data.frame(perfil, parte = "no_medido")
  expect_true(ausente %in% no_medido$identificador)
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(perfil, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl(anios, html, fixed = TRUE))
  expect_true(grepl(ausente, html, fixed = TRUE))
})

test_that("un marco que no es un marco se rechaza en todas las partes, como en analizar()", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  perfil <- .perfil_coleccion_informe(conservar_perfiles = FALSE)
  esperado <- tryCatch(
    analizar(datos_operativos, marco = 1), error = conditionMessage
  )
  expect_identical(esperado, "`marco` debe provenir de marco_calidad().")
  for (parte in c("tablas", "matriz", "dimensiones", "no_medido", "hallazgos")) {
    for (marco in list(1, "propio", NA, list())) {
      obtenido <- tryCatch(
        suppressWarnings(as.data.frame(perfil, parte = parte, marco = marco)),
        error = conditionMessage
      )
      expect_identical(obtenido, esperado, info = parte)
    }
  }
  expect_error(summary(perfil, marco = 1), esperado, fixed = TRUE)
})

test_that("el indice dice cuantas metricas no lista el tope de la cobertura", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "altas", data.frame(
    id = 1:6, rubro = c("a", "b", NA, "a", "c", "b"), stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "bajas", data.frame(
    id = 1:3, motivo = c("x", "y", NA), stringsAsFactors = FALSE
  ))
  col <- coleccion(con, c("altas", "bajas", "inexistente"))
  sin_tope <- suppressWarnings(perfilar_coleccion(col))
  recortada <- suppressWarnings(perfilar_coleccion(col, tope_cobertura_metricas = 4))
  # Primera mitad: el tope corta de verdad, y sin el se listan todas.
  expect_true(recortada$meta$cobertura_metricas_truncada)
  expect_identical(nrow(recortada$cobertura_metricas), 4L)
  expect_identical(summary(sin_tope)$n_metricas_sin_listar, c(0, 0, NA))

  indice <- summary(recortada)
  # Por tabla: las no calculadas que publico menos las que quedaron listadas.
  listadas <- table(factor(
    recortada$cobertura_metricas$identificador, levels = indice$identificador
  ))
  no_calculadas <- indice$n_metricas_declaradas - indice$n_metricas_calculadas
  expect_identical(
    indice$n_metricas_sin_listar[1:2],
    no_calculadas[1:2] - as.numeric(listadas[1:2])
  )
  expect_true(all(indice$n_metricas_sin_listar[1:2] > 0))
  expect_true(is.na(indice$n_metricas_sin_listar[[3L]]))
  # Lo listado y lo que falta suman el total que el objeto declara.
  expect_identical(
    sum(indice$n_metricas_sin_listar, na.rm = TRUE) +
      nrow(recortada$cobertura_metricas),
    recortada$meta$n_metricas_no_medidas
  )
  ninguna <- suppressWarnings(perfilar_coleccion(col, cobertura_metricas = "ninguna"))
  expect_true(all(is.na(summary(ninguna)$n_metricas_sin_listar)))

  # La consola y el informe tambien lo dicen.
  impreso <- paste(utils::capture.output(print(recortada), type = "message"), collapse = " ")
  expect_true(grepl("recortada a", impreso, fixed = TRUE))
  impreso_sin_tope <- paste(utils::capture.output(print(sin_tope), type = "message"), collapse = " ")
  expect_false(grepl("recortada a", impreso_sin_tope, fixed = TRUE))
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(recortada, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("<th>n_metricas_sin_listar</th>", html, fixed = TRUE))
})
