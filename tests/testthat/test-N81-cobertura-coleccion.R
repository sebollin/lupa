# Cuatro capas tiraban la misma declaracion: la cobertura de la coleccion dice
# sobre cuantas de las tablas DECLARADAS se calculo un numero, y por que quedaron
# afuera las otras. El objeto la traia y cada consumidor conservaba menos que el
# anterior. `NEWS.md` promete que una declaracion vale en todas las salidas.

.n81_coleccion <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "buena", data.frame(valor = c(1, 2, NA, 4)))
  DBI::dbWriteTable(con, "vacia", data.frame(valor = numeric()))
  perfil <- suppressWarnings(perfilar_coleccion(
    coleccion(con, data.frame(
      esquema = c(NA, NA, NA), tabla = c("buena", "vacia", "falla"),
      stringsAsFactors = FALSE
    ), nombre = "mixta"),
    bloque_muestra = "solo_agregados"
  ))
  nucleo <- metricas_nucleo()
  medida <- medir(
    modelo(instanciar(especializar(nucleo$NoNulo), "buena", "valor")),
    list(buena = data.frame(valor = c(1, 2, NA, 4)))
  )
  agregado <- agregar(
    agregar(agregar(medida, "atributo", "ratio"), "entidad", "promedio"),
    "coleccion", "promedio_ponderado", pesos = 1, coleccion = perfil
  )
  list(con = con, perfil = perfil, agregado = agregado)
}

.n81_impreso <- function(objeto) {
  salida <- character()
  destino <- textConnection("salida", "w", local = TRUE)
  sink(destino, type = "message")
  sink(destino)
  on.exit({ sink(); sink(type = "message"); close(destino) }, add = TRUE)
  try(print(objeto), silent = TRUE)
  paste(salida, collapse = " ")
}

test_that("el motivo de cada tabla sin medir sobrevive hasta el numero", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  piezas <- .n81_coleccion()
  on.exit(DBI::dbDisconnect(piezas$con), add = TRUE)

  cobertura <- attr(piezas$agregado, "cobertura_coleccion", exact = TRUE)
  expect_false(is.null(cobertura))
  expect_setequal(as.character(cobertura$tablas_sin_medir), c("vacia", "falla"))
  motivos <- stats::setNames(
    as.character(cobertura$motivo_sin_medir),
    as.character(cobertura$tablas_sin_medir)
  )
  # Una tabla que no existe y una tabla vacia NO son el mismo problema, y el
  # perfil de la coleccion los distingue. La frontera reemplazaba los dos por
  # "no hay una medida de esta tabla en la entrada".
  expect_match(motivos[["vacia"]], "cero filas")
  expect_match(motivos[["falla"]], "No se pudo perfilar")
})

test_that("la cobertura de la coleccion llega al indice por las dos vias", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  piezas <- .n81_coleccion()
  on.exit(DBI::dbDisconnect(piezas$con), add = TRUE)

  tablero <- tablero_calidad(piezas$agregado)
  indice <- indice_calidad(piezas$agregado, pesos = c(Completitud = 1))
  # El tablero la conserva como atributo; el indice la publicaba SOLO como
  # elemento de la lista, asi que un consumidor generico con `attr()` -el mismo
  # que funciona con el tablero- la perdia en el ultimo eslabon.
  expect_false(is.null(attr(tablero, "cobertura_coleccion", exact = TRUE)))
  expect_false(is.null(attr(indice, "cobertura_coleccion", exact = TRUE)))
  expect_false(is.null(indice$cobertura_coleccion))
})

test_that("un indice sin componentes conserva la cobertura de la coleccion", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  piezas <- .n81_coleccion()
  on.exit(DBI::dbDisconnect(piezas$con), add = TRUE)

  # Sin pesos no hay indice numerico; la declaracion de cuantas tablas quedaron
  # afuera es justamente lo que mas falta hace cuando el numero no sale.
  tablero <- tablero_calidad(piezas$agregado)
  sin_componentes <- getFromNamespace(".nuevo_indice_sin_componentes", "lupa")
  indice <- sin_componentes(tablero)
  expect_true(is.na(indice$valor))
  expect_false(is.null(indice$cobertura_coleccion))
  expect_false(is.null(attr(indice, "cobertura_coleccion", exact = TRUE)))
})

test_that("las dos pantallas y el informe publican el motivo, no solo el nombre", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  piezas <- .n81_coleccion()
  on.exit(DBI::dbDisconnect(piezas$con), add = TRUE)

  for (objeto in list(
    tablero_calidad(piezas$agregado),
    indice_calidad(piezas$agregado, pesos = c(Completitud = 1))
  )) {
    impreso <- .n81_impreso(objeto)
    expect_match(impreso, "Cobertura de la colecci")
    expect_match(impreso, "cero filas")
    expect_match(impreso, "No se pudo perfilar")
  }

  ruta <- tempfile(fileext = ".html")
  on.exit(unlink(ruta), add = TRUE)
  invisible(reportar(piezas$agregado, archivo = ruta))
  html <- paste(readLines(ruta, warn = FALSE), collapse = "")
  expect_true(grepl("tablas_sin_medir", html, fixed = TRUE))
  expect_true(grepl("cero filas", html, fixed = TRUE))
  expect_true(grepl("No se pudo perfilar", html, fixed = TRUE))
})

test_that("una coleccion completa no inventa tablas sin medir", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # La mitad de control: sin tablas afuera, ninguna capa publica un motivo ni una
  # tabla sin medir. Una guarda que siempre imprima algo no distingue los casos.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "unica", data.frame(valor = c(1, 2, NA, 4)))
  perfil <- suppressWarnings(perfilar_coleccion(
    coleccion(con, data.frame(esquema = NA, tabla = "unica",
                              stringsAsFactors = FALSE), nombre = "completa"),
    bloque_muestra = "solo_agregados"
  ))
  nucleo <- metricas_nucleo()
  medida <- medir(
    modelo(instanciar(especializar(nucleo$NoNulo), "unica", "valor")),
    list(unica = data.frame(valor = c(1, 2, NA, 4)))
  )
  agregado <- agregar(
    agregar(agregar(medida, "atributo", "ratio"), "entidad", "promedio"),
    "coleccion", "promedio_ponderado", pesos = 1, coleccion = perfil
  )
  cobertura <- attr(agregado, "cobertura_coleccion", exact = TRUE)
  expect_length(cobertura$tablas_sin_medir, 0L)
  impreso <- .n81_impreso(tablero_calidad(agregado))
  expect_match(impreso, "ninguna")
  expect_false(grepl("cero filas", impreso, fixed = TRUE))
})

test_that("la identidad con catalogo sobrevive a la frontera", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # Una tabla declarada como `catalogo.esquema.tabla` que no se puede perfilar se
  # publicaba en la frontera como `esquema.tabla`: la identidad completa se caia
  # justo en la tabla que fallo, mientras la que si se perfilo conservaba la
  # suya. El motivo quedaba asociado a un nombre que nadie declaro.
  #
  # El caso NO necesita un motor con catalogos: la tabla falla porque no existe,
  # y eso es lo que hace falta para que su fila llegue a la cobertura.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "presente", data.frame(valor = c(1, 2, NA, 4)))
  declarada <- coleccion(con, data.frame(
    catalogo = c(NA, "c2"), esquema = c(NA, "s"),
    tabla = c("presente", "ausente"), stringsAsFactors = FALSE
  ), nombre = "cat")
  expect_true("c2.s.ausente" %in% declarada$tablas$identificador)

  perfil <- suppressWarnings(perfilar_coleccion(
    declarada, bloque_muestra = "solo_agregados"
  ))
  nucleo <- metricas_nucleo()
  medida <- medir(
    modelo(instanciar(especializar(nucleo$NoNulo), "presente", "valor")),
    list(presente = data.frame(valor = c(1, 2, NA, 4)))
  )
  agregado <- agregar(
    agregar(agregar(medida, "atributo", "ratio"), "entidad", "promedio"),
    "coleccion", "promedio_ponderado", pesos = 1, coleccion = perfil
  )
  cobertura <- attr(agregado, "cobertura_coleccion", exact = TRUE)
  # La tabla que quedo afuera se nombra como se declaro, con su catalogo.
  expect_true("c2.s.ausente" %in% as.character(cobertura$tablas_sin_medir))
  expect_false("s.ausente" %in% as.character(cobertura$tablas_sin_medir))

  # Control: sin catalogo declarado, el identificador sigue siendo de dos partes
  # y no se inventa un prefijo.
  sin_catalogo <- coleccion(con, data.frame(
    esquema = c(NA, "s"), tabla = c("presente", "ausente"),
    stringsAsFactors = FALSE
  ), nombre = "sincat")
  perfil2 <- suppressWarnings(perfilar_coleccion(
    sin_catalogo, bloque_muestra = "solo_agregados"
  ))
  agregado2 <- agregar(
    agregar(agregar(medida, "atributo", "ratio"), "entidad", "promedio"),
    "coleccion", "promedio_ponderado", pesos = 1, coleccion = perfil2
  )
  cobertura2 <- attr(agregado2, "cobertura_coleccion", exact = TRUE)
  expect_true("s.ausente" %in% as.character(cobertura2$tablas_sin_medir))
})

test_that("los seis alcances de cobertura se enumeran y tres explican la tabla", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # La regla que elige el motivo publicado aceptaba dos alcances y el paquete
  # produce SEIS. El que faltaba, `medicion_incompleta`, dice que la tabla se
  # perfilo pero no se pudo medir la proporcion de ausentes en NINGUNA columna:
  # explica de lleno que no pueda aportar, y caia al motivo generico.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "incompleta", data.frame(a = 1:3, b = 4:6))
  DBI::dbWriteTable(con, "otra", data.frame(a = c(1, NA, 3)))
  # `metricas = "moda"` deja `prop_faltantes` desconocida en todas las columnas.
  perfil <- suppressWarnings(perfilar_coleccion(
    coleccion(con, data.frame(tabla = c("incompleta", "otra"),
                              stringsAsFactors = FALSE), nombre = "c"),
    metricas = "moda", bloque_muestra = "solo_agregados"
  ))
  cobertura <- as.data.frame(perfil$cobertura_coleccion)
  expect_true("medicion_incompleta" %in% as.character(cobertura$alcance))

  medida <- medir(
    modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "otra", "a")),
    data.frame(a = c(1, NA, 3))
  )
  agregado <- agregar(
    agregar(agregar(medida, "atributo", "ratio"),
            "entidad", "promedio_ponderado", pesos = c("otra$a" = 1)),
    "coleccion", "promedio_ponderado", pesos = c(otra = 1), coleccion = perfil
  )
  final <- attr(agregado, "cobertura_coleccion", exact = TRUE)
  expect_true("incompleta" %in% as.character(final$tablas_sin_medir))
  motivo <- as.character(final$motivo_sin_medir)[
    as.character(final$tablas_sin_medir) == "incompleta"
  ]
  expect_match(motivo, "no se pudo medir la proporcion")
  expect_false(grepl("No hay una medida de esta tabla", motivo, fixed = TRUE))
})

test_that("la cobertura sobrevive cuando el numero sube a organizacion", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # Al subir de nivel la cobertura queda HEREDADA dentro de
  # `cobertura_de_partes`, y los consumidores leian solo el atributo directo: el
  # tablero, el indice y el informe de un numero de organizacion se publicaban
  # sin la declaracion de que una de dos tablas quedo afuera.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "good", data.frame(a = c(1, NA, 3)))
  perfil <- suppressWarnings(perfilar_coleccion(
    coleccion(con, c("good", "missing"), nombre = "catcase"),
    bloque_muestra = "solo_agregados"
  ))
  medida <- medir(
    modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "good", "a")),
    data.frame(a = c(1, NA, 3))
  )
  por_coleccion <- agregar(
    agregar(agregar(medida, "atributo", "ratio"), "entidad", "promedio"),
    "coleccion", "promedio_ponderado", pesos = c(good = 1), coleccion = perfil
  )
  por_organizacion <- agregar(
    por_coleccion, "organizacion", "promedio_ponderado", pesos = c(good = 1),
    organizacion = organizacion("org", c("catcase"))
  )
  tablero <- tablero_calidad(por_organizacion)
  indice <- indice_calidad(por_organizacion, pesos = c(Completitud = 1))
  expect_false(is.null(attr(tablero, "cobertura_coleccion", exact = TRUE)))
  expect_false(is.null(indice$cobertura_coleccion))

  ruta <- tempfile(fileext = ".html")
  on.exit(unlink(ruta), add = TRUE)
  invisible(reportar(por_organizacion, archivo = ruta))
  html <- paste(readLines(ruta, warn = FALSE), collapse = "")
  expect_true(grepl("missing", html, fixed = TRUE))
})

test_that("un DBI::Id se lee por etiqueta y no por posicion", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # `DBI::Id(catalog = "cat", table = "t")` es una forma valida de DOS
  # componentes, y aparearla por posicion publicaba el catalogo en la columna
  # `esquema`: la coleccion declaraba un esquema que el usuario nunca nombro.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  partes <- function(id) {
    f <- as.data.frame(coleccion(con, id, nombre = "x")$tablas)
    c(catalogo = as.character(f$catalogo[[1L]]),
      esquema = as.character(f$esquema[[1L]]),
      tabla = as.character(f$tabla[[1L]]))
  }
  con_catalogo <- partes(DBI::Id(catalog = "cat", table = "t"))
  expect_identical(unname(con_catalogo[["catalogo"]]), "cat")
  expect_true(is.na(con_catalogo[["esquema"]]))

  # Control: las otras tres formas no se mueven.
  con_esquema <- partes(DBI::Id(schema = "s", table = "t"))
  expect_true(is.na(con_esquema[["catalogo"]]))
  expect_identical(unname(con_esquema[["esquema"]]), "s")
  las_tres <- partes(DBI::Id(catalog = "c", schema = "s", table = "t"))
  expect_identical(unname(las_tres[["catalogo"]]), "c")
  expect_identical(unname(las_tres[["esquema"]]), "s")
  sola <- partes(DBI::Id(table = "t"))
  expect_true(is.na(sola[["catalogo"]]))
  expect_true(is.na(sola[["esquema"]]))
  # Y un `schema` sin `table` sigue siendo un nombre que no nombra una tabla.
  expect_error(coleccion(con, DBI::Id(schema = "s"), nombre = "x"), "no nombra")
})
