# Lo que se compara entre dos perfiles es el valor, no como lo imprimio la
# sesion que hizo cada uno. La moda de una columna numerica se publica con la
# marca decimal de `options(OutDec)`: la misma tabla perfilada con punto y con
# coma salia `materialmente_distinto` en `moda` al lado de `identico` en
# `media`. Una columna de texto se sigue comparando como texto.
#
# Y cuatro restos de la misma zona: un perfil antiguo sin politica de
# centinelas no es un cambio de politica; una columna que se llama `<tabla>` no
# es la tabla; la coleccion consultaba el esquema de cada tabla dos veces; el
# mensaje de las geometrias mezclaba dos universos; y una funcion del paquete
# estaba definida dos veces, con cuerpos distintos, y ganaba la del archivo que
# se carga despues.

testthat::skip_on_cran()

.fecha_comparacion <- function(texto) {
  as.POSIXct(texto, tz = "UTC")
}

testthat::test_that("la equivalencia ignora la marca decimal de valores numericos", {
  datos <- data.frame(
    monto = c(2245.63, 2245.63, 1.5, 3, 1000000000000),
    entero = c(1, 1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  previas <- options(OutDec = ".", scipen = 0, digits = 7)
  on.exit(options(previas), add = TRUE)
  configuraciones <- expand.grid(
    OutDec = c(".", ","), scipen = c(0, 100), digits = c(7, 15),
    stringsAsFactors = FALSE
  )
  perfiles <- lapply(seq_len(nrow(configuraciones)), function(i) {
    options(
      OutDec = configuraciones$OutDec[[i]],
      scipen = configuraciones$scipen[[i]],
      digits = configuraciones$digits[[i]]
    )
    perfilar(
      datos, fecha = .fecha_comparacion("2026-01-01"),
      analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    )
  })
  for (perfil in perfiles) {
    equivalencia <- comparar_equivalencia(
      perfiles[[1L]], perfil, tolerancia = 0
    )
    veredictos <- as.character(equivalencia$veredicto)
    expect_true(all(veredictos %in% c("identico", "no_comparable")))
    expect_false(any(veredictos == "materialmente_distinto"))
    expect_equal(
      nrow(comparar_perfiles(perfiles[[1L]], perfil)), 0L
    )
  }

  punto <- data.frame(
    columna = "monto", tipo_declarado = "doble",
    moda = "2245.63", centinela_valor = "999.5",
    stringsAsFactors = FALSE
  )
  coma <- punto
  coma$moda <- "2245,63"
  coma$centinela_valor <- "999,5"
  equivalencia <- comparar_equivalencia(punto, coma, tolerancia = 0)
  expect_true(all(as.character(equivalencia$veredicto) == "identico"))

  texto <- punto
  texto$tipo_declarado <- "texto"
  distinto <- comparar_equivalencia(texto, transform(texto, moda = "2245,63"), 0)
  expect_equal(as.character(distinto$veredicto[[1L]]), "materialmente_distinto")

  entero64_a <- punto
  entero64_a$tipo_declarado <- "integer64"
  entero64_a$moda <- "09007199254740993"
  entero64_b <- entero64_a
  entero64_b$moda <- "9007199254740993"
  igual_exacto <- comparar_equivalencia(entero64_a, entero64_b, 0)
  expect_true(all(as.character(igual_exacto$veredicto) == "identico"))
  distinto_exacto <- entero64_b
  distinto_exacto$moda <- "9007199254740994"
  cambio_exacto <- comparar_equivalencia(entero64_b, distinto_exacto, 0)
  expect_equal(as.character(cambio_exacto$veredicto[[1L]]), "materialmente_distinto")
})

testthat::test_that("un cambio real sigue apareciendo en las comparaciones", {
  base <- data.frame(monto = c(1.5, 1.5, 2, 3), stringsAsFactors = FALSE)
  con_moda_distinta <- data.frame(
    monto = c(2, 2, 1.5, 3), stringsAsFactors = FALSE
  )
  p_base <- perfilar(
    base, fecha = .fecha_comparacion("2026-01-01"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  p_moda <- perfilar(
    con_moda_distinta, fecha = .fecha_comparacion("2026-02-01"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  eq <- comparar_equivalencia(p_base, p_moda, tolerancia = 0)
  fila_moda <- eq[eq$columna == "monto" & eq$campo == "moda", , drop = FALSE]
  expect_equal(as.character(fila_moda$veredicto), "materialmente_distinto")

  con_maximo_distinto <- data.frame(
    monto = c(1.5, 1.5, 2, 30), stringsAsFactors = FALSE
  )
  p_maximo <- perfilar(
    con_maximo_distinto, fecha = .fecha_comparacion("2026-02-01"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  deriva <- comparar_perfiles(p_base, p_maximo)
  expect_true(any(deriva$aspecto == "rango" & deriva$columna == "monto"))
})

testthat::test_that("la geometria declara el universo que se intento convertir", {
  testthat::skip_if_not_installed("sf")
  previas <- options(lupa.max_geometrias = 2L)
  on.exit(options(previas), add = TRUE)
  traza <- capture.output({
    trace(
      ".geometria_alternativa", where = asNamespace("lupa"),
      tracer = quote(cat("TRACE geometria_alternativa\n")), print = FALSE
    )
    on.exit(
      untrace(".geometria_alternativa", where = asNamespace("lupa")),
      add = TRUE
    )
    perfil <- suppressWarnings(perfilar(
      data.frame(geometria = c(
        "INVALID", "POINT (1 2)", "POINT (3 4)"
      ), stringsAsFactors = FALSE),
      fecha = .fecha_comparacion("2026-01-01"),
      analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    ))
  })
  expect_true(any(grepl(
    "TRACE geometria_alternativa", traza, fixed = TRUE, useBytes = TRUE
  )))
  publicados <- as.character(unlist(perfil$columnas, use.names = FALSE))
  expect_true(any(grepl(
    "1 de 2 valores que se intentaron convertir", publicados,
    fixed = TRUE, useBytes = TRUE
  )))
})

testthat::test_that("no hay funciones de paquete definidas dos veces", {
  raiz <- getNamespaceInfo(asNamespace("lupa"), "path")
  archivos <- list.files(
    file.path(raiz, "R"), pattern = "\\.R$",
    full.names = TRUE
  )
  nombres <- unlist(lapply(archivos, function(archivo) {
    expresiones <- parse(archivo, keep.source = FALSE)
    vapply(expresiones, function(expresion) {
      es_asignacion <- is.call(expresion) &&
        as.character(expresion[[1L]]) %in% c("<-", "=")
      es_funcion <- length(expresion) >= 3L &&
        is.symbol(expresion[[2L]]) && is.call(expresion[[3L]]) &&
        identical(as.character(expresion[[3L]][[1L]]), "function")
      if (es_asignacion && es_funcion) as.character(expresion[[2L]]) else NA_character_
    }, character(1L))
  }), use.names = FALSE)
  nombres <- nombres[!is.na(nombres)]
  duplicadas <- unique(nombres[duplicated(nombres)])
  expect_true(
    length(duplicadas) == 0L,
    info = paste(duplicadas, collapse = ", ")
  )
})

testthat::test_that("un perfil antiguo sin politica de sentinelas no se acusa como cambio", {
  datos <- data.frame(x = c(1, 1, 2, 999), stringsAsFactors = FALSE)
  anterior <- perfilar(
    datos, fecha = .fecha_comparacion("2026-01-01"),
    analizar_dependencias = FALSE
  )
  anterior$meta$sentinelas_numericos <- NULL
  actual <- perfilar(
    datos, fecha = .fecha_comparacion("2026-02-01"),
    analizar_dependencias = FALSE
  )
  traza <- capture.output({
    trace(
      ".configuracion_sentinelas_perfil", where = asNamespace("lupa"),
      tracer = quote(cat("TRACE sentinelas\n")), print = FALSE
    )
    on.exit(
      untrace(".configuracion_sentinelas_perfil", where = asNamespace("lupa")),
      add = TRUE
    )
    deriva <- comparar_perfiles(anterior, actual)
  })
  expect_true(any(grepl("TRACE sentinelas", traza, fixed = TRUE, useBytes = TRUE)))
  expect_false(any(deriva$aspecto == "configuracion_sentinelas_numericos"))

  sin_sentinelas <- perfilar(
    datos, fecha = .fecha_comparacion("2026-03-01"),
    analizar_dependencias = FALSE, sentinelas_numericos = numeric()
  )
  cambio_politica <- comparar_perfiles(actual, sin_sentinelas)
  fila <- cambio_politica[
    cambio_politica$aspecto == "configuracion_sentinelas_numericos", ,
    drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_equal(as.character(fila$cambio), "modificado")
  expect_equal(as.character(fila$severidad), "error")
})

testthat::test_that("el nombre de columna tabla no es el marcador de tabla", {
  anterior <- perfilar(
    data.frame(
      "<tabla>" = c(1, 1, 2), otra = c(1, 1, 2), check.names = FALSE
    ),
    fecha = .fecha_comparacion("2026-01-01"),
    analizar_dependencias = FALSE
  )
  actual <- perfilar(
    data.frame(
      "<tabla>" = 1:3, otra = 3:1, check.names = FALSE
    ),
    fecha = .fecha_comparacion("2026-02-01"),
    analizar_dependencias = FALSE
  )
  traza <- capture.output({
    trace(
      ".resumir_hallazgos_deriva", where = asNamespace("lupa"),
      tracer = quote(cat("TRACE resumen_hallazgos\n")), print = FALSE
    )
    on.exit(
      untrace(".resumir_hallazgos_deriva", where = asNamespace("lupa")),
      add = TRUE
    )
    deriva <- comparar_perfiles(anterior, actual)
  })
  expect_true(any(grepl(
    "TRACE resumen_hallazgos", traza, fixed = TRUE, useBytes = TRUE
  )))
  fila <- deriva[
    deriva$aspecto == "hallazgo" &
      deriva$valor_anterior == "columnas_duplicadas", , drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_identical(fila$columna[[1L]], "<tabla>")
})

testthat::test_that("la lectura de relaciones no repite el esquema por tabla", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "uno", data.frame(id = 1:3, texto = letters[1:3]))
  DBI::dbWriteTable(con, "dos", data.frame(clave = 1:3, nota = letters[1:3]))
  conjunto <- coleccion(con, c("uno", "dos"), nombre = "b4")
  pares <- data.frame(tabla_1 = "uno", tabla_2 = "dos", stringsAsFactors = FALSE)
  traza <- capture.output({
    trace(
      ".leer_esquema_dbi", where = asNamespace("lupa"),
      tracer = quote(cat("TRACE schema\n")), print = FALSE
    )
    on.exit(
      untrace(".leer_esquema_dbi", where = asNamespace("lupa")),
      add = TRUE
    )
    relaciones_coleccion(
      conjunto, pares = pares, muestra = Inf,
      columnas_candidatas = list(uno = "id", dos = "clave")
    )
  })
  expect_equal(sum(grepl("TRACE schema", traza, fixed = TRUE)), 2L)
})
