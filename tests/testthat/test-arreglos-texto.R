test_that("match conserva las clases seguras y acelera fechas", {
  testthat::skip_on_cran()
  fechas <- as.Date(c(
    "1969-12-31", NA, "1970-01-01", "1970-01-01", "2026-01-01"
  ))
  representantes <- as.Date(c("1970-01-01", "1969-12-31", NA))
  expect_true(.puede_vectorizar_match_R(fechas, representantes))
  expect_identical(
    .match_R(fechas, representantes),
    match(unclass(fechas), unclass(representantes), nomatch = 0L)
  )

  instantes <- structure(
    c(-0.5, NA_real_, 0, 0.25), class = c("POSIXct", "POSIXt"),
    tzone = "UTC"
  )
  instantes_otra_zona <- structure(
    c(0.25, -0.5, 0), class = c("POSIXct", "POSIXt"),
    tzone = "America/Montevideo"
  )
  expect_identical(
    .match_R(instantes, instantes_otra_zona),
    match(unclass(instantes), unclass(instantes_otra_zona), nomatch = 0L)
  )

  duraciones <- as.difftime(c(1, NA, 2), units = "hours")
  duraciones_segundos <- as.difftime(c(7200, 3600), units = "secs")
  expect_identical(
    .match_R(duraciones, duraciones_segundos),
    match(unclass(duraciones), unclass(duraciones_segundos), nomatch = 0L)
  )

  factores <- factor(c("uno", NA, "dos"), levels = c("uno", "dos"))
  factores_reordenados <- factor(
    c("dos", "uno", NA), levels = c("dos", "uno", "tres")
  )
  expect_identical(
    .match_R(factores, factores_reordenados),
    match(as.character(factores), as.character(factores_reordenados),
          nomatch = 0L)
  )

  factor_repetido <- structure(
    c(1L, 2L, 3L, 1L), class = "factor",
    levels = c("a", "a", "b")
  )
  mapa <- .mapa_distintos_bloques(factor_repetido)
  expect_identical(mapa$resultado$frecuencia, c(3L, 1L))
  expect_true(is.null(.match_R(
    structure(c(1L, 2L), class = "factor"),
    structure(c(1L, 2L), class = "factor")
  )))

  difftime_sin_unidades <- structure(c(1, 2), class = "difftime")
  difftime_unidades_mal_formadas <- structure(
    c(1, 2), class = "difftime", units = c("secs", "hours")
  )
  expect_false(.puede_vectorizar_match_R(
    difftime_sin_unidades, difftime_sin_unidades
  ))
  expect_false(.puede_vectorizar_match_R(
    difftime_unidades_mal_formadas, difftime_unidades_mal_formadas
  ))
  expect_identical(
    .match_R(difftime_sin_unidades, difftime_sin_unidades), c(1L, 2L)
  )
  expect_identical(
    .match_R(difftime_unidades_mal_formadas,
             difftime_unidades_mal_formadas), c(1L, 2L)
  )
})

test_that("la tabla de fechas repetidas no cambia el perfil", {
  testthat::skip_on_cran()
  valores <- as.Date("1969-01-01") + rep(0:2000, length.out = 10000L)
  datos <- data.frame(fecha = valores)
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    normalizar = FALSE, casi_duplicados_vocabulario = FALSE,
    duplicados_aproximados = FALSE, max_patrones = 1L
  )
  expect_equal(perfil$general$filas, 10000)
  expect_equal(perfil$columnas$n_distintos[[1L]], 2001)
  expect_equal(perfil$columnas$n_faltantes[[1L]], 0)
  expect_true(.puede_vectorizar_match_R(valores, valores))
  ejecutar <- function(vectorizado) {
    espacio <- asNamespace("lupa")
    original <- espacio$.puede_vectorizar_match_R
    unlockBinding(".puede_vectorizar_match_R", espacio)
    assign(
      ".puede_vectorizar_match_R",
      if (vectorizado) original else function(...) FALSE,
      espacio
    )
    lockBinding(".puede_vectorizar_match_R", espacio)
    on.exit({
      unlockBinding(".puede_vectorizar_match_R", espacio)
      assign(".puede_vectorizar_match_R", original, espacio)
      lockBinding(".puede_vectorizar_match_R", espacio)
    }, add = TRUE)
    suppressWarnings(perfilar(
      datos, analizar_dependencias = FALSE, ausencia_estructural = FALSE,
      normalizar = FALSE, casi_duplicados_vocabulario = FALSE,
      duplicados_aproximados = FALSE, max_patrones = 1L
    ))
  }
  perfil_sin_camino_rapido <- ejecutar(FALSE)
  perfil_sin_camino_rapido$meta$fecha_hora <- NULL
  perfil$meta$fecha_hora <- NULL
  expect_true(isTRUE(all.equal(perfil, perfil_sin_camino_rapido)))

  datos_clases <- data.frame(
    fecha = as.Date(c("1969-12-31", NA, "1970-01-01", "2026-01-01")),
    instante_utc = structure(
      c(-0.5, NA_real_, 0, 0.25), class = c("POSIXct", "POSIXt"),
      tzone = "UTC"
    ),
    instante_local = structure(
      c(0.25, -0.5, 0, NA_real_), class = c("POSIXct", "POSIXt"),
      tzone = "America/Montevideo"
    ),
    duracion_horas = as.difftime(c(1, NA, 2, 3), units = "hours"),
    duracion_segundos = as.difftime(c(3600, 7200, NA, 0), units = "secs"),
    factor_original = factor(
      c("uno", NA, "dos", "uno"), levels = c("uno", "dos")
    ),
    factor_reordenado = factor(
      c("dos", "uno", NA, "dos"), levels = c("dos", "uno", "tres")
    )
  )
  ejecutar_clases <- function(vectorizado) {
    espacio <- asNamespace("lupa")
    original <- espacio$.puede_vectorizar_match_R
    unlockBinding(".puede_vectorizar_match_R", espacio)
    assign(
      ".puede_vectorizar_match_R",
      if (vectorizado) original else function(...) FALSE,
      espacio
    )
    lockBinding(".puede_vectorizar_match_R", espacio)
    on.exit({
      unlockBinding(".puede_vectorizar_match_R", espacio)
      assign(".puede_vectorizar_match_R", original, espacio)
      lockBinding(".puede_vectorizar_match_R", espacio)
    }, add = TRUE)
    suppressWarnings(perfilar(
      datos_clases, analizar_dependencias = FALSE,
      ausencia_estructural = FALSE, normalizar = FALSE,
      casi_duplicados_vocabulario = FALSE, duplicados_aproximados = FALSE,
      max_patrones = 1L
    ))
  }
  perfil_clases <- ejecutar_clases(TRUE)
  perfil_clases_lento <- ejecutar_clases(FALSE)
  perfil_clases$meta$fecha_hora <- NULL
  perfil_clases_lento$meta$fecha_hora <- NULL
  expect_true(isTRUE(all.equal(perfil_clases, perfil_clases_lento)))
})

test_that("los textos largos quedan fuera de los diagnosticos costosos", {
  testthat::skip_on_cran()
  valores <- vapply(seq_len(20L), function(i) paste0(
    "<raiz>", strrep("<n>texto</n>", 1200L), "</raiz>", i
  ), character(1L))
  datos <- data.frame(texto = valores, stringsAsFactors = FALSE)
  # Se declara en la cobertura, como el tope de casi duplicados, sin aviso.
  expect_no_warning(
    perfil <- perfilar(
      datos, analizar_dependencias = FALSE, ausencia_estructural = FALSE,
      normalizar = FALSE, casi_duplicados_vocabulario = TRUE,
      max_patrones = 1L
    )
  )
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "texto_largo_no_evaluado",
    , drop = FALSE
  ]
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$motivo[[1L]], "20 valor")
  expect_equal(perfil$columnas$n_distintos[[1L]], 20)
  expect_gt(perfil$columnas$longitud_maxima[[1L]], 12000)
  expect_true(all(perfil$patrones[[1L]]$n == 0L))
})

test_that("aplicabilidad conserva el universo con y sin textos largos", {
  testthat::skip_on_cran()
  datos <- data.frame(
    n = c(1, 1, 2, 2, 3),
    cond = c(TRUE, TRUE, FALSE, FALSE, TRUE)
  )
  argumentos <- list(
    datos = datos, aplicabilidad = list(n = ~cond),
    analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    normalizar = FALSE, casi_duplicados_vocabulario = FALSE,
    duplicados_aproximados = FALSE, max_patrones = 5L
  )
  perfil <- do.call(perfilar, argumentos)
  expect_equal(
    unname(as.list(perfil$columnas[1L, c(
      "n_aplicables", "minimo", "maximo", "media", "mediana", "desvio"
    )])),
    list(3, 1, 3, 5 / 3, 1, sqrt(4 / 3))
  )
  con_largo_fuera <- data.frame(
    v = c("a", "b", "a", paste0("z", strrep("q", 10001L))),
    cond = c(TRUE, TRUE, TRUE, FALSE), stringsAsFactors = FALSE
  )
  avisos <- character()
  perfil_largo <- withCallingHandlers(
    do.call(perfilar, c(
      list(datos = con_largo_fuera, aplicabilidad = list(v = ~cond)),
      argumentos[!names(argumentos) %in% c("datos", "aplicabilidad")]
    )),
    warning = function(w) {
      avisos <<- c(avisos, class(w)[[1L]])
      invokeRestart("muffleWarning")
    }
  )
  expect_false("lupa_texto_largo" %in% avisos)
  expect_false(any(
    perfil_largo$cobertura_diagnosticos$diagnostico ==
      "texto_largo_no_evaluado"
  ))
  expect_equal(perfil_largo$columnas$n_aplicables[[1L]], 3)
  fila_cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "faltantes", , drop = FALSE
  ]
  expect_true(is.list(fila_cobertura$columnas))
  expect_identical(
    fila_cobertura[1L, , drop = FALSE]$columnas[[1L]], "n"
  )
})

test_that("el tope no altera valores cortos ni el perfil sin largos", {
  testthat::skip_on_cran()
  datos <- data.frame(
    v = c("-9999", "-9999", "1", "2", strrep("9", 10001L)),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(
    datos, sentinelas_numericos = c(-9999, -999),
    analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    normalizar = FALSE, casi_duplicados_vocabulario = FALSE,
    duplicados_aproximados = FALSE
  ))
  # Los dos -9999 son centinelas declarados: salen de los conteos y se cuentan
  # como excluidos, como en la rama numerica y en `integer64`.
  expect_identical(perfil$columnas$n_negativos[[1L]], 0L)
  expect_identical(
    as.integer(perfil$columnas$n_valores_excluidos_resumen[[1L]]), 2L
  )
  expect_identical(perfil$columnas$n_faltantes_disfrazados[[1L]], 2L)
  expect_identical(perfil$columnas$n_distintos[[1L]], 4L)
  expect_identical(perfil$columnas$tipo_inferido[[1L]], "entero")
  expect_identical(
    perfil$columnas$estado_resumen_cuantitativo[[1L]], "omitidos_precision"
  )

  cortos <- data.frame(v = c("a", "b", "a"), stringsAsFactors = FALSE)
  tope <- suppressWarnings(perfilar(
    cortos, max_largo_valor_vocabulario = 10000L,
    analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    normalizar = FALSE, casi_duplicados_vocabulario = FALSE,
    duplicados_aproximados = FALSE
  ))
  infinito <- suppressWarnings(perfilar(
    cortos, max_largo_valor_vocabulario = Inf,
    analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    normalizar = FALSE, casi_duplicados_vocabulario = FALSE,
    duplicados_aproximados = FALSE
  ))
  tope$meta$fecha_hora <- NULL
  infinito$meta$fecha_hora <- NULL
  tope$meta$max_largo_valor_vocabulario <- NULL
  infinito$meta$max_largo_valor_vocabulario <- NULL
  expect_true(isTRUE(all.equal(tope, infinito)))

  cortos$cond <- c(TRUE, FALSE, TRUE)
  tope_aplicable <- suppressWarnings(perfilar(
    cortos, aplicabilidad = list(v = ~cond),
    max_largo_valor_vocabulario = 10000L,
    analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    normalizar = FALSE, casi_duplicados_vocabulario = FALSE,
    duplicados_aproximados = FALSE
  ))
  infinito_aplicable <- suppressWarnings(perfilar(
    cortos, aplicabilidad = list(v = ~cond),
    max_largo_valor_vocabulario = Inf,
    analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    normalizar = FALSE, casi_duplicados_vocabulario = FALSE,
    duplicados_aproximados = FALSE
  ))
  tope_aplicable$meta$fecha_hora <- NULL
  infinito_aplicable$meta$fecha_hora <- NULL
  tope_aplicable$meta$max_largo_valor_vocabulario <- NULL
  infinito_aplicable$meta$max_largo_valor_vocabulario <- NULL
  expect_true(isTRUE(all.equal(tope_aplicable, infinito_aplicable)))
})

test_that("cobertura_analisis acepta el perfil de muestra DBI", {
  testthat::skip_on_cran()
  datos <- data.frame(valor = 1:4)
  perfil_muestra <- perfilar(
    datos, analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  perfil_dbi <- structure(
    list(perfil_muestra = perfil_muestra), class = "perfil_dbi"
  )
  cobertura <- cobertura_analisis(perfil_dbi)
  expect_true(nrow(cobertura) > 0L)
  expect_true(all(grepl("perfil_muestra", cobertura$motivo, fixed = TRUE)))
  expect_error(
    cobertura_analisis(structure(list(perfil_muestra = NULL),
                                 class = "perfil_dbi")),
    "perfil_muestra"
  )
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "tabla_prueba", datos)
  perfil_dbi_real <- suppressWarnings(perfilar_dbi(
    conexion, "tabla_prueba", muestra = 2L,
    analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    casi_duplicados_vocabulario = FALSE
  ))
  cobertura_real <- cobertura_analisis(perfil_dbi_real)
  expect_true(all(grepl(
    "perfil_muestra", cobertura_real$motivo, fixed = TRUE
  )))

  largo <- data.frame(
    texto = c(paste0("x", strrep("q", 10001L)), "corto"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conexion, "tabla_larga", largo)
  # Sin aviso: el alcance queda en la cobertura de la muestra.
  expect_no_warning(perfil_largo <- perfilar_dbi(
    conexion, "tabla_larga", muestra = Inf,
    analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    normalizar = FALSE, casi_duplicados_vocabulario = FALSE,
    duplicados_aproximados = FALSE
  ))
  expect_true(any(
    perfil_largo$perfil_muestra$cobertura_diagnosticos$diagnostico ==
      "texto_largo_no_evaluado"
  ))
  expect_true(any(
    perfil_largo$perfil_muestra$cobertura_diagnosticos$diagnostico ==
      "texto_largo_no_evaluado"
  ))

  perfil_agregados <- suppressWarnings(perfilar_dbi(
    conexion, "tabla_prueba", bloque_muestra = "solo_agregados",
    analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    casi_duplicados_vocabulario = FALSE
  ))
  cobertura_agregados <- cobertura_analisis(perfil_agregados)
  expect_true(nrow(cobertura_agregados) > 0L)
  expect_false(any(grepl(
    "no representa la tabla completa", cobertura_agregados$motivo,
    fixed = TRUE
  )))

  perfil_infinito <- suppressWarnings(perfilar_dbi(
    conexion, "tabla_prueba", muestra = Inf,
    analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    casi_duplicados_vocabulario = FALSE
  ))
  cobertura_infinito <- cobertura_analisis(perfil_infinito)
  expect_false(any(grepl(
    "no representa la tabla completa", cobertura_infinito$motivo,
    fixed = TRUE
  )))
  vacia <- suppressWarnings(perfilar(
    data.frame(valor = character()), analizar_dependencias = FALSE,
    ausencia_estructural = FALSE, casi_duplicados_vocabulario = FALSE
  ))
  perfil_infinito$perfil_muestra <- vacia
  cobertura_vacia <- cobertura_analisis(perfil_infinito)
  expect_true(any(grepl(
    "La muestra no tiene filas", cobertura_vacia$motivo, fixed = TRUE
  )))
})

test_that("los nombres con coma conservan la traza y los consumidores", {
  testthat::skip_on_cran()
  datos <- data.frame(inicio = 1:30, check.names = FALSE)
  fin <- 1:30
  fin[c(28L, 29L, 30L)] <- c(31, 30, 1)
  datos[["fin, real"]] <- fin
  perfil <- suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    normalizar = FALSE, casi_duplicados_vocabulario = FALSE,
    umbral_orden_columnas = 0.8, umbral_aritmetica = 1
  ))
  indice <- which(perfil$hallazgos$tipo_hallazgo ==
                    "relacion_orden_columnas")
  expect_length(indice, 1L)
  expect_identical(
    perfil$hallazgos$trazabilidad[[indice]]$columnas,
    c("inicio", "fin, real")
  )
  expect_identical(
    .separar_nombres_columnas(
      "inicio,fin, real", c("inicio", "fin, real")
    ),
    c("inicio", "fin, real")
  )
  expect_identical(
    .separar_nombres_columnas(
      "a, b,\"c\",d\ne", c("a", "b", "\"c\"", "d\ne")
    ),
    c("a", "b", "\"c\"", "d\ne")
  )
  cobertura_nueva <- data.frame(
    columna = "a,b", diagnostico = "x", stringsAsFactors = FALSE
  )
  cobertura_nueva$columnas <- I(list(character()))
  expect_length(.columnas_cobertura(
    cobertura_nueva, 1L, c("a", "b", "a,b")
  ), 0L)
  cobertura_historica <- cobertura_nueva[, c("columna", "diagnostico"),
                                          drop = FALSE]
  expect_identical(
    .columnas_cobertura(cobertura_historica, 1L, c("a", "b", "a,b")),
    c("a", "b")
  )
  tabla_ambigua <- data.frame(a = 1:3, b = 4:6, check.names = FALSE)
  tabla_ambigua[["a,b"]] <- 7:9
  expect_null(suppressMessages(.clave_escrita("a,b", tabla_ambigua)))
  perfil_clave_vector <- suppressWarnings(perfilar(
    tabla_ambigua, clave = c("a,b"), analizar_dependencias = FALSE,
    ausencia_estructural = FALSE, normalizar = FALSE,
    casi_duplicados_vocabulario = FALSE, duplicados_aproximados = FALSE
  ))
  expect_identical(perfil_clave_vector$meta$declaracion_clave, "a,b")
  aritmetica <- data.frame(a = 1:30, check.names = FALSE)
  b <- 2:31
  total <- (1:30) + b
  total[[30L]] <- 0
  aritmetica[["b, real"]] <- b
  aritmetica$total <- total
  perfil_aritmetica <- suppressWarnings(perfilar(
    aritmetica, analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    normalizar = FALSE, casi_duplicados_vocabulario = FALSE,
    umbral_orden_columnas = 1, umbral_aritmetica = 0.8
  ))
  indice_aritmetica <- which(
    perfil_aritmetica$hallazgos$tipo_hallazgo ==
      "relacion_aritmetica_columnas"
  )
  expect_true(length(indice_aritmetica) > 0L)
  expect_true(any(vapply(
    perfil_aritmetica$hallazgos$trazabilidad[indice_aritmetica],
    function(traza) "b, real" %in% traza$columnas, logical(1L)
  )))

  colision <- data.frame(x = c("aa", "ab", "aa", "zz"),
                         check.names = FALSE)
  colision[["fin, real"]] <- c("bb", "bc", "bb", "yy")
  colision[["x, fin, real"]] <- c("constante", "otra", "constante", "otra")
  hallazgo_colision <- .nuevo_hallazgo(
    "x, fin, real", "duplicados_exactos_columnas", "sospechoso",
    "", "", "", n_evaluados = 4, n_afectados = 2, unidad_conteo = "fila"
  )
  aproximados_colision <- list(
    pares = data.frame(
      fila_1 = 1L, fila_2 = 3L, tipo_par = "exacto",
      stringsAsFactors = FALSE
    ),
    columnas = c("x", "fin, real"),
    alcance = list(n_pares_comparados = 1, n_pares_posibles = 1)
  )
  traza_colision <- .trazabilidad_hallazgo(
    "duplicados_exactos_columnas", hallazgo_colision, colision,
    names(colision), list(), FALSE, aproximados_colision, 1L,
    "duplicados_exactos_columnas", 1000L
  )
  expect_identical(traza_colision$columnas, c("x", "fin, real"))

  aritmetica_exacta <- data.frame(a = 1:40, check.names = FALSE)
  aritmetica_exacta[["b, real"]] <- 2:41
  aritmetica_exacta$total <- aritmetica_exacta$a + aritmetica_exacta[["b, real"]]
  perfil_aritmetica_exacta <- suppressWarnings(perfilar(
    aritmetica_exacta, analizar_dependencias = FALSE,
    ausencia_estructural = FALSE, normalizar = FALSE,
    casi_duplicados_vocabulario = FALSE, duplicados_aproximados = FALSE,
    umbral_orden_columnas = 1, umbral_aritmetica = 0.9
  ))
  indice_aritmetica_exacta <- which(
    perfil_aritmetica_exacta$hallazgos$tipo_hallazgo ==
      "relacion_aritmetica_columnas"
  )[[1L]]
  expect_identical(
    perfil_aritmetica_exacta$hallazgos$trazabilidad[[indice_aritmetica_exacta]]$columnas,
    c("a", "b, real", "total")
  )
  casos_retirada <- list(
    c("a, b", "b"), c("a, b, c", "c"), c("a, b, c", "b"),
    c("fin, real", "real"), c("x, fin, real", "real"),
    c("a b, c", "c"), c("a ,b", "a")
  )
  expect_true(all(vapply(
    casos_retirada,
    function(caso) .hallazgo_nombra_columna_retirada(
      caso[[1L]], caso[[2L]]
    ),
    logical(1L)
  )))
  especiales <- data.frame(id = 1:3, check.names = FALSE)
  especiales[["fin, real"]] <- c(1, 2, 3)
  especiales[["\"c\""]] <- c("x", "y", "z")
  especiales[["linea\nreal"]] <- c("x", "y", "z")
  perfil_especiales <- suppressWarnings(perfilar(
    especiales, analizar_dependencias = FALSE, ausencia_estructural = FALSE,
    normalizar = FALSE, casi_duplicados_vocabulario = FALSE,
    umbral_orden_columnas = 1, umbral_aritmetica = 1
  ))
  expect_true(all(c("fin, real", "\"c\"", "linea\nreal") %in%
                  as.character(perfil_especiales$columnas$columna)))
  if (requireNamespace("stringdist", quietly = TRUE)) {
    duplicados <- data.frame(x = rep(c("aa", "ab", "zz"), 4L),
                             check.names = FALSE)
    duplicados[["fin, real"]] <- rep(c("bb", "bc", "yy"), 4L)
    perfil_duplicados <- suppressWarnings(perfilar(
      duplicados, analizar_dependencias = FALSE, ausencia_estructural = FALSE,
      normalizar = FALSE, casi_duplicados_vocabulario = FALSE,
      duplicados_aproximados = TRUE, max_patrones = 1L
    ))
    indice_duplicado <- which(
      perfil_duplicados$hallazgos$tipo_hallazgo ==
        "duplicados_exactos_columnas"
    )
    expect_true(length(indice_duplicado) > 0L)
    expect_true(all(vapply(
      perfil_duplicados$hallazgos$trazabilidad[indice_duplicado],
      function(traza) identical(traza$columnas, c("x", "fin, real")),
      logical(1L)
    )))
  }
  expect_identical(
    hallazgos(perfil)$trazabilidad[[indice]]$columnas,
    c("inicio", "fin, real")
  )
  plan <- planificar_limpieza(perfil, datos)
  expect_true(inherits(plan, "plan_limpieza"))
  expect_true(is.data.frame(attr(plan, "hallazgos_sin_accion")))
  datos_sin_relacion <- data.frame(inicio = 1:30, check.names = FALSE)
  datos_sin_relacion[["fin, real"]] <- 1:30
  deriva <- comparar_perfiles(
    perfil,
    suppressWarnings(perfilar(
      datos_sin_relacion, fecha = as.POSIXct("2026-02-01", tz = "UTC"),
      analizar_dependencias = FALSE, ausencia_estructural = FALSE,
      normalizar = FALSE, casi_duplicados_vocabulario = FALSE,
      umbral_orden_columnas = 0.8, umbral_aritmetica = 1
    )))
  expect_true(any(grepl("fin, real", as.character(deriva$columna), fixed = TRUE)))
  archivo <- "reporte-arreglos-texto.html"
  on.exit(unlink(archivo), add = TRUE)
  reportar(perfil, archivo = archivo, sobrescribir = TRUE)
  expect_true(grepl("fin, real", paste(readLines(archivo, warn = FALSE),
                                       collapse = "\n"), fixed = TRUE))
  archivo_especiales <- "reporte-arreglos-texto-especiales.html"
  on.exit(unlink(archivo_especiales), add = TRUE)
  reportar(perfil_especiales, archivo = archivo_especiales, sobrescribir = TRUE)
  expect_true(grepl(
    "fin, real",
    paste(readLines(archivo_especiales, warn = FALSE), collapse = "\n"),
    fixed = TRUE
  ))
})
