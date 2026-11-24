# La politica de ausencia declarada (`sentinelas_numericos`, `columnas_opcionales`,
# `aplicabilidad`) en el resumen de tabla completa de `perfilar_dbi()`.
#
# La promesa: el resumen mide lo mismo que `perfilar()` sobre la tabla leida,
# campo por campo, por sus tres caminos -las consultas agregadas, la lectura por
# bloques y la muestra del motor-, o dice por columna que no pudo. Un centinela
# declarado sale de minimo, maximo, media, mediana, desvio, ceros y negativos, y
# sigue contando en validos, distintos y moda.

# Varias corridas completas contra dos motores: pesada para CRAN.
testthat::skip_on_cran()

.aud_sqlite <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}

.aud_duckdb <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  DBI::dbConnect(.duckdb_prueba(), ":memory:")
}

.aud_desconectar <- function(con) {
  if (inherits(con, "duckdb_connection")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else DBI::dbDisconnect(con)
}

.aud_dbi <- function(con, tabla, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, ...
  )))
}

.aud_memoria <- function(datos, ...) {
  suppressWarnings(suppressMessages(perfilar(
    datos, proteger_datos_personales = FALSE, analizar_dependencias = FALSE,
    ...
  )))
}

.aud_fila <- function(columnas, columna) {
  columnas[columnas$columna == columna, , drop = FALSE]
}

.aud_magnitudes <- c(
  "minimo", "maximo", "media", "mediana", "desvio", "n_ceros", "n_negativos"
)

# Un bucle sobre una lista vacia no afirma nada y pasa: cada lista de casos de
# este archivo se recorre por aca, que falla si no trae ninguno.
.aud_casos <- function(x) {
  expect_gt(length(x), 0L)
  x
}

# Los campos de una columna del resumen iguales a los de `perfilar()`.
.aud_esperar_memoria <- function(resumen, memoria, columna, campos, info) {
  expect_identical(length(.aud_magnitudes), 7L)
  r <- .aud_fila(resumen$resumen_tabla$columnas, columna)
  m <- .aud_fila(memoria$columnas, columna)
  for (campo in .aud_casos(campos)) {
    expect_equal(
      as.numeric(r[[campo]]), as.numeric(m[[campo]]),
      info = paste(info, columna, campo)
    )
  }
}

.aud_degradadas <- function(resumen) {
  cobertura <- resumen$resumen_tabla$cobertura
  cobertura$elemento[
    cobertura$bloque == "resumen_tabla" & cobertura$estado == "degradado"
  ]
}

# Una columna con dos centinelas, un cero, un negativo y un `NULL`. A mano, sin
# los dos -9 quedan 0, -3, 4 y 10: minimo -3, maximo 10, media 2,75, mediana 2,
# un cero y un negativo. Los validos son 6, los distintos 5 y la moda es -9.
.aud_datos <- function() {
  data.frame(id = 1:7, x = c(-9, -9, 0, -3, 4, 10, NA))
}

test_that("un centinela declarado sale de las magnitudes del resumen y sigue en los conteos", {
  datos <- .aud_datos()
  memoria <- .aud_memoria(datos, sentinelas_numericos = -9)
  for (motor in .aud_casos(c("sqlite", "duckdb"))) {
    con <- if (motor == "sqlite") .aud_sqlite() else .aud_duckdb()
    DBI::dbWriteTable(con, "lupa_aud", datos)
    r <- .aud_dbi(con, "lupa_aud", sentinelas_numericos = -9)
    fila <- .aud_fila(r$resumen_tabla$columnas, "x")
    expect_equal(fila$minimo, -3, info = motor)
    expect_equal(fila$maximo, 10, info = motor)
    expect_equal(fila$media, 2.75, info = motor)
    expect_equal(fila$mediana, 2, info = motor)
    expect_equal(fila$desvio, stats::sd(c(0, -3, 4, 10)), info = motor)
    expect_equal(fila$n_ceros, 1, info = motor)
    expect_equal(fila$n_negativos, 1, info = motor)
    expect_equal(fila$n_validos, 6, info = motor)
    expect_equal(fila$n_faltantes, 1, info = motor)
    expect_equal(fila$n_distintos, 5, info = motor)
    expect_equal(as.numeric(fila$moda), -9, info = motor)
    expect_equal(fila$frecuencia_moda, 2, info = motor)
    expect_equal(fila$n_valores_excluidos_resumen, 2, info = motor)
    .aud_esperar_memoria(r, memoria, "x", c(
      .aud_magnitudes, "n_faltantes", "n_distintos", "frecuencia_moda",
      "n_valores_excluidos_resumen"
    ), motor)
    expect_identical(.aud_degradadas(r), character(), info = motor)
    expect_identical(
      r$resumen_tabla$meta$politica_ausencia$sentinelas_numericos,
      c("id", "x"), info = motor
    )
    # Sin declararlo, el resumen es el de siempre: cuenta el -9 y no agrega
    # columnas ni metadatos.
    sin <- .aud_dbi(con, "lupa_aud")
    expect_equal(.aud_fila(sin$resumen_tabla$columnas, "x")$minimo, -9,
                 info = motor)
    expect_false("n_valores_excluidos_resumen" %in%
                   names(sin$resumen_tabla$columnas), info = motor)
    expect_null(sin$resumen_tabla$meta$politica_ausencia, info = motor)
    .aud_desconectar(con)
  }
})

test_that("una columna que es toda centinela queda sin magnitudes y con sus conteos", {
  datos <- data.frame(id = 1:4, x = c(-9, -9, -9, NA))
  memoria <- .aud_memoria(datos, sentinelas_numericos = -9)
  for (motor in .aud_casos(c("sqlite", "duckdb"))) {
    con <- if (motor == "sqlite") .aud_sqlite() else .aud_duckdb()
    DBI::dbWriteTable(con, "lupa_aud", datos)
    r <- .aud_dbi(con, "lupa_aud", sentinelas_numericos = -9)
    fila <- .aud_fila(r$resumen_tabla$columnas, "x")
    for (campo in .aud_casos(.aud_magnitudes)) {
      expect_true(is.na(fila[[campo]]), info = paste(motor, campo))
      expect_true(is.na(.aud_fila(memoria$columnas, "x")[[campo]]),
                  info = paste(motor, campo, "memoria"))
    }
    expect_equal(fila$n_validos, 3, info = motor)
    expect_equal(fila$n_distintos, 1, info = motor)
    expect_equal(fila$frecuencia_moda, 3, info = motor)
    expect_equal(fila$n_valores_excluidos_resumen, 3, info = motor)
    .aud_desconectar(con)
  }
})

test_that("la lista por columna excluye en cada columna lo suyo", {
  datos <- data.frame(a = c(-9, 1, 2, 999), b = c(-9, 1, 2, 999))
  con <- .aud_sqlite()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_aud", datos)
  declaracion <- list(a = -9, b = 999)
  r <- .aud_dbi(con, "lupa_aud", sentinelas_numericos = declaracion)
  memoria <- .aud_memoria(datos, sentinelas_numericos = declaracion)
  columnas <- r$resumen_tabla$columnas
  expect_equal(.aud_fila(columnas, "a")$minimo, 1)
  expect_equal(.aud_fila(columnas, "a")$maximo, 999)
  expect_equal(.aud_fila(columnas, "b")$minimo, -9)
  expect_equal(.aud_fila(columnas, "b")$maximo, 2)
  for (columna in .aud_casos(c("a", "b"))) {
    .aud_esperar_memoria(r, memoria, columna, .aud_magnitudes, "lista")
  }
})

test_that("una columna opcional no cuenta sus `NULL` como faltantes en el resumen", {
  datos <- data.frame(id = 1:5, x = c(1, NA, 3, NA, 3))
  memoria <- .aud_memoria(datos, columnas_opcionales = "x")
  for (motor in .aud_casos(c("sqlite", "duckdb"))) {
    con <- if (motor == "sqlite") .aud_sqlite() else .aud_duckdb()
    DBI::dbWriteTable(con, "lupa_aud", datos)
    r <- .aud_dbi(con, "lupa_aud", columnas_opcionales = "x")
    fila <- .aud_fila(r$resumen_tabla$columnas, "x")
    expect_equal(fila$n_faltantes, 0, info = motor)
    expect_equal(fila$prop_faltantes, 0, info = motor)
    expect_equal(fila$n_validos, 3, info = motor)
    expect_equal(fila$n_aplicables, 3, info = motor)
    expect_equal(fila$n_no_aplica, 2, info = motor)
    .aud_esperar_memoria(r, memoria, "x", c(
      "n_faltantes", "prop_faltantes", "n_aplicables", "n_no_aplica",
      "n_aplicabilidad_indeterminada", "n_distintos", .aud_magnitudes
    ), motor)
    # La otra columna no tiene universo declarado: el suyo es la tabla.
    expect_equal(.aud_fila(r$resumen_tabla$columnas, "id")$n_aplicables, 5,
                 info = motor)
    expect_identical(.aud_degradadas(r), character(), info = motor)
    .aud_desconectar(con)
  }
})

test_that("la regla de aplicabilidad se aplica por las consultas agregadas, por bloques y por la muestra del motor", {
  datos <- data.frame(
    id = 1:8, g = c("a", "a", "a", "b", "b", NA, "a", "b"),
    x = c(1, 2, NA, 50, 60, 70, 3, NA), stringsAsFactors = FALSE
  )
  regla <- list(x = ~ g == "a")
  memoria <- .aud_memoria(datos, aplicabilidad = regla)
  campos <- c(
    "n_faltantes", "prop_faltantes", "n_distintos", "frecuencia_moda",
    "n_aplicables", "n_no_aplica", "n_aplicabilidad_indeterminada",
    .aud_magnitudes
  )
  con <- .aud_sqlite()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_aud", datos)
  sin <- .aud_dbi(con, "lupa_aud")
  # A mano: cuatro filas con `g == "a"`, tres sin, una sin saber; de las cuatro,
  # `x` vale 1, 2, `NULL` y 3.
  for (camino in .aud_casos(c("consultas", "bloques", "muestra_motor"))) {
    r <- switch(
      camino,
      consultas = .aud_dbi(con, "lupa_aud", aplicabilidad = regla),
      bloques = .aud_dbi(con, "lupa_aud", aplicabilidad = regla,
                         bloque_filas = 3L),
      muestra_motor = .aud_dbi(con, "lupa_aud", aplicabilidad = regla,
                               universo = "muestra_motor", muestra_motor = 8L)
    )
    fila <- .aud_fila(r$resumen_tabla$columnas, "x")
    expect_equal(fila$n_aplicables, 4, info = camino)
    expect_equal(fila$n_no_aplica, 3, info = camino)
    expect_equal(fila$n_aplicabilidad_indeterminada, 1, info = camino)
    expect_equal(fila$n_validos, 3, info = camino)
    expect_equal(fila$n_faltantes, 1, info = camino)
    expect_equal(fila$prop_faltantes, 1 / 4, info = camino)
    expect_equal(fila$maximo, 3, info = camino)
    expect_equal(fila$media, 2, info = camino)
    .aud_esperar_memoria(r, memoria, "x", campos, camino)
    expect_identical(.aud_degradadas(r), character(), info = camino)
    expect_identical(r$resumen_tabla$meta$politica_ausencia$aplicabilidad, "x",
                     info = camino)
    # La otra columna no tiene regla: su universo es la tabla.
    expect_equal(.aud_fila(r$resumen_tabla$columnas, "g")$n_aplicables, 8,
                 info = camino)
  }
  # La regla viaja dentro de las mismas consultas: ni una mas.
  con_regla <- .aud_dbi(con, "lupa_aud", aplicabilidad = regla)
  expect_identical(con_regla$resumen_tabla$meta$consultas$emitidas,
                   sin$resumen_tabla$meta$consultas$emitidas)
  # Y la corroboracion compara esa columna en lugar de dejarla afuera.
  no_comparadas <- con_regla$resumen_tabla$meta$corroboracion_bloques$no_comparadas
  expect_false(any(no_comparadas$causa %in% "aplicabilidad"))
  # Con centinelas declarados, las dos cosas a la vez.
  ambas <- list(aplicabilidad = regla, sentinelas_numericos = 3)
  r <- do.call(.aud_dbi, c(list(con, "lupa_aud"), ambas))
  memoria_ambas <- do.call(.aud_memoria, c(list(datos), ambas))
  expect_equal(.aud_fila(r$resumen_tabla$columnas, "x")$maximo, 2)
  .aud_esperar_memoria(r, memoria_ambas, "x", c(
    campos, "n_valores_excluidos_resumen"
  ), "regla y centinela")
})

test_that("la regla sobre una columna NOT NULL o unica cuenta las filas donde se cumple", {
  datos <- data.frame(
    id = 1:8, g = c("a", "a", "a", "b", "b", NA, "a", "b"),
    h = c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L),
    u = c(10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L), stringsAsFactors = FALSE
  )
  regla <- list(h = ~ g == "a", u = ~ g == "a", id = ~ g == "b")
  memoria <- .aud_memoria(datos, aplicabilidad = regla)
  for (motor in .aud_casos(c("sqlite", "duckdb"))) {
    con <- if (motor == "sqlite") .aud_sqlite() else .aud_duckdb()
    DBI::dbExecute(con, paste0(
      "CREATE TABLE lupa_aud (id INTEGER PRIMARY KEY, g ",
      if (motor == "sqlite") "TEXT" else "VARCHAR",
      ", h INTEGER NOT NULL, u INTEGER NOT NULL UNIQUE)"
    ))
    DBI::dbAppendTable(con, "lupa_aud", datos)
    for (metricas in .aud_casos(list(
      c("validos", "distintos", "moda", "basicos", "mediana", "desvio"),
      "validos"
    ))) {
      info <- paste(motor, paste(metricas, collapse = "+"))
      r <- .aud_dbi(con, "lupa_aud", aplicabilidad = regla, metricas = metricas)
      # A mano: cuatro filas con `g == "a"` y tres con `g == "b"`. El catalogo
      # garantiza que esas columnas no tienen `NULL`, y eso no dice cuantas
      # filas cumplen la regla.
      for (columna in .aud_casos(c("h", "u"))) {
        fila <- .aud_fila(r$resumen_tabla$columnas, columna)
        expect_equal(fila$n_aplicables, 4, info = paste(info, columna))
        expect_equal(fila$n_validos, 4, info = paste(info, columna))
        expect_equal(fila$n_faltantes, 0, info = paste(info, columna))
      }
      expect_equal(.aud_fila(r$resumen_tabla$columnas, "id")$n_validos, 3,
                   info = info)
      if ("distintos" %in% metricas) {
        for (columna in .aud_casos(c("h", "u", "id"))) {
          .aud_esperar_memoria(r, memoria, columna, c(
            "n_distintos", "frecuencia_moda", "n_no_aplica",
            "n_aplicabilidad_indeterminada", .aud_magnitudes
          ), paste(info, columna))
        }
      }
      # El plan anuncia las consultas que la corrida manda.
      expect_identical(r$resumen_tabla$meta$consultas$conciliacion$estado,
                       "conciliado", info = info)
    }
    .aud_desconectar(con)
  }
})

test_that("la regla puede nombrar un valor de quien la escribe", {
  datos <- data.frame(
    id = 1:8, g = c("a", "a", "c", "b", "b", NA, "a", "c"),
    h = c(1, 2, 3, 4, 5, 1, 2, 3), x = c(1, 2, NA, 50, 60, 70, 3, 9),
    stringsAsFactors = FALSE
  )
  con <- .aud_sqlite()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_aud", datos)
  tope <- 1
  permitidos <- c("a", "c")
  regla <- list(x = ~ h > tope & g %in% permitidos)
  memoria <- .aud_memoria(datos, aplicabilidad = regla)
  r <- .aud_dbi(con, "lupa_aud", aplicabilidad = regla)
  # A mano: `h > 1` y `g` en a o c deja las filas 2, 3, 7 y 8.
  fila <- .aud_fila(r$resumen_tabla$columnas, "x")
  expect_equal(fila$n_aplicables, 4)
  expect_equal(fila$n_validos, 3)
  expect_equal(fila$maximo, 9)
  .aud_esperar_memoria(r, memoria, "x", c(
    "n_faltantes", "n_distintos", "n_aplicables", "n_no_aplica",
    "n_aplicabilidad_indeterminada", .aud_magnitudes
  ), "valor de quien llama")
  expect_identical(.aud_degradadas(r), character())
})

test_that("una regla que el motor no evalua como R se declara para su columna y no se aplica a medias", {
  datos <- data.frame(
    id = 1:8, g = c("a", "a", "a", "b", "b", NA, "a", "b"),
    h = c(1, 2, 3, 4, 5, 1, 2, 3), x = c(1, 2, NA, 50, 60, 70, 3, NA),
    stringsAsFactors = FALSE
  )
  con <- .aud_sqlite()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_aud", datos)
  # Una funcion, aritmetica sobre una columna y el orden de un texto.
  reglas <- list(~ nchar(g) == 1 & g == "a", ~ h + 1 > 3, ~ g < "b")
  for (regla in .aud_casos(reglas)) {
    info <- paste(deparse(regla[[2L]]), collapse = "")
    declaracion <- list(x = regla)
    r <- .aud_dbi(con, "lupa_aud", aplicabilidad = declaracion)
    # El resumen cuenta las ocho filas y lo dice para `x`.
    expect_equal(.aud_fila(r$resumen_tabla$columnas, "x")$maximo, 70,
                 info = info)
    expect_equal(.aud_fila(r$resumen_tabla$columnas, "x")$n_aplicables, 8,
                 info = info)
    expect_identical(.aud_degradadas(r), "x::aplicabilidad", info = info)
    politica <- r$resumen_tabla$meta$politica_ausencia
    expect_identical(politica$aplicabilidad, character(), info = info)
    expect_identical(politica$sin_aplicar$columna, "x", info = info)
    expect_identical(politica$sin_aplicar$politica, "aplicabilidad",
                     info = info)
    # La muestra, que lee las filas, si la aplica; y los bloques tambien.
    memoria <- .aud_memoria(datos, aplicabilidad = declaracion)
    expect_equal(.aud_fila(r$perfil_muestra$columnas, "x")$maximo,
                 .aud_fila(memoria$columnas, "x")$maximo, info = info)
    por_bloques <- .aud_dbi(con, "lupa_aud", aplicabilidad = declaracion,
                            bloque_filas = 3L)
    .aud_esperar_memoria(por_bloques, memoria, "x", c(
      "n_aplicables", "n_no_aplica", "maximo", "media"
    ), info)
    expect_identical(.aud_degradadas(por_bloques), character(), info = info)
  }
})

# ---- La regla que no da un valor logico --------------------------------------
#
# `~ activo` sobre una columna de 0 y 1 no es una condicion en R, y `perfilar()`
# la rechaza. `perfilar_dbi()` no la aplica en ningun bloque -ni en la muestra
# ni en el resumen, por ninguno de sus caminos- y la declara una sola vez para
# su columna. Con el tipo de las columnas se sabe antes de leer; si el motor no
# lo declara, lo dicen las filas.

.aud_vias <- function(motor, bloque_filas = 3L) {
  vias <- list(
    agregados = list(bloque_muestra = "solo_agregados"),
    con_muestra = list(),
    bloques = list(bloque_filas = bloque_filas),
    muestra_motor = list(universo = "muestra_motor", muestra_motor = 100)
  )
  # DuckDB no tiene lectura por bloques.
  if (identical(motor, "duckdb")) vias$bloques <- NULL
  vias
}

.aud_por_via <- function(con, via, ...) {
  do.call(.aud_dbi, c(list(con, "lupa_aud", ...), via))
}

# Las filas de cobertura que hablan de la regla de una columna: la del resumen
# que no la pudo aplicar (`<columna>::aplicabilidad`), la que declara la
# politica entera sin aplicar (`aplicabilidad`) y la de la regla que no da un
# valor logico (bloque `perfil_muestra`, la columna).
.aud_filas_regla <- function(r, columna) {
  cobertura <- r$resumen_tabla$cobertura
  del_resumen <- cobertura$bloque == "resumen_tabla" &
    cobertura$elemento %in% c(paste0(columna, "::aplicabilidad"), "aplicabilidad")
  no_logica <- cobertura$bloque == "perfil_muestra" &
    cobertura$elemento == columna & cobertura$estado == "degradado"
  cobertura[del_resumen | no_logica, , drop = FALSE]
}

# Seis filas. A mano: `x` las cuenta todas (10 a 60, media 35). `y`, con
# `~ g == "a"`, las filas 1, 3 y 5 (1, 3 y 5: media 3); la regla no se cumple en
# dos filas y en la del `NULL` no se sabe.
.aud_filas_regla_sql <- paste(
  "(1, 'a', 1, 1, 10, 1), (2, 'b', 0, 0, 20, 2), (3, 'a', 1, 1, 30, 3),",
  "(4, NULL, NULL, 1, 40, 4), (5, 'a', 1, 0, 50, 5), (6, 'b', 0, 1, 60, 6)"
)

.aud_esperar_regla_sin_aplicar <- function(r, info) {
  x <- .aud_fila(r$resumen_tabla$columnas, "x")
  y <- .aud_fila(r$resumen_tabla$columnas, "y")
  expect_equal(c(x$n_validos, x$minimo, x$maximo, x$media), c(6, 10, 60, 35),
               info = info)
  expect_equal(c(y$n_validos, y$minimo, y$maximo, y$media), c(3, 1, 5, 3),
               info = info)
  expect_equal(
    c(y$n_aplicables, y$n_no_aplica, y$n_aplicabilidad_indeterminada),
    c(3, 2, 1), info = info
  )
  if (!is.null(r$perfil_muestra)) {
    mx <- .aud_fila(r$perfil_muestra$columnas, "x")
    my <- .aud_fila(r$perfil_muestra$columnas, "y")
    expect_equal(c(mx$minimo, mx$maximo, mx$media), c(10, 60, 35), info = info)
    expect_equal(c(my$minimo, my$maximo, my$media), c(1, 5, 3), info = info)
  }
  expect_identical(r$resumen_tabla$meta$politica_ausencia$aplicabilidad, "y",
                   info = info)
}

test_that("una regla que no da un valor logico no se aplica por ningun camino y se declara una vez", {
  for (motor in .aud_casos(c("sqlite", "duckdb"))) {
    con <- if (identical(motor, "sqlite")) .aud_sqlite() else .aud_duckdb()
    on.exit(.aud_desconectar(con), add = TRUE)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE lupa_aud (id INTEGER, g ",
      if (identical(motor, "sqlite")) "TEXT" else "VARCHAR",
      ", activo INTEGER, firme INTEGER NOT NULL, x DOUBLE, y DOUBLE)"
    ))
    DBI::dbExecute(con, paste("INSERT INTO lupa_aud VALUES", .aud_filas_regla_sql))
    vias <- .aud_vias(motor)
    for (via in .aud_casos(names(vias))) {
      for (regla in .aud_casos(list(~ activo, ~ (activo), ~ firme))) {
        info <- paste(motor, via, paste(deparse(regla[[2L]]), collapse = ""))
        r <- .aud_por_via(
          con, vias[[via]], aplicabilidad = list(x = regla, y = ~ g == "a")
        )
        .aud_esperar_regla_sin_aplicar(r, info)
        expect_identical(
          nrow(r$resumen_tabla$meta$politica_ausencia$sin_aplicar), 0L,
          info = info
        )
        filas <- .aud_filas_regla(r, "x")
        expect_identical(nrow(filas), 1L, info = info)
        expect_identical(filas$bloque, "perfil_muestra", info = info)
      }
      # Control: la misma columna comparada si es una condicion, y se aplica:
      # las filas 1, 3 y 5.
      r <- .aud_por_via(con, vias[[via]], aplicabilidad = list(x = ~ activo == 1))
      x <- .aud_fila(r$resumen_tabla$columnas, "x")
      expect_equal(c(x$n_validos, x$minimo, x$maximo), c(3, 10, 50),
                   info = paste(motor, via))
      expect_identical(nrow(.aud_filas_regla(r, "x")), 0L, info = paste(motor, via))
    }
  }
})

test_that("declarar solo una regla que no da un valor logico deja el resumen y el plan como sin declararla", {
  # Una columna `NOT NULL` con una regla cuenta sus validos en lugar de tomarlos
  # del catalogo. La regla que no se aplica no cambia como se mide nada: ni las
  # cifras, ni de donde sale cada una, ni las consultas, ni el plan.
  fecha <- as.POSIXct("2026-01-02 03:04:05", tz = "UTC")
  regla <- list(firme = ~ activo)
  for (motor in .aud_casos(c("sqlite", "duckdb"))) {
    con <- if (identical(motor, "sqlite")) .aud_sqlite() else .aud_duckdb()
    on.exit(.aud_desconectar(con), add = TRUE)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE lupa_aud (id INTEGER, g ",
      if (identical(motor, "sqlite")) "TEXT" else "VARCHAR",
      ", activo INTEGER, firme INTEGER NOT NULL, x DOUBLE, y DOUBLE)"
    ))
    DBI::dbExecute(con, paste("INSERT INTO lupa_aud VALUES", .aud_filas_regla_sql))
    vias <- .aud_vias(motor)
    for (via in .aud_casos(names(vias))) {
      info <- paste(motor, via)
      con_regla <- .aud_por_via(con, vias[[via]], aplicabilidad = regla,
                                instrumentar = FALSE, fecha = fecha)
      sin_regla <- .aud_por_via(con, vias[[via]], instrumentar = FALSE,
                                fecha = fecha)
      expect_identical(con_regla$resumen_tabla$columnas,
                       sin_regla$resumen_tabla$columnas, info = info)
      # De cada cifra, de donde sale: el estado, el motivo y la sentencia. (El
      # tamanio de lo que R recibio depende del orden de la muestra del motor.)
      procedencia <- c("columna", "metrica", "estado", "motivo", "sql")
      expect_identical(con_regla$resumen_tabla$sql[procedencia],
                       sin_regla$resumen_tabla$sql[procedencia], info = info)
      expect_identical(con_regla$resumen_tabla$meta$consultas$emitidas,
                       sin_regla$resumen_tabla$meta$consultas$emitidas,
                       info = info)
      expect_null(con_regla$resumen_tabla$meta$politica_ausencia)
      expect_identical(nrow(.aud_filas_regla(con_regla, "firme")), 1L, info = info)
    }
    # Con muestra finita el plan anuncia la cuenta de faltantes disfrazados de
    # cada columna numerica; la de una columna con regla sale de la muestra.
    plan <- function(...) {
      suppressWarnings(suppressMessages(as.data.frame(plan_perfilado_dbi(
        con, "lupa_aud", muestra = 3, ...
      ))))
    }
    expect_identical(plan(aplicabilidad = regla), plan(), info = motor)
    # Control: una regla que si se aplica cambia el resumen.
    aplicada <- .aud_por_via(con, vias$agregados,
                             aplicabilidad = list(firme = ~ activo == 1))
    expect_equal(.aud_fila(aplicada$resumen_tabla$columnas, "firme")$n_validos, 3,
                 info = motor)
  }
})

test_that("la regla que solo las filas muestran que no da un valor logico tampoco se aplica, y se declara una vez", {
  # Una columna sin tipo declarado en SQLite llega sin filas como logica y con
  # filas como entero: antes de leer no se sabe que `~ libre` no es una
  # condicion.
  con <- .aud_sqlite()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE lupa_aud (id INTEGER, g TEXT, libre, firme INTEGER NOT NULL,",
    "x DOUBLE, y DOUBLE)"
  ))
  DBI::dbExecute(con, paste("INSERT INTO lupa_aud VALUES", .aud_filas_regla_sql))
  expect_true(is.logical(DBI::dbGetQuery(
    con, "SELECT libre FROM lupa_aud WHERE 1 = 0"
  )$libre))
  expect_true(is.integer(DBI::dbGetQuery(con, "SELECT libre FROM lupa_aud")$libre))
  reglas <- list(x = ~ libre, y = ~ g == "a")
  vias <- .aud_vias("sqlite")
  for (via in .aud_casos(names(vias))) {
    r <- .aud_por_via(con, vias[[via]], aplicabilidad = reglas)
    .aud_esperar_regla_sin_aplicar(r, via)
    filas <- .aud_filas_regla(r, "x")
    expect_identical(nrow(filas), 1L, info = via)
    if (identical(via, "agregados")) {
      # Sin filas leidas nadie sabe que la regla no es una condicion: el resumen
      # declara que no la tradujo, y no afirma lo que haria una muestra que
      # esta corrida no tiene.
      expect_identical(filas$elemento, "x::aplicabilidad")
      expect_true(grepl("cuando lo hay", filas$motivo, fixed = TRUE,
                        useBytes = TRUE))
      expect_identical(
        r$resumen_tabla$meta$politica_ausencia$sin_aplicar$columna, "x"
      )
    } else {
      expect_identical(filas$bloque, "perfil_muestra", info = via)
      expect_identical(
        nrow(r$resumen_tabla$meta$politica_ausencia$sin_aplicar), 0L, info = via
      )
    }
    # Sola, la regla que nadie aplico no deja una fila que diga que la muestra
    # la aplica.
    sola <- .aud_por_via(con, vias[[via]], aplicabilidad = list(x = ~ libre))
    expect_identical(nrow(.aud_filas_regla(sola, "x")), 1L, info = via)
    x <- .aud_fila(sola$resumen_tabla$columnas, "x")
    expect_equal(c(x$n_validos, x$minimo, x$maximo, x$media), c(6, 10, 60, 35),
                 info = via)
  }
})

test_that("por bloques, una regla que se sabe tarde que no da un valor logico deja a su columna sin cifras", {
  # Las dos primeras filas de `libre` son `NULL`: el primer bloque de dos filas
  # la trae como logica, la regla da `NA` y esas filas quedan fuera de `x`. El
  # segundo bloque la trae como entero. Lo acumulado de `x` mezcla las dos
  # lecturas: no se publica.
  con <- .aud_sqlite()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE lupa_aud (id INTEGER, g TEXT, libre, x DOUBLE, y DOUBLE)")
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_aud VALUES (1, 'a', NULL, 10, 1), (2, 'b', NULL, 20, 2),",
    "(3, 'a', 1, 30, 3), (4, NULL, 0, 40, 4), (5, 'a', 1, 50, 5), (6, 'b', 0, 60, 6)"
  ))
  expect_true(is.logical(DBI::dbGetQuery(
    con, "SELECT libre FROM lupa_aud WHERE id <= 2"
  )$libre))
  reglas <- list(x = ~ libre, y = ~ g == "a")
  tarde <- .aud_dbi(con, "lupa_aud", aplicabilidad = reglas, bloque_filas = 2L)
  x <- .aud_fila(tarde$resumen_tabla$columnas, "x")
  expect_true(all(is.na(c(x$n_validos, x$minimo, x$maximo, x$media))))
  estados <- tarde$resumen_tabla$sql
  estados <- estados[estados$columna == "x" & estados$metrica == "n_validos", ]
  expect_identical(estados$estado, "no_disponible")
  expect_true(startsWith(
    estados$motivo, "aplicabilidad_no_logica_tras_bloques_leidos"
  ))
  # La otra columna sigue con su regla, y la muestra cuenta las seis filas.
  y <- .aud_fila(tarde$resumen_tabla$columnas, "y")
  expect_equal(c(y$n_validos, y$minimo, y$maximo, y$media), c(3, 1, 5, 3))
  mx <- .aud_fila(tarde$perfil_muestra$columnas, "x")
  expect_equal(c(mx$minimo, mx$maximo, mx$media), c(10, 60, 35))
  expect_identical(nrow(.aud_filas_regla(tarde, "x")), 1L)
  # Control: con un primer bloque que ya trae un valor, se sabe a tiempo.
  a_tiempo <- .aud_dbi(con, "lupa_aud", aplicabilidad = reglas, bloque_filas = 3L)
  .aud_esperar_regla_sin_aplicar(a_tiempo, "bloques de tres filas")
})

test_that("una columna que se mide con su propia consulta dice cuantos centinelas saco de sus magnitudes", {
  # Si el motor rechaza el lote de agregados por una columna, esa columna se
  # mide con una consulta propia. Ningun motor de los que se prueban rechaza
  # hoy un lote por un centinela declarado: se simula el rechazo de todo lote
  # que nombre a `lupa_rara`. A mano, sin los dos -9 quedan 0, -3, 4 y 10.
  for (motor in .aud_casos(c("sqlite", "duckdb"))) {
    con <- if (identical(motor, "sqlite")) .aud_sqlite() else .aud_duckdb()
    on.exit(.aud_desconectar(con), add = TRUE)
    DBI::dbExecute(con, "CREATE TABLE lupa_aud (id INTEGER, lupa_rara DOUBLE, y DOUBLE)")
    DBI::dbExecute(con, paste(
      "INSERT INTO lupa_aud VALUES (1, -9, 1), (2, -9, 2), (3, 0, -9), (4, -3, 4),",
      "(5, 4, 5), (6, 10, 6), (7, NULL, 7)"
    ))
    original <- lupa:::.consultar_dbi
    rechazadas <- 0L
    r <- testthat::with_mocked_bindings(
      .aud_dbi(con, "lupa_aud", sentinelas_numericos = -9,
               bloque_muestra = "solo_agregados"),
      .consultar_dbi = function(conexion, sql, ...) {
        if (grepl("lupa_l[0-9]+_c[0-9]+_minimo", sql, useBytes = TRUE) &&
            grepl("lupa_rara", sql, fixed = TRUE, useBytes = TRUE)) {
          rechazadas <<- rechazadas + 1L
          return(list(ok = FALSE, datos = NULL, motivo = "rechazo de prueba"))
        }
        original(conexion, sql, ...)
      },
      .package = "lupa"
    )
    expect_gt(rechazadas, 0L)
    fila <- .aud_fila(r$resumen_tabla$columnas, "lupa_rara")
    expect_equal(c(fila$minimo, fila$maximo, fila$media), c(-3, 10, 2.75),
                 info = motor)
    expect_equal(fila$n_valores_excluidos_resumen, 2, info = motor)
    # La otra columna sale del lote, con su centinela afuera: 1, 2, 4, 5, 6, 7.
    otra <- .aud_fila(r$resumen_tabla$columnas, "y")
    expect_equal(c(otra$minimo, otra$n_valores_excluidos_resumen), c(1, 1),
                 info = motor)
  }
})

test_that("la lectura por bloques saca los centinelas declarados de las magnitudes", {
  datos <- .aud_datos()
  memoria <- .aud_memoria(datos, sentinelas_numericos = -9)
  con <- .aud_sqlite()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_aud", datos)
  r <- .aud_dbi(con, "lupa_aud", sentinelas_numericos = -9, bloque_filas = 2L)
  fila <- .aud_fila(r$resumen_tabla$columnas, "x")
  expect_equal(fila$minimo, -3)
  expect_equal(fila$maximo, 10)
  expect_equal(fila$media, 2.75)
  expect_equal(fila$mediana, 2)
  expect_equal(fila$n_ceros, 1)
  expect_equal(fila$n_negativos, 1)
  expect_equal(fila$n_validos, 6)
  expect_equal(fila$n_distintos, 5)
  expect_equal(fila$frecuencia_moda, 2)
  expect_equal(fila$n_valores_excluidos_resumen, 2)
  .aud_esperar_memoria(r, memoria, "x", c(
    .aud_magnitudes, "n_faltantes", "n_distintos",
    "n_valores_excluidos_resumen"
  ), "bloques")
  expect_identical(.aud_degradadas(r), character())
  # Sin declararlo, los bloques cuentan el -9 como siempre.
  sin <- .aud_dbi(con, "lupa_aud", bloque_filas = 2L)
  expect_equal(.aud_fila(sin$resumen_tabla$columnas, "x")$minimo, -9)
  expect_null(sin$resumen_tabla$meta$politica_ausencia)
})

test_that("por la muestra del motor los validos siguen al universo declarado y el resumen no se declara sin politica", {
  datos <- data.frame(id = 1:5, x = c(1, NA, 3, NA, 3), y = c(-9, 1, 2, 3, -9))
  con <- .aud_sqlite()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_aud", datos)
  r <- .aud_dbi(con, "lupa_aud", columnas_opcionales = "x",
                sentinelas_numericos = -9, universo = "muestra_motor",
                muestra_motor = 5L)
  memoria <- .aud_memoria(datos, columnas_opcionales = "x",
                          sentinelas_numericos = -9)
  x <- .aud_fila(r$resumen_tabla$columnas, "x")
  # Tres celdas con valor: tres validos, no los cinco de la tabla.
  expect_equal(x$n_validos, 3)
  expect_equal(x$n_aplicables, 3)
  expect_equal(x$n_faltantes, 0)
  expect_equal(x$n_distintos, 2)
  y <- .aud_fila(r$resumen_tabla$columnas, "y")
  expect_equal(y$minimo, 1)
  expect_equal(y$n_valores_excluidos_resumen, 2)
  .aud_esperar_memoria(r, memoria, "y", .aud_magnitudes, "muestra_motor")
  # Ese resumen sale de las filas leidas y ya aplica la politica: no hay nada
  # que declarar.
  expect_identical(.aud_degradadas(r), character())
  expect_identical(nrow(r$resumen_tabla$meta$politica_ausencia$sin_aplicar), 0L)
})

test_that("un decimal por la muestra del motor publica media, mediana y desvio sin los centinelas declarados", {
  con <- .aud_duckdb()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE lupa_aud (id INTEGER, m DECIMAL(12,2))")
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_aud VALUES (1, -9), (2, 0.3), (3, 1.25), (4, 2.5),",
    "(5, -9)"
  ))
  r <- .aud_dbi(con, "lupa_aud", sentinelas_numericos = c(-9, 0.3),
                universo = "muestra_motor", muestra_motor = 5L)
  fila <- .aud_fila(r$resumen_tabla$columnas, "m")
  # A mano, sin -9 ni 0,3 quedan 1,25 y 2,5.
  expect_equal(fila$minimo, 1.25)
  expect_equal(fila$maximo, 2.5)
  expect_equal(fila$media, 1.875)
  expect_equal(fila$mediana, 1.875)
  expect_equal(fila$desvio, stats::sd(c(1.25, 2.5)))
  muestra <- .aud_fila(r$perfil_muestra$columnas, "m")
  expect_equal(fila$media, muestra$media)
  expect_equal(fila$mediana, muestra$mediana)
})

test_that("en un BIGINT de SQLite el centinela 2^53 no cuenta a su vecino", {
  skip_if_not_installed("bit64")
  con <- .aud_sqlite()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE lupa_aud (id INTEGER, b BIGINT)")
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_aud VALUES (1, 9007199254740992),",
    "(2, 9007199254740993), (3, 9007199254740993), (4, 9007199254740993),",
    "(5, 1), (6, 2), (7, 3), (8, 4)"
  ))
  # 2^53 esta una vez; 2^53 + 1, tres. Pasada a doble, la columna los iguala.
  r <- .aud_dbi(con, "lupa_aud", sentinelas_numericos = 2^53, muestra = 2L,
                orden_muestra = "id")
  fila <- .aud_fila(r$resumen_tabla$columnas, "b")
  expect_equal(as.numeric(fila$n_faltantes_disfrazados), 1)
  estado <- r$resumen_tabla$sql
  estado <- estado$estado[estado$columna == "b" &
                            estado$metrica == "n_faltantes_disfrazados"]
  expect_identical(unique(as.character(estado)), "calculado")
})

test_that("en un FLOAT de DuckDB un centinela con fraccion se reconoce en el resumen y en la muestra", {
  con <- .aud_duckdb()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE lupa_aud (id INTEGER, f FLOAT)")
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_aud VALUES (1, -9.9), (2, -9.9), (3, -9.9), (4, 1.5),",
    "(5, 2.5)"
  ))
  # La premisa: la celda guarda el numero de 4 bytes, que no es -9.9.
  guardado <- DBI::dbGetQuery(con, "SELECT f FROM lupa_aud WHERE id = 1")$f
  expect_false(isTRUE(guardado == -9.9))
  for (camino in .aud_casos(c("consultas", "muestra_motor"))) {
    r <- if (camino == "consultas") {
      .aud_dbi(con, "lupa_aud", sentinelas_numericos = -9.9)
    } else {
      .aud_dbi(con, "lupa_aud", sentinelas_numericos = -9.9,
               universo = "muestra_motor", muestra_motor = 5L)
    }
    fila <- .aud_fila(r$resumen_tabla$columnas, "f")
    expect_equal(fila$minimo, 1.5, info = camino)
    expect_equal(fila$media, 2, info = camino)
    expect_equal(fila$n_negativos, 0, info = camino)
    expect_equal(fila$n_valores_excluidos_resumen, 3, info = camino)
    muestra <- .aud_fila(r$perfil_muestra$columnas, "f")
    expect_equal(muestra$minimo, 1.5, info = camino)
    expect_equal(muestra$n_faltantes_disfrazados, 3, info = camino)
  }
  # Con una muestra que no cubre la tabla, la cuenta la hace el motor.
  r <- .aud_dbi(con, "lupa_aud", sentinelas_numericos = -9.9, muestra = 2L,
                orden_muestra = "id")
  expect_equal(as.numeric(
    .aud_fila(r$resumen_tabla$columnas, "f")$n_faltantes_disfrazados
  ), 3)
  # Control: otro numero de 4 bytes no es ese centinela.
  otro <- .aud_dbi(con, "lupa_aud", sentinelas_numericos = -9.9000011)
  expect_lt(.aud_fila(otro$resumen_tabla$columnas, "f")$minimo, -9.89)
  expect_equal(
    .aud_fila(otro$resumen_tabla$columnas, "f")$n_valores_excluidos_resumen, 0
  )
})

test_that("una declaracion que perfilar() rechaza se rechaza igual, tambien sin muestra", {
  datos <- data.frame(g = c("a", "a", "b", NA), x = c(1, NA, 3, 4),
                      stringsAsFactors = FALSE)
  con <- .aud_sqlite()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_aud", datos)
  declaraciones <- list(
    list(columnas_opcionales = "edad"),
    list(aplicabilidad = list(edad = ~ g == "a")),
    list(aplicabilidad = list(x = ~ g == "a"), columnas_opcionales = "x"),
    list(aplicabilidad = list(x = ~ edad == "a")),
    list(aplicabilidad = list(x = ~ mean(x) > 1))
  )
  for (declaracion in .aud_casos(declaraciones)) {
    info <- paste(names(declaracion), collapse = "+")
    de_memoria <- tryCatch(
      do.call(.aud_memoria, c(list(datos), declaracion)),
      error = function(e) e
    )
    expect_s3_class(de_memoria, "error")
    for (extra in .aud_casos(list(
      list(), list(bloque_muestra = "solo_agregados"), list(bloque_filas = 2L),
      list(universo = "muestra_motor", muestra_motor = 4L)
    ))) {
      del_motor <- tryCatch(
        do.call(.aud_dbi, c(list(con, "lupa_aud"), declaracion, extra)),
        error = function(e) e
      )
      expect_s3_class(del_motor, "lupa_error_argumento_dbi")
      expect_identical(conditionMessage(del_motor),
                       conditionMessage(de_memoria),
                       info = paste(info, paste(names(extra), collapse = "+")))
    }
  }
  # Y no rechaza lo que `perfilar()` acepta: el argumento de una funcion escrita
  # en la regla, el paquete de `pkg::f()`, el campo de `x$campo` y lo que una
  # funcion evalua a su manera no son nombres que falten. A mano, sobre
  # `x = 1, NA, 3, 4`: la primera regla se cumple en dos filas, la segunda en
  # tres y las otras en dos.
  limites <- list(tope = 2)
  # Una funcion que no admite una columna sin filas: la regla que la usa no se
  # puede evaluar antes de leer, y no por eso nombra algo que falte.
  exigente <- function(v) {
    stopifnot(length(v) > 0L)
    v > 2
  }
  aceptadas <- list(
    list(regla = ~ sapply(x, function(v) isTRUE(v > 2)), aplicables = 2),
    list(regla = ~ base::is.na(x) | x > 2, aplicables = 3),
    list(regla = ~ x > limites$tope, aplicables = 2),
    list(regla = ~ with(list(tope = 2), x > tope), aplicables = 2),
    list(regla = ~ exigente(x) & sapply(x, function(v) !is.na(v)),
         aplicables = 2)
  )
  for (aceptada in .aud_casos(aceptadas)) {
    regla <- list(g = aceptada$regla)
    info <- paste(deparse(aceptada$regla[[2L]]), collapse = "")
    de_memoria <- .aud_memoria(datos, aplicabilidad = regla)
    expect_equal(.aud_fila(de_memoria$columnas, "g")$n_aplicables,
                 aceptada$aplicables, info = info)
    vias <- .aud_vias("sqlite", bloque_filas = 2L)
    for (via in .aud_casos(names(vias))) {
      del_motor <- tryCatch(
        .aud_por_via(con, vias[[via]], aplicabilidad = regla),
        error = function(e) e
      )
      expect_false(inherits(del_motor, "error"), info = paste(info, via))
      if (inherits(del_motor, "error")) next
      # Donde la regla se evalua en R -la muestra, los bloques y la muestra del
      # motor- cuenta lo mismo que la memoria.
      evaluada <- if (via %in% c("bloques", "muestra_motor")) {
        del_motor$resumen_tabla$columnas
      } else if (identical(via, "con_muestra")) {
        del_motor$perfil_muestra$columnas
      }
      if (!is.null(evaluada)) {
        expect_equal(.aud_fila(evaluada, "g")$n_aplicables, aceptada$aplicables,
                     info = paste(info, via))
      }
    }
  }
  # Los nombres con espacio, palabra reservada o comilla son nombres de la
  # tabla: se aceptan.
  DBI::dbExecute(con, paste(
    "CREATE TABLE lupa_nombres (\"mi columna\" DOUBLE, \"select\" DOUBLE)"
  ))
  DBI::dbExecute(con, "INSERT INTO lupa_nombres VALUES (-9, -9), (2, 2), (3, 3)")
  r <- .aud_dbi(con, "lupa_nombres",
                sentinelas_numericos = list(`mi columna` = -9, `select` = -9),
                columnas_opcionales = "mi columna")
  expect_equal(.aud_fila(r$resumen_tabla$columnas, "mi columna")$minimo, 2)
  expect_equal(.aud_fila(r$resumen_tabla$columnas, "select")$minimo, 2)
})

test_that("un infinito no entra en las magnitudes de una columna con centinelas declarados", {
  con <- .aud_sqlite()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE lupa_aud (x REAL)")
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_aud VALUES (-9), (1), (2), (3), (4), (9e999), (-9e999)"
  ))
  datos <- DBI::dbReadTable(con, "lupa_aud")
  memoria <- .aud_memoria(datos, sentinelas_numericos = -9)
  r <- .aud_dbi(con, "lupa_aud", sentinelas_numericos = -9)
  fila <- .aud_fila(r$resumen_tabla$columnas, "x")
  expect_equal(fila$minimo, 1)
  expect_equal(fila$maximo, 4)
  expect_equal(fila$mediana, 2.5)
  expect_equal(fila$media, 2.5)
  # El centinela y los dos infinitos.
  expect_equal(fila$n_valores_excluidos_resumen, 3)
  .aud_esperar_memoria(r, memoria, "x", c(
    .aud_magnitudes, "n_valores_excluidos_resumen"
  ), "infinitos")
})

test_that("las consultas agregadas son las mismas con politica y sin ella", {
  datos <- data.frame(
    id = 1:40, x = c(rep(-9, 4), seq(0.5, 18, by = 0.5)),
    w = c(rep(NA, 10), 1:30)
  )
  con <- .aud_sqlite()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_aud", datos)
  sin <- .aud_dbi(con, "lupa_aud")
  con_politica <- .aud_dbi(con, "lupa_aud", sentinelas_numericos = -9,
                           columnas_opcionales = "w")
  emitidas <- function(r) r$resumen_tabla$meta$consultas$emitidas
  expect_identical(emitidas(con_politica), emitidas(sin))
  expect_identical(
    con_politica$resumen_tabla$meta$consultas$conciliacion$estado,
    sin$resumen_tabla$meta$consultas$conciliacion$estado
  )
  expect_equal(.aud_fila(con_politica$resumen_tabla$columnas, "x")$minimo, 0.5)
})

# `fixtures/perfil-dbi-resumen-sin-politica.rds` es un objeto de `perfilar_dbi()`
# guardado por la version 0.1.2.9000, que calculaba el resumen sin la politica y
# lo decia con una fila `degradado` para toda la tabla. Se hizo con la tabla y
# la llamada de `.aud_entrega()`, con fecha 2026-01-01.
.aud_entrega <- function(con, fecha) {
  DBI::dbWriteTable(con, "lupa_entrega", data.frame(
    id = 1:60, v = c(rep(-999, 6), seq(10.5, 63.5, by = 1)),
    w = rep(c(1, 2, 3), 20)
  ), overwrite = TRUE)
  .aud_dbi(con, "lupa_entrega", sentinelas_numericos = -999, muestra = 20L,
           orden_muestra = "id", fecha = as.POSIXct(fecha, tz = "UTC"),
           instrumentar = FALSE)
}

test_that("un resumen calculado sin la politica no se compara con uno calculado con ella", {
  anterior <- readRDS(testthat::test_path(
    "fixtures", "perfil-dbi-resumen-sin-politica.rds"
  ))
  # La premisa: el objeto guardado cuenta los -999 y dice que no aplico la
  # politica.
  cobertura <- anterior$resumen_tabla$cobertura
  expect_true(any(
    cobertura$bloque == "resumen_tabla" & cobertura$estado == "degradado" &
      cobertura$elemento == "sentinelas_numericos"
  ))
  expect_equal(.aud_fila(anterior$resumen_tabla$columnas, "v")$minimo, -999)
  con <- .aud_sqlite()
  on.exit(.aud_desconectar(con), add = TRUE)
  nueva <- .aud_entrega(con, "2026-02-01")
  expect_equal(.aud_fila(nueva$resumen_tabla$columnas, "v")$minimo, 10.5)

  # La tabla es la misma: no hay deriva del rango, hay una cifra que no se
  # puede comparar, y dice por que.
  deriva <- as.data.frame(suppressWarnings(suppressMessages(
    comparar_perfiles(anterior, nueva)
  )))
  rango <- deriva[deriva$aspecto == "rango", , drop = FALSE]
  expect_identical(rango$cambio, "no_comparable")
  expect_identical(rango$evidencia,
                   "v: politica_ausencia:sin_aplicar_vs_aplicada")

  equivalencia <- as.data.frame(suppressWarnings(suppressMessages(
    comparar_equivalencia(anterior, nueva, tolerancia = 1e-9)
  )))
  de <- function(columna, campos) {
    equivalencia[equivalencia$columna == columna &
                   equivalencia$campo %in% campos, , drop = FALSE]
  }
  magnitudes <- de("v", .aud_magnitudes)
  expect_identical(nrow(magnitudes), length(.aud_magnitudes))
  expect_identical(unique(as.character(magnitudes$veredicto)),
                   "no_comparable")
  expect_identical(unique(as.character(magnitudes$motivo)),
                   "politica_ausencia:sin_aplicar_vs_aplicada")
  # Lo que la politica no toca se compara como siempre: los conteos de `v` y
  # todo `w`, que no tiene centinelas.
  veredictos <- function(columna, campos) {
    unique(as.character(de(columna, campos)$veredicto))
  }
  expect_identical(veredictos("v", c("n_validos", "n_distintos")), "identico")
  expect_identical(veredictos("w", .aud_magnitudes), "identico")

  # Control: dos corridas que la aplican se comparan, y son iguales.
  otra <- .aud_entrega(con, "2026-03-01")
  deriva <- as.data.frame(suppressWarnings(suppressMessages(
    comparar_perfiles(nueva, otra)
  )))
  expect_false(any(deriva$aspecto == "rango"))
  equivalencia <- as.data.frame(suppressWarnings(suppressMessages(
    comparar_equivalencia(nueva, otra, tolerancia = 1e-9)
  )))
  expect_identical(veredictos("v", .aud_magnitudes), "identico")
})

.aud_coleccion <- function(con, ...) {
  avisos <- character()
  x <- withCallingHandlers(
    perfilar_coleccion(
      coleccion(con, tablas = c("lupa_a", "lupa_b", "lupa_c")),
      conservar_perfiles = TRUE, proteger_datos_personales = FALSE, ...
    ),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage")
  )
  list(x = x, avisos = avisos)
}

test_that("en una coleccion, una declaracion sobre una columna que no todas las tablas tienen no las hace fallar", {
  con <- .aud_sqlite()
  on.exit(.aud_desconectar(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_a", data.frame(
    id = 1:6, tiene = c("s", "s", "n", "n", "s", NA),
    nacimiento = c(1, NA, 3, NA, 5, 6), stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "lupa_b", data.frame(id = 1:6, otra = c(1:5, NA)))
  DBI::dbWriteTable(con, "lupa_c", data.frame(id = 1:4, nacimiento = 1:4))
  de_tabla <- function(x, tabla, columna) {
    perfil <- x$perfiles[[tabla]]
    list(
      resumen = .aud_fila(perfil$resumen_tabla$columnas, columna),
      muestra = if (is.null(perfil$perfil_muestra)) NULL else {
        .aud_fila(perfil$perfil_muestra$columnas, columna)
      }
    )
  }
  esperar_completas <- function(r, info) {
    # Las tres tablas se perfilaron, con resumen y con muestra.
    expect_identical(nrow(r$x$resumen_coleccion), 3L, info = info)
    for (tabla in .aud_casos(c("lupa_a", "lupa_b", "lupa_c"))) {
      expect_false(is.null(r$x$perfiles[[tabla]]$perfil_muestra),
                   info = paste(info, tabla))
    }
    cobertura <- r$x$cobertura_coleccion
    expect_false(any(cobertura$alcance %in% c("tabla", "muestra_no_disponible")),
                 info = info)
    # La que no tiene la columna lo dice, y solo ella.
    expect_identical(
      cobertura$tabla[cobertura$alcance == "declaracion_no_aplicada"], "lupa_b",
      info = info
    )
    expect_identical(sum(grepl("nacimiento", r$avisos, fixed = TRUE,
                               useBytes = TRUE)), 1L, info = info)
  }

  opcional <- .aud_coleccion(con, columnas_opcionales = "nacimiento")
  esperar_completas(opcional, "columnas_opcionales")
  # Donde la columna esta, la declaracion se aplica: dos `NULL` que no son
  # faltantes, en el resumen y en la muestra.
  en_a <- de_tabla(opcional$x, "lupa_a", "nacimiento")
  expect_equal(en_a$resumen$n_faltantes, 0)
  expect_equal(en_a$muestra$n_faltantes, 0)
  # Y la tabla que no la tiene se midio como sin declaracion.
  en_b <- de_tabla(opcional$x, "lupa_b", "otra")
  expect_equal(en_b$resumen$n_faltantes, 1)
  expect_equal(en_b$muestra$n_faltantes, 1)

  # Una regla: vale donde estan su columna y las que nombra. `lupa_c` tiene
  # `nacimiento` y no tiene `tiene`.
  regla <- list(nacimiento = stats::as.formula("~ tiene == 's'", env = baseenv()))
  con_regla <- .aud_coleccion(con, aplicabilidad = regla)
  expect_identical(nrow(con_regla$x$resumen_coleccion), 3L)
  cobertura <- con_regla$x$cobertura_coleccion
  expect_false(any(cobertura$alcance %in% c("tabla", "muestra_no_disponible")))
  expect_setequal(
    cobertura$tabla[cobertura$alcance == "declaracion_no_aplicada"],
    c("lupa_b", "lupa_c")
  )
  expect_equal(de_tabla(con_regla$x, "lupa_a", "nacimiento")$resumen$n_aplicables,
               3)

  # Un nombre que no esta en ninguna tabla es una errata: se rechaza antes de
  # leer.
  for (declaracion in .aud_casos(list(
    list(columnas_opcionales = "nacimento"),
    list(aplicabilidad = list(nacimento = ~ tiene == "s"))
  ))) {
    error <- tryCatch(do.call(.aud_coleccion, c(list(con), declaracion)),
                      error = function(e) e)
    expect_s3_class(error, "lupa_error_argumento_dbi")
    expect_true(grepl("ninguna", conditionMessage(error), fixed = TRUE,
                      useBytes = TRUE))
    expect_true(grepl("nacimento", conditionMessage(error), fixed = TRUE,
                      useBytes = TRUE))
  }
  # Y una regla que nombra algo que ninguna tabla tiene, con el error de
  # `perfilar()`.
  error <- tryCatch(
    .aud_coleccion(con, aplicabilidad = list(nacimiento = ~ tine == "s")),
    error = function(e) e
  )
  expect_s3_class(error, "lupa_error_argumento_dbi")
  expect_true(grepl("`nacimiento`", conditionMessage(error), fixed = TRUE,
                    useBytes = TRUE))
  # Fuera de la coleccion, una sola tabla sigue estricta.
  expect_error(
    .aud_dbi(con, "lupa_b", columnas_opcionales = "nacimiento"),
    class = "lupa_error_argumento_dbi"
  )
})
