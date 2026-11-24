# La politica de ausencia declarada en los motores de servidor: lo que el
# resumen de tabla completa no puede excluir igual que `perfilar()` lo declara,
# y lo que puede lo excluye. Cada prueba se saltea sola sin su motor.
testthat::skip_on_cran()

.adm_dbi <- function(con, tabla, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, ...
  )))
}

.adm_fila <- function(columnas, columna) {
  columnas[columnas$columna == columna, , drop = FALSE]
}

.adm_borrar <- function(con, tabla) {
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
}

.adm_divergencias <- function(r, columna) {
  cobertura <- r$resumen_tabla$cobertura
  cobertura$elemento[
    cobertura$bloque == "corroboracion" & cobertura$estado == "divergencia" &
      grepl(paste0("^", columna, "::"), cobertura$elemento, useBytes = TRUE)
  ]
}

test_that("SQL Server: un centinela con fraccion sobre una columna real sale del resumen", {
  con <- .dlot_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .adm_borrar(con, "lupa_aus_real")
  on.exit(.adm_borrar(con, "lupa_aus_real"), add = TRUE, after = FALSE)
  DBI::dbExecute(con, "CREATE TABLE lupa_aus_real (id INT PRIMARY KEY, x REAL)")
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_aus_real VALUES (1, -9.9), (2, -9.9), (3, -9.9),",
    "(4, 1.5), (5, 2.5), (6, NULL)"
  ))
  # La premisa: la columna guarda el numero de 4 bytes, que no es -9.9.
  guardado <- DBI::dbGetQuery(con, "SELECT x FROM lupa_aus_real WHERE id = 1")$x
  expect_false(isTRUE(guardado == -9.9))
  caminos <- list(consultas = list(), bloques = list(bloque_filas = 2L))
  expect_identical(length(caminos), 2L)
  for (camino in names(caminos)) {
    r <- do.call(.adm_dbi, c(
      list(con, "lupa_aus_real", sentinelas_numericos = -9.9), caminos[[camino]]
    ))
    fila <- .adm_fila(r$resumen_tabla$columnas, "x")
    # A mano: sin los tres -9.9 quedan 1,5 y 2,5.
    expect_equal(fila$minimo, 1.5, info = camino)
    expect_equal(fila$media, 2, info = camino)
    expect_equal(fila$mediana, 2, info = camino)
    expect_equal(fila$n_negativos, 0, info = camino)
    expect_equal(fila$n_valores_excluidos_resumen, 3, info = camino)
    # El resumen y la muestra miden lo mismo: nada que declarar.
    expect_identical(.adm_divergencias(r, "x"), character(), info = camino)
  }
})

test_that("PostgreSQL: una columna money dice cuantos centinelas saco de sus magnitudes", {
  con <- .dlot_pg()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .adm_borrar(con, "lupa_aus_moneda")
  on.exit(.adm_borrar(con, "lupa_aus_moneda"), add = TRUE, after = FALSE)
  DBI::dbExecute(con, "CREATE TABLE lupa_aus_moneda (id INTEGER PRIMARY KEY, x MONEY)")
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_aus_moneda VALUES (1, -9), (2, 1.25), (3, 2.5), (4, -9),",
    "(5, NULL)"
  ))
  r <- .adm_dbi(con, "lupa_aus_moneda", sentinelas_numericos = -9)
  fila <- .adm_fila(r$resumen_tabla$columnas, "x")
  # A mano: sin los dos -9 quedan 1,25 y 2,5.
  expect_equal(fila$minimo, 1.25)
  expect_equal(fila$maximo, 2.5)
  expect_equal(fila$n_validos, 4)
  expect_equal(fila$n_valores_excluidos_resumen, 2)
  muestra <- .adm_fila(r$perfil_muestra$columnas, "x")
  expect_equal(muestra$n_valores_excluidos_resumen, 2)
})

test_that("PostgreSQL anterior a la 12: con politica, la lectura exacta de un doble se calcula una vez por fila", {
  con <- .nf_pg(55493L)
  skip_if(is.null(con), "PostgreSQL 9.3 no disponible.")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .adm_borrar(con, "lupa_aus_doble")
  on.exit(.adm_borrar(con, "lupa_aus_doble"), add = TRUE, after = FALSE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE lupa_aus_doble (id INTEGER PRIMARY KEY, v DOUBLE PRECISION)"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_aus_doble VALUES (1, -999), (2, 1.5), (3, 2.5), (4, 4),",
    "(5, -999)"
  ))
  consulta_de_la_media <- function(...) {
    r <- .adm_dbi(con, "lupa_aus_doble", bloque_muestra = "solo_agregados", ...)
    sql <- r$resumen_tabla$sql
    list(
      media = .adm_fila(r$resumen_tabla$columnas, "v")$media,
      sql = unique(sql$sql[sql$columna == "v" & sql$metrica == "media"])
    )
  }
  # El costo que el planificador le estima a la consulta entera: la primera
  # linea de su plan, `(cost=inicio..total ...)`.
  costo_de <- function(consulta) {
    plan <- DBI::dbGetQuery(con, paste("EXPLAIN", consulta))[[1L]]
    as.numeric(sub("^.*cost=[0-9.]+\\.\\.([0-9.]+) .*$", "\\1", plan[[1L]]))
  }
  con_politica <- consulta_de_la_media(sentinelas_numericos = -999)
  expect_identical(length(con_politica$sql), 1L)
  # La premisa: en este servidor el doble se lee por una subconsulta que lo
  # reconstruye de sus bytes, que es lo caro.
  expect_true(grepl("float8send", con_politica$sql, fixed = TRUE,
                    useBytes = TRUE))
  # A mano: sin los dos -999 quedan 1,5, 2,5 y 4.
  expect_equal(con_politica$media, (1.5 + 2.5 + 4) / 3)
  sin_politica <- consulta_de_la_media()
  expect_identical(length(sin_politica$sql), 1L)
  expect_equal(sin_politica$media, (-999 * 2 + 1.5 + 2.5 + 4) / 5)
  # Con politica cada agregado nombra la columna dos veces. Si la subconsulta
  # se fundiera con los agregados, la reconstruccion se calcularia en cada
  # mencion y la consulta costaria mas que sin politica (medido: 2,9 veces).
  # Como se calcula una vez por fila, cuesta menos.
  costos <- c(costo_de(con_politica$sql), costo_de(sin_politica$sql))
  expect_true(all(is.finite(costos)))
  expect_lt(costos[[1L]], costos[[2L]])
})
