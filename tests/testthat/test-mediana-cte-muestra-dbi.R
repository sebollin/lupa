# La mediana de `muestra_motor` con `bloque_muestra = "solo_agregados"` sale de
# una sola sentencia sobre la muestra: la consolidada si la columna puede
# usarla y, si no, la CTE de ventanas. Quedaba `no_disponible`, con el motivo
# de una prueba de capacidad que no habia corrido, en tres casos:
#
#   - un `float8` de PostgreSQL, que no usa `PERCENTILE_CONT` (`numeric_exact`)
#     y con la consolidada disponible nunca probaba la CTE;
#   - una columna con extremos en cualquier motor con consolidada, por lo mismo;
#   - todo MySQL, cuya prueba leia de `VALUES (...) AS t(v)`, que rechaza por
#     sintaxis (ver `test-local-mysql.R`).
#
# Y la forma elegia las centrales con `(n + 1) / 2`, que en DuckDB, MySQL y
# MariaDB es division decimal: la mediana de 1, 2, 3 y 4 daba 3.

testthat::skip_on_cran()

skip_if_not_installed("DBI")

test_that("la CTE de ventanas elige las dos centrales sin division entera", {
  skip_if_not_installed("duckdb")
  conexion <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conexion, shutdown = TRUE), add = TRUE)
  cte <- .candidatos_mediana_cte_ventana_dbi(conexion)[[1L]]

  prueba <- DBI::dbGetQuery(conexion, cte$sonda("mediana"))
  expect_equal(as.numeric(prueba$mediana), 2.5)
  resolucion <- .sondar_mediana_cte_ventana_dbi(conexion, NULL)
  expect_true(resolucion$disponible)

  # Cantidades pares e impares, con centrales que redondean al promediarse
  # (0,1 y 0,2) y con extremos que desbordan la suma y piden la escalada.
  casos <- list(
    7, c(1, 2), c(3, 1, 2), c(4, 1, 3, 2), c(5, 1, 4, 2, 3),
    c(6, 1, 5, 2, 4, 3), c(-1, 0.1, 0.2, 5), c(NA, 2, NA, 1, 4)
  )
  for (valores in casos) {
    fuente <- paste0(
      "(SELECT * FROM (VALUES ",
      paste0(
        "(CAST(", ifelse(is.na(valores), "NULL", sprintf("%.17g", valores)),
        " AS DOUBLE))", collapse = ", "
      ),
      ") AS mcm_v(v)) AS mcm_fuente"
    )
    for (escalada in c(FALSE, TRUE)) {
      valor <- DBI::dbGetQuery(
        conexion, cte$construir("v", fuente, "mediana", escalada = escalada)
      )$mediana
      expect_identical(
        as.numeric(valor), stats::median(valores, na.rm = TRUE),
        info = paste(paste(valores, collapse = ","), escalada)
      )
    }
  }
  # El extremo se arma con aritmetica exacta y viaja escrito con 17 cifras: el
  # lector de R de macOS ARM64 no tiene `long double` mas ancho que `double` y
  # lee `1.7e308` cuatro ulps por debajo del double mas cercano, que es el que
  # devuelve el motor.
  grande <- 1.5 * 2^1023
  literal <- sprintf("%.17g", grande)
  extremos <- paste0(
    "(SELECT * FROM (VALUES (-", literal, "), (", literal, "), (", literal,
    "), (", literal, ")) AS mcm_v(v)) AS mcm_fuente"
  )
  expect_identical(
    as.numeric(DBI::dbGetQuery(
      conexion, cte$construir("v", extremos, "mediana", escalada = TRUE)
    )$mediana),
    grande
  )
})

test_that("una columna con extremos tiene mediana en la muestra de DuckDB", {
  skip_if_not_installed("duckdb")
  conexion <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conexion, shutdown = TRUE), add = TRUE)

  # La muestra cubre la tabla: valores contra `median()`.
  datos <- .mcm_datos(1500L)
  .mcm_escribir(conexion, "mcm_cubre", datos)
  medianas <- .mcm_medianas(conexion, "mcm_cubre", 2000L)
  .mcm_comprobar(
    medianas, c("x", "y", "z", "w"), metodo_cte = "w", datos = datos
  )
  expect_identical(medianas$solo_agregados["x", "metodo"], "PERCENTILE_CONT")

  # Una muestra de verdad: el par de la mediana de una muestra.
  .mcm_escribir(conexion, "mcm_muestra", .mcm_datos(20000L))
  medianas <- .mcm_medianas(conexion, "mcm_muestra", 2000L)
  .mcm_comprobar(medianas, c("x", "y", "z", "w"), metodo_cte = "w")
  expect_identical(
    unname(unlist(medianas$solo_agregados["w", c("estado", "error_esperado")])),
    c("estimado", "no_estimable")
  )
})

test_that("un float8 de PostgreSQL tiene mediana en la muestra", {
  conexion <- .mcm_con_pg()
  tablas <- c("mcm_pg_cubre", "mcm_pg_muestra")
  on.exit({
    for (tabla in tablas) {
      try(DBI::dbRemoveTable(conexion, tabla), silent = TRUE)
    }
    DBI::dbDisconnect(conexion)
  }, add = TRUE)

  datos <- .mcm_datos(1500L)
  .mcm_escribir(conexion, tablas[[1L]], datos)
  medianas <- .mcm_medianas(conexion, tablas[[1L]], 2000L)
  # El entero conserva la consolidada; los `float8` y el extremo, la CTE.
  .mcm_comprobar(medianas, c("x", "y", "z", "w"),
                 metodo_cte = c("x", "z", "w"), datos = datos)
  expect_identical(medianas$solo_agregados["y", "metodo"], "PERCENTILE_CONT")

  .mcm_escribir(conexion, tablas[[2L]], .mcm_datos(20000L))
  medianas <- .mcm_medianas(conexion, tablas[[2L]], 2000L)
  .mcm_comprobar(medianas, c("x", "y", "z", "w"),
                 metodo_cte = c("x", "z", "w"))
  expect_identical(
    medianas$solo_agregados[c("x", "z", "w"), "error_esperado"],
    rep("no_estimable", 3L)
  )
})

test_that("PostgreSQL escala la CTE aunque no se conozca el conteo", {
  conexion <- .mcm_con_pg()
  on.exit({
    try(DBI::dbRemoveTable(conexion, "mcm_pg_extremos"), silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)

  datos <- .mcm_datos_extremos(1500L)
  .mcm_escribir(conexion, "mcm_pg_extremos", datos)
  medianas <- .mcm_medianas(
    conexion, "mcm_pg_extremos", 2000L, metricas = c("basicos", "mediana")
  )
  .mcm_comprobar(medianas, c("x", "w", "g"), metodo_cte = c("x", "w", "g"),
                 datos = datos)
})

test_that("una columna con extremos tiene mediana en la muestra de MSSQL", {
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
  conexion <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS", Server = "127.0.0.1",
    Port = 1433, UID = "sa", PWD = clave,
    Database = "lupa_pruebas", TDS_Version = "8.0", encoding = "UTF-8"
  ), error = function(e) NULL)
  skip_if(is.null(conexion), "SQL Server no disponible en 1433.")
  on.exit({
    try(DBI::dbRemoveTable(conexion, "mcm_ms_cubre"), silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)

  datos <- .mcm_datos(1500L)
  .mcm_escribir(conexion, "mcm_ms_cubre", datos)
  medianas <- .mcm_medianas(conexion, "mcm_ms_cubre", 2000L)
  .mcm_comprobar(
    medianas, c("x", "y", "z", "w"), metodo_cte = "w", datos = datos
  )
})
