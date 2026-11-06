# `meta$estrategia_mediana` publica UNA forma por tabla: la que el motor acepto.
# Pero una columna que no puede usarla -una con extremos, o un `float8` de
# PostgreSQL- corre por otra via, y la tabla sigue nombrando la consolidada. El
# metodo de cada columna esta en `resumen_tabla$sql$metodo`. Estas pruebas fijan
# que esa columna es la verdad por columna: dice la via cuyo SQL corrio, y la
# mediana sale identica a la de R.

# Fuera de CRAN: cuatro perfiles contra el motor tardan mas de los 0,2 s que se
# admiten en modo CRAN (medido el 2026-11-06). Corre en la suite completa y en el
# CI, con NOT_CRAN=true.
testthat::skip_on_cran()

# La via que cada metodo publicado promete, leida del SQL que corrio.
.via_del_sql_mediana <- function(sql) {
  if (grepl("ROW_NUMBER() OVER", sql, fixed = TRUE, useBytes = TRUE)) {
    return("cte_ventana")
  }
  if (grepl("PERCENTILE_CONT", sql, fixed = TRUE, useBytes = TRUE)) {
    return("PERCENTILE_CONT")
  }
  if (grepl(" OFFSET ", sql, fixed = TRUE, useBytes = TRUE)) {
    return("dos_consultas")
  }
  NA_character_
}

.comprobar_mediana_por_columna <- function(perfil, datos, esperado) {
  sql <- perfil$resumen_tabla$sql
  mediana <- sql[sql$metrica == "mediana", , drop = FALSE]
  columnas <- perfil$resumen_tabla$columnas
  for (columna in names(esperado)) {
    fila <- mediana[mediana$columna == columna, , drop = FALSE]
    expect_identical(nrow(fila), 1L)
    expect_identical(fila$metodo, esperado[[columna]], info = columna)
    expect_identical(.via_del_sql_mediana(fila$sql), fila$metodo, info = columna)
    expect_identical(
      as.numeric(columnas$mediana[columnas$columna == columna]),
      as.numeric(stats::median(datos[[columna]])), info = columna
    )
  }
  # Las columnas de estrategia de `sql` son las de `n_distintos`, en todas las
  # filas: tampoco describen la mediana.
  distintos <- perfil$resumen_tabla$meta$estrategia_distintos
  expect_true(all(mediana$estrategia_resuelta == distintos$estrategia_resuelta))
  invisible(perfil$resumen_tabla$meta$estrategia_mediana)
}

test_that("en DuckDB la columna con extremos no usa la consolidada que nombra la tabla", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  conexion <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conexion, shutdown = TRUE), add = TRUE)
  set.seed(41)
  datos <- data.frame(x = stats::rnorm(501L), e = stats::rnorm(501L) * 1e200)
  DBI::dbWriteTable(conexion, "emc_extremos", datos)

  completa <- suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, "emc_extremos", proteger_datos_personales = FALSE
  )))
  tabla <- .comprobar_mediana_por_columna(
    completa, datos, list(x = "PERCENTILE_CONT", e = "dos_consultas")
  )
  # Lo que la tabla publica es su resolucion, no el metodo de `e`.
  expect_identical(tabla$estrategia_resuelta, "PERCENTILE_CONT")

  # La muestra que cubre la tabla: `e` va por la CTE de ventanas.
  muestra <- suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, "emc_extremos", universo = "muestra_motor",
    muestra_motor = 1000L, bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))
  tabla <- .comprobar_mediana_por_columna(
    muestra, datos, list(x = "PERCENTILE_CONT", e = "cte_ventana")
  )
  expect_identical(tabla$estrategia_resuelta, "PERCENTILE_CONT")
})

test_that("en PostgreSQL los float8 no usan la consolidada que nombra la tabla", {
  conexion <- .mcm_con_pg()
  tabla_pg <- "emc_float8"
  on.exit({
    try(DBI::dbRemoveTable(conexion, tabla_pg), silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)
  set.seed(43)
  datos <- data.frame(x = stats::rnorm(501L), y = sample(1:40, 501L, TRUE))
  try(DBI::dbRemoveTable(conexion, tabla_pg), silent = TRUE)
  DBI::dbWriteTable(conexion, tabla_pg, datos)

  completa <- suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, tabla_pg, proteger_datos_personales = FALSE
  )))
  tabla <- .comprobar_mediana_por_columna(
    completa, datos, list(x = "dos_consultas", y = "PERCENTILE_CONT")
  )
  expect_identical(tabla$estrategia_resuelta, "PERCENTILE_CONT")
})
