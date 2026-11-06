# Un estado que afirma un valor -`calculado`, `estimado`, `observado_muestra`-
# no puede publicarlo ausente: `NA` dice "no se midio" y el estado dice "se
# midio". Pasaba en todos los motores: `tasa_distintos` de una columna vacia en
# el camino por omision, la moda de una columna sin repetidos, la mediana
# consolidada y los basicos de una columna vacia sin `validos`, y en
# `con_muestra` las metricas numericas de una columna de texto. La regla se
# aplica sobre el resumen ya armado; las pruebas contra MySQL y MariaDB estan en
# `test-local-mysql.R`.

testthat::skip_on_cran()

skip_if_not_installed("DBI")

test_that("ningun registro afirma un valor ausente en DuckDB", {
  skip_if_not_installed("duckdb")
  conexion <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conexion, shutdown = TRUE), add = TRUE)
  .mcm_tabla_vacias(conexion, "mcm_vacias")
  hallados <- .mcm_recorrer_afirmados(conexion, "mcm_vacias")
  expect_identical(nrow(hallados), 0L)

  # Y cada caso dice lo que paso, como el resto de los caminos.
  registro <- function(perfil, columna, metrica) {
    sql <- perfil$resumen_tabla$sql
    salida <- sql[sql$columna == columna & sql$metrica == metrica,
                  c("estado", "motivo"), drop = FALSE]
    rownames(salida) <- NULL
    salida
  }
  perfil <- function(...) suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, "mcm_vacias", proteger_datos_personales = FALSE, ...
  )))
  completo <- perfil()
  expect_identical(
    registro(completo, "vacia", "tasa_distintos")$estado, "sin_valores"
  )
  sin_moda <- registro(completo, "unicos", "moda")
  expect_identical(sin_moda$estado, "no_aplica")
  expect_match(sin_moda$motivo, "se repite", fixed = TRUE)
  # La consolidada sobre la columna vacia dice lo mismo que la via exacta.
  basicos <- perfil(metricas = c("basicos", "mediana"))
  expect_identical(
    registro(basicos, "vacia", "mediana"),
    registro(completo, "vacia", "mediana")
  )
  expect_identical(registro(basicos, "vacia", "mediana")$estado, "sin_valores")
  muestra <- perfil(
    universo = "muestra_motor", muestra_motor = 1000L,
    bloque_muestra = "con_muestra"
  )
  expect_identical(registro(muestra, "texto", "media")$estado, "no_aplica")
  expect_identical(registro(muestra, "uno", "desvio")$estado, "no_aplica")
  expect_identical(
    registro(muestra, "vacia", "minimo")$estado, "no_disponible"
  )
})

test_that("ningun registro afirma un valor ausente en SQLite", {
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  .mcm_tabla_vacias(conexion, "mcm_vacias")
  expect_identical(nrow(.mcm_recorrer_afirmados(conexion, "mcm_vacias")), 0L)
})

# El dialecto `portable` no supone ninguna clausula de salto de filas, y la via
# de dos consultas no se puede escribir. La mediana quedaba `no_disponible`
# aunque el motor aceptara la CTE de ventanas, que no necesita salto.
test_that("sin salto de filas la mediana sale de la CTE de ventanas", {
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  set.seed(20261106)
  datos <- data.frame(
    x = stats::rnorm(1001), g = stats::runif(1001) * 1e12,
    e = stats::rnorm(1001) * 1e200, par = c(stats::rnorm(1000), NA)
  )
  DBI::dbWriteTable(conexion, "mcm_portable", datos)
  perfil <- suppressWarnings(perfilar_dbi(
    conexion, "mcm_portable", dialecto = "portable",
    proteger_datos_personales = FALSE
  ))
  columnas <- perfil$resumen_tabla$columnas
  sql <- perfil$resumen_tabla$sql
  for (columna in names(datos)) {
    expect_identical(
      as.numeric(columnas$mediana[columnas$columna == columna]),
      stats::median(datos[[columna]], na.rm = TRUE), info = columna
    )
    expect_identical(
      sql$metodo[sql$metrica == "mediana" & sql$columna == columna],
      "cte_ventana", info = columna
    )
  }
  expect_identical(
    perfil$resumen_tabla$meta$estrategia_mediana$estrategia_resuelta,
    "cte_ventana"
  )

  # Si el motor tampoco acepta la CTE, el motivo dice las dos cosas.
  testthat::local_mocked_bindings(
    .sondar_mediana_cte_ventana_dbi = function(conexion, presupuesto) list(
      disponible = FALSE, candidato = NULL, sondas = character(),
      motivo = "capacidad_no_aceptada:sonda_mediana_cte_ventana"
    ),
    .package = "lupa"
  )
  perfil <- suppressWarnings(perfilar_dbi(
    conexion, "mcm_portable", dialecto = "portable", metricas = "mediana",
    proteger_datos_personales = FALSE
  ))
  sql <- perfil$resumen_tabla$sql
  motivo <- sql$motivo[sql$metrica == "mediana" & sql$columna == "x"]
  expect_identical(
    sql$estado[sql$metrica == "mediana" & sql$columna == "x"], "no_disponible"
  )
  expect_match(motivo, "no expresa un salto de filas", fixed = TRUE)
  expect_match(motivo, "CTE de ventanas", fixed = TRUE)
})
