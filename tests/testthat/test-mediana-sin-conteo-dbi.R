# La mediana exacta de la tabla completa por la via de dos consultas necesita
# el conteo de valores no nulos para el salto. Con `metricas` sin `validos`
# -por ejemplo `c("basicos", "mediana")`- el conteo no se conocia y la mediana
# quedaba `no_disponible` en todo motor sin consolidada (MySQL, MariaDB) y en
# un `float8` de PostgreSQL, que no usa la consolidada. Ahora se cuenta y la
# mediana sale por el mismo camino que con todas las metricas. Las pruebas de
# MySQL y MariaDB estan en `test-local-mysql.R`.

testthat::skip_on_cran()

test_that("PostgreSQL calcula la mediana de un float8 sin pedir validos", {
  conexion <- .mcm_con_pg()
  tabla <- "mcm_pg_sin_conteo"
  on.exit({
    try(DBI::dbRemoveTable(conexion, tabla), silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)
  datos <- .mcm_datos(1500L)
  datos$nulo <- NA_real_
  .mcm_escribir(conexion, tabla, datos)
  esperado <- lapply(datos[c("x", "y", "z", "w")], stats::median, na.rm = TRUE)
  esperado <- lapply(esperado, as.numeric)
  parcial <- .mcm_comprobar_sin_conteo(
    conexion, tabla, esperado, sin_valores = "nulo"
  )
  sql <- parcial$resumen_tabla$sql
  metodo <- sql$metodo[sql$metrica == "mediana"]
  names(metodo) <- sql$columna[sql$metrica == "mediana"]
  # El entero conserva la consolidada; los float8, la via exacta por columna.
  expect_identical(unname(metodo["y"]), "PERCENTILE_CONT")
  expect_identical(unname(metodo[c("x", "z", "w")]), rep("dos_consultas", 3L))
})
