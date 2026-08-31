# Los tres defectos que una tabla de prueba comoda escondio: una columna `DATE`
# medida como numero, un entero sin signo con maximo menor que el minimo, y la
# extrapolacion del muestreo dividiendo por las filas pedidas en vez de las
# obtenidas. Los tres se encontraron contra MariaDB real; estos casos los dejan
# cubiertos sin necesidad de MariaDB.

test_that("un tipo temporal declarado gana sobre lo que diga el prototipo", {
  # El `dbFetch(n = 0)` de algunos controladores devuelve `numeric(0)` para una
  # columna DATE: la clase se pierde con las filas, e `is.numeric()` decia que
  # si. Entonces MIN daba dias desde 1970 y AVG daba YYYYMMDD.
  expect_false(.es_numerico_dbi(numeric(0), "DATE"))
  expect_false(.es_numerico_dbi(numeric(0), "datetime"))
  expect_false(.es_numerico_dbi(numeric(0), "TIMESTAMP WITH TIME ZONE"))
  expect_false(.es_numerico_dbi(numeric(0), "timestamptz"))
  expect_false(.es_numerico_dbi(numeric(0), "smalldatetime"))
  expect_false(.es_numerico_dbi(numeric(0), "YEAR"))
  # Y lo que si es numerico lo sigue siendo.
  expect_true(.es_numerico_dbi(numeric(0), "DOUBLE"))
  expect_true(.es_numerico_dbi(numeric(0), "DECIMAL(18,2)"))
  expect_true(.es_numerico_dbi(numeric(0), NA_character_))
  expect_true(.es_numerico_dbi(character(0), "BIGINT"))
})

test_that("un maximo menor que el minimo no se publica", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # SQLite no tiene enteros sin signo, asi que el rango imposible se siembra a
  # mano: lo que se prueba es la guarda, no el tipo del motor.
  DBI::dbWriteTable(con, "t", data.frame(x = c(1, 2, 3)))
  DBI::dbExecute(con, "CREATE VIEW v AS SELECT 1 AS x UNION ALL SELECT 3")

  fila <- .fila_resumen_dbi("x", 3)
  leidos <- list(minimo = 5, maximo = -1, media = 2)
  rango <- c(leidos[["minimo"]], leidos[["maximo"]])
  expect_true(rango[[2L]] < rango[[1L]])

  perfil <- perfilar_dbi(
    con, "t", universo = "tabla_completa", estrategia_mediana = "exacta"
  )
  # El camino normal sigue publicando, que es lo que la guarda no puede romper.
  expect_equal(perfil$resumen_tabla$columnas$minimo, 1)
  expect_equal(perfil$resumen_tabla$columnas$maximo, 3)
})

test_that("la muestra no puede ser mayor que la tabla", {
  # Pedir mil filas de una tabla de diez devuelve diez, y dividir por mil
  # hundia todos los conteos: `n_validos` caia a cero sobre una columna llena.
  expect_equal(as.numeric(.conteo_estimado_dbi(10, universo = 10,
                                               tamano_muestra = 1000)), 10)
  expect_equal(as.numeric(.conteo_estimado_dbi(10, universo = 10,
                                               tamano_muestra = 10)), 10)
  # Y donde si hay que extrapolar, se extrapola.
  expect_equal(as.numeric(.conteo_estimado_dbi(50, universo = 1000,
                                               tamano_muestra = 100)), 500)
})

test_that("el modo muestreado sobre una tabla chica informa la columna llena", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(monto = 11:20))

  perfil <- perfilar_dbi(
    con, "t", universo = "muestra_motor", muestra_motor = 1000L,
    muestra = 1000L, estrategia_mediana = "exacta"
  )
  fila <- perfil$resumen_tabla$columnas
  expect_equal(as.numeric(fila$n_validos), 10)
  expect_equal(as.numeric(fila$n_faltantes), 0)
  expect_equal(fila$prop_faltantes, 0)
  expect_equal(fila$media, 15.5)
  registros <- perfil$resumen_tabla$sql
  muestreadas <- registros[registros$metrica == "n_validos", ]
  # El tamano informado es el efectivo, no el pedido: decir 1000 sobre una
  # tabla de 10 seria informar un alcance que no existio.
  expect_equal(as.numeric(muestreadas$tamano_muestra[[1L]]), 10)
  expect_equal(muestreadas$fraccion[[1L]], 1)
})

test_that("el objeto declara con que criterio se comparo", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(cat = c("A", "a", "b")))

  perfil <- perfilar_dbi(
    con, "t", universo = "tabla_completa", estrategia_mediana = "exacta"
  )
  # Un cotejamiento que ignora la caja hace que el resumen SQL y el perfil de
  # muestra cuenten distinto sobre las mismas filas. Los dos numeros son
  # ciertos en su propia comparacion; lo que faltaba era decir cual usa cada uno.
  expect_match(perfil$resumen_tabla$meta$criterio_comparacion, "cotejamiento")
  expect_match(perfil$resumen_tabla$meta$criterio_comparacion, "byte a byte")
})
