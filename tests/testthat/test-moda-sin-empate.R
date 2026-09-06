test_that("sin repeticiones no se publica una moda", {
  # Los cuatro valores son distintos: no hay moda. Lo que se publicaba era el
  # que ganaba un desempate de cuatro vias, y el desempate sigue el orden de
  # ordenamiento, que depende de como este guardada la columna.
  numerico <- perfilar(data.frame(v = c(-5.5, -1, 0, 3.75)))
  texto <- perfilar(data.frame(
    v = c("-5.5", "-1", "0", "3.75"), stringsAsFactors = FALSE
  ))

  expect_true(is.na(numerico$columnas$moda[[1L]]))
  expect_true(is.na(texto$columnas$moda[[1L]]))
  # La frecuencia se conserva: es cierta y es la evidencia de por que la moda
  # quedo callada.
  expect_equal(as.numeric(numerico$columnas$frecuencia_moda[[1L]]), 1)
  expect_equal(as.numeric(texto$columnas$frecuencia_moda[[1L]]), 1)
})

test_that("una moda real no se toca", {
  perfil <- perfilar(data.frame(v = c(7, 7, 7, 1, 2)))

  expect_false(is.na(perfil$columnas$moda[[1L]]))
  expect_equal(as.numeric(perfil$columnas$frecuencia_moda[[1L]]), 3)
})

test_that("con un solo valor distinto si hay moda, aunque la frecuencia sea 1", {
  # Control del borde: sin empate que resolver, el valor publicado no depende
  # de nada. Una sola fila entra en este caso.
  repetido <- perfilar(data.frame(v = c(5, 5, 5)))
  unica <- perfilar(data.frame(v = 42))

  expect_false(is.na(repetido$columnas$moda[[1L]]))
  expect_false(is.na(unica$columnas$moda[[1L]]))
  expect_equal(as.numeric(unica$columnas$frecuencia_moda[[1L]]), 1)
  expect_equal(as.numeric(unica$columnas$n_distintos[[1L]]), 1)
})

test_that("el hallazgo de columna constante conserva su evidencia", {
  # Ese hallazgo localiza sus filas comparando contra la moda. Exige
  # `n_distintos == 1`, que es justo el caso que la regla no toca.
  perfil <- perfilar(data.frame(v = c(5, 5, 5)))
  constante <- perfil$hallazgos[perfil$hallazgos$tipo == "constante", ]

  expect_gte(nrow(constante), 1L)
  expect_false(is.na(perfil$columnas$moda[[1L]]))
})

test_that("las tres puertas dicen lo mismo sobre la columna empatada", {
  # El test que importa no es "la moda quedo en NA" sino que memoria y motor no
  # diverjan: antes coincidian, y arreglar una sola puerta habria creado la
  # divergencia.
  skip_if_not_installed("duckdb")
  datos <- data.frame(v = c(-5.5, -1, 0, 3.75))
  conexion <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(duckdb::duckdb_shutdown(conexion), silent = TRUE), add = TRUE)
  DBI::dbWriteTable(conexion, "datos", datos)

  memoria <- perfilar(datos)
  motor <- perfilar_dbi(conexion, "datos")

  expect_true(is.na(memoria$columnas$moda[[1L]]))
  expect_true(is.na(motor$resumen_tabla$columnas$moda[[1L]]))
  expect_true(is.na(motor$perfil_muestra$columnas$moda[[1L]]))
})
