# Salidos de una refutacion adversarial sobre la consolidacion. Los tres son la
# misma familia: el motor devolvio algo, R no lo pudo leer como el numero que la
# metrica supone, y el resultado se publicaba igual.

test_that("un valor que la conversion pierde no se publica como calculado", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # SQLite no tiene tipo fecha: `MIN` de una columna declarada `DATE` vuelve
  # como el texto "2020-01-01", y `as.numeric()` lo convierte en NA. Publicarlo
  # como `calculado` con valor `NA` es decir "se midio" y "no se midio" a la vez.
  DBI::dbExecute(con, "CREATE TABLE t (f DATE)")
  DBI::dbExecute(con, "INSERT INTO t VALUES ('2020-01-01'), ('2021-12-31'), (NULL)")

  perfil <- perfilar_dbi(con, "t", modo = "exacto", muestra = 5L,
                         proteger_datos_personales = FALSE)
  fila <- perfil$resumen_tabla$columnas
  expect_true(is.na(fila$minimo[[1L]]))
  expect_true(is.na(fila$media[[1L]]))
  registros <- perfil$resumen_tabla$sql
  basicos <- registros[registros$metrica %in% c("minimo", "maximo", "media"), ]
  expect_true(all(basicos$estado == "no_disponible"))
  expect_match(basicos$motivo[[1L]], "no se pudo leer como numero")
  # El valor que devolvio el motor viaja en el motivo: el diagnostico no
  # reemplaza la evidencia.
  expect_match(basicos$motivo[[1L]], "2020-01-01", fixed = TRUE)
})

test_that("un entero por encima de 2^53 no se publica redondeado", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("bit64")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  datos <- data.frame(id = 1:5)
  datos$v <- bit64::as.integer64(c("1", "2", "3", "9007199254740993", "5"))
  DBI::dbWriteTable(con, "t", datos)

  perfil <- suppressWarnings(perfilar_dbi(
    con, "t", modo = "exacto", muestra = 5L, proteger_datos_personales = FALSE
  ))
  fila <- perfil$resumen_tabla$columnas
  # El maximo real es 2^53+1; pasarlo a doble lo vuelve 2^53, que no esta en la
  # columna. Antes se publicaba ese numero como `calculado`.
  expect_true(is.na(fila$maximo[fila$columna == "v"]))
  registros <- perfil$resumen_tabla$sql
  estado <- registros$estado[registros$columna == "v" &
                               registros$metrica == "maximo"]
  expect_equal(unique(estado), "no_disponible")
})

test_that("una columna numerica normal se sigue midiendo", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "n", data.frame(a = c(1.5, 2.5, 10), b = 1:3))
  fila <- perfilar_dbi(con, "n", modo = "exacto")$resumen_tabla$columnas
  expect_equal(fila$minimo, c(1.5, 1))
  expect_equal(fila$maximo, c(10, 3))
  expect_equal(fila$media, c(14 / 3, 2))
})

test_that("el plan declara un rango sin consultar los datos", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(a = 1:10, b = 2:11))
  plan <- plan_perfilado_dbi(con, "t")
  # El plan no cuenta filas ni despeja cardinalidades para estimar el costo.
  # Publica el rango de consultas y deja la explicacion en sus atributos.
  expect_match(attr(plan, "supuesto"), "no escanea datos")
  expect_match(attr(plan, "supuesto"), "cardinalidad es desconocida")
  expect_match(attr(plan, "supuesto"), "rechaza un lote")
  expect_false(grepl("es un techo", attr(plan, "supuesto"), fixed = TRUE))
  # Y el otro extremo del rango existe y no es menor que el total.
  total <- attr(plan, "total", exact = TRUE)
  peor <- attr(plan, "total_lotes_rechazados", exact = TRUE)
  expect_false(is.null(peor))
  expect_gte(peor, total)
})

test_that("la puerta de exactitud cubre el entero que redondea hacia el limite", {
  skip_if_not_installed("bit64")
  # 2^53+1 pasa a doble como 2^53, asi que comparar el doble ya convertido
  # contra el limite deja pasar el unico caso que hay que atajar.
  limite <- bit64::as.integer64("9007199254740993")
  expect_null(.numerico_trazable(list(clase = "integer64", valores = limite)))
  expect_null(.numerico_trazable(list(clase = "integer64", valores = -limite)))
  mezcla <- c(bit64::as.integer64(c(10, 11)), limite)
  expect_null(.numerico_trazable(list(clase = "integer64", valores = mezcla)))
  # Y lo que si es exacto sigue pasando, incluido el propio 2^53.
  dentro <- c(bit64::as.integer64(c(10, 11)), bit64::as.integer64("9007199254740992"))
  expect_equal(
    .numerico_trazable(list(clase = "integer64", valores = dentro)),
    c(10, 11, 2^53)
  )
})
