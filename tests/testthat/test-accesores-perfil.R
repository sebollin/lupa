test_that("los accesores leen las tres formas de salida sin conocer su forma", {
  perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
  expect_s3_class(hallazgos(perfil), "data.frame")
  expect_equal(nrow(columnas(perfil)), ncol(datos_administrativos))
  expect_equal(n_filas(perfil), nrow(datos_administrativos))
  expect_s3_class(cobertura(perfil), "data.frame")
  # Un perfil en memoria no emitio SQL, y una tabla vacia sugeriria que emitio
  # y no encontro.
  expect_null(sql_perfil(perfil))
})

test_that("la salida DBI se lee con los mismos nombres", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(a = 1:100, b = seq_len(100) / 100))

  perfil <- perfilar_dbi(con, "t")
  # La trampa que este accesor existe para tapar: `perfil$general$filas` da
  # NULL sobre una salida DBI, y un NULL silencioso calcula sobre nada.
  expect_null(perfil$general$filas)
  expect_equal(as.numeric(n_filas(perfil)), 100)
  expect_equal(nrow(columnas(perfil)), 2L)
  expect_gt(nrow(sql_perfil(perfil)), 0L)
  expect_s3_class(cobertura(perfil), "data.frame")
})

test_that("un perfil DBI sin muestra avisa en vez de aparentar cero hallazgos", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(a = 1:50))

  perfil <- perfilar_dbi(con, "t", muestra = 10)
  perfil$perfil_muestra <- NULL
  expect_warning(salida <- hallazgos(perfil), "no trae muestra leida")
  expect_equal(nrow(salida), 0L)
  # El resumen por columna sigue estando: lo que falta es la parte por fila.
  expect_equal(nrow(columnas(perfil)), 1L)
})

test_that("una coleccion no inventa un total de filas que ninguna tabla tiene", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "a", data.frame(x = 1:10))
  DBI::dbWriteTable(con, "b", data.frame(y = 1:20))

  perfil <- perfilar_coleccion(coleccion(con, c("a", "b")))
  expect_equal(nrow(columnas(perfil)), 2L)
  expect_s3_class(cobertura(perfil), "data.frame")
})

test_that("una clase ajena se rechaza con su nombre", {
  expect_error(hallazgos(1:5), "debe ser un objeto de perfilar")
  expect_error(columnas(data.frame(a = 1)), "debe ser un objeto de perfilar")
  expect_error(n_filas(NULL), "debe ser un objeto de perfilar")
})
