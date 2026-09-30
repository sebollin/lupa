# Con una frontera de `coleccion()`, una tabla VACIA y una que no esta en la
# entrada recibian el mismo motivo -"No hay una medida de esta tabla en la
# entrada"-, aunque la medicion SI sabia que la vacia tenia cero filas: lo dejo en
# `cobertura_metricas`. Ahora ese motivo viaja a la cobertura de la coleccion.

test_that("la tabla vacia dice que tiene cero filas; la ausente, que no hay medida", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "a", data.frame(x = c(1, NA, 3)))
  DBI::dbWriteTable(conexion, "b", data.frame(x = numeric()))

  nucleo <- metricas_nucleo()
  medidas <- medir(
    modelo(instanciar(especializar(nucleo$NoNulo), "a", "x"),
           instanciar(especializar(nucleo$NoNulo), "b", "x")),
    list(a = DBI::dbReadTable(conexion, "a"), b = DBI::dbReadTable(conexion, "b"))
  )
  por_coleccion <- agregar(
    agregar(agregar(medidas, "atributo", "ratio"), "entidad", "promedio"),
    "coleccion", "promedio_ponderado",
    coleccion = coleccion(conexion, c("a", "b", "zz"), nombre = "c1"), pesos = 1
  )
  cobertura <- attr(por_coleccion, "cobertura_coleccion", exact = TRUE)
  motivos <- stats::setNames(cobertura$motivo_sin_medir, cobertura$tablas_sin_medir)
  expect_setequal(names(motivos), c("b", "zz"))
  # La vacia: el motivo de la medicion, con la causa.
  expect_match(motivos[["b"]], "cero filas", fixed = TRUE)
  # La que no estaba en el modelo: el generico, que es lo unico que se sabe.
  expect_match(motivos[["zz"]], "No hay una medida de esta tabla", fixed = TRUE)
  expect_false(grepl("cero filas", motivos[["zz"]], fixed = TRUE))
})
