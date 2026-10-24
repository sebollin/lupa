# El motivo de una poda es una explicacion, y explicaba publicando los cuatro
# extremos crudos. Sobre una columna de documentos eso es una fuga: `perfilar()`
# y `perfilar_dbi()` enmascaran el minimo y el maximo de esa misma columna y por
# esta puerta salian literales. Las dos mitades se prueban: que deje de publicar
# lo identificante, y que SIGA publicando lo que no lo es -una guarda que
# enmascara todo no se distingue de una que no mide-.

test_that("una poda por rangos disjuntos no publica extremos identificantes", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t1", data.frame(documento = 45120001:45120040))
  DBI::dbWriteTable(con, "t2", data.frame(documento = 91330001:91330030))

  rc <- relaciones_coleccion(
    coleccion(con, c("t1", "t2"), nombre = "prueba"),
    data.frame(tabla_1 = "t1", col_1 = "documento",
               tabla_2 = "t2", col_2 = "documento",
               stringsAsFactors = FALSE)
  )

  expect_true(nrow(rc$cobertura_podas) >= 1L)
  expect_true(all(rc$cobertura_podas$motivo == "rangos_disjuntos"))

  # Los extremos reales no aparecen en NINGUNA parte del objeto, no solo en el
  # campo que se arreglo: la fuga se busca donde pueda haber quedado.
  texto <- paste(
    utils::capture.output(utils::str(rc, max.level = 6, list.len = 400)),
    collapse = " "
  )
  for (extremo in c("45120001", "45120040", "91330001", "91330030")) {
    expect_false(grepl(extremo, texto, fixed = TRUE))
  }
  expect_true(any(grepl("valor protegido", rc$cobertura_podas$detalle,
                        fixed = TRUE)))
})

test_that("una poda entre valores que no identifican sigue publicando el rango", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Cinco caracteres: por debajo del piso. Son rangos disjuntos igual.
  DBI::dbWriteTable(con, "a", data.frame(codigo = c(10.05, 94.98)))
  DBI::dbWriteTable(con, "b", data.frame(codigo = c(101.5, 199.5)))

  rc <- relaciones_coleccion(
    coleccion(con, c("a", "b"), nombre = "prueba"),
    data.frame(tabla_1 = "a", col_1 = "codigo",
               tabla_2 = "b", col_2 = "codigo",
               stringsAsFactors = FALSE)
  )

  expect_true(nrow(rc$cobertura_podas) >= 1L)
  detalle <- rc$cobertura_podas$detalle[[1L]]
  expect_false(grepl("valor protegido", detalle, fixed = TRUE))
  expect_true(grepl("10.05", detalle, fixed = TRUE))
  expect_true(grepl("94.98", detalle, fixed = TRUE))
})
