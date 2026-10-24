# `hallazgos()` sobre una coleccion devolvia una tabla vacia y callada, aunque los
# perfiles conservados tuvieran decenas de hallazgos: se leia como "ninguna tabla
# tiene hallazgos". La ayuda de los accesores promete lo contrario -lo que no hay
# no se inventa, y una tabla vacia viene con su aviso-. Ahora une los de cada
# perfil conservado con la identidad de la tabla adelante, como `sql_perfil()`.

# Pesada para CRAN (perfilar una coleccion por DBI tarda varios segundos): corre en
# la suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.coleccion_de_prueba <- function(tablas = c("eventos", "personas")) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "eventos", datos_operativos)
  DBI::dbWriteTable(con, "personas", datos_administrativos)
  list(con = con, coleccion = coleccion(con, tablas = tablas))
}

test_that("hallazgos() de una coleccion une los de cada tabla con su identidad", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  base <- .coleccion_de_prueba()
  on.exit(DBI::dbDisconnect(base$con), add = TRUE)
  r <- suppressWarnings(perfilar_coleccion(base$coleccion, conservar_perfiles = TRUE))

  por_tabla <- vapply(r$perfiles, function(p) nrow(hallazgos(p)), integer(1L))
  # Primera mitad: los perfiles conservados tienen hallazgos. Si no tuvieran,
  # una tabla unida vacia pasaria la comprobacion de abajo sin probar nada.
  expect_true(all(por_tabla > 0L))

  h <- expect_silent(hallazgos(r))
  expect_identical(names(h)[1:3], c("tabla", "esquema", "identificador"))
  expect_identical(nrow(h), sum(por_tabla))
  expect_identical(as.integer(table(h$tabla)[names(por_tabla)]), unname(por_tabla))
  # Las columnas del hallazgo llegan intactas: la severidad sigue ordenada y la
  # trazabilidad sigue siendo una lista.
  expect_s3_class(h$severidad, "ordered")
  expect_true(is.list(h$trazabilidad))
  eventos <- h[h$tabla == "eventos", names(hallazgos(r$perfiles$eventos))]
  rownames(eventos) <- NULL
  esperado <- hallazgos(r$perfiles$eventos)
  rownames(esperado) <- NULL
  expect_identical(eventos, esperado)
})

test_that("sin perfiles conservados, la tabla vacia llega con su aviso", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  base <- .coleccion_de_prueba("eventos")
  on.exit(DBI::dbDisconnect(base$con), add = TRUE)
  r <- suppressWarnings(perfilar_coleccion(base$coleccion))
  expect_length(r$perfiles, 0L)
  expect_warning(h <- hallazgos(r), "conservar_perfiles = TRUE", fixed = TRUE)
  expect_identical(nrow(h), 0L)
  expect_identical(names(h)[1:3], c("tabla", "esquema", "identificador"))
})

test_that("las tablas conservadas sin muestra se nombran en un aviso", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  base <- .coleccion_de_prueba()
  on.exit(DBI::dbDisconnect(base$con), add = TRUE)
  r <- suppressWarnings(perfilar_coleccion(
    base$coleccion, conservar_perfiles = TRUE, bloque_muestra = "solo_agregados"
  ))
  expect_warning(h <- hallazgos(r), "las tablas eventos, personas", fixed = TRUE)
  expect_identical(nrow(h), 0L)
})
