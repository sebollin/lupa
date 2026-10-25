# Fuera de CRAN: perfila tres veces la tabla de ejemplo (mas de 0,2 s en modo
# CRAN). Corre en la suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

# Desde el 2026-10-25 la proteccion de datos personales se pide: publicar o no
# los valores de una base es decision de quien la usa. La clasificacion de
# posibles datos personales corre igual, porque es un diagnostico.

test_that("ninguna funcion exportada protege por omision", {
  # Se recorren las firmas, no una lista escrita a mano: una funcion nueva con la
  # bandera entra sola.
  ns <- asNamespace("lupa")
  con_bandera <- Filter(function(n) {
    f <- get(n, envir = ns)
    is.function(f) && "proteger_datos_personales" %in% names(formals(f))
  }, getNamespaceExports("lupa"))
  expect_gt(length(con_bandera), 10L)
  for (n in con_bandera) {
    expect_false(eval(formals(get(n, envir = ns))$proteger_datos_personales),
                 info = n)
  }
})

test_that("por omision el perfil clasifica pero no enmascara", {
  perfil <- suppressWarnings(perfilar(datos_operativos))
  h <- hallazgos(perfil)
  expect_false(perfil$meta$proteger_datos_personales)
  expect_true("dato_personal_posible" %in% h$tipo_hallazgo)
  expect_false(any(grepl("[evidencia protegida]", h$evidencia, fixed = TRUE)))

  # Control: pedida, la misma evidencia sale enmascarada.
  pedido <- suppressWarnings(
    perfilar(datos_operativos, proteger_datos_personales = TRUE)
  )
  expect_true(any(grepl("[evidencia protegida]", hallazgos(pedido)$evidencia,
                        fixed = TRUE)))
})

test_that("perfilar_dbi() tampoco protege por omision", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", datos_operativos)
  abierto <- suppressWarnings(suppressMessages(perfilar_dbi(con, "t")))
  expect_identical(abierto$resumen_tabla$meta$motor$informacion_dbi$dbname,
                   ":memory:")
  # Control: pedida, el dato de conexion se tapa.
  cerrado <- suppressWarnings(suppressMessages(
    perfilar_dbi(con, "t", proteger_datos_personales = TRUE)
  ))
  expect_identical(cerrado$resumen_tabla$meta$motor$informacion_dbi$dbname,
                   "[dato de conexion protegido]")
})
