# O15. Las puertas que no aceptan un `perfil_dbi` dicen que hacer con el.
#
# `reportar()` lo recorria como lista y terminaba culpando a alguno de sus
# componentes con una enumeracion de clases; `comparar_perfiles()` decia
# "debe ser un objeto producido por perfilar()". Ninguno de los dos mensajes
# decia lo unico que el usuario necesita: que el perfil esta adentro, en
# `$perfil_muestra`. No es una cifra que mienta, es una salida sin salida.

.o15_perfil_dbi <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(
    a = 1:10, b = rep(letters[1:5], 2), stringsAsFactors = FALSE
  ))
  perfilar_dbi(con, "t")
}

test_that("reportar dice que el perfil de un DBI esta en perfil_muestra", {
  perfil_dbi <- .o15_perfil_dbi()
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  expect_error(
    reportar(perfil_dbi, archivo = archivo),
    "perfil_muestra", fixed = TRUE
  )
  # Y lo que el mensaje indica funciona.
  expect_no_error(reportar(perfil_dbi$perfil_muestra, archivo = archivo))
})

test_that("comparar_perfiles nombra el argumento y el camino", {
  perfil_dbi <- .o15_perfil_dbi()
  otro <- perfilar(data.frame(a = 1:10, b = rep(letters[1:5], 2),
                              stringsAsFactors = FALSE))
  expect_error(comparar_perfiles(otro, perfil_dbi),
               "actual$perfil_muestra", fixed = TRUE)
  expect_error(comparar_perfiles(perfil_dbi, otro),
               "anterior$perfil_muestra", fixed = TRUE)
  expect_no_error(comparar_perfiles(otro, perfil_dbi$perfil_muestra))
})

test_that("una lista cualquiera sigue dando el mensaje de siempre", {
  # El control: el mensaje nuevo es para `perfil_dbi`, no para todo lo que no
  # se puede reportar ni comparar.
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  expect_error(reportar(list(a = 1), archivo = archivo), "Cada objeto debe ser",
               fixed = TRUE)
  expect_error(comparar_perfiles(perfilar(data.frame(a = 1:3)), list(b = 2)),
               "producido por perfilar()", fixed = TRUE)
})
