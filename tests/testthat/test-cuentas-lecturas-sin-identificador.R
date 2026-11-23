# Dos o mas lecturas de la misma tabla nunca suman `una_lectura`. Una parte de
# `rbind()` cuyas lecturas no traen su identificador (`lectura` `NA`, como las
# de un resumen anterior a ese campo) recibia el de la parte delante, y dos
# lecturas de la misma tabla sin identificador quedaban como una sola.

.sinid_fecha <- as.POSIXct("2026-06-30", tz = "UTC")

.sinid_con <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "ventas", data.frame(x = c(1, 2, NA, 4, 5)))
  DBI::dbWriteTable(con, "otras", data.frame(y = c(1, NA)))
  con
}

.sinid_dos_rotulos <- function(con) {
  nucleo <- metricas_nucleo()
  medir_dbi(
    modelo(instanciar(especializar(nucleo$NoNulo, "Primera"), "Uno", "x"),
           instanciar(especializar(nucleo$NoNulo, "Segunda"), "Dos", "x")),
    con, tablas = list(Uno = "ventas", Dos = "ventas"),
    id_medicion = "j", fecha = .sinid_fecha
  )
}

.sinid_otra_parte <- function(con) {
  medir_dbi(
    modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "Y"), "otras", "y")),
    con, id_medicion = "j", fecha = .sinid_fecha
  )
}

.sinid_cortes <- list(Completitud = c(0.5, 0.9))

test_that("dos lecturas de una tabla sin identificador no son una_lectura tras rbind()", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- .sinid_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  m <- .sinid_dos_rotulos(con)
  l <- attr(m, "lecturas", exact = TRUE)
  expect_identical(nrow(l), 2L)
  expect_identical(length(unique(l$tabla)), 1L)
  l$lectura <- NA_character_
  attr(m, "lecturas") <- l
  u <- rbind(m, .sinid_otra_parte(con))
  s <- semaforo_calidad(u, cortes = .sinid_cortes)
  expect_false(any(s$control_lecturas[s$entidad %in% c("Uno", "Dos")] == "una_lectura"))
  expect_identical(s$control_lecturas[s$entidad %in% c("Uno", "Dos")],
                   c("limitado", "limitado"))
})

test_that("el control: dos rotulos de una tabla en una llamada son limitado, no una_lectura", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- .sinid_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  m <- .sinid_dos_rotulos(con)
  s <- semaforo_calidad(m, cortes = .sinid_cortes)
  expect_identical(s$control_lecturas, c("limitado", "limitado"))
  u <- rbind(m, .sinid_otra_parte(con))
  s <- semaforo_calidad(u, cortes = .sinid_cortes)
  expect_identical(s$control_lecturas[s$entidad %in% c("Uno", "Dos")],
                   c("limitado", "limitado"))
})
