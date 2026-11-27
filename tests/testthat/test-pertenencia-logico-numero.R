# La pertenencia por valor entre un logico y un numero: `TRUE` es `1` y `FALSE`
# es `0`, en las dos direcciones, como `%in%` de R base; nunca contra un numero
# con clase propia (`units`, `difftime`, fechas). Y el sello
# de las metricas que dependen de esa regla lleva su version: una medicion
# hecha con la regla de antes y una de ahora no tienen la misma definicion.

testthat::skip_on_cran()

.plnum_medir <- function(columna, ...) {
  nuc <- metricas_nucleo()
  mod <- modelo(instanciar(especializar(...), "d", "b", nombre_instancia = "M"))
  r <- suppressWarnings(medir(mod, list(d = data.frame(b = columna)),
                              id_medicion = "p", fecha = as.Date("2026-01-01"),
                              detalle = "resumen"))
  paste0(r$numerador, "/", r$denominador)
}

test_that("un logico y un numero se igualan en las dos direcciones, y nada mas", {
  nuc <- metricas_nucleo()
  vpe <- nuc$ValoresPosiblesPorExtension
  expect_identical(.plnum_medir(c(1L, 0L, NA, 2L), vpe, "M", valores = TRUE), "1/3")
  expect_identical(.plnum_medir(c(1, 0, NA, 0.5), vpe, "M", valores = c(TRUE, FALSE)), "2/3")
  expect_identical(.plnum_medir(c(TRUE, FALSE, NA, TRUE), vpe, "M", valores = 1L), "2/3")
  expect_identical(.plnum_medir(c(1L, 0L, NA, 1L), nuc$NoNulo, "M", valores_nulos = FALSE), "2/4")
})

test_that("un numero con clase propia no se iguala con un logico", {
  skip_if_not_installed("units")
  # La regla deja los dos lados como vienen: `%in%` de `units` no iguala una
  # longitud con `TRUE`, y una duracion o una fecha tampoco son un numero.
  metro <- units::set_units(1, "m")
  expect_identical(.logico_contra_numero(metro, TRUE)$conjunto, TRUE)
  expect_identical(.logico_contra_numero(TRUE, metro)$x, TRUE)
  expect_identical(.logico_contra_numero(as.difftime(1, units = "days"), TRUE)$conjunto, TRUE)
  expect_identical(.logico_contra_numero(as.Date("1970-01-02"), TRUE)$conjunto, TRUE)
  expect_identical(.logico_contra_numero(1L, TRUE)$conjunto, 1L)
  expect_identical(.logico_contra_numero(TRUE, 2.5)$x, 1L)
})

test_that("el sello cambia con la version de la regla solo donde la regla decide", {
  nuc <- metricas_nucleo()
  ins <- function(met, nombre, atr, ...) {
    instanciar(especializar(met, nombre, ...), "d", atr, nombre_instancia = nombre)
  }
  mod <- modelo(
    ins(nuc$ValoresPosiblesPorExtension, "Ei", "i", valores = TRUE),
    ins(nuc$NoNulo, "Ni", "i", valores_nulos = FALSE),
    ins(nuc$ValoresPosiblesPorExtension, "Et", "t", valores = c("a", "b")),
    ins(nuc$NoNulo, "N0", "i"),
    ins(nuc$ValoresPosiblesPorComprension, "R", "i", minimo = 0, maximo = 1)
  )
  datos <- list(d = data.frame(i = c(1L, 0L, 2L), t = c("a", "b", "a")))
  medir_con <- function(id) {
    suppressWarnings(medir(mod, datos, id_medicion = id, fecha = as.Date("2026-01-01"),
                           detalle = "completo"))
  }
  nueva <- medir_con("nueva")
  vieja <- local({
    local_mocked_bindings(.VERSION_PERTENENCIA_VALOR = 1L)
    medir_con("vieja")
  })
  s_n <- attr(nueva, "sello_metricas", exact = TRUE)
  s_v <- attr(vieja, "sello_metricas", exact = TRUE)
  igual <- stats::setNames(s_n$huella_definicion == s_v$huella_definicion,
                           s_n$metrica_instanciada)
  expect_identical(igual[["Ei"]], FALSE)
  expect_identical(igual[["Ni"]], FALSE)
  expect_true(all(igual[c("Et", "N0", "R")]))
  regla <- regla_evaluacion("Pasa", function(x) x > 0, metricas = c("Ei", "Ni"))
  perfil <- perfil_evaluacion("Operativo", regla)
  comp <- suppressWarnings(comparar_evaluaciones(
    suppressWarnings(evaluar(vieja, perfil)), suppressWarnings(evaluar(nueva, perfil))
  ))
  expect_true(is.na(comp$delta))
  expect_true(grepl("definici", comp$comparacion, fixed = TRUE, useBytes = TRUE))
})

# Un numero con clase (`I(1L)`) sobre una columna logica: R no lo iguala con
# `TRUE`, y el motor tampoco lo cuenta.
test_that("un numero con clase sobre una columna logica no se iguala, en R ni en el motor", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE d (b BOOLEAN)")
  DBI::dbExecute(con, "INSERT INTO d VALUES (TRUE), (FALSE), (NULL), (TRUE)")
  nuc <- metricas_nucleo()
  mod <- modelo(instanciar(especializar(nuc$ValoresPosiblesPorExtension, "Ea",
                                        valores = I(1L)),
                           "d", "b", nombre_instancia = "Ea"))
  en_r <- suppressWarnings(medir(mod, list(d = DBI::dbReadTable(con, "d")),
                                 id_medicion = "c", fecha = as.Date("2026-01-01"),
                                 detalle = "resumen"))
  expect_identical(c(en_r$numerador, en_r$denominador), c(0, 3))
  omision <- suppressWarnings(medir_dbi(mod, con, id_medicion = "c",
                                        fecha = as.Date("2026-01-01")))
  expect_identical(nrow(omision), 0L)
  bloques <- suppressWarnings(medir_dbi(mod, con, id_medicion = "c",
                                        fecha = as.Date("2026-01-01"),
                                        estrategia_no_sql = "bloques"))
  expect_identical(c(bloques$numerador, bloques$denominador), c(0, 3))
})
