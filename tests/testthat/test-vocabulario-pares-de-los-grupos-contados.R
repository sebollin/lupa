# La fila de cobertura de la via equifrecuente dice "N grupos de formas
# cercanas no se formaron... Se identificaron M pares cercanos con asimetria
# <= X", y M se lee como los pares de esos N grupos. Contaba todos los pares
# registrados, tambien los de un grupo que el camino principal ya publico
# entero: con la estrella `Zona A`..`Zona O` publicada, sus catorce hojas
# empatadas sumaban 91 pares a los de otros dos grupos, y el par de una clase
# exacta unida por distancia a su vecina se sumaba al de otra pareja.
#
# La propiedad que se fija: M es la cantidad de pares cercanos de los grupos
# que la fila cuenta, calculada aparte con la distancia, y es la misma con el
# detector encendido o apagado.

# Fuera de CRAN: el archivo arma cinco perfiles; tarda 3,9 s si es lo primero
# que corre y 0,47 s con el paquete ya cargado (medido el 2026-11-06), por
# encima de los 0,2 s que se admiten en modo CRAN. Corre en la suite completa
# y en el CI, con NOT_CRAN=true.
testthat::skip_on_cran()

.pares_contados_palabras <- function() {
  c(paste0("palabra", letters, "x"), paste0("termino", letters[1:14], "y"))
}

.pares_contados_zonas <- function() {
  rep(paste0("Zona ", LETTERS[1:15]), times = c(100L, rep(4L, 14L)))
}

.pares_contados_grupos <- function(x, equi) {
  lupa:::.grupos_casi_duplicados_vocabulario(
    x, lupa:::.resolver_normalizacion(TRUE), "x",
    detectar_variantes_equifrecuentes = equi
  )
}

# M tal como lo publica la fila de cobertura.
.pares_contados_publicado <- function(x, equi) {
  p <- suppressWarnings(suppressMessages(perfilar(
    data.frame(x = x, stringsAsFactors = FALSE),
    fecha = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    variantes_equifrecuentes_vocabulario = equi
  )))
  cb <- cobertura(p)
  motivo <- cb$motivo[cb$diagnostico == "proximidad_vocabulario"]
  m <- regmatches(motivo, regexpr("Se identificaron [0-9]+ par(es)? ", motivo))
  expect_length(m, 1L)
  as.integer(gsub("[^0-9]", "", m))
}

# Los pares de cada grupo contado, calculados aparte: Jaro-Winkler con el
# prefijo y el umbral del detector, entre formas sin numeros, y con las
# frecuencias dentro del tope.
.pares_contados_aparte <- function(grupos, umbral = 0.10, p = 0.1, tope = 2) {
  sum(vapply(grupos, function(g) {
    d <- stringdist::stringdistmatrix(
      tolower(g$variantes), tolower(g$variantes), method = "jw", p = p
    )
    f <- g$frecuencias
    razon <- outer(f, f, pmax) / outer(f, f, pmin)
    as.numeric(sum(upper.tri(d) & d <= umbral & razon <= tope))
  }, numeric(1L)))
}

test_that("las hojas de una estrella publicada no suman pares a la fila", {
  skip_if_not_installed("stringdist")
  x <- c(.pares_contados_zonas(), .pares_contados_palabras())
  encendido <- .pares_contados_grupos(x, TRUE)
  apagado <- .pares_contados_grupos(x, FALSE)
  # Control de que el caso es el que se quiere: la estrella sale publicada
  # entera y los dos grupos contados no la tocan.
  expect_length(encendido$grupos, 1L)
  expect_length(encendido$grupos[[1L]]$variantes, 15L)
  expect_identical(encendido$alcance$n_grupos_sin_variante_rara, 2L)
  expect_length(encendido$grupos_equifrecuentes, 2L)
  expect_false(any(grepl("Zona", unlist(lapply(
    encendido$grupos_equifrecuentes, `[[`, "variantes"
  )), fixed = TRUE, useBytes = TRUE)))
  aparte <- .pares_contados_aparte(encendido$grupos_equifrecuentes)
  expect_gt(aparte, 0)
  expect_identical(encendido$alcance$n_pares_equifrecuentes, aparte)
  expect_identical(apagado$alcance$n_pares_equifrecuentes, aparte)
  expect_identical(.pares_contados_publicado(x, FALSE), as.integer(aparte))
  expect_identical(.pares_contados_publicado(x, TRUE), as.integer(aparte))
  # Y es lo que cuentan los mismos dos grupos sin la estrella al lado.
  sin_estrella <- .pares_contados_grupos(.pares_contados_palabras(), FALSE)
  expect_identical(sin_estrella$alcance$n_pares_equifrecuentes, aparte)
})

test_that("el par de una clase ya publicada entera no se suma al de otra pareja", {
  skip_if_not_installed("stringdist")
  x <- c(rep("Montevideo", 25L), rep("MONTEVIDEO", 25L), rep("Montevido", 48L),
         rep("Paisandu", 30L), rep("Paysandu", 30L), rep("Salto", 30L))
  encendido <- .pares_contados_grupos(x, TRUE)
  apagado <- .pares_contados_grupos(x, FALSE)
  expect_identical(encendido$alcance$n_grupos_sin_variante_rara, 1L)
  expect_identical(
    encendido$grupos_equifrecuentes[[1L]]$variantes, c("Paisandu", "Paysandu")
  )
  expect_identical(encendido$alcance$n_pares_equifrecuentes, 1)
  expect_identical(apagado$alcance$n_pares_equifrecuentes, 1)
  expect_identical(.pares_contados_publicado(x, FALSE), 1L)
  expect_identical(.pares_contados_publicado(x, TRUE), 1L)
})

test_that("sin hojas publicadas la cuenta de pares no cambia", {
  skip_if_not_installed("stringdist")
  # Control: los mismos dos grupos sin la estrella al lado. Ningun candidato
  # queda afuera, asi que M es el total de pares registrados, como siempre, y
  # el mismo que con la estrella.
  x <- .pares_contados_palabras()
  encendido <- .pares_contados_grupos(x, TRUE)
  apagado <- .pares_contados_grupos(x, FALSE)
  expect_length(encendido$grupos, 0L)
  expect_identical(encendido$alcance$n_grupos_sin_variante_rara, 2L)
  aparte <- .pares_contados_aparte(encendido$grupos_equifrecuentes)
  expect_identical(encendido$alcance$n_pares_equifrecuentes, aparte)
  expect_identical(apagado$alcance$n_pares_equifrecuentes, aparte)
  expect_identical(.pares_contados_publicado(x, FALSE), as.integer(aparte))
})
