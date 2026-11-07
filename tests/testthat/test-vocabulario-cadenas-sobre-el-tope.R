# El tope de asimetria de un grupo equifrecuente es del GRUPO: `Montevideo`
# (25), `Montevido` (20) y `Montividoo` (10) estan a 1,25 y 2 de a pares, pero
# a 2,5 de punta a punta, y con el tope 2 la cadena no se agrupa. El camino
# principal tampoco la forma, asi que sus dos pares cercanos no se declaraban
# en ningun lado. Ahora la fila de cobertura los declara aparte: "K pares
# cercanos ... forman cadenas cuya asimetria de punta a punta supera 2.0". El
# tope no cambia, y ningun par entra en dos cuentas.

# Fuera de CRAN: el archivo arma cinco perfiles y una docena de corridas del
# detector, por encima de los 0,2 s que se admiten en modo CRAN. Corre en la
# suite completa y en el CI, con NOT_CRAN=true.
testthat::skip_on_cran()

.cadena_detector <- function(x, equi) {
  lupa:::.grupos_casi_duplicados_vocabulario(
    x, lupa:::.resolver_normalizacion(TRUE), "x",
    detectar_variantes_equifrecuentes = equi
  )
}

.cadena_motivo <- function(x) {
  p <- suppressWarnings(suppressMessages(perfilar(
    data.frame(localidad = x, stringsAsFactors = FALSE),
    fecha = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )))
  cb <- cobertura(p)
  cb[cb$diagnostico == "proximidad_vocabulario", c("motivo", "como_resolverlo"),
     drop = FALSE]
}

# Los pares cercanos calculados aparte, como en
# test-vocabulario-cada-par-una-vez.R: Jaro-Winkler con el prefijo y el umbral
# del detector y la razon de frecuencias dentro del tope. Fixtures en letras
# ASCII sin espacios ni numeros.
.cadena_pares_aparte <- function(x, umbral = 0.10, p = 0.1, tope = 2) {
  unidad <- tolower(x[!is.na(x)])
  u <- unique(unidad)
  f <- vapply(u, function(v) sum(unidad == v), numeric(1L), USE.NAMES = FALSE)
  d <- stringdist::stringdistmatrix(u, u, method = "jw", p = p)
  razon <- outer(f, f, pmax) / outer(f, f, pmin)
  idx <- which(upper.tri(d) & d <= umbral & razon <= tope, arr.ind = TRUE)
  data.frame(a = u[idx[, 1L]], b = u[idx[, 2L]], stringsAsFactors = FALSE)
}

.cadena_dentro <- function(pares, grupos) {
  vapply(seq_len(nrow(pares)), function(k) {
    any(vapply(grupos, function(g) {
      u <- tolower(g$variantes)
      pares$a[[k]] %in% u && pares$b[[k]] %in% u
    }, logical(1L)))
  }, logical(1L))
}

# Cada par cercano se declara exactamente una vez: en un grupo del camino
# principal, en M o en K. Igual con el detector apagado y encendido.
.cadena_comprobar <- function(x, etiqueta) {
  apagado <- .cadena_detector(x, FALSE)
  encendido <- .cadena_detector(x, TRUE)
  pares <- .cadena_pares_aparte(x)
  principal <- .cadena_dentro(pares, encendido$grupos)
  resto <- as.numeric(sum(!principal))
  expect_identical(
    encendido$alcance$n_pares_equifrecuentes +
      encendido$alcance$n_pares_cadenas_equifrecuentes,
    resto, label = etiqueta
  )
  expect_identical(
    apagado$alcance$n_pares_cadenas_equifrecuentes,
    encendido$alcance$n_pares_cadenas_equifrecuentes, label = etiqueta
  )
  expect_identical(apagado$grupos, encendido$grupos, label = etiqueta)
  encendido
}

.cadena_tres <- function(ultima = 10L) {
  c(rep("Montevideo", 25L), rep("Montevido", 20L), rep("Montividoo", ultima))
}

test_that("una cadena que pasa el tope de punta a punta declara sus pares", {
  skip_if_not_installed("stringdist")
  x <- .cadena_tres()
  r <- .cadena_comprobar(x, "cadena sola")
  expect_length(r$grupos, 0L)
  expect_length(r$grupos_equifrecuentes, 0L)
  expect_identical(r$alcance$n_grupos_sin_variante_rara, 0L)
  expect_identical(r$alcance$n_pares_equifrecuentes, 0)
  expect_identical(r$alcance$n_pares_cadenas_equifrecuentes, 2)
  # La fila de la columna junta lo que declara cada criterio -aca tambien el
  # par bajo el piso de asimetria-; la clausula de las cadenas va al final, y
  # sin un grupo que no se formo no empieza con "Otros".
  fila <- .cadena_motivo(x)
  expect_identical(nrow(fila), 1L)
  expect_true(endsWith(fila$motivo, paste0(
    ". 2 pares cercanos, con asimetr\u00eda <= 2.0 entre ellos, forman cadenas ",
    "cuya asimetr\u00eda de punta a punta supera 2.0 y no se agrupan."
  )))
  expect_true(endsWith(fila$como_resolverlo, paste0(
    " Las cadenas se pueden agrupar subiendo ",
    "`max_asimetria_equifrecuente_vocabulario`, o revisar sus pares manualmente."
  )))
})

test_that("con un grupo que no se formo, la cadena va en una clausula aparte", {
  skip_if_not_installed("stringdist")
  x <- c(.cadena_tres(), rep("Paisandu", 30L), rep("Paysandu", 30L))
  r <- .cadena_comprobar(x, "cadena y empate")
  expect_identical(r$alcance$n_grupos_sin_variante_rara, 1L)
  expect_identical(r$alcance$n_pares_equifrecuentes, 1)
  expect_identical(r$alcance$n_pares_cadenas_equifrecuentes, 2)
  # El grupo equifrecuente es el empate; la cadena no entra.
  expect_identical(
    lapply(unname(r$grupos_equifrecuentes), `[[`, "variantes"),
    list(c("Paisandu", "Paysandu"))
  )
  fila <- .cadena_motivo(x)
  expect_identical(nrow(fila), 1L)
  expect_true(endsWith(fila$motivo, paste0(
    " 1 grupo de formas cercanas no se form\u00f3 porque sus frecuencias fueron ",
    "parecidas: el criterio de variante rara no pudo distinguir una forma ",
    "correcta de otra. Se identific\u00f3 1 par cercano con asimetr\u00eda <= 2.0. ",
    "Otros 2 pares cercanos, con asimetr\u00eda <= 2.0 entre ellos, forman ",
    "cadenas cuya asimetr\u00eda de punta a punta supera 2.0 y no se agrupan."
  )))
})

test_that("una cadena dentro del tope sigue siendo un grupo, sin clausula", {
  skip_if_not_installed("stringdist")
  # Control: 25, 20 y 15 dan 1,67 de punta a punta.
  x <- .cadena_tres(15L)
  r <- .cadena_comprobar(x, "cadena dentro del tope")
  expect_identical(r$alcance$n_grupos_sin_variante_rara, 1L)
  expect_identical(r$alcance$n_pares_equifrecuentes, 2)
  expect_identical(r$alcance$n_pares_cadenas_equifrecuentes, 0)
  fila <- .cadena_motivo(x)
  expect_identical(nrow(fila), 1L)
  expect_true(endsWith(fila$motivo, paste0(
    " 1 grupo de formas cercanas no se form\u00f3 porque sus frecuencias fueron ",
    "parecidas: el criterio de variante rara no pudo distinguir una forma ",
    "correcta de otra. Se identificaron 2 pares cercanos con asimetr\u00eda <= 2.0."
  )))
  expect_false(grepl("forman cadenas", fila$motivo, fixed = TRUE, useBytes = TRUE))
})
