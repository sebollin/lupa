# Cada par cercano de formas normalizadas se declara una sola vez: dentro de un
# grupo de `casi_duplicados_vocabulario`, o en la fila de cobertura "N grupos de
# formas cercanas no se formaron... Se identificaron M pares" (y, con el
# detector encendido, en un grupo `variantes_equifrecuentes_vocabulario`).
# Nunca en los dos y nunca en ninguno.
#
# Lo que se decide es el par y no el grupo. Se decidia por grupo: un candidato
# equifrecuente se descartaba solo si cabia entero en un grupo publicado. Con
# una tercera forma colgando de la pareja publicada -`Montevideo` (100),
# `Montevido` (50), `Montividoo` (50)- el candidato ya no cabia, y la salida por
# omision publicaba [Montevideo / Montevido] y a la vez "1 grupo de formas
# cercanas no se formo... 2 pares", que contaba esa misma pareja; con el
# detector, la pareja salia otra vez dentro de un grupo de tres. Y al reves, si
# la pareja publicada llevaba la asimetria del candidato por encima del tope, el
# par que quedaba suelto no salia en ningun lado.
#
# La forma que comparten el grupo publicado y el par que no lo esta se nombra en
# los dos, como con una clase exacta empatada con su vecina: sacarla dejaria al
# par sin uno de sus extremos.

# Fuera de CRAN: el archivo arma cinco perfiles y dieciseis corridas del
# detector; tarda 3,3 s si es lo primero que corre y 0,37 s con el paquete ya
# cargado (medido el 2026-11-06), por encima de los 0,2 s que se admiten en
# modo CRAN. Corre en la suite completa y en el CI, con NOT_CRAN=true.
testthat::skip_on_cran()

.cada_par_grupos <- function(x, equi) {
  lupa:::.grupos_casi_duplicados_vocabulario(
    x, lupa:::.resolver_normalizacion(TRUE), "x",
    detectar_variantes_equifrecuentes = equi
  )
}

# Los pares cercanos por forma normalizada, calculados aparte: Jaro-Winkler con
# el prefijo y el umbral del detector, y la razon de frecuencias de las formas
# normalizadas dentro del tope. Los fixtures son letras ASCII sin espacios ni
# numeros, asi que la normalizacion es pasar a minusculas.
.cada_par_aparte <- function(x, umbral = 0.10, p = 0.1, tope = 2) {
  unidad <- tolower(x[!is.na(x)])
  u <- unique(unidad)
  f <- vapply(u, function(v) sum(unidad == v), numeric(1L), USE.NAMES = FALSE)
  d <- stringdist::stringdistmatrix(u, u, method = "jw", p = p)
  razon <- outer(f, f, pmax) / outer(f, f, pmin)
  idx <- which(upper.tri(d) & d <= umbral & razon <= tope, arr.ind = TRUE)
  data.frame(a = u[idx[, 1L]], b = u[idx[, 2L]], stringsAsFactors = FALSE)
}

# Si los dos extremos de cada par caen en un mismo grupo de la lista.
.cada_par_dentro <- function(pares, grupos) {
  vapply(seq_len(nrow(pares)), function(k) {
    any(vapply(grupos, function(g) {
      u <- tolower(g$variantes)
      pares$a[[k]] %in% u && pares$b[[k]] %in% u
    }, logical(1L)))
  }, logical(1L))
}

# La propiedad, con el detector apagado y encendido. Devuelve la salida
# encendida para las comprobaciones de cada caso.
.cada_par_comprobar <- function(x, etiqueta) {
  apagado <- .cada_par_grupos(x, FALSE)
  encendido <- .cada_par_grupos(x, TRUE)
  pares <- .cada_par_aparte(x)
  expect_gt(nrow(pares), 0L, label = etiqueta)
  principal <- .cada_par_dentro(pares, encendido$grupos)
  equifrecuente <- .cada_par_dentro(pares, encendido$grupos_equifrecuentes)
  # Ninguno sin declarar.
  expect_true(all(principal | equifrecuente), label = etiqueta)
  # M cuenta los que el camino principal no publico, y solo esos: un par
  # publicado que se contara lo subiria, uno suelto lo bajaria.
  m <- as.numeric(sum(!principal))
  expect_identical(encendido$alcance$n_pares_equifrecuentes, m, label = etiqueta)
  expect_identical(apagado$alcance$n_pares_equifrecuentes, m, label = etiqueta)
  # N es la cantidad de grupos que se publican con el detector, y no depende
  # de encenderlo; el detector no cambia el camino principal.
  expect_identical(
    encendido$alcance$n_grupos_sin_variante_rara,
    length(encendido$grupos_equifrecuentes), label = etiqueta
  )
  expect_identical(
    apagado$alcance$n_grupos_sin_variante_rara,
    encendido$alcance$n_grupos_sin_variante_rara, label = etiqueta
  )
  expect_identical(apagado$grupos, encendido$grupos, label = etiqueta)
  encendido
}

.cada_par_variantes <- function(grupos) {
  lapply(unname(grupos), `[[`, "variantes")
}

.cada_par_tres <- function() {
  c(rep("Montevideo", 100L), rep("Montevido", 50L), rep("Montividoo", 50L))
}

test_that("la pareja publicada no se cuenta aunque el par vecino si", {
  skip_if_not_installed("stringdist")
  # d(Montevideo, Montevido) = 0,020 y d(Montevido, Montividoo) = 0,087, pero
  # d(Montevideo, Montividoo) = 0,105 queda fuera del umbral: es una cadena.
  r <- .cada_par_comprobar(.cada_par_tres(), "tres formas")
  expect_identical(
    .cada_par_variantes(r$grupos), list(c("Montevideo", "Montevido"))
  )
  expect_identical(
    .cada_par_variantes(r$grupos_equifrecuentes),
    list(c("Montevido", "Montividoo"))
  )
  expect_identical(r$alcance$n_grupos_sin_variante_rara, 1L)
  expect_identical(r$alcance$n_pares_equifrecuentes, 1)
  equi <- r$grupos_equifrecuentes[[1L]]
  expect_identical(equi$asimetria, 1)
  # La distancia publicada es la del par que se declara, no la de la pareja.
  expect_equal(equi$distancia_minima, equi$distancia_maxima)
  expect_gt(equi$distancia_minima, 0.05)
})

test_that("lo publicado por omision y con el detector declara la pareja una vez", {
  skip_if_not_installed("stringdist")
  publicar <- function(equi) {
    p <- suppressWarnings(suppressMessages(perfilar(
      data.frame(localidad = .cada_par_tres(), stringsAsFactors = FALSE),
      fecha = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      variantes_equifrecuentes_vocabulario = equi
    )))
    h <- suppressWarnings(suppressMessages(hallazgos(p)))
    cb <- cobertura(p)
    list(
      casi = h[h$tipo_hallazgo == "casi_duplicados_vocabulario", , drop = FALSE],
      equi = h[h$tipo_hallazgo == "variantes_equifrecuentes_vocabulario", ,
               drop = FALSE],
      motivos = cb$motivo[cb$diagnostico == "proximidad_vocabulario"]
    )
  }
  for (equi in c(FALSE, TRUE)) {
    salida <- publicar(equi)
    etiqueta <- paste("detector", equi)
    expect_identical(nrow(salida$casi), 1L, label = etiqueta)
    expect_true(grepl(
      "[Montevideo (100) / Montevido (50)];", salida$casi$evidencia,
      fixed = TRUE, useBytes = TRUE
    ), label = etiqueta)
    # La fila cuenta el par que no se formo, no la pareja publicada.
    expect_identical(length(salida$motivos), 1L, label = etiqueta)
    expect_true(grepl(
      "1 grupo de formas cercanas no se form", salida$motivos,
      fixed = TRUE, useBytes = TRUE
    ), label = etiqueta)
    expect_true(grepl(
      "Se identific\u00f3 1 par cercano con asimetr", salida$motivos,
      fixed = TRUE, useBytes = TRUE
    ), label = etiqueta)
    if (equi) {
      expect_identical(nrow(salida$equi), 1L)
      expect_true(grepl(
        "[Montevido (50) / Montividoo (50)]; asimetria=1.0;",
        salida$equi$evidencia, fixed = TRUE, useBytes = TRUE
      ))
      expect_false(grepl(
        "Montevideo (", salida$equi$evidencia, fixed = TRUE, useBytes = TRUE
      ))
    } else {
      expect_identical(nrow(salida$equi), 0L)
    }
  }
  # Encender el detector no cambia la fila de casi duplicados.
  expect_identical(publicar(FALSE)$casi, publicar(TRUE)$casi)
})

test_that("en una cadena de cuatro formas se cuentan solo los pares sin publicar", {
  skip_if_not_installed("stringdist")
  # `Mantividoo` solo esta cerca de `Montividoo` (d = 0,060; contra las otras
  # dos, 0,20 y 0,22).
  #
  # Empatadas detras de la pareja publicada: los dos pares que siguen se
  # cuentan, y la pareja no.
  x <- c(.cada_par_tres(), rep("Mantividoo", 50L))
  r <- .cada_par_comprobar(x, "cuatro empatadas")
  expect_identical(
    .cada_par_variantes(r$grupos), list(c("Montevideo", "Montevido"))
  )
  expect_identical(
    .cada_par_variantes(r$grupos_equifrecuentes),
    list(c("Mantividoo", "Montevido", "Montividoo"))
  )
  expect_identical(r$alcance$n_pares_equifrecuentes, 2)
  # Con las dos puntas publicadas, el par del medio quedaba sin declarar: el
  # candidato de las cuatro formas iba de 100 a 25, por encima del tope, y se
  # descartaba entero. Sin las parejas publicadas adentro, es un empate.
  x <- c(.cada_par_tres(), rep("Mantividoo", 25L))
  r <- .cada_par_comprobar(x, "cuatro con las puntas publicadas")
  expect_identical(
    .cada_par_variantes(r$grupos),
    list(c("Montevideo", "Montevido"), c("Mantividoo", "Montividoo"))
  )
  expect_identical(
    .cada_par_variantes(r$grupos_equifrecuentes),
    list(c("Montevido", "Montividoo"))
  )
  expect_identical(
    unname(vapply(r$grupos_equifrecuentes, `[[`, numeric(1L), "asimetria")), 1
  )
  expect_identical(r$alcance$n_pares_equifrecuentes, 1)
})

test_that("la clase exacta dentro de la pareja publicada no cambia la cuenta", {
  skip_if_not_installed("stringdist")
  x <- c(rep("Montevideo", 50L), rep("MONTEVIDEO", 50L),
         rep("Montevido", 50L), rep("Montividoo", 50L))
  r <- .cada_par_comprobar(x, "clase en la pareja")
  expect_identical(
    .cada_par_variantes(r$grupos),
    list(c("MONTEVIDEO", "Montevideo", "Montevido"))
  )
  expect_identical(r$grupos[[1L]]$origen, "normalizacion+distancia")
  expect_identical(
    .cada_par_variantes(r$grupos_equifrecuentes),
    list(c("Montevido", "Montividoo"))
  )
  expect_identical(r$alcance$n_pares_equifrecuentes, 1)
})

test_that("una clase publicada no arrastra al grupo equifrecuente sus otras formas", {
  skip_if_not_installed("stringdist")
  # Un vocabulario de erratas al azar: el camino principal publica
  # [HJEYLLVOZR / hjeyllvozf / hjeyllvozr / qjeyllvozr], y `hjeyllvvar` (100)
  # empata con la clase `hjeyllvozr` (50 + 50). El grupo equifrecuente es esa
  # pareja, con las grafias de la clase; no las otras dos formas publicadas.
  x <- rep(
    c("hjeyllvozr", "qjeyllvozr", "hjeyllvozf", "hjeyllvvar", "mfyroqopm",
      "mfyzoqopm", "mfyroqozm", "mfybomopm", "HJEYLLVOZR"),
    c(50L, 50L, 50L, 100L, 4L, 10L, 100L, 4L, 50L)
  )
  r <- .cada_par_comprobar(x, "clase publicada")
  expect_identical(
    .cada_par_variantes(r$grupos_equifrecuentes),
    list(c("HJEYLLVOZR", "hjeyllvozr", "hjeyllvvar"))
  )
  expect_identical(
    unname(vapply(r$grupos_equifrecuentes, `[[`, numeric(1L), "asimetria")), 1
  )
  expect_identical(r$alcance$n_pares_equifrecuentes, 1)
  expect_true(any(vapply(
    .cada_par_variantes(r$grupos), identical, logical(1L),
    c("HJEYLLVOZR", "hjeyllvozf", "hjeyllvozr", "qjeyllvozr")
  )))
})

test_that("dos formas de un grupo publicado en un equifrecuente no suman su par", {
  skip_if_not_installed("stringdist")
  # `ewgxgsucy` (50) empata entre `ewgxgjucy` (100) y `eqgxgsucy` (100), asi
  # que no tiene centro, y esta cerca tambien de `ewgxusmcy` (50), que el
  # camino principal publica con `ewgxgjucy`. El grupo equifrecuente necesita
  # las dos formas publicadas -cada una es extremo de un par sin publicar-,
  # pero el par entre ellas lo declara solo el camino principal: no entra en
  # M ni en las distancias del grupo.
  x <- rep(c("eqgxgsucy", "ewgxgjucy", "ewgxgsucy", "ewgxusmcy"),
           c(100L, 100L, 50L, 50L))
  r <- .cada_par_comprobar(x, "dos del principal")
  expect_identical(
    .cada_par_variantes(r$grupos), list(c("ewgxgjucy", "ewgxusmcy"))
  )
  expect_identical(
    .cada_par_variantes(r$grupos_equifrecuentes),
    list(c("eqgxgsucy", "ewgxgjucy", "ewgxgsucy", "ewgxusmcy"))
  )
  expect_identical(r$alcance$n_pares_equifrecuentes, 3)
  distancias <- stringdist::stringdist(
    c("ewgxgjucy", "ewgxgsucy"), c("ewgxusmcy", "ewgxusmcy"),
    method = "jw", p = 0.1
  )
  # La mayor del grupo es la de `ewgxgsucy` con `ewgxusmcy`, no la del par
  # publicado, que es mayor.
  expect_gt(distancias[[1L]], distancias[[2L]])
  expect_equal(
    unname(vapply(r$grupos_equifrecuentes, `[[`, numeric(1L),
                  "distancia_maxima")),
    distancias[[2L]]
  )
})

test_that("un par publicado no une dos grupos equifrecuentes", {
  skip_if_not_installed("stringdist")
  # `Mtntevideo` (100) empata con `Montevideo` (100) y `Mvntevido` (50) con
  # `Montevido` (50); cada una esta cerca solo de su pareja. Lo unico que une
  # los dos empates es [Montevideo / Montevido], que el camino principal
  # publica: son dos grupos que no se formaron, no uno de cuatro formas.
  x <- c(rep("Montevideo", 100L), rep("Montevido", 50L),
         rep("Mtntevideo", 100L), rep("Mvntevido", 50L))
  r <- .cada_par_comprobar(x, "puente publicado")
  expect_identical(
    .cada_par_variantes(r$grupos), list(c("Montevideo", "Montevido"))
  )
  expect_setequal(
    vapply(.cada_par_variantes(r$grupos_equifrecuentes), paste, character(1L),
           collapse = " / "),
    c("Montevideo / Mtntevideo", "Montevido / Mvntevido")
  )
  expect_identical(r$alcance$n_grupos_sin_variante_rara, 2L)
  expect_identical(r$alcance$n_pares_equifrecuentes, 2)
  expect_identical(
    unname(vapply(r$grupos_equifrecuentes, `[[`, numeric(1L), "asimetria")),
    c(1, 1)
  )
})

test_that("sola, la pareja publicada no deja fila ni grupo equifrecuente", {
  skip_if_not_installed("stringdist")
  # Control: sin la tercera forma el candidato cabe entero en el grupo
  # publicado, y la salida era la misma antes del cambio.
  x <- c(rep("Montevideo", 100L), rep("Montevido", 50L))
  r <- .cada_par_comprobar(x, "solo la pareja")
  expect_identical(
    .cada_par_variantes(r$grupos), list(c("Montevideo", "Montevido"))
  )
  expect_length(r$grupos_equifrecuentes, 0L)
  expect_identical(r$alcance$n_grupos_sin_variante_rara, 0L)
  expect_identical(r$alcance$n_pares_equifrecuentes, 0)
  p <- suppressWarnings(suppressMessages(perfilar(
    data.frame(localidad = x, stringsAsFactors = FALSE),
    fecha = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    variantes_equifrecuentes_vocabulario = TRUE
  )))
  cb <- cobertura(p)
  expect_false(any(grepl(
    "de formas cercanas no se form",
    cb$motivo[cb$diagnostico == "proximidad_vocabulario"],
    fixed = TRUE, useBytes = TRUE
  )))
})
