# Una pareja cercana cuyas unidades normalizadas empatan -`Montevideo` (50)
# contra `Montevido` (50)- no forma estrella: no hay centro de frecuencia
# unico. Lo que la declara es la via equifrecuente: la fila de cobertura
# "1 grupo de formas cercanas no se formo" por omision, y el hallazgo
# `variantes_equifrecuentes_vocabulario` con el detector encendido.
#
# Si una de las dos unidades es ademas una clase exacta -`Montevideo` (25) y
# `MONTEVIDEO` (25)-, el empate es el mismo, pero el candidato se descartaba
# por tocar la clase, como si el camino principal ya lo hubiera publicado, y
# su asimetria se media por grafia (10 + 40 contra 50 daba 5). La pareja no se
# declaraba en ningun lado.
#
# La propiedad que se fija: con la clase dentro de la pareja, la pareja se
# declara igual que sin ella. Lo unico que cambia es lo que la clase agrega
# por si misma: su grupo por normalizacion y sus grafias en el grupo.

# Fuera de CRAN: el archivo arma cuatro perfiles; tarda 3,4 s si es lo primero
# que corre y 0,27 s con el paquete ya cargado (medido el 2026-11-06), por
# encima de los 0,2 s que se admiten en modo CRAN. Corre en la suite completa
# y en el CI, con NOT_CRAN=true.
testthat::skip_on_cran()

.pareja_grupos <- function(x, ...) {
  lupa:::.grupos_casi_duplicados_vocabulario(
    x, lupa:::.resolver_normalizacion(TRUE), "x", ...
  )
}

.pareja_resto <- function() rep("Salto", 30L)

.pareja_sin_clase <- function() {
  c(rep("Montevideo", 50L), rep("Montevido", 50L), .pareja_resto())
}

# Las variantes de la clase dentro de la pareja empatada. En orden de bytes
# `MONTEVIDEO < MONTEVIDO < Montevideo < Montevido < montevideo`.
.pareja_con_clase <- function() {
  resto <- .pareja_resto()
  list(
    clase_en_un_lado = c(rep("Montevideo", 25L), rep("MONTEVIDEO", 25L),
                         rep("Montevido", 50L), resto),
    clase_en_el_otro = c(rep("Montevideo", 50L), rep("Montevido", 25L),
                         rep("MONTEVIDO", 25L), resto),
    clase_en_los_dos = c(rep("Montevideo", 25L), rep("MONTEVIDEO", 25L),
                         rep("Montevido", 25L), rep("MONTEVIDO", 25L), resto),
    tres_formas = c(rep("Montevideo", 20L), rep("MONTEVIDEO", 20L),
                    rep("montevideo", 10L), rep("Montevido", 50L), resto),
    # Por grafia la asimetria es 50 / 10 = 5; por unidad, 50 / 50.
    clase_desigual = c(rep("Montevideo", 40L), rep("MONTEVIDEO", 10L),
                       rep("Montevido", 50L), resto)
  )
}

.pareja_campos_equi <- function(grupos) {
  lapply(unname(grupos), `[`, c(
    "asimetria", "distancia_minima", "distancia_maxima", "clase_diferencia",
    "origen"
  ))
}

test_that("la pareja empatada con una clase adentro se cuenta como sin ella", {
  skip_if_not_installed("stringdist")
  gemelo <- .pareja_grupos(
    .pareja_sin_clase(), detectar_variantes_equifrecuentes = TRUE
  )
  expect_identical(gemelo$alcance$n_grupos_sin_variante_rara, 1L)
  for (nombre in names(.pareja_con_clase())) {
    x <- .pareja_con_clase()[[nombre]]
    apagado <- .pareja_grupos(x)
    encendido <- .pareja_grupos(x, detectar_variantes_equifrecuentes = TRUE)
    # El conteo es el del gemelo, y el mismo encendido o apagado.
    expect_identical(
      apagado$alcance$n_grupos_sin_variante_rara, 1L, label = nombre
    )
    expect_identical(
      encendido$alcance$n_grupos_sin_variante_rara, 1L, label = nombre
    )
    expect_length(apagado$grupos_equifrecuentes, 0L)
    # El grupo equifrecuente es el del gemelo con las grafias de la clase.
    expect_length(encendido$grupos_equifrecuentes, 1L)
    expect_identical(
      .pareja_campos_equi(encendido$grupos_equifrecuentes),
      .pareja_campos_equi(gemelo$grupos_equifrecuentes),
      label = nombre
    )
    grupo <- encendido$grupos_equifrecuentes[[1L]]
    formas <- lupa:::.sort_por_bytes(unique(x[x != "Salto"]))
    expect_identical(grupo$variantes, formas, label = nombre)
    expect_identical(
      grupo$frecuencias,
      vapply(formas, function(f) sum(x == f), integer(1L), USE.NAMES = FALSE),
      label = nombre
    )
    # Lo que la clase agrega por si misma: su grupo por normalizacion, igual
    # encendido o apagado.
    expect_identical(encendido$grupos, apagado$grupos, label = nombre)
    expect_true(all(vapply(
      apagado$grupos, function(g) identical(g$origen, "normalizacion"),
      logical(1L)
    )), label = nombre)
  }
})

test_that("una cadena empatada con una clase en el medio se cuenta entera", {
  skip_if_not_installed("stringdist")
  resto <- .pareja_resto()
  sin_clase <- c(rep("Montevideo", 50L), rep("Montevido", 50L),
                 rep("Montevidoo", 50L), resto)
  con_clase <- c(rep("Montevideo", 50L), rep("Montevido", 25L),
                 rep("MONTEVIDO", 25L), rep("Montevidoo", 50L), resto)
  gemelo <- .pareja_grupos(sin_clase, detectar_variantes_equifrecuentes = TRUE)
  cadena <- .pareja_grupos(con_clase, detectar_variantes_equifrecuentes = TRUE)
  expect_identical(cadena$alcance$n_grupos_sin_variante_rara, 1L)
  expect_identical(
    cadena$alcance$n_pares_equifrecuentes,
    gemelo$alcance$n_pares_equifrecuentes
  )
  expect_identical(
    .pareja_campos_equi(cadena$grupos_equifrecuentes),
    .pareja_campos_equi(gemelo$grupos_equifrecuentes)
  )
  expect_identical(
    cadena$grupos_equifrecuentes[[1L]]$variantes,
    c("MONTEVIDO", "Montevideo", "Montevido", "Montevidoo")
  )
})

test_that("lo publicado con la clase adentro es lo del gemelo sin ella", {
  skip_if_not_installed("stringdist")
  fecha <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  publicar <- function(x, equi) {
    p <- suppressWarnings(suppressMessages(perfilar(
      data.frame(x = x, stringsAsFactors = FALSE), fecha = fecha,
      variantes_equifrecuentes_vocabulario = equi
    )))
    cb <- cobertura(p)
    cb <- cb[cb$diagnostico == "proximidad_vocabulario",
             c("diagnostico", "columna", "motivo", "como_resolverlo"),
             drop = FALSE]
    rownames(cb) <- NULL
    h <- suppressWarnings(suppressMessages(hallazgos(p)))
    list(cobertura = cb, hallazgos = h)
  }
  gemelo_apagado <- publicar(.pareja_sin_clase(), FALSE)
  gemelo_encendido <- publicar(.pareja_sin_clase(), TRUE)
  expect_identical(nrow(gemelo_apagado$cobertura), 1L)
  x <- .pareja_con_clase()$clase_en_un_lado
  apagado <- publicar(x, FALSE)
  encendido <- publicar(x, TRUE)
  # La misma fila de cobertura que el gemelo, en los dos modos.
  expect_identical(apagado$cobertura, gemelo_apagado$cobertura)
  expect_identical(encendido$cobertura, gemelo_encendido$cobertura)
  # Con el detector encendido, el hallazgo, con las tres grafias y la
  # asimetria de las unidades.
  equi <- encendido$hallazgos[
    encendido$hallazgos$tipo_hallazgo == "variantes_equifrecuentes_vocabulario",
  ]
  expect_identical(nrow(equi), 1L)
  expect_identical(as.numeric(equi$n_afectados), 3)
  expect_true(grepl(
    "[MONTEVIDEO (25) / Montevideo (25) / Montevido (50)]; asimetria=1.0;",
    equi$evidencia, fixed = TRUE, useBytes = TRUE
  ))
  sin_equi <- apagado$hallazgos[
    apagado$hallazgos$tipo_hallazgo == "variantes_equifrecuentes_vocabulario",
  ]
  expect_identical(nrow(sin_equi), 0L)
  # Aditivo: encender el detector no cambia la fila casi_duplicados.
  fila_casi <- function(h) {
    h[h$tipo_hallazgo == "casi_duplicados_vocabulario", , drop = FALSE]
  }
  expect_identical(fila_casi(encendido$hallazgos), fila_casi(apagado$hallazgos))
})

test_that("sin empate la clase sigue en el grupo del camino principal", {
  skip_if_not_installed("stringdist")
  # Control: con 50 contra 48 hay centro de frecuencia y arista de distancia,
  # el grupo sale `normalizacion+distancia` con todas las grafias, y el
  # candidato equifrecuente, que ya esta publicado entero, no se cuenta.
  resto <- .pareja_resto()
  casos <- list(
    c(rep("Montevideo", 25L), rep("MONTEVIDEO", 25L), rep("Montevido", 48L),
      resto),
    c(rep("Montevideo", 50L), rep("Montevido", 24L), rep("MONTEVIDO", 24L),
      resto),
    c(rep("Montevideo", 20L), rep("MONTEVIDEO", 20L), rep("montevideo", 10L),
      rep("Montevido", 48L), resto)
  )
  for (x in casos) {
    r <- .pareja_grupos(x, detectar_variantes_equifrecuentes = TRUE)
    expect_identical(r$alcance$n_grupos_sin_variante_rara, 0L)
    expect_length(r$grupos_equifrecuentes, 0L)
    expect_length(r$grupos, 1L)
    expect_identical(r$grupos[[1L]]$origen, "normalizacion+distancia")
    expect_identical(
      r$grupos[[1L]]$variantes,
      lupa:::.sort_por_bytes(unique(x[x != "Salto"]))
    )
  }
})
