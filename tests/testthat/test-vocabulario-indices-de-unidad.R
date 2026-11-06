# Dentro del detector de vocabulario conviven dos espacios de indices: las
# FORMAS, que son los valores tal como se escribieron, y las UNIDADES, que son
# las formas ya normalizadas. Una clase exacta -`ALFA` y `Alfa`- junta dos
# formas en una unidad, y desde ahi la unidad `k` deja de ser la forma `k`.
# Sin clases los dos espacios coinciden, y por eso un indice leido en el
# espacio equivocado pasaba todas las pruebas: ninguna ponia una clase antes
# de la pareja que miraba.
#
# La propiedad que se fija: lo que se informa de una pareja no depende de que
# haya una clase exacta antes o despues de ella en el orden del vocabulario.
# La clase misma se informa igual en los dos casos y se separa al comparar.
# Las filas de la clase van siempre al final de la tabla, para que lo unico
# que cambie sea su lugar en el orden de bytes (`#` y `A` antes de los
# valores, `~` y `Z` despues).

# Fuera de CRAN: el archivo arma dos perfiles y nueve vocabularios; tarda
# 3,3 s si es lo primero que corre y 0,16 s con el paquete ya cargado (medido
# el 2026-11-06). Lo primero pasa holgado los 0,2 s que se admiten en modo
# CRAN y lo segundo queda al borde. Corre en la suite completa y en el CI, con
# NOT_CRAN=true.
testthat::skip_on_cran()

.indices_grupos <- function(x, ...) {
  lupa:::.grupos_casi_duplicados_vocabulario(
    x, lupa:::.resolver_normalizacion(TRUE), "x", ...
  )
}

# Los nombres de la lista son la raiz de cada componente, un indice que se
# corre con la clase por construccion y que ningun consumidor lee.
.indices_sin_clase <- function(grupos, clase) {
  unname(Filter(function(g) !any(g$variantes %in% clase), grupos))
}

.indices_pareja <- function() {
  c(rep("Montevideo", 50L), rep("Montevido", 48L), rep("Salto", 30L))
}

test_that("una clase exacta antes de la pareja no borra el grupo equifrecuente", {
  skip_if_not_installed("stringdist")
  pareja <- .indices_pareja()
  sola <- .indices_grupos(pareja, detectar_variantes_equifrecuentes = TRUE)
  antes <- .indices_grupos(
    c(pareja, rep("ALFA", 3L), rep("Alfa", 3L)),
    detectar_variantes_equifrecuentes = TRUE
  )
  despues <- .indices_grupos(
    c(pareja, rep("ZETA", 3L), rep("Zeta", 3L)),
    detectar_variantes_equifrecuentes = TRUE
  )
  expect_identical(antes$alcance, despues$alcance)
  expect_identical(
    unname(antes$grupos_equifrecuentes), unname(despues$grupos_equifrecuentes)
  )
  expect_identical(
    .indices_sin_clase(antes$grupos, c("ALFA", "Alfa")),
    .indices_sin_clase(despues$grupos, c("ZETA", "Zeta"))
  )
  # Y no es que los dos pierdan lo mismo: es lo que da la pareja sola.
  expect_identical(
    unname(antes$grupos_equifrecuentes), unname(sola$grupos_equifrecuentes)
  )
  expect_identical(antes$alcance$n_grupos_sin_variante_rara, 1L)
  expect_identical(
    antes$grupos_equifrecuentes[[1L]]$variantes, c("Montevideo", "Montevido")
  )
})

test_that("el grupo equifrecuente informa la pareja y no a su vecina", {
  skip_if_not_installed("stringdist")
  # Con `Canelones` entre la clase y la pareja, el indice corrido no caia en
  # la clase y el grupo se publicaba, pero con las formas de al lado:
  # `Canelones (49) / Montevideo (50)`.
  x <- c(.indices_pareja(), rep("ALFA", 3L), rep("Alfa", 3L),
         rep("Canelones", 49L))
  resultado <- .indices_grupos(x, detectar_variantes_equifrecuentes = TRUE)
  expect_length(resultado$grupos_equifrecuentes, 1L)
  grupo <- resultado$grupos_equifrecuentes[[1L]]
  expect_identical(grupo$variantes, c("Montevideo", "Montevido"))
  expect_identical(grupo$frecuencias, c(50L, 48L))
  expect_identical(grupo$clase_diferencia, "token_unico")
})

test_that("perfilar publica la pareja con una clase exacta delante", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    x = c(.indices_pareja(), rep("ALFA", 3L), rep("Alfa", 3L)),
    stringsAsFactors = FALSE
  )
  perfil_de <- function(...) {
    perfilar(
      datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
      fecha = as.POSIXct("2026-01-01", tz = "UTC"), ...
    )
  }
  # Por omision se publica la fila que declara la pareja no formada.
  omision <- perfil_de()
  cobertura <- omision$cobertura_diagnosticos[
    omision$cobertura_diagnosticos$diagnostico == "proximidad_vocabulario", ,
    drop = FALSE
  ]
  expect_true(any(grepl(
    "1 grupo de formas cercanas no se form", cobertura$motivo,
    fixed = TRUE, useBytes = TRUE
  )))
  # Con el detector encendido, el hallazgo nombra la pareja.
  encendido <- perfil_de(variantes_equifrecuentes_vocabulario = TRUE)
  hallazgo <- encendido$hallazgos[
    encendido$hallazgos$tipo_hallazgo == "variantes_equifrecuentes_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(
    hallazgo$evidencia, "[Montevideo (50) / Montevido (48)]", fixed = TRUE
  )
})

test_that("el filtro de formato lee el par que se comparo", {
  skip_if_not_installed("stringdist")
  # `a.m.` y `p.m.` aparecen cada uno en la mitad de las formas: son formato.
  # Con frecuencias desparejas, un par de meridianos que el filtro no
  # descarta forma grupo. Con la clase delante el filtro leia otro par, los
  # 23 pares de meridianos se publicaban y el estado pegado desaparecia.
  horas <- sprintf("%d:%02d", rep(1:12, each = 2L), rep(c(5L, 35L), 12L))
  x <- c(rep(paste(horas, "a.m."), 30L), rep(paste(horas, "p.m."), 5L),
         rep("7:05 p.m.", 20L), "7:05 p.m. Delayed")
  antes <- .indices_grupos(c(x, rep("#A", 3L), rep("#a", 3L)))
  despues <- .indices_grupos(c(x, rep("~A", 3L), rep("~a", 3L)))
  expect_identical(antes$alcance, despues$alcance)
  sin_clase <- .indices_sin_clase(antes$grupos, c("#A", "#a"))
  expect_identical(sin_clase, .indices_sin_clase(despues$grupos, c("~A", "~a")))
  expect_identical(
    lapply(sin_clase, `[[`, "variantes"),
    list(c("7:05 p.m.", "7:05 p.m. Delayed"))
  )
})

test_that("la ruta de edicion corta pasa por el mismo filtro", {
  skip_if_not_installed("stringdist")
  # `am AB` y `pm AB` estan a una edicion y fuera del umbral Jaro-Winkler, asi
  # que solo los compara la ruta de valores cortos; difieren en la marca de
  # formato, que esta en la mitad de las formas.
  codigos <- c("CD", "EF", "GH", "IJ", "KL", "MN", "OP", "QR", "ST", "UV")
  x <- c(rep("am AB", 1000L), rep("pm AB", 5L),
         rep(paste("am", codigos), 3L), rep(paste("pm", codigos), 3L))
  antes <- .indices_grupos(c(x, rep("#A", 3L), rep("#a", 3L)))
  despues <- .indices_grupos(c(x, rep("~A", 3L), rep("~a", 3L)))
  expect_identical(antes$alcance, despues$alcance)
  expect_length(.indices_sin_clase(antes$grupos, c("#A", "#a")), 0L)
  # Sin esta mitad la prueba pasaria con una ruta que no compara nada.
  expect_gt(antes$alcance$n_pares_descartados_formato, 0L)
})

test_that("la regla del limite lee las formas del candidato equifrecuente", {
  skip_if_not_installed("stringdist")
  # Una familia numerada desparejada deja la columna fuera del limite, y la
  # pareja sin numeros se juzga aparte. Esa decision mira si el candidato
  # tiene numeros, y con la clase delante tiene que mirar las unidades de sus
  # formas y no las que vienen despues. Sin nada entre la pareja y la familia
  # en el orden de bytes, un indice corrido cae justo en un `valor_n`.
  familia <- rep(paste0("valor_", seq_len(40L)), times = seq(60L, 21L, by = -1L))
  x <- c(familia, rep("Montevideo", 50L), rep("Montevido", 48L),
         rep("ALFA", 3L), rep("Alfa", 3L))
  resultado <- .indices_grupos(x, detectar_variantes_equifrecuentes = TRUE)
  expect_false(resultado$alcance$aplicable)
  expect_true(resultado$alcance$evaluado_aparte_sin_numeros)
  expect_identical(resultado$alcance$n_grupos_sin_variante_rara, 1L)
  expect_identical(
    unname(lapply(resultado$grupos_equifrecuentes, `[[`, "variantes")),
    list(c("Montevideo", "Montevido"))
  )
})
