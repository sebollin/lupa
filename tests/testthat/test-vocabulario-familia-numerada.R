# El limite de proporcion del vocabulario mide el componente previo al filtro
# numerico: asi una familia numerada desparejada -`valor_1` ... `valor_40`- no
# pasa en pedazos despues de que el filtro la separa. Pero ese componente
# decidia tambien sobre los grupos SIN numeros de la misma columna, que el
# filtro nunca une con una forma con numeros, y una errata real quedaba callada
# porque la columna entera pasaba a "no aplica".
#
# Estas pruebas fijan las dos mitades: el grupo sin numeros se publica igual
# que si estuviera solo, y el grupo que la familia contiene sigue fuera.

# Fuera de CRAN: el archivo arma ocho perfiles y tarda unos 0,4 s (medido el
# 2026-11-06), por encima de los 0,2 s que se admiten en modo CRAN. Corre
# en la suite completa y en el CI, con NOT_CRAN=true.
testthat::skip_on_cran()

# Frecuencias estrictamente desparejas: con ellas la familia forma un solo
# componente antes del filtro (cada codigo cuelga del vecino mas frecuente).
.familia_desparejada <- function() {
  rep(paste0("valor_", seq_len(40L)), times = seq(60L, 21L, by = -1L))
}

.vocabulario_de <- function(x, ...) {
  perfil <- perfilar(
    data.frame(x = x, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    fecha = as.POSIXct("2026-01-01", tz = "UTC"), ...
  )
  list(
    hallazgos = perfil$hallazgos[
      perfil$hallazgos$tipo_hallazgo %in% c(
        "casi_duplicados_vocabulario", "variantes_equifrecuentes_vocabulario"
      ), , drop = FALSE
    ],
    cobertura = perfil$cobertura_diagnosticos[
      perfil$cobertura_diagnosticos$diagnostico == "proximidad_vocabulario", ,
      drop = FALSE
    ]
  )
}

.grupos_vocabulario <- function(x, ...) {
  lupa:::.grupos_casi_duplicados_vocabulario(
    x, lupa:::.resolver_normalizacion(FALSE), "x", ...
  )
}

test_that("una errata sin numeros no queda callada por la familia numerada", {
  skip_if_not_installed("stringdist")
  errata <- c(rep("Montevideo", 90L), rep("Montevido", 4L))
  familia <- .familia_desparejada()

  # Control de que el caso es el que se quiere: la familia sola no aplica, y
  # junto a la errata abarca mas de la mitad del vocabulario.
  sola <- .grupos_vocabulario(familia)
  expect_false(sola$alcance$aplicable)
  mezcla <- .grupos_vocabulario(c(familia, errata))
  expect_false(mezcla$alcance$aplicable)
  expect_gt(mezcla$alcance$proporcion_grupo_maximo, 0.9)

  expect_length(mezcla$grupos, 1L)
  expect_setequal(mezcla$grupos[[1L]]$variantes, c("Montevideo", "Montevido"))
  expect_true(mezcla$alcance$evaluado_aparte_sin_numeros)
  expect_equal(mezcla$alcance$tamano_grupo_maximo_sin_numeros, 2L)

  # Lo publicado es lo que la errata publica sola: el mismo grupo, con las
  # mismas frecuencias, la misma asimetria y el mismo origen.
  publicado <- .vocabulario_de(c(familia, errata))
  sin_familia <- .vocabulario_de(c(errata, rep("Canelones", 6L)))
  expect_equal(nrow(publicado$hallazgos), 1L)
  expect_equal(as.character(publicado$hallazgos$severidad), "sospechoso")
  grupo <- "[Montevideo (90) / Montevido (4)]; asimetria=22.5; origen=distancia"
  expect_match(publicado$hallazgos$evidencia, grupo, fixed = TRUE)
  expect_match(sin_familia$hallazgos$evidencia, grupo, fixed = TRUE)
  expect_equal(publicado$hallazgos$n_afectados, 2)
  expect_match(
    publicado$hallazgos$sugerencia, "Revisar las variantes", fixed = TRUE
  )
  expect_match(
    publicado$hallazgos$evidencia, "grupo_maximo sin numeros=2 (0.048)",
    fixed = TRUE
  )

  # Y la columna no dice "no aplica" a secas, que contradiria el hallazgo:
  # dice a que formas no aplica.
  expect_equal(nrow(publicado$cobertura), 1L)
  expect_match(
    publicado$cobertura$motivo,
    "no aplica a las formas con n\u00fameros", fixed = TRUE
  )
  expect_match(
    publicado$cobertura$motivo,
    "su grupo mayor abarca 0.048 del vocabulario", fixed = TRUE
  )
})

test_that("un grupo que la familia numerada contiene sigue sin aplicar", {
  skip_if_not_installed("stringdist")
  # `valro_7` es una errata de `valor_7`: tienen la misma firma numerica, el
  # filtro los deja juntos y el grupo tiene numeros. Lo decide el componente
  # previo al filtro, que es la familia entera.
  x <- c(.familia_desparejada(), rep("valro_7", 3L))
  resultado <- .grupos_vocabulario(x)
  # El grupo existe despues del filtro: si no, la prueba no probaria nada.
  expect_equal(resultado$alcance$tamano_grupo_maximo_numerico, 2L)
  expect_false(resultado$alcance$aplicable)
  expect_length(resultado$grupos, 0L)
  expect_false(isTRUE(resultado$alcance$evaluado_aparte_sin_numeros))

  publicado <- .vocabulario_de(x)
  expect_equal(nrow(publicado$hallazgos), 0L)
  expect_equal(nrow(publicado$cobertura), 1L)
  expect_match(
    publicado$cobertura$motivo,
    "del vocabulario y el diagn\u00f3stico no aplica.", fixed = TRUE
  )
  expect_false(grepl(
    "formas con n\u00fameros", publicado$cobertura$motivo, fixed = TRUE
  ))

  # Las dos a la vez: sale la errata sin numeros y no la de la familia.
  ambas <- .vocabulario_de(c(
    x, rep("Montevideo", 90L), rep("Montevido", 4L)
  ))
  expect_equal(nrow(ambas$hallazgos), 1L)
  expect_match(
    ambas$hallazgos$evidencia, "[Montevideo (90) / Montevido (4)]",
    fixed = TRUE
  )
  expect_false(grepl(
    "valro_7", ambas$hallazgos$evidencia, fixed = TRUE, useBytes = TRUE
  ))
  expect_match(ambas$hallazgos$evidencia, "grupos: 1,", fixed = TRUE)
})

test_that("una cadena sin numeros que abarca la columna no se rescata", {
  skip_if_not_installed("stringdist")
  # Diez codigos y una cadena de quince formas sin numeros: la cadena sola
  # abarca 15 de 25 formas, mas de la mitad, y el limite la suprime igual que
  # sin la familia. El rescate no afloja el limite: solo deja de mirar a la
  # familia para decidir sobre lo que no tiene numeros.
  zonas <- rep(paste0("Zona ", LETTERS[1:15]), times = c(100L, rep(4L, 14L)))
  codigos <- rep(paste0("valor_", seq_len(10L)), times = seq(40L, 31L, by = -1L))
  resultado <- .grupos_vocabulario(c(codigos, zonas))
  expect_false(resultado$alcance$aplicable)
  expect_equal(resultado$alcance$tamano_grupo_maximo, 15L)
  expect_equal(resultado$alcance$proporcion_grupo_maximo, 15 / 25)
  expect_length(resultado$grupos, 0L)
  expect_false(isTRUE(resultado$alcance$evaluado_aparte_sin_numeros))
  publicado <- .vocabulario_de(c(codigos, zonas))
  expect_equal(nrow(publicado$hallazgos), 0L)
  expect_match(
    publicado$cobertura$motivo,
    "del vocabulario y el diagn\u00f3stico no aplica.", fixed = TRUE
  )
})

test_that("el limite sin numeros mide contra el vocabulario entero", {
  skip_if_not_installed("stringdist")
  # El denominador del limite sin numeros es el vocabulario entero de la
  # columna, con las formas con numeros: el veredicto es el que la columna
  # daria si la familia no se encadenara. Con las formas sin numeros de
  # denominador, el veredicto dependeria de que la familia se encadene, y una
  # errata con muchas grafias quedaria callada junto a la familia mientras se
  # publica junto a codigos que no se encadenan.
  sin_encadenar <- rep(paste0("valor_", seq_len(40L)), each = 10L)
  # Una errata con once grafias: sola abarca la columna y no aplica.
  errata <- c(
    rep("Montevideo", 200L),
    rep(c("Montevido", "Montevdeo", "Montvideo", "Mntevideo", "Montevideoo",
          "Monteviedo", "Montevideu", "Montevidei", "Montevidao",
          "Montevidwo"), each = 3L)
  )
  sola <- .grupos_vocabulario(errata)
  expect_false(sola$alcance$aplicable)
  expect_length(sola$grupos, 0L)
  con_familia <- .grupos_vocabulario(c(.familia_desparejada(), errata))
  con_codigos <- .grupos_vocabulario(c(sin_encadenar, errata))
  # Control: una columna decide por la familia y la otra no.
  expect_false(con_familia$alcance$aplicable)
  expect_true(con_familia$alcance$evaluado_aparte_sin_numeros)
  expect_true(con_codigos$alcance$aplicable)
  expect_equal(con_familia$alcance$n_valores_evaluados, 51L)
  expect_equal(con_familia$alcance$tamano_grupo_maximo_sin_numeros, 11L)
  expect_equal(con_familia$alcance$proporcion_grupo_maximo_sin_numeros, 11 / 51)
  expect_length(con_familia$grupos, 1L)
  expect_length(con_familia$grupos[[1L]]$variantes, 11L)
  # La participacion de la variante depende de las filas de la columna; lo
  # demas del grupo es lo mismo.
  campos <- function(grupos) lapply(unname(grupos), `[`, c(
    "variantes", "frecuencias", "asimetria", "distancia_minima",
    "distancia_maxima", "clase_diferencia", "origen"
  ))
  expect_identical(campos(con_familia$grupos), campos(con_codigos$grupos))

  # La otra direccion, declarada: una cadena sin numeros que ocupa una
  # fraccion chica del vocabulario se publica junto a la familia como junto a
  # codigos que no se encadenan, que es la regla general del limite.
  zonas <- rep(paste0("Zona ", LETTERS[1:15]), times = c(100L, rep(4L, 14L)))
  zonas_familia <- .grupos_vocabulario(c(.familia_desparejada(), zonas))
  zonas_codigos <- .grupos_vocabulario(c(sin_encadenar, zonas))
  expect_false(zonas_familia$alcance$aplicable)
  expect_equal(zonas_familia$alcance$proporcion_grupo_maximo_sin_numeros, 15 / 55)
  expect_true(zonas_codigos$alcance$aplicable)
  expect_length(zonas_familia$grupos, 1L)
  expect_identical(campos(zonas_familia$grupos), campos(zonas_codigos$grupos))
})

test_that("las variantes equifrecuentes sin numeros siguen la misma regla", {
  skip_if_not_installed("stringdist")
  pareja <- c(rep("Montevideo", 50L), rep("Montevido", 48L))
  # Una sola regla para los dos tipos de grupo: con el detector encendido, la
  # pareja de frecuencias parejas sale igual que sin la familia al lado.
  con_familia <- .vocabulario_de(
    c(.familia_desparejada(), pareja),
    variantes_equifrecuentes_vocabulario = TRUE
  )
  sola <- .vocabulario_de(
    c(pareja, rep("Salto", 30L)), variantes_equifrecuentes_vocabulario = TRUE
  )
  tipo <- "variantes_equifrecuentes_vocabulario"
  expect_equal(sum(con_familia$hallazgos$tipo_hallazgo == tipo), 1L)
  expect_equal(sum(sola$hallazgos$tipo_hallazgo == tipo), 1L)
  expect_match(
    con_familia$hallazgos$evidencia[con_familia$hallazgos$tipo_hallazgo == tipo],
    "[Montevideo (50) / Montevido (48)]", fixed = TRUE
  )
  # Con el detector apagado, la fila que declara la pareja no formada tambien
  # se publica, como sin la familia.
  apagado <- .vocabulario_de(c(.familia_desparejada(), pareja))
  expect_true(any(grepl(
    "1 grupo de formas cercanas no se form\u00f3", apagado$cobertura$motivo,
    fixed = TRUE
  )))
})
