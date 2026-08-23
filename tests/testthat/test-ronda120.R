# Cruces medidos contra datos con la respuesta conocida.

test_that("un valor centinela se reconoce por tres senales y no por su forma", {
  # `9999` es una edad imposible y un codigo postal valido, asi que la lista de
  # `sentinelas_numericos` no lo trae por omision y hace bien. Lo que si lo
  # distingue es que cumpla las tres a la vez: extremo, repetido y con forma de
  # centinela. Medido sobre ocho columnas con la respuesta conocida.
  perfilar_valores <- function(v) {
    perfilar(
      data.frame(x = v), analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
    )
  }
  senalado <- function(perfil) {
    "posible_centinela_numerico" %in%
      as.character(perfil$hallazgos$tipo_hallazgo)
  }

  set.seed(5)
  # Son centinelas.
  expect_true(senalado(perfilar_valores(
    c(sample(18:80, 200L, TRUE), rep(9999L, 12L))
  )))
  expect_true(senalado(perfilar_valores(
    c(sample(0:100, 300L, TRUE), rep(-999L, 20L))
  )))

  # No lo son, y cada uno falla una senal distinta.
  # El codigo postal 9999 se repite pero no es extremo en su columna.
  expect_false(senalado(perfilar_valores(
    c(sample(1000:9998, 200L, TRUE), rep(9999L, 30L))
  )))
  # El ano 1999 se repite y es extremo, pero no tiene forma de centinela.
  expect_false(senalado(perfilar_valores(
    c(sample(1990:2020, 200L, TRUE), rep(1999L, 40L))
  )))
  # El monto real de 9999 tiene la forma pero no se repite.
  expect_false(senalado(perfilar_valores(
    c(round(stats::rlnorm(200L, 7, 1)), 9999)
  )))
})

test_that("el centinela se informa aparte de los faltantes declarados", {
  # No se cuenta como ausencia: se dice que tiene la forma de serlo, y la
  # accion que se ofrece es agregarlo a la lista.
  set.seed(5)
  perfil <- perfilar(
    data.frame(edad = c(sample(18:80, 200L, TRUE), rep(9999L, 12L))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  expect_equal(perfil$columnas$n_faltantes_disfrazados, 0L)
  expect_equal(perfil$columnas$centinela_valor, 9999)
  expect_equal(perfil$columnas$centinela_repeticiones, 12L)

  hallazgo <- perfil$hallazgos[
    as.character(perfil$hallazgos$tipo_hallazgo) ==
      "posible_centinela_numerico", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(as.character(hallazgo$sugerencia[[1L]]),
               "sentinelas_numericos")
})

test_that("el costo de convertir un tipo se dice, no se deja implicito", {
  # La proporcion compatible se calculaba, se imprimia y no decidia: el
  # hallazgo sugeria "convertir" igual con 0,98 que con 0,85, y la segunda
  # conversion deja quince de cada cien filas en NA.
  evidencia_de <- function(compatibles, basura) {
    v <- c(as.character(seq_len(compatibles)), rep("s/d", basura))
    perfil <- perfilar(
      data.frame(x = v), analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
    )
    hallazgo <- perfil$hallazgos[
      as.character(perfil$hallazgos$tipo_hallazgo) == "tipo_declarado_distinto",
      , drop = FALSE
    ]
    if (!nrow(hallazgo)) return(NA_character_)
    as.character(hallazgo$evidencia[[1L]])
  }

  expect_match(evidencia_de(190L, 10L), "10 de 200 valores en NA")
  expect_match(evidencia_de(170L, 30L), "30 de 200 valores en NA")
})

test_that("una union invisible con significado no es un error", {
  # Los tres subtotales estaban calculados y ninguno se consultaba, asi que una
  # columna cuyo unico invisible es un ZWJ -que une emojis o ligaduras, y es
  # parte del texto- se marcaba con la severidad mas alta.
  zwj <- "‍"
  bom <- "﻿"
  perfilar_texto <- function(v) {
    perfilar(
      data.frame(x = v), analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
    )
  }
  severidad_de <- function(perfil) {
    hallazgo <- perfil$hallazgos[
      as.character(perfil$hallazgos$tipo_hallazgo) == "controles_invisibles", ,
      drop = FALSE
    ]
    if (!nrow(hallazgo)) return(NA_character_)
    as.character(hallazgo$severidad[[1L]])
  }

  solo_significativos <- perfilar_texto(
    c(paste0("emoji", zwj, "compuesto"), rep("simple", 9L))
  )
  expect_equal(solo_significativos$columnas$n_invisibles_significativos, 1L)
  expect_equal(solo_significativos$columnas$n_invisibles_eliminables, 0L)
  expect_equal(severidad_de(solo_significativos), "ok")

  # Con basura de transporte sigue siendo un error: ahi si hay algo que sacar.
  con_transporte <- perfilar_texto(
    c(paste0(bom, "Ana"), rep("Juan", 9L))
  )
  expect_gt(con_transporte$columnas$n_invisibles_eliminables, 0L)
  expect_equal(severidad_de(con_transporte), "error")
})

test_that("dentro de un grupo se separa la escritura de la errata", {
  # Son dos arreglos distintos: normalizar la columna entera, o corregir las
  # filas. La distancia de edicion no los separa -medido sobre 1.718 pares que
  # un humano normalizo a mano, las variantes de escritura estan MAS lejos
  # (2,29) que las erratas (1,56)-. Lo que si los separa es si colapsan al
  # quitar caja, acentos y puntuacion.
  columna <- c(
    rep("San Jose de Mayo", 40L),
    rep("SAN JOSE DE MAYO", 12L),
    rep("San José de Mayo", 9L),
    rep("San Jose de Mayp", 4L)
  )
  perfil <- perfilar(
    data.frame(localidad = columna), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  grupo <- perfil$hallazgos[
    as.character(perfil$hallazgos$tipo_hallazgo) ==
      "casi_duplicados_vocabulario", , drop = FALSE
  ]
  expect_equal(nrow(grupo), 1L)
  # De las tres variantes, dos son la dominante escrita distinto y una no.
  expect_match(as.character(grupo$evidencia[[1L]]),
               "2 de 3 variantes son la forma dominante escrita distinto")
})

test_that("la funcion que separa escritura de contenido no se equivoca", {
  # Los casos que decidieron el diseno.
  variantes <- lupa:::.variantes_de_escritura_vocabulario
  expect_equal(variantes(c("San Jose", "SAN JOSE")), 1L)
  expect_equal(variantes(c("San Jose", "San José")), 1L)
  expect_equal(variantes(c("San Jose", "San-Jose")), 1L)
  expect_equal(variantes(c("San Jose", "San Josa")), 0L)
  expect_equal(variantes(c("Montevideo", "Montevido")), 0L)
  expect_equal(variantes(c("uno", "UNO", "Un0")), 1L)
  expect_equal(variantes("sola"), 0L)
})
