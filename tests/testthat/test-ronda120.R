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
  # `-888` no esta en la lista por omision -`-999` si, y ese lo informa
  # `faltantes_disfrazados`-, asi que es el que le toca a este diagnostico.
  expect_true(senalado(perfilar_valores(
    c(sample(0:100, 300L, TRUE), rep(-888L, 20L))
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
  # No es `error` -no hay nada que eliminar- pero tampoco `ok`: un ZWJ entre
  # `Juan` y `Perez` no cumple ninguna funcion y rompe las comparaciones. El
  # paquete no puede saber si la union corresponde a esa columna.
  expect_equal(severidad_de(solo_significativos), "sospechoso")

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
  # La errata `Mayp` entra al grupo por distancia de edicion, que la calcula
  # `stringdist`. Sin ese paquete el grupo se forma igual pero solo con las
  # variantes de escritura, y el caso que esta prueba mide -separar una errata
  # de una variante- no existe.
  skip_if_not_installed("stringdist")
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
               "2 de 3 variantes difieren de la forma dominante solo en")
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

test_that("una numeracion con centinela sigue siendo una numeracion", {
  # El centinela hunde la densidad y abre el salto de escala: un identificador
  # de 1 a 4000 con quince `9999` pasaba de densidad 0,626 a 0,250 y dejaba de
  # reconocerse, asi que se le corrian Benford y los limites de Tukey. Lo que
  # hay que mirar para decidir si es una numeracion es la columna sin el
  # centinela; el centinela se informa aparte, que es su propio diagnostico.
  set.seed(3)
  valores <- c(sort(sample(4000L, 2500L)), rep(9999L, 15L))
  perfil <- perfilar(
    data.frame(x = valores), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  fila <- perfil$columnas
  tipos <- as.character(perfil$hallazgos$tipo_hallazgo)

  expect_lt(fila$densidad_secuencia_entera, 0.5)
  expect_gt(fila$densidad_sin_centinela, 0.5)

  # Se reconoce la numeracion...
  expect_true("posible_identificador" %in% tipos)
  # ...no se le aplican pruebas de magnitud...
  expect_false("desviacion_benford" %in% tipos)
  expect_false("outliers" %in% tipos)
  # ...y el centinela se informa igual.
  expect_true("posible_centinela_numerico" %in% tipos)
})

test_that("la nota de escritura no afirma que sean la misma palabra", {
  # La comparacion ignora acentos, y en español el acento distingue palabras.
  # El mismo mecanismo que junta `Jose` con `José` -que si es la misma- junta
  # `papa` con `papá` y `ano` con `año`, que no lo son. Sin un diccionario del
  # idioma no se pueden separar, asi que el texto dice EN QUE DIFIEREN, que es
  # verificable, y no que sean lo mismo, que no se sabe.
  variantes <- lupa:::.variantes_de_escritura_vocabulario
  # Las que son la misma palabra.
  expect_equal(variantes(c("Jose", "José")), 1L)
  expect_equal(variantes(c("Penarol", "Peñarol")), 1L)
  # Las que no lo son, y colapsan igual: es el limite del metodo.
  expect_equal(variantes(c("papa", "papá")), 1L)
  expect_equal(variantes(c("ano", "año")), 1L)

  columna <- c(rep("San Jose de Mayo", 40L), rep("San José de Mayo", 9L))
  perfil <- perfilar(
    data.frame(localidad = columna), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  grupo <- perfil$hallazgos[
    as.character(perfil$hallazgos$tipo_hallazgo) ==
      "casi_duplicados_vocabulario", , drop = FALSE
  ]
  evidencia <- as.character(grupo$evidencia[[1L]])
  expect_match(evidencia, "difieren de la forma dominante solo en")
  # Y avisa del limite en vez de callarlo.
  expect_match(evidencia, "el acento puede cambiar la palabra")
})

test_that("el valor centinela no se publica en una columna protegida", {
  # `centinela_valor` publica un valor de celda, igual que el minimo o la moda,
  # y quedaba afuera de la proteccion: sobre una columna de documentos el perfil
  # mostraba `moda = "[valor protegido]"`, `minimo = NA` y `centinela_valor =
  # 9999`. La descripcion del hallazgo lo nombraba tambien, y asi llegaba hasta
  # el informe HTML. Que el valor sea casi seguro un centinela y no un documento
  # no cambia la regla: la proteccion no adivina cuales valores son inocentes.
  documentos <- c(seq(10000001, 10000100), rep(9999, 5))
  perfil <- perfilar(
    data.frame(documento = documentos), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  expect_true(perfil$columnas$dato_personal_protegido)
  expect_true(is.na(perfil$columnas$centinela_valor))
  # Las repeticiones si se informan: son un conteo, no un valor de celda.
  expect_equal(perfil$columnas$centinela_repeticiones, 5L)

  hallazgo <- perfil$hallazgos[
    as.character(perfil$hallazgos$tipo_hallazgo) ==
      "posible_centinela_numerico", , drop = FALSE
  ]
  if (nrow(hallazgo)) {
    expect_false(grepl("9999", as.character(hallazgo$descripcion[[1L]]),
                       fixed = TRUE))
    expect_false(grepl("9999", as.character(hallazgo$evidencia[[1L]]),
                       fixed = TRUE))
    expect_match(as.character(hallazgo$descripcion[[1L]]), "esta protegida")
  }
})

test_that("sin proteccion el valor centinela se dice, que para eso esta", {
  set.seed(5)
  perfil <- perfilar(
    data.frame(edad = c(sample(18:80, 200L, TRUE), rep(9999L, 12L))),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE
  )
  expect_false(isTRUE(perfil$columnas$dato_personal_protegido))
  expect_equal(perfil$columnas$centinela_valor, 9999)
  hallazgo <- perfil$hallazgos[
    as.character(perfil$hallazgos$tipo_hallazgo) ==
      "posible_centinela_numerico", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(as.character(hallazgo$descripcion[[1L]]), "9999")
})

test_that("un centinela ya declarado no se informa dos veces", {
  # `999` esta en la lista por omision, asi que `faltantes_disfrazados` ya lo
  # cuenta como ausencia con severidad `error`. El diagnostico nuevo decia sobre
  # la misma columna que "no se cuenta como ausencia porque no esta declarado",
  # que era falso: los dos hallazgos se contradecian.
  perfil <- perfilar(
    data.frame(x = c(1:100, rep(999, 5))), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  tipos <- as.character(perfil$hallazgos$tipo_hallazgo)
  expect_equal(perfil$columnas$n_faltantes_disfrazados, 5L)
  expect_true("faltantes_disfrazados" %in% tipos)
  expect_false("posible_centinela_numerico" %in% tipos)
  expect_true(is.na(perfil$columnas$centinela_valor))

  # Y uno que NO esta en la lista sigue informandose: para eso existe.
  set.seed(9)
  sin_declarar <- perfilar(
    data.frame(x = c(sample(18:80, 200L, TRUE), rep(8888L, 20L))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  expect_equal(sin_declarar$columnas$centinela_valor, 8888)
  expect_true("posible_centinela_numerico" %in%
                as.character(sin_declarar$hallazgos$tipo_hallazgo))
})

test_that("un hueco grande en proporcion al rango tambien es salto de escala", {
  # Puntajes del 1 al 13 con un 22 repetido veinte veces abren un hueco de 9
  # sobre un tipico de 1: no llega al factor y la columna quedaba clasificada
  # como numeracion, callando veinte valores extremos. Contra el rango, ese
  # hueco es 0,41, y ahi la separacion es limpia: las numeraciones reales no
  # pasan de 0,067.
  set.seed(1)
  perfil <- perfilar(
    data.frame(puntaje = c(sample(1:13, 400L, TRUE), rep(22L, 20L))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  fila <- perfil$columnas
  expect_gt(fila$densidad_secuencia_entera, 0.5)
  expect_true(fila$salto_de_escala_secuencia_entera)
  expect_true("outliers" %in% as.character(perfil$hallazgos$tipo_hallazgo))

  # Y una numeracion de verdad no se rompe por esto.
  set.seed(4)
  numeracion <- perfilar(
    data.frame(MotId = sort(sample(4557L, 3159L))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  expect_false(numeracion$columnas$salto_de_escala_secuencia_entera)
  expect_true("posible_identificador" %in%
                as.character(numeracion$hallazgos$tipo_hallazgo))
})

test_that("perfilar con do.call no rompe el informe", {
  # `deparse()` de una expresion larga devuelve varias lineas, y con `do.call`
  # la expresion es la tabla entera: el nombre salia con ocho elementos y
  # `reportar()` reventaba con "values must be length 1". Pasar los argumentos
  # en una lista es lo natural cuando se perfila en un bucle.
  opciones <- list(
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  perfil <- do.call(
    perfilar, c(list(data.frame(x = c(1:100, rep(999, 5)))), opciones)
  )
  expect_length(perfil$meta$nombre, 1L)

  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  expect_no_error(
    suppressMessages(reportar(perfil, archivo = archivo, sobrescribir = TRUE))
  )

  # El nombre de siempre se conserva.
  tabla <- data.frame(y = 1:20)
  normal <- perfilar(
    tabla, analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  expect_equal(normal$meta$nombre, "tabla")
})
