# Un aborto con datos legitimos: `perfilar()` se caia cuando la tabla mezclaba
# columnas de estructura compuesta numericas. `.es_columna_aritmetica()` aceptaba
# cualquier `is.numeric`, y una matriz y un arreglo de otra dimension entraban al
# analisis de relaciones: `is.finite(x) & is.finite(y)` da "arreglos de dimension
# no compatibles". Con una sola matriz no abortaba y era peor de otra manera:
# publicaba una relacion aritmetica sobre una columna que el mismo perfil declara
# `tipo_compuesto_no_analizado`.

test_that("una tabla con matriz y arreglo de otra dimension se perfila sin abortar", {
  datos <- data.frame(num = c(1, 2, NA, 4))
  datos$m <- I(matrix(1:8, nrow = 4L))
  datos$a <- I(array(1:16, dim = c(4L, 2L, 2L)))

  perfil <- suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  expect_s3_class(perfil, "perfil")
  columnas <- as.data.frame(perfil$columnas)
  expect_equal(nrow(columnas), 3L)
  # Y las dos columnas compuestas siguen declarandose como lo que son.
  compuestas <- columnas[columnas$columna %in% c("m", "a"), , drop = FALSE]
  expect_true(all(as.character(compuestas$tipo_inferido) == "desconocido"))
  hallazgos <- as.data.frame(perfil$hallazgos)
  expect_true(all(
    c("m", "a") %in%
      hallazgos$columna[as.character(hallazgos$tipo_hallazgo) ==
                          "tipo_compuesto_no_analizado"]
  ))
  # Ninguna relacion aritmetica puede nombrar una columna con dimensiones.
  relaciones <- hallazgos[
    as.character(hallazgos$tipo_hallazgo) == "relacion_aritmetica_columnas", ,
    drop = FALSE
  ]
  if (nrow(relaciones)) {
    nombradas <- unlist(strsplit(as.character(relaciones$columna), ","))
    expect_false(any(trimws(nombradas) %in% c("m", "a")))
  }
})

test_that("la relacion aritmetica entre columnas de verdad sigue apareciendo", {
  # La mitad de control: la guarda excluye columnas con dimensiones, no el
  # analisis. Sin esto, apagar el analisis entero pasaria la prueba de arriba.
  set.seed(3)
  a <- sample(1:500, 60L)
  b <- sample(1:500, 60L)
  datos <- data.frame(a = a, b = b, total = a + b)
  hallazgos <- as.data.frame(suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))$hallazgos)
  expect_true(any(
    as.character(hallazgos$tipo_hallazgo) == "relacion_aritmetica_columnas"
  ))

  # Y el mismo control con una columna compuesta al lado: la relacion real se
  # sigue encontrando aunque la tabla traiga una matriz.
  con_matriz <- datos
  con_matriz$m <- I(matrix(seq_len(120L), nrow = 60L))
  hallazgos2 <- as.data.frame(suppressWarnings(perfilar(
    con_matriz, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))$hallazgos)
  expect_true(any(
    as.character(hallazgos2$tipo_hallazgo) == "relacion_aritmetica_columnas"
  ))
})

test_that("el camino por bloques elige las mismas columnas que el de memoria", {
  # La regla estaba escrita dos veces y la del camino por bloques era mas debil:
  # un `is.numeric` pelado, que acepta fechas, `integer64` y columnas con
  # dimensiones. La misma tabla recibia dos conjuntos de columnas segun por donde
  # se la perfilara.
  bloques <- getFromNamespace(".relaciones_aritmeticas_bloques", "lupa")
  predicado <- getFromNamespace(".es_columna_aritmetica", "lupa")

  datos <- data.frame(num = c(1, 2, NA, 4))
  datos$m <- I(matrix(1:8, nrow = 4L))
  datos$a <- I(array(1:16, dim = c(4L, 2L, 2L)))
  expect_false(predicado(datos$m))
  expect_false(predicado(datos$a))
  # Con una sola columna elegible no hay par que comparar: lista vacia.
  expect_length(bloques(datos), 0L)

  # Control: con dos columnas de verdad el gemelo sigue trabajando.
  reales <- data.frame(a = c(1, 2, 3, 4), b = c(2, 4, 6, 8))
  expect_gt(length(bloques(reales)), 0L)
})

test_that("un arreglo de tres dimensiones no publica cifras en otra unidad", {
  # La regla "esta columna tiene mas de una dimension" estaba escrita dos veces y
  # las dos versiones no coincidian: el ruteo del perfilado de columnas preguntaba
  # `is.matrix(x)`, que es FALSE para un arreglo de tres dimensiones. Sobre una
  # tabla de CUATRO filas, la columna de un arreglo 4x2x2 publicaba `n = 16` -los
  # elementos del arreglo-, `n_distintos = 16`, `minimo 1`, `maximo 16`, y de ahi
  # salia un hallazgo `posible_identificador`. Una cifra cuya unidad no es la fila,
  # publicada al lado del conteo de filas de la tabla.
  datos <- data.frame(num = c(1, 2, NA, 4))
  datos$a <- I(array(1:16, dim = c(4L, 2L, 2L)))

  perfil <- suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  columnas <- as.data.frame(perfil$columnas)
  fila <- columnas[columnas$columna == "a", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  # La unidad es la fila de la tabla, no el elemento del arreglo.
  expect_equal(fila$n[[1L]], nrow(datos))
  expect_identical(as.character(fila$tipo_inferido[[1L]]), "desconocido")
  for (metrica in c("n_distintos", "minimo", "maximo")) {
    if (metrica %in% names(fila)) {
      expect_true(is.na(fila[[metrica]][[1L]]), info = metrica)
    }
  }
  # Y ninguna afirmacion derivada de esas cifras.
  hallazgos <- as.data.frame(perfil$hallazgos)
  sobre_a <- hallazgos[!is.na(hallazgos$columna) & hallazgos$columna == "a", ,
                       drop = FALSE]
  expect_true(all(
    as.character(sobre_a$tipo_hallazgo) == "tipo_compuesto_no_analizado"
  ))

  # Control: una columna entera de verdad, en la misma tabla, sigue publicando
  # sus cifras y su hallazgo de identificador cuando corresponde.
  con_id <- data.frame(id = 1:60, valor = rep(c(1, 2, 3), 20L))
  columnas_id <- as.data.frame(suppressWarnings(perfilar(
    con_id, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))$columnas)
  fila_id <- columnas_id[columnas_id$columna == "id", , drop = FALSE]
  expect_equal(fila_id$n[[1L]], 60)
  expect_equal(fila_id$n_distintos[[1L]], 60)
})
