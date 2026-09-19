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

test_that("las matrices y arreglos de texto y numero se perfilan sin abortar", {
  datos <- data.frame(num = 1:4, texto = letters[1:4])
  datos$matriz_num <- I(matrix(1:8, nrow = 4L))
  datos$matriz_texto <- I(matrix(letters[1:8], nrow = 4L))
  datos$arreglo_num <- I(array(1:16, dim = c(4L, 2L, 2L)))
  datos$arreglo_texto <- I(array(letters[1:16], dim = c(4L, 2L, 2L)))

  perfil <- expect_no_error(suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    ausencia_estructural = FALSE
  )))
  columnas <- as.data.frame(perfil$columnas)
  compuestas <- c("matriz_num", "matriz_texto", "arreglo_num", "arreglo_texto")
  filas <- columnas[columnas$columna %in% compuestas, , drop = FALSE]
  expect_equal(nrow(filas), length(compuestas))
  expect_true(all(filas$n == nrow(datos)))
  expect_true(all(as.character(filas$tipo_inferido) == "desconocido"))
  expect_true(all(
    as.character(filas$estado_resumen_cuantitativo) ==
      "tipo_compuesto_no_analizado"
  ))

  hallazgos <- as.data.frame(perfil$hallazgos)
  compuestos_hallados <- hallazgos[
    !is.na(hallazgos$columna) & hallazgos$columna %in% compuestas, ,
    drop = FALSE
  ]
  expect_equal(nrow(compuestos_hallados), length(compuestas))
  expect_true(all(
    as.character(compuestos_hallados$tipo_hallazgo) ==
      "tipo_compuesto_no_analizado"
  ))
})

test_that("ninguna variante compuesta publica elementos como valores por fila", {
  compuestas <- list(
    matriz_num = I(matrix(1:8, nrow = 4L)),
    matriz_texto = I(matrix(letters[1:8], nrow = 4L)),
    arreglo_num = I(array(1:16, dim = c(4L, 2L, 2L))),
    arreglo_texto = I(array(letters[1:16], dim = c(4L, 2L, 2L)))
  )
  campos_ambiguos <- c(
    "n_distintos", "frecuencia_moda", "minimo", "maximo", "media",
    "mediana", "desvio", "n_faltantes", "n_ceros", "n_negativos"
  )

  for (nombre in names(compuestas)) {
    datos <- data.frame(num = 1:4, texto = letters[1:4])
    datos$compuesta <- compuestas[[nombre]]
    perfil <- suppressWarnings(perfilar(
      datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
      ausencia_estructural = FALSE
    ))
    fila <- as.data.frame(perfil$columnas)
    fila <- fila[fila$columna == "compuesta", , drop = FALSE]
    expect_equal(fila$n[[1L]], nrow(datos), info = nombre)
    for (campo in intersect(campos_ambiguos, names(fila))) {
      expect_true(is.na(fila[[campo]][[1L]]), info = paste(nombre, campo))
    }
  }
})

test_that("las columnas compuestas no reciben claves, identificadores, Benford ni dependencias", {
  chicos <- data.frame(id = 1:4, texto = letters[1:4])
  chicos$matriz_num <- I(matrix(1:8, nrow = 4L))
  chicos$matriz_texto <- I(matrix(letters[1:8], nrow = 4L))
  chicos$arreglo_num <- I(array(1:16, dim = c(4L, 2L, 2L)))
  chicos$arreglo_texto <- I(array(letters[1:16], dim = c(4L, 2L, 2L)))
  compuestas <- c("matriz_num", "matriz_texto", "arreglo_num", "arreglo_texto")

  claves <- detectar_claves(chicos)
  expect_false(any(compuestas %in% as.character(claves$columnas)))
  sugerencias <- sugerir_clave(chicos)
  expect_false(any(compuestas %in% as.character(sugerencias$columna)))

  dependencias <- detectar_dependencias(
    chicos, min_observaciones = 2L, incluir_claves = TRUE
  )
  if (nrow(dependencias)) {
    expect_false(any(compuestas %in% as.character(dependencias$determinante)))
    expect_false(any(compuestas %in% as.character(dependencias$dependiente)))
  }

  asociaciones <- detectar_asociaciones(chicos, umbral = 0)
  if (nrow(asociaciones)) {
    expect_false(any(compuestas %in% as.character(asociaciones$columna_1)))
    expect_false(any(compuestas %in% as.character(asociaciones$columna_2)))
  }

  valores <- rep(c(1.1, 10.1, 100.1, 1000.1, 10000.1), 48L)
  grandes <- data.frame(
    monto = valores[seq_len(60L)], texto = rep(letters[1:3], 20L)
  )
  grandes$compuesta <- I(array(valores, dim = c(60L, 2L, 2L)))
  perfil <- suppressWarnings(perfilar(
    grandes, analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    ausencia_estructural = FALSE
  ))
  hallazgos <- as.data.frame(perfil$hallazgos)
  sobre_compuesta <- hallazgos[
    !is.na(hallazgos$columna) & hallazgos$columna == "compuesta", ,
    drop = FALSE
  ]
  expect_true(all(as.character(sobre_compuesta$tipo_hallazgo) ==
                    "tipo_compuesto_no_analizado"))
  expect_false(any(
    as.character(hallazgos$tipo_hallazgo) == "desviacion_benford" &
      as.character(hallazgos$columna) == "compuesta"
  ))
  expect_true("monto" %in% names(perfil$meta$benford$resultados))
  expect_false("compuesta" %in% names(perfil$meta$benford$resultados))
})

test_that("un arreglo de una dimension conserva sus diagnosticos y cifras", {
  una_dimension <- I(array(seq_len(60L), dim = 60L))
  datos <- data.frame(
    grupo = rep(1:3, each = 20L), texto = rep(letters[1:3], each = 20L)
  )
  datos$una_dimension <- una_dimension

  perfil <- suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    ausencia_estructural = FALSE
  ))
  columnas <- as.data.frame(perfil$columnas)
  fila <- columnas[columnas$columna == "una_dimension", , drop = FALSE]
  expect_equal(fila$n[[1L]], 60L)
  expect_equal(fila$n_distintos[[1L]], 60L)
  expect_equal(fila$minimo[[1L]], 1)
  expect_equal(fila$maximo[[1L]], 60)
  expect_equal(as.character(fila$tipo_inferido[[1L]]), "entero")
  expect_true(any(
    as.character(perfil$hallazgos$tipo_hallazgo) == "posible_identificador" &
      as.character(perfil$hallazgos$columna) == "una_dimension"
  ))

  claves <- detectar_claves(data.frame(una_dimension = una_dimension))
  expect_true("una_dimension" %in% as.character(claves$columnas))
  sugerencias <- sugerir_clave(data.frame(una_dimension = una_dimension))
  expect_true("una_dimension" %in% as.character(sugerencias$columna))
  dependencias <- detectar_dependencias(
    data.frame(grupo = datos$grupo, una_dimension = una_dimension),
    min_observaciones = 2L, incluir_claves = TRUE
  )
  expect_true(any(as.character(dependencias$determinante) == "una_dimension"))
  asociaciones <- detectar_asociaciones(
    data.frame(una_dimension = una_dimension, doble = 2 * una_dimension),
    umbral = 0
  )
  expect_true(nrow(asociaciones) >= 1L)
  expect_true(any(as.character(asociaciones$columna_1) == "una_dimension"))
  expect_true("una_dimension" %in% names(perfil$meta$benford$resultados))
})

test_that("el motivo de Benford nombra el hecho y no una disyuncion", {
  # La cobertura publicaba "parece un identificador (tipo_inferido,
  # posible_identificador o secuencia correlativa)": tres alternativas sin decir
  # cual. Medido sobre `c(1:2000, rep(500, 500))`, dos de las tres eran FALSAS en
  # esa misma salida -el perfil no publicaba `posible_identificador`, publicaba
  # `valor_concentrado`-, asi que la razon publicada para excluir la columna
  # invocaba una propiedad que el propio perfil negaba en la misma corrida.
  tabla <- data.frame(
    id = seq_len(2500L), monto = c(1:2000, rep(500, 500))
  )
  perfil <- suppressWarnings(perfilar(
    tabla, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  cobertura <- as.data.frame(perfil$cobertura_diagnosticos)
  fila <- cobertura[!is.na(cobertura$columna) & cobertura$columna == "monto", ,
                    drop = FALSE]
  skip_if(!nrow(fila), "la cobertura no trae la columna monto")
  motivos <- as.character(fila$motivo)
  benford <- motivos[grepl("Benford", motivos, fixed = TRUE)]
  skip_if(!length(benford), "Benford no declaro precondiciones para monto")

  # El motivo nombra el hecho: la secuencia correlativa.
  expect_match(benford[[1L]], "secuencia correlativa")
  # Y ya no publica la disyuncion con las tres alternativas.
  expect_false(grepl("tipo_inferido, posible_identificador o", benford[[1L]],
                     fixed = TRUE))
  # La razon nombrada tiene que ser verdadera en esta salida: el perfil NO
  # publica `posible_identificador` para esta columna, asi que el motivo no puede
  # invocarlo.
  hallazgos <- as.data.frame(perfil$hallazgos)
  publico_identificador <- any(
    !is.na(hallazgos$columna) & hallazgos$columna == "monto" &
      as.character(hallazgos$tipo_hallazgo) == "posible_identificador"
  )
  expect_false(publico_identificador)
  expect_false(grepl("posible_identificador", benford[[1L]], fixed = TRUE))

  # Control: una columna donde el hecho SI es el otro -el perfil publica
  # `posible_identificador`- nombra ese hecho y no el de la secuencia.
  fila_id <- cobertura[!is.na(cobertura$columna) & cobertura$columna == "id", ,
                       drop = FALSE]
  if (nrow(fila_id)) {
    motivos_id <- as.character(fila_id$motivo)
    benford_id <- motivos_id[grepl("Benford", motivos_id, fixed = TRUE)]
    if (length(benford_id)) {
      expect_match(benford_id[[1L]], "posible_identificador")
    }
  }
})
