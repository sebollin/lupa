test_that("cada hallazgo declara sus conteos y su unidad", {
  perfil <- perfilar(
    data.frame(
      importe = c(999, 1, NA),
      constante = c("A", "A", "A"),
      stringsAsFactors = FALSE
    ),
    analizar_dependencias = FALSE
  )

  expect_true(all(c("n_evaluados", "n_afectados", "unidad_conteo") %in%
                    names(perfil$hallazgos)))
  expect_true(all(!is.na(perfil$hallazgos$unidad_conteo)))
  faltantes <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "faltantes_disfrazados", ,
    drop = FALSE
  ]
  expect_equal(faltantes$n_evaluados, 3)
  expect_equal(faltantes$n_afectados, 1)
  expect_equal(faltantes$unidad_conteo, "fila")
  constante <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "constante", ,
    drop = FALSE
  ]
  expect_equal(constante$n_evaluados, 3)
  expect_equal(constante$n_afectados, 3)
  expect_equal(constante$unidad_conteo, "fila")
})

test_that("mayusculas inconsistentes cuentan valores y trazan filas", {
  categoria <- c(
    rep("alto", 339L), rep("altO", 4L), rep("bajo", 332L),
    rep("BAJO", 6L), rep("medio", 319L)
  )
  perfil <- perfilar(
    data.frame(categoria = categoria, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "mayusculas_inconsistentes", ,
    drop = FALSE
  ]
  expect_equal(hallazgo$n_evaluados, 5)
  expect_equal(hallazgo$n_afectados, 4)
  expect_equal(hallazgo$unidad_conteo, "valor_distinto")
  expect_equal(hallazgo$trazabilidad[[1L]]$total, 681)
  expect_setequal(hallazgo$trazabilidad[[1L]]$indices_fila, c(1:343, 344:681))
})

test_that("normalizacion Unicode cuenta valores y traza filas", {
  skip_if_not_installed("stringi")
  valores <- c(rep("caf\u00e9", 300L), rep("cafe\u0301", 305L))
  perfil <- perfilar(
    data.frame(nombre = valores, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "normalizacion_unicode", ,
    drop = FALSE
  ]
  expect_equal(hallazgo$n_evaluados, 2)
  expect_equal(hallazgo$n_afectados, 2)
  expect_equal(hallazgo$unidad_conteo, "valor_distinto")
  expect_equal(hallazgo$trazabilidad[[1L]]$total, 605)
  expect_setequal(hallazgo$trazabilidad[[1L]]$indices_fila, seq_len(605L))
})

test_that("las filas duplicadas cuentan participantes y dejan excedentes en evidencia", {
  datos <- data.frame(
    id = c(rep(1, 5L), rep(2, 5L)),
    valor = c(rep("A", 5L), rep("B", 5L))
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "filas_duplicadas", , drop = FALSE
  ]
  expect_equal(hallazgo$n_evaluados, 10)
  expect_equal(hallazgo$n_afectados, 10)
  expect_equal(hallazgo$unidad_conteo, "fila")
  expect_match(hallazgo$evidencia, "10 filas en grupos duplicados (8 excedentes)",
               fixed = TRUE)
  expect_equal(hallazgo$trazabilidad[[1L]]$total, 10)
})

test_that("una constante de listas declara que no pudo contar afectados", {
  datos <- data.frame(x = seq_len(20L))
  datos$compuesta <- I(replicate(20L, list(1:2), simplify = FALSE))
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$columna == "compuesta" &
      perfil$hallazgos$tipo_hallazgo == "constante", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_true(is.na(hallazgo$n_afectados))
  expect_equal(hallazgo$n_evaluados, 20)
  expect_equal(hallazgo$trazabilidad[[1L]]$estado, "no_disponible")
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "constante" &
      perfil$cobertura_diagnosticos$columna == "compuesta", , drop = FALSE
  ]
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$motivo, "lista", ignore.case = TRUE)
})

test_that("los hallazgos estructurales declaran unidades distintas", {
  datos <- data.frame(a = c(1, 1), b = c(1, 1), check.names = FALSE)
  names(datos) <- c("a", "a")
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  duplicadas <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "columnas_duplicadas", ,
    drop = FALSE
  ]
  expect_true(nrow(duplicadas) >= 1)
  expect_equal(duplicadas$unidad_conteo, "columna")
  expect_equal(duplicadas$n_evaluados, 2)
  expect_equal(duplicadas$n_afectados, 2)
})

test_that("los hallazgos de pares cuentan pares comparados", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = c("Ana Perez", "Ana Peres"),
    domicilio = c("Calle 1", "Calle 1")
  )
  perfil <- perfilar(
    datos,
    analizar_dependencias = FALSE,
    duplicados_aproximados = list(
      columnas = c("nombre", "domicilio"),
      estrategia = "teselas",
      max_resultados = Inf
    )
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "duplicados_aproximados", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1)
  expect_equal(hallazgo$unidad_conteo, "par")
  expect_equal(hallazgo$n_evaluados, 1)
  expect_equal(hallazgo$n_afectados, 1)
})

test_that("las propiedades de columna cuentan columnas, no filas", {
  datos <- datos_operativos
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  tipos <- c("alta_cardinalidad", "posible_identificador",
             "tipo_declarado_distinto")
  hallazgos <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo %in% tipos, , drop = FALSE
  ]
  expect_true(nrow(hallazgos) > 0)
  expect_true(all(hallazgos$unidad_conteo == "columna"))
  expect_true(all(hallazgos$n_evaluados == 1))
  expect_true(all(hallazgos$n_afectados == 1))
})

test_that("las fechas partidas cuentan las columnas involucradas", {
  datos <- data.frame(
    fecha_anio = c(2020, 2021, 2022),
    fecha_mes = c(1, 2, 3), fecha_dia = c(3, 28, 15)
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "fecha_partida_columnas", , drop = FALSE
  ]
  expect_equal(hallazgo$unidad_conteo, "columna")
  expect_equal(hallazgo$n_evaluados, 3)
  expect_equal(hallazgo$n_afectados, 3)
})

test_that("patron_raro conserva el total de valores afectados y sus filas", {
  datos <- data.frame(cod = c(rep("USR-001", 40L), "S/D", "MAL-5"))
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "patron_raro", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1)
  expect_equal(hallazgo$n_afectados, 2)
  traza <- hallazgo$trazabilidad[[1L]]
  expect_equal(traza$total, 2)
  expect_equal(traza$indices_fila, c(41L, 42L))
  expect_equal(traza$estado, "disponible")
  expect_match(
    hallazgo$evidencia,
    "proporcion_dominante=0.952",
    fixed = TRUE
  )
})

test_that("patron_raro declara cuando no hay dominante suficiente", {
  direcciones <- c(
    sprintf("Av. 18 de Julio %04d", seq_len(936L)),
    sprintf("Av 18 de Julio %04d", seq_len(500L)),
    sprintf("18 de Julio %04d", seq_len(500L)),
    rep("SIN NUMERO", 64L)
  )
  perfil <- perfilar(
    data.frame(direccion = direcciones, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  expect_false("patron_raro" %in% as.character(perfil$hallazgos$tipo_hallazgo))
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "patron_raro" &
      perfil$cobertura_diagnosticos$columna == "direccion", , drop = FALSE
  ]
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$motivo, "0.468", fixed = TRUE)
  expect_match(cobertura$motivo, "umbral_patron_dominante=0.500", fixed = TRUE)
  expect_match(cobertura$como_resolverlo, "umbral_patron_dominante", fixed = TRUE)
})

test_that("la trazabilidad de patrones muestreados declara su alcance", {
  valores <- rep("AA-111", 2000L)
  valores[c(1980L, 2000L)] <- c("S/D", "MAL-5")
  perfil <- perfilar(
    data.frame(cod = valores), muestra = 100L,
    analizar_dependencias = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "patron_raro", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1)
  expect_equal(hallazgo$n_evaluados, 100)
  expect_equal(hallazgo$n_afectados, 2)
  expect_equal(hallazgo$trazabilidad[[1L]]$alcance, "muestra_patrones")
  expect_equal(hallazgo$trazabilidad[[1L]]$indices_fila, c(1980L, 2000L))
})

test_that("patron_raro separa patrones raros de patrones intermedios", {
  valores <- c(
    rep("AL", 880L), rep("uAL", 38L), rep("AiL", 22L), rep("L", 59L)
  )
  perfil <- perfilar(
    data.frame(state = valores), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  patrones <- perfil$patrones$state
  expect_equal(attr(patrones, "n_patrones_distintos"), 4L)
  expect_equal(attr(patrones, "n_patrones_raros"), 2L)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "patron_raro", , drop = FALSE
  ]
  expect_equal(hallazgo$n_afectados, 60)
  expect_equal(hallazgo$trazabilidad[[1L]]$alcance, "completo")
  expect_equal(hallazgo$trazabilidad[[1L]]$total, 60)
  expect_equal(hallazgo$trazabilidad[[1L]]$indices_fila, 881:940)
  expect_match(
    hallazgo$evidencia,
    "patrones_no_dominantes_excluidos_por_umbral=59 filas",
    fixed = TRUE
  )
  expect_equal(nrow(perfil$cobertura_diagnosticos), 0L)
})

test_that("la evidencia de patron_raro muestra filas intermedias excluidas", {
  valores <- c(
    rep("ABC123", 1840L), rep("ABC-123", 128L),
    rep("ABC/123", 8L), rep("ABC 123", 8L), rep("ABC_123", 8L),
    rep("ABC.123", 8L)
  )
  perfil <- perfilar(
    data.frame(codigo = valores, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "patron_raro", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(hallazgo$n_afectados, 32)
  expect_match(hallazgo$evidencia, "proporcion_dominante=0.920", fixed = TRUE)
  expect_match(
    hallazgo$evidencia,
    "patrones_no_dominantes_excluidos_por_umbral=128 filas",
    fixed = TRUE
  )
})

test_that("la presentacion de patrones raros no recorta la trazabilidad", {
  raros <- c("x", "x1", "1x", "x-x", "1", "X-X", "x_x")
  valores <- c(rep("AB", 90L), raros)
  perfil <- perfilar(
    data.frame(codigo = valores), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  patrones <- perfil$patrones$codigo
  expect_equal(attr(patrones, "n_patrones_raros"), 7L)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "patron_raro", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(hallazgo$n_afectados, 7)
  traza <- hallazgo$trazabilidad[[1L]]
  expect_equal(traza$estado, "disponible")
  expect_equal(traza$alcance, "completo")
  esperados <- which(valores %in% raros)
  expect_equal(traza$total, length(esperados))
  expect_setequal(traza$indices_fila, esperados)
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "patron_raro", , drop = FALSE
  ]
  expect_equal(nrow(cobertura), 0L)
})

test_that("el alcance conserva muestreo y recorte de patrones", {
  raros <- c("x", "x1", "1x", "x-x", "1", "X-X", "x_x")
  valores <- rep("AB", 200L)
  valores[seq(1L, 13L, by = 2L)] <- raros
  perfil <- perfilar(
    data.frame(codigo = valores), muestra = 100L,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "patron_raro", , drop = FALSE
  ]
  expect_equal(hallazgo$n_evaluados, 100)
  expect_equal(hallazgo$n_afectados, 7)
  expect_equal(
    hallazgo$trazabilidad[[1L]]$alcance,
    "muestra_patrones"
  )
  expect_equal(nrow(perfil$cobertura_diagnosticos), 0L)
})

test_that("diez patrones raros conservan todas sus filas", {
  base <- rep("ABC0001", 2000L)
  simbolos <- c("#", "_", "/", "-", ".", ":", ";", "?", "=", "%")
  indices <- seq_len(200L)
  base[indices] <- rep(paste0(simbolos, "ABC0001"), each = 20L)
  perfil <- perfilar(
    data.frame(codigo = base), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE,
    max_filas_hallazgo = Inf
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "patron_raro" &
      as.character(perfil$hallazgos$severidad) != "ok", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(hallazgo$n_afectados, 200)
  traza <- hallazgo$trazabilidad[[1L]]
  expect_equal(traza$alcance, "completo")
  expect_equal(traza$total, 200)
  expect_setequal(traza$indices_fila, indices)
  expect_equal(traza$n_patrones_raros, 10L)
  expect_equal(traza$n_patrones_raros_trazabilidad, 10L)
  expect_equal(traza$limite_patrones_raros_trazabilidad, 5000L)
  expect_equal(nrow(perfil$cobertura_diagnosticos), 0L)
})

test_that("la traza de vocabulario prioriza formas no dominantes", {
  skip_if_not_installed("stringdist")
  n <- 5000L
  erratas <- c(seq_len(10L), 2001:2015)
  ciudad <- rep("Montevideo", n)
  ciudad[erratas] <- rep(
    c("Montevido", "Montevideoo", "Motevideo", "Montevideo ",
      "MONTEVIDEO"), length.out = length(erratas)
  )
  perfil <- perfilar(
    data.frame(ciudad = ciudad), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, max_filas_hallazgo = 1000L
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario" &
      as.character(perfil$hallazgos$severidad) != "ok", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  traza <- hallazgo$trazabilidad[[1L]]
  expect_equal(traza$total, n)
  expect_equal(traza$mostrados, 1000L)
  expect_true(all(ciudad[traza$indices_fila[seq_len(25L)]] != "Montevideo"))
  expect_true(all(ciudad[traza$indices_fila[26:1000]] == "Montevideo"))
  expect_equal(traza$mostrados_formas_variantes, 25L)
  expect_equal(traza$mostrados_formas_dominantes, 975L)
  expect_match(hallazgo$evidencia, "975 formas dominantes", fixed = TRUE)
})

test_that("la trazabilidad es uniforme, acotada y no se imprime como indices", {
  datos <- data.frame(x = rep(1, 2005L))
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, max_filas_hallazgo = 10L
  )
  constante <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "constante", , drop = FALSE
  ]
  expect_equal(nrow(constante), 1)
  traza <- constante$trazabilidad[[1L]]
  expect_equal(traza$estado, "truncada")
  expect_equal(traza$total, 2005)
  expect_equal(traza$mostrados, 10)
  expect_true(traza$truncado)
  expect_equal(length(traza$indices_fila), 10)

  ruta <- reportar(perfil)
  html <- paste(readLines(ruta, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  unlink(ruta)
  expect_false(grepl("indices_fila", html, fixed = TRUE))
  expect_false(grepl("1, 2, 3, 4", html, fixed = TRUE))
  expect_match(html, "filas mostradas")
})
