datos_pares_aproximados <- function() {
  data.frame(
    nombre = c("Juan Pérez", "Juan Peres", "Ana Silva", "Luis Díaz"),
    domicilio = c(
      "Calle 18 de Julio 123", "Calle 18 de Julio 123",
      "Rambla 1", "Camino del Prado 44"
    ),
    stringsAsFactors = FALSE
  )
}

test_that("el resultado siempre declara el alcance", {
  resultado <- lupa:::.vacio_duplicados_aproximados(
    100L, "nombre", "jw", 0.15, 10L, 20L, 5L,
    disponible = FALSE, razon = "paquete ausente"
  )
  expect_false(resultado$disponible)
  expect_equal(resultado$alcance$n_pares_posibles, 4950)
  expect_equal(resultado$alcance$n_pares_comparados, 0)
  expect_equal(resultado$alcance$n_pares_sin_comparar, 4950)
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(resultado, archivo = archivo)
  html <- paste(readLines(archivo, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "No se ejecutó la comparación aproximada", fixed = TRUE)
  expect_match(html, "paquete ausente", fixed = TRUE)
})

test_that("sin stringdist la degradacion es explicita", {
  local_mocked_bindings(
    .stringdist_disponible = function() FALSE,
    .package = "lupa"
  )
  resultado <- detectar_duplicados_aproximados(datos_pares_aproximados())
  expect_false(resultado$disponible)
  expect_match(resultado$razon, "stringdist", ignore.case = TRUE)
  expect_equal(nrow(resultado$pares), 0L)
})

test_that("los pares aproximados declaran distancia y no identidad", {
  skip_if_not_installed("stringdist")
  datos <- datos_pares_aproximados()
  resultado <- detectar_duplicados_aproximados(
    datos, columnas = c("nombre", "domicilio"),
    max_resultados = 10L
  )
  expect_true(resultado$disponible)
  expect_equal(resultado$alcance$n_pares_posibles, 6)
  expect_equal(resultado$alcance$n_pares_comparados, 6)
  expect_equal(resultado$alcance$n_pares_hallados, 1)
  expect_equal(nrow(resultado$pares), 1L)
  expect_true(resultado$pares$distancia[[1L]] > 0)
  expect_lte(resultado$pares$distancia[[1L]], 0.15)
  expect_true(all(as.character(resultado$hallazgos$severidad) == "sospechoso"))
  expect_true(all(grepl("no demuestra identidad", resultado$hallazgos$descripcion,
                         fixed = TRUE)))
  expect_false(any(grepl("Juan|Ana|Calle|Rambla", resultado$pares$evidencia_1,
                         perl = TRUE)))
  expect_true(all(grepl("valor protegido", resultado$pares$evidencia_1,
                        fixed = TRUE)))
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(resultado, archivo = archivo)
  html <- paste(readLines(archivo, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("Juan|Ana|Calle|Rambla", html, perl = TRUE))
  expect_match(html, "Duplicados aproximados", fixed = TRUE)
  expect_match(html, "n_pares_comparados", fixed = TRUE)
})

test_that("MinHash y LSH generan pares unicos y declaran su garantia", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = c("Juan Perez", "Juan Peres", "Ana Silva", "Luis Diaz"),
    domicilio = c("Calle 1", "Calle 1", "Rambla 1", "Camino")
  )
  resultado <- detectar_duplicados_aproximados(
    datos, columnas = c("nombre", "domicilio"), estrategia = "lsh",
    lsh_bandas = 3L, lsh_filas = 2L, lsh_q = 2L, lsh_max_cubeta = 100L,
    max_resultados = Inf
  )
  expect_equal(resultado$alcance$modo_comparacion, "lsh_minhash")
  expect_equal(resultado$alcance$lsh_tamano_firma, 6L)
  expect_equal(resultado$alcance$lsh_semilla_hash, 1L)
  expect_equal(resultado$alcance$lsh_hash_familia,
               "permutacion_aleatoria_determinista_inyectiva")
  expect_gte(resultado$alcance$lsh_candidatos_generados,
             resultado$alcance$lsh_candidatos_unicos)
  expect_equal(
    resultado$alcance$lsh_garantia_jaccard_07,
    1 - (1 - 0.7^2)^3,
    tolerance = 1e-12
  )
  expect_equal(nrow(resultado$pares), 1L)
  expect_equal(anyDuplicated(
    paste(resultado$pares$fila_1, resultado$pares$fila_2, sep = ":")
  ), 0L)
  parcial <- detectar_duplicados_aproximados(
    datos, columnas = c("nombre", "domicilio"), estrategia = "lsh",
    muestra = 2L, lsh_bandas = 2L, lsh_filas = 2L, lsh_q = 2L
  )
  expect_equal(parcial$alcance$n_filas_muestra, 2L)
})

test_that("LSH publica la estimacion previa y el vocabulario", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(v = rep(c("Ana Perez", "Ana Peres", "Luis Diaz"), 20L))
  mensajes <- utils::capture.output(
    resultado <- detectar_duplicados_aproximados(
      datos, columnas = "v", estrategia = "lsh", max_resultados = Inf,
      lsh_muestra_estimacion = 100L
    ),
    type = "message"
  )
  expect_length(mensajes, 0L)
  alcance <- resultado$alcance
  expect_gt(alcance$lsh_vocabulario, 0)
  expect_true(isTRUE(alcance$lsh_candidatos_previstos_es_estimacion))
  expect_gt(alcance$lsh_muestra_estimacion, 0)
  expect_gte(alcance$lsh_candidatos_previstos, 0)
  expect_gte(alcance$lsh_candidatos_unicos, 0)
  expect_equal(alcance$lsh_presupuesto_pares, Inf)
  expect_true(is.list(resultado$estimacion))
  expect_false(isTRUE(resultado$estimacion$tiempo_determinista))
  expect_gt(resultado$estimacion$pares_benchmark, 0)
  expect_false(any(c(
    "lsh_pares_benchmark", "lsh_velocidad_comparacion",
    "lsh_tiempo_estimado_segundos"
  ) %in% names(alcance)))
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(resultado, archivo = archivo)
  html <- paste(readLines(archivo, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "Referencia temporal", fixed = TRUE)
  expect_match(html, "tiempo_determinista", fixed = TRUE)
})

test_that("el presupuesto LSH corta antes del recorrido", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(v = rep(c("Ana Perez", "Ana Peres", "Luis Diaz"), 20L))
  error <- tryCatch(
    detectar_duplicados_aproximados(
      datos, columnas = "v", estrategia = "lsh", max_resultados = Inf,
      lsh_muestra_estimacion = 100L, presupuesto_pares = 1L
    ),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "presupuesto_pares.*No se inició")
  expect_match(conditionMessage(error), "reduzca los datos")
  expect_match(conditionMessage(error), "suba el umbral")
  expect_match(conditionMessage(error), "divida el conjunto")
})

test_that("los textos de estimacion declaran pares y cantidades enteras", {
  fijo <- list(
    candidatos_previstos = 491344.2, pares_benchmark = 382654L,
    tiempo = 4.4, tiempo_benchmark = 0.051
  )
  texto <- lupa:::.texto_tiempo_lsh(fijo)
  expect_match(texto, "491[.]344 candidatos")
  expect_match(texto, "382[.]654 pares")
  expect_match(texto, "piso")
  sin_reloj <- fijo
  sin_reloj$tiempo <- NA_real_
  expect_match(lupa:::.texto_tiempo_lsh(sin_reloj), "no se pudo medir")
  expect_identical(lupa:::.formato_pares_lsh(NA_real_), "NA")
  expect_identical(lupa:::.formato_pares_lsh(Inf), "Inf")
})

test_that("estimar_costo separa el pronostico del recorrido", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = rep(c("Ana Perez", "Ana Peres", "Luis Diaz"), 8L),
    grupo = rep(c("A", "A", "B", NA), length.out = 24L),
    stringsAsFactors = FALSE
  )
  costo <- estimar_costo(
    datos, columnas = "nombre", estrategia = "lsh",
    lsh_muestra_estimacion = 100L, bloquear_por = "grupo"
  )
  expect_s3_class(costo, "estimacion_costo_lupa")
  expect_true(is.finite(costo$candidatos_previstos))
  expect_false(isTRUE(costo$tiempo_determinista))
  expect_true(all(c("bloqueo_pares_alcanzables",
                    "bloqueo_pares_fuera_alcance") %in%
                  names(costo$alcance)))
  costo_2 <- suppressMessages(estimar_costo(
    datos, columnas = "nombre", estrategia = "lsh",
    lsh_muestra_estimacion = 100L, bloquear_por = "grupo"
  ))
  expect_identical(costo$alcance, costo_2$alcance)
})

test_that("bloquear_por es explicito y declara pares fuera de alcance", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = c("Ana Perez", "Ana Peres", "Ana Perez", "Luis Diaz"),
    grupo = c("A", "B", "A", NA_character_),
    stringsAsFactors = FALSE
  )
  resultado <- detectar_duplicados_aproximados(
    datos, columnas = "nombre", estrategia = "teselas", muestra = Inf,
    max_pares = Inf, proteger_datos_personales = FALSE, bloquear_por = "grupo"
  )
  expect_true(any(resultado$pares$fila_1 == 1L & resultado$pares$fila_2 == 3L))
  expect_false(any(resultado$pares$fila_1 == 1L & resultado$pares$fila_2 == 2L))
  expect_equal(resultado$alcance$n_pares_comparados, 1)
  expect_equal(resultado$alcance$bloqueo_pares_alcanzables, 1)
  expect_equal(resultado$alcance$bloqueo_pares_fuera_alcance, 5)
  expect_equal(resultado$alcance$bloqueo_tratamiento_na, "bloque_propio")
  expect_true(any(resultado$hallazgos$tipo_hallazgo ==
                  "bloqueo_por_con_perdida"))
  expect_equal(resultado$hallazgos$severidad[
    resultado$hallazgos$tipo_hallazgo == "bloqueo_por_con_perdida"
  ], factor("sospechoso", levels = c("ok", "sospechoso", "error"),
            ordered = TRUE))
})

test_that("bloquear_por valida columnas atomicas y filtra candidatos", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = c("Ana", "Ana"), grupo = c("A", "B"))
  expect_error(
    detectar_duplicados_aproximados(datos, columnas = "nombre",
                                     bloquear_por = "inexistente"),
    "columna existente"
  )
  datos$matriz <- matrix(1:4, nrow = 2L)
  expect_error(
    detectar_duplicados_aproximados(datos, columnas = "nombre",
                                     bloquear_por = "matriz"),
    "columna atomica"
  )
  comparado <- lupa:::.comparar_bloques_duplicados(
    c("Ana", "Ana"), 1:2, "jw", 0.2, 10L, Inf,
    bloqueos = c(1L, 2L)
  )
  expect_equal(nrow(comparado$pares), 0L)
  vacio <- lupa:::.nuevo_acumulador_duplicados(Inf)
  vacio$lotes <- list(vacio$pares)
  expect_equal(lupa:::.pares_acumulador_duplicados(vacio), vacio$pares)
})

test_that("LSH mide y compara bloques declarados", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = rep("Ana Perez Calle 1", 6L),
    grupo = c("A", "A", "A", "B", "B", "B")
  )
  resultado <- suppressMessages(detectar_duplicados_aproximados(
    datos, columnas = "nombre", estrategia = "lsh", max_resultados = Inf,
    lsh_max_cubeta = 2L, lsh_muestra_estimacion = 100L,
    proteger_datos_personales = FALSE, bloquear_por = "grupo"
  ))
  expect_true(resultado$alcance$lsh_cubetas_grandes[[1L]] > 0L)
  expect_gt(resultado$alcance$lsh_pares_comparados[[1L]], 0)
  expect_true(resultado$alcance$bloqueo_pares_fuera_alcance[[1L]] > 0)
  costo <- estimar_costo(
    datos, columnas = "nombre", estrategia = "teselas",
    max_pares = Inf, bloquear_por = "grupo"
  )
  expect_true(costo$tiempo_determinista)
  expect_equal(costo$alcance$pares_alcanzables, 6)
})

test_that("las ramas de estimación y alcance sin comparables quedan declaradas", {
  skip_if_not_installed("stringdist")
  firmas <- matrix(seq_len(12L), nrow = 4L, ncol = 3L)
  sin_pares_benchmark <- lupa:::.estimar_lsh(
    firmas, letters[1:4], "jw", bandas = 1L, filas_banda = 3L,
    tamano_muestra = 20L, bloqueos = seq_len(4L)
  )
  expect_true(is.na(sin_pares_benchmark$tiempo))
  datos <- data.frame(nombre = c("", ""), grupo = c("A", NA_character_))
  resultado <- detectar_duplicados_aproximados(
    datos, columnas = "nombre", estrategia = "teselas", muestra = Inf,
    max_pares = Inf, bloquear_por = "grupo"
  )
  expect_equal(resultado$alcance$n_filas_validas, 0L)
  simple <- data.frame(nombre = c("Ana", "Ana"))
  costo <- estimar_costo(
    simple, columnas = "nombre", estrategia = "teselas", max_pares = Inf
  )
  expect_true(costo$tiempo_determinista)
})

test_that("las salidas tempranas conservan el resumen de bloqueo", {
  local_mocked_bindings(
    .stringdist_disponible = function() FALSE,
    .package = "lupa"
  )
  datos <- data.frame(nombre = c("Ana", "Ana"), grupo = c("A", "B"))
  no_disponible <- detectar_duplicados_aproximados(
    datos, bloquear_por = "grupo"
  )
  expect_equal(no_disponible$alcance$bloqueo_por, "grupo")
  solo_numericas <- data.frame(valor = 1:2, grupo = c("A", "B"))
  sin_columnas <- detectar_duplicados_aproximados(
    solo_numericas, bloquear_por = "grupo"
  )
  expect_equal(sin_columnas$alcance$bloqueo_por, "grupo")
  costo <- estimar_costo(datos, bloquear_por = "grupo")
  expect_true(costo$tiempo_determinista)
})

test_that("el resumen de Jaccard se recorta con alcance declarado", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = rep("Ana Perez Calle 1", 250L))
  resultado <- suppressMessages(detectar_duplicados_aproximados(
    datos, columnas = "nombre", estrategia = "lsh", max_resultados = 1L,
    lsh_max_cubeta = 1000L, proteger_datos_personales = FALSE
  ))
  expect_true(resultado$alcance$lsh_jaccard_evaluados[[1L]] <= 10000L)
  expect_match(resultado$alcance$lsh_jaccard_alcance[[1L]],
               "primeros_del_recorrido", fixed = TRUE)
})

test_that("el aviso de tiempo no escribe fuera de una sesion interactiva", {
  recibido <- character()
  withCallingHandlers(
    lupa:::.emitir_tiempo_lsh("LSH: 491.344 candidatos previstos"),
    message = function(condicion) {
      recibido <<- c(recibido, conditionMessage(condicion))
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(recibido, "LSH: 491.344 candidatos previstos")
  expect_length(capture.output(
    lupa:::.emitir_tiempo_lsh("LSH: 491.344 candidatos previstos"),
    type = "message"
  ), 0L)
})

test_that("la familia MinHash conserva RNGkind y .Random.seed", {
  original <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) seed_original <- get(".Random.seed", .GlobalEnv)
  on.exit({
    suppressWarnings(do.call(RNGkind, as.list(original)))
    if (had_seed) {
      assign(".Random.seed", seed_original, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  suppressWarnings(RNGkind(kind = "Wichmann-Hill", sample.kind = "Rounding"))
  set.seed(123)
  kind_antes <- RNGkind()
  seed_antes <- get(".Random.seed", .GlobalEnv)
  ids <- matrix(c(1L, 2L, 0L, 2L, 3L, 0L), nrow = 2L, byrow = TRUE)
  firma_1 <- lupa:::.firmas_minhash_lsh(ids, 6L)
  firma_2 <- lupa:::.firmas_minhash_lsh(ids, 6L)
  expect_identical(firma_1, firma_2)
  expect_identical(kind_antes, RNGkind())
  expect_identical(seed_antes, get(".Random.seed", .GlobalEnv))
})

test_that("la estimacion vacia y la muestra de una fila son explicitas", {
  expect_equal(nrow(lupa:::.muestra_pares_lsh(1L, 10L)), 0L)
  estimacion <- lupa:::.estimar_lsh(
    matrix(numeric(), nrow = 0L, ncol = 6L), character(), "jw", 2L, 3L, 10L
  )
  expect_equal(estimacion$candidatos_previstos, 0)
  expect_equal(estimacion$muestra_usada, 0L)
  expect_true(is.na(estimacion$tiempo))
})

test_that("las cubetas LSH grandes se declaran y procesan por troceo", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = rep("mismo valor", 30L))
  resultado <- detectar_duplicados_aproximados(
    datos, estrategia = "lsh", lsh_max_cubeta = 2L,
    lsh_bandas = 2L, lsh_filas = 2L, max_resultados = 5L
  )
  expect_gt(resultado$alcance$lsh_cubetas_grandes, 0)
  expect_equal(resultado$alcance$lsh_pares_descartados_cubetas, 0)
  expect_gt(resultado$alcance$lsh_pares_cubetas_troceadas, 0)
  expect_gt(resultado$alcance$lsh_teselas_cubetas_grandes, 0)
  expect_equal(resultado$alcance$lsh_lotes_cubetas_grandes, 0)
  expect_equal(
    resultado$alcance$lsh_candidatos_generados,
    resultado$alcance$lsh_candidatos_unicos +
      resultado$alcance$lsh_candidatos_descartados_bandas
  )
  expect_equal(
    resultado$alcance$lsh_garantia_estado,
    "valida_generacion_lsh_sin_cubetas_descartadas"
  )
  expect_false(anyNA(resultado$alcance$lsh_garantia_jaccard_07))
  expect_false(resultado$alcance$comparacion_exhaustiva)
})

test_that("las colisiones de bandas posteriores se deduplican sin recorte", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = rep("mismo valor", 30L))
  resultado <- detectar_duplicados_aproximados(
    datos, estrategia = "lsh", lsh_max_cubeta = 100L,
    lsh_bandas = 2L, lsh_filas = 2L, max_resultados = 10L
  )
  expect_equal(resultado$alcance$lsh_candidatos_generados,
               resultado$alcance$lsh_candidatos_unicos +
                 resultado$alcance$lsh_candidatos_descartados_bandas)
  expect_equal(anyDuplicated(
    paste(resultado$pares$fila_1, resultado$pares$fila_2, sep = ":")
  ), 0L)
  directo <- lupa:::.comparar_lsh_duplicados(
    rep("mismo valor", 6L), 6:1, "jw", 0.12, 2L, 2L, 3L, 100L, 10L
  )
  expect_true(nrow(directo$pares) > 0L)
  expect_true(all(directo$pares$fila_1 < directo$pares$fila_2))
})

test_that("una garantia LSH se invalida si se descarta una cubeta", {
  expect_true(is.na(lupa:::.garantia_lsh(0.8, 12L, 3L, 1)))
  expect_equal(lupa:::.garantia_lsh(0.8, 12L, 3L, 0),
               1 - (1 - 0.8^3)^12, tolerance = 1e-12)
  expect_equal(lupa:::.estado_garantia_lsh(1),
               "no_valida_hay_cubetas_descartadas")
  expect_equal(lupa:::.estado_garantia_lsh(0),
               "valida_generacion_lsh_sin_cubetas_descartadas")
})

test_that("una cubeta posterior puede trocearse sin perder su alcance", {
  skip_if_not_installed("stringdist")
  firmas <- matrix(c(1L, 5L, 2L, 5L, 3L, 5L, 4L, 5L),
                   nrow = 4L, byrow = TRUE)
  local_mocked_bindings(
    .firmas_minhash_lsh = function(ids, n_hashes) firmas,
    .package = "lupa"
  )
  resultado <- detectar_duplicados_aproximados(
    data.frame(v = c("Ana", "Anb", "Anc", "And")), columnas = "v",
    estrategia = "lsh", lsh_bandas = 2L, lsh_filas = 1L,
    lsh_q = 1L, lsh_max_cubeta = 2L, max_resultados = 10L
  )
  expect_gt(resultado$alcance$lsh_pares_cubetas_troceadas, 0)
  expect_gt(resultado$alcance$lsh_lotes_cubetas_grandes, 0)
  expect_equal(resultado$alcance$lsh_teselas_cubetas_grandes, 0)
})

test_that("las cubetas troceadas tambien entran al diagnostico de Jaccard", {
  skip_if_not_installed("stringdist")
  resultado <- detectar_duplicados_aproximados(
    data.frame(nombre = rep("mismo valor", 30L)), estrategia = "lsh",
    lsh_max_cubeta = 2L, lsh_bandas = 2L, lsh_filas = 2L,
    max_resultados = 5L
  )
  expect_gt(resultado$alcance$lsh_teselas_cubetas_grandes, 0)
  expect_gt(resultado$alcance$lsh_jaccard_evaluados, 0)
  expect_equal(resultado$alcance$lsh_jaccard_bajo_07, 0)
  expect_equal(resultado$alcance$lsh_jaccard_minimo, 1)
  masivo <- detectar_duplicados_aproximados(
    data.frame(nombre = rep("mismo valor", 150L)), estrategia = "lsh",
    lsh_max_cubeta = 2L, lsh_bandas = 2L, lsh_filas = 2L,
    max_resultados = 5L
  )
  expect_gt(masivo$alcance$lsh_jaccard_pares_elegibles,
            masivo$alcance$lsh_jaccard_evaluados)
  expect_match(masivo$alcance$lsh_jaccard_alcance,
               "primeros_del_recorrido", fixed = TRUE)
})

test_that("el limite de pares declara si aplica al camino usado", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = c("Ana", "Ana", "Luis"))
  lsh <- detectar_duplicados_aproximados(
    datos, estrategia = "lsh", max_pares = 2L, max_resultados = Inf
  )
  expect_true(is.na(lsh$alcance$limite_pares))
  expect_equal(lsh$alcance$limite_pares_configurado, 2L)
  expect_false(lsh$alcance$limite_pares_aplica)
  exacto <- detectar_duplicados_aproximados(
    datos, estrategia = "teselas", max_pares = 2L
  )
  expect_equal(exacto$alcance$limite_pares, 2L)
  expect_true(exacto$alcance$limite_pares_aplica)
})

test_that("las primitivas LSH cubren casos vacios, cortos y con padding", {
  expect_equal(lupa:::.validar_parametro_lsh(3, "b"), 3L)
  expect_error(lupa:::.validar_parametro_lsh(1.5, "b"), "entero")
  expect_equal(lupa:::.qgramas_lsh(c("", "ab", "abcd"), 3L),
               list(character(), "ab", c("abc", "bcd")))
  vacios <- lupa:::.ids_qgramas_por_bloques(character(), 3L)
  expect_equal(dim(vacios$ids), c(0L, 1L))
  firmas <- lupa:::.firmas_minhash_lsh(
    matrix(c(1L, 0L, 1L, 2L), nrow = 2L, byrow = TRUE), 2L
  )
  expect_equal(dim(firmas), c(2L, 2L))
  solo_primer_id <- lupa:::.firmas_minhash_lsh(
    matrix(c(1L, 0L, 2L, 0L), nrow = 2L, byrow = TRUE), 2L
  )
  expect_true(all(is.finite(solo_primer_id)))
  set.seed(42)
  estado <- .Random.seed
  expect_identical(
    lupa:::.firmas_minhash_lsh(matrix(c(1L, 2L, 3L, 2L), 2L, 2L), 4L),
    lupa:::.firmas_minhash_lsh(matrix(c(1L, 2L, 3L, 2L), 2L, 2L), 4L)
  )
  expect_identical(.Random.seed, estado)
  expect_equal(dim(lupa:::.firmas_minhash_lsh(matrix(0L, 2L, 2L), 2L)),
               c(2L, 2L))
  expect_equal(dim(lupa:::.firmas_minhash_lsh(matrix(0L, 2L, 2L), 0L)),
               c(2L, 0L))
  expect_equal(lupa:::.pares_acumulador_duplicados(
    lupa:::.nuevo_acumulador_duplicados(Inf)
  ), data.frame(fila_1 = integer(), fila_2 = integer(), distancia = numeric()))
  expect_equal(lupa:::.jaccard_qgramas(character(), character()), 1)
  expect_equal(lupa:::.jaccard_qgramas(c("ab"), c("bc")), 0)
})

test_that("auto usa LSH por encima del tope sin recortar filas", {
  skip_on_cran()
  skip_if_not_installed("stringdist")
  n <- 10001L
  datos <- data.frame(
    nombre = paste0("persona", seq_len(n)),
    domicilio = paste0("calle", seq_len(n))
  )
  datos$nombre[5000:5001] <- c("Juan Perez", "Juan Peres")
  datos$domicilio[5000:5001] <- "Calle Centro"
  resultado <- detectar_duplicados_aproximados(datos, max_resultados = 1L)
  expect_equal(resultado$alcance$estrategia, "lsh_min_hash")
  expect_equal(resultado$alcance$n_filas_muestra, n)
  expect_true(any(resultado$pares$fila_1 == 5000L &
                 resultado$pares$fila_2 == 5001L))
  expect_true(all(c("lsh_bandas", "lsh_filas", "lsh_garantia_jaccard_07") %in%
                  names(resultado$alcance)))
})

test_that("el perfil integra pares aproximados sin proponer eliminacion", {
  skip_if_not_installed("stringdist")
  datos <- datos_pares_aproximados()
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    duplicados_aproximados = list(
      columnas = c("nombre", "domicilio"), max_resultados = 10L
    )
  )
  expect_true(inherits(perfil$duplicados_aproximados,
                       "duplicados_aproximados"))
  expect_true(any(perfil$hallazgos$tipo_hallazgo == "duplicados_aproximados"))
  plan <- planificar_limpieza(perfil, datos)
  expect_false(any(grepl("duplicados_aproximados", plan$hallazgo, fixed = TRUE)))
})

test_that("el alcance declara el recorte de pares", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = sprintf("persona %03d", 1:100))
  resultado <- detectar_duplicados_aproximados(
    datos, max_pares = 100L, muestra = Inf, max_resultados = 10L
  )
  expect_true(resultado$alcance$n_pares_comparados <= 100)
  expect_true(resultado$alcance$n_pares_sin_comparar > 0)
  expect_true(resultado$alcance$muestreado)
  expect_match(resultado$alcance$estrategia,
               "muestra_sistematica", fixed = TRUE)
})

test_that("el recorte sistematico conserva los extremos de la tabla", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = paste0("persona", seq_len(1000L)),
    domicilio = paste0("calle", seq_len(1000L))
  )
  datos$nombre[999:1000] <- c("Juan Perez", "Juan Peres")
  datos$domicilio[999:1000] <- "Calle Centro"
  resultado <- detectar_duplicados_aproximados(
    datos, muestra = 1000L, max_pares = 10000L, max_resultados = Inf
  )
  expect_equal(resultado$alcance$n_filas_muestra, 141)
  expect_match(resultado$alcance$estrategia, "limite_de_pares", fixed = TRUE)
  expect_true(any(
    resultado$pares$fila_1 == 999L & resultado$pares$fila_2 == 1000L
  ))
  solo_muestra <- detectar_duplicados_aproximados(
    datos, muestra = 100L, max_pares = Inf, max_resultados = 1L
  )
  expect_equal(solo_muestra$alcance$estrategia,
               "muestra_sistematica_por_muestra")
  ambos_limites <- detectar_duplicados_aproximados(
    datos, muestra = 500L, max_pares = 100L, max_resultados = 1L
  )
  expect_equal(ambos_limites$alcance$estrategia,
               "muestra_sistematica_por_muestra_y_limite_de_pares")
  primeras <- detectar_duplicados_aproximados(
    datos, muestra = Inf, max_pares = 1L, max_resultados = 1L
  )
  expect_match(primeras$alcance$estrategia, "primeras_n_filas", fixed = TRUE)
})

test_that("el valor predeterminado recorre la tabla completa por bloques", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = paste0("persona", seq_len(1000L)),
    domicilio = paste0("calle", seq_len(1000L))
  )
  datos$nombre[500:501] <- c("Juan Perez", "Juan Peres")
  datos$domicilio[500:501] <- "Calle Centro"
  resultado <- detectar_duplicados_aproximados(datos)
  expect_true(resultado$alcance$comparacion_exhaustiva)
  expect_equal(resultado$alcance$n_filas_muestra, 1000)
  expect_equal(resultado$alcance$n_pares_comparados, 499500)
  expect_equal(resultado$alcance$tamano_bloque, 1000)
  expect_equal(resultado$alcance$modo_comparacion, "exhaustiva_por_bloques")
  expect_true(any(resultado$pares$fila_1 == 500L & resultado$pares$fila_2 == 501L))
})

test_that("el recorrido por bloques conserva exactitud y acota su tesela", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = c("Ana Perez", "Ana Perez", "Luis Diaz", "Marta Silva"),
    domicilio = c("Calle 1", "Calle 1", "Calle 2", "Calle 3")
  )
  resultado <- detectar_duplicados_aproximados(
    datos, muestra = Inf, max_pares = Inf, max_resultados = Inf, bloque = 2L
  )
  expect_equal(resultado$alcance$n_pares_comparados, 6)
  expect_equal(resultado$alcance$n_pares_exactos, 1)
  expect_equal(resultado$alcance$n_bloques, 3)
  expect_equal(resultado$alcance$tamano_bloque, 2)
  expect_true(any(resultado$pares$tipo_par == "exacto"))
})

test_that("el acumulador procesa lotes y conserva sólo el límite", {
  acumulador <- lupa:::.nuevo_acumulador_duplicados(2L)
  expect_identical(
    lupa:::.acumular_pares_duplicados(acumulador, integer(), integer(), numeric()),
    acumulador
  )
  expect_identical(
    lupa:::.acumular_pares_duplicados(acumulador, 1L, 2L, Inf), acumulador
  )
  for (inicio in seq(1, 1000, by = 10)) {
    indices <- inicio:(inicio + 9)
    acumulador <- lupa:::.acumular_pares_duplicados(
      acumulador, indices, indices + 1L,
      rep(c(0.08, 0.01, 0.05, 0.03, 0.02), length.out = 10L)
    )
  }
  expect_equal(nrow(acumulador$pares), 2L)
  expect_equal(acumulador$n_hallados, 1000L)
  expect_equal(acumulador$n_exactos, 0L)
  expect_equal(acumulador$n_aproximados, 1000L)
  expect_equal(acumulador$pares$distancia, c(0.01, 0.01))
  expect_error(
    lupa:::.acumular_pares_duplicados(acumulador, 1L, c(2L, 3L), 0.1),
    "igual longitud"
  )
})

test_that("el tope predeterminado alcanza diez mil filas", {
  skip_on_cran()
  skip_if_not_installed("stringdist")
  n <- 10000L
  datos <- data.frame(
    nombre = paste0("persona", seq_len(n)),
    domicilio = paste0("calle", seq_len(n))
  )
  datos$nombre[5000:5001] <- c("Juan Perez", "Juan Peres")
  datos$domicilio[5000:5001] <- "Calle Centro"
  resultado <- detectar_duplicados_aproximados(datos)
  expect_true(resultado$alcance$comparacion_exhaustiva)
  expect_equal(resultado$alcance$n_filas_muestra, n)
  expect_equal(resultado$alcance$n_pares_comparados, 49995000)
  expect_true(any(resultado$pares$fila_1 == 5000L &
                 resultado$pares$fila_2 == 5001L))
})

test_that("el truncamiento conserva los pares de menor distancia", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = paste0("Ana Perez ", seq_len(30L)))
  resultado <- detectar_duplicados_aproximados(
    datos, muestra = Inf, max_pares = Inf, max_resultados = 10L,
    umbral = 0.2
  )
  expect_true(resultado$alcance$truncado)
  expect_equal(nrow(resultado$pares), 10L)
  expect_true(all(diff(resultado$pares$distancia) >= 0))
})

test_that("los pares exactos en las columnas elegidas quedan visibles", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    id = c("a", "b"), nombre = c("Ana", "Ana"),
    domicilio = c("Calle 1", "Calle 1")
  )
  resultado <- detectar_duplicados_aproximados(
    datos, muestra = Inf, max_pares = Inf
  )
  expect_equal(resultado$columnas, c("nombre", "domicilio"))
  expect_equal(resultado$pares$tipo_par, "exacto")
  expect_equal(resultado$alcance$n_pares_exactos, 1)
  expect_equal(resultado$alcance$n_pares_aproximados, 0)
  expect_equal(resultado$hallazgos$tipo_hallazgo, "duplicados_exactos_columnas")
  expect_equal(sum(duplicated(datos)), 0L)
})

test_that("el reenmascarado protege hallazgos compuestos nuevos y exactos", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = c("Ana Perez", "Ana Perez", "Luis Diaz"),
    domicilio = c("Calle Uruguay 123", "Calle Uruguay 123", "Av Italia 900"),
    stringsAsFactors = FALSE
  )
  analisis <- analizar(
    datos, proteger_datos_personales = FALSE,
    argumentos_perfil = list(
      duplicados_aproximados = list(columnas = c("nombre", "domicilio"))
    )
  )
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  invisible(reportar(analisis, archivo = archivo))
  html <- paste(readLines(archivo, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("Ana Perez|Calle Uruguay 123", html, perl = TRUE))
  expect_match(html, "evidencia protegida", fixed = TRUE)

  compuesto <- data.frame(
    columna = "nombre, domicilio", tipo_hallazgo = "hallazgo_compuesto_nuevo",
    evidencia = "Ana Perez / Calle Uruguay 123", stringsAsFactors = FALSE
  )
  compuesto$severidad <- factor("sospechoso",
    levels = c("ok", "sospechoso", "error"), ordered = TRUE
  )
  compuesto$descripcion <- "prueba"
  compuesto$sugerencia <- "revisar"
  base <- perfilar(datos, analizar_dependencias = FALSE)
  protegido <- lupa:::.proteger_componentes_perfil(
    base$columnas, base$patrones, base$dependencias, compuesto,
    base$datos_personales
  )
  expect_equal(protegido$hallazgos$evidencia, "[evidencia protegida]")
})

test_that("la seleccion automatica evita mezclar demasiadas columnas", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(a = "x", b = "y", c = "z")
  resultado <- detectar_duplicados_aproximados(datos)
  expect_length(resultado$columnas, 0L)
  expect_match(resultado$razon, "indique `columnas`", fixed = TRUE)
})

test_that("valida entradas y tipos no comparables", {
  expect_length(lupa:::.indices_duplicados_aproximados(0, 10L), 0L)
  expect_length(lupa:::.indices_duplicados_aproximados(10, 3L), 3L)
  expect_equal(lupa:::.indices_duplicados_aproximados(3, 2L), 1:2)
  vacio <- lupa:::.vacio_duplicados_aproximados(
    2L, character(), "jw", 0.12, 2L, 1L, 1L
  )
  expect_identical(
    lupa:::.proteger_duplicados_aproximados(vacio, character()), vacio
  )
  expect_equal(
    lupa:::.evidencia_fila_aproximada(
      data.frame(a = NA_character_), "a", 1L, character()
    ),
    "a=[ausente]"
  )
  expect_error(detectar_duplicados_aproximados(1:3), "data.frame")
  datos <- data.frame(a = c("x", "y"), b = 1:2)
  expect_error(detectar_duplicados_aproximados(datos, columnas = "no"),
               "columnas")
  expect_error(detectar_duplicados_aproximados(datos, columnas = c("a", "a")),
               "columnas")
  matriz <- data.frame(a = I(matrix(1:4, nrow = 2)))
  expect_error(detectar_duplicados_aproximados(matriz, columnas = "a"),
               "matrices")
  expect_error(detectar_duplicados_aproximados(datos, metodo = "inventado"),
               "medida")
  expect_error(detectar_duplicados_aproximados(datos, umbral = -1), "finito")
  expect_error(detectar_duplicados_aproximados(datos, muestra = 1.5), "muestra")
  expect_error(detectar_duplicados_aproximados(datos, normalizar = NA), "lógicos")
  expect_error(detectar_duplicados_aproximados(datos, bloque = Inf), "bloque")
  expect_error(detectar_duplicados_aproximados(datos, bloque = 1.5), "bloque")
  tesela_vacia <- lupa:::.comparar_bloques_duplicados(
    "solo", 1L, "jw", 0.12, 1000L, 1L
  )
  expect_equal(tesela_vacia$n_bloques, 0L)
  expect_length(lupa:::.indices_duplicados_aproximados(3, 10L), 3L)
  expect_error(
    detectar_duplicados_aproximados(
      datos, perfil = perfilar(data.frame(z = 1:2), analizar_dependencias = FALSE)
    ),
    "corresponder"
  )
})

test_that("declara ausencia de columnas, filas o pares comparables", {
  skip_if_not_installed("stringdist")
  sin_texto <- detectar_duplicados_aproximados(data.frame(a = 1:3))
  expect_true(sin_texto$disponible)
  expect_match(sin_texto$razon, "texto comparables", fixed = TRUE)
  sin_filas <- detectar_duplicados_aproximados(
    data.frame(nombre = c(NA_character_, ""))
  )
  expect_match(sin_filas$razon, "dos filas", fixed = TRUE)
  sin_pares <- detectar_duplicados_aproximados(
    data.frame(nombre = c("Ana", "Luis")), umbral = 0
  )
  expect_equal(nrow(sin_pares$pares), 0L)
  expect_equal(sin_pares$alcance$n_pares_hallados, 0)
  sin_normalizar <- detectar_duplicados_aproximados(
    data.frame(nombre = c("Ana", "ana")), normalizar = FALSE, umbral = 0.01,
    muestra = Inf, max_pares = Inf
  )
  expect_true(sin_normalizar$disponible)
})

test_that("los bytes UTF-8 inválidos se excluyen sin abortar", {
  skip_if_not_installed("stringdist")
  invalido <- rawToChar(as.raw(c(0x61, 0xff, 0x62)))
  resultado <- detectar_duplicados_aproximados(
    data.frame(nombre = c(invalido, "Ana")), muestra = Inf
  )
  expect_true(resultado$disponible)
  expect_equal(resultado$alcance$n_filas_validas, 1)
  expect_equal(nrow(resultado$pares), 0L)
})

test_that("limita resultados, protege o expone evidencia según la opción", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = paste0("Ana Perez ", seq_len(6L)))
  truncado <- detectar_duplicados_aproximados(
    datos, max_resultados = 2L, umbral = 0.2
  )
  expect_true(truncado$alcance$truncado)
  expect_equal(nrow(truncado$pares), 2L)
  archivo_truncado <- tempfile(fileext = ".html")
  on.exit(unlink(archivo_truncado), add = TRUE)
  reportar(truncado, archivo = archivo_truncado)
  expect_match(
    paste(readLines(archivo_truncado, encoding = "UTF-8"), collapse = "\n"),
    "tabla de pares mostrados está truncada", fixed = TRUE
  )
  visible <- detectar_duplicados_aproximados(
    data.frame(nombre = c("Ana Pérez", "Ana Peres")),
    proteger_datos_personales = FALSE, umbral = 0.2
  )
  expect_true(any(grepl("Ana", visible$pares$evidencia_1, fixed = TRUE)))
  archivo_protegido <- tempfile(fileext = ".html")
  on.exit(unlink(archivo_protegido), add = TRUE)
  reportar(visible, archivo = archivo_protegido)
  html_protegido <- paste(
    readLines(archivo_protegido, encoding = "UTF-8"), collapse = "\n"
  )
  expect_false(grepl("Ana", html_protegido, fixed = TRUE))
  con_perfil <- perfilar(
    datos_pares_aproximados(), analizar_dependencias = FALSE
  )
  reutilizado <- detectar_duplicados_aproximados(
    datos_pares_aproximados(), perfil = con_perfil
  )
  expect_true(reutilizado$disponible)
})

test_that("la protección del reporte también cubre análisis con evidencia cruda", {
  skip_if_not_installed("stringdist")
  datos <- datos_pares_aproximados()
  analisis <- analizar(
    datos,
    proteger_datos_personales = FALSE,
    argumentos_perfil = list(
      analizar_dependencias = FALSE,
      duplicados_aproximados = list(columnas = c("nombre", "domicilio"))
    )
  )
  expect_true(any(grepl("Juan", analisis$perfil$duplicados_aproximados$pares$evidencia_1,
                     fixed = TRUE)))
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(analisis, archivo = archivo)
  html <- paste(readLines(archivo, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("Juan|Calle", html, perl = TRUE))
  expect_match(html, "valores personales protegidos", fixed = TRUE)
})

test_that("perfilar valida la configuración aproximada", {
  datos <- data.frame(nombre = c("Ana", "Ana"))
  expect_error(perfilar(datos, analizar_dependencias = NA), "analizar_dependencias")
  expect_error(perfilar(datos, datos_personales_permitidos = NA), "TRUE o FALSE")
  expect_error(perfilar(datos, muestra_validadores = 0.5), "muestra_validadores")
  expect_error(perfilar(datos, duplicados_aproximados = 1),
               "FALSE, TRUE o una lista")
  expect_error(
    perfilar(datos, duplicados_aproximados = list(proteger_datos_personales = FALSE)),
    "argumentos coordinados"
  )
  if (requireNamespace("stringdist", quietly = TRUE)) {
    perfil <- perfilar(
      datos, analizar_dependencias = FALSE,
      duplicados_aproximados = TRUE
    )
    expect_true(inherits(perfil$duplicados_aproximados,
                         "duplicados_aproximados"))
    archivo <- tempfile(fileext = ".html")
    on.exit(unlink(archivo), add = TRUE)
    reportar(perfil, archivo = archivo)
  }
})
