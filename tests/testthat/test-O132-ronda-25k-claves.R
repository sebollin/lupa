# Ronda 25-K: claves, relaciones y el referencial. Cada prueba rehace la cifra
# a mano con base R y la compara con lo que publica el paquete.

.o132_medicion_referencial <- function(metrica, ref, objetivo, ...) {
  inst <- instanciar(
    especializar(metricas_referencial()[[metrica]], ...), "t", "k",
    referencial = ref
  )
  medir(modelo(inst), objetivo)
}

.o132_medir_referencial <- function(metrica, ref, objetivo, ...) {
  as.data.frame(.o132_medicion_referencial(metrica, ref, objetivo, ...))
}

test_that("H10: los bytes invalidos se comparan por sus bytes, no como NA", {
  # Lo que deja un `read.csv()` de un archivo latin1 sin declarar.
  maria <- rawToChar(as.raw(c(0x4d, 0x61, 0x72, 0xed, 0x61)))
  jose <- rawToChar(as.raw(c(0x4a, 0x6f, 0x73, 0xe9)))
  expect_false(validUTF8(maria))

  # Relaciones: dos invalidos distintos no casan; el mismo si.
  a <- data.frame(k = c("Ana", maria), stringsAsFactors = FALSE)
  b <- data.frame(k = c(jose, "Ana"), stringsAsFactors = FALSE)
  rel <- as.data.frame(detectar_relaciones(a, b))
  expect_identical(rel$n_valores_comunes, length(intersect(a$k, b$k)))
  expect_equal(rel$cobertura_tabla2_en_tabla1, mean(b$k %in% a$k))
  expect_equal(rel$cobertura_tabla1_en_tabla2, mean(a$k %in% b$k))
  b2 <- data.frame(k = c(maria, "Pedro"), stringsAsFactors = FALSE)
  rel2 <- as.data.frame(detectar_relaciones(a, b2))
  expect_identical(rel2$n_valores_comunes, 1L)
  expect_equal(rel2$cobertura_tabla2_en_tabla1, mean(b2$k %in% a$k))

  # Referencial: `Jose` no esta en el padron y no se declara conforme.
  ref <- referencial(
    data.frame(k = c("Ana", maria), stringsAsFactors = FALSE), "k",
    completo = TRUE, alcance = "x"
  )
  objetivo <- data.frame(k = c("Ana", jose, maria), stringsAsFactors = FALSE)
  fuerte <- expect_silent(
    .o132_medir_referencial("CorrectitudSemFuerte", ref, objetivo)
  )
  expect_identical(
    as.numeric(fuerte$resultado),
    as.numeric(objetivo$k %in% c("Ana", maria))
  )
  # La proximidad no ofrece como candidato otro valor ilegible: su distancia
  # se mediria sobre la clave de bytes, no sobre el texto.
  expect_false(any(grepl("candidato_referencial", fuerte$objeto_medible)))
  cobertura <- .o132_medir_referencial(
    "RatioCobertura", ref, data.frame(k = jose, stringsAsFactors = FALSE)
  )
  expect_equal(cobertura$resultado, mean(c("Ana", maria) %in% jose))

  # Dependencias: tres grupos, no dos ni un `casi_constante`.
  d <- data.frame(
    x = rep(c(maria, jose, "Ana"), each = 5),
    y = rep(c("p", "q", "r"), each = 5), stringsAsFactors = FALSE
  )
  dep <- as.data.frame(detectar_dependencias(d, min_observaciones = 5))
  fila <- dep[dep$determinante == "x" & dep$dependiente == "y", , drop = FALSE]
  expect_identical(nrow(fila), 1L)
  expect_identical(as.integer(fila$n_grupos), length(unique(d$x)))
  # Y la evidencia publica los bytes como la consola, no la clave interna.
  d2 <- data.frame(
    x = c(rep(maria, 5), rep("Ana", 5)), y = c(rep("p", 4), "q", rep("r", 5)),
    stringsAsFactors = FALSE
  )
  dep2 <- as.data.frame(
    detectar_dependencias(d2, min_observaciones = 5, umbral = 0.8)
  )
  evidencia <- dep2$evidencia[dep2$determinante == "x" & dep2$dependiente == "y"]
  expect_true(startsWith(evidencia, encodeString(maria, quote = '"')))

  # La pertenencia de las reglas del modelo usa la misma clave.
  expect_identical(
    .en_conjunto_por_valor(c(jose, maria, "Ana"), c(maria, "Ana")),
    c(jose, maria, "Ana") %in% c(maria, "Ana")
  )
})

test_that("H6: un BIGINT que llega como doble por encima de 2^53 no se compara", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  armar <- function(bigint) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", bigint = bigint)
    DBI::dbExecute(con, "CREATE TABLE a (id BIGINT)")
    DBI::dbExecute(con, "CREATE TABLE b (fk BIGINT)")
    DBI::dbExecute(con, paste(
      "INSERT INTO a VALUES (9007199254740993), (9007199254740995),",
      "(9007199254740997)"
    ))
    DBI::dbExecute(con, paste(
      "INSERT INTO b VALUES (9007199254740992), (9007199254740994),",
      "(9007199254740996)"
    ))
    con
  }
  pares <- data.frame(tabla_1 = "a", tabla_2 = "b")
  orden <- list(a = "id", b = "fk")

  con <- armar("numeric")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # A mano, en el propio motor: ningun valor de `b` esta en `a`.
  en_motor <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM b WHERE fk IN (SELECT id FROM a)"
  )$n
  expect_equal(as.numeric(en_motor), 0)
  # Y la lectura como doble los funde: por eso no se puede comparar en R.
  leidos <- DBI::dbGetQuery(con, "SELECT fk FROM b")$fk
  expect_gt(sum(leidos %in% DBI::dbGetQuery(con, "SELECT id FROM a")$id), 0)
  res <- relaciones_coleccion(
    coleccion(con, c("a", "b")), pares, umbral_cobertura = 0, orden = orden
  )
  expect_identical(nrow(res$relaciones), 0L)
  poda <- res$cobertura_podas[res$cobertura_podas$columna_tabla1 == "id", ]
  expect_identical(poda$motivo, "entero_como_doble")
  expect_match(poda$detalle, "integer64", fixed = TRUE)

  # Control: pidiendo `integer64` la misma tabla se compara, y da lo del motor.
  con64 <- armar("integer64")
  on.exit(DBI::dbDisconnect(con64), add = TRUE)
  res64 <- relaciones_coleccion(
    coleccion(con64, c("a", "b")), pares, umbral_cobertura = 0, orden = orden
  )
  expect_identical(nrow(res64$relaciones), 1L)
  expect_equal(res64$relaciones$n_valores_comunes, as.numeric(en_motor))
  expect_identical(res64$relaciones$cardinalidad, "sin_coincidencias")
})

test_that("H1: un integer64 con patron de bits NaN es un entero, no un ausente", {
  skip_if_not_installed("bit64")
  # Todo entero de -1 a -4503599627370495 tiene el patron de bits de un NaN.
  id <- bit64::as.integer64(c(-1, -2, -3))
  expect_false(anyNA(id))
  expect_identical(length(unique(as.character(id))), 3L)
  claves <- as.data.frame(detectar_claves(data.frame(id = id)))
  expect_identical(claves$columnas, "id")
  expect_identical(
    as.data.frame(sugerir_clave(data.frame(id = id)))$identifica, TRUE
  )
  grandes <- bit64::as.integer64(c(
    "9218868437227405313", "9218868437227405314", "9218868437227405315"
  ))
  expect_identical(
    as.data.frame(detectar_claves(data.frame(id = grandes)))$columnas, "id"
  )
  # Compuesta: cuatro combinaciones distintas a mano.
  compuesta <- data.frame(
    a = bit64::as.integer64(c(-1, -2, -1, -2)), b = c("x", "x", "y", "y"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    anyDuplicated(paste(as.character(compuesta$a), compuesta$b)), 0L
  )
  expect_true("a + b" %in% as.data.frame(detectar_claves(compuesta))$columnas)

  # El referencial lo acepta: no hay ausentes.
  expect_s3_class(
    referencial(data.frame(k = id), "k", completo = TRUE, alcance = "x"),
    "referencial"
  )

  # La medicion no borra la fila que falla.
  padron <- c(-1, -2, 3)
  ref <- referencial(data.frame(k = padron), "k", completo = TRUE, alcance = "x")
  objetivo <- c(-1, -5, 3, 7)
  medida <- .o132_medir_referencial(
    "CorrectitudSemFuerte", ref,
    data.frame(k = bit64::as.integer64(objetivo))
  )
  expect_identical(nrow(medida), length(objetivo))
  expect_identical(as.numeric(medida$resultado), as.numeric(objetivo %in% padron))
  cobertura <- .o132_medir_referencial(
    "RatioCobertura", ref, data.frame(k = bit64::as.integer64(objetivo))
  )
  expect_equal(cobertura$resultado, mean(padron %in% objetivo))

  # Y el perfil cuenta las filas completas y no funde columnas ni filas.
  d <- data.frame(
    a = bit64::as.integer64(c(-1, -2, -5)),
    b = bit64::as.integer64(c(-3, -4, -6)), g = c("x", "x", "y"),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(
    d, analizar_dependencias = FALSE,
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  ))
  expect_identical(
    as.integer(perfil$general$filas_completas),
    sum(!is.na(d$a) & !is.na(d$b) & !is.na(d$g))
  )
  expect_identical(
    as.integer(perfil$general$filas_duplicadas),
    sum(duplicated(paste(as.character(d$a), as.character(d$b), d$g)))
  )
  expect_identical(nrow(perfil$general$columnas_duplicadas), 0L)
})

test_that("H2: instantes y fechas con fraccion se comparan por su valor", {
  t0 <- as.POSIXct("2020-01-01 10:00:00", tz = "UTC")
  cobertura_a_mano <- function(y, x) mean(as.numeric(y) %in% as.numeric(x))

  # La respuesta no depende de que la poda corra o no: el mismo valor de `b`.
  a1 <- data.frame(t = t0 + 0.2)
  a2 <- data.frame(t = t0 + c(0.2, 0.9))
  b <- data.frame(t = t0 + 0.7)
  r1 <- as.data.frame(detectar_relaciones(a1, b))
  r2 <- as.data.frame(detectar_relaciones(a2, b))
  expect_identical(r2$n_valores_comunes, 0L)
  expect_equal(r2$cobertura_tabla2_en_tabla1, cobertura_a_mano(b$t, a2$t))
  expect_identical(r1$cardinalidad, r2$cardinalidad)
  # A un microsegundo tambien son distintos.
  r3 <- as.data.frame(detectar_relaciones(
    data.frame(t = t0 + c(0, 1e-6)), data.frame(t = t0 + c(1e-6, 5))
  ))
  expect_identical(r3$n_valores_comunes, 1L)
  # Fechas con fraccion de dia.
  fechas <- as.data.frame(detectar_relaciones(
    data.frame(d = as.Date(c(18262.2, 18262.9))), data.frame(d = as.Date(18262.7))
  ))
  expect_identical(fechas$n_valores_comunes, 0L)
  # Control: al segundo exacto, un texto sigue coincidiendo con un instante.
  control <- as.data.frame(detectar_relaciones(
    data.frame(t = t0), data.frame(t = "2020-01-01 10:00:00")
  ))
  expect_identical(control$n_valores_comunes, 1L)

  # Referencial: el instante que el padron no tiene no es conforme.
  ref <- referencial(data.frame(k = t0 + 0.2), "k", completo = TRUE, alcance = "x")
  objetivo <- t0 + c(0.2, 0.7, 1.2)
  medida <- .o132_medir_referencial(
    "CorrectitudSemFuerte", ref, data.frame(k = objetivo)
  )
  expect_identical(
    as.numeric(medida$resultado),
    as.numeric(as.numeric(objetivo) %in% as.numeric(t0 + 0.2))
  )

  # Dependencias: veinte instantes distintos, dos por segundo, son una clave.
  d <- data.frame(
    t = t0 + rep(0:9, each = 2) + rep(c(0.1, 0.6), 10),
    g = rep(c("a", "b"), 10), stringsAsFactors = FALSE
  )
  expect_identical(length(unique(as.numeric(d$t))), nrow(d))
  omitidas <- as.data.frame(detectar_dependencias(d, min_observaciones = 5))
  expect_false("t" %in% omitidas$determinante)
  todas <- as.data.frame(
    detectar_dependencias(d, min_observaciones = 5, incluir_claves = TRUE)
  )
  fila <- todas[todas$determinante == "t" & todas$dependiente == "g", ]
  expect_identical(nrow(fila), 1L)
  expect_identical(as.integer(fila$n_grupos), nrow(d))
})

test_that("H8: la casi-clave usa la mascara de faltantes disfrazados de perfilar", {
  docs <- sprintf("%08d", 10000000 + seq_len(190))
  # K1: una clave sana con diez "SIN DATO" no es una clave rota.
  k1 <- data.frame(documento = c(docs, rep("SIN DATO", 10)),
                   stringsAsFactors = FALSE)
  expect_identical(anyDuplicated(docs), 0L)
  expect_identical(nrow(as.data.frame(detectar_claves(k1))), 0L)

  # K2: las colisiones reales no se diluyen con los "SIN DATO".
  k2 <- data.frame(
    documento = c(docs[1:180], rep(docs[1], 6), rep("SIN DATO", 14)),
    stringsAsFactors = FALSE
  )
  tabla <- table(k2$documento[k2$documento != "SIN DATO"])
  fila <- as.data.frame(detectar_claves(k2))
  expect_identical(nrow(fila), 1L)
  expect_true(fila$casi_clave)
  expect_identical(as.integer(fila$n_duplicados_excedentes), as.integer(sum(tabla - 1L)))
  expect_identical(as.integer(fila$n_valores_colisionados), sum(tabla > 1L))
  expect_equal(fila$concentracion_colisiones, max(tabla - 1L) / sum(tabla - 1L))
  expect_false(grepl("SIN DATO", fila$colisiones, fixed = TRUE))
  # La fila mide una sola poblacion: normalizar no agrega distintos.
  expect_lte(fila$n_distintos_normalizados, fila$n_distintos_exactos)
  expect_identical(as.integer(fila$n_distintos_exactos), length(tabla))

  # Con un perfil, la politica que ese perfil declaro.
  k3 <- data.frame(documento = c(docs, rep("ZQZQ", 10)),
                   stringsAsFactors = FALSE)
  expect_true(as.data.frame(detectar_claves(k3))$casi_clave)
  perfil <- suppressWarnings(perfilar(
    k3, analizar_dependencias = FALSE, cadenas_ausencia = "ZQZQ",
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  ))
  expect_identical(
    nrow(as.data.frame(detectar_claves(k3, perfil = perfil))), 0L
  )
})

test_that("H5: RatioCobertura sobre una entidad sin valores es sin_valores", {
  ref <- referencial(
    data.frame(k = c("a", "b"), stringsAsFactors = FALSE), "k",
    completo = TRUE, alcance = "x"
  )
  for (objetivo in list(
    data.frame(k = character(), stringsAsFactors = FALSE),
    data.frame(k = c(NA_character_, NA_character_), stringsAsFactors = FALSE)
  )) {
    medicion <- .o132_medicion_referencial("RatioCobertura", ref, objetivo)
    # Cero filas medibles: no se publica una cobertura cero.
    expect_identical(nrow(as.data.frame(medicion)), 0L)
    cobertura <- attr(medicion, "cobertura_metricas")
    expect_identical(cobertura$estado[cobertura$metrica == "RatioCobertura"],
                     "sin_valores")
  }
  # Control: con una fila se mide, y da lo de la mano.
  una <- .o132_medir_referencial(
    "RatioCobertura", ref, data.frame(k = "a", stringsAsFactors = FALSE)
  )
  expect_equal(una$resultado, mean(c("a", "b") %in% "a"))
})

test_that("H11: la poda por cardinalidad acota la cobertura por filas", {
  a <- data.frame(id = 1L)
  b <- data.frame(fk = c(rep(1L, 91), 2:10))
  cobertura <- mean(b$fk %in% a$id)
  expect_gte(cobertura, 0.9)
  sin_poda <- as.data.frame(detectar_relaciones(a, b))
  con_poda <- as.data.frame(detectar_relaciones(a, b, podar = TRUE))
  expect_identical(con_poda$cardinalidad, sin_poda$cardinalidad)
  expect_equal(con_poda$cobertura_tabla2_en_tabla1, cobertura)
  # Control: donde ni los valores mas frecuentes alcanzan, la poda sigue.
  b2 <- data.frame(fk = c(rep(1L, 50), 2:51))
  expect_lt(mean(b2$fk %in% a$id), 0.9)
  podado <- as.data.frame(detectar_relaciones(a, b2, podar = TRUE))
  expect_identical(podado$motivo_poda, "cardinalidades_imposibles")
})

test_that("H7: un MAX de texto en SQLite no es el extremo de un rango numerico", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Columna sin afinidad: guarda 1..100 como enteros y un '5' como texto.
  DBI::dbExecute(con, "CREATE TABLE a (id)")
  DBI::dbExecute(con, "CREATE TABLE b (fk INTEGER)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO a VALUES ", paste0("(", 1:100, ")", collapse = ", "), ", ('5')"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO b VALUES ", paste0("(", 50:60, ")", collapse = ", ")
  ))
  extremos <- DBI::dbGetQuery(
    con, "SELECT typeof(MAX(id)) AS tipo, COUNT(*) AS n FROM a"
  )
  expect_identical(extremos$tipo, "text")
  en_motor <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM b WHERE fk IN (SELECT id FROM a)"
  )$n
  res <- suppressWarnings(relaciones_coleccion(
    coleccion(con, c("a", "b")), data.frame(tabla_1 = "a", tabla_2 = "b"),
    umbral_cobertura = 0, orden = list(a = "id", b = "fk")
  ))
  expect_false("rangos_disjuntos" %in% res$cobertura_podas$motivo)
  expect_equal(res$relaciones$cobertura_tabla2_en_tabla1, en_motor / 11)
})

test_that("H4: el universo del referencial se cuenta con la identidad del apareo", {
  padron <- c("A", "a", "B")
  ref <- referencial(
    data.frame(k = padron, stringsAsFactors = FALSE), "k",
    completo = TRUE, alcance = "x"
  )
  objetivo <- data.frame(k = "a", stringsAsFactors = FALSE)
  # Normalizado -por omision, caja-: el universo tiene dos claves.
  medicion <- .o132_medicion_referencial("RatioCobertura", ref, objetivo)
  universo <- unique(tolower(padron))
  expect_equal(as.data.frame(medicion)$resultado, mean(universo %in% "a"))
  alcance <- attr(medicion, "alcance_metricas")[[1L]]
  expect_identical(as.integer(alcance$n_referencial), length(universo))
  # Exacto: tres claves.
  exacta <- .o132_medir_referencial(
    "RatioCobertura", ref, objetivo, normalizar = FALSE
  )
  expect_equal(exacta$resultado, mean(padron %in% "a"))
})

test_that("H9: un par con la referencia truncada no se descarta en silencio", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "a", data.frame(id = 1:200))
  # `b` lee primero las claves que la parte leida de `a` no tiene.
  DBI::dbWriteTable(con, "b", data.frame(fk = 200:1, n = 1:200))
  en_motor <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM b WHERE fk IN (SELECT id FROM a)"
  )$n
  expect_equal(as.numeric(en_motor), 200)
  pares <- data.frame(tabla_1 = "a", tabla_2 = "b")
  col <- coleccion(con, c("a", "b"))
  orden <- list(a = "id", b = "n")
  truncada <- relaciones_coleccion(
    col, pares, muestra = 100, orden = orden,
    columnas_candidatas = list(a = "id", b = "fk")
  )
  expect_identical(nrow(truncada$relaciones), 0L)
  expect_identical(nrow(truncada$cobertura_pares), 1L)
  expect_match(truncada$cobertura_pares$motivo, "Sin conclusion", fixed = TRUE)
  expect_identical(truncada$meta$pares_sin_conclusion, 1L)
  expect_identical(truncada$meta$pares_comparados, 1L)
  # Control: leida entera, la referencia da la cobertura del motor.
  completa <- relaciones_coleccion(
    col, pares, muestra = 1000, orden = orden,
    columnas_candidatas = list(a = "id", b = "fk")
  )
  expect_equal(completa$relaciones$cobertura_tabla2_en_tabla1, en_motor / 200)
  expect_identical(nrow(completa$cobertura_pares), 0L)
})

test_that("la clave de un texto ASCII con barra no depende de sus vecinos", {
  # Hallazgo de esta ronda, fuera de la refutacion: la misma cadena tenia una
  # clave en un vector todo ASCII y otra en uno con algun acento.
  barra <- "a\\b"
  enie <- "\u00f1"
  expect_identical(
    .nombres_para_operar(c(barra, enie))[[1L]], .nombres_para_operar(barra)
  )
  a <- data.frame(k = c(barra, "z"), stringsAsFactors = FALSE)
  b <- data.frame(k = c(barra, enie), stringsAsFactors = FALSE)
  rel <- as.data.frame(detectar_relaciones(a, b))
  expect_identical(rel$n_valores_comunes, length(intersect(a$k, b$k)))
  expect_equal(rel$cobertura_tabla2_en_tabla1, mean(b$k %in% a$k))
})
