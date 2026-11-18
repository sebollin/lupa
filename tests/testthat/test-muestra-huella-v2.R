test_that("la huella textual v2 no colisiona en valores estructurados", {
  testthat::skip_on_cran()
  set.seed(114)
  valores <- list(
    ids = sprintf("id%05d", 1:50000),
    ids_seis = sprintf("%06d", 1:50000),
    fechas = as.character(as.Date("1970-01-01") + 1:20000),
    pares = sprintf("%d-%d", 1000000 + 1:50000, (1:50000) %% 10),
    aleatorios = replicate(50000, paste(sample(c(letters, 0:9), 8, TRUE), collapse = "")),
    prefijo = sprintf("AB%05d", 1:20000),
    decimales = sprintf("%.2f", (1:20000) / 100)
  )
  for (valor in valores) {
    hash <- lupa:::.hash_texto_huella(valor)
    pares <- paste(hash$h1, hash$h2, sep = ":")
    expect_identical(length(unique(pares)), length(valor))
  }
})

test_that("el hash de un valor no depende del lote", {
  testthat::skip_on_cran()
  valores <- sprintf("id%07d", 1:20)
  lote <- c(valores, strrep("x", 2000L))
  solo <- lupa:::.hash_texto_huella(valores)
  junto <- lupa:::.hash_texto_huella(lote)
  expect_identical(solo$h1, junto$h1[seq_along(valores)])
  expect_identical(solo$h2, junto$h2[seq_along(valores)])
})

test_that("los caminos tabulado y directo del hash son equivalentes", {
  testthat::skip_on_cran()
  valores <- sprintf("id%08d", 1:1000)
  solo <- lupa:::.hash_texto_huella(valores[[1L]])
  lote <- lupa:::.hash_texto_huella(valores)
  expect_identical(solo$h1, lote$h1[[1L]])
  expect_identical(solo$h2, lote$h2[[1L]])
})

test_that("agregar filas largas conserva la muestra existente", {
  testthat::skip_on_cran()
  n <- 10000L
  muestra <- 1000L
  datos <- data.frame(id = sprintf("id%05d", seq_len(n)))
  agregadas <- data.frame(id = paste0(
    "texto-largo-", sprintf("%05d", seq_len(n / 10L)), "-",
    strrep("z", 100L)
  ))
  ampliados <- rbind(datos, agregadas)
  antes <- lupa:::.indices_huella_tabla(datos, muestra)$indices
  despues <- lupa:::.indices_huella_tabla(ampliados, muestra)$indices
  conservadas <- intersect(datos$id[antes], ampliados$id[despues])
  expect_gte(length(conservadas), ceiling(muestra * 0.85))
})

test_that("la huella conserva los valores elegidos al invertir filas", {
  testthat::skip_on_cran()
  n <- 50000L
  datos <- data.frame(
    id = sprintf("%06d", seq_len(n)),
    numero = seq_len(n),
    grupo = factor(rep(sprintf("g-%02d", 1:20), length.out = n)),
    stringsAsFactors = FALSE
  )
  invertidos <- datos[rev(seq_len(n)), , drop = FALSE]
  s1 <- lupa:::.indices_huella_tabla(datos, 10000L)$indices
  s2 <- lupa:::.indices_huella_tabla(invertidos, 10000L)$indices
  expect_setequal(datos$id[s1], invertidos$id[s2])
  expect_identical(table(datos$numero[s1]), table(invertidos$numero[s2]))
  expect_identical(table(datos$grupo[s1]), table(invertidos$grupo[s2]))
})

test_that("la huella publica el metodo v2", {
  expect_identical(lupa:::.METODO_MUESTRA_HUELLA, "huella_v2")
})

test_that("el hash textual v2 conserva sus valores", {
  # Valores fijados con la implementacion que recorria todos los bytes de una
  # vez. Un cambio aca cambia que filas entran en la muestra.
  x <- c("a", "abc", "", NA, rawToChar(as.raw(c(0x61, 0xc3, 0xb1, 0x6f))),
         strrep("xy", 3000))
  Encoding(x[[5L]]) <- "UTF-8"
  hash <- lupa:::.hash_texto_huella(x)
  expect_identical(hash$h1, c(2961546592, 662257936, 1947474976, 528772982,
                              3799696468, 1172572404))
  expect_identical(hash$h2, c(1818325621, 1944727240, 33350994, 2060038521,
                              3743254932, 1743921137))
})

test_that("recorrer los bytes por bloques no cambia el hash", {
  set.seed(4471)
  texto <- function(k) paste(sample(c(letters, " ", "\\"), k, TRUE), collapse = "")
  acentos <- rawToChar(as.raw(c(0x61, 0xc3, 0xb1, 0x6f, 0xe4, 0xb8, 0xad)))
  Encoding(acentos) <- "UTF-8"
  crudo <- rawToChar(as.raw(c(0x41, 0xff, 0xfe, 0x42)))
  Encoding(crudo) <- "bytes"
  semillas <- c(0x811c9dc5, 0x9e3779b9)
  hash <- function(x, ...) {
    lupa:::.hash_bytes_huella_vectorizado(x, semillas[[1L]], semillas[[2L]], ...)
  }
  # Pocos bytes: sin tabla precalculada, cada termino byte por byte.
  pocos <- c(vapply(c(0L, 1L, 2L, 7L, 8L, 63L, 64L, 65L, 300L), texto, ""),
             acentos, strrep(acentos, 40L), crudo)
  completo <- hash(pocos)
  for (bloque in c(1, 7, 64)) {
    expect_identical(hash(pocos, bloque = bloque), completo, info = paste("bloque", bloque))
  }
  # Muchos bytes: con la tabla, y un valor mas largo que ella para que sus
  # ultimas posiciones se calculen aparte.
  muchos <- c(vapply(sample(0:600, 4000L, TRUE), texto, ""), texto(5000L), pocos)
  completo <- hash(muchos)
  for (bloque in c(1000, 4999, 100000)) {
    expect_identical(hash(muchos, bloque = bloque), completo, info = paste("bloque", bloque))
  }
  for (i in c(1L, 2L, 4001L, length(muchos) - 1L, length(muchos))) {
    solo <- hash(muchos[i])
    expect_identical(solo$h1, completo$h1[i], info = i)
    expect_identical(solo$h2, completo$h2[i], info = i)
  }
})

test_that("la seleccion por huella conserva sus filas", {
  # Filas fijadas con la implementacion que calculaba el hash fila por fila.
  n <- 60L
  i <- seq_len(n)
  d <- data.frame(
    id = sprintf("c%03d", i %% 23L),
    monto = c(0, -0, NA, NaN, Inf)[(i %% 5L) + 1L] + (i %% 3L),
    fecha = as.Date("2024-01-01") + (i %% 7L),
    marca = (i %% 4L) == 0L,
    grupo = factor(c("a", "b", NA)[(i %% 3L) + 1L]),
    stringsAsFactors = FALSE
  )
  expect_identical(
    lupa:::.indices_huella_tabla(d, 12L)$indices,
    c(54L, 40L, 7L, 52L, 46L, 34L, 39L, 4L, 22L, 15L, 47L, 58L)
  )
  expect_identical(
    lupa:::.indices_huella_vector(d$monto, 9L),
    c(31L, 32L, 24L, 2L, 50L, 9L, 14L, 53L, 60L)
  )
})

test_that("el hash por valor distinto es el de cada elemento", {
  set.seed(5512)
  n <- 3000L
  columnas <- list(
    dobles = sample(c(0, -0, NA, NaN, Inf, -Inf, 2^31, 2^32, 1.5, -2.25), n, TRUE),
    enteros = sample(c(NA, -3:3, .Machine$integer.max), n, TRUE),
    fechas = as.Date("2020-01-01") + sample(c(NA, 0:9), n, TRUE),
    horas = as.POSIXct("2020-01-01", tz = "UTC") + sample(c(NA, 0:9) * 60, n, TRUE),
    logicos = sample(c(TRUE, FALSE, NA), n, TRUE),
    crudos = as.raw(sample(0:3, n, TRUE)),
    textos = sample(c(NA, "", "a", "b\\c", strrep("z", 40L)), n, TRUE),
    faltantes = rep(NA_character_, n),
    factor = factor(sample(c("x", "y", NA), n, TRUE))
  )
  if (requireNamespace("bit64", quietly = TRUE)) {
    columnas$int64 <- bit64::as.integer64(sample(c(-2:2, NA), n, TRUE))
  }
  for (nombre in names(columnas)) {
    x <- columnas[[nombre]]
    unicos <- lupa:::.hash_columna_huella_unicos(x)
    elemento <- lupa:::.hash_columna_huella(x)
    if (!is.null(unicos$indice)) {
      expect_lte(length(unicos$h1), 12L)
      unicos$h1 <- unicos$h1[unicos$indice]
      unicos$h2 <- unicos$h2[unicos$indice]
    }
    expect_identical(unicos$h1, elemento$h1, info = nombre)
    expect_identical(unicos$h2, elemento$h2, info = nombre)
  }
})
