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
