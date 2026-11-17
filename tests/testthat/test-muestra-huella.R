test_that("la huella conserva la muestra al permutar filas repetidas", {
  d <- data.frame(
    id = rep(c("a", "b", "c", "d"), each = 30L),
    valor = rep(c("uno", "dos", "tres", "cuatro"), each = 30L),
    stringsAsFactors = FALSE
  )
  permutacion <- rev(seq_len(nrow(d)))
  primero <- lupa:::.indices_huella_tabla(d, 40L)$indices
  segundo <- lupa:::.indices_huella_tabla(d[permutacion, , drop = FALSE], 40L)$indices
  expect_identical(
    table(d$id[primero]),
    table(d$id[permutacion[segundo]])
  )
})

test_that("la canonizacion une NA, NaN, ceros y representaciones de texto", {
  valores <- c(NA_real_, NaN, 0, -0, 1, 2, 0, -0, 3, 4)
  d1 <- data.frame(v = valores, stringsAsFactors = FALSE)
  d2 <- d1[c(10L, 3L, 1L, 8L, 2L, 7L, 5L, 9L, 4L, 6L), , drop = FALSE]
  s1 <- lupa:::.indices_huella_tabla(d1, 5L)$indices
  s2 <- lupa:::.indices_huella_tabla(d2, 5L)$indices
  expect_identical(table(d1$v[s1]), table(d2$v[s2]))

  texto <- rep(c("uno", "dos", "tres", "cuatro"), 30L)
  character_datos <- data.frame(v = texto, stringsAsFactors = FALSE)
  factor_datos <- data.frame(v = factor(texto), stringsAsFactors = FALSE)
  expect_identical(
    lupa:::.indices_huella_tabla(character_datos, 40L)$indices,
    lupa:::.indices_huella_tabla(factor_datos, 40L)$indices
  )
})

test_that("la huella trata integer64 grande sin convertirlo a doble", {
  testthat::skip_if_not_installed("bit64")
  valores <- bit64::as.integer64(c(
    "9007199254740992", "9007199254740993", "9007199254740994",
    "9007199254740995", "9007199254740996"
  ))
  d1 <- data.frame(v = rep(valores, 20L))
  d2 <- d1[c(80:1, 81:nrow(d1)), , drop = FALSE]
  s1 <- lupa:::.indices_huella_tabla(d1, 20L)$indices
  s2 <- lupa:::.indices_huella_tabla(d2, 20L)$indices
  expect_identical(
    bit64::as.character.integer64(d1$v[s1]),
    bit64::as.character.integer64(d2$v[s2])
  )
})

test_that("la huella canoniza latin1 y UTF-8 y conserva bytes invalidos", {
  latin1 <- rawToChar(as.raw(c(97L, 241L, 111L)))
  Encoding(latin1) <- "latin1"
  utf8 <- "a\u00f1o"
  d1 <- data.frame(v = rep(c(latin1, "x", "y"), 40L))
  d2 <- data.frame(v = rep(c(utf8, "x", "y"), 40L))
  expect_identical(
    lupa:::.indices_huella_tabla(d1, 50L)$indices,
    lupa:::.indices_huella_tabla(d2, 50L)$indices
  )

  invalido <- rawToChar(as.raw(c(65L, 255L, 66L)))
  Encoding(invalido) <- "bytes"
  con_invalido <- data.frame(v = rep(c(invalido, "x", "y"), 40L))
  permutacion <- sample.int(nrow(con_invalido))
  s1 <- lupa:::.indices_huella_tabla(con_invalido, 50L)$indices
  s2 <- lupa:::.indices_huella_tabla(
    con_invalido[permutacion, , drop = FALSE], 50L
  )$indices
  expect_identical(
    table(lupa:::.clave_bytes(lupa:::.canonizar_texto_huella(con_invalido$v[s1]))),
    table(lupa:::.clave_bytes(lupa:::.canonizar_texto_huella(
      con_invalido$v[permutacion[s2]]
    )))
  )
})

test_that("la huella es igual bajo C y C.UTF-8", {
  texto <- rep(c("a\u00f1o", "x", "y"), 40L)
  datos <- data.frame(v = texto, stringsAsFactors = FALSE)
  locales <- c("C", "C.UTF-8")
  resultados <- lapply(locales, function(locale) {
    anterior <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", anterior), add = TRUE)
    if (is.na(Sys.setlocale("LC_CTYPE", locale))) return(NULL)
    lupa:::.indices_huella_tabla(datos, 50L)$indices
  })
  testthat::skip_if(any(vapply(resultados, is.null, logical(1L))))
  expect_identical(resultados[[1L]], resultados[[2L]])
})

test_that("cambiar una celda y agregar filas tienen localidad", {
  d <- data.frame(id = seq_len(1000L), valor = seq_len(1000L))
  s1 <- lupa:::.indices_huella_tabla(d, 100L)$indices
  cambiado <- d
  cambiado$valor[[777L]] <- -1L
  s2 <- lupa:::.indices_huella_tabla(cambiado, 100L)$indices
  expect_gte(length(intersect(s1, s2)), 99L)

  agregado <- rbind(d, data.frame(id = 1001L, valor = 1001L))
  s3 <- lupa:::.indices_huella_tabla(agregado, 100L)$indices
  expect_gte(length(intersect(s1, s3)), 99L)
})

test_that("una clave excluye las columnas nuevas de la huella", {
  d <- data.frame(id = seq_len(1000L), valor = rep(letters, length.out = 1000L))
  d2 <- transform(d, nueva = rev(d$valor))
  s1 <- lupa:::.indices_huella_tabla(d, 100L, columnas = "id")$indices
  s2 <- lupa:::.indices_huella_tabla(d2, 100L, columnas = "id")$indices
  expect_identical(s1, s2)
})

test_that("las columnas se combinan sin depender de su orden", {
  d <- data.frame(id = seq_len(1000L), valor = rep(letters, length.out = 1000L))
  d2 <- d[c("valor", "id")]
  expect_identical(
    lupa:::.indices_huella_tabla(d, 100L)$indices,
    lupa:::.indices_huella_tabla(d2, 100L)$indices
  )
})

test_that("una columna lista queda fuera y se declara en meta", {
  testthat::skip_on_cran()
  d <- data.frame(id = seq_len(20L))
  d$lista <- I(vector("list", nrow(d)))
  huella <- lupa:::.indices_huella_tabla(d, 10L)
  expect_identical(huella$columnas, "id")
  expect_identical(huella$columnas_omitidas, "lista")
  perfil <- perfilar(
    d, muestra = 10L, fecha = "2026-01-01", analizar_dependencias = FALSE
  )
  expect_identical(perfil$meta$muestra_huella$columnas_omitidas, "lista")
})

test_that("la periodicidad no elimina el residuo raro", {
  testthat::skip_on_cran()
  n <- 19961L
  k <- 500L
  valor <- seq_len(n)
  raras <- seq.int(2L, n, by = 40L)
  datos <- data.frame(valor = valor)
  datos$valor[raras] <- -valor[raras]
  huella <- lupa:::.indices_huella_tabla(datos, k)
  expect_gt(sum(huella$indices %in% raras), 0L)
})

test_that("la seleccion por bloques coincide con la seleccion completa", {
  datos <- data.frame(
    id = seq_len(2000L), grupo = rep(seq_len(1000L), each = 2L)
  )
  huella <- lupa:::.indices_huella_tabla(datos, 500L)
  muestra <- lupa:::.muestra_con_indices(500L, huella$indices)
  for (k in c(1L, 3L, 17L)) {
    por_bloques <- lupa:::.muestrear_vector_bloques(
      datos$id, muestra, k = k, tamano = 137L
    )
    entero <- lupa:::.muestrear_vector(datos$id, muestra)
    expect_identical(por_bloques, entero)
  }
})

test_that("una permutacion publica los mismos diagnosticos muestreados", {
  testthat::skip_on_cran()
  datos <- data.frame(
    id = rep(seq_len(100L), each = 3L),
    texto = rep(c("A", "B", "C"), 100L),
    grupo = rep(c("G1", "G2", "G3"), each = 100L),
    valor = rep(c("uno", "dos", "tres"), each = 100L),
    stringsAsFactors = FALSE
  )
  datos$texto[299L] <- "RARA"
  candidatos <- setdiff(
    seq_len(nrow(datos)),
    lupa:::.indices_huella_tabla(datos, 100L)$indices
  )
  indice_candidato <- which(vapply(candidatos, function(i) {
    candidato <- datos
    candidato$valor[[i]] <- "otro"
    !i %in% lupa:::.indices_huella_tabla(candidato, 100L)$indices
  }, logical(1L)))[[1L]]
  violacion <- candidatos[[indice_candidato]]
  datos$valor[[violacion]] <- "otro"
  permutacion <- sample.int(nrow(datos))
  uno <- perfilar(
    datos, muestra = 100L, fecha = "2026-01-01", analizar_dependencias = TRUE
  )
  dos <- perfilar(
    datos[permutacion, , drop = FALSE], muestra = 100L,
    fecha = "2026-01-01", analizar_dependencias = TRUE
  )
  publicar <- function(perfil) {
    hallazgos <- perfil$hallazgos
    hallazgos$trazabilidad <- NULL
    list(
      patrones = perfil$patrones,
      formatos_fecha = perfil$formatos_fecha,
      dependencias = perfil$dependencias,
      hallazgos = hallazgos,
      cobertura = perfil$cobertura_diagnosticos
    )
  }
  expect_identical(publicar(uno), publicar(dos))
  expect_true(any(nzchar(uno$dependencias$evidencia)))
  expect_identical(uno$dependencias$evidencia, dos$dependencias$evidencia)
})
