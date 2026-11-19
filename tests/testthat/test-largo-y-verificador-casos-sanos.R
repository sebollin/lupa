testthat::skip_on_cran()

# Datos sanos que el hallazgo de largo y el del verificador no deben acusar:
# formatos validos de largo distinto y documentos validos escritos de otra forma.

.n_hallazgos <- function(datos, tipo, ...) {
  p <- suppressWarnings(perfilar(datos, nombre = "t", ...))
  h <- as.data.frame(p$hallazgos)
  h[h$tipo_hallazgo == tipo, , drop = FALSE]
}

.digito_ci_s <- function(base) {
  pesos <- c(2, 9, 8, 7, 6, 3, 4)
  vapply(base, function(b) {
    d <- as.integer(strsplit(sprintf("%07d", b), "")[[1L]])
    (10 - sum(d * pesos) %% 10) %% 10
  }, numeric(1L))
}

.rut_valido <- function(pref) {
  repeat {
    cuerpo <- sprintf("%02d%06d%03d", pref, sample(0:999999, 1L), 1)
    d <- as.integer(strsplit(cuerpo, "")[[1L]])
    v <- (-sum(d[1:11] * c(4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2))) %% 11
    r <- paste0(cuerpo, sprintf("%d", v))
    if (validar_rut_uy(r)) return(r)
  }
}

test_that("el cero inicial constante es un prefijo y no un relleno perdido", {
  set.seed(101)
  # Celulares 09... y fijos de ocho cifras.
  tel <- c(sprintf("09%07d", sample(1000000:9999999, 400L, TRUE)),
           sprintf("%d%07d", sample(2:4, 40L, TRUE), sample(0:9999999, 40L, TRUE)))
  expect_equal(nrow(.n_hallazgos(data.frame(tel = tel), "largo_codigo_inconsistente")), 0L)
})

test_that("cedulas y RUT validos sin su cero inicial no son relleno perdido", {
  set.seed(109)
  base <- sample(1:999999, 400L)
  ocho <- sprintf("%07d%d", base, .digito_ci_s(base))
  siete <- sub("^0", "", ocho[341:400])
  expect_true(all(validar_ci_uy(siete)))
  d <- data.frame(documento = c(ocho[1:340], siete), stringsAsFactors = FALSE)
  expect_equal(nrow(.n_hallazgos(d, "largo_codigo_inconsistente")), 0L)
  expect_equal(nrow(.n_hallazgos(d, "digito_verificador_invalido")), 0L)
  set.seed(110)
  rut <- vapply(sample(1:9, 400L, TRUE), .rut_valido, character(1L))
  once <- sub("^0", "", rut[341:400])
  expect_true(all(validar_rut_uy(once)))
  d <- data.frame(documento = c(rut[1:340], once), stringsAsFactors = FALSE)
  expect_equal(nrow(.n_hallazgos(d, "largo_codigo_inconsistente")), 0L)
  expect_equal(nrow(.n_hallazgos(d, "digito_verificador_invalido")), 0L)
})

test_that("un solo valor con cero inicial no sostiene un relleno", {
  set.seed(104)
  cod <- c(as.character(sample(10000:99999, 399L)), "01234",
           sprintf("%04d", sample(1000:9999, 40L)))
  expect_equal(nrow(.n_hallazgos(data.frame(cod = cod), "largo_codigo_inconsistente")), 0L)
})

test_that("si el largo es ambiguo, la evidencia dice tambien la otra lectura", {
  set.seed(103)
  fact <- c(sprintf("%06d", sample(1:999999, 360L, TRUE)),
            sprintf("%05d", sample(10000:99999, 40L, TRUE)))
  h <- .n_hallazgos(data.frame(factura = fact), "largo_codigo_inconsistente")
  expect_equal(nrow(h), 1L)
  expect_match(h$sugerencia, "otra numeraci", fixed = TRUE)
})

test_that("los positivos verdaderos de largo siguen: relleno parcial de un codigo", {
  set.seed(2)
  cod <- sprintf("%05d", sample(1:99999, 400L, TRUE))
  cortos <- c(20L, 90L, 200L)
  cod[cortos] <- c("123", "7", "4567")
  h <- .n_hallazgos(data.frame(cod = cod), "largo_codigo_inconsistente")
  expect_equal(h$n_afectados, 3)
  # Y una secuencia rellenada entera: todos empiezan con dos ceros.
  sec <- sprintf("%05d", 1:300)
  sec[c(5L, 6L)] <- c("5", "6")
  h <- .n_hallazgos(data.frame(cod = sec), "largo_codigo_inconsistente")
  expect_equal(h$n_afectados, 2)
})

test_that("el verificador no acusa lo que una exportacion agrega ni otros tipos", {
  set.seed(5)
  base <- sample(1000000:4900000, 400L)
  ci <- sprintf("%d%d", base, .digito_ci_s(base))
  # `.0`, punto final y comas de miles: validos.
  ex <- ci
  ex[1:20] <- paste0(ci[1:20], ".0")
  ex[21:40] <- paste0(ci[21:40], ".")
  ex[41:60] <- paste0(substr(ci[41:60], 1, 1), ",", substr(ci[41:60], 2, 4), ",",
                      substr(ci[41:60], 5, 7), "-", substr(ci[41:60], 8, 8))
  expect_equal(nrow(.n_hallazgos(data.frame(documento = ex), "digito_verificador_invalido")), 0L)
  # Cedulas y RUT validos en la misma columna.
  set.seed(6)
  rut <- vapply(sample(1:9, 60L, TRUE), .rut_valido, character(1L))
  mezcla <- c(ci[1:340], rut)
  expect_equal(nrow(.n_hallazgos(data.frame(documento = mezcla), "digito_verificador_invalido")), 0L)
  # Pasaportes con letra, series de seis cifras y el cuerpo de ceros.
  otro <- c(ci[1:340], sprintf("A%07d", sample(1:9999999, 20L)),
            sprintf("%06d", sample(100000:999999, 20L)), rep("00000000", 20L))
  expect_equal(nrow(.n_hallazgos(data.frame(documento = otro), "digito_verificador_invalido")), 0L)
})

test_that("con otras formas presentes, el verificador cuenta solo las de la misma forma", {
  set.seed(7)
  base <- sample(1000000:4900000, 400L)
  ci <- sprintf("%d%d", base, .digito_ci_s(base))
  malas <- c(3L, 50L, 120L)
  ci[malas] <- sprintf("%d%d", base[malas], (.digito_ci_s(base[malas]) + 1) %% 10)
  ci[300:319] <- sprintf("A%07d", sample(1:9999999, 20L))
  h <- .n_hallazgos(data.frame(documento = ci), "digito_verificador_invalido")
  expect_equal(h$n_afectados, 3)
  expect_match(h$evidencia, "otra forma", fixed = TRUE)
})
