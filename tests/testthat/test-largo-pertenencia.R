testthat::skip_on_cran()

# Un valor corto es un cero perdido solo si, rellenado con ceros, su principio
# es uno que los valores dominantes tienen. Mide los dos lados: lo que se
# acusaba de mas (prefijos que no son relleno) y lo que se callaba (relleno de
# ancho fijo con un solo cero).

.largo_h <- function(datos, ...) {
  p <- suppressWarnings(perfilar(datos, nombre = "t", ...))
  h <- as.data.frame(p$hallazgos)
  h[h$tipo_hallazgo == "largo_codigo_inconsistente", , drop = FALSE]
}

.pierde_ceros <- function(correctos, n_perdidos) {
  perdidos <- sub("^0+", "", sample(correctos, n_perdidos))
  c(correctos, perdidos)
}

test_that("un prefijo internacional 00 no vuelve relleno al fijo local", {
  set.seed(205)
  inter <- paste0("00598", sprintf("%08d", sample(0:99999999, 364L, TRUE)))
  local <- sprintf("%d%07d", sample(2:4, 36L, TRUE), sample(0:9999999, 36L, TRUE))
  expect_equal(nrow(.largo_h(data.frame(tel = c(inter, local)))), 0L)
  # Ni los celulares 09... con fijos de ocho cifras.
  cel <- sprintf("09%07d", sample(1000000:9999999, 400L, TRUE))
  expect_equal(nrow(.largo_h(data.frame(tel = c(cel, local)))), 0L)
})

test_that("el relleno de ancho fijo con un solo cero se acusa aunque encabece casi todo", {
  set.seed(216)
  casos <- list(
    departamento = sprintf("%02d", sample(1:9, 360L, TRUE)),
    barrio = sprintf("%03d", sample(10:99, 364L, TRUE)),
    ciiu = sprintf("%04d", sample(111:999, 360L, TRUE)),
    cuenta = sprintf("%010d", sample(100000000:999999999, 360L, TRUE))
  )
  for (nombre in names(casos)) {
    perdidos <- 40L
    valores <- c(casos[[nombre]], sub("^0+", "", sample(casos[[nombre]], perdidos)))
    h <- .largo_h(data.frame(v = valores))
    expect_equal(nrow(h), 1L, info = nombre)
    expect_equal(h$n_afectados, perdidos, info = nombre)
  }
})

test_that("el veredicto no depende de que el cero encabece el 89 % o el 91 %", {
  set.seed(203)
  for (con_cero in c(356L, 364L)) {
    sin_cero <- 400L - con_cero
    correctos <- c(sprintf("%03d", sample(10:99, con_cero, TRUE)),
                   sprintf("%03d", sample(100:999, sin_cero, TRUE)))
    valores <- c(correctos, sub("^0+", "", sample(correctos[1:con_cero], 40L)))
    h <- .largo_h(data.frame(v = valores))
    expect_equal(nrow(h), 1L, info = con_cero)
    expect_equal(h$n_afectados, 40, info = con_cero)
  }
})

test_that("un grupo con pocos truncados no acusa a los documentos validos", {
  set.seed(301)
  digito <- function(b) {
    pesos <- c(2, 9, 8, 7, 6, 3, 4)
    vapply(b, function(x) {
      d <- as.integer(strsplit(sprintf("%07d", x), "")[[1L]])
      (10 - sum(d * pesos) %% 10) %% 10
    }, numeric(1L))
  }
  ocho <- function(b) sprintf("%07d%d", b, digito(b))
  grande <- ocho(sample(1:4900000, 200L))
  corto_b <- sample(1:999999, 20L)
  grupo_a <- c(ocho(c(sample(1:999999, 40L), sample(1000000:4900000, 60L))),
               sub("^0", "", ocho(corto_b)),
               sprintf("%d", sample(1000000:9999999, 5L)))
  datos <- data.frame(
    doc = c(grande, grupo_a),
    g = c(rep("b", length(grande)), rep("a", length(grupo_a))),
    stringsAsFactors = FALSE
  )
  h <- .largo_h(datos[datos$g == "a", "doc", drop = FALSE])
  # Los de siete cifras que cumplen el verificador se excluyen; quedan los cinco.
  expect_true(nrow(h) == 0L || h$n_afectados <= 5)
  por <- suppressWarnings(perfilar_por(datos, "g", analizar_dependencias = FALSE))
  filas <- por[por$tipo_hallazgo == "largo_codigo_inconsistente" & por$grupo == "a", ]
  expect_true(nrow(filas) == 0L || all(filas$n_afectados <= 5))
})

test_that("los formatos ambiguos siguen avisando y dicen la otra lectura", {
  set.seed(103)
  fact <- c(sprintf("%06d", sample(1:999999, 360L, TRUE)),
            sprintf("%05d", sample(10000:99999, 40L, TRUE)))
  h <- .largo_h(data.frame(f = fact))
  expect_equal(nrow(h), 1L)
  expect_match(h$sugerencia, "otra numeraci", fixed = TRUE)
})

test_that("el verificador avisa que un documento de otro tipo con las mismas cifras no se distingue", {
  set.seed(7)
  digito <- function(b) {
    pesos <- c(2, 9, 8, 7, 6, 3, 4)
    vapply(b, function(x) {
      d <- as.integer(strsplit(sprintf("%07d", x), "")[[1L]])
      (10 - sum(d * pesos) %% 10) %% 10
    }, numeric(1L))
  }
  b <- sample(1000000:4900000, 400L)
  ci <- sprintf("%d%d", b, digito(b))
  ci[1:3] <- sprintf("%d%d", b[1:3], (digito(b[1:3]) + 1) %% 10)
  p <- suppressWarnings(perfilar(data.frame(documento = ci), nombre = "t"))
  h <- as.data.frame(p$hallazgos)
  h <- h[h$tipo_hallazgo == "digito_verificador_invalido", ]
  expect_equal(h$n_afectados, 3)
  expect_match(h$evidencia, "no se distingue de un d", fixed = FALSE)
})
