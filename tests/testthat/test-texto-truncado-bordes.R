testthat::skip_on_cran()

# Texto cortado por un tope de campo que el hallazgo callaba: el corte cae justo
# despues de un punto, valores sin espacios (URLs) y un tope largo con apenas
# dos valores; y la errata `Q` por `O` entre categorias de una letra. Cada caso
# se acompana del dato sano que se le parece y que no se acusa.

fecha_fija <- as.POSIXct("2026-01-01", tz = "UTC")
palabras_t <- c("el", "la", "de", "paciente", "consulta", "control", "por",
                "dolor", "fiebre", "tratamiento", "se", "indica", "reposo",
                "y", "evolucion", "favorable", "sin", "complicaciones", "alta")

frases_t <- function(n, k, punto = FALSE) {
  vapply(k, function(j) {
    s <- paste(sample(palabras_t, j, TRUE), collapse = " ")
    if (punto) paste0(s, ".") else s
  }, "")
}
urls_t <- function(largos) {
  vapply(largos, function(l) {
    paste0("https://sitio.example.org/", paste(sample(letters, l, TRUE), collapse = ""))
  }, "")
}
tipos_de <- function(d, ...) {
  as.character(as.data.frame(hallazgos(perfilar(d, nombre = "t", fecha = fecha_fija, ...)))$tipo_hallazgo)
}
trunc_de <- function(x) "texto_truncado" %in% tipos_de(data.frame(v = x, stringsAsFactors = FALSE))

test_that("URLs cortadas en el tope del campo se acusan y las completas no", {
  set.seed(13)
  completas <- urls_t(sample(5:400, 600, TRUE))
  expect_false(trunc_de(completas))
  cortadas <- substr(completas, 1, 255)
  expect_gt(sum(nchar(cortadas) == 255), 20)
  expect_true(trunc_de(cortadas))
})

test_that("hashes y codigos de ancho fijo no se leen como texto cortado", {
  set.seed(14)
  hex <- function(n, k) vapply(seq_len(n), function(i) {
    paste(sample(c(0:9, letters[1:6]), k, TRUE), collapse = "")
  }, "")
  expect_false(trunc_de(c(hex(300, 32), hex(300, 40))))
  expect_false(trunc_de(c(hex(200, 32), hex(200, 40), hex(200, 64))))
  expect_false(trunc_de(hex(500, 32)))
})

test_that("las URLs cortadas dan el mismo hallazgo por grupo y desde una base", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  set.seed(17)
  cortadas <- substr(urls_t(sample(5:400, 600, TRUE)), 1, 255)
  d <- data.frame(id = seq_along(cortadas), g = rep(c("a", "b"), 300),
                  v = cortadas, stringsAsFactors = FALSE)
  expect_true("texto_truncado" %in% tipos_de(d))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", d)
  h <- as.data.frame(hallazgos(perfilar_dbi(con, "t", fecha = fecha_fija)))
  expect_true("texto_truncado" %in% as.character(h$tipo_hallazgo))
  expect_equal(
    h$n_afectados[h$tipo_hallazgo == "texto_truncado"],
    sum(nchar(cortadas) == 255)
  )
  pp <- as.data.frame(perfilar_por(d, por = "g", fecha = fecha_fija))
  expect_true("texto_truncado" %in% as.character(pp$tipo_hallazgo))
})

test_that("una URL canonica o un JSON o una ruta que se repiten en el tope no se acusan", {
  set.seed(21)
  canonica <- paste0("https://servidor.example.org/api/v1/items?", strrep("a", 213))
  base <- urls_t(sample(5:60, 400, TRUE))
  x <- c(base, rep(canonica, 150))
  expect_equal(max(nchar(x)), nchar(canonica))
  expect_false(trunc_de(x))
  json <- paste0("{\"k\":\"", strrep("v", 255 - 8), "\"}")
  expect_false(trunc_de(c(vapply(sample(10:90, 300, TRUE), function(l) {
    paste0("{\"k\":\"", strrep("v", l), "\"}")
  }, ""), rep(json, 100))))
  rutas <- vapply(sample(5:80, 300, TRUE), function(l) {
    paste0("/var/log/", paste(sample(letters, l, TRUE), collapse = ""))
  }, "")
  largo_ruta <- vapply(1:60, function(i) {
    paste0("/var/log/", paste(sample(letters, 246, TRUE), collapse = ""))
  }, "")
  expect_false(trunc_de(c(rutas, largo_ruta)))
})

test_that("las URL cortadas en 1024 y 2048 se acusan", {
  set.seed(22)
  for (tope in c(1024L, 2048L)) {
    completas <- urls_t(sample(50:(tope + 300), 400, TRUE))
    expect_true(trunc_de(substr(completas, 1, tope)))
  }
})

test_that("dos URL cortadas no alcanzan", {
  set.seed(23)
  x <- urls_t(sample(5:120, 400, TRUE))
  x[1:2] <- substr(urls_t(c(400, 400)), 1, 255)
  expect_false(trunc_de(x))
})

url_hallazgo <- function(x) {
  h <- as.data.frame(hallazgos(perfilar(
    data.frame(v = x, stringsAsFactors = FALSE), nombre = "t",
    fecha = fecha_fija
  )))
  h[h$tipo_hallazgo == "texto_truncado", , drop = FALSE]
}

test_that("el hallazgo de URL en el tope es un aviso ok que nombra las dos lecturas", {
  set.seed(16)
  cortadas <- substr(urls_t(sample(5:400, 600, TRUE)), 1, 255)
  h <- url_hallazgo(cortadas)
  expect_equal(nrow(h), 1L)
  expect_equal(as.character(h$severidad), "ok")
  expect_false(grepl("mitad de palabra", h$descripcion))
  expect_match(h$descripcion, "Puede ser el tope de un campo", fixed = TRUE)
  expect_match(h$descripcion, "largo fijo del sistema", fixed = TRUE)
  expect_match(h$evidencia, "empiezan con un esquema")
})

test_that("URL firmadas de largo fijo dan un aviso ok, nunca una sospecha", {
  set.seed(24)
  fijas <- vapply(1:120, function(i) {
    paste0("https://bucket.example.org/", paste(sample(letters, 255 - 27, TRUE), collapse = ""))
  }, "")
  h <- url_hallazgo(c(fijas, urls_t(sample(3:100, 80, TRUE))))
  expect_equal(as.character(h$severidad), "ok")
})

test_that("una columna mixta cuenta solo las URL del tope y no depende del orden", {
  set.seed(25)
  urls <- substr(urls_t(sample(300:500, 126, TRUE)), 1, 255)
  prosa <- vapply(1:14, function(i) {
    substr(paste(sample(palabras_t, 60, TRUE), collapse = " "), 1, 255)
  }, "")
  prosa <- formatC(prosa, width = -255)
  expect_true(all(nchar(prosa) == 255))
  cortas <- urls_t(sample(3:100, 60, TRUE))
  a <- url_hallazgo(c(prosa, urls, cortas))
  b <- url_hallazgo(c(urls, prosa, cortas))
  d <- url_hallazgo(c(cortas, prosa[1], urls, prosa[-1]))
  for (h in list(a, b, d)) {
    expect_equal(nrow(h), 1L)
    expect_equal(h$n_afectados, 126)
    expect_equal(as.character(h$severidad), "ok")
  }
  expect_identical(a$evidencia, b$evidencia)
  expect_identical(a$evidencia, d$evidencia)
  # Un valor que no es URL y cae primero no apaga el aviso.
  codigo <- paste0("CODIGO-", strrep("X", 248))
  expect_equal(nrow(url_hallazgo(c(codigo, urls, cortas))), 1L)
})

test_that("el aviso de URL no cuenta como correcto en print() ni en el reporte", {
  set.seed(26)
  cortadas <- substr(urls_t(sample(5:400, 600, TRUE)), 1, 255)
  p <- perfilar(data.frame(v = cortadas, stringsAsFactors = FALSE),
                nombre = "t", fecha = fecha_fija)
  salida <- paste(cli::ansi_strip(testthat::capture_messages(print(p))), collapse = "\n")
  n_ok <- sum(p$hallazgos$severidad == "ok")
  expect_match(salida, paste0("\\b", n_ok - 1L, " hallazgos? informativos? ok"))
  expect_match(salida, "aviso de columna sobre un largo de texto")
})
