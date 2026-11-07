# Las dependencias se buscan en la muestra y las publicadas se recalculan sobre
# la tabla: el informe y la consola no pueden decir que sus cifras son de la
# muestra.

test_that("el informe y la consola dicen que lo publicado se recalculo", {
  set.seed(2582)
  n <- 2000L
  x <- rep(paste0("g", 1:20), each = 100)
  y <- paste0("v", x)
  y[c(2L, 4L)] <- "ROTO"
  datos <- data.frame(x = x, y = y, stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(datos, muestra = 1000))
  expect_true(isTRUE(attr(perfil$dependencias, "muestreado")))
  fila <- perfil$dependencias[perfil$dependencias$determinante == "x", ]
  expect_equal(fila$n_evaluados, n)
  expect_equal(fila$n_violaciones, 2L)
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(perfil, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "")
  expect_match(html, "sobre una muestra de 1.000 de 2.000 filas", fixed = TRUE)
  expect_match(html, "las publicadas se recalcularon sobre la tabla entera",
               fixed = TRUE)
  expect_false(grepl("son los de la muestra", html, fixed = TRUE))
  consola <- paste(cli::cli_fmt(print(perfil)), collapse = " ")
  expect_match(consola, "recalcularon sobre la tabla entera", fixed = TRUE)
})
