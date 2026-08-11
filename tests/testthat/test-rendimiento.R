test_that("un millón de valores se perfila en pocos segundos", {
  skip_on_cran()
  skip_on_ci()
  invisible(perfilar(data.frame(codigo = "AB1234")))
  datos <- data.frame(
    codigo = rep(c("AB1234", "CD5678", "EF9012", "GH3456"), length.out = 1e6)
  )

  tiempo <- system.time(resultado <- perfilar(datos))[["elapsed"]]

  expect_lt(unname(tiempo), 8)
})

test_that("un millón de valores activa el muestreo declarado", {
  skip_on_cran()
  datos <- data.frame(
    codigo = rep(c("AB1234", "CD5678", "EF9012", "GH3456"), length.out = 1e6)
  )
  resultado <- perfilar(datos)
  expect_true(resultado$meta$muestreo)
  expect_equal(resultado$meta$filas_analizadas, 1e5)
})

test_that("los predicados invisibles decodifican cada celda como maximo una vez", {
  llamadas <- 0L
  valores_decodificados <- 0L
  original <- lupa:::.predicados_invisibles
  local_mocked_bindings(
    .predicados_invisibles = function(textos) {
      llamadas <<- llamadas + 1L
      valores_decodificados <<- valores_decodificados + length(textos)
      original(textos)
    },
    .package = "lupa"
  )
  n <- 2000L
  datos <- data.frame(
    a = rep(c("Ana", "Beto"), length.out = n),
    b = rep(c("Ana", "Beto"), length.out = n),
    c = rep(c("Ana", "Beto"), length.out = n),
    d = rep(c("Ana", "Beto"), length.out = n),
    e = rep(c("Ana", "Beto"), length.out = n),
    stringsAsFactors = FALSE
  )
  perfilar(datos, analizar_dependencias = FALSE)
  expect_equal(llamadas, ncol(datos))
  expect_lte(valores_decodificados, nrow(datos) * ncol(datos))
})

test_that("texto libre de cardinalidad alta no degrada el perfil", {
  skip_on_cran()
  skip_on_ci()
  set.seed(3)
  n <- 1e4
  libre <- vapply(seq_len(n), function(i) {
    paste(sample(
      c(letters, LETTERS, 0:9, " ", ".", ",", "-"),
      sample(10:40, 1), TRUE
    ), collapse = "")
  }, character(1L))
  datos <- data.frame(obs1 = libre, obs2 = rev(libre), obs3 = libre)

  tiempo <- system.time(resultado <- perfilar(datos))[["elapsed"]]

  expect_lt(unname(tiempo), 5)
})

test_that("texto libre conserva memoria y resumen de patrones", {
  skip_on_cran()
  set.seed(3)
  n <- 1e4
  libre <- vapply(seq_len(n), function(i) {
    paste(sample(
      c(letters, LETTERS, 0:9, " ", ".", ",", "-"),
      sample(10:40, 1), TRUE
    ), collapse = "")
  }, character(1L))
  datos <- data.frame(obs1 = libre, obs2 = rev(libre), obs3 = libre)
  resultado <- perfilar(datos)
  expect_lt(as.numeric(object.size(resultado)), 5 * 1024^2)
  expect_true(all(
    attr(resultado$dependencias, "columnas_descartadas")$motivo == "casi_clave"
  ))
  expect_true(all(vapply(
    resultado$patrones,
    function(x) nrow(attr(x, "resumen_patrones")) <= 7L,
    logical(1L)
  )))
})
