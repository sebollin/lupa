# La proteccion de datos personales, mas rapida y con el mismo resultado.

test_that("el formateo vectorial da lo mismo que el de uno en uno", {
  set.seed(16)
  x <- c(stats::runif(2000, -1e9, 1e9),
         stats::rexp(2000) * 10^sample(-12:18, 2000, TRUE),
         0.1 + 0.2, -0, NA, NaN, Inf, -Inf, 1e15, 1e-5, 1e-7, 2^53)
  for (marca in c(".", ",")) {
    viejas <- options(OutDec = marca)
    uno_a_uno <- vapply(x, function(v) {
      if (is.na(v)) NA_character_ else lupa:::.formatear_numero_publicado(v)
    }, character(1L))
    expect_identical(lupa:::.formatear_numeros_uno_a_uno(x), uno_a_uno)
    options(viejas)
  }
  casos <- list(
    c("Ana", NA, ""), factor(c("a", NA)), c(TRUE, NA), c(1L, NA, -5L),
    as.Date("2020-01-01") + c(0:3, NA),
    as.POSIXct("2020-01-01 10:00:00", tz = "America/Montevideo") + c(0, 3600.5, NA)
  )
  for (caso in casos) {
    expect_identical(
      lupa:::.texto_valor_vector(caso),
      vapply(seq_along(caso), function(i) lupa:::.texto_valor(caso[i]), character(1L))
    )
  }
})

test_that("el prefiltro nunca descarta un valor que aparece", {
  set.seed(17)
  piezas <- c("ana", "perez", "771", "77177101", "caf\u00e9", "\u00f1and\u00fa",
              "maria", "nunez", "x", "zz", "0.5")
  textos <- vapply(1:300, function(i) {
    paste(sample(piezas, sample(1:6, 1), TRUE), collapse = sample(c(" ", "", ".", "@"), 1))
  }, "")
  valores <- unique(c(piezas, vapply(1:200, function(i) {
    paste(sample(piezas, 2, TRUE), collapse = "")
  }, ""), "ausente1", "otro ausente"))
  for (k in c(3L, 6L)) {
    posibles <- lupa:::.valores_que_pueden_aparecer(valores, textos, k)
    aparecen <- valores[vapply(valores, function(v) {
      any(grepl(v, textos, fixed = TRUE, useBytes = TRUE))
    }, logical(1L))]
    expect_true(all(aparecen %in% posibles))
  }
  # Control: lo que no aparece en ningun lado se descarta.
  expect_false("ausente1" %in% lupa:::.valores_que_pueden_aparecer(valores, textos, 3L))
})

test_that("el trazador registra la clasificacion y la proteccion", {
  datos <- data.frame(documento = c(41234567, 51234568, 31234561),
                      x = c(1, 2, 3))
  trazador <- lupa:::.trazador_tiempos_dbi(TRUE)
  attr(datos, "lupa_trazador_tiempos_dbi") <- trazador
  invisible(suppressWarnings(perfilar(datos, columnas_personales = "documento")))
  etapas <- lupa:::.resumen_tiempos_dbi(trazador)$etapa
  expect_true(all(c("clasificacion_datos_personales",
                    "proteccion_datos_personales") %in% etapas))
})
