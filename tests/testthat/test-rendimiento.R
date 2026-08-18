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
  n <- 20000L
  valores <- as.character(seq.int(10000000L, length.out = n))
  datos <- data.frame(
    a = valores,
    b = rev(valores),
    c = paste0("9", valores),
    stringsAsFactors = FALSE
  )
  perfilar(datos, analizar_dependencias = FALSE)
  expect_equal(llamadas, ncol(datos))
  celdas <- nrow(datos) * ncol(datos)
  expect_equal(valores_decodificados, celdas)
  # Margen cero a propósito: cinco pasadas por valor (la regresión de la
  # ronda 75) tiene que fallar este mismo guardián.
  expect_failure(expect_lte(valores_decodificados * 5L, celdas))
})

test_that("la trazabilidad de otro hallazgo no calcula invisibles", {
  x <- c(rep("AB123", 90L), sprintf("texto-%02d", seq_len(10L)))
  resultado <- lupa:::.perfilar_columna(
    x, "x", Inf, 20L, TRUE, FALSE, 0.05,
    sentinelas_numericos = c(-9, -99, -999, -9999, 999)
  )
  patrones <- resultado$patrones
  attr(patrones, "resumen_patrones") <- as.data.frame(patrones)
  attr(patrones, "n_patrones_distintos") <- 2L
  attr(patrones, "desvios_patron_raro") <- as.data.frame(patrones)[
    -1L, , drop = FALSE
  ]
  resultado$patrones <- patrones
  llamadas <- 0L
  original <- lupa:::.predicados_invisibles
  local_mocked_bindings(
    .predicados_invisibles = function(textos) {
      llamadas <<- llamadas + 1L
      original(textos)
    },
    .package = "lupa"
  )
  indices <- lupa:::.indices_hallazgo_columna(
    "patron_raro", x, resultado$fila, resultado,
    expandir = FALSE, distinguir_mayusculas = TRUE
  )
  expect_gt(length(indices), 0L)
  expect_equal(llamadas, 0L)
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

  # La ronda 78 triplicó el costo del diagnóstico de vocabulario y lo llevó
  # de 1,5 s a casi 5 s. El contador determinista del test siguiente vigila
  # el trabajo fino; este reloj queda como red de arrastre con margen para el
  # calentamiento de un proceso limpio, pero sigue detectando un desastre
  # algorítmico.
  expect_lt(unname(tiempo), 12)
})

test_that("el diagnóstico de vocabulario respeta su presupuesto de pares", {
  skip_if_not_installed("stringdist")
  pares_comparados <- 0
  llamadas <- 0L
  original <- lupa:::.comparar_bloques_duplicados
  local_mocked_bindings(
    .comparar_bloques_duplicados = function(
        valores, filas, metodo, umbral, bloque, max_resultados, ...) {
      llamadas <<- llamadas + 1L
      pares_comparados <<- pares_comparados +
        choose(length(valores), 2L)
      original(valores, filas, metodo, umbral, bloque, max_resultados, ...)
    },
    .package = "lupa"
  )
  valores <- sprintf("valor-%08d", seq_len(10000L))
  perfil <- perfilar(
    data.frame(valor = valores), analizar_dependencias = FALSE
  )
  expect_equal(llamadas, 1L)
  expect_equal(pares_comparados, choose(2000L, 2L))
  expect_lte(pares_comparados, 2000000)
  # Margen cero a propósito: una segunda pasada completa vuelve a fallar.
  expect_failure(expect_lte(pares_comparados * 2L, 2000000))
})

test_that("el perfil no conserva el caché del detector de meses", {
  datos <- data.frame(
    a = rep("texto", 100000L), b = rep("otro", 100000L),
    c = rep("15 de marzo de 2024", 100000L)
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  inferencia <- inferir_tipo(datos$a)
  expect_false("meses_texto" %in% names(attributes(inferencia$formatos_fecha)))
  expect_lt(as.numeric(object.size(inferencia)), 1 * 1024^2)
  expect_false(any(vapply(
    perfil$formatos_fecha,
    function(x) "meses_texto" %in% names(attributes(x)),
    logical(1L)
  )))
  expect_lt(as.numeric(object.size(perfil)), 2 * 1024^2)
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
