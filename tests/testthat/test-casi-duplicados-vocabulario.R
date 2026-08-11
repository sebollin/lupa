test_that("las variantes que la normalizacion funde quedan nombradas", {
  datos <- data.frame(
    localidad = c(
      rep("San José", 20), rep("San Jose", 5),
      rep("Montevideo", 20), rep("MONTEVIDEO", 3), rep("Montevido", 2),
      rep("Canelones", 10), rep("Canelónes", 2), rep("Canelone", 1),
      "sin_variantes"
    ),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, duplicados_aproximados = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(hallazgo$evidencia, "San José")
  expect_match(hallazgo$evidencia, "San Jose")
  expect_match(hallazgo$evidencia, "\\(20\\)")
  expect_match(hallazgo$evidencia, "\\(5\\)")
  expect_match(hallazgo$evidencia, "origen=normalizacion")
  expect_false(grepl("sin_variantes", hallazgo$evidencia, fixed = TRUE))
  expect_equal(hallazgo$unidad_conteo, "valor_distinto")
  grupos <- lupa:::.grupos_casi_duplicados_vocabulario(
    datos$localidad, lupa:::.resolver_normalizacion(TRUE), "localidad"
  )$grupos
  asimetrias <- vapply(grupos, `[[`, numeric(1L), "asimetria")
  expect_true(all(diff(asimetrias) <= 0))
})

test_that("los grupos usan el vocabulario y no la cantidad de filas", {
  perfil <- lupa:::.resolver_normalizacion(TRUE)
  corto <- c("San José", "San Jose", "Canelones", "Canelónes", "aislado")
  largo <- c(
    rep("San José", 100), rep("San Jose", 4),
    rep("Canelones", 80), rep("Canelónes", 2), rep("aislado", 60)
  )
  grupos_corto <- lupa:::.grupos_casi_duplicados_vocabulario(
    corto, perfil, "x"
  )$grupos
  grupos_largo <- lupa:::.grupos_casi_duplicados_vocabulario(
    largo, perfil, "x"
  )$grupos
  formas <- function(grupos) {
    salida <- lapply(grupos, function(x) sort(x$variantes))
    salida[order(vapply(salida, function(x) paste(x, collapse = "|"),
                            character(1L)))]
  }
  expect_equal(formas(grupos_corto), formas(grupos_largo))
  expect_equal(
    sort(unname(unlist(lapply(grupos_largo, `[[`, "frecuencias")))),
    sort(c(100L, 4L, 80L, 2L))
  )
})

test_that("un vocabulario sin variantes no recibe grupos", {
  set.seed(7801)
  valores <- replicate(300, paste(sample(letters, 18, replace = TRUE),
                                  collapse = ""))
  valores <- make.unique(valores)
  resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
    valores, lupa:::.resolver_normalizacion(TRUE), "x"
  )
  expect_length(resultado$grupos, 0L)
})

test_that("sin stringdist se conservan las fusiones exactas y se declara la ausencia", {
  local_mocked_bindings(
    .stringdist_disponible = function() FALSE,
    .package = "lupa"
  )
  resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
    c("San José", "San Jose", "aislado"),
    lupa:::.resolver_normalizacion(TRUE), "x"
  )
  expect_length(resultado$grupos, 1L)
  expect_equal(sort(resultado$grupos[[1L]]$variantes), sort(c("San José", "San Jose")))
  expect_false(resultado$alcance$distancia_disponible)
  expect_match(resultado$alcance$motivo_distancia, "stringdist")
})

test_that("el alcance declara un recorte del vocabulario o de sus pares", {
  valores <- paste0("v", seq_len(40))
  resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
    valores, lupa:::.resolver_normalizacion(TRUE), "x",
    max_valores = 10L, max_pares = 10L
  )
  expect_true(resultado$alcance$truncado)
  expect_equal(resultado$alcance$n_valores_evaluados, 10L)
  expect_true(resultado$alcance$n_pares_sin_comparar > 0)
})
