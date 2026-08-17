.tipos_casi_clave_r113b <- function(x) {
  perfil <- perfilar(
    data.frame(valor = x), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  perfil$hallazgos$tipo_hallazgo[
    perfil$hallazgos$columna == "valor" &
      as.character(perfil$hallazgos$severidad) != "ok"
  ]
}

test_that("los dobles fraccionarios no son candidatos a casi-clave", {
  id_doble <- as.double(c(1:295, 150))
  id_entero <- as.integer(c(1:295, 150))
  set.seed(7)
  importe <- round(stats::runif(300, 10, 900), 2)
  set.seed(1)
  coordenada <- round(stats::runif(300, -34.9, -30.1), 6)

  expect_equal(typeof(id_doble), "double")
  expect_equal(sum(is.finite(id_doble) & id_doble != trunc(id_doble)), 0L)
  expect_equal(typeof(id_entero), "integer")
  expect_equal(sum(is.finite(id_entero) & id_entero != trunc(id_entero)), 0L)
  expect_equal(typeof(importe), "double")
  expect_equal(sum(is.finite(importe) & importe != trunc(importe)), 299L)
  expect_equal(typeof(coordenada), "double")
  expect_equal(
    sum(is.finite(coordenada) & coordenada != trunc(coordenada)), 300L
  )

  expect_true("casi_clave" %in% .tipos_casi_clave_r113b(id_doble))
  expect_true("casi_clave" %in% .tipos_casi_clave_r113b(id_entero))
  expect_false("casi_clave" %in% .tipos_casi_clave_r113b(importe))
  expect_false("casi_clave" %in% .tipos_casi_clave_r113b(coordenada))

  claves <- lapply(
    list(id_doble, id_entero, importe, coordenada),
    function(x) detectar_claves(data.frame(valor = x), max_combinacion = 1L)
  )
  expect_equal(
    vapply(claves, function(x) any(x$casi_clave), logical(1L)),
    c(TRUE, TRUE, FALSE, FALSE)
  )
  expect_equal(vapply(claves, nrow, integer(1L)), c(1L, 1L, 0L, 0L))
})

test_that("la evidencia declara el criterio para dobles enteros", {
  perfil <- perfilar(
    data.frame(valor = as.double(c(1:295, 150))),
    analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$columna == "valor" &
      perfil$hallazgos$tipo_hallazgo == "casi_clave", , drop = FALSE
  ]

  expect_equal(nrow(hallazgo), 1L)
  expect_match(
    hallazgo$evidencia,
    paste0(
      "criterio_tipo_casi_clave: tipo=double; ",
      "valores_fraccionarios_finitos=0; ",
      "doble_admitido_solo_sin_fraccionarios_finitos=TRUE"
    ),
    fixed = TRUE
  )
})
