test_that("cada hallazgo declara sus conteos y su unidad", {
  perfil <- perfilar(
    data.frame(
      importe = c(999, 1, NA),
      constante = c("A", "A", "A"),
      stringsAsFactors = FALSE
    ),
    analizar_dependencias = FALSE
  )

  expect_true(all(c("n_evaluados", "n_afectados", "unidad_conteo") %in%
                    names(perfil$hallazgos)))
  expect_true(all(!is.na(perfil$hallazgos$unidad_conteo)))
  faltantes <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "faltantes_disfrazados", ,
    drop = FALSE
  ]
  expect_equal(faltantes$n_evaluados, 3)
  expect_equal(faltantes$n_afectados, 1)
  expect_equal(faltantes$unidad_conteo, "fila")
  constante <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "constante", ,
    drop = FALSE
  ]
  expect_equal(constante$n_evaluados, 3)
  expect_equal(constante$n_afectados, 3)
  expect_equal(constante$unidad_conteo, "fila")
})

test_that("los hallazgos estructurales declaran unidades distintas", {
  datos <- data.frame(a = c(1, 1), b = c(1, 1), check.names = FALSE)
  names(datos) <- c("a", "a")
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  duplicadas <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "columnas_duplicadas", ,
    drop = FALSE
  ]
  expect_true(nrow(duplicadas) >= 1)
  expect_equal(duplicadas$unidad_conteo, "columna")
  expect_equal(duplicadas$n_evaluados, 2)
  expect_equal(duplicadas$n_afectados, 2)
})

test_that("los hallazgos de pares cuentan pares comparados", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = c("Ana Perez", "Ana Peres"),
    domicilio = c("Calle 1", "Calle 1")
  )
  perfil <- perfilar(
    datos,
    analizar_dependencias = FALSE,
    duplicados_aproximados = list(
      columnas = c("nombre", "domicilio"),
      estrategia = "teselas",
      max_resultados = Inf
    )
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "duplicados_aproximados", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1)
  expect_equal(hallazgo$unidad_conteo, "par")
  expect_equal(hallazgo$n_evaluados, 1)
  expect_equal(hallazgo$n_afectados, 1)
})
