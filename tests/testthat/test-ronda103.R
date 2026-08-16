test_that("las cedulas puntuadas se detectan como multivaluadas", {
  construir <- function(n) {
    n_multivaluadas <- if (n == 4L) 1L else n / 5L
    c(
      rep("1234567-8; 2345678-9", n_multivaluadas),
      rep("1234567-8", n - n_multivaluadas)
    )
  }

  for (n in c(4L, 20L, 100L)) {
    perfil <- perfilar(
      data.frame(cedula = construir(n)),
      analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    )
    hallazgo <- perfil$hallazgos[
      perfil$hallazgos$tipo_hallazgo == "celdas_multivaluadas", , drop = FALSE
    ]
    expect_equal(nrow(hallazgo), 1L)
    expect_match(hallazgo$evidencia, "Delimitador: ;", fixed = TRUE)
    expect_match(
      hallazgo$evidencia,
      paste0("celdas: ", if (n == 4L) 1L else n / 5L),
      fixed = TRUE
    )
  }
})

test_that("la puntuacion interna no abre falsos positivos de multivaluados", {
  controles <- list(
    texto_libre = c("casa, jardin y garaje", "parque, piscina"),
    direcciones = c("18 de Julio 1234, Piso 3", "Rivera 2020, Apto 4"),
    nombres = c("Perez, Juan", "Gomez, Ana"),
    fechas = c("2024-01-01", "2025-02-02"),
    decimales = c("1,50", "2,75")
  )

  for (nombre in names(controles)) {
    perfil <- perfilar(
      setNames(data.frame(controles[[nombre]]), nombre),
      analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    )
    expect_false(
      "celdas_multivaluadas" %in% perfil$hallazgos$tipo_hallazgo,
      info = nombre
    )
  }
})

test_that("las monedas mixtas se informan sin convertirlas ni tratarlas como unidades", {
  sufijos <- perfilar(
    data.frame(numero_texto = c("100 UYU", "25 USD", "300 UYU", "40 USD")),
    analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  simbolos <- perfilar(
    data.frame(numero_texto = c("$ 100", "U$S 25", "$ 300", "U$S 40")),
    analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )

  for (perfil in list(sufijos, simbolos)) {
    hallazgo <- perfil$hallazgos[
      perfil$hallazgos$tipo_hallazgo == "monedas_mixtas", , drop = FALSE
    ]
    expect_equal(nrow(hallazgo), 1L)
    expect_equal(hallazgo$n_evaluados, 4)
    expect_equal(hallazgo$n_afectados, 4)
    expect_equal(hallazgo$unidad_conteo, "fila")
    expect_match(hallazgo$evidencia, "celdas con moneda: 4", fixed = TRUE)
    expect_equal(perfil$columnas$numero_texto_unidad, "")
    expect_equal(perfil$columnas$numero_texto_moneda, "")
  }
  expect_match(
    sufijos$hallazgos$evidencia[
      sufijos$hallazgos$tipo_hallazgo == "monedas_mixtas"
    ],
    "UYU (2); USD (2)", fixed = TRUE
  )
  expect_match(
    simbolos$hallazgos$evidencia[
      simbolos$hallazgos$tipo_hallazgo == "monedas_mixtas"
    ],
    "$ (2); U$S (2)", fixed = TRUE
  )
})

test_that("una moneda y los numeros sin simbolo no son monedas mixtas", {
  una_moneda <- perfilar(
    data.frame(numero_texto = c("100 UYU", "200 UYU", "300 UYU")),
    analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  solo_dolar <- perfilar(
    data.frame(numero_texto = c("$ 100", "$ 200", "$ 300")),
    analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  sin_simbolo <- perfilar(
    data.frame(numero_texto = c("100", "200", "300")),
    analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )

  expect_false("monedas_mixtas" %in% una_moneda$hallazgos$tipo_hallazgo)
  expect_false("monedas_mixtas" %in% solo_dolar$hallazgos$tipo_hallazgo)
  expect_false("monedas_mixtas" %in% sin_simbolo$hallazgos$tipo_hallazgo)
  expect_equal(sin_simbolo$columnas$n_numeros_texto, 0L)
})

