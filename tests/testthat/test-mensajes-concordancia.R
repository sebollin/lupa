test_that("los mensajes visibles distinguen singular y plural", {
  imprimir <- function(datos, muestra) {
    objeto <- lupa::perfilar(
      datos, muestra = muestra, analizar_dependencias = FALSE
    )
    mensajes <- character()
    salida <- withCallingHandlers(
      capture.output(print(objeto)),
      message = function(condicion) {
        mensajes <<- c(mensajes, conditionMessage(condicion))
        invokeRestart("muffleMessage")
      }
    )
    paste(
      c(mensajes, salida),
      collapse = "\n"
    )
  }

  salida_una <- imprimir(data.frame(x = 1:2), 1L)
  salida_dos <- imprimir(data.frame(x = 1:3), 2L)
  expect_match(salida_una, "muestra de 1 de 2 filas", fixed = TRUE)
  expect_false(grepl("1 filas", salida_una, fixed = TRUE))
  expect_match(salida_dos, "muestra de 2 de 3 filas", fixed = TRUE)
  expect_false(grepl("2 fila:", salida_dos, fixed = TRUE))

  evidencia_una <- lupa::perfilar(
    data.frame(x = " valor "), analizar_dependencias = FALSE
  )$hallazgos
  evidencia_una <- evidencia_una[
    evidencia_una$tipo_hallazgo == "espacios_sobrantes", "evidencia"
  ]
  evidencia_dos <- lupa::perfilar(
    data.frame(x = c(" valor ", " otro ")), analizar_dependencias = FALSE
  )$hallazgos
  evidencia_dos <- evidencia_dos[
    evidencia_dos$tipo_hallazgo == "espacios_sobrantes", "evidencia"
  ]
  expect_match(evidencia_una, "1 valor", fixed = TRUE)
  expect_false(grepl("1 valores", evidencia_una, fixed = TRUE))
  expect_match(evidencia_dos, "2 valores", fixed = TRUE)
  expect_false(grepl("2 valor;", evidencia_dos, fixed = TRUE))

  faltantes_una <- lupa::perfilar(
    data.frame(x = c("A", "B", "C", NA_character_)),
    analizar_dependencias = FALSE
  )$hallazgos
  faltantes_una <- faltantes_una[
    faltantes_una$tipo_hallazgo == "faltantes", "evidencia"
  ]
  faltantes_dos <- lupa::perfilar(
    data.frame(x = c("A", "B", "C", NA_character_, NA_character_)),
    analizar_dependencias = FALSE
  )$hallazgos
  faltantes_dos <- faltantes_dos[
    faltantes_dos$tipo_hallazgo == "faltantes", "evidencia"
  ]
  expect_match(faltantes_una, "1 ausente real", fixed = TRUE)
  expect_false(grepl("1 ausentes reales", faltantes_una, fixed = TRUE))
  expect_match(faltantes_dos, "2 ausentes reales", fixed = TRUE)
  expect_false(grepl("2 ausente real", faltantes_dos, fixed = TRUE))

  html_una <- lupa:::.valor_celda_reporte(list(data.frame(a = 1)), 1L)
  html_dos <- lupa:::.valor_celda_reporte(list(data.frame(a = 1:2)), 1L)
  expect_match(html_una, "&lt;tabla&#58; 1 fila", fixed = TRUE)
  expect_false(grepl("&lt;tabla&#58; 1 filas", html_una, fixed = TRUE))
  expect_match(html_dos, "&lt;tabla&#58; 2 filas", fixed = TRUE)
  expect_false(grepl("&lt;tabla&#58; 2 fila ", html_dos, fixed = TRUE))

  proyeccion_una <- list(
    disponible = TRUE, duracion_estimada_ms = 1000, n_columnas = 1L,
    fuente = "prueba", columnas_sin_cardinalidad = character()
  )
  proyeccion_dos <- proyeccion_una
  proyeccion_dos$n_columnas <- 2L
  expect_message(
    lupa:::.avisar_costo_moda_dbi(proyeccion_una, TRUE, 1),
    "1 columna\\. Fuente"
  )
  expect_message(
    lupa:::.avisar_costo_moda_dbi(proyeccion_dos, TRUE, 1),
    "2 columnas\\. Fuente"
  )
})
