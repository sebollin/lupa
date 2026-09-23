test_that("la deriva no resuelve duplicados cuando cambian las columnas", {
  datos <- data.frame(
    id = 1:120L,
    grupo = rep(c("A", "B", "C"), c(50, 40, 30)),
    valor = rep(c(1.5, 2.5, NA, 4.5, 5.5), 24),
    texto = rep(c("a", "b", "a", "c", NA), 24),
    constante = 7L,
    fecha = as.Date("2024-01-01") + rep(0:4, 24),
    stringsAsFactors = FALSE
  )
  datos <- rbind(datos, datos[1:10, , drop = FALSE])
  perfil_antes <- perfilar(
    datos, muestra = Inf, analizar_dependencias = FALSE
  )
  plan <- planificar_limpieza(perfil_antes, datos = datos)
  aplicada <- aplicar(plan, datos)

  sin_marcas <- aplicada$datos
  sin_marcas[c(".fila_duplicada", ".grupo_duplicado")] <- NULL
  expect_equal(nrow(sin_marcas) - nrow(unique(sin_marcas)), 10L)
  expect_equal(aplicada$registro$n_filas_eliminadas[[1L]], 0)
  expect_match(aplicada$registro$justificacion[[1L]], "No elimina filas", fixed = TRUE)

  perfil_despues <- perfilar(
    aplicada$datos, muestra = Inf, analizar_dependencias = FALSE
  )
  expect_equal(perfil_despues$general$filas_duplicadas, 0L)
  deriva <- comparar_perfiles(perfil_antes, perfil_despues)
  fila <- deriva[
    !is.na(deriva$valor_anterior) & deriva$aspecto == "hallazgo" &
      deriva$valor_anterior == "filas_duplicadas", , drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_equal(as.character(fila$cambio), "no_comparable")
  expect_equal(as.character(fila$severidad), "error")
  expect_match(fila$descripcion, ".grupo_duplicado", fixed = TRUE)
  expect_false(any(
    !is.na(deriva$valor_anterior) & deriva$aspecto == "hallazgo" &
      deriva$cambio == "resuelto" &
      deriva$valor_anterior == "filas_duplicadas"
  ))
})

test_that("con las mismas columnas un duplicado eliminado sigue resuelto", {
  antes <- perfilar(
    data.frame(a = c("x", "x", "y"), b = c(1L, 1L, 3L)),
    muestra = Inf, analizar_dependencias = FALSE
  )
  despues <- perfilar(
    data.frame(a = c("x", "y"), b = c(1L, 3L)),
    muestra = Inf, analizar_dependencias = FALSE
  )
  deriva <- comparar_perfiles(antes, despues)
  fila <- deriva[
    !is.na(deriva$valor_anterior) & deriva$aspecto == "hallazgo" &
      deriva$valor_anterior == "filas_duplicadas", , drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_equal(as.character(fila$cambio), "resuelto")
  expect_equal(as.character(fila$severidad), "ok")
})

test_that("la relectura declara la propuesta rederivada y la reporta", {
  datos <- data.frame(
    id = 1:120L,
    grupo = rep(c("A", "B", "C"), c(50, 40, 30)),
    valor = rep(c(1.5, 2.5, NA, 4.5, 5.5), 24),
    texto = rep(c("a", "b", "a", "c", NA), 24),
    constante = 7L,
    fecha = as.Date("2024-01-01") + rep(0:4, 24),
    stringsAsFactors = FALSE
  )
  datos <- rbind(datos, datos[1:10, , drop = FALSE])
  original <- analizar(
    datos, muestra = Inf, proteger_datos_personales = FALSE
  )
  expect_false(any(
    original$advertencias$tipo == "persistencia_funciones_sustituidas"
  ))
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  guardar_analisis(original, archivo)
  releido <- leer_analisis(archivo)

  aviso <- releido$advertencias[
    releido$advertencias$tipo == "persistencia_funciones_sustituidas", ,
    drop = FALSE
  ]
  expect_equal(nrow(aviso), 1L)
  expect_match(aviso$descripcion, "volvio a derivar", fixed = TRUE)
  expect_match(aviso$descripcion, "decision original", fixed = TRUE)
  funciones <- grepl(
    "dependencia_funcional", releido$propuesta_modelo$origen, fixed = TRUE
  )
  expect_equal(sum(funciones), 5L)
  expect_true(all(releido$propuesta_modelo$estado[funciones] == "requiere_datos"))
  expect_true(all(releido$decision_medicion$estado == "lista"))
  expect_true(all(releido$decision_medicion$medida))

  informe <- tempfile(fileext = ".html")
  on.exit(unlink(informe), add = TRUE)
  reportar(releido, archivo = informe, sobrescribir = TRUE)
  html <- paste(readLines(informe, warn = FALSE), collapse = "\n")
  expect_match(html, "persistencia_funciones_sustituidas", fixed = TRUE)
  expect_match(html, "volvio a derivar", fixed = TRUE)
  expect_match(html, "decision original", fixed = TRUE)
})

test_that("un analisis en memoria no declara sustituciones de persistencia", {
  resultado <- analizar(
    data.frame(x = 1:3), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  expect_false(any(
    resultado$advertencias$tipo == "persistencia_funciones_sustituidas"
  ))
  expect_false(any(grepl("volvio a derivar", resultado$advertencias$descripcion,
                          fixed = TRUE)))
})
