# La politica numerica predeterminada es una sospecha del paquete. Una politica
# completa suministrada por el usuario declara sus valores y esos valores salen
# de los resumenes cuantitativos.

test_that("el default no excluye y una politica reemplazada si excluye", {
  datos <- data.frame(x = c(999, 1:9))

  predeterminado <- perfilar(datos, analizar_dependencias = FALSE)
  declarado <- perfilar(
    datos, sentinelas_numericos = 999,
    analizar_dependencias = FALSE
  )

  expect_equal(predeterminado$columnas$media, mean(datos$x))
  expect_equal(predeterminado$columnas$n_valores_excluidos_resumen, 0L)
  expect_equal(predeterminado$columnas$estado_resumen_cuantitativo, "calculados")
  expect_equal(declarado$columnas$media, mean(1:9))
  expect_equal(declarado$columnas$n_valores_excluidos_resumen, 1L)
  expect_equal(
    declarado$columnas$estado_resumen_cuantitativo,
    "calculados_sobre_valores"
  )
})

test_that("los centinelas declarados salen de los estadisticos y n_distintos no cambia", {
  set.seed(1)
  base <- round(stats::rnorm(950, 40, 8))
  datos <- data.frame(h = c(base, rep(9999L, 30), rep(9998L, 20)))
  limpio <- perfilar(
    data.frame(h = base), sentinelas_numericos = numeric(),
    analizar_dependencias = FALSE
  )$columnas
  declarado <- perfilar(
    datos, sentinelas_numericos = c(9999, 9998),
    analizar_dependencias = FALSE
  )$columnas

  fila_limpia <- limpio[1, ]
  fila_declarada <- declarado[1, ]
  campos <- c(
    "media", "mediana", "minimo", "maximo", "desvio", "n_ceros",
    "n_negativos", "n_outliers"
  )
  expect_equal(fila_declarada[, campos], fila_limpia[, campos])
  expect_equal(fila_declarada$n_valores_excluidos_resumen, 50L)
  expect_equal(
    fila_declarada$estado_resumen_cuantitativo,
    "calculados_sobre_valores"
  )
  # Es una medida de la representacion almacenada, no del subconjunto que
  # alimenta media/mediana/rango.
  expect_equal(fila_declarada$n_distintos, length(unique(datos$h)))
})

test_that("aplicabilidad y centinelas dan el mismo resumen sobre el mismo universo", {
  set.seed(1)
  base <- round(stats::rnorm(950, 40, 8))
  datos <- data.frame(
    universo = c(rep(TRUE, 950), rep(FALSE, 50)),
    h = c(base, rep(9999L, 30), rep(9998L, 20))
  )
  por_aplicabilidad <- perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(h = ~ universo)
  )$columnas
  por_sentinelas <- perfilar(
    datos, analizar_dependencias = FALSE,
    sentinelas_numericos = c(9999, 9998)
  )$columnas
  campos <- c(
    "media", "mediana", "minimo", "maximo", "desvio", "n_ceros",
    "n_negativos", "n_outliers"
  )

  expect_equal(
    por_sentinelas[por_sentinelas$columna == "h", campos],
    por_aplicabilidad[por_aplicabilidad$columna == "h", campos]
  )
  expect_equal(
    por_sentinelas[por_sentinelas$columna == "h", ]$n_valores_excluidos_resumen,
    50L
  )
  expect_equal(
    por_aplicabilidad[por_aplicabilidad$columna == "h", ]$n_valores_excluidos_resumen,
    0L
  )
  expect_equal(
    por_sentinelas[por_sentinelas$columna == "h", ]$estado_resumen_cuantitativo,
    "calculados_sobre_valores"
  )
})

test_that("una declaracion atraviesa la guarda de secuencia entera densa", {
  datos <- data.frame(x = c(1:100, rep(9999, 10)))
  perfil <- perfilar(
    datos, sentinelas_numericos = 9999,
    analizar_dependencias = FALSE
  )
  fila <- perfil$columnas

  expect_equal(fila$n_faltantes_disfrazados, 10L)
  expect_equal(fila$media, mean(1:100))
  expect_equal(fila$maximo, 100)
  expect_equal(fila$n_valores_excluidos_resumen, 10L)
})

test_that("un centinela declarado eleva la severidad de faltantes", {
  casos <- list(
    reales = c(rep(NA_real_, 600), seq_len(400)),
    declarados = c(rep(9999, 600), seq_len(400)),
    mixtos = c(rep(NA_real_, 300), rep(9999, 300), seq_len(400))
  )

  for (nombre in names(casos)) {
    perfil <- perfilar(
      data.frame(x = casos[[nombre]]),
      sentinelas_numericos = 9999,
      analizar_dependencias = FALSE
    )
    faltantes <- perfil$hallazgos[
      perfil$hallazgos$tipo_hallazgo == "faltantes", , drop = FALSE
    ]
    disfrazados <- perfil$hallazgos[
      perfil$hallazgos$tipo_hallazgo == "faltantes_disfrazados", , drop = FALSE
    ]
    expect_equal(as.character(faltantes$severidad), "error", info = nombre)
    if (nombre == "declarados") {
      expect_equal(as.character(disfrazados$severidad), "error")
    }
  }
})
