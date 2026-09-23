# O23. La muestra por grupo se declara donde se lee el numero.
#
# `perfilar_por()` pasa sus argumentos a `perfilar()` para cada grupo, asi que
# `muestra` acota DENTRO de cada grupo: con `muestra = 100` y grupos de 500
# filas, un hallazgo que muestrea publica `n_evaluados = 100`. La evidencia lo
# declara -desde que los conteos muestrales dicen que lo son-, pero la
# documentacion de `perfilar_por()` no mencionaba `muestra` ni una vez.

test_that("el hallazgo por grupo declara sobre cuantos valores se midio", {
  fechas <- as.Date("2020-01-01") + 0:499
  texto <- format(fechas, "%Y-%m-%d")
  texto[293L] <- format(fechas[293L], "%d/%m/%Y")
  datos <- data.frame(
    g = c(rep("A", 500L), rep("B", 500L)),
    f = c(texto, format(fechas, "%Y-%m-%d")),
    stringsAsFactors = FALSE
  )

  por_grupo <- perfilar_por(datos, "g", muestra = 100L)
  fila <- por_grupo[por_grupo$tipo_hallazgo == "patron_raro", , drop = FALSE]
  expect_true(nrow(fila) >= 1L)
  # El numero publicado es el de la muestra del grupo...
  expect_equal(fila$n_evaluados[[1L]], 100)
  # ...y el grupo sigue siendo de 500 filas: las dos cifras conviven.
  expect_equal(fila$n_filas_grupo[[1L]], 500)
  # La evidencia dice de donde salio el conteo.
  expect_match(fila$evidencia[[1L]], "muestra de 100 de 500", fixed = TRUE)

  # Control: sin muestra, ni la evidencia declara recorte ni el conteo cambia.
  completo <- perfilar_por(datos, "g", muestra = Inf)
  fila_completa <- completo[completo$tipo_hallazgo == "patron_raro", , drop = FALSE]
  expect_true(nrow(fila_completa) >= 1L)
  expect_equal(fila_completa$n_evaluados[[1L]], 500)
  expect_false(grepl("muestra", fila_completa$evidencia[[1L]], fixed = TRUE))
})
