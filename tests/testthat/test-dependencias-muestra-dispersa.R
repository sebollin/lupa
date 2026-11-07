# La muestra de las dependencias no se alinea con datos periodicos: una muestra
# a paso fijo sobre un panel mensual cuyo tamano hace que el paso caiga en el
# periodo toma siempre los mismos meses, y no ve lo que pasa en los demas.

panel_mensual <- function() {
  entidades <- 500L
  meses <- 12L
  mes <- rep(seq_len(meses), entidades)
  d <- data.frame(
    entidad = rep(sprintf("E%04d", seq_len(entidades)), each = meses),
    mes = mes,
    tipo_mes = ifelse(mes %in% c(6L, 12L), "cierre", "normal"),
    stringsAsFactors = FALSE
  )
  # Una fila mas: 6001 filas y una muestra de 1001 dan un paso de 6 exacto.
  rbind(d, data.frame(entidad = "E0501", mes = 1L, tipo_mes = "normal"))
}

test_that("la muestra toma una fila por tramo, siempre la misma, y recorre todos los meses", {
  d <- panel_mensual()
  m <- lupa:::.muestrear_filas_dispersas(nrow(d), 1001)
  expect_true(m$muestreado)
  expect_equal(m$analizados, 1001L)
  expect_length(m$valores, 1001L)
  expect_true(all(diff(m$valores) > 0L))
  expect_true(min(m$valores) >= 1L && max(m$valores) <= nrow(d))
  expect_identical(m, lupa:::.muestrear_filas_dispersas(nrow(d), 1001))
  expect_setequal(unique(d$mes[m$valores]), 1:12)
  # La premisa del caso: a paso fijo se toman solo los meses 1 y 7.
  sistematica <- unique(as.integer(round(seq.int(1, nrow(d), length.out = 1001))))
  expect_setequal(unique(d$mes[sistematica]), c(1L, 7L))
  # Sin muestreo, todas las filas.
  completa <- lupa:::.muestrear_filas_dispersas(10L, 1e5)
  expect_false(completa$muestreado)
  expect_equal(completa$valores, 1:10)
})

test_that("una dependencia de los meses que la muestra alineada no veia se publica", {
  d <- panel_mensual()
  dep <- detectar_dependencias(d, muestra = 1001)
  expect_true(attr(dep, "muestreado"))
  fila <- dep[dep$determinante == "mes" & dep$dependiente == "tipo_mes", ]
  expect_equal(nrow(fila), 1L)
  expect_true(fila$exacta)
  # Verificada sobre la tabla entera.
  expect_equal(fila$n_evaluados, nrow(d))
  expect_false("tipo_mes" %in% attr(dep, "columnas_descartadas")$columna)
})
