# O14. Lo que se cuenta sobre la muestra dice que es de la muestra.
#
# Patrones y formatos de fecha se descubren sobre una muestra -lo documenta
# `perfilar()`-, pero lo publicado no lo decia: `patron_raro` informaba
# `n_afectados = 2` y `proporcion_dominante = 0.980` donde la tabla tenia 10 y
# 0.999; `formatos_fecha_mixtos` publicaba `%d/%m/%Y (50)` donde habia 5.000; la
# consola imprimia "0 hallazgos sospechosos" sobre 10 de 10.000 filas sin
# mencionar la muestra, y el reporte anotaba la muestra en patrones y no en
# formatos. `tipo_declarado_distinto` ya lo declaraba; ahora todos.

.o14_salida <- function(expr) {
  mensajes <- character()
  salida <- withCallingHandlers(
    utils::capture.output(expr),
    message = function(m) {
      mensajes <<- c(mensajes, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  paste(c(mensajes, salida), collapse = "\n")
}

.o14_evidencia <- function(perfil, tipo) {
  h <- as.data.frame(hallazgos(perfil))
  h$evidencia[h$tipo_hallazgo == tipo]
}

test_that("un patron raro medido sobre la muestra lo declara en la evidencia", {
  g <- rep("C001", 10000L)
  g[seq(1L, 10000L, by = 1000L)] <- "c-02"
  datos <- data.frame(g = g, stringsAsFactors = FALSE)
  muestreado <- perfilar(datos, muestra = 100)
  evidencia <- .o14_evidencia(muestreado, "patron_raro")
  # La premisa: la muestra ve el patron raro.
  expect_length(evidencia, 1L)
  expect_match(evidencia, "sobre una muestra de 100 de 10.000 valores", fixed = TRUE)
  completo <- perfilar(datos, muestra = Inf)
  expect_false(grepl("muestra", .o14_evidencia(completo, "patron_raro"), fixed = TRUE))
})

test_that("los formatos de fecha contados sobre la muestra lo declaran", {
  datos <- data.frame(f = rep(c("2024-01-15", "15/01/2024"), 5000L),
                      stringsAsFactors = FALSE)
  muestreado <- perfilar(datos, muestra = 100)
  evidencia <- .o14_evidencia(muestreado, "formatos_fecha_mixtos")
  expect_length(evidencia, 1L)
  expect_match(evidencia, "(50)", fixed = TRUE)
  expect_match(evidencia, "los conteos son de la muestra", fixed = TRUE)

  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(muestreado, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "Formatos estimados sobre 100 de 10000 valores", fixed = TRUE)

  completo <- perfilar(datos, muestra = Inf)
  expect_false(grepl("muestra", .o14_evidencia(completo, "formatos_fecha_mixtos"),
                     fixed = TRUE))
})

test_that("la consola dice que el perfil trabajo sobre una muestra", {
  datos <- data.frame(g = sprintf("C%05d", seq_len(10000L)), stringsAsFactors = FALSE)
  salida <- .o14_salida(print(perfilar(datos, muestra = 10)))
  expect_match(salida, "sobre una muestra de 10 de 10.000 filas", fixed = TRUE)
  sin_muestra <- .o14_salida(print(perfilar(datos[1:50, , drop = FALSE])))
  expect_false(grepl("sobre una muestra", sin_muestra, fixed = TRUE))
})
