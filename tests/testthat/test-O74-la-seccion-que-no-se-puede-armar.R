# Un historico al que le falta una columna mataba el informe ENTERO.
#
# El subset es legitimo -`print()` y el validador del historico lo manejan- y el
# objeto CONSERVA su clase, asi que `reportar()` lo aceptaba y reventaba adentro,
# en `order(NULL)`, con el mensaje de R base "argumento 1 no es un vector": sin
# seccion, sin campo y sin que hacer. Se perdia todo el documento.
#
# El alcance esta medido, no supuesto: recorri las clases reportables quitandoles
# una columna por vez. `medicion` aguanta las diecisiete; un `plan_limpieza`
# recortado PIERDE su clase y ahi el usuario ya recibe el mensaje deliberado de
# "esto no es un objeto reportable"; el historico era el unico que pasaba la puerta
# roto, en cinco de sus veintidos columnas.

historico_o74 <- function() {
  instancia <- instanciar(
    especializar(metricas_nucleo()$NoNulo, nombre_especifico = "PresenciaCodigo"),
    "personas", "codigo"
  )
  medicion <- medir(
    modelo(instancia), data.frame(codigo = c("A", NA, "C"),
                                  stringsAsFactors = FALSE),
    id_medicion = "enero", fecha = as.POSIXct("2026-01-31", tz = "UTC")
  )
  historico_calidad(evaluar(
    medicion,
    perfil_evaluacion("B", regla_evaluacion("Dato presente", function(x) x > 0))
  ))
}

reportar_a_texto <- function(objeto) {
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(objeto, archivo = archivo)
  paste(readLines(archivo, warn = FALSE), collapse = "\n")
}

test_that("un historico sin una columna no mata el informe y declara el motivo", {
  historico <- historico_o74()
  for (campo in c("fecha", "nivel", "id_medicion", "perfil", "resultado")) {
    recortado <- historico[, setdiff(names(historico), campo), drop = FALSE]
    html <- expect_no_error(reportar_a_texto(recortado))
    # La seccion existe, dice que no se pudo armar y NOMBRA el campo que falta.
    expect_true(grepl("no se pudo armar", html, fixed = TRUE))
    expect_true(grepl(campo, html, fixed = TRUE))
    expect_true(grepl("no es conformidad", html, fixed = TRUE))
  }
})

test_that("el historico completo sigue publicando su seccion", {
  # Mitad de control: la guarda no puede quedarse con el caso bueno. Si el
  # historico esta entero, la seccion se arma y NO aparece la declaracion.
  html <- reportar_a_texto(historico_o74())

  expect_true(grepl("Registros hist", html, fixed = TRUE))
  expect_false(grepl("no se pudo armar", html, fixed = TRUE))
})

test_that("el resto del informe sobrevive a la seccion que no se pudo armar", {
  # Lo que el defecto costaba no era la seccion: era el documento. Se reporta el
  # historico roto JUNTO a un perfil y el perfil tiene que estar.
  historico <- historico_o74()
  roto <- historico[, setdiff(names(historico), "fecha"), drop = FALSE]
  perfil <- perfilar(data.frame(monto = c(1, 2, 3), stringsAsFactors = FALSE))
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(perfil, roto, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")

  expect_true(grepl("monto", html, fixed = TRUE))
  expect_true(grepl("no se pudo armar", html, fixed = TRUE))
})
