# O36: la proteccion de datos personales se decide con una lista de campos a
# ocultar, y una lista asi falla ABIERTA: el campo que se agregue manana se
# publica salvo que alguien se acuerde de anotarlo. Se vio con la etiqueta, que
# decia "[estadisticos de orden y momentos protegidos]" mientras publicaba un
# momento -el desvio-.
#
# La decision sobre el desvio esta tomada y registrada -"F-6 conserva el desvio
# numerico y protege la media"-: una dispersion no identifica a nadie, y la
# media, que junto con ella reconstruiria los valores, si se enmascara. Lo que
# se corrigio fue la etiqueta.
#
# Esta prueba no lee la lista de campos: perfila una columna protegida con
# magnitudes conocidas y exige que ningun campo publicado las traiga, con el
# desvio como UNICA excepcion declarada. Un campo nuevo que filtre cae aca
# aunque nadie lo anote.

.columna_protegida_de_prueba <- function() {
  set.seed(536)
  as.numeric(sample(10000000:99999999, 40L))
}

# La excepcion se nombra aca, una sola vez, para que agregar otra sea un acto
# deliberado y visible en el diff.
.campos_con_magnitud_permitida <- c("desvio")

test_that("solo el desvio publica una magnitud de la columna protegida", {
  valores <- .columna_protegida_de_prueba()
  datos <- data.frame(id = seq_len(40L), documento = valores)

  perfil <- perfilar(datos, columnas_personales = "documento",
                     analizar_dependencias = FALSE)
  fila <- perfil$columnas[perfil$columnas$columna == "documento", ,
                          drop = FALSE]

  expect_true(isTRUE(fila$dato_personal_protegido[[1L]]))
  # La columna vale entre 1e7 y 1e8; ningun conteo de una tabla de 40 filas
  # llega a 1e6, asi que cualquier cifra de esa magnitud viene de los valores.
  filtrados <- character()
  for (campo in names(fila)) {
    valor <- fila[[campo]]
    if (is.list(valor)) next
    numero <- suppressWarnings(as.numeric(valor[[1L]]))
    if (length(numero) == 1L && !is.na(numero) && is.finite(numero) &&
        abs(numero) >= 1e6) {
      filtrados <- c(filtrados, campo)
    }
  }
  expect_equal(setdiff(filtrados, .campos_con_magnitud_permitida), character(),
               info = paste("campos con magnitud de la columna:",
                            paste(filtrados, collapse = ", ")))

  # Y ningun campo de texto puede contener un valor literal.
  textos <- character()
  for (campo in names(fila)) {
    valor <- fila[[campo]]
    if (is.list(valor)) next
    texto <- as.character(valor[[1L]])
    if (is.na(texto)) next
    if (any(vapply(as.character(valores), function(v) {
      grepl(v, texto, fixed = TRUE)
    }, logical(1L)))) {
      textos <- c(textos, campo)
    }
  }
  expect_equal(textos, character(),
               info = paste("campos de texto con un valor:",
                            paste(textos, collapse = ", ")))
})

test_that("la etiqueta nombra lo que efectivamente protege", {
  valores <- .columna_protegida_de_prueba()
  datos <- data.frame(id = seq_len(40L), documento = valores)

  perfil <- perfilar(datos, columnas_personales = "documento",
                     analizar_dependencias = FALSE)
  fila <- perfil$columnas[perfil$columnas$columna == "documento", ,
                          drop = FALSE]

  # Decia "momentos" y publicaba uno: ahora nombra la media, que es el que
  # protege, y el desvio queda declarado como lo que es.
  expect_equal(as.character(fila$detalle_proteccion_personal[[1L]]),
               "[estadisticos de orden y la media protegidos]")
  expect_true(is.na(fila$media[[1L]]))
  expect_true(is.na(fila$minimo[[1L]]))
  expect_true(is.finite(fila$desvio[[1L]]))

  # Y lo que declara proteger no llega al informe.
  destino <- tempfile(fileext = ".html")
  on.exit(unlink(destino), add = TRUE)
  suppressMessages(reportar(perfil, archivo = destino))
  html <- paste(readLines(destino, warn = FALSE), collapse = "\n")
  expect_false(grepl(format(mean(valores), scientific = FALSE), html,
                     fixed = TRUE))
  expect_false(grepl(format(min(valores), scientific = FALSE), html,
                     fixed = TRUE))
})

test_that("sin proteccion la columna publica sus estadisticos", {
  # Control: lo que cambia es la proteccion, no el calculo.
  valores <- .columna_protegida_de_prueba()
  datos <- data.frame(id = seq_len(40L), documento = valores)

  perfil <- perfilar(datos, analizar_dependencias = FALSE,
                     proteger_datos_personales = FALSE)
  fila <- perfil$columnas[perfil$columnas$columna == "documento", ,
                          drop = FALSE]

  expect_equal(fila$desvio[[1L]], stats::sd(valores))
  expect_equal(fila$media[[1L]], mean(valores))
})

test_that("una columna no personal de la misma tabla conserva su desvio", {
  # Control: la proteccion es por columna, no por tabla.
  valores <- .columna_protegida_de_prueba()
  datos <- data.frame(
    documento = valores,
    monto = as.numeric(seq_len(40L)) * 3
  )

  perfil <- perfilar(datos, columnas_personales = "documento",
                     analizar_dependencias = FALSE)
  monto <- perfil$columnas[perfil$columnas$columna == "monto", , drop = FALSE]

  expect_equal(monto$desvio[[1L]], stats::sd(datos$monto))
})
