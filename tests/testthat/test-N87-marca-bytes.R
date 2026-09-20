# `Encoding() == "bytes"` es la declaracion de que eso NO se interprete como
# texto. El paquete la respetaba al imprimir -lo fija `test-N68`- y la ignoraba
# en todo lo demas, asi que sobre el MISMO dato daba dos respuestas.
#
# Medido sobre `c(utf8, bytes, "z", utf8)` con el mismo `a\u00f1o`:
#
#   unique() de R                     -> 3
#   n_distintos que publicaba lupa    -> 2
#   evidencia de duplicados           -> `v=a\u00f1o` en los dos lados
#   print() del mismo data.frame      -> `a\u00f1o` y `a\xc3\xb1o`, distintos
#
# La identidad fundia lo que la consola separaba, y la cardinalidad publicada
# contradecia a `unique()` de R sobre la misma columna.
#
# Lo que NO cambia, y es a proposito: `.texto_analizable()` sigue marcando UTF-8
# lo declarado `bytes` cuando sus bytes son validos, porque ese es el camino de
# ANALISIS -`tolower()`, `trimws()`, las expresiones regulares- y ahi
# interpretar no pierde nada. La regla es por destino: interpretar para
# analizar, no interpretar para publicar ni para decidir identidad.

.n87_bytes <- function(texto) {
  x <- texto
  Encoding(x) <- "bytes"
  x
}

.n87_utf8 <- function(texto) {
  x <- texto
  Encoding(x) <- "UTF-8"
  x
}

test_that("la identidad no funde lo declarado `bytes` con el mismo texto", {
  crudo <- .n87_bytes("a\u00f1o")
  texto <- .n87_utf8("a\u00f1o")
  # R mismo los separa; la clave tiene que decir lo mismo que R.
  expect_false(crudo == texto)
  expect_false(identical(.clave_bytes(crudo), .clave_bytes(texto)))
  # Y la clave de lo declarado `bytes` no puede contener el caracter: si lo
  # contuviera seria porque se interpreto.
  expect_false(grepl("\u00f1", .clave_bytes(crudo), fixed = TRUE))
  expect_no_error(.escapar_clave(crudo))
})

test_that("la cardinalidad publicada coincide con unique() de R", {
  valores <- c(.n87_utf8("a\u00f1o"), .n87_bytes("a\u00f1o"), "z",
               .n87_utf8("a\u00f1o"))
  datos <- data.frame(v = valores, stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(
    datos, fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  expect_identical(
    perfil$columnas$n_distintos[[1L]], length(unique(valores))
  )
})

test_that("la evidencia publicada dice lo mismo que la consola", {
  skip_if_not_installed("stringdist")
  valores <- c(.n87_utf8("a\u00f1o"), .n87_bytes("a\u00f1o"), "z",
               .n87_utf8("a\u00f1o"))
  datos <- data.frame(v = valores, stringsAsFactors = FALSE)
  resultado <- suppressWarnings(detectar_duplicados_aproximados(datos, "v"))

  # Lo que distingue a las dos evidencias NO son sus bytes -son los mismos, y
  # tiene que ser asi: el dato del usuario no se altera- sino la MARCA, que es
  # lo que decide si R lo muestra como `a\u00f1o` o como `a\xc3\xb1o`. Por eso
  # la comprobacion mira `Encoding()` y no el contenido: buscar los bytes de la
  # letra los encuentra en los dos casos, y eso no dice nada.
  evidencias <- c(resultado$pares$evidencia_1, resultado$pares$evidencia_2)
  filas <- c(resultado$pares$fila_1, resultado$pares$fila_2)
  de_la_declarada <- evidencias[filas == 2L]
  skip_if(!length(de_la_declarada),
          "no se formo ningun par con la fila declarada `bytes`")
  for (evidencia in de_la_declarada) {
    expect_identical(
      Encoding(evidencia), "bytes",
      info = paste("evidencia que perdio la declaracion:", evidencia)
    )
  }
  de_las_otras <- evidencias[filas != 2L]
  for (evidencia in de_las_otras) {
    expect_false(identical(Encoding(evidencia), "bytes"), info = evidencia)
  }
})

test_that("el reporte HTML no interpreta lo declarado `bytes`", {
  html <- getFromNamespace(".html_utf8", "lupa")
  crudo <- .n87_bytes("a\u00f1o")
  publicado <- html(crudo)
  expect_false(grepl("\u00f1", publicado, fixed = TRUE))
  # Y no puede abortar: `enc2utf8()` aborta sobre esa marca, y este camino no
  # puede heredar ese aborto.
  expect_no_error(html(c(crudo, "normal", NA_character_)))
  # El texto que SI declara su codificacion sigue publicandose como texto.
  expect_identical(html(.n87_utf8("a\u00f1o")), .n87_utf8("a\u00f1o"))
})

test_that("`comprimir` rechaza lo que saveRDS rechaza", {
  analisis <- suppressWarnings(analizar(
    data.frame(x = c(1, 2, NA, 4)), fecha = as.POSIXct("2026-09-17", tz = "UTC")
  ))
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)

  # La validacion admitia las CADENAS "TRUE" y "FALSE" -comparaba sobre
  # `as.character()`- y `saveRDS()` las rechazaba despues con un error crudo de
  # R. Una validacion existe para que ese error no llegue al usuario.
  for (malo in list("TRUE", "FALSE", "sarasa", NA, c(TRUE, TRUE))) {
    expect_error(
      guardar_analisis(analisis, archivo, sobrescribir = TRUE, comprimir = malo),
      "comprimir",
      info = paste("no rechazo:", paste(as.character(malo), collapse = ","))
    )
  }
  for (bueno in list(TRUE, FALSE, "gzip", "bzip2", "xz")) {
    expect_no_error(
      guardar_analisis(analisis, archivo, sobrescribir = TRUE, comprimir = bueno)
    )
  }
})
