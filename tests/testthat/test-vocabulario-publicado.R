# Los dos README publican la lista de nombres de `tipo_hallazgo` y su cantidad.
# Esa cifra ya envejecio una vez: la tabla de evidencia decia 43 tablas de
# control cuando eran 31, y nadie se entero hasta que una auditoria las conto.
# La leccion quedo escrita en `test-ronda107.R` -«un numero publicado que
# ninguna prueba vigila se vuelve mentira sin que nadie se entere»- y esta
# comprobacion la aplica al otro numero publicado que seguia sin vigilancia.
#
# Comprueba tres cosas, y ninguna necesita un catalogo que el paquete no tiene:
#   1. la cifra que dice la prosa es la cantidad de nombres que lista el bloque;
#   2. los dos README listan exactamente los mismos nombres;
#   3. cada nombre listado aparece en `R/`, asi que uno que se quite del codigo
#      y quede en el README hace fallar esto.
#
# Necesita los README y `R/`, que bajo `R CMD check` no estan: alli corre contra
# el paquete instalado. Se saltea diciendo por que.

.raiz_proyecto_vocabulario <- function() {
  ruta <- normalizePath(testthat::test_path(), winslash = "/", mustWork = FALSE)
  if (basename(ruta) == "testthat") dirname(dirname(ruta)) else ruta
}

.lista_vocabulario_readme <- function(ruta, marca) {
  texto <- readLines(ruta, warn = FALSE, encoding = "UTF-8")
  inicio <- grep(marca, texto, fixed = TRUE)
  if (!length(inicio)) return(NULL)
  abre <- inicio[[1L]] + which(startsWith(texto[(inicio[[1L]] + 1L):length(texto)], "```text"))[[1L]]
  cierra <- abre + which(startsWith(texto[(abre + 1L):length(texto)], "```"))[[1L]]
  palabras <- unlist(strsplit(texto[(abre + 1L):(cierra - 1L)], "[[:space:]]+"))
  palabras <- palabras[nzchar(palabras)]
  list(
    nombres = sort(unique(palabras[grepl("^[a-z0-9_]+$", palabras)])),
    cifra = as.integer(sub(".*?([0-9]+).*", "\\1", texto[[inicio[[1L]]]]))
  )
}

test_that("la lista de tipo_hallazgo publicada coincide entre README y con R/", {
  raiz <- .raiz_proyecto_vocabulario()
  es <- file.path(raiz, "README.es.md")
  en <- file.path(raiz, "README.md")
  fuentes <- file.path(raiz, "R")
  skip_if_not(
    file.exists(es) && file.exists(en) && dir.exists(fuentes),
    "Sin README ni `R/` a la vista: bajo R CMD check no estan en el paquete instalado."
  )

  lista_es <- .lista_vocabulario_readme(es, "nombres de `tipo_hallazgo`")
  lista_en <- .lista_vocabulario_readme(en, "`tipo_hallazgo` names")
  expect_false(is.null(lista_es))
  expect_false(is.null(lista_en))

  # 1. La prosa dice un numero; el bloque lista nombres. Tienen que ser el mismo.
  expect_equal(length(lista_es$nombres), lista_es$cifra)
  expect_equal(length(lista_en$nombres), lista_en$cifra)

  # 2. Los dos README publican la misma lista.
  expect_equal(lista_es$nombres, lista_en$nombres)

  # 3. Ningun nombre publicado desaparecio del codigo.
  codigo <- paste(
    unlist(lapply(
      list.files(fuentes, pattern = "\\.R$", full.names = TRUE),
      readLines, warn = FALSE
    )),
    collapse = "\n"
  )
  huerfanos <- lista_es$nombres[
    !vapply(lista_es$nombres, function(n) grepl(n, codigo, fixed = TRUE), logical(1L))
  ]
  expect_equal(huerfanos, character())
})
