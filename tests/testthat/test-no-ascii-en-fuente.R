# `R CMD check` avisa "code files for non-ASCII characters" cuando un archivo de
# `R/` trae un caracter no ASCII fuera de un comentario. La suite no podia verlo:
# el barrido de `test-escapes-en-mensajes.R` mira los literales del espacio de
# nombres ya cargado, y ahi "Sesion" con acento crudo y con `ó` se ven
# identicos, porque en tiempo de ejecucion son la misma cadena.
#
# La unica forma de distinguirlos es mirar el fuente, asi que esta comprobacion
# necesita `R/`, que bajo `R CMD check` no existe -alli corre contra el paquete
# instalado-. Se saltea diciendo por que, y en ese entorno el trabajo lo hace el
# check nativo, que es quien avisa.

test_that("ningun archivo de R/ trae no-ASCII fuera de los comentarios", {
  raiz <- testthat::test_path("..", "..", "R")
  skip_if_not(
    dir.exists(raiz),
    "Sin `R/` a la vista: bajo R CMD check lo comprueba el check nativo."
  )
  archivos <- list.files(raiz, pattern = "\\.R$", full.names = TRUE)
  expect_gt(length(archivos), 0L)

  ofensores <- character()
  for (archivo in archivos) {
    lineas <- readLines(archivo, warn = FALSE, encoding = "UTF-8")
    # Fuera los comentarios enteros -roxygen incluido-: ahi los acentos van en
    # UTF-8 a proposito, porque escribir `\uXXXX` en roxygen se lee como una
    # macro de Rd y produce su propio aviso.
    codigo <- lineas[!grepl("^\\s*#", lineas)]
    if (!length(codigo)) next
    crudos <- grepl("[^\x01-\x7F]", codigo, useBytes = FALSE)
    if (any(crudos)) {
      ofensores <- c(ofensores, paste0(
        basename(archivo), ": ", substr(trimws(codigo[crudos][[1L]]), 1L, 60L)
      ))
    }
  }
  expect_equal(ofensores, character())
})
