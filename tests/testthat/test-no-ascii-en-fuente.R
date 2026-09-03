# `R CMD check` avisa "code files for non-ASCII characters" cuando un archivo de
# codigo trae un caracter no ASCII fuera de un comentario. La suite no podia
# verlo: el barrido de `test-escapes-en-mensajes.R` mira los literales del espacio
# de nombres ya cargado, y ahi "Sesion" con acento crudo y con `ó` se ven
# identicos, porque en tiempo de ejecucion son la misma cadena.
#
# La unica forma de distinguirlos es mirar el fuente, asi que esta comprobacion
# necesita `R/` y `tests/`, que bajo `R CMD check` no existen -alli corre contra
# el paquete instalado-. Se saltea diciendo por que, y en ese entorno el trabajo
# lo hace el check nativo, que es quien avisa.

# Deuda historica congelada el 2026-09-02: archivos que ya traian no-ASCII
# literal cuando esta comprobacion paso a mirar `tests/`. La lista solo puede
# ACHICARSE. No se limpian de golpe porque muchos usan acentos como dato de
# prueba legitimo -normalizacion, reparacion de texto- y cada uno hay que
# mirarlo; lo que esta lista impide es que entre uno nuevo, que es como entro
# el `O` con tilde que dejo una columna sin proteger en macOS.
.NO_ASCII_DEUDA_HISTORICA <- c(
  "tests/testthat/test-afirmaciones.R",
  "tests/testthat/test-analisis-integral.R",
  "tests/testthat/test-arreglos-coleccion.R",
  "tests/testthat/test-asociaciones-spearman.R",
  "tests/testthat/test-ausencia-estructural.R",
  "tests/testthat/test-casi-duplicados-vocabulario.R",
  "tests/testthat/test-claves-relaciones.R",
  "tests/testthat/test-cobertura-declaraciones.R",
  "tests/testthat/test-cobertura-por-razon.R",
  "tests/testthat/test-coleccion.R",
  "tests/testthat/test-contratos-extension.R",
  "tests/testthat/test-datos.R",
  "tests/testthat/test-dependencias-propuesta.R",
  "tests/testthat/test-diagnosticos-invisibles.R",
  "tests/testthat/test-distancias-publicadas.R",
  "tests/testthat/test-duplicados-aproximados.R",
  "tests/testthat/test-estimaciones-externas.R",
  "tests/testthat/test-fechas-tipos.R",
  "tests/testthat/test-formatos-adicionales.R",
  "tests/testthat/test-grupos-guiado.R",
  "tests/testthat/test-hallazgos-administrativos.R",
  "tests/testthat/test-historico-deriva.R",
  "tests/testthat/test-magnitud-plan-dbi.R",
  "tests/testthat/test-marco-generico.R",
  "tests/testthat/test-matriz-tipos.R",
  "tests/testthat/test-mensajes-honestos-dbi.R",
  "tests/testthat/test-modelo-calidad.R",
  "tests/testthat/test-modelo-validaciones.R",
  "tests/testthat/test-no-modifica-los-datos.R",
  "tests/testthat/test-normalizacion.R",
  "tests/testthat/test-patron-raro-clase-desvio.R",
  "tests/testthat/test-patrones.R",
  "tests/testthat/test-perfil.R",
  "tests/testthat/test-referencial.R",
  "tests/testthat/test-relaciones-orden.R",
  "tests/testthat/test-remediacion.R",
  "tests/testthat/test-rendimiento.R",
  "tests/testthat/test-reparacion-texto.R",
  "tests/testthat/test-reportar.R",
  "tests/testthat/test-ronda100.R",
  "tests/testthat/test-ronda101.R",
  "tests/testthat/test-ronda104.R",
  "tests/testthat/test-ronda106.R",
  "tests/testthat/test-ronda120.R",
  "tests/testthat/test-ronda153.R",
  "tests/testthat/test-ronda98.R",
  "tests/testthat/test-ronda99.R",
  "tests/testthat/test-senales-redundantes.R",
  "tests/testthat/test-spool-muestra-dbi.R",
  "tests/testthat/test-trazabilidad-por-clave.R",
  "tests/testthat/test-umbrales-de-regla.R",
  "tests/testthat/test-vocabulario-texto.R"
)

test_that("ningun archivo de R/ ni tests/ trae no-ASCII nuevo fuera de comentarios", {
  raiz_pruebas <- normalizePath(
    testthat::test_path(), winslash = "/", mustWork = FALSE
  )
  raiz_proyecto <- if (basename(raiz_pruebas) == "testthat") {
    dirname(dirname(raiz_pruebas))
  } else {
    raiz_pruebas
  }
  raices <- file.path(raiz_proyecto, c("R", "tests"))
  raices <- raices[dir.exists(raices)]
  skip_if_not(
    length(raices) > 0L,
    "Sin `R/` ni `tests/` a la vista: bajo R CMD check lo comprueba el check nativo."
  )
  archivos <- unlist(lapply(
    raices,
    list.files,
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  ), use.names = FALSE)
  # `_problems/` lo escribe testthat sola con recortes de pruebas que fallaron:
  # no esta versionado y su contenido cambia entre corridas. Vigilarlo haria
  # fallar la suite por un archivo que nadie escribio a mano.
  archivos <- archivos[!grepl("(^|/)_problems/", archivos)]
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
        sub(
          paste0(normalizePath(raiz_proyecto, winslash = "/"), "/"),
          "",
          normalizePath(archivo, winslash = "/"),
          fixed = TRUE
        ),
        ": ", substr(trimws(codigo[crudos][[1L]]), 1L, 60L)
      ))
    }
  }
  rutas <- sub(":.*$", "", ofensores)
  expect_equal(
    ofensores[!rutas %in% .NO_ASCII_DEUDA_HISTORICA],
    character()
  )
})
