# O08. El NAMESPACE exporta lo publico y nada interno.
#
# Una funcion auxiliar insertada ENTRE el bloque roxygen de
# `planificar_limpieza()` y la funcion hizo que `devtools::document()` le
# pegara ese bloque -con su `@export`- a la auxiliar: el NAMESPACE paso a
# exportar `.estrategias_por_celda` y dejo de exportar `planificar_limpieza`.
# La suite entera paso en verde, porque `load_all()` expone todo, exportado o
# no. Instalado, el paquete habria dejado a los usuarios sin una de sus
# funciones centrales. Lo atrapo un `git status`, no una prueba: esto es la
# prueba.

.o08_exports <- function() {
  raiz <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  archivo <- file.path(raiz, "NAMESPACE")
  if (!file.exists(archivo)) return(NULL)
  lineas <- readLines(archivo, warn = FALSE)
  exports <- regmatches(lineas, regexpr("^export\\(([^)]+)\\)", lineas))
  sub("^export\\((.+)\\)$", "\\1", exports)
}

test_that("ningun objeto interno queda exportado", {
  exports <- .o08_exports()
  skip_if(is.null(exports), "el NAMESPACE no esta a la vista")
  # Control de la sonda: si no leyo exports, lo de abajo pasaria en falso.
  expect_gt(length(exports), 20L)
  internos <- exports[startsWith(exports, ".")]
  expect_identical(internos, character())
})

test_that("las funciones publicas centrales estan exportadas", {
  exports <- .o08_exports()
  skip_if(is.null(exports), "el NAMESPACE no esta a la vista")
  centrales <- c(
    "perfilar", "hallazgos", "planificar_limpieza", "aplicar",
    "comparar_perfiles", "reportar", "detectar_claves", "analizar"
  )
  expect_true(all(centrales %in% exports),
              info = paste(setdiff(centrales, exports), collapse = ", "))
})
