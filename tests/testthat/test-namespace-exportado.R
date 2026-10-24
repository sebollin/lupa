# Esta prueba lee el archivo NAMESPACE, no el espacio de nombres cargado, y esa
# distincion es el punto entero.
#
# `pkgload::load_all()` expone TODAS las funciones del paquete, incluidas las
# internas. Bajo esa carga, `getNamespaceExports()` no dice nada sobre lo que un
# usuario va a poder llamar. Un defecto real quedo escondido asi: al insertar
# dos ayudantes **entre** el bloque `roxygen` de `agregar()` y su definicion,
# `roxygen` le pego el `@export` al ayudante. Resultado: `agregar()` dejo de
# exportarse y un interno con punto quedo exportado. Las 14.004 aserciones de la
# suite pasaron igual, porque ninguna miraba el NAMESPACE.

.namespace_declarado <- function() {
  ruta <- system.file("NAMESPACE", package = "lupa")
  if (!nzchar(ruta) || !file.exists(ruta)) {
    ruta <- testthat::test_path("..", "..", "NAMESPACE")
  }
  skip_if_not(file.exists(ruta), "No se encontro el archivo NAMESPACE.")
  readLines(ruta, warn = FALSE)
}

.exportados_declarados <- function() {
  lineas <- .namespace_declarado()
  exportaciones <- grep("^export\\(", lineas, value = TRUE)
  sub("^export\\((.*)\\)$", "\\1", exportaciones)
}

test_that("ninguna funcion interna queda exportada", {
  # Las internas del paquete empiezan con punto. Que una salga en NAMESPACE
  # significa casi siempre que un bloque `roxygen` se pego a quien no debia.
  internas <- grep("^\\.", .exportados_declarados(), value = TRUE)
  expect_equal(internas, character())
})

test_that("las funciones de la ruta principal estan exportadas de verdad", {
  # Una por cada etapa del recorrido que el README promete. Si alguna se cae del
  # NAMESPACE, el paquete instalado no la tiene aunque la suite pase.
  esperadas <- c(
    "perfilar", "analizar", "reportar",
    "metrica", "especializar", "instanciar", "modelo",
    "medir", "agregar", "evaluar", "regla_evaluacion",
    "tablero_calidad", "indice_calidad",
    "planificar_limpieza", "aplicar", "guiar_limpieza",
    "detectar_duplicados_aproximados", "detectar_claves",
    "detectar_dependencias", "detectar_relaciones",
    "coleccion", "perfilar_coleccion", "perfilar_dbi",
    "historico_calidad", "detectar_deriva_calidad"
  )
  faltantes <- setdiff(esperadas, .exportados_declarados())
  expect_equal(faltantes, character())
})

test_that("el numero de exportaciones no cambia sin que nadie lo note", {
  # No es un numero magico: es un guardian de cambios silenciosos. Si sube o
  # baja, hay que mirar por que y actualizar el numero a proposito. La
  # documentacion de cada export la comprueba `R CMD check`.
  expect_gt(length(.exportados_declarados()), 60L)
})
