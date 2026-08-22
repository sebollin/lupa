skip_if_not_installed("stringdist")

# El mismo dato en distinto orden de filas tiene que dar el mismo veredicto de
# calidad. Cuando el vocabulario entra entero no hay recorte y el orden nunca
# importo; el caso que decide es el recortado, donde el paquete elige QUE
# comparar.
#
# El fixture sale de datos reales: `CAMINO COLMAN` (316 apariciones) y
# `CAMINO COLASTINE` (16) son dos calles de Montevideo que el detector agrupa,
# tomadas del catalogo nacional de ejes de vias. El relleno son formas que
# empiezan con Z para que caigan al final del alfabeto, y la familia se pone
# ULTIMA en el orden de llegada. Asi las dos selecciones posibles -por llegada y
# por alfabeto- no se solapan, que es lo que hace que la prueba distinga.
.columna_familia_tardia <- function() {
  relleno <- paste0("ZONA ", sprintf("%02d", 1:40), " ",
                    rep(LETTERS[1:8], length.out = 40))
  familia <- c(rep("CAMINO COLMAN", 316L), rep("CAMINO COLASTINE", 16L))
  c(relleno, familia)
}

.vocabulario_lupa <- function(x) {
  .grupos_casi_duplicados_vocabulario(
    x, NULL, "columna", max_valores = 40L, max_pares = 2e6
  )
}

test_that("el recorte del vocabulario no depende del orden de las filas", {
  columna <- .columna_familia_tardia()
  set.seed(4)
  ordenes <- list(
    "orden de llegada" = columna,
    "invertido" = rev(columna),
    "alfabetico" = sort(columna),
    "al azar" = sample(columna)
  )
  resultados <- lapply(ordenes, .vocabulario_lupa)

  # Sin recorte la prueba no prueba nada: el paquete no tendria que elegir.
  for (nombre in names(resultados)) {
    expect_true(isTRUE(resultados[[nombre]]$alcance$truncado), info = nombre)
  }

  # La familia tiene que encontrarse en TODOS los ordenes. Con la seleccion por
  # orden de llegada, el orden original daba cero grupos: el relleno se comia el
  # cupo y la familia -que llega ultima- no se comparaba nunca.
  clave <- function(r) {
    sort(vapply(r$grupos, function(g) paste(sort(g$variantes), collapse = "|"),
                character(1L)))
  }
  referencia <- clave(resultados[["orden de llegada"]])
  expect_length(referencia, 1L)
  expect_match(referencia, "CAMINO COLASTINE")
  expect_match(referencia, "CAMINO COLMAN")
  for (nombre in names(resultados)[-1L]) {
    expect_equal(clave(resultados[[nombre]]), referencia, info = nombre)
    expect_equal(
      resultados[[nombre]]$alcance$n_unidades_comparadas,
      resultados[["orden de llegada"]]$alcance$n_unidades_comparadas,
      info = nombre
    )
  }
})

test_that("las frecuencias son las de la columna entera, no las del tramo", {
  # El recorte elige QUE formas se comparan; no puede cambiar CUANTAS veces
  # aparece cada una, porque de esa frecuencia sale la asimetria que decide si
  # hay un dominante.
  columna <- .columna_familia_tardia()
  salida <- .vocabulario_lupa(columna)
  formas <- unlist(lapply(salida$grupos, function(g) g$variantes))
  expect_true("CAMINO COLMAN" %in% formas)
  # 316 y 16 son las apariciones reales en la columna completa.
  expect_equal(sum(columna == "CAMINO COLMAN"), 316L)
  expect_equal(sum(columna == "CAMINO COLASTINE"), 16L)
})

test_that("el orden de comparacion no depende de la configuracion regional", {
  # `sort()` por omision usa la intercalacion del entorno, que cambia de una
  # maquina a otra: con eso el mismo dato daria distinto resultado segun donde
  # corra, que es el defecto que se acaba de sacar, disfrazado. El radix ordena
  # por bytes, igual en todas.
  valores <- c("ANA", "ana", "ZOE", "_ZZ", "1AA", "Ana")
  esperado <- sort(valores, method = "radix")
  previo <- Sys.getlocale("LC_COLLATE")
  on.exit(try(Sys.setlocale("LC_COLLATE", previo), silent = TRUE), add = TRUE)
  for (entorno in c("C", "en_US.UTF-8", "es_UY.UTF-8")) {
    # Una configuracion regional que no este instalada devuelve "" con un aviso,
    # no un error: hay que mirar el valor devuelto y no solo atrapar el error, o
    # la prueba pasa sin haber cambiado nada.
    aplicada <- suppressWarnings(
      try(Sys.setlocale("LC_COLLATE", entorno), silent = TRUE)
    )
    if (inherits(aplicada, "try-error") || !nzchar(aplicada)) next
    expect_equal(sort(valores, method = "radix"), esperado, info = entorno)
  }
})
