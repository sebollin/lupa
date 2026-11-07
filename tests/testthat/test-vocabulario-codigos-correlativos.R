# Una columna de codigos correlativos -`valor_1`, `valor_2`, ...- no tiene
# variantes escritas: dos codigos que difieren en sus digitos son dos entidades,
# y la regla de secuencias numericas descarta el par. Pero el descarte no es
# mudo. El perfil publica cuantos pares cercanos descarto, y usa el componente
# previo al filtro para decidir si el detector aplica (`?perfilar`: "El tamano
# maximo usado para el limite conserva el componente potencial antes del filtro
# numerico"). Por eso esos pares se siguen comparando aunque no puedan terminar
# en un grupo, y lo que se abarato es todo lo que rodeaba a la comparacion.
#
# Estas pruebas fijan la regla, no el tiempo: que los codigos no se agrupan, que
# el recuento publicado es exactamente el de los pares cercanos, que cada forma
# rara se cuelga de la vecina mas frecuente y que un empate en el maximo la deja
# suelta, que la comparacion por pares entrega lo mismo que la matriz, y que k
# columnas iguales siguen dando un hallazgo por par.

test_that("los codigos correlativos no se agrupan y se cuentan todos sus descartes", {
  skip_if_not_installed("stringdist")
  codigos <- paste0("valor_", seq_len(60L))
  resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
    codigos, lupa:::.resolver_normalizacion(FALSE), "x"
  )
  expect_length(resultado$grupos, 0L)
  # Recuento independiente: todo par a distancia Jaro-Winkler <= 0,10 tiene
  # firmas numericas distintas (1 a 60), asi que todos se descartan. Los
  # codigos tienen siete caracteres o mas, y la ruta de edicion corta no entra.
  distancias <- stringdist::stringdistmatrix(
    codigos, codigos, method = "jw", p = 0.1
  )
  cercanos <- sum(distancias[upper.tri(distancias)] <= 0.10)
  expect_gt(cercanos, 0L)
  expect_equal(resultado$alcance$n_pares_descartados_numeros, cercanos)
  expect_equal(resultado$alcance$tamano_grupo_maximo_numerico, 0L)

  perfil <- perfilar(
    data.frame(codigo = codigos, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  )
  vocabulario <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(vocabulario), 1L)
  expect_equal(as.character(vocabulario$severidad), "ok")
  expect_equal(vocabulario$n_afectados, 0)
  expect_match(
    vocabulario$evidencia,
    paste0("pares descartados por secuencia num\u00e9rica=", cercanos, ";"),
    fixed = TRUE
  )

  # Con frecuencias desparejas, la familia entera queda en un componente antes
  # del filtro y el detector declara que no aplica, como fija la prueba de
  # `Zona 01..15`.
  desparejos <- rep(codigos[1:15], times = seq(100L, 72L, by = -2L))
  perfil_desparejo <- perfilar(
    data.frame(codigo = desparejos, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  )
  expect_equal(sum(
    perfil_desparejo$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario"
  ), 0L)
  cobertura <- perfil_desparejo$cobertura_diagnosticos
  cobertura <- cobertura[cobertura$diagnostico == "proximidad_vocabulario", ,
                         drop = FALSE]
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$motivo, "no aplica")
})

test_that("la forma rara se cuelga de la vecina mas frecuente y el empate la deja suelta", {
  skip_if_not_installed("stringdist")
  normalizacion <- lupa:::.resolver_normalizacion(FALSE)
  grupo_de <- function(resultado, forma) {
    for (grupo in resultado$grupos) {
      if (forma %in% grupo$variantes) return(.sort_ascii(grupo$variantes))
    }
    character()
  }
  .sort_ascii <- function(x) x[order(x, method = "radix")]

  # `Montevido` esta cerca de `Montevideo` (60) y de `Montevidoe` (20): queda
  # con la de frecuencia maxima, y `Montevidoe` tambien.
  distinto <- c(rep("Montevideo", 60L), rep("Montevidoe", 20L),
                rep("Montevido", 2L))
  resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
    distinto, normalizacion, "x"
  )
  expect_length(resultado$grupos, 1L)
  expect_identical(
    grupo_de(resultado, "Montevido"),
    .sort_ascii(c("Montevideo", "Montevidoe", "Montevido"))
  )
  expect_equal(resultado$grupos[[1L]]$frecuencias[
    resultado$grupos[[1L]]$variantes == "Montevideo"
  ], 60L)

  # Si las dos vecinas empatan en el maximo, la forma rara no tiene un centro
  # unico y no se cuelga de ninguna: no hay grupo por distancia.
  empate <- c(rep("Montevideo", 40L), rep("Montevidoe", 40L),
              rep("Montevido", 2L))
  resultado_empate <- lupa:::.grupos_casi_duplicados_vocabulario(
    empate, normalizacion, "x"
  )
  expect_length(resultado_empate$grupos, 0L)
  expect_gt(resultado_empate$alcance$n_candidatos_distancia, 0L)
})

test_that("comparar por pares entrega los mismos pares que la matriz de cada tesela", {
  skip_if_not_installed("stringdist")
  valores <- c(
    paste0("valor_", 1:9), "Montevideo", "Montevido", "Montevideo ",
    "Canelones", "Canelone", "cod1", "cod2", "x", "xy", "Rivera", "Riviera",
    "abc"
  )
  filas <- seq_along(valores) + 100L
  bloqueos <- rep(c(1L, 2L, 1L), length.out = length(valores))
  recolectar <- function(metodo, umbral, bloque, bloquear, solo_pares) {
    entregas <- list()
    guardar <- function(f1, f2, d) {
      entregas[[length(entregas) + 1L]] <<- list(f1, f2, d)
    }
    bloqueo <- if (bloquear) bloqueos else NULL
    if (solo_pares) {
      lupa:::.comparar_bloques_duplicados(
        valores, filas, metodo, umbral, bloque = bloque, max_resultados = 1L,
        on_pairs = guardar, bloqueos = bloqueo, nucleos = 1L, p = 0.1,
        solo_pares = TRUE
      )
    } else {
      lupa:::.comparar_bloques_duplicados(
        valores, filas, metodo, umbral, bloque = bloque, max_resultados = 1L,
        on_pairs = guardar, bloqueos = bloqueo, nucleos = 1L, p = 0.1
      )
    }
    entregas
  }
  # Teselas de 1 (sin diagonal util), de 2, una que deja una ultima de una
  # sola forma (20 de 21) y una que cubre todo.
  for (bloque in c(1L, 2L, 6L, 20L, 1000L)) {
    for (bloquear in c(FALSE, TRUE)) {
      for (criterio in list(list("jw", 0.10), list("lv", 1))) {
        con_matriz <- recolectar(criterio[[1L]], criterio[[2L]], bloque,
                                 bloquear, solo_pares = FALSE)
        por_pares <- recolectar(criterio[[1L]], criterio[[2L]], bloque,
                                bloquear, solo_pares = TRUE)
        expect_gt(length(con_matriz), 0L)
        expect_identical(por_pares, con_matriz)
      }
    }
  }
})

test_that("k columnas iguales dan un hallazgo por grupo, con todas", {
  v <- c("a", "b", NA, "a")
  datos <- data.frame(p = v, q = v, r = v, s = 1:4, t = v,
                      stringsAsFactors = FALSE)
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE
  )
  todos <- hallazgos(perfil)
  iguales <- todos[todos$tipo_hallazgo == "columnas_duplicadas", ,
                   drop = FALSE]
  expect_identical(
    iguales$evidencia, "p = q = r = t; comparadas sobre 4 filas"
  )
  expect_identical(iguales$columna, "p")
  expect_identical(iguales$descripcion, "4 columnas tienen el mismo contenido.")
  expect_equal(iguales$n_evaluados, 5)
  expect_equal(iguales$n_afectados, 4)
  expect_true(all(iguales$unidad_conteo == "columna"))
  expect_true(all(vapply(iguales$trazabilidad, function(traza) {
    identical(traza$estado, "no_aplica") && identical(traza$alcance, "no_aplica")
  }, logical(1L))))
})
