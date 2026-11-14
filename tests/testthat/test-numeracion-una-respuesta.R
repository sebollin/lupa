# Dentro de un perfil, "es una numeracion?" tiene una sola respuesta, y la leen
# `posible_identificador`, `tipo_declarado_distinto` y `patron_raro`. Un
# identificador guardado como texto con huecos -densidad 0,693- recibia los tres
# a la vez: el perfil lo llamaba identificador, proponia convertirlo a numero y
# marcaba como patron raro sus valores de una cifra. El mismo identificador sin
# huecos recibia solo el primero.
testthat::skip_on_cran()

.perfil_numeracion <- function(x) {
  perfilar(
    data.frame(x = x, stringsAsFactors = FALSE),
    fecha = "2026-01-15", analizar_dependencias = FALSE
  )
}

.tres_hallazgos <- function(perfil) {
  tipos <- as.character(perfil$hallazgos$tipo_hallazgo)
  c(
    posible_identificador = "posible_identificador" %in% tipos,
    tipo_declarado_distinto = "tipo_declarado_distinto" %in% tipos,
    patron_raro = "patron_raro" %in% tipos
  )
}

test_that("un identificador con huecos guardado como texto es solo un identificador", {
  set.seed(5)
  datos <- data.frame(
    x = as.character(sample(1:4557, 3159)), stringsAsFactors = FALSE
  )
  perfil <- .perfil_numeracion(datos$x)
  fila <- perfil$columnas

  # La columna es la del caso: no llega a la secuencia densa, que pide 0,8,
  # pero cubre mas de la mitad de su tramo sin repetir valores.
  expect_false(fila$secuencia_entera_densa)
  expect_gt(fila$densidad_secuencia_entera, 0.5)
  expect_lt(fila$densidad_secuencia_entera, 0.8)
  expect_equal(fila$tasa_distintos, 1)

  expect_identical(
    .tres_hallazgos(perfil),
    c(posible_identificador = TRUE, tipo_declarado_distinto = FALSE,
      patron_raro = FALSE)
  )
  # Y el plan no propone convertir a numero lo que el perfil llama identificador.
  plan <- planificar_limpieza(perfil, datos)
  expect_false(any(plan$hallazgo %in% "tipo_declarado_distinto"))

  # Lo mismo que la misma numeracion sin huecos.
  denso <- .perfil_numeracion(as.character(sample(1:3159)))
  expect_true(denso$columnas$secuencia_entera_densa)
  expect_identical(.tres_hallazgos(denso), .tres_hallazgos(perfil))
})

test_that("lo que no es numeracion conserva la conversion y el patron raro", {
  # Sin esta mitad, callar los dos hallazgos siempre pasaria la prueba de arriba.
  # Un codigo de catalogo que se repite no se distingue por su forma de una
  # magnitud: el perfil no lo llama identificador y propone convertirlo.
  catalogo <- .perfil_numeracion(as.character(rep(1:284, length.out = 20000L)))
  expect_identical(
    .tres_hallazgos(catalogo),
    c(posible_identificador = FALSE, tipo_declarado_distinto = TRUE,
      patron_raro = TRUE)
  )
  # Un tramo lleno con un valor que concentra filas no es numeracion -la moda
  # sobresale-, pero sus `1` a `9` siguen sin ser desvios: un tramo lleno trae
  # todos los largos de cifras.
  concentrada <- .perfil_numeracion(as.character(c(1:2000, rep(500, 500))))
  expect_identical(
    .tres_hallazgos(concentrada),
    c(posible_identificador = FALSE, tipo_declarado_distinto = TRUE,
      patron_raro = FALSE)
  )
})

test_that("una numeracion densa con un bloque borrado sigue siendo numeracion", {
  # Treinta numeros borrados seguidos abren un hueco de 31 contra un tipico de
  # 1, que la regla de forma lee como salto de escala. La secuencia sigue densa y
  # sin repetir, y es un caso de la numeracion: si la respuesta unica no la
  # contara, la columna perderia `posible_identificador` y pasaria a recibir la
  # conversion a numero.
  x <- seq_len(3189L)
  x <- x[-(1501:1530)]
  set.seed(7)
  perfil <- .perfil_numeracion(as.character(sample(x)))
  expect_true(perfil$columnas$salto_de_escala_secuencia_entera)
  expect_true(perfil$columnas$secuencia_entera_densa)
  expect_identical(
    .tres_hallazgos(perfil),
    c(posible_identificador = TRUE, tipo_declarado_distinto = FALSE,
      patron_raro = FALSE)
  )
  # Guardada como numero, Benford la sigue excluyendo como identificador.
  numerica <- .perfil_numeracion(sample(x))
  expect_true("posible_identificador" %in% numerica$hallazgos$tipo_hallazgo)
  expect_false("desviacion_benford" %in% numerica$hallazgos$tipo_hallazgo)
})

test_that("guardar la numeracion con huecos en integer64 no cambia la respuesta", {
  skip_if_not_installed("bit64")
  set.seed(5)
  ids <- sample(1:4557, 3159)
  en_doble <- .perfil_numeracion(as.numeric(ids))
  en_64 <- .perfil_numeracion(bit64::as.integer64(ids))
  expect_true(.tres_hallazgos(en_doble)[["posible_identificador"]])
  expect_identical(.tres_hallazgos(en_64), .tres_hallazgos(en_doble))
})
