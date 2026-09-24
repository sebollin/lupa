# O35: `n_no_reversibles` contaba por accion -toda celda que la accion tocaba, o
# ninguna-, y la documentacion define otra cosa: "las celdas cuyo VALOR se
# perdio", que no cuenta "los cambios de forma que dejan el valor en su lugar".
# La misma accion hace las dos cosas segun el dato, asi que la perdida se mide
# sobre el resultado: una celda pierde su valor cuando la transformacion la
# vuelve indistinguible de otra que era distinta, o cuando la deja ausente.

.blando <- function() {
  x <- rawToChar(as.raw(c(0xC2, 0xAD)))
  Encoding(x) <- "UTF-8"
  x
}

.ancho_cero <- function() {
  x <- rawToChar(as.raw(c(0xE2, 0x80, 0x8B)))
  Encoding(x) <- "UTF-8"
  x
}

.aplicar_recomendadas <- function(datos) {
  perfil <- perfilar(datos, analizar_dependencias = FALSE,
                     proteger_datos_personales = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)
  plan$aplicar <- plan$recomendada
  suppressMessages(aplicar(plan, datos))
}

.registro_de <- function(resultado, estrategia) {
  registro <- as.data.frame(resultado$registro)
  registro[registro$estrategia == estrategia, , drop = FALSE]
}

test_that("quitar un invisible que no colapsa nada no cuenta como perdida", {
  blando <- .blando()
  datos <- data.frame(
    v = c(paste0("PRO", blando, "DUCTO-A"), paste0("PRO", blando, "DUCTO-B"),
          paste0("PRO", blando, "DUCTO-C"), "PRODUCTO-D"),
    stringsAsFactors = FALSE
  )

  resultado <- .aplicar_recomendadas(datos)
  fila <- .registro_de(resultado, "eliminar_controles_invisibles")

  # El valor queda exactamente en su forma canonica y sigue distinguiendose.
  expect_equal(resultado$datos$v,
               c("PRODUCTO-A", "PRODUCTO-B", "PRODUCTO-C", "PRODUCTO-D"))
  expect_equal(length(unique(resultado$datos$v)), length(unique(datos$v)))
  expect_equal(fila$n_cambiadas[[1L]], 3)
  expect_equal(fila$n_no_reversibles[[1L]], 0)
})

test_that("quitar un invisible que fusiona claves distintas si cuenta", {
  zwsp <- .ancho_cero()
  datos <- data.frame(
    v = c(paste0("APLI", zwsp, "24"), "APLI24",
          paste0("APLI", zwsp, "26"), "APLI26", "OTRO30"),
    stringsAsFactors = FALSE
  )

  resultado <- .aplicar_recomendadas(datos)
  fila <- .registro_de(resultado, "eliminar_controles_invisibles")

  # Cinco claves distintas quedan en tres: lo que las separaba no esta en
  # ningun lado, y eso es exactamente lo que el conteo declara.
  expect_equal(length(unique(datos$v)), 5L)
  expect_equal(length(unique(resultado$datos$v)), 3L)
  expect_equal(fila$n_cambiadas[[1L]], 2)
  expect_equal(fila$n_no_reversibles[[1L]], 2)
})

test_that("un recorte que fusiona dos valores distintos declara la perdida", {
  # El caso simetrico, que no habia reportado nadie: `recortar_espacios`
  # informaba cero irreversibles incluso cuando dejaba dos valores en uno.
  datos <- data.frame(v = c(" ana ", "ana", " beto ", "caro"),
                      stringsAsFactors = FALSE)

  resultado <- .aplicar_recomendadas(datos)
  fila <- .registro_de(resultado, "recortar_espacios")

  expect_equal(length(unique(datos$v)), 4L)
  expect_equal(length(unique(resultado$datos$v)), 3L)
  expect_equal(fila$n_cambiadas[[1L]], 2)
  expect_equal(fila$n_no_reversibles[[1L]], 1)
})

test_that("un recorte que no fusiona nada sigue sin declarar perdida", {
  # Control: la regla no convierte toda normalizacion en perdida.
  datos <- data.frame(v = c(" ana ", " beto ", " caro ", "dina"),
                      stringsAsFactors = FALSE)

  resultado <- .aplicar_recomendadas(datos)
  fila <- .registro_de(resultado, "recortar_espacios")

  expect_equal(length(unique(resultado$datos$v)), 4L)
  expect_equal(fila$n_cambiadas[[1L]], 3)
  expect_equal(fila$n_no_reversibles[[1L]], 0)
})

test_that("convertir un marcador de ausencia sigue contando cada celda", {
  # Control de la otra familia: una accion que REEMPLAZA el valor por una
  # ausencia lo pierde siempre, colapse o no, y la documentacion la nombra.
  datos <- data.frame(
    v = c("12", "13", "N/A", "sin dato", "15"),
    stringsAsFactors = FALSE
  )

  perfil <- perfilar(datos, analizar_dependencias = FALSE,
                     proteger_datos_personales = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)
  plan$aplicar <- plan$estrategia == "convertir_ausencias_textuales"
  if (!any(plan$aplicar)) {
    skip("esta version no propone la conversion de ausencias para esta tabla")
  }
  resultado <- suppressMessages(aplicar(plan, datos))
  fila <- .registro_de(resultado, "convertir_ausencias_textuales")

  expect_equal(fila$n_cambiadas[[1L]], fila$n_no_reversibles[[1L]])
  expect_true(fila$n_no_reversibles[[1L]] > 0)
})
