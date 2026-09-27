# Cuatro cosas que `perfilar_por()` publicaba y no cerraban.
#
# Las tres primeras son del libro de cobertura, que existe para poder verificar que
# no se pierde ninguna fila; la cuarta es el rotulo del grupo, que tiene que volver
# al valor del que salio.

reconciliar_o76 <- function(hallazgos) {
  cobertura <- attr(hallazgos, "cobertura_grupos", exact = TRUE)
  # Hay una fila por grupo y motivo, asi que la cuenta va sobre grupos DISTINTOS.
  perfilados <- unique(hallazgos[, c("grupo", "n_filas_grupo"), drop = FALSE])
  desde_cobertura <- unique(
    cobertura[, c("grupo", "n_filas_grupo"), drop = FALSE]
  )
  todos <- unique(rbind(perfilados, desde_cobertura))
  sum(as.numeric(todos$n_filas_grupo))
}

test_that("el libro de cobertura reconcilia con las filas de la tabla", {
  # El grupo `(ausente)` aparecia en los hallazgos Y en la cobertura, las dos veces
  # con sus 40 filas: la suma daba 120 sobre 80 filas reales. Ahora `grupo_perfilado`
  # dice de que clase de grupo habla cada fila y la cuenta cierra.
  datos <- data.frame(
    g = c(rep(NA_character_, 30), rep("(ausente)", 10), rep("B", 40)),
    x = rep(c(1, 2, 3, 4), length.out = 80),
    stringsAsFactors = FALSE
  )

  hallazgos <- perfilar_por(datos, "g", min_filas = 20)
  cobertura <- attr(hallazgos, "cobertura_grupos", exact = TRUE)

  expect_true("grupo_perfilado" %in% names(cobertura))
  expect_identical(reconciliar_o76(hallazgos), 80)
  # La fila de colision habla de un grupo que SI se perfilo.
  colision <- cobertura[cobertura$grupo == "(ausente)", , drop = FALSE]
  expect_identical(nrow(colision), 1L)
  expect_true(colision$grupo_perfilado[[1L]])
})

test_that("la marca de la fila de colision se mide y no se supone", {
  # Si el grupo fusionado queda por debajo de `min_filas` NO se perfila, y la fila de
  # colision tiene que decirlo. La primera version del arreglo dejaba la marca fija
  # en TRUE y era falsa justo en este caso.
  datos <- data.frame(
    g = c(rep(NA_character_, 8), rep("(ausente)", 2), rep("B", 40)),
    x = rep(c(1, 2, 3, 4), length.out = 50),
    stringsAsFactors = FALSE
  )

  hallazgos <- perfilar_por(datos, "g", min_filas = 20)
  cobertura <- attr(hallazgos, "cobertura_grupos", exact = TRUE)
  colision <- cobertura[cobertura$grupo == "(ausente)", , drop = FALSE]

  expect_true(all(!colision$grupo_perfilado))
  expect_identical(reconciliar_o76(hallazgos), 50)
})

test_that("sin ausentes no se declara ninguna colision", {
  # La declaracion decia "junta 0 fila(s) con la columna de agrupacion ausente y 40
  # fila(s) cuyo valor real es el texto `(ausente)`. Son dos cosas distintas": una
  # afirmacion falsa dentro del libro de cobertura.
  datos <- data.frame(
    g = c(rep("(ausente)", 40), rep("B", 40)),
    x = rep(c(1, 2, 3, 4), length.out = 80),
    stringsAsFactors = FALSE
  )

  hallazgos <- perfilar_por(datos, "g", min_filas = 1L)
  cobertura <- attr(hallazgos, "cobertura_grupos", exact = TRUE)

  expect_identical(sum(is.na(datos$g)), 0L)
  expect_false(any(grepl("dos cosas distintas", cobertura$motivo, fixed = TRUE)))
  expect_true("(ausente)" %in% hallazgos$grupo)
})

test_that("con ausentes Y el literal, la colision sigue declarandose", {
  # Mitad de control: la guarda nueva no puede haberse comido la declaracion real.
  datos <- data.frame(
    g = c(rep(NA_character_, 20), rep("(ausente)", 20), rep("B", 40)),
    x = rep(c(1, 2, 3, 4), length.out = 80),
    stringsAsFactors = FALSE
  )

  cobertura <- attr(
    perfilar_por(datos, "g", min_filas = 1L), "cobertura_grupos", exact = TRUE
  )

  expect_true(any(grepl("dos cosas distintas", cobertura$motivo, fixed = TRUE)))
})

test_that("un nivel de factor sin filas se declara en la cobertura", {
  datos <- data.frame(
    g = factor(c(rep("A", 40), rep("B", 40)), levels = c("A", "B", "C", "D")),
    x = rep(c(1, 2, 3, 4), length.out = 80)
  )

  hallazgos <- perfilar_por(datos, "g", min_filas = 1L)
  cobertura <- attr(hallazgos, "cobertura_grupos", exact = TRUE)
  sin_filas <- cobertura[cobertura$n_filas_grupo == 0L, , drop = FALSE]

  expect_setequal(as.character(sin_filas$grupo), c("C", "D"))
  expect_true(all(!sin_filas$grupo_perfilado))
  expect_true(all(grepl("no tiene ninguna fila", sin_filas$motivo, fixed = TRUE)))
  # `n_grupos` sigue contando los grupos con filas, que es lo que se perfila.
  expect_identical(attr(hallazgos, "n_grupos", exact = TRUE), 2L)
})

test_that("un factor con todos sus niveles observados no declara nada de mas", {
  # Mitad de control del anterior.
  datos <- data.frame(
    g = factor(c(rep("A", 40), rep("B", 40))),
    x = rep(c(1, 2, 3, 4), length.out = 80)
  )

  cobertura <- attr(
    perfilar_por(datos, "g", min_filas = 1L), "cobertura_grupos", exact = TRUE
  )

  expect_false(any(grepl("no tiene ninguna fila", cobertura$motivo, fixed = TRUE)))
})

test_that("dos dobles distintos que se escriben igual son dos grupos", {
  # `as.character()` sobre un doble usa 15 cifras: los dos valores daban "1e+17" y
  # caian en un solo grupo de 80 filas cuyos numeros no correspondian a ninguno de
  # los dos. La etiqueta ahora vuelve al valor.
  primero <- 1e17
  segundo <- primero + 32
  datos <- data.frame(
    g = c(rep(primero, 40), rep(segundo, 40)),
    x = c(rep(c(1, 2, 3, 4), length.out = 40), rep(c(5, 6, 7, 8), length.out = 40))
  )
  expect_identical(length(unique(datos$g)), 2L)

  hallazgos <- perfilar_por(datos, "g", min_filas = 1L)

  expect_identical(attr(hallazgos, "n_grupos", exact = TRUE), 2L)
  etiquetas <- unique(hallazgos$grupo)
  expect_identical(length(etiquetas), 2L)
  # Y cada etiqueta vuelve a su valor, que es lo que la hace util.
  expect_setequal(as.numeric(etiquetas), c(primero, segundo))
})

test_that("la etiqueta de un numero corriente no se alarga", {
  # Mitad de control: si la etiqueta reversible se aplicara a todo, un grupo `0.1`
  # pasaria a llamarse `0.10000000000000001` y la salida se volveria ilegible.
  expect_identical(lupa:::.etiqueta_numero_reversible(c(0.1, 3.5, 2)),
                   c("0.1", "3.5", "2"))
  expect_identical(lupa:::.etiqueta_numero_reversible(c("a", "b")), c("a", "b"))
  # Y un NA sigue siendo NA, que es lo que la rama de ausentes espera.
  expect_true(is.na(lupa:::.etiqueta_numero_reversible(NA_real_)))
})
