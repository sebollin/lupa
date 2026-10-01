# Ronda 15: claves, relaciones, dependencias y el referencial.

.medir_fuerte_O105 <- function(padron, entidad) {
  ref <- referencial(
    data.frame(id = padron, nombre = paste0("n", seq_along(padron))),
    "id", "nombre", completo = TRUE, alcance = "prueba"
  )
  instancia <- instanciar(
    especializar(metricas_referencial()$CorrectitudSemFuerte), "personas", "id",
    referencial = ref
  )
  suppressWarnings(medir(modelo(instancia), data.frame(id = entidad)))$resultado
}

test_that("el referencial empareja numeros por su valor", {
  # Dos dobles distintos que `as.character()` escribe igual: el id de la fila 1
  # no esta en el padron.
  expect_equal(.medir_fuerte_O105(c(0.3, 1.5), c(0.1 + 0.2, 1.5)), c(0, 1))
  # El mismo numero como entero en una tabla y como doble en la otra.
  expect_equal(.medir_fuerte_O105(c(1e5, 2), c(100000L, 2L)), c(1, 1))
  expect_equal(.medir_fuerte_O105(c(100000L, 2L), c(1e5, 2)), c(1, 1))
  # Control: el cero negativo es el cero.
  expect_equal(.medir_fuerte_O105(c(0, 1), c(-0, 1)), c(1, 1))
})

test_that("EntidadDuplicada no funde dos claves dobles distintas", {
  instancia <- instanciar(
    especializar(metricas_nucleo()$EntidadDuplicada), "t", "clave"
  )
  distintas <- data.frame(clave = c(0.1 + 0.2, 0.3, 7), v = c("p", "p", "q"))
  expect_equal(
    suppressWarnings(medir(modelo(instancia), list(t = distintas)))$resultado,
    c(0, 0, 0)
  )
  # Control: con la misma clave, si es la misma entidad.
  iguales <- data.frame(clave = c(0.3, 0.3, 7), v = c("p", "p", "q"))
  expect_equal(
    suppressWarnings(medir(modelo(instancia), list(t = iguales)))$resultado,
    c(1, 1, 0)
  )
})

test_that("sugerir_clave no mide una columna cuyo nombre se repite", {
  datos <- data.frame(a = 1:3, b = c(1, 1, 2), z = 11:13)
  names(datos) <- c("id", "id", "z")
  s <- sugerir_clave(datos)
  # Antes: dos filas `id` con identifica TRUE y tasa 1, y la segunda columna
  # tiene un duplicado.
  fila <- s[s$columna == "id", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_true(is.na(fila$identifica))
  expect_match(fila$motivo, "no se midio", fixed = TRUE)
  expect_true(isTRUE(s$identifica[s$columna == "z"]))
  mensajes <- character()
  withCallingHandlers(
    elegir_clave(datos),
    message = function(m) {
      mensajes <<- c(mensajes, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("No se ofrece `id`", mensajes, fixed = TRUE)))
  expect_false(any(grepl("ofrecido `id`", mensajes, fixed = TRUE)))
  # Control: con nombres unicos se mide.
  unicos <- data.frame(id = 1:3, x = c(1, 1, 2))
  expect_true(isTRUE(sugerir_clave(unicos)$identifica[[1L]]))
})

test_that("detectar_claves no prueba columnas que su filtro excluyo", {
  # Una sola columna analizable que no es la primera: `combn(k, 1)` probaba
  # `seq_len(k)`.
  claves <- detectar_claves(data.frame(importe = c(1.5, 2.5, 3.5), id = 1:3))
  expect_identical(claves$columnas, "id")
  claves <- detectar_claves(
    data.frame(l = I(list(1, 2, 3)), importe = c(1.5, 2.5, 3.5), id = 1:3)
  )
  expect_identical(claves$columnas, "id")
  # Control: en el otro orden ya salia bien.
  expect_identical(
    detectar_claves(data.frame(id = 1:3, importe = c(1.5, 2.5, 3.5)))$columnas,
    "id"
  )
})

test_that("detectar_claves avisa y no publica un nombre repetido", {
  datos <- data.frame(id = 1:40, id = c(1, 1, 3:40), x = stats::runif(40),
                      check.names = FALSE)
  expect_warning(claves <- detectar_claves(datos), "repetido")
  expect_false("id" %in% claves$columnas)
  expect_identical(attr(claves, "columnas_nombre_repetido"), "id")
})

test_that("detectar_relaciones no compara lo que daria una respuesta falsa", {
  t1 <- data.frame(a = 1:3, b = c("x", "y", "z"))
  names(t1) <- c("id", "id")
  r <- detectar_relaciones(t1, data.frame(id = c(1, 4, 9)))
  expect_equal(nrow(r), 1L)
  expect_identical(r$cardinalidad, "sin_comparar")
  expect_identical(r$motivo_poda, "nombre_repetido")

  padre <- data.frame(id = as.POSIXct(c("2020-01-01", "2020-01-02"), tz = "UTC"))
  hijo <- data.frame(pid = as.Date(c("2020-01-01", "2020-01-02", "2020-01-02")))
  r <- detectar_relaciones(padre, hijo)
  # Antes: `sin_coincidencias` con cobertura 0 sobre los mismos dias.
  expect_identical(r$cardinalidad, "sin_comparar")
  expect_identical(r$motivo_poda, "fecha_contra_instante")
  # Control: las dos como fecha se relacionan.
  r <- detectar_relaciones(data.frame(id = as.Date(padre$id)), hijo)
  expect_identical(r$cardinalidad, "1:m")
  expect_equal(r$cobertura_tabla2_en_tabla1, 1)
})

test_that("detectar_dependencias descarta un nombre repetido y perfilar lo dice", {
  a <- rep(1:6, each = 10)
  unicos <- data.frame(id = 1:60, a = a, b = a * 10, t = rep(c("x", "y"), 30))
  repetidos <- unicos
  names(repetidos) <- c("id", "k", "k", "t")
  dep <- detectar_dependencias(repetidos)
  # Antes: `k -> k` dos veces.
  expect_false(any(dep$determinante == "k" | dep$dependiente == "k"))
  descartadas <- attr(dep, "columnas_descartadas")
  expect_equal(sum(descartadas$motivo == "nombre_repetido"), 2L)
  cobertura <- suppressWarnings(perfilar(repetidos))$cobertura_diagnosticos
  fila <- cobertura[cobertura$diagnostico == "dependencias_funcionales", ]
  expect_match(fila$motivo, "nombre se repite", fixed = TRUE)
  # Control: con nombres unicos, las dos dependencias.
  dep <- detectar_dependencias(unicos)
  expect_true(all(c("a", "b") %in% dep$determinante))
})

test_that("RatioCobertura no publica 1 sobre un referencial sin claves", {
  cobertura <- function(padron) {
    ref <- referencial(
      data.frame(id = padron, nombre = rep("n", length(padron))),
      "id", "nombre", completo = TRUE, alcance = "prueba"
    )
    instancia <- instanciar(
      especializar(metricas_referencial()$RatioCobertura), "personas", "id",
      referencial = ref
    )
    suppressWarnings(medir(modelo(instancia), data.frame(id = c(1, 2))))
  }
  vacio <- cobertura(numeric())
  # Antes: resultado 1, cobertura perfecta de un universo vacio.
  expect_equal(nrow(vacio), 0L)
  estado <- attr(vacio, "cobertura_metricas")
  expect_identical(as.character(estado$estado), "sin_valores")
  expect_match(estado$motivo, "no tiene claves", fixed = TRUE)
  # Control: con claves, la proporcion.
  expect_equal(cobertura(c(1, 3))$resultado, 0.5)
})
