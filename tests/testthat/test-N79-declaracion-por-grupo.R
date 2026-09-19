# N79: cada rebanada puede retirar declaraciones que ya no describen sus
# columnas, pero ese retiro tiene que quedar publicado.

.datos_N79 <- function() {
  data.frame(
    id = 1:200,
    atributo = rep(c("pais", "edad"), each = 100),
    pais = c(rep(c("UY", "AR"), 50), rep(NA, 100)),
    edad = c(rep(NA, 100), rep(20:69, 2)),
    stringsAsFactors = FALSE
  )
}

.perfilar_N79 <- function(datos, ...) {
  perfilar_por(
    datos, por = "atributo", min_filas = 1L,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE, ...
  )
}

.cobertura_N79 <- function(salida) {
  as.data.frame(attr(salida, "cobertura_grupos", exact = TRUE))
}

test_that("N79 recorta columnas_opcionales por grupo y lo declara", {
  salida <- .perfilar_N79(
    .datos_N79(), columnas_opcionales = "edad"
  )

  expect_setequal(unique(as.character(salida$grupo)), c("pais", "edad"))
  cobertura <- .cobertura_N79(salida)
  fila <- cobertura[cobertura$grupo == "pais", , drop = FALSE]
  expect_match(fila$columnas_descartadas, "edad", fixed = TRUE)
  expect_match(fila$columnas_descartadas, "columnas_opcionales", fixed = TRUE)
  expect_match(fila$columnas_descartadas, "rebanada", fixed = TRUE)
})

test_that("N79 recorta columnas_personales por grupo y lo declara", {
  salida <- .perfilar_N79(
    .datos_N79(), columnas_personales = "edad"
  )

  expect_setequal(unique(as.character(salida$grupo)), c("pais", "edad"))
  cobertura <- .cobertura_N79(salida)
  fila <- cobertura[cobertura$grupo == "pais", , drop = FALSE]
  expect_match(fila$columnas_descartadas, "edad", fixed = TRUE)
  expect_match(fila$columnas_descartadas, "columnas_personales", fixed = TRUE)
  expect_match(fila$columnas_descartadas, "rebanada", fixed = TRUE)
})

test_that("N79 recorta una aplicabilidad cuyo destino falta y lo declara", {
  salida <- .perfilar_N79(
    .datos_N79(), aplicabilidad = list(edad = ~ TRUE)
  )

  expect_setequal(unique(as.character(salida$grupo)), c("pais", "edad"))
  cobertura <- .cobertura_N79(salida)
  fila <- cobertura[cobertura$grupo == "pais", , drop = FALSE]
  expect_match(fila$columnas_descartadas, "edad", fixed = TRUE)
  expect_match(fila$columnas_descartadas, "aplicabilidad", fixed = TRUE)
  expect_match(fila$columnas_descartadas, "rebanada", fixed = TRUE)
})

test_that("N79 recorta una aplicabilidad por variables de la formula", {
  salida <- .perfilar_N79(
    .datos_N79(), aplicabilidad = list(pais = ~ atributo == "pais")
  )

  expect_setequal(unique(as.character(salida$grupo)), c("pais", "edad"))
  cobertura <- .cobertura_N79(salida)
  expect_true(any(grepl("aplicabilidad", cobertura$columnas_descartadas,
                        fixed = TRUE)))
  expect_true(any(grepl("atributo", cobertura$columnas_descartadas,
                        fixed = TRUE)))
  expect_true(any(grepl("rebanada", cobertura$columnas_descartadas,
                        fixed = TRUE)))

  # Una variable del entorno no es una columna ausente: no se recorta por
  # error mientras la columna de la tabla sí siga en la rebanada.
  objetivo <- "pais"
  con_entorno <- .perfilar_N79(
    .datos_N79(), aplicabilidad = list(pais = ~ pais == objetivo)
  )
  cobertura_entorno <- .cobertura_N79(con_entorno)
  pais <- cobertura_entorno[cobertura_entorno$grupo == "pais", , drop = FALSE]
  expect_false(any(grepl("aplicabilidad", pais$columnas_descartadas,
                        fixed = TRUE)))
})

test_that("N79 declara el recorte silencioso de ceros y negativos", {
  datos <- data.frame(
    grupo = c(0, 0, 1, 1), valor = c(0, 1, 0, 1),
    stringsAsFactors = FALSE
  )
  declarado <- perfilar_por(
    datos, por = "grupo", min_filas = 1L,
    columnas_sin_ceros = "grupo", columnas_no_negativas = "grupo",
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  cobertura <- as.data.frame(attr(declarado, "cobertura_grupos", exact = TRUE))

  expect_true(any(grepl("grupo", cobertura$columnas_descartadas,
                        fixed = TRUE)))
  expect_true(any(grepl("columnas_sin_ceros", cobertura$columnas_descartadas,
                        fixed = TRUE)))
  expect_true(any(grepl("columnas_no_negativas",
                        cobertura$columnas_descartadas, fixed = TRUE)))

  directo <- perfilar(
    datos, columnas_sin_ceros = "grupo", columnas_no_negativas = "grupo",
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  expect_true(any(directo$hallazgos$tipo_hallazgo == "ceros_no_permitidos" &
                    directo$hallazgos$columna == "grupo"))
})

test_that("N79 conserva el comportamiento cuando la declaracion sigue presente", {
  datos <- data.frame(
    atributo = c("pais", "pais", "edad", "edad"),
    pais = c(0, 1, NA, NA), edad = c(NA, NA, 20, 21),
    stringsAsFactors = FALSE
  )
  salida <- .perfilar_N79(datos, columnas_sin_ceros = "pais")

  # `pais` está presente en esa rebanada: la regla sigue llegando a
  # `perfilar()` y produce el mismo hallazgo que por la puerta directa.
  pais <- salida[salida$grupo == "pais", , drop = FALSE]
  expect_true(any(pais$tipo_hallazgo == "ceros_no_permitidos" &
                    pais$columna == "pais"))
  cobertura <- .cobertura_N79(salida)
  cobertura_pais <- cobertura[cobertura$grupo == "pais", , drop = FALSE]
  expect_false(any(grepl("columnas_sin_ceros",
                        cobertura_pais$columnas_descartadas, fixed = TRUE)))

  # Control de no-regresion: con una columna presente y sin ceros, declarar la
  # politica conserva exactamente los hallazgos y la cobertura de la salida de
  # hoy sin esa declaracion.
  limpio <- data.frame(
    atributo = c("pais", "pais", "edad", "edad"), valor = 1:4,
    stringsAsFactors = FALSE
  )
  sin_declarar <- .perfilar_N79(limpio)
  con_declaracion <- .perfilar_N79(limpio, columnas_sin_ceros = "valor")
  expect_equal(con_declaracion, sin_declarar)
  expect_equal(
    attr(con_declaracion, "cobertura_grupos", exact = TRUE),
    attr(sin_declarar, "cobertura_grupos", exact = TRUE)
  )
  expect_equal(
    attr(con_declaracion, "cobertura_diagnosticos", exact = TRUE),
    attr(sin_declarar, "cobertura_diagnosticos", exact = TRUE)
  )
})
