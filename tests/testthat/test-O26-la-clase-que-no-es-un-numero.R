# O26: cuatro silencios de la misma familia. Tres son columnas que el paquete
# trataba por el nombre de su clase -o por no tener nombre ninguno- y una es un
# hallazgo medido que el plan dejaba caer sin decirlo.

test_that("una columna Period no mata a perfilar y queda declarada", {
  skip_if_not_installed("lubridate")
  valores <- as.integer(c(1:40, 41:80))
  # Con una relacion entre las dos columnas peladas, el perfil publica el
  # alcance del diagnostico; ahi se lee que la tercera quedo afuera y por que.
  datos <- data.frame(neto = as.numeric(valores))
  datos$total <- datos$neto * 1.22
  datos$plazo <- lubridate::days(valores)

  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  expect_equal(nrow(perfil$columnas), 3L)
  expect_equal(sum(perfil$hallazgos$tipo_hallazgo ==
                     "relacion_aritmetica_columnas"), 1L)
  alcance <- perfil$meta$aritmetica_columnas
  expect_false("plazo" %in% alcance$columnas_numericas)
  expect_true("Period" %in% alcance$clases_excluidas)
  expect_equal(unname(alcance$columnas_excluidas_por_clase[["plazo"]]), "Period")

  cobertura <- perfil$cobertura_diagnosticos
  fila <- cobertura[
    cobertura$diagnostico == "relacion_aritmetica_columnas" &
      cobertura$columna == "plazo", ,
    drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$motivo[[1L]], "Period", fixed = TRUE)
  expect_match(fila$como_resolverlo[[1L]], "as.numeric()", fixed = TRUE)
})

test_that("una columna units tampoco aborta y Benford la declara", {
  skip_if_not_installed("units")
  set.seed(126)
  valores <- round(stats::runif(120, 1, 9999), 2)
  datos <- data.frame(base = valores * 2)
  datos$distancia <- units::set_units(valores, "m")

  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  expect_equal(nrow(perfil$columnas), 2L)
  cobertura <- perfil$cobertura_diagnosticos
  benford <- cobertura[
    cobertura$diagnostico == "ley_benford" & cobertura$columna == "distancia", ,
    drop = FALSE
  ]
  expect_equal(nrow(benford), 1L)
  expect_match(benford$motivo[[1L]], "units", fixed = TRUE)
})

test_that("las columnas numericas peladas siguen entrando al analisis", {
  # Control del caso anterior: el criterio nuevo no puede callar lo que ya
  # medía. `I()` no es una declaracion sobre el dato y tampoco excluye.
  set.seed(1266)
  neto <- round(stats::runif(60, 10, 900), 2)
  datos <- data.frame(neto = neto)
  datos$total <- I(array(neto * 1.22, 60L))

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  relaciones <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "relacion_aritmetica_columnas", ,
    drop = FALSE
  ]

  expect_equal(nrow(relaciones), 1L)
  expect_true(all(c("neto", "total") %in%
                    perfil$meta$aritmetica_columnas$columnas_numericas))
  expect_length(perfil$meta$aritmetica_columnas$columnas_excluidas_por_clase, 0L)
  expect_false(any(
    perfil$cobertura_diagnosticos$diagnostico == "relacion_aritmetica_columnas"
  ))
})

test_that("una columna de listas junto a una matriz no rompe el conteo", {
  datos <- data.frame(id = c(1, 2, 1))
  datos$m <- matrix(c(10, 20, 10, 30, 40, 30), nrow = 3L)
  datos$l <- list(c(1, 2), NULL, c(1, 2))

  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  expect_equal(perfil$general$filas_duplicadas, 1)
  expect_equal(perfil$general$filas_en_grupos_duplicados, 2)
  expect_equal(perfil$columnas$columna, c("id", "m", "l"))
})

test_that("la misma tabla sin la columna de listas cuenta igual", {
  # Control: el aplanado de la matriz no cambio para las tablas que ya andaban.
  datos <- data.frame(id = c(1, 2, 1))
  datos$m <- matrix(c(10, 20, 10, 30, 40, 30), nrow = 3L)

  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  expect_equal(perfil$general$filas_duplicadas, 1)
  expect_equal(perfil$general$filas_en_grupos_duplicados, 2)
})

test_that("sobre un factor ordenado la limpieza de texto no se activa sola", {
  datos <- data.frame(n = 1:4)
  datos$nivel <- factor(
    c(" bajo", "medio ", "alto", "medio "),
    levels = c(" bajo", "medio ", "alto", "critico"), ordered = TRUE
  )

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)
  fila <- as.data.frame(plan)
  fila <- fila[fila$estrategia == "recortar_espacios", , drop = FALSE]

  expect_equal(nrow(fila), 1L)
  expect_true(fila$recomendada[[1L]])
  expect_false(fila$aplicar[[1L]])
  expect_match(fila$justificacion[[1L]], "devuelve texto", fixed = TRUE)
  expect_match(fila$justificacion[[1L]], "sin activar", fixed = TRUE)

  aplicado <- aplicar(plan, datos)
  expect_true(is.ordered(aplicado$datos$nivel))
  expect_equal(levels(aplicado$datos$nivel),
               c(" bajo", "medio ", "alto", "critico"))
})

test_that("sobre texto plano la limpieza sigue activada y sobre factor avisa", {
  # Control: lo que cambia es lo que la columna declara, no la estrategia.
  datos <- data.frame(n = 1:4,
                      nivel = c(" bajo", "medio ", "alto", "medio "),
                      stringsAsFactors = FALSE)
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)
  fila <- as.data.frame(plan)
  fila <- fila[fila$estrategia == "recortar_espacios", , drop = FALSE]

  expect_true(fila$aplicar[[1L]])
  expect_false(grepl("devuelve texto", fila$justificacion[[1L]], fixed = TRUE))
  aplicado <- aplicar(plan, datos)
  expect_equal(aplicado$datos$nivel, c("bajo", "medio", "alto", "medio"))

  sin_orden <- data.frame(n = 1:4)
  sin_orden$nivel <- factor(c(" bajo", "medio ", "alto", "medio "))
  perfil_sin_orden <- perfilar(sin_orden, analizar_dependencias = FALSE)
  plan_sin_orden <- planificar_limpieza(perfil_sin_orden, datos = sin_orden)
  fila_sin_orden <- as.data.frame(plan_sin_orden)
  fila_sin_orden <- fila_sin_orden[
    fila_sin_orden$estrategia == "recortar_espacios", ,
    drop = FALSE
  ]
  expect_true(fila_sin_orden$aplicar[[1L]])
  expect_match(fila_sin_orden$justificacion[[1L]], "devuelve texto",
               fixed = TRUE)
})

test_that("un hallazgo sin accion por nombre repetido se declara", {
  datos <- data.frame(a = c(" x ", "Y", "z"), b = c(" p ", "q", "r"),
                      stringsAsFactors = FALSE)
  names(datos) <- c("a", "a")

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)
  sin_accion <- attr(plan, "hallazgos_sin_accion_por_columna_ambigua")

  expect_equal(sum(perfil$hallazgos$tipo_hallazgo == "espacios_sobrantes"), 2L)
  expect_false("recortar_espacios" %in% as.data.frame(plan)$estrategia)
  expect_equal(nrow(sin_accion), 2L)
  expect_true(all(sin_accion$hallazgo == "espacios_sobrantes"))
  expect_true(all(sin_accion$columna == "a"))
  expect_match(sin_accion$como_resolverlo[[1L]], "normalizar_nombres",
               fixed = TRUE)
  expect_message(print(plan), "no tienen acci")
})

test_that("con nombres distintos el plan no declara nada nuevo", {
  # Control: la declaracion aparece por la ambiguedad, no por la tabla.
  datos <- data.frame(a = c(" x ", "Y", "z"), b = c(" p ", "q", "r"),
                      stringsAsFactors = FALSE)

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)
  sin_accion <- attr(plan, "hallazgos_sin_accion_por_columna_ambigua")

  expect_equal(nrow(sin_accion), 0L)
  expect_equal(sum(as.data.frame(plan)$estrategia == "recortar_espacios"), 2L)
})
