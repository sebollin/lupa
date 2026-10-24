# La proximidad de vocabulario se calcula con `stringdist`, que es opcional.
# Sin el paquete el detector no corre y estas comprobaciones miran una salida
# que no existe: hay que saltearlas, no dejarlas fallar.
test_that("el alcance declara las variantes cercanas sin forma dominante", {
  skip_if_not_installed("stringdist")
  resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
    c(rep("Montevideo", 5L), rep("Montevido", 5L)),
    lupa:::.resolver_normalizacion(TRUE), "departamento"
  )

  expect_length(resultado$grupos, 0L)
  expect_equal(resultado$alcance$n_grupos_sin_variante_rara, 1L)
  expect_equal(resultado$alcance$n_pares_equifrecuentes, 1L)

  perfil <- perfilar(
    data.frame(departamento = c(
      rep("Montevideo", 5L), rep("Montevido", 5L)
    )),
    analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "proximidad_vocabulario" &
      perfil$cobertura_diagnosticos$columna == "departamento", ,
    drop = FALSE
  ]
  expect_true(any(grepl("nunca pudo distinguir|frecuencias fueron parecidas",
                       cobertura$motivo)))
})

test_that("la variante rara dominante conserva su deteccion calibrada", {
  skip_if_not_installed("stringdist")
  perfil <- perfilar(
    data.frame(departamento = c(
      rep("Montevideo", 40L), rep("Montevido", 5L)
    )),
    analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(as.character(hallazgo$severidad), "sospechoso")
  expect_equal(hallazgo$n_afectados, 2L)
})

test_that("las formas equifrecuentes tienen un diagnostico separado", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    departamento = c(rep("Montevideo", 5L), rep("Montevido", 5L))
  )
  hallazgos <- lupa:::.hallazgos_casi_duplicados_vocabulario(
    datos, names(datos), lupa:::.resolver_normalizacion(TRUE),
    detectar_variantes_equifrecuentes = TRUE
  )
  resultado <- do.call(rbind, hallazgos)
  nuevo <- resultado[
    resultado$tipo_hallazgo == "variantes_equifrecuentes_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(nuevo), 1L)
  expect_equal(as.character(nuevo$severidad), "sospechoso")
  expect_match(nuevo$descripcion, "ninguna evidencia dice cual es la correcta")
  expect_match(nuevo$evidencia, "origen=distancia_equifrecuente")
  expect_equal(nuevo$n_afectados, 2L)
})

test_that("patron_raro declara cuando el desvio solo cambia el largo numerico", {
  perfil <- perfilar(
    data.frame(correo = paste0("persona", seq_len(300L), "@x.uy")),
    analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "patron_raro", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(
    hallazgo$evidencia,
    "desvio_unicamente_largo_corrida_numerica=TRUE",
    fixed = TRUE
  )
})

test_that("la busqueda de fechas partidas respeta el presupuesto", {
  nombres <- c(
    paste0("anio_", seq_len(7L)), paste0("mes_", seq_len(7L)),
    paste0("dia_", seq_len(6L))
  )
  datos <- as.data.frame(setNames(
    replicate(length(nombres), c(2020L, 2021L), simplify = FALSE), nombres
  ))
  resultado <- lupa:::.detectar_fecha_partida(
    datos, names(datos), max_candidatos = 10L
  )
  expect_true(resultado$alcance$truncado)
  expect_equal(resultado$alcance$n_candidatos_evaluados, 10L)
  expect_gt(resultado$alcance$n_candidatos_sin_comparar, 0L)
})

test_that("las dependencias declaran los pares que el presupuesto omite", {
  datos <- data.frame(
    a = rep(1:5, each = 20L), b = rep(letters[1:5], each = 20L),
    c = rep(LETTERS[1:5], each = 20L), d = rep(1:5, 20L)
  )
  resultado <- detectar_dependencias(
    datos, muestra = Inf, max_comparaciones = 2L
  )
  expect_true(isTRUE(attr(resultado, "presupuesto_agotado")))
  expect_equal(attr(resultado, "n_pares_comparados"), 2)
  expect_gt(attr(resultado, "n_pares_sin_comparar"), 0)
  cobertura <- lupa:::.cobertura_dependencias(resultado)
  expect_equal(nrow(cobertura), 1L)
  # El motivo declara las dos unidades del presupuesto, no solo los pares: el
  # tope efectivo puede venir de `max_comparaciones` o de `max_trabajo`, y
  # decir cuantos pares quedaron sin decir cuanto trabajo era no permite
  # elegir cual aflojar.
  expect_match(cobertura$motivo, "quedaron 10 pares", fixed = TRUE)
  expect_match(cobertura$motivo, "unidades sin comparar", fixed = TRUE)
  expect_equal(attr(resultado, "unidad_trabajo"), "fila-par")
  expect_equal(
    attr(resultado, "trabajo_comparado") + attr(resultado, "trabajo_sin_comparar"),
    attr(resultado, "trabajo_estimado")
  )
})

test_that("la razon de permutacion queda como evidencia descriptiva", {
  inicio <- seq(100, 399)
  fin <- inicio + 10
  fin[seq_len(6L)] <- inicio[seq_len(6L)] - 1
  perfil <- perfilar(
    data.frame(inicio = inicio, fin = fin),
    analizar_dependencias = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "relacion_orden_columnas", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(hallazgo$evidencia, "Razon de permutacion descriptiva", fixed = TRUE)
  expect_match(hallazgo$evidencia, "no filtra la deteccion", fixed = TRUE)
})
