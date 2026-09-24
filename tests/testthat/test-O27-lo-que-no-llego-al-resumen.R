# O27: dos cifras que se publicaban sin poder sostenerse. El denominador de los
# diagnosticos que salen del resumen cuantitativo contaba filas que el resumen
# nunca vio, y una accion recomendada que pierde el valor de la celda no lo
# decia en el unico texto que se lee antes de aplicarla.

test_that("outliers no cuenta como evaluadas las filas ausentes", {
  set.seed(270)
  valores <- stats::rnorm(1000)
  valores[3] <- 999
  valores[501:550] <- NA
  datos <- data.frame(id = seq_len(1000), num = valores)

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  fila <- perfil$columnas[perfil$columnas$columna == "num", , drop = FALSE]
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "outliers" &
      perfil$hallazgos$columna == "num", ,
    drop = FALSE
  ]

  expect_equal(fila$n_faltantes[[1L]], 50L)
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(hallazgo$n_evaluados[[1L]], 950)
  expect_equal(as.character(hallazgo$unidad_conteo[[1L]]), "fila")
})

test_that("sin ausentes el denominador sigue siendo la columna entera", {
  # Control: lo que cambia es lo que no llego al resumen, no el diagnostico.
  set.seed(271)
  valores <- stats::rnorm(1000)
  valores[3] <- 999
  datos <- data.frame(id = seq_len(1000), num = valores)

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "outliers" &
      perfil$hallazgos$columna == "num", ,
    drop = FALSE
  ]

  expect_equal(hallazgo$n_evaluados[[1L]], 1000)
})

test_that("los ceros y negativos no permitidos descuentan los ausentes", {
  # El test hermano de `test-alcance-sin-muestreo.R` mide el mismo denominador
  # con cien valores que no convierten. Esta es la otra mitad, la que nunca se
  # habia probado: cien ausentes y ningun valor no convertible.
  utiles <- c(rep("0", 50L), rep("-5", 50L), rep("100", 800L))
  datos <- data.frame(x = c(utiles, rep(NA_character_, 100L)),
                      stringsAsFactors = FALSE)

  perfil <- perfilar(datos, columnas_sin_ceros = "x",
                     columnas_no_negativas = "x", analizar_dependencias = FALSE)

  expect_equal(perfil$columnas$n, 1000L)
  expect_equal(perfil$columnas$n_faltantes, 100L)
  expect_equal(perfil$columnas$n_valores_excluidos_resumen, 0L)
  sobre_resumen <- c("outliers", "ceros_no_permitidos",
                     "negativos_no_permitidos")
  filas <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo %in% sobre_resumen, , drop = FALSE
  ]
  expect_true(nrow(filas) >= 1L)
  expect_true(all(filas$n_evaluados == 900))

  # Control: un diagnostico que cuenta FILAS conserva la columna entera.
  por_filas <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "filas_duplicadas", , drop = FALSE
  ]
  if (nrow(por_filas)) expect_true(all(por_filas$n_evaluados == 1000))
})

test_that("la accion que quita un caracter declara lo que el registro cuenta", {
  datos <- data.frame(
    x = rep(c("dato", paste0("otro", intToUtf8(1)), "tres", "cuatro"), 25),
    stringsAsFactors = FALSE
  )

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)
  fila <- as.data.frame(plan)
  fila <- fila[fila$estrategia == "eliminar_controles_invisibles", ,
               drop = FALSE]

  expect_equal(nrow(fila), 1L)
  expect_true(fila$aplicar[[1L]])
  expect_match(fila$justificacion[[1L]], "n_no_reversibles", fixed = TRUE)

  registro <- suppressMessages(aplicar(plan, datos))$registro
  ejecutada <- registro[
    registro$estrategia == "eliminar_controles_invisibles", ,
    drop = FALSE
  ]
  expect_true(ejecutada$n_no_reversibles[[1L]] > 0L)
})

test_that("recortar espacios no anuncia una perdida que no tiene", {
  # Control: la declaracion nueva es de la accion que pierde el valor, no de
  # todas. El registro de `recortar_espacios` cuenta cero irreversibles.
  datos <- data.frame(x = rep(c("dato ", " otro", "tres", "cuatro"), 25),
                      stringsAsFactors = FALSE)

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)
  fila <- as.data.frame(plan)
  fila <- fila[fila$estrategia == "recortar_espacios", , drop = FALSE]

  expect_false(grepl("n_no_reversibles", fila$justificacion[[1L]], fixed = TRUE))
  registro <- suppressMessages(aplicar(plan, datos))$registro
  ejecutada <- registro[registro$estrategia == "recortar_espacios", ,
                        drop = FALSE]
  expect_equal(ejecutada$n_no_reversibles[[1L]], 0)
})

test_that("los presentes no finitos cuentan como excluidos del resumen", {
  # Los dos README prometen que un resumen que deja afuera `NaN` o `Inf` lo
  # dice: los cuenta en `n_valores_excluidos_resumen` y el estado deja de decir
  # "calculados". Con 28 finitos, un `Inf` y un `-Inf` decia `calculados` y
  # `0` excluidos, con el minimo y la media ya calculados sin ellos.
  set.seed(272)
  datos <- data.frame(x = c(stats::rnorm(28), Inf, -Inf), id = seq_len(30L))

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  fila <- perfil$columnas[perfil$columnas$columna == "x", , drop = FALSE]

  expect_equal(as.character(fila$estado_resumen_cuantitativo[[1L]]),
               "calculados_sobre_valores")
  expect_equal(fila$n_valores_excluidos_resumen[[1L]], 2L)
  expect_equal(fila$n_infinito_positivo[[1L]], 1L)
  expect_equal(fila$n_infinito_negativo[[1L]], 1L)
  expect_true(is.finite(fila$minimo[[1L]]))

  # Y queda declarado donde el paquete declara lo que no midio, con el desglose.
  cobertura <- perfil$cobertura_diagnosticos
  cobertura <- cobertura[cobertura$diagnostico == "resumen_cuantitativo", ,
                         drop = FALSE]
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$motivo[[1L]], "no finitos", fixed = TRUE)
  expect_false(grepl("no pudo convertir", cobertura$motivo[[1L]], fixed = TRUE))
})

test_that("una columna sin un solo valor utilizable no dice calculados", {
  casos <- list(
    todo_inf = rep(Inf, 30L),
    todo_nan = rep(NaN, 30L),
    todo_na = rep(NA_real_, 30L)
  )
  for (nombre in names(casos)) {
    datos <- data.frame(x = casos[[nombre]], id = seq_len(30L))
    perfil <- perfilar(datos, analizar_dependencias = FALSE)
    fila <- perfil$columnas[perfil$columnas$columna == "x", , drop = FALSE]

    expect_equal(as.character(fila$estado_resumen_cuantitativo[[1L]]),
                 "sin_valores", info = nombre)
    expect_true(is.na(fila$minimo[[1L]]), info = nombre)
    expect_true(is.na(fila$media[[1L]]), info = nombre)
  }

  # Control: con valores utilizables el estado sigue siendo `calculados`.
  set.seed(273)
  datos <- data.frame(x = stats::rnorm(30L), id = seq_len(30L))
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  fila <- perfil$columnas[perfil$columnas$columna == "x", , drop = FALSE]
  expect_equal(as.character(fila$estado_resumen_cuantitativo[[1L]]),
               "calculados")
  expect_equal(fila$n_valores_excluidos_resumen[[1L]], 0L)
})

test_that("un centinela declarado sigue contandose como antes", {
  # Control de la cuenta nueva: lo que ya se excluia no cambia de numero.
  set.seed(274)
  datos <- data.frame(x = c(stats::rnorm(90), rep(-999, 10L)),
                      id = seq_len(100L))

  perfil <- perfilar(datos, analizar_dependencias = FALSE,
                     sentinelas_numericos = -999)
  fila <- perfil$columnas[perfil$columnas$columna == "x", , drop = FALSE]

  expect_equal(fila$n_valores_excluidos_resumen[[1L]], 10L)
  expect_equal(as.character(fila$estado_resumen_cuantitativo[[1L]]),
               "calculados_sobre_valores")
})
