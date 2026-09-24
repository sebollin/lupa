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
