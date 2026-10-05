# Tres conteos de la tabla de hallazgos que no describian lo que decian.
#
# El invariante de esta tabla es que `n_evaluados` y `n_afectados` estan en la unidad
# que `unidad_conteo` declara, y que `n_afectados` nunca puede ser mayor.

test_that("el conteo de fecha partida no sale de partir su propia evidencia", {
  # El conteo se derivaba partiendo el TEXTO de la evidencia por `+`, asi que un
  # nombre de columna con un `+` -legal en R y en cualquier base- publicaba
  # `n_afectados = 4` sobre una tabla de TRES columnas: `n_afectados > n_evaluados`,
  # que es imposible con un conteo honesto en la unidad `columna`.
  datos <- data.frame(
    a = c(2020, 2021, 2022), b = c(1, 2, 3), c = c(4, 5, 6)
  )
  names(datos) <- c("anio+bis", "mes", "dia")

  fila <- hallazgos(perfilar(datos, analizar_dependencias = FALSE))
  fila <- fila[fila$tipo_hallazgo == "fecha_partida_columnas", , drop = FALSE]

  expect_identical(nrow(fila), 1L)
  expect_identical(as.character(fila$unidad_conteo), "columna")
  expect_identical(as.numeric(fila$n_evaluados), 3)
  expect_identical(as.numeric(fila$n_afectados), 3)
  expect_lte(fila$n_afectados[[1L]], fila$n_evaluados[[1L]])
})

test_that("un nombre con parentesis no funde dos columnas en el conteo", {
  # La otra punta del mismo defecto: `sub("\\s*\\(.*$", "", ...)` recortaba
  # `anio (provisorio)` a `anio`, `unique()` lo fundia con la columna `anio` real y
  # el conteo SUBcontaba.
  datos <- data.frame(
    a = c(2020, 2021, 2020), b = c(2019, 2022, 2021),
    c = c(1, 2, 3), d = c(4, 5, 6)
  )
  names(datos) <- c("anio (provisorio)", "anio", "mes", "dia")

  fila <- hallazgos(perfilar(datos, analizar_dependencias = FALSE))
  fila <- fila[fila$tipo_hallazgo == "fecha_partida_columnas", , drop = FALSE]

  expect_identical(nrow(fila), 1L)
  expect_identical(as.numeric(fila$n_afectados), 4)
  expect_lte(fila$n_afectados[[1L]], fila$n_evaluados[[1L]])
})

test_that("un nombre corriente sigue contando lo mismo que antes", {
  # Mitad de control: el conteo por posiciones tiene que dar lo mismo que el viejo
  # donde el viejo andaba, o el arreglo cambio la cifra publicada sin decirlo.
  datos <- data.frame(
    anio = c(2020, 2021, 2022), mes = c(1, 2, 3), dia = c(4, 5, 6)
  )

  fila <- hallazgos(perfilar(datos, analizar_dependencias = FALSE))
  fila <- fila[fila$tipo_hallazgo == "fecha_partida_columnas", , drop = FALSE]

  expect_identical(as.numeric(fila$n_afectados), 3)
  expect_identical(as.numeric(fila$n_evaluados), 3)
})

test_that("sin patron dominante suficiente, la ausencia se declara", {
  # El README y una vinieta prometen que si ningun patron dominante alcanza el
  # umbral, eso queda en `cobertura_diagnosticos`. Con veinte valores en cinco
  # patrones y el mayor en 0,25 contra un umbral de 0,5, la salida no tenia ni
  # hallazgo ni cobertura: la condicion exigia mas de una fila en el resumen de
  # patrones, y el resumen que llega trae una.
  datos <- data.frame(
    x = c("A1", "A2", "A3", "A4", "A5", "12", "13", "14", "15",
          "AAA", "AAB", "AAC", "AAD", "b1", "b2", "b3",
          "c-a", "c-b", "c-c", "c-d"),
    stringsAsFactors = FALSE
  )

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  declaracion <- cobertura(perfil)
  declaracion <- declaracion[
    declaracion$diagnostico == "patron_raro", , drop = FALSE
  ]

  expect_identical(nrow(declaracion), 1L)
  expect_match(as.character(declaracion$motivo), "patron dominante ocupa")
  expect_false("patron_raro" %in% hallazgos(perfil)$tipo_hallazgo)
})

test_that("con un patron dominante claro no se declara nada de mas", {
  # Mitad de control: la declaracion aparece SOLO cuando el dominante no alcanza.
  datos <- data.frame(
    x = c(rep("AB1234", 18), "zz", "yy"), stringsAsFactors = FALSE
  )

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  declaracion <- cobertura(perfil)

  expect_false(any(
    declaracion$diagnostico == "patron_raro" &
      grepl("patron dominante ocupa", declaracion$motivo, fixed = TRUE)
  ))
})

test_that("el centinela declara el mismo universo que sus hermanos", {
  # Sobre la misma columna y en la misma corrida, `ceros_no_permitidos` y `outliers`
  # publicaban `n_evaluados = 26` -la formula del propio paquete: 28 menos una
  # ausente menos una no convertible- y `posible_centinela_numerico` publicaba 28,
  # contando como evaluadas dos filas donde su deteccion no miro nada.
  datos <- data.frame(edad = c(
    as.character(35:54), "0", "8888", "8888", "8888", "8888", "8888",
    "guarro", NA
  ), stringsAsFactors = FALSE)

  fila <- hallazgos(perfilar(
    datos, analizar_dependencias = FALSE,
    columnas_sin_ceros = "edad", columnas_no_negativas = "edad"
  ))
  universo <- function(tipo) {
    as.numeric(fila$n_evaluados[fila$tipo_hallazgo == tipo])
  }

  expect_identical(universo("posible_centinela_numerico"), 26)
  expect_identical(universo("ceros_no_permitidos"), 26)
  expect_identical(universo("outliers"), 26)
  # Y los que miran la columna entera siguen en 28: la agenda no es igualar todo,
  # es que cada uno declare el universo donde de verdad miro.
  #
  # `patron_raro` NO mira la columna entera, y aca decia 28. La entrada de NEWS
  # que fijo este valor agrupaba tres diagnosticos -`patron_raro`, `faltantes`,
  # `filas_duplicadas`- como "su universo real", y para `patron_raro` la premisa
  # era falsa: la fila `NA` no tiene patron, sus proporciones se calculan sobre
  # las filas con valor y su traza no la mira. Su universo son las 27 filas con
  # valor (ronda 24-C). `faltantes` y `filas_duplicadas` si miran la columna
  # entera y siguen en 28.
  expect_identical(universo("patron_raro"), 27)
})

test_that("sin valores excluidos, el centinela cuenta la columna entera", {
  # Mitad de control: si no hay nada que no llegue al resumen, el denominador del
  # centinela es la columna, y el arreglo no puede restarle nada.
  datos <- data.frame(
    edad = c(as.character(35:54), "8888", "8888", "8888", "8888", "8888"),
    stringsAsFactors = FALSE
  )

  fila <- hallazgos(perfilar(datos, analizar_dependencias = FALSE))
  centinela <- fila[fila$tipo_hallazgo == "posible_centinela_numerico", , drop = FALSE]

  expect_identical(nrow(centinela), 1L)
  expect_identical(as.numeric(centinela$n_evaluados), 25)
})
