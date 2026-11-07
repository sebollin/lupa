# Tres bordes de `columnas_duplicadas`.
#
# Nombres: la evidencia `A = B; comparadas...` se arma con los nombres sueltos.
# Con dos columnas llamadas `X` publicaba `X = X`, una igualdad que no dice de
# que columnas, y una columna llamada `A = B` igual a `C` se leia como un grupo
# de tres. Ahora, si algun nombre del grupo no se puede leer suelto, se citan
# todos y el que se repite lleva su posicion.
#
# Cero filas: dos columnas vacias coinciden sin que eso sea evidencia. El
# perfil no publicaba el hallazgo y declaraba en la cobertura que no las habia
# comparado, pero `general` las listaba como iguales.
#
# Tipos: la igualdad es la del detector, que solo iguala columnas del mismo
# tipo declarado. Si un grupo se separa porque una columna cambio de tipo, el
# detector no volvio a compararlas, y `comparar_perfiles()` publicaba
# `resuelto` con los mismos valores. Y el orden de los niveles de un factor sin
# orden, que el perfil no lee como contenido, separaba dos columnas iguales.

# Fuera de CRAN: el archivo arma unos treinta perfiles, por encima de los 0,2 s
# que se admiten en modo CRAN. Corre en la suite completa y en el CI, con
# NOT_CRAN=true.
testthat::skip_on_cran()

.nt_perfil <- function(datos, mes = 1L) {
  suppressWarnings(suppressMessages(perfilar(
    datos, fecha = as.POSIXct(sprintf("2026-%02d-01", mes), tz = "UTC"),
    analizar_dependencias = FALSE
  )))
}

.nt_evidencia <- function(datos) {
  h <- hallazgos(.nt_perfil(datos))
  h$evidencia[h$tipo_hallazgo == "columnas_duplicadas"]
}

.nt_con_nombres <- function(nombres, columnas) {
  d <- as.data.frame(columnas, stringsAsFactors = FALSE)
  names(d) <- nombres
  d
}

test_that("un nombre repetido lleva su posicion y uno con separadores va citado", {
  expect_identical(
    .nt_evidencia(.nt_con_nombres(c("X", "X", "Y"), list(1:3, 1:3, 4:6))),
    '"X" (columna 1) = "X" (columna 2); comparadas sobre 3 filas'
  )
  expect_identical(
    .nt_evidencia(.nt_con_nombres(c("X", "Y", "X"), list(1:3, 1:3, 1:3))),
    '"X" (columna 1) = "Y" = "X" (columna 3); comparadas sobre 3 filas'
  )
  expect_identical(
    .nt_evidencia(.nt_con_nombres(c("A = B", "C"), list(1:3, 1:3))),
    '"A = B" = "C"; comparadas sobre 3 filas'
  )
  expect_identical(
    .nt_evidencia(.nt_con_nombres(c("A; B", "C"), list(1:3, 1:3))),
    '"A; B" = "C"; comparadas sobre 3 filas'
  )
  expect_identical(
    .nt_evidencia(.nt_con_nombres(c("A, B", "C"), list(1:3, 1:3))),
    '"A, B" = "C"; comparadas sobre 3 filas'
  )
  expect_identical(
    .nt_evidencia(.nt_con_nombres(c("", ""), list(1:3, 1:3))),
    '"" (columna 1) = "" (columna 2); comparadas sobre 3 filas'
  )
  expect_identical(
    .nt_evidencia(.nt_con_nombres(c("A ", "B"), list(1:3, 1:3))),
    '"A " = "B"; comparadas sobre 3 filas'
  )
  # Control: con nombres que se leen sueltos, la evidencia de siempre.
  expect_identical(
    .nt_evidencia(data.frame(id = 1:3, copia = 1:3, otro = 4:6)),
    "id = copia; comparadas sobre 3 filas"
  )
  expect_identical(
    .nt_evidencia(data.frame(A = 1:3, B = 1:3, C = 1:3, D = 3:1)),
    "A = B = C; comparadas sobre 3 filas"
  )
})

test_that("sin filas, general no lista columnas que no se compararon", {
  d <- data.frame(A = integer(), B = integer(), C = character(), D = character(),
                  stringsAsFactors = FALSE)
  p <- .nt_perfil(d)
  expect_identical(nrow(p$general$columnas_duplicadas), 0L)
  expect_identical(names(p$general$columnas_duplicadas), c("columna_1", "columna_2"))
  expect_false(any(p$hallazgos$tipo_hallazgo == "columnas_duplicadas"))
  expect_true(any(p$cobertura_diagnosticos$diagnostico == "columnas_duplicadas"))
  # Control: con filas, aunque todas sean faltantes, la igualdad se publica en
  # los dos lados.
  p2 <- .nt_perfil(data.frame(A = c(NA, NA), B = c(NA, NA)))
  expect_identical(nrow(p2$general$columnas_duplicadas), 1L)
  expect_identical(sum(p2$hallazgos$tipo_hallazgo == "columnas_duplicadas"), 1L)
})

test_that("dos factores sin orden con los niveles en otro orden son la misma columna", {
  a <- factor(c("a", "b", "a"), levels = c("a", "b"))
  b <- factor(c("a", "b", "a"), levels = c("b", "a"))
  expect_true(lupa:::.columnas_identicas(a, b))
  # Un nivel de mas sin usar sigue siendo otra columna, y en un factor
  # ordenado el orden de los niveles es el de los valores.
  expect_false(lupa:::.columnas_identicas(
    a, factor(c("a", "b", "a"), levels = c("a", "b", "c"))
  ))
  expect_false(lupa:::.columnas_identicas(
    factor(c("a", "b", "a"), levels = c("a", "b"), ordered = TRUE),
    factor(c("a", "b", "a"), levels = c("b", "a"), ordered = TRUE)
  ))
  # Tampoco un entero y un doble: el perfil los declara de tipos distintos.
  expect_false(lupa:::.columnas_identicas(1:3, c(1, 2, 3)))

  d <- data.frame(A = a, B = b)
  p <- .nt_perfil(d)
  expect_identical(p$general$columnas_duplicadas$columna_2, "B")
  plan <- planificar_limpieza(p, d)
  destructivo <- plan
  destructivo$aplicar <- destructivo$estrategia == "eliminar_columna_duplicada"
  r <- aplicar(destructivo, d, permitir_eliminacion = TRUE)
  expect_identical(r$registro$estado, "ejecutada")
  expect_identical(names(r$datos), "A")
  # Y la deriva no publica nada: el grupo sigue.
  anterior <- .nt_perfil(data.frame(A = a, B = a), 1L)
  dv <- as.data.frame(comparar_perfiles(anterior, .nt_perfil(d, 2L)))
  expect_false(any(dv$valor_anterior %in% "columnas_duplicadas"))
})

.nt_deriva <- function(a, b) {
  d <- as.data.frame(suppressMessages(comparar_perfiles(.nt_perfil(a, 1L), .nt_perfil(b, 2L))))
  d[d$aspecto == "hallazgo" &
      (d$valor_anterior %in% "columnas_duplicadas" |
         d$valor_actual %in% "columnas_duplicadas"),
    c("columna", "cambio", "severidad", "descripcion", "evidencia"), drop = FALSE]
}

.nt_dice <- function(texto, parte) grepl(parte, texto, fixed = TRUE, useBytes = TRUE)

test_that("un grupo que se separa solo por un cambio de tipo no se publica resuelto", {
  d <- .nt_deriva(data.frame(A = 1:3, B = 1:3), data.frame(A = 1:3, B = c(1, 2, 3)))
  expect_identical(nrow(d), 1L)
  expect_identical(d$cambio, "no_evaluado")
  expect_identical(as.character(d$severidad), "sospechoso")
  expect_true(.nt_dice(d$descripcion, "`B` (entero a doble)"))

  d <- .nt_deriva(data.frame(A = c(TRUE, FALSE, TRUE), B = c(TRUE, FALSE, TRUE)),
                  data.frame(A = c(TRUE, FALSE, TRUE), B = c(1L, 0L, 1L)))
  expect_identical(d$cambio, "no_evaluado")
  expect_true(.nt_dice(d$descripcion, "`B` (logico a entero)"))

  # La igualdad de A y B sigue; C solo cambio de tipo.
  d <- .nt_deriva(data.frame(A = 1:3, B = 1:3, C = 1:3),
                  data.frame(A = 1:3, B = 1:3, C = c(1, 2, 3)))
  expect_identical(d$cambio, "no_evaluado")
  expect_true(.nt_dice(d$descripcion, "`C` (entero a doble)"))
  expect_false(.nt_dice(d$descripcion, "`B`"))

  # Controles: si cambian los valores, o el perfil nuevo ya los muestra
  # distintos, o una columna del mismo tipo se separa, es `resuelto`.
  expect_identical(
    .nt_deriva(data.frame(A = 1:3, B = 1:3), data.frame(A = 1:3, B = 3:1))$cambio,
    "resuelto"
  )
  expect_identical(
    .nt_deriva(data.frame(A = 1:3, B = 1:3), data.frame(A = 1:3, B = c(9, 8, 7)))$cambio,
    "resuelto"
  )
  expect_identical(
    .nt_deriva(data.frame(A = 1:3, B = 1:3, C = 1:3),
               data.frame(A = 1:3, B = 3:1, C = c(1, 2, 3)))$cambio,
    "resuelto"
  )
  # Las dos cambian de tipo juntas: el grupo sigue.
  expect_identical(
    nrow(.nt_deriva(data.frame(A = 1:3, B = 1:3), data.frame(A = c(1, 2, 3), B = c(1, 2, 3)))),
    0L
  )
})

test_that("un grupo que aparece solo por un cambio de tipo lo dice", {
  d <- .nt_deriva(data.frame(A = 1:3, B = c(1, 2, 3)), data.frame(A = 1:3, B = 1:3))
  expect_identical(d$cambio, "aparecido")
  expect_true(.nt_dice(d$descripcion, "`B` (doble a entero)"))
  # Control: una columna nueva igual a otra es una igualdad nueva, sin esa causa.
  d <- .nt_deriva(data.frame(A = 1:3), data.frame(A = 1:3, F = 1:3))
  expect_identical(d$cambio, "aparecido")
  expect_false(.nt_dice(d$descripcion, "tipo declarado"))
})
