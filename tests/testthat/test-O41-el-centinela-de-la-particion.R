# O41: `perfilar_por()` promete que si un grupo vuelve a conjeturar un centinela
# POR LA PARTICION, el hallazgo se mueve a la cobertura y no se publica como
# defecto del grupo. El puente solo cubria las columnas de numeracion densa: en
# una columna comun, un grupo que conjeturaba `-999` mientras la columna entera
# conjetura `-9999` publicaba el hallazgo, con la cobertura vacia.

# La conjetura de centinelas necesita una columna entera con muchos distintos;
# con `integer64` es donde el caso aparece, y `bit64` ya esta en Suggests.
.datos_con_centinelas_por_grupo <- function() {
  set.seed(17)
  normales <- function(n) bit64::as.integer64(round(stats::rnorm(n, 10, 1) * 1000))
  data.frame(
    g = rep(c("a", "b", "c"), each = 1000L),
    x = c(
      rep(bit64::as.integer64(-9999), 60L), normales(940L),
      rep(bit64::as.integer64(-999), 60L), normales(940L),
      normales(1000L)
    )
  )
}

test_that("el centinela que sale de la particion se declara, no se publica", {
  skip_if_not_installed("bit64")
  datos <- .datos_con_centinelas_por_grupo()

  por_grupo <- perfilar_por(datos, "g", analizar_dependencias = FALSE,
                            proteger_datos_personales = FALSE)
  tabla <- as.data.frame(por_grupo)
  centinelas <- tabla[tabla$tipo_hallazgo == "posible_centinela_numerico", ,
                      drop = FALSE]
  cobertura <- as.data.frame(attr(por_grupo, "cobertura_diagnosticos"))
  movidos <- cobertura[cobertura$diagnostico == "centinelas_numericos", ,
                       drop = FALSE]

  # El grupo `b` conjetura `-999`, que la columna entera no conjetura.
  expect_false("b" %in% centinelas$grupo)
  expect_true("b" %in% movidos$grupo)
  expect_match(movidos$motivo[movidos$grupo == "b"][[1L]],
               "de la partici", fixed = TRUE)
})

test_that("el centinela que la columna entera si conjetura sigue publicandose", {
  skip_if_not_installed("bit64")
  # Control: lo que se mueve es lo que sale de la particion, no todo centinela
  # de un grupo. Sin esta mitad, callar todos pasaria la prueba anterior.
  datos <- .datos_con_centinelas_por_grupo()

  por_grupo <- perfilar_por(datos, "g", analizar_dependencias = FALSE,
                            proteger_datos_personales = FALSE)
  tabla <- as.data.frame(por_grupo)
  centinelas <- tabla[tabla$tipo_hallazgo == "posible_centinela_numerico", ,
                      drop = FALSE]

  expect_true("a" %in% centinelas$grupo)
  expect_match(centinelas$evidencia[centinelas$grupo == "a"][[1L]],
               "-9999", fixed = TRUE)
})
