# Ronda 18-A: el nombre de una persona en un texto libre de otra columna.

.hojas_O114 <- function(x) {
  hojas <- character()
  recorrer <- function(y) {
    for (a in setdiff(names(attributes(y)), c("names", "class", "row.names"))) {
      recorrer(attr(y, a, exact = TRUE))
    }
    if (is.factor(y)) hojas <<- c(hojas, levels(y))
    else if (is.character(y)) hojas <<- c(hojas, y)
    else if (is.list(y)) for (e in y) recorrer(e)
  }
  recorrer(x)
  hojas[!is.na(hojas)]
}

test_that("las variantes del nombre protegido no salen en otra columna", {
  datos <- data.frame(
    titular = rep(c("Juan P\u00e9rez", "Mar\u00eda N\u00fa\u00f1ez", "Luis Costa"), length.out = 30),
    observaciones = rep(c(
      "correo juanp\u00e9rezsrl@correo.uy informado",
      "usuario marianunez123@correo.uy",
      "mailto juan.perez@correo.uy",
      "cliente luiscostamayorista del rubro",
      "sin novedad"
    ), length.out = 30),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "titular"))
  todo <- paste(.hojas_O114(perfil), collapse = " || ")
  # Antes: las cuatro en los ejemplos de los patrones de `observaciones`.
  expect_false(grepl("juanp\u00e9rezsrl", todo, fixed = TRUE))
  expect_false(grepl("marianunez123", todo, fixed = TRUE))
  expect_false(grepl("juan.perez", todo, fixed = TRUE))
  expect_false(grepl("luiscostamayorista", todo, fixed = TRUE))
  # Control: lo que no nombra a nadie se publica.
  expect_true(grepl("sin novedad", todo, fixed = TRUE))
})
