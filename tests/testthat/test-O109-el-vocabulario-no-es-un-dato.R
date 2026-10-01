# Tercera evaluacion real: el vocabulario del paquete no es un dato.
#
# La proteccion enmascara por contenido: una celda que, sin separadores y en
# mayusculas, contiene un valor protegido de seis caracteres o mas se tapa
# entera. Un nombre de persona contenido en `faltantes_disfrazados` borraba el
# tipo del hallazgo, y el plan se quedaba sin accion para el.

.datos_O109 <- function() {
  set.seed(4)
  n <- 400
  nombres <- c("Faltantes", "Luis", "Marta", "Jose", "Rosa", "Pedro", "Lucia")
  data.frame(
    id = seq_len(n),
    segundo_nombre = ifelse(stats::runif(n) < 0.6, NA, sample(nombres, n, TRUE)),
    segundo_apellido = ifelse(stats::runif(n) < 0.3, "", sample(nombres, n, TRUE)),
    stringsAsFactors = FALSE
  )
}

.hojas_texto_O109 <- function(x) {
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

test_that("un nombre protegido no borra el tipo del hallazgo ni la accion", {
  d <- .datos_O109()
  p <- suppressWarnings(perfilar(d))
  h <- hallazgos(p)
  # Antes: tres hallazgos con `tipo_hallazgo = "[valor protegido]"`.
  expect_false(any(h$tipo_hallazgo == "[valor protegido]"))
  expect_true(all(c("faltantes", "faltantes_disfrazados") %in% h$tipo_hallazgo))
  plan <- suppressWarnings(planificar_limpieza(p, datos = d))
  # Antes: el plan quedaba sin ninguna accion.
  expect_true("convertir_ausencias_textuales" %in% as.data.frame(plan)$estrategia)
  medidos <- h[as.character(h$severidad) != "ok", ]
  claves <- paste(medidos$columna, medidos$tipo_hallazgo)
  sin_accion <- attr(plan, "hallazgos_sin_accion")
  cubiertos <- c(
    paste(as.data.frame(plan)$columna, as.data.frame(plan)$hallazgo),
    paste(sin_accion$columna, sin_accion$hallazgo)
  )
  expect_true(all(claves %in% cubiertos))
})

test_that("el nombre protegido sigue sin publicarse", {
  d <- .datos_O109()
  p <- suppressWarnings(perfilar(d))
  plan <- suppressWarnings(planificar_limpieza(p, datos = d))
  a <- suppressWarnings(suppressMessages(analizar(d)))
  # El valor de la celda, tal como esta en los datos. El tipo `faltantes` -en
  # minusculas, del paquete- no es el dato de nadie.
  for (objeto in list(p, plan, a)) {
    expect_false(any(grepl("Faltantes", .hojas_texto_O109(objeto), fixed = TRUE)))
  }
  expect_false(any(grepl("Faltantes", utils::capture.output(print(p)),
                         fixed = TRUE)))
  # Control: sin proteccion, el valor si aparece -la sonda puede verlo-.
  abierto <- suppressWarnings(perfilar(d, proteger_datos_personales = FALSE))
  expect_true(any(grepl("Faltantes", .hojas_texto_O109(abierto), fixed = TRUE)))
})
