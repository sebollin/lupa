# Ninguna funcion del paquete llama a `.Internal()`. R CMD check lo marca con un
# WARNING («Packages should not call .Internal()»), y la suite no lo veia: el
# `match()` propio del paquete empezo copiando `base::match` dentro del espacio
# de nombres, con su `.Internal(match(...))` adentro, y todas las pruebas
# pasaban. Se recorre el espacio de nombres entero, no una lista de nombres.

.llama_a_internal <- function(expresion) {
  if (is.call(expresion)) {
    if (identical(expresion[[1L]], as.name(".Internal"))) return(TRUE)
    for (parte in as.list(expresion)) {
      if (!missing(parte) && .llama_a_internal(parte)) return(TRUE)
    }
  } else if (is.function(expresion) && !is.primitive(expresion)) {
    return(.llama_a_internal(body(expresion)))
  }
  FALSE
}

test_that("el control ve un .Internal() donde lo hay", {
  # Sin esto, un recorrido que nunca entra en los cuerpos daria verde siempre.
  expect_true(.llama_a_internal(base::match))
  expect_false(.llama_a_internal(function(x) base::match(x, 1)))
})

test_that("ninguna funcion del paquete llama a .Internal()", {
  espacio <- asNamespace("lupa")
  nombres <- ls(espacio, all.names = TRUE)
  con_internal <- nombres[vapply(nombres, function(nombre) {
    objeto <- get(nombre, envir = espacio, inherits = FALSE)
    is.function(objeto) && .llama_a_internal(objeto)
  }, logical(1L))]
  expect_identical(con_internal, character())
})
