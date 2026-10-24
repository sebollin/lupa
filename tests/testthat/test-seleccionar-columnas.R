.seleccion_columnas_fixture <- function() {
  acentuada <- intToUtf8(0xE1)
  salida <- data.frame(
    `a b` = 11:13,
    i = 21:23,
    j = 31:33,
    drop = 41:43,
    acentuada = 51:53,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(salida)[[5L]] <- acentuada
  salida
}

.seleccion_columnas_referencia <- function(tabla, columnas, filas = NULL) {
  tabla <- as.data.frame(tabla, stringsAsFactors = FALSE)
  if (is.null(filas)) {
    tabla[, columnas, drop = FALSE]
  } else {
    tabla[filas, columnas, drop = FALSE]
  }
}

.seleccion_columnas_convertir <- function(tabla, tipo) {
  if (identical(tipo, "data.frame")) return(as.data.frame(tabla))
  if (identical(tipo, "data.table")) return(data.table::as.data.table(tabla))
  tibble::as_tibble(tabla)
}

test_that("la seleccion explicita coincide con data.frame", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("tibble")
  base <- .seleccion_columnas_fixture()
  acentuada <- names(base)[[5L]]
  casos <- list(
    una = "drop",
    varias = c("a b", acentuada, "i", "j"),
    ninguna = character(),
    repetidas = c("j", "j", "a b")
  )
  tablas <- lapply(
    c("data.frame", "data.table", "tibble"),
    function(tipo) .seleccion_columnas_convertir(base, tipo)
  )
  names(tablas) <- c("data.frame", "data.table", "tibble")
  for (tipo in names(tablas)) {
    for (nombre in names(casos)) {
      esperado <- .seleccion_columnas_referencia(tablas[[tipo]], casos[[nombre]])
      resultado <- lupa:::.seleccionar_columnas(tablas[[tipo]], casos[[nombre]])
      expect_identical(resultado, esperado, info = paste(tipo, nombre))
      expect_identical(class(resultado), "data.frame")
    }
  }
  for (tipo in names(tablas)) {
    expect_error(
      lupa:::.seleccionar_columnas(tablas[[tipo]], "no existe"),
      "undefined columns selected",
      info = tipo
    )
  }
})

test_that("la seleccion puede fijar filas sin cambiar la semantica", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("tibble")
  base <- .seleccion_columnas_fixture()
  for (tipo in c("data.frame", "data.table", "tibble")) {
    tabla <- .seleccion_columnas_convertir(base, tipo)
    esperado <- .seleccion_columnas_referencia(tabla, c("i", "drop"), c(3L, 1L))
    resultado <- lupa:::.seleccionar_columnas(
      tabla, c("i", "drop"), filas = c(3L, 1L)
    )
    expect_identical(resultado, esperado, info = tipo)
  }
})

test_that("la primitiva no vuelve a convertir un data.table", {
  skip_if_not_installed("data.table")
  tabla <- data.table::data.table(a = 1:3, b = 4:6)
  conversiones <- 0L
  trace(
    "as.data.frame.data.table", where = asNamespace("data.table"),
    tracer = quote(conversiones <<- conversiones + 1L), print = FALSE
  )
  on.exit(
    untrace("as.data.frame.data.table", where = asNamespace("data.table")),
    add = TRUE
  )
  lupa:::.seleccionar_columnas(tabla, "a")
  expect_identical(conversiones, 0L)
})
