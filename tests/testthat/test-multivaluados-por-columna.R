# La deteccion de valores multiples mira la columna entera en cada pasada, no
# celda por celda: cada llamada chica a una expresion regular tiene un costo fijo
# que en una columna de cientos de miles de celdas con coma era casi todo el
# perfil. Y lo que responde tiene que ser lo que respondia celda por celda.

# Cuantas veces se llama a `grepl(perl = TRUE)` con un solo valor mientras corre
# `codigo`.
.grepl_chicas_multivaluados <- function(codigo) {
  registro <- new.env()
  registro$n <- 0L
  suppressMessages(trace(
    "grepl", where = asNamespace("base"), print = FALSE,
    tracer = bquote(if (isTRUE(perl) && length(x) == 1L) {
      assign("n", get("n", envir = .(registro)) + 1L, envir = .(registro))
    })
  ))
  on.exit(suppressMessages(untrace("grepl", where = asNamespace("base"))),
          add = TRUE)
  valor <- codigo
  list(valor = valor, n = registro$n)
}

# Una columna con decimales de coma (que no son listas), listas de enteros con
# coma y con punto y coma, enteros sueltos, vacios y faltantes, y un valor
# acentuado. `veces` la repite entera.
.columna_multivaluados <- function(veces) {
  bloque <- c("1,5", "12,75", "12, 15, 18", "12, 15", "13", "14", "15",
              "16; 17", "16; 17; 18", "", NA, "17", "18", "19", "20", "21",
              paste0("A", intToUtf8(0xf1L), "o"))
  rep(bloque, veces)
}

test_that("la deteccion de valores multiples no pregunta celda por celda", {
  skip_on_cran()
  chica <- .grepl_chicas_multivaluados(.detectar_multivaluados(
    .columna_multivaluados(2L)
  ))
  grande <- .grepl_chicas_multivaluados(.detectar_multivaluados(
    .columna_multivaluados(200L)
  ))
  # Lo que se detecta escala con la columna...
  expect_identical(chica$valor[[1L]]$delimitador, ", / ;")
  expect_identical(grande$valor[[1L]]$delimitador, ", / ;")
  expect_identical(grande$valor[[1L]]$n_celdas, 800L)
  expect_identical(
    grande$valor[[1L]]$valores_por_celda, c(`2` = 400L, `3` = 400L)
  )
  # ...y las llamadas chicas no: eran una por celda con coma y varias por cada
  # parte, mas de 18.000 con 200 repeticiones contra menos de 200 con dos.
  expect_identical(grande$n, chica$n)
  expect_lte(grande$n, 10L)
})

test_that("el tipo de cada parte no depende de las partes que la acompanan", {
  skip_on_cran()
  latin1 <- rawToChar(as.raw(c(0xd1, 0x31)))
  Encoding(latin1) <- "latin1"
  partes <- c("12", "+12", "A1", "1A", "AB", "1234567-8", "2023.15", "12/3",
              "1-2", "", "1 2", paste0(intToUtf8(0xd1L), "1"), latin1,
              "12345678901")
  esperado <- c("numerico", "numerico", "alfanumerico", "alfanumerico",
                "texto", "identificador", "identificador", "texto", "texto",
                "texto", "texto", "texto", "texto", "numerico")
  expect_identical(.tipo_parte_multivaluada(partes), esperado)
  solas <- vapply(partes, .tipo_parte_multivaluada, character(1L),
                  USE.NAMES = FALSE)
  expect_identical(solas, esperado)
  expect_identical(.tipo_parte_multivaluada(character()), character())
})

test_that("cada descarte de la deteccion de valores multiples, con su caso", {
  skip_on_cran()
  # Todas las celdas con coma son numeros regionales: no hay listas.
  expect_identical(
    .detectar_multivaluados(c("1,5", "2,25", "1.234,5", "$ 3,5", "7")),
    list()
  )
  # Los numeros con coma se excluyen y la lista con punto y coma queda.
  con_punto_y_coma <- .detectar_multivaluados(
    c("1,5", "2,25", "10; 11", "12", "13", "14")
  )
  expect_identical(con_punto_y_coma[[1L]]$delimitador, ";")
  expect_identical(con_punto_y_coma[[1L]]$indices, 3L)
  # Mas de la mitad de las celdas presentes con delimitador: es la forma de la
  # columna, no una lista dentro de ella.
  expect_identical(
    .detectar_multivaluados(c("10, 11", "12, 13", "14, 15", "16", NA, "")),
    list()
  )
  # Exactamente la mitad todavia cuenta.
  mitad <- .detectar_multivaluados(c("10, 11", "12, 13", "14", "16"))
  expect_identical(mitad[[1L]]$indices, c(1L, 2L))
  # Ninguna celda suelta del mismo tipo que las partes: no hay lista.
  expect_identical(
    .detectar_multivaluados(c("10, 11", "x", "y", "z")),
    list()
  )
  # Sin celdas sueltas, la homogeneidad de las partes alcanza.
  solas_listas <- .detectar_multivaluados(
    c("A1;B2", "C3;D4", "E5|F6", "G7|H8", "", NA)
  )
  expect_identical(solas_listas[[1L]]$delimitador, "; / |")
  expect_identical(solas_listas[[1L]]$n_valores, 8)
  # Una parte vacia invalida la celda; un delimitador final no.
  bordes <- .detectar_multivaluados(c("10,,11", "12,13,", "14", "15", "16"))
  expect_identical(bordes[[1L]]$indices, 2L)
})

test_that("listas con dos delimitadores y largos distintos no abortan el perfil", {
  # Las listas separadas por `,` tienen tres valores y las separadas por `;`,
  # dos: ningun largo es comun a los dos delimitadores. Sumar las dos
  # distribuciones le pedia a cada una el largo que no tenia, y eso abortaba
  # `perfilar()` sobre la tabla entera.
  x <- c("12, 15, 18", "13", "14", "15", "16; 17", "19", "20", "21")
  deteccion <- .detectar_multivaluados(x)
  expect_length(deteccion, 1L)
  expect_identical(deteccion[[1L]]$delimitador, ", / ;")
  expect_identical(deteccion[[1L]]$indices, c(1L, 5L))
  expect_identical(deteccion[[1L]]$valores_por_celda, c(`2` = 1L, `3` = 1L))
  expect_identical(deteccion[[1L]]$n_valores, 5)

  perfil <- perfilar(data.frame(x = x, y = seq_along(x)), fecha = "2026-01-15")
  multivaluadas <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "celdas_multivaluadas", ,
    drop = FALSE
  ]
  expect_identical(multivaluadas$columna, "x")
})
