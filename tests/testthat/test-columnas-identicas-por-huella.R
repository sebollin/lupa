# `.pares_de_columnas_identicas()` comparaba todos los pares: con 1.000 columnas
# eran 499.500 llamadas a `.columnas_identicas()`, y cada una normalizaba las dos
# columnas otra vez (unos 10 s, iguales o distintas). Ahora agrupa por una huella
# y compara contra un representante por clase. Estas pruebas fijan las dos
# cosas que eso promete: la salida es la de comparar todos los pares -los
# mismos, en el mismo orden-, y cada columna se normaliza una sola vez. El
# perfil guarda una fila por columna repetida, junto a la primera de su clase
# (`.columnas_duplicadas()`); `detectar_claves()` sigue recibiendo todos los
# pares (`.pares_redundantes()`). Las dos salen de la misma definicion.

# Fuera de CRAN: el archivo tarda unos 0,3 s (medido el 2026-11-06), por encima
# de los 0,2 s que se admiten en modo CRAN. Corre en la suite completa y en el
# CI, con NOT_CRAN=true.
testthat::skip_on_cran()

# La referencia es la definicion: todos los pares de `combn()`, comparados uno
# por uno con la regla del paquete.
# Con `primera = TRUE`, solo los pares cuya primera columna no es la segunda de
# ningun otro: con una equivalencia, cada repetida junto a la primera de su
# clase. Se decide por posicion, porque dos columnas pueden llamarse igual.
.pares_por_definicion <- function(datos, indices, nombres, primera = FALSE) {
  if (length(indices) < 2L) {
    return(data.frame(columna_1 = character(), columna_2 = character(),
                      stringsAsFactors = FALSE))
  }
  pares <- utils::combn(indices, 2L)
  iguales <- apply(pares, 2L, function(p) {
    lupa:::.columnas_identicas(datos[[p[[1L]]]], datos[[p[[2L]]]])
  })
  pares <- pares[, iguales, drop = FALSE]
  if (primera) pares <- pares[, !pares[1L, ] %in% pares[2L, ], drop = FALSE]
  data.frame(columna_1 = nombres[pares[1L, ]], columna_2 = nombres[pares[2L, ]],
             stringsAsFactors = FALSE)
}

.comprobar_como_definicion <- function(datos) {
  nombres <- names(datos)
  esperado <- .pares_por_definicion(datos, seq_along(datos), nombres)
  obtenido <- lupa:::.pares_de_columnas_identicas(datos, seq_along(datos), nombres)
  expect_identical(obtenido$columna_1, esperado$columna_1)
  expect_identical(obtenido$columna_2, esperado$columna_2)
  n_pares <- nrow(esperado)
  esperado <- .pares_por_definicion(datos, seq_along(datos), nombres,
                                    primera = TRUE)
  obtenido <- lupa:::.columnas_duplicadas(datos, nombres)
  expect_identical(obtenido$columna_1, esperado$columna_1)
  expect_identical(obtenido$columna_2, esperado$columna_2)
  # Indices desordenados y con una columna repetida, como puede llegar un
  # subconjunto de claves: el orden de los pares es el de `combn()` sobre ese
  # vector, no el de las columnas.
  indices <- c(rev(seq_along(datos)), 1L)
  esperado <- .pares_por_definicion(datos, indices, nombres)
  obtenido <- lupa:::.pares_redundantes(datos, indices, nombres)
  expect_identical(obtenido$columna_1, esperado$columna_1)
  expect_identical(obtenido$columna_2, esperado$columna_2)
  invisible(n_pares)
}

test_that("los pares de columnas iguales son los de comparar todos los pares", {
  n <- 40L
  v <- rep(c("si", "no", NA, "si"), length.out = n)
  casi <- v
  casi[[21L]] <- "no"
  x <- seq_len(n) / 7
  cero <- c(0, x[-1L])
  menos_cero <- c(-0, x[-1L])
  con_nan <- x
  con_nan[[5L]] <- NaN
  con_na <- x
  con_na[[5L]] <- NA
  f <- factor(v)
  instante <- as.POSIXct("2020-01-01", tz = "UTC") + seq_len(n) * 3600
  datos <- data.frame(
    a = v, b = v, casi = casi, f1 = f, f2 = f, fna = addNA(f),
    entero = seq_len(n), doble = as.numeric(seq_len(n)), x1 = x, x2 = x,
    cero = cero, menos_cero = menos_cero, nan = con_nan, na = con_na,
    na2 = con_na, inst = instante, stringsAsFactors = FALSE
  )
  datos$lt <- as.POSIXlt(instante)
  datos$fecha <- as.Date("2020-01-01") + seq_len(n)
  datos$fecha2 <- datos$fecha
  names(datos)[[2L]] <- "a"
  pares <- .comprobar_como_definicion(datos)
  # Control de que la tabla ejerce lo que dice: hay pares por normalizacion
  # (factor con nivel `NA`, `POSIXlt`), por `0` y `-0`, y no por `NA` y `NaN`.
  obtenido <- lupa:::.columnas_duplicadas(datos, names(datos))
  claves <- paste(obtenido$columna_1, obtenido$columna_2)
  expect_true(all(c("f1 fna", "inst lt", "cero menos_cero", "na na2") %in% claves))
  expect_false("nan na" %in% claves)
  expect_gt(pares, 5L)

  vacias <- data.frame(a = rep(NA_character_, 5L), b = rep(NA_character_, 5L),
                       c = rep(NA_real_, 5L), d = rep(NA, 5L))
  .comprobar_como_definicion(vacias)
  .comprobar_como_definicion(data.frame(a = character(), b = character(),
                                        c = numeric()))
  .comprobar_como_definicion(data.frame(p = "x", q = "x", r = "y"))
})

test_that("columnas que la huella confunde se separan al compararlas", {
  # Mismo tipo, largo, faltantes y extremos: difieren en una posicion interior
  # cada una, y tres se repiten. La huella las junta; la comparacion decide.
  base <- sprintf("v%03d", seq_len(120L))
  columnas <- lapply(seq_len(30L), function(k) {
    y <- base
    y[[k + 40L]] <- "otro"
    y
  })
  columnas <- c(columnas, columnas[c(3L, 9L, 3L)])
  datos <- as.data.frame(stats::setNames(columnas, paste0("c", seq_along(columnas))),
                         stringsAsFactors = FALSE)
  pares <- .comprobar_como_definicion(datos)
  expect_equal(pares, 4L)
})

test_that("los textos se comparan como identical() en cualquier codificacion", {
  # `identical()` iguala `latin1` y UTF-8 del mismo texto en un locale UTF-8 y
  # no bajo `C`: la huella no puede separar por bytes lo que la comparacion
  # junta, y la salida tiene que ser la de la definicion en los dos.
  utf8 <- "caf\u00e9"
  latin1 <- iconv(utf8, "UTF-8", "latin1")
  bytes <- utf8
  Encoding(bytes) <- "bytes"
  datos <- data.frame(
    u = rep(c(utf8, "te"), 10L), l = rep(c(latin1, "te"), 10L),
    b = rep(c(bytes, "te"), 10L), u2 = rep(c(utf8, "te"), 10L),
    stringsAsFactors = FALSE
  )
  .comprobar_como_definicion(datos)
})

test_that("integer64 se compara por sus bits", {
  skip_if_not_installed("bit64")
  # `-2` y `-3` son NaN en sus bits: `identical()` los iguala, la regla no.
  a <- bit64::as.integer64(c(-1, -2, 3, NA, 5))
  b <- bit64::as.integer64(c(-1, -3, 3, NA, 5))
  datos <- data.frame(a = a, a2 = a, b = b, d = c(-1, -2, 3, NA, 5))
  pares <- .comprobar_como_definicion(datos)
  expect_equal(pares, 1L)
})

test_that("cada columna se normaliza una sola vez", {
  # Con 200 columnas iguales, comparar todos los pares normalizaba 39.800
  # veces. Lo que se promete es lineal: una normalizacion por columna.
  valores <- paste0("valor_", seq_len(50L))
  datos <- as.data.frame(stats::setNames(rep(list(valores), 200L),
                                         paste0("V", seq_len(200L))),
                         stringsAsFactors = FALSE)
  llamadas <- 0L
  original <- lupa:::.normalizar_columna_entrada
  testthat::local_mocked_bindings(
    .normalizar_columna_entrada = function(x) {
      llamadas <<- llamadas + 1L
      original(x)
    },
    .package = "lupa"
  )
  # Y la salida tambien es lineal: 199 repetidas junto a la primera, no los
  # 19.900 pares.
  pares <- lupa:::.columnas_duplicadas(datos, names(datos))
  expect_equal(nrow(pares), 199L)
  expect_identical(unique(pares$columna_1), "V1")
  expect_identical(pares$columna_2, paste0("V", 2:200))
  expect_lte(llamadas, 200L)

  llamadas <- 0L
  distintas <- as.data.frame(lapply(seq_len(200L), function(k) {
    c(paste0("k", k), valores[-1L])
  }), stringsAsFactors = FALSE)
  names(distintas) <- paste0("V", seq_len(200L))
  expect_equal(nrow(lupa:::.columnas_duplicadas(distintas, names(distintas))), 0L)
  expect_lte(llamadas, 200L)
})
