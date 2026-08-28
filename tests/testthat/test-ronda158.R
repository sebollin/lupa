.tabla_ronda158 <- function(repetidas = FALSE) {
  base <- data.frame(
    codigo = c("a01", "a02", "a03", "a04", "a05", "a06"),
    con_na = c(NA_real_, 10, 20, NA_real_, 40, 50),
    factor = factor(
      c("rojo", "azul", NA, "verde", "rojo", "azul"),
      levels = c("azul", "rojo", "verde"), exclude = NULL
    ),
    fecha = as.Date("2020-01-01") + 0:5,
    entero64 = bit64::as.integer64(101:106),
    lista = I(lapply(seq_len(6L), function(i) c(i, i + 1L))),
    stringsAsFactors = FALSE
  )
  if (repetidas) rbind(base, base[1:3, , drop = FALSE]) else base
}

.conteos_base_ronda158 <- function(datos) {
  adelante <- base::duplicated.data.frame(datos)
  atras <- base::duplicated.data.frame(datos, fromLast = TRUE)
  c(
    filas_duplicadas = sum(adelante),
    filas_en_grupos_duplicados = sum(adelante | atras)
  )
}

test_that("el contador repliega las columnas de lista a base", {
  skip_if_not_installed("bit64")
  datos <- .tabla_ronda158(repetidas = TRUE)
  esperado <- .conteos_base_ronda158(datos)

  resultado <- lupa:::.conteos_filas_duplicadas(datos)

  expect_identical(unname(unlist(resultado)), unname(esperado))
  expect_false(anyNA(unlist(resultado)))
})

test_that("el contador conserva una fila por elemento con matrices", {
  datos <- data.frame(
    codigo = c("a", "a", "b"),
    matriz = I(rbind(c(1, 2), c(1, 2), c(3, 4)))
  )

  resultado <- lupa:::.conteos_filas_duplicadas(datos)

  expect_identical(
    unname(unlist(resultado)),
    unname(c(filas_duplicadas = 1L, filas_en_grupos_duplicados = 2L))
  )
})

test_that("las tablas chicas no pagan la via rapida", {
  datos <- data.frame(a = seq_len(10L), b = rev(seq_len(10L)))
  llamada <- FALSE
  local_mocked_bindings(
    .filas_duplicadas_frank = function(datos) {
      llamada <<- TRUE
      NULL
    },
    .package = "lupa"
  )

  lupa:::.conteos_filas_duplicadas(datos)

  expect_false(llamada)
})

test_that("las tablas suficientes usan la via rapida y dan lo mismo que base", {
  datos <- data.frame(a = rep(c("a", "b"), 15L), b = seq_len(30L))
  llamada <- FALSE
  original <- lupa:::.filas_duplicadas_frank
  local_mocked_bindings(
    .filas_duplicadas_frank = function(datos) {
      llamada <<- TRUE
      original(datos)
    },
    .package = "lupa"
  )

  resultado <- lupa:::.conteos_filas_duplicadas(datos)

  expect_true(llamada)
  expect_identical(
    unname(unlist(resultado)),
    unname(.conteos_base_ronda158(datos))
  )
})

test_that("un fallo inesperado de la via rapida repliega a base", {
  # El resultado exacto lo fija `duplicated()`. Si el atajo falla, la respuesta
  # sigue siendo la de base y no un ausente.
  datos <- data.frame(a = rep(c("a", "b"), 15L), b = rep(seq_len(15L), 2L))
  local_mocked_bindings(
    .filas_duplicadas_frank = function(datos) stop("fallo de prueba"),
    .package = "lupa"
  )

  resultado <- lupa:::.conteos_filas_duplicadas(datos)

  expect_identical(
    unname(unlist(resultado)),
    unname(.conteos_base_ronda158(datos))
  )
})

test_that("la via rapida y la de base coinciden en los tipos y bordes", {
  # `frank()` ordena y `duplicated.data.frame` pega cadenas: los casos donde
  # ordenar e imprimir difieren son los que hay que fijar.
  casos <- list(
    texto = data.frame(a = rep(c("a", "b", NA), 10L), stringsAsFactors = FALSE),
    doble_infinitos = data.frame(a = rep(c(1.5, Inf, -Inf, NA), 8L)),
    factor = data.frame(a = factor(rep(c("a", "b", NA), 10L))),
    logico = data.frame(a = rep(c(TRUE, FALSE, NA), 10L)),
    fecha = data.frame(a = as.Date("2020-01-01") + rep(c(0L, 1L, NA), 10L)),
    momento = data.frame(
      a = as.POSIXct("2020-01-01", tz = "UTC") + rep(c(0L, 1L, NA), 10L)
    ),
    cero_y_menos_cero = data.frame(a = rep(c(0, -0, 1), 10L)),
    dos_columnas = data.frame(
      a = rep(c("x", "y"), 15L), b = rep(c(1L, 1L, 2L), 10L),
      stringsAsFactors = FALSE
    )
  )
  for (nombre in names(casos)) {
    datos <- casos[[nombre]]
    expect_identical(
      unname(unlist(lupa:::.conteos_filas_duplicadas(datos))),
      unname(.conteos_base_ronda158(datos)),
      info = nombre
    )
  }
})

test_that("una tabla con NaN se mide por la via de base", {
  # `frank()` no distingue `NaN` de `NA`; `duplicated.data.frame` si. Donde hay
  # un `NaN` manda la semantica de base, que es la que fija el resultado.
  datos <- data.frame(a = rep(c(NA_real_, NaN, 1), 10L))
  expect_true(lupa:::.tiene_nan_en_dobles(datos))
  llamada <- FALSE
  local_mocked_bindings(
    .filas_duplicadas_frank = function(datos) {
      llamada <<- TRUE
      NULL
    },
    .package = "lupa"
  )

  resultado <- lupa:::.conteos_filas_duplicadas(datos)

  expect_false(llamada)
  expect_identical(
    unname(unlist(resultado)),
    unname(.conteos_base_ronda158(datos))
  )
})

test_that("perfilar deja identico el data.frame y data.table de entrada", {
  skip_if_not_installed("bit64")
  for (repetidas in c(FALSE, TRUE)) {
    datos_df <- .tabla_ronda158(repetidas)
    datos_dt <- data.table::as.data.table(datos_df)
    for (datos in list(datos_df, datos_dt)) {
      antes <- data.table::copy(datos)
      resultado <- perfilar(
        datos,
        analizar_dependencias = FALSE,
        casi_duplicados_vocabulario = FALSE
      )

      expect_s3_class(resultado, "perfil")
      expect_identical(datos, antes)
    }
  }
})

test_that("los conteos rapidos coinciden con base sin filas repetidas", {
  skip_if_not_installed("bit64")
  datos <- .tabla_ronda158(repetidas = FALSE)
  esperado <- .conteos_base_ronda158(datos)
  resultado <- lupa:::.conteos_filas_duplicadas(datos)

  expect_identical(unname(unlist(resultado)), unname(esperado))
})
