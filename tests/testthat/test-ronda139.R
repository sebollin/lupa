test_that("los cuantiles compartidos conservan el resumen numerico", {
  valores <- c(seq_len(60L), rep(999, 5L))
  observado <- lupa:::.resumen_cuantitativo(
    valores, list(tipo = "doble"), data.frame(),
    sentinelas_numericos = numeric()
  )

  iqr <- stats::IQR(valores, na.rm = TRUE, type = 7)
  cuartiles <- stats::quantile(
    valores, probs = c(0.25, 0.75), names = FALSE, type = 7
  )
  centinela <- lupa:::.centinela_por_tres_senales(
    valores, iqr, sentinelas_numericos = numeric()
  )
  esperado <- list(
    mediana = stats::median(valores),
    n_outliers = sum(
      valores < cuartiles[[1L]] - 1.5 * iqr |
        valores > cuartiles[[2L]] + 1.5 * iqr
    ),
    centinela_valor = centinela$valor,
    centinela_repeticiones = centinela$n,
    densidad_sin_centinela = centinela$densidad_sin_centinela
  )
  observado <- observado[names(esperado)]

  expect_identical(observado, esperado)
})

test_that("la guarda corta sigue antes de los cuantiles del centinela", {
  expect_identical(
    lupa:::.centinela_por_tres_senales(
      seq_len(19L), 1, sentinelas_numericos = numeric(), q1 = 1, q3 = 19
    ),
    list(valor = NA_real_, n = NA_integer_, densidad_sin_centinela = NA_real_)
  )
})

test_that("los conteos de duplicados comparten sus dos vectores", {
  datos <- data.frame(
    a = c("A", "A", "B", "B"), b = c(1L, 1L, 1L, 2L),
    stringsAsFactors = FALSE
  )
  esperado <- list(
    filas_duplicadas = sum(duplicated(datos)),
    filas_en_grupos_duplicados = sum(
      duplicated(datos) | duplicated(datos, fromLast = TRUE)
    )
  )

  expect_identical(lupa:::.conteos_filas_duplicadas(datos), esperado)
})

test_that("los dos conteos caen por separado ante fallos de duplicated", {
  todos_fallan <- data.frame(a = c(1L, 1L))
  class(todos_fallan) <- c("ronda139_falla", class(todos_fallan))
  metodo_falla <- function(
      x, incomparables = FALSE, fromLast = FALSE, ...) {
    stop("fallo de prueba")
  }
  assign("duplicated.ronda139_falla", metodo_falla, envir = .GlobalEnv)
  on.exit(rm(duplicated.ronda139_falla, envir = .GlobalEnv), add = TRUE)
  expect_identical(
    lupa:::.conteos_filas_duplicadas(todos_fallan),
    list(filas_duplicadas = NA_integer_,
         filas_en_grupos_duplicados = NA_integer_)
  )

  falla_atras <- data.frame(a = c(1L, 1L))
  class(falla_atras) <- c("ronda139_falla_atras", class(falla_atras))
  metodo_falla_atras <- function(
      x, incomparables = FALSE, fromLast = FALSE, ...) {
    if (isTRUE(fromLast)) stop("fallo de prueba atras")
    base::duplicated.data.frame(
      unclass(x), incomparables = incomparables, fromLast = fromLast, ...
    )
  }
  assign("duplicated.ronda139_falla_atras", metodo_falla_atras,
         envir = .GlobalEnv)
  on.exit(rm(duplicated.ronda139_falla_atras, envir = .GlobalEnv), add = TRUE)
  expect_identical(
    lupa:::.conteos_filas_duplicadas(falla_atras),
    list(filas_duplicadas = 1L,
         filas_en_grupos_duplicados = NA_integer_)
  )
})

.ronda139_datos_dependencias <- function() {
  n <- 1000L
  data.frame(
    x = rep(c("A", "B"), each = n / 2L),
    y = rep(sprintf("Y%02d", seq_len(20L)), length.out = n),
    stringsAsFactors = FALSE
  )
}

.ronda139_comparar_dependencias <- function(datos, ...) {
  argumentos <- list(
    datos = datos, muestra = Inf, min_observaciones = 10L,
    max_comparaciones = Inf, max_trabajo = Inf, ...
  )
  podada <- do.call(detectar_dependencias, argumentos)
  sin_poda <- testthat::with_mocked_bindings(
    do.call(detectar_dependencias, argumentos),
    .poda_dependencia_cardinalidad = function(...) FALSE,
    .package = "lupa"
  )
  expect_identical(podada, sin_poda)
  invisible(podada)
}

test_that("la poda de cardinalidad no cambia el data frame completo", {
  datos <- .ronda139_datos_dependencias()
  resultado <- .ronda139_comparar_dependencias(datos)

  expect_false(any(
    resultado$determinante == "x" & resultado$dependiente == "y"
  ))
  expect_true(.poda_dependencia_cardinalidad(
    list(n = 1000L, n_distintos = 2L),
    list(n = 1000L, n_distintos = 20L), 1000L, 0.995
  ))
})

test_that("la poda no usa cardinalidades de columnas con ausentes", {
  datos <- data.frame(
    x = rep(c("A", "B"), each = 500L),
    y = rep(c("a", "b"), each = 500L),
    stringsAsFactors = FALSE
  )
  datos$x[seq_len(100L)] <- NA_character_
  resultado <- .ronda139_comparar_dependencias(datos)

  expect_true(any(
    resultado$determinante == "x" & resultado$dependiente == "y" &
      resultado$exacta
  ))
  expect_false(.poda_dependencia_cardinalidad(
    list(n = 900L, n_distintos = 2L),
    list(n = 1000L, n_distintos = 20L), 1000L, 0.995
  ))
})

test_that("la poda conserva empates de moda y el umbral aproximado", {
  n <- 1000L
  datos <- data.frame(
    x = rep(c("A", "B"), each = n / 2L),
    y = rep(c("a", "b", "c", "d"), each = n / 4L),
    stringsAsFactors = FALSE
  )
  .ronda139_comparar_dependencias(datos)

  contraejemplo <- data.frame(
    x = rep(c("A", "B"), each = 1000L),
    y = c(rep("a", 1000L), rep("b", 1000L)),
    stringsAsFactors = FALSE
  )
  contraejemplo$y[seq_len(8L)] <- c("b", letters[3:9])
  resultado <- detectar_dependencias(
    contraejemplo, muestra = Inf, min_observaciones = 10L,
    max_comparaciones = Inf, max_trabajo = Inf
  )
  fila <- resultado[
    resultado$determinante == "x" & resultado$dependiente == "y", ,
    drop = FALSE
  ]
  expect_equal(fila$cumplimiento, 0.996)
  expect_equal(fila$n_violaciones, 8L)
})

test_that("normalizar y bloquear_por no alteran las dependencias podadas", {
  skip_if_not_installed("stringdist")
  # La tabla grande se reserva para medir la poda. Para probar la coordinacion
  # con el detector aproximado alcanza una muestra chica: el detector de
  # duplicados compara instancias y aqui solo importa que reciba el bloqueo.
  datos <- .ronda139_datos_dependencias()[seq_len(40L), , drop = FALSE]
  datos$bloque <- rep(c("A", "B"), each = nrow(datos) / 2L)
  datos$texto <- rep(c("foo", "fo0"), length.out = nrow(datos))

  for (normalizar in c(TRUE, FALSE)) {
    argumentos <- list(
      datos = datos, normalizar = normalizar,
      proteger_datos_personales = FALSE,
      duplicados_aproximados = list(
      columnas = "texto", bloquear_por = "bloque",
        max_pares = 100L, max_resultados = 10L, nucleos = 1L
      )
    )
    podada <- do.call(perfilar, argumentos)$dependencias
    sin_poda <- testthat::with_mocked_bindings(
      do.call(perfilar, argumentos)$dependencias,
      .poda_dependencia_cardinalidad = function(...) FALSE,
      .package = "lupa"
    )
    expect_identical(podada, sin_poda)
  }
})
