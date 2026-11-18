test_that("los topes que cambian la muestra explican la deriva de patrones", {
  testthat::skip_on_cran()
  n <- 62L
  datos <- data.frame(
    ciudad = rep("Montevideo", n),
    a = rep(1, n), b = rep(2, n), c = rep(3, n), d = rep(4, n),
    e = rep(5, n), f = rep(6, n), stringsAsFactors = FALSE
  )
  datos$ciudad[1:4] <- sprintf("%04d", 1:4)
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  base <- suppressWarnings(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE
  ))
  movido <- suppressWarnings(perfilar(
    datos, fecha = fecha + 60, max_celdas_muestra = 200,
    analizar_dependencias = FALSE
  ))

  expect_identical(as.integer(movido$meta$muestra_efectiva), 28L)
  hallazgo <- movido$hallazgos[
    movido$hallazgos$tipo_hallazgo == "patron_raro" &
      movido$hallazgos$columna == "ciudad", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  deriva <- suppressWarnings(comparar_perfiles(base, movido))
  expect_true(any(deriva$aspecto == "configuracion_patrones"))
  expect_true(any(
    deriva$aspecto == "hallazgo" & deriva$valor_actual == "patron_raro"
  ))
})

test_that("una tabla que pasa de `muestra` filas a una mas sigue comparando patrones", {
  testthat::skip_on_cran()
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  base <- data.frame(codigo = sprintf("AB-%03d", seq_len(200L)),
                     stringsAsFactors = FALSE)
  # La fila nueva tiene otra forma y tiene que entrar en la muestra de 200: se
  # busca entre valores de la misma forma uno que la huella elija.
  actual <- NULL
  for (i in seq_len(500L)) {
    nuevo <- paste(letters[c(i %% 26L + 1L, (i %/% 26L) %% 26L + 1L, 3L)],
                   collapse = "")
    prueba <- rbind(base, data.frame(codigo = nuevo, stringsAsFactors = FALSE))
    if (201L %in% lupa:::.indices_huella_tabla(prueba, 200L)$indices) {
      actual <- prueba
      break
    }
  }
  expect_false(is.null(actual))
  anterior <- suppressWarnings(perfilar(
    base, muestra = 200L, fecha = fecha, analizar_dependencias = FALSE
  ))
  posterior <- suppressWarnings(perfilar(
    actual, muestra = 200L, fecha = fecha + 60, analizar_dependencias = FALSE
  ))
  # La premisa: un lado se lee entero y el otro es una muestra.
  expect_gte(as.numeric(anterior$meta$muestra_efectiva), 200)
  expect_lt(as.numeric(posterior$meta$muestra_efectiva), 201)
  deriva <- suppressWarnings(comparar_perfiles(anterior, posterior))
  expect_false(any(deriva$aspecto == "configuracion_patrones"))
  expect_true(any(deriva$aspecto == "patron" & deriva$cambio == "aparecido"))
})

test_that("el mismo tope de bytes que recorta distinto en cada entrega no corta los patrones", {
  testthat::skip_on_cran()
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  n <- 400L
  corta <- data.frame(codigo = sprintf("AB-%03d", seq_len(n)),
                      nota = sprintf("n%03d", seq_len(n)),
                      stringsAsFactors = FALSE)
  larga <- corta
  larga$nota <- paste0(larga$nota, strrep("x", 40L))
  tope <- as.numeric(utils::object.size(corta)) / 2
  anterior <- suppressWarnings(perfilar(
    corta, fecha = fecha, max_bytes_muestra = tope,
    analizar_dependencias = FALSE
  ))
  posterior <- suppressWarnings(perfilar(
    larga, fecha = fecha + 60, max_bytes_muestra = tope,
    analizar_dependencias = FALSE
  ))
  # La premisa: el tope recorta las dos, a tamanos distintos.
  efectiva_a <- as.numeric(anterior$meta$muestra_efectiva)
  efectiva_b <- as.numeric(posterior$meta$muestra_efectiva)
  expect_lt(efectiva_a, n)
  expect_lt(efectiva_b, n)
  expect_false(identical(efectiva_a, efectiva_b))
  deriva <- suppressWarnings(comparar_perfiles(anterior, posterior))
  expect_false(any(deriva$aspecto == "configuracion_patrones"))
})
