test_that("F1: la lectura convertida decide conservar float8", {
  con <- structure(list(), class = "PqConnection")
  lectura <- lupa:::.lectura_convertida_dbi(
    con, "x", "\"x\"", "float8"
  )
  manual <- c(0.1 * 3, 0.3)
  expect_identical(length(unique(manual)), 2L)
  expect_identical(lectura$convertidas$columna, "x")
  expect_identical(lectura$convertidas$tipo_motor, "float8")
  expect_false(identical(lectura$campos_sql[[1L]], "\"x\""))
})

test_that("F2: el desvio decimal se decide en FLOAT", {
  alias <- function(nombre) "\"desvio\""
  valores <- c(0.0001, 0.0002, 0.0008, 0.0011)
  esperado <- stats::sd(valores)
  forma <- lupa:::.formas_desvio_dbi(
    "[t]", "[v]", alias, "microsoft sql server", tipo = "decimal(38, 8)"
  )[[2L]]
  expect_true(is.finite(esperado) && esperado > 0)
  expect_match(forma, "STDEV(CAST([v] AS FLOAT))", fixed = TRUE)
  expect_false(grepl("* 1.0", forma, fixed = TRUE))
})

test_that("F4: el centinela real se alinea antes de perfilar", {
  centinela <- -99.9
  guardado <- lupa:::.real_dbi_a_doble(centinela)
  datos <- data.frame(temp = c(guardado, 9.1, 20.2, 22.4))
  ajustados <- lupa:::.ajustar_sentinelas_real_dbi(
    datos, "temp", "7", centinela
  )
  expect_identical(ajustados$temp[[1L]], centinela)
  p <- perfilar(
    ajustados, sentinelas_numericos = centinela,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  )
  h <- p$hallazgos
  h <- h[h$tipo_hallazgo == "faltantes_disfrazados", , drop = FALSE]
  expect_equal(nrow(h), 1L)
  expect_match(h$evidencia[[1L]], "-99.9", fixed = TRUE)
  expect_equal(p$columnas$minimo[p$columnas$columna == "temp"], 9.1)
})

test_that("F5: el desvio por columna conserva el centro sin basicos", {
  alias <- function(nombre) "\"desvio\""
  valores <- 1760000000000 + c(1, 17, 208, 999)
  esperado <- stats::sd(valores)
  forma <- lupa:::.formas_desvio_dbi(
    "[t]", "[v]", alias, "microsoft sql server",
    centro = mean(valores), tipo = "bigint"
  )[[2L]]
  expect_true(is.finite(esperado) && esperado > 0)
  expect_match(forma, "STDEV(CAST(([v] - ", fixed = TRUE)
  expect_false(grepl("SELECT AVG", forma, fixed = TRUE))
})

test_that("F3: las columnas duplicadas se comparan por contenido", {
  set.seed(7)
  n <- 400L
  d <- data.frame(
    id = seq_len(n),
    nombre = sample(c("ana", "bea", "carla", "dora"), n, TRUE),
    stringsAsFactors = FALSE
  )
  d$ciudad <- d$nombre
  d$monto <- round(stats::rlnorm(n, 5, 1), 2)
  d$monto[1:5] <- c(-999, -999, 0, 1e7, -5)
  d$doble <- d$monto * 2
  d$estado <- sample(c("activo", "baja", "N/A", ""), n, TRUE)
  p1 <- perfilar(
    d, fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    analizar_dependencias = FALSE
  )
  p2 <- perfilar(
    d[c("id", "ciudad", "nombre", "doble", "monto", "estado")],
    fecha = as.POSIXct("2026-02-01", tz = "UTC"),
    analizar_dependencias = FALSE
  )
  deriva <- as.data.frame(suppressMessages(comparar_perfiles(p1, p2)))
  duplicadas <- deriva[deriva$valor_anterior == "columnas_duplicadas" |
                         deriva$valor_actual == "columnas_duplicadas", , drop = FALSE]
  expect_equal(nrow(duplicadas), 0L)
})

test_that("F6: un tope de orden declina el hallazgo anterior", {
  n <- 300L
  d <- data.frame(
    id = seq_len(n), relleno = seq_len(n) + 100,
    ingreso = seq_len(n), egreso = seq_len(n) + 1,
    stringsAsFactors = FALSE
  )
  d$egreso[1:3] <- d$ingreso[1:3] - 5
  p1 <- perfilar(d, max_columnas_orden = 20L,
                 umbral_solapamiento_orden = 0,
                 proteger_datos_personales = FALSE,
                 analizar_dependencias = FALSE)
  p2 <- perfilar(d, max_columnas_orden = 2L,
                 umbral_solapamiento_orden = 0,
                 proteger_datos_personales = FALSE,
                 analizar_dependencias = FALSE,
                 fecha = as.POSIXct("2026-02-01", tz = "UTC"))
  p1$meta$fecha_hora <- as.POSIXct("2026-01-01", tz = "UTC")
  cobertura <- p2$cobertura_diagnosticos
  expect_true(any(cobertura$diagnostico == "relacion_orden_columnas"))
  deriva <- as.data.frame(suppressMessages(comparar_perfiles(p1, p2)))
  expect_true(any(deriva$cambio == "no_evaluado"))
  expect_false(any(deriva$cambio == "resuelto"))
})

test_that("B1: la muestra truncada declina los hallazgos de la muestra", {
  d <- data.frame(
    id = seq_len(40), estado = c(rep("a", 20), rep("b", 20)),
    valor = seq_len(40), stringsAsFactors = FALSE
  )
  p1 <- perfilar(d, max_celdas_muestra = 20L,
                 proteger_datos_personales = FALSE,
                 analizar_dependencias = FALSE)
  p2 <- perfilar(d, proteger_datos_personales = FALSE,
                 analizar_dependencias = FALSE,
                 fecha = as.POSIXct("2026-02-01", tz = "UTC"))
  p1$meta$fecha_hora <- as.POSIXct("2026-01-01", tz = "UTC")
  expect_true(any(p1$cobertura_diagnosticos$diagnostico ==
                  "muestra_perfilado"))
  deriva <- as.data.frame(suppressMessages(comparar_perfiles(p1, p2)))
  expect_false(any(deriva$cambio == "resuelto" &
                   deriva$aspecto == "hallazgo"))
})
