# Las marcas que agrega aplicar() no son dato: perfilar() las reconoce por el
# atributo `marcas_limpieza` y no buscan ausencia estructural sobre ellas (la
# marca de grupo esta vacia por diseno en las filas sin duplicado).
testthat::skip_on_cran()

.pmh_datos <- function() {
  set.seed(2)
  n <- 400L
  x <- rnorm(n)
  x[sample(n, 60L)] <- NA
  x[1:3] <- 50
  d <- data.frame(id = c(1:398, 5L, 7L), x = x, g = sample(letters[1:4], n, TRUE))
  rbind(d, d[1:20, ])
}

.pmh_aplicar <- function(d0) {
  p0 <- suppressWarnings(suppressMessages(perfilar(d0)))
  plan <- suppressWarnings(suppressMessages(planificar_limpieza(p0, d0)))
  plan$aplicar <- as.logical(plan$recomendada)
  suppressWarnings(suppressMessages(aplicar(plan, d0)))$datos
}

test_that("la ausencia de una marca de aplicar() no es estructural", {
  d1 <- .pmh_aplicar(.pmh_datos())
  marcas <- c(".fila_duplicada", ".grupo_duplicado", ".ausente_x", ".outlier_x")
  expect_true(all(marcas %in% names(d1)))
  p1 <- suppressWarnings(suppressMessages(perfilar(d1)))
  expect_setequal(p1$general$marcas_limpieza$marca, marcas)
  expect_false(any(p1$hallazgos$tipo_hallazgo == "posible_ausencia_estructural" &
                     p1$hallazgos$columna %in% marcas))
  # Lo demas se sigue diagnosticando.
  expect_true(any(p1$hallazgos$columna %in% "x"))
})

test_that("una columna del usuario con nombre de marca se sigue diagnosticando", {
  d <- .pmh_datos()
  d$.grupo_duplicado <- ifelse(d$id < 21L, 1L, NA_integer_)
  p <- suppressWarnings(suppressMessages(perfilar(d)))
  expect_null(p$general$marcas_limpieza)
  expect_true(any(p$hallazgos$tipo_hallazgo == "posible_ausencia_estructural" &
                    p$hallazgos$columna %in% ".grupo_duplicado"))
})
