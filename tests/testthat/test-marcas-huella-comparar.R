# comparar_perfiles() da por de diseno un hallazgo sobre una columna solo si la
# columna es marca `coincide` -o sin huella- en cada entrega donde esta. Una
# marca en una sola de dos entregas que tienen la columna no tapa el hallazgo
# de la otra.
testthat::skip_on_cran()

.pmhk_perfil <- function(d, fecha) {
  suppressWarnings(suppressMessages(perfilar(d, fecha = fecha)))
}

.pmhk_comparar <- function(a, b) {
  suppressWarnings(suppressMessages(comparar_perfiles(a, b)))
}

.pmhk_a <- as.POSIXct("2026-01-01", tz = "UTC")
.pmhk_b <- as.POSIXct("2026-02-01", tz = "UTC")

.pmhk_datos <- function() {
  set.seed(11)
  n <- 300L
  x <- rnorm(n)
  x[sample(n, 50L)] <- NA
  d <- data.frame(id = c(1:298, 5L, 7L), x = x,
                  g = sample(letters[1:4], n, TRUE),
                  texto = sample(c("a", "b", "c"), n, TRUE),
                  stringsAsFactors = FALSE)
  rbind(d, d[1:15, ])
}

.pmhk_limpio <- function(d0) {
  p0 <- .pmhk_perfil(d0, .pmhk_a)
  plan <- suppressWarnings(suppressMessages(planificar_limpieza(p0, d0)))
  plan$aplicar <- as.logical(plan$recomendada)
  suppressWarnings(suppressMessages(aplicar(plan, d0)))$datos
}

# Si la fila dice que la columna es una marca del plan.
.pmhk_dice <- function(texto) {
  grepl("La columna es una marca que agrega el plan", as.character(texto),
        fixed = TRUE, useBytes = TRUE)
}

.pmhk_severidad_marca <- function(cmp) {
  cmp[cmp$columna %in% ".grupo_duplicado" & cmp$aspecto == "severidad_hallazgo", ,
      drop = FALSE]
}

test_that("una marca en una sola entrega no tapa el empeoramiento de la otra", {
  limpio <- .pmhk_limpio(.pmhk_datos())
  antes <- .pmhk_perfil(limpio, .pmhk_a)
  h <- antes$hallazgos
  expect_identical(
    as.character(h$severidad[h$columna %in% ".grupo_duplicado" &
                               h$tipo_hallazgo == "faltantes"]),
    "ok"
  )
  # La misma tabla sin el atributo: lo que devuelve un CSV, `merge()` o
  # `cbind()`.
  sin_atributo <- limpio
  attr(sin_atributo, "marcas_limpieza") <- NULL
  # El nombre de la marca en una columna del usuario con ausentes.
  usuario <- sin_atributo
  usuario$.grupo_duplicado <- ifelse(usuario$g == "a", NA_integer_, 1L)
  # La marca renombrada y su nombre reasignado a una columna nueva.
  renombrada <- sin_atributo
  renombrada[[".grupo_duplicado_vieja"]] <- renombrada$.grupo_duplicado
  renombrada$.grupo_duplicado <- ifelse(renombrada$g == "a", NA_integer_, 1L)
  for (despues in list(sin_atributo, usuario, renombrada)) {
    cmp <- .pmhk_comparar(antes, .pmhk_perfil(despues, .pmhk_b))
    fila <- .pmhk_severidad_marca(cmp)
    expect_equal(nrow(fila), 1L)
    expect_identical(as.character(fila$cambio), "agravado")
    expect_false(as.character(fila$severidad) == "ok")
    expect_false(.pmhk_dice(fila$descripcion))
  }
})

test_that("antes y despues de limpiar, los hallazgos de las marcas siguen en ok", {
  d0 <- .pmhk_datos()
  limpio <- .pmhk_limpio(d0)
  cmp <- .pmhk_comparar(.pmhk_perfil(d0, .pmhk_a), .pmhk_perfil(limpio, .pmhk_b))
  marcas <- as.character(attr(limpio, "marcas_limpieza")$marca)
  filas <- cmp[cmp$columna %in% marcas, , drop = FALSE]
  expect_true(all(c("columna") %in% filas$aspecto))
  expect_true(all(as.character(filas$severidad) == "ok"))
})
