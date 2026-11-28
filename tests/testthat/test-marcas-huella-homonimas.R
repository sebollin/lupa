# Con columnas del mismo nombre que una marca, la marca es la columna cuya
# huella coincide, este antes o despues de la copia del usuario.
testthat::skip_on_cran()

.pmho_perfil <- function(d, fecha = as.POSIXct("2026-01-01", tz = "UTC")) {
  suppressWarnings(suppressMessages(perfilar(d, fecha = fecha)))
}

.pmho_aplicar <- function(d0) {
  p0 <- .pmho_perfil(d0)
  plan <- suppressWarnings(suppressMessages(planificar_limpieza(p0, d0)))
  plan$aplicar <- as.logical(plan$recomendada) & as.character(plan$estado) == "lista"
  suppressWarnings(suppressMessages(aplicar(plan, d0)))$datos
}

.pmho_limpio <- function() {
  set.seed(11)
  n <- 300L
  x <- rnorm(n)
  x[sample(n, 50L)] <- NA
  d <- data.frame(id = c(1:298, 5L, 7L), x = x,
                  g = sample(letters[1:4], n, TRUE), stringsAsFactors = FALSE)
  .pmho_aplicar(rbind(d, d[1:15, ]))
}

# La tabla limpia con una copia del usuario de `.grupo_duplicado`, con
# ausentes, en la primera columna.
.pmho_copia_antes <- function(limpio) {
  copia <- data.frame(usuario = ifelse(limpio$g == "a", NA_integer_, 1L))
  d <- cbind(copia, limpio)
  names(d)[[1L]] <- ".grupo_duplicado"
  attr(d, "marcas_limpieza") <- attr(limpio, "marcas_limpieza")
  d
}

test_that("la marca intacta detras de una copia del usuario sigue siendo la marca", {
  limpio <- .pmho_limpio()
  d <- .pmho_copia_antes(limpio)
  real <- which(names(d) == ".grupo_duplicado")[[2L]]
  p <- .pmho_perfil(d)
  m <- p$general$marcas_limpieza
  expect_identical(as.character(m$estado[m$marca == ".grupo_duplicado"]), "coincide")
  expect_identical(as.integer(m$posicion[m$marca == ".grupo_duplicado"]), real)
  h <- p$hallazgos[p$hallazgos$columna %in% ".grupo_duplicado" &
                     p$hallazgos$tipo_hallazgo == "faltantes", , drop = FALSE]
  expect_equal(nrow(h), 2L)
  marca <- h$n_afectados == sum(is.na(limpio$.grupo_duplicado))
  expect_identical(as.character(h$severidad[marca]), "ok")
  expect_false(any(as.character(h$severidad[!marca]) == "ok"))
  impreso <- testthat::capture_messages(print(p))
  expect_false(any(grepl("ya no describe", impreso, fixed = TRUE, useBytes = TRUE)))
})

test_that("el orden de la copia y la marca no cambia la comparacion", {
  limpio <- .pmho_limpio()
  antes <- .pmho_copia_antes(limpio)
  despues <- limpio
  despues[[".grupo_duplicado_copia"]] <- antes[[1L]]
  names(despues)[names(despues) == ".grupo_duplicado_copia"] <- ".grupo_duplicado"
  attr(despues, "marcas_limpieza") <- attr(limpio, "marcas_limpieza")
  cmp <- suppressWarnings(suppressMessages(comparar_perfiles(
    .pmho_perfil(antes), .pmho_perfil(despues, as.POSIXct("2026-02-01", tz = "UTC"))
  )))
  # Las metricas de las dos homonimas se aparean por orden de nombre, como
  # siempre; lo que no puede aparecer es un cambio de la marca.
  fila <- cmp[cmp$columna %in% ".grupo_duplicado", , drop = FALSE]
  expect_false(any(fila$aspecto == "columna"))
  expect_false(any(fila$aspecto %in% c("hallazgo", "severidad_hallazgo") &
                     as.character(fila$severidad) != "ok"))
})

test_that("aplicar() vuelve a firmar la marca y no la copia del usuario", {
  limpio <- .pmho_limpio()
  d <- .pmho_copia_antes(limpio)
  plan <- suppressWarnings(suppressMessages(planificar_limpieza(.pmho_perfil(d), d)))
  plan$aplicar <- as.logical(plan$recomendada) & as.character(plan$estado) == "lista"
  # Sin renombrar: las dos homonimas siguen.
  sin_renombrar <- plan
  sin_renombrar$aplicar[sin_renombrar$estrategia == "normalizar_nombres"] <- FALSE
  de_nuevo <- suppressWarnings(suppressMessages(aplicar(sin_renombrar, d)))$datos
  real <- which(names(de_nuevo) == ".grupo_duplicado")[[2L]]
  m <- .pmho_perfil(de_nuevo)$general$marcas_limpieza
  expect_identical(as.character(m$estado[m$marca == ".grupo_duplicado"]), "coincide")
  expect_identical(as.integer(m$posicion[m$marca == ".grupo_duplicado"]), real)
  # Si el plan renombra las homonimas, la copia que queda con el nombre no se
  # firma como la marca.
  expect_true(any(plan$aplicar & plan$estrategia == "normalizar_nombres"))
  renombrada <- suppressWarnings(suppressMessages(aplicar(plan, d)))$datos
  expect_equal(sum(names(renombrada) == ".grupo_duplicado"), 1L)
  m <- .pmho_perfil(renombrada)$general$marcas_limpieza
  expect_false(identical(as.character(m$estado[m$marca == ".grupo_duplicado"]), "coincide"))
})
