# El estado de cada marca de aplicar() se decide una vez y por posicion, con
# cuatro valores (`coincide`, `desactualizada`, `no_coincide`, `sin_huella`), se
# publica en `general$marcas_limpieza` y todo lo lee de ahi: los hallazgos de
# la marca, la impresion del perfil, comparar_perfiles() y el plan.
testthat::skip_on_cran()

.pmhe_perfil <- function(d, fecha = as.POSIXct("2026-01-01", tz = "UTC")) {
  suppressWarnings(suppressMessages(perfilar(d, fecha = fecha)))
}

.pmhe_plan <- function(p, d) {
  suppressWarnings(suppressMessages(planificar_limpieza(p, d)))
}

.pmhe_aplicar <- function(d0) {
  plan <- .pmhe_plan(.pmhe_perfil(d0), d0)
  plan$aplicar <- as.logical(plan$recomendada)
  suppressWarnings(suppressMessages(aplicar(plan, d0)))$datos
}

.pmhe_comparar <- function(a, b) {
  suppressWarnings(suppressMessages(comparar_perfiles(a, b)))
}

.pmhe_estado <- function(p, marca) {
  m <- p$general$marcas_limpieza
  as.character(m$estado[m$marca == marca])
}

.pmhe_dice <- function(texto, fragmento) {
  grepl(fragmento, as.character(texto), fixed = TRUE, useBytes = TRUE)
}

.pmhe_limpio <- function() {
  set.seed(11)
  n <- 300L
  x <- rnorm(n)
  x[sample(n, 50L)] <- NA
  d <- data.frame(id = c(1:298, 5L, 7L), x = x,
                  g = sample(letters[1:4], n, TRUE), stringsAsFactors = FALSE)
  .pmhe_aplicar(rbind(d, d[1:15, ]))
}

# Edita `g` en la primera fila de un grupo duplicado, con un valor que ya existe.
.pmhe_rompe_un_grupo <- function(d) {
  fila <- match(1L, d$.grupo_duplicado)
  d$g[[fila]] <- if (identical(d$g[[fila]], "a")) "b" else "a"
  d
}

test_that("el estado se publica con la posicion de la columna de cada marca", {
  limpio <- .pmhe_limpio()
  p <- .pmhe_perfil(limpio)
  m <- p$general$marcas_limpieza
  expect_true(all(c("marca", "columna", "estado", "posicion") %in% names(m)))
  expect_identical(as.integer(m$posicion), match(m$marca, names(limpio)))
  expect_true(all(m$estado == "coincide"))
  # Sin marcas que dejaron de describir los datos, la impresion no las nombra.
  impreso <- testthat::capture_messages(print(p))
  expect_false(any(.pmhe_dice(impreso, "ya no describe")))
})

test_that("una segunda columna con el nombre de la marca de grupo es dato", {
  limpio <- .pmhe_limpio()
  doble <- limpio
  doble[["usuario"]] <- ifelse(doble$g == "a", NA_integer_, 1L)
  names(doble)[names(doble) == "usuario"] <- ".grupo_duplicado"
  attr(doble, "marcas_limpieza") <- attr(limpio, "marcas_limpieza")
  p <- .pmhe_perfil(doble)
  m <- p$general$marcas_limpieza
  expect_identical(as.integer(m$posicion[m$marca == ".grupo_duplicado"]),
                   match(".grupo_duplicado", names(doble)))
  h <- p$hallazgos[p$hallazgos$columna %in% ".grupo_duplicado", , drop = FALSE]
  faltantes <- h[h$tipo_hallazgo == "faltantes", , drop = FALSE]
  expect_equal(nrow(faltantes), 2L)
  marca <- faltantes$n_afectados == sum(is.na(limpio$.grupo_duplicado))
  expect_identical(as.character(faltantes$severidad[marca]), "ok")
  # La del usuario conserva su severidad y no habla de la marca.
  expect_false(any(as.character(faltantes$severidad[!marca]) == "ok"))
  expect_false(any(.pmhe_dice(faltantes$descripcion[!marca], "aplicar()")))
  # Y su ausencia, que decide `g`, se diagnostica como la de cualquier columna.
  expect_equal(sum(h$tipo_hallazgo == "posible_ausencia_estructural"), 1L)
  plan <- .pmhe_plan(p, doble)
  expect_false(any(plan$columna %in% ".grupo_duplicado"))
})

test_that("una marca de grupo cuyos grupos se rompieron queda desactualizada", {
  editado <- .pmhe_rompe_un_grupo(.pmhe_limpio())
  p <- .pmhe_perfil(editado)
  expect_identical(.pmhe_estado(p, ".grupo_duplicado"), "desactualizada")
  expect_identical(.pmhe_estado(p, ".fila_duplicada"), "coincide")
  h <- p$hallazgos[p$hallazgos$columna %in% ".grupo_duplicado" &
                     p$hallazgos$tipo_hallazgo == "faltantes", , drop = FALSE]
  expect_equal(nrow(h), 1L)
  expect_false(as.character(h$severidad) == "ok")
  expect_true(.pmhe_dice(h$descripcion, "no se confirman con las filas duplicadas"))
  impreso <- testthat::capture_messages(print(p))
  expect_true(any(.pmhe_dice(impreso, ".grupo_duplicado (desactualizada)")))
  plan <- .pmhe_plan(p, editado)
  expect_false(any(plan$columna %in% ".grupo_duplicado"))
  sin_accion <- attr(plan, "hallazgos_sin_accion")
  motivo <- sin_accion$motivo[sin_accion$columna %in% ".grupo_duplicado"]
  expect_true(length(motivo) > 0L)
  expect_true(all(.pmhe_dice(motivo, "ya no son las filas duplicadas de hoy")))
})

test_that("comparar_perfiles() no da por de diseno el hallazgo de una marca desactualizada", {
  limpio <- .pmhe_limpio()
  antes <- .pmhe_perfil(limpio)
  despues <- .pmhe_perfil(.pmhe_rompe_un_grupo(limpio),
                          as.POSIXct("2026-02-01", tz = "UTC"))
  cmp <- .pmhe_comparar(antes, despues)
  marca <- cmp[cmp$columna %in% ".grupo_duplicado", , drop = FALSE]
  hallazgo <- marca[marca$aspecto %in% c("hallazgo", "severidad_hallazgo"), , drop = FALSE]
  expect_true(nrow(hallazgo) > 0L)
  expect_false(any(as.character(hallazgo$severidad) == "ok"))
  estado <- marca[marca$aspecto == "columna", , drop = FALSE]
  expect_equal(nrow(estado), 1L)
  expect_identical(as.character(estado$cambio), "modificado")
  expect_identical(as.character(estado$valor_anterior), "coincide")
  expect_identical(as.character(estado$valor_actual), "desactualizada")
  expect_identical(as.character(estado$severidad), "sospechoso")
  # Entre dos entregas limpias e intactas no hay fila de estado.
  otra <- .pmhe_perfil(limpio, as.POSIXct("2026-02-01", tz = "UTC"))
  cmp <- .pmhe_comparar(antes, otra)
  expect_false(any(cmp$columna %in% ".grupo_duplicado" & cmp$aspecto == "columna"))
})

test_that("una marca de grupo sin ningun NA cuyos grupos se rompen no queda muda", {
  set.seed(7)
  x <- round(rnorm(20L) * 100, 3)
  d <- data.frame(id = rep(sprintf("p%02d", 1:20), each = 2L),
                  x = rep(x, each = 2L), stringsAsFactors = FALSE)
  limpio <- .pmhe_aplicar(d)
  expect_false(anyNA(limpio$.grupo_duplicado))
  antes <- .pmhe_perfil(limpio)
  expect_identical(.pmhe_estado(antes, ".grupo_duplicado"), "coincide")
  # Se intercambian dos valores de `x` entre las primeras filas de dos parejas:
  # la distribucion de `x` no cambia y las marcas quedan intactas.
  roto <- limpio
  roto$x[c(1L, 3L)] <- roto$x[c(3L, 1L)]
  despues <- .pmhe_perfil(roto, as.POSIXct("2026-02-01", tz = "UTC"))
  expect_identical(.pmhe_estado(despues, ".grupo_duplicado"), "desactualizada")
  impreso <- testthat::capture_messages(print(despues))
  expect_true(any(.pmhe_dice(impreso, ".grupo_duplicado (desactualizada)")))
  cmp <- .pmhe_comparar(antes, despues)
  fila <- cmp[cmp$columna %in% ".grupo_duplicado" & cmp$aspecto == "columna", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$severidad), "sospechoso")
})
