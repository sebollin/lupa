# El contrato del atributo `marcas_limpieza`: la huella de una marca de columna
# cubre tambien la columna que describe, de la identidad de la fila sale solo
# la columna cuya huella coincidio, una marca de grupo toda NA vale si hoy no
# hay duplicados, y una marca que no coincide se compara y se explica como
# cualquier columna.
testthat::skip_on_cran()

.pmhc_fecha <- as.POSIXct("2026-01-01", tz = "UTC")

.pmhc_perfil <- function(d, fecha = .pmhc_fecha) {
  suppressWarnings(suppressMessages(perfilar(d, fecha = fecha)))
}

.pmhc_plan <- function(p, d) {
  suppressWarnings(suppressMessages(planificar_limpieza(p, d)))
}

.pmhc_aplicar <- function(d0) {
  plan <- .pmhc_plan(.pmhc_perfil(d0), d0)
  plan$aplicar <- as.logical(plan$recomendada)
  suppressWarnings(suppressMessages(aplicar(plan, d0)))$datos
}

.pmhc_estado <- function(p, marca) {
  m <- p$general$marcas_limpieza
  as.character(m$estado[m$marca == marca])
}

.pmhc_sobre <- function(p, columna, tipo = NULL) {
  h <- p$hallazgos
  h <- h[as.character(h$columna) %in% columna, , drop = FALSE]
  if (!is.null(tipo)) h <- h[as.character(h$tipo_hallazgo) %in% tipo, , drop = FALSE]
  h
}

.pmhc_grupo_ok <- function(p) {
  h <- .pmhc_sobre(p, ".grupo_duplicado", "faltantes")
  expect_equal(nrow(h), 1L)
  identical(as.character(h$severidad), "ok")
}

.pmhc_dice <- function(texto, fragmento) {
  grepl(fragmento, as.character(texto), fixed = TRUE, useBytes = TRUE)
}

# Veinte parejas y diez filas sueltas, con los duplicados ya marcados.
.pmhc_parejas <- function(prefijo, paso) {
  d <- data.frame(
    id = c(rep(sprintf("%s%02d", prefijo, 1:20), each = 2L),
           sprintf("%s%02d", toupper(prefijo), 1:10)),
    valor = c(rep(seq_len(20L) * paso, each = 2L), seq_len(10L) * 7),
    stringsAsFactors = FALSE
  )
  .pmhc_aplicar(d)
}

test_that("el registro y la marca de otra ejecucion no coinciden sobre otra x", {
  # Donante: x con ausentes que alternan dentro de cada pareja, marcados en un
  # segundo aplicar().
  donante <- .pmhc_parejas("p", 10)
  donante$x <- rep(c(NA, 1), length.out = nrow(donante))
  donante <- .pmhc_aplicar(donante)
  expect_true(".ausente_x" %in% attr(donante, "marcas_limpieza")$marca)
  # Destino: la misma forma de grupos, una x completa, y el atributo y las
  # columnas de marca del donante copiados tal cual.
  destino <- .pmhc_parejas("q", 3)
  destino$x <- 1
  for (m in as.character(attr(donante, "marcas_limpieza")$marca)) {
    destino[[m]] <- donante[[m]]
  }
  attr(destino, "marcas_limpieza") <- attr(donante, "marcas_limpieza")
  p <- .pmhc_perfil(destino)
  expect_identical(.pmhc_estado(p, ".ausente_x"), "no_coincide")
  # La marca de grupo es la del donante, pero `.ausente_x` entra como dato y
  # sus grupos ya no son duplicados.
  expect_identical(.pmhc_estado(p, ".grupo_duplicado"), "desactualizada")
  expect_false(.pmhc_grupo_ok(p))
  # Solo la fila del registro, sumada al atributo propio del destino.
  propio <- .pmhc_parejas("q", 3)
  propio$.ausente_x <- donante$.ausente_x
  registro <- attr(donante, "marcas_limpieza")
  attr(propio, "marcas_limpieza") <- rbind(
    attr(propio, "marcas_limpieza"), registro[registro$marca == ".ausente_x", ]
  )
  p <- .pmhc_perfil(propio)
  expect_identical(.pmhc_estado(p, ".ausente_x"), "no_coincide")
  expect_false(.pmhc_grupo_ok(p))
})

test_that("mover los ausentes de x despues de limpiar deja de callar su ausencia", {
  set.seed(31)
  n <- 300L
  x <- rnorm(n)
  ausentes <- sample(n, 100L)
  x[ausentes] <- NA
  limpio <- .pmhc_aplicar(data.frame(id = seq_len(n), x = x))
  expect_identical(.pmhc_estado(.pmhc_perfil(limpio), ".ausente_x"), "coincide")
  movido <- limpio
  movido$x[ausentes] <- 0
  movido$x[setdiff(seq_len(n), ausentes)[seq_along(ausentes)]] <- NA
  p <- .pmhc_perfil(movido)
  expect_identical(.pmhc_estado(p, ".ausente_x"), "no_coincide")
  sin_atributo <- movido
  attr(sin_atributo, "marcas_limpieza") <- NULL
  tipos <- c("ausencia_dependiente", "posible_ausencia_estructural",
             "faltantes_conjuntos")
  con <- .pmhc_sobre(p, "x", tipos)
  sin <- .pmhc_sobre(.pmhc_perfil(sin_atributo), "x", tipos)
  expect_true(nrow(sin) > 0L)
  expect_setequal(paste(con$tipo_hallazgo, as.character(con$severidad)),
                  paste(sin$tipo_hallazgo, as.character(sin$severidad)))
})

test_that("marcar e imputar en el mismo aplicar() deja una marca que coincide", {
  set.seed(41)
  n <- 300L
  z <- sample(letters[1:3], n, TRUE)
  x <- c(a = 10, b = 20, c = 30)[z]
  x[sample(n, 90L)] <- NA
  d0 <- data.frame(id = seq_len(n), z = z, x = as.numeric(x), stringsAsFactors = FALSE)
  d1 <- .pmhc_aplicar(d0)
  plan <- .pmhc_plan(.pmhc_perfil(d1), d1)
  plan$aplicar <- as.logical(plan$recomendada)
  imputar <- grepl("imputar_dependencia_funcional", plan$estrategia, fixed = TRUE,
                   useBytes = TRUE)
  expect_true(any(imputar))
  plan$aplicar[imputar] <- TRUE
  d2 <- suppressWarnings(suppressMessages(aplicar(plan, d1)))$datos
  expect_false(anyNA(d2$x))
  expect_equal(sum(d2$.ausente_x), 90L)
  # La marca registra que valores faltaban antes de imputarlos.
  p <- .pmhc_perfil(d2)
  expect_identical(.pmhc_estado(p, ".ausente_x"), "coincide")
})

test_that("de la identidad de la fila sale la columna de la marca, no su nombre", {
  limpio <- .pmhc_parejas("n", 10)
  limpio$x <- rep(rep(c(NA, 1), each = 2L), length.out = nrow(limpio))
  limpio <- .pmhc_aplicar(limpio)
  expect_true(.pmhc_grupo_ok(.pmhc_perfil(limpio)))
  # Una columna del usuario con el mismo nombre que la marca, que alterna
  # dentro de cada pareja: las filas ya no son duplicadas.
  doble <- cbind(limpio, usuario = rep(c(TRUE, FALSE), length.out = nrow(limpio)))
  names(doble)[ncol(doble)] <- ".ausente_x"
  attr(doble, "marcas_limpieza") <- attr(limpio, "marcas_limpieza")
  p <- .pmhc_perfil(doble)
  expect_identical(.pmhc_estado(p, ".ausente_x"), "coincide")
  expect_false(.pmhc_grupo_ok(p))
})

test_that("una marca de grupo toda NA vale si hoy no hay filas duplicadas", {
  d <- data.frame(
    id = c(rep(1:10, each = 2L), 100L + seq_len(10L)),
    x = c(rep(NA_real_, 20L), rep(1, 10L)),
    v = c(rep(seq_len(10L), each = 2L), seq_len(10L))
  )
  plan <- .pmhc_plan(.pmhc_perfil(d), d)
  plan$aplicar <- as.logical(plan$recomendada)
  plan$aplicar[plan$estrategia == "marcar_filas_ausentes"] <- FALSE
  plan$aplicar[plan$estrategia == "eliminar_filas_ausentes"] <- TRUE
  r <- suppressWarnings(suppressMessages(aplicar(plan, d, permitir_eliminacion = TRUE)))
  expect_true(all(c("marcar_filas_duplicadas", "eliminar_filas_ausentes") %in%
                    r$registro$estrategia[r$registro$estado == "ejecutada"]))
  expect_true(all(is.na(r$datos$.grupo_duplicado)))
  p <- .pmhc_perfil(r$datos)
  expect_true(.pmhc_grupo_ok(p))
  h <- .pmhc_sobre(p, ".grupo_duplicado", "faltantes")
  expect_false(.pmhc_dice(h$descripcion, "no se confirman"))
  # Con una fila repetida agregada a mano la marca deja de coincidir.
  otra <- rbind(r$datos, r$datos[1L, ])
  expect_false(.pmhc_grupo_ok(.pmhc_perfil(otra)))
})

test_that("comparar_perfiles() compara como cualquier columna una marca que no coincide", {
  set.seed(11)
  n <- 300L
  x <- rnorm(n)
  x[sample(n, 50L)] <- NA
  d <- data.frame(id = c(1:298, 5L, 7L), x = x,
                  g = sample(letters[1:4], n, TRUE), stringsAsFactors = FALSE)
  limpio <- .pmhc_aplicar(rbind(d, d[1:15, ]))
  antes <- .pmhc_perfil(limpio)
  editado <- limpio
  editado$.ausente_x[] <- TRUE
  despues <- .pmhc_perfil(editado, as.POSIXct("2026-02-01", tz = "UTC"))
  expect_identical(.pmhc_estado(despues, ".ausente_x"), "no_coincide")
  cmp <- suppressWarnings(suppressMessages(comparar_perfiles(antes, despues)))
  filas <- cmp[cmp$columna %in% ".ausente_x" & cmp$aspecto == "hallazgo", , drop = FALSE]
  expect_true(nrow(filas) > 0L)
  expect_false(any(as.character(filas$severidad) == "ok"))
  # Aparecer o desaparecer sigue siendo `ok` para una marca registrada.
  sin_marca <- limpio
  sin_marca$.ausente_x <- NULL
  cmp <- suppressWarnings(suppressMessages(comparar_perfiles(
    antes, .pmhc_perfil(sin_marca, as.POSIXct("2026-02-01", tz = "UTC"))
  )))
  filas <- cmp[cmp$columna %in% ".ausente_x", , drop = FALSE]
  expect_identical(as.character(filas$aspecto), "columna")
  expect_identical(as.character(filas$severidad), "ok")
  # Con el registro sin huella, como antes: el hallazgo de la marca sale `ok`.
  viejo <- editado
  attr(viejo, "marcas_limpieza") <- attr(editado, "marcas_limpieza")[, c("marca", "columna")]
  antes_viejo <- limpio
  attr(antes_viejo, "marcas_limpieza") <- attr(limpio, "marcas_limpieza")[, c("marca", "columna")]
  cmp <- suppressWarnings(suppressMessages(comparar_perfiles(
    .pmhc_perfil(antes_viejo), .pmhc_perfil(viejo, as.POSIXct("2026-02-01", tz = "UTC"))
  )))
  filas <- cmp[cmp$columna %in% ".ausente_x" & cmp$aspecto == "hallazgo", , drop = FALSE]
  expect_true(nrow(filas) > 0L)
  expect_true(all(as.character(filas$severidad) == "ok"))
})

test_that("el plan no actua sobre una marca que no coincide y dice por que", {
  set.seed(11)
  n <- 300L
  x <- rnorm(n)
  x[sample(n, 50L)] <- NA
  d <- data.frame(id = c(1:298, 5L, 7L), x = x,
                  g = sample(letters[1:4], n, TRUE), stringsAsFactors = FALSE)
  limpio <- .pmhc_aplicar(rbind(d, d[1:15, ]))
  editado <- limpio
  editado$.ausente_x[] <- TRUE
  p <- .pmhc_perfil(editado)
  plan <- .pmhc_plan(p, editado)
  expect_false(any(plan$columna %in% ".ausente_x"))
  sin_accion <- attr(plan, "hallazgos_sin_accion")
  motivo <- sin_accion$motivo[sin_accion$columna %in% ".ausente_x"]
  expect_true(length(motivo) > 0L)
  expect_true(all(.pmhc_dice(motivo, "no coincide con la que escribi")))
  # La marca intacta conserva su motivo.
  plan <- .pmhc_plan(.pmhc_perfil(limpio), limpio)
  sin_accion <- attr(plan, "hallazgos_sin_accion")
  motivo <- sin_accion$motivo[sin_accion$columna %in% ".grupo_duplicado"]
  expect_true(length(motivo) > 0L)
  expect_false(any(.pmhc_dice(motivo, "no coincide con la que escribi")))
})

test_that("la huella con su columna sobrevive a guardar texto con ausentes", {
  n <- 200L
  enie <- intToUtf8(c(0x45, 0x73, 0x70, 0x61, 0xf1, 0x61))
  zona <- rep(c("Norte", enie, "Sur", NA), length.out = n)
  d0 <- data.frame(id = seq_len(n), t = zona, stringsAsFactors = FALSE)
  limpio <- .pmhc_aplicar(d0)
  expect_true(".ausente_t" %in% names(limpio))
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  saveRDS(limpio, archivo)
  leido <- readRDS(archivo)
  expect_identical(.pmhc_estado(.pmhc_perfil(leido), ".ausente_t"), "coincide")
  # Ordenar la tabla cambia la huella.
  orden <- leido[rev(seq_len(nrow(leido))), , drop = FALSE]
  expect_identical(.pmhc_estado(.pmhc_perfil(orden), ".ausente_t"), "no_coincide")
  # El registro que publica un perfil no trae huella: se lee como uno viejo.
  pegado <- leido
  attr(pegado, "marcas_limpieza") <- .pmhc_perfil(leido)$general$marcas_limpieza
  expect_identical(.pmhc_estado(.pmhc_perfil(pegado), ".ausente_t"), "sin_huella")
})
