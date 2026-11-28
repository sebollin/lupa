# Una columna es la marca de aplicar() solo si su huella -la que aplicar()
# guarda en el atributo `marcas_limpieza`- coincide: nadie la toco despues.
# Con la huella que coincide, el vacio de `.grupo_duplicado` es de diseno; con
# la que no coincide, la columna se diagnostica como cualquier otra.
testthat::skip_on_cran()

.pmhu_datos <- function(semilla = 2L) {
  set.seed(semilla)
  n <- 400L
  x <- rnorm(n)
  x[sample(n, 60L)] <- NA
  x[1:3] <- 50
  d <- data.frame(id = c(1:398, 5L, 7L), x = x,
                  g = sample(letters[1:4], n, TRUE), stringsAsFactors = FALSE)
  rbind(d, d[1:20, ])
}

.pmhu_resultado <- function(d0) {
  p0 <- suppressWarnings(suppressMessages(perfilar(d0)))
  plan <- suppressWarnings(suppressMessages(planificar_limpieza(p0, d0)))
  plan$aplicar <- as.logical(plan$recomendada)
  suppressWarnings(suppressMessages(aplicar(plan, d0)))
}

.pmhu_aplicar <- function(d0) .pmhu_resultado(d0)$datos

.pmhu_perfil <- function(d, fecha = as.POSIXct("2026-01-01", tz = "UTC")) {
  suppressWarnings(suppressMessages(perfilar(d, fecha = fecha)))
}

.pmhu_sobre <- function(p, columnas, tipo = NULL) {
  h <- p$hallazgos
  h <- h[as.character(h$columna) %in% columnas, , drop = FALSE]
  if (!is.null(tipo)) h <- h[as.character(h$tipo_hallazgo) %in% tipo, , drop = FALSE]
  h
}

.pmhu_grupo_ok <- function(p) {
  h <- .pmhu_sobre(p, ".grupo_duplicado", "faltantes")
  expect_true(nrow(h) > 0L)
  any(as.character(h$severidad) == "ok")
}

.pmhu_dice <- function(h, texto) {
  grepl(texto, as.character(h$descripcion), fixed = TRUE, useBytes = TRUE)
}

# Ninguno de los hallazgos que la columna tendria como columna del usuario
# baja a `ok`. Los que son `ok` por naturaleza -una sospecha de ausencia
# estructural o dependiente- pueden aparecer: es diagnosticarla como cualquiera.
.pmhu_sin_bajar <- function(h) {
  expect_true(nrow(h) > 0L)
  propios <- c("faltantes", "faltantes_disfrazados", "tipo_declarado_distinto",
               "constante")
  expect_false(any(as.character(h$severidad) == "ok" &
                     as.character(h$tipo_hallazgo) %in% propios))
}

.PMHU_MARCAS <- c(".fila_duplicada", ".grupo_duplicado", ".ausente_x", ".outlier_x")
.PMHU_AJENA <- "no coincide con la"
.PMHU_VENCIDA <- "no se confirman con las filas duplicadas"
.PMHU_VIGENTE <- "grupos siguen siendo los de los datos"

test_that("la marca de grupo intacta sale ok con su razon y el plan la lista", {
  d1 <- .pmhu_aplicar(.pmhu_datos())
  p1 <- .pmhu_perfil(d1)
  m <- p1$general$marcas_limpieza
  expect_setequal(m$marca, .PMHU_MARCAS)
  expect_true(all(m$estado == "coincide"))
  h <- .pmhu_sobre(p1, ".grupo_duplicado", "faltantes")
  expect_equal(nrow(h), 1L)
  expect_identical(as.character(h$severidad), "ok")
  expect_true(.pmhu_dice(h, .PMHU_VIGENTE))
  # Nada mas baja: ninguna otra descripcion habla de la marca.
  otros <- p1$hallazgos[!(p1$hallazgos$columna %in% ".grupo_duplicado" &
                            p1$hallazgos$tipo_hallazgo == "faltantes"), ]
  for (texto in c(.PMHU_AJENA, .PMHU_VENCIDA, .PMHU_VIGENTE)) {
    expect_false(any(.pmhu_dice(otros, texto)))
  }
  expect_true(any(as.character(p1$hallazgos$severidad) != "ok"))
  plan <- suppressWarnings(suppressMessages(planificar_limpieza(p1, d1)))
  sin_accion <- attr(plan, "hallazgos_sin_accion")
  expect_true(".grupo_duplicado" %in%
                sin_accion$columna[sin_accion$hallazgo == "faltantes"])
  expect_false(any(plan$columna %in% .PMHU_MARCAS))
})

test_that("la huella sobrevive a guardar lo limpio y a la clase de la tabla", {
  d0 <- .pmhu_datos()
  d1 <- .pmhu_aplicar(d0)
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  saveRDS(d1, archivo)
  expect_true(.pmhu_grupo_ok(.pmhu_perfil(readRDS(archivo))))
  dt <- .pmhu_aplicar(data.table::as.data.table(d0))
  expect_true(inherits(dt, "data.table"))
  expect_true(.pmhu_grupo_ok(.pmhu_perfil(dt)))
  skip_if_not_installed("tibble")
  tb <- .pmhu_aplicar(tibble::as_tibble(d0))
  expect_true(.pmhu_grupo_ok(.pmhu_perfil(tb)))
})

test_that("lo limpiado por una version anterior, sin huella, no cambia", {
  d1 <- .pmhu_aplicar(.pmhu_datos())
  viejo <- d1
  attr(viejo, "marcas_limpieza") <- attr(d1, "marcas_limpieza")[, c("marca", "columna")]
  p <- .pmhu_perfil(viejo)
  expect_true(all(p$general$marcas_limpieza$estado == "sin_huella"))
  expect_false(.pmhu_grupo_ok(p))
  expect_false(any(.pmhu_dice(p$hallazgos, .PMHU_AJENA)))
  expect_false(any(.pmhu_dice(p$hallazgos, .PMHU_VIGENTE)))
  # La ausencia estructural sigue apartando las marcas, como antes.
  expect_false(any(p$hallazgos$tipo_hallazgo == "posible_ausencia_estructural" &
                     p$hallazgos$columna %in% .PMHU_MARCAS))
})

test_that("filas agregadas despues de limpiar dejan de ser marcas", {
  d1 <- .pmhu_aplicar(.pmhu_datos())
  # Un lote con las marcas en NA.
  nuevo <- d1[1:120, , drop = FALSE]
  nuevo$id <- 5000L + seq_len(120L)
  nuevo$.fila_duplicada <- NA
  nuevo$.grupo_duplicado <- NA_integer_
  nuevo$.ausente_x <- NA
  d2 <- rbind(d1, nuevo)
  expect_false(is.null(attr(d2, "marcas_limpieza")))
  p <- .pmhu_perfil(d2)
  expect_true(all(p$general$marcas_limpieza$estado == "no_coincide"))
  h <- .pmhu_sobre(p, .PMHU_MARCAS, "faltantes")
  expect_setequal(h$columna, c(".fila_duplicada", ".grupo_duplicado", ".ausente_x"))
  .pmhu_sin_bajar(h)
  expect_true(all(.pmhu_dice(h, .PMHU_AJENA)))
  # Un lote con las marcas pasadas a texto y un "NA" escrito.
  texto <- d1[1:120, , drop = FALSE]
  texto$id <- 6000L + seq_len(120L)
  for (m in c(".fila_duplicada", ".grupo_duplicado", ".ausente_x")) {
    texto[[m]] <- as.character(texto[[m]])
  }
  texto$.ausente_x[1:60] <- "NA"
  h <- .pmhu_sobre(.pmhu_perfil(rbind(d1, texto)), .PMHU_MARCAS)
  .pmhu_sin_bajar(h)
  expect_true(all(.pmhu_dice(h, .PMHU_AJENA)))
  # Copias de las filas ya marcadas, con marcas completas: siguen escondidas
  # para la identidad de fila, pero la marca de grupo no sale ok.
  copias <- d1[match(1:15, d1$.grupo_duplicado), , drop = FALSE]
  copias$.fila_duplicada <- FALSE
  copias$.grupo_duplicado <- NA_integer_
  p <- .pmhu_perfil(rbind(d1, copias))
  expect_false(.pmhu_grupo_ok(p))
  expect_true(any(.pmhu_dice(.pmhu_sobre(p, ".grupo_duplicado"), .PMHU_AJENA)))
})

test_that("una columna de marca recreada o editada deja de ser marca", {
  d1 <- .pmhu_aplicar(.pmhu_datos())
  # Recreada con otro contenido.
  d2 <- d1
  d2$.ausente_x <- NULL
  d2$.ausente_x <- c(rep(NA, 250L), rep(TRUE, nrow(d2) - 250L))
  h <- .pmhu_sobre(.pmhu_perfil(d2), ".ausente_x", "faltantes")
  expect_equal(nrow(h), 1L)
  expect_false(as.character(h$severidad) == "ok")
  expect_true(.pmhu_dice(h, .PMHU_AJENA))
  # Recreada logica y sin NA, alternando dentro de cada grupo: es dato, entra
  # en la identidad de la fila, y los grupos ya no son duplicados.
  d3 <- d1
  d3$.ausente_x <- NULL
  d3$.ausente_x <- seq_len(nrow(d3)) > 410L
  p3 <- .pmhu_perfil(d3)
  expect_false(.pmhu_grupo_ok(p3))
  expect_true(any(.pmhu_dice(.pmhu_sobre(p3, ".grupo_duplicado", "faltantes"),
                             .PMHU_VENCIDA)))
  # Editada en su lugar.
  d4 <- d1
  d4$.ausente_x[] <- TRUE
  h <- .pmhu_sobre(.pmhu_perfil(d4), ".ausente_x", "constante")
  expect_equal(nrow(h), 1L)
  expect_false(as.character(h$severidad) == "ok")
  expect_true(.pmhu_dice(h, .PMHU_AJENA))
})

test_that("el atributo de otra ejecucion no vuelve marca a una columna del usuario", {
  ajeno <- attr(.pmhu_aplicar(.pmhu_datos(7L)), "marcas_limpieza")
  # Una columna del usuario con ausencias reales.
  d <- .pmhu_datos()
  d$.grupo_duplicado <- ifelse(d$id < 21L, 1L, NA_integer_)
  attr(d, "marcas_limpieza") <- ajeno
  p <- .pmhu_perfil(d)
  expect_false(.pmhu_grupo_ok(p))
  expect_true(all(.pmhu_dice(.pmhu_sobre(p, ".grupo_duplicado"), .PMHU_AJENA)))
  # Columnas que imitan la forma de las marcas sin ser duplicados.
  n <- 300L
  imitada <- data.frame(id = seq_len(n), valor = seq_len(n) * 3,
                        g = rep(c("a", "b", "c"), each = 100L))
  grupo <- rep(NA_integer_, n)
  grupo[c(1:30, 101:130, 201:230)] <- rep(1:45, each = 2L)
  imitada$.grupo_duplicado <- grupo
  imitada$.fila_duplicada <- !is.na(grupo) & duplicated(grupo)
  attr(imitada, "marcas_limpieza") <- ajeno
  expect_false(.pmhu_grupo_ok(.pmhu_perfil(imitada)))
  imitada$.grupo_duplicado <- NA_integer_
  imitada$.fila_duplicada <- FALSE
  expect_false(.pmhu_grupo_ok(.pmhu_perfil(imitada)))
})

test_that("con el atributo ajeno, la ausencia estructural se busca como siempre", {
  # La ausencia de `y` es al azar: solo la explica `.ausente_y`, una columna
  # del usuario.
  set.seed(5)
  n <- 300L
  d <- data.frame(id = sample(n), z = rnorm(n), y = rnorm(n))
  d$y[sample(n, 100L)] <- NA
  d$.ausente_y <- is.na(d$y)
  sin <- .pmhu_perfil(d)
  base <- .pmhu_sobre(sin, "y", "posible_ausencia_estructural")
  expect_equal(nrow(base), 1L)
  # Un atributo de otra tabla que nombra `.ausente_y`.
  otra <- data.frame(id = 1:200, y = c(rnorm(150), rep(NA, 50)))
  otra <- rbind(otra, otra[1:5, ])
  ajeno <- attr(.pmhu_aplicar(otra), "marcas_limpieza")
  expect_true(".ausente_y" %in% ajeno$marca)
  attr(d, "marcas_limpieza") <- ajeno
  con <- .pmhu_perfil(d)
  expect_equal(nrow(.pmhu_sobre(con, "y", "posible_ausencia_estructural")), 1L)
})

test_that("filas editadas despues de marcar dejan la marca con su severidad", {
  d1 <- .pmhu_aplicar(.pmhu_datos())
  expect_true(.pmhu_grupo_ok(.pmhu_perfil(d1)))
  primeras <- tapply(seq_len(nrow(d1)), d1$.grupo_duplicado, min)
  editada <- d1
  editada$g[primeras] <- "editada"
  p <- .pmhu_perfil(editada)
  expect_identical(
    p$general$marcas_limpieza$estado[p$general$marcas_limpieza$marca == ".grupo_duplicado"],
    "desactualizada"
  )
  expect_false(.pmhu_grupo_ok(p))
  expect_true(.pmhu_dice(.pmhu_sobre(p, ".grupo_duplicado", "faltantes"), .PMHU_VENCIDA))
  nuevo_id <- d1
  nuevo_id$id <- seq_len(nrow(nuevo_id))
  expect_false(.pmhu_grupo_ok(.pmhu_perfil(nuevo_id)))
})

test_that("subconjuntos y orden nuevo de las filas no salen ok", {
  d1 <- .pmhu_aplicar(.pmhu_datos())
  for (sub in list(d1[1:80, , drop = FALSE], d1[401:420, , drop = FALSE],
                   d1[rev(seq_len(nrow(d1))), , drop = FALSE])) {
    p <- .pmhu_perfil(sub)
    h <- .pmhu_sobre(p, .PMHU_MARCAS)
    expect_false(any(as.character(h$severidad) == "ok" & .pmhu_dice(h, .PMHU_VIGENTE)))
    expect_true(all(p$general$marcas_limpieza$estado == "no_coincide"))
    expect_true(any(.pmhu_dice(h, .PMHU_AJENA)))
  }
})

test_that("un plan que normaliza despues de marcar no deja una marca ok", {
  d0 <- data.frame(id = c(1L, 1L, 1L), texto = c("alfa", "alfa", "alfa "),
                   valor = c(10, 10, 10), stringsAsFactors = FALSE)
  r <- .pmhu_resultado(d0)
  expect_true(all(c("marcar_filas_duplicadas", "recortar_espacios") %in%
                    r$registro$estrategia[r$registro$estado == "ejecutada"]))
  p <- .pmhu_perfil(r$datos)
  expect_false(.pmhu_grupo_ok(p))
  h <- .pmhu_sobre(p, ".grupo_duplicado", "faltantes")
  expect_true(.pmhu_dice(h, .PMHU_VENCIDA))
  expect_false(.pmhu_dice(h, .PMHU_AJENA))
})

test_that("perfilar_por() lleva las marcas intactas a cada grupo y no las ajenas", {
  set.seed(11)
  x <- rnorm(300L)
  x[sample(300L, 50L)] <- NA
  d <- data.frame(id = c(1:298, 5L, 7L), x = x,
                  g = sample(letters[1:4], 300L, TRUE),
                  texto = sample(c("a", "b", "c"), 300L, TRUE))
  d1 <- .pmhu_aplicar(rbind(d, d[1:15, ]))
  por <- as.data.frame(suppressWarnings(suppressMessages(
    perfilar_por(d1, "g", min_filas = 30L)
  )))
  h <- por[por$columna == ".grupo_duplicado" & por$tipo_hallazgo == "faltantes", ,
           drop = FALSE]
  expect_true(nrow(h) > 0L)
  expect_true(all(as.character(h$severidad) == "ok"))
  # Las columnas del usuario que imitan las marcas, con un atributo ajeno: en
  # cada grupo son columnas del usuario, tambien para la ausencia estructural.
  n <- 300L
  imitada <- data.frame(id = seq_len(n), valor = seq_len(n) * 3,
                        g = rep(c("a", "b", "c"), each = 100L))
  grupo <- rep(NA_integer_, n)
  grupo[c(1:30, 101:130, 201:230)] <- rep(1:45, each = 2L)
  imitada$.grupo_duplicado <- grupo
  imitada$.fila_duplicada <- !is.na(grupo) & duplicated(grupo)
  sin_atributo <- as.data.frame(suppressWarnings(suppressMessages(
    perfilar_por(imitada, "g", min_filas = 30L)
  )))
  attr(imitada, "marcas_limpieza") <- attr(d1, "marcas_limpieza")
  con_atributo <- as.data.frame(suppressWarnings(suppressMessages(
    perfilar_por(imitada, "g", min_filas = 30L)
  )))
  sobre <- function(t) {
    t <- t[t$columna == ".grupo_duplicado", , drop = FALSE]
    paste(t$grupo, t$tipo_hallazgo, as.character(t$severidad))
  }
  expect_false(any(con_atributo$columna == ".grupo_duplicado" &
                     con_atributo$tipo_hallazgo == "faltantes" &
                     con_atributo$severidad == "ok"))
  expect_true(length(sobre(sin_atributo)) > 0L)
  expect_equal(length(sobre(con_atributo)), length(sobre(sin_atributo)))
  expect_setequal(sobre(con_atributo), sobre(sin_atributo))
})

test_that("comparar_perfiles() dice cuando la marca ya no es la que escribio aplicar()", {
  d0 <- .pmhu_datos()
  antes <- .pmhu_perfil(d0)
  d1 <- .pmhu_aplicar(d0)
  despues <- .pmhu_perfil(d1, fecha = as.POSIXct("2026-02-01", tz = "UTC"))
  completa <- suppressWarnings(suppressMessages(comparar_perfiles(antes, despues)))
  filas <- completa[completa$aspecto == "columna" & completa$columna %in% .PMHU_MARCAS, ]
  expect_equal(nrow(filas), 4L)
  expect_true(all(as.character(filas$severidad) == "ok"))
  expect_false(any(grepl("ya no coincide", filas$evidencia, fixed = TRUE, useBytes = TRUE)))
  algunas <- .pmhu_perfil(d1[1:200, , drop = FALSE],
                          fecha = as.POSIXct("2026-02-01", tz = "UTC"))
  parcial <- suppressWarnings(suppressMessages(comparar_perfiles(antes, algunas)))
  filas <- parcial[parcial$aspecto == "columna" & parcial$columna %in% .PMHU_MARCAS, ]
  expect_equal(nrow(filas), 4L)
  expect_true(all(as.character(filas$severidad) == "ok"))
  expect_true(all(grepl("ya no coincide", filas$evidencia, fixed = TRUE, useBytes = TRUE)))
})

test_that("aplicar() vuelve a firmar las marcas intactas y no las que cambiaron", {
  d1 <- .pmhu_aplicar(.pmhu_datos())
  # Una segunda limpieza sobre lo limpio: las marcas siguen coincidiendo.
  d2 <- .pmhu_aplicar(d1)
  p2 <- .pmhu_perfil(d2)
  expect_true(all(p2$general$marcas_limpieza$estado == "coincide"))
  # Una marca que llego cambiada no se firma de nuevo.
  cambiada <- d1
  cambiada$.ausente_x[1:5] <- !cambiada$.ausente_x[1:5]
  d3 <- .pmhu_aplicar(cambiada)
  estado <- .pmhu_perfil(d3)$general$marcas_limpieza
  expect_identical(estado$estado[estado$marca == ".ausente_x"], "no_coincide")
  # La marca de grupo se vuelve a firmar, pero `.ausente_x` cambiada es dato y
  # rompe sus grupos.
  expect_identical(estado$estado[estado$marca == ".grupo_duplicado"], "desactualizada")
})
