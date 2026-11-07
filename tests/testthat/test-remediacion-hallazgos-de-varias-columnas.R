# Un hallazgo de varias columnas -una relacion aritmetica, una de orden- no
# nombra UNA columna del perfil, y el plan lo declaraba en
# `hallazgos_sin_accion_por_columna_ambigua` con el motivo "el nombre de columna
# se repite en la tabla", aunque ninguna se repetia; tambien los `ok`.

fecha_plan <- as.POSIXct("2026-01-01", tz = "UTC")

sin_accion_de <- function(d, ...) {
  perfil <- suppressWarnings(perfilar(d, fecha = fecha_plan, ...))
  plan <- planificar_limpieza(perfil, d)
  list(perfil = perfil,
       ambigua = attr(plan, "hallazgos_sin_accion_por_columna_ambigua"),
       sin_accion = attr(plan, "hallazgos_sin_accion"))
}

aritmetica <- function(rota) {
  set.seed(2598)
  neto <- round(stats::runif(200, 100, 1000), 2)
  iva <- round(neto * 0.22, 2)
  total <- neto + iva
  if (rota) total[c(3, 50)] <- total[c(3, 50)] + 1
  data.frame(neto = neto, iva = iva, total = total)
}

test_that("una relacion aritmetica rota dice su razon y no se declara de columna ambigua", {
  r <- sin_accion_de(aritmetica(rota = TRUE))
  expect_true("neto,iva,total" %in% r$perfil$hallazgos$columna)
  expect_false("relacion_aritmetica_columnas" %in% r$ambigua$hallazgo)
  fila <- r$sin_accion[r$sin_accion$hallazgo == "relacion_aritmetica_columnas", ]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$motivo, "varias columnas (`neto`, `iva`, `total`)", fixed = TRUE)
  expect_false(grepl("se repite", fila$motivo, fixed = TRUE))
})

test_that("una relacion aritmetica ok no entra a ninguna lista sin accion", {
  r <- sin_accion_de(aritmetica(rota = FALSE))
  expect_true("relacion_aritmetica_columnas" %in% r$perfil$hallazgos$tipo_hallazgo)
  expect_false("relacion_aritmetica_columnas" %in% r$ambigua$hallazgo)
  expect_false("relacion_aritmetica_columnas" %in% r$sin_accion$hallazgo)
})

test_that("una relacion de orden rota tambien dice sus columnas", {
  set.seed(2599)
  inicio <- as.Date("2024-01-01") + sample(0:300, 200, TRUE)
  fin <- inicio + sample(0:30, 200, TRUE)
  fin[c(5, 9)] <- inicio[c(5, 9)] - 3
  r <- sin_accion_de(data.frame(inicio = inicio, fin = fin))
  expect_false("relacion_orden_columnas" %in% r$ambigua$hallazgo)
  fila <- r$sin_accion[r$sin_accion$hallazgo == "relacion_orden_columnas", ]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$motivo, "varias columnas (`inicio`, `fin`)", fixed = TRUE)
})

test_that("un hallazgo de la tabla no habla de 'esta columna'", {
  d <- data.frame(a = rep(1:10, each = 20), b = rep(1:19, length.out = 200),
                  v = seq_len(200) / 7)
  r <- sin_accion_de(d, clave = c("a", "b"))
  fila <- r$sin_accion[r$sin_accion$hallazgo == "clave_no_unica", ]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$motivo, "de la tabla y no de una columna", fixed = TRUE)
  expect_false(grepl("esta columna", fila$motivo, fixed = TRUE))
})

test_that("una columna que si se repite sigue declarada como ambigua", {
  d <- data.frame(a = c(" x", "y ", "z", rep("w", 17)), b = c(" x", "y ", "z", rep("w", 17)),
                  stringsAsFactors = FALSE)
  names(d) <- c("x", "x")
  r <- sin_accion_de(d)
  expect_true("espacios_sobrantes" %in% r$ambigua$hallazgo)
  expect_match(r$ambigua$motivo[[1L]], "se repite en la tabla", fixed = TRUE)
})

test_that("un hallazgo cuya columna no esta en el perfil lo dice, y no que se repite", {
  d <- data.frame(a = c(" x", "y ", "z", rep("w", 17)), b = seq_len(20) / 3,
                  stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(d, fecha = fecha_plan))
  perfil$hallazgos$columna[perfil$hallazgos$columna %in% "a"] <- "no_esta"
  amb <- lupa:::.hallazgos_sin_accion_por_columna_ambigua(perfil)
  fila <- amb[amb$columna == "no_esta", ]
  expect_gt(nrow(fila), 0L)
  expect_true(all(grepl("no est", fila$motivo, fixed = TRUE)))
  expect_false(any(grepl("se repite", fila$motivo, fixed = TRUE)))
})
