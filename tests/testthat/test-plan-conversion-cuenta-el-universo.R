# Una conversion de tipo cambia la columna entera -una columna tiene un solo
# tipo-, asi que el plan promete todos sus valores presentes: los del universo
# aplicable y los que quedan fuera. Una celda vacia fuera del universo es vacia
# por diseno: el perfil no la cuenta como faltante, y tampoco es un presente.
#
# El plan contaba `n - n_faltantes`. Con `aplicabilidad` declarada y nada fuera
# del universo, 30 celdas vacias fuera y 60 presentes dentro, prometia 90 y
# `aplicar()` cambiaba 60; con un solo presente fuera prometia 61, que es lo
# cierto, porque ese caso se contaba por otro camino. Lo mismo con
# `columnas_opcionales`, cuyo universo son las celdas con valor.

testthat::skip_on_cran()

.universo_conversion <- function(datos, ...) {
  perfil <- suppressWarnings(
    perfilar(datos, analizar_dependencias = FALSE, ...)
  )
  plan <- planificar_limpieza(perfil, datos)
  i <- which(as.character(plan$estrategia) %in% c(
    "convertir_numero_regional", "convertir_tipo", "convertir_fecha_confirmada"
  ) & plan$columna %in% setdiff(names(datos), "g"))
  expect_length(i, 1L)
  expect_identical(as.character(plan$estado[[i]]), "lista")
  plan$aplicar <- seq_len(nrow(plan)) == i
  resultado <- suppressWarnings(aplicar(plan, datos))
  list(
    estrategia = as.character(plan$estrategia[[i]]),
    prometidas = plan$n_afectadas[[i]],
    cambiadas = resultado$registro$n_cambiadas[[1L]],
    estado = resultado$registro$estado[[1L]]
  )
}

.universo_g <- function() c(rep("no", 30L), rep("si", 70L))

.universo_x <- function() {
  c(rep(NA, 30L), sprintf("%d.5", 1:60), rep(NA, 10L))
}

test_that("sin presentes fuera del universo, promete los presentes de dentro", {
  datos <- data.frame(g = .universo_g(), x = .universo_x(),
                      stringsAsFactors = FALSE)
  medido <- .universo_conversion(datos, aplicabilidad = list(x = ~ g == "si"))
  expect_identical(medido$estrategia, "convertir_numero_regional")
  expect_identical(medido$estado, "ejecutada")
  # A mano: 70 filas aplicables, 10 faltantes dentro, ningun presente fuera.
  expect_equal(medido$prometidas, 60)
  expect_equal(medido$cambiadas, 60)
})

test_that("con un presente fuera del universo, lo suma: la columna cambia entera", {
  x <- .universo_x()
  x[[1L]] <- "7.5"
  datos <- data.frame(g = .universo_g(), x = x, stringsAsFactors = FALSE)
  medido <- .universo_conversion(datos, aplicabilidad = list(x = ~ g == "si"))
  expect_identical(medido$estado, "ejecutada")
  expect_equal(medido$prometidas, 61)
  expect_equal(medido$cambiadas, 61)
})

test_that("un presente donde la regla no se pudo decidir tambien cambia", {
  g <- .universo_g()
  g[[2L]] <- NA
  x <- .universo_x()
  x[[2L]] <- "8.5"
  datos <- data.frame(g = g, x = x, stringsAsFactors = FALSE)
  medido <- .universo_conversion(datos, aplicabilidad = list(x = ~ g == "si"))
  expect_identical(medido$estado, "ejecutada")
  # 60 dentro y el de la fila indeterminada: las 29 vacias de fuera no.
  expect_equal(medido$prometidas, 61)
  expect_equal(medido$cambiadas, 61)
})

test_that("una columna opcional no promete sus celdas vacias", {
  y <- sprintf("%d.5", 1:100)
  y[seq(1L, 100L, by = 4L)] <- NA
  datos <- data.frame(y = y, stringsAsFactors = FALSE)
  medido <- .universo_conversion(datos, columnas_opcionales = "y")
  expect_identical(medido$estrategia, "convertir_numero_regional")
  expect_identical(medido$estado, "ejecutada")
  expect_equal(medido$prometidas, 75)
  expect_equal(medido$cambiadas, 75)
})

test_that("las otras conversiones de tipo cuentan igual", {
  f <- format(as.Date("2021-03-13") + 0:99, "%Y-%m-%d")
  f[c(1:30, 91:100)] <- NA
  datos <- data.frame(g = .universo_g(), f = f, stringsAsFactors = FALSE)
  medido <- .universo_conversion(datos, aplicabilidad = list(f = ~ g == "si"))
  expect_identical(medido$estrategia, "convertir_tipo")
  expect_equal(medido$prometidas, 60)
  expect_equal(medido$cambiadas, 60)

  # Sin universo declarado, la cuenta es la de siempre: todos los presentes.
  datos <- data.frame(f = f, stringsAsFactors = FALSE)
  medido <- .universo_conversion(datos)
  expect_equal(medido$prometidas, 60)
  expect_equal(medido$cambiadas, 60)
})
