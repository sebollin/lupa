# Dos silencios de la aritmetica en importes grandes: la suma con cancelacion,
# cuyas columnas varian menos que la tolerancia relativa de su magnitud, y la
# proporcionalidad con la respuesta redondeada, cuya tolerancia relativa traga
# los centavos; y lo que la cota de la proporcionalidad tiene que respetar: el
# redondeo que cae justo en la mitad y la direccion de la constante.

# Pesada para CRAN (mas de 0,2 s en la medicion sin red del 2026-11-10): corre en
# la suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

escrito <- function(v, decimales = 2L) {
  as.numeric(sprintf(paste0("%.", decimales, "f"), v))
}

relaciones <- function(perfil) {
  h <- perfil$hallazgos
  h[h$tipo_hallazgo == "relacion_aritmetica_columnas", , drop = FALSE]
}

cobertura_aritmetica <- function(perfil) {
  cb <- perfil$cobertura_diagnosticos
  cb[cb$diagnostico == "relacion_aritmetica_columnas", , drop = FALSE]
}

perfil_sin_dependencias <- function(d) {
  perfilar(d, analizar_dependencias = FALSE,
           fecha = as.POSIXct("2026-01-01", tz = "UTC"))
}

cancelacion <- function(grande, rotas = 0L, semilla = 7L) {
  set.seed(semilla)
  n <- 300L
  x <- round(stats::runif(n, -1e4, 1e4)) / 100
  y <- round(stats::runif(n, -1e4, 1e4)) / 100
  total <- x + y
  i <- sort(sample(n, rotas))
  total[i] <- total[i] + 0.01 * sample(c(-1, 1, 30), rotas, TRUE)
  list(datos = data.frame(a = escrito(grande + x), b = escrito(-grande + y),
                          total = escrito(total)),
       rotas = i)
}

test_that("una suma con cancelacion en centavos sobre 1e11 se evalua con la mitad del cuanto", {
  sana <- relaciones(perfil_sin_dependencias(cancelacion(1e11)$datos))
  expect_equal(nrow(sana), 1L)
  expect_equal(as.character(sana$severidad), "ok")
  expect_equal(sana$n_afectados, 0)
  expect_match(sana$evidencia, "min(1e-08", fixed = TRUE)
  c_rota <- cancelacion(1e11, rotas = 30L)
  rota <- relaciones(perfil_sin_dependencias(c_rota$datos))
  expect_equal(nrow(rota), 1L)
  expect_equal(rota$n_afectados, 30)
  expect_equal(sort(rota$trazabilidad[[1L]]$indices_fila), c_rota$rotas)
})

test_that("una relacion que no se puede evaluar exacta deja su fila de cobertura", {
  set.seed(7)
  x <- stats::runif(300, -100, 100)
  y <- stats::runif(300, -100, 100)
  perfil <- perfil_sin_dependencias(
    data.frame(a = 1e12 + x, b = -1e12 + y, c = x + y)
  )
  expect_equal(nrow(relaciones(perfil)), 0L)
  cb <- cobertura_aritmetica(perfil)
  expect_setequal(cb$columna, c("a", "b"))
  expect_true(all(grepl("La tolerancia relativa reconoc", cb$motivo, fixed = TRUE)))
  expect_true(all(grepl("a + b ~= c", cb$motivo, fixed = TRUE)))
  expect_true(all(grepl("1e-08 * 1000000000000 = 10000", cb$motivo, fixed = TRUE)))
  expect_true(all(grepl("tolerancia_aritmetica", cb$como_resolverlo, fixed = TRUE)))
  # Una columna constante de verdad sigue sin fila: el perfil ya lo dice.
  constante <- perfil_sin_dependencias(
    data.frame(a = rep(5, 300), b = x, c = x + 5)
  )
  expect_equal(nrow(cobertura_aritmetica(constante)), 0L)
})

producto <- function(escala, rotas = 0L, semilla = 11L) {
  set.seed(semilla)
  n <- 300L
  neto <- escrito(stats::runif(n, escala / 10, escala))
  iva <- escrito(neto * 0.22)
  i <- sort(sample(n, rotas))
  iva[i] <- escrito(iva[i] + 0.01 * sample(c(-1, 1, 30), rotas, TRUE))
  list(datos = data.frame(neto = neto, iva = iva), rotas = i)
}

test_that("un producto redondeado a centavos sobre importes grandes cuenta los centavos rotos", {
  for (escala in c(1e7, 1e9, 1e11)) {
    sano <- relaciones(perfil_sin_dependencias(producto(escala)$datos))
    expect_equal(nrow(sano), 1L, info = escala)
    expect_equal(as.character(sano$severidad), "ok", info = escala)
    expect_equal(sano$n_afectados, 0, info = escala)
    p_roto <- producto(escala, rotas = 30L)
    roto <- relaciones(perfil_sin_dependencias(p_roto$datos))
    expect_equal(nrow(roto), 1L, info = escala)
    expect_equal(sort(roto$trazabilidad[[1L]]$indices_fila), p_roto$rotas,
                 info = escala)
    expect_match(roto$descripcion, "neto * 0.22", fixed = TRUE, info = escala)
    expect_match(roto$evidencia, "<= 0.005: la mitad del cuanto decimal de `iva`",
                 fixed = TRUE, info = escala)
  }
})

test_that("con la columna redondeada como base se lee al reves, con la constante corta", {
  set.seed(17)
  n <- 300L
  dolares <- escrito(stats::runif(n, 1e6, 1e7))
  pesos <- escrito(dolares * 39.8734)
  i <- sort(sample(n, 30L))
  pesos[i] <- escrito(pesos[i] + 0.01 * sample(c(-1, 1, 30), 30L, TRUE))
  rel <- relaciones(perfil_sin_dependencias(data.frame(dolares = dolares, pesos = pesos)))
  expect_equal(nrow(rel), 1L)
  expect_equal(sort(rel$trazabilidad[[1L]]$indices_fila), i)
  expect_match(rel$descripcion, "pesos ~= dolares * 39.8734", fixed = TRUE)
  expect_match(rel$evidencia, "<= 0.005: la mitad del cuanto decimal de `pesos` (0.01)",
               fixed = TRUE)
})

test_that("con mas de la mitad de los centavos rotos la proporcionalidad se descarta y se cuenta", {
  p <- producto(1e10)
  iva <- p$datos$iva
  set.seed(3)
  j <- sample(length(iva), 120L)
  iva[j] <- escrito(iva[j] + 0.01)
  r <- lupa:::.detectar_aritmetica_columnas(data.frame(neto = p$datos$neto, iva = iva))
  expect_length(r$hallazgos, 0L)
  expect_equal(r$alcance$proporcionalidades_descartadas_cuanto, 1)
})

test_that("las filas acusadas de un redondeo independiente son las que no son el producto redondeado", {
  set.seed(23)
  exacta <- stats::runif(300, 1e8, 1e9)
  neto <- escrito(exacta)
  iva <- escrito(exacta * 0.22)
  rel <- relaciones(perfil_sin_dependencias(data.frame(neto = neto, iva = iva)))
  expect_equal(nrow(rel), 1L)
  # A mano: las filas donde el iva guardado no es el redondeo del neto guardado.
  esperadas <- which(abs(iva - neto * 0.22) > 0.005 + 1e-6)
  expect_gt(length(esperadas), 0L)
  expect_equal(sort(rel$trazabilidad[[1L]]$indices_fila), esperadas)
})

test_that("las proporcionalidades redondeadas a cifras significativas siguen publicandose sanas", {
  set.seed(13)
  for (escala in c(1e4, 1e8, 1e12)) {
    base <- stats::runif(300, escala / 100, escala)
    for (cifras in c(9L, 11L)) {
      rel <- relaciones(perfil_sin_dependencias(
        data.frame(base = signif(base, cifras), resp = signif(base * 0.22, cifras))
      ))
      info <- paste(escala, cifras)
      expect_equal(nrow(rel), 1L, info = info)
      expect_equal(rel$n_afectados, 0, info = info)
    }
  }
})

test_that("con enteros y un redondeo que cae justo en la mitad no se acusa la fila sana", {
  set.seed(1221)
  n <- 300L
  base <- round(exp(stats::runif(n, log(10), log(1e8))))
  # Veinte bases con `base * 1.22` terminado en .5: `4075 * 1.22 = 4971.5`.
  mitad <- sample(n, 20L)
  base[mitad] <- base[mitad] - base[mitad] %% 50 + 25
  resp <- round(base * 1.22)
  expect_true(all(abs(abs(resp[mitad] - base[mitad] * 1.22) - 0.5) < 1e-6))
  sana <- relaciones(perfil_sin_dependencias(data.frame(base = base, resp = resp)))
  expect_equal(nrow(sana), 1L)
  expect_equal(as.character(sana$severidad), "ok")
  expect_equal(sana$n_afectados, 0)
  # Leida en la direccion de la constante corta.
  expect_match(sana$descripcion, "resp ~= base * 1.22", fixed = TRUE)
  rotas <- sort(sample(setdiff(seq_len(n), mitad), 5L))
  rota <- resp
  rota[rotas] <- rota[rotas] + c(-1, 1, 1, -1, 1)
  rel <- relaciones(perfil_sin_dependencias(data.frame(base = base, resp = rota)))
  expect_equal(nrow(rel), 1L)
  expect_equal(sort(rel$trazabilidad[[1L]]$indices_fila), rotas)
})

test_that("con dos constantes cortas manda la resolucion mas fina", {
  set.seed(25)
  n <- 300L
  base <- round(exp(stats::runif(n, log(10), log(1e8))))
  resp <- round(base * 2.5)
  # Rotas sobre bases pares, donde el producto es entero y un corrimiento de
  # uno se distingue del redondeo.
  pares <- which(base %% 2 == 0)
  rotas <- sort(sample(pares, 5L))
  resp[rotas] <- resp[rotas] + c(1, -1, 1, 1, -1)
  rel <- relaciones(perfil_sin_dependencias(data.frame(base = base, resp = resp)))
  expect_equal(nrow(rel), 1L)
  expect_match(rel$descripcion, "resp ~= base * 2.5", fixed = TRUE)
  expect_equal(sort(rel$trazabilidad[[1L]]$indices_fila), rotas)
})

test_that("un total calculado en R con cancelacion conserva sus centavos", {
  set.seed(7)
  n <- 300L
  x <- round(stats::runif(n, -1e4, 1e4)) / 100
  y <- round(stats::runif(n, -1e4, 1e4)) / 100
  # Sumado en doble, sin pasar por texto: `0.30 - 0.29` no es el doble de 0.01.
  total <- x + y
  i <- sort(sample(n, 30L))
  total[i] <- total[i] + 0.01 * sample(c(-1, 1, 30), 30L, TRUE)
  expect_equal(lupa:::.cuanto_decimal_aritmetica(total), 0.01)
  rel <- relaciones(perfil_sin_dependencias(data.frame(a = 1e8 + x, b = -1e8 + y,
                                                       total = total)))
  expect_equal(nrow(rel), 1L)
  expect_equal(sort(rel$trazabilidad[[1L]]$indices_fila), i)
  expect_match(rel$evidencia, "min(1e-08", fixed = TRUE)
})
