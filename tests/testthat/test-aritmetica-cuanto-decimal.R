# La tolerancia relativa por omision (1e-8) vale un peso sobre importes de 1e8:
# una identidad aditiva rota por centavos salia `ok 0`, "en todas las filas
# comparables", con los mismos datos que en importes de 1e4 salian rotos. Con
# decimales fijos en las tres columnas, una diferencia de medio cuanto o mas
# esta en el dato y no en la representacion.

importes_con_centavos_rotos <- function(escala, semilla = 7L) {
  set.seed(semilla)
  n <- 300L
  bruto <- round(stats::runif(n, 0.9, 1.1) * escala, 2)
  comision <- round(stats::runif(n, 1, 50), 2)
  error <- c(rep(0, n - 30L), rep(c(-0.01, 0.01, 0.3), 10L))
  neto <- round(bruto - comision + error, 2)
  list(
    datos = data.frame(bruto = bruto, comision = comision, neto = neto),
    # A mano, en centavos enteros: ninguna tolerancia interviene.
    rotas = which(round(neto * 100) != round((bruto - comision) * 100))
  )
}

relacion_aritmetica <- function(perfil) {
  h <- perfil$hallazgos
  h[h$tipo_hallazgo == "relacion_aritmetica_columnas", , drop = FALSE]
}

test_that("los centavos rotos se cuentan igual en cualquier escala", {
  for (escala in c(1e4, 1e6, 1e8)) {
    caso <- importes_con_centavos_rotos(escala)
    expect_length(caso$rotas, 30L)
    rel <- relacion_aritmetica(
      perfilar(caso$datos, analizar_dependencias = FALSE)
    )
    expect_equal(nrow(rel), 1L)
    expect_equal(as.character(rel$severidad), "sospechoso")
    expect_equal(rel$n_afectados, 30)
    expect_equal(sort(rel$trazabilidad[[1L]]$indices_fila), caso$rotas)
  }
})

test_that("la evidencia declara la cota cuando acota la tolerancia", {
  caso <- importes_con_centavos_rotos(1e8)
  perfil <- perfilar(caso$datos, analizar_dependencias = FALSE)
  rel <- relacion_aritmetica(perfil)
  expect_match(rel$evidencia, "Tolerancia declarada: 1e-08", fixed = TRUE)
  expect_match(
    rel$evidencia,
    "min(1e-08 * max(1, |observado|, |esperado|), 0.005)", fixed = TRUE
  )
  expect_match(rel$evidencia, "(0.01)", fixed = TRUE)
  expect_match(
    perfil$meta$aritmetica_columnas$criterio_tolerancia, "cuanto decimal",
    fixed = TRUE
  )
  # Sobre importes chicos la cota no acota nada y el texto es el de siempre.
  chico <- relacion_aritmetica(perfilar(
    importes_con_centavos_rotos(1e4)$datos, analizar_dependencias = FALSE
  ))
  expect_false(grepl("min(", chico$evidencia, fixed = TRUE))
})

test_that("una tolerancia declarada no se acota", {
  caso <- importes_con_centavos_rotos(1e8)
  d <- caso$datos
  rel <- relacion_aritmetica(perfilar(
    d, analizar_dependencias = FALSE, tolerancia_aritmetica = 2e-8
  ))
  esperado <- d$comision + d$neto
  fuera <- which(abs(d$bruto - esperado) >
                   2e-8 * pmax(1, abs(d$bruto), abs(esperado)))
  expect_lt(length(fuera), 30L)
  expect_equal(rel$n_afectados, length(fuera))
  expect_false(grepl("min(", rel$evidencia, fixed = TRUE))
})

test_that("las relaciones verdaderas siguen publicandose sin filas rotas", {
  set.seed(31)
  n <- 400L
  escala <- 1e8
  a <- round(stats::runif(n, 0.5, 1) * escala, 2)
  b <- round(stats::runif(n, 0.01, 0.1) * escala, 2)
  u <- stats::runif(n, 0.5, 1) * escala
  v <- stats::runif(n, 0.01, 0.1) * escala
  quince <- function(x) as.numeric(format(x, digits = 15))
  nueve <- function(x) signif(x, 9)
  tablas <- list(
    suma_centavos = data.frame(a = a, b = b, total = a + b),
    resta_calculada = data.frame(bruto = a, comision = b, neto = a - b),
    texto_quince_cifras = data.frame(u = quince(u), v = quince(v),
                                     total = quince(u + v)),
    nueve_cifras = data.frame(u = nueve(u), v = nueve(v),
                              total = nueve(u + v)),
    iva_redondeado = data.frame(neto = a, iva = round(a * 0.22, 2))
  )
  for (nombre in names(tablas)) {
    rel <- relacion_aritmetica(
      perfilar(tablas[[nombre]], analizar_dependencias = FALSE)
    )
    expect_gte(nrow(rel), 1L)
    expect_true(all(as.character(rel$severidad) == "ok"), info = nombre)
    expect_true(all(rel$n_afectados == 0), info = nombre)
  }
})

test_that("el cuanto se reconoce en decimales fijos y no en texto de 15 cifras", {
  set.seed(32)
  cuanto <- lupa:::.cuanto_decimal_aritmetica
  expect_equal(cuanto(round(stats::runif(200, 1, 1e8), 2)), 0.01)
  expect_equal(cuanto(as.double(sample(1e6, 200))), 1)
  expect_equal(cuanto(1:10), 1)
  expect_true(is.na(cuanto(as.numeric(format(stats::runif(200), digits = 15)))))
  expect_true(is.na(cuanto(stats::runif(200) * 1e8)))
  # Centavos sobre 1e12 tienen 14 cifras: su ultima cifra ya no se distingue
  # del ruido de un texto de 15, y no se acota.
  expect_true(is.na(cuanto(round(stats::runif(200, 1, 2) * 1e12, 2))))
  expect_true(is.na(cuanto(numeric())))
  expect_identical(
    formals(perfilar)$tolerancia_aritmetica,
    lupa:::.TOLERANCIA_ARITMETICA_OMISION
  )
})

test_that("una identidad descartada por escala deja su conteo en el alcance", {
  set.seed(77)
  a <- stats::runif(200, 1e9, 2e9)
  b <- stats::runif(200, -5, 5)
  perfil <- perfilar(data.frame(a = a, b = b, t = a),
                     analizar_dependencias = FALSE)
  expect_equal(nrow(relacion_aritmetica(perfil)), 0L)
  alcance <- perfil$meta$aritmetica_columnas
  expect_false(is.null(alcance))
  expect_gte(alcance$identidades_descartadas_escala, 1)
  expect_equal(alcance$resolucion_exacta, 4 * .Machine$double.eps)
})
