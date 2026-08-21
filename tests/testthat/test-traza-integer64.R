# Un `bigint` de Postgres llega a R como `integer64`, y la trazabilidad lo
# rechazaba: el hallazgo se publicaba y la guarda avisaba que no habia con que
# nombrar las filas. Salio de correr el paquete contra bases reales, donde el
# aviso se atribuyo a las geometrias; la causa era el tipo, y afecta a cualquier
# columna `bigint`, que no tiene nada de espacial.

.columna_con_extremos <- function(x) {
  datos <- data.frame(id = seq_along(x))
  datos$cod <- x
  datos
}

.traza_de_outliers <- function(datos) {
  perfil <- suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE, max_filas_hallazgo = Inf
  ))
  fila <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "outliers" &
      perfil$hallazgos$columna == "cod",
  ]
  if (!nrow(fila)) return(NULL)
  list(traza = fila$trazabilidad[[1L]], afectados = fila$n_afectados[[1L]])
}

test_that("una columna integer64 se puede trazar igual que una doble", {
  skip_if_not_installed("bit64")
  set.seed(3)
  valores <- c(stats::rnorm(300, 100, 5), 5000, -4000)

  doble <- .traza_de_outliers(.columna_con_extremos(valores))
  entera <- .traza_de_outliers(.columna_con_extremos(as.integer(round(valores))))
  larga <- .traza_de_outliers(
    .columna_con_extremos(bit64::as.integer64(round(valores)))
  )

  for (caso in list(doble, entera, larga)) {
    expect_false(is.null(caso))
    expect_equal(caso$traza$estado, "disponible")
    expect_equal(length(caso$traza$indices_fila), caso$afectados)
  }
  # Y senala las mismas filas: el tipo no cambia cuales son los extremos.
  expect_setequal(larga$traza$indices_fila, doble$traza$indices_fila)
})

test_that("por encima de 2^53 no se entrega una traza que seria inexacta", {
  skip_if_not_installed("bit64")
  # La conversion a doble deja de ser exacta ahi, y dos valores distintos pueden
  # volverse el mismo. Una fila mal senalada es peor que una fila sin senalar.
  cuantitativos <- list(
    clase = "integer64",
    valores = bit64::as.integer64("9007199254740995") + bit64::as.integer64(0:9)
  )
  expect_null(.numerico_trazable(cuantitativos))

  chicos <- list(clase = "integer64", valores = bit64::as.integer64(1:10))
  expect_equal(.numerico_trazable(chicos), as.numeric(1:10))
})

test_that("el hallazgo de integer64 ya no dispara la guarda de incoherencia", {
  skip_if_not_installed("bit64")
  set.seed(3)
  valores <- c(stats::rnorm(300, 100, 5), 5000, -4000)
  datos <- .columna_con_extremos(bit64::as.integer64(round(valores)))
  expect_no_warning(
    perfilar(datos, analizar_dependencias = FALSE, max_filas_hallazgo = Inf)
  )
})
