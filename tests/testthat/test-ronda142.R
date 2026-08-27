# Dos defectos que encontro una refutacion sobre los cambios de las rondas 139 y
# 140, y que ni la bateria de 180 comparaciones ni las 136 pruebas nuevas vieron,
# porque las dos usaban casos bien condicionados. Los dos aparecen en el borde.

# --- 1. La mediana no es el cuantil 0,5 hasta el ultimo bit -------------------
#
# `median()` promedia los dos centrales con `(a + b) / 2`; `quantile(type = 7)`
# interpola con `a + 0,5 * (b - a)`. Con centrales de magnitudes muy dispares
# redondean distinto, y la diferencia llegaba a la mediana informada. Ahorrar un
# recorrido no vale cambiar un numero que se publica.

test_that("la mediana informada es la de `median()`, hasta el ultimo bit", {
  casos <- list(
    c(-1000, 0.000111, 0.25, 1000),
    c(-1e6, 1e-6, 2e-6, 1e6),
    c(-2^40, 0.1, 0.3, 2^40),
    c(1, 2, 3, 4),
    c(1, 2, 3)
  )
  for (valores in casos) {
    perfil <- perfilar(data.frame(x = valores))
    expect_identical(
      perfil$columnas$mediana, stats::median(valores),
      info = paste(format(valores, digits = 3), collapse = ", ")
    )
  }
})

test_that("el caso exacto que lo delato sigue delatandolo", {
  valores <- c(-1000, 0.000111, 0.25, 1000)
  # Las dos formas difieren: si algun dia dejan de diferir, esta prueba deja de
  # proteger nada y hay que buscar otro caso.
  expect_false(identical(
    stats::median(valores),
    stats::quantile(valores, 0.5, names = FALSE, type = 7)
  ))
  expect_identical(
    perfilar(data.frame(x = valores))$columnas$mediana,
    stats::median(valores)
  )
})

# --- 2. La poda callaba el par que iguala el umbral ---------------------------
#
# La cota se escribia `d > n * (1 - umbral)`. `1 - 0.8` es 0,19999999999999996 y
# `5 * (1 - 0.8)` es 0,99999999999999978, asi que con `d = 1` la poda se
# disparaba y desaparecia un par con cumplimiento exactamente 0,8. El filtro de
# informe es `cumplimiento < umbral`, o sea que igualar el umbral SI se informa.

test_that("la poda no calla un par cuyo cumplimiento iguala el umbral", {
  datos <- data.frame(
    c4 = c("a", "a", "b", "b", "c"),
    c1 = c("p", "q", "r", "r", "s"),
    stringsAsFactors = FALSE
  )
  resultado <- detectar_dependencias(datos, umbral = 0.8, min_observaciones = 2L)
  fila <- resultado[
    resultado$determinante == "c4" & resultado$dependiente == "c1", ,
    drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$cumplimiento, 0.8)
})

test_that("ninguna poda descarta un par que todavia puede alcanzar el umbral", {
  # Propiedad, no ejemplo: si el cumplimiento maximo alcanzable es mayor o igual
  # que el umbral, podar es callar. Se recorren los umbrales que no son exactos
  # en punto flotante, que es donde la forma anterior fallaba.
  umbrales <- c(0.5, 0.6, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.99, 0.995, 0.999, 1)
  indebidas <- 0L
  for (n in c(2L, 3L, 5L, 7L, 10L, 20L, 50L, 100L, 333L)) {
    for (d in 0:min(n, 12L)) {
      for (umbral in umbrales) {
        maximo <- (n - d) / n
        # El dependiente tiene `d` formas mas que el determinante.
        poda <- lupa:::.poda_dependencia_cardinalidad(
          list(n = n, n_distintos = 1L), list(n = n, n_distintos = 1L + d),
          n, umbral
        )
        if (isTRUE(poda) && maximo >= umbral) indebidas <- indebidas + 1L
      }
    }
  }
  expect_identical(indebidas, 0L)
})
