# `muestra = Inf` es el valor por omision y significa "la tabla entera". El plan
# lo contaba como CERO: `.trabajo_plan_dbi()` normaliza sus entradas con un
# `numero()` que rechaza todo lo no finito -correcto para `filas` y los conteos,
# que con `Inf` hacian NaN- y el `NA` resultante caia en `muestra <- 0`.
#
# Consecuencia medida sobre 200.000 x 4: `muestra = Inf` declaraba 400.000
# lecturas y 0 pares de formas, y `muestra = 200000` -que pide exactamente las
# mismas filas- declaraba 600.000 y 4.000.000. El bloque de muestra se contaba
# como cero filas leidas en los dos lados. Y como la magnitud se decide sobre
# esos numeros, el caso por omision caia en "baja", que es justo la que no
# imprime las palancas para bajar el costo.
#
# Lo delata pedir las mismas filas de las dos maneras: si `Inf` y `nrow` no
# declaran lo mismo, una de las dos esta mal.

.tabla_plan_137 <- function() {
  set.seed(137L)
  n <- 5000L
  data.frame(
    id = seq_len(n),
    texto = sample(paste0("forma_", 1:80), n, TRUE),
    num = stats::rnorm(n),
    bin = sample(c("S", "N"), n, TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("`muestra = Inf` declara el mismo trabajo que pedir todas las filas", {
  skip_if_not_installed("RSQLite")
  datos <- .tabla_plan_137()
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)

  entera <- plan_perfilado_dbi(conexion, "t", muestra = Inf)
  explicita <- plan_perfilado_dbi(conexion, "t", muestra = nrow(datos))

  for (campo in c("filas_leidas", "pares_texto", "magnitud",
                  "magnitud_texto", "magnitud_motor")) {
    expect_identical(
      attr(entera, campo, exact = TRUE),
      attr(explicita, campo, exact = TRUE),
      info = campo
    )
  }
})

test_that("el bloque de muestra no se cuenta como cero filas leidas", {
  skip_if_not_installed("RSQLite")
  datos <- .tabla_plan_137()
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)

  entera <- plan_perfilado_dbi(conexion, "t", modo = "conteos", muestra = Inf)
  acotada <- plan_perfilado_dbi(conexion, "t", modo = "conteos", muestra = 100)

  # Traer 5.000 filas no puede declarar menos trabajo que traer 100.
  expect_gt(attr(entera, "filas_leidas", exact = TRUE),
            attr(acotada, "filas_leidas", exact = TRUE))
  expect_gt(attr(entera, "pares_texto", exact = TRUE),
            attr(acotada, "pares_texto", exact = TRUE))
  # Y la diferencia es exactamente las filas de mas que se traen.
  expect_equal(
    attr(entera, "filas_leidas", exact = TRUE) -
      attr(acotada, "filas_leidas", exact = TRUE),
    nrow(datos) - 100
  )
})

test_that("la traduccion de `Inf` no ablanda la guarda de valores invalidos", {
  # `numero()` sigue rechazando lo que no es un conteo utilizable. Solo el
  # infinito POSITIVO significa "la tabla entera"; el resto cae en cero como
  # antes, y ninguno rompe el plan.
  plan <- data.frame(
    clase_consulta = "muestra", n_consultas = 1,
    alcance = "lee las filas pedidas", stringsAsFactors = FALSE
  )
  referencia <- lupa:::.trabajo_plan_dbi(plan, filas = 1000, muestra = 1000,
                                         columnas_texto = 1)
  entera <- lupa:::.trabajo_plan_dbi(plan, filas = 1000, muestra = Inf,
                                     columnas_texto = 1)
  expect_identical(entera$filas_leidas, referencia$filas_leidas)

  for (invalida in list(-Inf, NA_real_, "muchas", NULL, -5)) {
    salida <- lupa:::.trabajo_plan_dbi(plan, filas = 1000, muestra = invalida,
                                       columnas_texto = 1)
    expect_true(is.finite(salida$filas_leidas))
    expect_true(is.finite(salida$pares_texto))
    # Un valor invalido no puede declarar el trabajo de la tabla entera.
    expect_lt(salida$filas_leidas, referencia$filas_leidas)
  }
})
