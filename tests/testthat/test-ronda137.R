# `muestra = Inf` es el valor por omision y significa "la tabla entera". Un plan
# ya no cuenta filas para decidir el costo, asi que la mitad del motor queda
# desconocida hasta que la corrida la mida. Los topes de la muestra acotan el
# trabajo que se hara en R, sin convertir esa cota en un conteo de la tabla.

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

test_that("una muestra sin conteo declara desconocida la mitad del motor", {
  skip_if_not_installed("RSQLite")
  datos <- .tabla_plan_137()
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)

  entera <- plan_perfilado_dbi(conexion, "t", muestra = Inf)
  explicita <- plan_perfilado_dbi(conexion, "t", muestra = nrow(datos))

  expect_true(is.na(attr(entera, "filas_leidas", exact = TRUE)))
  expect_equal(attr(entera, "pares_texto", exact = TRUE),
               2 * lupa:::.max_pares_vocabulario_dbi())
  expect_equal(attr(entera, "magnitud_motor", exact = TRUE), "desconocida")
  expect_equal(attr(entera, "magnitud", exact = TRUE), "desconocida")
  expect_true(is.na(attr(explicita, "filas_leidas", exact = TRUE)))
  expect_equal(attr(explicita, "pares_texto", exact = TRUE),
               2 * lupa:::.max_pares_vocabulario_dbi())
})

test_that("el bloque de muestra conserva la incertidumbre del motor", {
  skip_if_not_installed("RSQLite")
  datos <- .tabla_plan_137()
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)

  entera <- plan_perfilado_dbi(
    conexion, "t", universo = "tabla_completa", metricas = "validos",
    estrategia_mediana = "exacta", muestra = Inf
  )
  acotada <- plan_perfilado_dbi(
    conexion, "t", universo = "tabla_completa", metricas = "validos",
    estrategia_mediana = "exacta", muestra = 100
  )

  # Traer la tabla entera deja la mitad del motor desconocida, mientras que el
  # tope predeterminado de celdas acota los pares de formas del cliente.
  expect_true(is.na(attr(entera, "filas_leidas", exact = TRUE)))
  expect_equal(attr(entera, "pares_texto", exact = TRUE),
               2 * lupa:::.max_pares_vocabulario_dbi())
  expect_true(is.na(attr(acotada, "filas_leidas", exact = TRUE)))
  expect_equal(attr(acotada, "pares_texto", exact = TRUE), 9900)
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

# Una muestra finita mayor que la tabla tambien queda acotada en el lado del
# cliente. El motor sigue desconocido, porque el plan no pide el conteo solo
# para poder aplicar ese minimo.

test_that("pedir mas filas no cambia la incertidumbre del plan", {
  skip_if_not_installed("RSQLite")
  datos <- .tabla_plan_137()
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)

  entera <- plan_perfilado_dbi(conexion, "t", muestra = Inf)
  de_mas <- plan_perfilado_dbi(conexion, "t", muestra = nrow(datos) * 1000)

  expect_true(is.na(attr(entera, "filas_leidas", exact = TRUE)))
  expect_true(is.na(attr(de_mas, "filas_leidas", exact = TRUE)))
  expect_equal(attr(entera, "magnitud", exact = TRUE), "desconocida")
  expect_equal(attr(de_mas, "magnitud", exact = TRUE), "desconocida")
})

test_that("la equivalencia vale tambien con cada estrategia ortogonal", {
  skip_if_not_installed("RSQLite")
  datos <- .tabla_plan_137()
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)

  for (caso in c("aproximado", "seguro")) {
    entera <- do.call(
      plan_perfilado_dbi,
      c(list(conexion, "t", muestra = Inf),
        .argumentos_caso_dbi(caso, muestra = nrow(datos)))
    )
    explicita <- do.call(
      plan_perfilado_dbi,
      c(list(conexion, "t", muestra = nrow(datos)),
        .argumentos_caso_dbi(caso, muestra = nrow(datos)))
    )
    expect_true(is.na(attr(entera, "filas_leidas", exact = TRUE)), info = caso)
    expect_equal(attr(entera, "pares_texto", exact = TRUE),
                 2 * lupa:::.max_pares_vocabulario_dbi(), info = caso)
    expect_true(is.na(attr(explicita, "filas_leidas", exact = TRUE)), info = caso)
    expect_equal(attr(explicita, "pares_texto", exact = TRUE),
                 2 * lupa:::.max_pares_vocabulario_dbi(), info = caso)
  }
  # `muestra_motor` reemplaza el antiguo acoplamiento y exige un
  # tamano finito: la prueba vieja que usaba `muestra = Inf`
  # ya no describe una llamada valida. El contrato nuevo se prueba con un
  # valor explicito y conserva la incertidumbre del plan.
  muestreado <- plan_perfilado_dbi(
    conexion, "t", universo = "muestra_motor", muestra_motor = 100L,
    muestra = 100L, estrategia_mediana = "exacta"
  )
  expect_true(is.na(attr(muestreado, "filas_leidas", exact = TRUE)))
})

test_that("los bordes del tamano de muestra no rompen la cuenta", {
  plan <- data.frame(
    clase_consulta = "muestra", n_consultas = 1,
    alcance = "lee las filas pedidas", stringsAsFactors = FALSE
  )
  # `NaN` y un vector de largo dos no son "la tabla entera": caen en cero.
  for (borde in list(NaN, c(Inf, Inf), Inf, 0, 1e9)) {
    salida <- lupa:::.trabajo_plan_dbi(plan, filas = 500, muestra = borde,
                                       columnas_texto = 1)
    expect_true(is.finite(salida$filas_leidas))
    expect_true(is.finite(salida$pares_texto))
    expect_lte(salida$filas_leidas, 500)
  }
  # Una tabla vacia no declara trabajo, venga la muestra como venga.
  for (borde in list(Inf, 1000, 0)) {
    vacia <- lupa:::.trabajo_plan_dbi(plan, filas = 0, muestra = borde,
                                      columnas_texto = 2)
    expect_identical(vacia$filas_leidas, 0)
    expect_identical(vacia$pares_texto, 0)
  }
})

test_that("la equivalencia no depende de cuantas columnas de texto haya", {
  plan <- data.frame(
    clase_consulta = "muestra", n_consultas = 1,
    alcance = "lee las filas pedidas", stringsAsFactors = FALSE
  )
  for (n_texto in c(0, 1, 5)) {
    entera <- lupa:::.trabajo_plan_dbi(plan, filas = 800, muestra = Inf,
                                       columnas_texto = n_texto)
    explicita <- lupa:::.trabajo_plan_dbi(plan, filas = 800, muestra = 800,
                                          columnas_texto = n_texto)
    expect_identical(entera, explicita)
  }
})
