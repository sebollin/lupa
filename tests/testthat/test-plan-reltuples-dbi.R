test_that("el denominador del catalogo reutiliza solo las hojas", {
  datos <- data.frame(
    relacion_oid = c("raiz", "hija", "hija"),
    reltuples = c(999, 1200, 1200),
    hoja = c(FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  filas <- .denominador_catalogo_dbi(datos)

  expect_true(filas$disponible)
  expect_equal(filas$filas, 1200)
  expect_equal(filas$n_relaciones, 1L)
  expect_identical(filas$fuente, "pg_class.reltuples")
  expect_match(filas$motivo, "estimacion de catalogo")
})

test_that("reltuples cero o negativo conserva sin dato filas", {
  for (valor in c(0, -1)) {
    filas <- .denominador_catalogo_dbi(data.frame(
      relacion_oid = "tabla", reltuples = valor, hoja = TRUE,
      stringsAsFactors = FALSE
    ))
    expect_false(filas$disponible)
    expect_true(is.na(filas$filas))
    expect_match(filas$motivo, "ANALYZE")
    expect_match(filas$motivo, "no se supone cero")
  }
})

test_that("las proyecciones del plan declaran el catalogo y no segundos", {
  prep <- list(
    metricas_ejecucion = c("moda", "mediana"),
    campos = c("id", "valor"), es_numerico = c(TRUE, TRUE),
    estimacion_derrame = list(
      columnas = data.frame(
        columna = c("id", "valor"),
        n_distintos_estimados = c(1000, 750),
        stringsAsFactors = FALSE
      )
    )
  )
  filas <- list(
    disponible = TRUE, estado = "estimado_catalogo", filas = 4500,
    fuente = "pg_class.reltuples", n_relaciones = 1L,
    motivo = "estimacion de catalogo"
  )
  proyecciones <- .proyecciones_plan_catalogo_dbi(prep, filas)

  expect_true(proyecciones$moda$disponible)
  expect_equal(proyecciones$moda$magnitud, 1750)
  expect_true(proyecciones$mediana$disponible)
  expect_equal(proyecciones$mediana$magnitud, 9000)
  expect_match(proyecciones$moda$motivo, "no es una duracion")
  expect_match(proyecciones$mediana$fuente, "pg_class.reltuples")
})

test_that("la proyeccion de moda reutiliza el catalogo ya leido por la estrategia", {
  preparacion <- list(
    metricas_ejecucion = "moda",
    campos = c("a", "b"),
    es_numerico = c(FALSE, TRUE),
    estimacion_derrame = list(columnas = data.frame()),
    fuentes_cardinalidad_costo = list(
      a = list(n_distintos = 20),
      b = list(n_distintos = 30)
    )
  )
  filas <- list(
    disponible = TRUE,
    estado = "estimado_catalogo",
    filas = 100,
    fuente = "pg_class.reltuples",
    motivo = "estimacion de catalogo"
  )

  proyeccion <- .proyecciones_plan_catalogo_dbi(preparacion, filas)$moda

  expect_true(proyeccion$disponible)
  expect_equal(proyeccion$magnitud, 50)
  expect_match(proyeccion$fuente, "pg_stats\\.n_distinct")
})

test_that("el plan sigue sin filas en motores sin catalogo", {
  preparacion <- list(
    estimacion_derrame = list(
      filas_catalogo = .denominador_catalogo_vacio_dbi("motor sin catalogo")
    ), estrategia_distintos = list(estado = "no_disponible")
  )
  filas <- .filas_plan_dbi(preparacion)

  expect_false(filas$disponible)
  expect_true(is.na(filas$filas))
  expect_match(filas$motivo, "motor sin catalogo")
})
