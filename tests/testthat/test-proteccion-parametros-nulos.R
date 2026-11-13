# Los parametros de una accion sobre una columna protegida pasan por el barrido
# que tapa las fechas. Un parametro `NULL` -`cadenas_ausencia` sin declarar- se
# borraba en ese barrido y corria los que venian detras: con la `aplicabilidad`
# que el plan agrega al final, `planificar_limpieza()` y `analizar()` abortaban
# con "subindice fuera de los limites"; sin la regla, el plan protegido perdia
# el parametro sin avisar.
testthat::skip_on_cran()

.datos_parametro_nulo <- function() {
  data.frame(
    g = rep(c("si", "no"), 30),
    x = c(as.character(1:50), rep("S/D", 10)),
    stringsAsFactors = FALSE
  )
}

.perfil_parametro_nulo <- function(datos, proteger, reglas = NULL) {
  argumentos <- list(datos, fecha = "2026-01-15", analizar_dependencias = FALSE)
  if (!is.null(reglas)) argumentos$aplicabilidad <- reglas
  if (proteger) {
    argumentos$proteger_datos_personales <- TRUE
    argumentos$columnas_personales <- "x"
  }
  do.call(perfilar, argumentos)
}

.accion_de_x <- function(plan, estrategia) {
  which(plan$columna %in% "x" & plan$estrategia == estrategia)
}

test_that("una columna protegida con aplicabilidad y un parametro NULL se planifica", {
  datos <- .datos_parametro_nulo()
  reglas <- list(x = ~ g == "si")
  # Con y sin los datos: las dos llamadas pasaban por el mismo barrido.
  for (con_datos in c(FALSE, TRUE)) {
    planificar <- function(perfil) {
      if (con_datos) planificar_limpieza(perfil, datos) else planificar_limpieza(perfil)
    }
    plan <- planificar(.perfil_parametro_nulo(datos, TRUE, reglas))
    sin_proteger <- planificar(.perfil_parametro_nulo(datos, FALSE, reglas))

    # Las mismas acciones, y cada una con los mismos parametros, que sin
    # proteger: `cadenas_ausencia` sigue en su lugar, antes de la regla.
    expect_identical(
      paste(plan$columna, plan$estrategia),
      paste(sin_proteger$columna, sin_proteger$estrategia)
    )
    expect_identical(
      lapply(plan$parametros, names), lapply(sin_proteger$parametros, names)
    )
    i <- .accion_de_x(plan, "convertir_ausencias_textuales")
    expect_length(i, 1L)
    expect_identical(
      names(plan$parametros[[i]]),
      c("valores", "cadenas_ausencia", "aplicabilidad")
    )
    expect_null(plan$parametros[[i]]$cadenas_ausencia)

    # Y lo de la columna protegida sale tapado como sin la regla: fuera de la
    # `aplicabilidad`, cada accion de `x` trae los mismos parametros.
    sin_regla <- planificar(.perfil_parametro_nulo(datos, TRUE))
    for (k in which(plan$columna %in% "x")) {
      j <- .accion_de_x(sin_regla, plan$estrategia[[k]])
      expect_length(j, 1L)
      con <- plan$parametros[[k]]
      con$aplicabilidad <- NULL
      expect_identical(con, sin_regla$parametros[[j]])
    }
  }
})

test_that("la regla de aplicabilidad sale del barrido como entro y se ejecuta", {
  datos <- .datos_parametro_nulo()
  reglas <- list(x = ~ g == "si")
  plan <- planificar_limpieza(.perfil_parametro_nulo(datos, TRUE, reglas), datos)
  sin_proteger <- planificar_limpieza(
    .perfil_parametro_nulo(datos, FALSE, reglas), datos
  )
  i <- .accion_de_x(plan, "convertir_ausencias_textuales")
  expect_length(i, 1L)
  # La formula no es una lista: el barrido no la recorre ni la cambia.
  expect_identical(plan$parametros[[i]]$aplicabilidad, reglas$x)

  aplicado <- aplicar(plan, datos)
  referencia <- aplicar(sin_proteger, datos)
  expect_identical(aplicado$datos$x, referencia$datos$x)
  # Solo las `S/D` del universo declarado -las filas impares, `g == "si"`- se
  # convierten; las otras cinco quedan.
  expect_identical(sum(is.na(aplicado$datos$x)), 5L)
  expect_identical(sum(aplicado$datos$x %in% "S/D"), 5L)
  expect_true(all(is.na(aplicado$datos$x[seq(51L, 59L, by = 2L)])))
})

test_that("sin regla, la accion protegida conserva el parametro NULL", {
  datos <- .datos_parametro_nulo()
  plan <- planificar_limpieza(.perfil_parametro_nulo(datos, TRUE), datos)
  sin_proteger <- planificar_limpieza(.perfil_parametro_nulo(datos, FALSE), datos)
  expect_identical(
    lapply(plan$parametros, names), lapply(sin_proteger$parametros, names)
  )
  i <- .accion_de_x(plan, "convertir_ausencias_textuales")
  expect_length(i, 1L)
  expect_identical(names(plan$parametros[[i]]), c("valores", "cadenas_ausencia"))
})

test_that("el barrido de fechas conserva cada parametro NULL en su lugar", {
  fecha <- as.Date("2026-01-15")
  parametros <- list(
    primero = NULL, desde = fecha, medio = NULL,
    guarda = list(a = NULL, b = fecha, c = NULL),
    aplicabilidad = ~ g == "si", ultimo = NULL
  )
  tapados <- .proteger_fechas_parametros(parametros)

  expect_identical(names(tapados), names(parametros))
  expect_identical(
    vapply(tapados, is.null, logical(1L)),
    vapply(parametros, is.null, logical(1L))
  )
  expect_identical(names(tapados$guarda), c("a", "b", "c"))
  expect_null(tapados$guarda$a)
  expect_null(tapados$guarda$c)
  # Lo que el barrido tapa sigue tapado, con su clase.
  expect_s3_class(tapados$desde, "Date")
  expect_true(is.na(tapados$desde))
  expect_s3_class(tapados$guarda$b, "Date")
  expect_true(is.na(tapados$guarda$b))
  expect_identical(tapados$aplicabilidad, parametros$aplicabilidad)
})

test_that("analizar() con proteccion y una regla de aplicabilidad no aborta", {
  datos <- .datos_parametro_nulo()
  resultado <- analizar(
    datos, fecha = "2026-01-15",
    argumentos_perfil = list(
      analizar_dependencias = FALSE, aplicabilidad = list(x = ~ g == "si"),
      columnas_personales = "x"
    ),
    proteger_datos_personales = TRUE
  )
  plan <- resultado$plan_limpieza
  i <- .accion_de_x(plan, "convertir_ausencias_textuales")
  expect_length(i, 1L)
  expect_identical(
    names(plan$parametros[[i]]),
    c("valores", "cadenas_ausencia", "aplicabilidad")
  )
})
