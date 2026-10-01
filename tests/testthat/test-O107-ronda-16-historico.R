# Ronda 16-A: el historico entre sesiones.

.evaluacion_O107 <- function(id = "corrida",
                             fecha = as.POSIXct("2026-01-01", tz = "UTC"),
                             maximo = 1e9) {
  m <- medir(
    modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "tab", "col")),
    data.frame(col = c(1, 2, NA)), id_medicion = id, fecha = fecha
  )
  # Un umbral con exponente y otro con decimales: `scipen` mueve la escritura del
  # primero y `OutDec` la del segundo. Con solo los dos primeros, `OutDec` no
  # cambiaba nada y su caso no probaba nada.
  evaluar(m, perfil_evaluacion("Basico", regla_evaluacion(
    "Rango", function(x, minimo, maximo, holgura) x >= minimo - holgura & x <= maximo,
    umbrales = list(minimo = 1e-7, maximo = maximo, holgura = 0.5)
  )))
}

test_that("la configuracion de una corrida no depende de scipen ni de OutDec", {
  base <- historico_calidad(.evaluacion_O107())
  for (opciones in list(list(scipen = 999), list(OutDec = ","))) {
    viejas <- options(opciones)
    misma <- .evaluacion_O107()
    options(viejas)
    # Antes: "Una corrida ya existente no coincide ... Difiere en:
    # configuracion_perfil".
    expect_no_error(acumular_historico(base, historico_calidad(misma)))
  }
  viejas <- options(scipen = 999)
  segunda <- .evaluacion_O107("corrida-2", as.POSIXct("2026-01-02", tz = "UTC"))
  options(viejas)
  deriva <- detectar_deriva_calidad(
    acumular_historico(base, historico_calidad(segunda))
  )
  expect_false(any(deriva$aspecto %in% "configuracion_perfil"))
  # Control: un umbral que de verdad cambia si es un cambio de configuracion.
  otra <- .evaluacion_O107("corrida-3", as.POSIXct("2026-01-03", tz = "UTC"),
                           maximo = 2e9)
  deriva <- detectar_deriva_calidad(
    acumular_historico(base, historico_calidad(otra))
  )
  expect_true(any(deriva$aspecto %in% "configuracion_perfil"))
})

test_that("guardar_historico no guarda adentro de un directorio", {
  historico <- historico_calidad(.evaluacion_O107())
  directorio <- tempfile("destino-")
  dir.create(directorio)
  on.exit(unlink(directorio, recursive = TRUE), add = TRUE)
  # Antes: devolvia la ruta del directorio y el RDS quedaba adentro con nombre
  # de temporal.
  expect_error(
    guardar_historico(historico, directorio, sobrescribir = TRUE),
    "es un directorio"
  )
  expect_length(list.files(directorio, all.files = TRUE, no.. = TRUE), 0L)
  # Control: un archivo dentro del directorio se guarda y se lee.
  archivo <- file.path(directorio, "historico.rds")
  guardar_historico(historico, archivo)
  expect_equal(nrow(leer_historico(archivo)), nrow(historico))
})

test_that("la deriva no dice que cambio un tipo cuando cambio el conjunto", {
  evaluacion <- function(id, fecha, dos_metricas) {
    datos <- data.frame(col = c(1, 2, 3, 4))
    i1 <- instanciar(especializar(metricas_nucleo()$NoNulo), "tab", "col")
    m <- if (dos_metricas) {
      i2 <- instanciar(especializar(metricas_nucleo()$NoNulo), "tab", "ausente")
      suppressWarnings(medir(modelo(i1, i2), datos, id_medicion = id, fecha = fecha))
    } else {
      medir(modelo(i1), datos, id_medicion = id, fecha = fecha)
    }
    evaluar(m, perfil_evaluacion("B", regla_evaluacion("R", function(x) x > 0)))
  }
  h <- historico_calidad(
    evaluacion("u1", as.POSIXct("2026-01-01", tz = "UTC"), TRUE),
    evaluacion("u2", as.POSIXct("2026-02-01", tz = "UTC"), FALSE)
  )
  fila <- detectar_deriva_calidad(h)
  fila <- fila[fila$aspecto == "configuracion_modelo", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  # Antes: "Cambio el tipo_resultado de una o mas metricas", que afirmaba un
  # cambio de tipo que no ocurrio.
  expect_false(grepl("tipo_resultado de una o mas metricas", fila$descripcion,
                     fixed = TRUE))
  expect_match(fila$descripcion, "o el conjunto de metricas", fixed = TRUE)
})

test_that("el diagnostico de una serie sin par nombra su tabla", {
  evaluacion <- function(id, entidad, columna) {
    datos <- stats::setNames(data.frame(c(1, 2, 3, 4)), columna)
    m <- medir(
      modelo(instanciar(especializar(metricas_nucleo()$NoNulo), entidad, columna)),
      datos, id_medicion = id, fecha = as.POSIXct("2026-01-01", tz = "UTC")
    )
    evaluar(m, perfil_evaluacion("Basico", regla_evaluacion(
      "Presente", function(x) x > 0
    )))
  }
  h <- historico_calidad(evaluacion("c1", "tabA", "colA"),
                         evaluacion("c2", "tabB", "colB"))
  diagnosticos <- attr(detectar_deriva_calidad(h), "cobertura_diagnosticos")
  sin_par <- diagnosticos[grepl("una sola medicion", diagnosticos$motivo), ]
  # Antes: dos filas identicas, "Basico" y "Basico".
  expect_equal(nrow(sin_par), 2L)
  expect_false(anyDuplicated(sin_par$columna) > 0L)
  expect_true(all(grepl("tabA|tabB", sin_par$columna)))
})
