# Un resumen recortado a algunas columnas (`x[, c("atributo", "estado_cuentas")]`)
# sigue siendo un `medicion_resumen`, y su impresion lista las cuentas que no
# cierran con el control de lecturas de cada una. Ese control se lee de
# `attr(x, "lecturas")` con `id_medicion`, `entidad` y `entidad_referida`: sin
# alguna de ellas, la impresion abortaba con "argumento tiene longitud cero".
# Imprimir nunca aborta: sin esas columnas el control es `desconocido`.

.recortado_partes <- function() {
  nucleo <- metricas_nucleo()
  d1 <- data.frame(x = c(1, NA, 3), y = c(1, 2, NA))
  d2 <- d1
  d2$x[2] <- 2
  m1 <- modelo(instanciar(especializar(nucleo$NoNulo, "X"), "d", "x"))
  m2 <- modelo(instanciar(especializar(nucleo$NoNulo, "Y"), "d", "y"))
  p1 <- medir(m1, list(d = d1), id_medicion = "c", fecha = "2026-01-15",
              detalle = "resumen")
  p2 <- medir(m2, list(d = d2), id_medicion = "c", fecha = "2026-01-15",
              detalle = "resumen")
  # Dos lecturas de `d` con datos distintos: las dos cifras no cierran.
  list(p1 = p1, unido = suppressWarnings(rbind(p1, p2)))
}

# Lo que imprime `print()`: su salida, el error si aborta y la tabla de
# "Cuentas que no cierran" tal como se ve, leida de la impresion. Es la ultima
# tabla y la unica con la columna `control_lecturas`.
.recortado_imprimir <- function(x) {
  anterior <- options(width = 250L)
  on.exit(options(anterior), add = TRUE)
  salida <- NULL
  error <- tryCatch({
    salida <- utils::capture.output(print(x), type = "output")
    NULL
  }, error = function(e) conditionMessage(e))
  encabezado <- grep("control_lecturas", salida, fixed = TRUE)
  cuentas <- if (length(encabezado) == 1L) {
    utils::read.table(
      text = salida[encabezado:length(salida)], header = TRUE,
      stringsAsFactors = FALSE, na.strings = c("NA", "<NA>")
    )
  }
  list(salida = paste(salida, collapse = "\n"), error = error, cuentas = cuentas)
}

# Las columnas de la tabla de cuentas que no cierran, en su orden.
.recortado_columnas_cuentas <- c(
  "metrica_instanciada", "entidad", "atributo", "estado_cuentas",
  "motivo_cuentas", "diferencia_cuentas"
)

test_that("imprimir un resumen recortado no aborta y muestra solo las columnas que conserva", {
  skip_on_cran()
  partes <- .recortado_partes()
  u <- partes$unido
  expect_true(all(u$estado_cuentas == "no_cierran"))
  for (columnas in list(
    c("atributo", "estado_cuentas"),
    c("entidad", "atributo", "estado_cuentas"),
    setdiff(names(u), "id_medicion"),
    setdiff(names(u), "entidad_referida")
  )) {
    etiqueta <- paste(columnas, collapse = ",")
    impreso <- .recortado_imprimir(u[, columnas])
    expect_null(impreso$error, label = etiqueta)
    # Las cuentas que no cierran, con las columnas del recorte y el control:
    # una columna que el recorte no tiene no se imprime vacia.
    expect_identical(
      names(impreso$cuentas),
      c(intersect(.recortado_columnas_cuentas, columnas), "control_lecturas"),
      label = etiqueta
    )
    expect_identical(impreso$cuentas$estado_cuentas, c("no_cierran", "no_cierran"),
                     label = etiqueta)
    expect_identical(impreso$cuentas$control_lecturas, rep("desconocido", 2L),
                     label = etiqueta)
  }
})

test_that("el control de un resumen recortado es desconocido, fila por fila", {
  skip_on_cran()
  u <- .recortado_partes()$unido
  impreso <- .recortado_imprimir(u[, c("atributo", "estado_cuentas")])
  expect_identical(impreso$cuentas, data.frame(
    atributo = c("x", "y"), estado_cuentas = rep("no_cierran", 2L),
    control_lecturas = rep("desconocido", 2L), stringsAsFactors = FALSE
  ))
})

test_that("el control: el resumen entero conserva su control de lecturas", {
  skip_on_cran()
  u <- .recortado_partes()$unido
  # Dos partes en memoria, cada una con la huella de sus datos.
  s <- semaforo_calidad(u, cortes = list(Completitud = c(0.5, 0.9)))
  expect_identical(s$control_lecturas, c("completo", "completo"))
  impreso <- .recortado_imprimir(u)
  expect_null(impreso$error)
  expect_identical(names(impreso$cuentas),
                   c(.recortado_columnas_cuentas, "control_lecturas"))
  expect_identical(impreso$cuentas$control_lecturas, c("completo", "completo"))
  expect_false(grepl("desconocido", impreso$salida, fixed = TRUE))
})

test_that("el control: un recorte cuyas cifras cierran se imprime igual que antes", {
  skip_on_cran()
  p1 <- .recortado_partes()$p1
  impreso <- .recortado_imprimir(p1[, c("atributo", "estado_cuentas")])
  expect_null(impreso$error)
  expect_null(impreso$cuentas)
})

test_that("el semaforo de un resumen sin entidad_referida declara el control desconocido", {
  skip_on_cran()
  nucleo <- metricas_nucleo()
  medido <- medir(
    modelo(instanciar(especializar(nucleo$NoNulo, "X"), "d", "x")),
    list(d = data.frame(x = c(1, NA, 3, 4))),
    id_medicion = "c", fecha = "2026-01-15", detalle = "resumen"
  )
  completo <- semaforo_calidad(medido, cortes = list(Completitud = c(0.5, 0.9)))
  expect_identical(completo$control_lecturas, "una_lectura")
  recorte <- medido[, setdiff(names(medido), "entidad_referida")]
  s <- semaforo_calidad(recorte, cortes = list(Completitud = c(0.5, 0.9)))
  expect_identical(s$control_lecturas, "desconocido")
  expect_false(is.na(s$nivel))
  mensajes <- character()
  withCallingHandlers(
    utils::capture.output(print(s)),
    message = function(m) {
      mensajes <<- c(mensajes, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("control de lecturas desconocido", mensajes,
                        fixed = TRUE, useBytes = TRUE)))
})
