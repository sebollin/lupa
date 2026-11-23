# `tablero_calidad()`, `indice_calidad()` y `semaforo_calidad()` necesitan el
# resumen de medicion entero. Sobre uno recortado abortaban con un error de R
# -"los argumentos implican un numero diferente de filas"- o, sin `entidad`,
# trabajaban sobre `entidad_referida`, que `$` tomaba por coincidencia parcial.
# Ahora se detienen con un error propio que nombra las columnas que faltan. Sin
# `entidad_referida` si trabajan, y el control de lecturas lo declara
# `desconocido`.

.entero_resumen <- function() {
  nucleo <- metricas_nucleo()
  d1 <- data.frame(x = c(1, NA, 3, 4), y = c(1, 2, NA, 4))
  d2 <- d1
  d2$x[2] <- 2
  m1 <- modelo(instanciar(especializar(nucleo$NoNulo, "X"), "d", "x"),
               instanciar(especializar(nucleo$AtributoDuplicado, "U"), "d", "x"))
  m2 <- modelo(instanciar(especializar(nucleo$NoNulo, "Y"), "d", "y"))
  p1 <- medir(m1, list(d = d1), id_medicion = "c", fecha = "2026-01-15",
              detalle = "resumen")
  p2 <- medir(m2, list(d = d2), id_medicion = "c", fecha = "2026-01-15",
              detalle = "resumen")
  suppressWarnings(rbind(p1, p2))
}

.entero_cortes <- list(Completitud = c(0.5, 0.9), Unicidad = c(0.5, 0.9))

.entero_funciones <- list(
  tablero_calidad = function(x) tablero_calidad(x),
  indice_calidad = function(x) {
    indice_calidad(x, pesos = c(Completitud = 0.5, Unicidad = 0.5))
  },
  semaforo_calidad = function(x) semaforo_calidad(x, cortes = .entero_cortes)
)

.entero_esperar_rechazo <- function(funcion, x, faltan) {
  e <- tryCatch(.entero_funciones[[funcion]](x), error = function(e) e)
  expect_s3_class(e, "lupa_resumen_incompleto")
  mensaje <- conditionMessage(e)
  expect_true(startsWith(mensaje, paste0("`", funcion, "()` necesita el resumen")),
              label = funcion)
  for (columna in faltan) {
    expect_true(grepl(paste0("`", columna, "`"), mensaje, fixed = TRUE,
                      useBytes = TRUE), label = paste(funcion, columna))
  }
}

for (funcion in names(.entero_funciones)) {
  test_that(paste0(funcion, "() rechaza un resumen sin id_medicion nombrando lo que falta"), {
    skip_on_cran()
    u <- .entero_resumen()
    .entero_esperar_rechazo(funcion, u[, setdiff(names(u), "id_medicion")],
                            "id_medicion")
    # Sin `entidad` tampoco trabaja sobre `entidad_referida`.
    .entero_esperar_rechazo(funcion, u[, setdiff(names(u), "entidad")], "entidad")
    .entero_esperar_rechazo(funcion, u[, c("atributo", "estado_cuentas")],
                            c("id_medicion", "metrica", "entidad", "resultado"))
  })

  test_that(paste0(funcion, "() trabaja sin entidad_referida y declara el control desconocido"), {
    skip_on_cran()
    u <- .entero_resumen()
    entero <- suppressWarnings(.entero_funciones[[funcion]](u))
    sin <- suppressWarnings(
      .entero_funciones[[funcion]](u[, setdiff(names(u), "entidad_referida")])
    )
    cuentas <- function(x) {
      if (identical(funcion, "indice_calidad")) x <- x$tablero
      attr(x, "cuentas", exact = TRUE)$control_lecturas
    }
    expect_length(cuentas(sin), sum(u$estado_cuentas == "no_cierran"))
    expect_gt(length(cuentas(sin)), 0L)
    expect_true(all(cuentas(sin) == "desconocido"))
    expect_false(any(cuentas(entero) == "desconocido"))
  })
}

test_that("el semaforo necesita ademas los conteos de su fraccion evaluada", {
  skip_on_cran()
  u <- .entero_resumen()
  .entero_esperar_rechazo("semaforo_calidad", u[, setdiff(names(u), "n_universo")],
                          "n_universo")
  # El tablero no los lee y trabaja sin ellos.
  expect_s3_class(tablero_calidad(u[, setdiff(names(u), "n_universo")]),
                  "tablero_calidad")
})
