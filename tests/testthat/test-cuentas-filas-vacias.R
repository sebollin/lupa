# `x[cond, ]` con un `NA` en `cond` agrega filas con todas sus columnas `NA`
# (`[.data.frame`). Sobre ellas `tablero_calidad()` e `indice_calidad()`
# abortaban con "valor ausente donde TRUE/FALSE es necesario", y
# `semaforo_calidad()` las publicaba como una celda sin valor. Ahora los tres
# se detienen con `lupa_resumen_incompleto`, la causa y como evitarlo.

.vacias_resumen <- function() {
  nucleo <- metricas_nucleo()
  d1 <- data.frame(x = c(1, NA, 3, 4), y = c(1, 2, NA, 4))
  d2 <- d1
  d2$x[2] <- 2
  fecha <- as.POSIXct("2026-01-15", tz = "UTC")
  p1 <- medir(modelo(instanciar(especializar(nucleo$NoNulo, "X"), "d", "x")),
              list(d = d1), id_medicion = "c", fecha = fecha, detalle = "resumen")
  p2 <- medir(modelo(instanciar(especializar(nucleo$NoNulo, "Y"), "d", "y")),
              list(d = d2), id_medicion = "c", fecha = fecha, detalle = "resumen")
  p3 <- medir(modelo(instanciar(especializar(nucleo$AtributoDuplicado, "U"), "e", "z")),
              list(e = data.frame(z = c(1, 2, 2))), id_medicion = "c",
              fecha = fecha, detalle = "resumen")
  suppressWarnings(rbind(p1, p2, p3))
}

.vacias_funciones <- list(
  tablero_calidad = function(x) tablero_calidad(x),
  indice_calidad = function(x) indice_calidad(x, pesos = c(Unicidad = 1)),
  semaforo_calidad = function(x) {
    semaforo_calidad(x, cortes = list(Completitud = c(0.5, 0.9), Unicidad = c(0.5, 0.9)))
  }
)

for (funcion in names(.vacias_funciones)) {
  test_that(paste0(funcion, "() rechaza las filas vacias de un filtro con NA"), {
    skip_on_cran()
    u <- .vacias_resumen()
    # `diferencia_cuentas` es NA en la fila que cierra: el filtro la vuelve vacia.
    filtrada <- u[u$diferencia_cuentas > 0, , drop = FALSE]
    expect_true(anyNA(filtrada$metrica_instanciada))
    e <- tryCatch(.vacias_funciones[[funcion]](filtrada), error = function(e) e)
    expect_s3_class(e, "lupa_resumen_incompleto")
    mensaje <- conditionMessage(e)
    for (p in c(paste0("`", funcion, "()`"), "1 fila vac", "x[which(cond), ]",
                "subset(x, cond)")) {
      expect_true(grepl(p, mensaje, fixed = TRUE, useBytes = TRUE), label = p)
    }
    e <- tryCatch(.vacias_funciones[[funcion]](u[NA, ]), error = function(e) e)
    expect_s3_class(e, "lupa_resumen_incompleto")
  })

  test_that(paste0("el control: ", funcion, "() trabaja con el filtro que recomienda"), {
    skip_on_cran()
    u <- .vacias_resumen()
    # Una condicion con un `NA`: el resumen tiene las filas X, Y y U.
    cond <- c(TRUE, NA, TRUE)
    expect_identical(u$metrica_instanciada, c("X@d.x", "Y@d.y", "U@e.z"))
    con_which <- suppressWarnings(.vacias_funciones[[funcion]](u[which(cond), ]))
    con_subset <- suppressWarnings(.vacias_funciones[[funcion]](subset(u, cond)))
    expect_identical(con_which, con_subset)
    entero <- suppressWarnings(.vacias_funciones[[funcion]](u[!is.na(u$metrica_instanciada), ]))
    expect_false(is.null(entero))
  })
}
