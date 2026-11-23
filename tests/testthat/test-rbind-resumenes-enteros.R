# `rbind()` y `rbind(prioridad = )` juntan resumenes de medicion enteros. Con
# una parte recortada, `rbind.data.frame()` abortaba con "el numero de columnas
# de los argumentos no coincide" y la consolidacion sin `entidad_referida` con
# "argumento tiene longitud cero"; sin `estado_cuentas` devolvia un resumen sin
# esa columna. Ahora se detienen con `lupa_resumen_incompleto`, la parte y lo
# que le falta.

.enteros_partes <- function() {
  nucleo <- metricas_nucleo()
  d <- data.frame(x = c(1, NA, 3), y = c(1, 2, NA))
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  a <- medir(modelo(instanciar(especializar(nucleo$NoNulo, "X"), "d", "x")),
             list(d = d), id_medicion = "p", fecha = fecha, detalle = "resumen")
  b <- medir(modelo(instanciar(especializar(nucleo$NoNulo, "Y"), "d", "y")),
             list(d = d), id_medicion = "p", fecha = fecha, detalle = "resumen")
  list(a = a, b = b)
}

.enteros_rechazo <- function(expr, patrones) {
  e <- tryCatch(expr, error = function(e) e)
  expect_s3_class(e, "lupa_resumen_incompleto")
  for (p in patrones) {
    expect_true(grepl(p, conditionMessage(e), fixed = TRUE, useBytes = TRUE),
                label = p)
  }
}

test_that("rbind() de una parte recortada se rechaza con lo que le falta", {
  skip_on_cran()
  p <- .enteros_partes()
  for (columna in c("entidad_referida", "id_medicion", "estado_cuentas", "resultado")) {
    recortada <- p$a[, setdiff(names(p$a), columna), drop = FALSE]
    .enteros_rechazo(rbind(recortada, p$b), c("parte 1", paste0("`", columna, "`")))
    .enteros_rechazo(
      rbind(a = recortada, b = p$b, prioridad = c("a", "b")),
      c("parte `a`", paste0("`", columna, "`"))
    )
  }
})

test_that("rbind() de partes con columnas que otra no trae se rechaza", {
  skip_on_cran()
  p <- .enteros_partes()
  con_nota <- p$b
  con_nota$nota <- "revisar"
  .enteros_rechazo(rbind(a = p$a, b = con_nota), c("parte `b`", "`nota`"))
})

test_that("rbind() con filas vacias de un filtro con NA se rechaza con la causa", {
  skip_on_cran()
  p <- .enteros_partes()
  filtrada <- p$a[c(NA, 1L), ]
  .enteros_rechazo(rbind(filtrada, p$b), c("parte 1", "1 fila vac", "which(cond)"))
})

test_that("el control: rbind() de partes enteras sigue juntando", {
  skip_on_cran()
  p <- .enteros_partes()
  u <- rbind(p$a, p$b)
  expect_s3_class(u, "medicion_resumen")
  expect_identical(nrow(u), 2L)
  u <- rbind(a = p$a, b = p$b, prioridad = c("a", "b"))
  expect_identical(nrow(u), 2L)
})
