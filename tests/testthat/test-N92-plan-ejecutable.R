# Lo que el paquete recomienda, el paquete lo tiene que poder ejecutar.
#
# `perfilar()` analiza sobre la forma saneada -`.texto_analizable()` marca
# UTF-8 lo declarado `bytes` cuando sus bytes son validos, a proposito- y sobre
# esa forma publica `n_codificacion_reparable` y su estado. La aplicacion del
# plan trabajaba sobre la columna CRUDA, todavia declarada `bytes`, y ahi el
# reparador muere en `nchar()`, que sobre esa marca no puede contar caracteres.
#
# Medido antes del arreglo, con el mismo dato:
#
#   perfilar()             reparable = 2, estado "reparado"
#   planificar_limpieza()  accion recomendada y activa, n_afectadas = 2
#   aplicar()              estado "fallida", n_cambiadas = 0, dato intacto
#
# Una cifra publicada y una accion recomendada que el propio paquete no puede
# ejecutar es el invariante roto un piso mas arriba: no en lo que mide, sino en
# lo que propone hacer con lo medido.

.n92_bytes <- function(codigos) {
  x <- rawToChar(as.raw(codigos))
  Encoding(x) <- "bytes"
  x
}

# "A" con tilde, espacio duro y "e" aguda: el mojibake que va por el camino de
# ftfy y no por las tablas de reemplazo.
.N92_MOJI <- c(0xc3, 0x83, 0xc2, 0xa0, 0xc3, 0xa9)

.n92_reparar <- function(valores) {
  datos <- data.frame(v = valores, stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(
    datos, fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  plan <- suppressWarnings(planificar_limpieza(perfil))
  accion <- plan[grepl("codificacion", plan$estrategia, fixed = TRUE), ,
                 drop = FALSE]
  if (!nrow(accion)) {
    return(list(perfil = perfil, plan = NULL, registro = NULL))
  }
  activo <- plan
  activo$aplicar <- plan$estrategia == accion$estrategia[[1L]]
  resultado <- suppressWarnings(aplicar(activo, datos))
  registro <- resultado$registro
  list(
    perfil = perfil, plan = accion,
    registro = registro[registro$estrategia == "reparar_codificacion", ,
                        drop = FALSE]
  )
}

test_that("la reparacion que el plan recomienda se ejecuta sobre lo declarado", {
  crudo <- .n92_bytes(.N92_MOJI)
  r <- .n92_reparar(c(crudo, crudo, "z"))

  # El perfil publica una cifra y el plan propone una accion sobre ella.
  expect_identical(r$perfil$columnas$n_codificacion_reparable[[1L]], 2L)
  expect_false(is.null(r$plan))
  expect_true(r$plan$recomendada[[1L]])

  # Y la accion se ejecuta, con las celdas que el plan anuncio.
  expect_identical(r$registro$estado[[1L]], "ejecutada")
  expect_equal(
    as.numeric(r$registro$n_cambiadas[[1L]]),
    as.numeric(r$plan$n_afectadas[[1L]])
  )
})

test_that("el control sin la marca se comporta igual", {
  # Sin este control, la comprobacion de arriba pasaria tambien si el paquete
  # dejara de recomendar la accion.
  sin_marca <- rawToChar(as.raw(.N92_MOJI))
  r <- .n92_reparar(c(sin_marca, sin_marca, "z"))
  expect_false(is.null(r$plan))
  expect_identical(r$registro$estado[[1L]], "ejecutada")
  expect_gt(as.numeric(r$registro$n_cambiadas[[1L]]), 0)
})
