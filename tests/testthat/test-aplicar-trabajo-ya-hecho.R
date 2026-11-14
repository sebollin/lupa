# Una accion del plan que no cambia nada porque una accion anterior del mismo
# plan ya resolvio su problema no fallo: hizo su trabajo antes de correr. Se
# registraba `fallida` y el resumen de `aplicar()` decia que algo habia salido
# mal sobre datos que quedaban limpios. Si el problema sigue en los datos -o
# nunca estuvo en los que recibe `aplicar()`-, la accion sigue `fallida`.
testthat::skip_on_cran()

.plan_tab_y_espacio <- function(datos) {
  plan <- planificar_limpieza(
    perfilar(datos, fecha = "2026-01-15", analizar_dependencias = FALSE),
    datos
  )
  plan$aplicar <- plan$estrategia %in%
    c("recortar_espacios", "reemplazar_separadores")
  plan
}

.mensajes_resultado <- function(resultado) {
  paste(testthat::capture_messages(print(resultado)), collapse = "\n")
}

test_that("una accion cuyo problema ya resolvio otra del plan queda ejecutada", {
  # El tabulador al final de `uno` es a la vez un espacio al borde y un
  # separador dentro del campo. `recortar_espacios` corre antes y lo quita, y a
  # `reemplazar_separadores` no le queda nada.
  datos <- data.frame(
    t = c(paste0("uno", "\t"), "dos ", "tres", "cuatro", "cinco"),
    stringsAsFactors = FALSE
  )
  plan <- .plan_tab_y_espacio(datos)
  separadores <- plan$estrategia == "reemplazar_separadores"
  # Primera mitad: las dos acciones vienen del plan, cada una con su problema
  # contado, y la de separadores corre despues del recorte.
  expect_equal(sum(plan$aplicar), 2L)
  expect_equal(plan$n_afectadas[separadores], 1)
  expect_gt(plan$orden[separadores],
            plan$orden[plan$estrategia == "recortar_espacios"])

  resultado <- aplicar(plan, datos)
  registro <- resultado$registro
  fila <- registro$estrategia == "reemplazar_separadores"
  expect_identical(registro$estado[fila], "ejecutada")
  expect_identical(registro$n_cambiadas[fila], 0)
  expect_true(is.na(registro$error[fila]))
  expect_false(any(registro$estado == "fallida"))
  expect_identical(resultado$datos$t, c("uno", "dos", "tres", "cuatro", "cinco"))
  # El resumen impreso no la cuenta entre las fallidas.
  mensajes <- .mensajes_resultado(resultado)
  expect_match(mensajes, "2 acciones ejecutadas", fixed = TRUE)
  expect_no_match(mensajes, "fallida", fixed = TRUE)
})

test_that("una accion que no pudo con su problema sigue fallida", {
  datos <- data.frame(
    t = c(paste0("uno", "\t", "dos"), "tres", "cuatro", "cinco"),
    stringsAsFactors = FALSE
  )
  plan <- .plan_tab_y_espacio(datos)
  # Sola, sobre los datos del perfil, la accion si cambia la celda: el cero de
  # abajo no viene de una accion que no corrio.
  sola <- aplicar(plan, datos)$registro
  expect_identical(
    sola$n_cambiadas[sola$estrategia == "reemplazar_separadores"], 1
  )
  # Si el ejecutor no toca la celda, el separador sigue en los datos cuando la
  # accion termina, y la accion fallo.
  local_mocked_bindings(
    .reemplazar_separadores = function(x, proteccion = NULL) {
      list(valor = x, n = 0L)
    },
    .package = "lupa"
  )
  resultado <- aplicar(plan, datos)
  fila <- resultado$registro$estrategia == "reemplazar_separadores"
  expect_identical(resultado$registro$estado[fila], "fallida")
  expect_match(resultado$registro$error[fila], "sin efecto", fixed = TRUE)
  expect_match(.mensajes_resultado(resultado), "1 acci\u00f3n fallida",
               fixed = TRUE)
})

test_that("un plan aplicado a datos sin el problema sigue marcando fallida", {
  # El problema nunca estuvo en lo que recibe `aplicar()`: ninguna accion
  # anterior hizo el trabajo, el plan es de otra entrega.
  sucios <- data.frame(t = c(paste0("uno", "\t", "dos"), "tres", "cuatro"),
                       stringsAsFactors = FALSE)
  limpios <- data.frame(t = c("uno dos", "tres", "cuatro"),
                        stringsAsFactors = FALSE)
  plan <- .plan_tab_y_espacio(sucios)
  resultado <- aplicar(plan, limpios)
  fila <- resultado$registro$estrategia == "reemplazar_separadores"
  expect_identical(resultado$registro$estado[fila], "fallida")
})
